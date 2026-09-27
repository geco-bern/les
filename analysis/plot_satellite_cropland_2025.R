# Run from the repository root:
# Rscript analysis/plot_satellite_cropland_2025.R [all|regions|global|plot]
# Dependencies: dplyr, tidyr, purrr, readr, tibble, ggplot2, gtable, terra, httr,
# jsonlite, here, sf, rnaturalearth, rnaturalearthdata.
# Sentinel-2 land-cover classification, Impact Observatory / Microsoft / Esri.
# 2025 data; Crops = class 5. See satellite_cropland_2025.md for limitations.
# Global: nearest-neighbour service overview at 0.01 degrees, then aggregate
# 10 x 10 valid samples into 0.1-degree cells. NOT an exact 10 m area census.
# Regions: 20 x 20 km windows exported in local UTM at 10 m, nearest neighbour.

service_url <- paste0("https://ic.imagery1.arcgis.com/arcgis/rest/services/",
                      "Sentinel2_10m_LandCover/ImageServer")
crop_year <- 2025L
crop_regions <- tibble::tibble(
  id = c("A", "B", "C", "D"),
  name = c("Iowa, USA", "Mato Grosso, Brazil", "Punjab, India", "Beauce, France"),
  slug = c("iowa", "mato_grosso", "punjab", "beauce"),
  lon = c(-93.7, -55.9, 75.2, 1.5),
  lat = c(42.2, -12.6, 30.8, 48.1),
  epsg = c(32615L, 32721L, 32643L, 32631L)
)

crop_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = 16)

crop_api <- function(endpoint, query) {
  response <- httr::RETRY("GET", paste0(service_url, endpoint), query = query,
                          httr::timeout(180), times = 3, quiet = TRUE)
  httr::stop_for_status(response)
  result <- httr::content(response, as = "parsed", encoding = "UTF-8")
  if (!is.null(result$error)) stop(crop_json(result$error), call. = FALSE)
  result
}

export_crop_classes <- function(path, bbox, sr, size, year = crop_year) {
  if (file.exists(path)) return(terra::rast(path))
  query <- list(
    f = "json", bbox = paste(bbox, collapse = ","), bboxSR = sr,
    imageSR = sr, size = paste(size, collapse = ","), format = "tiff",
    pixelType = "U8", noData = 0, interpolation = "RSP_NearestNeighbor",
    renderingRule = crop_json(list(rasterFunction = "None")),
    mosaicRule = crop_json(list(where = paste0("Year=", year))),
    adjustAspectRatio = "false"
  )
  result <- crop_api("/exportImage", query)
  if (is.null(result$href)) stop("No download URL returned for ", path)
  partial <- paste0(path, ".part.tif")
  on.exit(unlink(partial), add = TRUE)
  response <- httr::RETRY("GET", result$href, httr::write_disk(partial, overwrite = TRUE),
                          httr::timeout(180), times = 3, quiet = TRUE)
  httr::stop_for_status(response)
  r <- terra::rast(partial)
  if (terra::nlyr(r) != 1L || terra::ncol(r) != size[1] || terra::nrow(r) != size[2]) {
    stop("Unexpected raster dimensions: ", path)
  }
  expected <- bbox[c(1, 3, 2, 4)]
  if (any(abs(as.vector(terra::ext(r)) - expected) > 0.001)) {
    stop("Unexpected raster extent: ", path)
  }
  rm(r)
  if (!file.rename(partial, path)) stop("Cannot save ", path)
  jsonlite::write_json(list(request = query, response = result),
                       paste0(path, ".json"), auto_unbox = TRUE, pretty = TRUE)
  terra::rast(path)
}

crop_mask <- function(r) {
  # Explicitly distinguish missing/cloud pixels from observed non-cropland.
  codes <- c(1, 2, 4, 5, 7, 8, 9, 11)
  terra::classify(r, cbind(codes, as.integer(codes == 5)), others = NA)
}

make_crop_regions <- function(data_dir) {
  purrr::walk(seq_len(nrow(crop_regions)), function(i) {
    region <- dplyr::slice(crop_regions, i)
    message("Downloading 10 m window: ", region$name)
    point <- terra::vect(matrix(c(region$lon, region$lat), ncol = 2), crs = "EPSG:4326")
    xy <- round(terra::crds(terra::project(point, paste0("EPSG:", region$epsg))) / 10) * 10
    bbox <- c(xy[1] - 10000, xy[2] - 10000, xy[1] + 10000, xy[2] + 10000)
    path <- file.path(data_dir, paste0(region$slug, "_landcover_2025_10m.tif"))
    r <- export_crop_classes(path, bbox, region$epsg, c(2000, 2000))
    stopifnot(all(abs(terra::res(r) - 10) < 1e-6))
    terra::writeRaster(crop_mask(r),
                       file.path(data_dir, paste0(region$slug, "_crops_2025_10m.tif")),
                       overwrite = TRUE, datatype = "INT1U", NAflag = 255,
                       gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  })
  readr::write_csv(crop_regions, file.path(data_dir, "regions.csv"))
}

aggregate_crop_samples <- function(r, fact = 10L) {
  mask <- crop_mask(r)
  valid <- terra::ifel(is.na(mask), 0, 1)
  count <- terra::aggregate(valid, fact, "sum")
  crops <- terra::aggregate(terra::ifel(is.na(mask), 0, mask), fact, "sum")
  fraction <- terra::ifel(count > 0, crops / count, NA)
  names(fraction) <- "cropland_fraction_of_valid_samples"
  names(count) <- "valid_sample_count"
  c(fraction, count)
}

process_crop_tile <- function(i, tiles, tile_dir) {
  t <- dplyr::slice(tiles, i)
  stem <- sprintf("tile_%+04d_%+03d", t$west, t$south)
  result_path <- file.path(tile_dir, paste0(stem, "_0.1deg.tif"))
  if (file.exists(result_path)) return(result_path)
  source_path <- file.path(tile_dir, paste0(stem, "_0.01deg.tif"))
  message("Global tile ", i, "/", nrow(tiles), ": ", stem)
  r <- export_crop_classes(source_path, c(t$west, t$south, t$west + 20, t$north),
                            4326, c(2000, round((t$north - t$south) / 0.01)))
  out <- aggregate_crop_samples(r)
  terra::writeRaster(out, result_path, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  result_path
}

make_crop_global <- function(data_dir, workers = 3L) {
  tile_dir <- file.path(data_dir, "global_tiles")
  dir.create(tile_dir, showWarnings = FALSE)
  # Sentinel-2 coverage: 60 S to 84 N. The output grid extends to both poles,
  # with cells beyond coverage retained as NA, never presumed non-cropland.
  tiles <- tidyr::expand_grid(south = seq(-60, 80, 20), west = seq(-180, 160, 20)) |>
    dplyr::mutate(north = pmin(south + 20, 84))
  # Fresh R processes work in Positron and on Windows; never fork the IDE session.
  stopifnot(length(workers) == 1L, is.finite(workers), workers >= 1,
            workers == as.integer(workers))
  if (workers == 1L) {
    paths <- purrr::map(seq_len(nrow(tiles)), process_crop_tile, tiles = tiles, tile_dir = tile_dir)
  } else {
    cl <- parallel::makePSOCKcluster(min(workers, nrow(tiles)))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    # Propagate the active project library, including renv, to the fresh sessions.
    parallel::clusterCall(cl, function(paths) {
      .libPaths(paths)
      terra::terraOptions(progress = 0, memfrac = 0.3)
      NULL
    }, .libPaths())
    parallel::clusterExport(cl, c("crop_json", "crop_api", "export_crop_classes",
      "crop_mask", "aggregate_crop_samples", "service_url", "crop_year"),
      envir = environment(make_crop_global))
    paths <- parallel::parLapplyLB(cl, seq_len(nrow(tiles)), process_crop_tile,
                                   tiles = tiles, tile_dir = tile_dir)
  }
  sources <- terra::sprc(purrr::map(paths, terra::rast))
  world <- terra::merge(sources)
  world <- terra::extend(world, terra::ext(-180, 180, -90, 90))
  stopifnot(all(abs(terra::res(world) - 0.1) < 1e-8),
            terra::ncol(world) == 3600, terra::nrow(world) == 1800)
  terra::writeRaster(world, file.path(data_dir, "cropland_2025_sampled_0.1deg.tif"),
                     overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
}

draw_crop_world <- function(data_dir) {
  r <- terra::rast(file.path(data_dir, "cropland_2025_sampled_0.1deg.tif"))[[1]] * 100
  names(r) <- "percent"
  colours <- grDevices::colorRampPalette(c("#eceee9", "#fff0b7", "#e7af38", "#aa6017", "#542b0e"))(101)
  pixels <- scales::col_numeric(colours, domain = c(0, 100), na.color = "white")(
    as.vector(terra::values(r))) |>
    matrix(nrow = terra::nrow(r), byrow = TRUE) |>
    grDevices::as.raster()
  borders <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
  # A bitmap preserves the regular 0.1-degree grid even where observations are NA.
  legend_range <- tibble::tibble(x = c(-180, 180), y = c(-60, 85), percent = c(0, 100))
  ggplot2::ggplot(legend_range, ggplot2::aes(x, y, fill = percent)) +
    ggplot2::geom_point(shape = 22, alpha = 0, show.legend = TRUE) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(alpha = 1)) +
    ggplot2::annotation_raster(pixels, terra::xmin(r), terra::xmax(r),
      terra::ymin(r), terra::ymax(r), interpolate = FALSE) +
    ggplot2::geom_sf(data = borders, inherit.aes = FALSE, fill = NA,
      colour = "#7b8279", linewidth = .09) +
    ggplot2::geom_point(data = crop_regions, ggplot2::aes(lon, lat), inherit.aes = FALSE,
      shape = 21, fill = "white", colour = "#9f261b", size = 2) +
    ggplot2::geom_text(data = crop_regions, ggplot2::aes(lon, lat, label = id),
      inherit.aes = FALSE, nudge_y = 5, fontface = "bold", colour = "#9f261b", size = 3) +
    ggplot2::scale_fill_gradientn(colours = colours, limits = c(0, 100),
      name = "% crops", na.value = "white", oob = scales::squish) +
    ggplot2::coord_sf(crs = sf::st_crs(4326), datum = NA, xlim = c(-180, 180),
      ylim = c(-60, 85), expand = FALSE) +
    ggplot2::labs(title = "Cropland in 2025 | global satellite overview",
      subtitle = "0.1-degree grid: share of valid 0.01-degree service-overview samples; not an exact 10 m area fraction") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = .5),
      plot.subtitle = ggplot2::element_text(hjust = .5, size = 9),
      plot.margin = ggplot2::margin(12, 12, 12, 12))
}

draw_crop_region <- function(i, data_dir) {
  region <- dplyr::slice(crop_regions, i)
  r <- terra::rast(file.path(data_dir, paste0(region$slug, "_landcover_2025_10m.tif")))
  # Keep all 2000 x 2000 native pixels; water is context and clouds remain white.
  display <- terra::ifel(r == 1, 2, crop_mask(r))
  v <- as.vector(terra::values(display))
  colours <- dplyr::coalesce(c("#e3e5df", "#c58b22", "#aec8d4")[v + 1L], "white")
  pixels <- grDevices::as.raster(matrix(colours, nrow = terra::nrow(r), byrow = TRUE))
  ggplot2::ggplot() +
    ggplot2::annotation_raster(pixels, 0, 20, 0, 20, interpolate = FALSE) +
    ggplot2::annotate("rect", xmin = .6, xmax = 6.4, ymin = .5, ymax = 2.2, fill = "white") +
    ggplot2::annotate("segment", x = 1, xend = 6, y = 1, yend = 1, linewidth = .8) +
    ggplot2::annotate("segment", x = c(1, 6), xend = c(1, 6), y = .85, yend = 1.15, linewidth = .5) +
    ggplot2::annotate("text", x = 3.5, y = 1.65, label = "5 km", size = 3.5) +
    ggplot2::coord_fixed(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
    ggplot2::labs(title = paste(region$id, region$name),
      subtitle = sprintf("2025 | 10 m | 20 x 20 km | %.2f°, %.2f°", region$lat, region$lon),
      caption = "Ochre: crops | Grey: other land | Blue: water | White: cloud / no data") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = .5, size = 16),
      plot.subtitle = ggplot2::element_text(hjust = .5),
      plot.caption = ggplot2::element_text(hjust = .5, size = 9),
      plot.margin = ggplot2::margin(12, 12, 12, 12))
}

save_crop_plots <- function(data_dir, output_dir) {
  world <- draw_crop_world(data_dir)
  regions <- purrr::map(seq_len(nrow(crop_regions)), draw_crop_region, data_dir = data_dir)
  # gtable arranges ggplot grobs; no base-graphics device state is needed.
  combined <- gtable::gtable(widths = grid::unit(c(1, 1), "null"),
    heights = grid::unit(c(.8, 1, 1), "null"))
  combined <- gtable::gtable_add_grob(combined, ggplot2::ggplotGrob(world), t = 1, l = 1, r = 2)
  combined <- gtable::gtable_add_grob(combined, purrr::map(regions, ggplot2::ggplotGrob),
    t = c(2, 2, 3, 3), l = c(1, 2, 1, 2))
  combined <- gtable::gtable_add_rows(combined, grid::unit(.5, "in"))
  credit <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0,
      label = "Sentinel-2 10m Land Use/Land Cover Time Series, 2025 | Impact Observatory, Microsoft and Esri | CC BY 4.0", size = 3.5) +
    ggplot2::theme_void()
  combined <- gtable::gtable_add_grob(combined, ggplot2::ggplotGrob(credit), t = 4, l = 1, r = 2)
  save <- function(plot, stem, width, height) {
    purrr::walk(c("png", "pdf"), function(extension) {
      ggplot2::ggsave(file.path(output_dir, paste0(stem, ".", extension)), plot,
        width = width, height = height, dpi = 300, bg = "white",
        device = if (extension == "pdf") grDevices::cairo_pdf else "png")
    })
  }
  save(world, "cropland_2025_global", 14, 7)
  purrr::walk2(regions, crop_regions$slug, function(plot, slug) {
    save(plot, paste0("cropland_2025_", slug), 8, 8)
  })
  save(combined, "cropland_2025_global_and_regions", 16, 20)
}

main <- function(mode = "all") {
  required <- c("dplyr", "tidyr", "purrr", "readr", "tibble", "ggplot2", "gtable", "sf", "terra", "httr", "jsonlite", "here", "rnaturalearth", "rnaturalearthdata")
  missing <- purrr::discard(required, requireNamespace, quietly = TRUE)
  if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))
  stopifnot(mode %in% c("all", "regions", "global", "plot"))
  data_dir <- here::here("data", "satellite_cropland_2025")
  output_dir <- here::here("fig", "satellite_cropland_2025")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  terra::terraOptions(progress = 0, memfrac = 0.3)
  if (mode != "plot") {
    metadata <- crop_api("", list(f = "json"))
    catalog <- crop_api("/query", list(where = "Year=2025", outFields = "OBJECTID,Name,Year",
                                      returnGeometry = "false", f = "json"))
    if (length(catalog$features) != 1 || catalog$features[[1]]$attributes$Year != 2025) {
      stop("Could not uniquely identify the 2025 raster.")
    }
    jsonlite::write_json(list(retrieved_utc = format(Sys.time(), tz = "UTC"),
                              service = metadata, catalog = catalog),
                         file.path(data_dir, "source_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  }
  if (mode %in% c("all", "regions")) make_crop_regions(data_dir)
  if (mode %in% c("all", "global")) make_crop_global(data_dir)
  if (mode %in% c("all", "plot")) save_crop_plots(data_dir, output_dir)
  message("Completed ", mode, ". Output: ", output_dir)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  main(if (length(args)) args[1] else "all")
}
