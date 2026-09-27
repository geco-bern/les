# Hansen GFC v1.13: native-grid regional loss (2001-2025) and gain (2000-2012).
# Run from the repository root: Rscript analysis/plot_hansen_forest_change.R
# Append 'plot' to redraw cached windows. No Earth Engine account is needed.
# Dependencies: dplyr, tidyr, purrr, readr, tibble, ggplot2, gtable, terra, here.
# Fig. 2 footprints reconstructed by image registration; see hansen_support/README.md.
hansen_base <- "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2025-v1.13/"
hansen_regions <- tibble::tibble(
  id = LETTERS[1:4], name = c("Paraguay", "Indonesia", "USA", "Russia"),
  slug = c("paraguay", "indonesia", "usa", "russia"),
  lon = c(-59.8, 101.5, -93.3, 123.4), lat = c(-21.9, -0.4, 33.8, 62.1),
  tile = c("20S_060W", "00N_100E", "40N_100W", "70N_120E")
)

# Published centres are retained for provenance, not used as crop boundaries.
figure2_bounds <- readr::read_csv(here::here("analysis", "hansen_support", "figure2_bounds.csv"), show_col_types = FALSE)
stopifnot(identical(figure2_bounds$slug, hansen_regions$slug))
hansen_regions <- hansen_regions |>
  dplyr::select(-tile) |>
  dplyr::left_join(figure2_bounds, by = "slug")

hansen_window_path <- function(region, layer, data_dir) {
  file.path(data_dir, paste0(region$slug, "_", layer, ".tif"))
}

download_hansen_window <- function(region, layer, data_dir) {
  path <- hansen_window_path(region, layer, data_dir)
  bounds <- terra::ext(region$xmin, region$xmax, region$ymin, region$ymax)
  if (!file.exists(path)) {
    tile_dir <- file.path(data_dir, "tiles")
    dir.create(tile_dir, showWarnings = FALSE, recursive = TRUE)
    tiles <- tidyr::expand_grid(west = seq(floor(region$xmin / 10) * 10,
      floor((region$xmax - 1e-8) / 10) * 10, 10),
      north = seq(ceiling((region$ymin + 1e-8) / 10) * 10,
      ceiling(region$ymax / 10) * 10, 10))
    downloads <- purrr::map(seq_len(nrow(tiles)), function(i) {
      t <- dplyr::slice(tiles, i)
      tag <- sprintf("%02d%s_%03d%s", abs(t$north), ifelse(t$north >= 0, "N", "S"),
        abs(t$west), ifelse(t$west >= 0, "E", "W"))
      url <- paste0(hansen_base, "Hansen_GFC-2025-v1.13_", layer, "_", tag, ".tif")
      piece <- file.path(tile_dir, paste0(region$slug, "_", layer, "_", tag, ".tif"))
      if (!file.exists(piece)) {
        remote <- terra::rast(paste0("/vsicurl/", url))
        message("Reading Fig. 2 window: ", region$name, " / ", layer, " / ", tag)
        temporary <- paste0(piece, ".partial.tif")
        terra::crop(remote, bounds, snap = "out", filename = temporary, overwrite = TRUE,
          datatype = "INT1U", NAflag = 255, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
        stopifnot(file.rename(temporary, piece))
      }
      tibble::tibble(path = piece, url = url)
    }) |> purrr::list_rbind()
    pieces <- downloads$path
    temporary <- paste0(path, ".partial.tif")
    if (length(pieces) == 1L) {
      stopifnot(file.copy(pieces, temporary, overwrite = TRUE))
    } else {
      terra::merge(terra::sprc(purrr::map(pieces, terra::rast)), filename = temporary,
        overwrite = TRUE, datatype = "INT1U", NAflag = 255,
        gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    }
    stopifnot(file.rename(temporary, path))
    writeLines(downloads$url, paste0(path, ".source.txt"))
  }
  r <- terra::rast(path)
  stopifnot(max(abs(as.vector(terra::ext(r)) - as.vector(bounds))) <= 0.000251)
  path
}

read_hansen_region <- function(region, data_dir) {
  layers <- c("treecover2000", "lossyear", "gain", "datamask")
  r <- purrr::map(layers, function(layer) terra::rast(hansen_window_path(region, layer, data_dir)))
  purrr::walk(r, function(x) {
    terra::compareGeom(r[[1]], x, stopOnError = TRUE)
    stopifnot(all(abs(terra::res(x) - 0.00025) < 1e-10))
  })
  out <- purrr::reduce(r, c)
  names(out) <- layers
  ranges <- terra::global(out, c("min", "max"), na.rm = TRUE)
  stopifnot(all(ranges$min >= 0), all(ranges$max <= c(100, 25, 1, 2)))
  out
}

hansen_summary <- function(r, region) {
  # Stream native cells from disk; the much larger paper windows do not fit
  # comfortably in a single in-memory values matrix.
  area <- terra::cellSize(r[[1]], unit = "km", transform = TRUE)
  land <- r[["datamask"]] == 1
  loss <- land & r[["lossyear"]] > 0
  gain <- land & r[["gain"]] == 1
  area_sum <- function(mask) terra::global(terra::ifel(mask, area, 0), "sum", na.rm = TRUE)[1, 1]
  land_area <- area_sum(land)
  loss_area <- area_sum(loss); gain_area <- area_sum(gain)
  dplyr::mutate(region, nrow = terra::nrow(r), ncol = terra::ncol(r),
        resolution_deg = terra::res(r)[1], land_km2 = land_area,
        loss_2001_2025_km2 = loss_area, gain_2000_2012_km2 = gain_area,
        loss_fraction = loss_area / land_area, gain_fraction = gain_area / land_area,
        overlap_km2 = area_sum(loss & gain))
}

draw_hansen_panel <- function(r, region, stats) {
  v <- terra::values(r) |> tibble::as_tibble()
  cover_palette <- grDevices::colorRampPalette(c("#f0f1ec", "#46844b"))(101)
  colours <- v |>
    dplyr::transmute(colour = dplyr::case_when(
      is.na(datamask) | datamask == 0 ~ "white",
      datamask == 2 ~ "#bdd3df",
      lossyear > 0 & gain == 1 ~ "#7900A8",
      lossyear > 0 ~ "#E66101",
      gain == 1 ~ "#008BDC",
      .default = cover_palette[treecover2000 + 1L]
    )) |>
    dplyr::pull(colour)
  # Keep raster pixels as a bitmap rather than expanding them into ggplot tiles.
  pixels <- grDevices::as.raster(matrix(colours, nrow = terra::nrow(r), byrow = TRUE))
  centre_lat <- (region$ymin + region$ymax) / 2
  width <- (terra::xmax(r) - terra::xmin(r)) * cos(centre_lat * pi / 180) / 1000
  height <- (terra::ymax(r) - terra::ymin(r)) * cos(centre_lat * pi / 180) / 1000
  overlap <- stats$overlap_km2 / stats$land_km2
  ggplot2::ggplot() +
    ggplot2::annotation_raster(pixels, 0, width, 0, height, interpolate = FALSE) +
    ggplot2::annotate("rect", xmin = width * .025, xmax = width * .025 + 120,
      ymin = height * .025, ymax = height * .025 + 24, fill = "white") +
    ggplot2::annotate("segment", x = width * .025 + 10, xend = width * .025 + 110,
      y = height * .025 + 7, yend = height * .025 + 7, linewidth = .6) +
    ggplot2::annotate("text", x = width * .025 + 60, y = height * .025 + 17,
      label = "100 km", size = 3) +
    ggplot2::coord_fixed(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
    ggplot2::labs(title = paste(region$id, region$name),
      subtitle = sprintf("%.1f°%s, %.1f°%s | overview of ~30 m source", abs(centre_lat),
        ifelse(centre_lat < 0, "S", "N"), abs((region$xmin + region$xmax) / 2),
        ifelse((region$xmin + region$xmax) < 0, "W", "E")),
      caption = sprintf("Land area: loss only %.1f%% | gain only %.1f%% | both %.1f%%",
        100 * (stats$loss_fraction - overlap), 100 * (stats$gain_fraction - overlap), 100 * overlap)) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = .5),
      plot.subtitle = ggplot2::element_text(hjust = .5),
      plot.caption = ggplot2::element_text(hjust = .5), plot.margin = ggplot2::margin(10, 8, 10, 8))
}

draw_hansen_legend <- function() {
  keys <- tibble::tibble(
    label = c("Loss only (2001–2025)", "Gain only (2000–2012)", "Gain and loss",
      "2000 tree cover: 0%", "100%", "Water", "No data"),
    colour = c("#E66101", "#008BDC", "#7900A8", "#f0f1ec", "#46844b", "#bdd3df", "white"),
    x = c(.4, 1.5, 2.6, .4, 1.5, 2.1, 2.6), y = c(1, 1, 1, 0, 0, 0, 0))
  ggplot2::ggplot(keys, ggplot2::aes(x, y)) +
    ggplot2::geom_point(ggplot2::aes(colour = colour), shape = 15, size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = label), nudge_x = .06, hjust = 0, size = 3) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 3.4), ylim = c(-.3, 1.3), clip = "off") +
    ggplot2::labs(caption = "Hansen/UMD/Google/USGS/NASA | GFC v1.13 | CC BY 4.0\nGain ends in 2012. Different periods: no net change inferred.") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = .5, size = 9),
      plot.margin = ggplot2::margin(0, 10, 8, 10))
}

assemble_hansen_plots <- function(plots, ncol) {
  nrow <- ceiling(length(plots) / ncol)
  panels <- gtable::gtable_matrix("regions",
    grobs = matrix(purrr::map(plots, ggplot2::ggplotGrob), ncol = ncol, byrow = TRUE),
    widths = grid::unit(rep(1, ncol), "null"), heights = grid::unit(rep(1, nrow), "null"))
  panels <- gtable::gtable_add_rows(panels, grid::unit(1.15, "in"))
  gtable::gtable_add_grob(panels, ggplot2::ggplotGrob(draw_hansen_legend()),
    t = nrow + 1, l = 1, r = ncol)
}

save_hansen_plots <- function(rasters, stats, output_dir) {
  plots <- purrr::map(seq_along(rasters), function(i) {
    draw_hansen_panel(rasters[[i]], dplyr::slice(hansen_regions, i), dplyr::slice(stats, i))
  })
  save <- function(plot, stem, width, height) {
    purrr::walk(c("png", "pdf"), function(extension) {
      ggplot2::ggsave(file.path(output_dir, paste0(stem, ".", extension)), plot,
        width = width, height = height, dpi = 300, bg = "white",
        device = if (extension == "pdf") grDevices::cairo_pdf else "png")
    })
  }
  save(assemble_hansen_plots(plots, 2), "hansen_forest_loss_gain_regions", 14, 11)
  purrr::walk2(plots, hansen_regions$slug, function(plot, slug) {
    save(assemble_hansen_plots(list(plot), 1), paste0("hansen_forest_loss_gain_", slug), 10, 7)
  })
}

main_hansen <- function(mode = "all") {
  stopifnot(mode %in% c("all", "plot"))
  data_dir <- here::here("data", "hansen_forest_change_2025", "figure2_native")
  output_dir <- here::here("fig", "hansen_forest_change_2025")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (mode == "plot") {
    required <- tidyr::expand_grid(slug = hansen_regions$slug, layer = c("treecover2000", "lossyear", "gain", "datamask")) |>
      dplyr::transmute(path = paste0(slug, "_", layer, ".tif")) |> dplyr::pull(path)
    stopifnot(all(file.exists(file.path(data_dir, required))))
  }
  terra::terraOptions(progress = 0, memfrac = 0.15)
  if (mode == "all") {
    jobs <- tidyr::expand_grid(i = seq_len(nrow(hansen_regions)), layer = c("treecover2000", "lossyear", "gain", "datamask"))
    cl <- parallel::makePSOCKcluster(3L, outfile = "")
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(script, libraries) {
      .libPaths(libraries)
      source(script, local = .GlobalEnv)
      terra::terraOptions(progress = 0, memfrac = 0.12)
      NULL
    }, here::here("analysis", "plot_hansen_forest_change.R"), .libPaths())
    parallel::parLapplyLB(cl, seq_len(nrow(jobs)), function(j, jobs, data_dir) {
      download_hansen_window(dplyr::slice(hansen_regions, jobs$i[j]), jobs$layer[j], data_dir)
    }, jobs, data_dir)
  }
  message("Validating native grids and source values")
  rasters <- purrr::map(seq_len(nrow(hansen_regions)), function(i) read_hansen_region(dplyr::slice(hansen_regions, i), data_dir))
  stats <- purrr::map(seq_along(rasters), function(i) {
    message("Summing native cell areas: ", hansen_regions$name[i])
    hansen_summary(rasters[[i]], dplyr::slice(hansen_regions, i))
  }) |> purrr::list_rbind()
  readr::write_csv(stats, file.path(data_dir, "regional_summary.csv"))
  # Render bounded-size display rasters. Sampling only affects the plotted
  # overview; all area statistics above use the full native-resolution grids.
  display <- purrr::map(seq_along(rasters), function(i) {
    message("Preparing map overview: ", hansen_regions$name[i])
    path <- file.path(data_dir, paste0(hansen_regions$slug[i], "_display_mercator.tif"))
    if (!file.exists(path)) {
      g <- dplyr::slice(hansen_regions, i)
      b <- terra::project(terra::as.polygons(terra::ext(g$xmin, g$xmax, g$ymin, g$ymax), crs = "EPSG:4326"), "EPSG:3857")
      e <- terra::ext(b)
      template <- terra::rast(e, ncols = 2400, nrows = round(2400 * (e$ymax-e$ymin)/(e$xmax-e$xmin)), crs = "EPSG:3857")
      temporary <- paste0(path, ".partial.tif")
      terra::project(rasters[[i]], template, method = "near", filename = temporary, overwrite = TRUE,
        datatype = "INT1U", NAflag = 255, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
      stopifnot(file.rename(temporary, path))
    }
    r <- terra::rast(path)
    names(r) <- c("treecover2000", "lossyear", "gain", "datamask")
    r
  })
  save_hansen_plots(display, stats, output_dir)
  print(dplyr::select(stats, name, loss_2001_2025_km2, gain_2000_2012_km2))
  message("Saved forest maps in ", output_dir)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  main_hansen(if (length(args)) args[1] else "all")
}
