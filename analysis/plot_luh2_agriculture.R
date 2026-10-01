# Global irrigation, crop functional types and grazing land fractions, LUH2 v2h.
# Run from repository root: Rscript analysis/plot_luh2_agriculture.R [all|plot]
# Dependencies: dplyr, tidyr, purrr, readr, tibble, ggplot2, ncdf4, terra,
# here, sf, rnaturalearth, rnaturalearthdata, jsonlite, curl, cowplot.
# Historical 2015, native 0.25 degrees; see luh2_agriculture.md for definitions.
luh_year <- 2015L
luh_base <- "https://luh.umd.edu/LUH2/LUH2_v2h/"
luh_crops <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")
luh_states <- c(luh_crops, "pastr", "range", "primf", "primn", "secdf", "secdn", "urban")
luh_labels <- c("C3 annuals (e.g. wheat, rice)", "C4 annuals (e.g. maize, millet)",
  "C3 perennials (e.g. tree crops)", "C4 perennials (e.g. sugarcane)",
  "C3 nitrogen-fixers (e.g. soybean, pulses)")
luh_threshold <- 0.01 # Only classify crop groups where at least 1% is cropland.

download_luh <- function(filename, data_dir) {
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) {
    temporary <- paste0(path, ".part")
    # Resume interrupted HTTP downloads; abort stalled connections after a minute.
    purrr::walk(seq_len(4), function(attempt) {
      if (file.exists(path)) return(invisible(NULL))
      offset <- if (file.exists(temporary)) file.info(temporary)$size else 0
      message("Downloading ", filename, " from byte ", offset, " (attempt ", attempt, ")")
      handle <- curl::new_handle(resume_from_large = offset, connecttimeout = 30,
        timeout = 7200, low_speed_limit = 1024, low_speed_time = 60, failonerror = TRUE)
      tryCatch({
        # Stream directly: curl_download replaces its destination via a temporary
        # file, so its mode argument cannot safely append a resumed transfer.
        connection <- file(temporary, open = if (offset > 0) "ab" else "wb")
        tryCatch(curl::curl_fetch_stream(paste0(luh_base, filename),
          function(bytes) writeBin(bytes, connection), handle = handle),
          finally = close(connection))
        nc <- ncdf4::nc_open(temporary)
        ncdf4::nc_close(nc)
        stopifnot(file.rename(temporary, path))
      }, error = function(e) {
        if (attempt == 4) stop(conditionMessage(e), call. = FALSE)
        message("Transfer interrupted; retrying from cached bytes: ", conditionMessage(e))
      })
    })
  }
  path
}

read_luh_layer <- function(nc, variable, year = NULL) {
  v <- nc$var[[variable]]
  stopifnot(!is.null(v))
  dims <- purrr::map_chr(v$dim, "name")
  stopifnot(identical(dims[1:2], c("lon", "lat")))
  start <- rep(1L, length(dims)); count <- v$varsize
  if ("time" %in% dims) {
    time <- nc$dim$time
    # This release encodes annual states as years since 850-01-01.
    stopifnot(grepl("^years since 0850|^years since 850", time$units))
    index <- which(abs(time$vals + 850 - year) < 1e-6)
    stopifnot(length(index) == 1L)
    start[match("time", dims)] <- index
    count[match("time", dims)] <- 1L
  }
  values <- ncdf4::ncvar_get(nc, variable, start = start, count = count)
  lon <- nc$dim$lon$vals; lat <- nc$dim$lat$vals
  stopifnot(length(lon) == 1440, length(lat) == 720,
    all(abs(diff(sort(lon)) - .25) < 1e-8), all(abs(diff(sort(lat)) - .25) < 1e-8),
    abs(min(lon) + 179.875) < 1e-8, abs(max(lat) - 89.875) < 1e-8)
  values <- values[order(lon), order(lat, decreasing = TRUE)]
  values[abs(values) > 1e15] <- NA_real_
  r <- terra::rast(ncols = 1440, nrows = 720, xmin = -180, xmax = 180,
    ymin = -90, ymax = 90, crs = "EPSG:4326")
  terra::values(r) <- as.vector(values) # Longitude varies fastest in the NetCDF.
  names(r) <- variable
  r
}

prepare_luh <- function(data_dir) {
  cache <- file.path(data_dir, "luh2_2015_inputs.tif")
  if (file.exists(cache)) return(terra::rast(cache))
  manifest <- list()
  sources <- tibble::tibble(file = c("states.nc", "management.nc", "staticData_quarterdeg.nc"),
    variables = list(luh_states, paste0("irrig_", luh_crops), c("icwtr", "carea")))
  layers <- purrr::map2(sources$file, sources$variables, function(file, variables) {
    path <- download_luh(file, data_dir)
    nc <- ncdf4::nc_open(path)
    on.exit(ncdf4::nc_close(nc))
    manifest[[file]] <<- list(url = paste0(luh_base, file), bytes = file.info(path)$size,
      global_attributes = ncdf4::ncatt_get(nc, 0),
      variables = purrr::map(variables, function(v) ncdf4::ncatt_get(nc, v)) |>
        purrr::set_names(variables))
    purrr::map(variables, function(v) read_luh_layer(nc, v, luh_year)) |> purrr::reduce(c)
  }) |> purrr::reduce(c)
  terra::writeRaster(layers, cache, overwrite = TRUE, datatype = "FLT8S",
    gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  jsonlite::write_json(list(year = luh_year, release = "LUH2 v2h", sources = manifest),
    file.path(data_dir, "source_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  layers
}

derive_luh <- function(r, data_dir) {
  d <- terra::as.data.frame(r, xy = TRUE, na.rm = FALSE) |> tibble::as_tibble()
  fractions <- d |> dplyr::select(dplyr::all_of(c(luh_states, paste0("irrig_", luh_crops), "icwtr")))
  stopifnot(purrr::every(fractions, function(x) all(is.na(x) | (x >= -1e-6 & x <= 1 + 1e-6))))
  d <- d |> dplyr::mutate(state_total = rowSums(dplyr::pick(dplyr::all_of(luh_states))),
    crop_fraction = rowSums(dplyr::pick(dplyr::all_of(luh_crops))),
    grazing_fraction = pastr + range)
  # State fractions are fractions of the whole cell: explicitly check closure
  # against the static ice/water fraction, rather than silently changing denominator.
  land <- is.finite(d$state_total) & is.finite(d$icwtr) & d$icwtr < 1
  stopifnot(any(land), max(abs(d$state_total[land] + d$icwtr[land] - 1)) < 1e-4)
  irrigated <- purrr::map(luh_crops, function(v) {
    crop <- d[[v]]; management <- d[[paste0("irrig_", v)]]
    stopifnot(!any(crop > 0 & is.na(management), na.rm = TRUE))
    dplyr::if_else(crop == 0, 0, crop * management)
  }) |> purrr::reduce(`+`)
  crop_matrix <- as.matrix(dplyr::select(d, dplyr::all_of(luh_crops)))
  crop_matrix[is.na(crop_matrix)] <- 0
  dominant <- max.col(crop_matrix, ties.method = "first")
  d <- d |> dplyr::mutate(irrigated_fraction = irrigated,
    irrigation_percent = dplyr::if_else(icwtr < 1, 100 * irrigated_fraction, NA_real_),
    crop_type = dplyr::if_else(crop_fraction >= luh_threshold, dominant, NA_integer_))
  stopifnot(all(is.na(irrigated) | irrigated <= d$crop_fraction + 1e-6))
  summary <- tibble::tibble(variable = c(luh_crops, "irrigated_fraction", "pastr", "range")) |>
    dplyr::mutate(area_km2 = purrr::map_dbl(variable, function(v) sum(d[[v]] * d$carea, na.rm = TRUE)),
      year = luh_year)
  readr::write_csv(summary, file.path(data_dir, "global_areas.csv"))
  output <- r[[1:5]]
  names(output) <- c("irrigated_fraction", "crop_type", "pastr", "range", "grazing_fraction")
  terra::values(output) <- as.matrix(dplyr::select(d, dplyr::all_of(names(output))))
  terra::writeRaster(output, file.path(data_dir, "agriculture_2015_0.25deg.tif"), overwrite = TRUE,
    datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  d
}

plot_luh <- function(d, output_dir) {
  borders <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
  frame <- function(title, subtitle, caption) {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = borders, fill = "#eceee9", colour = NA) +
      ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL,
        caption = paste0(caption, "\nSource: LUH2 v2h / HYDE 3.2 | Hurtt et al. (2020), doi:10.5194/gmd-13-5425-2020")) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(face = "bold", hjust = .5),
        plot.subtitle = ggplot2::element_text(hjust = .5, size = 10),
        plot.caption = ggplot2::element_text(hjust = 0, size = 9),
        plot.margin = ggplot2::margin(12, 12, 12, 12))
  }
  outlines <- ggplot2::geom_sf(data = borders, fill = NA, colour = "#7b8279", linewidth = .09)
  extent <- function() ggplot2::coord_sf(crs = sf::st_crs(4326), datum = NA,
    xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE)
  brown <- grDevices::colorRampPalette(c("#eceee9", "#fff0b7", "#e7af38", "#aa6017", "#542b0e"))(101)
  irrigation <- frame("Global irrigated cropland | 2015", "LUH2 historical reconstruction | native 0.25-degree grid",
    "Irrigated crop area as a percentage of the full grid cell; crop areas weighted by their irrigated fractions.") +
    ggplot2::geom_raster(data = d, ggplot2::aes(x, y, fill = irrigation_percent)) + outlines +
    ggplot2::scale_fill_gradientn(colours = brown, limits = c(0, 100), na.value = "transparent",
      oob = scales::squish, name = "Irrigated grid-cell area (%)", breaks = c(0, 25, 50, 75, 100)) + extent()
  crops <- frame("Dominant crop functional type | 2015", "LUH2 historical reconstruction | native 0.25-degree grid",
    "Largest crop group in cells with at least 1% cropland. Broad functional groups, not individual crop species.") +
    ggplot2::geom_raster(data = d, ggplot2::aes(x, y, fill = factor(crop_type, levels = 1:5)), na.rm = TRUE) + outlines +
    ggplot2::scale_fill_manual(values = c("#d9a441", "#914b16", "#168b80", "#7954a3", "#c95372"),
      labels = luh_labels, na.value = "transparent", na.translate = FALSE, drop = FALSE, name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) + extent()
  # Plot the two supplied fractional-area variables directly, without
  # dominance thresholds or a synthetic mixed class. Mask only ice/water cells.
  grazing_maps <- purrr::map2(c("pastr", "range"), c("Managed pasture", "Rangeland"),
    function(variable, label) {
      pixels <- d |> dplyr::mutate(fraction = dplyr::if_else(icwtr < 1,
        .data[[variable]], NA_real_))
      frame(paste0(label, " | 2015"),
        paste0("LUH2 variable: ", variable, " | native 0.25-degree grid"),
        "Supplied fractional area coverage of the full grid cell; all positive fractions retained.") +
        ggplot2::geom_raster(data = pixels, ggplot2::aes(x, y, fill = fraction), na.rm = TRUE) +
        outlines + ggplot2::scale_fill_gradientn(colours = brown, limits = c(0, 1),
          breaks = c(0, .25, .5, .75, 1), na.value = "transparent",
          oob = scales::squish, name = "Grid-cell fraction") + extent()
    }) |> purrr::set_names(c("managed_pasture", "rangeland"))
  grazing <- cowplot::plot_grid(plotlist = grazing_maps, ncol = 1)
  # Retain the existing filename so documents referencing it get the new figure.
  plots <- c(list(irrigated_cropland = irrigation, dominant_crop_types = crops,
    grazing_management_proxy = grazing), grazing_maps)
  purrr::iwalk(plots, function(plot, stem) {
    purrr::walk(c("png", "pdf"), function(extension) {
      ggplot2::ggsave(file.path(output_dir, paste0(stem, "_2015.", extension)), plot,
        width = 14, height = if (stem == "grazing_management_proxy") 15 else 7.5, dpi = 300, bg = "white",
        device = if (extension == "pdf") grDevices::cairo_pdf else "png")
    })
  })
}

main_luh <- function(mode = "all") {
  stopifnot(mode %in% c("all", "plot"))
  data_dir <- here::here("data", "luh2_agriculture_2015")
  output_dir <- here::here("fig", "luh2_agriculture_2015")
  purrr::walk(c(data_dir, output_dir), dir.create, recursive = TRUE, showWarnings = FALSE)
  cache <- file.path(data_dir, "luh2_2015_inputs.tif")
  if (mode == "plot" && !file.exists(cache)) stop("Run 'all' first to cache the 2015 layers.")
  r <- prepare_luh(data_dir)
  d <- derive_luh(r, data_dir)
  plot_luh(d, output_dir)
  message("Saved LUH2 agriculture maps in ", output_dir)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  main_luh(if (length(args)) args[1] else "all")
}
