# HYDE 3.5 baseline: cropland and pasture at 0, 1850 and 2025 CE.
# Run from the repository root: Rscript --vanilla analysis/plot_hyde_land_use.R [all|plot]
# Dependencies: dplyr, tidyr, purrr, readr, tibble, terra, ggplot2, here, sf, rnaturalearth, cowplot.
# Latest published release checked 2026-09-24:
# https://landuse.sites.uu.nl/datasets/
# https://doi.org/10.24416/UU01-F45D44 (CC BY-NC 4.0)
# Pin the release for reproducibility. 2023-2025 are trend extrapolations.
# Pasture means HYDE's intensive pasture, NOT pasture plus rangeland.
# HYDE labels its year-zero reconstruction 0AD; retain that convention as 0 CE.

hyde_base_url <- paste0(
  "https://geo.public.data.uu.nl/vault-hyde/",
  "hyde35_c9_apr2025%5B1749214444%5D/original/gbc2025_7apr_base/zip/"
)

archive_members <- function(path) {
  members <- tryCatch(suppressWarnings(utils::unzip(path, list = TRUE)),
                      error = function(e) NULL)
  if (is.null(members) || !nrow(members)) {
    stop("Not a valid ZIP archive: ", path,
         "\nThe HYDE server may have returned a browser-verification page.\n",
         "Download the archive in a browser and place it at this path, then rerun.",
         call. = FALSE)
  }
  members$Name
}

download_hyde <- function(year, cache_dir) {
  name <- paste0(year, "AD_lu.zip")
  path <- file.path(cache_dir, name)
  if (!file.exists(path)) {
    url <- paste0(hyde_base_url, name)
    message("Downloading ", url)
    partial <- paste0(path, ".part")
    on.exit(unlink(partial), add = TRUE)
    utils::download.file(url, partial, mode = "wb", method = "libcurl")
    tryCatch(archive_members(partial), error = function(e) {
      stop("Download did not return a usable archive.\nOpen: ", url,
           "\nSave as: ", path, "\nThen rerun the script.\n", conditionMessage(e),
           call. = FALSE)
    })
    if (!file.rename(partial, path)) stop("Cannot save ", path)
  }
  archive_members(path)
  path
}

read_hyde_area <- function(archive, variable, year, extract_dir) {
  members <- archive_members(archive)
  # Select total cropland/pasture only, never irrigated crops or rangeland.
  pattern <- paste0("^", variable, "[_-]?", year, "AD\\.asc$")
  member <- members[grepl(pattern, basename(members), ignore.case = TRUE)]
  if (length(member) != 1L) {
    stop("Expected exactly one ", pattern, " in ", archive,
         "; found ", length(member), ". Archive contents:\n",
         paste(members, collapse = "\n"), call. = FALSE)
  }
  if (grepl("(^/|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))", member)) {
    stop("Unsafe archive member: ", member)
  }
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(extract_dir, member)
  if (!file.exists(target)) utils::unzip(archive, files = member, exdir = extract_dir)
  area <- terra::rast(target)
  terra::crs(area) <- "EPSG:4326"
  if (terra::nlyr(area) != 1 ||
      any(abs(as.vector(terra::ext(area)) - c(-180, 180, -90, 90)) > 0.001) ||
      any(abs(terra::res(area) - 1 / 12) > 1e-6)) {
    stop("Unexpected HYDE grid geometry: ", target)
  }
  # ASCII land-use layers contain km2 per cell; GDAL reads NODATA as NA.
  limits <- terra::global(area, c("min", "max"), na.rm = TRUE)
  if (any(!is.finite(as.matrix(limits))) || limits$min < 0) {
    stop("Invalid land-use areas in ", target)
  }
  area
}

prepare_hyde_panel <- function(area, year, land_use, resolution = 0.1) {
  total <- terra::global(area, "sum", na.rm = TRUE)[1, 1]
  # Match the satellite overview grid, anchored at (-180, 90). Five arc-minutes
  # does not divide 0.1 degrees: redistribute extensive km2 values using GDAL's
  # overlap-weighted sum, not an integer aggregate or interpolation of areas.
  # Denominator is full grid-cell area (including coastal water), not land area.
  template <- terra::rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
                          resolution = resolution, crs = "EPSG:4326")
  coarse_area <- terra::resample(area, template, method = "sum")
  remapped_total <- terra::global(coarse_area, "sum", na.rm = TRUE)[1, 1]
  if (abs(remapped_total - total) > max(1e-6, total * 1e-6)) {
    stop("Area conservation check failed during HYDE resampling.")
  }
  coarse_cells <- terra::cellSize(template, unit = "km", mask = FALSE)
  percent <- 100 * coarse_area / coarse_cells
  names(percent) <- "percent"
  panel <- terra::as.data.frame(percent, xy = TRUE, na.rm = TRUE)
  if (any(panel$percent > 101)) stop("Land-use area exceeds grid-cell area.")
  panel <- tibble::as_tibble(panel) |>
    dplyr::mutate(year = paste(.env$year, "CE"), land_use = .env$land_use)
  names(coarse_area) <- "area_km2"
  list(map = panel, raster = c(percent, coarse_area), total = tibble::tibble(
    year_ce = year, land_use = land_use, area_km2 = total,
    area_million_km2 = total / 1e6, remapped_area_km2 = remapped_total
  ))
}

main <- function(mode = "all") {
  stopifnot(mode %in% c("all", "plot"))
  packages <- c("dplyr", "tidyr", "purrr", "readr", "tibble", "terra", "ggplot2", "here", "sf", "rnaturalearth", "cowplot")
  missing <- purrr::discard(packages, requireNamespace, quietly = TRUE)
  if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "))
  options(timeout = max(1800, getOption("timeout")))
  cache_dir <- here::here("data", "hyde_3.5")
  output_dir <- here::here("book", "images")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  years <- c(0L, 1850L, 2025L)
  if (mode == "plot") {
    jobs <- tidyr::expand_grid(year = years, variable = c("cropland", "pasture"))
    panels <- purrr::pmap(jobs, function(year, variable) {
      path <- file.path(cache_dir, paste0(variable, "_", year, "_0.1deg.tif"))
      if (!file.exists(path)) stop("Run 'all' first to cache HYDE: ", path)
      terra::as.data.frame(terra::rast(path)[["percent"]], xy = TRUE, na.rm = TRUE) |>
        tibble::as_tibble() |>
        dplyr::mutate(year = paste(.env$year, "CE"),
          land_use = if (variable == "cropland") "Cropland" else "Pasture")
    })
    plot_hyde(purrr::list_rbind(panels), output_dir)
    message("Redrawn HYDE figures from cached rasters in ", output_dir)
    return(invisible(NULL))
  }
  sources <- tibble::tibble(year_ce = years) |>
    dplyr::mutate(archive = purrr::map_chr(year_ce, download_hyde, cache_dir = cache_dir))
  manifest <- sources |>
    dplyr::transmute(version = "3.5", scenario = "baseline", year_ce,
      url = paste0(hyde_base_url, basename(archive)),
      md5 = purrr::map_chr(archive, function(path) unname(tools::md5sum(path))))
  jobs <- tidyr::expand_grid(year = years, variable = c("cropland", "pasture")) |>
    dplyr::left_join(sources, by = c("year" = "year_ce"))
  panels <- purrr::pmap(jobs, function(year, variable, archive) {
    area <- read_hyde_area(archive, variable, year, file.path(cache_dir, as.character(year)))
    label <- if (variable == "cropland") "Cropland" else "Pasture"
    panel <- prepare_hyde_panel(area, year, label)
    terra::writeRaster(panel$raster,
      file.path(cache_dir, paste0(variable, "_", year, "_0.1deg.tif")),
      overwrite = TRUE, datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    panel
  })
  maps <- purrr::map(panels, "map") |>
    purrr::list_rbind() |>
    dplyr::mutate(year = factor(year, levels = paste(years, "CE")),
      land_use = factor(land_use, levels = c("Cropland", "Pasture")))
  totals <- purrr::map(panels, "total") |> purrr::list_rbind()
  readr::write_csv(totals, file.path(cache_dir, "hyde_land_use_totals.csv"))
  readr::write_csv(manifest, file.path(cache_dir, "sources.csv"))
  plot_hyde(maps, output_dir)
  print(totals)
  message("Plots saved in ", output_dir, "; global totals and source manifest in ", cache_dir)
}

plot_hyde <- function(maps, output_dir) {
  # Match the global Sentinel-2 overview's palette and 0–100% scale exactly.
  colours <- grDevices::colorRampPalette(
    c("#eceee9", "#fff0b7", "#e7af38", "#aa6017", "#542b0e")
  )(101)
  borders <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
  # Row-wise order: cropland, pasture for 0, 1850 and 2025 CE. The book caption
  # supplies these panel keys and the source/methods, keeping the maps untitled.
  panel_keys <- tidyr::expand_grid(year = paste(c(0, 1850, 2025), "CE"),
    land_use = c("Cropland", "Pasture"))
  panels <- purrr::pmap(panel_keys, function(year, land_use) {
    pixels <- dplyr::filter(maps, .data$year == .env$year,
      .data$land_use == .env$land_use)
    ggplot2::ggplot(pixels, ggplot2::aes(x, y, fill = percent)) +
      ggplot2::geom_raster() +
      ggplot2::geom_sf(data = borders, inherit.aes = FALSE, fill = NA,
        colour = "#7b8279", linewidth = 0.09) +
      ggplot2::coord_sf(crs = sf::st_crs(4326), datum = NA,
        xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
      ggplot2::scale_fill_gradientn(colours = colours, limits = c(0, 100),
        oob = scales::squish, name = "Grid-cell area (%)", na.value = "white") +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(8, 4, 4, 4))
  })
  legend <- cowplot::get_legend(panels[[1]])
  panel_grid <- cowplot::plot_grid(
    plotlist = purrr::map(panels, function(p) p + ggplot2::theme(legend.position = "none")),
    ncol = 2, labels = letters[1:6], label_size = 14,
    label_x = .01, label_y = .99, hjust = 0, vjust = 1)
  p <- cowplot::plot_grid(panel_grid, legend, ncol = 1, rel_heights = c(1, .065))
  purrr::walk(c("png", "pdf"), function(extension) {
    ggplot2::ggsave(file.path(output_dir, paste0("hyde_land_use.", extension)),
      p, width = 12, height = 8, dpi = 300, bg = "white",
      device = if (extension == "pdf") grDevices::cairo_pdf else "png")
  })
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  main(if (length(args)) args[1] else "all")
}
