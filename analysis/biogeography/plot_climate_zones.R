# Beck et al. (2018), present-day (1980–2016) Koppen–Geiger climate zones.
# Run from the repository root:
# Rscript analysis/biogeography/plot_climate_zones.R [all|plot]
# Keep the project .Rprofile enabled so renv loads the locked dependencies.
# 'all' downloads/caches the published data; 'plot' uses only local files.
# See README.md in this directory for provenance and plotting conventions.

beck_source <- list(
  article_doi = "10.1038/sdata.2018.214",
  dataset_doi = "10.6084/m9.figshare.6396959.v2",
  url = "https://ndownloader.figshare.com/files/12407516",
  archive = "Beck_KG_V1.zip",
  md5 = "69594689d3cdd8323a0f74ce658125a1",
  raster = "Beck_KG_V1_present_0p083.tif",
  legend = "legend.txt"
)

download_beck <- function(data_dir) {
  archive <- file.path(data_dir, beck_source$archive)
  if (!file.exists(archive)) {
    partial <- paste0(archive, ".part")
    on.exit(unlink(partial), add = TRUE)
    message("Downloading Beck et al. (2018) from ", beck_source$url)
    utils::download.file(beck_source$url, partial, method = "libcurl", mode = "wb")
    if (unname(tools::md5sum(partial)) != beck_source$md5) {
      stop("Downloaded archive does not match the published Figshare checksum.")
    }
    if (!file.rename(partial, archive)) stop("Could not save ", archive)
  }
  if (unname(tools::md5sum(archive)) != beck_source$md5) {
    stop("Cached archive checksum mismatch: ", archive)
  }
  # Extract only the authors' majority-aggregated present-day classification
  # and its legend, not the future scenario or confidence rasters.
  members <- c(beck_source$raster, beck_source$legend)
  if (!all(members %in% utils::unzip(archive, list = TRUE)$Name)) {
    stop("The expected present-day raster and legend are missing from the archive.")
  }
  utils::unzip(archive, files = members, exdir = data_dir, overwrite = TRUE)
  invisible(archive)
}

read_beck_legend <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[grepl("^\\s*[0-9]+:", lines)]
  pattern <- paste0(
    "^\\s*([0-9]+):\\s+(\\S+)\\s+(.+?)\\s+",
    "\\[([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\]\\s*$"
  )
  entries <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  if (length(entries) != 30L || any(lengths(entries) != 7L)) {
    stop("Unexpected format in Beck's legend: ", path)
  }
  entries <- do.call(rbind, entries)
  legend <- data.frame(
    id = as.integer(entries[, 2]), code = entries[, 3],
    description = trimws(entries[, 4]),
    colour = grDevices::rgb(as.integer(entries[, 5]), as.integer(entries[, 6]),
      as.integer(entries[, 7]), maxColorValue = 255)
  )
  stopifnot(identical(legend$id, 1:30), !anyDuplicated(legend$code))
  legend
}

read_beck_raster <- function(path) {
  r <- terra::rast(path)
  if (terra::nlyr(r) != 1L || terra::nrow(r) != 2160L ||
      terra::ncol(r) != 4320L || !terra::same.crs(r, "EPSG:4326") ||
      any(abs(terra::res(r) - 1 / 12) > 1e-8) ||
      any(abs(as.vector(terra::ext(r)) - c(-180, 180, -90, 90)) > 1e-8)) {
    stop("Unexpected geometry for Beck's global 0.0833-degree raster.")
  }
  frequencies <- terra::freq(r)
  if (!setequal(frequencies$value, 0:30)) {
    stop("Expected background code 0 and all 30 climate classes.")
  }
  # Zero is background, not a climate class; it is not flagged as NoData in
  # the GeoTIFF. Retain it in the source and render it white below.
  r
}

plot_beck <- function(r, legend, output_dir) {
  # Match the global maps in plot_luh2_agriculture.R: WGS84 lon/lat,
  # 180 W–180 E, 60 S–85 N, no axes/graticules, thin grey country outlines.
  r <- terra::crop(r, terra::ext(-180, 180, -60, 85))
  codes <- as.vector(terra::values(r))
  palette <- c("white", legend$colour)
  pixels <- palette[codes + 1L]
  pixels[is.na(pixels)] <- "white"
  pixels <- grDevices::as.raster(matrix(pixels, nrow = terra::nrow(r), byrow = TRUE))
  borders <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

  # Embed the categorical grid as one bitmap; country boundaries and the
  # legend remain vector graphics in the PDF. No averaging or interpolation.
  map <- ggplot2::ggplot() +
    ggplot2::annotation_raster(pixels, terra::xmin(r), terra::xmax(r),
      terra::ymin(r), terra::ymax(r), interpolate = FALSE) +
    ggplot2::geom_sf(data = borders, fill = NA, colour = "#7b8279", linewidth = .09) +
    ggplot2::coord_sf(crs = sf::st_crs(4326), datum = NA,
      xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(12, 12, 0, 12))

  # Keep related classes together in nine columns, as in the original figure:
  # A, B, Cs, Cw, Cf, Ds, Dw, Df, E. All 30 classes are shown, even rare ones.
  group_sizes <- c(3, 4, 3, 3, 3, 4, 4, 4, 2)
  legend$column <- rep(seq_along(group_sizes), group_sizes)
  legend$row <- unlist(lapply(group_sizes, seq_len), use.names = FALSE)
  key <- ggplot2::ggplot(legend, ggplot2::aes(column, -row)) +
    ggplot2::geom_point(ggplot2::aes(fill = colour), shape = 22, size = 4.2,
      stroke = .2, colour = "#7b8279") +
    ggplot2::geom_text(ggplot2::aes(label = code), nudge_x = .17,
      hjust = 0, size = 11 / ggplot2::.pt) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(.5, 9.75), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-4.5, -.5), expand = c(0, 0)) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 75, 8, 75))
  figure <- cowplot::plot_grid(map, key, ncol = 1, rel_heights = c(1, .21))
  for (extension in c("png", "pdf")) {
    ggplot2::ggsave(file.path(output_dir, paste0("koppen_geiger_1980_2016.", extension)),
      figure, width = 14, height = 7.2, dpi = 300, bg = "white",
      device = if (extension == "pdf") grDevices::cairo_pdf else "png")
  }
  invisible(figure)
}

main <- function(mode = "all") {
  if (!mode %in% c("all", "plot")) stop("Mode must be 'all' or 'plot'.")
  packages <- c("terra", "ggplot2", "sf", "rnaturalearth", "cowplot", "here")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "))
  here::i_am("analysis/biogeography/plot_climate_zones.R")
  options(timeout = max(600, getOption("timeout")))
  data_dir <- here::here("data", "beck_2018")
  output_dir <- here::here("book", "images", "biogeography")
  for (directory in c(data_dir, output_dir)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (mode == "all") download_beck(data_dir)
  paths <- file.path(data_dir, c(beck_source$raster, beck_source$legend))
  if (!all(file.exists(paths))) stop("Run in 'all' mode first to download the data.")
  r <- read_beck_raster(paths[1])
  legend <- read_beck_legend(paths[2])
  utils::write.csv(legend, file.path(data_dir, "classes.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    article_doi = beck_source$article_doi, dataset_doi = beck_source$dataset_doi,
    download_url = beck_source$url, archive_md5 = beck_source$md5,
    raster = beck_source$raster, raster_md5 = unname(tools::md5sum(paths[1])),
    legend_md5 = unname(tools::md5sum(paths[2])), period = "1980-2016",
    resolution_degrees = terra::res(r)[1], license = "CC BY 4.0"
  ), file.path(data_dir, "sources.csv"), row.names = FALSE)
  plot_beck(r, legend, output_dir)
  message("Saved climate-zone maps in ", output_dir)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  main(if (length(args)) args[1] else "all")
}
