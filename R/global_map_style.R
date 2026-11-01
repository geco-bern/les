# Shared presentation for the book's global maps, matching landusechange.qmd.
# Source rasters retain their original values, resolution and class palettes.
global_map_borders <- local({
  borders <- NULL
  function() {
    if (is.null(borders)) {
      stopifnot(requireNamespace("rnaturalearthdata", quietly = TRUE))
      borders <<- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
    }
    borders
  }
})

global_map_frame <- function() {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = global_map_borders(), fill = "#eceee9", colour = NA) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(legend.position = "bottom",
      plot.margin = ggplot2::margin(12, 12, 12, 12))
}

# Add after all layers so every map uses the same geographic window.
global_map_extent <- function() {
  ggplot2::coord_sf(crs = sf::st_crs(4326), datum = NA,
    xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE)
}

global_map_outlines <- function() {
  ggplot2::geom_sf(data = global_map_borders(), fill = NA,
    colour = "#7b8279", linewidth = .09)
}

# Embed the native raster as one bitmap, including in PDF output. Avoid
# geom_spatraster's automatic sampling and keep categorical pixels unblended.
global_map_raster <- function(r, colour_function, mask_ocean = FALSE) {
  stopifnot(terra::nlyr(r) == 1L, terra::is.lonlat(r))
  r <- terra::crop(r, terra::ext(-180, 180, -60, 85))
  if (mask_ocean) r <- terra::mask(r, terra::vect(global_map_borders()))
  colours <- colour_function(as.vector(terra::values(r)))
  stopifnot(length(colours) == terra::ncell(r))
  colours[is.na(colours)] <- "transparent"
  pixels <- grDevices::as.raster(matrix(colours, nrow = terra::nrow(r), byrow = TRUE))
  ggplot2::annotation_raster(pixels, terra::xmin(r), terra::xmax(r),
    terra::ymin(r), terra::ymax(r), interpolate = FALSE)
}

save_global_map <- function(plot, stem, height = 6.5) {
  dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
  for (extension in c("png", "pdf")) {
    ggplot2::ggsave(paste0(stem, ".", extension), plot,
      width = 14, height = height, dpi = 300, bg = "white",
      device = if (extension == "pdf") grDevices::cairo_pdf else "png")
  }
  invisible(plot)
}

# Retain the existing quantitative colour bins and their boundaries. The key
# moves below the map; unequal bins (e.g. biomass) retain proportional widths.
global_map_continuous <- function(r, colours, breaks, legend_title, legend_breaks = breaks) {
  stopifnot(length(colours) == length(breaks) - 1L)
  bin <- function(x) cut(x, breaks, include.lowest = TRUE, labels = FALSE)
  key <- data.frame(x = 0, y = 0, value = head(breaks, -1) + diff(breaks) / 2)
  global_map_frame() +
    global_map_raster(r, function(x) colours[bin(x)], mask_ocean = TRUE) +
    global_map_outlines() +
    ggplot2::geom_point(data = key, ggplot2::aes(x, y, fill = value),
      shape = 22, alpha = 0, show.legend = TRUE) +
    ggplot2::scale_fill_stepsn(colours = colours, breaks = breaks,
      labels = function(x) ifelse(x %in% legend_breaks, format(x, trim = TRUE), ""),
      limits = range(breaks), values = scales::rescale(key$value, from = range(breaks)),
      name = legend_title, show.limits = TRUE,
      guide = ggplot2::guide_coloursteps(even.steps = FALSE,
        theme = ggplot2::theme(legend.key.width = grid::unit(120, "mm"),
          legend.key.height = grid::unit(4, "mm")))) +
    global_map_extent()
}
