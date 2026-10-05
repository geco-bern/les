# Re-layout the three reproduced LUC composites with cowplot.
# Run from the repository root: Rscript --vanilla analysis/plot_luc_published_panels.R
# Dependencies: ggplot2, cowplot, terra. The original raster assets are preserved.
# This script changes composition and panel letters only; it does not reconstruct
# the studies' numerical data. Published map colours, scales and annotations are
# retained. Pixel crops below refer to the checked-in source image dimensions.

read_image <- function(name) {
  raster <- suppressWarnings(terra::rast(file.path("book", "images", name)))
  values <- terra::values(raster, mat = TRUE)
  img <- array(1, dim = c(terra::nrow(raster), terra::ncol(raster), terra::nlyr(raster)))
  for (band in seq_len(terra::nlyr(raster))) {
    img[, , band] <- matrix(values[, band] / 255, nrow = terra::nrow(raster),
      ncol = terra::ncol(raster), byrow = TRUE)
  }
  stopifnot(length(dim(img)) == 3L, dim(img)[3] %in% c(3L, 4L))
  img
}

# Coordinates use top-left origin and half-open pixel intervals, like image crops.
crop_image <- function(img, left, top, right, bottom) {
  stopifnot(left >= 0, top >= 0, right <= dim(img)[2], bottom <= dim(img)[1])
  img[seq.int(top + 1, bottom), seq.int(left + 1, right), , drop = FALSE]
}

# Only suppress the old dark/grey letter pixels in verified blank annotation
# locations. Coloured map pixels are not modified, nor are coastlines or axes.
remove_letter <- function(img, left, top, right, bottom) {
  part <- crop_image(img, left, top, right, bottom)
  channels <- part[, , 1:3, drop = FALSE]
  lo <- apply(channels, c(1, 2), min)
  hi <- apply(channels, c(1, 2), max)
  glyph <- hi < .97 & (hi - lo) < .08
  for (k in seq_len(dim(part)[3])) part[, , k][glyph] <- 1
  img[seq.int(top + 1, bottom), seq.int(left + 1, right), ] <- part
  img
}

image_plot <- function(img, margin = 10) {
  width <- dim(img)[2]; height <- dim(img)[1]
  ggplot2::ggplot() +
    ggplot2::annotation_raster(grDevices::as.raster(img), 0, width, 0, height,
      interpolate = FALSE) +
    ggplot2::coord_fixed(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(margin, 3, 3, 3))
}

label_grid <- function(..., labels, ncol = 2, rel_widths = 1, rel_heights = 1) {
  cowplot::plot_grid(..., ncol = ncol, labels = labels, label_size = 14,
    label_fontface = "bold", label_x = .01, label_y = .99,
    hjust = 0, vjust = 1, rel_widths = rel_widths, rel_heights = rel_heights)
}

save_composite <- function(plot, stem, width, height) {
  for (ext in c("png", "pdf")) {
    ggplot2::ggsave(file.path("book", "images", paste0(stem, "_cowplot.", ext)),
      plot, width = width, height = height, dpi = 300, bg = "white",
      device = if (ext == "pdf") grDevices::cairo_pdf else "png")
  }
}

# Erb et al. (2018): a/b biomass maps share the green stock legend;
# c is the land-cover-conversion impact with its own orange/red legend.
erb <- read_image("luc_pnv_erb18nat.png")
stopifnot(identical(dim(erb)[1:2], c(3290L, 6285L)))
erb <- remove_letter(erb, 0, 0, 155, 155)
erb <- remove_letter(erb, 3100, 0, 3310, 160)
erb <- remove_letter(erb, 0, 1630, 160, 1840)
potential <- image_plot(crop_image(erb, 0, 0, 3140, 1645))
actual <- image_plot(crop_image(erb, 3140, 0, 6285, 1645))
conversion <- image_plot(crop_image(erb, 0, 1645, 3300, 3290))
stock_legend <- image_plot(crop_image(erb, 5580, 1780, 6100, 3260), margin = 0)
legend_space <- cowplot::ggdraw() + cowplot::draw_plot(stock_legend,
  x = .32, y = .05, width = .36, height = .9)
erb_plot <- label_grid(potential, actual, conversion, legend_space,
  labels = c("a", "b", "c", ""), ncol = 2)
save_composite(erb_plot, "luc_pnv_erb18nat", 12, 6.8)
rm(erb); invisible(gc())

# Author's direct/indirect carbon-effect schematic: two upper panels and one
# full-width lower panel. All explanatory labels and arrows remain unchanged.
effects <- read_image("direct_indirect_luc_emissions.png")
stopifnot(identical(dim(effects)[1:2], c(1743L, 3331L)))
effects <- remove_letter(effects, 15, 15, 115, 130)
effects <- remove_letter(effects, 1670, 15, 1770, 130)
effects <- remove_letter(effects, 15, 880, 115, 1000)
primary <- image_plot(crop_image(effects, 0, 0, 1650, 825))
replaced <- image_plot(crop_image(effects, 1650, 0, 3331, 825))
feedback <- image_plot(crop_image(effects, 0, 825, 3331, 1743))
upper <- label_grid(primary, replaced, labels = c("a", "b"), ncol = 2)
lower <- label_grid(feedback, labels = "c", ncol = 1)
effects_plot <- cowplot::plot_grid(upper, lower, ncol = 1, rel_heights = c(1, 1.1))
save_composite(effects_plot, "direct_indirect_luc_emissions", 12, 6.7)
rm(effects); invisible(gc())

# Bala et al. (2007): four map panels and the original shared colour scale.
# Original uppercase letters sit in blank Pacific Ocean annotation locations.
bala <- read_image("luc_biogeophysical_bala07.jpeg")
stopifnot(identical(dim(bala)[1:2], c(760L, 1280L)))
for (box in list(c(51, 225, 78, 255), c(672, 229, 701, 259),
                 c(51, 537, 78, 568), c(670, 537, 702, 566))) {
  bala <- do.call(remove_letter, c(list(bala), as.list(box)))
}
albedo <- image_plot(crop_image(bala, 0, 0, 655, 319))
et <- image_plot(crop_image(bala, 655, 0, 1280, 319))
cloud <- image_plot(crop_image(bala, 0, 319, 655, 672))
planetary <- image_plot(crop_image(bala, 655, 319, 1280, 672))
scale <- image_plot(crop_image(bala, 185, 680, 1125, 760), margin = 0)
maps <- label_grid(albedo, et, cloud, planetary, labels = letters[1:4], ncol = 2)
bala_plot <- cowplot::plot_grid(maps, scale, ncol = 1, rel_heights = c(1, .12))
save_composite(bala_plot, "luc_biogeophysical_bala07", 12, 7.4)
