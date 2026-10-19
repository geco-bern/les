library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(khroma)

# Run from the repository root with renv enabled:
# Rscript analysis/plot_map_aridityindex.R maps
# Omit "maps" to also regenerate the separate fAPAR relationship figure.
maps_only <- "maps" %in% commandArgs(trailingOnly = TRUE)
source(here::here("R/global_map_style.R"))
source(here::here("R/plot_discrete_cbar.R"))

## Global map of AI ----------------
# breaks for color bar
breaks <- c(0, 0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.2, 1.6, 2, 2.5, 3, Inf)
# breaks <- c(0, 0.03, 0.2, 0.5, 0.65, Inf)

# raster_ai <- rast("~/data/greve/ep_over_p_cru_ncep.nc")

# data from https://doi.org/10.6084/m9.figshare.7504448.v4
rasta <- rast("~/data/aridityindex_zomer_2022/Global-AI_ET0_v3_annual/ai_v3_yr.tif")

# Aggregate the stored integers first, then apply their scale factor. This is
# the same mean as scaling before aggregation, without loading the entire
# high-resolution raster into an R vector.
rasta_agg <- aggregate(
  rasta,
  fact = round(0.1 / res(rasta)[1]),
  fun = "mean"
) * 1e-4
names(rasta_agg) <- "aridity_index"

# Keep the original left-closed intervals, including the open-ended >=3 bin.
colours <- as.character(khroma::colour("roma")(length(breaks) - 1))
ggmap <- global_map_frame() +
  global_map_raster(rasta_agg, function(x) {
    colours[cut(x, breaks = breaks, right = FALSE, labels = FALSE)]
  }, mask_ocean = TRUE) +
  global_map_outlines() +
  global_map_extent() +
  theme(plot.margin = margin(12, 12, 0, 12))

# Equal-width bins retain the original key's non-linear spacing; the triangle
# at the right denotes values >=3 rather than clipping the data at 3.
gglegend <- plot_discrete_cbar(
  breaks = breaks,
  colors = colours,
  legend_direction = "horizontal",
  width = 0.03,
  font_size = 3.5,
  expand_size_y = 0.85,
  spacing = "constant"
) +
  labs(title = expression(paste("Aridity index (", italic(P)/PET, ")"))) +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
    plot.margin = margin(0, 12, 8, 12))

figure <- cowplot::plot_grid(ggmap, gglegend, ncol = 1, rel_heights = c(1, 0.12))
save_global_map(figure, here::here("book/images/map_aridityindex"))

## "Donohue" plot --------------
if (!maps_only) {

  # get fAPAR at 0.5 deg
  rasta_fpar <- terra::rast("~/data/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/MODIS-C006_MOD15A2__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2018__MON__fv0.02_ANNMAX_MEAN.nc")["fpar"]$fpar

  # resample AI data to fAPAR grid
  rasta_ai_agg <- resample(rasta_agg, rasta_fpar, method = "bilinear")

  df <- as.data.frame(rasta_fpar, xy = TRUE) |>
    left_join(
      as.data.frame(rasta_ai_agg, xy = TRUE),
      by = c("x", "y")
    ) |>
    as_tibble()

  df |>
    ggplot(aes(aridity_index, fpar)) +
    geom_hex(
      bins = 50,
      # show.legend = FALSE
      ) +
    xlim(0, 1.5) +
    theme_classic() +
    scale_fill_gradientn(
      colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
      # trans = "log",
      ) +
    labs(
      x = expression(paste(italic(P)/PET)),
      y = "fAPAR"
    )

  ggsave(here::here("book/images/donohue.png"), width = 6, height = 4)
}
