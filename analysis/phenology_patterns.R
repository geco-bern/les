# This is code adopted from handful_of_pixels Chapter phenology_trends.qmd 
# written by Koen Hufkens
library(terra)
library(dplyr)
library(ggplot2)
library(hexbin)
library(MODISTools)
library(cowplot)

phenology <- readRDS(here::here("data-raw/phenology_2012.rds"))
dem <- terra::rast(here::here("data-raw/srtm_38_03.tif"))
# land_cover <- readRDS(here::here("data-raw/land-cover_2012.rds"))

# screening of data
phenology <- phenology |>
  mutate(
    value = ifelse(value > 32656, NA, value),
    value = as.numeric(format(as.Date("1970-01-01") + value, "%j")),
    value = ifelse (value < 200, value, NA)
  )

phenology_raster <- MODISTools::mt_to_terra(
  phenology,
  reproject = TRUE
)

gg1 <- ggplot() +
  tidyterra::geom_spatraster(data = phenology_raster) +
  scale_fill_viridis_c(
    na.value = NA,
    name = "DOY"
  ) +
  theme_bw()

# crop the dem
dem <- terra::crop(
  x = dem,
  y = phenology_raster
)

# resample the dem using
# the mean DEM value in a
# MODIS pixel
dem <- terra::resample(
  x = dem,
  y = phenology_raster,
  method = "average"
)

# mask the locations which
# have no data
dem <- terra::mask(
  dem,
  is.na(phenology_raster),
  maskvalues = TRUE
)

# convert to data frame and merge
dem_df <- as.vector(dem)
phenology_df <- as.vector(phenology_raster)
sct_df <- data.frame(
  altitude = dem_df,
  doy = phenology_df
)

gg2 <- ggplot(
  data = sct_df,
  aes(
    altitude,
    doy
  )) +
  geom_hex() +
  scale_fill_viridis_c(trans="log10", name = "Count") +
  labs(
    x = "Elevation (m)",
    y = "MODIS vegetation greenup (DOY)"
  ) +
  theme_bw()

plot_grid(gg1, gg2, labels = c("a", "b"), rel_widths = c(1, 0.85))

ggsave(here::here("book/images/phenology_patterns.png"), width = 10, height = 4)
