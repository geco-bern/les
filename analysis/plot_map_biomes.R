library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)

sf::sf_use_s2(FALSE)
source(here::here("R/global_map_style.R"))

# Biomes shapefile -------------
# Get data from Olson, D. M, 2020, "Terrestrial ecoregions of the world (Copy to use in GapAnalysis R package)", 
# https://doi.org/10.7910/DVN/WTLNRG, Harvard Dataverse, V1
biomes <- sf::read_sf("~/data/biomes/olson_harvard_dataverse/tnc_terr_ecoregions.shp")

# get biome names and code
df_biomes_codes <- biomes |> 
  select(WWF_MHTNAM, WWF_MHTNUM) |> 
  as.data.frame() |> 
  select(-geometry) |> 
  distinct() |> 
  arrange(WWF_MHTNUM)

write_csv(df_biomes_codes, file = here::here("data/df_biomes_codes.csv"))

# Rasterise ---------------
# Load template raster available in this repository
rasta <- terra::rast(
  here::here("data/modis_landcover__LPDAAC__v5.1__0.1deg__2010.nc")
  )

# perform shapefile to raster conversion
biomes_raster <- terra::rasterize(biomes, rasta, field = "WWF_MHTNAM")

# write to file as NetCDF
terra::writeRaster(biomes_raster, filename = here::here("data/biomes_raster_0.1deg.tif"), overwrite = TRUE)

# get locations of representative sites ----------------
sites <- read_rds(here::here("data/df_sites_sub.rds")) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(biomes))

# Plot map ----------------
# Map raster codes back to the original biome palette without downsampling.
biome_colours <- c(
  "Boreal Forests/Taiga" = "dodgerblue4",
  "Deserts and Xeric Shrublands" = "#FFD3A0",
  "Flooded Grasslands and Savannas" = "indianred3",
  "Inland Water" = "white", "Mangroves" = "violetred",
  "Mediterranean Forests, Woodlands and Scrub" = "orangered3",
  "Montane Grasslands and Shrublands" = "steelblue3", "Rock and Ice" = "azure4",
  "Temperate Broadleaf and Mixed Forests" = "springgreen3",
  "Temperate Conifer Forests" = "#31A278",
  "Temperate Grasslands, Savannas and Shrublands" = "goldenrod3",
  "Tropical and Subtropical Coniferous Forests" = "lightseagreen",
  "Tropical and Subtropical Dry Broadleaf Forests" = "darkolivegreen",
  "Tropical and Subtropical Grasslands, Savannas and Shrublands" = "goldenrod4",
  "Tropical and Subtropical Moist Broadleaf Forests" = "darkgreen", "Tundra" = "lightcyan3"
)
classes <- levels(biomes_raster)[[1]]
key <- data.frame(x = 0, y = 0, biome = factor(names(biome_colours), levels = names(biome_colours)))
gg <- global_map_frame() +
  global_map_raster(biomes_raster, function(x) {
    unname(biome_colours[classes[[2]][match(x, classes[[1]])]])
  }) +
  global_map_outlines() +
  geom_point(data = key, aes(x, y, fill = biome), shape = 22, alpha = 0, show.legend = TRUE) +
  geom_sf(data = sites, fill = "red", shape = 21) +
  ggrepel::geom_label_repel(data = sites, 
                           aes(label = sitename, geometry = geometry),
                           stat = "sf_coordinates",
                           min.segment.length = 0,
                           segment.size = 0.2,
                           size = 3,
                           seed = 42,
                           box.padding = 0.5,
                           color = "black") +
  scale_fill_manual(values = biome_colours, name = NULL, drop = FALSE,
    labels = function(x) vapply(x, function(label) paste(strwrap(label, width = 44), collapse = "\n"), character(1))) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE,
    override.aes = list(alpha = 1, size = 3, colour = "#7b8279"))) +
  theme(legend.text = element_text(size = 9)) +
  global_map_extent()

save_global_map(gg, here::here("book/images/map_biomes"), height = 8.2)
