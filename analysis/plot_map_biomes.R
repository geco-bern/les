library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)

sf::sf_use_s2(FALSE)

# Biomes shapefile -------------
# Get data from Olson, D. M, 2020, "Terrestrial ecoregions of the world (Copy to use in GapAnalysis R package)", 
# https://doi.org/10.7910/DVN/WTLNRG, Harvard Dataverse, V1
biomes <- sf::read_sf("~/data/biomes/olson_harvard_dataverse/tnc_terr_ecoregions.shp")

# Rasterise ---------------
# Load template raster available in this repository
rasta <- terra::rast(
  here::here("data/modis_landcover__LPDAAC__v5.1__0.1deg__2010.nc")
  )

# perform shapefile to raster conversion
biomes_raster <- terra::rasterize(biomes, rasta, field = "WWF_MHTNAM")

# get coastline -------------------
coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

# get locations of representative sites ----------------
sites <- read_rds(here::here("data/df_sites_sub.rds")) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(biomes))

# Plot map ----------------
gg <- ggplot() +
  tidyterra::geom_spatraster(data = biomes_raster) +
  geom_sf(data = coast,
          colour = 'black',
          size = 0.05) +
  geom_sf(data = sites, fill = "red", shape = 21) +
  ggrepel::geom_label_repel(data = sites, 
                           aes(label = sitename, geometry = geometry),
                           stat = "sf_coordinates",
                           min.segment.length = 0,
                           segment.size = 0.2,
                           size = 2,
                           seed = 42,
                           box.padding = 0.5,
                           color = "black") +
  scale_fill_manual(
    values = c(
      "Boreal Forests/Taiga"                                         = "dodgerblue4", 
      "Deserts and Xeric Shrublands"                                 = "#FFD3A0", 
      "Flooded Grasslands and Savannas"                              = "indianred3", 
      "Inland Water"                                                 = "azure", 
      "Mangroves"                                                    = "violetred", 
      "Mediterranean Forests, Woodlands and Scrub"                   = "orangered3", 
      "Montane Grasslands and Shrublands"                            = "steelblue3", 
      "Rock and Ice"                                                 = "azure4", 
      "Temperate Broadleaf and Mixed Forests"                        = "springgreen3", 
      "Temperate Conifer Forests"                                    = "#31A278", 
      "Temperate Grasslands, Savannas and Shrublands"                = "goldenrod3", 
      "Tropical and Subtropical Coniferous Forests"                  = "lightseagreen", 
      "Tropical and Subtropical Dry Broadleaf Forests"               = "darkolivegreen", 
      "Tropical and Subtropical Grasslands, Savannas and Shrublands" = "goldenrod4", 
      "Tropical and Subtropical Moist Broadleaf Forests"             = "darkgreen", 
      "Tundra"                                                       = "lightcyan3"
    )) + 
  # set extent in longitude and latitude
  coord_sf(ylim = c(-60, 85),
           expand = FALSE   # to draw map strictly bounded by the specified extent
  ) +
  labs(title = "Biomes", x = "", y = "") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_blank())

ggsave(here::here("book/images/map_biomes.png"), width = 12, height = 7)

