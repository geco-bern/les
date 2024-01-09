# library(sf)
# library(remotes)
# install_github("r-tmap/tmaptools")
# install_github("r-tmap/tmap")

library(dplyr)
# library(tmap)
# library(tmaptools)
library(ggplot2)
library(readr)

# # biomes <- sf::read_sf("~/data/biomes/wwf_ecoregions/official/wwf_terr_ecos.shp")
# biomes <- sf::read_sf("~/Downloads/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
# 
# rasta <- terra::rast(
#   here::here("data/modis_landcover__LPDAAC__v5.1__0.1deg__2010.nc")
#   )
# 
# biomes_raster <- terra::rasterize(biomes, rasta, field = "WWF_MHTNAM")
# 
# coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")
# 
# sites <- read_rds(here::here("data/df_sites_sub.rds")) |>
#   sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(biomes))


gg <- ggplot() +
  tidyterra::geom_spatraster(data = biomes_raster) +
  geom_sf(data = coast,
          colour = 'black',
          size = 0.05) +
  geom_sf(data = sites, col = "red") +
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
      "Montane Grasslands and Shrublands"                            = "cadetblue3", 
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
        legend.text = element_text(size = 6),
        legend.title = element_blank())

ggsave(here::here("book/images/map_biomes.png"), width = 10, height = 6)

# # biomes_agg <- biomes |>
# #   group_by(BIOME) |>
# #   summarise()
# # 
# # saveRDS(biomes_agg, file = here::here("data/biomes_agg.rds"))
# 
# # biomes_agg <- read_rds(here::here("data/biomes_agg.rds"))
# 
# sites <- read_rds(here::here("data/df_sites_sub.rds")) |> 
#   sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(biomes))
# 
# tm_shape(biomes) +
#   tm_polygons(fill = "WWF_MHTNAM", 
#               fill.scale = tm_scale_ordinal(values = "Earth"), 
#               fill.legend = tm_legend(title = "Biome")) +
#   # tm_layout(legend.outside.position = "left", legend.outside.size = 0.2) +
#   tm_shape(sites) +
#   tm_dots(size = 0.2, col = "red") +
#   tm_place_legends_right(width = 0.2)

