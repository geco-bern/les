# library(sf)
# library(remotes)
# install_github("r-tmap/tmaptools")
# install_github("r-tmap/tmap")

library(dplyr)
library(tmap)
library(tmaptools)
library(ggplot2)
library(readr)

sf::sf_use_s2(FALSE)

biomes <- sf::read_sf("~/data/biomes/wwf_ecoregions/official/wwf_terr_ecos.shp")

biomes_agg <- biomes |> 
  group_by(BIOME) |> 
  summarise()

saveRDS(biomes_agg, file = here::here("data/biomes_agg.rds"))

sites <- read_rds(here::here("data/df_sites.rds")) |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(biomes))

# tmap_options(check.and.fix = FALSE)

tm_shape(biomes_agg) +
  tm_fill(col = "BIOME", 
          breaks = 1:14, 
          title = "Biome",
          style = "cat"
          ) +
  # tm_layout(legend.outside.position = "left", legend.outside.size = 0.2) +
  tm_shape(sites) +
  tm_dots(size = 0.2, col = "black")


tm_shape(biomes_agg) +
  tm_polygons(fill = "BIOME") +
  # tm_layout(legend.outside.position = "left", legend.outside.size = 0.2) +
  tm_shape(sites) +
  tm_dots(size = 0.2, col = "black")

# ggplot() +
#   geom_sf(data = biomes_agg, aes(fill = BIOME))
