# library(vhs)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(ggridges)
library(remotes)
# install_github("geco-bern/ingestr", auth_token = "XXX")
# install_github("geco-bern/rgeco", auth_token = "XXX")
library(ingestr)
library(rgeco)

df <- read_csv("~/data/rootingdepth/rsip/RSIP_Analysis_sheet_210721.csv") |>
  rename(lon = Long, lat = Lat) |> 
  rowid_to_column(var = "id") |> 
  
  ## problem: some have a reference error
  dplyr::filter(lon != "#REF!") |> 
  mutate(lon = as.numeric(lon), lat = as.numeric(lat), 
         Dr = as.numeric(Dr),
         wtd = as.numeric(Water_Table_Depth_Fan))

## add WWF biome info
df_wwf <- ingest(
  df |> 
    dplyr::select(sitename = id, lon, lat),
  source = "wwf",
  dir = "~/data/biomes/wwf_ecoregions/official/",
  settings = list(layer = "wwf_terr_ecos")
)|> 
  mutate(data = purrr::map(data, ~slice(., 1))) |> 
  unnest(data)

df <- df %>% 
  left_join(
    df_wwf %>% 
      dplyr::select(id = sitename, biome = BIOME, biome_name = BIOME_NAME),
    by = "id"
  )


# Distributions -------------

## By biome ----------------
biome_exclude <- df |> 
  group_by(biome_name) |> 
  summarise(count = n()) |> 
  dplyr::filter(count < 3) |> 
  pull(biome_name)

gg1 <- df |> 
  dplyr::filter(!(biome_name %in% biome_exclude)) |> 
  dplyr::filter(!is.na(biome_name)) |> 
  dplyr::filter(biome_name!="Mangroves") |>   
  ggplot(aes(x = Dr, y = biome_name)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 1.5, size = 0.25,
    # point_alpha = 0.2,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0), name = "") +
  scale_x_continuous(expand = c(0, 0), name = "Rooting depth (m)", limits = c(-2, 20)) +
  scale_discrete_manual("point_color", values = rgb(0,0,0,0.1), guide = "none") +
  coord_cartesian(clip = "off") +
  theme_ridges(center = TRUE)


## By species -------------------
df_species <- df |> 
  group_by(Species) |> 
  summarise(
    n = n(),
    zr = mean(Dr)) |> 
  arrange(desc(zr)) |> 
  filter(n>5)

# Comparing some obvious ones
use_species_1 <- c(
  "Pinus sylvestris",
  "Picea abies",
  "Quercus robur",
  "Zea Mays L.",
  "Triticum aestivum",
  "Fagus sylvatica",
  "Larix decidua",
  "Pinus pinaster",
  "Lolium perenne"
)

gg2 <- df |> 
  filter(Species %in% use_species_1) |> 
  ggplot(aes(x = Dr, y = reorder(Species, -Dr, FUN = function(x) quantile(x, probs = 0.75, na.rm = TRUE)))) +
  geom_boxplot(
    width = 0.5, 
    fill = "grey70",
    outlier.shape = NA
    ) +
  geom_jitter(
    alpha = 0.3,
    width = 0.01
  ) +
  # geom_density_ridges(
  #   # bandwidth = 1,
  #   jittered_points = TRUE, scale = .95, rel_min_height = .01,
  #   point_shape = "|", point_size = 1.5, size = 0.25,
  #   # point_alpha = 0.2,
  #   position = position_points_jitter(height = 0)
  # ) +
  scale_y_discrete(
    # expand = c(0, 0), 
    name = ""
    ) +
  scale_x_continuous(
    # expand = c(0, 0), 
    limits = c(0, 10),
    name = "Rooting depth (m)"
    ) +
  scale_discrete_manual(
    "point_color", 
    values = rgb(0,0,0,0.1), 
    guide = "none"
    ) +
  coord_cartesian(clip = "off") +
  theme_ridges(center = TRUE)


use_species_2 <- c(
  "Pinus edulis",
  "Boscia albitrunca",
  "Acacia erioloba"
)

gg2b <- df |> 
  filter(Species %in% use_species_2) |> 
  ggplot(aes(x = Dr, y = reorder(Species, -Dr, FUN = function(x) quantile(x, probs = 0.75, na.rm = TRUE)))) +
  geom_boxplot(
    width = 0.5, 
    fill = "grey70",
    outlier.shape = NA
  ) +
  geom_jitter(
    alpha = 0.3,
    width = 0.01
  ) +
  # geom_density_ridges(
  #   # bandwidth = 1,
  #   jittered_points = TRUE, scale = .95, rel_min_height = .01,
  #   point_shape = "|", point_size = 1.5, size = 0.25,
  #   # point_alpha = 0.2,
  #   position = position_points_jitter(height = 0)
  # ) +
  scale_y_discrete(
    # expand = c(0, 0), 
    name = ""
  ) +
  scale_x_continuous(
    # expand = c(0, 0), 
    name = "Rooting depth (m)"
  ) +
  scale_discrete_manual(
    "point_color", 
    values = rgb(0,0,0,0.1), 
    guide = "none"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(center = TRUE)


## By PFT ---------------
gg3 <- df |> 
  mutate(
    Growth_form = ifelse(Growth_form == "shrub", "Shrub", Growth_form)
  ) |> 
  mutate(
    Growth_form = ifelse(Growth_form == "succulent", "Succulent", Growth_form)
  ) |> 
  filter(!is.na(Growth_form), !(Growth_form %in% c("shrub", "succulent"))) |>
  ggplot(aes(x = Dr, y = reorder(Growth_form, -Dr, FUN = function(x) quantile(x, probs = 0.75, na.rm = TRUE)))) +
  # geom_boxplot(
  #   width = 0.5,
  #   fill = "grey70",
  #   outlier.shape = NA
  # ) +
  # geom_jitter(
  #   alpha = 0.1,
  #   width = 0.01
  # ) +
  geom_density_ridges(
    # bandwidth = 1,
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 1.5, size = 0.25,
    # point_alpha = 0.2,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(
    # expand = c(0, 0), 
    name = ""
  ) +
  scale_x_continuous(
    # expand = c(0, 0), 
    name = "Rooting depth (m)",
    limits = c(-1, 20)
  ) +
  scale_discrete_manual(
    "point_color", 
    values = rgb(0,0,0,0.1), 
    guide = "none"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(center = TRUE)

plot_grid(
  gg3,
  # gg1,
  gg2,
  gg2b,
  rel_heights = c(0.6, 0.8, 0.38),
  labels = c("a", "b", "c"),
  ncol = 1
)
ggsave(here::here("book/images/rootingdepth.png"), width = 10, height = 8)


# Maps ---------------
## WHC -------------
df_whc_agg <- readRDS("~/mct/data/df_whc_agg.rds")

gg1 <- plot_map4(
  df_whc_agg, 
  varnam = "whc_1m", 
  breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
  latmin = -60, latmax = 80,
  spacing = "constant", 
  maxval = 6000, 
  combine = FALSE, 
  colorscale = "batlowK", 
  legend_title = "",
  expand_size_y = 0.5,
  ocean = TRUE,
  legend_direction = "horizontal"
)


## S_CWDX80 ---------------
load("~/mct/data/df_vegmask_tenthdeg.RData")  # loads df_vegmask_tenthdeg
nc_tenthdeg <- nc_to_df("~/mct/data/cwdx80_tenthdeg.nc", varnam = "cwdx80") |> 
  mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) |>
  left_join(df_vegmask_tenthdeg |>
              mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)),
            by = c("lon", "lat")) |>
  mutate(cwdx80 = ifelse(nonveg > 99, NA, cwdx80)) |> 
  mutate(cwdx80 = ifelse(lat > 74, NA, cwdx80))

gg2 <- plot_map4(
  nc_tenthdeg, 
  varnam = "cwdx80", 
  breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
  latmin = -60, latmax = 80,
  spacing = "constant", 
  maxval = 6000, 
  combine = FALSE, 
  colorscale = "batlowK", 
  legend_title = "",
  hillshade = FALSE,
  rivers = FALSE,
  ocean = TRUE,
  expand_size_y = 0.5,
  legend_direction = "vertical"
  )

cowplot::plot_grid(
  gg1$ggmap, 
  gg2$ggmap, 
  gg1$gglegend,
  ncol = 1,
  rel_heights = c(1, 1, 0.3),
  labels = c("a", "b", "")
  )

ggsave(
  here::here("book/images/map_whc_s0.png"),
  width = 8,
  height = 8
)

