library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)

# load Kumarathunge et al. data
df <- read_csv("~/data/aci-tglob_v1.0/ACi-TGlob_V1.0.csv")

df <- read_rds("~/Downloads/all_sites_simulation_and_original-data.rds")

# good: 5
gg1 <- df$data_org[[9]] |> 
  ggplot(
    aes(
      x = Tleaf,
      y = Photo,
      color = Ci,
      shape = as.factor(Curve_Id)
    )
  ) +
  geom_point()

gg2 <- df$data_org[[9]] |> 
  filter(Curve_Id == 1) |> 
  ggplot(
    aes(
      x = Ci,
      y = Photo
    )
  ) +
  geom_point()

plot_grid(gg1, gg2, nrow = 2)

ggsave(here::here("fig/example.pdf"), width = 8, height = 6)

# 
# df |> 
#   filter(Dataset == "Tundra USA-AK") |> 
#   group_by(Species) |> 
#   summarise(n = n())
# 
# # spatial - adaptation
# df |> 
#   filter(
#     Dataset == "Tundra USA-AK" &
#     Ci > 270 & Ci < 280
#   ) |> 
#   ggplot(
#     aes(
#       x = Tleaf,
#       y = Photo
#     )
#   ) +
#   geom_point()
# 
# # seasonal - acclimation
# df |> 
#   filter(
#     Dataset == "Maritime Pine, France" &
#       Ci > 250 & Ci < 300
#   ) |> 
#   ggplot(
#     aes(
#       x = Tleaf,
#       y = Photo
#     )
#   ) +
#   geom_point()
# 
