library(tidyverse)

# load Kumarathunge et al. data
df <- read_csv("~/data/aci-tglob_v1.0/ACi-TGlob_V1.0.csv")

df |> 
  filter(Dataset == "Tundra USA-AK") |> 
  group_by(Species) |> 
  summarise(n = n())

# spatial - adaptation
df |> 
  filter(
    Dataset == "Tundra USA-AK" &
    Ci > 270 & Ci < 280
  ) |> 
  ggplot(
    aes(
      x = Tleaf,
      y = Photo
    )
  ) +
  geom_point()

# seasonal - acclimation
df |> 
  filter(
    Dataset == "Maritime Pine, France" &
      Ci > 250 & Ci < 300
  ) |> 
  ggplot(
    aes(
      x = Tleaf,
      y = Photo
    )
  ) +
  geom_point()

