# Libraries
library(dplyr)       # for working with ease
library(tidyr)       # for working with ease
library(stringr)
library(ggplot2)
library(readr)

# MESI data
# IMPORTANT: LOAD DATA FROM REPO AT COMMIT d73bbdb12f469e219e3bb360f4b79fdb0dfa0891 Mon Nov 28 11:37:49 2022 +0100
df <- read_csv("~/mesi-db/data/mesi_main.csv")

df |> 
  filter(str_detect(response, pattern = "cn")) |> 
  pull(response) |>
  unique()

use_response <- c(
  "leaf_cn",
  "wood_cn",
  "leaf_litter_cn",
  "mbcn",
  "fine_root_cn",
  "soil_total_cn",
  "litter_cn",
  "som_cn"
)

df <- df %>%
  filter(response %in% use_response)

df |> 
  ggplot(aes(x_c, color = response)) +
  geom_density()

  mutate(myvar = response) %>%

  ## variables re-grouped by myself
  mutate(myvar = ifelse(myvar %in% c("agb", "agb_coarse"), "agb", myvar)) %>%

  # subset
  df_mesi %>%

  
