# install from local repositories (could also use github)
install.packages("~/flux_data_kit-stineb/")
install.packages("~/ingestr-stineb/")
library(FluxDataKit)
library(ingestr)

# cran libraries
library(dplyr)
library(tidyr)
library(climatol)
library(purrr)

# Get representative locations for each biome as FLUXNET stations
df_sites <- FluxDataKit::fdk_site_info |> 
  as_tibble()

# extract biome for each site using ingestr
df_wwf <- ingest(
  df_sites |> 
    dplyr::select(sitename, lon, lat),
  source = "wwf",
  dir = "~/data/biomes/wwf_ecoregions/official/",
  settings = list(layer = "wwf_terr_ecos")) |> 
  tidyr::unnest(data) |> 
  select(sitename, BIOME_NAME)

# combine data frames
df_sites <- df_sites |> 
  left_join(df_wwf,
            by = c("sitename"))

# chose representative sites
use_sites <- c(
  "FI-Hyy", # Boreal Forests/Taiga
  "US-SRM", # Deserts & Xeric Shrublands
  "FR-Pue", # Mediterranean Forests, Woodlands & Scrub
  "DE-Hai", # Temperate Broadleaf & Mixed Forests
  "US-Tw1", # Temperate Grasslands, Savannas & Shrublands
  "AU-How", # Tropical & Subtropical Grasslands, Savannas & Shrublan
  "GF-Guy", # Tropical & Subtropical Moist Broadleaf Forests
  "GL-ZaH" # Tundra
)

# subset sites
df_sites <- df_sites |> 
  filter(sitename %in% use_sites)

# read daily flux data for each site
read_fdk <- function(site, path){
  filn <- list.files(path = path)
  filn <- filn[which(stringr::str_detect(filn, pattern = site))]
  filn <- filn[which(stringr::str_detect(filn, pattern = "DD"))]
  df <- readr::read_csv(paste0(path, "/", filn))
}

df <- df_sites |> 
  select(sitename, lon, lat, BIOME_NAME) |> 
  mutate(data = purrr::map(sitename, ~read_fdk(., path = "~/data/FluxDataKit/FLUXDATAKIT_FLUXNET")))

dat <- df |> 
  slice(1) |> 
  unnest(data) |> 
  select(TIMESTAMP, P_F, TMIN_F_MDS, TMAX_F_MDS) |> 
  mutate(year = lubridate::year(TIMESTAMP),
         month = lubridate::month(TIMESTAMP),
         day = lubridate::mday(TIMESTAMP)) |> 
  select(year, month, day, P_F, TMAX_F_MDS, TMIN_F_MDS)

climatol::diagwl(dat)
