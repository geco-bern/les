library(dplyr)
library(tidyr)
library(climatol)
library(purrr)
library(readr)

source(here::here("R/read_fdk.R"))

# For preparing site meta data -----------
# install from local repositories (could also use github)
# install.packages("~/flux_data_kit-stineb/")
# install.packages("~/ingestr-stineb/")
# library(FluxDataKit)
# library(ingestr)
# # Get representative locations for each biome as FLUXNET stations
# df_sites <- FluxDataKit::fdk_site_info |> 
#   as_tibble()
# 
# # extract biome for each site using ingestr
# df_wwf <- ingest(
#   df_sites |> 
#     dplyr::select(sitename, lon, lat),
#   source = "wwf",
#   dir = "~/data/biomes/wwf_ecoregions/official/",
#   settings = list(layer = "wwf_terr_ecos")) |> 
#   tidyr::unnest(data) |> 
#   select(sitename, BIOME_NAME)
# 
# # combine data frames
# df_sites <- df_sites |> 
#   left_join(df_wwf,
#             by = c("sitename"))
# 
# # write to file
# saveRDS(df_sites, file = here::here("data/df_sites.rds"))

# Reading from file prepared above -----------
# read from file
# df_sites <- read_rds(here::here("data/df_sites.rds"))
df_sites <- read_csv(here::here("data/fdk_site_info.csv"))

# chose representative sites
use_sites <- c(
  "FI-Hyy", # Boreal Forests/Taiga
  "US-SRM", # Deserts & Xeric Shrublands
  "FR-Pue", # Mediterranean Forests, Woodlands & Scrub
  "DE-Hai", # Temperate Broadleaf & Mixed Forests
  "US-Tw1", # Temperate Grasslands, Savannas & Shrublands
  "AU-How", # Tropical & Subtropical Grasslands, Savannas & Shrubland
  # "MY-PSO", # Tropical moist broadleaved (evergreen) forest
  "BR-Sa3", # Tropical
  "ZM-Mon", # Tropical deciduous forest (xeric woodland)
  "US-ICh" # Tundra
  # "GL-ZaH"  # Tundra
)


# subset sites
df_sites_sub <- df_sites |> 
  filter(sitename %in% use_sites)

saveRDS(df_sites_sub, file = here::here("data/df_sites_sub.rds"))

# read daily flux data for each site
df <- df_sites_sub |> 
  select(sitename, lon, lat, elv) |> 
  # mutate(data = purrr::map(sitename, ~read_fdk(., path = "~/data/FluxDataKit/FLUXDATAKIT_FLUXNET"))) |> 
  mutate(data = purrr::map(sitename, ~read_fdk(., path = "~/data/FluxDataKit/v3.1/zenodo_upload/fluxnet/"))) |>
  unnest(data) |> 
  group_by(sitename) |> 
  nest()

create_diagram_per_site <- function(site, df){
  
  dat <- df |> 
    mutate(year = lubridate::year(TIMESTAMP),
           month = lubridate::month(TIMESTAMP),
           day = lubridate::mday(TIMESTAMP)) |> 
    select(year, month, day, P_F, TMAX_F_MDS, TMIN_F_MDS, lat, lon, elv) |> 
    as.data.frame()
  
  filn <- paste0(here::here("book/images/diagram_"),
                 site,
                 ".png")
  png(filn,
      width = 15, height = 11, units = "cm", res = 300)
  climatol::diagwl(dat[,1:6], 
                   # stname = paste0(site, "\n", dat$BIOME_NAME[1]),
                   stname = site,
                   shem = ifelse(dat$lat[1] < 0, TRUE, FALSE),
                   alt = dat$elv[1])
  dev.off()
  return(filn)
}

df <- df |> 
  mutate(file = purrr::map2_chr(sitename, data, ~create_diagram_per_site(.x, .y)))
