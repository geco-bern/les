# install from local repositories (could also use github)
# install.packages("~/flux_data_kit-stineb/")
# install.packages("~/ingestr-stineb/")
library(FluxDataKit)
library(ingestr)

# cran libraries
library(dplyr)
library(tidyr)
library(climatol)
library(purrr)
library(readr)

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
  "MY-PSO", # Tropical moist broadleaved (evergreen) forest
  "ZM-Mon", # Tropical deciduous forest (xeric woodland)
  "GL-ZaH"  # Tundra
)

# subset sites
df_sites_sub <- df_sites |> 
  filter(sitename %in% use_sites)

# read daily flux data for each site
read_fdk <- function(site, path){
  print(site)
  filn <- list.files(path = path)
  filn <- filn[which(stringr::str_detect(filn, pattern = site))]
  filn <- filn[which(stringr::str_detect(filn, pattern = "DD"))]
  readr::read_csv(paste0(path, "/", filn))
}

df <- df_sites_sub |> 
  select(sitename, lon, lat, elv, BIOME_NAME) |> 
  mutate(data = purrr::map(sitename, ~read_fdk(., path = "~/data/FluxDataKit/FLUXDATAKIT_FLUXNET"))) |> 
  unnest(data) |> 
  group_by(sitename) |> 
  nest()

create_diagram_per_site <- function(site, df){
  
  dat <- df |> 
    mutate(year = lubridate::year(TIMESTAMP),
           month = lubridate::month(TIMESTAMP),
           day = lubridate::mday(TIMESTAMP)) |> 
    select(year, month, day, P_F, TMAX_F_MDS, TMIN_F_MDS, lat, lon, elv, BIOME_NAME) |> 
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

saveRDS(df_sites, file = here::here("data/df_sites.rds"))
saveRDS(df_sites_sub, file = here::here("data/df_sites_sub.rds"))
