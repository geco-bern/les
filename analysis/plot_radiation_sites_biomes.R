library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(hms)
library(khroma)
library(ggplot2)
library(cowplot)

source(here::here("R/read_fdk.R"))

# Reading -----------
# read from file
# df_sites <- read_rds(here::here("data/df_sites.rds"))
df_sites <- read_csv(here::here("data/fdk_site_info.csv"))

# chose representative sites
use_sites <- c(
  "FI-Hyy", # Boreal Forests/Taiga
  "DE-Hai", # Temperate Broadleaf & Mixed Forests
  "DE-Tha", # Temperate Coniferous
  "DE-Gri", # Grassland just next to DE-Tha
  "BR-Sa3", # Tropical
  "US-ICh"  # Tundra
)

# subset sites
df_sites_sub <- df_sites |> 
  filter(sitename %in% use_sites)

# saveRDS(df_sites_sub, file = here::here("data/df_sites_sub.rds"))

## Half-hourly data --------------
# read hourly flux data for each site
hdf <- df_sites_sub |> 
  dplyr::select(sitename, lon, lat, elv) |> 
  mutate(data = purrr::map(
    sitename, ~read_fdk(
      ., 
      path = "~/data/FluxDataKit/v3.1/fluxnet/",
      # path = "~/data/FluxDataKit/FLUXDATAKIT_FLUXNET",
      pattern = "HH"
    ))) |> 
  unnest(data) |> 
  group_by(sitename) |> 
  nest()

## Daily data --------------
# read daily flux data for each site
ddf <- df_sites_sub |> 
  select(sitename, lon, lat, elv) |> 
  mutate(data = purrr::map(
    sitename, ~read_fdk(
      ., 
      path = "~/data/FluxDataKit/v3.1/fluxnet/",
      # path = "~/data/FluxDataKit/FLUXDATAKIT_FLUXNET",
      pattern = "DD"
      ))) |> 
  unnest(data) |> 
  group_by(sitename) |> 
  nest()

# Aggregate -----------
## Hourly -----------
# aggregate to hours for mid-summer
hdf_sub <- hdf |> 
  unnest(data) |> 
  mutate(TIMESTAMP_START = ymd_hm(TIMESTAMP_START),
         TIMESTAMP_END = ymd_hm(TIMESTAMP_END)) |> 
  mutate(year = lubridate::year(TIMESTAMP_START),
         month = lubridate::month(TIMESTAMP_START),
         day = lubridate::mday(TIMESTAMP_START)) |> 
  filter(month == 7) |> 
  mutate(hm = as_hms(TIMESTAMP_START)) |> 
  group_by(sitename, hm) |> 
  summarise(
    # latent heat flux
    le = mean(LE_F_MDS, na.rm = TRUE),
    
    # sensible heat flux
    heat = mean(H_F_MDS, na.rm = TRUE), 
    
    # net radiation
    netrad = mean(NETRAD, na.rm = TRUE),
    
    # shortwave radiation
    sw = mean(SW_IN_F_MDS, na.rm = TRUE),
    
    # longwave radiation
    lw = mean(LW_IN_F_MDS, na.rm = TRUE)
    )

## Daily to mean season -----------
# aggregate to mean seasonal cycle
ddf_sub <- ddf |> 
  unnest(data) |> 
  mutate(TIMESTAMP = ymd(TIMESTAMP)) |> 
  mutate(year = year(TIMESTAMP),
         doy = yday(TIMESTAMP)) |> 
  group_by(sitename, doy) |> 
  summarise(
    # latent heat flux
    le = mean(LE_F_MDS, na.rm = TRUE),
    
    # sensible heat flux
    heat = mean(H_F_MDS, na.rm = TRUE), 
    
    # net radiation
    netrad = mean(NETRAD, na.rm = TRUE),
    
    # shortwave radiation
    sw = mean(SW_IN_F_MDS, na.rm = TRUE),
    
    # longwave radiation
    lw = mean(LW_IN_F_MDS, na.rm = TRUE)
  ) |> 
  mutate(date = ymd("2023-01-01") + doy - 1)


# Plot -----------
## Diurnal cycle in July  -----------
### Components ----------------
hdf_sub |> 
  mutate(res = heat + le - netrad) |> 
  pivot_longer(cols = c(netrad, heat, le)) |>  # , res
  mutate(sitename = factor(
    sitename, 
    levels = c(
      "US-ICh", # Tundra
      "FI-Hyy", # Boreal Forests/Taiga
      "BR-Sa3", # Tropical
      "DE-Hai", # Temperate Broadleaf & Mixed Forests
      "DE-Tha", # Temperate Coniferous
      "DE-Gri"  # Grassland just next to DE-Tha
      )
    )
    # name = factor(name, levels = c("netrad", "heat", "le", "res"))
    ) |> 
  ggplot(aes(hm, value, color = name)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  scale_x_time() +
  # scale_x_datetime(date_breaks= "2 hours", date_labels = "%H:%M") +
  scale_color_manual(
    name = "",
    labels = c(
      heat = expression(paste(italic("H"))),
      le = expression(paste(lambda, italic("E"))),
      netrad = expression(paste(italic("R")[net]))
      # res = "Residual"
      ),
    values = c(
      heat = "#E69F00",
      le = "#56B4E9",
      netrad = "black"
      # res = "grey70"
    )
    ) +
  # scale_linetype_manual(
  #   name = "",
  #   labels = c(
  #     heat = expression(paste(italic("H"))),
  #     le = expression(paste(lambda, italic("E"))),
  #     netrad = expression(paste(italic("R")[net])),
  #     res = "Residual"
  #   ),
  #   values = c(
  #     heat = "solid",
  #     le = "solid",
  #     netrad = "solid",
  #     res = "dashed"
  #   )
  # ) +
  theme_classic() +
  labs(
    x = "Time of day",
    y = expression(paste("Energy flux (W m"^-2, ")"))
  ) +
  facet_wrap(~sitename)

ggsave(
  here::here("book/images/diurnal_cycle_radiation.png"),
  width = 12, 
  height = 7
)

### Residual (ground heat flux) -----------
hdf_sub |> 
  mutate(res = heat + le - netrad) |> 
  mutate(sitename = factor(
    sitename, 
    levels = c(
      "US-ICh",  # Tundra
      "FI-Hyy", # Boreal Forests/Taiga
      "BR-Sa3", # Tropical
      "DE-Hai", # Temperate Broadleaf & Mixed Forests
      "DE-Tha", # Temperate Coniferous
      "DE-Gri"  # Grassland just next to DE-Tha
    )
  )) |> 
  ggplot(aes(hm, res)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  scale_x_time() +
  theme_classic() +
  labs(
    x = "Time of day",
    y = expression(paste("Energy flux (W m"^-2, ")"))
  ) +
  facet_wrap(~sitename)

## Mean seasonal cycle  -----------
### Components ----------
ddf_sub |> 
  # mutate(res = heat + le - netrad) |> 
  pivot_longer(cols = c(netrad, heat, le)) |>  # , res
  mutate(sitename = factor(
    sitename, 
    levels = c(
      "US-ICh",  # Tundra
      "FI-Hyy", # Boreal Forests/Taiga
      "BR-Sa3", # Tropical
      "DE-Hai", # Temperate Broadleaf & Mixed Forests
      "DE-Tha", # Temperate Coniferous
      "DE-Gri"  # Grassland just next to DE-Tha
    )
  )) |> 
  ggplot(aes(date, value, color = name)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(
    name = "",
    labels = c(
      heat = expression(paste(italic("H"))),
      le = expression(paste(lambda, italic("E"))),
      netrad = expression(paste(italic("R")[net]))
      # res = "Residual"
    ),
    values = c(
      heat = "#E69F00",
      le = "#56B4E9",
      netrad = "black"
      # res = "grey70"
    )
  ) +
  theme_classic() +
  labs(
    x = "",
    y = expression(paste("Energy flux (W m"^-2, ")"))
  ) +
  facet_wrap(~sitename)

ggsave(
  here::here("book/images/seasonal_cycle_radiation.png"),
  width = 12, 
  height = 7
)

# # annual values: GPP vs PPFD
# adf_mean <- ddf |> 
#   unnest(data) |> 
#   mutate(year = year(TIMESTAMP)) |> 
#   group_by(sitename, year) |> 
#   summarise(
#     gpp = sum(GPP_NT_VUT_REF),
#     sw = sum(SW_IN_F_MDS)
#     ) |> 
#   ungroup() |> 
#   group_by(sitename) |> 
#   summarise(
#     gpp = mean(gpp),
#     sw = mean(sw)
#   )
# 
# adf_mean |> 
#   ggplot(aes(sw, gpp)) +
#   geom_point()


### Residual (ground heat flux) --------
# ground heat flux
ddf_sub |> 
  mutate(res = heat + le - netrad) |> 
  mutate(sitename = factor(
    sitename, 
    levels = c(
      "US-ICh",  # Tundra
      "FI-Hyy", # Boreal Forests/Taiga
      "BR-Sa3", # Tropical
      "DE-Hai", # Temperate Broadleaf & Mixed Forests
      "DE-Tha", # Temperate Coniferous
      "DE-Gri"  # Grassland just next to DE-Tha
    )
  )) |> 
  ggplot(aes(date, res)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_classic() +
  labs(
    x = "",
    y = expression(paste("Energy flux (W m"^-2, ")"))
  ) +
  facet_wrap(~sitename)

