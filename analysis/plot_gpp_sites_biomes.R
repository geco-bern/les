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
  "BR-Sa3", # Tropical
  "US-ICh" # Tundra
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
      path = "~/data/FluxDataKit/v3.1/zenodo_upload/fluxnet/",
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
      path = "~/data/FluxDataKit/v3.1/zenodo_upload/fluxnet/",
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
  filter(day == 15, month == 7) |> 
  mutate(hm = as_hms(TIMESTAMP_START)) |> 
  group_by(sitename, hm) |> 
  summarise(
    # shortwave radiation
    sw = mean(SW_IN_F_MDS, na.rm = TRUE),
    sw_lo = quantile(SW_IN_F_MDS, 0.25, na.rm = TRUE),
    sw_hi = quantile(SW_IN_F_MDS, 0.75, na.rm = TRUE),
    
    # net radiation
    rn = mean(NETRAD, na.rm = TRUE),
    
    # evapotranspiration
    et = mean(LE_F_MDS, na.rm = TRUE),
    
    # leaf area index
    lai = mean(LAI, na.rm = TRUE),

    # gross primary production    
    gpp = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    gpp_lo = quantile(GPP_NT_VUT_REF, 0.25, na.rm = TRUE),
    gpp_hi = quantile(GPP_NT_VUT_REF, 0.75, na.rm = TRUE),
    
    # ecosystem respiration
    reco = mean(RECO_NT_VUT_REF, na.rm = TRUE),

    # net ecosystem exchange
    nee = mean(NEE_VUT_REF, na.rm = TRUE)
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
    # shortwave radiation
    sw = mean(SW_IN_F_MDS, na.rm = TRUE),
    
    # net radiation
    rn = mean(NETRAD, na.rm = TRUE),
    
    # evapotranspiration
    et = mean(LE_F_MDS, na.rm = TRUE),
    
    # leaf area index
    lai = mean(LAI, na.rm = TRUE),
    
    # gross primary production    
    gpp = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    
    # ecosystem respiration
    reco = mean(RECO_NT_VUT_REF, na.rm = TRUE),
    
    # net ecosystem exchange
    nee = mean(NEE_VUT_REF)
  ) |> 
  mutate(date = ymd("2023-01-01") + doy - 1)

# Daily C budget
daily_cbalance <- hdf_sub |> 
  group_by(sitename) |> 
  summarise(
    gpp = -sum(gpp),
    reco = sum(reco),
    nee = sum(nee)
    )

# Annual C budget
annual_cbalance <- ddf_sub |> 
  group_by(sitename) |> 
  summarise(
    gpp = -sum(gpp),
    reco = sum(reco),
    nee = sum(nee)
  )
# Plot -----------
## Diurnal cycle at mid-summer  -----------
### Shortwave radiation  -----------
gg1 <- hdf_sub |> 
  # ggplot(aes(hm, sw, color = sitename, ymin = sw_lo, ymax = sw_hi, fill = sitename)) +
  ggplot(aes(hm, sw, color = sitename)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  # geom_ribbon(alpha = .5, color = NA) +
  theme_classic() +
  # facet_wrap(~sitename) +
  scale_color_okabeito(name = "") +
  scale_fill_okabeito(name = "") +
  labs(
    x = "Time of day",
    y = expression(paste(italic("I")[0], " (W m"^-2, ")"))
  )

### GPP  -----------
gg2 <- hdf_sub |> 
  ggplot(aes(hm, gpp, color = sitename)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  theme_classic() +
  scale_color_okabeito(name = "") +
  labs(
    x = "Time of day",
    y = expression(paste("GPP (", mu, "mol m"^-2, "s"^-1, ")"))
  )

### NEE  -----------
gg3 <- hdf_sub |> 
  ggplot(aes(hm, nee, color = sitename)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line() +
  theme_classic() +
  scale_color_okabeito(name = "") +
  labs(
    x = "Time of day",
    y = expression(paste("NEE (", mu, "mol m"^-2, "s"^-1, ")"))
  )

### Daily total ----------
gg4 <- daily_cbalance |> 
  tidyr::pivot_longer(cols = 2:4) |> 
  ggplot(
    aes(
      sitename, value, fill = name
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_bar(
    position = "dodge", 
    stat = "identity"
  ) +
  khroma::scale_fill_highcontrast(
    name = "", 
    labels = c("GPP", "NEE", expression(paste(italic("R")[eco])))
  ) +
  theme_classic() +
  labs(
    y = expression(paste("C flux (", mu, "mol m"^-2, "d"^-1, ")")),
    x = ""
  )

### Combine ----------
legend <- get_legend(
  # create some space to the left of the legend
  gg1 + theme(legend.box.margin = margin(0, 0, 0, 12),
              legend.position = "bottom")
)

plot_grid(
  gg1 + theme(legend.position="none"), 
  gg2 + theme(legend.position="none"),
  gg4,
  gg3 + theme(legend.position="none"),
  legend,
  labels = c("a", "b", "c", "d", ""),
  hjust = -1,
  nrow = 3, 
  rel_heights = c(1, 1, 0.2)
  )

ggsave(
  here::here("book/images/diurnal_cycle.png"),
  width = 8, 
  height = 7
)

## Mean seasonal cycle  -----------
### Shortwave radiation  -----------
gg_sw <- ddf_sub |> 
  ggplot(aes(date, sw, color = sitename)) +
  geom_line() +
  theme_classic() +
  scale_color_okabeito(name = "") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    x = "",
    y = expression(paste(italic("I")[0], " (W m"^-2, ")"))
  )

### LAI  -----------
gg_lai <- ddf_sub |> 
  ggplot(aes(date, lai, color = sitename)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  scale_color_okabeito(name = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    x = "",
    y = expression(paste("LAI (m"^2, " m"^-2, ")"))
  )

### GPP  -----------
gg_gpp <- ddf_sub |> 
  ggplot(aes(date, gpp, color = sitename)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  scale_color_okabeito(name = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    x = "",
    y = expression(paste("GPP (", mu, "mol m"^-2, "s"^-1, ")"))
  )

### NEE  -----------
gg_nee <- ddf_sub |> 
  ggplot(aes(date, nee, color = sitename)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  scale_color_okabeito(name = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    x = "",
    y = expression(paste("NEE (", mu, "mol m"^-2, "s"^-1, ")"))
  )

### Annual total ----------
gg_cbal <- annual_cbalance |> 
  tidyr::pivot_longer(cols = 2:4) |> 
  ggplot(
    aes(
      sitename, value, fill = name
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_bar(
    position = "dodge", 
    stat = "identity"
  ) +
  khroma::scale_fill_highcontrast(
    name = "", 
    labels = c("-GPP", "NEE", expression(paste(italic("R")[eco])))
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  labs(
    y = expression(paste("C flux (", mu, "mol m"^-2, "yr"^-1, ")")),
    x = ""
  )

### Combine ----------
legend <- get_legend(
  # create some space to the left of the legend
  gg_sw + theme(legend.box.margin = margin(0, 0, 0, 12),
                legend.position = "bottom")
)

plot_grid(
  gg_sw + theme(legend.position="none"), 
  gg_lai + theme(legend.position="none"),
  gg_gpp + theme(legend.position="none"),
  gg_nee + theme(legend.position="none"),
  gg_cbal,  
  legend,
  labels = c("a", "b", "c", "d", "e", ""),
  # align = 'vh',
  # hjust = -1,
  nrow = 3
  # rel_heights = c(1, 1, 0.1)
)

ggsave(
  here::here("book/images/seasonal_cycle.png"),
  width = 8, 
  height = 9
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
