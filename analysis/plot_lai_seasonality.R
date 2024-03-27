library(FluxDataKit)
library(tidyverse)
library(cowplot)
source(here::here("R/read_fdk.R"))

# function based on FluxDataKit::fdk_match_modis()
get_modis <- function(
    df,  # a data frame with site info
    path # path where site-specific modis downloads are stored (used FluxDataKit)
    ){
  
  #----- settings and startup -----
  df_modis <- try(read.table(
    file.path(path, paste0(df["sitename"],"_MODIS_data.csv")),
    sep = ",",
    header = TRUE
  )
  )
  
  if (inherits(df_modis, "try-error")) {
    warning("MODIS data not found, please download data for this site.")
    return(NULL)
  }
  
  #----- QC screening -----
  
  # apply scaling factors, ignore if not available
  df_modis <- df_modis |>
    dplyr::mutate(
      scale = ifelse(scale == "Not Available", NA, scale),
      value = ifelse(!is.na(scale),
                     value * as.numeric(scale),
                     value),
      calendar_date = as.Date(calendar_date, "%Y-%m-%d")
    )
  
  df_modis <- df_modis |>
    tidyr::pivot_wider(
      id_cols = c(site, calendar_date, pixel),
      names_from = band,
      values_from = value
    )
  
  df_modis <- df_modis |>
    dplyr::rename(
      'lai' = 'Lai_500m',
      'sd_lai' = 'LaiStdDev_500m',
      'sd_fpar' = 'FparStdDev_500m',
      'fpar' = 'Fpar_500m',
      'qc' = 'FparLai_QC'
    )
  
  # Extracting pixels in the center and immediately around it (*)
  # These correspond to a radius of 500m around site coordinates
  pixel_no <- c(7, 8, 9,
                12, 13, 14,
                17, 18, 19)
  
  # Use only good quality data
  # Random bit integer format, ask Martin if need to work these out again...
  qc_flags <- c(0, 2, 24 ,26, 32, 34, 56, 58)
  
  df_modis <- df_modis |>
    filter(
      qc %in% qc_flags,
      pixel %in% pixel_no
    ) |>
    mutate(
      lai = ifelse(sd_lai < 0.01, NA, lai),
      lai = ifelse(lai > 10, NA, lai),
      fpar = ifelse(sd_fpar < 0.01, NA, fpar),
      fpar = ifelse(fpar > 1, NA, fpar)
    ) |>
    na.omit()
  
  #---- Apply weighted mean across pixels ----
  df_modis_mean <- df_modis |>
    group_by(site, calendar_date) |>
    mutate(
      weights_lai = (1/sd_lai^2)/sum(1/sd_lai^2),
      weights_fpar = (1/sd_fpar^2)/sum(1/sd_fpar^2),
    ) |>
    dplyr::summarize(
      lai = stats::weighted.mean(lai, w = weights_lai, na.rm = TRUE),
      fpar = stats::weighted.mean(fpar, w = weights_fpar, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(
      "date" = "calendar_date"
    )
  
  #---- expand dates ----
  
  # Use all available data, if required
  # expand to match the data range of the
  # flux data
  
  start_date <- as.Date(paste0(df['year_start'],"-01-01"))
  end_date <-as.Date(paste0(df['year_end'],"-12-31"))
  
  if (start_date > min(df_modis$calendar_date, na.rm = TRUE)){
    start_date <- min(df_modis$calendar_date, na.rm = TRUE)
  }
  
  if (end_date < min(df_modis$calendar_date, na.rm = TRUE)){
    end_date <- min(df_modis$calendar_date, na.rm = TRUE)
  }
  
  dates <- seq.Date(
    start_date - 100,
    end_date + 100,
    by = "day"
  )
  
  dates <- data.frame(
    date = dates,
    doy = as.numeric(format(dates, "%j"))
  )
  
  # only retain valid dates
  dates <- dates |>
    filter(
      doy %in% seq(1, 365, 8)
    )
  
  df_modis_mean <- dplyr::left_join(
    dates,
    df_modis_mean,
    by = "date"
  )
  
  return(df_modis_mean)
  
}

sites <- readRDS(here::here("data/df_sites_sub.rds"))

df_modis <- get_modis(
  sites |> 
    filter(sitename == "DE-Hai"),
  path = "~/data/FluxDataKit/FDK_inputs/modis/"
)

df_flx <- sites |> 
  filter(sitename == "DE-Hai") |> 
  select(sitename, lon, lat, elv, BIOME_NAME) |> 
  mutate(data = purrr::map(sitename, ~read_fdk(., path = "~/data/FluxDataKit/FLUXDATAKIT_FLUXNET"))) |> 
  unnest(data)

df_flx_msc <- df_flx |> 
  mutate(doy = lubridate::yday(TIMESTAMP)) |> 
  group_by(doy) |> 
  summarise(
    temp_min = min(TA_F_MDS, na.rm = TRUE),
    temp_max = max(TA_F_MDS, na.rm = TRUE),
    temp = mean(TA_F_MDS, na.rm = TRUE)
    ) |> 
  mutate(
    gdd = cumsum(ifelse(temp >= 5, temp - 5, 0))
  )
  
# get mean seasonality
df_modis_msc <- df_modis |> 
  mutate(doy = lubridate::yday(date)) |> 
  filter(!is.na(lai)) |>
  group_by(doy) |> 
  summarize(lai_min = quantile(lai, probs = 0.1, na.rm = TRUE),
            lai_max = quantile(lai, probs = 0.9, na.rm = TRUE),
            lai_mean = mean(lai, na.rm = TRUE)
            )

gg1 <- df_flx_msc |> 
  ggplot(aes(doy, temp)) +
  geom_ribbon(aes(ymin = temp_min, ymax = temp_max), fill = "grey") +
  geom_line() + 
  geom_hline(yintercept = 5, linetype = "dotted") +
  theme_classic() +
  labs(
    x = "",
    y = "Air temperature (°C)"
  )

gg2 <- df_flx_msc |> 
  ggplot(aes(doy, gdd)) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_classic() +
  labs(
    x = "",
    y = "GDD (°C)"
  )

gg3 <- df_modis_msc |> 
  ggplot(aes(doy, lai_mean)) +
  geom_ribbon(aes(ymin = lai_min, ymax = lai_max), fill = "grey") +
  # geom_point() +
  geom_line() + 
  theme_classic() +
  labs(
    x = "Day of year",
    y = "LAI"
  )

plot_grid(gg1, gg2, gg3, ncol = 1, labels = c("a", "b", "c"))

ggsave(here::here("book/images/lai_seasonality.png"), 
       width = 12, 
       height = 8
       )
