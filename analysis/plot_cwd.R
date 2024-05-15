library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(cwd)
library(here)
library(cowplot)
library(recipes)

le_to_et <- function(le, tc, patm){
  1000 * 30 * 30 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
}

df <- read_csv(paste0(here(), "/data-raw/FLX_US-Ton_FLUXDATAKIT_FULLSET_DD_2001_2014_2-3.csv")) |>
  select(TIMESTAMP, P_F, TA_F_MDS, PA_F, LE_F_MDS, SW_IN_F_MDS, LW_IN_F_MDS, NETRAD, LE_CORR)

df <- df |> 
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    le_mm = le_to_et(LE_CORR, TA_F_MDS, PA_F)
  ) |> 
  mutate(
    wbal = P_F - le_mm,
    wsur = le_mm - P_F
  )

mdf <- df |> 
  mutate(
    year = year(TIMESTAMP),
    moy = month(TIMESTAMP)
  ) |> 
  group_by(year, moy) |> 
  summarise(
    P_F = sum(P_F),
    le_mm = sum(le_mm)
  ) |> 
  ungroup() |> 
  group_by(moy) |> 
  summarise(
    P_F = mean(P_F),
    le_mm = mean(le_mm)
  ) |> 
  pivot_longer(cols = c(P_F, le_mm)) |> 
  mutate(date = ymd("2023-01-01") + months(moy - 1))

gg1 <- mdf |> 
  ggplot(aes(date, value, fill = name)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_fill_manual(
    name = "",
    values = c(
      "le_mm" = "#E69F00",
      "P_F" = "#0072B2"
    ),
    labels = c(
      "le_mm" = "ET",
      "P_F" = expression(paste(italic(P)))
    )
  ) +
  labs(
    x = "",
    y = "Monthly total (mm)"
  )

## CWD -------------------
# Get cumulative water deficit time series
out_cwd <- cwd::cwd(
  df,
  varname_wbal = "wbal",
  varname_date = "TIMESTAMP",
  thresh_terminate = 0.0,
  thresh_drop = 0.0
)

# retain long events with accumulating water deficts
out_cwd$inst <- out_cwd$inst |>
  filter(len >= 20)

# visualise
gg2 <- ggplot() +
  geom_rect(
    data = out_cwd$inst,
    aes(xmin = date_start, xmax = date_end, ymin = -99, ymax = 99999),
    fill = rgb(0,0,0,0.3),
    color = NA) +
  geom_line(data  =  out_cwd$df, aes(TIMESTAMP, deficit), color = "tomato") +
  coord_cartesian(ylim = c(0, 80)) +
  theme_classic() +
  labs(
    x = "Date", 
    y = "Cumulative water deficit (mm)"
    )

## Plot ----------------
plot_grid(
  gg1,
  gg2,
  nrow = 2,
  labels = c("a", "b")
)

ggsave(here::here("book/images/cwd.png"), width = 7, height = 6)
