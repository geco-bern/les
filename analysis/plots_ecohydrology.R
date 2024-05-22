library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(cwd)
library(here)
library(cowplot)
library(recipes)

## Read data ----------------
df_ton <- read_csv(here("data-raw/FLX_US-Ton_FLUXDATAKIT_FULLSET_DD_2001_2014_2-3.csv")) |>
  select(TIMESTAMP, P_F, TA_F_MDS, PA_F, LE_F_MDS, SW_IN_F_MDS, LW_IN_F_MDS, NETRAD, LE_CORR)

# df_var <- read_csv(here("data-raw/FLX_US-Var_FLUXDATAKIT_FULLSET_DD_2000_2021_2-3.csv")) |>
#   select(TIMESTAMP, P_F, TA_F_MDS, PA_F, LE_F_MDS, SW_IN_F_MDS, LW_IN_F_MDS, NETRAD, LE_CORR) |> 
#   filter(year(TIMESTAMP) > 2000)

## Complement variables --------------
le_to_et <- function(le, tc, patm){
  1000 * 30 * 30 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
}

df_ton <- df_ton |> 
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    le_mm = le_to_et(LE_CORR, TA_F_MDS, PA_F)
  ) |> 
  mutate(
    wbal = P_F - le_mm,
    wsur = le_mm - P_F
  )

# df_var <- df_var |> 
#   mutate(
#     # convert latent heat flux into mass flux in mm day-1
#     le_mm = le_to_et(LE_CORR, TA_F_MDS, PA_F)
#   ) |> 
#   mutate(
#     wbal = P_F - le_mm,
#     wsur = le_mm - P_F
#   )

## CWD -------------------
# Get cumulative water deficit time series
out_cwd_ton <- cwd::cwd(
  df_ton,
  varname_wbal = "wbal",
  varname_date = "TIMESTAMP",
  thresh_terminate = 0.0,
  thresh_drop = 0.0
)

# out_cwd_var <- cwd::cwd(
#   df_var,
#   varname_wbal = "wbal",
#   varname_date = "TIMESTAMP",
#   thresh_terminate = 0.0,
#   thresh_drop = 0.0
# )

# retain only data from events with largest deficit per year
# tonzi
out_cwd_ton$inst <- out_cwd_ton$inst |>
  mutate(year = year(date_start)) |> 
  group_by(year) |> 
  filter(deficit == max(deficit))

out_cwd_ton$df <- out_cwd_ton$df |> 
  filter(iinst %in% out_cwd_ton$inst$iinst)

# # vaira
# out_cwd_var$inst <- out_cwd_var$inst |>
#   mutate(year = year(date_start)) |> 
#   group_by(year) |> 
#   filter(deficit == max(deficit))
# 
# out_cwd_var$df <- out_cwd_var$df |> 
#   filter(iinst %in% out_cwd_var$inst$iinst)

# visualise ET decay per site for multiple years
# tonzi
tmp <- out_cwd_ton$df |> 
  mutate(year = year(TIMESTAMP)) |> 
  filter(doy > 100, doy < 300)

tmp_agg <- tmp |> 
  group_by(dday) |> 
  summarise(
    le_mm = mean(le_mm, na.rm = TRUE)
  )

## Plots -----------
### Time series ------------
gg0 <- ggplot() +
  geom_rect(
    data = out_cwd_ton$inst |> 
      filter(year %in% (2009:2013)),
    aes(xmin = date_start, xmax = date_end, ymin = -0.1, ymax = 1.7),
    fill = rgb(0,0,0,0.3),
    color = NA) +
  geom_line(
    data = df_ton |> 
      filter(year(TIMESTAMP) %in% (2009:2013)),
    aes(TIMESTAMP, le_mm),
    color = "royalblue"
  ) +
  theme_classic() +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  labs(
    x = "Date",
    y = expression(paste(italic("E"), " (mm d"^-1, ")"))
  )

### Decay ------------
gg1 <- ggplot() +
  geom_line(
    data = tmp, 
    aes(dday, le_mm, group = iinst), 
    alpha= 0.2
    ) +
  geom_line(
    data = tmp_agg,
    aes(dday, le_mm),
    color = "royalblue"
  ) +
  xlim(0, 170) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(
    x = "Day into dry season",
    y = expression(paste(italic("E"), " (mm d"^-1, ")"))
  )

gg2 <- ggplot() +
  geom_line(
    data = tmp, 
    aes(dday, le_mm, group = iinst), 
    alpha= 0.2
  ) +
  geom_line(
    data = tmp_agg,
    aes(dday, le_mm),
    color = "royalblue"
  ) +
  theme_classic() +
  scale_y_log10(limits = c(0.01, NA)) +
  xlim(0, 170) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(
    x = "Day into dry season",
    y = expression(paste(italic("E"), " (mm d"^-1, ")"))
  )


bottomrow <- plot_grid(gg1, gg2, labels = c("b", "c"))
plot_grid(gg0, bottomrow, nrow = 2, labels = c("a", NA, NA), rel_heights = c(0.7, 1))

ggsave(here::here("book/images/etdecay.png"), width = 8, height = 6)
