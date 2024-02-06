# libraries--------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(here)
library(cowplot)
library(readr)

# Model-----------
## 1-box model--------------------------------------------------------------------------------------------------------------------------------------------
onebox <- function( c_influx, tau, cpool0, return_cpool = TRUE, dist = NA) {
  # ------------------------------------------------
  # c_influx:  flux into pool (GtC a-1)
  # cpool0:    initial pool size (GtC)
  # tau:       turnover (residence) time (a)
  # ------------------------------------------------
  tauisvec <- ifelse(length(tau)==length(c_influx), TRUE, FALSE)

  # determine integration length (number of time steps) from length of 'c_influx'
  len <- length(c_influx)

  # initialise output variable (time series of pool size)
  out_cpool <- rep( NA, len )
  out_outflux <- rep(NA, len)
  
  # copy initial pool size to first element in output time series
  cpool <- cpool0

  # integrate over each time step (this is an implementation of the differential equation)
  for (yr in seq(len) ) {
    
    print(paste("yr, dist: ", yr, dist))
    if (yr == dist){
      cpool <- 0 
    }

    # copy current pool size to output time series
    out_cpool[yr] <- cpool
    
    # outflux
    outflux <- 1/ifelse(tauisvec, tau[yr], tau) * cpool

    # update pool size with input and decay
    cpool <- cpool + c_influx[yr] - outflux
    out_outflux[yr] <- outflux

  }

  # function return value is a vector containing the output time series
  if (return_cpool){
    return( out_cpool )
  } else {
    return( out_outflux )
  }

}

# Simulations----------
# One-box response to CO2 trajectories------------------
# read historical CO2 time series (165 years), standard CMIP6 from Meinshausen et al. 2017
df_co2 <- read_csv(here("data/Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017_tab_historical-annualmean-Global.csv")) |> 
  select(year, co2 = CO2) |> 
  bind_rows(
    # get Mauna Loa CO2 data to extend the CMIP dataset (which goes only to 2014)
    read_csv(here("data/co2_mm_mlo.csv"),
             skip = 40) |> 
      group_by(year) |> 
      summarise(co2 = mean(average)) |> 
      filter(year %in% 2015:2023)
  )


# extend df_co2 to create forcing, covering also the spinup period at constant 
# CO2 corresponding to the first year in the observations
firstyeartrend <- df_co2$year[1]
co2_init <- df_co2$co2[1]
nyears_spinup <- 500

df <- tibble(
  year = (firstyeartrend - nyears_spinup):(firstyeartrend-1),
  co2 =  co2_init) |> 
  bind_rows(
    df_co2
  )

## Calculate GPP-------

# calculate GPP as a function of CO2, given the sensitivity beta
calc_gpp <- function(co2, co2_init, gpp_init, beta){
  # solve for GPP(t) using Eq. 1 in Keenan et al., 2023
  gpp <- beta * gpp_init/co2_init * (co2 - co2_init) + gpp_init
  return(gpp)
}

# assume influx is preindustrial GPP (113 GtC), increased by the CO2 response
# relative to the preindustrial CO2 level
gpp_preindustrial <- 113

df <- df |> 
  mutate(gpp = calc_gpp(co2, 
                        co2_init = co2_init, 
                        gpp_init = gpp_preindustrial, 
                        beta = 0.59))



## Simulate C balance--------------
# apply the 1-box model assuming a turnover time to match vegetation and soil C 
# pool size from IPCC, given preindustrial GPP. Spinup from zero C.
vegc <- 450
soilc <- 1700
# tau <- (vegc + soilc) / gpp_preindustrial  # 19 years
tau <- 7

df <- df |>
  mutate(c_pool = onebox(c_influx = gpp, 
                         tau = tau, 
                         cpool0 = 0),
         c_outflux = onebox(c_influx = gpp, 
                            tau = tau, 
                            cpool0 = 0, 
                            return_cpool = FALSE))  |> 
  mutate(sink = c_pool - lag(c_pool))


# Scenarios -------------
## Constant at current levels----------
lastyeartrend <- df$year[nrow(df)]
nyears_scenario <- 200
df_const <- df |> 
  bind_rows(
    tibble(
      year = (lastyeartrend + 1):(lastyeartrend + nyears_scenario),
      co2 = rep(df$co2[nrow(df)], nyears_scenario)
    )
  ) 

df_const <- df_const |> 
  mutate(gpp = calc_gpp(co2, 
                        co2_init = co2_init, 
                        gpp_init = gpp_preindustrial, 
                        beta = 0.59))

df_const <- df_const |>
  mutate(c_pool = onebox(c_influx = gpp, 
                         tau = tau, 
                         cpool0 = 0),
         c_outflux = onebox(c_influx = gpp, 
                            tau = tau, 
                            cpool0 = 0, 
                            return_cpool = FALSE))  |> 
  mutate(sink = c_pool - lag(c_pool))

## Reduction ----------
# to pre-industrial levels
lastyeartrend <- df$year[nrow(df)]
nyears_scenario <- 200
df_reduce <- df |> 
  bind_rows(
    tibble(
      year = (lastyeartrend + 1):(lastyeartrend + nyears_scenario),
      co2 = (df$co2[nrow(df)] - df$co2[1]) * exp(-0.1 * seq(nyears_scenario)) + df$co2[1]
    )
  ) 

df_reduce <- df_reduce |> 
  mutate(gpp = calc_gpp(co2, 
                        co2_init = co2_init, 
                        gpp_init = gpp_preindustrial, 
                        beta = 0.59))

df_reduce <- df_reduce |>
  mutate(c_pool = onebox(c_influx = gpp, 
                         tau = tau, 
                         cpool0 = 0),
         c_outflux = onebox(c_influx = gpp, 
                            tau = tau, 
                            cpool0 = 0, 
                            return_cpool = FALSE))  |> 
  mutate(sink = c_pool - lag(c_pool))


## Linear increase ----------
# Continued linear increase
trend <- lm(co2 ~ year, data = filter(df, year > 1990))$coefficients["year"]
lastyeartrend <- df$year[nrow(df)]
nyears_scenario <- 200
df_lin <- df |> 
  bind_rows(
    tibble(
      year = (lastyeartrend + 1):(lastyeartrend + nyears_scenario),
      co2 = df$co2[nrow(df)] + trend * seq(nyears_scenario)
    )
  ) 

df_lin <- df_lin |> 
  mutate(gpp = calc_gpp(co2, 
                        co2_init = co2_init, 
                        gpp_init = gpp_preindustrial, 
                        beta = 0.59))

df_lin <- df_lin |>
  mutate(c_pool = onebox(c_influx = gpp, 
                         tau = tau, 
                         cpool0 = 0),
         c_outflux = onebox(c_influx = gpp, 
                            tau = tau, 
                            cpool0 = 0, 
                            return_cpool = FALSE))  |> 
  mutate(sink = c_pool - lag(c_pool))

## Exponential increase ----------
# Continued linear increase
trend <- lm(co2 ~ year, data = filter(df, year > 1960))$coefficients["year"]
lastyeartrend <- df$year[nrow(df)]
nyears_scenario <- 200
df_exp <- df |> 
  bind_rows(
    tibble(
      year = (lastyeartrend + 1):(lastyeartrend + nyears_scenario),
      co2 = df$co2[nrow(df)] * exp(0.008 * seq(nyears_scenario))
    )
  ) 

df_exp <- df_exp |> 
  mutate(gpp = calc_gpp(co2, 
                        co2_init = co2_init, 
                        gpp_init = gpp_preindustrial, 
                        beta = 0.59))

df_exp <- df_exp |>
  mutate(c_pool = onebox(c_influx = gpp, 
                         tau = tau, 
                         cpool0 = 0),
         c_outflux = onebox(c_influx = gpp, 
                            tau = tau, 
                            cpool0 = 0, 
                            return_cpool = FALSE))  |> 
  mutate(sink = c_pool - lag(c_pool))


# Compare to GCB--------
# Data from Friedlingstein et al. 2023. Downloaded excel file 'Global 
# Carbon Budget v2023' from
# https://globalcarbonbudgetdata.org/latest-data.html
# Saved tab Global Carbon Budget as a CSV file. 
# Reading this CSV here.
df_gcb <- read_csv(here("data/Global_Carbon_Budget_2023v1.0_tabGlobalCarbonBudget.csv"))

# (Re-) definitions
df_gcb <- df_gcb |> 
  
  # Include the carbonation sink in the fossil fuel emissions
  mutate(e_ff = e_ff - s_cement) |> 
  
  # Land sink defined as the budget residual
  mutate(s_res = e_ff + e_luc - g_atm - s_ocean)

df_plot <- df_exp

make_plots <- function(df_plot, title){
  
  ### co2---------
  gg1 <- ggplot() +
    geom_line(aes(year, co2),
              data = df_plot |> 
                filter(year >= 1850),
              color = "black"
    ) +
    theme_classic() +
    labs(x = "Year", 
         y = expression(paste("CO"[2], " (ppm)")),
         subtitle = expression(paste("Atmospheric CO"[2])),
         title = title)
  
  
  ### GPP--------
  gg2 <- ggplot() +
    geom_line(aes(year, gpp),
              data = df_plot |> 
                filter(year >= 1850),
              color = "black"
    ) +
    theme_classic() +
    labs(x = "Year", 
         y = expression(paste("GPP (PgC yr"^-1, ")")),
         subtitle = expression(paste("Global GPP")))
  
  
  ### pool size---------
  gg3 <- ggplot() +
    geom_line(aes(year, c_pool),
              data = df_plot |> 
                filter(year >= 1850),
              color = "black"
    ) +
    theme_classic() +
    labs(x = "Year", 
         y = "Terrestrial C (PgC)",
         subtitle = "Terrestrial C pool")
  
  
  ### Sink------------
  gg4 <- ggplot() +
    geom_line(aes(year, s_res),
              data = df_gcb,
              color = "yellowgreen") +
    geom_line(aes(year, sink),
              data = df_plot |> 
                filter(year >= 1850),
              color = "black"
    ) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_classic() +
    labs(x = "Year", 
         y = expression(paste("Land sink (PgC yr"^-1, ")")),
         subtitle = "Land sink"
    )
  
  column <- plot_grid(gg1, gg2, gg3, gg4, ncol = 1)
  
  return(column)
  
}

# Plot--------------
## constant------
column2 <- make_plots(df_const, title = "Constant concentration")

## reduction------
column3 <- make_plots(df_reduce, title = "Reversal to preindustrial")

## linear increase--------
column4 <- make_plots(df_lin, title = "Linear increase")

## exponential increase--------
column5 <- make_plots(df_exp, title = "Exponential increase")

mygrid <- plot_grid(column2, column3, column4, column5, ncol = 4)

title <- ggdraw() + 
  draw_label(
    expression(paste("Atmospheric CO"[2], " trajectories and the land C cycle response (1-box model)")),
    fontface = 'bold',
    size = 18,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, 
  mygrid,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.04, 1)
)

ggsave(here("book/images/landsink_co2trajectories.png"), width = 12, height = 10)

# Disturbance-----------
tau <- 10
df_dist <- tibble(
  c_pool = onebox(c_influx = rep(1, 100), 
                  tau = tau, 
                  cpool0 = tau, 
                  dist = 10))  |> 
  mutate(sink = c_pool - lag(c_pool),
         t = 1:n())

df_dist |> 
  ggplot(aes(t, c_pool/tau)) +
  geom_line() +
  theme_classic() +
  labs(x = "Time", y = "C pool")

ggsave(here("book/images/disturbance_recovery.png"), width = 4, height = 3)
