# Chapter 9: CO2 doubling, Planck feedback and ocean heat uptake.
# Run from the repository root: Rscript analysis/earth_system_feedbacks.R
# Load the tidyverse packages used here individually, as in other LES analyses.
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(ggplot2)
library(here)

source(here("R", "earth_system_model.R"))

# Experiments ---------------------------------------------------------------
# Store each parameter set and simulation in a list-column for easy comparison.
experiments <- tibble(mixing_years = c(500, 1500)) |>
  mutate(parameters = map(mixing_years, earth_system_parameters),
         simulation = map(parameters, ~ as_tibble(simulate_earth_system(p = .x))))

p <- experiments$parameters[[1]]
warming <- experiments$simulation[[1]]
control <- simulate_earth_system(years = 100, co2 = p$co2_0, p = p) |> as_tibble()
co2_doubled <- 2 * p$co2_0
delta_Q <- co2_radiative_forcing(co2_doubled, p)
delta_T_eq <- earth_system_equilibrium(co2_doubled, p) - p$T0

# Print forcing, feedback and equilibrium diagnostics with their units.
model_summary <- tibble(
  quantity = c("CO2 doubling forcing", "Planck feedback at T0", "Exact equilibrium warming",
               "Linearised equilibrium warming", "Total ocean heat capacity"),
  value = c(delta_Q, p$lambda_P, delta_T_eq, -delta_Q / p$lambda_P,
            (p$C_m + p$C_d) * p$area),
  unit = c("W m-2", "W m-2 K-1", "K", "K", "J K-1")
)
print(model_summary)
warming |>
  filter(year %in% c(0, 10, 100, 500, 1000, 3000)) |>
  select(year, delta_T, delta_Td, net_toa, ocean_heat_ZJ) |>
  print()

# Physical and numerical checks --------------------------------------------
stopifnot(max(abs(control$delta_T)) < 1e-10,
          abs(first(warming$net_toa) - delta_Q) < 1e-10,
          max(abs(warming$energy_residual_J_m2)) < 1,
          all(diff(warming$delta_T) >= -1e-10),
          abs(last(warming$delta_T) - delta_T_eq) < 0.01,
          abs(last(warming$net_toa)) < 0.01)

# Compare solutions at identical times after halving the integration step.
convergence <- simulate_earth_system(years = 100, dt = 0.125, p = p) |>
  as_tibble() |>
  select(year, fine = delta_T) |>
  inner_join(select(warming, year, coarse = delta_T), by = "year")
stopifnot(max(abs(convergence$fine - convergence$coarse)) < 1e-5)

# Tidy data for time-series plots -------------------------------------------
temperatures <- warming |>
  select(year, `Surface air / mixed layer` = delta_T, `Deep ocean` = delta_Td) |>
  pivot_longer(-year, names_to = "reservoir", values_to = "warming")
mixing_comparison <- experiments |>
  select(mixing_years, simulation) |>
  unnest(simulation) |>
  mutate(scenario = paste(mixing_years, "years"))
fluxes <- warming |>
  select(year, `TOA imbalance` = net_toa, `Planck response` = planck_response,
         `Deep ocean uptake` = deep_ocean_uptake, `CO2 forcing` = forcing) |>
  pivot_longer(-year, names_to = "flux", values_to = "value")

# Store the theme locally rather than changing the user's global ggplot theme.
demo_theme <- theme_classic(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold"))

plot_temperature <- ggplot(temperatures, aes(year, warming, colour = reservoir)) +
  geom_hline(yintercept = delta_T_eq, linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Deep ocean" = "#24689B",
                                 "Surface air / mixed layer" = "#B43C39")) +
  labs(x = "Years after CO2 doubling", y = "Temperature change (K)",
       title = "Planck-only adjustment", subtitle = "Dotted line: final equilibrium") +
  demo_theme

plot_mixing <- ggplot(mixing_comparison, aes(year, delta_T, colour = scenario)) +
  geom_hline(yintercept = delta_T_eq, linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  coord_cartesian(xlim = c(0, 200)) +
  scale_colour_manual(values = c("500 years" = "#B43C39", "1500 years" = "#A27520")) +
  labs(x = "Years after CO2 doubling", y = "Surface-air warming (K)",
       title = "Mixing controls the transient", subtitle = "Legend: deep-ocean mixing time") +
  demo_theme

plot_fluxes <- ggplot(fluxes, aes(year, value, colour = flux, linetype = flux)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("CO2 forcing" = "black", "Deep ocean uptake" = "#24689B",
                                 "Planck response" = "#A27520", "TOA imbalance" = "#B43C39")) +
  scale_linetype_manual(values = c("CO2 forcing" = "dotted", "Deep ocean uptake" = "dashed",
                                   "Planck response" = "solid", "TOA imbalance" = "solid")) +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  labs(x = "Years after CO2 doubling", y = expression("Energy flux (W " * m^-2 * ")"),
       title = "Radiation and heat uptake") + demo_theme

plot_heat <- ggplot(warming, aes(year, ocean_heat_ZJ)) +
  geom_line(colour = "#24689B", linewidth = 0.8) +
  labs(x = "Years after CO2 doubling", y = "Ocean heat gain (ZJ)",
       title = "Accumulated ocean heat uptake") + demo_theme

plot_overview <- cowplot::plot_grid(plot_temperature, plot_mixing, plot_fluxes, plot_heat,
                                   ncol = 2, align = "hv")

# Forcing-feedback (Gregory) plot, as in LES Figure 9.7 -----------------------
# These are instantaneous annual samples, not annual means or noisy observations.
annual <- warming |>
  filter(year >= 1, year %% 1 == 0) |>
  mutate(period = if_else(year <= 150, "Years 1-150", "Years 151-3000"))

# A Gregory regression estimates the forcing, feedback and zero-flux intercept.
# The exact T^4 model is slightly curved, so this fitted slope is an approximation.
gregory_fit <- lm(net_toa ~ delta_T, data = filter(annual, year <= 150))
gregory_summary <- tibble(
  forcing_estimate = unname(coef(gregory_fit)[1]),
  feedback_estimate = unname(coef(gregory_fit)[2]),
  equilibrium_estimate = -forcing_estimate / feedback_estimate
)
print(gregory_summary)

# The analytic curve reaches true equilibrium, beyond the finite simulation.
radiation_curve <- tibble(delta_T = seq(0, delta_T_eq, length.out = 300)) |>
  mutate(net_toa = delta_Q - p$epsilon * p$sigma * ((p$T0 + delta_T)^4 - p$T0^4))

plot_gregory <- ggplot(radiation_curve, aes(delta_T, net_toa)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = 0, colour = "grey75") +
  geom_line(linewidth = 0.8) +
  geom_abline(intercept = gregory_summary$forcing_estimate,
              slope = gregory_summary$feedback_estimate,
              linetype = "dashed", colour = "#A27520", linewidth = 0.7) +
  geom_point(data = annual, aes(colour = period), alpha = 0.5, size = 1.9) +
  annotate("point", x = 0, y = delta_Q, colour = "#CC672D", size = 3) +
  annotate("point", x = delta_T_eq, y = 0, shape = 4, size = 4, stroke = 1.2) +
  annotate("text", x = 0.03, y = 4.13, hjust = 0, size = 3.8,
           label = sprintf("Forcing = %.3f W/m² at zero warming", delta_Q)) +
  annotate("text", x = 0.56, y = 2.95, hjust = 0, size = 3.8,
           label = sprintf("Planck slope at T0 = %.3f W/m²/K", p$lambda_P)) +
  annotate("text", x = 0.56, y = 2.58, hjust = 0, size = 3.5, colour = "#A27520",
           label = sprintf("Dashed: years 1-150 fit (slope %.3f)",
                           gregory_summary$feedback_estimate)) +
  annotate("segment", x = 0.25, xend = 0.43,
           y = delta_Q + p$lambda_P * 0.25, yend = delta_Q + p$lambda_P * 0.43,
           arrow = grid::arrow(length = grid::unit(0.13, "inches")), linewidth = 0.7) +
  annotate("text", x = 0.10, y = 2.2, hjust = 0, label = "Time →", size = 3.8) +
  annotate("text", x = 1.28, y = 0.85, hjust = 1, size = 3.8,
           label = sprintf("Planck-only equilibrium\nΔT = %.3f K; N = 0", delta_T_eq)) +
  annotate("segment", x = 1.13, xend = delta_T_eq, y = 0.58, yend = 0.10,
           arrow = grid::arrow(length = grid::unit(0.1, "inches"))) +
  scale_colour_manual(values = c("Years 1-150" = "#3993C1", "Years 151-3000" = "#62558C")) +
  coord_cartesian(xlim = c(-0.03, 1.32), ylim = c(-0.3, 4.4), expand = FALSE) +
  labs(x = expression("Global-mean surface-air temperature change, " * Delta*T * " (K)"),
       y = expression("Net downward TOA radiation, N (W " * m^-2 * ")"),
       title = "CO2 doubling: forcing, feedback and equilibrium",
       subtitle = "Black curve: exact radiation balance; points: annual simulation samples",
       caption = "Planck feedback only. Ocean mixing changes the speed along the curve, not the curve itself.") +
  demo_theme

# Save reproducible outputs -------------------------------------------------
output_dir <- here("fig", "earth_system_feedbacks")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(warming, file.path(output_dir, "co2_doubling.csv"))
write_csv(gregory_summary, file.path(output_dir, "gregory_diagnostics.csv"))
ggsave(file.path(output_dir, "co2_doubling.png"), plot_overview,
       width = 11, height = 8, dpi = 160, bg = "white")
ggsave(file.path(output_dir, "toa_radiation_temperature.png"), plot_gregory,
       width = 10, height = 6.5, dpi = 180, bg = "white")
if (interactive()) {
  print(plot_overview)
  print(plot_gregory)
}
