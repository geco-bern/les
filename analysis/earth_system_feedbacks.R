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

# Component sketch ---------------------------------------------------------
# Circular Earth: projected interception area versus emitting surface area.
angle <- seq(0, 2 * pi, length.out = 361)
earth_outline <- tibble(x = 2.2 + 2 * cos(angle), y = 2 * sin(angle))
deep_outline <- tibble(x = 2.2 + 1.05 * cos(angle), y = 1.05 * sin(angle))
interception_disk <- tibble(x = -.7 + .18 * cos(angle), y = 2 * sin(angle))
solar_rays <- tibble(y = seq(-1.8, 1.8, length.out = 7)) |>
  mutate(x = -4.1, xend = 2.2 - sqrt(4 - y^2), yend = y)
lw_rays <- tibble(angle = c(-120, -80, -40, 0, 40, 80, 120) * pi / 180) |>
  transmute(x = 2.2 + 2.05 * cos(angle), y = 2.05 * sin(angle),
            xend = 2.2 + 2.85 * cos(angle), yend = 2.85 * sin(angle))
plot_components <- ggplot() +
  geom_segment(data = solar_rays, aes(x, y, xend = xend, yend = yend),
    colour = "#D79922", linewidth = .65,
    arrow = grid::arrow(length = grid::unit(.10, "inches"))) +
  geom_polygon(data = interception_disk, aes(x, y), fill = "#f4cf79", alpha = .7,
    colour = "#ad6631", linewidth = .6) +
  geom_polygon(data = earth_outline, aes(x, y), fill = "#cce5ee", colour = "#24689B", linewidth = .8) +
  geom_polygon(data = deep_outline, aes(x, y), fill = "#78adcc", colour = "#24689B", linewidth = .5) +
  geom_segment(data = lw_rays, aes(x, y, xend = xend, yend = yend),
    colour = "#a83232", linewidth = .8,
    arrow = grid::arrow(length = grid::unit(.12, "inches"))) +
  annotate("text", x = -2.8, y = 2.5, label = "S[0]", parse = TRUE, size = 5) +
  annotate("text", x = -2.8, y = 2.9, label = "Parallel solar rays", size = 3.8) +
  annotate("text", x = -.7, y = -2.5, label = "A[disc] == pi*R^2", parse = TRUE, size = 4.5) +
  annotate("text", x = -.7, y = -2.9, label = "Projected disk (shown obliquely)", size = 3.3) +
  annotate("text", x = 2.2, y = 1.55, label = "T", parse = TRUE, size = 6) +
  annotate("text", x = 2.2, y = .0, label = "T[d]", parse = TRUE, size = 6) +
  annotate("segment", x = 2.2, xend = 2.2, y = 1.3, yend = .65,
    arrow = grid::arrow(length = grid::unit(.12, "inches")), linewidth = .7) +
  annotate("text", x = 2.4, y = .95, hjust = 0, label = "H", parse = TRUE, size = 5) +
  annotate("text", x = 5.3, y = 2.15, hjust = 0, label = "Q[LW]", parse = TRUE,
    colour = "#a83232", size = 5) +
  annotate("text", x = 5.3, y = 1.65, hjust = 0, label = "Radial emission over", size = 3.6) +
  annotate("text", x = 5.3, y = 1.2, hjust = 0, label = "A[E] == 4*pi*R^2", parse = TRUE, size = 4.5) +
  annotate("text", x = 5.3, y = .2, hjust = 0,
    label = "Q[SW] == frac((1-alpha)*S[0],4)", parse = TRUE, size = 4.6) +
  annotate("text", x = 5.3, y = -.75, hjust = 0,
    label = "N == Q[SW]-Q[LW]", parse = TRUE, size = 4.6) +
  annotate("text", x = 5.3, y = -1.5, hjust = 0,
    label = "H == kappa*(T-T[d])", parse = TRUE, size = 4.6) +
  coord_fixed(xlim = c(-4.4, 9.8), ylim = c(-3.3, 3.3), expand = FALSE, clip = "off") +
  theme_void() +
  labs(caption = "Outer layer: surface air / mixed layer; inner circle: deep ocean (schematic, not to scale).\nSolar power is intercepted over πR²; fluxes QSW, QLW, N and H are normalised by 4πR². Reflected sunlight is excluded from QSW.") +
  theme(plot.caption = element_text(hjust = .5, size = 10), plot.margin = margin(12, 15, 12, 15))

# Additional feedback: ice-albedo ------------------------------------------
# AR6 WGI 7.4.2.3: surface-albedo feedback +0.35 W m-2 K-1.
# Match the nonlinear function's derivative at T0 to that assessed coefficient.
lambda_ice_ar6 <- 0.35
p_ice <- earth_system_parameters(ice_albedo = TRUE,
  albedo_amplitude = 4 * lambda_ice_ar6 * 10 / p$S0)
warming_ice <- simulate_earth_system(p = p_ice) |> as_tibble()
control_ice <- simulate_earth_system(years = 100, co2 = p_ice$co2_0, p = p_ice)
delta_T_eq_ice <- earth_system_equilibrium(co2_doubled, p_ice) - p_ice$T0
ice_convergence <- simulate_earth_system(years = 100, dt = .125, p = p_ice) |>
  as_tibble() |> select(year, fine = delta_T) |>
  inner_join(select(warming_ice, year, coarse = delta_T), by = "year")
stopifnot(max(abs(control_ice$delta_T)) < 1e-10,
          abs(first(warming_ice$net_toa) - delta_Q) < 1e-10,
          max(abs(warming_ice$energy_residual_J_m2)) < 1,
          max(abs(ice_convergence$fine - ice_convergence$coarse)) < 1e-5,
          abs(last(warming_ice$delta_T) - delta_T_eq_ice) < .02,
          abs(last(warming_ice$net_toa)) < .02,
          delta_T_eq_ice > delta_T_eq,
          all(warming_ice$albedo > 0 & warming_ice$albedo < 1),
          max(abs(warming_ice$net_toa - warming_ice$forcing -
                    warming_ice$planck_response - warming_ice$ice_albedo_response)) < 1e-10)

feedback_comparison <- bind_rows(
  mutate(warming, scenario = "Planck only"),
  mutate(warming_ice, scenario = "Planck + ice–albedo"))
feedback_diagnostics <- tibble(
  scenario = c("Planck only", "Planck + ice–albedo"),
  forcing_W_m2 = delta_Q,
  initial_feedback_W_m2_K = c(p$lambda_P, p_ice$lambda_P + p_ice$lambda_ice),
  equilibrium_warming_K = c(delta_T_eq, delta_T_eq_ice))
feedback_colours <- c("Planck only" = "#24689B", "Planck + ice–albedo" = "#B43C39")
plot_ice_temperature <- ggplot(feedback_comparison, aes(year, delta_T, colour = scenario)) +
  geom_hline(data = feedback_diagnostics, aes(yintercept = equilibrium_warming_K, colour = scenario),
             linetype = "dotted") +
  geom_line(linewidth = .9) +
  scale_colour_manual(values = feedback_colours) +
  labs(x = "Years after CO2 doubling", y = "Surface-air warming (K)",
       title = "An additional positive feedback amplifies warming",
       subtitle = "Same CO2 forcing and ocean mixing; dotted lines mark exact equilibria") + demo_theme

feedback_curves <- tibble(scenario = names(feedback_colours),
                         parameters = list(p, p_ice),
                         equilibrium = c(delta_T_eq, delta_T_eq_ice)) |>
  mutate(curve = map2(parameters, equilibrium, function(parameters, equilibrium) {
    tibble(delta_T = seq(0, equilibrium, length.out = 300)) |>
      mutate(net_toa = earth_system_fluxes(parameters$T0 + delta_T,
        parameters$T0 + delta_T, co2_doubled, parameters)$net_toa)
  })) |>
  select(scenario, curve) |> unnest(curve)
feedback_annual <- feedback_comparison |> filter(year >= 1, year <= 150, year %% 1 == 0)
feedback_fits <- feedback_annual |>
  group_by(scenario) |>
  group_modify(~ {
    fit <- lm(net_toa ~ delta_T, data = .x)
    tibble(fitted_forcing_W_m2 = unname(coef(fit)[1]),
           fitted_feedback_W_m2_K = unname(coef(fit)[2]))
  }) |> ungroup()
feedback_diagnostics <- feedback_diagnostics |> left_join(feedback_fits, by = "scenario")
plot_ice_gregory <- ggplot(feedback_curves, aes(delta_T, net_toa, colour = scenario)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_abline(data = feedback_fits, aes(intercept = fitted_forcing_W_m2,
    slope = fitted_feedback_W_m2_K, colour = scenario), linetype = "dashed", linewidth = .6) +
  geom_point(data = feedback_annual, size = 1.5, alpha = .5) +
  geom_point(data = feedback_diagnostics, aes(x = equilibrium_warming_K, y = 0),
             shape = 4, size = 4, stroke = 1.2) +
  annotate("point", x = 0, y = delta_Q, colour = "black", size = 2.5) +
  scale_colour_manual(values = feedback_colours) +
  coord_cartesian(xlim = c(0, delta_T_eq_ice * 1.08), ylim = c(-.2, delta_Q * 1.08)) +
  labs(x = expression("Surface-air temperature change, " * Delta*T * " (K)"),
       y = expression("Net downward TOA radiation, N (W " * m^-2 * ")"),
       title = "Ice–albedo feedback in the Gregory plot",
       subtitle = "Same forcing intercept; weaker restoring slope; larger equilibrium warming",
       caption = "Solid: exact radiation balance. Points and dashed regression: years 1–150. Crosses: exact equilibria.") + demo_theme


# AR6 feedback parameter versus dimensionless feedback factor ----------------
# The assessed +0.35 is dimensional; f_ice = -lambda_ice/lambda_P is dimensionless.
feedback_factor_ice_ar6 <- -lambda_ice_ar6 / p$lambda_P
lambda_linear <- p$lambda_P + lambda_ice_ar6
equilibrium_linear <- -delta_Q / lambda_linear
# Exact solution of the LINEARISED two-box model, independent of the RK4 solver:
# dx/dt = A x + b. Both reservoirs tend to equilibrium_linear.
linear_operator <- matrix(c((lambda_linear - p$kappa) / p$C_m,
                            p$kappa / p$C_d, p$kappa / p$C_m,
                            -p$kappa / p$C_d), nrow = 2)
modes <- eigen(linear_operator)
coefficients <- solve(modes$vectors, rep(-equilibrium_linear, 2))
decay <- exp(outer(modes$values, warming_ice$year * p$seconds_per_year))
linear_state <- modes$vectors %*% (coefficients * decay) + equilibrium_linear
linear_ice <- tibble(year = warming_ice$year, delta_T = linear_state[1, ],
                    delta_Td = linear_state[2, ]) |>
  mutate(net_toa = delta_Q + lambda_linear * delta_T)
linear_comparison <- bind_rows(
  warming_ice |> transmute(year, delta_T, net_toa, representation = "Nonlinear (AR6-calibrated)"),
  linear_ice |> mutate(representation = "Linearised (AR6)") |> select(-delta_Td))
linear_diagnostics <- tibble(
  representation = c("Nonlinear (AR6-calibrated)", "Linearised (AR6)"),
  lambda_ice_W_m2_K = c(p_ice$lambda_ice, lambda_ice_ar6),
  equilibrium_warming_K = c(delta_T_eq_ice, equilibrium_linear))
linear_max_difference <- max(abs(linear_ice$delta_T - warming_ice$delta_T))
linear_equilibrium_difference_pct <- 100 * (equilibrium_linear / delta_T_eq_ice - 1)
stopifnot(abs(p_ice$lambda_ice - lambda_ice_ar6) < 1e-12,
          max(abs(linear_state[, 1])) < 1e-12,
          abs(equilibrium_linear - (-delta_Q / p$lambda_P) / (1 - feedback_factor_ice_ar6)) < 1e-12,
          linear_max_difference < .02, abs(linear_equilibrium_difference_pct) < 2)
comparison_colours <- c("Nonlinear (AR6-calibrated)" = "#B43C39", "Linearised (AR6)" = "#202020")
comparison_lines <- c("Nonlinear (AR6-calibrated)" = "solid", "Linearised (AR6)" = "dashed")
plot_linear_temperature <- ggplot(linear_comparison,
  aes(year, delta_T, colour = representation, linetype = representation)) +
  geom_line(linewidth = .85) +
  scale_colour_manual(values = comparison_colours) + scale_linetype_manual(values = comparison_lines) +
  labs(x = "Years after CO2 doubling", y = "Surface-air warming (K)",
       title = "Linearised and nonlinear warming",
       subtitle = sprintf("Maximum difference over 3000 years: %.3f K", linear_max_difference)) + demo_theme
linear_gregory <- bind_rows(
  feedback_curves |> filter(scenario == "Planck + ice–albedo") |>
    transmute(delta_T, net_toa, representation = "Nonlinear (AR6-calibrated)"),
  tibble(delta_T = seq(0, equilibrium_linear, length.out = 300)) |>
    mutate(net_toa = delta_Q + lambda_linear * delta_T, representation = "Linearised (AR6)"))
plot_linear_gregory <- ggplot(linear_gregory,
  aes(delta_T, net_toa, colour = representation, linetype = representation)) +
  geom_hline(yintercept = 0, colour = "grey60") + geom_line(linewidth = .85) +
  geom_point(data = linear_diagnostics, aes(x = equilibrium_warming_K, y = 0), shape = 4, size = 3) +
  scale_colour_manual(values = comparison_colours) + scale_linetype_manual(values = comparison_lines) +
  labs(x = "Surface-air warming (K)", y = expression(N~(W~m^{-2})),
       title = "Consistency in the Gregory plot",
       subtitle = sprintf("Equilibrium difference: %.2f%%", linear_equilibrium_difference_pct)) + demo_theme
plot_linear_comparison <- cowplot::plot_grid(plot_linear_temperature, plot_linear_gregory, ncol = 1)


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

write_csv(feedback_comparison, file.path(output_dir, "ice_albedo_comparison.csv"))
write_csv(feedback_diagnostics, file.path(output_dir, "ice_albedo_diagnostics.csv"))
ggsave(file.path(output_dir, "model_components.png"), plot_components,
       width = 12, height = 6, dpi = 180, bg = "white")
ggsave(file.path(output_dir, "ice_albedo_temperature.png"), plot_ice_temperature,
       width = 10, height = 6, dpi = 180, bg = "white")
ggsave(file.path(output_dir, "ice_albedo_gregory.png"), plot_ice_gregory,
       width = 10, height = 6.5, dpi = 180, bg = "white")

write_csv(linear_diagnostics, file.path(output_dir, "ice_albedo_linear_diagnostics.csv"))
write_csv(linear_comparison, file.path(output_dir, "ice_albedo_linear_comparison.csv"))
ggsave(file.path(output_dir, "ice_albedo_linear_comparison.png"), plot_linear_comparison,
       width = 10, height = 10, dpi = 180, bg = "white")
