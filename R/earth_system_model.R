# Global point energy balance model (EBM), with ocean heat uptake.
# Stocker (2024), Introduction to Climate Modelling, sections 2.2 and 2.4:
# https://climatehomes.unibe.ch/~stocker/stocker24icm.pdf
# Radiation: eqs. 2.1, 2.2; feedback sign: 2.27-2.34; CO2: 2.43-2.44.
#
# T is globally averaged surface-air temperature, assumed equal to mixed-layer
# temperature. Atmospheric absorption/back radiation is PARAMETERISED by the
# planetary emissivity and CO2 forcing, not resolved as a separate radiating slab.
# Albedo includes atmospheric/surface reflection; absorbed sunlight is the total
# for the atmosphere-surface system. No vertical atmospheric profile is resolved.
#
# Per unit total Earth area (all fluxes W m-2; capacities J m-2 K-1):
#   C_m dT/dt   = (1-alpha) S0/4 - epsilon sigma T^4 + DeltaQ - H
#   C_d dTd/dt  = H,             H = kappa (T - Td)
#   DeltaQ     = 5.35 log(CO2 / CO2_0)
#   kappa      = C_d / tau_mix
# tau_mix is the deep ocean's e-folding time if T were held fixed; it is NOT
# the coupled climate equilibration time. Exchange conserves energy and is a
# heat redistribution process, not an additional radiative climate feedback.
# Ocean heat capacity replaces Stocker's atmospheric h*rho*c. The two reservoirs
# partition the FULL ocean volume. Atmospheric/land heat capacities are neglected.
# By default only the Planck feedback operates; optional ice-albedo feedback
# makes planetary albedo a bounded, decreasing function of surface temperature.

# Construct the constants and derived parameters used by all model functions.
# Inputs: deep-ocean relaxation time (years), mixed-layer depth (metres), and
# optional bounded ice-albedo response (amplitude dimensionless, scale in K).
# Returns a named list; capacities, exchange and local feedbacks are derived.
earth_system_parameters <- function(mixing_years = 500, mixed_layer_depth = 50,
                                    ice_albedo = FALSE, albedo_amplitude = 0.02,
                                    albedo_temperature_scale = 10) {
  # Validate scalar controls and keep the planetary albedo bounds inside (0, 1).
  stopifnot(length(mixing_years) == 1L, is.finite(mixing_years), mixing_years > 0,
            length(mixed_layer_depth) == 1L, is.finite(mixed_layer_depth),
            mixed_layer_depth > 0,
            is.logical(ice_albedo), length(ice_albedo) == 1L, !is.na(ice_albedo),
            length(albedo_amplitude) == 1L, is.finite(albedo_amplitude),
            albedo_amplitude >= 0, albedo_amplitude < 0.3,
            length(albedo_temperature_scale) == 1L, is.finite(albedo_temperature_scale),
            albedo_temperature_scale > 0)
  p <- list(
    S0 = 1361,                       # Solar irradiance normal to the beam (W m-2).
    ice_albedo = ice_albedo,
    albedo_amplitude = albedo_amplitude, # Maximum absolute albedo anomaly.
    albedo_temperature_scale = albedo_temperature_scale, # Saturation scale (K).
    alpha = 0.3,                     # Fraction of incident sunlight reflected.
    sigma = 5.67e-8,                 # Stefan-Boltzmann constant (W m-2 K-4).
    T0 = 287.15,                     # Initial surface-air temperature (K; 14 C).
    co2_0 = 280,                     # Reference CO2 concentration (ppm).
    radius = 6371e3,                 # Earth radius (m).
    seconds_per_year = 365.25 * 86400, # Convert years to SI seconds for the ODE.
    # Rounded teaching values; ocean geometry and mixing are extensions to
    # Stocker's atmospheric point model, not parameters taken from eq. 2.1.
    ocean_volume = 1.332e18,         # Total ocean water volume (m3).
    ocean_fraction = 0.71,           # Ocean area / total Earth surface area.
    rho_water = 1025,                # Seawater density (kg m-3).
    cp_water = 3990,                 # Seawater specific heat (J kg-1 K-1).
    mixed_layer_depth = mixed_layer_depth, # Uniform upper-ocean thickness (m).
    mixing_years = mixing_years      # Deep-ocean relaxation time (years).
  )
  # Fluxes and heat capacities are normalised to the entire Earth, including land.
  p$area <- 4 * pi * p$radius^2
  # Only the ocean-covered fraction contains a mixed layer of water.
  p$volume_mixed <- p$ocean_fraction * p$area * mixed_layer_depth
  # Reserve a positive volume for the deep reservoir; do not count water twice.
  stopifnot(p$volume_mixed < p$ocean_volume)
  # Mass times specific heat, divided by Earth area: J m-2 K-1.
  p$C_m <- p$rho_water * p$cp_water * p$volume_mixed / p$area
  # All remaining ocean water contributes to the deep reservoir's heat capacity.
  p$C_d <- p$rho_water * p$cp_water * (p$ocean_volume - p$volume_mixed) / p$area
  # Tune effective emissivity so absorbed sunlight equals outgoing LW at T0.
  # This is a planetary parameter, not the material emissivity of seawater.
  p$epsilon <- (1 - p$alpha) * p$S0 / (4 * p$sigma * p$T0^4)
  # An exchange coefficient (W m-2 K-1), obtained from C_d / tau_mix in seconds.
  p$kappa <- p$C_d / (mixing_years * p$seconds_per_year)
  # Stocker's sign convention: stabilising feedback has negative lambda.
  # Chapter 9 sometimes uses the opposite sign for its damping coefficient.
  p$lambda_P <- -4 * p$epsilon * p$sigma * p$T0^3
  p$lambda_ice <- if (ice_albedo) p$S0 / 4 * albedo_amplitude / albedo_temperature_scale else 0
  # This demonstration considers a stable reference climate, not a tipping model.
  stopifnot(p$lambda_P + p$lambda_ice < 0)
  # Return both prescribed and derived parameters for inspection/reuse.
  p
}

# Convert one or more CO2 concentrations (ppm) into forcing (W m-2).
co2_radiative_forcing <- function(co2, p = earth_system_parameters()) {
  # The logarithm requires a finite, strictly positive concentration.
  stopifnot(is.numeric(co2), all(is.finite(co2)), all(co2 > 0))
  # Natural logarithm: every doubling gives 5.35 * log(2), independent of baseline.
  5.35 * log(co2 / p$co2_0)
}

# Illustrative instantaneous ice/snow response, expressed as planetary albedo.
# alpha(T0) = alpha0; tanh bounds the response as reflective cover is depleted.
# No explicit ice area, latent heat, seasonal cycle or ice-sheet dynamics.
earth_system_albedo <- function(T, p) {
  if (!p$ice_albedo) return(rep(p$alpha, length(T)))
  p$alpha - p$albedo_amplitude * tanh((T - p$T0) / p$albedo_temperature_scale)
}

# Diagnose fluxes at given surface-air and deep-ocean temperatures (kelvin).
# T and Td may be equal-length vectors; co2 may be a scalar or matching vector.
# Returns planetary albedo (dimensionless) and globally averaged fluxes (W m-2).
earth_system_fluxes <- function(T, Td, co2, p) {
  # Prescribed CO2 forcing is independent of temperature: it is not a feedback.
  forcing <- co2_radiative_forcing(co2, p)
  # The disk intercepting sunlight has one quarter of the sphere's surface area.
  albedo <- earth_system_albedo(T, p)
  solar_absorbed <- (1 - albedo) * p$S0 / 4
  # Effective net outgoing longwave at TOA. At fixed T, increasing CO2
  # reduces OLR; baseline greenhouse absorption is embedded in epsilon.
  longwave_out <- p$epsilon * p$sigma * T^4 - forcing
  data.frame(
    albedo = albedo,
    ice_albedo_response = (p$alpha - albedo) * p$S0 / 4,
    forcing = forcing,                         # Positive forcing adds energy.
    solar_reflected = albedo * p$S0 / 4,       # Shortwave returned to space.
    solar_absorbed = solar_absorbed,             # Shortwave retained by Earth.
    longwave_out = longwave_out,                 # Net longwave loss to space.
    # Warming increases emission: this anomaly is negative for T > T0.
    planck_response = -p$epsilon * p$sigma * (T^4 - p$T0^4),
    net_toa = solar_absorbed - longwave_out,     # N > 0 means Earth gains heat.
    deep_ocean_uptake = p$kappa * (T - Td)       # H > 0 transfers heat downward.
  )
}

# Solve the stationary radiation balance exactly; returns temperature in kelvin.
earth_system_equilibrium <- function(co2, p = earth_system_parameters()) {
  # At equilibrium H = 0 and T = Td. Preserve the analytic baseline solution.
  forcing <- co2_radiative_forcing(co2, p)
  absorbed <- (1 - p$alpha) * p$S0 / 4 + forcing
  stopifnot(all(absorbed > 0))
  if (!p$ice_albedo) return((absorbed / (p$epsilon * p$sigma))^0.25)
  # Albedo bounds bracket the radiative root independently of the time integration.
  vapply(forcing, function(F) {
    inputs <- (1 - p$alpha + c(-1, 1) * p$albedo_amplitude) * p$S0 / 4 + F
    stopifnot(all(inputs > 0))
    bracket <- (inputs / (p$epsilon * p$sigma))^0.25 + c(-1e-6, 1e-6)
    uniroot(function(T) (1 - earth_system_albedo(T, p)) * p$S0 / 4 -
      p$epsilon * p$sigma * T^4 + F, interval = bracket, tol = 1e-10)$root
  }, numeric(1))
}

# Instantaneous, permanent CO2 step at t=0; initial temperatures are the
# unperturbed equilibrium. Time is in years, temperature in kelvin.
# RK4 integrates both temperatures and accumulated TOA energy with the same
# quadrature, allowing a conservation-of-energy diagnostic. This checks budget
# consistency; timestep convergence in the analysis is a separate accuracy test.
# Inputs: duration (years), maximum timestep dt (years), constant post-step CO2
# (ppm), and the list returned by earth_system_parameters().
# Returns one row per output time, including t=0 immediately AFTER the CO2 step.
simulate_earth_system <- function(years = 3000, dt = 0.25, co2 = 560,
                                  p = earth_system_parameters()) {
  # Duration and timestep must be finite positive scalars; this is a step experiment.
  stopifnot(length(years) == 1L, is.finite(years), years > 0,
            length(dt) == 1L, is.finite(dt), dt > 0,
            length(co2) == 1L)
  T_eq <- earth_system_equilibrium(co2, p) # Also validates the specified CO2.
  # Conservative accuracy/stability guard for the fast radiative/exchange mode.
  damping <- 4 * p$epsilon * p$sigma * max(p$T0, T_eq)^3
  # Sum of diagonal relaxation rates bounds the fastest coupled decay rate (s-1).
  fast_rate <- (damping + p$kappa) / p$C_m + p$kappa / p$C_d
  # Require at least two steps per fastest timescale; RK4 remains well resolved.
  if (dt * p$seconds_per_year * fast_rate > 0.5) {
    stop("dt too large: reduce below ", signif(0.5 / fast_rate / p$seconds_per_year, 3),
         " years for these parameters.")
  }
  time <- seq(0, years, by = dt) # Uniform output times in years, starting at zero.
  # Include the requested endpoint even when years is not an integer multiple of dt.
  if (tail(time, 1) < years) time <- c(time, years)
  # Preallocate columns for T (K), Td (K), and integrated TOA input (J m-2).
  state <- matrix(0, nrow = length(time), ncol = 3)
  # CO2 jumps instantly; temperatures and stored heat cannot jump instantly.
  state[1, ] <- c(p$T0, p$T0, 0)
  # These inputs are constant throughout the permanent step experiment.
  forcing <- co2_radiative_forcing(co2, p)
  # Right-hand side of the ODE: rates in K s-1, K s-1, and J m-2 s-1.
  rhs <- function(x) {
    # The TOA budget is absorbed solar minus emitted longwave plus CO2 forcing.
    solar_absorbed <- (1 - earth_system_albedo(x[1], p)) * p$S0 / 4
    net_toa <- solar_absorbed - p$epsilon * p$sigma * x[1]^4 + forcing
    # Deep uptake removes exactly the heat that is added to the deep reservoir.
    uptake <- p$kappa * (x[1] - x[2])
    # Divide each reservoir's net heat input by its areal heat capacity.
    c((net_toa - uptake) / p$C_m, uptake / p$C_d, net_toa)
  }
  # Advance the solution using the classical fourth-order Runge-Kutta scheme.
  for (i in seq_len(length(time) - 1L)) {
    # Each step uses seconds; the final step may be shorter than dt.
    h <- (time[i + 1L] - time[i]) * p$seconds_per_year
    x <- state[i, ]               # Starting temperatures and integrated energy.
    k1 <- rhs(x)                 # Slope at the start of the step.
    k2 <- rhs(x + h * k1 / 2)    # Midpoint slope predicted from k1.
    k3 <- rhs(x + h * k2 / 2)    # Improved midpoint slope predicted from k2.
    k4 <- rhs(x + h * k3)        # Endpoint slope predicted from k3.
    # Weighted slopes update all three states with the same energy quadrature.
    state[i + 1L, ] <- x + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
  }
  T <- state[, 1]  # Extract surface-air / mixed-layer temperature history (K).
  Td <- state[, 2] # Extract deep-ocean temperature history (K).
  # Stored heat anomaly per Earth area, relative to the initial equilibrium.
  heat <- p$C_m * (T - p$T0) + p$C_d * (Td - p$T0)
  # Combine state histories and instantaneous radiation/exchange diagnostics.
  cbind(data.frame(year = time, co2 = co2, T = T, Td = Td,
                   delta_T = T - p$T0, delta_Td = Td - p$T0,
                   # Multiply areal heat by Earth area, then convert J to ZJ.
                   ocean_heat_ZJ = heat * p$area / 1e21,
                   integrated_toa_J_m2 = state[, 3],
                   # Stored heat minus integrated external input should be zero.
                   energy_residual_J_m2 = heat - state[, 3]),
        earth_system_fluxes(T, Td, co2, p))
}
