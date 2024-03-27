# Chapter 4 Exercise -----------------------------------------------------------

# Rising CO2 increases leaf-level photosynthesis. This sensitivity can be quantified using following Equation 3.8. Calculate for the light-saturated photosynthesis rate (taking the equation for ), considering an increase in atmospheric CO2 from 400 to 600 ppm. Use  Pa,  Pa and mol m-2 s-1. Note that  in Equation 4.2 is expressed in Pa. Convert an atmospheric CO2 concentration in units of ppm to Pa by multiplication with 0.101325 Pa ppm-1 (considering standard atmospheric pressure). Assume that the ratio of ambient-to-leaf-internal CO2 () remains constant at 0.7
# Compare your calculated  of the light-saturated leaf-level photosynthesis rate with observations from Free Air CO2 Enrichment (FACE) experiments, as summarized by Ainsworth and Long (2005). For this, download the paper here and find the reported value for the CO2 response of the light-saturated leaf-level photosynthesis rate. Convert the percentage increase reported by Ainsworth and Long (2005) into a  factor. Assume that on average, the CO2 levels in the FACE experiments were 350 ppm at ambient and 550 ppm at elevated levels.

# Get values for gammastar, kmm, and Vcmax
# Set model parameters (constants)
# see Stocker et al., 2020 GMD for a description
beta <- 146          # unit cost ratio a/b
c_cost <- 0.41       # marginal cost of Jmax
gamma <- 0.105       # unit cost ratio c/b
kphio <- 0.085       # quantum yield efficiency
c_molmass <- 12.0107 # molar mass, g / mol

# Define environmental conditions
tc <- 20             # temperature, deg C
ppfd <- 500          # micro-mol/m2/s
vpd  <- 300          # Pa
co2  <- 400          # ppm
elv  <- 0            # m.a.s.l.
fapar <- 1           # fraction
patm <- 101325       # Pa

# get photosynthesis parameters gammastar, vcmax, jmax from p-model
# this assumes vcmax and jmax to be optimally acclimated/adapted to
# the specified environmental conditions and considers the temperature
# and atmospheric-pressure dependence of all parameters.

# first for low light
out_pmodel <- rpmodel(
  tc             = tc,
  vpd            = vpd,
  co2            = co2,
  elv            = elv,
  kphio          = kphio,
  beta           = beta,
  fapar          = fapar,
  ppfd           = ppfd,
  method_optci   = "prentice14",
  method_jmaxlim = "wang17",
  do_ftemp_kphio = FALSE
)

# electron transport-limited assimilation rate as a function of CO2 partial pressure
calc_aj <- function(ci, gammastar, kphio, ppfd, jmax){
  kphio * ppfd * (ci - gammastar)/(ci + 2 * gammastar) * 1/sqrt(1+((4 * kphio * ppfd)/jmax)^2)
}

# RuBP carboxylation-limited assimilation rate as a function of CO2 partial pressure
calc_ac <- function(ci, gammastar, kmm, vcmax){
  vcmax * (ci - gammastar)/(ci + kmm)
}

co2_to_ca <- function( co2, patm ){
  ( 1.0e-6 ) * co2 * patm
}

# beta: CO2 sensitivity of GPP
calc_beta <- function(gpp0, gpp1, co20, co21){
  log(gpp1/gpp0) / log(co21/co20)
}

# AC under ambient and elevated CO2
ac_amb <- calc_ac(
  0.7 * co2_to_ca(400, patm), 
  gammastar = 3.3, 
  kmm = 46, 
  vcmax = 49
)

ac_ele <- calc_ac(
  0.7 * co2_to_ca(600, patm), 
  gammastar = 3.3, 
  kmm = 46, 
  vcmax = 49
)

log(ac_ele/ac_amb)

# beta of Ac
calc_beta(ac_amb, ac_ele, 400, 600)

# in ainsworth: +31% for 
calc_beta(1, 1.31, 350, 550)


# AJ under ambient and elevated CO2
aj_amb <- calc_aj(
  0.7 * co2_to_ca(400, patm), 
  gammastar = 3.3, 
  kphio = 0.085, 
  ppfd = 1000, 
  jmax = 119
)

aj_ele <- calc_aj(
  0.7 * co2_to_ca(600, patm), 
  gammastar = 3.3, 
  kphio = 0.085, 
  ppfd = 1000, 
  jmax = 119
)

log(aj_ele/aj_amb)
calc_beta(aj_amb, aj_ele, 400, 600)

# ggplot() +
#   geom_function(
#     fun = calc_aj,
#     args = list(
#       gammastar = 3.3, 
#       kphio = 0.085, 
#       ppfd = 500, 
#       jmax = 119
#     )
#     ) + 
#   xlim(0, 1000)
