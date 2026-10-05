# One daily step of the full rsofun inorganic-N transformation routine.
# Equations and operation order follow the ACTIVE Fortran statements at:
# https://github.com/geco-bern/rsofun/blob/bdd2711fb660339276c68d092c8e138c46622c97/src/ntransform.mod.f90
# Temperature response: src/rates.mod.f90 at the same commit.
# This is the full routine, not ntransform_simpl.mod.f90.
# R uses double precision; the upstream Fortran uses default REAL precision.

ntransform_source <- list(
  repository = "https://github.com/geco-bern/rsofun",
  commit = "bdd2711fb660339276c68d092c8e138c46622c97",
  files = c("src/ntransform.mod.f90", "src/rates.mod.f90",
            "src/waterbal_splash_cnmodel.mod.f90")
)

# Pool units: g N m-2, except doc (labile exudate C) in g C m-2.
# Wet/dry gas pools persist between days; mineral pools are repartitioned daily.
ntransform_state <- function(nh4, no3, doc, no2 = 0, no_d = 0, no_w = 0,
                             n2o_d = 0, n2o_w = 0, n2_w = 0) {
  list(nh4 = nh4, no3 = no3, no2 = no2, doc = doc, no_d = no_d,
       no_w = no_w, n2o_d = n2o_d, n2o_w = n2o_w, n2_w = n2_w)
}

ntransform_ftemp <- function(temp, ref_temp = 10) {
  # Preserve the constants and the -40 deg C floor in md_rates::ftemp.
  exp(308.56 * (1 / (ref_temp + 273.15 - 227.13) -
                  1 / (pmax(temp, -40) + 273.15 - 227.13)))
}

# Arguments:
#   state: ntransform_state() or the state returned by the preceding call.
#   temp: soil temperature, deg C.
#   wscal: fraction of plant-available water-holding capacity, 0..1, NOT WFPS.
#   aprec: annual precipitation, mm yr-1 (used on doy == 1 only).
#   params: named list of maxnitr, non, n2on, kn, kdoc, docmax, dnitr2n2o.
#     maxnitr is the maximum daily nitrified fraction; non and n2on are
#     sequential product fractions. kn and kdoc have pool units as above.
#     docmax scales the denitrification rate; dnitr2n2o scales its N2O yield.
#     Parameters are mandatory: the module itself provides no defaults.
#   dnoy, dnhx: daily deposition, g N m-2 d-1.
#   dfleach: daily fraction of nitrate leached, 0..1.
#   annual: saved pH and nh3max returned by an earlier day-1 call; required
#     on subsequent days, reproducing the two Fortran SAVE variables.
#   dnloss: incoming loss accumulator (other processes may already add to it).
#
# Returns updated state and annual state, fluxes in g N m-2 d-1, and modifiers.
# dnloss follows Fortran exactly: it counts removal from NH4 + NO3, including
# NO3 -> NO2, rather than actual loss from all soil N pools. Actual export is
# dnvol + dnleach + dn2o + dno + dn2. Production and emission are separate.
# No DOC consumption or reduction of stored N2O is added to the source model.
# Each call handles one land unit; apply separately to independent land units.
ntransform_full <- function(state, temp, wscal, aprec, params,
                            dnoy = 0, dnhx = 0, dfleach = 0,
                            doy = 1L, annual = NULL, dnloss = 0) {
  pool_names <- c("nh4", "no3", "no2", "doc", "no_d", "no_w",
                  "n2o_d", "n2o_w", "n2_w")
  param_names <- c("maxnitr", "non", "n2on", "kn", "kdoc", "docmax", "dnitr2n2o")
  scalar_finite <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  stopifnot(is.list(state), all(pool_names %in% names(state)),
            is.list(params), all(param_names %in% names(params)))
  inputs <- c(state[pool_names], params[param_names],
              list(temp, wscal, aprec, dnoy, dnhx, dfleach, doy, dnloss))
  stopifnot(all(vapply(inputs, scalar_finite, logical(1))),
            all(unlist(state[pool_names]) >= 0),
            all(unlist(params[param_names]) >= 0),
            params$kn > 0, params$kdoc > 0,
            wscal >= 0, wscal <= 1, dfleach >= 0, dfleach <= 1,
            aprec >= 0, dnoy >= 0, dnhx >= 0, doy >= 1, doy == floor(doy))

  if (doy == 1L) {
    ph_soil <- 3810 / (762 + aprec) + 3.8
    annual <- list(ph_soil = ph_soil, nh3max = if (ph_soil > 6) 1 else 0.00001)
  } else {
    if (is.null(annual) ||
        !all(c("ph_soil", "nh3max") %in% names(annual)) ||
        !all(vapply(annual[c("ph_soil", "nh3max")], scalar_finite, logical(1)))) {
      stop("Pass annual state from a day-1 call when doy is greater than 1.")
    }
  }

  # Deposition, volatilisation and leaching precede microsite partitioning.
  state$no3 <- state$no3 + dnoy
  state$nh4 <- state$nh4 + dnhx
  ftemp_vol <- min(1, ntransform_ftemp(temp, ref_temp = 25))
  fph <- exp(2 * (annual$ph_soil - 10))
  dnvol <- annual$nh3max * ftemp_vol^2 * fph * wscal * (1 - wscal) * state$nh4
  state$nh4 <- state$nh4 - dnvol
  dnloss <- dnloss + dnvol
  dnleach <- state$no3 * dfleach
  state$no3 <- state$no3 - dnleach
  dnloss <- dnloss + dnleach

  fwet <- wscal / 3.3
  nh4_w <- fwet * state$nh4
  no3_w <- fwet * state$no3
  no2_w <- fwet * state$no2
  doc_w <- state$doc * fwet
  fdry <- 1 - fwet
  nh4_d <- fdry * state$nh4
  no3_d <- fdry * state$no3
  no2_d <- fdry * state$no2

  # Nitrification: NO is deducted before calculating N2O from the remainder.
  ftemp_nitr <- max(min(((70 - temp) / (70 - 38))^12 *
                         exp(12 * (temp - 38) / (70 - 38)), 1), 0)
  no3_inc <- params$maxnitr * ftemp_nitr * nh4_d
  dnitr <- no3_inc
  nh4_d <- nh4_d - no3_inc
  no_inc <- params$non * no3_inc
  no3_inc <- no3_inc - no_inc
  state$no_d <- state$no_d + no_inc
  no_nitrification <- no_inc
  n2o_inc <- params$n2on * no3_inc
  no3_inc <- no3_inc - n2o_inc
  state$n2o_d <- state$n2o_d + n2o_inc
  n2o_nitrification <- n2o_inc
  no3_d <- no3_d + no3_inc
  dnloss <- dnloss + n2o_inc + no_inc

  # Denitrification: freshly formed dry NO3 is not part of the wet substrate.
  ftemp_denitr <- ntransform_ftemp(temp, ref_temp = 22)
  dnmax <- params$docmax * doc_w / (params$kdoc + doc_w)
  no2_inc <- min(dnmax * ftemp_denitr * no3_w /
                   (params$kn + no3_w) * 1000, no3_w)
  no3_w <- no3_w - no2_inc
  no2_w <- no2_w + no2_inc
  ddenitr <- no2_inc
  dnloss <- dnloss + no2_inc

  n2_inc <- min(dnmax * ftemp_denitr * no2_w /
                  (params$kn + no2_w) * 1000, no2_w)
  no2_w <- no2_w - n2_inc
  dno2_to_gas <- n2_inc
  n2o_inc <- params$dnitr2n2o * ftemp_denitr * (1.01 - 0.8 * wscal) * n2_inc
  n2_inc <- n2_inc - n2o_inc
  state$n2o_w <- state$n2o_w + n2o_inc
  n2o_denitrification <- n2o_inc
  no_inc <- 0.0001 * ftemp_denitr * (1.01 - 0.8 * wscal) * n2_inc
  n2_inc <- n2_inc - no_inc
  state$no_w <- state$no_w + no_inc
  no_denitrification <- no_inc
  state$n2_w <- state$n2_w + n2_inc
  n2_denitrification <- n2_inc

  state$nh4 <- nh4_w + nh4_d
  state$no3 <- no3_w + no3_d
  state$no2 <- no2_w + no2_d
  no <- state$no_w + state$no_d
  n2o <- state$n2o_w + state$n2o_d
  n2 <- state$n2_w

  # Gas escape acts on old + new gas, leaving the rest in the soil.
  ftemp_diffus <- min(1, ntransform_ftemp(temp, ref_temp = 25))
  dn2o <- ftemp_diffus * (1 - wscal) * n2o
  dno <- ftemp_diffus * (1 - wscal) * no
  dn2 <- ftemp_diffus * (1 - wscal) * n2
  dno_d <- ftemp_diffus * (1 - wscal) * state$no_d
  state$no_d <- state$no_d - dno_d
  dn2o_d <- ftemp_diffus * (1 - wscal) * state$n2o_d
  state$n2o_d <- state$n2o_d - dn2o_d
  dno_w <- ftemp_diffus * (1 - wscal) * state$no_w
  state$no_w <- state$no_w - dno_w
  dn2o_w <- ftemp_diffus * (1 - wscal) * state$n2o_w
  state$n2o_w <- state$n2o_w - dn2o_w
  state$n2_w <- state$n2_w - dn2

  list(
    state = state, annual = annual,
    fluxes = c(dnitr = dnitr, ddenitr = ddenitr, dno2_to_gas = dno2_to_gas,
               dnvol = dnvol, dnleach = dnleach, dnloss = dnloss,
               n2o_nitrification = n2o_nitrification,
               n2o_denitrification = n2o_denitrification,
               no_nitrification = no_nitrification,
               no_denitrification = no_denitrification,
               n2_denitrification = n2_denitrification,
               dn2o = dn2o, dno = dno, dn2 = dn2),
    modifiers = c(fwet = fwet, fdry = fdry, fph = fph, dnmax = dnmax,
                  ftemp_vol = ftemp_vol, ftemp_nitr = ftemp_nitr,
                  ftemp_denitr = ftemp_denitr, ftemp_diffus = ftemp_diffus)
  )
}
