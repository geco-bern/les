ccascade <- function( 
  gpp, 
  state = initpools(),
  par = getpar(),
  fluxes = initfluxes()
  ){

  # determine integration length (number of time steps) from length of 'c_influx'
  len <- length(gpp)

  # output the state and fluxes in one data frame
  df <- dplyr::tibble()

  # integrate over each time step (this is an implementation of the differential equation)
  for (yr in seq(len)){

    # update states and fluxes
    out <- ccascade_onestep(gpp[yr], state, par, fluxes)
    state <- out$state
    fluxes <- out$fluxes

    # record for output
    df <- dplyr::bind_rows(
      df,
      dplyr::bind_cols( 
        dplyr::tibble(year = yr),
        dplyr::as_tibble(state), 
        dplyr::as_tibble(fluxes))
      )
  }
  return(df)
}

ccascade_onestep <- function(gpp, state, par, fluxes){

  # total biomass production
  bp <- gpp * par$bpe
  fluxes$ra <- gpp * (1 - par$bpe)

  # biomass turnover by pool, use exponential here to avoid numerical instability with k -> 1
  dleaf <- (1 - exp(-par$kleaf)) * state$cleaf
  dwood <- (1 - exp(-par$kwood)) * state$cwood
  droot <- (1 - exp(-par$kroot)) * state$croot

  # update biomass pools
  state$cleaf <- state$cleaf + bp * par$fleaf - dleaf
  state$cwood <- state$cwood + bp * par$fwood - dwood
  state$croot <- state$croot + bp * par$froot - droot

  # litter turnover
  dflitt <- (1 - exp(-par$kflitt)) * state$flitt
  dslitt <- (1 - exp(-par$kslitt)) * state$slitt

  # update litter: add biomass turnover and update litter pools
  state$flitt <- state$flitt + dleaf + droot - dflitt
  state$slitt <- state$slitt + dwood         - dslitt

  # heterotrophic respiration during litter decomposition
  rh_litt <- (1 - par$eff) * (dflitt + dslitt)

  # soil turnover
  dfsoil <- (1 - exp(-par$kfsoil)) * state$fsoil
  dssoil <- (1 - exp(-par$kssoil)) * state$ssoil

  # update soil: remainder of litter turnover goes to soil, subtract decomposing portion
  state$fsoil <- state$fsoil + par$eff * par$ffast       * (dflitt + dslitt) - dfsoil
  state$ssoil <- state$ssoil + par$eff * (1 - par$ffast) * (dflitt + dslitt) - dssoil

  # heterotrophic respiration from soil decomposition
  rh_soil <- dfsoil + dssoil
  fluxes$rh <- rh_litt + rh_soil

  return(list(state = state, fluxes = fluxes))
}

getpar <- function(){
  list(
    bpe = 0.4,
    fleaf = 0.3,
    froot = 3,
    fwood = 0.4,
    kleaf = 0.5,
    kwood = 0.02,
    kroot = 0.5,
    kflitt = 0.5,
    kslitt = 0.1,
    kfsoil = 0.1,
    kssoil = 0.003,
    eff = 0.6,
    ffast = 0.95
    )
}

initpools <- function(){
  list(
    cleaf = 0,
    cwood = 0,
    croot = 0,
    flitt = 0,
    slitt = 0,
    fsoil = 0,
    ssoil = 0
    )
}

initfluxes <- function(){
  list(
    ra = 0,
    rh = 0
    )
}