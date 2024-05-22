waterbucketmodel <- function( 
  prec,
  pet,
  state = initpools(),
  par = getpar(),
  fluxes = initfluxes()
  ){

  # determine integration length (number of time steps) from length of 'prec' (precipitation vector)
  len <- length(prec)

  # output the state and fluxes in one data frame
  df <- dplyr::tibble()

  # integrate over each time step (this is an implementation of the differential equation)
  for (day in seq(len)){

    # update states and fluxes
    out <- waterbucketmodel_onestep(prec[day], pet[day], state, par, fluxes)
    state <- out$state
    fluxes <- out$fluxes

    # record for output
    df <- dplyr::bind_rows(
      df,
      dplyr::bind_cols( 
        dplyr::as_tibble(state), 
        dplyr::as_tibble(fluxes))
      )
  }
  return(df)
}

waterbucketmodel_onestep <- function(
    prec,
    pet,
    state, 
    par, 
    fluxes
    ){
  
  fluxes$aet <- pet * state$wcont/par$s0
  capacity_left <- par$s0 - state$wcont
  state$wcont <- state$wcont + min(prec, capacity_left) - fluxes$aet
  fluxes$runoff <- max(0, prec - capacity_left)

  return(list(state = state, fluxes = fluxes))
}

getpar <- function(){
  list(
    s0 = 200
    )
}

initpools <- function(){
  list(
    wcont = 0
    )
}

initfluxes <- function(){
  list(
    aet = 0,
    runoff = 0
    )
}