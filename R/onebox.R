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
