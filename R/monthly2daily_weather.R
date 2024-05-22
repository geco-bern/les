monthly2daily_weather <- function(mval_prec, mval_wet, prdaily_random) {
  #/////////////////////////////////////////////////////////////////////////
  # Weather (rain) generator.
  # Distributes monthly total precipitation to days, given number of 
  # monthly wet days. Adopted from LPX.
  #--------------------------------------------------------------------
  # Arguments
  # mval_prec: numeric vector of length nmonth - monthly precipitation totals
  # mval_wet: numeric vector of length nmonth - monthly number of wet days
  # prdaily_random: matrix of dimensions ndayyear x 2 - random number seed
  
  nmonth <- length(mval_prec)
  ndayyear <- nrow(prdaily_random)
  ndaymonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  # days in each month
  
  # Local variables
  doy <- 0
  daysum <- 0
  c1 <- 1.0
  c2 <- 1.2
  prob <- 0
  
  prob_rain <- numeric(nmonth)
  mprecave <- numeric(nmonth)
  mprecip <- numeric(nmonth)
  dval_prec <- numeric(ndayyear)
  
  for (moy in 1:nmonth) {
    prob_rain[moy] <- 0.0
    mprecave[moy] <- 0.0
    mprecip[moy] <- 0.0
  }
  
  # Initialize random number generator
  set.seed(as.integer(prdaily_random[1, 1]))
  
  for (moy in 1:nmonth) {
    if (mval_wet[moy] <= 1.0) mval_wet[moy] <- 1.0
    prob_rain[moy] <- mval_wet[moy] / ndaymonth[moy]
    mprecave[moy] <- mval_prec[moy] / mval_wet[moy]
    dry <- TRUE
    iloop <- 0
    
    while (dry) {
      iloop <- iloop + 1
      nwet <- 0
      for (dm in 1:ndaymonth[moy]) {
        doy <- doy + 1
        
        # Transitional probabilities (Geng et al. 1986)
        if (doy > 1) {
          if (dval_prec[doy - 1] < 0.1) {
            prob <- 0.75 * prob_rain[moy]
          } else {
            prob <- 0.25 + 0.75 * prob_rain[moy]
          }
        }
        
        # Determine we randomly and use Krysanova / Cramer estimates of 
        # parameter values (c1,c2) for an exponential distribution
        if (iloop == 1) {
          vv <- as.numeric(prdaily_random[doy, 1])
        } else {
          vv <- runif(1)
        }
        
        if (vv > prob) {
          dval_prec[doy] <- 0.0
        } else {
          nwet <- nwet + 1
          v1 <- as.numeric(prdaily_random[doy, 2])
          dval_prec[doy] <- ((-log(v1)) ^ c2) * mprecave[moy] * c1
          if (dval_prec[doy] < 0.1) dval_prec[doy] <- 0.0
        }
        
        mprecip[moy] <- mprecip[moy] + dval_prec[doy]
      }
      
      # If it never rained this month and mprec(moy)>0 and mval_wet(moy)>0, do
      # again
      dry <- (nwet == 0 & iloop < 50 & mval_prec[moy] > 0.1)
      if (iloop > 50) {
        warning("monthly2daily_weather: prdaily: Warning stopped after 50 tries in cell")
      }
      
      # Reset counter to start of month          
      if (dry) {
        doy <- doy - ndaymonth[moy]
      }
    }
    
    # normalise generated precipitation by monthly CRU values
    if (moy > 1) daysum <- daysum + ndaymonth[moy - 1]
    if (mprecip[moy] < 1.0) mprecip[moy] <- 1.0
    for (dm in 1:ndaymonth[moy]) {
      doy <- daysum + dm
      dval_prec[doy] <- dval_prec[doy] * (mval_prec[moy] / mprecip[moy])
      if (dval_prec[doy] < 0.1) dval_prec[doy] <- 0.0
    }
  }
  
  return(dval_prec)
}
