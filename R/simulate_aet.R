simulate_aet <- function(
    mprec,  # monthly total precipitation
    mwetd,  # number of wet days per month
    mpet,   # monthly total PET
    s0,     # root zone water storage capacity
    nyears_spinup
){
  
  # create vector of monthly precipitation
  ndayyear <- 365
  mprec_vec <- rep(mprec, 12)
  mwetd_vec <- rep(mwetd, 12)
  random_array <- array(
    data = runif(2*ndayyear),
    dim = c(ndayyear, 2)
  )
  
  # create five years data with precipitation from weather generator, repeating
  # a given year five times (exactly the same rain sequence each year)
  ddf <- tibble(
    date = seq(
      from = ymd(paste0(as.character(2023 - nyears_spinup + 1), "-01-01")), 
      to = ymd("2023-12-31"), 
      by = "days")
  ) |> 
    # avoid leap years
    filter(!(month(date) == 2 & mday(date) == 29)) |> 
    mutate(
      # run weather generator, given monthly precipitation and number of wet days
      prec = rep(monthly2daily_weather(mprec_vec, mwetd_vec, random_array), nyears_spinup),
      
      # PET is constant throughout the year and defined in relation to annual
      # precipitation and the moisture index
      pet  = rep(rep(mpet * 12 / ndayyear, ndayyear), nyears_spinup)
    )
  
  # # test
  # mdf <- ddf |> 
  #   mutate(month = month(date)) |> 
  #   group_by(month) |> 
  #   summarise(
  #     wet = sum(prec > 0),
  #     prec = sum(prec)
  #   )
  
  # run water bucket model with repeated five years (soil water balance needs spin-up)
  ddf <- waterbucketmodel(
    ddf$prec,
    ddf$pet,
    par = list(s0 = s0)
  ) |> 
    bind_cols(
      ddf
    ) |> 
    filter(
      year(date) == 2023
    )
  
  # get mean annual AET
  aet <- ddf |> 
    mutate(year = lubridate::year(date)) |> 
    group_by(year) |> 
    summarise(
      aet = sum(aet)
    ) |> 
    pull(aet) |> 
    mean()
  
  return(aet)
}
