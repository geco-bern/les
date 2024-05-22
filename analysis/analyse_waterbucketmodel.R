library(dplyr)
library(tidyr)
library(lubridate)

source(here("R/monthly2daily_weather.R"))
source(here("R/waterbucketmodel.R"))
source(here("R/simulate_aet.R"))

moisture_index <- 1
nspinup_years <- 5

# Plot AET vs P ---------------------
## By PET ---------------------

# simulate annual AET for a sequence of combinations of precipitation and 
# root zone water storage capacity
# warning: this takes a few minutes (on a fast machine)
# s0: 200
# PET: = PREC
df_pet <- expand.grid(
  prec = seq(400, 2000, by = 100),
  pet = seq(400, 4000, by = 400)
) |> 
  as_tibble() |> 
  mutate(
    mprec = prec / 12,
    s0 = 200,
    mwetd = 10,
    mpet = pet / 12
  ) |> 
  rowwise() |> 
  mutate(
    aet = simulate_aet(
      mprec,
      mwetd, 
      mpet,
      s0,
      nspinup_years
    )
  )

saveRDS(df_pet, file = here("data/df_pet.rds"))


## By S0 ---------------------

# simulate annual AET for a sequence of combinations of precipitation and 
# root zone water storage capacity
# warning: this takes a few minutes (on a fast machine)
# number of wet days per month: 10
# PET: = 2000 mm yr-1
df_s0 <- expand.grid(
  prec = seq(400, 3000, by = 200),
  s0 = seq(20, 500, by = 20),
  pet = 2000
) |> 
  as_tibble() |> 
  mutate(
    mprec = prec / 12,
    mwetd = 10,
    mpet = pet / 12
  ) |> 
  rowwise() |> 
  mutate(
    aet = simulate_aet(
      mprec,
      mwetd, 
      mpet,
      s0,
      nspinup_years
    )
  )

saveRDS(df_s0, file = here("data/df_s0.rds"))


## By wet days ---------------------

# simulate annual AET for a sequence of combinations of precipitation and 
# root zone water storage capacity
# warning: this takes a few minutes (on a fast machine)
# s0: 200
# PET: = PREC
df_wetd <- expand.grid(
  prec = seq(400, 3000, by = 200),
  mwetd = c(2, 3, 5, 7, 10, 12, 15, 20, 30),
  pet = 2000
) |> 
  as_tibble() |> 
  mutate(
    mprec = prec / 12,
    s0 = 200,
    mpet = pet / 12
  ) |> 
  rowwise() |> 
  mutate(
    aet = simulate_aet(
      mprec,
      mwetd, 
      mpet,
      s0,
      nspinup_years
    )
  )

saveRDS(df_wetd, file = here("data/df_wetd.rds"))



