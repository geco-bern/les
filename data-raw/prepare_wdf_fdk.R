# prepare weekly data of fluxnet gpp, fapar, and ppfd using FluxDataKit 
# FLUXNET data obtained from Zenodo ([Hufkens, 2022](https://doi.org/10.5281/zenodo.8403081))
# this is not part of the LES repository. Get it from zenodo and place it locally
driver <- readRDS("~/data/FluxDataKit/rsofun_driver_data_clean.rds")
filnam <- here::here("data/wdf_fdk.rds")

# aggregate to weekly
wdf <- driver |> 
  select(sitename, forcing) |> 
  unnest(forcing) |> 
  select(sitename, date, ppfd, fapar, gpp) |> 
  mutate(week = lubridate::week(date),
         year = lubridate::year(date)) |> 
  group_by(sitename, year, week) |> 
  summarise(across(c(ppfd, fapar, gpp), ~mean(.x, na.rm = TRUE)))

saveRDS(wdf, file = filnam)
