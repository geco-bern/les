# prepare weekly data of fluxnet gpp, fapar, and ppfd using FluxDataKit 
# FLUXNET data obtained from Zenodo ([Hufkens, 2022](https://doi.org/10.5281/zenodo.8403081))
# this is not part of the LES repository. Get it from zenodo and place it locally
driver <- readRDS("~/data/FluxDataKit/v3/rsofun_driver_data_v3.rds")

# aggregate to weekly
filnam <- here::here("data/wdf_fdk.rds")
wdf <- driver |> 
  select(sitename, forcing) |> 
  unnest(forcing) |> 
  select(sitename, date, ppfd, fapar, gpp) |> 
  mutate(week = lubridate::week(date),
         year = lubridate::year(date)) |> 
  group_by(sitename, year, week) |> 
  summarise(across(c(ppfd, fapar, gpp), ~mean(.x, na.rm = TRUE)))

saveRDS(wdf, file = filnam)

# aggregate to annual
filnam <- here::here("data/adf_fdk.rds")
adf <- wdf |> 
  mutate(apar = fapar * ppfd) |> 
  ungroup() |> 
  group_by(sitename, year) |> 
  summarise(across(c(apar, gpp), ~sum(.x, na.rm = TRUE))) |> 
  ungroup() |> 
  group_by(sitename) |> 
  summarise(across(c(apar, gpp), ~mean(.x, na.rm = TRUE)))

adf |> 
  ggplot(aes(apar, gpp)) +
  geom_point()

saveRDS(adf, file = filnam)
