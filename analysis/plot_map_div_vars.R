library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(ggridges)
library(GECOr)
library(khroma)
sf::sf_use_s2(FALSE)
source(here::here("R/create_map_byvar.R"))


# Veg height -------------------
create_map_byvar(
  path = "~/data/archive/vegheight_simard_2011/data/vegheight_simard_2011.nc",
  varnam = "Band1",
  labelnam = "vegheight",
  longnam = "Vegetation height",
  colorscale = "bamako",
  units = "(m)"
)

# Aboveground biomass carbon ---------
create_map_byvar(
  path = "~/data/archive/biomass_liu_2015/data/Global_annual_mean_ABC_lc2001_1993_2012_20150331_MEAN.nc",
  varnam = "Aboveground Biomass Carbon",
  labelnam = "agc",
  longnam = "Aboveground biomass carbon",
  colorscale = "lajolla",
  units = expression(paste("(Mg C ha"^-1,")")),
  use_raster = FALSE,
  transpose = TRUE,
  breaks = c(0, 5, 10, 15, seq(from = 30, to = 165, by = 15)),
  xaxis_ridgeplot = expression(paste("Aboveground biomass carbon (Mg C ha"^-1,")"))
)

# LAI -------------
create_map_byvar(
  path = "~/data/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/MODIS-C006_MOD15A2__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2018__MON__fv0.02_ANNMAX_MEAN.nc",
  varnam = "lai",
  labelnam = "lai",
  longnam = "Leaf area index",
  colorscale = "navia",
  units = expression(paste("(m"^2, " m"^-2,")")),
  use_raster = TRUE,
  xaxis_ridgeplot = expression(paste("Leaf area index (m"^2, " m"^-2,")"))
)

# forest cover fraction -----------------
create_map_byvar(
  path = "~/data/MODIS-C006_MOD44B_ForestCoverFraction/MODIS-TERRA_C6__MOD44B__ForestCoverFraction__LPDAAC__GLOBAL__0.5degree__UHAM-ICDC__20100306__fv0.02.nc",
  varnam = "forestcoverfraction",
  labelnam = "forestcoverfraction",
  longnam = "Tree cover fraction",
  colorscale = "lipari",
  units = "(%)",
  breaks = c(0, 5,  seq(10, 100, by = 10))
)




