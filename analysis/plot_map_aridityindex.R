library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(rgeco)
library(sf)
library(khroma)

source(here::here("R/plot_discrete_cbar.R"))

## Global map of AI ----------------
# breaks for color bar
breaks <- c(0, 0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 0.65, 0.8, 1, 1.2, 1.6, 2, 2.5, 3, Inf)
# breaks <- c(0, 0.03, 0.2, 0.5, 0.65, Inf)

# raster_ai <- rast("~/data/greve/ep_over_p_cru_ncep.nc")

# data from https://doi.org/10.6084/m9.figshare.7504448.v4
rasta <- rast("~/data/aridityindex_zomer_2022/Global-AI_ET0_v3_annual/ai_v3_yr.tif")

# values are provided as integers, multiplied by 1e4
values(rasta) <- values(rasta) * 1e-4

# aggregate to 0.1 deg
rasta_agg <- aggregate(
  rasta,
  fact = res(rasta)[1]^-1*0.1,
  fun = "mean"
)

# bin values to get a discrete color scale (personal preference)
rasta_bin <- rasta_agg
values(rasta_bin) <- cut(
  values(rasta_agg),
  breaks = breaks, 
  right = FALSE
)

# get coast outlines
layer_coast <- rnaturalearth::ne_coastline(
  scale = 110, 
  returnclass = "sf"
  )

# get ocean layer
layer_ocean <- rnaturalearth::ne_download(
  scale = 110,
  type = "ocean",
  category = "physical",
  returnclass = "sf",
  destdir = here::here("data/")
)

# construct map
ggmap <- ggplot() +
  
  # aridity index raster
  tidyterra::geom_spatraster(
    data = rasta_bin,
    show.legend = FALSE
    ) +
  
  # coastline
  geom_sf(
    data = coast,
    colour = 'black',
    linewidth = 0.1
  ) +
  
  # ocean to mask zeros
  geom_sf(
    data = layer_ocean,
    color = NA,
    fill = "azure3"
    ) +
  
  # color palette from the khroma package
  scale_fill_roma(discrete = TRUE, name = "")  +
  coord_sf(
    ylim = c(-60, 85),
    expand = FALSE   # to draw map strictly bounded by the specified extent
  ) +
  xlab('') +
  ylab('') +
  theme_bw() +
  theme(axis.ticks.y.right = element_line(),
        axis.ticks.x.top = element_line(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white")
  )

gglegend <- plot_discrete_cbar(
  breaks = breaks,
  colors = c(khroma::color("roma")(length(breaks)-1)),
  legend_title = "",
  legend_direction = "vertical",
  width = 0.03,
  font_size = 3,
  expand_size_y = 0.5,
  spacing = "constant"
)

cowplot::plot_grid(ggmap, gglegend, ncol = 2, rel_widths = c(1, 0.10))

ggsave(here::here("book/images/map_aridityindex.png"), width = 12*0.8, height = 7*0.8)

## "Donohue" plot --------------

# get fAPAR at 0.5 deg
rasta_fpar <- terra::rast("~/data/MODIS-C006_MOD15A2_LAI_FPAR_zmaw/MODIS-C006_MOD15A2__LAI_FPAR__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__2000_2018__MON__fv0.02_ANNMAX_MEAN.nc")["fpar"]$fpar

# resample AI data to fAPAR grid
rasta_ai_agg <- resample(rasta_agg, rasta_fpar, method = "bilinear")

df <- as.data.frame(rasta_fpar, xy = TRUE) |> 
  left_join(
    as.data.frame(rasta_ai_agg, xy = TRUE),
    by = c("x", "y")
  ) |> 
  as_tibble()

df |> 
  ggplot(aes(awi_pm_sr_yr, fpar)) +
  geom_hex(
    bins = 50,
    # show.legend = FALSE
    ) +
  xlim(0, 1.5) +
  theme_classic() +
  scale_fill_gradientn(
    colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
    # trans = "log",
    ) +
  labs(
    x = expression(paste(italic(P)/PET)),
    y = "fAPAR"
  )

ggsave(here::here("book/images/donohue.png"), width = 6, height = 4)





