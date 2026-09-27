# Optional registration previews; run from the repository root. Requires gdalwarp.
source('analysis/plot_hansen_forest_change.R')
dir.create('data/hansen_forest_change_2025/figure2_registration',showWarnings=FALSE)
for(i in 1:4) {
 g <- hansen_regions[i,]; if (g$slug == 'indonesia') g$lat <- 0.45
 dx <- 250/111.32/cos(g$lat*pi/180); dy <- 160/111.32
 bounds <- c(g$lon-dx,g$lat-dy,g$lon+dx,g$lat+dy)
 west <- seq(floor(bounds[1]/10)*10,floor(bounds[3]/10)*10,10)
 north <- seq(ceiling(bounds[2]/10)*10,ceiling(bounds[4]/10)*10,10)
 ts <- expand.grid(west=west,north=north)
 tag <- function(lat,lon) sprintf('%02d%s_%03d%s',abs(lat),ifelse(lat>=0,'N','S'),abs(lon),ifelse(lon>=0,'E','W'))
 urls <- paste0('/vsicurl/',hansen_base,'Hansen_GFC-2025-v1.13_treecover2000_',mapply(tag,ts$north,ts$west),'.tif')
 out <- paste0('data/hansen_forest_change_2025/figure2_registration/',g$slug,if (g$slug == 'indonesia') '_reference_north.tif' else '_reference.tif')
 if(!file.exists(out)) {
  args <- c('-overwrite','-te',format(bounds,scientific=FALSE,digits=12),'-ts','1600','1000','-r','near','-of','GTiff','-co','COMPRESS=DEFLATE',urls,out)
  message(g$name)
  stopifnot(system2('gdalwarp',args)==0)
 }
}
