# MODIS IGBP vegetation types, using the land-use chapter's map styling.
# Run from the repository root: Rscript analysis/biogeography/plot_vegetation_types.R
source(here::here("R/global_map_style.R"))

r <- terra::rast(here::here("data/modis_landcover__LPDAAC__v5.1__0.1deg__2010.nc"))
# Preserve the chapter's category codes, descriptions and terrestrial colours.
legend <- list(
    values = c(
      "1" = "white", # worldgrids_pal$IGBP["Water " ] , #"water" ;
      "2" = "dodgerblue4" , #"evergreen needleleaf forest" ;
      "3" = "forestgreen" , #"evergreen broadleaf forest" ;
      "4" = "#9ED003", # worldgrids_pal$IGBP["Deciduous Needleleaf forest "] , #"deciduous needleleaf forest" ;
      "5" =  "springgreen3" , #"deciduous broadleaf forest" ;
      "6" =  "#31A278", # worldgrids_pal$IGBP["Mixed forest "] #"mixed forest" ;
      "7" =  "#A84974", #worldgrids_pal$IGBP["Closed shrublands " ] , #"closed shrublands" ;
      "8" =  "#FFD3A0" , #"open shrublands" ;
      "9" =  "#CAFE8F", #worldgrids_pal$IGBP["Woody savannas "] , #"woody savannas" ;
      "10" = "#F7D600", #worldgrids_pal$IGBP["Savannas " ] , # "savannas" ;
      "11" = "khaki3" , # "grasslands" ;
      "12" = "violetred" , # "permanent wetlands" ;
      "13" = "#FAFE03", # worldgrids_pal$IGBP["Croplands " ] , # "croplands" ;
      "14" = "#FF2A00", #worldgrids_pal$IGBP["Urban and built-up "] , # "urban and built-up" ;
      "15" = "#8A9000" , #worldgrids_pal$IGBP["Cropland/Natural vegetation mosaic "] , # "cropland (natural vegetation mosaic)" ;
      "16" = "#FFFFFF", #worldgrids_pal$IGBP["Snow and ice "] , # "snow and ice" ;
      "17" = "#AAAAAA", # worldgrids_pal$IGBP["Barren or sparsely vegetated "] , # "barren or sparsely vegetated" ;
      "18" = "#424242" # worldgrids_pal$IGBP["Unclassified "] # "unclassified or fill values" ;      
    ),
    labels = c(
      "1" = "water",
      "2" = "evergreen needleleaf forest",
      "3" = "evergreen broadleaf forest",
      "4" = "deciduous needleleaf forest",
      "5" = "deciduous broadleaf forest",
      "6" = "mixed forest",
      "7" = "closed shrublands",
      "8" = "open shrublands",
      "9" = "woody savannas",
      "10" = "savannas",
      "11" = "grasslands",
      "12" = "permanent wetlands",
      "13" = "croplands",
      "14" = "urban and built-up",
      "15" = "cropland (natural vegetation mosaic)",
      "16" = "snow and ice",
      "17" = "barren or sparsely vegetated",
      "18" = "unclassified or fill values"      
    )
)
key <- data.frame(x = 0, y = 0, code = factor(names(legend$values), levels = names(legend$values)))
plot <- global_map_frame() +
  global_map_raster(r, function(x) unname(legend$values[as.character(x)])) +
  global_map_outlines() +
  ggplot2::geom_point(data = key, ggplot2::aes(x, y, fill = code),
    shape = 22, alpha = 0, show.legend = TRUE) +
  ggplot2::scale_fill_manual(values = legend$values, labels = legend$labels,
    drop = FALSE, name = NULL) +
  ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3, byrow = TRUE,
    override.aes = list(alpha = 1, size = 3, colour = "#7b8279"))) +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10)) +
  global_map_extent()
save_global_map(plot, here::here("book/images/biogeography/vegetation_types"), height = 7.6)
