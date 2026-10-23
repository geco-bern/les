# Three-panel HaNi figure: annual source-resolved trends and two input maps.
# Run from the repository root: Rscript --vanilla analysis/plot_nitrogen_hani.R
# First run download_nitrogen_hani.R and prepare_nitrogen_hani.R.
# Dependencies: ggplot2, cowplot, terra, sf, rnaturalearth, scales.
# The denominator is ALL REPRESENTED LAND AREA, not just agricultural area.
# Native HaNi cells with data define the land footprint; units: g N m^-2 yr^-1.

plot_hani <- function(root = getwd()) {
  library(ggplot2)
  derived <- file.path(root, "data", "nitrogen_hani", "derived")
  out <- file.path(root, "book", "images", "nitrogen")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  series <- read.csv(file.path(derived, "hani_annual_components.csv"))
  total <- read.csv(file.path(derived, "hani_annual_totals.csv"))
  keys <- c("nfer_crop_nh4", "nfer_crop_no3", "nfer_pas_nh4", "nfer_pas_no3",
    "nmanure_app_crop", "nmanure_app_pas", "nmanure_dep_pas", "nmanure_dep_range",
    "ndep_nhx", "ndep_noy")
  labels <- c("Fertiliser, cropland, NH₄⁺", "Fertiliser, cropland, NO₃⁻",
    "Fertiliser, pasture, NH₄⁺", "Fertiliser, pasture, NO₃⁻",
    "Manure, applied to cropland", "Manure, applied to pasture",
    "Manure, deposited on pasture", "Manure, deposited on rangeland",
    "Atmospheric deposition, NHₓ", "Atmospheric deposition, NOᵧ")
  # Okabe–Ito colours, with two additional blue shades for four fertiliser types.
  colours <- c("#0072B2", "#56B4E9", "#00476F", "#A6D8EF",
    "#E69F00", "#F0E442", "#D55E00", "#009E73", "#CC79A7", "#777777")
  names(colours) <- keys
  series$component <- factor(series$component, levels = keys)
  theme_base <- theme_classic(base_size = 12, base_family = "Helvetica") +
    theme(text = element_text(colour = "#222222"),
      axis.text = element_text(colour = "#222222"),
      plot.title = element_text(face = "bold", size = 13),
      legend.title = element_blank(), legend.text = element_text(size = 10),
      legend.key.height = grid::unit(.4, "cm"),
      plot.margin = margin(8, 10, 6, 8))
  trend <- ggplot(series, aes(year, Tg_N_yr, fill = component)) +
    geom_area(position = position_stack(reverse = TRUE), linewidth = 0) +
    geom_line(
      data = total,
      aes(year, Tg_N_yr),
      inherit.aes = FALSE,
      colour = "#222222",
      linewidth = .45
    ) +
    geom_vline(
      xintercept = c(1959, 2019),
      colour = "#333333",
      linetype = "dotted",
      linewidth = .4
    ) +
    scale_fill_manual(values = colours, breaks = keys, labels = labels) +
    scale_x_continuous(
      breaks = c(1860, 1900, 1940, 1980, 2019),
      limits = c(1860, 2019),
      expand = expansion(mult = c(0, .006))
    ) +
    scale_y_continuous(
      breaks = seq(0, 300, 50),
      expand = expansion(mult = c(0, .06))
    ) +
    labs(
      title = "a   Global total anthropogenic reactive nitrogen inputs, 1860–2019",
      x = NULL,
      y = expression(paste(
        "Global N"[r],
        " input (Tg N " *
          yr^{
            -1
          } *
          ")"
      ))
    ) +
    theme_base +
    theme(legend.position = "right")

  world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
  # Sequential light-to-dark pink ramp centred on Okabe–Ito reddish purple.
  map_colours <- c("#FFF4FA", "#E9C1D9", "#CC79A7", "#994F7C", "#54263F")
  panels <- lapply(c(1959, 2019), function(year) {
    raster <- terra::rast(file.path(derived, paste0("hani_total_", year, "_025deg.tif")))
    # Retain the full regular grid so geom_raster cannot infer an incorrect
    # pixel spacing from gaps over the ocean; missing cells are transparent.
    d <- terra::as.data.frame(raster, xy = TRUE, na.rm = FALSE)
    names(d)[3] <- "input"
    ggplot(d, aes(x, y, fill = input)) +
      geom_raster() +
      geom_sf(
        data = world,
        inherit.aes = FALSE,
        fill = NA,
        colour = "#6C6C6C",
        linewidth = .1
      ) +
      coord_sf(
        crs = sf::st_crs(4326),
        datum = NA,
        xlim = c(-180, 180),
        ylim = c(-60, 85),
        expand = FALSE
      ) +
      scale_fill_gradientn(
        colours = map_colours,
        # transform = "sqrt",
        na.value = "transparent",
        limits = c(0, 20),
        # breaks = c(0, .5, 2, 5, 10, 20),
        # labels = c("0", "0.5", "2", "5", "10", "≥20"),
        oob = scales::squish,
        name = expression(
          "Anthropogenic reactive N input per grid-cell land area (g N " *
            m^{
              -2
            } ~ yr^{
            -1
          } *
            ")"
        ),
        guide = guide_colourbar(
          title.position = "top",
          title.hjust = .5,
          barwidth = grid::unit(12, "cm"),
          barheight = grid::unit(.32, "cm")
        )
      ) +
      labs(
        title = paste(if (year == 1959) "b  " else "c  ", year),
        x = NULL,
        y = NULL
      ) +
      theme_base +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 11),
        plot.margin = margin(5, 8, 0, 8)
      )
  })
  legend <- cowplot::get_legend(panels[[1]])
  maps <- cowplot::plot_grid(plotlist = lapply(panels, function(p)
    p + theme(legend.position = "none")), nrow = 1)
  plot <- cowplot::plot_grid(trend, maps, legend, ncol = 1,
    rel_heights = c(1.05, .9, .17))
  for (ext in c("png", "pdf", "svg")) {
    path <- file.path(out, paste0("anthropogenic_n_inputs.", ext))
    device <- switch(ext, png = "png", pdf = grDevices::cairo_pdf, svg = grDevices::svg)
    ggsave(path, plot, device = device, width = 12, height = 7.6,
      dpi = 250, bg = "white")
  }
  invisible(plot)
}

if (sys.nframe() == 0L) plot_hani()
