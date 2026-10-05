# Reproduce the nitrogen-chapter figures and the full-model moisture comparison.
# Run from the repository root:
# R --vanilla -q -e 'source("analysis/plot_nitrogen.R")'
# To export only the book moisture figure, set options(nitrogen.figures = "moisture_response").
# The retained full-model plot is exported as "moisture_response_full".
# The ecosystem and decomposition figures are in plot_nitrogen_cycle.R.
# Requires ggplot2 and cowplot; uses base R graphics devices, not svglite.
library(ggplot2)
library(grid)

out <- file.path("book", "images", "nitrogen")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
# Helvetica and the Okabe–Ito palette are shared with plot_nitrogen_cycle.R.
font_family <- "Helvetica"
ink <- "#222222"
blue <- "#0072B2"
green <- "#009E73"
orange <- "#E69F00"
vermillion <- "#D55E00"
purple <- "#CC79A7"
sky_blue <- "#56B4E9"
pale <- function(colour) adjustcolor(colour, alpha.f = .07)
theme_set(theme_classic(base_size = 12, base_family = font_family) +
  theme(text = element_text(colour = ink),
        axis.text = element_text(colour = ink),
        axis.line = element_line(colour = ink, linewidth = .4),
        axis.ticks = element_line(colour = ink, linewidth = .4),
        plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.margin = margin(14, 10, 8, 10),
        legend.position = "bottom"))

save_figure <- function(name, width, height, draw) {
  selected <- getOption("nitrogen.figures")
  if (!is.null(selected) && !name %in% selected) return(invisible(NULL))
  draw_file <- function(extension) {
    path <- file.path(out, paste0(name, ".", extension))
    if (extension == "svg") {
      svg(path, width, height, family = font_family, bg = "white")
    } else if (extension == "pdf") {
      cairo_pdf(path, width, height, family = font_family, bg = "white")
    } else {
      png(path, width = width, height = height, units = "in", res = 180,
          type = "cairo", family = font_family, bg = "white")
    }
    on.exit(dev.off())
    draw()
  }
  invisible(lapply(c("svg", "png", "pdf"), draw_file))
}

# NOAA Global Monitoring Laboratory, approximate mid-2020 dry-air composition:
# https://gml.noaa.gov/ccgg/about/co2_measurements.html
# Other = Ne 18 + He 5 + CH4 2 + Kr 1 + remaining trace gases 1 ppm.
air <- data.frame(
  # gas = c("Nitrogen", "Oxygen", "Argon", "Carbon dioxide", "Other"),
  gas = c("Nitrogen", "Oxygen", "Other"),
  # ppm = c(780900, 209360, 9300, 413, 27))
  ppm = c(780900, 209360, 9740)
)
stopifnot(sum(air$ppm) == 1e6)
air$gas <- factor(air$gas, levels = air$gas)
air$pct <- air$ppm / 10000
# palette <- c(
#   "Nitrogen" = blue,
#   "Oxygen" = sky_blue,
#   "Argon" = orange,
#   "Carbon dioxide" = vermillion,
#   "Other" = purple
# )
palette <- c(
  "Nitrogen" = blue,
  "Oxygen" = sky_blue,
  "Other" = orange
)
air_plot <- function(d, xmax, breaks) {
  ggplot(d, aes(x = 1, y = pct, fill = gas)) +
    geom_col(width = .48, position = position_stack(reverse = TRUE)) +
    coord_flip(ylim = c(0, xmax)) +
    scale_fill_manual(values = palette, breaks = as.character(d$gas)) +
    scale_y_continuous(breaks = breaks,
                       expand = expansion(mult = c(0, .005))) +
    scale_x_continuous(breaks = NULL) +
    labs(x = NULL, y = "Percentage of all dry-air molecules", fill = NULL) +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())
}

p_all <- air_plot(air, 100, seq(0, 100, 20)) +
  annotate("text", x = 1, y = 39, label = "Nitrogen (N₂)\n78.09%", colour = "white", size = 4.5, family = font_family) +
  annotate("text", x = 1, y = 88.5, label = "Oxygen (O₂)\n20.936%", size = 4, colour = ink, family = font_family)
  # theme(legend.position = "none")

p_minor <- air_plot(air[3:5, ], .974, c(0, .2, .4, .6, .8)) +
  annotate("text", x = 1, y = .44, label = "Argon (Ar)  0.930%", size = 4.5, colour = ink, family = font_family) +
  guides(fill = guide_legend(nrow = 1))

save_figure("atmospheric_composition", 10, 3, function() {
  print(cowplot::plot_grid(
    p_all,
    ncol = 1,
    align = "v",
    axis = "lr",
    label_size = 14,
    label_fontfamily = font_family
  ))
  # print(cowplot::plot_grid(
  #   p_all,
  #   p_minor,
  #   ncol = 1,
  #   align = "v",
  #   axis = "lr",
  #   labels = c("a", "b"),
  #   label_size = 14,
  #   label_fontfamily = font_family
  # ))
})

# Teaching curves, not a reproduction of DyN or rsofun.
# Xu-Ri & Prentice (2008), Tables 8–11, motivates separate pathways and yields;
# Butterbach-Bahl et al. (2013) describes the moisture/oxygen dependence.
# WFPS w is explicitly [0,1]. No equivalence to rsofun's wscal is assumed.
w <- seq(0, 1, length.out = 1001)
n <- 4 * w * (1 - w)
d <- w^6
pn <- .002 * n
pd <- .02 * d * (1 - w) / (1.2 - w)

nitrogen_rates <- data.frame(
  wfps = rep(100*w, 2), 
  rate = c(n, d),
  pathway = rep(c("Nitrification", "Denitrification"),
  each = length(w))
)

gas_rates <- data.frame(
  wfps = rep(100*w, 3), 
  rate = c(pn, pd, pn + pd),
  pathway = rep(c("From nitrification", "From denitrification", "Total"),
  each = length(w))
)

stopifnot(all(n >= 0), all(d >= 0), all(pd >= 0), all(pn >= 0))

p_rate <- ggplot(nitrogen_rates, aes(wfps, rate, colour = pathway)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("Nitrification" = blue, "Denitrification" = green)) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(x = "Water-filled pore space (%)", y = "Relative N transformation rate", colour = NULL)

p_gas <- ggplot(gas_rates, aes(wfps, rate, colour = pathway, linetype = pathway)) +
  geom_line(linewidth = 1.15) +
  scale_colour_manual(values = c("From nitrification" = blue, "From denitrification" = green, "Total" = vermillion)) +
  scale_linetype_manual(values = c("From nitrification" = "dashed", "From denitrification" = "dotted", "Total" = "solid")) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(x = "Water-filled pore space (%)", y = "Net N₂O production (arbitrary units)",
       colour = NULL, linetype = NULL) +
  guides(colour = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1))

save_figure("moisture_response", 11, 5, function() {
  print(cowplot::plot_grid(p_rate, p_gas, nrow = 1, align = "h", axis = "tb",
                          labels = c("a", "b"), label_size = 14,
                          label_fontfamily = font_family))
})

# Full rsofun ntransform.mod.f90, evaluated independently for each moisture.
# See ntransform.R for the pinned source and complete daily pool updates.
source(file.path("analysis", "ntransform.R"))
# Explicit demonstration parameters, not a calibrated site simulation.
# maxnitr = 0.1 d-1 follows the value reported by Xu-Ri & Prentice (2008).
# Other values follow analysis/example_cnmodel.R at the pinned commit.
# Its maxnitr = 0.00005 is labelled as reinterpreted for the SIMPLE routine;
# that decay coefficient is not used here as a full-model nitrification rate.
moisture_params <- list(maxnitr = 0.1, non = 0.01, n2on = 0.0005,
                        kn = 83, kdoc = 17, docmax = 1, dnitr2n2o = 0.01)
moisture_initial <- ntransform_state(nh4 = 1, no3 = 1, doc = 10)
w <- seq(0, 1, length.out = 1001)
moisture_steps <- lapply(w, function(water) {
  ntransform_full(moisture_initial, temp = 20, wscal = water,
                  aprec = 1000, params = moisture_params)
})
moisture_fluxes <- t(vapply(moisture_steps, function(step) step$fluxes,
                            moisture_steps[[1]]$fluxes))
n <- moisture_fluxes[, "dnitr"]
d <- moisture_fluxes[, "ddenitr"]
# Production is in g N m-2 d-1; convert to mg N for a readable axis.
# Do not multiply by the escape fraction: emission is a separate output (dn2o).
pn <- 1000 * moisture_fluxes[, "n2o_nitrification"]
pd <- 1000 * moisture_fluxes[, "n2o_denitrification"]

nitrogen_rates <- data.frame(
  water = rep(100*w, 2),
  rate = c(n / max(n), d / max(d)),
  pathway = rep(c("Nitrification", "Denitrification"),
  each = length(w))
)

gas_rates <- data.frame(
  water = rep(100*w, 3),
  rate = c(pn, pd, pn + pd),
  pathway = rep(c("From nitrification", "From denitrification", "Total"),
  each = length(w))
)

stopifnot(all(n >= 0), all(d >= 0), all(pd >= 0), all(pn >= 0))

p_rate <- ggplot(nitrogen_rates, aes(water, rate, colour = pathway)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("Nitrification" = blue, "Denitrification" = green)) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(x = "Plant-available water (% of capacity)",
       y = "Relative N transformation rate", colour = NULL)

p_gas <- ggplot(gas_rates, aes(water, rate, colour = pathway, linetype = pathway)) +
  geom_line(linewidth = 1.15) +
  scale_colour_manual(values = c("From nitrification" = blue, "From denitrification" = green, "Total" = vermillion)) +
  scale_linetype_manual(values = c("From nitrification" = "dashed", "From denitrification" = "dotted", "Total" = "solid")) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(x = "Plant-available water (% of capacity)",
       y = "N₂O production (mg N m⁻² d⁻¹)",
       colour = NULL, linetype = NULL) +
  guides(colour = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1))

save_figure("moisture_response_full", 11, 5, function() {
  print(cowplot::plot_grid(p_rate, p_gas, nrow = 1, align = "h", axis = "tb",
                          labels = c("a", "b"), label_size = 14,
                          label_fontfamily = font_family))
})

# Original schematic. Gas escape is distinct from a chemical transformation.
# Rooted in Stocker's thesis (2013), Fig. 1.5, and Xu-Ri & Prentice (2008).
save_figure("soil_transformations", 11, 6.8, function() {
  grid.newpage()
  text_at <- function(label, x, y, size = 12, ...) {
    grid.text(label, x, y, gp = gpar(fontfamily = font_family, fontsize = size, col = ink, ...))
  }
  box <- function(label, x, y, width = .13, fill = pale(blue), size = 15, edge = blue) {
    grid.roundrect(x, y, width, .09, r = unit(.025, "snpc"),
                   gp = gpar(fill = fill, col = edge, lwd = 1.2))
    text_at(label, x, y, size)
  }
  arrow_at <- function(x0, y0, x1, y1, col = ink, dashed = FALSE) {
    grid.lines(c(x0, x1), c(y0, y1),
               arrow = arrow(length = unit(.09, "inches"), type = "closed"),
               gp = gpar(col = col, lwd = 1.6, lty = if (dashed) 2 else 1))
  }
  grid.rect(.5, .935, 1, .13, gp = gpar(fill = pale(blue), col = NA))
  grid.rect(.5, .15, 1, .14, gp = gpar(fill = pale(blue), col = NA))
  text_at("ATMOSPHERE", .105, .95, 11, fontface = "bold")
  text_at("ATMOSPHERE", .855, .15, 11, fontface = "bold")
  text_at("Solid arrows: transformations     Dashed arrows: gas transfer", .5, .035, 11)
  text_at("NH₃", .31, .935, 16)
  text_at("NO / N₂O", .58, .935, 16)
  box("Organic N", .105, .64, .17, pale(green), edge = green)
  box("NH₄⁺", .31, .64)
  box("NO₂⁻", .55, .64)
  box("NO₃⁻", .82, .64)
  box("NH₃", .31, .80)
  box("NO / N₂O", .58, .80, .17, pale(vermillion), 13, edge = vermillion)
  arrow_at(.2, .66, .235, .66, green)
  text_at("Mineralisation", .19, .725, 10)
  arrow_at(.235, .615, .2, .615, green)
  text_at("Immobilisation", .19, .555, 10)
  arrow_at(.3, .69, .3, .745, ink)
  arrow_at(.32, .745, .32, .69, ink)
  text_at("Higher pH\nfavours NH₃", .14, .82, 10)
  arrow_at(.31, .85, .31, .895, vermillion, TRUE)
  arrow_at(.385, .64, .475, .64, blue)
  arrow_at(.625, .64, .745, .64, blue)
  text_at("Nitrification", .66, .56, 12, fontface = "bold")
  text_at("Oxygen available", .66, .515, 10)
  arrow_at(.43, .65, .505, .765, vermillion)
  text_at("By-products", .46, .855, 10)
  arrow_at(.58, .85, .58, .895, vermillion, TRUE)
  # The nitrate formed above is the same pool reduced along the lower row.
  arrow_at(.82, .59, .82, .425, green)
  box("NO₂⁻", .82, .37)
  box("NO", .60, .37)
  box("N₂O", .38, .37)
  box("N₂", .16, .37)
  for (x in c(.82, .60, .38)) arrow_at(x-.075, .37, x-.145, .37, green)
  text_at("Denitrification: oxygen scarce", .295, .46, 12, fontface = "bold")
  for (i in seq_along(c(.16, .38, .60))) {
    x <- c(.16, .38, .60)[i]
    arrow_at(x, .315, x, .205, vermillion, TRUE)
    text_at(c("N₂", "N₂O", "NO")[i], x, .15, 16)
  }
})
