# Reproduce the three figures added in the nitrogen-chapter revision.
# Run from the repository root:
# R --vanilla -q -e 'source("analysis/plot_nitrogen.R")'
# The ecosystem and decomposition figures are in plot_nitrogen_cycle.R.
# Requires ggplot2; uses base R graphics devices, not svglite.
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
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"))

save_figure <- function(name, width, height, draw) {

  svg(file.path(out, paste0(name, ".svg")), width, height,
      family = font_family, bg = "white")
  draw()
  dev.off()

  png(file.path(out, paste0(name, ".png")), width = width, height = height,
      units = "in", res = 180, type = "cairo", family = font_family, bg = "white")
  draw()
  dev.off()
  
}

# NOAA Global Monitoring Laboratory, approximate mid-2020 dry-air composition:
# https://gml.noaa.gov/ccgg/about/co2_measurements.html
# Other = Ne 18 + He 5 + CH4 2 + Kr 1 + remaining trace gases 1 ppm.
air <- data.frame(
  gas = c("Nitrogen", "Oxygen", "Argon", "Carbon dioxide", "Other"),
  ppm = c(780900, 209360, 9300, 413, 27))
stopifnot(sum(air$ppm) == 1e6)
air$gas <- factor(air$gas, levels = air$gas)
air$pct <- air$ppm / 10000
palette <- c("Nitrogen" = blue, "Oxygen" = sky_blue, "Argon" = orange,
             "Carbon dioxide" = vermillion, "Other" = purple)
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
  annotate("text", x = 1, y = 88.5, label = "Oxygen (O₂)\n20.936%", size = 4, colour = ink, family = font_family) +
  labs(title = "A  |  Dry lower atmosphere", subtitle = "Approximate composition at mid-2020") +
  theme(legend.position = "none")
p_minor <- air_plot(air[3:5, ], .974, c(0, .2, .4, .6, .8)) +
  annotate("text", x = 1, y = .44, label = "Argon (Ar)  0.930%", size = 4.5, colour = ink, family = font_family) +
  labs(title = "B  |  The remaining 0.974%, enlarged",
       subtitle = "CO₂: 0.0413% (413 ppm)     •     Other gases: 0.0027% (27 ppm)") +
  guides(fill = guide_legend(nrow = 1))
save_figure("atmospheric_composition", 10, 5.8, function() {
  grid.newpage()
  print(p_all, vp = viewport(x = .5, y = .75, width = .98, height = .48))
  print(p_minor, vp = viewport(x = .5, y = .25, width = .98, height = .49))
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
  labs(title = "A  |  N transformation", subtitle = "Each pathway scaled to its own maximum",
       x = "Water-filled pore space (%)", y = "Relative rate", colour = NULL)

p_gas <- ggplot(gas_rates, aes(wfps, rate, colour = pathway, linetype = pathway)) +
  geom_line(linewidth = 1.15) +
  scale_colour_manual(values = c("From nitrification" = blue, "From denitrification" = green, "Total" = vermillion)) +
  scale_linetype_manual(values = c("From nitrification" = "dashed", "From denitrification" = "dotted", "Total" = "solid")) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(title = "B  |  Net N₂O production", subtitle = "After reduction to N₂; illustrative common scale",
       x = "Water-filled pore space (%)", y = "Arbitrary units", colour = NULL, linetype = NULL) +
  guides(colour = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1))

save_figure("moisture_response", 11, 5, function() {
  # Align the plotting areas despite different numbers of legend entries.
  g_rate <- ggplotGrob(p_rate)
  g_gas <- ggplotGrob(p_gas)
  common_heights <- unit.pmax(g_rate$heights, g_gas$heights)
  g_rate$heights <- common_heights
  g_gas$heights <- common_heights
  grid.newpage()
  pushViewport(viewport(x = .25, y = .5, width = .48, height = .98))
  grid.draw(g_rate)
  popViewport()
  pushViewport(viewport(x = .75, y = .5, width = .48, height = .98))
  grid.draw(g_gas)
  popViewport()
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
