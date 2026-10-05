# Recreate the three nitrogen figures formerly generated in Python.
# From the repository root:
#   Rscript --vanilla analysis/plot_nitrogen_cycle.R
#   R --vanilla -q -e 'source("analysis/plot_nitrogen_cycle.R")'
# Rscript also accepts this script's absolute path from another directory.
# Requires ggplot2 and cowplot; grid and the graphics devices are included with R.
# Outputs: ecosystem_n_cycle, mineralisation_stoichiometry, and
# nutrient_release_curves, each as SVG, PNG, and PDF in book/images/nitrogen/.
#
# The schematic follows the process framework in Stocker et al. (2016),
# doi:10.1111/nph.13997. The illustrative stoichiometric balance follows
# Stocker & Prentice (2024), doi:10.1101/2024.04.25.591063.
# Nutrient-release curves use Eq. 1 of Manzoni et al. (2008),
# doi:10.1126/science.1159792, expressed using C:N mass ratios.
# None of these figures contains observations or externally sourced images.

library(ggplot2)
library(grid)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
project_root <- if (length(script_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))))
} else {
  getwd()
}
if (!file.exists(file.path(project_root, "book", "nitrogen.qmd"))) {
  stop("When using source(), run from the repository root; otherwise use Rscript with the script path.")
}
out <- file.path(project_root, "book", "images", "nitrogen")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Match analysis/plot_nitrogen.R: Helvetica, classic axes, Okabe–Ito colours.
font_family <- "Helvetica"
ink <- "#222222"
blue <- "#0072B2"
green <- "#009E73"
vermillion <- "#D55E00"
purple <- "#CC79A7"
pale <- function(colour) adjustcolor(colour, alpha.f = .07)
theme_nitrogen <- theme_classic(base_size = 12, base_family = font_family) +
  theme(text = element_text(colour = ink),
        axis.text = element_text(colour = ink),
        axis.line = element_line(colour = ink, linewidth = .4),
        axis.ticks = element_line(colour = ink, linewidth = .4),
        plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.caption = element_text(hjust = 0, size = 10),
        legend.title = element_text(size = 10),
        legend.background = element_blank())

# Explicit graphics devices avoid an additional svglite dependency.
# The on.exit handler also closes the device if drawing fails.
save_figure <- function(name, width, height, draw) {
  draw_file <- function(extension) {
    path <- file.path(out, paste0(name, ".", extension))
    if (extension == "svg") {
      svg(path, width = width, height = height,
          family = font_family, bg = "white")
    } else if (extension == "pdf") {
      cairo_pdf(path, width = width, height = height,
                family = font_family, bg = "white")
    } else {
      png(path, width = width, height = height, units = "in", res = 220,
          type = "cairo", family = font_family, bg = "white")
    }
    on.exit(dev.off())
    draw()
  }
  invisible(lapply(c("svg", "png", "pdf"), draw_file))
}

ecosystem_cycle <- function() {
  # Diagram coordinates are independent of pool sizes and flux magnitudes.
  rect <- function(x, y, width, height, colour, fill = pale(colour)) {
    annotate("rect", xmin = x, xmax = x + width, ymin = y, ymax = y + height,
             colour = colour, fill = fill, linewidth = .65)
  }
  txt <- function(x, y, label, size = 12, ...) {
    annotate("text", x = x, y = y, label = label, size = size / (72.27 / 25.4),
             family = font_family, colour = ink, lineheight = 1.05, ...)
  }
  flow <- function(x, y, xend, yend, colour, width = .7, curve = 0,
                   head = .09) {
    dat <- data.frame(x, y, xend, yend)
    mapping <- aes(x = x, y = y, xend = xend, yend = yend)
    tip <- arrow(length = unit(head, "inches"), type = "closed")
    if (curve == 0) {
      geom_segment(data = dat, mapping = mapping, colour = colour,
                   linewidth = width, arrow = tip, lineend = "round")
    } else {
      geom_curve(data = dat, mapping = mapping, colour = colour,
                 linewidth = width, arrow = tip, curvature = curve,
                 lineend = "round", ncp = 30)
    }
  }
  p <- ggplot() +
    # Inputs and plant-internal recycling.
    rect(.8, 9.55, 3, .85, blue) +
    txt(2.3, 9.975, "Atmospheric N₂", 13) +
    rect(12, 9.55, 3.2, .85, blue) +
    txt(13.6, 9.975, "Reactive atmospheric N", 11.5) +
    rect(5.15, 7.35, 5.4, 1.95, green) +
    txt(7.85, 8.98, "Plant organic N", 13) +
    rect(5.55, 7.64, 1.75, .7, green, "white") +
    txt(6.425, 7.99, "Leaves", 11) +
    rect(8.32, 7.64, 1.85, .7, green, "white") +
    txt(9.245, 7.99, "Other tissues\n& N stores", 10.5) +
    flow(7.34, 7.99, 8.28, 7.99, green, .6) +
    txt(7.8, 8.51, "Resorption", 10) +
    flow(3.84, 9.77, 5.11, 8.65, blue) +
    txt(5.35, 10.12, "Symbiotic N fixation", 11) +
    flow(1.14, 9.50, 1.26, 3.58, blue, curve = .19) +
    txt(.48, 6.83, "Free-living N fixation", 10.5, angle = 90) +
    flow(12.72, 9.51, 12.72, 5.79, blue) +
    txt(13.99, 7.78, "Wet and dry\nN deposition", 11) +
    # Soil organic pools, microbial turnover, and recycling.
    rect(1.3, 2.75, 5.3, 3, purple) +
    txt(3.95, 5.42, "Soil organic N", 13) +
    txt(3.95, 4.84, "Litter", 12) +
    txt(3.95, 3.90, "Decomposer biomass", 12) +
    txt(3.95, 3.02, "Soil organic matter", 12) +
    flow(3.95, 4.61, 3.95, 4.14, purple, .55, head = .07) +
    flow(3.95, 3.67, 3.95, 3.27, purple, .55, head = .07) +
    txt(5.25, 4.41, "Decomposition", 9.3) +
    txt(5.22, 3.44, "Turnover", 9.3) +
    flow(2.52, 3.08, 2.48, 3.83, purple, .5, curve = -.45, head = .07) +
    txt(1.96, 3.48, "Recycling", 8.8, angle = 90) +
    flow(5.64, 7.31, 4.34, 5.79, green) +
    txt(3.05, 6.70, "Leaf abscission,\nroot turnover & mortality", 10.8) +
    flow(6.40, 5.79, 6.40, 7.31, green, .6) +
    txt(7.64, 6.5, "Organic N\ntransfer", 10.3) +
    # Mineral pools and plant uptake.
    rect(9.25, 2.75, 4.15, 3, blue) +
    txt(11.32, 5.4, "Soil mineral N", 13) +
    txt(10.24, 4.37, "NH₄⁺", 17) +
    txt(12.35, 4.37, "NO₃⁻", 17) +
    flow(10.74, 4.37, 11.79, 4.37, blue, .6) +
    txt(11.28, 3.83, "Nitrification", 10.5) +
    txt(11.32, 3.13, "Available for biological uptake", 9.5) +
    flow(9.68, 5.79, 9.68, 7.31, green) +
    txt(10.94, 6.55, "Root and\nmycorrhizal uptake", 10.7) +
    flow(6.64, 4.77, 9.21, 4.77, purple) +
    txt(7.91, 5.1, "Mineralisation", 10.5) +
    flow(9.21, 3.54, 6.64, 3.54, purple) +
    txt(7.93, 3.2, "Immobilisation", 10.5) +
    # Gaseous and dissolved export.
    rect(14.22, 3.93, 1.48, 1.17, vermillion) +
    txt(14.96, 4.515, "N₂, N₂O\n(+ NO)", 11.5) +
    flow(13.44, 4.51, 14.18, 4.51, vermillion) +
    txt(14.76, 5.55, "Gaseous loss", 10.5) +
    flow(11.94, 2.71, 13.20, 2.04, vermillion) +
    flow(5.04, 2.71, 13.20, 2.02, vermillion, .6, curve = .07) +
    txt(14.14, 2.04, "Leaching", 11) +
    txt(11.76, 2.4, "NO₃⁻", 9.5) +
    txt(8.28, 1.83, "Dissolved organic N", 9.5)

    # # Chemical sequences use vector arrows, avoiding missing font glyphs.
    # rect(1.3, .39, 14.37, 1.12, "#BDBDBD", "white") +
    # txt(2.48, .95, "Soil chemistry", 10.5) +
    # txt(3.8, 1.12, "Nitrification:", 10.2, hjust = 0) +
    # txt(3.8, .73, "Denitrification:", 10.2, hjust = 0)

  chemistry_row <- function(species, x, y, suffix = NULL) {
    layers <- list()
    for (i in seq_along(species)) {
      layers <- c(layers, list(txt(x[i], y, species[i], 10.2)))
      if (i < length(species)) {
        layers <- c(layers, list(flow(x[i] + .30, y, x[i + 1] - .30, y,
                                      ink, .35, head = .045)))
      }
    }
    if (!is.null(suffix)) {
      layers <- c(layers, list(txt(tail(x, 1) + .40, y, suffix, 10.2, hjust = 0)))
    }
    layers
  }
  p + 
    # chemistry_row(c("NH₄⁺", "NO₂⁻", "NO₃⁻"), c(5.7, 6.7, 7.7), 1.12,
    #                  "; N₂O can be produced") +
    # chemistry_row(c("NO₃⁻", "NO₂⁻", "NO", "N₂O", "N₂"),
    #               c(5.9, 6.9, 7.9, 8.9, 9.9), .73) +
    # txt(8.4, .05, "Conceptual schematic; arrows are not scaled", 9.5) +
    coord_fixed(xlim = c(0, 16), ylim = c(0, 10.85), expand = FALSE, clip = "off") +
    theme_nitrogen +
    theme(axis.line = element_blank(), axis.title = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank(),
          plot.margin = margin(8, 8, 8, 8))
}

mineralisation_stoichiometry <- function() {
  r_b <- 8
  efficiencies <- c(.2, .4, .6)
  dat <- expand.grid(r_l = seq(8, 100, length.out = 900), efficiency = efficiencies)
  dat$release <- 1 / dat$r_l - dat$efficiency / r_b
  dat$series <- factor(dat$efficiency, levels = efficiencies)
  thresholds <- data.frame(efficiency = efficiencies, r_l = r_b / efficiencies)
  thresholds$series <- factor(thresholds$efficiency, levels = efficiencies)
  thresholds$label <- format(round(thresholds$r_l, 1), trim = TRUE, nsmall = 0)
  # Check the accounting at the zero crossings and in the chapter's example.
  stopifnot(all(abs(1 / thresholds$r_l - efficiencies / r_b) < 1e-12),
            abs(100 * (1 / 50 - .4 / r_b) - (-3)) < 1e-12)

  plot <- ggplot(dat, aes(r_l, release, colour = series)) +
    geom_hline(yintercept = 0, colour = ink, linewidth = .4) +
    geom_line(linewidth = 1) +
    geom_segment(data = thresholds,
                 aes(x = r_l, xend = r_l, y = -.075, yend = 0, colour = series),
                 inherit.aes = FALSE, linetype = "dashed", linewidth = .4,
                 show.legend = FALSE) +
    geom_point(data = thresholds, aes(x = r_l, y = 0, colour = series),
               inherit.aes = FALSE, size = 2.4, show.legend = FALSE) +
    geom_label(data = thresholds, aes(x = r_l, y = -.070, label = label),
               inherit.aes = FALSE, family = font_family, colour = ink,
               fill = "white", linewidth = 0, size = 10 / (72.27 / 25.4),
               label.padding = unit(.07, "lines"), show.legend = FALSE) +
    annotate("text", x = 98, y = .102, label = "Net mineralisation", hjust = 1,
             family = font_family, colour = ink, size = 12 / (72.27 / 25.4)) +
    annotate("text", x = 98, y = -.006, label = "Net immobilisation", hjust = 1,
             family = font_family, colour = ink, size = 12 / (72.27 / 25.4)) +
    annotate("text", x = 97, y = -.070, label = "Threshold C:N", hjust = 1,
             family = font_family, colour = ink, size = 9.5 / (72.27 / 25.4)) +
    scale_colour_manual(values = c(blue, green, vermillion),
                         labels = parse(text = paste0("epsilon == ", efficiencies))) +
    scale_x_continuous(breaks = seq(20, 100, 20), expand = expansion(mult = 0)) +
    scale_y_continuous(breaks = seq(-.06, .10, .02), expand = expansion(mult = 0)) +
    coord_cartesian(xlim = c(8, 100), ylim = c(-.075, .115)) +
    labs(x = expression("Litter C:N mass ratio, " * R[L] * " (g C " * g~N^{-1} * ")"),
         y = expression("Net N release per decomposed C (g N " * g~C^{-1} * ")"),
         colour = "Microbial C-use efficiency") +
    theme_nitrogen +
    theme(legend.position = "inside", legend.position.inside = c(.96, .85),
          legend.justification = c(1, 1), plot.margin = margin(10, 12, 10, 10))

  note_text <- function(y, label, size = 11, parse = FALSE, ...) {
    annotate("text", x = 0, y = y, label = label, family = font_family,
             colour = ink, hjust = 0, size = size / (72.27 / 25.4),
             parse = parse, ...)
  }
  note <- ggplot() +
    note_text(.855, "'Decomposed litter carbon: '*D", parse = TRUE) +
    note_text(.745, "'N supplied by litter: '*D/R[L]", parse = TRUE) +
    note_text(.635, "'New microbial carbon: '*epsilon*D", parse = TRUE) +
    note_text(.525, "'Microbial N demand: '*epsilon*D/R[B]", parse = TRUE) +
    annotate("segment", x = 0, xend = .98, y = .47, yend = .47,
             colour = "#BDBDBD", linewidth = .4) +
    note_text(.375, "frac(M[N], D) == frac(1, R[L]) - frac(epsilon, R[B])",
              22, parse = TRUE) +
    note_text(.235, "'Zero net release when '*R[L] == R[B]/epsilon", parse = TRUE) +
    note_text(.135, "Here microbial C:N is fixed:") +
    note_text(.075, "R[B] == 8~g~C~g~N^{-1}", parse = TRUE) +
    coord_cartesian(xlim = c(0, 1.05), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_nitrogen +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), axis.title = element_blank(),
          plot.margin = margin(10, 15, 10, 10))

  # The companion panel gives the accounting behind the response curves.
  function() {
    # print(cowplot::plot_grid(
    #   plot,
    #   note,
    #   nrow = 1,
    #   rel_widths = c(.64, .36),
    #   labels = c("a", "b"),
    #   label_size = 14,
    #   label_fontfamily = font_family
    # ))
    print(plot)
  }
}

nutrient_release_curves <- function() {
  # Manzoni et al. (2008), Eq. 1. Their N:C ratio quotient r_B/r_L0 is
  # R_L0/R_B with the C:N mass-ratio convention used throughout this chapter.
  r_b <- 8
  efficiency <- .4
  initial_cn <- c(12, 25, 60)
  exponent <- 1 / (1 - efficiency)
  dat <- expand.grid(lost = seq(0, 1, length.out = 901), initial_cn = initial_cn)
  dat$c <- 1 - dat$lost
  dat$ratio <- dat$initial_cn / r_b
  dat$n <- dat$c * dat$ratio + (1 - dat$ratio) * dat$c^exponent
  dat$series <- factor(dat$initial_cn, levels = initial_cn)

  peaks <- data.frame(initial_cn = initial_cn[initial_cn > r_b / efficiency])
  ratio <- peaks$initial_cn / r_b
  peaks$c <- (ratio / ((ratio - 1) * exponent))^(1 / (exponent - 1))
  peaks$lost <- 1 - peaks$c
  peaks$n <- ratio * peaks$c + (1 - ratio) * peaks$c^exponent
  peaks$series <- factor(peaks$initial_cn, levels = initial_cn)
  largest_peak <- peaks[peaks$initial_cn == 60, ]
  stopifnot(all(abs(dat$n[dat$lost == 0] - 1) < 1e-12),
            all(dat$n[dat$lost == 1] == 0), all(dat$n >= 0))

  ggplot(dat, aes(lost, n, colour = series)) +
    geom_hline(yintercept = 1, colour = ink, linetype = "dashed", linewidth = .45) +
    geom_line(linewidth = 1.1) +
    geom_point(data = peaks, aes(lost, n, colour = series), size = 2.5, show.legend = FALSE) +
    annotate("text", x = .06, y = 1.93, label = "Peak: onset of net N release",
             family = font_family, colour = ink, hjust = 0, size = 10.5 / (72.27 / 25.4)) +
    annotate("segment", x = .24, y = 1.87, xend = largest_peak$lost,
             yend = largest_peak$n, colour = vermillion, linewidth = .45) +
    annotate("text", x = .99, y = 1.045, label = "Initial N content",
             family = font_family, colour = ink, hjust = 1, size = 10 / (72.27 / 25.4)) +
    scale_colour_manual(values = c(blue, green, vermillion),
                         labels = parse(text = paste0("R[L*','*0] == ", initial_cn))) +
    scale_x_continuous(breaks = seq(0, 1, .2), expand = expansion(mult = 0)) +
    scale_y_continuous(breaks = seq(0, 2, .25), expand = expansion(mult = 0)) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.02)) +
    labs(x = expression("Fraction of initial litter C lost, " * 1 - C/C[0]),
         y = expression("N remaining relative to initial litter N, " * N/N[0]),
         colour = "Initial litter C:N\n(g C per g N)",
         caption = paste("Illustrative fixed parameters: microbial C:N = 8; C-use efficiency = 0.4. No observations shown.",
                         "Rising curves indicate net immobilisation; falling curves indicate net release.",
                         "The horizontal axis tracks decomposition progress, not elapsed time.", sep = "\n")) +
    theme_nitrogen +
    theme(legend.position = "inside", legend.position.inside = c(.99, .99),
          legend.justification = c(1, 1),
          plot.caption = element_text(margin = margin(t = 12)),
          plot.margin = margin(12, 14, 12, 12))
}

save_figure("ecosystem_n_cycle", 13.5, 9.2, function() print(ecosystem_cycle()))
save_figure("mineralisation_stoichiometry", 12, 7.1, mineralisation_stoichiometry())
save_figure("nutrient_release_curves", 8.9, 6.15, function() print(nutrient_release_curves()))
message("Wrote SVG, PNG, and PDF figures to ", out)
