# Plot and summarise the observed C:N distributions for book/nitrogen.qmd.
# Run from the repository root after prepare_nitrogen_cn.R:
# Rscript --vanilla analysis/plot_nitrogen_cn.R
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
})

data_dir <- "data/nitrogen_cn/derived"
figure_dir <- "book/images/nitrogen"
observations <- read_csv(file.path(data_dir, "cn_observations.csv.gz"),
                         show_col_types = FALSE, progress = FALSE)
compartments <- c("Green leaves", "Senesced leaves", "Fine roots", "Coarse roots",
                  "Wood (initial samples)", "Leaf litter (LIDET)",
                  "Root litter (LIDET)", "Bulk soil organic matter",
                  "Microbial biomass", "Fungi")
stopifnot(setequal(unique(observations$compartment), compartments),
          all(is.finite(observations$cn_mass) & observations$cn_mass > 0))
observations$compartment <- factor(observations$compartment, levels = compartments)

# Each source observation has equal weight; these are not area-weighted global
# distributions. Fungal observations are species means (see preparation script).
summary <- observations |>
  group_by(compartment, source) |>
  summarise(n = n(), minimum = min(cn_mass),
            p05 = quantile(cn_mass, .05), p25 = quantile(cn_mass, .25),
            median = median(cn_mass), p75 = quantile(cn_mass, .75),
            p95 = quantile(cn_mass, .95), maximum = max(cn_mass), .groups = "drop")
write_csv(summary, file.path(data_dir, "cn_summary.csv"))

citations <- c("[@vergutz2012data]", "[@vergutz2012data]",
               "[@nikitin2026fred4]", "[@nikitin2026fred4]",
               "[@wijas2024data; @wijas2025]", "[@harmon2016lidet]",
               "[@harmon2016lidet]", "[@calisto2023wosis; @batjes2024wosis]",
               "[@xu2014microbialdata]", "[@zhangelser2017]")
labels <- c("Green leaves", "Senesced leaves", "Fine roots (living)",
            "Coarse roots (living)", "Wood (initial samples)",
            "Leaf litter (0–9 yr)", "Fine-root litter (0–9 yr)",
            "Bulk SOM (soil OC/total N, 0–30 cm)", "Microbial biomass",
            "Fungi (species means)")
table_lines <- c(
  "| Compartment | Median C:N | 25th–75th percentile | 5th–95th percentile | $n$ | Source |",
  "|:--|--:|:--|:--|--:|:--|",
  sprintf("| %s | %.1f | %.1f–%.1f | %.1f–%.1f | %s | %s |", labels,
          summary$median, summary$p25, summary$p75, summary$p05, summary$p95,
          format(summary$n, big.mark = ",", trim = TRUE), citations))
writeLines(table_lines, file.path(data_dir, "cn_table.qmd"))

# Optional explicit refresh of the chapter's table body. Preserve its caption
# and the surrounding prose, including user edits outside the marked table.
if ("--update-chapter" %in% commandArgs(trailingOnly = TRUE)) {
  chapter_path <- "book/nitrogen.qmd"
  chapter <- readLines(chapter_path, warn = FALSE)
  first <- grep("<!-- BEGIN generated C:N table:", chapter, fixed = TRUE)
  last <- grep("<!-- END generated C:N table -->", chapter, fixed = TRUE)
  caption <- grep("^: C:N by mass", chapter)
  stopifnot(length(first) == 1, length(last) == 1, length(caption) == 1,
            first < caption, caption < last)
  chapter <- c(chapter[seq_len(first)], "", table_lines, "",
               chapter[seq.int(caption, length(chapter))])
  writeLines(chapter, chapter_path)
}

# Okabe–Ito colours. Shared colours link related tissues and decomposing litter.
palette <- setNames(c("#009E73", "#E69F00", "#0072B2", "#56B4E9", "#D55E00",
                      "#E69F00", "#E69F00", "#000000", "#CC79A7", "#56B4E9"),
                    compartments)
plot_labels <- setNames(c("Green leaves", "Senesced leaves", "Fine roots",
                         "Coarse roots", "Wood (initial samples)",
                         "Leaf litter", "Fine-root litter",
                         "Bulk SOM (0–30 cm)", "Microbial biomass",
                         "Fungi (species means)"), compartments)

# Log transformation precedes density estimation. All positive observations
# enter the violin, including extreme soil ratios; no ratio cutoffs are applied.
p <- ggplot(observations, aes(x = cn_mass, y = compartment, fill = compartment)) +
  geom_violin(orientation = "y", scale = "width", trim = TRUE,
              width = .82, alpha = .7, colour = NA, adjust = .9) +
  geom_segment(data = summary, aes(x = p05, xend = p95,
                                   yend = compartment), linewidth = .45) +
  geom_segment(data = summary, aes(x = p25, xend = p75,
                                   yend = compartment), linewidth = 2.1,
               lineend = "round") +
  geom_point(data = summary, aes(x = median), shape = 21, fill = "white",
             size = 2.5, stroke = .6) +
  geom_text(data = summary, aes(x = Inf, label = paste0("n = ",
             format(n, big.mark = ",", trim = TRUE))), hjust = 1, size = 3.3,
             family = "Helvetica", colour = "#222222") +
  scale_x_log10(breaks = c(.1, 1, 10, 100, 1000, 10000),
                labels = c("0.1", "1", "10", "100", "1,000", "10,000"),
                expand = expansion(mult = c(.015, .24))) +
  scale_y_discrete(limits = rev(compartments), labels = plot_labels,
                   expand = expansion(add = .55)) +
  scale_fill_manual(values = palette, guide = "none") +
  labs(x = expression("C:N by mass (g C per g N; logarithmic scale)"), y = NULL) +
  theme_classic(base_size = 12, base_family = "Helvetica") +
  theme(axis.text = element_text(colour = "#222222"),
        axis.text.y = element_text(size = 11, margin = margin(r = 9)),
        axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin = margin(8, 10, 8, 8))

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
save_plot <- function(extension) {
  path <- file.path(figure_dir, paste0("cn_distributions.", extension))
  if (extension == "svg") {
    svg(path, width = 10, height = 6.4, family = "Helvetica", bg = "white")
  } else if (extension == "pdf") {
    cairo_pdf(path, width = 10, height = 6.4, family = "Helvetica", bg = "white")
  } else {
    png(path, width = 10, height = 6.4, units = "in", res = 180,
        type = "cairo", family = "Helvetica", bg = "white")
  }
  on.exit(dev.off())
  print(p)
}
invisible(lapply(c("svg", "pdf", "png"), save_plot))
writeLines(capture.output(sessionInfo()), file.path(data_dir, "session_info.txt"))
print(summary, width = Inf)
