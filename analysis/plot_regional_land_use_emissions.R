# Regional net land-use-change carbon emissions, GCB 2025 (1850-2024).
# Run: Rscript analysis/plot_regional_land_use_emissions.R
# Dependencies: dplyr, tidyr, purrr, readr, tibble, ggplot2, readxl, countrycode, here, digest.
# Downloads the pinned CC BY 4.0 workbook once; subsequent runs use the cache.

gcb_models <- c("BLUE", "OSCAR", "LUCE")
gcb_regions <- c("North America", "Latin America & Caribbean", "Europe excluding Russia",
                 "Russia", "Africa", "East Asia", "South & Southeast Asia",
                 "West & Central Asia", "Oceania", "Other / disputed territories")
gcb_source <- "https://meta.icos-cp.eu/objects/milTbWkl0G-MSpdYG3IBIfzy"
gcb_download <- "https://data.icos-cp.eu/licence_accept?ids=%5B%22milTbWkl0G-MSpdYG3IBIfzy%22%5D"
gcb_sha256 <- "9a29536d6925d06f8c4a97581b720121fcf219732c240e970bc24167d74e38d1"

download_gcb_luc <- function(data_dir) {
  path <- file.path(data_dir, "National_LandUseChange_Carbon_Emissions_2025_v1.0.xlsx")
  if (!file.exists(path)) {
    partial <- paste0(path, ".part")
    on.exit(unlink(partial), add = TRUE)
    utils::download.file(gcb_download, partial, mode = "wb", method = "libcurl")
    stopifnot(identical(digest::digest(file = partial, algo = "sha256"), gcb_sha256))
    stopifnot(file.rename(partial, path))
  }
  stopifnot(identical(digest::digest(file = path, algo = "sha256"), gcb_sha256))
  path
}

gcb_country_regions <- function(countries, path) {
  if (file.exists(path)) {
    mapping <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    mapping <- tibble::tibble(country = countries) |>
      dplyr::mutate(
        lookup = dplyr::if_else(country %in% c("OTHER", "DISPUTED"), NA_character_, country),
        iso3 = countrycode::countrycode(lookup, "country.name", "iso3c"),
        un_region = countrycode::countrycode(lookup, "country.name", "un.region.name"),
        un_subregion = countrycode::countrycode(lookup, "country.name", "un.regionsub.name"),
        region = dplyr::case_when(
          country %in% c("OTHER", "DISPUTED") ~ "Other / disputed territories",
          iso3 == "RUS" ~ "Russia",
          un_subregion == "Northern America" ~ "North America",
          un_subregion == "Latin America and the Caribbean" ~ "Latin America & Caribbean",
          un_region == "Europe" ~ "Europe excluding Russia",
          un_region == "Africa" ~ "Africa",
          un_subregion == "Eastern Asia" ~ "East Asia",
          un_subregion %in% c("Southern Asia", "South-eastern Asia") ~ "South & Southeast Asia",
          un_subregion %in% c("Western Asia", "Central Asia") ~ "West & Central Asia",
          un_region == "Oceania" ~ "Oceania"
        )
      ) |>
      dplyr::select(-lookup)
    classified <- dplyr::filter(mapping, !country %in% c("OTHER", "DISPUTED"))
    stopifnot(!anyNA(classified$iso3), !anyNA(classified$un_region), !anyNA(mapping$region))
    readr::write_csv(mapping, path, na = "")
  }
  stopifnot(!anyDuplicated(mapping$country), setequal(mapping$country, countries),
            all(mapping$region %in% gcb_regions))
  dplyr::left_join(tibble::tibble(country = countries), mapping, by = "country")
}

prepare_gcb_luc <- function(path, data_dir) {
  sheets <- purrr::map(gcb_models, function(model) {
    x <- readxl::read_excel(path, sheet = model, skip = 7, .name_repair = "minimal")
    stopifnot(names(x)[1] == "unit: Tg C/year", identical(as.integer(x[[1]]), 1850:2024),
              !anyDuplicated(names(x)), purrr::every(x, is.numeric),
              all(is.finite(as.matrix(x))))
    x
  }) |> purrr::set_names(gcb_models)
  stopifnot(purrr::every(sheets, function(x) identical(names(x), names(sheets[[1]]))))
  # Global and EU27 overlap the national series and must not be summed as countries.
  countries <- setdiff(names(sheets[[1]])[-1], c("Global", "EU27"))
  mapping <- gcb_country_regions(countries, file.path(data_dir, "country_regions.csv"))
  long <- purrr::imap(sheets, function(x, model) {
    x |>
      dplyr::rename(year = 1) |>
      tidyr::pivot_longer(-year, names_to = "country", values_to = "emissions_TgC_yr") |>
      dplyr::mutate(model = model, emissions_GtC_yr = emissions_TgC_yr / 1000)
  }) |> purrr::list_rbind()
  national <- long |>
    dplyr::filter(country %in% countries) |>
    dplyr::left_join(dplyr::select(mapping, country, region), by = "country")
  annual <- national |>
    dplyr::group_by(model, region, year) |>
    dplyr::summarise(emissions_GtC_yr = sum(emissions_GtC_yr), .groups = "drop") |>
    dplyr::select(year, model, region, emissions_GtC_yr)
  checks <- national |>
    dplyr::group_by(model, year) |>
    dplyr::summarise(regional_sum_GtC_yr = sum(emissions_GtC_yr), .groups = "drop") |>
    dplyr::left_join(long |>
      dplyr::filter(country == "Global") |>
      dplyr::transmute(model, year, source_global_GtC_yr = emissions_GtC_yr),
      by = c("model", "year")) |>
    dplyr::mutate(residual_GtC_yr = regional_sum_GtC_yr - source_global_GtC_yr) |>
    dplyr::select(year, model, dplyr::everything())
  # National source values are rounded to five decimal places in Tg C/year.
  stopifnot(max(abs(checks$residual_GtC_yr)) < 1e-6,
            nrow(annual) == 175 * length(gcb_models) * length(gcb_regions))
  readr::write_csv(annual, file.path(data_dir, "regional_annual_by_model.csv"))
  readr::write_csv(checks, file.path(data_dir, "global_reconciliation.csv"))
  annual
}

summarise_gcb_luc <- function(annual) {
  checks <- annual |>
    dplyr::group_by(region, year) |>
    dplyr::summarise(valid = dplyr::n() == 3L && setequal(model, gcb_models), .groups = "drop")
  stopifnot(all(checks$valid))
  summary <- annual |>
    dplyr::group_by(region, year) |>
    dplyr::summarise(mean_GtC_yr = mean(emissions_GtC_yr),
      min_GtC_yr = min(emissions_GtC_yr), max_GtC_yr = max(emissions_GtC_yr), .groups = "drop")
  stopifnot(nrow(summary) == 175 * dplyr::n_distinct(annual$region),
            all(summary$min_GtC_yr <= summary$mean_GtC_yr),
            all(summary$mean_GtC_yr <= summary$max_GtC_yr))
  summary
}

plot_gcb_luc <- function(d, output_dir, global = FALSE) {
  if (!global) {
    d <- d |>
      dplyr::filter(region != "Other / disputed territories") |>
      dplyr::mutate(region = factor(region, levels = gcb_regions[1:9]))
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(year, mean_GtC_yr)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = min_GtC_yr, ymax = max_GtC_yr), fill = "#8cb9c5", alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555", linewidth = 0.35) +
    ggplot2::geom_line(colour = "#205d73", linewidth = 0.8) +
    ggplot2::scale_x_continuous(limits = c(1850, 2025), breaks = c(1850, 1900, 1950, 2000, 2025)) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
    ggplot2::labs(title = if (global) "Global carbon emissions from land-use change" else "Historical carbon emissions from land-use change",
      subtitle = "Global Carbon Budget 2025 · 1850–2024 data\nLine: three-model mean; shading: model range. Annual values; no temporal smoothing.",
      x = "Year", y = expression("Net land-use-change emissions (Gt C " * yr^{-1} * ")"),
      caption = paste("BLUE, OSCAR and LUCE · Friedlingstein et al. (2026), doi:10.5194/essd-18-3211-2026",
        "Positive: net emissions. Negative: net removals associated with land use. Shading is not a confidence interval.",
        if (global) "Global total includes other/disputed territories. Land-use change before 1850 is not shown." else
          "Other/disputed territories are omitted from these panels but included in the global total.", sep = "\n")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.title = ggplot2::element_text(size = 19, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11, margin = ggplot2::margin(b = 14)),
      plot.caption = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.2),
      panel.spacing = grid::unit(1.1, "lines"), plot.margin = ggplot2::margin(16, 20, 14, 14))
  if (!global) {
    p <- p +
      ggplot2::facet_wrap(~region, ncol = 3, nrow = 3, scales = "fixed",
        axes = "all_x", axis.labels = "all_x") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_line(colour = "#555555", linewidth = 0.35),
        axis.ticks.length.x = grid::unit(2, "mm"))
  }
  stem <- file.path(output_dir, paste0(if (global) "global" else "regional", "_land_use_emissions_1850_2024"))
  width <- if (global) 11 else 13
  height <- if (global) 6.5 else 10
  ggplot2::ggsave(paste0(stem, ".png"), p, width = width, height = height, dpi = 300, bg = "white")
  ggplot2::ggsave(paste0(stem, ".pdf"), p, width = width, height = height, device = grDevices::cairo_pdf)
  invisible(p)
}

main_regional_luc <- function() {
  data_dir <- here::here("data", "gcb_luc_2025")
  output_dir <- here::here("fig", "gcb_luc_2025")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  path <- download_gcb_luc(data_dir)
  annual <- prepare_gcb_luc(path, data_dir)
  result <- summarise_gcb_luc(annual)
  readr::write_csv(result, file.path(data_dir, "regional_annual_summary.csv"))
  writeLines(c(paste("Source:", gcb_source), paste("Download:", gcb_download),
    paste("SHA256:", gcb_sha256), "License: CC BY 4.0 https://creativecommons.org/licenses/by/4.0/",
    "Citation: Friedlingstein et al. (2026), Global Carbon Budget 2025, https://doi.org/10.5194/essd-18-3211-2026",
    paste("Country mapping generated with countrycode", utils::packageVersion("countrycode")),
    "Changes: country aggregation, Tg C to Gt C conversion, annual model summary (no temporal smoothing)."),
    file.path(data_dir, "sources.txt"))
  plot_gcb_luc(result, output_dir)
  # Use each model's supplied global total, including unallocated territories.
  checks <- readr::read_csv(file.path(data_dir, "global_reconciliation.csv"), show_col_types = FALSE)
  global_annual <- checks |>
    dplyr::transmute(year, model, region = "Global", emissions_GtC_yr = source_global_GtC_yr)
  global_summary <- summarise_gcb_luc(global_annual)
  readr::write_csv(global_annual, file.path(data_dir, "global_annual_by_model.csv"))
  readr::write_csv(global_summary, file.path(data_dir, "global_annual_summary.csv"))
  plot_gcb_luc(global_summary, output_dir, global = TRUE)
  print(result |> dplyr::filter(year == 2024) |> dplyr::select(-year))
  message("Saved figures in ", output_dir)
}
if (sys.nframe() == 0L) main_regional_luc()
