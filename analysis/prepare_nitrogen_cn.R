# Build the nitrogen chapter's C:N observations from the original data.
# Run from the repository root: Rscript --vanilla analysis/prepare_nitrogen_cn.R
# See data/nitrogen_cn/README.md for downloads, definitions, and limitations.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
})

raw <- "data/nitrogen_cn/raw"
out <- "data/nitrogen_cn/derived"

dir.create(out, recursive = TRUE, showWarnings = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
positive <- function(x) is.finite(x) & x > 0
read_source <- function(name) {
  read_csv(file.path(raw, name), col_types = cols(.default = col_character()),
           progress = FALSE, show_col_types = FALSE)
}
valid_ratio <- function(c, n) ifelse(positive(c) & positive(n), c / n, NA_real_)

# Preserve the observation identifiers and source units in the derived data.
# n_source_records counts records represented by each plotted observation.
leaves <- read_source("Leaf_Carbon_Nutrients_data.csv")
leaf_data <- bind_rows(lapply(c("green", "senesced"), function(stage) {
  leaves |>
    transmute(compartment = if (stage == "green") "Green leaves" else "Senesced leaves",
              source = "Vergutz et al. (2012)",
              source_id = Record_number, study = Ref, species = Species,
              site = paste(Country, Lat, Long, Site_name, sep = ";"),
              latitude = num(Lat), longitude = num(Long),
              group = Growth_habit,
              carbon = num(.data[[paste0("C_", stage, "_leaf")]]),
              nitrogen = num(.data[[paste0("N_", stage, "_leaf")]]),
              source_units = "% dry mass", cn_mass = valid_ratio(carbon, nitrogen),
              derivation = "paired measured C/N", n_source_records = 1L)
})) |> filter(positive(cn_mass))
stopifnot(!anyDuplicated(leaf_data[c("compartment", "source_id")]))

# FRED4 has six descriptive rows beneath its header. Requiring a numeric row ID
# removes them without depending on their positions. Only unambiguously living,
# fine (FR) or coarse (CR) roots enter the comparison. Mixed classes are excluded.
fred <- read_source("FRED4_Entire_Database_2026.csv") |>
  filter(!is.na(num(F00002))) |>
  mutate(reported_cn = num(F00413), c = num(F00253), n = num(F00261),
         cn_mass = coalesce(ifelse(positive(reported_cn), reported_cn, NA_real_),
                            valid_ratio(c, n)),
         treatment_type = tolower(coalesce(F01159, "")),
         treatment = tolower(coalesce(F01160, "")))
# Accept observational gradients and controls. Non-control experimental and
# managed treatments are excluded when identified by the source. Blank fields
# mean unreported treatment status, not proof that a site is undisturbed.
roots <- fred |>
  filter(is.na(F00010) | tolower(F00010) != "x",
         F00055 %in% c("FR", "CR"), F00064 == "living",
         treatment_type %in% c("", "control", "gradient"),
         treatment_type %in% c("control", "gradient") |
           treatment %in% c("", "control", "ambient", "ambient co2", "none"),
         positive(cn_mass)) |>
  transmute(compartment = ifelse(F00055 == "FR", "Fine roots", "Coarse roots"),
            source = "FRED 4 (2026)", source_id = F00002, study = F00003,
            species = coalesce(F01287, F00019), site = F00008,
            latitude = num(F01185), longitude = num(F01186), group = F00032,
            carbon = c, nitrogen = n, source_units = "mg/g dry root",
            cn_mass, derivation = ifelse(positive(reported_cn),
                                        "reported mass C:N", "paired measured C/N"),
            n_source_records = 1L)
stopifnot(!anyDuplicated(roots$source_id))

# Initial samples only: pooling harvests would mix living-tissue chemistry with
# changing stoichiometry during decay. Match C and N on the original sample ID.
wood_long <- read_source("wood_wijas2024.csv") |>
  filter(num(time) == 0, trait %in% c("C", "N"))
stopifnot(!anyDuplicated(wood_long[c("unique", "time", "trait")]))
wood <- wood_long |>
  select(unique, codeStem, hostSpecies, size, time, trait, trait.val) |>
  pivot_wider(names_from = trait, values_from = trait.val) |>
  transmute(compartment = "Wood (initial samples)", source = "Wijas et al. (2025)",
            source_id = unique, study = "Wijas et al. (2025)", species = hostSpecies,
            site = "One temperate forest near Sydney, Australia", group = size,
            carbon = num(C), nitrogen = num(N), source_units = "% dry mass",
            cn_mass = valid_ratio(carbon, nitrogen),
            derivation = "paired measured C/N; time = 0 months", n_source_records = 1L) |>
  filter(positive(cn_mass))

# Soil organic C and total N are both g/kg. Join by profile AND horizon, not by
# coordinates alone. value_avg is WoSIS's mean when a property has repeat assays.
# Include complete horizons within 0--30 cm, including flagged organic horizons.
# These are bulk-soil OC/TN ratios, not measurements on an isolated SOM fraction.
soil_c <- read_tsv(file.path(raw, "wosis/wosis_202312_orgc.tsv"),
                   show_col_types = FALSE, progress = FALSE)
soil_n <- read_tsv(file.path(raw, "wosis/wosis_202312_nitkjd.tsv"),
                   show_col_types = FALSE, progress = FALSE)
stopifnot(!anyDuplicated(soil_c[c("profile_id", "layer_id")]),
          !anyDuplicated(soil_n[c("profile_id", "layer_id")]))
soil <- soil_c |>
  inner_join(select(soil_n, profile_id, layer_id, nitrogen = value_avg,
                    n_upper = upper_depth, n_lower = lower_depth,
                    nitrogen_licence = licence),
             by = c("profile_id", "layer_id"), relationship = "one-to-one") |>
  filter(upper_depth == n_upper, lower_depth == n_lower,
         upper_depth >= 0, lower_depth <= 30, lower_depth > upper_depth) |>
  transmute(compartment = "Bulk soil organic matter", source = "WoSIS 2023",
            source_id = as.character(layer_id), study = dataset_id,
            site = as.character(profile_id), latitude, longitude,
            group = ifelse(organic_surface, "organic surface horizon", "other horizon"),
            carbon = value_avg, nitrogen, source_units = "g/kg fine earth",
            cn_mass = valid_ratio(carbon, nitrogen),
            derivation = "matched-horizon organic C / total N",
            upper_depth_cm = upper_depth, lower_depth_cm = lower_depth,
            country = country_name, carbon_licence = licence, nitrogen_licence,
            n_source_records = 1L) |> filter(positive(cn_mass))

# The spatial CSV includes a units row: concentrations are mmol/kg, NOT mg/kg.
# Do not append the vertical CSV, which repeats a subset of the spatial records.
microbes_raw <- read_source("Soil_Microbial_Biomass_C_N_P_spatial.csv")
stopifnot(grepl("mmol", microbes_raw$Soil_microbial_biomass_carbon[1], ignore.case = TRUE),
          grepl("mmol", microbes_raw$Soil_microbial_biomass_nitrogen[1], ignore.case = TRUE))
microbes <- microbes_raw |>
  mutate(record_id = row_number() - 1L) |>
  transmute(compartment = "Microbial biomass", source = "Xu, Thornton & Post (2014)",
            source_id = as.character(record_id), study = Reference_number,
            site = paste(Country, Latitude, Longitude, Vegetation, sep = ";"),
            latitude = num(Latitude), longitude = num(Longitude), group = Biome,
            carbon = num(Soil_microbial_biomass_carbon),
            nitrogen = num(Soil_microbial_biomass_nitrogen), source_units = "mmol/kg soil",
            cn_mass = valid_ratio(carbon, nitrogen) * 12 / 14,
            derivation = "molar C/N multiplied by 12/14",
            upper_depth_cm = num(Upper_depth), lower_depth_cm = num(Lower_depth),
            method = Method, n_source_records = 1L) |> filter(positive(cn_mass))

# Reproduce the paper's species-level summaries (252 species). Appendix S2 CN
# is molar. Some source ratios use an assumed 44% carbon: retain and disclose
# those values; absence of a flag means they cannot be reliably removed by
# checking for missing C (imputed 44s are already stored in the C column).
fungal_raw <- read_excel(file.path(raw, "Data_Sheet_2.XLSX"), sheet = "fundata")
fungi <- fungal_raw |>
  filter(!is.na(Species), positive(CN)) |>
  group_by(Species) |>
  summarise(cn_mass = mean(CN) * 12 / 14, n_source_records = n(),
            study = paste(sort(unique(Reference)), collapse = "; "),
            .groups = "drop") |>
  transmute(compartment = "Fungi", source = "Zhang & Elser (2017)",
            source_id = Species, species = Species, study,
            cn_mass, source_units = "molar C:N",
            derivation = "species mean of reported molar CN * 12/14; partly inferred C",
            n_source_records)
stopifnot(nrow(fungi) == 252L,
          abs(median(fungi$cn_mass) - 13.65 * 12 / 14) < .01)

# LIDET wet-chemistry table: A/B and other types are not assumed to mean leaves.
# Type codes are interpreted from the accompanying EML metadata. This block is
# completed by the explicit material selection below, not by inferred C content.
lidet_file <- file.path(raw, "TD02303.csv")
if (!file.exists(lidet_file)) stop("LIDET TD02303.csv is required; see data/nitrogen_cn/README.md")
lidet_raw <- read_source("TD02303.csv")
lidet <- lidet_raw |>
  filter(TYPE1 %in% c("L", "R")) |>
  mutate(c = num(CARBON), n = num(NITROGEN), cn = valid_ratio(c, n)) |>
  filter(positive(cn)) |>
  group_by(TYPE1, NIR_NUM, SPECIES, SITE, DURATION, REP) |>
  summarise(cn_mass = mean(cn), carbon = mean(c), nitrogen = mean(n),
            n_source_records = n(), .groups = "drop") |>
  transmute(compartment = ifelse(TYPE1 == "L", "Leaf litter (LIDET)", "Root litter (LIDET)"),
            source = "LIDET (Harmon 2016)", source_id = paste(TYPE1, NIR_NUM, sep = ":"),
            study = "LIDET", species = SPECIES, site = SITE,
            group = TYPE1, carbon, nitrogen, source_units = "% dry mass",
            cn_mass, derivation = "mean of paired wet-chemistry C/N within sample",
            decomposition_years = num(DURATION), n_source_records)

observations <- bind_rows(leaf_data, roots, wood, lidet, soil, microbes, fungi)
stopifnot(all(positive(observations$cn_mass)),
          !anyDuplicated(observations[c("compartment", "source_id")]))
write_csv(observations, file.path(out, "cn_observations.csv.gz"), na = "")
coverage <- observations |>
  group_by(compartment, source) |>
  summarise(n = n(), source_records = sum(n_source_records),
            studies = n_distinct(study, na.rm = TRUE),
            species = n_distinct(species, na.rm = TRUE),
            sites = n_distinct(site, na.rm = TRUE),
            .groups = "drop")
write_csv(coverage, file.path(out, "coverage.csv"))
print(coverage, width = Inf)
