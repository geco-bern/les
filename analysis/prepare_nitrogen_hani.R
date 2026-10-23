# HaNi annual global totals and input per grid-cell land area.
# Run after download_nitrogen_hani.R, from the repository root:
#   Rscript --vanilla analysis/prepare_nitrogen_hani.R [component ...]
# With no arguments, processes all ten sources and builds the combined outputs.
# Requires ncdf4, terra, jsonlite and xml2. Extracts one large file at a time, then
# removes only that disposable extraction (original ZIP archives are retained).
# IMPORTANT: the distributed NetCDFs contain ANNUAL MASS (g N/grid cell),
# not the per-cropland/pasture rates discussed in the paper's methods.
# Never multiply these masses by agricultural fractions again.
# E.g. 20 g N/m2 cropland/year on 10% cropland contributes 2 g N/m2 land/year.

hani_components <- c("nfer_crop_nh4", "nfer_crop_no3", "nfer_pas_nh4",
  "nfer_pas_no3", "nmanure_app_crop", "nmanure_app_pas",
  "nmanure_dep_pas", "nmanure_dep_range", "ndep_nhx", "ndep_noy")
hani_years <- 1860:2019
hani_map_years <- c(1959L, 2019L)

hani_grid <- function() terra::rast(nrows = 2160, ncols = 4320,
  xmin = -180, xmax = 180, ymin = -90, ymax = 90, crs = "EPSG:4326")

# Geometric cell area before applying the native HaNi land mask. Fractional
# coastlines within native cells are unresolved by this dataset.
hani_cell_area <- function(r, radius_m = 6371000) {
  lat <- terra::yFromRow(r, 1:terra::nrow(r))
  dy <- terra::res(r)[2] / 2
  band <- radius_m^2 * terra::res(r)[1] * pi / 180 *
    (sin((lat + dy) * pi / 180) - sin((lat - dy) * pi / 180))
  terra::setValues(terra::rast(r), rep(band, each = terra::ncol(r)))
}

prepare_hani_component <- function(component, root = getwd()) {
  stopifnot(component %in% hani_components)
  base <- file.path(root, "data", "nitrogen_hani")
  out <- file.path(base, "derived")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(out, paste0(component, "_annual.csv"))
  maps <- file.path(out, paste0(component, "_", hani_map_years, "_mass_g.tif"))
  meta <- file.path(out, paste0(component, "_metadata.json"))
  if (all(file.exists(c(csv, maps, meta)))) return(invisible(csv))
  archive <- file.path(base, "raw", paste0(component, ".zip"))
  if (!file.exists(archive)) stop("Download first: ", archive)
  entries <- unzip(archive, list = TRUE)
  member <- paste0(component, ".nc")
  if (!member %in% entries$Name) stop("Missing NetCDF: ", member)
  work <- file.path(base, "work")
  dir.create(work, showWarnings = FALSE)
  path <- file.path(work, member)
  message("Extracting and processing ", component)
  # A complete previous extraction can be reused after an interrupted run.
  if (!file.exists(path) || file.info(path)$size != entries$Length[match(member, entries$Name)]) {
    unzip(archive, files = member, exdir = work)
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  v <- nc$var[[component]]
  attrs <- ncdf4::ncatt_get(nc, component)
  stopifnot(v$units == "g N", identical(as.integer(v$size[1:2]), c(4320L, 2160L)))
  stopifnot(max(abs(nc$dim$lon$vals - seq(-180 + 1/24, 180 - 1/24, length.out = 4320))) < 1e-5,
    max(abs(nc$dim$lat$vals - seq(90 - 1/24, -90 + 1/24, length.out = 2160))) < 1e-5)
  time_units <- nc$dim$time$units
  stopifnot(grepl("^years since [0-9]{4}-01-01", time_units))
  years <- as.integer(sub("^years since ([0-9]{4}).*", "\\1", time_units)) + nc$dim$time$vals
  expected <- if (startsWith(component, "nfer_crop")) 1925:2019 else
    if (startsWith(component, "nfer_pas")) 1961:2019 else
      if (startsWith(component, "ndep")) 1850:2020 else 1860:2019
  stopifnot(identical(as.integer(years), expected))
  total <- numeric(length(years))
  for (i in seq_along(years)) {
    if (!years[i] %in% hani_years) next
    x <- ncdf4::ncvar_get(nc, component, start = c(1, 1, i), count = c(-1, -1, 1))
    if (any(x < 0, na.rm = TRUE) || any(is.infinite(x))) stop("Invalid mass in ", component)
    total[i] <- sum(x, na.rm = TRUE) / 1e12  # g N -> Tg N
    if (startsWith(component, "nfer_pas") && i == 1L) {
      # Pasture fertiliser is archived only from 1961; its first field is zero.
      # Extend that zero baseline backwards, explicitly as a reconstruction
      # convention, not as an observation of historical absence of fertiliser.
      stopifnot(total[i] == 0)
      r <- terra::setValues(hani_grid(), as.vector(x))
      names(r) <- paste0(component, "_g_N_per_cell_per_year")
      terra::writeRaster(r, maps[match(1959, hani_map_years)], overwrite = TRUE,
        datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    }
    if (years[i] %in% hani_map_years) {
      r <- terra::setValues(hani_grid(), as.vector(x))
      names(r) <- paste0(component, "_g_N_per_cell_per_year")
      terra::writeRaster(r, maps[match(years[i], hani_map_years)], overwrite = TRUE,
        datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    }
    if (i %% 40 == 0) message(component, ": through ", years[i])
  }
  result <- data.frame(year = years, component = component, Tg_N_yr = total,
    provenance = "sum of archived grid-cell masses")
  result <- result[result$year %in% hani_years, ]
  if (startsWith(component, "nfer_crop")) {
    early <- data.frame(year = 1860:1924, component = component,
      Tg_N_yr = pmax(0, (1860:1924 - 1910) / (1925 - 1910)) * total[1],
      provenance = "paper section 2.3: zero before 1910, linear to archived 1925")
    result <- rbind(early, result)
  }
  if (startsWith(component, "nfer_pas")) {
    early <- data.frame(year = 1860:1960, component = component, Tg_N_yr = 0,
      provenance = "zero baseline extended backwards from archived zero in 1961")
    result <- rbind(early, result)
  }
  write.csv(result, csv, row.names = FALSE)
  jsonlite::write_json(list(component = component, variable_attributes = attrs,
    global_attributes = ncdf4::ncatt_get(nc, 0), time_units = time_units,
    first_year = min(years), last_year = max(years),
    dimensions = v$size, mass_units = "g N per grid cell per year"),
    meta, pretty = TRUE, auto_unbox = TRUE)
  # Close before deleting the disposable extraction; the archive is preserved.
  ncdf4::nc_close(nc)
  on.exit(NULL)
  unlink(path)
  message("Saved ", component, " and removed its temporary NetCDF extraction.")
  invisible(csv)
}

combine_hani <- function(root = getwd()) {
  out <- file.path(root, "data", "nitrogen_hani", "derived")
  series <- do.call(rbind, lapply(hani_components, function(x)
    read.csv(file.path(out, paste0(x, "_annual.csv")))))
  stopifnot(nrow(series) == 1600, !anyNA(series$Tg_N_yr))
  series$category <- ifelse(startsWith(series$component, "nfer"), "Synthetic fertiliser",
    ifelse(startsWith(series$component, "nmanure"), "Livestock manure", "Atmospheric deposition"))
  write.csv(series, file.path(out, "hani_annual_components.csv"), row.names = FALSE)
  totals <- aggregate(Tg_N_yr ~ year, series, sum)
  write.csv(totals, file.path(out, "hani_annual_totals.csv"), row.names = FALSE)
  checks <- list()
  for (year in hani_map_years) {
    layers <- terra::rast(file.path(out, paste0(hani_components, "_", year, "_mass_g.tif")))
    # Missing cells contribute no mass, but cells missing in every source stay NA.
    mass <- sum(layers, na.rm = TRUE)
    mask <- sum(!is.na(layers))
    mass <- terra::ifel(mask > 0, mass, NA)
    # The archive supplies a binary land footprint, not fractional coastlines.
    # Count complete native cells with at least one finite input as represented
    # land; exclude ocean/no-data cells when aggregating the display denominator.
    area <- terra::ifel(mask > 0, hani_cell_area(mass), NA)
    names(area) <- "represented_land_area_m2"
    terra::writeRaster(area, file.path(out, paste0("hani_land_area_", year, "_5arcmin.tif")),
      overwrite = TRUE, datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    rate <- mass / area
    names(rate) <- "g_N_m2_land_yr"
    terra::writeRaster(rate, file.path(out, paste0("hani_total_", year, "_5arcmin.tif")),
      overwrite = TRUE, datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    # Sum both N mass and represented land area when coarsening. This avoids
    # diluting coastal inputs with the area of ocean/no-data subcells.
    coarse_mass <- terra::aggregate(mass, fact = 3, fun = "sum", na.rm = TRUE)
    coarse_area <- terra::aggregate(area, fact = 3, fun = "sum", na.rm = TRUE)
    terra::writeRaster(coarse_area, file.path(out, paste0("hani_land_area_", year, "_025deg.tif")),
      overwrite = TRUE, datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    coarse_rate <- coarse_mass / coarse_area
    names(coarse_rate) <- "g_N_m2_land_yr"
    terra::writeRaster(coarse_rate, file.path(out, paste0("hani_total_", year, "_025deg.tif")),
      overwrite = TRUE, datatype = "FLT8S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    full_total <- terra::global(rate * area, "sum", na.rm = TRUE)[1, 1] / 1e12
    coarse_total <- terra::global(coarse_rate * coarse_area, "sum", na.rm = TRUE)[1, 1] / 1e12
    annual_total <- totals$Tg_N_yr[totals$year == year]
    stopifnot(abs(full_total / annual_total - 1) < 1e-8,
      abs(coarse_total / annual_total - 1) < 1e-8)
    checks[[as.character(year)]] <- data.frame(year, annual_Tg_N = annual_total,
      map_5arcmin_Tg_N = full_total, map_025deg_Tg_N = coarse_total)
  }
  write.csv(do.call(rbind, checks), file.path(out, "mass_conservation_checks.csv"), row.names = FALSE)
  decadal <- aggregate(Tg_N_yr ~ decade + category,
    transform(series, decade = year %/% 10 * 10), function(x) sum(x)/10)
  write.csv(decadal, file.path(out, "decadal_inputs.csv"), row.names = FALSE)
  # Independent check against every component in the article's Table 2.
  article_text <- paste(readLines(file.path(root, "data", "nitrogen_hani", "raw",
    "essd-14-4551-2022.xml"), warn = FALSE), collapse = "\n")
  # Publisher-only xmltex processing instructions use a reserved XML prefix.
  # Remove those layout instructions; all table elements and values are kept.
  article_text <- gsub("<\\?xmltex[\\s\\S]*?\\?>", "", article_text, perl = TRUE)
  article <- xml2::read_xml(article_text)
  table <- xml2::xml_find_first(article, ".//table-wrap[@id='Ch1.T2']")
  rows <- xml2::xml_find_all(table, ".//*[local-name()='tbody']/*[local-name()='row']")
  published <- do.call(rbind, lapply(rows, function(row) {
    cells <- xml2::xml_text(xml2::xml_find_all(row, "./*[local-name()='entry']"))
    data.frame(decade = as.integer(sub("s$", "", cells[1])),
      component = c(hani_components, "total"), published_Tg_N_yr = as.numeric(cells[-1]))
  }))
  reconstructed <- aggregate(Tg_N_yr ~ decade + component,
    transform(series, decade = year %/% 10 * 10), mean)
  decade_totals <- aggregate(Tg_N_yr ~ decade, transform(totals, decade = year %/% 10 * 10), mean)
  decade_totals$component <- "total"
  reconstructed <- rbind(reconstructed, decade_totals[names(reconstructed)])
  comparison <- merge(published, reconstructed, by = c("decade", "component"))
  comparison$difference_Tg_N_yr <- comparison$Tg_N_yr - comparison$published_Tg_N_yr
  write.csv(comparison, file.path(out, "published_table2_checks.csv"), row.names = FALSE)
  # Published values are rounded to 0.01 Tg. Small differences are reported,
  # not silently rescaled away; a >1% discrepancy warrants investigation.
  if (any(abs(comparison$difference_Tg_N_yr) > pmax(.015, .01 * comparison$published_Tg_N_yr))) {
    warning("Some Table 2 checks differ by >1% (or >0.015 Tg); inspect published_table2_checks.csv.")
  }
  capture.output(sessionInfo(), file = file.path(out, "session_info.txt"))
  invisible(series)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "combine")) {
    combine_hani()
  } else {
    components <- if (length(args)) args else hani_components
    for (component in components) prepare_hani_component(component)
    if (!length(args)) combine_hani()
  }
}
