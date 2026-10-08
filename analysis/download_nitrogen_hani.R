# Download the ten original HaNi archives, Tian et al. (2022).
# Run from the project root: Rscript --vanilla analysis/download_nitrogen_hani.R
# Data: https://doi.org/10.1594/PANGAEA.942069 (CC BY 4.0).
# Archives are retained; processing extracts only one NetCDF file at a time.
# If a download fails, rerun: completed ZIPs are reused and partial files resumed.

download_hani <- function(root = getwd(), components = NULL) {
  raw <- file.path(root, "data", "nitrogen_hani", "raw")
  dir.create(raw, recursive = TRUE, showWarnings = FALSE)
  components_all <- c("nfer_crop_nh4", "nfer_crop_no3", "nfer_pas_nh4",
    "nfer_pas_no3", "nmanure_app_crop", "nmanure_app_pas",
    "nmanure_dep_pas", "nmanure_dep_range", "ndep_nhx", "ndep_noy")
  if (is.null(components)) components <- components_all
  stopifnot(all(components %in% components_all))
  base <- "https://download.pangaea.de/dataset/942069/files/"
  options(timeout = max(3600, getOption("timeout")))
  manifest <- file.path(raw, "PANGAEA.942069.txt")
  if (!file.exists(manifest)) download.file(
    "https://doi.pangaea.de/10.1594/PANGAEA.942069?format=textfile",
    manifest, mode = "wb", method = "libcurl")
  article <- file.path(raw, "essd-14-4551-2022.xml")
  if (!file.exists(article)) download.file(
    "https://essd.copernicus.org/articles/14/4551/2022/essd-14-4551-2022.xml",
    article, mode = "wb", method = "libcurl")
  for (component in components) {
    name <- paste0(component, ".zip")
    target <- file.path(raw, name)
    if (!file.exists(target)) {
      partial <- paste0(target, ".part")
      message("Downloading ", name)
      for (attempt in 1:5) {
        status <- system2("curl", c("--location", "--fail", "--silent",
          "--show-error", "--connect-timeout", "30", "--speed-limit", "1024",
          "--speed-time", "45", "--max-time", "900", "--continue-at", "-", "--output",
          shQuote(partial), shQuote(paste0(base, name))))
        if (status == 0) break
        message("Resuming after interrupted transfer (attempt ", attempt, ").")
      }
      if (status != 0) stop("Download failed: ", name)
      entries <- unzip(partial, list = TRUE)
      if (!paste0(component, ".nc") %in% entries$Name) {
        stop("Unexpected archive contents: ", partial)
      }
      if (!file.rename(partial, target)) stop("Could not save ", target)
    }
    message("Available: ", name)
  }
  paths <- file.path(raw, paste0(components_all, ".zip"))
  present <- file.exists(paths)
  write.csv(data.frame(component = components_all[present],
    url = paste0(base, basename(paths[present])),
    bytes = file.info(paths[present])$size,
    md5 = unname(tools::md5sum(paths[present]))),
    file.path(root, "data", "nitrogen_hani", "sources.csv"), row.names = FALSE)
  invisible(paths[present])
}

if (sys.nframe() == 0L) download_hani()
