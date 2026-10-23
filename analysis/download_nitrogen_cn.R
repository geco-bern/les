# Retrieve the exact inputs used by prepare_nitrogen_cn.R.
# Run from the repository root. Requires the R package digest.
#
#   Rscript --vanilla analysis/download_nitrogen_cn.R
#   Rscript --vanilla analysis/download_nitrogen_cn.R --manual-dir=~/Downloads
#   Rscript --vanilla analysis/download_nitrogen_cn.R --check-only
#
# Public files are downloaded automatically; existing files are verified and
# reused. ZIP archives are cached and only the required members are extracted.
# Every input must match the SHA-256 recorded in data/nitrogen_cn/sources.csv.
# Changed or incomplete files are reported, never silently overwritten.
#
# MANUAL DOWNLOADS (access restrictions encountered when assembling the data):
# 1. Vergutz leaves: https://doi.org/10.3334/ORNLDAAC/1106
#    Sign in to NASA Earthdata and download Leaf_Carbon_Nutrients_data.csv.
# 2. Xu microbial biomass: https://doi.org/10.3334/ORNLDAAC/1264
#    Sign in to NASA Earthdata and download
#    Soil_Microbial_Biomass_C_N_P_spatial.csv (not the vertical file).
# 3. LIDET: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-and.4041.11
#    In a signed-in browser, download the wet-chemistry table TD02303.csv from
#    version 11. Complete any archive verification in the browser.
# 4. Wood: https://doi.org/10.5061/dryad.w9ghx3g0g
#    Download traitdat_final.csv from the 6 December 2024 version. These data
#    are public (CC0), but Dryad rejected scripted downloads during collection.
#
# Save these four files in one folder and pass --manual-dir=/path/to/folder.
# The wood file is copied to wood_wijas2024.csv automatically. Alternatively,
# place them directly in data/nitrogen_cn/raw/ using the manifest's filenames.
# Keep original downloads unchanged: opening/re-saving a CSV can change its
# checksum. No passwords, browser cookies, or Earthdata tokens are needed by
# this script. Missing manual files do not prevent downloading public inputs;
# the script exits unsuccessfully after listing everything still needed.
#
# Optional: --dest=/path/to/raw changes the destination (e.g. for a fresh cache).
# --only=FRED4_dd.csv selects one manifest local_file for retrieval/checking.

args <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% args) {
  cat(paste(
    "Usage: Rscript --vanilla analysis/download_nitrogen_cn.R [options]",
    "  --manual-dir=PATH   Import manually downloaded files from this folder",
    "  --dest=PATH         Destination; default: data/nitrogen_cn/raw",
    "  --only=LOCAL_FILE   Select one local_file from sources.csv",
    "  --check-only        Verify inputs without downloads or filesystem changes",
    "  --help              Show this help; see script comments for manual steps",
    sep = "\n"), "\n")
  quit(status = 0)
}
known <- args == "--check-only" |
  grepl("^--(manual-dir|dest|only)=.+$", args)
if (any(!known)) stop("Unknown or empty option: ", paste(args[!known], collapse = ", "),
                      call. = FALSE)
option_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  values <- substring(args[startsWith(args, prefix)], nchar(prefix) + 1L)
  if (length(values) > 1L) stop("Repeated option: --", name, call. = FALSE)
  if (length(values)) values else default
}
raw_dir <- path.expand(option_value("dest", "data/nitrogen_cn/raw"))
manual_dir <- option_value("manual-dir")
if (!is.null(manual_dir)) manual_dir <- path.expand(manual_dir)
check_only <- "--check-only" %in% args
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Install the checksum dependency with install.packages('digest').", call. = FALSE)
}
manifest_path <- "data/nitrogen_cn/sources.csv"
if (!file.exists(manifest_path)) stop("Run this script from the repository root.", call. = FALSE)
manifest <- read.csv(manifest_path, colClasses = "character", check.names = FALSE)
only <- option_value("only")
if (!is.null(only)) {
  if (!only %in% manifest$local_file) stop("No manifest input named: ", only, call. = FALSE)
  manifest <- manifest[manifest$local_file == only, , drop = FALSE]
}
options(timeout = max(1800, getOption("timeout", 60)))

verify <- function(path, row) {
  actual <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  if (!identical(actual, row$sha256)) {
    stop("SHA-256 mismatch for ", row$local_file,
         ". Expected ", row$sha256, "; found ", actual,
         ". Retain the file for inspection; obtain the recorded version before retrying.",
         call. = FALSE)
  }
}

# Publish only a complete, verified input. Temporary files live on the same
# filesystem as the destination; interruption cannot leave a partial CSV there.
install_input <- function(candidate, target, row) {
  verify(candidate, row)
  if (file.exists(target)) stop("Destination appeared during retrieval: ", target,
                                call. = FALSE)
  if (!file.rename(candidate, target)) stop("Cannot install ", target, call. = FALSE)
}

download <- function(url, target, label) {
  message("Downloading ", label, " ...")
  status <- utils::download.file(url, target, method = "libcurl", mode = "wb", quiet = TRUE)
  if (status != 0L || !file.exists(target) || file.info(target)$size == 0) {
    stop("Download failed: ", url, call. = FALSE)
  }
}

manual_files <- c("Leaf_Carbon_Nutrients_data.csv", "TD02303.csv",
                  "Soil_Microbial_Biomass_C_N_P_spatial.csv", "wood_wijas2024.csv")
manual_help <- function(row) {
  original <- if (row$local_file == "wood_wijas2024.csv") "traitdat_final.csv" else row$local_file
  paste0("Manual download needed: ", original, " from https://doi.org/", row$doi,
         ". Save it in ", raw_dir, " as ", row$local_file,
         " or rerun with --manual-dir=/path/to/downloads. See script comments for sign-in steps.")
}

retrieve <- function(row) {
  target <- file.path(raw_dir, row$local_file)
  if (file.exists(target)) {
    verify(target, row)
    message("Verified cached input: ", row$local_file)
    return(invisible(NULL))
  }
  if (check_only) stop("Missing input: ", row$local_file, call. = FALSE)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  candidate <- tempfile(pattern = ".cn-input-", tmpdir = dirname(target))
  on.exit(unlink(candidate), add = TRUE)

  if (row$local_file %in% manual_files) {
    if (is.null(manual_dir)) stop(manual_help(row), call. = FALSE)
    names <- row$local_file
    if (row$local_file == "wood_wijas2024.csv") names <- c(names, "traitdat_final.csv")
    paths <- file.path(manual_dir, names)
    paths <- paths[file.exists(paths)]
    if (!length(paths)) stop(manual_help(row), call. = FALSE)
    if (!file.copy(paths[1], candidate)) stop("Cannot copy ", paths[1], call. = FALSE)
    install_input(candidate, target, row)
    message("Imported and verified: ", row$local_file)
    return(invisible(NULL))
  }

  # The two soil rows share one 435 MiB download. Reuse the local ZIP for the
  # second row and on subsequent runs; member checksums pin its actual content.
  archive_name <- if (startsWith(row$local_file, "wosis/")) {
    "WoSIS_2023_December.zip"
  } else if (row$local_file == "Data_Sheet_2.XLSX") {
    "fungi_supplement.zip"
  } else NULL

  if (!is.null(archive_name)) {
    archive <- file.path(raw_dir, archive_name)
    if (!file.exists(archive)) {
      partial <- tempfile(pattern = ".cn-archive-", tmpdir = raw_dir, fileext = ".zip")
      on.exit(unlink(partial), add = TRUE)
      download(row$download_url, partial, archive_name)
      listing <- utils::unzip(partial, list = TRUE)
      if (!nrow(listing)) stop("Empty or invalid ZIP from ", row$download_url, call. = FALSE)
      if (file.exists(archive) || !file.rename(partial, archive)) {
        stop("Cannot install archive: ", archive, call. = FALSE)
      }
    }
    listing <- utils::unzip(archive, list = TRUE)
    member <- listing$Name[basename(listing$Name) == row$archive_member_or_original_name]
    if (length(member) != 1L) {
      stop("Expected one ", row$archive_member_or_original_name, " in ", archive,
           ". Inspect the cached archive before retrying.", call. = FALSE)
    }
    extraction <- tempfile(pattern = ".cn-extract-", tmpdir = dirname(target))
    dir.create(extraction)
    on.exit(unlink(extraction, recursive = TRUE), add = TRUE)
    utils::unzip(archive, files = member, exdir = extraction, junkpaths = TRUE)
    extracted <- file.path(extraction, basename(member))
    install_input(extracted, target, row)
  } else {
    download(row$download_url, candidate, row$local_file)
    install_input(candidate, target, row)
  }
  message("Retrieved and verified: ", row$local_file)
}

# Continue independent downloads when a source is unavailable, but signal an
# incomplete cache with a non-zero exit status so a pipeline cannot proceed.
problems <- character()
for (i in seq_len(nrow(manifest))) {
  tryCatch(retrieve(manifest[i, , drop = FALSE]), error = function(e) {
    problem <- conditionMessage(e)
    message(problem)
    problems <<- c(problems, problem)
  })
}
if (length(problems)) {
  stop(length(problems), " input(s) remain unavailable or unverified; see messages above. ",
       "Existing inputs were preserved. Rerun after resolving these files.", call. = FALSE)
}
message("All ", nrow(manifest), " requested inputs verified in ", raw_dir, ".")
