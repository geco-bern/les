# Biogeography figures

## Köppen–Geiger climate zones

Run from the repository root:

```sh
Rscript analysis/biogeography/plot_climate_zones.R
```

Dependencies: `terra`, `ggplot2`, `sf`, `rnaturalearth`, `cowplot`, and `here`.
Use the project's renv environment: run `Rscript -e 'renv::restore()'` from the
repository root when setting up a fresh checkout. Do not pass `--vanilla`, which
would skip `.Rprofile` and bypass renv. The existing lockfile also includes
`tidyterra` and `rnaturalearthdata`, needed by the chapter's vegetation map.

The default `all` mode downloads the published archive if absent, verifies its
checksum, extracts the selected files, and draws the map. Use `plot` as the final
argument to redraw from the extracted local files without network access.

Render the chapter from the repository root so Quarto uses the project profile:

```sh
quarto render book/biogeography.qmd --to html
```

### Source and processing

- **Paper:** Beck et al. (2018), *Present and future Köppen–Geiger climate
  classification maps at 1-km resolution*,
  [doi:10.1038/sdata.2018.214](https://doi.org/10.1038/sdata.2018.214).
- **Data:** [Figshare article 6396959, version 2](https://doi.org/10.6084/m9.figshare.6396959.v2),
  archive `Beck_KG_V1.zip`, file ID `12407516`, licensed CC BY 4.0.
  The pinned archive MD5 is `69594689d3cdd8323a0f74ce658125a1`.
- **Layer:** `Beck_KG_V1_present_0p083.tif`, the global **1980–2016**
  classification, not a future scenario or a confidence map. Its WGS84 grid
  has 4,320 columns and 2,160 rows at 1/12° (0.0833°) resolution.
  The authors aggregated this layer from their 1/120° (approximately 1 km)
  product using majority resampling. We use their supplied overview directly;
  no further spatial aggregation or interpolation is performed.
- **Classes and colours:** codes 1–30 and their RGB colours are parsed directly
  from the archive's `legend.txt`. Code 0 is background and appears white.
  The script checks the global extent, CRS, resolution, and class values.
- **Map style:** consistent with `analysis/plot_luh2_agriculture.R`: longitude
  −180° to 180°, latitude −60° to 85°, WGS84 longitude/latitude coordinates,
  white background, no axes or graticules, Natural Earth 1:110 million country
  outlines in `#7b8279` at linewidth 0.09, and an untitled map with a bottom
  legend. The legend groups the 30 classes by climate family. Antarctica is
  outside the displayed extent, but remains in the downloaded global raster.

### Files

Downloads, the extracted raster and legend, `classes.csv`, and a checksum/source
manifest (`sources.csv`) are cached in `data/beck_2018/` and excluded from Git.
The archive is about 71 MB. Only the selected raster and legend are extracted.

The script writes `koppen_geiger_1980_2016.png` (300 dpi) and
`koppen_geiger_1980_2016.pdf` into `book/images/biogeography/`, at 14 × 7.2 inches.
The PDF embeds the categorical raster with vector outlines and legend text.
The PNG is included by chunk `fig-climate_zones` in `book/biogeography.qmd`.

The existing `@beck18scidat` citation is retained in the book caption, which also
identifies the data period and the authors' aggregation method.
