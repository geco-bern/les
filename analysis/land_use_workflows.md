# Three land-use and forest-change workflows

Run these independently from the repository root:

| Subject | Script | Figures |
| --- | --- | --- |
| Past land use: HYDE 3.5, 0 / 1850 / 2025 CE | `Rscript analysis/plot_hyde_land_use.R` | `fig/hyde/` |
| Present-day cropland: Sentinel-2, 2025 | `Rscript analysis/plot_satellite_cropland_2025.R` | `fig/satellite_cropland_2025/` |
| Forest loss and gain: Hansen GFC v1.13 | `Rscript analysis/plot_hansen_forest_change.R` | `fig/hansen_forest_change_2025/` |

The four active workflows use tidyverse conventions: tibbles and dplyr/tidyr for
tables, purrr for iteration, readr for CSV files, and ggplot2 for every figure.
Package namespaces are explicit; attaching the entire tidyverse is unnecessary.
Terra retains raster processing, and PSOCK clusters retain compatibility with
Positron. The gtable package arranges ggplot panels. Spatial grids, classifications,
aggregation rules, time periods, and unsmoothed annual series are preserved.

HYDE is conservatively remapped from 5 arc minutes to 0.1°, matching the grid
of the sampled satellite global overview. Both use the same brown 0–100% scale.
The satellite regional cropland maps use 10 m exports. See
[the satellite methods](satellite_cropland_2025.md) for its sampling limitations.
The former Sentinel-2 cropland-change experiment and its documentation are
preserved under `analysis/archive/`; its data and figures remain available.

A separate carbon-emissions analysis complements these three spatial workflows:
`Rscript analysis/plot_regional_land_use_emissions.R` downloads GCB 2025 national
estimates and plots regional histories for 1850–2024. See
[regional_land_use_emissions.md](regional_land_use_emissions.md) for aggregation,
model spread and averaging definitions. Figures are in `fig/gcb_luc_2025/`.

## Hansen forest maps

Source: [Hansen Global Forest Change v1.13](https://developers.google.com/earth-engine/datasets/catalog/UMD_hansen_global_forest_change_2025_v1_13),
with [public GeoTIFF downloads and release notes](https://storage.googleapis.com/earthenginepartners-hansen/GFC-2025-v1.13/download.html).
No Earth Engine account is required. The R script uses terra/GDAL HTTP range
reads to download regional windows, caches them, and can redraw without network
access using `Rscript analysis/plot_hansen_forest_change.R plot`.

**Loss covers 2001–2025; gain covers only 2000–2012.** The gain band was never
updated for later releases. The two periods are labelled separately throughout;
subtracting these layers would not estimate net change over 2000–2025.

The four windows reconstruct the **displayed footprints in Figure 2** of
[Hansen et al. (2013)](https://doi.org/10.1126/science.1244693), replacing the
previous 40 km squares. Their boundaries were fitted to landscape features in
the published figure. The paper does not supply exact bounding boxes: median
registration discrepancies are about 150–220 m, so these are publication-matched
reconstructions rather than exact author-specified coordinates. See the
[registration method and bounds](hansen_support/README.md). The displayed
Indonesia geography is centred near 0.45°N, despite the caption's 0.4°S.

Source GeoTIFFs retain the native WGS84 0.00025° grid (about 28 m north–south;
east–west spacing varies with latitude), usually described as the Landsat-based
30 m product. Native cells are used for all area statistics. PNG/PDF maps use
2400-column Mercator overviews sampled by nearest neighbour; they are not
full-resolution pixel exports. Use the cached GeoTIFFs for native detail.
Scale bars are approximate distances at each region's middle latitude.

The combined figure has a 2 × 2 layout with one panel per region: orange for loss
only, blue for gain only, and purple for both gain and loss. All panels use 2000 canopy cover as a continuous light-grey-to-green background,
with water light blue and missing data white. That background is a baseline,
not forest cover remaining in 2025. Individual region figures are also supplied.

Loss is any nonzero `lossyear` value (1–25); gain is `gain == 1`. Both are masked
to `datamask == 1` (mapped land). No arbitrary canopy threshold is imposed on
these supplied change classifications. The source `treecover2000` measures
canopy closure of vegetation taller than 5 m. Gross tree-cover loss includes
harvest, fire and other disturbances, and is not necessarily permanent
deforestation. Gain includes regeneration and plantations. A cell can have both
loss and gain and is displayed in purple. The overlap combines the two different
observation periods; it does not imply an event order or estimate net change. Methods changes
within the loss time series also limit year-to-year comparisons; consult the
release notes. These mapped areas are not error-adjusted estimates.

`data/hansen_forest_change_2025/figure2_native/` contains the four source layers for each region,
source-URL sidecars, and `regional_summary.csv`. Areas are summed using geodesic
cell areas in km²; fractions use mapped land area as the denominator. The summary
also reports overlap, grid dimensions and resolution. `lossyear` retains event
years for further analysis. Source value ranges and alignment are checked before
plotting. Figures are saved as PNG and PDF, with the prefix
`hansen_forest_loss_gain_` and suffix `regions` or the regional slug.

Credit: Hansen/UMD/Google/USGS/NASA, GFC v1.13, CC BY 4.0; Hansen et al. (2013),
Science 342, 850–853, DOI: 10.1126/science.1244693.

## Global irrigation, crop types and grazing management

`Rscript analysis/plot_luh2_agriculture.R` adds three consistent LUH2 historical
2015 maps using the satellite overview's visual style. The native grid is 0.25°.
Crop types are functional groups; managed pasture and rangeland are shown as
separate supplied grid-cell fractions, not measured grazing intensity. See [methods](luh2_agriculture.md).
Figures are saved in `fig/luh2_agriculture_2015/`.
