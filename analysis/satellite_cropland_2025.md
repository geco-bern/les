# Satellite-derived cropland maps for 2025

Run from the repository root:

```sh
Rscript analysis/plot_satellite_cropland_2025.R
```

Optional steps: append `regions`, `global`, or `plot`. Downloads are cached, so
rerunning resumes completed tiles. Global tiles run in three fresh R processes
using a PSOCK cluster, compatible with Positron and Windows. For serial execution,
source the script and call `make_crop_global(data_dir, workers = 1L)`. Packages come from the project's R library;
the script does not install packages or change the HYDE workflow.

## Product choice

**Sentinel-2 10m Land Use/Land Cover Time Series**, produced by Impact Observatory,
Microsoft and Esri, has an annual **2025** classification derived from Sentinel-2
imagery. It provides global coverage and a `Crops` class (pixel value **5**).
The public service catalog is queried for `Year=2025`; the service metadata and
catalog response are saved with the outputs. The imagery service's 2025 raster
is `Sentinel_2_10m_LandCover_2025` (catalog object 9 at retrieval).

- [2025 release announcement](https://www.esri.com/about/newsroom/arcnews/latest-land-cover-data-release-shows-more-change-over-time)
- [Dataset viewer and downloads](https://livingatlas.arcgis.com/landcoverexplorer/)
- [Image service](https://ic.imagery1.arcgis.com/arcgis/rest/services/Sentinel2_10m_LandCover/ImageServer)
- [Class definitions and license](https://docs.impactobservatory.com/lulc-maps/maps-for-good.html)

The crop class represents planted low-growing crops and structured fallow plots.
Tree plantations can be classified as trees, so this is **not an inventory of all
agricultural land or a direct equivalent of HYDE cropland**. Classification errors
and seasonal compositing also affect the result. It is a raster classification,
not a cadastral field-boundary dataset. The annual maps are licensed CC BY 4.0;
credit Impact Observatory, Microsoft and Esri.

Two crop-specific alternatives were checked on 24 September 2026:

- [GLAD annual cropland](https://glad.umd.edu/dataset/annual-croplands): Landsat,
  30 m, currently 2015–2024. Suitable for a 2024 cropland-specific comparison,
  but not substituted for the requested 2025 map.
- [ESA WorldCereal](https://esa-worldcereal.org/en/products/global-maps): 10 m
  temporary-crop maps; the published global collection currently covers 2021.

## Global map: sampled overview, approved for this task

The service is requested at **0.01°** using nearest-neighbour resampling and its
unrendered categorical values. At this scale the service can use categorical
overviews; these are **not guaranteed to be direct samples of native 10 m cells**.
Each group of 10 × 10 samples is aggregated locally to a **0.1°** cell:

`fraction = count(class == 5) / count(valid classified samples)`

Classes 1, 2, 4, 5, 7, 8, 9 and 11 count as valid. Cloud (10) and missing data (0)
are excluded, rather than treated as non-cropland. Water counts as valid
non-cropland. A second band records the valid sample count (0–100). Cells with
no valid observations have a missing fraction. The GeoTIFF spans the full globe
with a grid anchored at −180° / 90°; outside the requested Sentinel-2 coverage
(60°S–84°N), both bands are missing. The plot crops the empty polar margins.

This is a **visual overview of sampled crop prevalence**, not an exact
area-weighted aggregation of every 10 m pixel. It should not be used for precise
global cropland totals, uncertainty-free comparisons to HYDE, or small-change
estimates. In a development check, a server-side average of a binary crop mask
did not match a direct 10 m pixel count, so that shortcut was rejected. The user
explicitly chose the sampled global overview instead of full global aggregation.

## Regional maps: 10 m

Four contrasting cropland landscapes are downloaded as 20 × 20 km windows, each
with 2,000 × 2,000 pixels in its local WGS84 UTM projection. Nearest-neighbour
resampling preserves class codes when reprojecting the service's nominal 10 m
Web Mercator source. The exports are 10 m in local UTM; they are not claimed to
be the original Sentinel-2 sensor grid. No spatial aggregation is applied.

| Panel | Region | Centre (longitude, latitude) | EPSG |
| --- | --- | --- | --- |
| A | Iowa, USA | −93.70, 42.20 | 32615 |
| B | Mato Grosso, Brazil | −55.90, −12.60 | 32721 |
| C | Punjab, India | 75.20, 30.80 | 32643 |
| D | Beauce, France | 1.50, 48.10 | 32631 |

The layout follows the global-context-plus-detailed-regional-panels idea in
[Hansen et al. (2013)](https://doi.org/10.1126/science.1244693), using agricultural
regions rather than reproducing that paper's forest-change sites. The maps show
cropland extent, not crop change. All regional pixels are passed to the plotting
device, with individual PNGs large enough to display the 2,000-pixel-wide data.
Scale bars are 5 km. Natural Earth boundaries provide global context.

## Files

Under `data/satellite_cropland_2025/`:

- `cropland_2025_sampled_0.1deg.tif`: global fraction and valid-sample-count bands.
- `<region>_landcover_2025_10m.tif`: original exported land-cover classes.
- `<region>_crops_2025_10m.tif`: binary crops (1), observed non-crops (0), missing (255).
- `regions.csv`, `source_metadata.json`, and per-download request/response JSON.
- `global_tiles/`: cached overview samples and 0.1° aggregates for reproducibility.

Under `fig/satellite_cropland_2025/`, in PNG and PDF:

- `cropland_2025_global_and_regions`: combined global map and four regional panels.
- `cropland_2025_global`: global overview.
- `cropland_2025_<region>`: each full-resolution regional map.

The earlier cropland-change experiment is preserved in `analysis/archive/`, with its existing data and figures. The three active workflows are described in [land_use_workflows.md](land_use_workflows.md).
