# Global agricultural land use, LUH2 historical 2015

Run from the repository root:

```sh
Rscript analysis/plot_luh2_agriculture.R
# Redraw entirely from the compact extracted cache:
Rscript analysis/plot_luh2_agriculture.R plot
```

This companion to `plot_satellite_cropland_2025.R` uses the same map extent
(180°W–180°E, 60°S–85°N), grey land background, country outlines and brown
0–100% palette for irrigation area. Crop groups use discrete colors; both grazing
land variables use the same continuous brown scale.
The native **0.25°** LUH2 grid is retained; no finer resolution is implied.
All three maps use **2015**, the last year in LUH2 v2h's historical reconstruction,
rather than a future scenario for 2025.

## Files

Source files are cached in `data/luh2_agriculture_2015/`: `states.nc` (about 5.8 GB),
`management.nc` (about 1.4 GB) and `staticData_quarterdeg.nc`. The script extracts
only the required 2015 variables to `luh2_2015_inputs.tif`; subsequent runs use
that compact cache. It also writes derived layers to
`agriculture_2015_0.25deg.tif`, source metadata to `source_metadata.json`, and
unthresholded area totals to `global_areas.csv`. Downloads are written to `.part`
files and validated before becoming reusable source files.

PNG and PDF figures are saved in `fig/luh2_agriculture_2015/`:

- `irrigated_cropland_2015`: irrigated crop area / full grid-cell area.
- `dominant_crop_types_2015`: largest of five crop functional groups.
- `grazing_management_proxy_2015`: two panels of supplied pasture and rangeland fractions.

## Calculations and interpretation

Irrigated fraction is `sum(crop_type_fraction * irrigated_share_of_that_type)`.
The management variables' denominator is **crop area**, whereas the state
variables use **full grid-cell area**. The map therefore shows area distribution,
not irrigation's share of cropland or water consumption. Missing management
values are accepted only where that crop has zero area. Fractions, geographic
coordinates, the selected year and land/ice/water closure are checked.

The crop map selects the largest area among `c3ann`, `c4ann`, `c3per`, `c4per`,
and `c3nfx`. It does not identify individual crops such as wheat or rice.
Cells with less than 1% cropland are left grey. Exact ties use the variable
order above. This threshold is solely for legibility; cached fractions and
area totals retain all cells. Crop-group proportions in the historical product
are based on contemporary crop information and should not be interpreted as
independent annual observations of crop composition.

The grazing figures display the supplied `pastr` (managed pasture) and `range`
(rangeland) variables directly in separate panels, each as a fraction of the
full grid cell on the same brown 0–1 scale. No dominance classification, mixed
class, or grazing-area threshold is applied. Cells with no grazing land are
shown at zero; ice/water-only cells are masked. These are land-use areas, not
measured livestock stocking rates or forage-removal intensity.

`managed_pasture_2015` and `rangeland_2015` are separate PNG/PDF figures.
The existing `grazing_management_proxy_2015` filename now contains both panels,
stacked vertically, for compatibility with existing figure references.

The irrigation layer is the LUH2/HYDE reconstruction, not a direct download of
the Siebert et al. (2015) historical irrigation dataset. These sources should not
be presented as interchangeable observations. The latter covers 1900–2005;
LUH2 provides a common year and grid for the three requested subjects.

## Sources

- [LUH2 official download page](https://luh.umd.edu/data.shtml), historical v2h.
- [LUH2 v2h variable definitions](https://luh.umd.edu/LUH2/LUH2_v2h_README.pdf).
- Hurtt et al. (2020), [Harmonization of global land use change and management
  for the period 850–2100 (LUH2) for CMIP6](https://doi.org/10.5194/gmd-13-5425-2020).
- [LUH2 FAQ: managed pasture and rangeland](https://luh.umd.edu/faq.shtml).
- Siebert et al. (2015), [A global data set of the extent of irrigated land from
  1900 to 2005](https://doi.org/10.5194/hess-19-1521-2015), original requested reference.

NetCDF source attributes, including licensing and release metadata, are retained
in the JSON provenance file. Raster processing uses terra/ncdf4; tables and
iteration use tidyverse packages, and figures use ggplot2.
