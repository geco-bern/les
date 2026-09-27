# Historical regional land-use-change carbon emissions

Run from the repository root:

```sh
Rscript analysis/plot_regional_land_use_emissions.R
```

Dependencies: `readxl`, `countrycode`, `ggplot2`, `here`, and `digest`.
The script downloads the source if absent, verifies its SHA-256, processes the
workbook, validates the regional sums, and writes PNG/PDF figures. Repeated runs
use the cached workbook and country mapping and require no download. The script
does not install packages or modify other analysis workflows.

## Data and attribution

- [GCB 2025 national land-use-change emissions, v1.0](https://meta.icos-cp.eu/objects/milTbWkl0G-MSpdYG3IBIfzy).
- [Friedlingstein et al. (2026), Global Carbon Budget 2025](https://doi.org/10.5194/essd-18-3211-2026), ESSD 18, 3211–3288.
- [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

The pinned ICOS object is the May 2026 version of the GCB 2025 supplement.
Its workbook contains annual national estimates for 1850–2024 from BLUE, OSCAR
and LUCE, plus the model citations in their worksheet headers. Source values are
Tg C/year (million tonnes of carbon/year); divide by 1000 to obtain Gt C/year.
No CO2 mass conversion is applied. The original workbook is preserved unchanged.
We acknowledge the Global Carbon Project and the BLUE, OSCAR and LUCE modelling
groups for producing and making these estimates available.

## Regions

The 197 named countries are mapped using the UN geographic regions and subregions
in `countrycode`. The resulting explicit mapping is saved in `country_regions.csv`
and reused, making membership inspectable and editable without altering source
data. Russia is separated from Europe. All of each supplied national series is
assigned to one region; transcontinental countries are not split geographically.

| Plot region | Membership rule |
| --- | --- |
| North America | UN Northern America: Canada and USA in this workbook |
| Latin America & Caribbean | UN Latin America and the Caribbean, including Mexico |
| Europe excluding Russia | UN Europe, excluding Russia |
| Russia | Russia alone |
| Africa | UN Africa |
| East Asia | UN Eastern Asia |
| South & Southeast Asia | UN Southern Asia and South-eastern Asia; includes Iran under the UN classification |
| West & Central Asia | UN Western Asia and Central Asia |
| Oceania | UN Oceania |
| Other / disputed territories | Source columns OTHER and DISPUTED, retained without inventing a geographic allocation |

`Global` and `EU27` are overlapping aggregates and are excluded from the regional
summation. The complete regional sum is checked against the supplied `Global`
series for every model and year, allowing only the source rounding tolerance
(less than 0.000001 Gt C/year). Other/disputed territories are retained in the data and global total but omitted
from the nine regional panels.

## Annual values and display

1. Sum countries **within each model**, then convert Tg C to Gt C.
2. For each year from 1850 through 2024, plot the unweighted mean of the three
   annual model estimates. No temporal smoothing or averaging is applied.
3. The ribbon spans the minimum to maximum of those three annual regional
   estimates. This is **model spread**, not a confidence interval or a complete
   estimate of uncertainty. Model minima/maxima are computed after regional
   aggregation, not summed across countries.
4. Use identical axes for the nine regional panels (3 × 3); positive values are net emissions and
   negative values net removals associated with land use.

Net land-use-change emissions include legacy decay and regrowth after earlier
activities. They are not the complete terrestrial carbon sink or emissions solely
from land newly cleared that year. The curves describe carbon consequences and
do not independently attribute socioeconomic drivers. Changes before 1850 are
outside the plotted period. Bookkeeping model estimates and their regional
histories differ substantially; the range makes that disagreement visible.

## Outputs and checks

Figures in PNG and PDF:

- `fig/gcb_luc_2025/regional_land_use_emissions_1850_2024`: nine regions in a 3 × 3 grid.
- `fig/gcb_luc_2025/global_land_use_emissions_1850_2024`: separate global total,
  based on each model's supplied Global column, including other/disputed territories.
  Its line and ribbon are the mean and range of the three annual global model totals,
  not sums of the regional model minima/maxima.

The global model series and summary are also saved as `global_annual_by_model.csv`
and `global_annual_summary.csv`.
`data/gcb_luc_2025/` contains the source workbook, country mapping, provenance,
annual regional model series, the plotted annual summary, and
`global_reconciliation.csv` with the source and recomputed global sums.

Validation checks the exact source checksum, units, complete 1850–2024 coverage,
consistent country columns across models, finite numeric source values, country
mapping completeness and uniqueness, global reconciliation, complete annual coverage, and the three-model summary bounds.
The earlier `regional_10year_*.csv` tables are legacy outputs and are not used by
the current script or figures.
