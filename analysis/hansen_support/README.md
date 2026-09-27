# Reconstructing Hansen et al. (2013), Figure 2

The plotting script reads `figure2_bounds.csv` instead of making small squares
around the caption coordinates. These bounds reconstruct the four displayed
footprints from the published image; they are **not author-supplied exact bounds**.
The windows span approximately 350–420 km east–west, rather than the previous
40 km windows.

Source: [paper and supplement](https://storage.googleapis.com/gweb-research2023-stg-media/pubtools/pdf/42119.pdf),
Figure 2, page 2; [DOI](https://doi.org/10.1126/science.1244693).

## Method and reproducibility

1. Cache the paper as `data/hansen_forest_change_2025/figure2_registration/hansen_2013.pdf`.
2. Run `Rscript analysis/hansen_support/download_references.R` to retrieve coarse
   baseline previews covering the four candidate regions. These previews are
   for registration only, not area calculations.
3. Run `python analysis/hansen_support/register_figure2.py` with NumPy, SciPy,
   Pillow, OpenCV, `gdalinfo` and Poppler's `pdfimages` available. It extracts
   the embedded RGB figure (2526 × 1739 pixels), masks change overlays and
   scale bars, and matches baseline landscape features using SIFT and affine
   RANSAC. Fixed panel boxes delimit the visible maps in that embedded image.
4. Fit independent longitude and Mercator-y axes to the matched points using
   robust least squares. The north-up Mercator model fits better than a linear
   latitude model, particularly for Russia. Write bounds and matching errors
   to `figure2_bounds.csv`; retain point correspondences in `*_matches.npz`.

The retained matches number 283 (Paraguay), 25 (Indonesia), 101 (USA), and
165 (Russia). Median ground-coordinate residuals are approximately 156, 212,
148 and 219 m respectively. These describe the registration fit, not independent
accuracy bounds. Image resolution, feature placement, projection assumptions and
cropping introduce uncertainty; the many decimal places preserve the fitted
transform and do not imply metre-scale accuracy. Rivers, coastlines and landscape
patterns were also compared visually across all four panels.

The matched Indonesia footprint is centred near **0.45°N, 101.48°E**, despite
“0.4°S” in the caption. The script follows the displayed geography. Russia's
matched centre also differs slightly from the rounded caption centre.

## Outputs and resolution

The main R script caches all four source layers for each reconstructed footprint
in `data/hansen_forest_change_2025/figure2_native/`. Multiple 10° source tiles are
joined where necessary. These files retain the native 0.00025° grid (nominally
30 m); crop boundaries snap outward by at most one source cell. Area statistics
use all native cells, with geodesic cell areas and mapped land as denominator.

For printable PNG/PDF maps, each region is projected to a 2400-column Mercator
display grid using nearest-neighbour sampling. These are overviews, not full
native-pixel exports or spatially averaged fractions. Inspect the cached source
GeoTIFFs for full-resolution detail. Scale bars describe approximate distances
at the middle latitude. The 2000 canopy background is not remaining 2025 forest.

Loss is `lossyear` 1–25 (2001–2025, relative to baseline 2000). Gain remains
`gain == 1`, **2000–2012**, as supplied in v1.13. No 2013–2025 gain estimate or
net 2000–2025 change is inferred. Loss and gain may occur in the same cell.

The combined figure uses one panel per region in a 2 × 2 layout. Orange denotes
loss only, blue gain only, and purple both gain and loss. The three classes are
mutually exclusive, and percentages beneath each map use native-pixel areas.
Overlap does not establish the order of events and combines the stated different
observation periods. Individual region figures use the same classification.
