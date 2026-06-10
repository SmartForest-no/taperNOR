# taper-nor

Python port of the taper, bark, volume, and inverse-taper "calculator"
functions from the [taperNOR](https://github.com/SmartForest-no/taperNOR) R
package, for spruce, pine, and birch in Norway.

This package does **not** include `kublin_nor()`, which depends on the
`TapeR` mixed-effects model and remains R-only (use the R package, e.g. via
`Rscript`, for that function).

## Installation

```bash
pip install "git+https://github.com/SmartForest-no/taperNOR.git#subdirectory=python"
```

## Usage

```python
from taper_nor import taper_nor, bark_nor, volume, hfromd, dlocation

# Diameter (cm) at 1 m above ground for a spruce, dbh=30 cm, total height=25 m
taper_nor(h=1, dbh=30, h_top=25, sp="spruce")

# Bark thickness (mm)
bark_nor(d=25, h=1, dbh=30, h_top=25, sp="spruce")

# Stem volume (m^3)
volume(dbh=[30], h_top=[25], sp="spruce")

# Estimate total height and dbh from diameter measurements
hfromd(d=[39, 27], h=[2, 7], sp="birch")

# Height(s) along the stem at which given diameters occur
dlocation(dbh=30, h_top=25, d=[25, 15], sp="spruce")
```

## Species

`sp` accepts the same aliases as the R package: `"spruce"`/`"s"`/`"gran"`/`"g"`/`"1"`,
`"pine"`/`"p"`/`"furu"`/`"f"`/`"2"`, and `"birch"`/`"b"`/`"bjørk"`/`"bjork"`/`"bj"`/`"lauv"`/`"l"`/`"3"`.

## Differences from the R package

- `taper_nor()`, `bark_nor()`, `volume()`, and `hfromd()` return unrounded
  floats, matching R's behavior (`dlocation()` rounds to 2 decimals, also
  matching R).
- `volume()`'s `sp` and `with_bark` accept either a single value (applied to
  every tree) or one value per tree, like R.
- If `h_vol_lower > h_vol_upper` for a tree, `h_vol_lower` is clamped to
  `h_vol_upper` for that tree (volume 0). R has an inverted-condition bug in
  this edge case that instead zeroes out the *valid* trees; this port does
  not replicate that bug.
- `hfromd()` always runs the 12x8 grid search of starting points (R's
  `grd_search=TRUE` mode) rather than R's default single-optimization
  attempt, and always returns both `h_top` and `dbh` (R's `output=`
  parameter is not exposed). Because R's `optim()` and SciPy's optimizers
  differ internally, results are close but not bit-identical to R.
