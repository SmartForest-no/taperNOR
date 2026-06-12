## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local Windows 11, R 4.6.0
* GitHub Actions: ubuntu-latest, macos-latest, windows-latest (R release)

## Notes

* The package depends on 'TapeR' (CRAN) for the `kublin_nor()` function.
* The Kublin (2013) mixed-effects taper model is currently fitted for Norway
  spruce only; pine and birch raise an informative error. The Kozak-style
  taper, bark, and volume functions (`taperNOR()`, `barkNOR()`, `volume()`,
  `hfromd()`, `dlocation()`) support all three species.

## Reverse dependencies

There are currently no reverse dependencies.
