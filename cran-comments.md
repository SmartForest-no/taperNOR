## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is the expected new-submission note. It also flags two
"possibly misspelled words" in DESCRIPTION, `et` and `al`, which are part of
"Hansen et al. (2023)" in a citation and are not misspellings.

## Test environments

* win-builder, R-devel (R Under development)
* win-builder, R-release (R 4.6.0)
* GitHub Actions: ubuntu-latest, macos-latest, windows-latest (R release)
* local Windows 11, R 4.6.0

All of the above gave 0 errors and 0 warnings, with only the new-submission
NOTE described above.

## Notes

* This is a new submission.
* The package imports 'TapeR' (CRAN) for the `kublin_nor()` function.
* The Kublin (2013) mixed-effects taper model (`kublin_nor()`) is currently
  fitted for Norway spruce and Scots pine; birch is not yet fitted and raises
  an informative error. The Kozak-style taper, bark, and volume functions
  (`taperNOR()`, `barkNOR()`, `volume()`, `hfromd()`, `dlocation()`) support
  all three species.

## Reverse dependencies

There are currently no reverse dependencies.
