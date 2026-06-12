# taperNOR 0.1.0

* `taperNOR()`, `barkNOR()`, and `volume()` implement the taper, bark
  thickness, and volume models of Hansen et al. (2023) for Norway spruce,
  Scots pine, and birch.
* `hfromd()` and `dlocation()` invert the taper curve to estimate tree height
  and diameter at breast height from measured diameters, and to locate the
  height at which a given diameter occurs.
* `kublin_nor()` provides the Kublin (2013) mixed-effects taper model via
  `TapeR`. The model is currently fitted for spruce only; pine and birch now
  raise an informative error instead of silently using the spruce model.
* `kublin_nor()` accepts a vector of diameters in `Dx`, returning one height
  per element.
* `volume()` accepts integer inputs and allows per-tree `sp` and `with_bark`,
  and the `h_vol_lower`/`h_vol_upper` clamping no longer affects valid trees.
* Added a Python port of the calculator functions under `python/`.
