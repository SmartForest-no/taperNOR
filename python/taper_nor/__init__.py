"""Taper, bark, volume, and inverse-taper models for spruce, pine, and birch in Norway.

Python port of the "calculator" functions in the taperNOR R package
(https://github.com/SmartForest-no/taperNOR). ``kublin_nor()`` is not
included here, as it depends on the TapeR mixed-effects model and remains
R-only.
"""

from .bark import bark_nor
from .coefficients import normalize_species
from .inverse import dlocation, hfromd
from .taper import taper_nor
from .volume import volume

__all__ = [
    "taper_nor",
    "bark_nor",
    "volume",
    "hfromd",
    "dlocation",
    "normalize_species",
]
