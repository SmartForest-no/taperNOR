"""Bark thickness model for spruce, pine, and birch in Norway."""

from __future__ import annotations

from typing import Sequence

import numpy as np
import numpy.typing as npt

from .coefficients import BARK_COEFFICIENTS, normalize_species


def _bark(
    d: npt.ArrayLike, h: npt.ArrayLike, dbh: float, h_top: float, sp: str
) -> np.ndarray:
    """Bark thickness (mm) for a normalized species key."""
    c = BARK_COEFFICIENTS[sp]
    d = np.asarray(d, dtype=float)
    return c["b0"] + c["b1"] * dbh + c["b2"] * d


def bark_nor(
    d: Sequence[float] | npt.ArrayLike,
    h: Sequence[float] | npt.ArrayLike,
    dbh: float,
    h_top: float,
    sp: str = "spruce",
) -> list[float]:
    """Bark thickness (mm) at given diameter/height pairs.

    Based on Hannrup (2004), via Stängle et al. (2017).

    Args:
        d: Diameter(s) over bark (cm) at the corresponding height(s) in ``h``.
        h: Height(s) above ground (m) corresponding to ``d``.
        dbh: Diameter at breast height, 1.3 m above ground, over bark (cm).
        h_top: Total tree height (m).
        sp: Species, see :func:`taper_nor.coefficients.normalize_species`.

    Returns:
        Bark thickness (mm) for each element of ``d``. Unrounded, matching
        R's ``barkNOR()``.
    """
    sp_n = normalize_species(sp)
    result = _bark(d, h, dbh, h_top, sp_n)
    return [float(x) for x in np.atleast_1d(result)]
