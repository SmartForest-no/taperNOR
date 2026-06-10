"""Taper model for spruce, pine, and birch in Norway.

Implements the variable-exponent taper equation of Hansen et al. (2023),
based on Kozak's (1988) variable-exponent taper equation.
"""

from __future__ import annotations

import math
from typing import Sequence

import numpy as np
import numpy.typing as npt

from .bark import _bark
from .coefficients import TAPER_COEFFICIENTS, normalize_species

_R02 = math.sqrt(0.2)


def _taper(
    h: npt.ArrayLike, dbh: float, h_top: float, sp: str, with_bark: bool
) -> np.ndarray:
    """Diameter (cm) at height(s) ``h`` for a normalized species key."""
    c = TAPER_COEFFICIENTS[sp]
    h = np.asarray(h, dtype=float)

    ratio = h / h_top
    exponent = (
        c["b4"] * ratio**2
        + c["b5"] * np.log(ratio + 0.001)
        + c["b6"] * np.sqrt(ratio)
        + c["b7"] * np.exp(ratio)
        + c["b8"] * (dbh / h_top)
    )

    # h > h_top makes the base of this term negative, which yields NaN for
    # fractional exponents (e.g. during optimization in hfromd/dlocation).
    with np.errstate(invalid="ignore", divide="ignore"):
        d = (
            c["b1"]
            * dbh ** c["b2"]
            * c["b3"] ** dbh
            * ((1 - np.sqrt(ratio)) / (1 - _R02)) ** exponent
        )

    if with_bark:
        return d

    bark = _bark(d, h, dbh, h_top, sp)
    d_ub = d - bark / 10
    return np.where(d_ub < 0, 0, d_ub)


def taper_nor(
    h: Sequence[float] | npt.ArrayLike,
    dbh: float,
    h_top: float,
    sp: str = "spruce",
    with_bark: bool = True,
) -> list[float]:
    """Stem diameter(s) (cm) at given height(s) above ground.

    Based on Hansen et al. (2023), a Kozak (1988) variable-exponent taper
    equation.

    Args:
        h: Height(s) above ground (m) at which to return diameters.
        dbh: Diameter at breast height, 1.3 m above ground, over bark (cm).
        h_top: Total tree height (m).
        sp: Species, see :func:`taper_nor.coefficients.normalize_species`.
        with_bark: If ``True`` (default), return diameter over bark.
            If ``False``, subtract bark thickness via :func:`taper_nor.bark.bark_nor`.

    Returns:
        Diameter (cm) for each element of ``h``. Unrounded, matching R's
        ``taperNOR()``.
    """
    sp_n = normalize_species(sp)
    result = _taper(h, dbh, h_top, sp_n, with_bark)
    return [float(x) for x in np.atleast_1d(result)]
