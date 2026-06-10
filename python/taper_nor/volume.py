"""Stem volume by integration of the taper curve."""

from __future__ import annotations

import math
from typing import Sequence

import numpy as np
from scipy import integrate

from .coefficients import normalize_species
from .taper import _taper


def volume(
    dbh: Sequence[float],
    h_top: Sequence[float],
    h_vol_lower: Sequence[float | None] | None = None,
    h_vol_upper: Sequence[float | None] | None = None,
    sp: str = "spruce",
    with_bark: bool = True,
) -> list[float]:
    """Stem volume (m^3) by integrating the taper curve.

    Args:
        dbh: Diameter at breast height (cm), one entry per tree.
        h_top: Total tree height (m), one entry per tree.
        h_vol_lower: Lower integration height (m) per tree. Defaults to 1% of
            ``h_top``. ``None`` entries fall back to this default.
        h_vol_upper: Upper integration height (m) per tree. Defaults to
            ``h_top``. ``None`` entries fall back to this default.
        sp: Species, see :func:`taper_nor.coefficients.normalize_species`.
        with_bark: Integrate diameter over bark (``True``, default) or under
            bark (``False``).

    Returns:
        Stem volume (m^3) for each tree.

    Raises:
        ValueError: If ``dbh``, ``h_top``, ``h_vol_lower``, or ``h_vol_upper``
            do not all have the same length.
    """
    n = len(dbh)
    if len(h_top) != n:
        raise ValueError("dbh and h_top must have the same length")

    sp_n = normalize_species(sp)

    lowers = h_vol_lower if h_vol_lower is not None else [None] * n
    uppers = h_vol_upper if h_vol_upper is not None else [None] * n
    if len(lowers) != n or len(uppers) != n:
        raise ValueError("h_vol_lower and h_vol_upper must be the same length as dbh")

    results = []
    for d, ht, lo, up in zip(dbh, h_top, lowers, uppers):
        upper = ht if up is None else up
        lower = ht * 0.01 if lo is None else lo
        if lower > upper:
            lower = upper

        def integrand(h: float, d: float = d, ht: float = ht) -> float:
            taper = _taper(np.array([h]), d, ht, sp_n, with_bark)[0]
            return ((taper / 100) / 2) ** 2

        val, _ = integrate.quad(integrand, lower, upper)
        results.append(round(math.pi * val, 6))

    return results
