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
    sp: str | Sequence[str] = "spruce",
    with_bark: bool | Sequence[bool] = True,
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
            Either a single value applied to every tree, or one value per
            tree (same length as ``dbh``).
        with_bark: Integrate diameter over bark (``True``, default) or under
            bark (``False``). Either a single value applied to every tree, or
            one value per tree (same length as ``dbh``).

    Returns:
        Stem volume (m^3) for each tree. Unrounded, matching R's ``volume()``.

    Raises:
        ValueError: If ``dbh``, ``h_top``, ``h_vol_lower``, ``h_vol_upper``,
            ``sp``, or ``with_bark`` do not all have the same length (where
            ``sp``/``with_bark`` may also be a single scalar applied to every
            tree).

    Note:
        If ``h_vol_lower`` ends up greater than ``h_vol_upper`` for a given
        tree, ``h_vol_lower`` is clamped to ``h_vol_upper`` for that tree
        (yielding a volume of 0). This differs from the R implementation,
        which has an inverted-condition bug in this edge case (it instead
        clamps the *valid* trees to zero and leaves the actually-invalid
        ones to integrate "backwards" into a negative volume).
    """
    n = len(dbh)
    if len(h_top) != n:
        raise ValueError("dbh and h_top must have the same length")

    lowers = h_vol_lower if h_vol_lower is not None else [None] * n
    uppers = h_vol_upper if h_vol_upper is not None else [None] * n
    if len(lowers) != n or len(uppers) != n:
        raise ValueError("h_vol_lower and h_vol_upper must be the same length as dbh")

    sps = [sp] * n if isinstance(sp, str) else list(sp)
    if len(sps) != n:
        raise ValueError("sp must be a single value or the same length as dbh")
    sps_n = [normalize_species(s) for s in sps]

    with_barks = [with_bark] * n if isinstance(with_bark, bool) else list(with_bark)
    if len(with_barks) != n:
        raise ValueError("with_bark must be a single value or the same length as dbh")

    results = []
    for d, ht, lo, up, sp_n, wb in zip(dbh, h_top, lowers, uppers, sps_n, with_barks):
        upper = ht if up is None else up
        lower = ht * 0.01 if lo is None else lo
        if lower > upper:
            lower = upper

        def integrand(h: float, d: float = d, ht: float = ht, sp_n: str = sp_n, wb: bool = wb) -> float:
            taper = _taper(np.array([h]), d, ht, sp_n, wb)[0]
            return ((taper / 100) / 2) ** 2

        val, _ = integrate.quad(integrand, lower, upper)
        results.append(math.pi * val)

    return results
