"""Inverse taper functions: estimate height/dbh from diameters, and the
height at which a given diameter occurs.
"""

from __future__ import annotations

from typing import Sequence

import numpy as np
from scipy import optimize

from .coefficients import normalize_species
from .taper import _taper


def hfromd(d: Sequence[float], h: Sequence[float], sp: str = "spruce") -> dict[str, float]:
    """Estimate total tree height and dbh from diameter measurements.

    Fits ``h_top`` and ``dbh`` by minimizing the mean absolute error between
    measured diameters ``d`` (at heights ``h``) and the taper curve.

    Args:
        d: Measured diameters (cm).
        h: Heights above ground (m) corresponding to ``d``. Measurements
            with ``h < 0.5`` are dropped, as the optimization is unstable
            near ground level.
        sp: Species, see :func:`taper_nor.coefficients.normalize_species`.

    Returns:
        A dict with keys ``"h_top"`` (estimated total height, m) and
        ``"dbh"`` (estimated diameter at breast height, cm).

    Raises:
        ValueError: If all diameter measurements have ``h < 0.5``.
    """
    sp_n = normalize_species(sp)
    d_arr = np.asarray(d, dtype=float)
    h_arr = np.asarray(h, dtype=float)

    mask = h_arr >= 0.5
    d_arr, h_arr = d_arr[mask], h_arr[mask]
    if len(d_arr) == 0:
        raise ValueError("All diameter measurements have h < 0.5 m")

    def objective(x: np.ndarray) -> float:
        h_top, dbh = x
        if h_top <= 0 or dbh <= 0:
            return 1e6
        try:
            taper = _taper(h_arr, dbh, h_top, sp_n, True)
        except Exception:
            return 1e6
        err = float(np.mean(np.abs(d_arr - taper)))
        return err if np.isfinite(err) else 1e6

    best = None
    for st_h in np.arange(1.5, 46, 4):
        for st_d in np.arange(10, 90, 10):
            res = optimize.minimize(objective, x0=[st_h, st_d], method="Nelder-Mead")
            if best is None or res.fun < best.fun:
                best = res

    return {"h_top": round(float(best.x[0]), 4), "dbh": round(float(best.x[1]), 4)}


def dlocation(
    dbh: float,
    h_top: float,
    d: Sequence[float],
    sp: str = "spruce",
    with_bark: bool = True,
) -> list[float]:
    """Estimate the height(s) at which given diameters occur along the stem.

    Args:
        dbh: Diameter at breast height, 1.3 m above ground (cm).
        h_top: Total tree height (m).
        d: Diameter(s) (cm) for which to find the corresponding height(s).
        sp: Species, see :func:`taper_nor.coefficients.normalize_species`.
        with_bark: Match against diameter over bark (``True``, default) or
            under bark (``False``).

    Returns:
        Estimated height (m) for each element of ``d``.
    """
    sp_n = normalize_species(sp)

    results = []
    for d_o in d:

        def objective(h_x: float, d_o: float = d_o) -> float:
            if h_x < 0 or h_x > h_top:
                return 1e6
            d_i = _taper(np.array([h_x]), dbh, h_top, sp_n, with_bark)[0]
            return float(abs(d_o - d_i))

        res = optimize.minimize_scalar(objective, bounds=(0, h_top), method="bounded")
        results.append(round(float(res.x), 2))

    return results
