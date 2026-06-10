"""Species coefficients and species-name normalization.

Coefficients are taken from Hansen et al. (2023) (taper, exponent terms) and
Hannrup (2004), via Stängle et al. (2017) (bark thickness).
"""

from __future__ import annotations

TAPER_COEFFICIENTS: dict[str, dict[str, float]] = {
    "spruce": dict(
        b1=1.0625010,
        b2=0.9590684,
        b3=0.9982461,
        b4=2.2909135,
        b5=-0.5201230,
        b6=3.8808849,
        b7=-2.1078922,
        b8=0.1695809,
    ),
    "pine": dict(
        b1=1.14798036,
        b2=0.90295964,
        b3=1.00118665,
        b4=0.24116857,
        b5=-0.09667025,
        b6=-0.50359177,
        b7=0.32132441,
        b8=0.05546691,
    ),
    "birch": dict(
        b1=0.9810885,
        b2=0.9936293,
        b3=0.9941538,
        b4=0.8526987,
        b5=-0.1819791,
        b6=0.4687623,
        b7=-0.2198294,
        b8=0.1591102,
    ),
}

BARK_COEFFICIENTS: dict[str, dict[str, float]] = {
    "spruce": dict(b0=2.32391608, b1=0.06830421, b2=0.39936183),
    "pine": dict(b0=2.9306985, b1=-0.4045155, b2=1.2126856),
    "birch": dict(b0=-0.48552367, b1=0.04957885, b2=0.84676248),
}

_SPECIES_ALIASES: dict[str, str] = {
    "spruce": "spruce",
    "s": "spruce",
    "gran": "spruce",
    "g": "spruce",
    "1": "spruce",
    "pine": "pine",
    "p": "pine",
    "furu": "pine",
    "f": "pine",
    "2": "pine",
    "birch": "birch",
    "b": "birch",
    "bjørk": "birch",
    "bjork": "birch",
    "bj": "birch",
    "lauv": "birch",
    "l": "birch",
    "3": "birch",
}


def normalize_species(sp: str) -> str:
    """Normalize a species identifier to one of ``"spruce"``, ``"pine"``, ``"birch"``.

    Accepts the same aliases as the R package, e.g. ``"gran"``/``"g"``/``"1"``
    for spruce, ``"furu"``/``"f"``/``"2"`` for pine, and
    ``"bjørk"``/``"bjork"``/``"lauv"``/``"3"`` for birch (case-insensitive).

    Raises:
        ValueError: If ``sp`` is not a recognized species or alias.
    """
    key = str(sp).strip().lower()
    try:
        return _SPECIES_ALIASES[key]
    except KeyError as exc:
        raise ValueError(
            "sp must be one of spruce/pine/birch (or their aliases)"
        ) from exc
