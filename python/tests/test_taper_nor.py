import math

import pytest

from taper_nor import bark_nor, dlocation, hfromd, normalize_species, taper_nor, volume


def test_normalize_species_aliases():
    assert normalize_species("spruce") == "spruce"
    assert normalize_species("Gran") == "spruce"
    assert normalize_species("1") == "spruce"
    assert normalize_species("furu") == "pine"
    assert normalize_species("bjork") == "birch"
    assert normalize_species("bjørk") == "birch"


def test_normalize_species_invalid():
    with pytest.raises(ValueError):
        normalize_species("oak")


def test_taper_nor_known_value_spruce():
    # Known output for spruce, dbh=30, h_top=25 at breast height (1.3 m).
    result = taper_nor(h=1.3, dbh=30, h_top=25, sp="spruce")
    assert result == pytest.approx([30.0913], abs=1e-4)


def test_taper_nor_multiple_heights_decreasing():
    result = taper_nor(h=[1, 5, 10, 20], dbh=30, h_top=25, sp="spruce")
    assert len(result) == 4
    # Diameter should decrease monotonically up the stem.
    assert result == sorted(result, reverse=True)


def test_taper_nor_under_bark_smaller_than_over_bark():
    over = taper_nor(h=1, dbh=30, h_top=25, sp="spruce", with_bark=True)[0]
    under = taper_nor(h=1, dbh=30, h_top=25, sp="spruce", with_bark=False)[0]
    assert under < over


def test_taper_nor_species_aliases_match():
    assert taper_nor(h=1, dbh=30, h_top=25, sp="furu") == taper_nor(
        h=1, dbh=30, h_top=25, sp="pine"
    )


def test_bark_nor_known_value_spruce():
    result = bark_nor(d=25, h=1, dbh=30, h_top=25, sp="spruce")
    assert result == pytest.approx([14.3571], abs=1e-4)


def test_volume_known_value_spruce():
    result = volume(dbh=[30], h_top=[25], sp="spruce")
    assert result == pytest.approx([0.810026], abs=1e-5)


def test_volume_under_bark_smaller():
    over = volume(dbh=[30], h_top=[25], sp="spruce", with_bark=True)[0]
    under = volume(dbh=[30], h_top=[25], sp="spruce", with_bark=False)[0]
    assert under < over


def test_volume_length_mismatch_raises():
    with pytest.raises(ValueError):
        volume(dbh=[30, 25], h_top=[25])


def test_volume_per_tree_species_and_bark():
    # Per-tree sp/with_bark should match calling volume() once per tree.
    dbh, h_top = [30, 25], [25, 20]
    sp, with_bark = ["spruce", "pine"], [True, False]

    combined = volume(dbh=dbh, h_top=h_top, sp=sp, with_bark=with_bark)
    individual = [
        volume(dbh=[d], h_top=[ht], sp=[s], with_bark=[wb])[0]
        for d, ht, s, wb in zip(dbh, h_top, sp, with_bark)
    ]

    assert combined == pytest.approx(individual)


def test_volume_sp_length_mismatch_raises():
    with pytest.raises(ValueError):
        volume(dbh=[30, 25], h_top=[25, 20], sp=["spruce"])


def test_dlocation_inverts_taper():
    dbh, h_top, sp = 30.0, 25.0, "spruce"
    target_d = [25.0, 15.0]

    heights = dlocation(dbh=dbh, h_top=h_top, d=target_d, sp=sp)
    recovered_d = taper_nor(h=heights, dbh=dbh, h_top=h_top, sp=sp)

    for expected, actual in zip(target_d, recovered_d):
        assert actual == pytest.approx(expected, abs=0.01)


def test_hfromd_recovers_known_tree():
    dbh, h_top, sp = 30.0, 25.0, "spruce"
    h = [1, 5, 10, 20]
    d = taper_nor(h=h, dbh=dbh, h_top=h_top, sp=sp)

    result = hfromd(d=d, h=h, sp=sp)

    assert result["h_top"] == pytest.approx(h_top, abs=0.1)
    assert result["dbh"] == pytest.approx(dbh, abs=0.1)


def test_hfromd_raises_when_all_heights_too_low():
    with pytest.raises(ValueError):
        hfromd(d=[10, 12], h=[0.1, 0.4], sp="spruce")
