"""Tests for fieldgen2d_sva_z and fieldgen3d_sva_z (user-supplied deviates)."""

import numpy as np
import pytest

from pypestutils.pestutilslib import PestUtilsLib, PestUtilsLibError


@pytest.fixture
def lib():
    lib = PestUtilsLib()
    yield lib
    lib.free_all_memory()


@pytest.fixture
def grid2d():
    x, y = np.meshgrid(np.arange(20.0), np.arange(15.0))
    x, y = x.ravel(), y.ravel()
    active = np.ones(x.size, dtype=int)
    active[::11] = 0
    return x, y, active


def _f2d(lib, grid, z, transtype=0, avetype=2, mean=2.0, var=0.5):
    x, y, active = grid
    return lib.fieldgen2d_sva_z(
        x, y, 1.0, active, mean, var, 4.0, 2.0, 30.0, transtype, avetype, 1.5, z
    )


def _f3d(lib, grid, z, transtype=0, avetype=2, mean=2.0, var=0.5):
    x, y, active = grid
    zc = 0.3 * x
    return lib.fieldgen3d_sva_z(
        x,
        y,
        zc,
        1.0,
        1.0,
        active,
        mean,
        var,
        5.0,
        3.0,
        2.0,
        30.0,
        10.0,
        5.0,
        transtype,
        avetype,
        1.5,
        z,
    )


@pytest.fixture(params=["2d", "3d"])
def fgen(request):
    return _f2d if request.param == "2d" else _f3d


@pytest.mark.parametrize("avetype", [1, 2, 3, 4])
def test_zero_z_gives_mean(lib, grid2d, fgen, avetype):
    _, _, active = grid2d
    z = np.zeros((active.size, 2))
    res = fgen(lib, grid2d, z, transtype=0, avetype=avetype)
    assert res.shape == (active.size, 2)
    np.testing.assert_allclose(res[active != 0], 2.0)
    res = fgen(lib, grid2d, z, transtype=1, avetype=avetype)
    np.testing.assert_allclose(res[active != 0], 2.0)


def test_inactive_nodes_untouched(lib, grid2d, fgen):
    _, _, active = grid2d
    rng = np.random.default_rng(0)
    z = rng.standard_normal((active.size, 3))
    res = fgen(lib, grid2d, z)
    assert np.all(res[active == 0] == 0.0)
    # deviates at inactive nodes do not influence the result
    z2 = z.copy()
    z2[active == 0] = 99.0
    np.testing.assert_array_equal(fgen(lib, grid2d, z2), res)


@pytest.mark.parametrize("avetype", [1, 2, 3, 4])
def test_linear_in_z(lib, grid2d, fgen, avetype):
    _, _, active = grid2d
    act = active != 0
    rng = np.random.default_rng(1)
    z1 = rng.standard_normal(active.size)
    z2 = rng.standard_normal(active.size)
    mean = 2.0
    f1 = fgen(lib, grid2d, z1, avetype=avetype)[act, 0] - mean
    f2 = fgen(lib, grid2d, z2, avetype=avetype)[act, 0] - mean
    f12 = fgen(lib, grid2d, 2.5 * z1 - z2, avetype=avetype)[act, 0] - mean
    np.testing.assert_allclose(f12, 2.5 * f1 - f2, atol=1e-10)
    # log transform is linear in log10 space
    g1 = fgen(lib, grid2d, z1, transtype=1, avetype=avetype)[act, 0]
    np.testing.assert_allclose(np.log10(g1 / mean), f1, atol=1e-10)


def test_1d_z_and_multi_real_consistent(lib, grid2d, fgen):
    _, _, active = grid2d
    rng = np.random.default_rng(2)
    z = rng.standard_normal((active.size, 4))
    multi = fgen(lib, grid2d, z)
    for i in range(4):
        single = fgen(lib, grid2d, z[:, i])
        assert single.shape == (active.size, 1)
        np.testing.assert_array_equal(single[:, 0], multi[:, i])
    # C-ordered input gives the same answer as Fortran-ordered
    np.testing.assert_array_equal(fgen(lib, grid2d, np.ascontiguousarray(z)), multi)


def test_variance_reproduced(lib, grid2d, fgen):
    _, _, active = grid2d
    rng = np.random.default_rng(3)
    z = rng.standard_normal((active.size, 4000))
    var = 0.5
    res = fgen(lib, grid2d, z, var=var)
    sample_var = res[active != 0].var(axis=1)
    np.testing.assert_allclose(sample_var, var, rtol=0.12)


def test_no_seed_required():
    # fresh library with memory freed, i.e. the generator is not seeded
    lib = PestUtilsLib()
    lib.free_all_memory()
    x, y = np.meshgrid(np.arange(5.0), np.arange(4.0))
    with pytest.raises(PestUtilsLibError, match="seeded"):
        lib.fieldgen2d_sva(
            x.ravel(), y.ravel(), 1.0, 1, 1.0, 1.0, 2.0, 1.0, 0.0, 0, 2, 1.0, 1
        )
    res = lib.fieldgen2d_sva_z(
        x.ravel(), y.ravel(), 1.0, 1, 1.0, 1.0, 2.0, 1.0, 0.0, 0, 2, 1.0, np.ones(20)
    )
    assert res.shape == (20, 1)
    lib.free_all_memory()


def test_bad_z(lib, grid2d, fgen):
    _, _, active = grid2d
    n = active.size
    with pytest.raises(ValueError, match="first dimension"):
        fgen(lib, grid2d, np.zeros((n - 1, 2)))
    with pytest.raises(ValueError, match="ndim"):
        fgen(lib, grid2d, np.zeros((n, 2, 1)))
    with pytest.raises(ValueError, match="at least one"):
        fgen(lib, grid2d, np.zeros((n, 0)))
    z = np.zeros(n)
    z[3] = np.nan
    with pytest.raises(ValueError, match="finite"):
        fgen(lib, grid2d, z)


def test_fortran_error_propagates(lib, grid2d, fgen):
    _, _, active = grid2d
    with pytest.raises(PestUtilsLibError, match="AVETYPE"):
        fgen(lib, grid2d, np.zeros(active.size), avetype=5)


def test_matches_fieldgen_sva_statistics(lib, grid2d):
    """fieldgen2d_sva and fieldgen2d_sva_z share one convolution kernel; with
    many realisations their node-wise variances should agree."""
    x, y, active = grid2d
    nreal = 3000
    lib.initialize_randgen(4321)
    a = lib.fieldgen2d_sva(
        x, y, 1.0, active, 2.0, 0.5, 4.0, 2.0, 30.0, 0, 2, 1.0, nreal
    )
    z = np.random.default_rng(5).standard_normal((x.size, nreal))
    b = _f2d(lib, grid2d, z)
    act = active != 0
    np.testing.assert_allclose(a[act].var(axis=1), b[act].var(axis=1), rtol=0.15)
