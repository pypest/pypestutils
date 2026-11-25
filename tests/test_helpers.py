import os

import numpy as np
import pandas as pd


def test_mf6_mod2obs():
    import pypestutils.helpers as helpers

    start_datetime = pd.to_datetime("1-1-2000")

    dts = []
    for dt in np.linspace(100, 300, 10):
        dts.append(start_datetime + pd.to_timedelta(dt, unit="d"))
    obs_df = pd.DataFrame(
        {
            "site": ["site1" for _ in range(len(dts))],
            "x": [101 for _ in range(len(dts))],
            "y": [221 for _ in range(len(dts))],
            "layer": [1 for _ in range(len(dts))],
        }
    )
    obs_df.loc[:, "datetime"] = dts

    ws = os.path.join("tests", "data", "p09")
    case = "gwf-p09-mf6"
    obs_csv = os.path.join(ws, "obs.csv")
    obs_df.to_csv(obs_csv)

    dv_name = os.path.join(ws, case + ".hds")
    grb_name = os.path.join(ws, case + ".dis.grb")

    results = helpers.mod2obs_mf6(
        grb_name, dv_name, obs_csv, 31, start_datetime, 1, "head"
    )
    iresults = results["interpolated_results"]
    assert iresults.shape[0] == obs_df.shape[0]

    dv_name = os.path.join(ws, "gwt-p09-mf6.ucn")
    results = helpers.mod2obs_mf6(
        grb_name, dv_name, obs_csv, 31, start_datetime, 1, "concentration"
    )
    iresults = results["interpolated_results"]
    assert iresults.shape[0] == obs_df.shape[0]


def test_mf6_mod2obs_after_failed_call(tmp_path):
    """A failed call must not leave the grid installed and block later calls."""
    import shutil
    from pathlib import Path

    import pytest

    import pypestutils.helpers as helpers
    from pypestutils.pestutilslib import PestUtilsLibError

    ws = Path(__file__).parent / "data" / "p09"
    case = "gwf-p09-mf6"
    grb_name = ws / f"{case}.dis.grb"
    dv_name = tmp_path / f"{case}.hds"  # outputs are written next to this file
    shutil.copyfile(ws / f"{case}.hds", dv_name)
    start_datetime = pd.to_datetime("1-1-2000")
    obs_df = pd.DataFrame(
        {
            "site": ["site1"],
            "x": [101.0],
            "y": [221.0],
            "layer": [1],
            "datetime": [start_datetime + pd.to_timedelta(150, unit="d")],
        }
    )

    bad_df = obs_df.copy()
    bad_df["layer"] = 99  # more than NLAY: calc_mf6_interp_factors fails
    with pytest.raises(PestUtilsLibError, match="NLAY"):
        helpers.mod2obs_mf6(str(grb_name), str(dv_name), bad_df, 31, start_datetime)

    results = helpers.mod2obs_mf6(
        str(grb_name), str(dv_name), obs_df, 31, start_datetime
    )
    assert results["interpolated_results"].shape[0] == 1


def test_generate_2d_grid_field_from_z():
    import pytest

    import pypestutils.helpers as helpers
    from pypestutils.pestutilslib import PestUtilsLib

    grb_name = os.path.join("tests", "data", "p09", "gwf-p09-mf6.dis.grb")
    nrow, ncol = 18, 14
    nnodes = nrow * ncol
    kwargs = dict(mean=10.0, variance=0.5, variorange=500.0, variotransform="log")

    # all z values zero gives the mean
    field = helpers.generate_2d_grid_field_from_z(np.zeros(nnodes), grb_name, **kwargs)
    assert field.shape == (nrow, ncol)
    np.testing.assert_allclose(field, 10.0)

    # matches the low-level function
    rng = np.random.default_rng(0)
    z = rng.standard_normal(nnodes)
    field = helpers.generate_2d_grid_field_from_z(z, grb_name, **kwargs)
    grid_info = helpers.get_2d_grid_info_from_file(grb_name)
    expected = PestUtilsLib().fieldgen2d_sva_z(
        grid_info["x"].flatten(),
        grid_info["y"].flatten(),
        1.0,
        1,
        10.0,
        0.5,
        500.0,
        1.0,
        0.0,
        "log",
        "exp",
        1.0,
        z,
    )[:, 0]
    np.testing.assert_allclose(field.flatten(), expected)

    # grid-shaped array, Series and DataFrame inputs give the same field
    for zin in [
        z.reshape(nrow, ncol),
        pd.Series(z),
        pd.DataFrame({"z": z}),
        pd.DataFrame({"node": np.arange(nnodes)[::-1], "z": z[::-1]}),
    ]:
        np.testing.assert_allclose(
            helpers.generate_2d_grid_field_from_z(zin, grb_name, **kwargs), field
        )

    # nodes missing from a DataFrame with a "node" column have z of zero
    zdf = pd.DataFrame({"node": [0, 100], "z": [z[0], z[100]]})
    zfull = np.zeros(nnodes)
    zfull[[0, 100]] = z[[0, 100]]
    np.testing.assert_allclose(
        helpers.generate_2d_grid_field_from_z(zdf, grb_name, **kwargs),
        helpers.generate_2d_grid_field_from_z(zfull, grb_name, **kwargs),
    )

    # inactive nodes are zero
    zone = np.ones((nrow, ncol), dtype=int)
    zone[0, :] = 0
    field = helpers.generate_2d_grid_field_from_z(
        z, grb_name, zone_array=zone, **kwargs
    )
    assert np.all(field[0, :] == 0.0)
    assert np.all(field[1:, :] > 0.0)

    with pytest.raises(ValueError, match="expected 252"):
        helpers.generate_2d_grid_field_from_z(z[:-1], grb_name, **kwargs)
    with pytest.raises(ValueError, match="'z' column"):
        helpers.generate_2d_grid_field_from_z(
            pd.DataFrame({"value": z}), grb_name, **kwargs
        )
    with pytest.raises(ValueError, match="'node'"):
        helpers.generate_2d_grid_field_from_z(
            pd.DataFrame({"node": [nnodes], "z": [1.0]}), grb_name, **kwargs
        )


def test_generate_2d_grid_field_from_z_disv():
    import pypestutils.helpers as helpers

    grb_name = os.path.join("tests", "data", "vdl.disv.grb")
    z = np.random.default_rng(1).standard_normal(2751)
    field = helpers.generate_2d_grid_field_from_z(z, grb_name, variorange=1000.0)
    assert field.shape == (2751,)
    assert np.all(np.isfinite(field))


def test_generate_3d_grid_field_from_z():
    import pytest

    import pypestutils.helpers as helpers
    from pypestutils.pestutilslib import PestUtilsLib

    # MODFLOW-6 DIS grid: field is returned as NLAY X NROW X NCOL
    grb_name = os.path.join("tests", "data", "p09", "gwf-p09-mf6.dis.grb")
    field = helpers.generate_3d_grid_field_from_z(
        np.zeros(252), grb_name, mean=3.0, variorange_hmax=500.0
    )
    assert field.shape == (1, 18, 14)
    np.testing.assert_allclose(field, 3.0)

    # grid info dataframe for a 3 layer grid; field is returned as 1-D
    nlay, nrow, ncol = 3, 8, 10
    zz, yy, xx = np.meshgrid(
        -5.0 - 10.0 * np.arange(nlay),
        10.0 * np.arange(nrow)[::-1],
        10.0 * np.arange(ncol),
        indexing="ij",
    )
    grid_df = pd.DataFrame(
        {"x": xx.flatten(), "y": yy.flatten(), "z": zz.flatten(), "area": 100.0}
    )
    grid_df["height"] = 10.0
    nnodes = grid_df.shape[0]
    z = np.random.default_rng(3).standard_normal(nnodes)
    kwargs = dict(
        mean=5.0,
        variance=0.2,
        variorange_hmax=40.0,
        variorange_hmin=20.0,
        variorange_vert=15.0,
        variotransform="log",
        variobearing=30.0,
    )
    field = helpers.generate_3d_grid_field_from_z(z, grid_df, **kwargs)
    assert field.shape == (nnodes,)
    expected = PestUtilsLib().fieldgen3d_sva_z(
        grid_df.x.values,
        grid_df.y.values,
        grid_df.z.values,
        100.0,
        10.0,
        1,
        5.0,
        0.2,
        40.0,
        20.0,
        15.0,
        30.0,
        0.0,
        0.0,
        "log",
        "exp",
        1.0,
        z,
    )[:, 0]
    np.testing.assert_allclose(field, expected)

    # grid-shaped and DataFrame z inputs give the same field
    np.testing.assert_allclose(
        helpers.generate_3d_grid_field_from_z(
            z.reshape(nlay, nrow, ncol), grid_df, **kwargs
        ),
        field,
    )
    np.testing.assert_allclose(
        helpers.generate_3d_grid_field_from_z(
            pd.DataFrame({"node": np.arange(nnodes), "z": z}), grid_df, **kwargs
        ),
        field,
    )

    # inactive nodes (e.g. IDOMAIN of zero) are zero
    idomain = np.ones((nlay, nrow, ncol), dtype=int)
    idomain[2] = 0
    field = helpers.generate_3d_grid_field_from_z(
        z, grid_df, zone_array=idomain, **kwargs
    ).reshape(nlay, nrow, ncol)
    assert np.all(field[2] == 0.0)
    assert np.all(field[:2] > 0.0)

    with pytest.raises(Exception, match="'z' column"):
        helpers.generate_3d_grid_field_from_z(z, grid_df.drop(columns="z"))


if __name__ == "__main__":
    import sys

    sys.path.insert(0, os.path.join(".."))
    test_mf6_mod2obs()
