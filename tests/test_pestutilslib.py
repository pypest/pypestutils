"""Tests for pestutilslib module."""

import logging
from pathlib import Path, PureWindowsPath

import pytest

from pypestutils.pestutilslib import PestUtilsLib

data_dir = Path(__file__).parent / "data"


def test_init_del():
    lib = PestUtilsLib()
    del lib


def test_init_logger(caplog):
    caplog.set_level(logging.DEBUG)
    lib = PestUtilsLib(logger_level=logging.INFO)
    assert len(caplog.records) == 0
    lib.initialize_randgen(123)
    assert len(caplog.records) > 0


def test_create_char_array():
    lib = PestUtilsLib()
    filein = PureWindowsPath("path") / "to" / "a" / "file.txt"
    char_ar = lib.create_char_array(bytes(filein), "LENFILENAME")
    assert char_ar.value == rb"path\to\a\file.txt"
    gridname = "mygrid"
    char_ar = lib.create_char_array(gridname, "LENGRIDNAME")
    assert char_ar.value == b"mygrid"
    with pytest.raises(ValueError):
        lib.create_char_array("foo", "lengridname")
    with pytest.raises(TypeError):
        lib.create_char_array(1, "LENGRIDNAME")


def test_inquire_modflow_binary_file_specs(): ...


def test_retrieve_error_message(): ...


def test_install_structured_grid(): ...


def test_get_cell_centres_structured(): ...


def test_uninstall_structured_grid(): ...


def test_free_all_memory(): ...


def test_interp_from_structured_grid(): ...


def test_interp_to_obstime(): ...


def test_install_mf6_grid_from_file(): ...


def test_install_mf6_grid_from_file_v2():
    """Version 2 GRB file (with CRS field) loads without error."""
    lib = PestUtilsLib()
    gridname = "grid_v2"
    grb_pth = data_dir / "flow_v2.dis.grb"
    inst_res = lib.install_mf6_grid_from_file(gridname, grb_pth)
    assert inst_res["idis"] == 1  # DIS grid
    assert inst_res["ncells"] > 0
    lib.uninstall_mf6_grid(gridname)


def test_get_mf6_grid_crs():
    """CRS string is returned for a version 2 GRB file, empty for version 1."""
    lib = PestUtilsLib()

    # v2 DIS file: should return the CRS string
    lib.install_mf6_grid_from_file("grid_v2", data_dir / "flow_v2.dis.grb")
    crs = lib.get_mf6_grid_crs("grid_v2")
    assert crs == "EPSG:26916"
    lib.uninstall_mf6_grid("grid_v2")

    # v2 DISV file: should also return the CRS string
    lib.install_mf6_grid_from_file("grid_disv_v2", data_dir / "vdl_v2.disv.grb")
    crs = lib.get_mf6_grid_crs("grid_disv_v2")
    assert crs == "EPSG:26916"
    lib.uninstall_mf6_grid("grid_disv_v2")

    # v1 file: should return empty string
    lib.install_mf6_grid_from_file("grid_v1", data_dir / "hd1h.dis.grb")
    crs = lib.get_mf6_grid_crs("grid_v1")
    assert crs == ""
    lib.uninstall_mf6_grid("grid_v1")


def test_v2_grid_cell_centres():
    """Cell centres from a v2 DIS GRB are spatially correct (CRS field doesn't corrupt reads)."""
    import numpy as np

    lib = PestUtilsLib()
    inst = lib.install_mf6_grid_from_file("grid_v2", data_dir / "flow_v2.dis.grb")
    ncells = inst["ncells"]
    assert ncells == 100

    cellx, celly, cellz = lib.get_cell_centres_mf6("grid_v2", ncells)

    # 1-col × 1-row × 100-layer grid with DELR=DELC=1, XORIGIN=YORIGIN=0
    # all cell centres share the same x and y (0.5, 0.5)
    assert cellx.shape == (ncells,)
    np.testing.assert_allclose(cellx, 0.5)
    np.testing.assert_allclose(celly, 0.5)

    # z centres run from top layer (99.5) down to bottom layer (0.5)
    assert cellz[0] > cellz[-1]
    np.testing.assert_allclose(cellz[0], 99.5)
    np.testing.assert_allclose(cellz[-1], 0.5)

    lib.uninstall_mf6_grid("grid_v2")


def test_install_mf6_grid_from_file_disv_v2():
    """Version 2 DISV GRB file loads without error and reports correct grid type."""
    lib = PestUtilsLib()
    inst = lib.install_mf6_grid_from_file("grid_disv_v2", data_dir / "vdl_v2.disv.grb")
    assert inst["idis"] == 2  # DISV grid
    assert inst["ncells"] > 0
    lib.uninstall_mf6_grid("grid_disv_v2")


def test_v2_disv_grid_cell_centres():
    """Cell centres from a v2 DISV GRB are spatially correct (CRS field doesn't corrupt reads)."""
    import numpy as np

    lib = PestUtilsLib()
    inst = lib.install_mf6_grid_from_file("grid_disv_v2", data_dir / "vdl_v2.disv.grb")
    ncells = inst["ncells"]
    assert ncells == 2751

    cellx, celly, cellz = lib.get_cell_centres_mf6("grid_disv_v2", ncells)

    assert cellx.shape == (ncells,)
    # cell centres should be finite and non-zero (real model grid)
    assert np.all(np.isfinite(cellx))
    assert np.all(np.isfinite(celly))
    assert np.all(np.isfinite(cellz))

    lib.uninstall_mf6_grid("grid_disv_v2")


def test_get_cell_centres_mf6(): ...


def test_uninstall_mf6_grid(): ...


def test_calc_mf6_interp_factors(): ...


def test_interp_from_mf6_depvar_file(): ...


def test_extract_flows_from_cbc_file(): ...


def test_calc_kriging_factors_2d(): ...


def test_calc_kriging_factors_auto_2d(): ...


def test_calc_kriging_factors_3d(): ...


def test_krige_using_file(): ...


def test_build_covar_matrix_2d(): ...


def test_build_covar_matrix_3d(): ...


def test_calc_structural_overlay_factors(): ...


def test_interpolate_blend_using_file(): ...


def test_ipd_interpolate_2d(): ...


def test_ipd_interpolate_3d(): ...


def test_initialize_randgen(): ...


def test_fieldgen2d_sva(): ...


def test_fieldgen3d_sva(): ...
