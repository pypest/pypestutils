"""Tests for core module."""
import logging
from pathlib import PureWindowsPath

import pytest

from pypestutils.core import PestUtilsLib, PestUtilsLibError


def test_init_del():
    lib = PestUtilsLib()
    del lib


def test_init_logger(caplog):
    caplog.set_level(logging.DEBUG)
    lib = PestUtilsLib(logger_level=logging.INFO)
    assert len(caplog.records) == 0
    del lib
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


def test_inquire_modflow_binary_file_specs():
    ...


def test_retrieve_error_message():
    ...


def test_install_structured_grid():
    ...


def test_uninstall_structured_grid():
    ...


def test_free_all_memory():
    ...


def test_interp_from_structured_grid():
    ...


def test_interp_to_obstime():
    ...


def test_install_mf6_grid_from_file():
    ...


def test_uninstall_mf6_grid():
    ...


def test_calc_mf6_interp_factors():
    ...


def test_interp_from_mf6_depvar_file():
    ...


def test_extract_flows_from_cbc_file():
    ...


def test_calc_kriging_factors_2d():
    ...


def test_calc_kriging_factors_auto_2d():
    ...


def test_calc_kriging_factors_3d():
    ...


def test_krige_using_file():
    ...


def test_build_covar_matrix_2d():
    ...


def test_build_covar_matrix_3d():
    ...


def test_calc_structural_overlay_factors():
    ...


def test_interpolate_blend_using_file():
    ...


def test_ipd_interpolate_2d():
    ...


def test_ipd_interpolate_3d():
    ...


def test_initialize_randgen():
    ...


def test_fieldgen2d_sva():
    ...


def test_fieldgen3d_sva():
    ...
