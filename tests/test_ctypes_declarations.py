"""Tests for ctypes_declarations module."""
from ctypes import c_int

import pytest

from pypestutils.ctypes_declarations import get_char_array, get_dimvar_int, prototype
from pypestutils.finder import load

from .common import pestutils_function_names


def test_get_dimvar_int():
    lib = load()
    for name in ["LENVARTYPE", "LENFILENAME"]:
        res = get_dimvar_int(lib, name)
        assert isinstance(res, int)
        assert res > 0

    with pytest.raises(ValueError):
        get_dimvar_int(lib, "lenfilename")  # case-sensitive


def test_get_char_array():
    lib = load()
    filename_t = get_char_array(lib, "LENFILENAME")
    assert filename_t()


def test_prototype():
    lib = load()

    # not really necessary, but check they don't have a prototype after loading
    for function_name in pestutils_function_names.keys():
        obj = getattr(lib, function_name)
        assert obj.argtypes is None, function_name

    prototype(lib)

    # everything should be prototyped
    for function_name, num_args in pestutils_function_names.items():
        obj = getattr(lib, function_name)
        assert isinstance(obj.argtypes, tuple), function_name
        assert len(obj.argtypes) == num_args, function_name
        assert obj.restype is c_int, function_name
