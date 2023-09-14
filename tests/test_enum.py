"""Tests for enum module."""
import pytest

from pypestutils.enum import KrigType


def test_get_valid_options():
    assert KrigType.get_valid_options() == {0: "simple", 1: "ordinary"}


def test_get_value():
    assert KrigType.get_value("simple") == 0
    assert KrigType.get_value("ordinary") == 1

    with pytest.raises(ValueError, match="'other' is not a valid option"):
        KrigType.get_value("other")
    with pytest.raises(ValueError, match="'1' is not a valid option"):
        KrigType.get_value(1)
