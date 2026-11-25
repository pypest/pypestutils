"""Tests for controlling the pypestutils logger."""

import logging
from pathlib import Path

import pytest

from pypestutils import helpers
from pypestutils.logger import (
    DEFAULT_LEVEL,
    LOGGER_NAME,
    get_logger,
    set_logger_level,
    temporary_logger_level,
)
from pypestutils.pestutilslib import PestUtilsLib

grb_fname = str(Path(__file__).parent / "data" / "p09" / "gwf-p09-mf6.dis.grb")


class _Records(logging.Handler):
    def __init__(self):
        super().__init__()
        self.records = []

    def emit(self, record):
        self.records.append(record)


@pytest.fixture
def records():
    """Collect pypestutils log records; restore the default level afterwards."""
    logger = get_logger(LOGGER_NAME)
    set_logger_level(DEFAULT_LEVEL)
    handler = _Records()
    logger.addHandler(handler)
    yield handler.records
    logger.removeHandler(handler)
    set_logger_level(DEFAULT_LEVEL)


def test_default_level_is_warning(records):
    assert DEFAULT_LEVEL == logging.WARNING
    PestUtilsLib().initialize_randgen(1)
    helpers.get_grid_info_from_mf6_grb(grb_fname)
    assert logging.getLogger(LOGGER_NAME).level == logging.WARNING
    assert records == []


def test_set_logger_level_not_reset_by_new_instances(records):
    set_logger_level("INFO")
    lib = PestUtilsLib()  # default logger_level=None leaves the level alone
    lib.initialize_randgen(1)
    n = len(records)
    assert n > 0
    helpers.get_grid_info_from_mf6_grb(grb_fname)
    assert len(records) > n
    assert logging.getLogger(LOGGER_NAME).level == logging.INFO


def test_instance_logger_level(records):
    lib = PestUtilsLib(logger_level="INFO")
    lib.initialize_randgen(1)
    assert len(records) == 1
    PestUtilsLib(logger_level=logging.WARNING)
    lib.initialize_randgen(1)
    assert len(records) == 1


def test_temporary_logger_level(records):
    lib = PestUtilsLib()
    with temporary_logger_level("INFO"):
        lib.initialize_randgen(1)
    assert len(records) == 1
    assert logging.getLogger(LOGGER_NAME).level == logging.WARNING
    with pytest.raises(RuntimeError):
        with temporary_logger_level("INFO"):
            raise RuntimeError("restore on error")
    assert logging.getLogger(LOGGER_NAME).level == logging.WARNING
    with temporary_logger_level(None):
        lib.initialize_randgen(1)
    assert len(records) == 1


def test_helper_logger_level(records):
    data = helpers.get_grid_info_from_mf6_grb(grb_fname, logger_level="INFO")
    assert data["ncells"] == 252
    n = len(records)
    assert n > 0
    # the level only applies during the call
    assert logging.getLogger(LOGGER_NAME).level == logging.WARNING
    helpers.get_grid_info_from_mf6_grb(grb_fname)
    assert len(records) == n
    helpers.get_2d_grid_info_from_file(grb_fname, None, "INFO")  # positional
    assert len(records) > n
    # a helper level overrides the session level for that call
    set_logger_level("INFO")
    n = len(records)
    helpers.get_grid_info_from_mf6_grb(grb_fname, logger_level="WARNING")
    assert len(records) == n
    assert logging.getLogger(LOGGER_NAME).level == logging.INFO
