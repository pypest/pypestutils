"""Logger module."""
from __future__ import annotations

__all__ = ["get_logger"]

import logging
import sys
from logging import Logger


def get_logger(name: str, level: str | int = 0) -> Logger:
    """Return a named logger.

    Parameters
    ----------
    name : str
        Logger name.
    logger_level : str, int, optional
        Logger level, default 0 ("NOTSET"). Accepted values are
        "DEBUG" (10), "INFO" (20), "WARNING" (30), "ERROR" (40) or
        "CRITICAL" (50).

    Returns
    -------
    Logger
    """
    logger = logging.getLogger(name)
    logger.setLevel(level)

    if not logger.hasHandlers():
        formatter = logging.Formatter("%(levelname)s:%(name)s: %(message)s")
        handler = logging.StreamHandler(sys.stdout)
        handler.setFormatter(formatter)
        logger.addHandler(handler)

    return logger
