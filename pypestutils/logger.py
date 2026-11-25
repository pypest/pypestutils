"""Logger module.

All pypestutils messages go through one logger, named "PestUtilsLib", which
writes to stdout.  Its level controls how much is echoed:

* ``set_logger_level(level)`` sets it for the rest of the session,
* ``temporary_logger_level(level)`` sets it within a ``with`` block,
* ``PestUtilsLib(logger_level=level)`` sets it when creating a library instance,
* the ``logger_level`` argument of the helper functions sets it for that call.

The default level is "WARNING", so routine messages are not echoed.  Use
"INFO" (or ``logging.INFO``) to echo every library call.
"""

from __future__ import annotations

__all__ = [
    "LOGGER_NAME",
    "get_logger",
    "set_logger_level",
    "temporary_logger_level",
]

import logging
import sys
from collections.abc import Iterator
from contextlib import contextmanager
from logging import Logger

LOGGER_NAME = "PestUtilsLib"
DEFAULT_LEVEL = logging.WARNING

_configured: set[str] = set()


def get_logger(name: str = LOGGER_NAME, level: str | int | None = None) -> Logger:
    """Return a named logger.

    The first time a logger is requested, a stdout handler is added (if it has
    no handlers) and its level is set to "WARNING", unless `level` is given.

    Parameters
    ----------
    name : str, optional
        Logger name, default "PestUtilsLib".
    level : str, int or None, optional
        Logger level: "DEBUG" (10), "INFO" (20), "WARNING" (30), "ERROR" (40)
        or "CRITICAL" (50).  Default None leaves the current level unchanged.

    Returns
    -------
    Logger
    """
    logger = logging.getLogger(name)
    if name not in _configured:
        _configured.add(name)
        if not logger.hasHandlers():
            formatter = logging.Formatter("%(levelname)s:%(name)s: %(message)s")
            handler = logging.StreamHandler(sys.stdout)
            handler.setFormatter(formatter)
            logger.addHandler(handler)
        if level is None:
            level = DEFAULT_LEVEL
    if level is not None:
        logger.setLevel(level)
    return logger


def set_logger_level(level: str | int) -> None:
    """Set the level of the pypestutils logger.

    This applies to all library instances and helper functions, until it is
    changed again.

    Parameters
    ----------
    level : str or int
        Logger level: "DEBUG" (10), "INFO" (20), "WARNING" (30), "ERROR" (40)
        or "CRITICAL" (50).  Use "INFO" to echo every library call, or
        "WARNING" (the default) to silence routine messages.
    """
    get_logger(LOGGER_NAME, level)


@contextmanager
def temporary_logger_level(level: str | int | None) -> Iterator[None]:
    """Set the level of the pypestutils logger within a ``with`` block.

    The previous level is restored on exit.

    Parameters
    ----------
    level : str, int or None
        Logger level: "DEBUG" (10), "INFO" (20), "WARNING" (30), "ERROR" (40)
        or "CRITICAL" (50).  None leaves the level unchanged.

    Examples
    --------
    >>> from pypestutils.logger import temporary_logger_level
    >>> with temporary_logger_level("INFO"):
    ...     pass  # pypestutils calls here echo each library call
    """
    logger = get_logger(LOGGER_NAME)
    previous = logger.level
    if level is not None:
        logger.setLevel(level)
    try:
        yield
    finally:
        logger.setLevel(previous)
