"""Core library instance."""
from __future__ import annotations

import logging
from ctypes import byref, c_double, c_int, create_string_buffer
from os import PathLike
from pathlib import Path

import numpy as np
import numpy.typing as npt

from .ctypes_declarations import get_char_array, get_dimvar_int, prototype
from .finder import load
from .logger import get_logger

_dimvar_cache = {}


class PestUtilsLibException(BaseException):
    """Exception from PestUtilsLib."""

    pass


class PestUtilsLib:
    """Mid-level Fortran-Python handler for pestutils library via ctypes.

    Parameters
    ----------
    logger_level : int, str, default 20 (INFO)
    """

    def __init__(self, *, logger_level=logging.INFO):
        self.logger = get_logger(self.__class__.__name__, logger_level)
        self.lib = load()
        self.logger.debug("loaded %s", self.lib)
        prototype(self.lib)
        self.logger.debug("added prototypes")

    def __del__(self):
        try:
            self.logger.debug("calling free_all_memory()")
            self.lib.free_all_memory()
        except AttributeError as err:
            if hasattr(self, "logger"):
                self.logger.warning("%s", err)

    def get_char_array(self, name: str):
        """Get c_char Array with a fixed size from dimvar."""
        return get_char_array(self.lib, name)

    def create_char_array(self, init: str | bytes, name: str):
        """Create c_char Array with a fixed size from dimvar and intial value."""
        if isinstance(init, str):
            init = init.encode()
        elif isinstance(init, bytes):
            pass
        else:
            raise TypeError(f"expecting either str or bytes; found {type(init)}")
        size = self.get_dimvar_int(name)
        return create_string_buffer(init, size)

    def get_dimvar_int(self, name: str):
        """Get dimvar constant integer from library instance."""
        return get_dimvar_int(self.lib, name)

    def inquire_modflow_binary_file_specs(
        self, FileIn: str | PathLike, FileOut: str | PathLike, isim: int, itype: int
    ) -> dict:
        """Report some of the details of a MODFLOW-written binary file.

        Parameters
        ----------
        FileIn : str or PathLike
        FileOut : str or PathLike
        isim : int
            Simulator.
        itype : int
            Where 1 = system state; 2 = flows.

        Returns
        -------
        dict
            With keys: ["iprec", "narray", "ntime"]
        """
        FileIn = Path(FileIn)
        if not FileIn.is_file():
            raise FileNotFoundError(f"could not find FileIn {FileIn}")
        FileOut = Path(FileOut)  # TODO
        iprec = c_int()
        narray = c_int()
        ntime = c_int()
        res = self.lib.inquire_modflow_binary_file_specs(
            byref(self.create_char_array(bytes(FileIn), "LENFILENAME")),
            byref(self.create_char_array(bytes(FileOut), "LENFILENAME")),
            byref(c_int(isim)),
            byref(c_int(itype)),
            byref(iprec),
            byref(narray),
            byref(ntime),
        )
        if res != 0:
            raise PestUtilsLibException(self.retrieve_error_message())
        self.logger.info("inquired modflow binary file specs from %r", FileIn.name)
        return {
            "iprec": iprec.value,
            "narray": narray.value,
            "ntime": ntime.value,
        }

    def retrieve_error_message(self) -> str:
        """Retrieve error message from library.

        Returns
        -------
        str
        """
        charray = self.get_char_array("LENMESSAGE")()
        res = self.lib.retrieve_error_message(byref(charray))
        return charray[:res].rstrip(b"\x00").decode()

    def install_structured_grid(
        self,
        gridname: str,
        ncol: int,
        nrow: int,
        nlay: int,
        icorner: int,
        e0: float,
        n0: float,
        rotation: float,
        delr: float | npt.NDArray[np.float64],
        delc: float | npt.NDArray[np.float64],
    ) -> None:
        """Install specifications for a structured grid."""
        delr = np.array(delr)
        if delr.ndim == 0:
            delr = np.repeat(delr, ncol)
        elif delr.shape != (ncol,):
            raise ValueError(f"expected 'delr' array with shape {(ncol,)}")
        delc = np.array(delc)
        if delc.ndim == 0:
            delc = np.repeat(delc, nrow)
        elif delc.shape != (nrow,):
            raise ValueError(f"expected 'delc' array with shape {(nrow,)}")
        res = self.lib.install_structured_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME")),
            byref(c_int(ncol)),
            byref(c_int(nrow)),
            byref(c_int(nlay)),
            byref(c_int(icorner)),
            byref(c_double(e0)),
            byref(c_double(n0)),
            byref(c_double(rotation)),
            delr.astype(np.float64),
            delc.astype(np.float64),
        )
        if res != 0:
            raise PestUtilsLibException(self.retrieve_error_message())
        self.logger.info("installed strictured grid %r from specs", gridname)

    def uninstall_structured_grid(self, gridname: str) -> None:
        """Uninstall strictured grid set by :meth:`install_structured_grid`.

        Parameters
        ----------
        gridname : str
            Unique non-blank grid name.
        """
        res = self.lib.uninstall_structured_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME"))
        )
        if res != 0:
            raise PestUtilsLibException(self.retrieve_error_message())
        self.logger.info("uninstalled strictured grid %r", gridname)

    def install_mf6_grid_from_file(
        self, gridname: str, grbfile: str | PathLike
    ) -> dict:
        """Install specifications for a MF6 grid from a GRB file.

        Parameters
        ----------
        gridname : str
            Unique non-blank grid name.
        grbfile : str or PathLike
            Path to a GRB binary grid file.

        Returns
        -------
        idis : int
        ncells : int
        ndim1 : int
        ndim3 : int
        """
        grbfile = Path(grbfile)
        if not grbfile.is_file():
            raise FileNotFoundError(f"could not find grbfile {grbfile}")
        idis = c_int()
        ncells = c_int()
        ndim1 = c_int()
        ndim2 = c_int()
        ndim3 = c_int()
        res = self.lib.install_mf6_grid_from_file(
            byref(self.create_char_array(gridname, "LENGRIDNAME")),
            byref(self.create_char_array(bytes(grbfile), "LENFILENAME")),
            byref(idis),
            byref(ncells),
            byref(ndim1),
            byref(ndim2),
            byref(ndim3),
        )
        if res != 0:
            raise PestUtilsLibException(self.retrieve_error_message())
        self.logger.info(
            "installed mf6 grid %r from grbfile=%r", gridname, grbfile.name
        )
        return {
            "idis": idis.value,
            "ncells": ncells.value,
            "ndim1": ndim1.value,
            "ndim2": ndim2.value,
            "ndim3": ndim3.value,
        }

    def uninstall_mf6_grid(self, gridname: str) -> None:
        """Uninstall MF6 grid set by :meth:`install_mf6_grid_from_file`.

        Parameters
        ----------
        gridname : str
            Unique non-blank grid name.
        """
        res = self.lib.uninstall_mf6_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME"))
        )
        if res != 0:
            raise PestUtilsLibException(self.retrieve_error_message())
        self.logger.info("uninstalled mf6 grid %r", gridname)

    def calc_mf6_interp_factors(
        self,
        gridname: str,
        ecoord: npt.NDArray[np.float64],
        ncoord: npt.NDArray[np.float64],
        layer: npt.NDArray[np.int32],
        factorfile: str | PathLike,
        factorfiletype: int,
        blnfile: str | PathLike,
    ) -> npt.NDArray[np.int32]:
        """Calculate interpolation factors from a MODFLOW 6 DIS or DISV."""
        c_gridname = self.create_char_array(gridname, "LENGRIDNAME")
        factorfile = Path(factorfile)  # TODO
        blnfile = Path(blnfile)  # TODO
        if layer.ndim != 1:
            raise ValueError("expected 'layer' to have ndim=1")
        npts = len(layer)
        expected_shape = layer.shape
        if npts <= 0:
            raise ValueError("expected 'layer' with length greater than zero")
        elif not np.issubdtype(layer.dtype, np.integer):
            raise ValueError("expected 'layer' to be integer type")
        elif ecoord.shape != expected_shape:
            raise ValueError(f"expected 'ecoord' shape to be {expected_shape}")
        elif ncoord.shape != expected_shape:
            raise ValueError(f"expected 'ncoord' shape to be {expected_shape}")
        interp_success = np.zeros(npt, np.int32)
        res = self.lib.calc_mf6_interp_factors(
            byref(c_gridname),
            byref(c_int(npts)),
            ecoord.astype(np.float64),
            ncoord.astype(np.float64),
            layer.astype(np.int32),
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(self.create_char_array(bytes(blnfile), "LENFILENAME")),
            interp_success,
        )
        if res != 0:
            raise PestUtilsLibException(self.retrieve_error_message())
        self.logger.info("calculated mf6 interp factors for %r", gridname)
        return interp_success
