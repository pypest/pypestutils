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


class PestUtilsLibError(BaseException):
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
        """Clean-up library instance."""
        try:
            self.free_all_memory()
        except (AttributeError, PestUtilsLibError) as err:
            if hasattr(self, "logger"):
                self.logger.warning("cannot call __del__: %s", err)

    def get_char_array(self, name: str):
        """Get c_char Array with a fixed size from dimvar."""
        return get_char_array(self.lib, name)

    def create_char_array(self, init: str | bytes, name: str):
        """Create c_char Array with a fixed size from dimvar and intial value.

        Parameters
        ----------
        init : str or bytes
            Initial value.
        name : str
            Variable length name, e.g. LENFILENAME or LENVARTYPE.
        """
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
        self, filein: str | PathLike, fileout: str | PathLike, isim: int, itype: int
    ) -> dict:
        """Report some of the details of a MODFLOW-written binary file.

        Parameters
        ----------
        filein : str or PathLike
        fileout : str or PathLike
        isim : int
            Simulator.
        itype : int
            Where 1 = system state; 2 = flows.

        Returns
        -------
        iprec : int
            Where 1 = single; 2 = double.
        narray : int
            Number of arrays.
        ntime : int
            Number of times.
        """
        filein = Path(filein)
        if not filein.is_file():
            raise FileNotFoundError(f"could not find filein {filein}")
        fileout = Path(fileout)  # TODO
        iprec = c_int()
        narray = c_int()
        ntime = c_int()
        res = self.lib.inquire_modflow_binary_file_specs(
            byref(self.create_char_array(bytes(filein), "LENFILENAME")),
            byref(self.create_char_array(bytes(fileout), "LENFILENAME")),
            byref(c_int(isim)),
            byref(c_int(itype)),
            byref(iprec),
            byref(narray),
            byref(ntime),
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("inquired modflow binary file specs from %r", filein.name)
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
            raise PestUtilsLibError(self.retrieve_error_message())
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
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("uninstalled strictured grid %r", gridname)

    def free_all_memory(self) -> None:
        """Deallocate all memory that is being used."""
        ret = self.lib.free_all_memory()
        if ret != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("all memory was freed up")

    def _check_interp_arrays(
        self,
        ecoord: npt.NDArray[np.float64],
        ncoord: npt.NDArray[np.float64],
        layer: npt.NDArray[np.int32],
    ) -> int:
        """Check point interpolation arrays before passing to Fortran.

        Returns
        -------
        int
            npts
        """
        if layer.ndim != 1:
            raise ValueError("expected 'layer' to have ndim=1")
        npts = len(layer)
        expected_shape = layer.shape
        if npts <= 0:
            raise ValueError("expected 'layer' with length greater than zero")
        elif not np.issubdtype(layer.dtype, np.integer):
            raise ValueError(
                f"expected 'layer' to be integer type; found {layer.dtype}"
            )
        elif ecoord.shape != expected_shape:
            raise ValueError(f"expected 'ecoord' shape to be {expected_shape}")
        elif ncoord.shape != expected_shape:
            raise ValueError(f"expected 'ncoord' shape to be {expected_shape}")
        return npts

    def interp_from_structured_grid(
        self,
        gridname: str,
        depvarfile: str | PathLike,
        isim: int,
        iprec: int,
        ntime: int,
        vartype: str,
        interpthresh: float,
        nointerpval: float,
        ecoord: npt.NDArray[np.float64],
        ncoord: npt.NDArray[np.float64],
        layer: npt.NDArray[np.int32],
    ) -> dict:
        """Spatial interpolate points from a structured grid.

        Parameters
        ----------
        gridname : str
            Name of installed structured grid.
        depvarfile : str or PathLike
            Name of binary file to read.
        isim : int
            Specify -1 for MT3D; 1 for MODFLOW.
        iprec : int
            Specify -1 for MT3D; 1 for MODFLOW.
        ntime : int
            Number of output times.
        vartype : str
            Only read arrays of this type.
        interpthresh : float
            Absolute threshold for dry or inactive.
        nointerpval : float
            Value to use where interpolation is not possible.
        ecoord, ncoord : array_like
            X/Y or Easting/Northing coordinates for points.
        layer : array_like
            Layers of points.

        Returns
        -------
        nproctime : int
            Number of processed simulation times.
        simtime : np.ndarray
            Simulation time.
        simstate : np.ndarray
            Interpolated system state with dimensions (ntime,npts).
        """
        depvarfile = Path(depvarfile)
        if not depvarfile.is_file():
            raise FileNotFoundError(f"could not find depvarfile {depvarfile}")
        npts = self._check_interp_arrays(ecoord, ncoord, layer)
        simtime = np.zeros(ntime, np.float64)
        simstate = np.zeros((ntime, npts), np.float64, "F")
        c_nproctime = c_int()
        res = self.lib.interp_from_structured_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME")),
            byref(self.create_char_array(bytes(depvarfile), "LENFILENAME")),
            byref(c_int(isim)),
            byref(c_int(iprec)),
            byref(c_int(ntime)),
            byref(self.create_char_array(vartype, "LENVARTYPE")),
            byref(c_double(interpthresh)),
            byref(c_double(nointerpval)),
            byref(c_int(npts)),
            ecoord.astype(np.float64),
            ncoord.astype(np.float64),
            layer.astype(np.int32),
            byref(c_nproctime),
            simtime,
            simstate,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info(
            "interpolated %d points from structured grid %r", npts, gridname
        )
        return {
            "nproctime": c_nproctime.value,
            "simtime": simtime,
            "simstate": simstate,
        }

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
            raise PestUtilsLibError(self.retrieve_error_message())
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
            raise PestUtilsLibError(self.retrieve_error_message())
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
        factorfile = Path(factorfile)  # TODO
        blnfile = Path(blnfile)  # TODO
        npts = self._check_interp_arrays(ecoord, ncoord, layer)
        interp_success = np.zeros(npt, np.int32)
        res = self.lib.calc_mf6_interp_factors(
            byref(self.create_char_array(gridname, "LENGRIDNAME")),
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
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("calculated mf6 interp factors for %r", gridname)
        return interp_success
