"""Core library instance."""
from __future__ import annotations

import logging
from ctypes import byref, c_char, c_double, c_int, create_string_buffer
from os import PathLike
from pathlib import Path

import numpy as np
import numpy.typing as npt

from . import enum
from .data import ManyArrays, validate_scalar


class PestUtilsLibError(BaseException):
    """Exception from PestUtilsLib."""

    pass


class PestUtilsLib:
    """Mid-level Fortran-Python handler for pestutils library via ctypes.

    Parameters
    ----------
    logger_level : int, str, default 20 (INFO)
    """

    def __init__(self, *, logger_level=logging.INFO) -> None:
        from .ctypes_declarations import prototype
        from .finder import load
        from .logger import get_logger

        self.logger = get_logger(self.__class__.__name__, logger_level)
        self.pestutils = load()
        self.logger.debug("loaded %s", self.pestutils)
        prototype(self.pestutils)
        self.logger.debug("added prototypes")

    def __del__(self):
        """Clean-up library instance."""
        try:
            self.free_all_memory()
        except (AttributeError, PestUtilsLibError) as err:
            if hasattr(self, "logger"):
                self.logger.warning("cannot call __del__: %s", err)

    def create_char_array(self, init: str | bytes, name: str):
        """Create c_char Array with a fixed size from dimvar and intial value.

        Parameters
        ----------
        init : str or bytes
            Initial value.
        name : str
            Uppercase variable length name, e.g. LENFILENAME or LENVARTYPE.
        """
        from .ctypes_declarations import get_dimvar_int

        if isinstance(init, str):
            init = init.encode()
        elif isinstance(init, bytes):
            pass
        else:
            raise TypeError(f"expecting either str or bytes; found {type(init)}")
        size = get_dimvar_int(self.pestutils, name)
        return create_string_buffer(init, size)

    def inquire_modflow_binary_file_specs(
        self,
        filein: str | PathLike,
        fileout: str | PathLike | None,
        isim: int,
        itype: int,
    ) -> dict:
        """Report some of the details of a MODFLOW-written binary file.

        Parameters
        ----------
        filein : str or PathLike
            MODFLOW-generated binary file to be read.
        fileout : str, PathLike, None
            Output file with with table of array headers. Use None or "" for
            no output file.
        isim : int
            Inform the function the simulator that generated the binary file:

             * 1 = traditional MODFLOW
             * 21 = MODFLOW-USG with structured grid
             * 22 = MODFLOW-USG with unstructured grid
             * 31 = MODFLOW 6 with DIS grid
             * 32 = MODFLOW 6 with DISV grid
             * 33 = MODFLOW 6 with DISU grid

        itype : int
            Where 1 = system state or dependent variable;
            2 = cell-by-cell flows.

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
        if fileout:
            fileout = Path(fileout)
        validate_scalar("isim", isim, isin=[1, 21, 22, 31, 32, 33])
        validate_scalar("itype", itype, isin=[1, 2])
        iprec = c_int()
        narray = c_int()
        ntime = c_int()
        res = self.pestutils.inquire_modflow_binary_file_specs(
            byref(self.create_char_array(bytes(filein), "LENFILENAME")),
            byref(self.create_char_array(bytes(fileout) or b"", "LENFILENAME")),
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
        from .ctypes_declarations import get_char_array

        charray = get_char_array(self.pestutils, "LENMESSAGE")()
        res = self.pestutils.retrieve_error_message(byref(charray))
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
        delr: float | npt.ArrayLike,
        delc: float | npt.ArrayLike,
    ) -> None:
        """Install specifications for a structured grid.

        Parameters
        ----------
        gridname : str
            Unique non-blank grid name.
        ncol, nrow, nlay : int
            Grid dimensions.
        icorner : int
            Reference corner, use 1 for top left and 2 for bottom left.
        e0, n0 : float
            Reference offsets.
        rotation : float
            Grid rotation, counter-clockwise degrees.
        """
        validate_scalar("ncol", ncol, gt=0)
        validate_scalar("nrow", nrow, gt=0)
        validate_scalar("nlay", nlay, gt=0)
        col = ManyArrays(float_any={"delr": delr}, ar_len=ncol)
        row = ManyArrays(float_any={"delc": delc}, ar_len=nrow)
        col.validate("delr", gt=0.0)
        row.validate("delc", gt=0.0)
        validate_scalar("icorner", icorner, isin=[1, 2])
        res = self.pestutils.install_structured_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME")),
            byref(c_int(ncol)),
            byref(c_int(nrow)),
            byref(c_int(nlay)),
            byref(c_int(icorner)),
            byref(c_double(e0)),
            byref(c_double(n0)),
            byref(c_double(rotation)),
            col.delr,
            row.delc,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("installed structured grid %r from specs", gridname)

    def uninstall_structured_grid(self, gridname: str) -> None:
        """Uninstall structured grid set by :meth:`install_structured_grid`.

        Parameters
        ----------
        gridname : str
            Unique non-blank grid name.
        """
        res = self.pestutils.uninstall_structured_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME"))
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("uninstalled structured grid %r", gridname)

    def free_all_memory(self) -> None:
        """Deallocate all memory that is being used."""
        ret = self.pestutils.free_all_memory()
        if ret != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("all memory was freed up")

    def interp_from_structured_grid(
        self,
        gridname: str,
        depvarfile: str | PathLike,
        isim: int,
        iprec: int | str | enum.Prec,
        ntime: int,
        vartype: str,
        interpthresh: float,
        nointerpval: float,
        # npts: int,  # determined from layer.shape[0]
        ecoord: npt.ArrayLike,
        ncoord: npt.ArrayLike,
        layer: int | npt.ArrayLike,
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
        iprec : int, str or enum.Prec
            Specify 1 or "single", 2 or "double", or use enum.Prec.
        ntime : int
            Number of output times.
        vartype : str
            Only read arrays of this type.
        interpthresh : float
            Absolute threshold for dry or inactive.
        nointerpval : float
            Value to use where interpolation is not possible.
        ecoord, ncoord : array_like
            X/Y or Easting/Northing coordinates for points with shape (npts,).
        layer : int or array_like
            Layers of points with shape (npts,).

        Returns
        -------
        nproctime : int
            Number of processed simulation times.
        simtime : npt.NDArray[np.float64]
            Simulation times, with shape (ntime,).
        simstate : npt.NDArray[np.float64]
            Interpolated system states, with shape (ntime, npts).
        """
        depvarfile = Path(depvarfile)
        if not depvarfile.is_file():
            raise FileNotFoundError(f"could not find depvarfile {depvarfile}")
        if isinstance(iprec, str):
            iprec = enum.Prec.get_value(iprec)
        pts = ManyArrays({"ecoord": ecoord, "ncoord": ncoord}, int_any={"layer": layer})
        npts = len(pts)
        simtime = np.zeros(ntime, np.float64, order="F")
        simstate = np.zeros((ntime, npts), np.float64, order="F")
        nproctime = c_int()
        res = self.pestutils.interp_from_structured_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME")),
            byref(self.create_char_array(bytes(depvarfile), "LENFILENAME")),
            byref(c_int(isim)),
            byref(c_int(iprec)),
            byref(c_int(ntime)),
            byref(self.create_char_array(vartype, "LENVARTYPE")),
            byref(c_double(interpthresh)),
            byref(c_double(nointerpval)),
            byref(c_int(npts)),
            pts.ecoord,
            pts.ncoord,
            pts.layer,
            byref(nproctime),
            simtime,
            simstate,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info(
            "interpolated %d points from structured grid %r", npts, gridname
        )
        return {
            "nproctime": nproctime.value,
            "simtime": simtime,
            "simstate": simstate,
        }

    def interp_to_obstime(
        self,
        # nsimtime: int,  # determined from simval.shape[0]
        nproctime: int,
        # npts: int,  # determined from simval.shape[1]
        simtime: npt.ArrayLike,
        simval: npt.ArrayLike,
        interpthresh: float,
        how_extrap: str,
        time_extrap: float,
        nointerpval: float,
        # nobs: int,  # determined from obspoint.shape[0]
        obspoint: npt.ArrayLike,
        obstime: npt.ArrayLike,
    ) -> npt.NDArray[np.float64]:
        """Temporal interpolation for simulation times to observed times.

        Parameters
        ----------
        nproctime : int
            Number of times featured in simtime and simval.
        simtime : array_like
            1D array of simulation times with shape (nsimtime,).
        simval : array_like
            2D array of simulated values with shape (nsimtime, npts).
        interpthresh : float
            Values equal or above this in simval have no meaning.
        how_extrap : str
            Method, where 'L'=linear; 'C'=constant.
        time_extrap : float
            Permitted extrapolation time.
        nointerpval : float
            Value to use where interpolation is not possible.
        obspoint : array_like
            1D integer array of indices of observation points,
            which start at 0 and -1 means no index. Shape is (nobs,).
        obstime : array_like
            1D array of observation times with shape (nobs,).

        Returns
        -------
        np.ndarray
            Time-interpolated simulation values with shape (nobs,).
        """
        simtime = np.array(simtime, dtype=np.float64, order="F", copy=False)
        simval = np.array(simval, dtype=np.float64, order="F", copy=False)
        obspoint = np.array(obspoint, order="F", copy=False)
        obstime = np.array(obstime, dtype=np.float64, order="F", copy=False)
        if simtime.ndim != 1:
            raise ValueError("expected 'simtime' to have ndim=1")
        elif simval.ndim != 2:
            raise ValueError("expected 'simval' to have ndim=2")
        elif obspoint.ndim != 1:
            raise ValueError("expected 'obspoint' to have ndim=1")
        elif obstime.ndim != 1:
            raise ValueError("expected 'obstime' to have ndim=1")
        elif not np.issubdtype(obspoint.dtype, np.integer):
            raise ValueError(
                f"expected 'obspoint' to be integer type; found {obspoint.dtype}"
            )
        nsimtime, npts = simval.shape
        nobs = len(obspoint)
        obssimval = np.zeros(nobs, np.float64, order="F")
        res = self.pestutils.interp_to_obstime(
            byref(c_int(nsimtime)),
            byref(c_int(nproctime)),
            byref(c_int(npts)),
            simtime,
            simval,
            byref(c_double(interpthresh)),
            byref(c_char(how_extrap.encode())),
            byref(c_double(time_extrap)),
            byref(c_double(nointerpval)),
            byref(c_int(nobs)),
            obspoint.astype(np.int32, copy=False),
            obstime,
            obssimval,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("interpolated %d time points to %d observations", npts, nobs)
        return obssimval

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
            Where 1 is for DIS and 2 is for DISV.
        ncells : int
            Number of cells in the grid.
        ndim1, ndim2, ndim3 : int
            Grid dimensions.
        """
        grbfile = Path(grbfile)
        if not grbfile.is_file():
            raise FileNotFoundError(f"could not find grbfile {grbfile}")
        idis = c_int()
        ncells = c_int()
        ndim1 = c_int()
        ndim2 = c_int()
        ndim3 = c_int()
        res = self.pestutils.install_mf6_grid_from_file(
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
        res = self.pestutils.uninstall_mf6_grid(
            byref(self.create_char_array(gridname, "LENGRIDNAME"))
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("uninstalled mf6 grid %r", gridname)

    def calc_mf6_interp_factors(
        self,
        gridname: str,
        # npts: int,  # determined from ecoord.shape[0]
        ecoord: npt.ArrayLike,
        ncoord: npt.ArrayLike,
        layer: int | npt.ArrayLike,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
        blnfile: str | PathLike,
    ) -> npt.NDArray[np.int32]:
        """Calculate interpolation factors from a MODFLOW 6 DIS or DISV.

        Parameters
        ----------
        gridname : str
            Unique non-blank grid name.
        ecoord, ncoord : array_like
            X/Y or Easting/Northing coordinates for points with shape (npts,).
        layer : int or array_like
            Layers of points with shape (npts,).
        factorfile : str or PathLike
            File for kriging factors.
        factorfiletype : int, str or enum.FactorFileType
            Factor file type, where 0:binary, 1:text.
        blnfile : str or PathLike
            Name of bln file to write.

        Returns
        -------
        npt.NDArray[np.int32]
            Array interp_success(npts), where 1 is success and 0 is failure.
        """
        pts = ManyArrays({"ecoord": ecoord, "ncoord": ncoord}, int_any={"layer": layer})
        npts = len(pts)
        factorfile = Path(factorfile)
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        blnfile = Path(blnfile)  # TODO
        interp_success = np.zeros(npts, np.int32, order="F")
        res = self.pestutils.calc_mf6_interp_factors(
            byref(self.create_char_array(gridname, "LENGRIDNAME")),
            byref(c_int(npts)),
            pts.ecoord,
            pts.ncoord,
            pts.layer,
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(self.create_char_array(bytes(blnfile), "LENFILENAME")),
            interp_success,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("calculated mf6 interp factors for %r", gridname)
        return interp_success

    def interp_from_mf6_depvar_file(
        self,
        depvarfile: str | PathLike,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
        ntime: int,
        vartype: str,
        interpthresh: float,
        reapportion: int | bool,
        nointerpval: float,
        npts: int,
    ) -> dict:
        """
        Interpolate points using previously-calculated interpolation factors.

        Parameters
        ----------
        depvarfile : str or PathLike
            Name of binary file to read.
        factorfile : str or PathLike
            File containing spatial interpolation factors, written by
            :meth:`calc_mf6_interp_factors`.
        factorfiletype : int, str or enum.FactorFileType
            Use 0 for binary; 1 for text.
        ntime : int
            Number of output times.
        vartype : str
            Only read arrays of this type.
        interpthresh : float
            Absolute threshold for dry or inactive.
        reapportion : int or bool
            Use 0 for no (False); 1 for yes (True).
        nointerpval : float
            Value to use where interpolation is not possible.
        npts : int
            Number of points for interpolation.

        Returns
        -------
        nproctime : int
            Number of processed simulation times.
        simtime : npt.NDArray[np.float64]
            Simulation times, with shape (ntime,).
        simstate : npt.NDArray[np.float64]
            Interpolated system states, with shape (ntime, npts).
        """
        depvarfile = Path(depvarfile)
        if not depvarfile.is_file():
            raise FileNotFoundError(f"could not find depvarfile {depvarfile}")
        factorfile = Path(factorfile)  # TODO
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        simtime = np.zeros(ntime, np.float64, order="F")
        simstate = np.zeros((ntime, npts), np.float64, order="F")
        nproctime = c_int()
        res = self.pestutils.interp_from_mf6_depvar_file(
            byref(self.create_char_array(bytes(depvarfile), "LENFILENAME")),
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(c_int(ntime)),
            byref(self.create_char_array(vartype, "LENVARTYPE")),
            byref(c_double(interpthresh)),
            byref(c_int(reapportion)),
            byref(c_double(nointerpval)),
            byref(c_int(npts)),
            byref(nproctime),
            simtime,
            simstate,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info(
            "interpolated points from mf6 depvar file %r", npts, depvarfile.name
        )
        return {
            "nproctime": nproctime.value,
            "simtime": simtime,
            "simstate": simstate,
        }

    def extract_flows_from_cbc_file(
        self,
        cbcfile: str | PathLike,
        flowtype: str,
        isim: int,
        iprec: int | str | enum.Prec,
        # ncell: int,  # from izone.shape[0]
        izone: npt.ArrayLike,
        nzone: int,
        ntime: int,
    ) -> dict:
        """
        Read and accumulates flows from a CBC flow file to a user-specified BC.

        Parameters
        ----------
        cbcfile : str | PathLike
            Cell-by-cell flow term file written by any MF version.
        flowtype : str
            Type of flow to read.
        isim : int
            Simulator type.
        iprec : int, str or enum.Prec
            Precision used to record real variables in cbc file.
        izone : array_like
            Zonation of model domain, with shape (ncell,).
        nzone : int
            Equals or exceeds number of zones; zone 0 doesn't count.
        ntime : int
            Equals or exceed number of model output times for flow type.

        Returns
        -------
        numzone : int
            Number of non-zero-valued zones.
        zonenumber : npt.NDArray[np.int32]
            Zone numbers, with shape (nzone,).
        nproctime : int
            Number of processed simulation times.
        timestep : npt.NDArray[np.int32]
            Simulation time step, with shape (ntime,).
        stressperiod : npt.NDArray[np.int32]
            Simulation stress period, with shape (ntime,).
        simtime : npt.NDArray[np.int32]
            Simulation time, with shape (ntime,).
            A time of -1.0 indicates unknown.
        simflow : npt.NDArray[np.int32]
            Interpolated flows, with shape (ntime, nzone).
        """
        cbcfile = Path(cbcfile)
        if not cbcfile.is_file():
            raise FileNotFoundError(f"could not find cbcfile {cbcfile}")
        if isinstance(iprec, str):
            iprec = enum.Prec.get_value(iprec)
        cell = ManyArrays(int_any={"izone": izone})
        ncell = len(cell)
        numzone = c_int()
        zonenumber = np.zeros(nzone, np.int32, order="F")
        nproctime = c_int()
        timestep = np.zeros(ntime, np.int32, order="F")
        stressperiod = np.zeros(ntime, np.int32, order="F")
        simtime = np.zeros(ntime, np.float64, order="F")
        simflow = np.zeros((ntime, nzone), np.float64, order="F")
        res = self.pestutils.extract_flows_from_cbc_file(
            byref(self.create_char_array(bytes(cbcfile), "LENFILENAME")),
            byref(self.create_char_array(flowtype, "LENFLOWTYPE")),
            byref(c_int(isim)),
            byref(c_int(iprec)),
            byref(c_int(ncell)),
            cell.izone,
            byref(c_int(nzone)),
            byref(numzone),
            zonenumber,
            byref(c_int(ntime)),
            byref(nproctime),
            timestep,
            stressperiod,
            simtime,
            simflow,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("extracted flows from %r", cbcfile.name)
        return {
            "numzone": numzone.value,
            "zonenumber": zonenumber,
            "nproctime": nproctime.value,
            "timestep": timestep,
            "stressperiod": stressperiod,
            "simtime": simtime,
            "simflow": simflow,
        }

    def calc_kriging_factors_2d(
        self,
        # npts: int,  # determined from ecs.shape[0]
        ecs: npt.ArrayLike,
        ncs: npt.ArrayLike,
        zns: int | npt.ArrayLike,
        # mpts: int,  # determined from ect.shape[0]
        ect: npt.ArrayLike,
        nct: npt.ArrayLike,
        znt: int | npt.ArrayLike,
        vartype: int | str | enum.VarioType,
        krigtype: int | str | enum.KrigType,
        aa: float | npt.ArrayLike,
        anis: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        searchrad: float,
        maxpts: int,
        minpts: int,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
    ) -> int:
        """
        Calculate 2D kriging factors.

        Parameters
        ----------
        ecs, ncs : array_like
            Source point coordinates, each 1D array with shape (npts,).
        zns : int or array_like
            Source point zones, integer or 1D array with shape (npts,).
        ect, nct : array_like
            Target point coordinates, each 1D array with shape (mpts,).
        znt : int or array_like
            Target point zones, integer or 1D array with shape (mpts,).
        vartype : int, str or enum.VarioType
            Variogram type, where 1:spher, 2:exp, 3:gauss, 4:pow.
        krigtype : int, str, or enum.KrigType,
            Kriging type, where 0:simple, 1:ordinary.
        aa : float or array_like
            Variogram "a" value, float or 1D array with shape (mpts,).
        anis : float or array_like
            Variogram anisotropies, float or 1D array with shape (mpts,).
        bearing : float or array_like
            Variogram bearings, float or 1D array with shape (mpts,).
        searchrad : float
            Search radius.
        maxpts, minpts : int
            Search specifications.
        factorfile : str or PathLike
            File for kriging factors.
        factorfiletype : int, str or enum.FactorFileType
            Factor file type, where 0:binary, 1:text.

        Returns
        -------
        int
            Number of interp points.
        """
        npts = ManyArrays({"ecs": ecs, "ncs": ncs}, int_any={"zns": zns})
        mpts = ManyArrays(
            {"ect": ect, "nct": nct},
            {"aa": aa, "anis": anis, "bearing": bearing},
            {"znt": znt},
        )
        if isinstance(vartype, str):
            vartype = enum.VarioType.get_value(vartype)
        if isinstance(krigtype, str):
            krigtype = enum.KrigType.get_value(krigtype)
        factorfile = Path(factorfile)
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        icount_interp = c_int()
        res = self.pestutils.calc_kriging_factors_2d(
            byref(c_int(len(npts))),
            npts.ecs,
            npts.ncs,
            npts.zns,
            byref(c_int(len(mpts))),
            mpts.ect,
            mpts.nct,
            mpts.znt,
            byref(c_int(vartype)),
            byref(c_int(krigtype)),
            mpts.aa,
            mpts.anis,
            mpts.bearing,
            byref(c_double(searchrad)),
            byref(c_int(maxpts)),
            byref(c_int(minpts)),
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(icount_interp),
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("calculated 2D kriging factors to %r", factorfile.name)
        return icount_interp.value

    def calc_kriging_factors_auto_2d(
        self,
        # npts: int,  # determined from ecs.shape[0]
        ecs: npt.ArrayLike,
        ncs: npt.ArrayLike,
        zns: int | npt.ArrayLike,
        # mpts: int,  # determined from ect.shape[0]
        ect: npt.ArrayLike,
        nct: npt.ArrayLike,
        znt: int | npt.ArrayLike,
        krigtype: int | str | enum.KrigType,
        anis: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
    ) -> int:
        """
        Calculate 2D kriging factors, with automatic variogram properties.

        Parameters
        ----------
        ecs, ncs : array_like
            Source point coordinates, each 1D array with shape (npts,).
        zns : int or array_like
            Source point zones, integer or 1D array with shape (npts,).
        ect, nct : array_like
            Target point coordinates, each 1D array with shape (mpts,).
        znt : int or array_like
            Target point zones, integer or 1D array with shape (mpts,).
        krigtype : int, str, enum.KrigType
            Kriging type, where 0:simple, 1:ordinary.
        anis : float or array_like
            Variogram anisotropies, float or 1D array with shape (mpts,).
        bearing : float or array_like
            Variogram bearings, float or 1D array with shape (mpts,).
        factorfile : str or PathLike
            File for kriging factors.
        factorfiletype : int, str or enum.FactorFileType
            Factor file type, where 0:binary, 1:text.

        Returns
        -------
        int
            Number of interp points.
        """
        npts = ManyArrays({"ecs": ecs, "ncs": ncs}, int_d={"zns": zns})
        mpts = ManyArrays(
            {"ect": ect, "nct": nct}, {"anis": anis, "bearing": bearing}, {"znt": znt}
        )
        if isinstance(krigtype, str):
            krigtype = enum.KrigType.get_value(krigtype)
        factorfile = Path(factorfile)
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        icount_interp = c_int()
        res = self.pestutils.calc_kriging_factors_auto_2d(
            byref(c_int(len(npts))),
            npts.ecs,
            npts.ncs,
            npts.zns,
            byref(c_int(len(mpts))),
            mpts.ect,
            mpts.nct,
            mpts.znt,
            byref(c_int(krigtype)),
            mpts.anis,
            mpts.bearing,
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(icount_interp),
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("calculated 2D auto kriging factors to %r", factorfile.name)
        return icount_interp.value

    def calc_kriging_factors_3d(
        self,
        # npts: int,  # determined from ecs.shape[0]
        ecs: npt.ArrayLike,
        ncs: npt.ArrayLike,
        zcs: npt.ArrayLike,
        zns: int | npt.ArrayLike,
        # mpts: int,  # determined from ect.shape[0]
        ect: npt.ArrayLike,
        nct: npt.ArrayLike,
        zct: npt.ArrayLike,
        znt: int | npt.ArrayLike,
        zonenum: int | npt.ArrayLike,
        krigtype: int | str | enum.KrigType,
        # nzone: int,  # determined from shape[0] from any zonenum..rake else 1
        vartype: int | str | enum.VarioType | npt.ArrayLike,
        ahmax: float | npt.ArrayLike,
        ahmin: float | npt.ArrayLike,
        avert: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        dip: float | npt.ArrayLike,
        rake: float | npt.ArrayLike,
        srhmax: float,
        srhmin: float,
        srvert: float,
        maxpts: int,
        minpts: int,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
    ) -> int:
        """
        Calculate 3D kriging factors.

        Parameters
        ----------
        ecs, ncs, zcs : array_like
            Source point coordinates, each 1D array with shape (npts,).
        zns : int or array_like
            Source point zones, integer or 1D array with shape (npts,).
        ect, nct, zct : array_like
            Target point coordinates, each 1D array with shape (mpts,).
        znt : int or array_like
            Target point zones, integer or 1D array with shape (mpts,).
        krigtype : int, str, or enum.KrigType,
            Kriging type, where 0:simple, 1:ordinary.
        zonenum : int, or array_like
            Zone numbers, inteter or 1D array with shape (nzone,).
        vartype : int, str, enum.VarioType or array_like
            Variogram type, where 1:spher, 2:exp, 3:gauss, 4:pow. If array,
            then it should have shape (nzone,).
        ahmax, ahmin, avert : float or array_like
            Variogram "a" values in 3 orthogonal directions (hmax, hmin, vert).
            Each can be a float or 1D array with shape (nzone,).
        bearing : float or array_like
            Bearing of hmax, float or 1D array with shape (nzone,).
        dip : float or array_like
            Dip of hmax, float or 1D array with shape (nzone,).
        rake : float or array_like
            Twist about hmax axis, float or 1D array with shape (nzone,).
        srhmax, srhmin, srvert : float
            Search radius in hmax, hmin, and vert directions.
        maxpts, minpts : int
            Search specifications.
        factorfile : str or PathLike
            File for kriging factors.
        factorfiletype : int, str or enum.FactorFileType
            Factor file type, where 0:binary, 1:text.

        Returns
        -------
        int
            Number of interp points.
        """
        npts = ManyArrays({"ecs": ecs, "ncs": ncs, "zcs": zcs}, int_any={"zns": zns})
        mpts = ManyArrays({"ect": ect, "nct": nct, "zct": zct}, int_any={"znt": znt})
        if isinstance(krigtype, str):
            krigtype = enum.KrigType.get_value(krigtype)
        vartype = np.array(vartype)
        if np.issubdtype(vartype.dtype, str):
            vartype = np.vectorize(enum.VarioType.get_value)(vartype)
        if not np.issubdtype(vartype.dtype, np.integer):
            raise ValueError("expected 'vartype' to be integer, str or enum.VarioType")
        nzone = ManyArrays(
            float_any={
                "ahmax": ahmax,
                "ahmin": ahmin,
                "avert": avert,
                "bearing": bearing,
                "dip": dip,
                "rake": rake,
            },
            int_any={"zonenum": zonenum, "vartype": vartype},
        )
        factorfile = Path(factorfile)
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        icount_interp = c_int()
        res = self.pestutils.calc_kriging_factors_3d(
            byref(c_int(len(npts))),
            npts.ecs,
            npts.ncs,
            npts.zcs,
            npts.zns,
            byref(c_int(len(mpts))),
            mpts.ect,
            mpts.nct,
            mpts.zct,
            mpts.znt,
            byref(c_int(krigtype)),
            byref(c_int(len(nzone))),
            nzone.zonenum,
            nzone.vartype,
            nzone.ahmax,
            nzone.ahmin,
            nzone.avert,
            nzone.bearing,
            nzone.dip,
            nzone.rake,
            byref(c_double(srhmax)),
            byref(c_double(srhmin)),
            byref(c_double(srvert)),
            byref(c_int(maxpts)),
            byref(c_int(minpts)),
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(icount_interp),
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("calculated 3D kriging factors to %r", factorfile.name)
        return icount_interp.value

    def krige_using_file(
        self,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
        # npts: int,  # determined from sourceval.shape[0]
        mpts: int,
        krigtype: int | str | enum.KrigType,
        transtype: int | str | enum.TransType,
        sourceval: npt.ArrayLike,
        meanval: float | npt.ArrayLike | None,
    ) -> dict:
        """
        Apply interpolation factors calculated by other functions.

        Parameters
        ----------
        factorfile : str or PathLike
            Input file with kriging factors.
        factorfiletype : int, str or enum.FactorFileType
            Factor file type, where 0:binary, 1:text.
        mpts : int
            Number of target points, used to compare with value in factor file.
        krigtype : int, str, or enum.KrigType,
            Kriging type, where 0:simple, 1:ordinary.
        transtype : int, str, enum.TransType
            Tranformation type, where 0 is none and 1 is log.
        sourceval : array_like
            Values at sources, 1D array with shape (npts,).
        meanval : float, array_like, optional
            Mean values are required if simple kriging, described as a float
            or 1D array with shape (mpts,).

        Returns
        -------
        targval : npt.NDArray[np.float64]
            Values calculated for targets.
        icount_interp : int
            Number of interpolation pts.
        """
        factorfile = Path(factorfile)
        if not factorfile.is_file():
            raise FileNotFoundError(f"could not find factorfile {factorfile}")
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        if isinstance(krigtype, str):
            krigtype = enum.KrigType.get_value(krigtype)
        if isinstance(transtype, str):
            transtype = enum.TransType.get_value(transtype)
        float_arrays = {"sourceval": sourceval}
        meanval_is_None = meanval is None
        if meanval_is_None:
            if krigtype == enum.KrigType.simple:
                self.logger.error(
                    "simple kriging requires 'meanval'; assuming zero for now"
                )
                meanval = np.zeros(mpts, np.float64, order="F")
            else:
                meanval = np.zeros(0, np.float64, order="F")  # dummy pointer
        else:
            float_arrays["meanval"] = meanval
        pts = ManyArrays(float_arrays)
        if not meanval_is_None:
            meanval = pts.meanval
        targval = np.zeros(mpts, np.float64, order="F")
        icount_interp = c_int()
        res = self.pestutils.krige_using_file(
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(c_int(len(pts))),
            byref(c_int(mpts)),
            byref(c_int(krigtype)),
            byref(c_int(transtype)),
            pts.sourceval,
            targval,
            byref(icount_interp),
            meanval,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("kriged using factor file %r", factorfile.name)
        return {
            "targval": targval,
            "icount_interp": icount_interp.value,
        }

    def build_covar_matrix_2d(
        self,
        # npts: int,  # determined from ec.shape[0]
        ec: npt.ArrayLike,
        nc: npt.ArrayLike,
        zn: int | npt.ArrayLike,
        vartype: int | str | enum.VarioType,
        nugget: float | npt.ArrayLike,
        aa: float | npt.ArrayLike,
        sill: float | npt.ArrayLike,
        anis: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        ldcovmat: int,
    ) -> npt.NDArray[np.float64]:
        """
        Calculate a covariance matrix for a set of 2D pilot points.

        Parameters
        ----------
        ec, nc : array_like
            Pilot point coordinates, each 1D array with shape (npts,).
        zn : int or array_like
            Pilot point zones, integer or 1D array with shape (npts,).
        vartype : int, str or enum.VarioType
            Variogram type, where 1:spher, 2:exp, 3:gauss, 4:pow.
        nugget, aa, sill, anis, bearing : float or array_like
            Variogram parameters, each float or 1D array with shape (npts,).
        ldcovmat : int
            Leading dimension of covmat.

        Returns
        -------
        npt.NDArray[np.float64]
            2D matrix covmat(ldcovmat, npts).
        """
        pts = ManyArrays(
            {"ec": ec, "nc": nc},
            {
                "nugget": nugget,
                "aa": aa,
                "sill": sill,
                "anis": anis,
                "bearing": bearing,
            },
            {"zn": zn},
        )
        npts = len(pts)
        if isinstance(vartype, str):
            vartype = enum.VarioType.get_value(vartype)
        covmat = np.zeros((ldcovmat, npts), np.float64, order="F")
        res = self.pestutils.build_covar_matrix_2d(
            byref(c_int(npts)),
            pts.ec,
            pts.nc,
            pts.zn,
            byref(c_int(vartype)),
            pts.nugget,
            pts.aa,
            pts.sill,
            pts.anis,
            pts.bearing,
            byref(c_int(ldcovmat)),
            covmat,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("calculated covariance matrix for %d 2D pilot points", npts)
        return covmat

    def build_covar_matrix_3d(
        self,
        # npts: int,  # determined from ec.shape[0]
        ec: npt.ArrayLike,
        nc: npt.ArrayLike,
        zc: npt.ArrayLike,
        zn: int | npt.ArrayLike,
        vartype: int | str | enum.VarioType,
        nugget: float | npt.ArrayLike,
        sill: float | npt.ArrayLike,
        ahmax: float | npt.ArrayLike,
        ahmin: float | npt.ArrayLike,
        avert: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        dip: float | npt.ArrayLike,
        rake: float | npt.ArrayLike,
        ldcovmat: int,
    ) -> npt.NDArray[np.float64]:
        """
        Calculate a covariance matrix for a set of 3D pilot points.

        Parameters
        ----------
        ec, nc, zc: array_like
            Pilot point coordinates, each 1D array with shape (npts,).
        zn : int or array_like
            Pilot point zones, integer or 1D array with shape (npts,).
        vartype : int, str or enum.VarioType
            Variogram type, where 1:spher, 2:exp, 3:gauss, 4:pow.
        nugget, sill : float or array_like
            Variogram parameters, each float or 1D array with shape (npts,).
        ahmax, ahmin, avert : float or array_like
            Variogram a parameters, each float or 1D array with shape (npts,).
        bearing, dip, rake : float or array_like
            Variogram angles, each float or 1D array with shape (npts,).
        ldcovmat : int
            Leading dimension of covmat.

        Returns
        -------
        npt.NDArray[np.float64]
            2D matrix covmat(ldcovmat, npts).
        """
        pts = ManyArrays(
            {"ec": ec, "nc": nc, "zc": zc},
            {
                "nugget": nugget,
                "sill": sill,
                "ahmax": ahmax,
                "ahmin": ahmin,
                "avert": avert,
                "bearing": bearing,
                "dip": dip,
                "rake": rake,
            },
            {"zn": zn},
        )
        npts = len(pts)
        if isinstance(vartype, str):
            vartype = enum.VarioType.get_value(vartype)
        covmat = np.zeros((ldcovmat, npts), np.float64, order="F")
        res = self.pestutils.build_covar_matrix_3d(
            byref(c_int(npts)),
            pts.ec,
            pts.nc,
            pts.zc,
            pts.zn,
            byref(c_int(vartype)),
            pts.nugget,
            pts.sill,
            pts.ahmax,
            pts.ahmin,
            pts.avert,
            pts.bearing,
            pts.dip,
            pts.rake,
            byref(c_int(ldcovmat)),
            covmat,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("calculated covariance matrix for %d 3D pilot points", npts)
        return covmat

    def calc_structural_overlay_factors(
        self,
        # npts: int,  # determined from ecs.shape[0]
        ecs: npt.ArrayLike,
        ncs: npt.ArrayLike,
        ids: int | npt.ArrayLike,
        conwidth: npt.ArrayLike,
        aa: npt.ArrayLike,
        structype: int | str | enum.StrucType,
        inverse_power: float,
        # mpts: int,  # determined from ect.shape[0]
        ect: npt.ArrayLike,
        nct: npt.ArrayLike,
        active: int | npt.ArrayLike,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
    ) -> int:
        """
        Calculate interpolation/blending factors for structural overlay parameters.

        Parameters
        ----------
        ecs, ncs : array_like
            Source point coordinates, each 1D array with shape (npts,).
        ids : int or array_like
            Source point structure number, integer or 1D array with shape (npts,).
        conwidth, aa : float or array_like
            Blending parameters, float or 1D array with shape (npts,).
        structype : int, str or enum.StrucType
            Structure type, where 0 is polylinear and 1 is polygonal.
        inverse_power : float
            Inverse power of distance.
        ect, nct : array_like
            Target point coordinates, each 1D array with shape (mpts,).
        active : int or array_like
            Target point activity, integer or 1D array with shape (mpts,).
        factorfile : str or PathLike
            File for kriging factors.
        factorfiletype : int, str or enum.FactorFileType
            Factor file type, where 0:binary, 1:text.

        Returns
        -------
        int
            Number of interp points.
        """
        npts = ManyArrays(
            {"ecs": ecs, "ncs": ncs}, {"conwidth": conwidth, "aa": aa}, {"ids": ids}
        )
        mpts = ManyArrays({"ect": ect, "nct": nct}, int_any={"active": active})
        if isinstance(structype, str):
            structype = enum.StrucType.get_value(structype)
        factorfile = Path(factorfile)
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        icount_interp = c_int()
        res = self.pestutils.calc_structural_overlay_factors(
            byref(c_int(len(npts))),
            npts.ecs,
            npts.ncs,
            npts.ids,
            npts.conwidth,
            npts.aa,
            byref(c_int(structype)),
            byref(c_double(inverse_power)),
            byref(c_int(len(mpts))),
            mpts.ect,
            mpts.nct,
            mpts.active,
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(icount_interp),
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info(
            "calculated interpolation/blending factors to %r", factorfile.name
        )
        return icount_interp.value

    def interpolate_blend_using_file(
        self,
        factorfile: str | PathLike,
        factorfiletype: int | str | enum.FactorFileType,
        # npts: int,  # determined from sourceval.shape[0]
        # mpts: int,  # determined from targval.shape[0]
        transtype: int | str | enum.TransType,
        lt_target: str | bool,
        gt_target: str | bool,
        sourceval: npt.ArrayLike,
        targval: npt.ArrayLike,
    ) -> dict:
        """
        Apply interpolation factors calculated by :meth:`calc_structural_overlay_factors`.

        Parameters
        ----------
        factorfile : str or PathLike
            File for kriging factors.
        factorfiletype : int, str or enum.FactorFileType
            Factor file type, where 0:binary, 1:text.
        transtype : int, str, enum.TransType
            Tranformation type, where 0 is none and 1 is log.
        lt_target, gt_target : str or bool
            Whether to undercut or exceed target, use "Y"/"N" or bool.
        sourceval : array_like
            Values at sources, 1D array with shape (npts,).
        targval : array_like
            Values at targets, 1D array with shape (mpts,).

        Returns
        -------
        targval : npt.NDArray[np.float64]
            Values calculated for targets.
        icount_interp : int
            Number of interpolation pts.
        """
        factorfile = Path(factorfile)
        if not factorfile.is_file():
            raise FileNotFoundError(f"could not find factorfile {factorfile}")
        if isinstance(factorfiletype, str):
            factorfiletype = enum.FactorFileType.get_value(factorfiletype)
        if isinstance(transtype, str):
            transtype = enum.TransType.get_value(transtype)
        if isinstance(lt_target, bool):
            lt_target = "y" if lt_target else "n"
        if isinstance(gt_target, bool):
            gt_target = "y" if gt_target else "n"
        npts = ManyArrays({"sourceval": sourceval})
        mpts = ManyArrays({"targval": targval})
        icount_interp = c_int()
        res = self.pestutils.interpolate_blend_using_file(
            byref(self.create_char_array(bytes(factorfile), "LENFILENAME")),
            byref(c_int(factorfiletype)),
            byref(c_int(len(npts))),
            byref(c_int(len(mpts))),
            byref(c_int(transtype)),
            byref(c_char(lt_target.encode())),
            byref(c_char(gt_target.encode())),
            npts.sourceval,
            mpts.targval,
            byref(icount_interp),
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("applied interpolation factors from %r", factorfile.name)
        return {
            "targval": mpts.targval,
            "icount_interp": icount_interp.value,
        }

    def ipd_interpolate_2d(
        self,
        # npts: int,  # determined from ecs.shape[0]
        ecs: npt.ArrayLike,
        ncs: npt.ArrayLike,
        zns: int | npt.ArrayLike,
        sourceval: npt.ArrayLike,
        # mpts: int,  # determined from ect.shape[0]
        ect: npt.ArrayLike,
        nct: npt.ArrayLike,
        znt: int | npt.ArrayLike,
        transtype: int | str | enum.TransType,
        anis: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        invpow: float | npt.ArrayLike,
    ) -> npt.NDArray[np.float64]:
        """Undertake 2D inverse-power-of-distance spatial interpolation.

        Parameters
        ----------
        ecs, ncs : array_like
            Source point coordinates, each 1D array with shape (npts,).
        zns : int or array_like
            Source point zones, integer or 1D array with shape (npts,).
        sourceval : array_like
            Source values, 1D array with shape (npts,).
        ect, nct : array_like
            Target point coordinates, each 1D array with shape (mpts,).
        znt : int or array_like
            Target point zones, integer or 1D array with shape (mpts,).
        transtype : int, str, enum.TransType
            Tranformation type, where 0 is none and 1 is log.
        anis : float or array_like
            Local anisotropy, float or 1D array with shape (mpts,).
        bearing : float or array_like
            Local anisotropy bearing, float or 1D array with shape (mpts,).
        invpow : float or array_like
            Local inverse power of distance, float or 1D array with shape (mpts,).

        Returns
        -------
        npt.NDArray[np.float64]
            Values calculated for targets.
        """
        npts = ManyArrays(
            {"ecs": ecs, "ncs": ncs, "sourceval": sourceval}, int_any={"zns": zns}
        )
        mpts = ManyArrays(
            {"ect": ect, "nct": nct},
            {"anis": anis, "bearing": bearing, "invpow": invpow},
            {"znt": znt},
        )
        if isinstance(transtype, str):
            transtype = enum.TransType.get_value(transtype)
        targval = np.zeros(len(mpts), np.float64, order="F")
        res = self.pestutils.ipd_interpolate_2d(
            byref(c_int(len(npts))),
            npts.ecs,
            npts.ncs,
            npts.zns,
            npts.sourceval,
            byref(c_int(len(mpts))),
            mpts.ect,
            mpts.nct,
            mpts.znt,
            targval,
            byref(c_int(transtype)),
            mpts.anis,
            mpts.bearing,
            mpts.invpow,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("undertook 2D inverse-power-of-distance spatial interpolation")
        return targval

    def ipd_interpolate_3d(
        self,
        # npts: int,  # determined from ecs.shape[0]
        ecs: npt.ArrayLike,
        ncs: npt.ArrayLike,
        zcs: npt.ArrayLike,
        zns: int | npt.ArrayLike,
        sourceval: npt.ArrayLike,
        # mpts: int,  # determined from ect.shape[0]
        ect: npt.ArrayLike,
        nct: npt.ArrayLike,
        zct: npt.ArrayLike,
        znt: int | npt.ArrayLike,
        transtype: int | str | enum.TransType,
        ahmax: float | npt.ArrayLike,
        ahmin: float | npt.ArrayLike,
        avert: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        dip: float | npt.ArrayLike,
        rake: float | npt.ArrayLike,
        invpow: float | npt.ArrayLike,
    ) -> npt.NDArray[np.float64]:
        """Undertake 3D inverse-power-of-distance spatial interpolation.

        Parameters
        ----------
        ecs, ncs, zcs : array_like
            Source point coordinates, each 1D array with shape (npts,).
        zns : int or array_like
            Source point zones, integer or 1D array with shape (npts,).
        sourceval : array_like
            Source values, 1D array with shape (npts,).
        ect, nct, zct : array_like
            Target point coordinates, each 1D array with shape (mpts,).
        znt : int or array_like
            Target point zones, integer or 1D array with shape (mpts,).
        transtype : int, str, enum.TransType
            Tranformation type, where 0 is none and 1 is log.
        ahmax, ahmin, avert : float or array_like
            Relative correlation lengths, float or 1D array with shape (mpts,).
        bearing, dip, rake : float or array_like
            Correlation directions, float or 1D array with shape (mpts,).
        invpow : float or array_like
            Local inverse power of distance, float or 1D array with shape (mpts,).

        Returns
        -------
        npt.NDArray[np.float64]
            Values calculated for targets.
        """
        npts = ManyArrays(
            {"ecs": ecs, "ncs": ncs, "zcs": zcs, "sourceval": sourceval},
            int_any={"zns": zns},
        )
        mpts = ManyArrays(
            {"ect": ect, "nct": nct, "zct": zct},
            {
                "ahmax": ahmax,
                "ahmin": ahmin,
                "avert": avert,
                "bearing": bearing,
                "dip": dip,
                "rake": rake,
                "invpow": invpow,
            },
            {"znt": znt},
        )
        if isinstance(transtype, str):
            transtype = enum.TransType.get_value(transtype)
        targval = np.zeros(len(mpts), np.float64, order="F")
        res = self.pestutils.ipd_interpolate_3d(
            byref(c_int(len(npts))),
            npts.ecs,
            npts.ncs,
            npts.zcs,
            npts.zns,
            npts.sourceval,
            byref(c_int(len(mpts))),
            mpts.ect,
            mpts.nct,
            mpts.zct,
            mpts.znt,
            targval,
            byref(c_int(transtype)),
            mpts.ahmax,
            mpts.ahmin,
            mpts.avert,
            mpts.bearing,
            mpts.dip,
            mpts.rake,
            mpts.invpow,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("undertook 3D inverse-power-of-distance spatial interpolation")
        return targval

    def initialize_randgen(self, iseed: int) -> None:
        """
        Initialize the random number generator.

        Parameters
        ----------
        iseed : int
            Seed value.
        """
        res = self.pestutils.initialize_randgen(byref(c_int(iseed)))
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("initialized the random number generator")

    def fieldgen2d_sva(
        self,
        # nnode: int,  # determined from ec.shape[0]
        ec: npt.ArrayLike,
        nc: npt.ArrayLike,
        area: float | npt.ArrayLike,
        active: int | npt.ArrayLike,
        mean: float | npt.ArrayLike,
        var: float | npt.ArrayLike,
        aa: float | npt.ArrayLike,
        anis: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        transtype: int | str | enum.TransType,
        avetype: int | str | enum.VarioType,
        power: float,
        # ldrand: int,  # same as nnode
        nreal: int,
    ) -> npt.NDArray[np.float64]:
        """
        Generate 2D stochastic fields based on a spatially varying variogram.

        Parameters
        ----------
        ec, nc : array_like
            Model grid coordinates, each 1D array with shape (nnode,).
        area : float or array_like
            Areas of grid cells.
        active : int or array_like
            Inactive grid cells are equal to zero.
        mean : float or array_like
            Mean value of stochastic field.
        var : float or array_like
            Variance of stochastic field.
        aa : float or array_like
            Averaging function spatial dimension.
        anis : float or array_like
            Anisotropy ratio.
        bearing : float or array_like
            Bearing of principal anisotropy axis.
        transtype : int, str or enum.TransType
            Stochastic field pertains to natural(0) or log(1) properties.
        avetype : int, str or enum.VarioType
            Averaging function type, where 1:spher, 2:exp, 3:gauss, 4:pow.
        power : float
            Power used if avetype is 4 (pow).
        nreal : int
            Number of realisations to generate.

        Returns
        -------
        npt.NDArray[np.float64]
            Realisations with shape (nnode, nreal).
        """
        node = ManyArrays(
            {"ec": ec, "nc": nc},
            {
                "area": area,
                "mean": mean,
                "var": var,
                "aa": aa,
                "anis": anis,
                "bearing": bearing,
            },
            {"active": active},
        )
        if isinstance(transtype, str):
            transtype = enum.TransType.get_value(transtype)
        if isinstance(avetype, str):
            avetype = enum.VarioType.get_value(avetype)
        ldrand = nnode = len(node)
        randfield = np.zeros((ldrand, nreal), np.float64, order="F")
        res = self.pestutils.fieldgen2d_sva(
            byref(c_int(nnode)),
            node.ec,
            node.nc,
            node.area,
            node.active,
            node.mean,
            node.var,
            node.aa,
            node.anis,
            node.bearing,
            byref(c_int(transtype)),
            byref(c_int(avetype)),
            byref(c_double(power)),
            byref(c_int(ldrand)),
            byref(c_int(nreal)),
            randfield,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("generated 2D stochastic fields for %d realisations", nreal)
        return randfield

    def fieldgen3d_sva(
        self,
        # nnode: int,  # determined from ec.shape[0]
        ec: npt.ArrayLike,
        nc: npt.ArrayLike,
        zc: npt.ArrayLike,
        area: float | npt.ArrayLike,
        height: float | npt.ArrayLike,
        active: int | npt.ArrayLike,
        mean: float | npt.ArrayLike,
        var: float | npt.ArrayLike,
        ahmax: float | npt.ArrayLike,
        ahmin: float | npt.ArrayLike,
        avert: float | npt.ArrayLike,
        bearing: float | npt.ArrayLike,
        dip: float | npt.ArrayLike,
        rake: float | npt.ArrayLike,
        transtype: int | str | enum.TransType,
        avetype: int | str | enum.VarioType,
        power: float,
        # ldrand: int,  # same as nnode
        nreal: int,
    ) -> npt.NDArray[np.float64]:
        """
        Generate 3D stochastic fields based on a spatially varying variogram.

        Parameters
        ----------
        ec, nc, nz : array_like
            Model grid coordinates, each 1D array with shape (nnode,).
        area, height : float or array_like
            Areas and height of grid cells.
        active : int or array_like
            Inactive grid cells are equal to zero.
        mean : float or array_like
            Mean value of stochastic field.
        var : float or array_like
            Variance of stochastic field.
        ahmax, ahmin, avert : float or array_like
            Averaging function correlation lengths.
        bearing : float or array_like
            Bearing of ahmax direction.
        dip : float or array_like
            Dip of ahmax direction.
        rake : float or array_like
            Rotation of ahmin direction.
        transtype : int, str or enum.TransType
            Stochastic field pertains to natural(0) or log(1) properties.
        avetype : int, str or enum.VarioType
            Averaging function type, where 1:spher, 2:exp, 3:gauss, 4:pow.
        power : float
            Power used if avetype is 4 (pow).
        nreal : int
            Number of realisations to generate.

        Returns
        -------
        npt.NDArray[np.float64]
            Realisations with shape (nnode, nreal).
        """
        node = ManyArrays(
            {"ec": ec, "nc": nc, "zc": zc},
            {
                "area": area,
                "height": height,
                "mean": mean,
                "var": var,
                "ahmax": ahmax,
                "ahmin": ahmin,
                "avert": avert,
                "bearing": bearing,
                "dip": dip,
                "rake": rake,
            },
            {"active": active},
        )
        if isinstance(transtype, str):
            transtype = enum.TransType.get_value(transtype)
        if isinstance(avetype, str):
            avetype = enum.VarioType.get_value(avetype)
        ldrand = nnode = len(node)
        randfield = np.zeros((ldrand, nreal), np.float64, order="F")
        res = self.pestutils.fieldgen3d_sva(
            byref(c_int(nnode)),
            node.ec,
            node.nc,
            node.zc,
            node.area,
            node.height,
            node.active,
            node.mean,
            node.var,
            node.ahmax,
            node.ahmin,
            node.avert,
            node.bearing,
            node.dip,
            node.rake,
            byref(c_int(transtype)),
            byref(c_int(avetype)),
            byref(c_double(power)),
            byref(c_int(ldrand)),
            byref(c_int(nreal)),
            randfield,
        )
        if res != 0:
            raise PestUtilsLibError(self.retrieve_error_message())
        self.logger.info("generated 3D stochastic fields for %d realisations", nreal)
        return randfield
