"""Low-level Fortran-Python ctypes functions."""
from __future__ import annotations

from ctypes import ARRAY, CDLL, POINTER, c_char, c_double, c_int

from numpy.ctypeslib import ndpointer

# Cache variables by uppercase dimvar name
_dimvar_cache = {}
_char_array_cache = {}
_misc_lengths = {
    "LENVARTYPE": 17,
}


def get_dimvar_int(lib: CDLL, name: str) -> int:
    """Get dimvar constant integer from library instance."""
    if name in _dimvar_cache:
        return _dimvar_cache[name]
    elif name in _misc_lengths:
        # Special consideration for constants not specified by dimvar
        return _misc_lengths[name]
    c_var = c_int.in_dll(lib, name)
    _dimvar_cache[name] = c_var.value
    return c_var.value


def get_char_array(lib: CDLL, name: str):
    """Get c_char array with a fixed size from dimvar."""
    if name in _char_array_cache:
        return _char_array_cache[name]
    size = get_dimvar_int(lib, name)
    array_type = ARRAY(c_char, size)
    _char_array_cache[name] = array_type
    return array_type


def prototype(lib):
    """Add ctypes prototypes for each function."""
    # Generate c_char Array types based on dimvar sizes
    lenfilename_t = get_char_array(lib, "LENFILENAME")
    lenmessage_t = get_char_array(lib, "LENMESSAGE")
    lengridname_t = get_char_array(lib, "LENGRIDNAME")
    lenvartype_t = get_char_array(lib, "LENVARTYPE")

    # inquire_modflow_binary_file_specs(
    #   filein,fileout,isim,itype,iprec,narray,ntime)
    lib.inquire_modflow_binary_file_specs.argtypes = (
        POINTER(lenfilename_t),  # filein, in
        POINTER(lenfilename_t),  # fileout, in
        POINTER(c_int),  # isim, in
        POINTER(c_int),  # itype, in
        POINTER(c_int),  # iprec, out
        POINTER(c_int),  # narray, out
        POINTER(c_int),  # ntime, out
    )
    lib.inquire_modflow_binary_file_specs.restype = c_int

    # retrieve_error_message(errormessage)
    lib.retrieve_error_message.argtypes = (POINTER(lenmessage_t),)  # errormessage, out
    lib.retrieve_error_message.restype = c_int

    # install_structured_grid(
    #   gridname,ncol,nrow,nlay,icorner,e0,n0,rotation,delr,delc)
    lib.install_structured_grid.argtypes = (
        POINTER(lengridname_t),  # gridname, in
        POINTER(c_int),  # ncol, in
        POINTER(c_int),  # nrow, in
        POINTER(c_int),  # nlay, in
        POINTER(c_int),  # icorner, in
        POINTER(c_double),  # e0, in
        POINTER(c_double),  # n0, in
        POINTER(c_double),  # rotation, in
        ndpointer(c_double, ndim=1),  # delr(ncol), in
        ndpointer(c_double, ndim=1),  # delc(nrow), in
    )
    lib.install_structured_grid.restype = c_int

    # uninstall_structured_grid(gridname)
    lib.uninstall_structured_grid.argtypes = (POINTER(lengridname_t),)  # gridname, in
    lib.uninstall_structured_grid.restype = c_int

    # free_all_memory()
    lib.free_all_memory.argtypes = ()
    lib.free_all_memory.restype = c_int

    # interp_from_structured_grid(
    #   gridname,depvarfile,isim,iprec,ntime,vartype,interpthresh,nointerpval,
    #   npts,ecoord,ncoord,layer,nproctime,simtime,simstate)
    lib.interp_from_structured_grid.argtypes = (
        POINTER(lengridname_t),  # gridname, in
        POINTER(lenfilename_t),  # depvarfile, in
        POINTER(c_int),  # isim, in
        POINTER(c_int),  # iprec, in
        POINTER(c_int),  # ntime, in
        POINTER(lenvartype_t),  # vartype, in
        POINTER(c_double),  # interpthresh, in
        POINTER(c_double),  # nointerpval, in
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # ecoord, in
        ndpointer(c_double, ndim=1),  # ncoord, in
        ndpointer(c_int, ndim=1),  # layer, in
        POINTER(c_int),  # nproctime, out
        ndpointer(c_double, ndim=1),  # simtime(ntime), out
        ndpointer(c_double, ndim=2, flags="F"),  # simstate(ntime,npts), out
    )
    lib.interp_from_structured_grid.restype = c_int

    # interp_to_obstime(
    #   nsimtime,nproctime,npts,simtime,simval,interpthresh,how_extrap,
    #   time_extrap,nointerpval,nobs,obspoint,obstime,obssimval)
    lib.interp_to_obstime.argtypes = (
        POINTER(c_int),  # nsimtime, in
        POINTER(c_int),  # nproctime, in
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # simtime(nsimtime), in
        ndpointer(c_double, ndim=2, flags="F"),  # simval(nsimtime,npts), in
        POINTER(c_double),  # interpthresh, in
        POINTER(c_char),  # how_extrap, in
        POINTER(c_double),  # time_extrap, in
        POINTER(c_double),  # nointerpval, in
        POINTER(c_int),  # nobs, in
        ndpointer(c_int, ndim=1),  # obspoint(nobs), in
        ndpointer(c_double, ndim=1),  # obstime(nobs), in
        ndpointer(c_double, ndim=1),  # obssimval(nobs), out
    )
    lib.interp_to_obstime.restype = c_int

    # install_mf6_grid_from_file(
    #   gridname,grbfile,idis,ncells,ndim1,ndim2,ndim3)
    lib.install_mf6_grid_from_file.argtypes = (
        POINTER(lengridname_t),  # gridname, in
        POINTER(lenfilename_t),  # grbfile, in
        POINTER(c_int),  # idis, out
        POINTER(c_int),  # ncells, out
        POINTER(c_int),  # ndim1, out
        POINTER(c_int),  # ndim2, out
        POINTER(c_int),  # ndim3, out
    )
    lib.install_mf6_grid_from_file.restype = c_int

    # uninstall_mf6_grid(gridname)
    lib.uninstall_mf6_grid.argtypes = (POINTER(lengridname_t),)  # gridname, in
    lib.uninstall_mf6_grid.restype = c_int

    # calc_mf6_interp_factors(
    #   gridname,npts,ecoord,ncoord,layer,factorfile,
    #   factorfiletype,blnfile,interp_success)
    lib.calc_mf6_interp_factors.argtypes = (
        POINTER(lengridname_t),  # gridname, in
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # ecoord, in
        ndpointer(c_double, ndim=1),  # ncoord, in
        ndpointer(c_int, ndim=1),  # layer, in
        POINTER(lenfilename_t),  # factorfile, in
        POINTER(c_int),  # factorfiletype, in
        POINTER(lenfilename_t),  # blnfile, in
        ndpointer(c_int, ndim=1),  # interp_success, out
    )
    lib.calc_mf6_interp_factors.restype = c_int

    # interp_from_mf6_depvar_file(
    #   depvarfile,factorfile,factorfiletype,ntime,vartype,interpthresh,
    #   reapportion,nointerpval,npts,nproctime,simtime,simstate)
    lib.interp_from_mf6_depvar_file.argtypes = (
        POINTER(lenfilename_t),  # depvarfile, in
        POINTER(lenfilename_t),  # factorfile, in
        POINTER(c_int),  # factorfiletype, in
        POINTER(c_int),  # ntime, in
        POINTER(lenvartype_t),  # vartype(17), in
        POINTER(c_double),  # interpthresh, in
        POINTER(c_int),  # reapportion, in
        POINTER(c_double),  # nointerpval, in
        POINTER(c_int),  # npts, in
        POINTER(c_int),  # nproctime, out
        ndpointer(c_double, ndim=1),  # simtime(ntime), out
        ndpointer(c_double, ndim=2, flags="F"),  # simstate(ntime,npts), out
    )
    lib.interp_from_mf6_depvar_file.restype = c_int
