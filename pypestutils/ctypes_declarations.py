"""Low-level Fortran-Python ctypes functions."""

from ctypes import ARRAY, CDLL, POINTER, c_char, c_double, c_int

from numpy.ctypeslib import ndpointer

# Cache variables by uppercase dimvar name
_dimvar_cache = {}
_char_array_cache = {}


def get_dimvar_int(lib: CDLL, name: str) -> int:
    """Get dimvar constant integer from library instance."""
    try:
        return _dimvar_cache[name]
    except KeyError:
        pass
    c_var = c_int.in_dll(lib, name)
    _dimvar_cache[name] = c_var.value
    return c_var.value


def get_char_array(lib: CDLL, name: str):
    """Get c_char array with a fixed size from dimvar."""
    try:
        return _char_array_cache[name]
    except KeyError:
        pass
    array_type = ARRAY(c_char, get_dimvar_int(lib, name))
    _char_array_cache[name] = array_type
    return array_type


def prototype(lib):
    """Add ctypes prototypes for each function."""
    # Generate c_char Array types based on dimvar sizes
    lenfilename_t = get_char_array(lib, "LENFILENAME")
    lenmessage_t = get_char_array(lib, "LENMESSAGE")
    lengridname_t = get_char_array(lib, "LENGRIDNAME")

    # inquire_modflow_binary_file_specs(
    #   FileIn,FileOut,isim,itype,iprec,narray,ntime)
    lib.inquire_modflow_binary_file_specs.restype = c_int
    lib.inquire_modflow_binary_file_specs.argtypes = (
        POINTER(lenfilename_t),  # FileIn, in
        POINTER(lenfilename_t),  # FileOut, in
        POINTER(c_int),  # isim, in
        POINTER(c_int),  # itype, in
        POINTER(c_int),  # iprec, out
        POINTER(c_int),  # narray, out
        POINTER(c_int),  # ntime, out
    )

    # retrieve_error_message(errormessage)
    lib.retrieve_error_message.restype = c_int
    lib.retrieve_error_message.argtypes = (
        POINTER(lenmessage_t),  # errormessage, out
    )

    # install_structured_grid(
    #   gridname,ncol,nrow,nlay,icorner,e0,n0,rotation,delr,delc)
    lib.install_structured_grid.restype = c_int
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

    # uninstall_structured_grid(gridname)
    lib.uninstall_structured_grid.restype = c_int
    lib.uninstall_structured_grid.argtypes = (
        POINTER(lengridname_t),  # gridname, in
    )

    # free_all_memory()
    lib.free_all_memory.restype = c_int
    lib.free_all_memory.argtypes = ()

    # install_mf6_grid_from_file(
    #   gridname,grbfile,idis,ncells,ndim1,ndim2,ndim3)
    lib.install_mf6_grid_from_file.restype = c_int
    lib.install_mf6_grid_from_file.argtypes = (
        POINTER(lengridname_t),  # gridname, in
        POINTER(lenfilename_t),  # grbfile, in
        POINTER(c_int),  # idis, out
        POINTER(c_int),  # ncells, out
        POINTER(c_int),  # ndim1, out
        POINTER(c_int),  # ndim2, out
        POINTER(c_int),  # ndim3, out
    )

    # uninstall_mf6_grid(gridname)
    lib.uninstall_mf6_grid.restype = c_int
    lib.uninstall_mf6_grid.argtypes = (
        POINTER(lengridname_t),  # gridname, in
    )

    # calc_mf6_interp_factors(
    #   gridname,npts,ecoord,ncoord,layer,factorfile,
    #   factorfiletype,blnfile,interp_success)
    lib.calc_mf6_interp_factors.restype = c_int
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
