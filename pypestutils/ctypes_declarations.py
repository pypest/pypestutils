"""Low-level Fortran-Python ctypes functions."""
from __future__ import annotations

from ctypes import ARRAY, CDLL, POINTER, c_char, c_double, c_int

from numpy.ctypeslib import ndpointer

# Cache variables by uppercase dimvar name
_dimvar_cache = {}
_char_array_cache = {}
# other lengths not defined in dimvar
_misc_lengths = {
    "LENVARTYPE": 17,
    "LENFLOWTYPE": 17,
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
    lenflowtype_t = get_char_array(lib, "LENFLOWTYPE")

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

    # extract_flows_from_cbc_file(
    #   cbcfile,flowtype,isim,iprec,ncell,izone,nzone,numzone,zonenumber,
    #   ntime,nproctime,timestep,stressperiod,simtime,simflow)
    lib.extract_flows_from_cbc_file.argtypes = (
        POINTER(lenfilename_t),  # cbcfile, in
        POINTER(lenflowtype_t),  # flowtype, in
        POINTER(c_int),  # isim, in
        POINTER(c_int),  # iprec, in
        POINTER(c_int),  # ncell, in
        ndpointer(c_int, ndim=1),  # izone(ncell), in
        POINTER(c_int),  # nzone, in
        POINTER(c_int),  # numzone, out
        ndpointer(c_int, ndim=1),  # zonenumber(nzone), out
        POINTER(c_int),  # ntime, in
        POINTER(c_int),  # nproctime, out
        ndpointer(c_int, ndim=1),  # timestep(ntime), out
        ndpointer(c_int, ndim=1),  # stressperiod(ntime), out
        ndpointer(c_double, ndim=1),  # simtime(ntime), out
        ndpointer(c_double, ndim=2, flags="F"),  # simflow(ntime,nzone), out
    )
    lib.extract_flows_from_cbc_file.restype = c_int

    # calc_kriging_factors_2d(
    #   npts,ecs,ncs,zns,mpts,ect,nct,znt,vartype,krigtype,aa,anis,bearing,
    #   searchrad,maxpts,minpts,factorfile,factorfiletype,icount_interp)
    lib.calc_kriging_factors_2d.argtypes = (
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # ecs(npts), in
        ndpointer(c_double, ndim=1),  # ncs(npts), in
        ndpointer(c_int, ndim=1),  # zns(npts), in
        POINTER(c_int),  # mpts, in
        ndpointer(c_double, ndim=1),  # ect(mpts), in
        ndpointer(c_double, ndim=1),  # nct(mpts), in
        ndpointer(c_int, ndim=1),  # znt(mpts), in
        POINTER(c_int),  # vartype, in
        POINTER(c_int),  # krigtype, in
        ndpointer(c_double, ndim=1),  # aa(mpts), in
        ndpointer(c_double, ndim=1),  # anis(mpts), in
        ndpointer(c_double, ndim=1),  # bearing(mpts), in
        POINTER(c_double),  # searchrad, in
        POINTER(c_int),  # maxpts, in
        POINTER(c_int),  # minpts, in
        POINTER(lenfilename_t),  # factorfile, in
        POINTER(c_int),  # factorfiletype, in
        POINTER(c_int),  # icount_interp, out
    )
    lib.calc_kriging_factors_2d.restype = c_int

    # calc_kriging_factors_auto_2d(
    #   npts,ecs,ncs,zns,mpts,ect,nct,znt,krigtype,anis,bearing,
    #   factorfile,factorfiletype,icount_interp)
    lib.calc_kriging_factors_auto_2d.argtypes = (
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # ecs(npts), in
        ndpointer(c_double, ndim=1),  # ncs(npts), in
        ndpointer(c_int, ndim=1),  # zns(npts), in
        POINTER(c_int),  # mpts, in
        ndpointer(c_double, ndim=1),  # ect(mpts), in
        ndpointer(c_double, ndim=1),  # nct(mpts), in
        ndpointer(c_int, ndim=1),  # znt(mpts), in
        POINTER(c_int),  # krigtype, in
        ndpointer(c_double, ndim=1),  # anis(mpts), in
        ndpointer(c_double, ndim=1),  # bearing(mpts), in
        POINTER(lenfilename_t),  # factorfile, in
        POINTER(c_int),  # factorfiletype, in
        POINTER(c_int),  # icount_interp, out
    )
    lib.calc_kriging_factors_auto_2d.restype = c_int

    # calc_kriging_factors_3d(
    #   npts,ecs,ncs,zcs,zns,mpts,ect,nct,zct,znt,krigtype,nzone,zonenum,
    #   vartype,ahmax,ahmin,avert,bearing,dip,rake,srhmax,srhmin,srvert,
    #   maxpts,minpts,factorfile,factorfiletype,icount_interp)
    lib.calc_kriging_factors_3d.argtypes = (
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # ecs(npts), in
        ndpointer(c_double, ndim=1),  # ncs(npts), in
        ndpointer(c_double, ndim=1),  # zcs(npts), in
        ndpointer(c_int, ndim=1),  # zns(npts), in
        POINTER(c_int),  # mpts, in
        ndpointer(c_double, ndim=1),  # ect(mpts), in
        ndpointer(c_double, ndim=1),  # nct(mpts), in
        ndpointer(c_double, ndim=1),  # zct(mpts), in
        ndpointer(c_int, ndim=1),  # znt(mpts), in
        POINTER(c_int),  # krigtype, in
        POINTER(c_int),  # nzone, in
        ndpointer(c_int, ndim=1),  # zonenum(nzone), in
        ndpointer(c_int, ndim=1),  # vartype(nzone), in
        ndpointer(c_double, ndim=1),  # ahmax(nzone), in
        ndpointer(c_double, ndim=1),  # ahmin(nzone), in
        ndpointer(c_double, ndim=1),  # avert(nzone), in
        ndpointer(c_double, ndim=1),  # bearing(nzone), in
        ndpointer(c_double, ndim=1),  # dip(nzone), in
        ndpointer(c_double, ndim=1),  # rake(nzone), in
        POINTER(c_double),  # srhmax, in
        POINTER(c_double),  # srhmin, in
        POINTER(c_double),  # srvert, in
        POINTER(c_int),  # maxpts, in
        POINTER(c_int),  # minpts, in
        POINTER(lenfilename_t),  # factorfile, in
        POINTER(c_int),  # factorfiletype, in
        POINTER(c_int),  # icount_interp, out
    )
    lib.calc_kriging_factors_3d.restype = c_int

    # krige_using_file(
    #   factorfile,factorfiletype,npts,mpts,krigtype,transtype,
    #   sourceval,targval,icount_interp,meanval)
    lib.krige_using_file.argtypes = (
        POINTER(lenfilename_t),  # factorfile, in
        POINTER(c_int),  # factorfiletype, in
        POINTER(c_int),  # npts, in
        POINTER(c_int),  # mpts, in
        POINTER(c_int),  # krigtype, in
        POINTER(c_int),  # transtype, in
        ndpointer(c_double, ndim=1),  # sourceval(npts), in
        ndpointer(c_double, ndim=1),  # targval(mpts), out
        POINTER(c_int),  # icount_interp, out
        ndpointer(c_double, ndim=1),  # meanval(mpts), in, optional
    )
    lib.krige_using_file.restype = c_int

    # build_covar_matrix_2d(
    #   npts,ec,nc,zn,vartype,nugget,aa,sill,anis,bearing,ldcovmat,covmat)
    lib.build_covar_matrix_2d.argtypes = (
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # ec(npts), in
        ndpointer(c_double, ndim=1),  # nc(npts), in
        ndpointer(c_int, ndim=1),  # zn(npts), in
        POINTER(c_int),  # vartype, in
        ndpointer(c_double, ndim=1),  # nugget(npts), in
        ndpointer(c_double, ndim=1),  # aa(npts), in
        ndpointer(c_double, ndim=1),  # sill(npts), in
        ndpointer(c_double, ndim=1),  # anis(npts), in
        ndpointer(c_double, ndim=1),  # bearing(npts), in
        POINTER(c_int),  # ldcovmat, in
        ndpointer(c_double, ndim=2, flags="F"),  # covmat(ldcovmat,npts), out
    )
    lib.build_covar_matrix_2d.restype = c_int

    # build_covar_matrix_3d(
    #   npts,ec,nc,zc,zn,vartype,
    # nugget,sill,ahmax,ahmin,avert,bearing,dip,rake,ldcovmat,covmat)
    lib.build_covar_matrix_3d.argtypes = (
        POINTER(c_int),  # npts, in
        ndpointer(c_double, ndim=1),  # ec(npts), in
        ndpointer(c_double, ndim=1),  # nc(npts), in
        ndpointer(c_double, ndim=1),  # zc(npts), in
        ndpointer(c_int, ndim=1),  # zn(npts), in
        POINTER(c_int),  # vartype, in
        ndpointer(c_double, ndim=1),  # nugget(npts), in
        ndpointer(c_double, ndim=1),  # sill(npts), in
        ndpointer(c_double, ndim=1),  # ahmax(npts), in
        ndpointer(c_double, ndim=1),  # ahmin(npts), in
        ndpointer(c_double, ndim=1),  # avert(npts), in
        ndpointer(c_double, ndim=1),  # bearing(npts), in
        ndpointer(c_double, ndim=1),  # dip(npts), in
        ndpointer(c_double, ndim=1),  # rake(npts), in
        POINTER(c_int),  # ldcovmat, in
        ndpointer(c_double, ndim=2, flags="F"),  # covmat(ldcovmat,npts), out
    )
    lib.build_covar_matrix_3d.restype = c_int
