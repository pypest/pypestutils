"""Test previous crash scenarios."""
import numpy as np
import pytest

from pypestutils.pestutilslib import PestUtilsLib

from .common import data_dir


def test_kb2d_1(tmp_path):
    """This was a Fatal Python error: Segmentation fault
    gdb trace:
    0x00007fffde30ca80 in kb2d_1 (n_ndmin=1, n_ndmax=6, r_radius=2216.427,
        k_ktype=0, s_skmean=0, n_nst=1, c_c0=0, i_it=..., c_cc=..., a_ang=...,
        a_aa=..., a_anis=..., unestimated=0, n_npts=1, n_ndat=6, inumdat=...,
        icellno=..., epoint=..., npoint=..., aoutfile=..., outunit=10,
        pmx=10000, x=..., y=..., af1=..., af2=..., _aoutfile=1, _af1=10,
        _af2=10) at ../pestutils/sgsim_code.f90:317
    317	                  dist(isam) = 1.0e+20
    """
    flopy = pytest.importorskip("flopy")
    spc_pth = data_dir / "rectmodel.spc"
    spc = flopy.discretization.StructuredGrid.from_gridspec(spc_pth)
    lib = PestUtilsLib()
    aa = np.loadtxt(data_dir / "aa.ref", dtype=float)
    anis = np.loadtxt(data_dir / "anis.ref", dtype=float)
    bearing = np.loadtxt(data_dir / "bearing.ref", dtype=float)
    znt = np.loadtxt(data_dir / "zones.inf", dtype=int)
    npts = np.loadtxt(
        data_dir / "pp.dat",
        dtype=[
            ("point", "U8"),
            ("ecs", float),
            ("ncs", float),
            ("zns", int),
            ("vals", float),
        ],
    )
    vartype = 1  # enum.VarioType.spher
    searchrad = 1e20
    minpts = 10
    maxpts = 20
    krigtype = 1  # enum.KrigType.ordinary
    factorfile = "factors.dat"
    factorfile_pth = tmp_path / factorfile
    factorfiletype = 1  # enum.FactorFileType.text
    ect = spc.get_xcellcenters_for_layer(0).ravel("C")
    nct = spc.get_ycellcenters_for_layer(0).ravel("C")
    _ = lib.calc_kriging_factors_2d(
        npts["ecs"],
        npts["ncs"],
        npts["zns"],
        ect.copy(),
        nct.copy(),
        znt.copy(),
        vartype,
        krigtype,
        aa.copy(),
        anis.copy(),
        bearing.copy(),
        searchrad,
        maxpts,
        minpts,
        factorfile_pth,
        factorfiletype,
    )
    # crash only happened when this is called first, which deallocated dist
    lib.free_all_memory()
    # crash happened next, when assigment to dist was attempted
    _ = lib.calc_kriging_factors_auto_2d(
        npts["ecs"],
        npts["ncs"],
        npts["zns"],
        ect,
        nct,
        znt,
        krigtype,
        anis,
        bearing,
        factorfile_pth,
        factorfiletype,
    )
