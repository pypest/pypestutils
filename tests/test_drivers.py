"""John Doherty's driver test programs."""
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from pypestutils import enum
from pypestutils.pestutilslib import PestUtilsLib

from .common import data_dir


def compare_files(pth1: Path, pth2: Path) -> None:
    text1 = pth1.read_text()
    text2 = pth2.read_text()
    lines1 = text1.splitlines(keepends=True)
    lines2 = text2.splitlines(keepends=True)
    assert len(lines1) == len(lines2)
    assert lines1 == lines2


def compare_factor_files(pth1: Path, pth2: Path, atol=1e-08) -> None:
    items1 = pth1.read_text().split()
    items2 = pth2.read_text().split()
    # line 1: name
    assert items1[0].lower() == items2[0].lower()
    # line 2: int1 int2
    assert items1[1:3] == items2[1:3]
    # the remainder can be converted to float and checked
    ar1 = np.array(items1[3:]).astype(float)
    ar2 = np.array(items2[3:]).astype(float)
    np.testing.assert_allclose(ar1, ar2, atol=atol)


def read_inquire_modflow_binary_file_specs_file(
    pth: Path, itype: int, isim: int
) -> pd.DataFrame:
    if pth.suffix == ".csv":
        return pd.read_csv(pth)
    if itype == 1:
        widths = [9, 10, 14, 18, 24, 10, 10, 10]
    elif itype == 2:
        widths = [9, 10, 18, 12, 10, 10, 10, 14, 19, 17]
        if isim in [31, 32]:
            widths += [22, 20, 20, 20]
    else:
        raise ValueError(itype)
    with pth.open() as fp:
        header_line = fp.readline()
    header_names = header_line.split()
    # print(header_line)
    # print(''.join([a.rjust(b) for a, b in zip(header_names, widths)]))
    assert len(widths) == len(header_names)
    return pd.read_fwf(pth, widths=widths)


# driver1 inputs
@pytest.mark.parametrize(
    "filein, fileout, isim, itype, inq_exp",
    [
        pytest.param(
            "sva_tm.hds",
            "driver1a.out",
            31,
            1,
            {"iprec": 2, "narray": 367, "ntime": 367},
            id="a",
            marks=pytest.mark.skipif(
                not (data_dir / "sva_tm.hds").exists(), reason="full"
            ),
        ),
        pytest.param(
            "sva_tm_r.hds",
            "driver1a_r.out",
            31,
            1,
            {"iprec": 2, "narray": 4, "ntime": 4},
            id="a_r",
        ),
        pytest.param(
            "PT01.hds",
            "driver1b.out",
            22,
            1,
            {"iprec": 1, "narray": 32, "ntime": 16},
            id="b",
        ),
        pytest.param(
            "PT01.cbb",
            "driver1c.out",
            22,
            2,
            {"iprec": 1, "narray": 94, "ntime": 16},
            marks=pytest.mark.skipif(
                not (data_dir / "PT01.cbb").exists(), reason="full"
            ),
            id="c",
        ),
        pytest.param(
            "PT01_r.cbb",
            "driver1c_r.out",
            22,
            2,
            {"iprec": 1, "narray": 16, "ntime": 3},
            id="c_r",
        ),
        pytest.param(
            "sva_tm_t.cbc",
            "driver1d.out",
            31,
            2,
            {"iprec": 2, "narray": 900, "ntime": 300},
            marks=pytest.mark.skipif(
                not (data_dir / "sva_tm_t.cbc").exists(), reason="full"
            ),
            id="d",
        ),
        pytest.param(
            "sva_tm_t_r.cbc",
            "driver1d_r.out",
            31,
            2,
            {"iprec": 2, "narray": 6, "ntime": 2},
            id="d_r",
        ),
        pytest.param(
            "ex-gwf-u1disv.hds",
            "driver1e.out",
            32,
            1,
            {"iprec": 2, "narray": 1, "ntime": 1},
            id="e",
        ),
        pytest.param(
            "ex-gwf-u1disv.cbc",
            "driver1f.out",
            32,
            2,
            {"iprec": 2, "narray": 4, "ntime": 1},
            id="f",
        ),
        pytest.param(
            "ex-gwf-sfr-p01b.sfr.bud",
            "driver1g.out",
            32,
            2,
            {"iprec": 2, "narray": 240, "ntime": 24},
            id="g",
        ),
    ],
)
def test_driver1(tmp_path, filein, fileout, isim, itype, inq_exp):
    lib = PestUtilsLib()
    filein_pth = data_dir / filein
    fileout_pth = tmp_path / fileout
    inq_res = lib.inquire_modflow_binary_file_specs(
        filein_pth, fileout_pth, isim, itype
    )
    res_df = pd.read_csv(fileout_pth)
    exp_df = read_inquire_modflow_binary_file_specs_file(
        data_dir / (fileout + ".std"), itype, isim
    )
    assert inq_res == inq_exp
    pd.testing.assert_frame_equal(res_df, exp_df)
    assert inq_res["narray"] == len(res_df)


def read_crd(pth: Path) -> pd.DataFrame:
    """Read coordinate file, return DataFrame."""
    if pth.suffix == ".csv":
        df = pd.read_csv(pth, header=None)
    else:
        df = pd.read_fwf(pth, header=None)
    df.columns = ["point", "ee", "nn", "layer"]
    df["point"] = df["point"].str.lower()
    return df.set_index("point")


def install_structured_grid(lib, spc_pth: Path, gridname: str, nlay: int) -> None:
    """Install structured grid from a spec file."""
    import flopy

    spc = flopy.discretization.StructuredGrid.from_gridspec(spc_pth)
    icorner = 2  # driver programs use '1' but flopy's module uses lower left
    lib.install_structured_grid(
        gridname,
        spc.ncol,
        spc.nrow,
        nlay,
        icorner,
        spc.xoffset,
        spc.yoffset,
        spc.angrot,
        spc.delr,
        spc.delc,
    )


def interp_from_structured_grid(
    lib, crd_df: pd.DataFrame, depvar_pth: Path, gridname: str, ntime: int
) -> dict:
    isim = 1
    iprec = enum.Prec.single
    texttype = "head"
    interpthresh = 1e20
    nointerpval = 1.1e30
    return lib.interp_from_structured_grid(
        gridname,
        depvar_pth,
        isim,
        iprec,
        ntime,
        texttype,
        interpthresh,
        nointerpval,
        crd_df.ee,
        crd_df.nn,
        crd_df.layer,
    )


# driver2 inputs
@pytest.mark.parametrize(
    "spc, nlay, crd, depvar, ntime, fileout",
    [
        pytest.param(
            "rect.spc",
            1,
            "wells.crd",
            "rect_sgl.hds",
            1,
            "heads_interp1_sgl.dat",
            id="a",
        ),
        pytest.param(
            "coast.spc",
            15,
            "coastwells.crd",
            "coast.hds",
            36,
            "coast_heads_wells.dat",
            marks=pytest.mark.skipif(
                not (data_dir / "coast.hds").exists(), reason="full"
            ),
            id="b",
        ),
        pytest.param(
            "coast.spc",
            15,
            "coastwells.crd",
            "coast_r.hds",
            4,
            "coast_heads_wells_r.dat",
            id="b_r",
        ),
        pytest.param(
            "lockyer.spc",
            1,
            "lock_bore.csv",
            "lock.hds",
            114,
            "lock_heads_wells.dat",
            marks=pytest.mark.skipif(
                not (data_dir / "lock.hds").exists(), reason="full"
            ),
            id="c",
        ),
        pytest.param(
            "lockyer.spc",
            1,
            "lock_bore.csv",
            "lock_r.hds",
            4,
            "lock_heads_wells_r.dat",
            id="c_r",
        ),
    ],
)
def test_driver2(spc, nlay, crd, depvar, ntime, fileout):
    pytest.importorskip("flopy")
    lib = PestUtilsLib()
    gridname = "grid1"
    install_structured_grid(lib, data_dir / spc, gridname, nlay)
    crd_df = read_crd(data_dir / crd)
    res_d = interp_from_structured_grid(lib, crd_df, data_dir / depvar, gridname, ntime)
    simtime = res_d.pop("simtime")
    assert simtime.shape == (ntime,)
    simstate = res_d.pop("simstate")
    assert simstate.shape == (ntime, len(crd_df))
    assert res_d["nproctime"] == ntime
    lib.uninstall_structured_grid(gridname)
    lib.free_all_memory()
    pts_df = pd.read_fwf(
        data_dir / (fileout + ".std"),
        header=None,
        widths=[25, 16, 20],
        names=["point", "simtime", "simstate"],
    )
    res_df = pd.DataFrame(
        {
            "point": crd_df.index.repeat(ntime),
            "simtime": np.tile(simtime, len(crd_df)),
            "simstate": simstate.ravel("F"),
        }
    )
    pts_df["point"] = pts_df["point"].str.lower()
    res_df["point"] = res_df["point"].str.lower()
    pd.testing.assert_frame_equal(pts_df, res_df)


# driver3 inputs
@pytest.mark.parametrize(
    "spc, nlay, crd, depvar, ntime, obsdat, time_extrap, fileout",
    [
        pytest.param(
            "coast.spc",
            15,
            "coastwells.crd",
            "coast.hds",
            36,
            "coastwells_meastime.dat",
            50,
            "coast_heads_wells_time_interp.dat",
            marks=pytest.mark.skipif(
                not (data_dir / "coast.hds").exists(), reason="full"
            ),
            id="a",
        ),
        pytest.param(
            "coast.spc",
            15,
            "coastwells.crd",
            "coast_r.hds",
            4,
            "coastwells_meastime_r.dat",
            50,
            "coast_heads_wells_time_interp_r.dat",
            id="a_r",
        ),
        pytest.param(
            "lockyer.spc",
            1,
            "lock_bore.csv",
            "lock.hds",
            114,
            "lockwells_meastime.dat",
            2.0,
            "lock_heads_wells_time_interp.dat",
            marks=pytest.mark.skipif(
                not (data_dir / "lock.hds").exists(), reason="full"
            ),
            id="b",
        ),
        pytest.param(
            "lockyer.spc",
            1,
            "lock_bore.csv",
            "lock_r.hds",
            4,
            "lockwells_meastime.dat",
            2.0,
            "lock_heads_wells_time_interp_r.dat",
            id="b_r",
        ),
    ],
)
def test_driver3(spc, nlay, crd, depvar, ntime, obsdat, time_extrap, fileout):
    pytest.importorskip("flopy")
    lib = PestUtilsLib()
    gridname = "grid1"
    install_structured_grid(lib, data_dir / spc, gridname, nlay)
    crd_df = read_crd(data_dir / crd)
    res_d = interp_from_structured_grid(lib, crd_df, data_dir / depvar, gridname, ntime)
    lib.uninstall_structured_grid(gridname)
    nproctime = res_d["nproctime"]
    simtime = res_d["simtime"]
    simstate = res_d["simstate"]
    obsdat_df = pd.read_fwf(data_dir / obsdat, header=None, widths=[15, 20])
    obsdat_df.columns = ["point", "time"]
    obsdat_df["point"] = obsdat_df["point"].str.lower()
    # mapping between 0-based index and point name
    point_idx = {v: k for k, v in crd_df.reset_index().point.to_dict().items()}
    obsdat_df["num"] = -1
    sel = obsdat_df.point.isin(point_idx.keys())
    obsdat_df.loc[sel, "num"] = obsdat_df.loc[sel, "point"].map(point_idx)
    interpthresh = 1e20
    nointerpval = 1.1e30
    how_extrap = "L"  # linear
    obsdat_df["value"] = lib.interp_to_obstime(
        nproctime,
        simtime,
        simstate,
        interpthresh,
        how_extrap,
        time_extrap,
        nointerpval,
        obsdat_df["num"],
        obsdat_df["time"],
    )
    lib.free_all_memory()
    exp_df = pd.read_fwf(
        data_dir / (fileout + ".std"),
        header=None,
        widths=[25, 16, 30],
        names=["point", "time", "value"],
    )
    # print(exp_df.head())
    # print(obsdat_df.head())
    pd.testing.assert_frame_equal(exp_df, obsdat_df[exp_df.columns])


# driver4 inputs
@pytest.mark.parametrize(
    "grbfile, inst_exp, crd, factorfile, blnfile, nointerp_idx, depvar, ntime, interpthresh, interp_exp",
    [
        pytest.param(
            "hd1h.dis.grb",
            {"idis": 2, "ncells": 11976, "ndim1": 11976, "ndim2": 1, "ndim3": 1},
            "hd1h_wellcoords.csv",
            "hd1h.fac",
            "hd1h.bln",
            [218],
            "hd1h.hds",
            644,
            1e30,
            "hd1h_wells_sim.dat",
            marks=pytest.mark.skipif(
                not (data_dir / "hd1h.hds").exists(), reason="full"
            ),
            id="a",
        ),
        pytest.param(
            "hd1h.dis.grb",
            {"idis": 2, "ncells": 11976, "ndim1": 11976, "ndim2": 1, "ndim3": 1},
            "hd1h_wellcoords.csv",
            "hd1h.fac",
            "hd1h.bln",
            [218],
            "hd1h_r.hds",
            3,
            1e30,
            "hd1h_wells_sim_r.dat",
            id="a_r",
        ),
        pytest.param(
            "vdl.disv.grb",
            {"idis": 2, "ncells": 2751, "ndim1": 2751, "ndim2": 1, "ndim3": 1},
            "vdl_wells.csv",
            "vdl.fac",
            "vdl_interp.bln",
            [],
            "vdl.hds",
            4,
            1e20,
            "vdl_well_heads.dat",
            id="b",
        ),
    ],
)
def test_driver4(
    tmp_path,
    grbfile,
    inst_exp,
    crd,
    factorfile,
    blnfile,
    nointerp_idx,
    depvar,
    ntime,
    interpthresh,
    interp_exp,
):
    lib = PestUtilsLib()
    # option 1
    gridname = "grid1"
    grb_pth = data_dir / grbfile
    inst_res = lib.install_mf6_grid_from_file(gridname, grb_pth)
    assert inst_res == inst_exp
    # option 3
    crd_df = read_crd(data_dir / crd)
    # option 4
    factorfile_pth = tmp_path / factorfile
    blnfile_pth = tmp_path / blnfile
    interp_success = lib.calc_mf6_interp_factors(
        gridname,
        crd_df.ee,
        crd_df.nn,
        crd_df.layer,
        factorfile_pth,
        enum.FactorFileType.text,
        blnfile_pth,
    )
    assert len(interp_success) == len(crd_df)
    # check position of failures
    np.testing.assert_array_equal(np.where(interp_success == 0)[0], nointerp_idx)
    compare_factor_files(data_dir / (factorfile + ".std"), factorfile_pth)
    compare_files(data_dir / (blnfile + ".std"), blnfile_pth)
    # option 5
    vartype = "head"
    reapportion = 1  # yes / True
    nointerpval = 1e30
    npts = len(crd_df)
    interp_res = lib.interp_from_mf6_depvar_file(
        data_dir / depvar,
        factorfile_pth,
        enum.FactorFileType.text,
        ntime,
        vartype,
        interpthresh,
        reapportion,
        nointerpval,
        npts,
    )
    simtime = interp_res.pop("simtime")
    assert simtime.shape == (ntime,)
    simstate = interp_res.pop("simstate")
    assert simstate.shape == (ntime, len(crd_df))
    assert interp_res["nproctime"] == ntime
    # pd.read_fwf is slow!
    pts_ar = np.loadtxt(
        data_dir / (interp_exp + ".std"),
        dtype=[("point", "U20"), ("simtime", float), ("simstate", float)],
    )
    np.testing.assert_array_equal(pts_ar["point"], crd_df.index.repeat(ntime))
    np.testing.assert_allclose(pts_ar["simtime"], np.tile(simtime, len(crd_df)))
    np.testing.assert_allclose(pts_ar["simstate"], simstate.ravel("F"))
    # option 6 - not tested by driver
    # option 2
    lib.uninstall_mf6_grid(gridname)


# driver5 inputs
@pytest.mark.parametrize(
    "ncell, isim, zonefile, nzone, cbcfile, inq_exp, flowtype, ext_exp, obs_exp",
    [
        pytest.param(
            120_000,
            31,
            "sop_zones.dat",
            10,
            "sop.cbc",
            {"iprec": 2, "narray": 2, "ntime": 1, "fileout": "sop_flow_contents.dat"},
            "chd",
            {"numzone": 4, "nproctime": 1, "fileout": "sop_chd_flows.dat"},
            None,
            id="a",
        ),
        pytest.param(
            300,
            31,
            "inflow_zones.dat",
            4,
            "ex-gwf-sfr-p01b.sfr.bud",
            {"iprec": 2, "narray": 240, "ntime": 24},
            "inflow",
            {"numzone": 2, "nproctime": 24, "fileout": "inflows.dat"},
            None,
            id="b",
        ),
        pytest.param(
            29_113_440,
            32,
            "rchzones.dat",
            20,
            "ex-gwf-disvmesh.cbc",
            {"iprec": 2, "narray": 4, "ntime": 1},
            "rch",
            {"numzone": 7, "nproctime": 1, "fileout": "rchflow.dat"},
            None,
            id="c",
        ),
        pytest.param(
            1200,
            22,
            "wellzone.dat",
            20,
            "umodel_usg_wel.cbc",
            {"iprec": 1, "narray": 151, "ntime": 151},
            "wel",
            {"numzone": 12, "nproctime": 151, "fileout": "umodel_wellflow.dat"},
            {"obsfile": "umodel_obstimes.dat", "fileout": "umodel_wellflow_interp.dat"},
            id="d",
        ),
        pytest.param(
            3_705_856,
            1,
            "nfsegzone.dat",
            3,
            "nfseg.cbb",
            {"iprec": 1, "narray": 4, "ntime": 2, "fileout": "nfseg.cbb.contents"},
            "rech",
            {"numzone": 3, "nproctime": 2, "fileout": "nfseg_rech.dat"},
            None,
            id="e",
        ),
    ],
)
def test_driver5(
    tmp_path, ncell, isim, zonefile, nzone, cbcfile, inq_exp, flowtype, ext_exp, obs_exp
):
    lib = PestUtilsLib()
    itype = 2  # cell-by-cell flows
    if "fileout" in inq_exp:
        inqfile = inq_exp.pop("fileout")
        res_inqfile = tmp_path / inqfile
    else:
        res_inqfile = inqfile = ""
    cbc_pth = data_dir / cbcfile
    inq_res = lib.inquire_modflow_binary_file_specs(cbc_pth, res_inqfile, isim, itype)
    assert inq_res == inq_exp
    if inqfile:
        res_df = pd.read_csv(res_inqfile)
        exp_df = read_inquire_modflow_binary_file_specs_file(
            data_dir / (inqfile + ".std"), itype, isim
        )
        pd.testing.assert_frame_equal(res_df, exp_df)
    iprec = inq_res["iprec"]
    ntime = inq_res["ntime"]
    zonedat = np.loadtxt(data_dir / zonefile, dtype="i").T
    zone = np.unique(zonedat[1])
    nzone = len(zone)
    izone = np.zeros(ncell, np.int32)
    izone[zonedat[0] - 1] = zonedat[1]
    ext_res = lib.extract_flows_from_cbc_file(
        cbc_pth,
        flowtype,
        isim,
        iprec,
        izone,
        nzone,
        ntime,
    )
    header = ["ZONENUMBER", "KSTP", "KPER", "SIMTIME", "SIMFLOW"]
    dtypes = np.dtype(list(zip(header, [int] * 3 + [float] * 2)))
    with (data_dir / (ext_exp.pop("fileout") + ".std")).open("r") as fp:
        assert fp.readline().split() == header
        exp_ar = np.loadtxt(fp, dtypes)
    # print(pd.DataFrame(exp_ar))
    zonenumber = ext_res.pop("zonenumber")
    timestep = ext_res.pop("timestep")
    stressperiod = ext_res.pop("stressperiod")
    simtime = ext_res.pop("simtime")
    simflow = ext_res.pop("simflow")
    assert ext_res == ext_exp
    np.testing.assert_array_equal(exp_ar["ZONENUMBER"], np.repeat(zonenumber, ntime))
    np.testing.assert_array_equal(exp_ar["KSTP"], np.tile(timestep, nzone))
    np.testing.assert_array_equal(exp_ar["KPER"], np.tile(stressperiod, nzone))
    np.testing.assert_allclose(exp_ar["SIMTIME"], np.tile(simtime, nzone))
    np.testing.assert_allclose(exp_ar["SIMFLOW"], simflow.ravel("F"))
    if obs_exp is None:
        return
    nproctime = ext_res["nproctime"]
    obsdat_pth = data_dir / obs_exp["obsfile"]
    obsdat = np.loadtxt(obsdat_pth, dtype=[("zone", "U15"), ("time", float)])
    assert np.char.startswith(obsdat["zone"], "zone").all()
    obszone = np.char.replace(obsdat["zone"], "zone", "").astype(int)
    zonenumber_l = zonenumber.tolist()
    obszonenum = -np.ones(len(obsdat), np.int32)
    sel = np.isin(obszone, zonenumber)
    obszonenum[sel] = np.vectorize(lambda x: zonenumber_l.index(x))(obszone[sel])
    interpthresh = 1.0e300
    how_extrap = "c"
    time_extrap = 0.0
    nointerpval = 1e30
    obsdat_df = pd.DataFrame(obsdat)
    obsdat_df["value"] = lib.interp_to_obstime(
        nproctime,
        simtime,
        simflow,
        interpthresh,
        how_extrap,
        time_extrap,
        nointerpval,
        obszonenum,
        obsdat["time"],
    )
    lib.free_all_memory()
    exp_df = pd.read_fwf(
        data_dir / (obs_exp["fileout"] + ".std"),
        header=None,
        widths=[25, 16, 30],
        names=["zone", "time", "value"],
    )
    pd.testing.assert_frame_equal(exp_df, obsdat_df)


def test_driver6(tmp_path):
    flopy = pytest.importorskip("flopy")
    spc_pth = data_dir / "rectmodel.spc"
    spc = flopy.discretization.StructuredGrid.from_gridspec(spc_pth)
    lib = PestUtilsLib()
    aa = np.loadtxt(data_dir / "aa.ref", dtype=float)
    mean = np.loadtxt(data_dir / "mean.ref", dtype=float)
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
    icount_interp = lib.calc_kriging_factors_2d(
        npts["ecs"],
        npts["ncs"],
        npts["zns"],
        ect,
        nct,
        znt,
        vartype,
        krigtype,
        aa,
        anis,
        bearing,
        searchrad,
        maxpts,
        minpts,
        factorfile_pth,
        factorfiletype,
    )
    assert icount_interp == 19_895
    compare_factor_files(data_dir / (factorfile + ".std"), factorfile_pth, 1e-6)
    mpts = len(mean)
    transtype = 1  # "l" or "log"
    nointerpval = 1.1e30
    krige_res = lib.krige_using_file(
        factorfile_pth,
        factorfiletype,
        mpts,
        krigtype,
        transtype,
        npts["vals"],
        mean,
        nointerpval,
    )
    valt = krige_res["targval"]
    assert krige_res["icount_interp"] == 19_895
    valt_std = np.loadtxt(data_dir / "interpolated.ref.std")
    np.testing.assert_allclose(valt, valt_std, atol=8e-5)


def test_driver7(tmp_path):
    flopy = pytest.importorskip("flopy")
    spc_pth = data_dir / "rectmodel.spc"
    spc = flopy.discretization.StructuredGrid.from_gridspec(spc_pth)
    lib = PestUtilsLib()
    mean = np.loadtxt(data_dir / "mean.ref", dtype=float)
    anis = np.loadtxt(data_dir / "anis.ref", dtype=float)
    bearing = np.loadtxt(data_dir / "bearing.ref", dtype=float)
    znt = np.loadtxt(data_dir / "zones.inf", dtype=int)
    krigtype = 0  # enum.KrigType.simple
    factorfile = "factors1.dat"
    factorfile_pth = tmp_path / factorfile
    factorfiletype = 1  # enum.FactorFileType.text
    ect = spc.get_xcellcenters_for_layer(0).ravel("C")
    nct = spc.get_ycellcenters_for_layer(0).ravel("C")
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
    icount_interp = lib.calc_kriging_factors_auto_2d(
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
    assert icount_interp == 19_895
    compare_factor_files(data_dir / (factorfile + ".std"), factorfile_pth, 1e-5)
    mpts = len(mean)
    transtype = 0  # "n" or "natural"
    nointerpval = 1.1e30
    krige_res = lib.krige_using_file(
        factorfile_pth,
        factorfiletype,
        mpts,
        krigtype,
        transtype,
        npts["vals"],
        mean,
        nointerpval,
    )
    valt = krige_res["targval"]
    assert krige_res["icount_interp"] == 19_895
    valt_std = np.loadtxt(data_dir / "interpolated1.ref.std")
    np.testing.assert_allclose(valt, valt_std, atol=5e-5)
