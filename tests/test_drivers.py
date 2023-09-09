"""John Doherty's driver test programs."""
from pathlib import Path
import numpy as np
import pandas as pd
import pytest

from pypestutils import enum
from pypestutils.pestutilslib import PestUtilsLib

from .common import data_dir


# driver1 inputs
@pytest.mark.parametrize(
    "filein, fileout, isim, itype, exp_d",
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
def test_driver1(tmp_path, filein, fileout, isim, itype, exp_d):
    lib = PestUtilsLib()
    filein_pth = data_dir / filein
    fileout_pth = tmp_path / fileout
    res_d = lib.inquire_modflow_binary_file_specs(filein_pth, fileout_pth, isim, itype)
    res_df = pd.read_csv(fileout_pth)
    if itype == 1:
        widths = [9, 10, 14, 18, 24, 10, 10, 10]
    elif itype == 2:
        widths = [9, 10, 18, 12, 10, 10, 10, 14, 19, 17]
        if isim in [31, 32]:
            widths += [22, 20, 20, 20]
    else:
        raise ValueError(itype)
    exp_fileout_pth = data_dir / (fileout + ".std")
    with exp_fileout_pth.open() as fp:
        header_line = fp.readline()
    header_names = header_line.split()
    # print(header_line)
    # print(''.join([a.rjust(b) for a, b in zip(header_names, widths)]))
    assert len(widths) == len(header_names)
    exp_df = pd.read_fwf(exp_fileout_pth, widths=widths)
    # print(fileout_pth)
    # print(exp_df)
    assert res_d == exp_d
    pd.testing.assert_frame_equal(res_df, exp_df)
    assert res_d["narray"] == len(res_df)


def read_crd(pth: Path) -> pd.DataFrame:
    """Read coordinate file, return DataFrame."""
    if pth.suffix == ".csv":
        df = pd.read_csv(pth, header=None)
    else:
        df = pd.read_fwf(pth, header=None)
    df.columns = ["point", "ee", "nn", "layer"]
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
    )
    pts_df.columns = ["point", "simtime", "simstate"]
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
    )
    exp_df.columns = ["point", "time", "value"]
    exp_df["point"] = exp_df["point"].str.lower()
    obsdat_df["point"] = obsdat_df["point"].str.lower()
    # print(exp_df.head())
    # print(obsdat_df.head())
    pd.testing.assert_frame_equal(exp_df, obsdat_df[exp_df.columns])
