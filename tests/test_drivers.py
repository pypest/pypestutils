"""John Doherty's driver test programs."""
import numpy as np
import pandas as pd
import pytest

from pypestutils import enum
from pypestutils.pestutilslib import PestUtilsLib, PestUtilsLibError

from .common import data_dir


@pytest.mark.parametrize(
    "filein,fileout,isim,itype,exp_d",
    [
        pytest.param(
            "sva_tm.hds",
            "driver1a.out",
            31,
            1,
            {"iprec": 2, "narray": 4, "ntime": 4},
            id="a",
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
            {"iprec": 1, "narray": 16, "ntime": 3},
            id="c",
        ),
        pytest.param(
            "sva_tm_t.cbc",
            "driver1d.out",
            31,
            2,
            {"iprec": 2, "narray": 6, "ntime": 2},
            id="d",
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


@pytest.mark.parametrize(
    "spc, nlay, crd, depvar, ntime, fileout, exp_d",
    [
        pytest.param(
            "rect.spc",
            1,
            "wells.crd",
            "rect_sgl.hds",
            1,
            "heads_interp1_sgl.dat",
            {"nproctime": 1},
            id="a",
        ),
        pytest.param(
            "coast.spc",
            15,
            "coastwells.crd",
            "coast.hds",
            2,
            "coast_heads_wells.dat",
            {"nproctime": 2},
            id="b",
        ),
        pytest.param(
            "lockyer.spc",
            1,
            "lock_bore.csv",
            "lock.hds",
            4,
            "lock_heads_wells.dat",
            {"nproctime": 4},
            id="c",
        ),
    ],
)
def test_driver2(spc, nlay, crd, depvar, ntime, fileout, exp_d):
    flopy = pytest.importorskip("flopy")
    lib = PestUtilsLib()
    spc = flopy.discretization.StructuredGrid.from_gridspec(data_dir / spc)
    gridname = "grid1"
    icorner = 2  # driver1.f90 uses '1' but flopy's module uses lower left
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
    crd_pth = data_dir / crd
    if crd_pth.suffix == ".csv":
        crd_df = pd.read_csv(crd_pth, header=None)
    else:
        crd_df = pd.read_fwf(crd_pth, header=None)
    crd_df.columns = ["apoint", "ee", "nn", "layer"]
    crd_df.set_index("apoint", inplace=True)
    # print(crd_df)
    depvar_pth = data_dir / depvar
    isim = 1
    iprec = enum.Prec.single
    texttype = "head"
    interpthresh = 1e20
    nointerpval = 1.1e30
    res_d = lib.interp_from_structured_grid(
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
    simtime = res_d.pop("simtime")
    assert simtime.shape == (ntime,)
    simstate = res_d.pop("simstate")
    assert simstate.shape == (ntime, len(crd_df))
    assert exp_d == res_d
    lib.uninstall_structured_grid(gridname)
    lib.free_all_memory()
    exp_df = pd.read_fwf(
        data_dir / (fileout + ".std"),
        header=None,
        widths=[25, 16, 20],
    )
    exp_df.columns = ["apoint", "simtime", "simstate"]
    res_df = pd.DataFrame(
        {
            "apoint": crd_df.index.repeat(ntime),
            "simtime": np.tile(simtime, len(crd_df)),
            "simstate": simstate.ravel("F"),
        }
    )
    exp_df["apoint"] = exp_df["apoint"].str.lower()
    res_df["apoint"] = res_df["apoint"].str.lower()
    pd.testing.assert_frame_equal(exp_df, res_df)
