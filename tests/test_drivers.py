"""John Doherty's driver test programs."""
import pytest
import pandas as pd

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
