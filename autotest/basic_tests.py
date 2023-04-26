import os
import platform
import shutil
import numpy as np
import pandas as pd
import pyemu


bin_path = None
lib_path = None
if "darwin" in platform.platform().lower() or "mac" in platform.platform().lower():
    bin_path = os.path.join("bin","mac")
    lib_path = os.path.join("..","builddir","src","libppu.dylib")
elif "win" in platform.platform().lower():
    bin_path = os.path.join("bin","win")
    lib_path = os.path.join("..","builddir","src","libppu.dll")
else:
    bin_path = os.path.join("bin","linux")
    lib_path = os.path.join("..","builddir","src","libppu.so")


def structured_freyberg_invest():
    test_d = 'freyberg_structured_invest'
    if os.path.exists(test_d):
        shutil.rmtree(test_d)
    shutil.copytree("freyberg_structured","freyberg_structured_invest")
    for f in os.listdir(bin_path):
        shutil.copy2(os.path.join(bin_path,f),os.path.join(test_d,f))
    lib_name = os.path.split(lib_path)[-1]
    shutil.copy2(lib_path,os.path.join(test_d,lib_name)) 
    pyemu.os_utils.run("mf6",cwd=test_d)
    nrow = 40
    ncol = 20
    nlay = 3
    delc = np.zeros(nrow) + 250
    delr = np.zeros(ncol) + 250
    xll,yll = 0,0
    gs_fname = os.path.join(test_d,"grid.spc")
    pyemu.helpers.SpatialReference(delc=delc,delr=delr,xll=xll,yll=yll).write_gridspec(gs_fname)
    grb_fname = os.path.join(test_d,"freyberg6.dis.grb")
    assert os.path.exists(grb_fname)
    from ctypes import CDLL, POINTER, c_int, c_double,byref
    idis = c_int(-1)
    ncells = c_int(-1)
    ndim1 = c_int(-1)
    ndim2 = c_int(-1)
    ndim3 = c_int(-1)
    ppu = CDLL(os.path.join(test_d,lib_name))
    ppu.install_mf6_grid_from_file_(gs_fname.encode(),grb_fname.encode(),byref(idis),
        byref(ncells),byref(ndim1),byref(ndim2),byref(ndim3))
    print(idis.value,ncells.value,ndim1.value,ndim2.value,ndim3.value)
    assert idis.value == 1
    assert ncells.value == nrow * ncol * nlay
    assert ndim1.value == ncol
    assert ndim2.value == nrow
    assert ndim3.value == nlay



if __name__ == "__main__":
    structured_freyberg_invest()