import os
import platform
import shutil
import ctypes
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
    #gs_fname = os.path.join(test_d,"grid.spc")
    #pyemu.helpers.SpatialReference(delc=delc,delr=delr,xll=xll,yll=yll).write_gridspec(gs_fname)
    grb_fname = os.path.join(test_d,"freyberg6.dis.grb")
    assert os.path.exists(grb_fname)
    
    idis = ctypes.c_int(-1)
    ncells = ctypes.c_int(-1)
    ndim1 = ctypes.c_int(-1)
    ndim2 = ctypes.c_int(-1)
    ndim3 = ctypes.c_int(-1)
    
    ppu = ctypes.CDLL(os.path.join(test_d,lib_name))
    
    gridname = "freyberg"
    ppu.install_mf6_grid_from_file_(gridname.encode(),grb_fname.encode(),ctypes.byref(idis),
        ctypes.byref(ncells),ctypes.byref(ndim1),ctypes.byref(ndim2),ctypes.byref(ndim3))
    print(idis.value,ncells.value,ndim1.value,ndim2.value,ndim3.value)
    assert idis.value == 1
    assert ncells.value == nrow * ncol * nlay
    assert ndim1.value == ncol
    assert ndim2.value == nrow
    assert ndim3.value == nlay

    npts = np.zeros(1,dtype=np.int32) + 10

    np.random.seed(12345)
    ecoord = np.random.uniform(0,ncol*np.cumsum(delr)[-1],npts)
    ncoord = np.random.uniform(0,nrow*np.cumsum(delc)[-1],npts)
    print(ecoord)
    print(ncoord)
    layer = np.ones(npts,dtype=np.int32)
    print(layer)
    facfile = os.path.join(test_d,"factors.dat")
    blnfile = os.path.join(test_d,"bln_file.dat")
    isuccess = np.zeros(npts,dtype=int)
    print(isuccess)


    ppu.dummy_test_(gridname.encode(),npts.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
        isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))

    nnpts = ctypes.c_int(npts[0])
    ppu.dummy_test_(gridname.encode(),ctypes.byref(nnpts),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
        isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))

    get_err = ppu.retrieve_error_message_
    err_str = np.array([' ' for _ in range(1000)],dtype=np.dtype('a1'))

    string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
    retcode = ppu.retrieve_error_message_(string_ptr)

    factype = ctypes.c_int(1)
    retcode = ppu.calc_mf6_interp_factors_(gridname.encode(),ctypes.byref(nnpts),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
         ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),facfile.encode(),
         ctypes.byref(factype),blnfile.encode(),isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    if retcode != 0:
        err_str = np.array([' ' for _ in range(1000)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = ppu.retrieve_error_message_(string_ptr)
        print(string_ptr)


    






if __name__ == "__main__":
    structured_freyberg_invest()