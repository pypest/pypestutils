import os
import sys
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


def _rename_model(org_d,new_d):
    if os.path.exists(new_d):
        shutil.rmtree(new_d)
    os.makedirs(new_d)
    contents = os.listdir(org_d)
    for content in contents:
        if os.path.isdir(os.path.join(org_d,content)):
            shutil.copytree(os.path.join(org_d,content),os.path.join(new_d,content))
        elif content.startswith("."):
            continue
        else :
            print(content) 
            lines = open(os.path.join(org_d,content),'r').readlines()
            new_f = content.replace("project","freyberg6")
            with open(os.path.join(new_d,new_f),'w') as f:
                for line in lines:
                    f.write(line.replace("project","freyberg6"))
    
    pyemu.os_utils.run("mf6",cwd=new_d)
            
def structured_freyberg_invest():
    test_d = 'freyberg_structured_invest'
    if os.path.exists(test_d):
        shutil.rmtree(test_d)
    shutil.copytree("freyberg_structured",test_d)
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

    sys.path.append(os.path.join(".."))
    from pypestutils import PyPestUtils
    p = PyPestUtils(os.path.join(test_d,lib_name))
    p.install_grid(gridname+"1",grb_fname)
    

    npts = np.zeros(1,dtype=ctypes.c_int) + 10

    np.random.seed(12345)
    ecoord = np.random.uniform(125,4875,npts)
    ncoord = np.random.uniform(125,9875,npts)
    layer = np.ones(npts,dtype=np.int32)
    facfile = os.path.join(test_d,"factors.dat")
    blnfile = os.path.join(test_d,"bln_file.dat")
    isuccess = np.zeros(npts,dtype=np.int32)
    
    # ppu.dummy_test_(gridname.encode(),npts.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    #     ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    #     layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
    #     isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))

    nnpts = ctypes.c_int(npts[0])
    ppu.dummy_test_(gridname.encode(),ctypes.byref(nnpts),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
        isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    
    
    factype = ctypes.c_int(1)
    # note : casting ndarray using as_type works like layer.astype(np.int64).ctypes.data_as(ctypes.POINTER(ctypes.c_longlong))
    retcode = ppu.calc_mf6_interp_factors_(gridname.encode(),ctypes.byref(nnpts),
                                           ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),facfile.encode(),
                                           ctypes.byref(factype),blnfile.encode(),
                                           isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = ppu.retrieve_error_message_(string_ptr)
        if retcode != 0:
            print(retcode) 
            raise Exception(string_ptr[:retcode].decode())
    df = pd.DataFrame({"x":ecoord,"y":ncoord,"layer":layer})
    idf = p.calc_mf6_interp_factors(df)
    
    
    depvar_fname = os.path.join(test_d,"freyberg6_freyberg.hds")
    depvar_contents_fname = os.path.join(test_d,"freyberg6_freyberg.hds.out")
    isim = ctypes.c_int(1)
    itype = ctypes.c_int(1)
    iprec = ctypes.c_int(-1)
    narray = ctypes.c_int(-1)
    ntime = ctypes.c_int(-1)

    # todo: read output file to get a mapping of what var-times are available
    retcode = ppu.inquire_modflow_binary_file_specs_(depvar_fname.encode(),depvar_contents_fname.encode(),
                                          ctypes.byref(isim),ctypes.byref(itype),ctypes.byref(iprec),
                                          ctypes.byref(narray),ctypes.byref(ntime))
    
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = ppu.retrieve_error_message_(string_ptr)
        if retcode != 0:
            print(retcode) 
            raise Exception(string_ptr[:retcode].decode())
    #ntime = ctypes.c_int(25)
    vartype = np.zeros(17,dtype="a1")
    for i,c in enumerate("HEAD"):
        vartype[i] = c
    hdry = ctypes.c_double(1.0e+10)
    reapportion =ctypes.c_int(0)
    nproctime = ctypes.c_int(int(ntime.value))
       
    simtime = np.zeros(int(ntime.value),dtype=ctypes.c_double)
    simstate = np.zeros((int(ntime.value),npts[0]),dtype=ctypes.c_double,order='F')
    retcode = ppu.interp_from_mf6_depvar_file_(depvar_fname.encode(),facfile.encode(),ctypes.byref(factype),
                                               ctypes.byref(ntime),vartype.ctypes.data_as(ctypes.POINTER(ctypes.c_char)),
                                               ctypes.byref(hdry),ctypes.byref(reapportion),ctypes.byref(hdry),
                                               ctypes.byref(nnpts),ctypes.byref(nproctime),
                                               simtime.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                               simstate.ctypes.data_as(ctypes.POINTER(ctypes.c_double)))
    
                                               
                                           
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = ppu.retrieve_error_message_(string_ptr)
        if retcode != 0:
            print(retcode) 
            raise Exception(string_ptr[:retcode].decode())
    
    print(simtime)
    print(simstate)

    # commenting out for now - something is up with get file specs...
    # cbc_fname = os.path.join(test_d,"freyberg6_freyberg.cbc")
    # cbc_contents_fname = cbc_fname+".out"
    # retcode = ppu.inquire_modflow_binary_file_specs_(cbc_fname.encode(),cbc_contents_fname.encode(),
    #                                       ctypes.byref(isim),ctypes.byref(itype),ctypes.byref(iprec),
    #                                       ctypes.byref(narray),ctypes.byref(ntime))
    # if retcode != 0:
    #     err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
    #     string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
    #     retcode = ppu.retrieve_error_message_(string_ptr)
    #     if retcode != 0:
    #         print(retcode) 
    #         raise Exception(string_ptr[:retcode].decode())
   
    # zone_array = np.ones((nrow*ncol*nlay),dtype=ctypes.c_int)
    # ncell = ctypes.c_int(len(zone_array))
    # vartype = np.zeros(17,dtype="a1")
    # for i,c in enumerate("FLOW JA"):
    #     vartype[i] = c
    # nzone = ctypes.c_int(1)
    # numzone = ctypes.c_int(1)
    # zone_number = np.zeros((1),dtype=ctypes.c_int)
    # timestep = np.zeros((int(ntime.value)),dtype=ctypes.c_int)
    # stressperiod = np.zeros((int(ntime.value)),dtype=ctypes.c_int)
    # simtime = np.zeros((int(ntime.value)),dtype=ctypes.c_double)
    # simflow = np.zeros((int(ntime.value),int(nzone.value)),dtype=ctypes.c_double,order="F")
    

    # retcode = ppu.extract_flows_from_cbc_file(cbc_fname.encode(),
    #                                           vartype.ctypes.data_as(ctypes.POINTER(ctypes.c_char)),
    #                                           ctypes.byref(isim), ctypes.byref(iprec), 
    #                                           ctypes.byref(ncell),zone_array.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
    #                                           ctypes.byref(nzone),ctypes.byref(numzone),
    #                                           zone_number.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
    #                                           ctypes.byref(ntime),ctypes.byref(nproctime),
    #                                           timestep.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
    #                                           stressperiod.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
    #                                           simtime.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    #                                           simflow.ctypes.data_as(ctypes.POINTER(ctypes.c_double)))
    # if retcode != 0:
    #     err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
    #     string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
    #     retcode = ppu.retrieve_error_message_(string_ptr)
    #     if retcode != 0:
    #         print(retcode) 
    #         raise Exception(string_ptr[:retcode].decode())






def unstructured_freyberg_invest():
    test_d = 'freyberg_unstructured_invest'
    if os.path.exists(test_d):
        shutil.rmtree(test_d)
    shutil.copytree("freyberg_unstructured",test_d)
    for f in os.listdir(bin_path):
        shutil.copy2(os.path.join(bin_path,f),os.path.join(test_d,f))
    lib_name = os.path.split(lib_path)[-1]
    shutil.copy2(lib_path,os.path.join(test_d,lib_name)) 
    pyemu.os_utils.run("mf6",cwd=test_d)
    
    grb_fname = os.path.join(test_d,"freyberg6.disv.grb")
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
    assert idis.value == 2
    
    npts = np.zeros(1,dtype=np.int32) + 1000

    np.random.seed(12345)
    ecoord = np.random.uniform(0,1000,npts)
    ncoord = np.random.uniform(0,1000,npts)
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

    #string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
    #retcode = ppu.retrieve_error_message_(string_ptr)

    factype = ctypes.c_int(1)
    retcode = ppu.calc_mf6_interp_factors_(gridname.encode(),ctypes.byref(nnpts),
                                           ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),facfile.encode(),
                                           ctypes.byref(factype),blnfile.encode(),
                                           isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = ppu.retrieve_error_message_(string_ptr)
        if retcode != 0:
            print(retcode) 
            raise Exception(string_ptr[:retcode].decode())


    




if __name__ == "__main__":
    structured_freyberg_invest()
    