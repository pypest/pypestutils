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
    lib_path = os.path.join("..","builddir","src","libpestutils.dylib")
    
elif "win" in platform.platform().lower():
    bin_path = os.path.join("bin","win")
    lib_path = os.path.join("..","builddir","src","pestutils.dll")
    
else:
    bin_path = os.path.join("bin","linux")
    lib_path = os.path.join("..","builddir","src","libpestutils.so")
    


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
    
    lib = ctypes.CDLL(os.path.join(test_d,lib_name))
    
    gridname = "freyberg"
    lib.install_mf6_grid_from_file(gridname.encode(),grb_fname.encode(),ctypes.byref(idis),
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
    
    # lib.dummy_test_(gridname.encode(),npts.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    #     ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    #     layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
    #     isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))

    nnpts = ctypes.c_int(npts[0])
    # lib.dummy_test_(gridname.encode(),ctypes.byref(nnpts),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    #     ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    #     layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
    #     isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    
    
    factype = ctypes.c_int(1)
    # note : casting ndarray using as_type works like layer.astype(np.int64).ctypes.data_as(ctypes.POINTER(ctypes.c_longlong))
    retcode = lib.calc_mf6_interp_factors(gridname.encode(),ctypes.byref(nnpts),
                                           ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),facfile.encode(),
                                           ctypes.byref(factype),blnfile.encode(),
                                           isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = lib.retrieve_error_message(string_ptr)
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
    retcode = lib.inquire_modflow_binary_file_specs(depvar_fname.encode(),depvar_contents_fname.encode(),
                                          ctypes.byref(isim),ctypes.byref(itype),ctypes.byref(iprec),
                                          ctypes.byref(narray),ctypes.byref(ntime))
    
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = lib.retrieve_error_message(string_ptr)
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
    retcode = lib.interp_from_mf6_depvar_file(depvar_fname.encode(),facfile.encode(),ctypes.byref(factype),
                                               ctypes.byref(ntime),vartype.ctypes.data_as(ctypes.POINTER(ctypes.c_char)),
                                               ctypes.byref(hdry),ctypes.byref(reapportion),ctypes.byref(hdry),
                                               ctypes.byref(nnpts),ctypes.byref(nproctime),
                                               simtime.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                               simstate.ctypes.data_as(ctypes.POINTER(ctypes.c_double)))
    
                                               
                                           
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = lib.retrieve_error_message(string_ptr)
        if retcode != 0:
            print(retcode) 
            raise Exception(string_ptr[:retcode].decode())
    
    print(simtime)
    print(simstate)

    # commenting out for now - something is up with get file specs...
    # cbc_fname = os.path.join(test_d,"freyberg6_freyberg.cbc")
    # cbc_contents_fname = cbc_fname+".out"
    # retcode = lib.inquire_modflow_binary_file_specs(cbc_fname.encode(),cbc_contents_fname.encode(),
    #                                       ctypes.byref(isim),ctypes.byref(itype),ctypes.byref(iprec),
    #                                       ctypes.byref(narray),ctypes.byref(ntime))
    # if retcode != 0:
    #     err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
    #     string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
    #     retcode = lib.retrieve_error_message(string_ptr)
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
    

    # retcode = lib.extract_flows_from_cbc_file(cbc_fname.encode(),
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
    #     retcode = lib.retrieve_error_message(string_ptr)
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
    
    lib = ctypes.CDLL(os.path.join(test_d,lib_name))
    
    gridname = "freyberg"
    lib.install_mf6_grid_from_file(gridname.encode(),grb_fname.encode(),ctypes.byref(idis),
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


    lib.dummy_test_(gridname.encode(),npts.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
        isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))

    nnpts = ctypes.c_int(npts[0])
    lib.dummy_test_(gridname.encode(),ctypes.byref(nnpts),ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),
        isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    
    get_err = lib.retrieve_error_message_
    err_str = np.array([' ' for _ in range(1000)],dtype=np.dtype('a1'))

    #string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
    #retcode = lib.retrieve_error_message(string_ptr)

    factype = ctypes.c_int(1)
    retcode = lib.calc_mf6_interp_factors(gridname.encode(),ctypes.byref(nnpts),
                                           ecoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           ncoord.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                           layer.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),facfile.encode(),
                                           ctypes.byref(factype),blnfile.encode(),
                                           isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
    if retcode != 0:
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = lib.retrieve_error_message(string_ptr)
        if retcode != 0:
            print(retcode) 
            raise Exception(string_ptr[:retcode].decode())



def output_driver1_test():

    sys.path.append(os.path.join(".."))
    from pypestutils import PyPestUtils
    org_d = "output_driver_test"
    test_d = org_d+"1"
    if os.path.exists(test_d):
        shutil.rmtree(test_d)
    shutil.copytree(org_d,test_d)
    print(lib_path)
    lib_name = os.path.split(lib_path)[-1]
    shutil.copy2(lib_path,os.path.join(test_d,lib_name)) 

    # jwhite: for some reason, the driver program is erroring out...
    # driver_d = os.path.split(lib_path)[0]
    # print(os.listdir(driver_d))
    # drivers = [f for f in os.listdir(driver_d) if f.startswith("driver") and not f.endswith(".p")]
    # for driver in drivers:
    #     shutil.copy2(os.path.join(driver_d,driver),os.path.join(test_d,driver))
    #pyemu.os_utils.run("{0} <{1}".format("driver1","driver1a.in"),cwd=test_d)

    idf = pd.read_csv("driver1_inputs.csv")

    for isim,dp_fname,itype,out_name in zip(idf.isim,idf.depvar_fname,idf.itype,idf.depvar_contents_fname):

        print("***********\n{0}\n**********\n".format(out_name))
        # the details from driver1a.in
        test_name = "test_"+out_name
        depvar_fname = os.path.join(test_d,dp_fname)
        depvar_contents_fname = os.path.join(test_d,test_name)
        depvar_contents_fname_org = os.path.join(test_d,out_name)
        iisim = ctypes.c_int(int(isim))
        iitype = ctypes.c_int(int(itype))
        iprec = ctypes.c_int(-1)
        narray = ctypes.c_int(-1)
        ntime = ctypes.c_int(-1)

        lib = ctypes.CDLL(os.path.join(test_d,lib_name))

        # todo: read output file to get a mapping of what var-times are available
        retcode = lib.inquire_modflow_binary_file_specs(depvar_fname.encode(),depvar_contents_fname.encode(),
                                              ctypes.byref(iisim),ctypes.byref(iitype),ctypes.byref(iprec),
                                              ctypes.byref(narray),ctypes.byref(ntime))

        if retcode != 0:
            err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
            string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
            retcode = lib.retrieve_error_message(string_ptr)
            if retcode != 0:
                print(retcode) 
                raise Exception(string_ptr[:retcode].decode())
        assert os.path.exists(depvar_contents_fname)
        use_cols = None
        #if "driver1d" in out_name:
        #    continue

        org_df = pd.read_fwf(depvar_contents_fname_org).dropna(axis=1)
        new_df = pd.read_csv(depvar_contents_fname).dropna(axis=1)
        #print(org_df.dtypes)
        comp_cols = [c for c in org_df.columns if org_df.dtypes[c] != object]
        diff = org_df.loc[:,comp_cols] - new_df.loc[:,comp_cols]
        assert diff.shape == diff.dropna(axis=0).shape
        #print(diff)
        mx = np.abs(diff.values).max()

        print(mx)
        assert mx < 1.0e-7

        p = PyPestUtils(os.path.join(test_d,lib_name))

        mapping,_ = PyPestUtils.get_file_type_map()
        print(isim)
        if itype == 1:
            is_state=True
        elif itype == 2:
            is_state = False
        else:
            raise Exception("huh")
        df = p.inquire_modflow_binary_file_specs(depvar_fname,mapping[int(isim)],is_state=is_state)
        diff = org_df.loc[:,comp_cols] - df.loc[:,comp_cols]
        assert diff.shape == diff.dropna(axis=0).shape
        #print(diff)
        mx = np.abs(diff.values).max()

        print(mx)
        assert mx < 1.0e-7




    




if __name__ == "__main__":
    #structured_freyberg_invest()
    output_driver1_test()
