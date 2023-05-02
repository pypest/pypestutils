import os
import platform
import shutil
import ctypes
import numpy as np
import pandas as pd

class PyPestUtils(object):
    def __init__(self,library_fname):
        #todo logger
        if not os.path.exists(library_fname):
            raise FileNotFoundError("couldn't find library_fname '{0}'".format(library_fname))
        self.library_fname = library_fname       
        self.lib = self.initialize_library(self.library_fname)
        self.gridnames = []

    @staticmethod
    def initialize_library(library_fname):
        try:
            return ctypes.CDLL(library_fname)
        except Exception as e:
            raise Exception("error intializing library '{0}':{1}".format(library_fname,str(e)))
        
    def try_call(self,func,*args,**kwargs): 
        print("calling ",func.__name__)
        retcode = func(*args,**kwargs)
        if retcode != 0:
            message = self.get_error_message()
            raise Exception("function {0} raised an exception: {1}".format(func.__name__, message))

    def get_error_message(self,):
        err_str = np.array([' ' for _ in range(100)],dtype=np.dtype('a1'))
        string_ptr = err_str.ctypes.data_as(ctypes.POINTER(ctypes.c_char))
        retcode = self.lib.retrieve_error_message_(string_ptr)
        if retcode != 0:
            return string_ptr[:retcode].decode()
        else:
            return None


    def install_grid(self,gridname,grb_fname):
        # todo: setup grid dimension tracking for earlier error trapping
        idis = ctypes.c_int(-1)
        ncells = ctypes.c_int(-1)
        ndim1 = ctypes.c_int(-1)
        ndim2 = ctypes.c_int(-1)
        ndim3 = ctypes.c_int(-1)    
        if gridname.lower() in self.gridnames:
            raise Exception("gridname '{0}' already installed")

        self.try_call(self.lib.install_mf6_grid_from_file_,gridname.encode(),grb_fname.encode(),ctypes.byref(idis),
            ctypes.byref(ncells),ctypes.byref(ndim1),ctypes.byref(ndim2),ctypes.byref(ndim3))
        #print(idis.value,ncells.value,ndim1.value,ndim2.value,ndim3.value)
        self.gridnames.append(gridname.lower())

    def calc_mf6_interp_factors(self,df,gridname=None,facfile="factors.dat",facformat="ascii",blnfile="bln.dat"):
        if len(self.gridnames) == 0:
            raise Exception("no grids installed yet")
        if gridname is None:
            if len(self.gridnames) == 0:
                gridname = self.gridnames[0]
                print("Warning: 'gridname' not passed and more than one grid installed, using grid '{}'".format(gridname))
            else:
                gridname = self.gridnames[0]
        # todo: check for requried cols in df
        # todo: check if factor file exists and warn

        if facformat.lower().startswith('a'):
            factype = ctypes.c_int(1)
        elif facformat.lower().startswith('b'):
            factype = ctypes.c_int(2)
        else:
            raise Exception("unrecognized factor file format - should be either 'a'scii or 'b'inary, not '{0}'".format(facformat))

        nnpts = ctypes.c_int(df.shape[0])
        isuccess = np.zeros(df.shape[0],dtype=np.int32)
        self.try_call(self.lib.calc_mf6_interp_factors_,gridname.encode(),ctypes.byref(nnpts),
                                        df.x.values.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                        df.y.values.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
                                        df.layer.values.ctypes.data_as(ctypes.POINTER(ctypes.c_int)),facfile.encode(),
                                        ctypes.byref(factype),blnfile.encode(),
                                        isuccess.ctypes.data_as(ctypes.POINTER(ctypes.c_int)))
        
        # todo: check and warn for unsuccessful interp...   
        return pd.DataFrame({"interpolation_success":isuccess,"interpolation_order":np.arange(isuccess.shape[0],dtype=np.int32)},index=df.index)




    def read_mf6_output_file():
        #todo: make this a one-stop-shop to read a binary file to a dataframe - make
        # all the underlying calls here to hide to gory details..
        pass  