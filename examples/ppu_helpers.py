import os
import pyemu

def setup_pps(d):
    cwd = os.getcwd()
    os.chdir(d)
    apply_pps()
    os.chdir(cwd)

def apply_pps():
    
    from pypestutils import helpers
    import numpy as np
    import pandas as pd
    df = pd.read_csv("pp_info.csv")
    gridspec_fname = "org.grb"
    for model_file,pp_file in zip(df.model_file,df.pp_file):
        ppdf = pd.read_csv(pp_file)
        results = helpers.interpolate_with_sva_pilotpoints_2d(ppdf,gridspec_fname,vartransform="log")
        org_arr = np.loadtxt(model_file)
        interp = results["result"]
        interp = interp.reshape(org_arr.shape)
        new_arr = org_arr * interp
        new_arr = new_arr
        new_arr[new_arr<1.0e-10] = 1.0e-10
        np.savetxt(model_file,new_arr,fmt="%15.6E")
        np.savetxt("interp_"+model_file,interp,fmt="%15.6E")
        np.savetxt("log_"+model_file,np.log10(new_arr),fmt="%15.6E")
    

if __name__ == "__main__":
	setup_pps()
	