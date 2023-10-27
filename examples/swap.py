import os
import shutil
import numpy as np
import pandas as pd
import shapely
import pyemu
import flopy


def repair_and_prep_quadtree_model():
    org_d = "freyberg_quadtree_org"
    new_d = "freyberg_quadtree"
    if os.path.exists(new_d):
        shutil.rmtree(new_d)
    os.makedirs(new_d)

    files = os.listdir(org_d)

    for f in files:
        print(f)
        if f.startswith("."):
            continue
        try:
            lines = open(os.path.join(org_d,f),'r').readlines()
        except:
            print("error for file ",f)
            continue

        with open(os.path.join(new_d,f.replace("project","freyberg6")),'w') as ff:
            for line in lines:
                ff.write(line.lower().replace("project","freyberg6"))


    mn_dir = "freyberg_monthly"

    shutil.copy2(os.path.join(mn_dir,"freyberg6.tdis"),os.path.join(new_d,"freyberg6.tdis"))

    wel_files = [f for f in os.listdir(mn_dir) if "wel" in f and f.endswith(".txt")]
    org_uwell_file = os.path.join(new_d,"freyberg6.wel_stress_period_data_1.txt")
    org_wel_df = pd.read_csv(org_uwell_file,delim_whitespace=True,header=None,names=["layer","node","flux"])
    print(org_wel_df)
    for wel_file in wel_files:
        df = pd.read_csv(os.path.join(mn_dir,wel_file),delim_whitespace=True,header=None,names=["layer","row","col","flux"])
        new_df = org_wel_df.copy()
        new_df.loc[:,"flux"] = df.flux.values
        print(new_df)
        new_df.to_csv(os.path.join(new_d,wel_file),sep=" ",index=False,header=False)
    shutil.copy2(os.path.join(mn_dir,"freyberg6.wel"),os.path.join(new_d,"freyberg6.wel"))

    rch_files = [f for f in os.listdir(mn_dir) if "rcha" in f and f.endswith(".txt")]
    org_urch_file = os.path.join(new_d,"freyberg6.rcha_recharge_1.txt")
    vals = []
    with open(org_urch_file,'r') as f:
        for line in f:
            vals.extend([float(v) for v in line.strip().split()])
    org_urch_arr = np.array(vals)

    shutil.copy(os.path.join(mn_dir,"freyberg6.sto"),os.path.join(new_d,"freyberg6.sto"))
    sto_files = [f for f in os.listdir(mn_dir) if "sto" in f and f.endswith(".txt")]
    print(sto_files)

    rch_files.extend(sto_files)
    npf_files = [f for f in os.listdir(mn_dir) if "npf" in f and f.endswith(".txt")]
    rch_files.extend(npf_files)
    print(rch_files)
    for rch_file in rch_files:
        arr = np.loadtxt(os.path.join(mn_dir,rch_file))
        new_arr = np.zeros_like(org_urch_arr) + arr.mean()
        np.savetxt(os.path.join(new_d,rch_file),new_arr,fmt="%15.6E")
    shutil.copy2(os.path.join(mn_dir,"freyberg6.rcha"),os.path.join(new_d,"freyberg6.rcha"))

    # now unwrap all that dumbass wrapped format - what is this 1980?!
    txt_files = [f for f in os.listdir(os.path.join(new_d)) if f.startswith("freyberg6") and f.endswith(".txt") 
                and "stress" not in f and "continuous" not in f and "sfr" not in f and "vertices" not in f and "cell" not in f]

    txt_files.sort()
    for txt_file in txt_files:
        lines = open(os.path.join(new_d,txt_file),'r').readlines()
        vals = []
        for line in lines:
            vals.extend(line.strip().split())
        with open(os.path.join(new_d,txt_file),'w') as f:
            [f.write(v+"\n") for v in vals]
    #print(txt_files)
    botm = [32.5,30.0,10.0]
    for k,b in enumerate(botm):
        arr = np.zeros_like(org_urch_arr) + b
        np.savetxt(os.path.join(new_d,"freyberg6.disv_botm_layer{0}.txt".format(k+1)),arr,fmt="%15.6E")
    top = np.loadtxt(os.path.join(new_d,"freyberg6.disv_top.txt"))
    delt = top - botm[0]

    top += np.abs(delt.min()) + 1.1 #?
    np.savetxt(os.path.join(new_d,"freyberg6.disv_top.txt"),top,fmt="%15.6E")
    mnsim = flopy.mf6.MFSimulation.load(sim_ws=mn_dir,load_only=["dis"])
    mnm = mnsim.get_model()
    mnm.modelgrid.set_coord_info(angrot=0.0)

    usim = flopy.mf6.MFSimulation.load(sim_ws=new_d)#,load_only=["disv"])
    um = usim.get_model()
    um.modelgrid.set_coord_info(angrot=0.0)
    print(um.modelgrid,mnm.modelgrid)
     
    hobs_csv = "freyberg6.obs_continuous_heads.csv.txt"
    hds = pd.read_csv(os.path.join(mn_dir,hobs_csv),delim_whitespace=True,header=None,names=["site","otype","layer","row","col"])
    X,Y = mnm.modelgrid.xcellcenters,mnm.modelgrid.ycellcenters
    hds.loc[:,"x"] = hds.apply(lambda x: X[x.row-1,x.col-1],axis=1)
    hds.loc[:,"y"] = hds.apply(lambda x: Y[x.row-1,x.col-1],axis=1)
    hds.loc[:,"pt"] = hds.apply(lambda x: shapely.geometry.Point(x.x,x.y),axis=1)

    i = flopy.utils.GridIntersect(um.modelgrid)
    #print(i.intersect(shapely.geometry.Point(X[0,0],Y[0,0])))
    hds.loc[:,"inode"] = hds.pt.apply(lambda x: i.intersect(x)[0][0])
    print(hds.inode)

    hds.loc[:,"site"] = hds.apply(lambda x: "twgw_{0}_{1}".format(x.layer-1,x.inode),axis=1)
    hds.loc[:,["site","otype","layer","inode"]].to_csv(os.path.join(new_d,hobs_csv),index=False,header=False)
    shutil.copy2(os.path.join(mn_dir,"freyberg6.obs"),os.path.join(new_d,"freyberg6.obs"))
    lines = open(os.path.join(new_d,"freyberg6.nam"),'r').readlines()
    with open(os.path.join(new_d,"freyberg6.nam"),'w') as f:
        for line in lines:
            
            if "end packages" in line.lower():
                f.write("obs6  freyberg6.obs  head_obs\n")
                f.write("sto6  freyberg6.sto\n")
            f.write(line)

    sfr_file = os.path.join(new_d,"freyberg6.sfr_packagedata.txt")
    df = pd.read_csv(sfr_file,delim_whitespace=True,header=None)
    df.loc[:,1] = 1 # reset layer to 1
    #botm = np.loadtxt(os.path.join(new_d,"freyberg6.disv_botm_layer1.txt"))
    df.loc[:,5] = 5e-5 #gradient
    df.loc[:,4] = 5 # width
    cumdist = np.cumsum(df.loc[:,4].values)
    up = 34.0
    dn = 33.501
    totdrop = up - dn
    totlen = cumdist[-1]
    drop_grad = totdrop/totlen
    drop_reach = up - (cumdist * drop_grad)
    print(drop_reach)
    df.loc[:,6] = drop_reach #reach botm elev
    df.loc[:,7] = 1.0 #thickness of stream bed
    df.loc[:,8] = 0.1 #stream hk
    df.loc[:,9] = 0.3 #mannings
    print(df)
    df.to_csv(os.path.join(new_d,"freyberg6.sfr_packagedata.txt"),index=False,header=False,sep=" ")

    df = pd.read_csv(os.path.join(new_d,"freyberg6.chd_stress_period_data_1.txt"),header=None,delim_whitespace=True)
    df.loc[:,2] = 33.5
    df.to_csv(os.path.join(new_d,"freyberg6.chd_stress_period_data_1.txt"),index=False,header=False)

    for k in range(3):
        id_name = os.path.join(new_d,"freyberg6.disv_idomain_layer{0}.txt".format(k+1))
        arr = np.loadtxt(id_name)
        arr[arr>0] = 1
        np.savetxt(id_name,arr,fmt="%2.0f")


    pyemu.os_utils.run("mf6",cwd=new_d)

    #now write an "obs" twgw file with xy layer info
    mnm.modelgrid.set_coord_info(angrot=-55)
    X,Y = mnm.modelgrid.xcellcenters,mnm.modelgrid.ycellcenters
    hds.loc[:,"x"] = hds.apply(lambda x: X[x.row-1,x.col-1],axis=1)
    hds.loc[:,"y"] = hds.apply(lambda x: Y[x.row-1,x.col-1],axis=1)

    obsdf = pd.read_csv(os.path.join(new_d,"heads.csv"),index_col=0)
    obsdf.columns = [c.lower() for c in obsdf.columns]
    obsdf = obsdf.melt(ignore_index=False,var_name="site",value_name="obsval")
    start_datetime= pd.to_datetime("1-1-2018")
    obsdf.index = start_datetime + pd.to_timedelta(obsdf.index.values,unit="d")
    obsdf.index.name = "datetime"
    hds.index = hds.site.values
    noisex = np.random.uniform(-40,40,hds.x.shape[0])
    xdict = (hds.x + noisex).to_dict()
    noisey = np.random.uniform(-40,40,hds.y.shape[0])
    ydict = (hds.y + noisey).to_dict()
    layerdict = hds.layer.to_dict()
    obsdf.loc[:,"x"] = obsdf.site.apply(lambda x: xdict[x])
    obsdf.loc[:,"y"] = obsdf.site.apply(lambda x: ydict[x])
    obsdf.loc[:,"layer"] = obsdf.site.apply(lambda x: layerdict[x])
    #now remove some values
    keep_idx = np.arange(0,obsdf.shape[0],dtype=int)
    np.random.shuffle(keep_idx)
    keep_idx = keep_idx[:int(3*keep_idx.shape[0]/4)]
    kobsdf = obsdf.iloc[keep_idx,:].copy()
    kobsdf.sort_index(inplace=True)
    print(obsdf.shape,kobsdf.shape)
    print(kobsdf)
    # now some datetime noise
    noise = np.random.uniform(-30,30,kobsdf.shape[0])
    noise = np.round(noise,1)
    td_noise = pd.to_timedelta(noise,unit='d')
    kobsdf.index = kobsdf.index + td_noise
    kobsdf.loc[:,"datetime"] = kobsdf.index.values
    kobsdf.loc[:,["site","datetime","x","y","layer","obsval"]].to_csv(os.path.join("freyberg_aux_files","gwlevel_obs.csv"),index=False)

    print(kobsdf)

def invest_quadtree_results():
    import matplotlib.pyplot as plt
    ws = "freyberg_quadtree"
    sim = flopy.mf6.MFSimulation.load(sim_ws=ws)
    m = sim.get_model()
    hds = flopy.utils.HeadFile(os.path.join(ws,"freyberg6.hds"),model=m)
    hds.plot(colorbar=True)
    plt.show()

if __name__ == "__main__":
    repair_and_prep_quadtree_model()
    #invest_quadtree_results()
