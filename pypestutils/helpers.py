from __future__ import annotations

import os
import numpy as np
import pandas as pd

from .pestutilslib import PestUtilsLib


def mod2obs_mf6(gridinfo_fname: str,depvar_fname: str,obscsv_fname: str ,model_type: int,start_datetime: str | pd.TimeStamp,depvar_ftype=1,
                depvar_name="head",interp_thresh=1.0e+30,no_interp_val=1.0e+30,model_timeunit="d",
                time_extrap=1.0)->dict:

    """python implementation of mod2smp and mod2obs using modflow6 binary grid files
    Parameters
    ----------
    gridinfo_fname: str
        grid information file
    depvar_fname: str
        MODFLOW-6 output binary file
    obscsv_fname: str | pd.DataFrame
        observation information.  Must contain columns "site","x","y","datetime",and "layer"
    model_type: int
        type of model.  Must be either 31 (dis mf6) or 32 (disv mf6)
    start_datetime: str | datetime
        the simulation start datetime
    depvar_ftype : int
        the modflow-6 output file type.  1 for states, 2 or cell-by-cell budgets
    depvar_name: str
        the name of the dependent variable in `depvar_fname` to extract (for example "head")
    interp_thresh: float
        the upper limit above which extracted values are treated as invalid.  Default is 1.0+30
    no_interp_val: float
        value used to fill invalid/null extracted/interpolated values
    model_time_unit: str
        pandas style time unit.  Default is "d"ay
    time_extrap: float
        length of time units to extrapolate.  Default is 1.0 time unit

    Returns
    -------
    all_results: pd.DataFrame
        all simulated times at observation locations (ie mod2smp)
    interpolated_results: pd.DataFrame
        temporally interpolated simulated results at observation locations (ie mod2obs)
    """

    for fname in [gridinfo_fname,depvar_fname]:
        assert os.path.exists(fname),"file {0} not found".format(fname)
    lib = PestUtilsLib()
    is_mf6 = False
    is_structured = True
    model_type = int(model_type)
    if model_type == 1:
        is_mf6 = False
    elif model_type == 21:
        pass
    elif model_type == 22:
        is_structured = False
    elif model_type == 31:
        is_mf6 = True
    elif model_type == 32:
        is_mf6 = True
        is_structured = False
    elif model_type == 33:
        is_mf6 = True
        is_structured = False
    else:
        raise Exception("unrecognized 'model_type':{0}".format(model_type))

    depvar_ftype = int(depvar_ftype)
    if depvar_ftype not in [1,2]:
        raise Exception("unrecognized 'depvar_ftype':{0}".format(depvar_ftype))

    if is_mf6:
        grid_info = lib.install_mf6_grid_from_file("grid",gridinfo_fname)
    else:
        raise NotImplementedError()
    
    if isinstance(start_datetime,str):
        start_datetime = pd.to_datetime(start_datetime)

    depvar_info = lib.inquire_modflow_binary_file_specs(depvar_fname,depvar_fname+".out.csv",model_type,depvar_ftype)    
    depvar_df = pd.read_csv(depvar_fname+".out.csv")
    depvar_df.columns = [c.lower() for c in depvar_df.columns]
    #print(depvar_df)

    if isinstance(obscsv_fname,str):
        if not os.path.exists(obscsv_fname):
            raise Exception("obscsv_fname '{0}' not found".format(obscsv_fname))
        # todo: think about supporting a site sample file maybe?
        obsdf = pd.read_csv(os.path.join(obscsv_fname),parse_dates=["datetime"])
    elif isinstance(obscsv_fname,pd.DataFrame):
        obsdf = obscsv_fname.copy()
    else:
        raise Exception("obscsv arg type not recognized (looking for str or pd.DataFrame):'{0}'".format(type(obscsv_fname)))
    #check obsdf
    obsdf.columns = [c.lower() for c in obsdf.columns]
    for req_col in ["site","x","y","datetime","layer"]:
        if req_col not in obsdf.columns:
            raise Exception("observation dataframe missing column '{0}'".format(req_col))
    usitedf = obsdf.groupby("site").first()
    pth = os.path.split(depvar_fname)[0]
    fac_file = os.path.join(pth,"obs_interp_fac.bin")
    bln_file = fac_file.replace(".bin",".bln")
    interp_fac_results = lib.calc_mf6_interp_factors("grid",usitedf.x.values,usitedf.y.values,usitedf.layer.values,fac_file,"binary",bln_file)
    if 0 in interp_fac_results:
        print("warning: the following site(s) failed to have interpolation factors calculated:")
        fsites = usitedf.site.iloc[interp_fac_results==0].to_list()
        print(fsites)
    all_results = lib.interp_from_mf6_depvar_file(depvar_fname,fac_file,"binary",depvar_info["ntime"],"head",interp_thresh,True,
        no_interp_val,usitedf.shape[0])
    datetimes = start_datetime+pd.to_timedelta(all_results["simtime"],unit=model_timeunit)
    allresults_df = pd.DataFrame(all_results["simstate"],index=datetimes,columns=usitedf.index)
    allresults_df.to_csv(depvar_fname+".all.csv")

    if "totim" in obsdf:
        print("WARNING: replacing existing 'totim' column in observation dataframe")
    obsdf.loc[:,"totim"] = obsdf.datetime.apply(lambda x: x  - start_datetime).dt.days 

    usite = obsdf.site.unique()
    usite.sort()
    usite_dict = {s:c for s,c in zip(usite,np.arange(usite.shape[0],dtype=int))}
    obsdf.loc[:,"isite"] = obsdf.site.apply(lambda x: usite_dict[x])
    obsdf.sort_values(by=["isite","totim"],inplace=True)
    
    interp_results = lib.interp_to_obstime(all_results["nproctime"],all_results["simtime"],all_results["simstate"],interp_thresh,"L",
        time_extrap,no_interp_val,obsdf.isite.values,obsdf.totim.values)

    obsdf.loc[:,"simulated"] = interp_results
    lib.uninstall_mf6_grid('grid')
    lib.free_all_memory()
    return {"all_results":allresults_df,"interpolated_results":obsdf}

def get_grid_info_from_gridspec(gridspec_fname: str) -> dict:
    """Read structured grid info from a PEST-style grid specificatin file
    Parameters
    ----------
    gridspec_fname : str
        PEST-style grid specification file
    
    Returns
    -------
    grid_info: dict
        grid information
    """

    if not os.path.exists(gridspec_fname):
        raise FileNotFoundError(gridspec_fname)
    sr = SpatialReference.from_gridspec(gridspec_fname)
    return {
        "x": sr.xcentergrid.flatten(),
        "y": sr.ycentergrid.flatten(),
        "area": sr.areagrid.flatten(),
        "nrow": sr.nrow,
        "ncol": sr.ncol,
        "delr": sr.delr,
        "delc": sr.delc
    }


def get_grid_info_from_mf6_grb(grb_fname: str) -> dict:
    """Read grid info from a MODFLOW-6 binary grid file
    Parameters
    ----------
    grb_fname: str
        MODFLOW-6 binary grid file
    
    Returns
    -------
    grid_info: dict
        grid information
    """
    if not os.path.exists(grb_fname):
        raise FileNotFoundError(grb_fname)
    lib = PestUtilsLib()
    data = lib.install_mf6_grid_from_file("grid",grb_fname)
    data["x"],data["y"],data["z"] = lib.get_cell_centres_mf6("grid",data["ncells"])
    lib.uninstall_mf6_grid("grid")
    lib.free_all_memory()
    return data


def get_2d_grid_info_from_file(fname: str, layer=None) -> dict:
    """Try to read 2-D grid info from a variety of filename sources
    Parameters
    ----------
    fname: str
        filename that stores 2-D grid info.  Optionally, a pandas DataFrame
        at least columns 'x','y' and possibly 'layer'.
    layer: int (optional)
        the (one-based) layer number to use for 2-D.  If None and
        grid info is 3-D, a value of 1 is used
    
    Returns
    -------
    grid_info: dict
        grid information
    """ 

    grid_info = None
    if isinstance(fname,str):
        if not os.path.exists(fname):
            raise FileNotFoundError(fname)
        if fname.lower().endswith(".csv"):
            grid_info = pd.read_csv(fname)
            grid_info.columns = [c.lower() for c in grid_info.columns]
            fname = grid_info # for  checks and processing below
            
        else:
            try:
                grid_info = get_grid_info_from_gridspec(fname)
            except Exception as e1:
                try:
                    grid_info = get_2d_grid_info_from_mf6_grb(fname, layer=layer)
                except Exception as e2:
                    
                    raise Exception("error getting grid info from file '{0}'".format(fname))
        
    if isinstance(fname,pd.DataFrame):
        if 'x' not in fname.columns:
            raise Exception("required 'x' column not found in grid info dataframe")
        if 'y' not in fname.columns:
            raise Exception("required 'y' column not found in grid info dataframe")
        if layer is not None and 'layer' not in fname.columns:
            print("WARNING: 'layer' arg is not None but 'layer' not found in grid info dataframe...")
        # I think these should just be references to column values (not copies)
        grid_info = {c:fname[c].values for c in fname.columns}
    
    return grid_info


def get_2d_grid_info_from_mf6_grb(grb_fname: str,layer=None) -> dict:
    """Read grid info from a MODFLOW-6 binary grid file
    Parameters
    ----------
    grb_fname: str
        MODFLOW-6 binary grid file
    layer: int (optional)
        the layer number to use for 2-D.  If None,
        a value of 1 is used
    
    Returns
    -------
    grid_info: dict
        grid information
    """
    grid_info = get_grid_info_from_mf6_grb(grb_fname)
    nnodes = grid_info["ncells"]
    x = grid_info["x"].copy()
    y = grid_info["y"].copy()
    nrow,ncol = None,None
    if grid_info["idis"] == 1:
        nlay = grid_info["ndim3"]
        if layer is not None:
            assert layer != 0, "Value provided for layer should be 1 based, sorry"
            if layer > nlay:
                raise Exception("user-supplied 'layer' {0} greater than nlay {1}".format(layer,nlay))
        else:
            layer = 1
        nrow = grid_info["ndim2"]
        ncol = grid_info["ndim1"]
        x = x.reshape((nlay,nrow,ncol))[layer-1]
        y = y.reshape((nlay,nrow,ncol))[layer-1]
        grid_info["nnodes"] = nrow * ncol
        grid_info["x"] = x
        grid_info["y"] = y
        grid_info["nrow"] = nrow
        grid_info["ncol"] = ncol

    elif grid_info["idis"] == 2:
        nlay = grid_info["ndim3"]
        if layer is not None:
            if layer > nlay:
                raise Exception("user-supplied 'layer' {0} greater than nlay {1}".format(layer,nlay))
        else:
            layer = 1
        ncpl = grid_info["ndim1"]
        x = x.reshape((nlay,ncpl))[layer-1]
        y = y.reshape((nlay,ncpl))[layer-1]
        grid_info["nnodes"] = ncpl
        grid_info["x"] = x
        grid_info["y"] = y
    return grid_info


def get_2d_pp_info_structured_grid(
    pp_space: int,
    gridinfo_fname: str,
    array_dict = {},
    name_prefix="pp"
) -> pd.DataFrame:
    """Create a grid of pilot point locations for a 
    2-D structured grid
    Parameters
    ----------
    pp_space: int
        row and column spacing for pilot point locations
    gridinfo_fname: str
        file contain grid information
    array_dict: dict (optional)
        a dict of 2-D grid-shape arrays used to populate 
        pilot point attributes.  Special values include:
        "value","zone","bearing","aniso" and "corrlen", 
        although any number of arrays can be passed and will
        sampled at pilot point locations
    name_prefix: str
        pilot point name prefix. Default is "pp"
    
    Returns
    -------
    ppdf: pd.DataaFrame
        dataframe of pilot point information

    """

    grid_info = get_2d_grid_info_from_file(gridinfo_fname)
    pname, px, py, pval = [], [], [], []
    pi, pj = [], []
    parr_dict = {k:[] for k in array_dict.keys()}
    count = 0
    nrow = grid_info["nrow"]
    ncol = grid_info["ncol"]
    nlay = grid_info.get("nlay",1)

    zone_array = array_dict.get("zone",None)

    x = grid_info['x']
    y = grid_info['y']
    x = x.reshape((nlay,nrow,ncol))[0,:,:]
    y = y.reshape((nlay,nrow,ncol))[0,:,:]
    if nrow is None:
        raise Exception("unstructured grid loaded from gridinfo_fname '{0}'".format(gridspec_fname))
    for i in range(int(pp_space / 2), nrow, pp_space):
        for j in range(int(pp_space / 2), ncol, pp_space):
            if zone_array is not None and zone_array[i, j] <= 0:
                continue
            px.append(x[i, j])
            py.append(y[i, j])
            #if zone_array is not None:
            #    pzone.append(zone_array[i, j])
            #else:
            #    pzone.append(1)

            pname.append(name_prefix + "{0}".format(count))
            pi.append(i)
            pj.append(j)
            count += 1
    df = pd.DataFrame(
        {
            "ppname": pname,
            "x": px,
            "y": py,
            "i": pi,
            "j": pj,
        },
        index=pname,
    )
    df.loc[:,"value"] = 1.0
    df.loc[:, "bearing"] = 0.0
    df.loc[:, "aniso"] = 1.0
    delx = pp_space * 5 * int((x.max() - x.min()) / float(ncol))
    dely = pp_space * 5 * int((y.max() - y.min()) / float(nrow))
    df.loc[:, "corrlen"] = max(delx,dely)  # ?
    df.loc[:,"zone"] = 1
    for k,arr in array_dict.items():
        df.loc[:,k] = arr[df.i,df.j]
    df["zone"] = df.zone.astype(int)

    return df


def interpolate_with_sva_pilotpoints_2d(
    pp_info: pandas.DataFrame,
    gridinfo_fname: str,
    vartype="exp",
    krigtype="ordinary",
    vartransform="none",
    max_pts=50,
    min_pts=1,
    search_dist=1e30,
    zone_array=1,
    verbose=True,
    layer=None
) -> dict:
    """Perform 2-D pilot point interpolation using
    spatially varying geostatistical hyper-parameters
    Parameters
    ----------
    pp_info: pandas.DataFrame
        dataframe with pilot point info.  Required columns 
        include: "x","y",and "value".  optional columns include:
        "zone","bearing","aniso",and "corrlen"    
    gridinfo_fname: str
        file name storing grid information
    vartype: str
        variogram type.  Default is "exp"onential
    krigtype: str
        kriging type.  Default is "ordinary"
    vartransform: str
        variogram transformation.  Default is "none"
    max_pts: int
        maximum number of pilot points to use in interpolation.
        Default is 50
    min_pts: int
        minimum number of pilot points to use in interplation.
        Default is 1
    search_dict: float
        search distance to use when looking for nearby pilot points.
        Default is 1.0e+30
    zone_array: int | numpy.ndarray
        the zone array to match up with "zone" value in `pp_info`.  If 
        integer type, a constant zone array of value "zone_array" is used.
        Default is 1
    verbose: bool
        flag to output.  Default is True
    layer: int
        the (one-based) layer number to use if gridinfo_fname points
        to 3-D grid info. Default is None, which results in
        layer 1 being used

    Returns
    -------
    results: dict
        resulting arrays of the various interpolation from pilot 
        points to grid-shaped arrays
    """
    # some checks on pp_info
    req_cols = ["ppname", "x", "y", "value", "corrlen"]
    missing = []
    for req_col in req_cols:
        if req_col not in pp_info.columns:
            missing.append(req_col)
    if len(missing) > 0:
        raise Exception(
            "the following required columns are not in pp_info:{0}".format(
                ",".join(missing)
            )
        )

    if "zone" not in pp_info:
        pp_info.loc[:, "zone"] = 1

    nnodes, nrow, ncol = None, None, None
    x, y, area = None, None, None

    nrow, ncol = None, None
    x, y, area = None, None, None
    grid_info = get_2d_grid_info_from_file(gridinfo_fname, layer)
    nrow = grid_info.get("nrow",None)
    ncol = grid_info.get("ncol",None)
    x = grid_info['x']
    y = grid_info['y']
    area = grid_info.get("area",None)
    idis = grid_info.get("idis",None)
    nnodes = grid_info.get("nnodes",None)
    if area is None:
        area = np.ones_like(x)
    if nnodes is None:
        nnodes = x.shape[0]

    lib = PestUtilsLib()

    if not isinstance(zone_array, np.ndarray):
        zone_array = np.ones((nnodes), dtype=int)
        lib.logger.info("using 1s as zone array for interpolation")
    elif zone_array.dtype != int:
        # TODO warn here
        lib.logger.info("casting zone_array from %r to int", zone_array.dtype)
        zone_array = zone_array.astype(int)
    
    
    hyperfac_ftype = "binary"
    if verbose:
        hyperfac_ftype = "text"
    hyperbearing = 0.0
    hyperaniso = 1.0
    hypervartype = "exp"
    hyperkrigtype = "ordinary"
    hypertrans = "none"

    fac_files = []
    lib.logger.info("using bearing of %r and aniso of %r for hyperpar interpolation", hyperbearing, hyperaniso)
    lib.logger.info("using %r variogram with %r transform for hyperpar interpolation",hypervartype,hypertrans)
       
    results = {}    

    bearing = np.zeros_like(x)
    if "bearing" in pp_info.columns:
        hypernoint = pp_info.bearing.mean()
        lib.logger.info("using no-interpolation value of %r for 'bearing' hyperpar interpolation", hypernoint)
        hyperfac_fname = "tempbearing.fac"
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values.astype(int),
            x.flatten(),
            y.flatten(),
            zone_array.flatten().astype(int),
            hyperkrigtype,
            hyperaniso,
            hyperbearing,
            hyperfac_fname,
            hyperfac_ftype,
        )
        result = lib.krige_using_file(
            hyperfac_fname,
            hyperfac_ftype,
            nnodes,
            hyperkrigtype,
            hypertrans,
            pp_info.bearing.values,
            hypernoint,
            hypernoint,
        )
        bearing = result["targval"]
        fac_files.append(hyperfac_fname)
        if verbose:
            if nrow is not None:
                np.savetxt("bearing.txt",bearing.reshape(nrow,ncol),fmt="%15.6E")
            else:
                np.savetxt("bearing.txt",bearing,fmt="%15.6E")
    results["bearing"] = bearing
    

    aniso = np.ones_like(x)
    if "aniso" in pp_info.columns:
        hypernoint = pp_info.aniso.mean()
        lib.logger.info("using no-interpolation value of %r for 'aniso' hyperpar interpolation", hypernoint)
        
        hyperfac_fname = "tempaniso.fac"
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            x.flatten(),
            y.flatten(),
            zone_array.flatten().astype(int),
            hyperkrigtype,
            hyperaniso,
            hyperbearing,
            hyperfac_fname,
            hyperfac_ftype,
        )
        result = lib.krige_using_file(
            hyperfac_fname,
            hyperfac_ftype,
            nnodes,
            hyperkrigtype,
            hypertrans,
            pp_info.aniso.values,
            hypernoint,
            hypernoint,
        )
        aniso = result["targval"]
        if verbose:
            if nrow is not None:
                np.savetxt("aniso.txt",aniso.reshape(nrow,ncol),fmt="%15.6E")
            else:
                np.savetxt("aniso.txt",aniso,fmt="%15.6E")

    results["aniso"] = aniso

    use_auto = False
    corrlen = None  # todo corrlen default where not in ppdf
    if "corrlen" in pp_info.columns:
        hypernoint = pp_info.corrlen.mean()
        lib.logger.info("using no-interpolation value of %r for 'corrlen' hyperpar interpolation", hypernoint)
        hyperfac_fname = "tempcorrlen.fac"
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            x.flatten(),
            y.flatten(),
            zone_array.flatten().astype(int),
            hyperkrigtype,
            hyperaniso,
            hyperbearing,
            hyperfac_fname,
            hyperfac_ftype,
        )
        result = lib.krige_using_file(
            hyperfac_fname,
            hyperfac_ftype,
            nnodes,
            hyperkrigtype,
            hypertrans,
            pp_info.corrlen.values,
            hypernoint,
            hypernoint,
        )
        corrlen = result["targval"]
        fac_files.append(hyperfac_fname)
        use_auto = False
        if verbose:
            if nrow is not None:
                np.savetxt("corrlen.txt",corrlen.reshape(nrow,ncol),fmt="%15.6E")
            else:
                np.savetxt("corrlen.txt",corrlen,fmt="%15.6E")
        results["corrlen"] = corrlen

    if not verbose:
        for fac_file in fac_files:
            try:
                os.remove(fac_file)
            except Exception as e:
                pass

    # todo: maybe make these args?
    fac_fname = "var.fac"
    fac_ftype = "binary"
    if verbose:
        fac_ftype = "text"
    noint = pp_info.loc[:, "value"].mean()
    if use_auto:
        npts = lib.calc_kriging_factors_auto_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            x.flatten(),
            y.flatten(),
            zone_array.flatten().astype(int),
            krigtype,
            aniso.flatten(),
            bearing.flatten(),
            fac_fname,
            fac_ftype,
        )
    else:
        npts = lib.calc_kriging_factors_2d(
            pp_info.x.values,
            pp_info.y.values,
            pp_info.zone.values,
            x.flatten(),
            y.flatten(),
            zone_array.flatten().astype(int),
            vartype,
            krigtype,
            corrlen.flatten(),
            aniso.flatten(),
            bearing.flatten(),
            search_dist,
            max_pts,
            min_pts,
            fac_fname,
            fac_ftype,
        )

    result = lib.krige_using_file(
        fac_fname,
        fac_ftype,
        nnodes,
        krigtype,
        vartransform,
        pp_info.loc[:, "value"].values,
        noint,
        noint,
    )
    results["result"] = result["targval"]

    if nrow is not None:
        for k,v in results.items():
            results[k] = v.reshape(nrow,ncol)

    return results


def generate_2d_grid_realizations(
    gridinfo_fname: str,
    num_reals=100,
    variotype="exp",
    mean=1.0,
    variance=1.0,
    variorange=None,
    variotransform="none",
    zone_array=None,
    varioaniso=1.0,
    variobearing=0.0,
    random_seed=12345,
    layer=None,
) ->np.NDArray[float]:
    """draw 2-D realizations using sequential gaussian 
    simulations and optionally using spatially varying 
    geostatistical hyper parameters.
    Parameters
    ----------
    gridinfo_fname: str
        file containing grid information
    num_real : int
        number of realizations to generate
    variotype: str
        variogram type.  Default is "exp"onential
    mean: float or numpy.ndarray
        field mean.  Either a scalar or array of shape nnodes.  Default is 1.0
    variance: float
        field variance. Either a scalar or array of shape nnodes.  Default is 1.0
    variorange: float
        range of the variogram. Either a scalar or array of shape nnodes.
    variotransform: str
        variogram transform.  Default is "none".
    zone_array: int or numpy.ndarray
        the zone array
    varioaniso: float or numpy.ndarray
        the variogram anisotropy ratio.  Either a scalar or array of shape nnodes.
    variobearing: flaot or numpy.ndarray
        the variogram anisotropy bearing.  Either a scalar or array of shape nnodes
    random_seed: int
        the random seed.  Default is 12345
    layer : int or None
        the (one-based) layer to use of gridinfo_fname contains 3-D info.  Default
        is None, which results in layer 1 being used

    Returns
    -------
    results: numpy.ndarray(float)
        realizations (if `grid_info` indicates a structured grid, realizations
        will be reshaped to NROW X NCOL)
    """

    

    nrow, ncol = None, None
    x, y, area = None, None, None
    grid_info = get_2d_grid_info_from_file(gridinfo_fname, layer)
    nrow = grid_info.get("nrow",None)
    ncol = grid_info.get("ncol",None)
    x = grid_info['x']
    y = grid_info['y']
    area = grid_info.get("area",None)
    idis = grid_info.get("idis",None)
    nnodes = grid_info.get("nnodes",None)
    if area is None:
        area = np.ones_like(x)
    if nnodes is None:
        nnodes = x.shape[0]


    if not isinstance(mean, np.ndarray):
        mean = np.zeros((nnodes)) + mean
    if not isinstance(variance, np.ndarray):
        variance = np.zeros((nnodes)) + variance
    if variorange is None:
        delx = x.max() - x.min()
        dely = y.max() - y.min()

        variorange = np.zeros((nnodes)) + max(delx,dely) / 10 # ?
    elif not isinstance(variorange, np.ndarray):
        variorange = np.zeros((nnodes)) + variorange

    if not isinstance(variobearing, np.ndarray):
        variobearing = np.zeros((nnodes)) + variobearing

    if not isinstance(varioaniso, np.ndarray):
        varioaniso = np.zeros((nnodes)) + varioaniso

    if not isinstance(zone_array, np.ndarray):
        zone_array = np.ones((nnodes), dtype=int)
    elif zone_array.dtype != int:
        # TODO warn here
        zone_array = zone_array.astype(int)

    

    power = 1.0

    lib = PestUtilsLib()
    lib.initialize_randgen(random_seed)

    reals = lib.fieldgen2d_sva(
        x.flatten(),
        y.flatten(),
        area.flatten(),
        zone_array.flatten(),
        mean.flatten(),
        variance.flatten(),
        variorange.flatten(),
        varioaniso.flatten(),
        variobearing.flatten(),
        variotransform,
        variotype,
        power,
        num_reals,
    )
    lib.free_all_memory()
    if nrow is not None:
        return reals.transpose().reshape((num_reals, nrow, ncol))
    else:
        return reals.transpose()


class SpatialReference(object):
    """
    a class to locate a structured model grid in x-y space.

    Parameters
    ----------
    delr:  numpy.ndarray
        the model discretization delr vector (An array of spacings along a row)
    delc: numpy ndarray
        the model discretization delc vector (An array of spacings along a column)
    xul: float
        The x coordinate of the upper left corner of the grid. Enter either xul and yul or xll and yll.
    yul: float
        The y coordinate of the upper left corner of the grid. Enter either xul and yul or xll and yll.
    rotation: float
        The counter-clockwise rotation (in degrees) of the grid
    """

    def __init__(self, delr, delc, xul, yul, rotation=0.0):
        self.xul = float(xul)
        self.yul = float(yul)
        self.rotation = float(rotation)
        for delrc in [delr, delc]:
            if isinstance(delrc, float) or isinstance(delrc, int):
                msg = (
                    "delr and delcs must be an array or sequences equal in "
                    "length to the number of rows/columns."
                )
                raise TypeError(msg)

        self.delc = np.atleast_1d(np.array(delc)).astype(np.float64)
        self.delr = np.atleast_1d(np.array(delr)).astype(np.float64)

        self._xgrid = None
        self._ygrid = None
        self._xcentergrid = None
        self._ycentergrid = None

    @property
    def xll(self)->float:
        """lower left x coord
        """
        xll = self.xul - (np.sin(self.theta) * self.yedge[0])
        return xll

    @property
    def yll(self)->float:
        """lower left y coord
        """
        yll = self.yul - (np.cos(self.theta) * self.yedge[0])
        return yll

    @property
    def nrow(self)->int:
        """number of rows
        """
        return self.delc.shape[0]

    @property
    def ncol(self)->int:
        """number of cols
        """
        return self.delr.shape[0]

    @classmethod
    def from_gridspec(cls, gridspec_file)->SpatialReference:
        """instantiate from a pest-style grid specification file
        Parameters
        ----------
        gridspec_file: str
            grid specification file name

        Returns
        -------
        sr: SpatialReference
            sr instance
        """
        f = open(gridspec_file, "r")
        raw = f.readline().strip().split()
        nrow = int(raw[0])
        ncol = int(raw[1])
        raw = f.readline().strip().split()
        xul, yul, rot = float(raw[0]), float(raw[1]), float(raw[2])
        delr = []
        j = 0
        while j < ncol:
            raw = f.readline().strip().split()
            for r in raw:
                if "*" in r:
                    rraw = r.split("*")
                    for n in range(int(rraw[0])):
                        delr.append(float(rraw[1]))
                        j += 1
                else:
                    delr.append(float(r))
                    j += 1
        delc = []
        i = 0
        while i < nrow:
            raw = f.readline().strip().split()
            for r in raw:
                if "*" in r:
                    rraw = r.split("*")
                    for n in range(int(rraw[0])):
                        delc.append(float(rraw[1]))
                        i += 1
                else:
                    delc.append(float(r))
                    i += 1
        f.close()
        return cls(np.array(delr), np.array(delc), xul=xul, yul=yul, rotation=rot)

    @property
    def theta(self)->float:
        """rotation in radians
        """
        return -self.rotation * np.pi / 180.0

    @property
    def xedge(self)->np.NDArray[float]:
        """the xedge array of the grid
        """
        return self.get_xedge_array()

    @property
    def yedge(self)->np.NDArray[float]:
        """the yedge array of the grid
        """
        return self.get_yedge_array()

    @property
    def xgrid(self)->np.NDArray[float]:
        """xgrid array
        """
        if self._xgrid is None:
            self._set_xygrid()
        return self._xgrid

    @property
    def ygrid(self)->np.NDArray[float]:
        """ygrid array
        """
        if self._ygrid is None:
            self._set_xygrid()
        return self._ygrid

    @property
    def xcenter(self)->np.NDArray[float]:
        """grid x center array
        """
        return self.get_xcenter_array()

    @property
    def ycenter(self)->np.NDArray[float]:
        """grid y center array
        """
        return self.get_ycenter_array()

    @property
    def ycentergrid(self)->np.NDArray[float]:
        """grid y center array
        """
        if self._ycentergrid is None:
            self._set_xycentergrid()
        return self._ycentergrid

    @property
    def xcentergrid(self)->np.NDArray[float]:
        """grid x center array
        """
        if self._xcentergrid is None:
            self._set_xycentergrid()
        return self._xcentergrid

    @property
    def areagrid(self)->np.NDArray[float]:
        """area of grid nodes
        """
        dr, dc = np.meshgrid(self.delr, self.delc)
        return dr * dc

    def _set_xycentergrid(self):
        self._xcentergrid, self._ycentergrid = np.meshgrid(self.xcenter, self.ycenter)
        self._xcentergrid, self._ycentergrid = self.transform(
            self._xcentergrid, self._ycentergrid
        )

    def _set_xygrid(self):
        self._xgrid, self._ygrid = np.meshgrid(self.xedge, self.yedge)
        self._xgrid, self._ygrid = self.transform(self._xgrid, self._ygrid)

    def get_xedge_array(self)->np.NDArray[float]:
        """
        a numpy one-dimensional float array that has the cell edge x
        coordinates for every column in the grid in model space - not offset
        or rotated.  Array is of size (ncol + 1)

        """
        assert self.delr is not None and len(self.delr) > 0, (
            "delr not passed to " "spatial reference object"
        )
        xedge = np.concatenate(([0.0], np.add.accumulate(self.delr)))
        return xedge

    def get_yedge_array(self)->np.NDArray[float]:
        """
        a numpy one-dimensional float array that has the cell edge y
        coordinates for every row in the grid in model space - not offset or
        rotated. Array is of size (nrow + 1)

        """
        assert self.delc is not None and len(self.delc) > 0, (
            "delc not passed to " "spatial reference object"
        )
        length_y = np.add.reduce(self.delc)
        yedge = np.concatenate(([length_y], length_y - np.add.accumulate(self.delc)))
        return yedge

    def get_xcenter_array(self)->np.NDArray[float]:
        """
        a numpy one-dimensional float array that has the cell center x
        coordinate for every column in the grid in model space - not offset or rotated.

        """
        assert self.delr is not None and len(self.delr) > 0, (
            "delr not passed to " "spatial reference object"
        )
        x = np.add.accumulate(self.delr) - 0.5 * self.delr
        return x

    def get_ycenter_array(self)->np.NDArray[float]:
        """
        a numpy one-dimensional float array that has the cell center x
        coordinate for every row in the grid in model space - not offset of rotated.

        """
        assert self.delc is not None and len(self.delc) > 0, (
            "delc not passed to " "spatial reference object"
        )
        Ly = np.add.reduce(self.delc)
        y = Ly - (np.add.accumulate(self.delc) - 0.5 * self.delc)
        return y

    @staticmethod
    def rotate(x, y, theta, xorigin=0.0, yorigin=0.0):
        """
        Given x and y array-like values calculate the rotation about an
        arbitrary origin and then return the rotated coordinates.  theta is in
        degrees.

        """
        # jwhite changed on Oct 11 2016 - rotation is now positive CCW
        # theta = -theta * np.pi / 180.
        theta = theta * np.pi / 180.0

        xrot = xorigin + np.cos(theta) * (x - xorigin) - np.sin(theta) * (y - yorigin)
        yrot = yorigin + np.sin(theta) * (x - xorigin) + np.cos(theta) * (y - yorigin)
        return xrot, yrot

    def transform(self, x, y, inverse=False):
        """
        Given x and y array-like values, apply rotation, scale and offset,
        to convert them from model coordinates to real-world coordinates.
        """
        if isinstance(x, list):
            x = np.array(x)
            y = np.array(y)
        if not np.isscalar(x):
            x, y = x.copy(), y.copy()

        if not inverse:
            x += self.xll
            y += self.yll
            x, y = SpatialReference.rotate(
                x, y, theta=self.rotation, xorigin=self.xll, yorigin=self.yll
            )
        else:
            x, y = SpatialReference.rotate(x, y, -self.rotation, self.xll, self.yll)
            x -= self.xll
            y -= self.yll
        return x, y

    def get_extent(self)->tuple[float]:
        """
        Get the extent of the rotated and offset grid

        """
        x0 = self.xedge[0]
        x1 = self.xedge[-1]
        y0 = self.yedge[0]
        y1 = self.yedge[-1]

        # upper left point
        x0r, y0r = self.transform(x0, y0)

        # upper right point
        x1r, y1r = self.transform(x1, y0)

        # lower right point
        x2r, y2r = self.transform(x1, y1)

        # lower left point
        x3r, y3r = self.transform(x0, y1)

        xmin = min(x0r, x1r, x2r, x3r)
        xmax = max(x0r, x1r, x2r, x3r)
        ymin = min(y0r, y1r, y2r, y3r)
        ymax = max(y0r, y1r, y2r, y3r)

        return (xmin, xmax, ymin, ymax)

    def get_vertices(self, i, j)->list[list[float]]:
        """Get vertices for a single cell or sequence if i, j locations."""
        pts = []
        xgrid, ygrid = self.xgrid, self.ygrid
        pts.append([xgrid[i, j], ygrid[i, j]])
        pts.append([xgrid[i + 1, j], ygrid[i + 1, j]])
        pts.append([xgrid[i + 1, j + 1], ygrid[i + 1, j + 1]])
        pts.append([xgrid[i, j + 1], ygrid[i, j + 1]])
        pts.append([xgrid[i, j], ygrid[i, j]])
        if np.isscalar(i):
            return pts
        else:
            vrts = np.array(pts).transpose([2, 0, 1])
            return [v.tolist() for v in vrts]

    def get_ij(self, x, y)->tuple(int):
        """Return the row and column of a point or sequence of points
        in real-world coordinates.

        """
        if np.isscalar(x):
            c = (np.abs(self.xcentergrid[0] - x)).argmin()
            r = (np.abs(self.ycentergrid[:, 0] - y)).argmin()
        else:
            xcp = np.array([self.xcentergrid[0]] * (len(x)))
            ycp = np.array([self.ycentergrid[:, 0]] * (len(x)))
            c = (np.abs(xcp.transpose() - x)).argmin(axis=0)
            r = (np.abs(ycp.transpose() - y)).argmin(axis=0)
        return r, c

    def write_gridspec(self, filename):

        """write a PEST-style grid specification file
        Parameters
        ----------
        filename: str
            file to write


        """
        f = open(filename, "w")
        f.write("{0:10d} {1:10d}\n".format(self.delc.shape[0], self.delr.shape[0]))
        f.write(
            "{0:15.6E} {1:15.6E} {2:15.6E}\n".format(
                self.xul,
                self.yul,
                self.rotation,
            )
        )

        for r in self.delr:
            f.write("{0:15.6E} ".format(r))
        f.write("\n")
        for c in self.delc:
            f.write("{0:15.6E} ".format(c))
        f.write("\n")
        return

