import os


def setup_pps(d):
    cwd = os.getcwd()
    os.chdir(d)
    apply_pps()
    os.chdir(cwd)


def apply_pps():
    import numpy as np
    import pandas as pd

    from pypestutils import helpers

    df = pd.read_csv("pp_info.csv")
    gridspec_fname = "org.grb"
    for model_file, pp_file in zip(df.model_file, df.pp_file):
        ppdf = pd.read_csv(pp_file)
        results = helpers.interpolate_with_sva_pilotpoints_2d(
            ppdf, gridspec_fname, vartransform="log"
        )
        org_arr = np.loadtxt(model_file)
        interp = results["result"]
        interp = interp.reshape(org_arr.shape)
        new_arr = org_arr * interp
        new_arr = new_arr
        new_arr[new_arr < 1.0e-10] = 1.0e-10
        np.savetxt(model_file, new_arr, fmt="%15.6E")
        np.savetxt("interp_" + model_file, interp, fmt="%15.6E")
        np.savetxt("log_" + model_file, np.log10(new_arr), fmt="%15.6E")


# -----------------------------------------------------------------------------
# synth2d model and z-parameter helpers used by pestpp_ies_with_z_parameters.ipynb
# -----------------------------------------------------------------------------

# hyper parameter fields that control the hk variogram: the transform each one
# is kriged with
HYPER_FIELDS = {"range": "log", "bearing": "none", "aniso": "log"}


def synth2d_well_cells(nrow, ncol):
    """three pumping wells spread through the interior, scaled with the grid"""
    return [
        (0, int(nrow * f), int(ncol * g))
        for f, g in ((0.30, 0.35), (0.55, 0.60), (0.75, 0.30))
    ]


def synth2d_head_obs_cells(nrow, ncol):
    """a handful of monitoring locations, not on top of the wells"""
    return {
        f"gw{i:02d}": (0, int(nrow * f), int(ncol * g))
        for i, (f, g) in enumerate(
            ((0.20, 0.20), (0.45, 0.45), (0.65, 0.75), (0.85, 0.50))
        )
    }


def build_synth2d_model(
    ws,
    exe_name="mf6",
    nrow=25,
    ncol=25,
    delr=100.0,
    delc=100.0,
    top=60.0,
    botm=0.0,
    strt=45.0,
    hk=5.0,
    sy=0.15,
    ss=1.0e-5,
    riv_stage=44.0,
    riv_rbot=41.0,
    riv_cond=500.0,
    inflow_total=2500.0,
    wel_rate=-900.0,
    nper_trans=12,
    perlen_trans=30.4,
    run=True,
):
    """build (and optionally run) the synth2d model from the PEST++ benchmarks.

    one layer: specified inflow along the right edge flows west to a river on
    the left edge, with three pumping wells that switch on for the transient
    periods.  period 0 is steady state.  hk is written to the external file
    "hk.txt" so it can be replaced at run time.
    """
    import shutil

    import flopy
    import numpy as np

    if os.path.exists(ws):
        shutil.rmtree(ws)
    os.makedirs(ws)

    sim = flopy.mf6.MFSimulation(
        sim_name="synth2d", sim_ws=ws, exe_name=exe_name, version="mf6"
    )
    perioddata = [(1.0, 1, 1.0)] + [(perlen_trans, 1, 1.0)] * nper_trans
    flopy.mf6.ModflowTdis(
        sim, nper=len(perioddata), perioddata=perioddata, time_units="days"
    )
    flopy.mf6.ModflowIms(
        sim,
        complexity="moderate",
        outer_dvclose=1.0e-6,
        inner_dvclose=1.0e-7,
        linear_acceleration="bicgstab",
    )
    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname="synth2d",
        save_flows=True,
        newtonoptions="NEWTON UNDER_RELAXATION",
    )
    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=1,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        length_units="meters",
    )
    flopy.mf6.ModflowGwfic(gwf, strt=strt)
    flopy.mf6.ModflowGwfnpf(
        gwf,
        icelltype=1,
        k={"filename": "hk.txt", "data": np.full((nrow, ncol), hk)},
        save_specific_discharge=True,
    )
    flopy.mf6.ModflowGwfsto(
        gwf,
        iconvert=1,
        sy=sy,
        ss=ss,
        steady_state={0: True},
        transient={i: True for i in range(1, len(perioddata))},
    )

    # river along the left edge; one boundname so the total flux is one observation
    riv_spd = [[(0, i, 0), riv_stage, riv_cond, riv_rbot, "river"] for i in range(nrow)]
    riv = flopy.mf6.ModflowGwfriv(
        gwf, boundnames=True, save_flows=True, stress_period_data={0: riv_spd}
    )
    riv.obs.initialize(
        filename="synth2d.riv.obs",
        continuous={"riv_obs.csv": [("rivflux", "riv", "river")]},
    )

    # specified inflow along the right edge
    q_in = inflow_total / float(nrow)
    inflow = [[(0, i, ncol - 1), q_in] for i in range(nrow)]
    flopy.mf6.ModflowGwfwel(
        gwf,
        pname="inflow",
        filename="synth2d.inflow.wel",
        stress_period_data={0: inflow},
    )

    # pumping wells, on for the transient periods only
    pumps = [[c, wel_rate] for c in synth2d_well_cells(nrow, ncol)]
    flopy.mf6.ModflowGwfwel(
        gwf,
        pname="pump",
        filename="synth2d.pump.wel",
        stress_period_data={1: pumps},
    )

    hobs = {
        "head_obs.csv": [
            (nm, "head", c) for nm, c in synth2d_head_obs_cells(nrow, ncol).items()
        ]
    }
    flopy.mf6.ModflowUtlobs(gwf, filename="synth2d.obs", continuous=hobs)
    flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord="synth2d.hds",
        budget_filerecord="synth2d.cbc",
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )
    sim.write_simulation(silent=True)
    if run:
        ok, buff = sim.run_simulation(silent=True)
        if not ok:
            raise RuntimeError("synth2d model failed:\n" + "\n".join(buff[-25:]))
    return sim


def interpolate_hyper_field(ppdf, grb_fname, transform):
    """krige hyper parameter pilot point "value"s to the grid using each pilot
    point's "corrlen", "bearing" and "aniso".  the kriging factor files are
    written to a temporary directory."""
    import tempfile

    from pypestutils import helpers

    grb_fname = os.path.abspath(grb_fname)
    cwd = os.getcwd()
    with tempfile.TemporaryDirectory() as tmp_d:
        os.chdir(tmp_d)
        try:
            results = helpers.interpolate_with_sva_pilotpoints_2d(
                ppdf.copy(), grb_fname, vartransform=transform, verbose=False
            )
        finally:
            os.chdir(cwd)
    return results["result"]


def hk_from_z(z, hyper_ppdfs, grb_fname, mean, variance):
    """map z values to hk, with the hk variogram range, bearing and anisotropy
    interpolated from the hyper parameter pilot points.  returns hk and a dict
    of the hyper parameter fields"""
    from pypestutils import helpers

    fields = {
        name: interpolate_hyper_field(hyper_ppdfs[name], grb_fname, transform)
        for name, transform in HYPER_FIELDS.items()
    }
    hk = helpers.generate_2d_grid_field_from_z(
        z,
        os.path.abspath(grb_fname),
        mean=mean,
        variance=variance,
        variorange=fields["range"],
        variobearing=fields["bearing"],
        varioaniso=fields["aniso"],
        variotransform="log",
    )
    return hk, fields


def apply_hk_from_z():
    """forward run: map the current z and hyper parameter pilot point values
    (written by PEST++) to hk.txt for MODFLOW-6.  the hyper parameter fields are
    also written so they can be tracked as observations"""
    import numpy as np
    import pandas as pd

    info = pd.read_csv("hk_info.csv", index_col=0)["value"]
    z = pd.read_csv("z.csv")
    hyper_ppdfs = {name: pd.read_csv(f"hyperpp_{name}.csv") for name in HYPER_FIELDS}
    hk, fields = hk_from_z(z, hyper_ppdfs, "org.grb", info["mean"], info["variance"])
    np.savetxt("hk.txt", hk, fmt="%15.6E")
    for name, arr in fields.items():
        np.savetxt(f"hyper_{name}.txt", arr, fmt="%15.6E")


if __name__ == "__main__":
    setup_pps()
