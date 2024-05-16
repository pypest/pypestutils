import os
import logging
import pytest
import numpy as np
import pandas as pd



def test_mf6_mod2obs():
	import pypestutils.helpers as helpers
	start_datetime = pd.to_datetime("1-1-2000")
	
	dts = []
	for dt in np.linspace(100,300,10):
		dts.append(start_datetime+pd.to_timedelta(dt,unit="d"))
	obs_df = pd.DataFrame({"site":["site1" for _ in range(len(dts))],
		"x":[101 for _ in range(len(dts))],
		"y":[221 for _ in range(len(dts))],
		"layer":[1 for _ in range(len(dts))]})
	obs_df.loc[:,"datetime"] = dts
	

	ws = os.path.join("data","p09")
	case = "gwf-p09-mf6"
	obs_csv = os.path.join(ws,"obs.csv")
	obs_df.to_csv(obs_csv)

	dv_name = os.path.join(ws,case+".hds")
	grb_name = os.path.join(ws,case+".dis.grb")

	results = helpers.mod2obs_mf6(grb_name,dv_name,obs_csv,31,start_datetime,1,"head")
	iresults = results["interpolated_results"]
	assert iresults.shape[0] == obs_df.shape[0]

	dv_name = os.path.join(ws,"gwt-p09-mf6.ucn")
	results = helpers.mod2obs_mf6(grb_name,dv_name,obs_csv,31,start_datetime,1,"concentration")
	iresults = results["interpolated_results"]
	assert iresults.shape[0] == obs_df.shape[0]
	

if __name__ == "__main__":
	import sys
	sys.path.insert(0,os.path.join(".."))
	test_mf6_mod2obs()