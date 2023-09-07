REM Delete existing driver output files.

del driver1a.out
del driver1b.out
del driver1c.out
del driver1d.out
del driver1e.out
del driver1f.out
del driver1g.out
del umodel_usg_wel.contents
del heads_interp1.dat
del heads_interp1_sgl.dat
del coast_heads_wells.dat
del lock_heads_wells.dat
del coast_heads_wells_time_interp.dat
del lock_heads_wells_time_interp.dat
del hd1h.fac
del hd1h.bln
del hd1h_wells_sim.dat
del vdl.fac
del vdl_interp.bln
del vdl_well_heads.dat
del sop_flow_contents.dat
del sop_chd_flows.dat
del inflows.dat
del rchflow.dat
del umodel_wellflow.dat
del umodel_wellflow_interp.dat
del nfseg.cbb.contents
del nfseg_rech.dat
del factors.dat
del interpolated.ref
del factors1.dat
del interpolated1.ref
del factors3da.dat
del factors3da.dat
del factors3db.dat
del interptabb.dat
del cov2d.mat
del cov2d_unif.mat
del cov3d.mat
del cov3d_unif.mat
del fac_linear.dat
del targ1.dat
del fac_poly.dat
del targ2.dat
del interp_ipd.ref
del interptaba_ipd.dat
del interptabb_ipd.dat
del random1_2d.csv
del random1_3d.csv

REM Run the driver programs

..\driver1 < driver1a.in
..\driver1 < driver1b.in
..\driver1 < driver1c.in
..\driver1 < driver1d.in
..\driver1 < driver1e.in
..\driver1 < driver1f.in
..\driver1 < driver1g.in
..\driver1 < driver1h.in
..\driver2 < driver2a.in
..\driver2 < driver2b.in
..\driver2 < driver2c.in
..\driver3 < driver3a.in
..\driver3 < driver3b.in
..\driver4 < driver4a.in
..\driver4 < driver4b.in
..\driver5 < driver5a.in
..\driver5 < driver5b.in
..\driver5 < driver5c.in
..\driver5 < driver5d.in
..\driver5 < driver5e.in

..\driver6 < driver6a.in
..\driver7 < driver7a.in
..\driver8 < driver8a.in
..\driver8 < driver8b.in
..\driver9 < driver9a.in
..\driver9 < driver9b.in
..\driver10 < driver10a.in
..\driver10 < driver10b.in
..\driver11 < driver11a.in
..\driver11 < driver11b.in
..\driver12 < driver12a.in
..\driver13 < driver13a.in
..\driver13 < driver13b.in
..\driver14 < driver14a.in
..\driver15 < driver15a.in


REM compare
REM NOTE: fc sometimes finds differences where there are none

del diff_record.dat
fc driver1a.out driver1a.out.std > diff_record.dat
fc driver1b.out driver1b.out.std >> diff_record.dat
fc driver1c.out driver1c.out.std >> diff_record.dat
fc driver1d.out driver1d.out.std >> diff_record.dat
fc driver1e.out driver1e.out.std >> diff_record.dat
fc driver1f.out driver1f.out.std >> diff_record.dat
fc driver1g.out driver1g.out.std >> diff_record.dat
fc umodel_usg_wel.contents umodel_usg_wel.contents.std >> diff_record.dat
fc heads_interp1.dat heads_interp1.dat.std >> diff_record.dat
fc heads_interp1_sgl.dat heads_interp1_sgl.dat.std >> diff_record.dat
fc coast_heads_wells.dat coast_heads_wells.dat.std >> diff_record.dat
fc lock_heads_wells.dat lock_heads_wells.dat.std >> diff_record.dat
fc coast_heads_wells_time_interp.dat coast_heads_wells_time_interp.dat.std >> diff_record.dat
fc lock_heads_wells_time_interp.dat lock_heads_wells_time_interp.dat.std >> diff_record.dat
fc hd1h.fac hd1h.fac.std >> diff_record.dat
fc hd1h.bln hd1h.bln.std >> diff_record.dat
fc hd1h_wells_sim.dat hd1h_wells_sim.dat.std >> diff_record.dat
fc vdl.fac vdl.fac.std >> diff_record.dat
fc vdl_interp.bln vdl_interp.bln.std >> diff_record.dat
fc vdl_well_heads.dat vdl_well_heads.dat.std >> diff_record.dat
fc sop_flow_contents.dat sop_flow_contents.dat.std >> diff_record.dat
fc sop_chd_flows.dat sop_chd_flows.dat.std >> diff_record.dat
fc inflows.dat inflows.dat.std >> diff_record.dat
fc rchflow.dat rchflow.dat.std >> diff_record.dat
fc umodel_wellflow.dat umodel_wellflow.dat.std >> diff_record.dat
fc umodel_wellflow_interp.dat umodel_wellflow_interp.dat.std >> diff_record.dat
fc nfseg.cbb.contents nfseg.cbb.contents.std >> diff_record.dat
fc nfseg_rech.dat nfseg_rech.dat.std >> diff_record.dat
fc factors.dat factors.dat.std >> diff_record.dat
fc interpolated.ref interpolated.ref.std >> diff_record.dat
fc factors1.dat factors1.dat.std >> diff_record.dat
fc interpolated1.ref interpolated1.ref.std >> diff_record.dat
fc factors3da.dat factors3da.dat.std >> diff_record.dat
fc factors3da.dat factors3da.dat.std >> diff_record.dat
fc factors3db.dat factors3db.dat.std >> diff_record.dat
fc interptabb.dat interptabb.dat.std >> diff_record.dat
fc cov2d.mat cov2d.mat.std >> diff_record.dat
fc cov2d_unif.mat cov2d_unif.mat.std >> diff_record.dat
fc cov3d.mat cov3d.mat.std >> diff_record.dat
fc cov3d_unif.mat cov3d_unif.mat.std >> diff_record.dat
fc fac_linear.dat fac_linear.dat.std >> diff_record.dat
fc targ1.dat targ1.dat.std >> diff_record.dat
fc fac_poly.dat fac_poly.dat.std >> diff_record.dat
fc targ2.dat targ2.dat.std >> diff_record.dat
fc targ2.dat targ2.dat.std >> diff_record.dat
fc interp_ipd.ref interp_ipd.ref.std >> diff_record.dat
fc interptaba_ipd.dat interptaba_ipd.dat.std >> diff_record.dat
fc interptabb_ipd.dat interptabb_ipd.dat.std >> diff_record.dat
fc random1_2d.csv random1_2d.csv.std >> diff_record.dat
fc random1_3d.csv random1_3d.csv.std >> diff_record.dat