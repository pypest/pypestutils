module function_interfaces

! -- Interfaces to functions comprising proc_model_outputs.lib

  interface

    integer (kind=c_int) function inquire_modflow_binary_file_specs(            &
                         FileIn,FileOut,isim,itype,iprec,narray,ntime)          &
                     bind(C,name="inquire_modflow_binary_file_specs")
    !DIR$ ATTRIBUTES DLLEXPORT :: inquire_modflow_binary_file_specs
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char), intent(in)        :: FileIn(*)
       character (kind=c_char), intent(in)        :: FileOut(*)
       integer(kind=c_int), intent(in)            :: isim
       integer(kind=c_int), intent(in)            :: itype
       integer(kind=c_int), intent(out)           :: iprec
       integer(kind=c_int), intent(out)           :: narray
       integer(kind=c_int), intent(out)           :: ntime
    end function inquire_modflow_binary_file_specs

    integer (kind=c_int) function retrieve_error_message(errormessage)             &
                    bind(c, name="retrieve_error_message")
    !DIR$ ATTRIBUTES DLLEXPORT :: retrieve_error_message
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char), intent(out) :: errormessage(*)
    end function retrieve_error_message

    integer (kind=c_int) function install_structured_grid                            &
                       (gridname,ncol,nrow,nlay,icorner,e0,n0,rotation,delr,delc)    &
                     bind(c,name="install_structured_grid")
    !DIR$ ATTRIBUTES DLLEXPORT :: install_structured_grid
       use iso_c_binding, only: c_int,c_char,c_double
       character (kind=c_char), intent(in)  :: gridname(*)
       integer (kind=c_int), intent(in)     :: ncol,nrow,nlay
       integer (kind=c_int), intent(in)     :: icorner
       real (kind=c_double), intent(in)     :: e0,n0,rotation
       real (kind=c_double), intent(in)     :: delr(ncol),delc(nrow)
    end function install_structured_grid

    integer (kind=c_int) function uninstall_structured_grid(gridname)                &
                     bind(c,name="uninstall_structured_grid")
    !DIR$ ATTRIBUTES DLLEXPORT :: uninstall_structured_grid
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char), intent(in)  :: gridname(*)
    end function uninstall_structured_grid

    integer (kind=c_int) function free_all_memory() &
                     bind(c,name="free_all_memory")
    !DIR$ ATTRIBUTES DLLEXPORT :: free_all_memory
       use iso_c_binding, only: c_int
    end function free_all_memory

    integer (kind=c_int) function interp_from_structured_grid(               &
                             GridName,DepVarFile,isim,iprec,ntime,           &
                             VarType,InterpThresh,NoInterpVal,               &
                             npts,ecoord,ncoord,layer,                       &
                             nproctime,simtime,simstate)                     &
                     bind(c,name="interp_from_structured_grid")
    !DIR$ ATTRIBUTES DLLEXPORT :: interp_from_structured_grid
       use iso_c_binding, only: c_int,c_double,c_char
       character (kind=c_char), intent(in)  :: gridname(*)
       character (kind=c_char), intent(in)  :: depvarfile(*)
       integer(kind=c_int), intent(in)      :: isim
       integer(kind=c_int), intent(in)      :: iprec
       integer(kind=c_int), intent(in)      :: ntime
       character (kind=c_char), intent(in)  :: vartype(*)
       real(kind=c_double), intent(in)      :: interpthresh
       real(kind=c_double), intent(in)      :: nointerpval
       integer(kind=c_int), intent(in)      :: npts
       real(kind=c_double), intent(in)      :: ecoord(npts),ncoord(npts)
       integer(kind=c_int), intent(in)      :: layer(npts)
       integer(kind=c_int), intent(out)     :: nproctime
       real(kind=c_double), intent(out)     :: simtime(ntime)
       real(kind=c_double), intent(out)     :: simstate(ntime,npts)
    end function interp_from_structured_grid

    integer (kind=c_int) function interp_to_obstime(                           &
                             nsimtime,nproctime,npts,simtime,simval,           &
                             interpthresh,how_extrap,time_extrap,nointerpval,  &
                             nobs,obspoint,obstime,obssimval)                  &
                     bind(c,name="interp_to_obstime")
    !DIR$ ATTRIBUTES DLLEXPORT :: interp_to_obstime
       use iso_c_binding, only: c_int,c_double,c_char
       integer(kind=c_int), intent(in)         :: nsimtime
       integer(kind=c_int), intent(in)         :: nproctime
       integer(kind=c_int), intent(in)         :: npts
       real(kind=c_double), intent(in)         :: simtime(nsimtime)
       real(kind=c_double), intent(in)         :: simval(nsimtime,npts)
       real(kind=c_double), intent(in)         :: interpthresh
       character(kind=c_char,len=1),intent(in) :: how_extrap
       real(kind=c_double), intent(in)         :: time_extrap
       real(kind=c_double), intent(in)         :: nointerpval
       integer(kind=c_int), intent(in)         :: nobs
       integer(kind=c_int), intent(in)         :: obspoint(nobs)
       real(kind=c_double), intent(in)         :: obstime(nobs)
       real(kind=c_double), intent(out)        :: obssimval(nobs)
    end function interp_to_obstime

    integer (kind=c_int) function install_mf6_grid_from_file(         &
                         gridname,grbfile,                            &
                         idis,ncells,ndim1,ndim2,ndim3)               &
                     bind(c,name="install_mf6_grid_from_file")
    !DIR$ ATTRIBUTES DLLEXPORT :: install_mf6_grid_from_file
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char,len=1), intent(in)  :: gridname(*)
       character (kind=c_char,len=1), intent(in)  :: grbfile(*)
       integer(kind=c_int), intent(out)           :: idis
       integer(kind=c_int), intent(out)           :: ncells
       integer(kind=c_int), intent(out)           :: ndim1,ndim2,ndim3
    end function install_mf6_grid_from_file

    integer (kind=c_int) function uninstall_mf6_grid(gridname)         &
                     bind(c,name="uninstall_mf6_grid")
    !DIR$ ATTRIBUTES DLLEXPORT :: uninstall_mf6_grid
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char,len=1), intent(in)  :: gridname(*)
    end function uninstall_mf6_grid

    integer (kind=c_int) function calc_mf6_interp_factors(             &
                              gridname,                                &
                              npts,ecoord,ncoord,layer,                &
                              factorfile, factorfiletype,              &
                              blnfile,interp_success)                  &
                     bind(c,name="calc_mf6_interp_factors")
    !DIR$ ATTRIBUTES DLLEXPORT :: calc_mf6_interp_factors
       use iso_c_binding, only: c_int,c_double,c_char
       character (kind=c_char,len=1), intent(in)   :: gridname(*)
       integer(kind=c_int), intent(in)             :: npts
       real(kind=c_double), intent(in)             :: ecoord(npts),ncoord(npts)
       integer(kind=c_int), intent(in)             :: layer(npts)
       character(kind=c_char,len=1), intent(in)    :: factorfile(*)
       integer(kind=c_int), intent(in)             :: factorfiletype
       character(kind=c_char,len=1), intent(in)    :: blnfile(*)
       integer(kind=c_int), intent(out)            :: interp_success(npts)
    end function calc_mf6_interp_factors

    integer (kind=c_int) function interp_from_mf6_depvar_file(                 &
                         depvarfile,factorfile,factorfiletype,                 &
                         ntime,vartype,interpthresh,reapportion,nointerpval,   &
                         npts,nproctime,simtime,simstate)                      &
                     bind(c,name="interp_from_mf6_depvar_file")
    !DIR$ ATTRIBUTES DLLEXPORT :: interp_from_mf6_depvar_file
           use iso_c_binding, only: c_int,c_char,c_double
           character(kind=c_char,len=1), intent(in)   :: depvarfile(*)
           character(kind=c_char,len=1), intent(in)   :: factorfile(*)
           integer(kind=c_int), intent(in)            :: factorfiletype
           integer(kind=c_int), intent(in)            :: ntime
           character (kind=c_char,len=1), intent(in)  :: vartype(*)
           real(kind=c_double), intent(in)            :: interpthresh
           integer(kind=c_int), intent(in)            :: reapportion
           real(kind=c_double), intent(in)            :: nointerpval
           integer(kind=c_int), intent(in)            :: npts
           integer(kind=c_int), intent(out)           :: nproctime
           real(kind=c_double), intent(out)           :: simtime(ntime)
           real(kind=c_double), intent(out)           :: simstate(ntime,npts)
    end function interp_from_mf6_depvar_file

    integer (kind=c_int) function extract_flows_from_cbc_file(     &
                         cbcfile,flowtype,isim,iprec,              &
                         ncell,izone,nzone,                        &
                         numzone,zonenumber,                       &
                         ntime,nproctime,                          &
                         timestep,stressperiod,simtime,simflow)    &
                     bind(c,name="extract_flows_from_cbc_file")
    !DIR$ ATTRIBUTES DLLEXPORT :: extract_flows_from_cbc_file
       use iso_c_binding, only: c_int,c_char,c_double
       character (kind=c_char,len=1), intent(in)  :: cbcfile(*)
       character (kind=c_char,len=1), intent(in)  :: flowtype(*)
       integer(kind=c_int), intent(in)            :: isim
       integer(kind=c_int), intent(in)            :: iprec
       integer(kind=c_int), intent(in)            :: ncell
       integer(kind=c_int), intent(in)            :: izone(ncell)
       integer(kind=c_int), intent(in)            :: nzone
       integer(kind=c_int), intent(out)           :: numzone
       integer(kind=c_int), intent(out)           :: zonenumber(nzone)
       integer(kind=c_int), intent(in)            :: ntime
       integer(kind=c_int), intent(out)           :: nproctime
       integer(kind=c_int), intent(out)           :: timestep(ntime)
       integer(kind=c_int), intent(out)           :: stressperiod(ntime)
       real(kind=c_double), intent(out)           :: simtime(ntime)
       real(kind=c_double), intent(out)           :: simflow(ntime,nzone)
    end function extract_flows_from_cbc_file

  end interface

! -- Interfaces to functions comprising proc_model_outputs.lib

  interface

    integer (kind=c_int) function calc_kriging_factors_2d(        &
                            npts,ecs,ncs,zns,                     &
                            mpts,ect,nct,znt,                     &
                            vartype,krigtype,                     &
                            aa,anis,bearing,                      &
                            searchrad,maxpts,minpts,              &
                            factorfile,factorfiletype,            &
                            icount_interp)
       use iso_c_binding, only: c_int,c_char,c_double
       integer(kind=c_int), intent(in)           :: npts
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)
       integer(kind=c_int), intent(in)           :: zns(npts)
       integer(kind=c_int), intent(in)           :: mpts
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)
       integer(kind=c_int), intent(in)           :: znt(mpts)
       integer(kind=c_int), intent(in)           :: vartype
       integer(kind=c_int), intent(in)           :: krigtype
       real(kind=c_double), intent(in)           :: aa(mpts)
       real(kind=c_double), intent(in)           :: anis(mpts)
       real(kind=c_double), intent(in)           :: bearing(mpts)
       real(kind=c_double), intent(in)           :: searchrad
       integer(kind=c_int), intent(in)           :: maxpts, minpts
       character (kind=c_char,len=1), intent(in) :: factorfile(*)
       integer(kind=c_int), intent(in)           :: factorfiletype
       integer(kind=c_int), intent(out)          :: icount_interp
    end function calc_kriging_factors_2d

    integer (kind=c_int) function calc_kriging_factors_auto_2d(     &
                                   npts,ecs,ncs,zns,                &
                                   mpts,ect,nct,znt,                &
                                   krigtype,                        &
                                   anis,bearing,                    &
                                   factorfile,factorfiletype,       &
                                   icount_interp)
       use iso_c_binding, only: c_int,c_char,c_double
       integer(kind=c_int), intent(in)           :: npts
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)
       integer(kind=c_int), intent(in)           :: zns(npts)
       integer(kind=c_int), intent(in)           :: mpts
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)
       integer(kind=c_int), intent(in)           :: znt(mpts)
       integer(kind=c_int), intent(in)           :: krigtype
       real(kind=c_double), intent(in)           :: anis(mpts)
       real(kind=c_double), intent(in)           :: bearing(mpts)
       character (kind=c_char,len=1), intent(in) :: factorfile(*)
       integer(kind=c_int), intent(in)           :: factorfiletype
       integer(kind=c_int), intent(out)          :: icount_interp
    end function calc_kriging_factors_auto_2d

    integer (kind=c_int) function krige_using_file(      &
                         factorfile,factorfiletype,      &
                         npts,mpts,                      &
                         krigtype,transtype,             &
                         sourceval,targval,              &
                         icount_interp,                  &
                         meanval)
       use iso_c_binding, only: c_int,c_char,c_double
       character (kind=c_char,len=1), intent(in) :: factorfile(*)
       integer(kind=c_int), intent(in)           :: factorfiletype
       integer(kind=c_int), intent(in)           :: npts
       integer(kind=c_int), intent(in)           :: mpts
       integer(kind=c_int), intent(in)           :: krigtype
       integer(kind=c_int), intent(in)           :: transtype
       real(kind=c_double), intent(in)           :: sourceval(npts)
       real(kind=c_double), intent(out)          :: targval(mpts)
       integer(kind=c_int), intent(out)          :: icount_interp
       real(kind=c_double), intent(in), optional :: meanval(mpts)
    end function krige_using_file

    integer (kind=c_int) function calc_kriging_factors_3d(        &
                                   npts,ecs,ncs,zcs,zns,          &
                                   mpts,ect,nct,zct,znt,          &
                                   krigtype,                      &
                                   nzone,zonenum,                 &
                                   vartype,                       &
                                   ahmax,ahmin,avert,             &
                                   bearing,dip,rake,              &
                                   srhmax,srhmin,srvert,          &
                                   maxpts,minpts,                 &
                                   factorfile,factorfiletype,     &
                                   icount_interp)
       use iso_c_binding, only: c_int,c_char,c_double
       integer(kind=c_int), intent(in)         :: npts
       real(kind=c_double), intent(in)         :: ecs(npts),ncs(npts),zcs(npts)
       integer(kind=c_int), intent(in)         :: zns(npts)
       integer(kind=c_int), intent(in)         :: mpts
       real(kind=c_double), intent(in)         :: ect(mpts),nct(mpts),zct(mpts)
       integer(kind=c_int), intent(in)         :: znt(mpts)
       integer(kind=c_int), intent(in)         :: krigtype
       integer(kind=c_int), intent(in)         :: nzone
       integer(kind=c_int), intent(in)         :: zonenum(nzone)
       integer(kind=c_int), intent(in)         :: vartype(nzone)
       real(kind=c_double), intent(in)         :: ahmax(nzone),ahmin(nzone),avert(nzone)
       real(kind=c_double), intent(in)         :: bearing(nzone)
       real(kind=c_double), intent(in)         :: dip(nzone)
       real(kind=c_double), intent(in)         :: rake(nzone)
       real(kind=c_double), intent(in)         :: srhmax
       real(kind=c_double), intent(in)         :: srhmin
       real(kind=c_double), intent(in)         :: srvert
       integer(kind=c_int), intent(in)         :: maxpts, minpts
       character (kind=c_char,len=1), intent(in) :: factorfile(*)
       integer(kind=c_int), intent(in)         :: factorfiletype
       integer(kind=c_int), intent(out)        :: icount_interp
    end function calc_kriging_factors_3d

    integer (kind=c_int) function build_covar_matrix_2d(    &
                              npts,ec,nc,zn,                &
                              vartype,                      &
                              nugget,aa,sill,anis,bearing,  &
                              ldcovmat,covmat)
       use iso_c_binding, only: c_int,c_double
       integer(kind=c_int), intent(in)         :: npts
       real(kind=c_double), intent(in)         :: ec(npts),nc(npts)
       integer(kind=c_int), intent(in)         :: zn(npts)
       integer(kind=c_int), intent(in)         :: vartype
       real(kind=c_double), intent(in)         :: nugget(npts)
       real(kind=c_double), intent(in)         :: aa(npts)
       real(kind=c_double), intent(in)         :: sill(npts)
       real(kind=c_double), intent(in)         :: anis(npts)
       real(kind=c_double), intent(in)         :: bearing(npts)
       integer(kind=c_int), intent(in)         :: ldcovmat
       real(kind=c_double), intent(out)        :: covmat(ldcovmat,npts)
    end function build_covar_matrix_2d

    integer (kind=c_int) function build_covar_matrix_3d(        &
                                  npts,ec,nc,zc,zn,             &
                                  vartype,                      &
                                  nugget,sill,                  &
                                  ahmax,ahmin,avert,            &
                                  bearing,dip,rake,             &
                                  ldcovmat,covmat)
       use iso_c_binding, only: c_int,c_double
       integer(kind=c_int), intent(in)         :: npts
       real(kind=c_double), intent(in)         :: ec(npts),nc(npts),zc(npts)
       integer(kind=c_int), intent(in)         :: zn(npts)
       integer(kind=c_int), intent(in)         :: vartype
       real(kind=c_double), intent(in)         :: nugget(npts)
       real(kind=c_double), intent(in)         :: sill(npts)
       real(kind=c_double), intent(in)         :: ahmax(npts),ahmin(npts),avert(npts)
       real(kind=c_double), intent(in)         :: bearing(npts),dip(npts),rake(npts)
       integer(kind=c_int), intent(in)         :: ldcovmat
       real(kind=c_double), intent(out)        :: covmat(ldcovmat,npts)
    end function build_covar_matrix_3d

    integer (kind=c_int) function calc_structural_overlay_factors(     &
                         npts,                                         &
                         ecs,ncs,ids,                                  &
                         conwidth,aa,                                  &
                         structype,inverse_power,                      &
                         mpts,                                         &
                         ect,nct,active,                               &
                         factorfile,factorfiletype,                    &
                         icount_interp)
       use iso_c_binding, only: c_int,c_char,c_double
       integer(kind=c_int), intent(in)           :: npts
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)
       integer(kind=c_int), intent(in)           :: ids(npts)
       real(kind=c_double), intent(in)           :: conwidth(npts),aa(npts)
       integer(kind=c_int), intent(in)           :: structype
       real(kind=c_double), intent(in)           :: inverse_power
       integer(kind=c_int), intent(in)           :: mpts
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)
       integer(kind=c_int), intent(in)           :: active(mpts)
       character (kind=c_char,len=1), intent(in) :: factorfile(*)
       integer(kind=c_int), intent(in)           :: factorfiletype
       integer(kind=c_int), intent(out)          :: icount_interp
    end function calc_structural_overlay_factors

   integer (kind=c_int) function interpolate_blend_using_file(     &
                                 factorfile,factorfiletype,        &
                                 npts,mpts,                        &
                                 transtype,                        &
                                 lt_target,gt_target,              &
                                 sourceval,targval,                &
                                 icount_interp)
       use iso_c_binding, only: c_int,c_char,c_double
       character (kind=c_char,len=1), intent(in) :: factorfile(*)
       integer(kind=c_int), intent(in)           :: factorfiletype
       integer(kind=c_int), intent(in)           :: npts
       integer(kind=c_int), intent(in)           :: mpts
       integer(kind=c_int), intent(in)           :: transtype
       character (kind=c_char,len=1), intent(in) :: lt_target,gt_target
       real(kind=c_double), intent(in)           :: sourceval(npts)
       real(kind=c_double), intent(inout)        :: targval(mpts)
       integer(kind=c_int), intent(out)          :: icount_interp
   end function interpolate_blend_using_file

   integer (kind=c_int) function ipd_interpolate_2d(     &
                              npts,                      &
                              ecs,ncs,zns,sourceval,     &
                              mpts,                      &
                              ect,nct,znt,targval,       &
                              transtype,                 &
                              anis,bearing,invpow)
       use iso_c_binding, only: c_int,c_double
       integer(kind=c_int), intent(in)           :: npts
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)
       integer(kind=c_int), intent(in)           :: zns(npts)
       real(kind=c_double), intent(in)           :: sourceval(npts)
       integer(kind=c_int), intent(in)           :: mpts
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)
       integer(kind=c_int), intent(in)           :: znt(mpts)
       real(kind=c_double), intent(out)          :: targval(mpts)
       integer(kind=c_int), intent(in)           :: transtype
       real(kind=c_double), intent(in)           :: anis(mpts)
       real(kind=c_double), intent(in)           :: bearing(mpts)
       real(kind=c_double), intent(in)           :: invpow(mpts)
   end function ipd_interpolate_2d

   integer (kind=c_int) function ipd_interpolate_3d(  &
                        npts,                         &
                        ecs,ncs,zcs,zns,sourceval,    &
                        mpts,                         &
                        ect,nct,zct,znt,targval,      &
                        transtype,                    &
                        ahmax,ahmin,avert,            &
                        bearing,dip,rake,             &
                        invpow)
       use iso_c_binding, only: c_int,c_double
       integer(kind=c_int), intent(in)    :: npts
       real(kind=c_double), intent(in)    :: ecs(npts),ncs(npts),zcs(npts)
       integer(kind=c_int), intent(in)    :: zns(npts)
       real(kind=c_double), intent(in)    :: sourceval(npts)
       integer(kind=c_int), intent(in)    :: mpts
       real(kind=c_double), intent(in)    :: ect(mpts),nct(mpts),zct(mpts)
       integer(kind=c_int), intent(in)    :: znt(mpts)
       real(kind=c_double), intent(out)   :: targval(mpts)
       integer(kind=c_int), intent(in)    :: transtype
       real(kind=c_double), intent(in)    :: ahmax(mpts),ahmin(mpts),avert(mpts)
       real(kind=c_double), intent(in)    :: bearing(mpts),dip(mpts),rake(mpts)
       real(kind=c_double), intent(in)    :: invpow(mpts)
   end function ipd_interpolate_3d

   integer (kind=c_int) function initialize_randgen(iseed)
       use iso_c_binding, only: c_int
       integer(kind=c_int), intent(in)    :: iseed
   end function initialize_randgen

   integer (kind=c_int) function fieldgen2d_sva(             &
                              nnode,                         &
                              ec,nc,area,active,             &
                              mean,var,aa,anis,bearing,      &
                              transtype,avetype,power,       &
                              ldrand,nreal,randfield)
       use iso_c_binding, only: c_int,c_double
       integer(kind=c_int), intent(in)   :: nnode
       real(kind=c_double), intent(in)   :: ec(nnode),nc(nnode)
       real(kind=c_double), intent(in)   :: area(nnode)
       integer(kind=c_int), intent(in)   :: active(nnode)
       real(kind=c_double), intent(in)   :: mean(nnode)
       real(kind=c_double), intent(in)   :: var(nnode)
       real(kind=c_double), intent(in)   :: aa(nnode)
       real(kind=c_double), intent(in)   :: anis(nnode)
       real(kind=c_double), intent(in)   :: bearing(nnode)
       integer(kind=c_int), intent(in)   :: transtype
       integer(kind=c_int), intent(in)   :: avetype
       real(kind=c_double), intent(in)   :: power
       integer(kind=c_int), intent(in)   :: ldrand
       integer(kind=c_int), intent(in)   :: nreal
       real(kind=c_double), intent(out)  :: randfield(ldrand,nreal)
   end function fieldgen2d_sva

   integer (kind=c_int) function fieldgen3d_sva(             &
                              nnode,                         &
                              ec,nc,zc,                      &
                              area,height,active,            &
                              mean,var,                      &
                              ahmax,ahmin,avert,             &
                              bearing,dip,rake,              &
                              transtype,avetype,power,       &
                              ldrand,nreal,randfield)
       use iso_c_binding, only: c_int,c_double
       integer(kind=c_int), intent(in)   :: nnode
       real(kind=c_double), intent(in)   :: ec(nnode),nc(nnode),zc(nnode)
       real(kind=c_double), intent(in)   :: area(nnode)
       real(kind=c_double), intent(in)   :: height(nnode)
       integer(kind=c_int), intent(in)   :: active(nnode)
       real(kind=c_double), intent(in)   :: mean(nnode)
       real(kind=c_double), intent(in)   :: var(nnode)
       real(kind=c_double), intent(in)   :: ahmax(nnode),ahmin(nnode),avert(nnode)
       real(kind=c_double), intent(in)   :: bearing(nnode)
       real(kind=c_double), intent(in)   :: dip(nnode)
       real(kind=c_double), intent(in)   :: rake(nnode)
       integer(kind=c_int), intent(in)   :: transtype
       integer(kind=c_int), intent(in)   :: avetype
       real(kind=c_double), intent(in)   :: power
       integer(kind=c_int), intent(in)   :: ldrand
       integer(kind=c_int), intent(in)   :: nreal
       real(kind=c_double), intent(out)  :: randfield(ldrand,nreal)
   end function fieldgen3d_sva

end interface

end module function_interfaces

