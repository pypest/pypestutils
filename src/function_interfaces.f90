module function_interfaces

  interface

    integer (kind=c_int) function inquire_modflow_binary_file_specs              &
                         (FileIn,FileOut,isim,itype,iprec,narray,ntime)          &
                         bind(C,name="inquire_modflow_binary_file_specs_")
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char), intent(in)        :: FileIn(:)
       character (kind=c_char), intent(in)        :: FileOut(:)
       integer(kind=c_int), intent(in)            :: isim
       integer(kind=c_int), intent(in)            :: itype
       integer(kind=c_int), intent(out)           :: iprec
       integer(kind=c_int), intent(out)           :: narray
       integer(kind=c_int), intent(out)           :: ntime
    end function inquire_modflow_binary_file_specs

    integer(kind=c_int) function retrieve_error_message(errormessage)             &
                        bind(c, name="retrieve_error_message_")
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char), intent(out) :: errormessage(:)
    end function retrieve_error_message

    integer (kind=c_int) function install_structured_grid                            &
                       (gridname,ncol,nrow,nlay,icorner,e0,n0,rotation,delr,delc)    &
                        bind(c,name="install_structured_grid_")
       use iso_c_binding, only: c_int,c_char,c_double
       character (kind=c_char), intent(in)  :: gridname(:)
       integer (kind=c_int), intent(in)     :: ncol,nrow,nlay
       integer (kind=c_int), intent(in)     :: icorner
       real (kind=c_double), intent(in)     :: e0,n0,rotation
       real (kind=c_double), intent(in)     :: delr(ncol),delc(nrow)
    end function install_structured_grid

    integer (kind=c_int) function uninstall_structured_grid(gridname)                &
                         bind(c,name="uninstall_structured_grid_")
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char), intent(in)  :: gridname(:)
    end function uninstall_structured_grid

    integer (kind=c_int) function free_all_memory() bind(c,name="free_all_memory_")
       use iso_c_binding, only: c_int
    end function free_all_memory

    integer (kind=c_int) function interp_from_structured_grid(                   &
                             GridName,DepVarFile,isim,iprec,ntime,           &
                             VarType,InterpThresh,NoInterpVal,               &
                             npts,ecoord,ncoord,layer,                       &
                             nproctime,simtime,simstate)                     &
                             bind(c,name="interp_from_structured_grid_")
       use iso_c_binding, only: c_int,c_double,c_char
       character (kind=c_char), intent(in)  :: gridname(:)
       character (kind=c_char), intent(in)  :: depvarfile(:)
       integer(kind=c_int), intent(in)      :: isim
       integer(kind=c_int), intent(in)      :: iprec
       integer(kind=c_int), intent(in)      :: ntime
       character (kind=c_char), intent(in)  :: vartype(:)
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
                             bind(c,name="interp_to_obstime_")
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

    integer (kind=c_int) function install_mf6_grid_from_file(gridname,grbfile,    &
                         idis,ncells,ndim1,ndim2,ndim3)                           &
                         bind(c,name="install_mf6_grid_from_file_")
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char,len=1), intent(in)  :: gridname(:)
       character (kind=c_char,len=1), intent(in)  :: grbfile(:)
       integer(kind=c_int), intent(out)           :: idis
       integer(kind=c_int), intent(out)           :: ncells
       integer(kind=c_int), intent(out)           :: ndim1,ndim2,ndim3
    end function install_mf6_grid_from_file

    integer (kind=c_int) function uninstall_mf6_grid(gridname)         &
                         bind(c,name="uninstall_mf6_grid_")
       use iso_c_binding, only: c_int,c_char
       character (kind=c_char,len=1), intent(in)  :: gridname(:)
    end function uninstall_mf6_grid

    integer (kind=c_int) function calc_mf6_interp_factors(gridname,    &
                              npts,ecoord,ncoord,layer,                &
                              factorfile, factorfiletype,              &
                              blnfile,interp_success)                  &
                              bind(c,name="calc_mf6_interp_factors_")
       use iso_c_binding, only: c_int,c_double,c_char
       character (kind=c_char,len=1), intent(in)   :: gridname(:)
       integer(kind=c_int), intent(in)             :: npts
       real(kind=c_double), intent(in)             :: ecoord(npts),ncoord(npts)
       integer(kind=c_int), intent(in)             :: layer(npts)
       character(kind=c_char,len=1), intent(in)    :: factorfile(:)
       integer(kind=c_int), intent(in)             :: factorfiletype
       character(kind=c_char,len=1), intent(in)    :: blnfile(:)
       integer(kind=c_int), intent(out)            :: interp_success(npts)
    end function calc_mf6_interp_factors

    integer (kind=c_int) function interp_from_mf6_depvar_file(                 &
                         depvarfile,factorfile,factorfiletype,                 &
                         ntime,vartype,interpthresh,reapportion,nointerpval,   &
                         npts,nproctime,simtime,simstate)                      &
                         bind(c,name="interp_from_mf6_depvar_file_")
           use iso_c_binding, only: c_int,c_char,c_double
           character(kind=c_char,len=1), intent(in)   :: depvarfile(:)
           character(kind=c_char,len=1), intent(in)   :: factorfile(:)
           integer(kind=c_int), intent(in)            :: factorfiletype
           integer(kind=c_int), intent(in)            :: ntime
           character (kind=c_char,len=1), intent(in)  :: vartype(:)
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
                         bind(c,name="extract_flows_from_cbc_file_")
       use iso_c_binding, only: c_int,c_char,c_double
       character (kind=c_char,len=1), intent(in)  :: cbcfile(:)
       character (kind=c_char,len=1), intent(in)  :: flowtype(:)
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

end module function_interfaces

