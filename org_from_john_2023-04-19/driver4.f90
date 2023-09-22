       program driver4

! -- DRIVER4 tests functions related to interpolation from a MODFLOW 6 grid.

       implicit none

! -- Functions called.

       integer     :: install_mf6_grid_from_file,   &
                      uninstall_mf6_grid,           &
                      calc_mf6_interp_factors,      &
                      interp_from_mf6_depvar_file,  &
                      retrieve_error_message,       &
                      interp_to_obstime,            &
                      free_all_memory

       integer     :: ichoice,ierr
       integer     :: ifail,idis,ncells,ndim1,ndim2,ndim3
       integer     :: mpts,npts,ipts
       integer     :: factorfiletype,icount
       integer     :: ntime,nproctime,itime,reapportion
       integer     :: numobsdat,idat
       double precision      :: interpthresh,nointerpval
       double precision      :: time_extrap
       character (len=1)     :: how_extrap
       character (len=1)     :: vartype(17)
       character (len=1)     :: gridname(200)
       character (len=1)     :: grbfile(256)
       character (len=1)     :: factorfile(256)
       character (len=1)     :: depvarfile(256)
       character (len=1)     :: messagestring(1500)
       character (len=10)    :: acount,anum
       character (len=16)    :: text
       character (len=20)    :: atemp20
       character (len=200)   :: agridname
       character (len=256)   :: agrbfile,infile,blnfile,afile
       character (len=256)   :: outfile,dvfile,obstimfile
       character (len=500)   :: cline
       character (len=1500)  :: amessage

       integer, allocatable            :: layer(:),interp_success(:)
       integer, allocatable            :: obspointnum(:)
       double precision, allocatable   :: ecoord(:),ncoord(:)
       double precision, allocatable   :: simtime(:),simstate(:,:)
       double precision, allocatable   :: obstime(:),obsval(:)
       character (len=20), allocatable :: pointname(:),obspoint(:)

! -- Initialisation

       npts=0

! -- Find out what to do.

90     write(6,*)
100    write(6,110)
110    format(' What do you want to do?')
       write(6,130)
130    format('     To end this program                             - enter 0')
       write(6,140)
140    format('     To install a set of MF6 grid specs              - enter 1')
       write(6,150)
150    format('     To uninstall a set of MF6 grid specs            - enter 2')
       write(6,160)
160    format('     To read a set of point coordinates              - enter 3')
       write(6,170)
170    format('     To write an interpolation factor file           - enter 4')
       write(6,180)
180    format('     To interpolate from an interp factor file       - enter 5')
       write(6,190)
190    format('     To time-interpolate after spatial interpolation - enter 6')
199    write(6,200,advance='no')
200    format(' Enter your choice: ')
       read(5,*,err=199) ichoice
       if((ichoice.ne.0).and.(ichoice.ne.1).and.(ichoice.ne.2).and.  &
          (ichoice.ne.3).and.(ichoice.ne.4).and.(ichoice.ne.5).and.  &
          (ichoice.ne.6)) go to 199
       if(ichoice.eq.0) go to 2000

! -- Installing grid specs.

       if(ichoice.eq.1)then
         write(6,*)
350      write(6,360,advance='no')
360      format(' Enter name of MF6 GRB file: ')
         read(5,*,err=350) agrbfile
         call char2string(200,agrbfile,grbfile)
370      write(6,380,advance='no')
380      format(' Enter name for grid: ')
         read(5,*,err=370) agridname
         call char2string(200,agridname,gridname)
         write(6,375)
375      format(' - calling function install_mf6_grid_from_file()...')
         ifail=install_mf6_grid_from_file(gridname,grbfile,                  &
               idis,ncells,ndim1,ndim2,ndim3)
         if(ifail.ne.0)then
           write(6,2030)
           ifail=retrieve_error_message(messagestring)
           call string2char(1500,messagestring,amessage)
           amessage=' '//trim(amessage)
           call writmess(6,amessage)
         else
           write(6,2020)
           write(6,385) idis
385        format('   IDIS   = ',i2)
           write(6,390) ncells
390        format('   NCELLS = ',i6)
           write(6,400) ndim1,ndim2,ndim3
400        format('   IDIM1, IDIM2, IDIM3 = ',3i6)
         end if
       else if(ichoice.eq.2)then
         write(6,*)
420      write(6,430,advance='no')
430      format(' Enter name of MF6 grid to uninstall: ')
         read(5,*,err=420) agridname
         call char2string(200,agridname,gridname)
         write(6,440)
440      format(' - calling function uninstall_mf6_grid()...')
         ifail=uninstall_mf6_grid(gridname)
         if(ifail.ne.0)then
           write(6,2030)
           ifail=retrieve_error_message(messagestring)
           call string2char(1500,messagestring,amessage)
           amessage=' '//trim(amessage)
           call writmess(6,amessage)
         else
           write(6,450)
450        format(' - uninstallation successful')
         end if
       else if (ichoice.eq.3)then
         write(6,*)
459      write(6,460,advance='no')
460      format(' Enter name of points coordinate file: ')
         read(5,*,err=459) infile
         open(unit=10,file=infile,status='old',err=459)
! -- Find out how many points
         mpts=0
         do
           read(10,*,end=480) atemp20
           mpts=mpts+1
         end do
480      continue
         if(mpts.eq.0)then
           close(unit=10)
           write(amessage,490)
490        format(' No points were found in file.')
           call writmess(6,amessage)
         else
           if(npts.ne.0)then
             if(mpts.gt.npts)then
               deallocate(pointname,ecoord,ncoord,layer,interp_success)
               allocate(pointname(mpts),ecoord(mpts),ncoord(mpts),layer(mpts),interp_success(mpts))
             end if
           else
             allocate(pointname(mpts),ecoord(mpts),ncoord(mpts),layer(mpts),interp_success(mpts))
           end if
           rewind(unit=10)
           npts=mpts
           do ipts=1,npts
             read(10,*,err=500,end=500) pointname(ipts),ecoord(ipts),ncoord(ipts),layer(ipts)
             call lowcase(pointname(ipts))
           end do
           close(unit=10)
           write(6,495)
495        format(' - file read ok.')
           go to 550
500        continue
           write(amessage,510) trim(infile)
510        format(' Error reading file ',a,'.')
           call writmess(6,amessage)
           close(unit=10,iostat=ierr)
           npts=0
           deallocate(pointname,ecoord,ncoord,layer,interp_success)
550        continue
         end if
       else if(ichoice.eq.4)then
         if(npts.eq.0)then
           write(6,*)
           write(6,551)
551        format(' No point coordinates have been read.')
           write(6,*)
           go to 635
         end if
         write(6,*)
555      write(6,556,advance='no')
556      format(' Enter name of MF6 grid to interpolate from: ')
         read(5,*,err=555) afile
         call char2string(200,afile,gridname)
560      write(6,570,advance='no')
570      format(' Enter name of factor file to write: ')
         read(5,*,err=560) afile
         call char2string(200,afile,factorfile)
580      write(6,590,advance='no')
590      format(' Is this a binary or text file? [0/1]: ')
         read(5,*,err=580) factorfiletype
         if((factorfiletype.ne.0).and.(factorfiletype.ne.1)) go to 580
600      write(6,610,advance='no')
610      format(' Enter name of bln file to write: ')
         read(5,'(a)',err=600) afile                     ! use '(a)' because it may be blank
         call char2string(200,afile,blnfile)
         write(6,611)
611      format(' - calling function calc_mf6_interpolation_factors()...')
         ifail=calc_mf6_interp_factors(gridname,                      &
                                       npts,ecoord,ncoord,layer,      &
                                       factorfile, factorfiletype,    &
                                       blnfile,                       &
                                       interp_success)
         if(ifail.ne.0)then
           write(6,2030)
           ifail=retrieve_error_message(messagestring)
           call string2char(1500,messagestring,amessage)
           amessage=' '//trim(amessage)
           call writmess(6,amessage)
         else
           write(6,620)
620        format(' - factor calculation and recording successful')
           icount=count(interp_success(1:npts).ne.0)
           call writint(acount,icount)
           call writint(anum,npts)
           write(6,630) trim(acount),trim(anum)
630        format(' - interpolation success rate = ',a,'/',a)
         end if
       else if(ichoice.eq.5)then
         if(npts.eq.0)then
           write(6,*)
           write(6,551)
           write(6,*)
           go to 635
         end if
         write(6,*)
700      write(6,710,advance='no')
710      format(' Enter name of MODFLOW 6 dependent variable file: ')
         read(5,*,err=700) dvfile
712      write(6,714,advance='no')
714      format(' How many times are represented in this file? ')
         read(5,*,err=712) ntime
         if(ntime.le.0) go to 712
715      write(6,716,advance='no')
716      format(' Enter text array identifier: ')
         read(5,*,err=715)text
         text=adjustl(text)
7160     write(6,7170,advance='no')
7170     format(' Enter inactive threshold: ')
         read(5,*,err=7160) interpthresh
7175     write(6,7176,advance='no')
7176     format(' Reapportion interp factors for dry/inact cells? [n/y = 0/1]: ')
         read(5,*,err=7175) reapportion
         if((reapportion.ne.0).and.(reapportion.ne.1)) go to 7175
7180     write(6,7190,advance='no')
7190     format(' Enter value indicating no interpolation: ')
         read(5,*,err=7180) nointerpval
         if(allocated(simtime)) deallocate(simtime)
         if(allocated(simstate)) deallocate(simstate)
         allocate(simtime(ntime))
         allocate(simstate(ntime,npts))
720      write(6,730,advance='no')
730      format(' Enter name of interpolation factor file: ')
         read(5,*,err=720) afile
740      write(6,590,advance='no')
         read(5,*,err=740) factorfiletype
         if((factorfiletype.ne.0).and.(factorfiletype.ne.1)) go to 740
         call char2string(200,dvfile,depvarfile)
         call char2string(200,afile,factorfile)
         call char2string(17,text,vartype)
         write(6,741)
741      format(' - calling function interp_from_mf6_depvar_file()...')
         ifail= interp_from_mf6_depvar_file(                          &
                 depvarfile,factorfile,factorfiletype,                &
                 ntime,vartype,interpthresh,reapportion,nointerpval,  &
                 npts,nproctime,simtime,simstate)
         if(ifail.ne.0)then
           write(6,2030)
           ifail=retrieve_error_message(messagestring)
           call string2char(1500,messagestring,amessage)
           amessage=' '//trim(amessage)
           call writmess(6,amessage)
         else
           write(6,750)
750        format(' - interpolation successful')
           write(6,752) nproctime
752        format(' Number of interpolation times =',i5)
760        write(6,770,advance='no')
770        format(' Enter file to record interpolated values: ')
           read(5,*) outfile
           open(unit=20,file=outfile,action='write',err=760)
           do ipts=1,npts
             do itime=1,nproctime
               write(20,780) trim(pointname(ipts)),simtime(itime),simstate(itime,ipts)
780            format(1x,a,t25,1pg16.9,t45,1pg16.9)
             end do
           end do
           close(unit=20)
           write(6,790) trim(outfile)
790        format(' - file ',a,' written ok.')
         end if
       else if(ichoice.eq.6)then
         write(6,*)
820      write(6,825,advance='no')
825      format(' Enter name of observation time file: ')
         read(5,*,err=820) obstimfile
         open(unit=10,file=obstimfile,status='old',err=820)
         numobsdat=0
         do
           read(10,'(a)',end=830) cline
           if(cline.ne.' ') numobsdat=numobsdat+1
         end do
830      continue
         if(numobsdat.eq.0)then
           write(amessage,826)
826        format('No observation data in this file.')
           amessage=' '//trim(amessage)
           call writmess(6,amessage)
           close(unit=10)
           go to 90
         else
           rewind(unit=10)
         end if
         if(allocated(obspoint)) deallocate(obspoint)
         if(allocated(obstime)) deallocate(obstime)
         if(allocated(obsval)) deallocate(obsval)
         if(allocated(obspointnum)) deallocate(obspointnum)
         allocate(obspoint(numobsdat),obstime(numobsdat),obsval(numobsdat),    &
         obspointnum(numobsdat))
         do idat=1,numobsdat
           read(10,*,err=821,end=821) obspoint(idat),obstime(idat)
           call lowcase(obspoint(idat))
         end do
         close(unit=10)
         call writint(atemp20,numobsdat)
         write(6,827) trim(atemp20),trim(obstimfile)
827      format(' - ',a,' measurement times read from file ',a,'.')
         go to 849
821      continue
         write(amessage,845) trim(obstimfile)
845      format('Error encountered in reading file ',a,'.')
         close(unit=10,iostat=ierr)
         go to 90
! -- Fill the OBSPOINTNUM array.
849      continue
         do idat=1,numobsdat
           do ipts=1,npts
             if(obspoint(idat).eq.pointname(ipts)) go to 850
           end do
           obspointnum(idat)=-1
           cycle
850        obspointnum(idat)=ipts-1       ! Remember that external programs indices start at 0
         end do
         open(unit=30,file='debug.dat')   !debug
         do idat=1,numobsdat              !debug
           write(30,*) obspointnum(idat)  !debug
         end do                           !debug
         close(unit=30)                   !debug
! -- Gather a few more details on how to time-interpolate.
851      write(6,852,advance='no')
852      format(' Enter inactive threshold in spatially-interpolated data: ')
         read(5,*,err=851) interpthresh
860      write(6,870,advance='no')
870      format(' Enter time-extrapolation option (L/C): ')
         read(5,*,err=860) how_extrap
         if((how_extrap.ne.'L').and.(how_extrap.ne.'l').and.   &
          (how_extrap.ne.'C').and.(how_extrap.ne.'c')) go to 860
880      write(6,890,advance='no')
890      format(' Enter extrapolation time limit: ')
         read(5,*,err=880) time_extrap
         if(time_extrap.lt.0.0d0) go to 880
895      write(6,896,advance='no')
896      format(' Enter value indicating no time interpolation: ')
         read(5,*,err=895) nointerpval
         write(6,900)
900      format(' - calling function interp_to_obstime()....')
         ifail=interp_to_obstime(                                   &
               ntime,nproctime,npts,simtime,simstate,interpthresh,  &
               how_extrap,time_extrap,nointerpval,                  &
               numobsdat,obspointnum,obstime,obsval)
         if(ifail.ne.0)then
           ifail=retrieve_error_message(messagestring)
           call string2char(1500,messagestring,amessage)
           amessage=' '//trim(amessage)
           call writmess(6,amessage)
         else
           write(6,920)
920        format(' - temporal interpolation successful.')
! -- Write the output file.
940        write(6,950,advance='no')
950        format(' Enter name for post-time-interpolation output file: ')
           read(5,*,err=940) outfile
           open(unit=21,file=outfile,action='write',err=940)
           do idat=1,numobsdat
             write(21,960) trim(obspoint(idat)),obstime(idat),obsval(idat)
960          format(1x,a,t25,1pg16.9,t50,1pg16.9)
           end do
           close(unit=21)
           write(6,970) trim(outfile)
970        format(' - file ',a,' written ok.')
         end if
       end if
635    continue
       go to 90

2000   write(6,*)
       write(6,2010)
2010   format(' Calling free_all_memory()...')
       ifail = free_all_memory()
       if(ifail.eq.0)then
         write(6,2020)
2020     format(' - function call successful.')
       else
         write(6,2030)
2030     format(' Function call unsuccessful: error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         amessage=' '//trim(amessage)
         call writmess(6,amessage)
       end if

       deallocate(pointname,ecoord,ncoord,layer,interp_success,stat=ierr)
       deallocate(simtime,simstate,stat=ierr)

       end

