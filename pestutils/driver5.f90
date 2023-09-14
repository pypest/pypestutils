       program driver5

! -- DRIVER5 tests the reading of a cell-by-cell flow term file.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

       integer(kind=c_int)             :: ifail
       integer(kind=c_int)             :: ncell,nzone,isim
       integer(kind=c_int)             :: itype,iprec,narray,ntime
       integer(kind=c_int)             :: numzone,nproctime,numobsdat
       real(kind=c_double)             :: time_extrap,nointerpval,interpthresh
       character(kind=c_char,len=1)    :: how_extrap
       character(kind=c_char,len=1)    :: flowtype(17)
       character(kind=c_char,len=1)    :: cbcfile(256),recfile(256)
       character(kind=c_char,len=1)    :: messagestring(1500)

       integer(kind=c_int),allocatable :: izone(:)
       integer(kind=c_int),allocatable :: zonenumber(:)
       integer(kind=c_int),allocatable :: timestep(:),stressperiod(:)
       integer(kind=c_int),allocatable :: obszonenum(:)
       real(kind=c_double),allocatable :: simtime(:),simflow(:,:)
       real(kind=c_double),allocatable :: obstime(:),obsval(:)

       integer           :: ierr,itemp
       integer           :: icell,iz,itime,iobs
       character (len=1)    :: aa
       character (len=17)   :: atext
       character (len=20)   :: atemp20
       character (len=30)   :: atype,aprec
       character (len=256)  :: infile,afile1,afile2,outfile,cline
       character (len=1500) :: amessage

       character (len=20), allocatable   :: obspoint(:)

! -- Initialization
       cbcfile=' '  ! an array

       write(6,*)
100    write(6,110,advance='no')
110    format(' Enter number of cells in model: ')
       read(5,*,err=100) ncell
       if(ncell.le.0) go to 100
50     write(6,60)
60     format(' A simulator code is required.')
       write(6,70)
70     format(' (1=mf; 21=mfusg_s; 22=mfusg_us; 31=mf6_dis; 32=mfusg_disv; 33=mfusg_disu)')
79     write(6,80,advance='no')
80     format(' Enter simulation code: ')
       read(5,*,err=79) isim

! -- Allocate some memory

       allocate(izone(ncell),stat=ierr)
       if(ierr.ne.0) go to 9200
       izone=0        ! an array

! -- Read the zonation file.

       write(6,*)
150    write(6,160,advance='no')
160    format(' Enter file containing model cell zone numbers: ')
       read(5,*) infile
       open(unit=10,file=infile,status='old',err=150)
       do
         read(10,*,end=200) icell, itemp
         if((icell.le.0).or.(icell.gt.ncell)) then
           call writint(atemp20,icell)
           write(amessage,171) trim(atemp20),trim(infile)
171        format('Cell number ',a,' out of range in file ',a,'.')
           go to 9890
         end if
         izone(icell)=itemp
       end do
200    close(unit=10)
       write(6,210) trim(infile)
210    format(' - file ',a,' read ok.')
211    write(6,212,advance='no')
212    format(' Enter a number that equals or exceeds number of separate zones: ')
       read(5,*,err=211) nzone
       if(nzone.le.0) go to 211

! -- Obtain the name of the cbc flow term file to read.

       write(6,*)
230    write(6,240,advance='no')
240    format(' Enter name of model-generated cbc flow term file to read: ')
       read(5,*,err=230) afile1
250    write(6,260,advance='no')
260    format(' Enter name of file to record contents of this file: ')
       read(5,*,err=250) afile2
       call char2string(256,afile1,cbcfile)
       call char2string(256,afile2,recfile)

! -- Call function inquire_modflow_binary_file_specs() to pre-read file.

       write(6,*)
       write(6,270)
270    format(' - calling function inquire_modflow_binary_file_specs()...')
       itype=2
       ifail=inquire_modflow_binary_file_specs(cbcfile,recfile,     &
       isim,itype,iprec,narray,ntime)
       if(ifail.eq.0)then
         write(6,280)
280      format(' - function call successful')
         atype='cell-by-cell flow term'
         if(iprec.eq.1)then
           aprec='single'
         else if(iprec.eq.2)then
           aprec='double'
         endif
         write(6,170) trim(atype)
170      format(' FILE TYPE                      = ',a)
         write(6,180) trim(aprec)
180      format(' PRECISION                      = ',a)
         write(6,190) narray
190      format(' Number of arrays in file       =',i5)
         write(6,195) ntime
195      format(' Number of output times in file =',i5)
       else
         write(6,290,advance='no')
290      format(/,' - function call unsuccessful; error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       end if
       write(6,*)

! -- Obtain other information that is needed by function extract_flows_from_cbc_file().

350    write(6,360,advance='no')
360    format(' Enter text that denotes flow type of interest: ')
       read(5,*,err=350) atext
       call lowcase(atext)
       atext=adjustl(atext)
       call char2string(17,atext,flowtype)

! -- Allocate more memory

       allocate(zonenumber(nzone),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(timestep(ntime),stressperiod(ntime),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(simtime(ntime),simflow(ntime,nzone),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- Call the function.

       write(6,370)
370    format(' - calling function extract_flows_from_cbc_file()...')
       ifail=extract_flows_from_cbc_file(                 &
                 cbcfile,flowtype,isim,iprec,             &
                 ncell,izone,nzone,                       &
                 numzone,zonenumber,                      &
                 ntime,nproctime,                         &
                 timestep,stressperiod,simtime,simflow)
       if(ifail.eq.0)then
         write(6,280)
         write(6,402) nproctime
402      format('   NPROCTIME = ',i5)
         write(6,390) numzone
390      format('   NUMZONE   = ',i5)
         write(6,401)
401      format('   Zone numbers:-')
         write(6,403) (zonenumber(iz),iz=1,numzone)
403      format(5i8)
       else
         write(6,291,advance='no')
291      format(/,' - function call unsuccessful; error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       end if

! -- Record extracted results in a file.

       write(6,*)
400    write(6,410,advance='no')
410    format(' Enter name of flow-in-zone file to write: ')
       read(5,*) outfile
       open(unit=20,file=outfile,action='write')
       write(20,411)
411    format(' ZONENUMBER',t20,'KSTP',t30,'KPER',t40,'SIMTIME',t55,'SIMFLOW')
       do iz=1,numzone
         do itime=1,ntime
           write(20,420) zonenumber(iz),timestep(itime),stressperiod(itime),   &
           simtime(itime),simflow(itime,iz)
420        format(1x,i5,t20,i5,t30,i5,t40,1pg16.9,t55,1pg16.9)
         end do
       end do
       close(unit=20)
       write(6,430) trim(outfile)
430    format(' - file ',a,' written ok.')

! -- If the simulation times are all -1.0 (because of an old MF budget file type),
!    we give them artificial times.

       do itime=1,ntime
         if(simtime(itime).lt.-0.5) simtime(itime)=itime
       end do

! -- Time-interpolation will now be undertaken.

       write(6,*)
431    write(6,432,advance='no')
432    format(' Undertake time interpolation? (y/n): ')
       read(5,*,err=431) aa
       if((aa.eq.'n').or.(aa.eq.'N')) go to 1000
       if((aa.ne.'y').and.(aa.ne.'Y')) go to 431
440    write(6,450,advance='no')
450    format(' Enter name of observation time file: ')
       read(5,*,err=440) infile
       open(unit=10,file=infile,status='old',err=440)
       numobsdat=0
       do
         read(10,'(a)',end=460) cline
         if(cline.ne.' ') numobsdat=numobsdat+1
       end do
460    continue
       if(numobsdat.eq.0)then
         write(6,470)
470      format('No observation data in this file. Try again.')
         close(unit=10)
         go to 440
       else
         rewind(unit=10)
       end if
       allocate(obspoint(numobsdat),obstime(numobsdat),obsval(numobsdat),    &
       obszonenum(numobsdat),stat=ierr)
       if(ierr.ne.0) go to 9200
       do iobs=1,numobsdat
         read(10,*,err=9050,end=9050) obspoint(iobs),obstime(iobs)
         call lowcase(obspoint(iobs))
       end do
       close(unit=10)
       write(6,210) trim(infile)

! -- Extract zone numbers from from observation point names.

       do iobs=1,numobsdat
         obszonenum(iobs)=-1
         atemp20=obspoint(iobs)
         call lowcase(atemp20)
         atemp20=adjustl(atemp20)
         if(atemp20(1:4).eq.'zone')then
           atemp20=atemp20(5:)
           call intread(ifail,atemp20,itemp)
           if(ifail.eq.0)then
             do iz=1,numzone
               if(itemp.eq.zonenumber(iz))then
                 obszonenum(iobs)=iz-1             ! Don't forget requires indexing beginning at 0
                 go to 510
               end if
             end do
           end if
         end if
510      continue
       end do

! -- Now undertake temporal interpolation.

       write(6,*)
860    write(6,870,advance='no')
870    format(' Enter time-extrapolation option (L/C): ')
       read(5,*,err=860) how_extrap
       if((how_extrap.ne.'L').and.(how_extrap.ne.'l').and.   &
        (how_extrap.ne.'C').and.(how_extrap.ne.'c')) go to 860
880    write(6,890,advance='no')
890    format(' Enter extrapolation time limit: ')
       read(5,*,err=880) time_extrap
       if(time_extrap.lt.0.0d0) go to 880
895    write(6,896,advance='no')
896    format(' Enter value indicating no time interpolation: ')
       read(5,*,err=895) nointerpval
       interpthresh=1.0d300
       write(6,520)
520    format(' - calling function interp_to_obstime()....')
       ifail=interp_to_obstime(                                   &
             ntime,nproctime,nzone,simtime,simflow,interpthresh,  &
             how_extrap,time_extrap,nointerpval,                  &
             numobsdat,obszonenum,obstime,obsval)
       if(ifail.ne.0)then
         write(6,290,advance='no')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(6,920)
920      format(' - temporal interpolation successful.')
! -- Write the output file.
940      write(6,950,advance='no')
950      format(' Enter name for post-time-interpolation output file: ')
         read(5,*,err=940) outfile
         open(unit=21,file=outfile,action='write',err=940)
         do iobs=1,numobsdat
           write(21,960) trim(obspoint(iobs)),obstime(iobs),obsval(iobs)
960        format(1x,a,t25,1pg16.9,t50,1pg16.9)
         end do
         close(unit=21)
         write(6,970) trim(outfile)
970      format(' - file ',a,' written ok.')
       end if

1000   continue
       go to 9900

9050   write(amessage,9060) trim(infile)
9060   format('Error encountered in reading file ',a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format('DRIVER5 memory allocation error.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

! -- Tidy up

9900   continue
       write(6,*)
       write(6,9910)
9910   format(' Tidying up...')
       write(6,9915)
9915   format(' - calling function free_all_memory()...')
       ifail=free_all_memory()
       if(ifail.eq.0)then
         write(6,280)
         write(6,*)
       else
         write(6,9921)
9921     format(' Function call unsuccessful; error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         amessage=' '//trim(amessage)
         call writmess(6,amessage)
       end if
       deallocate(izone,zonenumber,stat=ierr)
       deallocate(timestep,stressperiod,stat=ierr)
       deallocate(obszonenum,stat=ierr)
       deallocate(simtime,simflow,stat=ierr)
       deallocate(obstime,obsval,stat=ierr)
       deallocate(obspoint,stat=ierr)

       end
