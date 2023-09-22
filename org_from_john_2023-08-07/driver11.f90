       program driver11

! -- DRIVER11 tests structural overlay parameter functionality. It tests the building
!    and use of interpolation and blending factors that implement it.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

! -- Variables used in function calls.

       integer(kind=c_int)               :: ifail
       integer(kind=c_int)               :: npts,mpts
       integer(kind=c_int)               :: structype,factorfiletype,transtype
       integer(kind=c_int)               :: icount_interp,icount
       real(kind=c_double)               :: inverse_power
       character(kind=c_char,len=1)      :: lt_target,gt_target
       character(kind=c_char,len=1)      :: factorfile(256)
       character(kind=c_char,len=1)      :: messagestring(1500)
       integer(kind=c_int), allocatable  :: active(:),ids(:)
       real(kind=c_double), allocatable  :: ecs(:),ncs(:)
       real(kind=c_double), allocatable  :: ect(:),nct(:),zct(:)
       real(kind=c_double), allocatable  :: conwidth(:),aa(:)
       real(kind=c_double), allocatable  :: sourceval(:),targval(:)

! -- Other variables.

       integer                       :: jfail,ierr,iline
       integer                       :: ipt
       integer                       :: ibeg,iend
       integer                       :: lw(15),rw(15)
       double precision              :: dtemp
       character (len=1)             :: ah,aaa,al
       character (len=10)            :: aline,anum
       character (len=256)           :: infile,outfile,facfile,afile
       character (len=500)           :: cline
       character (len=1500)          :: amessage

       character (len=20), allocatable    :: apoint(:)

! -- Obtain coordinates of grid cell centres.

100    write(6,110,advance='no')
110    format(' Enter name of file containing grid cell centre coordinates: ')
       read(5,*,err=100) infile
       open(unit=10,file=infile,status='old',err=100)
115    write(6,116,advance='no')
116    format(' Skip header line? (y/n): ')
       read(5,*) ah
       if((ah.eq.'Y').or.(ah.eq.'y'))then
         ah='y'
       else if((ah.eq.'N').or.(ah.eq.'n'))then
         ah='n'
       else
         go to 115
       end if
       mpts=0
       do
         read(10,'(a)',end=200) cline
         if(cline.eq.' ')cycle
         mpts=mpts+1
       end do
200    continue
       if(ah.eq.'y')mpts=mpts-1
       if(mpts.eq.0)then
         write(6,201)
201      format(/,' *** No coordinates in file. Try again ***',/)
         close(unit=10)
         go to 100
       end if
       allocate(ect(mpts),nct(mpts),zct(mpts),active(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(targval(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       rewind (unit=10)
       iline=0
       if(ah.eq.'y')then
         iline=iline+1
         read(10,*)
       end if
       do ipt=1,mpts
210      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(cline.eq.' ') go to 210
         call linesplit(jfail,4,lw,rw,cline)
         if(jfail.ne.0) go to 9070
         call drealread(jfail,cline(lw(1):rw(1)),ect(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(2):rw(2)),nct(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(3):rw(3)),zct(ipt))
         if(jfail.ne.0) go to 9150
         call intread(jfail,cline(lw(4):rw(4)),active(ipt))
         if(jfail.ne.0) go to 9150
       end do
       close(unit=10)
       call writint(anum,mpts)
       write(6,220) trim(anum),trim(infile)
220    format(' - coordinates of ',a,' points read from file ',a,'.')

       write(6,*)
250    write(6,260,advance='no')
260    format(' Enter background value for grid cell properties: ')
       read(5,*,err=250) dtemp
       targval=dtemp      ! an array

       write(6,*)
300    write(6,310,advance='no')
310    format(' Enter name of file containing structural overlay details: ')
       read(5,*,err=300) infile
       open(unit=10,file=infile,status='old',err=300)
315    write(6,316,advance='no')
316    format(' Skip header line? (y/n): ')
       read(5,*) ah
       if((ah.eq.'Y').or.(ah.eq.'y'))then
         ah='y'
       else if((ah.eq.'N').or.(ah.eq.'n'))then
         ah='n'
       else
         go to 315
       end if
       npts=0
       do
         read(10,'(a)',end=400) cline
         if(cline.eq.' ')cycle
         npts=npts+1
       end do
400    continue
       if(ah.eq.'y')npts=npts-1
       if(npts.eq.0)then
         write(6,401)
401      format(/,' *** File appears to be empty. Try again ***',/)
         close(unit=10)
         go to 300
       end if
       allocate(ecs(npts),ncs(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(apoint(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(ids(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(conwidth(npts),aa(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(sourceval(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       rewind (unit=10)
       iline=0
       if(ah.eq.'y')then
         iline=iline+1
         read(10,*)
       end if
       do ipt=1,npts
410      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(cline.eq.' ') go to 410
         call linesplit(jfail,7,lw,rw,cline)
         if(jfail.ne.0) go to 9070
         apoint(ipt)=cline(lw(1):rw(1))
         call drealread(jfail,cline(lw(2):rw(2)),ecs(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(3):rw(3)),ncs(ipt))
         if(jfail.ne.0) go to 9150
         call intread(jfail,cline(lw(4):rw(4)),ids(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(5):rw(5)),conwidth(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(6):rw(6)),aa(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(7):rw(7)),sourceval(ipt))
         if(jfail.ne.0) go to 9150
       end do
       close(unit=10)
       call writint(anum,npts)
       write(6,420) trim(anum),trim(infile)
420    format(' - coordinates of ',a,' points read from file ',a,'.')

       write(6,*)
430    write(6,440,advance='no')
440    format(' Are structures polylinear or polygonal? (l/p): ')
       read(5,*,err=430) ah
       if((ah.eq.'l').or.(ah.eq.'L'))then
         structype=0
       else if ((ah.eq.'p').or.(ah.eq.'P'))then
         structype=1
       else
         go to 430
       end if
445    write(6,446,advance='no')
446    format(' Enter inv power of distance: ')
       read(5,*,err=445) inverse_power

! -- The name of the factor file is aquired.

       write(6,*)
450    write(6,460,advance='no')
460    format(' Enter name for factor file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 450
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,facfile,ibeg,iend)
       if(ifail.ne.0) go to 450
       call char2string(256,facfile,factorfile)
471    write(6,480,advance='no')
480    format(' Is this a text or binary file? (t/b): ')
       read(5,*,err=471) aaa
       if((aaa.eq.'t').or.(aaa.eq.'T'))then
         factorfiletype=1
       else if ((aaa.eq.'b').or.(aaa.eq.'B'))then
         factorfiletype=0
       else
         go to 471
       end if

! -- Now call the function to calculate factors.

       write(6,*)
       write(6,520)
520    format(' Calling calc_structural_overlay_factors()....')
       ifail=calc_structural_overlay_factors(        &
                     npts,                           &
                     ecs,ncs,ids,                    &
                     conwidth,aa,                    &
                     structype,inverse_power,        &
                     mpts,                           &
                     ect,nct,active,                 &
                     factorfile,factorfiletype,      &
                     icount_interp)
       if(ifail.ne.0)then
         write(6,*)
         write(6,540)
540      format(' Function call unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(anum,'(i0)') icount_interp
         anum=adjustl(anum)
         write(6,550) trim(anum)
550      format(' Interpolation factors calculated for ',a,' points.')
       end if

! -- We will now re-read the factor file and undertake interpolation.

       write(6,*)
       write(6,600)
600    format(' Now undertaking spatial interpolation.')
       write(6,*)
609    write(6,610,advance='no')
610    format(' Interpolate in natural or log domain? (n/l): ')
       read(5,*) al
       if((al.eq.'N').or.(al.eq.'n'))then
         transtype=0
       else if((al.eq.'l').or.(al.eq.'L'))then
         transtype=1
       else
         go to 609
       end if
       lt_target='y'
       gt_target='y'
       ifail=interpolate_blend_using_file(               &
                         factorfile,factorfiletype,      &
                         npts,mpts,                      &
                         transtype,                      &
                         lt_target,gt_target,            &
                         sourceval,targval,              &
                         icount)
       if(ifail.ne.0)then
         write(6,*)
         write(6,540)
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(anum,'(i0)') icount
         anum=adjustl(anum)
         write(6,630) trim(anum)
630      format(' Interpolation undertaken to ',a,' points.')
       end if

! -- We now store the results as a list of numbers.

       write(6,*)
640    write(6,650,advance='no')
650    format(' Enter filename for target array storage: ')
       read(5,*,err=640) outfile
       open(unit=20,file=outfile,action='write',err=640)
       do ipt=1,mpts
         write(20,660) targval(ipt)
660      format(1pg16.9)
       end do
       close(unit=20)
       write(6,470) trim(outfile)
470    format(' - file ',a,' written ok.')

       go to 9900

9000   write(amessage,9010) trim(infile)
9010   format('Error encountered in reading file ',a,'.')
       go to 9890

9050   write(amessage,9060) trim(infile)
9060   format('Premature end encountered to file ',a,'.')
       go to 9890

9070   call writint(aline,iline)
       write(amessage,9080) trim(aline),trim(infile)
9080   format('Insufficient entries on line ',a,' of file ',a,'.')
       go to 9890

9150   call writint(aline,iline)
       write(amessage,9160) trim(aline),trim(infile)
9160   format('Error reading data on line ',a,' of file ',a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format('Insufficient memory to continue execution.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

! -- Tidy up

9900   continue

! -- Free local memory

       deallocate(active,ids,stat=ierr)
       deallocate(ecs,ncs,stat=ierr)
       deallocate(ect,nct,zct,stat=ierr)
       deallocate(conwidth,aa,stat=ierr)
       deallocate(sourceval,targval,stat=ierr)
       deallocate(apoint,stat=ierr)

! -- Free function interface memory

       ifail=free_all_memory()

       end



