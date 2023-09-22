       program driver10

! -- DRIVER10 tests construction of a 3D covariance matrix based on a spatially-varying variogram.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

! -- Variables used in function calls.

       integer(kind=c_int)           :: ifail
       integer(kind=c_int)           :: npts,ldcovmat
       integer(kind=c_int)           :: vartype
       character(kind=c_char,len=1)  :: messagestring(1500)

! -- Other variables.

       integer                       :: jfail,ierr,iline
       integer                       :: ipt,jpt
       integer                       :: lw(15),rw(15)
       character (len=1)             :: ah
       character (len=10)            :: aline
       character (len=256)           :: infile,outfile
       character (len=500)           :: cline
       character (len=1500)          :: amessage

! -- Allocatable variables that are used in function calls.

       integer(kind=c_int), allocatable   :: zn(:)
       real(kind=c_double), allocatable   :: ec(:),nc(:),zc(:)
       real(kind=c_double), allocatable   :: nugget(:),sill(:)
       real(kind=c_double), allocatable   :: ahmax(:),ahmin(:),avert(:)
       real(kind=c_double), allocatable   :: bearing(:),dip(:),rake(:)
       real(kind=c_double), allocatable   :: covmat(:,:)

! -- Other allocatable variables.

       character (len=20), allocatable    :: apoint(:)

! -- Read the pilot point statistical specification file.

100    write(6,110,advance='no')
110    format(' Enter name of 3D pilot points statistical specs file: ')
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
       npts=0
       do
         read(10,'(a)',end=200) cline
         if(cline.eq.' ')cycle
         npts=npts+1
       end do
200    continue
       if(ah.eq.'y')npts=npts-1
       if(npts.eq.0)then
         write(6,201)
201      format(/,' *** No points in file. Try again ***',/)
         close(unit=10)
         go to 100
       end if
       allocate(ec(npts),nc(npts),zc(npts),zn(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(nugget(npts),sill(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(ahmax(npts),ahmin(npts),avert(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(bearing(npts),dip(npts),rake(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(apoint(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       rewind (unit=10)
       iline=0
       if(ah.eq.'y')then
         iline=iline+1
         read(10,*)
       end if
       do ipt=1,npts
210      continue
         iline=iline+1
         read(10,'(a)',err=9000,end=9050) cline
         if(cline.eq.' ') go to 210
         call linesplit(jfail,13,lw,rw,cline)
         if(jfail.ne.0) go to 9070
         apoint(ipt)=cline(lw(1):rw(1))
         call lowcase(apoint(ipt))
         call drealread(jfail,cline(lw(2):rw(2)),ec(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(3):rw(3)),nc(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(4):rw(4)),zc(ipt))
         if(jfail.ne.0) go to 9150
         call intread(jfail,cline(lw(5):rw(5)),zn(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(6):rw(6)),nugget(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(7):rw(7)),sill(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(8):rw(8)),ahmax(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(9):rw(9)),ahmin(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(10):rw(10)),avert(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(11):rw(11)),bearing(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(12):rw(12)),dip(ipt))
         if(jfail.ne.0) go to 9150
         call drealread(jfail,cline(lw(13):rw(13)),rake(ipt))
         if(jfail.ne.0) go to 9150
       end do
       close(unit=10)
       write(6,220) trim(infile)
220    format(' - file ',a,' read ok.')

       write(6,*)
230    write(6,240,advance='no')
240    format(' Enter variogram type (1:spher,2:exp,3:gauss,4:pow): ')
       read(5,*,err=230) vartype
       if((vartype.lt.1).or.(vartype.gt.4)) go to 230

! -- Now calculate the covariance matrix.

       allocate(covmat(npts,npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       ldcovmat=npts
       write(6,*)
       write(6,320)
320    format(' Calling build_covar_matrix_2d()....')
       ifail=build_covar_matrix_3d(                         &
                              npts,ec,nc,zc,zn,             &
                              vartype,                      &
                              nugget,sill,                  &
                              ahmax,ahmin,avert,            &
                              bearing,dip,rake,             &
                              ldcovmat,covmat)
       if(ifail.ne.0)then
         write(6,340)
340      format(' Function call unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(6,350)
350      format(' Covariance matrix calculated.')
       end if

! -- Now store the covariance matrix in a file.

       write(6,*)
370    write(6,380,advance='no')
380    format(' Enter name of file to store covariance matrix: ')
       read(5,*,err=370) outfile
       open(unit=20,file=outfile,action='write',err=370)
       write(20,'(3i6)') npts,npts,1
       do ipt=1,npts
         write(20,430) (covmat(ipt,jpt),jpt=1,npts)
430      format(8(1x,1pg14.7))
       end do
       write(20,440)
440    format('* row and column names')
       do ipt=1,npts
         write(20,'(a)') trim(apoint(ipt))
       end do
       close(unit=20)
       write(6,450) trim(outfile)
450    format(' - file ',a,' written ok.')

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

       deallocate(zn,stat=ierr)
       deallocate(ec,nc,zc,stat=ierr)
       deallocate(nugget,sill,stat=ierr)
       deallocate(ahmax,ahmin,avert,stat=ierr)
       deallocate(bearing,dip,rake,stat=ierr)
       deallocate(covmat,stat=ierr)
       deallocate(apoint,stat=ierr)

! -- Free function interface memory

       ifail=free_all_memory()

       end



