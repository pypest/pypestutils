       program driver7

! -- DRIVER7 tests 2-D kriging interpolation using calc_kriging_factors_auto_2d().
!    This is a slightly-modified version of DRIVER6.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

! -- Variables used in function calls.

       integer(kind=c_int)           :: ifail
       integer(kind=c_int)           :: npts
       integer(kind=c_int)           :: mpts
       integer(kind=c_int)           :: krigtype
       integer(kind=c_int)           :: transtype
       character (kind=c_char,len=1) :: factorfile(256)
       integer(kind=c_int)           :: factorfiletype
       character(kind=c_char,len=1)  :: messagestring(1500)
       integer(kind=c_int)           :: icount_interp,icount

! -- Other variables

       integer               :: ierr,ifile
       integer               :: nrow,ncol,irow,icol
       integer               :: ipt,ibeg,iend
       double precision      :: pi
       double precision      :: e0,n0,angle
       double precision      :: cosang,sinang
       double precision      :: nointerpval
       character (len=1)     :: aaa,al
       character (len=20)    :: arraytype,anum
       character (len=200)   :: cline
       character (len=256)   :: infile,facfile,afile,outfile
       character (len=1500)  :: amessage

! -- Allocatable variables; first the ones that are used in function calls.

       integer(kind=c_int), allocatable   :: zns(:),znt(:)
       real(kind=c_double), allocatable   :: ecs(:),ncs(:),vals(:)
       real(kind=c_double), allocatable   :: ect(:),nct(:),valt(:)
       real(kind=c_double), allocatable   :: mean(:),anis(:),bearing(:)

! -- Other allocatable variables.

       double precision, allocatable      :: delr(:),delc(:)
       double precision, allocatable      :: x(:),y(:)
       character (len=20), allocatable    :: apoint(:)

! -- Initialization

       pi=3.14159265358979000
       icount_interp=0

! -- Read the grid specification file.

100    write(6,110,advance='no')
110    format(' Enter name of MF grid spec file: ')
       read(5,*,err=100) infile
       open(unit=10,file=infile,status='old',err=100)
       read(10,*,err=9000,end=9050) nrow,ncol
       read(10,*,err=9000,end=9050) e0,n0,angle
       allocate(delr(ncol),delc(nrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       read(10,*,err=9000,end=9050) (delr(icol),icol=1,ncol)
       read(10,*,err=9000,end=9050) (delc(irow),irow=1,nrow)
       close(unit=10)
       write(6,120) trim(infile)
120    format(' - file ',a,' read ok.')
       mpts=ncol*nrow

! -- Allocate target arrays.

       allocate(mean(mpts),anis(mpts),bearing(mpts),znt(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- Read arrays associated with this grid.

       do ifile=1,5
         if(ifile.eq.1) cycle
         write(6,*)
         if(ifile.eq.2)then
           arraytype='mean'
         else if(ifile.eq.3)then
           arraytype='anisotropy'
         else if(ifile.eq.4)then
           arraytype='anis. bearing'
         else if(ifile.eq.5)then
           arraytype='zone'
         end if
149      write(6,150,advance='no') trim(arraytype)
150      format(' Enter name of ',a,' array file: ')
         read(5,*,err=149) infile
         open(unit=10,file=infile,status='old',err=149)
         if(ifile.eq.2)then
           read(10,*,err=9000,end=9050) (mean(ipt),ipt=1,mpts)
         else if(ifile.eq.3)then
           read(10,*,err=9000,end=9050) (anis(ipt),ipt=1,mpts)
         else if(ifile.eq.4)then
           read(10,*,err=9000,end=9050) (bearing(ipt),ipt=1,mpts)
         else if(ifile.eq.5)then
           read(10,*,err=9000,end=9050) (znt(ipt),ipt=1,mpts)
         end if
         close(unit=10)
         write(6,120) trim(infile)
       end do

! -- The pilot points file is read.

       write(6,*)
180    write(6,190,advance='no')
190    format(' Enter name of pilot points file: ')
       read(5,*,err=180) infile
       open(unit=10,file=infile,status='old',err=180)
       npts=0
       do
         read(10,'(a)',end=200) cline
         if(cline.eq.' ')cycle
         npts=npts+1
       end do
200    continue
       if(npts.eq.0)then
         write(6,201)
201      format(/,' *** No points in file. Try again ***',/)
         close(unit=10)
         go to 180
       end if
       allocate(apoint(npts),ecs(npts),ncs(npts),zns(npts),vals(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       rewind(10)
       do ipt=1,npts
         read(10,*,err=9000,end=9050) apoint(ipt),ecs(ipt),ncs(ipt),zns(ipt),vals(ipt)
       end do
       close(unit=10)
       write(6,120) trim(infile)

! -- Enter some control variables.

       write(6,*)
230    write(6,240,advance='no')
240    format(' Enter kriging type (0=simple; 1=ordinary): ')
       read(5,*,err=230) krigtype
       write(6,*)
250    write(6,260,advance='no')
260    format(' Enter name for factor file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 250
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,facfile,ibeg,iend)
       if(ifail.ne.0) go to 250
       call char2string(256,facfile,factorfile)
270    write(6,280,advance='no')
280    format(' Is this a text or binary file? (t/b): ')
       read(5,*,err=270) aaa
       if((aaa.eq.'t').or.(aaa.eq.'T'))then
         factorfiletype=1
       else if ((aaa.eq.'b').or.(aaa.eq.'B'))then
         factorfiletype=0
       else
         go to 270
       end if

! -- Convert the array centre coordinates to lists.

       cosang=cos(angle*pi/180.0d0)
       sinang=sin(angle*pi/180.0d0)
       allocate(ect(mpts),nct(mpts),valt(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(x(ncol),y(nrow),stat=ierr)
       if(ierr.ne.0) go to 9200

       do icol=1,ncol
         if(icol.eq.1)then
           x(icol)=delr(1)*0.5
         else
           x(icol)=x(icol-1)+(delr(icol)+delr(icol-1))*0.5
         end if
       end do
       do irow=1,nrow
         if(irow.eq.1)then
           y(irow)=-delc(irow)*0.5
         else
           y(irow)=y(irow-1)-(delc(irow)+delc(irow-1))*0.5
         end if
       end do
       ipt=0
       do irow=1,nrow
         do icol=1,ncol
           ipt=ipt+1
           ect(ipt)=x(icol)*cosang-y(irow)*sinang+e0
           nct(ipt)=x(icol)*sinang+y(irow)*cosang+n0
         end do
       end do

! -- Now call the function to to the interpolation.

       write(6,*)
       write(6,320)
320    format(' Calling calc_kriging_factors_2d()....')
       ifail=calc_kriging_factors_auto_2d(npts,ecs,ncs,zns,                 &
                                   mpts,ect,nct,znt,                        &
                                   krigtype,                                &
                                   anis,bearing,                            &
                                   factorfile,factorfiletype,               &
                                   icount_interp)
       if(ifail.ne.0)then
         write(6,340)
340      format(' Function call unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(anum,'(i0)') icount_interp
         anum=adjustl(anum)
         write(6,350) trim(anum)
350      format(' Interpolation factors calculated for ',a,' points.')
       end if

! -- We will now re-read the file and undertake interpolation. The outcomes
!    will be written as a MODFLOW-compatible real array.

       write(6,*)
       write(6,400)
400    format(' Now undertaking spatial interpolation.')
       write(6,*)
409    write(6,410,advance='no')
410    format(' Interpolate in natural or log domain? (n/l): ')
       read(5,*) al
       if((al.eq.'N').or.(al.eq.'n'))then
         transtype=0
       else if((al.eq.'l').or.(al.eq.'L'))then
         transtype=1
       else
         go to 409
       end if
405    write(6,406,advance='no')
406    format(' Enter number indicating no interpolation to target array: ')
       read(5,*,err=405) nointerpval
       valt=nointerpval ! an array

       write(6,*)
       write(6,420)
420    format(' Calling krige_using_file()...')
       ifail=krige_using_file(factorfile,factorfiletype,   &
                              npts,mpts,                   &
                              krigtype,transtype,          &
                              vals,valt,                   &
                              icount,mean)
       if(ifail.ne.0)then
         write(6,430)
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(anum,'(i0)') icount
         anum=adjustl(anum)
         write(6,430) trim(anum)
430      format(' Interpolation undertaken to ',a,' points.')
       end if

! -- We now store the results as a real array.

       write(6,*)
440    write(6,450,advance='no')
450    format(' Enter filename for real array storage: ')
       read(5,*,err=440) outfile
       open(unit=20,file=outfile,action='write',err=440)
       write(20,460) (valt(ipt),ipt=1,mpts)
460    format(1pg16.9)
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

9200   write(amessage,9210)
9210   format('Insufficient memory to continue execution.')
       go to 9890


9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

! -- Tidy up

9900   continue

! -- Free local memory

       deallocate(zns,znt,stat=ierr)
       deallocate(ecs,ncs,vals,stat=ierr)
       deallocate(ect,nct,valt,stat=ierr)
       deallocate(mean,anis,bearing,stat=ierr)
       deallocate(delr,delc,stat=ierr)
       deallocate(x,y,stat=ierr)
       deallocate(apoint,stat=ierr)

! -- Free function interface memory

       ifail=free_all_memory()

       end



