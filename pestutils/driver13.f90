       program driver13

! -- DRIVER13 tests 3-D inverse power of distance interpolation.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

       integer, parameter :: MAXZONE=10

! -- Variables used in function calls.

       integer(kind=c_int)           :: ifail
       integer(kind=c_int)           :: npts
       integer(kind=c_int)           :: mpts
       integer(kind=c_int)           :: transtype
       character(kind=c_char,len=1)  :: messagestring(1500)

       integer(kind=c_int), allocatable   :: zns(:),znt(:)
       real(kind=c_double), allocatable   :: ecs(:),ncs(:),zcs(:),vals(:)
       real(kind=c_double), allocatable   :: ect(:),nct(:),zct(:),valt(:)
       real(kind=c_double), allocatable   :: bearing(:),dip(:),rake(:)
       real(kind=c_double), allocatable   :: ahmax(:),ahmin(:),avert(:)
       real(kind=c_double), allocatable   :: invpow(:)

! -- Other variables

       integer               :: ierr,ioption
       integer               :: nlay,nrow,ncol,ilay,irow,icol
       integer               :: ipt,iii
       integer               :: ncr,lastzone,itemp,izone,nzone
       integer               :: zonenum(MAXZONE)
       double precision      :: pi
       double precision      :: e0,n0,angle
       double precision      :: cosang,sinang
       double precision      :: nointerpval
       double precision      :: dtemp1,dtemp2
       double precision      :: z_ahmax(MAXZONE),z_ahmin(MAXZONE),z_avert(MAXZONE)
       double precision      :: z_bearing(MAXZONE),z_dip(MAXZONE),z_rake(MAXZONE)
       double precision      :: z_invpower(MAXZONE)
       character (len=1)     :: anl
       character (len=20)    :: anum
       character (len=200)   :: cline
       character (len=256)   :: infile,outfile
       character (len=1500)  :: amessage

! -- Other allocatable variables.

       double precision, allocatable      :: delr(:),delc(:)
       double precision, allocatable      :: x(:),y(:)
       double precision, allocatable      :: bot(:,:,:)
       character (len=20), allocatable    :: apoint(:)

! -- Initialization

       pi=3.14159265358979000

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

! -- Acquire number of layers.

       write(6,*)
130    write(6,140,advance='no')
140    format(' Enter number of model layers: ')
       read(5,*,err=130) nlay
       if(nlay.le.0) go to 130
       ncr=ncol*nrow
       mpts=ncol*nrow*nlay

! -- Calculate layer bottom elevations.

       allocate(bot(ncol,nrow,0:nlay),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ilay=0,nlay
         bot(:,:,ilay)=-ilay*20.0d0
       end do

! -- Acquire zones option

       write(6,*)
131    write(6,141,advance='no')
141    format(' Use option 1 or 2 for zones? ')
       read(5,*,err=131) ioption
       if((ioption.ne.1).and.(ioption.ne.2)) go to 131

! -- Calculate zones (zones are 0,1,2)

       allocate(znt(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(ioption.eq.2)then
         znt=1  ! an array
       else
         do ilay=1,nlay
           do irow=1,nrow
             do icol=1,ncol
               ipt=(ilay-1)*ncr+(irow-1)*ncol+icol
               if(irow.le.15)then
                 znt(ipt)=1
               else
                 znt(ipt)=2
               end if
             end do
           end do
         end do
         do ilay=1,5
           do irow=21,nrow
             do icol=16,ncol
               ipt=(ilay-1)*ncr+(irow-1)*ncol+icol
               znt(ipt)=0
             end do
           end do
         end do
       end if

! -- The pilot points file is read.

       write(6,*)
180    write(6,190,advance='no')
190    format(' Enter name of 3D pilot points file: ')
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
       allocate(apoint(npts),ecs(npts),ncs(npts),zcs(npts),zns(npts),vals(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       rewind(10)
       do ipt=1,npts
         read(10,*,err=9000,end=9050) apoint(ipt),ecs(ipt),ncs(ipt),zcs(ipt),zns(ipt),vals(ipt)
       end do
       close(unit=10)
       write(6,120) trim(infile)

! -- We calculate the number of zones in the target array.

       nzone=0
       lastzone=huge(lastzone)
       do ipt=1,mpts
         itemp=znt(ipt)
         if(itemp.ne.lastzone)then
           if(nzone.eq.0)then
             nzone=1
             zonenum(nzone)=itemp
           else
             do izone=1,nzone
               if(zonenum(izone).eq.itemp) go to 203
             end do
             nzone=nzone+1
             if(nzone.gt.MAXZONE)then
               call writint(anum,MAXZONE)
               write(amessage,2031) trim(anum)
2031           format('DRIVER13 allows a maximum of only ',a,' different zones ', &
               'in model grid.')
               go to 9890
             end if
             zonenum(nzone)=itemp
203          continue
           end if
           lastzone=itemp
         end if
       end do

! -- Enter anisotropy and other variables for different zones.

       do izone=1,nzone
         if(zonenum(izone).eq.0)then
           z_ahmax(izone)=1000.0
           z_ahmin(izone)=500.0
           z_avert(izone)=20.0
           z_bearing(izone)=0.0
           z_dip(izone)=0.0
           z_rake(izone)=0.0
           z_invpower(izone)=0.0
         else
           write(6,*)
           call writint(anum,zonenum(izone))
           write(6,2035) trim(anum)
2035       format(' For zone ',a,' ---->')
2050       write(6,2060,advance='no')
2060       format(' Enter ahmax, ahmin, avert: ')
           read(5,*,err=2050) z_ahmax(izone),z_ahmin(izone),z_avert(izone)
2070       write(6,2080,advance='no')
2080       format(' Enter bearing, dip, rake: ')
           read(5,*,err=2070) z_bearing(izone), z_dip(izone), z_rake(izone)
2090       write(6,2095,advance='no')
2095       format(' Enter inverse power of distance: ')
           read(5,*,err=2090) z_invpower(izone)
         end if
       end do

! -- Provide some control variables.

       write(6,*)
401    write(6,402,advance='no')
402    format(' Interpolate in natural or log domain? [n/l]: ')
       read(5,*,err=401) anl
       if((anl.eq.'N').or.(anl.eq.'n'))then
         transtype=0
       else if((anl.eq.'L').or.(anl.eq.'l'))then
         transtype=1
       else
         go to 401
       end if
405    write(6,406,advance='no')
406    format(' Enter number indicating no interpolation to target array: ')
       read(5,*,err=405) nointerpval

! -- Calculate target coordinates.

       allocate(ect(mpts),nct(mpts),zct(mpts),valt(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       valt=nointerpval        ! an array
       allocate(x(ncol),y(nrow),stat=ierr)
       if(ierr.ne.0) go to 9200

       cosang=cos(angle*pi/180.0d0)
       sinang=sin(angle*pi/180.0d0)
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
           dtemp1=x(icol)*cosang-y(irow)*sinang+e0
           dtemp2=x(icol)*sinang+y(irow)*cosang+n0
           do ilay=1,nlay
             ect(ipt+(ilay-1)*ncr)=dtemp1
             nct(ipt+(ilay-1)*ncr)=dtemp2
           end do
         end do
       end do
       ipt=0
       do ilay=1,nlay
         do irow=1,nrow
           do icol=1,ncol
             ipt=ipt+1
             zct(ipt)=(bot(icol,irow,ilay-1)+bot(icol,irow,ilay))*0.5
           end do
         end do
       end do

! -- Allocate and populate the other target arrays.

       allocate(ahmax(mpts),ahmin(mpts),avert(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(bearing(mpts),dip(mpts),rake(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(invpow(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200

       do izone=1,nzone
         iii=zonenum(izone)
         do ipt=1,mpts
           if(znt(ipt).eq.iii)then
             ahmax(ipt)=z_ahmax(izone)
             ahmin(ipt)=z_ahmin(izone)
             avert(ipt)=z_avert(izone)
             bearing(ipt)=z_bearing(izone)
             dip(ipt)=z_dip(izone)
             rake(ipt)=z_rake(izone)
             invpow(ipt)=z_invpower(izone)
           end if
         end do
       end do

! -- Now call the function to do the interpolation.

       write(6,*)
       write(6,320)
320    format(' Calling ipd_interpolate_3d()....')
       ifail=ipd_interpolate_3d(                 &
                  npts,ecs,ncs,zcs,zns,vals,     &
                  mpts,ect,nct,zct,znt,valt,     &
                  transtype,                     &
                  ahmax,ahmin,avert,             &
                  bearing,dip,rake,              &
                  invpow)
       if(ifail.ne.0)then
         write(6,340)
         write(6,*)
340      format(' Function call unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(6,350)
350      format(' Interpolation successful.')
       end if

! -- We now store the results as a real array.

       write(6,*)
440    write(6,450,advance='no')
450    format(' Enter filename for target array storage: ')
       read(5,*,err=440) outfile
       open(unit=20,file=outfile,action='write',err=440)
       write(20,455)
455    format(1x,'LAYER',t10,'ROW',t20,'COLUMN',t30'VALUE')
       ipt=0
       do ilay=1,nlay
         do irow=1,nrow
           do icol=1,ncol
             ipt=ipt+1
             write(20,460) ilay,irow,icol,valt(ipt)
460          format(1x,i8,t10,i8,t20,i8,t30,1pg16.9)
           end do
         end do
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
       deallocate(ecs,ncs,zcs,vals,stat=ierr)
       deallocate(ect,nct,zct,valt,stat=ierr)
       deallocate(bearing,dip,rake,stat=ierr)
       deallocate(ahmax,ahmin,avert,stat=ierr)
       deallocate(invpow,stat=ierr)
       deallocate(delr,delc,stat=ierr)
       deallocate(x,y,stat=ierr)
       deallocate(bot,stat=ierr)
       deallocate(apoint,stat=ierr)

! -- Free function interface memory

       ifail=free_all_memory()

       end



