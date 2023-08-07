       program driver8

! -- DRIVER8 tests 3-D kriging interpolation

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

       integer, parameter :: MAXZONE=10

! -- Variables used in function calls.

       integer(kind=c_int)           :: ifail
       integer(kind=c_int)           :: npts
       integer(kind=c_int)           :: mpts
       integer(kind=c_int)           :: vartype(MAXZONE)
       integer(kind=c_int)           :: krigtype
       integer(kind=c_int)           :: transtype
       integer(kind=c_int)           :: nzone
       integer(kind=c_int)           :: zonenum(MAXZONE)
       integer(kind=c_int)           :: maxpts,minpts
       integer(kind=c_int)           :: icount_interp,icount
       real(kind=c_double)           :: bearing(MAXZONE),dip(MAXZONE),rake(MAXZONE)
       real(kind=c_double)           :: ahmax(MAXZONE),ahmin(MAXZONE),avert(MAXZONE)
       real(kind=c_double)           :: srhmax,srhmin,srvert
       character (kind=c_char,len=1) :: factorfile(256)
       integer(kind=c_int)           :: factorfiletype
       character(kind=c_char,len=1)  :: messagestring(1500)

! -- Other variables

       integer               :: ierr,ifile,ioption
       integer               :: nlay,nrow,ncol,ilay,irow,icol
       integer               :: ipt,ibeg,iend,iptstart
       integer               :: ncr,lastzone,itemp,izone
       double precision      :: pi
       double precision      :: e0,n0,angle
       double precision      :: cosang,sinang
       double precision      :: nointerpval
       double precision      :: dtemp1,dtemp2
       character (len=1)     :: aaa,al
       character (len=20)    :: anum,alay
       character (len=200)   :: cline
       character (len=256)   :: infile,facfile,afile,outfile,inbase
       character (len=1500)  :: amessage

! -- Allocatable variables; first the ones that are used in function calls.

       integer(kind=c_int), allocatable   :: zns(:),znt(:)
       real(kind=c_double), allocatable   :: ecs(:),ncs(:),zcs(:),vals(:)
       real(kind=c_double), allocatable   :: ect(:),nct(:),zct(:),valt(:)
       real(kind=c_double), allocatable   :: mean(:)

! -- Other allocatable variables.

       double precision, allocatable      :: delr(:),delc(:)
       double precision, allocatable      :: x(:),y(:)
       double precision, allocatable      :: bot(:,:,:)
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
2031           format('DRIVER8 allows a maximum of only ',a,' different zones ', &
               'in model grid.')
               go to 9890
             end if
             zonenum(nzone)=itemp
203          continue
           end if
           lastzone=itemp
         end if
       end do

! -- Enter kriging type

       write(6,*)
2090   write(6,2100,advance='no')
2100   format(' Enter kriging type (0=simple; 1=ordinary): ')
       read(5,*,err=2090) krigtype

! -- Enter variogram variables for different zones.

       allocate(mean(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
       mean=0   ! an array
       do izone=1,nzone
         if(zonenum(izone).eq.0)then
           vartype(izone)=2
           ahmax(izone)=1000.0
           ahmin(izone)=500.0
           avert(izone)=20.0
           bearing(izone)=0.0
           dip(izone)=0.0
           rake(izone)=0.0
         else
           write(6,*)
           call writint(anum,zonenum(izone))
           write(6,2035) trim(anum)
2035       format(' For zone ',a,' ---->')
205        write(6,206,advance='no')
206        format(' Enter variogram type (1=spher;2=exp;3=gaus;4=pow): ')
           read(5,*,err=205) vartype(izone)
           if((vartype(izone).lt.1).or.(vartype(izone).gt.4)) go to 205
2050       write(6,2060,advance='no')
2060       format(' Enter ahmax, ahmin, avert: ')
           read(5,*,err=2050) ahmax(izone),ahmin(izone),avert(izone)
2070       write(6,2080,advance='no')
2080       format(' Enter bearing, dip, rake: ')
           read(5,*,err=2070) bearing(izone), dip(izone), rake(izone)
2110       write(6,2120,advance='no')
2120       format(' Enter mean value: ')
           read(5,*,err=2110) dtemp1
           itemp=zonenum(izone)
           do ipt=1,mpts
             if(znt(ipt).eq.itemp) mean(ipt)=dtemp1
           end do
         end if
       end do

! -- Enter control variables.

       write(6,*)
210    write(6,220,advance='no')
220    format(' Enter srhmax, srhmin, srvert: ')
       read(5,*,err=210) srhmax,srhmin,srvert
       if((srhmax.le.0.0).or.(srhmin.le.0.0).or.(srvert.le.0.0)) go to 210
230    write(6,240,advance='no')
240    format(' Enter maxpts, minpts: ')
       read(5,*,err=230) maxpts,minpts

! -- The name of the factor file is aquired.

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

! -- Calculate target coordinates.

       allocate(ect(mpts),nct(mpts),zct(mpts),valt(mpts),stat=ierr)
       if(ierr.ne.0) go to 9200
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

! -- Now call the function to do the interpolation.

       write(6,*)
       write(6,320)
320    format(' Calling calc_kriging_factors_3d()....')
       ifail=calc_kriging_factors_3d(            &
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

! -- We will now re-read the factor file and undertake interpolation. The outcomes
!    will be written as a MODFLOW-compatible tabular data file (for use of MF2VTK1).

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

9100   write(amessage,9110) trim(infile)
9110   format('Cannot open file ',a,'.')
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
       deallocate(delr,delc,stat=ierr)
       deallocate(bot,stat=ierr)
       deallocate(x,y,stat=ierr)
       deallocate(apoint,stat=ierr)
       deallocate(mean,stat=ierr)

! -- Free function interface memory

       ifail=free_all_memory()

       end



