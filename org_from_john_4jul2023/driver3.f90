       program driver3

! -- This driver tests temporal interpolation following spatial interpolation.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

       integer(kind=c_int)             :: nrow,ncol,nlay
       integer(kind=c_int)             :: icorner,ifail,npts
       integer(kind=c_int)             :: ntime,iprec,isim,proctime
       integer(kind=c_int)             :: numobsdat
       real(kind=c_double)             :: e0,n0,rotation
       real(kind=c_double)             :: interpthresh,nointerpval,time_extrap
       character(kind=c_char,len=1)    :: how_extrap
       character(kind=c_char,len=1)    :: texttype(17)
       character(kind=c_char,len=1)    :: gridname(200)
       character(kind=c_char,len=1)    :: depvarfile(256)
       character(kind=c_char,len=1)    :: messagestring(1500)

       integer(kind=c_int),allocatable :: layer(:)
       integer(kind=c_int),allocatable :: obspointnum(:)
       real(kind=c_double),allocatable :: delr(:),delc(:)
       real(kind=c_double),allocatable :: ee(:),nn(:)
       real(kind=c_double),allocatable :: simtime(:),simstate(:,:)
       real(kind=c_double),allocatable :: obstime(:),obsval(:)

       integer              :: ibeg,iend,ierr
       integer              :: irow,icol
       integer              :: ipts,itime
       integer              :: idat
       character (len=1)    :: aa
       character (len=17)   :: atext
       character (len=20)   :: atemp20
       character (len=200)  :: chargridname
       character (len=256)  :: afile,infile,outfile1,outfile2
       character (len=500)  :: cline
       character (len=1500) :: amessage

       character (len=20), allocatable :: apoint(:),obspoint(:)

! -- Obtain grid specifications from a grid specification file.

       write(6,*)
       write(6,50)
50     format(' Testing grid installation....')

100    write(6,*)
109    write(6,110,advance='no')
110    format(' Enter name of a GW Utils grid spec file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 100
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 100
       open(unit=10,file=infile,status='old',err=100)
       read(10,*,err=9000,end=9000) nrow,ncol
       read(10,*,err=9000,end=9000) e0,n0,rotation
       allocate(delr(ncol),delc(nrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       read(10,*,err=9000,end=9000) (delr(icol),icol=1,ncol)
       read(10,*,err=9000,end=9000) (delc(irow),irow=1,nrow)
       close(unit=10)
       write(6,130) trim(infile)
130    format(' - file ',a,' read ok.')

! -- Obtain other details for this structured grid from the user.

140    write(6,145,advance='no')
145    format(' How many layers in this grid? ')
       read(5,*,err=140) nlay
       if(nlay.le.0) go to 140
150    write(6,160,advance='no')
160    format(' Supply a name for this grid: ')
       read(5,'(a)') afile
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,chargridname,ibeg,iend)
       if(ifail.ne.0) go to 150
       call char2string(200,chargridname,gridname)

! -- Install the grid.

       write(6,165)
165    format(' Installing structured grid specs...')
       icorner=1
       ifail=install_structured_grid(gridname,ncol,nrow,nlay,icorner,e0,n0,rotation,delr,delc)
       if(ifail.eq.0)then
         write(6,170)
170      format(' Installation successful.')
       else
         write(6,171)
171      format(' Installation unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       end if

! -- We will now read a file with point coordinates, layers and simulation times.
!    These are the points to which spatial interpolation from the grid will take place.

       write(6,*)
520    write(6,530,advance='no')
530    format(' Enter name of point data file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 520
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 520
       open(unit=10,file=infile,status='old',err=520)
       npts=0
       do
         read(10,'(a)',end=550) cline
         if(cline.ne.' ') npts=npts+1
       end do
550    if(npts.eq.0)then
         write(6,560)
560      format('No points in file. Try again.')
         close(unit=10)
         go to 520
       else
         rewind(unit=10)
       end if
       allocate(apoint(npts),ee(npts),nn(npts),layer(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ipts=1,npts
         read(10,*,err=9050,end=9050) apoint(ipts),ee(ipts),nn(ipts),layer(ipts)
         call lowcase(apoint(ipts))
       end do
       close(unit=10)
       write(6,570) trim(infile)
570    format(' - file ',a,' read ok.')

! -- Ascertain the name of the MODFLOW output file to read.

       write(6,*)
580    write(6,590,advance='no')
590    format(' Enter name of MODFLOW binary dep. var. output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 580
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 580
595    write(6,596,advance='no')
596    format(' Enter grid name to which it pertains: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 595
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,chargridname,ibeg,iend)
       if(chargridname.eq.' ') go to 595
613    write(6,615,advance='no')
615    format(' Enter number of output times in this file: ')
       read(5,*,err=613) ntime
       if(ntime.le.0) go to 613
600    write(6,610,advance='no')
610    format(' Enter precision (1=single; 2=double): ')
       read(5,*,err=600) iprec
       if((iprec.ne.1).and.(iprec.ne.2)) go to 600
611    write(6,612,advance='no')
612    format(' Enter HDRY/HNOFLO threshold: ')
       read(5,*,err=611) interpthresh
       if(interpthresh.le.0) go to 611
617    write(6,618,advance='no')
618    format(' Enter header text of interest: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 617
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,atext,ibeg,iend)
       if(ifail.ne.0) go to 617

! -- Allocate some memory

       allocate(simtime(ntime),simstate(ntime,npts),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- Convert pertinent character variables to strings.

       call char2string(200,infile,depvarfile)
       call char2string(200,chargridname,gridname)
       call char2string(17,atext,texttype)
       isim=1
       nointerpval=1.1d30

! -- Call the function.

       write(6,650)
650    format(' Calling interp_from_structured_grid()....')
       ifail=interp_from_structured_grid(                                     &
                             GridName,DepVarFile,isim,iprec,ntime,            &
                             TextType,InterpThresh,NoInterpVal,               &
                             npts,ee,nn,layer,                                &
                             proctime,simtime,simstate)
       if(ifail.ne.0)then
         write(6,710)
710      format(' Function call unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       end if
       write(6,670)
670    format(' Spatial interpolation successful.')
       write(6,680) proctime
680    format(' Number of output times =',i5)

       write(6,*)
620    write(6,630,advance='no')
630    format(' Enter name for post-spatial-interpolation output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 620
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile1,ibeg,iend)
       if(ifail.ne.0) go to 620
       open(unit=20,file=outfile1,action='write',err=620)
       do ipts=1,npts
         do itime=1,ntime
           write(20,681) trim(apoint(ipts)),simtime(itime),simstate(itime,ipts)
681        format(1x,a,t25,1pg16.9,t45,1pg16.9)
         end do
       end do
       close(unit=20)
       write(6,682) trim(outfile1)
682    format(' - file ',a,' written ok.')

! -- Read observation time file.

       write(6,*)
690    write(6,720,advance='no')
720    format(' Enter name of observation time file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 690
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 690
       open(unit=10,file=infile,status='old',err=690)
       numobsdat=0
       do
         read(10,'(a)',end=730) cline
         if(cline.ne.' ') numobsdat=numobsdat+1
       end do
730    continue
       if(numobsdat.eq.0)then
         write(6,725)
725      format('No observation data in file. Try again.')
         close(unit=10)
         go to 690
       else
         rewind(unit=10)
       end if
       allocate(obspoint(numobsdat),obstime(numobsdat),obsval(numobsdat),    &
       obspointnum(numobsdat),stat=ierr)
       if(ierr.ne.0) go to 9200
       do idat=1,numobsdat
         read(10,*,err=9050,end=9050) obspoint(idat),obstime(idat)
         call lowcase(obspoint(idat))
       end do
       close(unit=10)
       write(6,570) trim(infile)

! -- Fill the OBSPOINTNUM array.

       do idat=1,numobsdat
         do ipts=1,npts
           if(obspoint(idat).eq.apoint(ipts)) go to 735
         end do
         obspointnum(idat)=-1
         cycle
735      obspointnum(idat)=ipts-1       ! Remember that external programs indices start at 0
       end do

! -- Gather a few more details on how to time-interpolate.

750    write(6,760,advance='no')
760    format(' Enter time-extrapolation option (L/C): ')
       read(5,*) how_extrap
       if((how_extrap.ne.'L').and.(how_extrap.ne.'l').and.   &
          (how_extrap.ne.'C').and.(how_extrap.ne.'c')) go to 750
769    write(6,770,advance='no')
770    format(' Enter extrapolation time limit: ')
       read(5,*) time_extrap
       if(time_extrap.lt.0.0d0) go to 769

! -- Undertaking temporal interpolation.

       write(6,775)
775    format(' Calling interp_to_obstime()....')
       ifail=interp_to_obstime(                                   &
             ntime,proctime,npts,simtime,simstate,interpthresh,   &
             how_extrap,time_extrap,nointerpval,                  &
             numobsdat,obspointnum,obstime,obsval)
       if(ifail.ne.0)then
         write(6,810)
810      format(' Function call unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       else
         write(6,820)
820      format(' Spatial interpolation successful.')
       end if

! -- Write the output file.

940    write(6,950,advance='no')
950    format(' Enter name for post-time-interpolation output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 940
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile2,ibeg,iend)
       if(ifail.ne.0) go to 940
       open(unit=21,file=outfile2,action='write',err=940)
       do idat=1,numobsdat
         write(21,960) trim(obspoint(idat)),obstime(idat),obsval(idat)
960      format(1x,a,t25,1pg16.9,t50,1pg16.9)
       end do
       close(unit=21)
       write(6,970) trim(outfile2)
970    format(' - file ',a,' written ok.')

       go to 9900

9000   write(amessage,9010) trim(infile)
9010   format('Error reading grid specification file ',a,'.')
       go to 9890

9050   write(amessage,9060) trim(infile)
9060   format('Error encountered in reading file ',a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format('Driver program suffers memory allocation error.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

! -- Tidy up

9900   continue
       write(6,*)
       write(6,9910)
9910   format(' Tidying up...')

       ifail=free_all_memory()
       if(ifail.eq.0)then
         write(6,9920)
9920     format(' Tidying up successful.')
         write(6,*)
       else
         write(6,9921)
9921     format(' Tidying up unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         amessage=' '//trim(amessage)
         call writmess(6,amessage)
       end if
       deallocate(layer,obspointnum,stat=ierr)
       deallocate(delr,delc,stat=ierr)
       deallocate(ee,nn,stat=ierr)
       deallocate(simtime,simstate,stat=ierr)
       deallocate(obstime,obsval,stat=ierr)
       deallocate(apoint,obspoint,stat=ierr)

       end


