       program driver2

! -- This driver tests functions that install structured grid specs and
!    perform spatial interpolation from an installed grid to arbitrary points.

       implicit none

       integer              :: install_structured_grid,        &
                               retrieve_error_message,         &
                               uninstall_structured_grid,      &
                               free_all_memory,                &
                               interp_from_structured_grid

       integer              :: ibeg,iend,ierr
       integer              :: nrow,ncol,nlay,irow,icol,ncolold,nrowold
       integer              :: icorner,ifail
       integer              :: npts,ipts
       integer              :: ntime,iprec,itime,isim,proctime
       double precision     :: e0,n0,rotation
       double precision     :: interpthresh,nointerpval
       double precision, allocatable :: delr(:),delc(:)
       character (len=1)    :: aa
       character (len=1)    :: texttype(17)
       character (len=1)    :: gridname(200)
       character (len=1)    :: depvarfile(256)
       character (len=1)    :: messagestring(1500)
       character (len=17)   :: atext
       character (len=200)  :: chargridname
       character (len=256)  :: afile,infile,outfile
       character (len=500)  :: cline
       character (len=1500) :: amessage

       integer, allocatable            :: layer(:)
       double precision, allocatable   :: ee(:),nn(:)
       double precision, allocatable   :: simtime(:),simstate(:,:)
       character (len=20), allocatable :: apoint(:)

! -- Initialization

       nrowold=0
       ncolold=0

! -- Obtain grid specifications from a grid specification file.

       write(6,*)
       write(6,50)
50     format(' Testing grid installation....')

100    write(6,*)
       write(6,110,advance='no')
110    format(' Enter name of a GW Utils grid spec file (<Enter> if no more): ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 300
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 100
       open(unit=10,file=infile,status='old',err=100)
       read(10,*,err=9000,end=9000) nrow,ncol
       read(10,*,err=9000,end=9000) e0,n0,rotation
       if(nrowold.eq.0)then
         allocate(delr(ncol),delc(nrow),stat=ierr)
         if(ierr.ne.0) go to 9200
       else
         if(ncol.gt.ncolold)then
           deallocate(delr,stat=ierr)
           allocate(delr(ncol),stat=ierr)
           if(ierr.ne.0) go to 9200
         end if
         if(nrow.gt.nrowold)then
           deallocate(delc,stat=ierr)
           allocate(delc(nrow),stat=ierr)
           if(ierr.ne.0) go to 9200
         end if
       end if
       ncolold=ncol
       nrowold=nrow
       read(10,*,err=9000,end=9000) (delr(icol),icol=1,ncol)
       read(10,*,err=9000,end=9000) (delc(irow),irow=1,nrow)
       close(unit=10)
       write(6,130) trim(infile)
130    format(' - file ',a,' read ok.')

! -- Obtain a name for this structured grid from the user.

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
         write(6,*)
       else
         write(6,171)
171      format(' Installation successful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         amessage=' '//trim(amessage)
         call writmess(6,amessage)
       end if
       go to 100

300    continue

! -- Test de-installation.

       write(6,*)
       write(6,*)
       write(6,350)
350    format(' Testing grid deinstallation....')

400    write(6,*)
420    write(6,430,advance='no')
430    format(' Enter name of grid to uninstall (<Enter> if no more): ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 500
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,chargridname,ibeg,iend)
       if(ifail.ne.0) go to 420
       call char2string(200,chargridname,gridname)
       ifail=uninstall_structured_grid(gridname)
       if(ifail.eq.0)then
         write(6,470)
470      format(' Uninstallation successful.')
         write(6,*)
       else
         write(6,471)
471      format(' Uninstallation unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         amessage=' '//trim(amessage)
         call writmess(6,amessage)
       end if
       go to 400

500    continue
       write(6,*)
509    write(6,510,advance='no')
510    format(' Install more grids? [y/n]: ')
       read(5,'(a)') aa
       if((aa.eq.'y').or.(aa.eq.'Y'))then
         go to 100
       else if((aa.eq.'n').or.(aa.eq.'N'))then
         continue
       else
         go to 509
       end if

! -- We will now read a file with point coordinates and layers.

       write(6,*)
520    write(6,530,advance='no')
530    format(' Enter name of point coordinates file (<Enter> if none): ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 9900
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 520
       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,540)
540      format('Cannot open file. Try again.')
         go to 520
       end if
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
       end do
       close(unit=10)
       write(6,570) trim(infile)
570    format(' - file ',a,' read ok.')

! -- Find out the MODFLOW output file to read.

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
620    write(6,630,advance='no')
630    format(' Enter name for output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 620
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile,ibeg,iend)
       if(ifail.ne.0) go to 620
       open(unit=20,file=outfile,action='write',err=620)

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
       if(ifail.eq.0)then
         write(6,670)
670      format(' Spatial interpolation successful.')
         write(6,680) proctime
680      format(' Number of output times =',i5)
         do ipts=1,npts
           do itime=1,ntime
             write(20,690) trim(apoint(ipts)),simtime(itime),simstate(itime,ipts)
690          format(1x,a,t25,1pg16.9,t45,1pg16.9)
           end do
         end do
         close(unit=20)
         write(6,700) trim(outfile)
700      format(' - file ',a,' written ok.')
       else
         write(6,710)
710      format(' Function call unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         amessage=' '//trim(amessage)
         call writmess(6,amessage)
       end if

! -- Repeat the cycle

       deallocate(simtime,simstate,stat=ierr)
       deallocate(apoint,ee,nn,layer,stat=ierr)
       write(6,*)
       go to 520

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
       deallocate(delr,delc,stat=ierr)
       deallocate(layer,ee,nn,stat=ierr)
       deallocate(simtime,simstate,stat=ierr)
       deallocate(apoint,stat=ierr)

       end


