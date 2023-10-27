       program driver17

! -- This driver records the coordinates of cell centres of one layer of a structured grid.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

       integer(kind=c_int)             :: nrow,ncol,nlay,ncpl
       integer(kind=c_int)             :: icorner,ifail
       real(kind=c_double)             :: e0,n0,rotation
       character(kind=c_char,len=1)    :: gridname(200)
       character(kind=c_char,len=1)    :: messagestring(1500)

       real(kind=c_double),allocatable :: delr(:),delc(:)
       real(kind=c_double),allocatable :: cellx(:),celly(:)

       integer              :: ibeg,iend,ierr
       integer              :: irow,icol,icell
       character (len=200)  :: chargridname
       character (len=256)  :: afile,infile,outfile
       character (len=1500) :: amessage

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

! -- Other details of the grid are supplied automatically.

       nlay=5
       chargridname='grid1'
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

! -- Request coordinates of grid cell centres.

       write(6,*)
180    write(6,190,advance='no')
190    format(' Enter name of file to store cell centre coordinates: ')
       read(5,*) outfile
       open(unit=20,file=outfile,action='write',err=180)
       write(20,200)
200    format(' ICOL',t15,'IROW',t30,'CELLX',t50,'CELLY')
       write(6,*)
       write(6,220)
220    format(' Retrieving cell centre coordinates...')
       ncpl=ncol*nrow
       allocate(cellx(ncpl),celly(ncpl),stat=ierr)
       if(ierr.ne.0) go to 9200
       ifail=get_cell_centres_structured(gridname,ncpl,cellx,celly)
       if(ifail.eq.0)then
         write(6,230)
230      format(' Retrieval successful.')
       else
         write(6,240)
240      format(' Retrieval unsuccessful. Error message follows.')
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         go to 9890
       end if
       icell=0
       do irow=1,nrow
         do icol=1,ncol
           icell=icell+1
           write(20,245) icol,irow,cellx(icell),celly(icell)
245        format(2x,i5,t15,i5,t30,f16.4,t50,f16.4)
         end do
       end do
       close(unit=20)
       write(6,250) trim(outfile)
250    format(' - file ',a,' written ok.')

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
       deallocate(delr,delc,stat=ierr)
       deallocate(cellx,celly,stat=ierr)

       end


