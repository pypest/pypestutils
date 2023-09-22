       program DRIVER14

! -- DRIVER14 tests 2-D random field generation.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char,c_double
       implicit none

! -- Variables used in function calls.

       integer(kind=c_int)           :: ifail
       integer(kind=c_int)           :: iseed
       integer(kind=c_int)           :: nnode
       integer(kind=c_int)           :: transtype,avetype
       integer(kind=c_int)           :: ldrand,nreal
       real(kind=c_double)           :: power
       character(kind=c_char,len=1)  :: messagestring(1500)

       integer(kind=c_int), allocatable   :: iarray(:)
       real(kind=c_double), allocatable   :: xx(:),yy(:)
       real(kind=c_double), allocatable   :: mean(:),area(:),var(:),aa(:),anis(:),bearing(:)
       real(kind=c_double), allocatable   :: darray(:,:)

! -- Other variables

        character (len=:), allocatable :: textout
        integer             :: nbb,nb,jfail,ierr,iline,nrecl
        integer             :: ireal
        integer             :: jnode,itemp
        integer             :: lw(15),rw(15)
        character (len=1)   :: anl
        character (len=15)  :: aline
        character (len=20)  :: ama1
        character (len=20)  :: atemp,anum2,anum
        character (len=256) :: afile,gridfile,avefile,outfile
        character (len=700) :: cline
        character (len=1500):: amessage

! -- Open the node specification file.

100     write(6,110,advance='no')
110     format(' Enter name of 2D node specifications file: ')
        read(5,'(a)') afile
        if(afile.eq.' ') go to 100
        afile=adjustl(afile)
        nbb=len_trim(afile)
        call getfile(jfail,afile,gridfile,1,nbb)
        if(jfail.ne.0) go to 100
        afile=gridfile
        open(unit=10,file=gridfile,status='old',iostat=ierr)
        if(ierr.ne.0) then
          write(6,150) trim(afile)
150       format(' Cannot open file ',a,'. Try again.')
          go to 100
        end if

! -- See if the column headers are ok.

        iline=1
        read(10,'(a)',err=9000,end=9000) cline
        if(cline.eq.' ') go to 9050
        call linesplit(jfail,5,lw,rw,cline)
        if(jfail.ne.0) go to 9050
        call lowcase(cline)
        atemp=cline(lw(1):rw(1))
        if(atemp(1:4).ne.'node') go to 9050
        atemp=cline(lw(2):rw(2))
        if(atemp(1:1).ne.'x') go to 9050
        atemp=cline(lw(3):rw(3))
        if(atemp(1:1).ne.'y') go to 9050
        atemp=cline(lw(4):rw(4))
        if(atemp(1:4).ne.'area') go to 9050
        atemp=cline(lw(5):rw(5))
        if(atemp.ne.'active') go to 9050

! -- Establish number of non-blank lines.

        iline=1
        nnode=0
        do
          iline=iline+1
          read(10,'(a)',err=9000,end=180) cline
          if(cline.eq.' ') cycle
          nnode=nnode+1
        end do
180     continue
        if(nnode.eq.0)then
          write(amessage,190) trim(afile)
190       format('File ',a,' appears to be empty.')
          go to 9890
        end if
        rewind(unit=10)

! -- Allocate memory.

        allocate(xx(nnode),yy(nnode),area(nnode),iarray(nnode),stat=ierr)
        if(ierr.ne.0) go to 9200

! -- Now read the file.

        read(10,*)
        iline=1
        jnode=0
        do
          iline=iline+1
          read(10,'(a)',err=9000,end=9000) cline
          if(cline.eq.' ') cycle
          jnode=jnode+1
          call linesplit(jfail,5,lw,rw,cline)
          if(jfail.ne.0) go to 9100
          call intread(jfail,cline(lw(1):rw(1)),itemp)
          if(jfail.ne.0) go to 9000
          if(itemp.ne.jnode)then
            call writint(aline,iline)
            write(amessage,210) trim(aline),trim(afile)
210         format('Node numbers should start at 1 and be sequential. This is ',  &
            'violated at line ',a,' of file ',a,'.')
            go to 9890
          end if
          call drealread(jfail,cline(lw(2):rw(2)),xx(jnode))
          if(jfail.ne.0) go to 9000
          call drealread(jfail,cline(lw(3):rw(3)),yy(jnode))
          if(jfail.ne.0) go to 9000
          call drealread(jfail,cline(lw(4):rw(4)),area(jnode))
          if(jfail.ne.0) go to 9000
          call intread(jfail,cline(lw(5):rw(5)),iarray(jnode))
          if(jfail.ne.0) go to 9000
          if(jnode.eq.nnode) exit
        end do
        close(unit=10)
        call writint(anum,nnode)
        write(6,221) trim(anum),trim(afile)
221     format(' - data for ',a,' nodes read from file ',a,'.')

! -- Allocate more memory.

        allocate(mean(nnode),var(nnode),aa(nnode),anis(nnode),bearing(nnode),stat=ierr)
        if(ierr.ne.0) go to 9200

! -- Read the averaging function specification file.

        write(6,*)
250     write(6,260,advance='no')
260     format(' Enter name of 2D averaging function specification file: ')
        read(5,'(a)') afile
        if(afile.eq.' ') go to 250
        afile=adjustl(afile)
        nbb=len_trim(afile)
        call getfile(jfail,afile,avefile,1,nbb)
        if(jfail.ne.0) go to 250
        afile=avefile
        open(unit=10,file=avefile,status='old',iostat=ierr)
        if(ierr.ne.0) then
          write(6,150) trim(afile)
          go to 250
        end if

! -- See if the column headers are ok.

        iline=1
        read(10,'(a)',err=9000,end=9000) cline
        if(cline.eq.' ') go to 9070
        call linesplit(jfail,6,lw,rw,cline)
        if(jfail.ne.0) go to 9070
        call lowcase(cline)
        atemp=cline(lw(1):rw(1))
        if(atemp(1:4).ne.'node') go to 9070
        atemp=cline(lw(2):rw(2))
        if(atemp.ne.'mean') go to 9070
        atemp=cline(lw(3):rw(3))
        if((atemp(1:4).ne.'sill').and.(atemp(1:3).ne.'var')) go to 9070
        atemp=cline(lw(4):rw(4))
        if((atemp.ne.'a').and.(atemp.ne.'aa')) go to 9070
        atemp=cline(lw(5):rw(5))
        if(atemp(1:4).ne.'anis') go to 9070
        atemp=cline(lw(6):rw(6))
        if(atemp.ne.'bearing') go to 9070

! -- Now read the file.

        jnode=0
        do
          iline=iline+1
          read(10,'(a)',err=9000,end=9000) cline
          if(cline.eq.' ') cycle
          jnode=jnode+1
          call linesplit(jfail,6,lw,rw,cline)
          if(jfail.ne.0) go to 9100
          call intread(jfail,cline(lw(1):rw(1)),itemp)
          if(jfail.ne.0) go to 9000
          if(itemp.ne.jnode)then
            call writint(aline,iline)
            write(amessage,210) trim(aline),trim(afile)
            go to 9890
          end if
          call drealread(jfail,cline(lw(2):rw(2)),mean(jnode))
          if(jfail.ne.0) go to 9000
          call drealread(jfail,cline(lw(3):rw(3)),var(jnode))
          if(jfail.ne.0) go to 9000
          call drealread(jfail,cline(lw(4):rw(4)),aa(jnode))
          if(jfail.ne.0) go to 9000
          call drealread(jfail,cline(lw(5):rw(5)),anis(jnode))
          if(jfail.ne.0) go to 9000
          call drealread(jfail,cline(lw(6):rw(6)),bearing(jnode))
          if(jfail.ne.0) go to 9000
          if(jnode.eq.nnode) exit
        end do
        close(unit=10)
        write(6,220) trim(afile)
220     format(' - file ',a,' read ok.')

! -- Acquire data pertaing to averaging function.

        write(6,*)
351     write(6,361,advance='no')
361     format(' Express stochasticity in natural or log domain? [n/l]: ')
        read(5,'(a)') anl
        if(anl.eq.' ') go to 351
        call lowcase(anl)
        if((anl.ne.'n').and.(anl.ne.'l'))go to 351
        if(anl.eq.'n')then
          transtype=0
        else
          transtype=1
        end if

        write(6,*)
370     write(6,371,advance='no')
371     format(' Is moving ave spherical, exponential, gaussian or power? [s/x/g/p(..)]: ')
        read(5,'(a)') ama1
        if(ama1.eq.' ') go to 370
        call lowcase(ama1)
        ama1=adjustl(ama1)
        if(ama1.eq.'s')then
          avetype=1
        else if(ama1.eq.'x')then
          avetype=2
        else if(ama1.eq.'g')then
          avetype=3
        else if(ama1(1:1).eq.'p')then
          avetype=4
          ama1=adjustl(ama1(2:))
          if(ama1(1:1).ne.'(') go to 370
          ama1=adjustl(ama1(2:))
          nb=len_trim(ama1)
          if(nb.eq.0) go to 370
          if(ama1(nb:nb).ne.')') go to 370
          ama1(nb:nb)=' '
          if(ama1.eq.' ') go to 370
          call drealread(jfail,ama1,power)
          if(jfail.ne.0) go to 370
        else
          go to 370
        end if

        write(6,*)
381     write(6,382,advance='no')
382     format(' How many realizations do you wish to generate? ')
        read(5,*,err=381) nreal
        if(nreal.le.0) go to 381
        write(6,*)
400     write(6,410,advance='no')
410     format(' Enter name for csv output file: ')
        read(5,'(a)') afile
        if(afile.eq.' ') go to 400
        nbb=len_trim(afile)
        call getfile(jfail,afile,outfile,1,nbb)
        if(jfail.ne.0) go to 400
        outfile=adjustl(outfile)
        afile=outfile
        nrecl=nreal*20+20
        open(unit=20,file=outfile,action='write',recl=nrecl,iostat=ierr)
        if(ierr.ne.0)then
          write(6,420) trim(afile)
420       format(' Cannot write to file ',a,'. Try again.')
          go to 400
        end if

! -- The random seed is acquired.

        write(6,*)
1309    write(6,1315,advance='no')
1315    format(' Enter integer seed for random number generator [324853]: ')
        read(5,'(a)') anum2
        if(anum2.eq.' ') then
          iseed=324853
        else
          call intread(jfail,anum2,iseed)
          if(jfail.ne.0)go to 1309
        end if

! -- Initialize the random number generator.

        write(6,*)
        write(6,320)
320     format(' Calling initialze_randgen()....')
        ifail=initialize_randgen(iseed)
        if(ifail.ne.0)then
          write(6,340)
          write(6,*)
340       format(' Function call unsuccessful. Error message follows.')
          ifail=retrieve_error_message(messagestring)
          call string2char(1500,messagestring,amessage)
          go to 9890
        else
          write(6,350)
350       format(' Function call successful.')
        end if

! -- Allocate the array that holds random fields.

        ldrand=nnode
        allocate(darray(ldrand,nreal),stat=ierr)
        if(ierr.ne.0) go to 9200
        darray=0.0d0      ! an array

! -- Call the function to fill the array.

        write(6,*)
        write(6,421)
421     format(' Calling fieldgen2d_sva()....')
        ifail=fieldgen2d_sva(                &
              nnode,                         &
              xx,yy,area,iarray,             &
              mean,var,aa,anis,bearing,      &
              transtype,avetype,power,       &
              ldrand,nreal,darray)
        if(ifail.ne.0)then
          write(6,340)
          write(6,*)
          ifail=retrieve_error_message(messagestring)
          call string2char(1500,messagestring,amessage)
          go to 9890
        else
          write(6,350)
        end if

! -- Write file header.

        write(6,*)
        write(6,430)
430     format(' Writing CSV output file...')
        textout='node'
        do ireal=1,nreal
          call writint(anum,ireal)
          textout=trim(textout)//','//'sim'//trim(anum)
        end do
        write(20,'(a)') trim(textout)

! -- Write the CSV output file.

        do jnode=1,nnode
          call writint(anum,jnode)
          textout=trim(anum)
          do ireal=1,nreal
            write(anum2,'(1pg16.9)')darray(jnode,ireal)
            textout=trim(textout)//','//trim(anum2)
          end do
          write(20,'(a)',err=9400) trim(textout)
        end do
        close(unit=20)
        write(6,1500) trim(afile)
1500    format(' File ',a,' written ok.')

        go to 9900

9000    continue
        call writint(aline,iline)
        write(amessage,9010) trim(aline),trim(afile)
9010    format('Error encountered in reading line ',a,' of file ',a,'.')
        go to 9890

9050    write(amessage,9060) trim(afile)
9060    format('Header on first line of file ',a,' expected to be ',   &
        '"node, x, y, area, active".')
        go to 9890

9070    write(amessage,9080) trim(afile)
9080    format('Header on first line of file ',a,' expected to be ',   &
        '"node, mean, var (or sill), aa, anis, bearing".')
        go to 9890

9100    call writint(aline,iline)
        write(amessage,9110) trim(aline),trim(afile)
9110    format('Insufficient items on line ',a,' of file ',a,'.')
        go to 9890

9200   write(amessage,9210)
9210   format('Insufficient memory to continue execution.')
       go to 9890

9400    write(amessage,9410) trim(afile)
9410    format('Cannot write to file ',a,'.')
        go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

! -- Tidy up

9900   continue

! -- Free local memory

       deallocate(iarray,stat=ierr)
       deallocate(xx,yy,stat=ierr)
       deallocate(mean,area,var,aa,anis,bearing,stat=ierr)
       deallocate(darray,stat=ierr)

! -- Free function interface memory

       ifail=free_all_memory()

       end



