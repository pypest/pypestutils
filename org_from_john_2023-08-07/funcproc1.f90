integer (kind=c_int) function inquire_modflow_binary_file_specs(FileIn,FileOut,isim,                          &
                                                   itype,iprec,narray,ntime)                     &
                                                   bind(C,name="inquire_modflow_binary_file_specs_")

! -- This function reports some of the details of a MODFLOW-written binary file. This file may be single or double
!    precision. It may be a system state or budget file.

       use iso_c_binding, only: c_int,c_char
       use dimvar
       use utilities

       implicit none

       character (kind=c_char), intent(in)        :: FileIn(LENFILENAME)
       character (kind=c_char), intent(in)        :: FileOut(LENFILENAME)
       integer(kind=c_int), intent(in)            :: isim             ! Simulator
       integer(kind=c_int), intent(in)            :: itype            ! 1 = system state; 2 = flows
       integer(kind=c_int), intent(out)           :: iprec            ! 1 = single; 2 = double
       integer(kind=c_int), intent(out)           :: narray
       integer(kind=c_int), intent(out)           :: ntime

       logical                        :: lout,lopened
       integer                        :: NDimFileIn,NDimFileOut
       integer                        :: ibig,itemp
       integer                        :: kstp,kper,kstpold,kperold,ilay
       integer                        :: iturn,ns,nss,is,i,j,itemp1,itemp2
       integer                        :: inunit,ierr,outunit,imeth,numread,nlist,ndat
       integer                        :: ndim1,ndim2,ndim3

       real                           :: rbig,rtemp
       real                           :: pertim,totim,delt
       double precision               :: dbig,dtemp
       double precision               :: dpertim,dtotim,ddelt

       character (len=16)             :: text,atemp16,txt1id1,txt2id1,txt1id2,txt2id2
       character (len=LENFILENAME)    :: infile,outfile

! -- Initialisation

       function_name='inquire_modflow_binary_file_specs()'
       inquire_modflow_binary_file_specs=0
       ibig=huge(ibig)/2                   ! arbitrary
       rbig=huge(rbig)*0.5                 ! arbitrary
       dbig=huge(dbig)*0.5d0               ! arbitrary
       iprec=0
       iturn=1
       inunit=0
       outunit=0
       amessage=' '
!       NDimFileIn=size(FileIn)               ! Inherited from declaration
!       NDimFileOut=size(FileOut)             ! Inherited from declaration
       NDimFileIn=LENFILENAME         ! I assume that FORTRAN cannot determine size of C-passed array.
       NDimFileOut=LENFILENAME        ! We rely on termination character to know end of string.

! -- Input arguments are checked.

       if((itype.ne.1).and.(itype.ne.2))then
         write(amessage,15) trim(function_name)
15       format(' The ITYPE argument of function ',a,' must be supplied as ',  &
         'either 1 or 2.')
         go to 9890
       end if

! -- Text strings are converted to character variables.

       call utl_string2char(NdimFileIn,FileIn,infile)
       infile=adjustl(infile)
       call utl_addquote(infile,afile1)

       call utl_string2char(NdimFileOut,FileOut,outfile)
       outfile=adjustl(outfile)
       if(outfile.eq.' ')then
         lout=.false.
       else
         lout=.true.
         call utl_addquote(outfile,afile2)
       end if

       if((isim.ne.0).and.(isim.ne.1).and.(isim.ne.21).and.(isim.ne.22).and.   &
          (isim.ne.31).and.(isim.ne.32).and.(isim.ne.33))then
          write(amessage,5)
5         format('The isim argument must be 0, 1, 21, 22, 31, 32 or 33.')
         go to 9890
       end if

! -- The MODFLOW output file is opened.

       inunit=utl_nextunit()
       open(unit=inunit,file=infile,status='old',form='unformatted',access='stream',iostat=ierr)
       if(ierr.ne.0) then
         write(amessage,10) trim(afile1)
10       format('Cannot open binary file ',a,'.')
         go to 9890
       end if

! -- An attempt is made to gather information by reading the first line of the file.

       if(utl_mfbinopspecs(inunit,itype,infile,iprec).ne.0) go to 9890
       rewind(unit=inunit)

! -- Read and report contents of system state file.

       if(itype.eq.2) go to 230
25     continue
       kstpold=huge(kstp)
       kperold=huge(kper)
       ntime=0
       narray=0
       if(lout)then
         outunit=utl_nextunit()
         open(unit=outunit,file=outfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,30) trim(afile2)
30         format('Cannot open file ',a,' for output.')
           go to 9890
         end if
         if((isim.eq.1).or.(isim.eq.21).or.(isim.eq.31))then
           write(outunit,40,err=9100)
40         format(t2,'    KSTP',t12,'    KPER',t22,'  PERTIM',  &
           t40,'  TOTIM',t58,'            TEXT',t78,'    NCOL',t88,'    NROW',t98,'    ILAY')
         else if(isim.eq.22)then
           write(outunit,41,err=9100)
41         format(t2,'    KSTP',t12,'    KPER',t22,'  PERTIM',  &
           t40,'  TOTIM',t58,'            TEXT',t78,'   NSTRT',t88,'    NVAL',t98,'    ILAY')
         else if(isim.eq.32)then
           write(outunit,42,err=9100)
42         format(t2,'    KSTP',t12,'    KPER',t22,'  PERTIM',  &
           t40,'  TOTIM',t58,'            TEXT',t78,'    NCPL',t88,'    INT1',t98,'    ILAY')
         else if(isim.eq.33)then
           write(outunit,43,err=9100)
43         format(t2,'    KSTP',t12,'    KPER',t22,'  PERTIM',  &
           t40,'  TOTIM',t58,'            TEXT',t78,'   NODES',t88,'    INT1',t98,'    INT2')
         else
           write(outunit,44,err=9100)
44         format(t2,'    KSTP',t12,'    KPER',t22,'  PERTIM',  &
           t40,'  TOTIM',t58,'            TEXT',t78,'    INT1',t88,'    INT2',t98,'    INT3')
         end if
       end if
       narray=0
       ntime=0
       do
         if(iprec.eq.1)then
           read(inunit,err=800,end=200) kstp,kper,pertim,totim, &
           text,itemp1,itemp2,ilay
         else
           read(inunit,err=800,end=200) kstp,kper,dpertim,dtotim, &
           text,itemp1,itemp2,ilay
         end if
         if((kstp.lt.0).or.(kper.lt.0).or.(itemp1.lt.0).or.(itemp2.lt.0).or.(ilay.le.0)) go to 800
         if(iprec.eq.1)then
           if((pertim.lt.0.0).or.(totim.lt.0.0)) go to 800
           if((pertim.gt.rbig).or.(totim.gt.rbig)) go to 800
         else
           if((dpertim.lt.0.0).or.(dtotim.lt.0.0)) go to 800
           if((dpertim.gt.dbig).or.(dtotim.gt.dbig)) go to 800
         end if
         if(utl_textcheck(text).ne.0) go to 800
         narray=narray+1
         if((kstp.ne.kstpold).or.(kper.ne.kperold))then
           ntime=ntime+1
           kstpold=kstp
           kperold=kper
         end if
         if(iprec.eq.1)then
           if(isim.eq.22)then
             read(inunit,err=800,end=800)(rtemp,i=itemp1,itemp2)
           else
             read(inunit,err=800,end=800) (rtemp,i=1,itemp1*itemp2)
           end if
         else
           if(isim.eq.22)then
             read(inunit,err=800,end=800)(dtemp,i=itemp1,itemp2)
           else
             read(inunit,err=800,end=800) (dtemp,i=1,itemp1*itemp2)
           end if
         end if
         if(lout)then
           if(iprec.eq.1)then
             write(outunit,120) kstp,kper,pertim,totim,text,itemp1,itemp2,ilay
120          format(t2,i8,t12,i8,t22,1pg16.9,t40,1pg16.9,t58,a16,t78,i8,t88,i8,t98,i8)
           else
             write(outunit,120) kstp,kper,dpertim,dtotim,text,itemp1,itemp2,ilay
           end if
         end if
       end do
200    continue
       go to 1000

! -- Read and report contents of cell-by-cell flow term file.

230    continue
       if(lout)then
         outunit=utl_nextunit()
         open(unit=outunit,file=outfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,30) trim(afile2)
           go to 9890
         end if
         if((isim.eq.1).or.(isim.eq.21).or.(isim.eq.31))then
           if(isim.eq.31)then
             write(outunit,239)
239          format(t2,'    KSTP',t12,'    KPER',t22,'            TEXT',t42,'    NCOL',  &
                   t52,'    NROW',t62,'    NLAY',t72,'   IMETH',t82,'    DELT',          &
                  t100,'   PERTIM',t118,'   TOTIM',t136,'         TXT1ID1',              &
                  t156,'         TXT2ID1',t176,'         TXT1ID2',t196,'         TXT2ID2')
           else
             write(outunit,240)
240          format(t2,'    KSTP',t12,'    KPER',t22,'            TEXT',t42,'    NCOL',  &
                   t52,'    NROW',t62,'    NLAY',t72,'   IMETH',t82,'    DELT',          &
                  t100,'   PERTIM',t118,'   TOTIM')
           end if
         else if(isim.eq.22)then
           write(outunit,241)
241        format(t2,'    KSTP',t12,'    KPER',t22,'            TEXT',t42,'    NVAL',    &
                 t52,'    INT1',t62,'   ICODE',t72,'   IMETH',t82,'    DELT',            &
                t100,'   PERTIM',t118,'   TOTIM')
         else if(isim.eq.32)then
           write(outunit,242)
242        format(t2,'    KSTP',t12,'    KPER',t22,'            TEXT',t42,'    NCPL',    &
                 t52,'    INT1',t62,'    INT2',t72,'   IMETH',t82,'    DELT',            &
                t100,'   PERTIM',t118,'   TOTIM',t136,'         TXT1ID1',                &
                t156,'         TXT2ID1',t176,'         TXT1ID2',t196,'         TXT2ID2')
         else if(isim.eq.33)then
           write(outunit,2431)
2431       format(t2,'    KSTP',t12,'    KPER',t22,'            TEXT',t42,'   NDIM1',    &
                 t52,'   NDIM2',t62,'   NDIM3',t72,'   IMETH',t82,'    DELT',            &
                t100,'   PERTIM',t118,'   TOTIM',t136,'         TXT1ID1',                &
                t156,'         TXT2ID1',t176,'         TXT1ID2',t196,'         TXT2ID2')
         else
           write(outunit,243)
243        format(t2,'    KSTP',t12,'    KPER',t22,'            TEXT',t42,'   NDIM1',    &
                 t52,'   NDIM2',t62,'   NDIM3',t72,'   IMETH',t82,'    DELT',            &
                t100,'   PERTIM',t118,'   TOTIM')
         end if
       end if
       kperold=huge(kperold)
       kstpold=huge(kstpold)
       narray=0
       ntime=0
       do
         read(inunit,err=800,end=1000) kstp,kper,text,ndim1,ndim2,ndim3
         if((kstp.lt.0).or.(kper.lt.0).or.(ndim1.lt.0).or.(ndim2.lt.0)) go to 800
         if(utl_textcheck(text).ne.0) go to 800
         if((kstp.gt.ibig).or.(kper.gt.ibig).or.(ndim1.gt.ibig).or.(ndim2.gt.ibig)     &
                          .or.(abs(ndim3).gt.ibig))go to 800
         if(ndim3.gt.0)then
           if(lout)then
             write(outunit,250) kstp,kper,text,ndim1,ndim2,ndim3
250          format(t2,i8,t12,i8,t22,a16,t42,i8,t52,i8,t62,i8)
             if(iprec.eq.1)then
               read(inunit,err=800,end=800) (rtemp,i=1,ndim1*ndim2*ndim3)
             else
               read(inunit,err=800,end=800) (dtemp,i=1,ndim1*ndim2*ndim3)
             end if
           end if
         else
           if(iprec.eq.1)then
             read(inunit,err=800,end=800) imeth,delt,pertim,totim
           else
             read(inunit,err=800,end=800) imeth,ddelt,dpertim,dtotim
           end if
           if((imeth.lt.1).or.(imeth.gt.6))go to 800
           if(imeth.eq.6)then
             read(inunit,err=800,end=800) txt1id1
             if(utl_textcheck(txt1id1).ne.0) go to 800
             read(inunit,err=800,end=800) txt2id1
             if(utl_textcheck(txt2id1).ne.0) go to 800
             read(inunit,err=800,end=800) txt1id2
             if(utl_textcheck(txt1id2).ne.0) go to 800
             read(inunit,err=800,end=800) txt2id2
             if(utl_textcheck(txt2id2).ne.0) go to 800
           end if
           if(iprec.eq.1)then
             if(delt.lt.0.0) go to 800
             if(pertim.lt.0.0) go to 800
             if(totim.lt.0.0) go to 800
             if(delt.gt.rbig) go to 800
             if(pertim.gt.rbig) go to 800
             if(totim.gt.rbig) go to 800
           else
             if(ddelt.lt.0.0d0) go to 800
             if(dpertim.lt.0.0d0) go to 800
             if(dtotim.lt.0.0d0) go to 800
             if(ddelt.gt.dbig) go to 800
             if(dpertim.gt.dbig) go to 800
             if(dtotim.gt.dbig) go to 800
           end if
           if(lout)then
             if(imeth.eq.6)then    ! mf6 and therefore double precision
               write(outunit,2601) kstp,kper,text,ndim1,ndim2,ndim3,imeth,ddelt,dpertim,dtotim,  &
               adjustr(txt1id1),adjustr(txt2id1),adjustr(txt1id2),adjustr(txt2id2)
2601           format(t2,i8,t12,i8,t22,a16,t42,i8,t52,i8,t62,i8,t72,i8,t82,1pg16.9,              &
                      t100,1pg16.9,t118,1pg16.9,t136,a16,t156,a16,t176,a16,t196,a16)
             else
               if(iprec.eq.1)then
                 write(outunit,260) kstp,kper,text,ndim1,ndim2,ndim3,imeth,delt,pertim,totim
260              format(t2,i8,t12,i8,t22,a16,t42,i8,t52,i8,t62,i8,t72,i8,t82,1pg16.9,            &
                        t100,1pg16.9,t118,1pg16.9)
               else
                 write(outunit,260) kstp,kper,text,ndim1,ndim2,ndim3,imeth,ddelt,dpertim,dtotim
               end if
             end if
           end if
           if((imeth.eq.1).or.(imeth.eq.4))then
             if(isim.eq.22)then
               numread=ndim1
             else
               numread=abs(ndim1*ndim2*ndim3)
             end if
             if(iprec.eq.1)then
               read(inunit,err=800,end=800) (rtemp,i=1,numread)
             else
               read(inunit,err=800,end=800) (dtemp,i=1,numread)
             end if
           else if(imeth.eq.2)then
             read(inunit,err=800,end=800) nlist
             if(nlist.lt.0) go to 800
             if(nlist.gt.ibig) go to 800
             if(iprec.eq.1)then
               read(inunit,err=800,end=800) ((itemp,rtemp),i=1,nlist)
             else
               read(inunit,err=800,end=800) ((itemp,dtemp),i=1,nlist)
             end if
           else if(imeth.eq.3)then
             if(isim.eq.22)then
               numread=ndim1
             else
               numread=ndim1*ndim2
             end if
             read(inunit,err=800,end=800) (itemp,i=1,numread)
             if(iprec.eq.1)then
               read(inunit,err=800,end=800) (rtemp,i=1,numread)
             else
               read(inunit,err=800,end=800) (dtemp,i=1,numread)
             end if
           else if(imeth.eq.5)then
             read(inunit,err=800,end=800) ndat
             if(ndat.lt.-1) go to 800
             if(ndat.gt.ibig) go to 800
             if(ndat.gt.1)then
               read(inunit,err=800,end=800) (atemp16,i=1,ndat-1)
             end if
             read(inunit,err=800,end=800) nlist
             if(nlist.lt.0) go to 800
             if(nlist.gt.ibig) go to 800
             if((nlist.gt.0).and.(ndat.gt.0))then
               if(iprec.eq.1)then
                 read(inunit,err=800,end=800)((itemp,(rtemp,i=1,ndat)),j=1,nlist)
               else
                 read(inunit,err=800,end=800)((itemp,(dtemp,i=1,ndat)),j=1,nlist)
               end if
             end if
           else if(imeth.eq.6)then
             read(inunit,err=800,end=800) ndat
             if(ndat.gt.ibig) go to 800
             if(ndat.lt.0) go to 800
             if(ndat-1.ge.1)then
               read(inunit,err=800,end=800) (atemp16,i=1,ndat-1)
             end if
             read(inunit,err=800,end=800) nlist
             if(nlist.lt.0) go to 800
             if(nlist.gt.ibig) go to 800
             if(nlist.gt.0)then
               if(iprec.eq.1)then
                 read(inunit,err=800,end=800) ((itemp,itemp,(rtemp,i=1,ndat)),j=1,nlist)
               else
                 read(inunit,err=800,end=800) ((itemp,itemp,(dtemp,i=1,ndat)),j=1,nlist)
               end if
             end if
           end if
         end if
         narray=narray+1
         if((kstp.ne.kstpold).or.(kper.ne.kperold))then
           kstpold=kstp
           kperold=kper
           ntime=ntime+1
         end if
       end do

800    continue
       if(iturn.eq.1)then
         iturn=2
         if(lout)close(unit=outunit,iostat=ierr)
         if(iprec.eq.1)then
           iprec=2
         else
           iprec=1
         end if
         inquire(unit=inunit,opened=lopened)
         if(lopened)then
           rewind(unit=inunit)
         else
           inunit=utl_nextunit()
           open(unit=inunit,file=infile,status='old',form='unformatted',access='stream',iostat=ierr)
           if(ierr.ne.0) go to 9150
         end if
         if(itype.eq.1)then
           go to 25
         else
           go to 230
         end if
       else
         go to 9150
       end if

! -- Tidy up

1000   go to 9999

9100   write(amessage,9110) trim(afile2)
9110   format('Cannot write to file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(afile1)
9160   format('Cannot interpret contents of file ',a,'.')
       go to 9890

9890   continue
       inquire_modflow_binary_file_specs=1
       iprec=0
       narray=0
       ntime=0

9999   continue
       if(inunit.ne.0) close(unit=inunit,iostat=ierr)
       if(outunit.ne.0) close(unit=outunit,iostat=ierr)
       return

end function inquire_modflow_binary_file_specs


integer (kind=c_int) function retrieve_error_message(errormessage) bind(c, name="retrieve_error_message_")

! -- This function retrieves a previously-recorded error message.
! -- It returns the size of the error message. This may be greater than the size
!    of the array that is provided in the calling program. This length does not
!    include the string termination character.

       use iso_c_binding, only: c_int,c_char
       use utilities
       implicit none

       character (kind=c_char,len=1), intent(out) :: errormessage(LENMESSAGE)
       integer                                    :: lenmess

       if(amessage.eq.' ')then
         retrieve_error_message=0
       else
         lenmess=size(errormessage)
         call utl_char2string(lenmess,amessage,errormessage)
         retrieve_error_message=len_trim(amessage)
       end if
       return

end function retrieve_error_message



integer (kind=c_int) function install_structured_grid(gridname,ncol,nrow,nlay,icorner,    &
                                      e0,n0,rotation,delr,delc)                           &
                                      bind(c,name="install_structured_grid_")

! -- This function installs specifications for a structured grid.

       use iso_c_binding, only: c_int,c_char,c_double
       use dimvar
       use deftypes
       use utilities
       implicit none

       character (kind=c_char), intent(in)  :: gridname(LENGRIDNAME)
       integer (kind=c_int), intent(in)     :: ncol,nrow,nlay
       integer (kind=c_int), intent(in)     :: icorner             ! 1=top left; 2=bottom left
       real (kind=c_double), intent(in)     :: e0,n0,rotation
       real (kind=c_double), intent(in)     :: delr(ncol),delc(nrow)

       integer                        :: ierr
       integer                        :: igrid,icol,irow,jgrid
       double precision               :: side
       character (len=10)             :: atemp10
       character (len=LENGRIDNAME)    :: aname

! -- Initialisation

       install_structured_grid=0
       igrid=0

! -- A spare array element is located.

       if(numstrucmodgrid.ge.MAXSTRUCMODGRID)then
         call utl_num2char(MAXSTRUCMODGRID,atemp10)
         write(amessage,40) trim(atemp10)
40       format('A maximum of ',a,' structured model grid specifications ',       &
         'can be installed. You must uninstall at least one structured model ',   &
         'grid specification before you can install a new one.')
         go to 9890
       end if
       if(numstrucmodgrid.eq.0)then
         igrid=1
       else
         do igrid=1,MAXSTRUCMODGRID
           if(strucmodgrid(igrid)%nrow.eq.0) go to 60
         end do
         write(amessage,50)
50       format('Programming error in install_structured_grid(): contact programmer.')
         go to 9890
60       continue
       end if

! -- The name is translated to a character variable.

       call utl_string2char(LENGRIDNAME,gridname,aname)
       aname=adjustl(aname)
       call utl_casetrans(aname,'lo')
       if(aname.eq.' ')then
         write(amessage,100)
100      format('Gridname argument is supplied as blank.')
         go to 9890
       end if
       if(numstrucmodgrid.gt.0)then
         do jgrid=1,MAXSTRUCMODGRID
           if(strucmodgrid(jgrid)%nrow.ne.0)then
             if(aname.eq.strucmodgrid(jgrid)%name)then
               write(amessage,110) trim(aname)
110            format('The name "',a,'" belongs to an already-installed ',  &
               'structured grid.')
               go to 9890
             end if
           endif
         enddo
       end if
       strucmodgrid(igrid)%name=aname

! -- Some error checking is done.

       if(nrow.le.0)then
         write(amessage,120) 'NROW'
120      format('Input argument ',a,' must be positive.')
         go to 9890
       end if
       if(ncol.le.0)then
         write(amessage,120) 'NCOL'
         go to 9890
       end if
       if(nlay.le.0)then
         write(amessage,120) 'NLAY'
         go to 9890
       end if
       if((icorner.ne.1).and.(icorner.ne.2))then
         write(amessage,130)
130      format('Input argument ICORNER must be 1 or 2.')
         go to 9890
       end if
       if(any(delr.le.0.0d0))then
         write(amessage,135) 'DELR'
135      format('At least one element of the ',a,' array is non-positive.')
         go to 9890
       end if
       if(any(delc.le.0.0d0))then
         write(amessage,135) 'DELC'
         go to 9890
       end if
       if((rotation.gt.180.0d0).or.(rotation.lt.-180.0d0))then
         write(amessage,140)
140      format('Input argument ROTATION must not be less than -180 degrees or ',  &
         'greater than 180 degrees.')
         go to 9890
       end if

! -- Memory is allocated.

       strucmodgrid(igrid)%ncol=ncol
       strucmodgrid(igrid)%nrow=nrow
       strucmodgrid(igrid)%nlay=nlay
       strucmodgrid(igrid)%rotation=rotation
       strucmodgrid(igrid)%cosang=cos(rotation*pi/180.0d0)
       strucmodgrid(igrid)%sinang=sin(rotation*pi/180.0d0)
       allocate(strucmodgrid(igrid)%delr(ncol),strucmodgrid(igrid)%delc(nrow),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,160)
160      format('Memory allocation error.')
         go to 9890
       end if
       do irow=1,nrow
         strucmodgrid(igrid)%delc(irow)=delc(irow)
       end do
       do icol=1,ncol
         strucmodgrid(igrid)%delr(icol)=delr(icol)
       end do
       if(icorner.eq.1)then
         strucmodgrid(igrid)%e0=e0
         strucmodgrid(igrid)%n0=n0
       else
         side=0.0d0
         do irow=1,nrow
           side=side+delc(irow)
         end do
         strucmodgrid(igrid)%e0=e0-side*strucmodgrid(igrid)%sinang
         strucmodgrid(igrid)%n0=n0+side*strucmodgrid(igrid)%cosang
       end if
       numstrucmodgrid=numstrucmodgrid+1
       go to 9999

9890   continue
       install_structured_grid=1
       if(igrid.ne.0)then
         if(associated(strucmodgrid(igrid)%delr)) deallocate(strucmodgrid(igrid)%delr,stat=ierr)
         if(associated(strucmodgrid(igrid)%delc)) deallocate(strucmodgrid(igrid)%delc,stat=ierr)
         strucmodgrid(igrid)%ncol=0
         strucmodgrid(igrid)%nrow=0
         strucmodgrid(igrid)%nlay=0
         strucmodgrid(igrid)%name=' '
       end if

9999   continue
       return

end function install_structured_grid


integer (kind=c_int) function uninstall_structured_grid(gridname)               &
                     bind(c,name="uninstall_structured_grid_")

! -- This function uninstalls a previously installed set of structured grid specifications.

       use iso_c_binding, only: c_int,c_char
       use dimvar
       use deftypes
       use utilities
       use high_level_utilities
       implicit none

       character (kind=c_char), intent(in)  :: gridname(LENGRIDNAME)
       integer                              :: igrid,ierr
       character (len=LENGRIDNAME)          :: aname

! -- Initialisation

       uninstall_structured_grid=0

! -- The name is translated to a character variable.

       call utl_string2char(LENGRIDNAME,gridname,aname)
       aname=adjustl(aname)
       call utl_casetrans(aname,'lo')
       if(aname.eq.' ')then
         write(amessage,100)
100      format('GRIDNAME argument is supplied as blank.')
         go to 9890
       end if

! -- Does this correspond to a stored structured grid?

       if(numstrucmodgrid.eq.0) go to 9000
       do igrid=1,MAXSTRUCMODGRID
         if(strucmodgrid(igrid)%name.eq.aname) go to 200
       end do
       go to 9000
200    continue

! -- The grid is now deallocated.

       ierr=uth_strucmodgrid_deallocate(igrid)
       if(ierr.ne.0) go to 9890
       go to 9990

9000   write(amessage,9010) trim(aname)
9010   format('The name "',a,'" does not correspond to an installed structured grid.')
       go to 9890

9890   continue
       uninstall_structured_grid=1

9990   continue
       return

end function uninstall_structured_grid



integer (kind=c_int) function free_all_memory() bind(c,name="free_all_memory_")

! -- This function deallocates all memory that is being used.

       use dimvar
       use deftypes
       use utilities
       use high_level_utilities
       use iso_c_binding, only: c_int
       implicit none

       integer   :: ifail,igrid,jfail,ierr

! -- Initialisation

       free_all_memory=0
       jfail=0

! -- First structured grids.

       if(numstrucmodgrid.gt.0)then
         do igrid=1,MAXSTRUCMODGRID
           if(strucmodgrid(igrid)%nrow.ne.0)then
             ifail=uth_strucmodgrid_deallocate(igrid)
             if(ifail.ne.0) jfail=1
           end if
         end do
       end if

       if(nummf6modgrid.gt.0)then
         do igrid=1,MAXMF6MODGRID
           if(mf6modgrid(igrid)%distype.ne.0)then
             ifail=uth_mf6modgrid_deallocate(igrid)
             if(ifail.ne.0) jfail=1
           end if
         end do
       end if

       call free_param_memory1()
       call free_param_memory2()
       if(allocated(seed))deallocate(seed,stat=ierr)
       if(allocated(ivector1))deallocate(ivector1,stat=ierr)
       if(allocated(ivector2))deallocate(ivector2,stat=ierr)
       if(allocated(rvector1))deallocate(rvector1,stat=ierr)
       if(allocated(rvector2))deallocate(rvector2,stat=ierr)
       if(allocated(rvector3))deallocate(rvector3,stat=ierr)
       if(allocated(dvector1))deallocate(dvector1,stat=ierr)
       if(allocated(dvector2))deallocate(dvector2,stat=ierr)
       if(allocated(dvector3))deallocate(dvector3,stat=ierr)

       if(jfail.ne.0)then
         free_all_memory=1
         write(amessage,100)
100      format('Unable to free all memory.')
       end if

       return

end function free_all_memory



integer (kind=c_int) function interp_from_structured_grid(                   &
                             GridName,DepVarFile,isim,iprec,ntime,           &
                             VarType,InterpThresh,NoInterpVal,               &
                             npts,ecoord,ncoord,layer,                       &
                             nproctime,simtime,simstate)                     &
                             bind(c,name="interp_from_structured_grid_")

! -- This function performs spatial interpolation from a structured grid to a set of points.

       use iso_c_binding, only: c_int,c_double,c_char
       use dimvar
       use deftypes
       use utilities
       use high_level_utilities
       implicit none

       character (kind=c_char), intent(in)  :: gridname(LENGRIDNAME)      ! name of installed structured grid
       character (kind=c_char), intent(in)  :: depvarfile(LENFILENAME)    ! name of binary file to read
       integer(kind=c_int), intent(in)      :: isim                       ! -1 for MT3D; 1 for MODFLOW
       integer(kind=c_int), intent(in)      :: iprec                      ! 1 for single; 2 for double
       integer(kind=c_int), intent(in)      :: ntime                      ! number of output times
       character (kind=c_char), intent(in)  :: vartype(17)                ! only read arrays of this type
       real(kind=c_double), intent(in)      :: interpthresh               ! abs threshold for dry or inactive
       real(kind=c_double), intent(in)      :: nointerpval                ! no-interpolation-possible value
       integer(kind=c_int), intent(in)      :: npts                       ! number of points for which interpolation required
       real(kind=c_double), intent(in)      :: ecoord(npts),ncoord(npts)  ! eastings and northing of points
       integer(kind=c_int), intent(in)      :: layer(npts)                ! layers of points
       integer(kind=c_int), intent(out)     :: nproctime                  ! number of processed simulation times
       real(kind=c_double), intent(out)     :: simtime(ntime)             ! simulation time
       real(kind=c_double), intent(out)     :: simstate(ntime,npts)       ! interpolated system state

       integer                        :: kstp,kper,ntrans,kstpold,kperold,ntransold
       integer                        :: inunit,ierr,ipts,igrid
       integer                        :: ncol,nrow,icol,irow,nlay
       integer                        :: iarray,itime,mcol,mrow,ilay,ibig,icount
       real                           :: rinterpthresh,rgt_thresh,pertim,totim
       double precision               :: gt_thresh
       double precision               :: dpertim,dtotim
       character (len=10)             :: alay
       character (len=16)             :: atext,text
       character (len=LENGRIDNAME)    :: chargridname
       character (len=LENFILENAME)    :: infile

! -- Allocatable data objects.

       integer, allocatable           :: icellno(:),jcellno(:)           ! Could be automatic (but worried about stack)
       real, allocatable              :: rarray(:,:)
       double precision, allocatable  :: darray(:,:)
       double precision, allocatable  :: fac1(:),fac2(:),fac3(:),fac4(:)

! -- Initialisation

       interp_from_structured_grid=0
       function_name='interp_from_structured_grid()'
       simtime=nointerpval      ! default output value
       simstate=nointerpval     ! an array
       inunit=0
       kstpold=-99999999
       kperold=-99999999
       ntransold=-99999999
       kstp=kstpold
       kper=kperold
       ntrans=ntransold
       nproctime=0
       icount=0
       ibig=huge(ibig)/2                   ! arbitrary

! -- Character arrays are translated to character variables.

       call utl_string2char(LENGRIDNAME,gridname,chargridname)
       chargridname=adjustl(chargridname)
       call utl_casetrans(chargridname,'lo')
       call utl_string2char(LENFILENAME,depvarfile,infile)
       infile=adjustl(infile)
       call utl_string2char(17,vartype,atext)
       atext=adjustl(atext)
       call utl_casetrans(atext,'lo')

! -- We check for errors in input arguments.

       if((isim.ne.1).and.(isim.ne.-1))then
         write(amessage,90)
90       format('ISIM argument must be supplied as -1 (MT3D) or +1 (MODFLOW).')
         go to 9890
       end if
       if((iprec.ne.1).and.(iprec.ne.2))then
         write(amessage,100)
100      format('IPREC argument must be supplied as 1 (single) or 2 (double).')
         go to 9890
       end if
       if(ntime.le.0)then
         write(amessage,110) 'NTIME'
110      format(a,' argument must be positive.')
         go to 9890
       end if
       if(npts.le.0)then
         write(amessage,110) 'NPTS'
         go to 9890
       end if
       if(chargridname.eq.' ')then
         write(amessage,130) 'GRIDNAME'
130      format(a,' argument must not be an empty string.')
         go to 9890
       end if
       if(infile.eq.' ')then
         write(amessage,130) 'DEPVARFILE'
         go to 9890
       end if
       if(iprec.eq.1)then
         if(interpthresh.gt.huge(rinterpthresh)-2.0*spacing(huge(rinterpthresh)))then
           write(amessage,135)
135        format('Value supplied for INTERPTHRESH argument is greater ',  &
           'than single precision allows.')
           go to 9890
         end if
         rinterpthresh=min(huge(rinterpthresh)-2.0*spacing(huge(rinterpthresh)),real(interpthresh))
       end if

! -- Identify the grid.

       do igrid=1,MAXSTRUCMODGRID
         if(strucmodgrid(igrid)%name.eq.chargridname) go to 150
       end do
       write(amessage,140) trim(chargridname)
140    format('"',a,'" is not an installed grid.')
       go to 9890
150    continue
       nrow=strucmodgrid(igrid)%nrow
       ncol=strucmodgrid(igrid)%ncol
       nlay=strucmodgrid(igrid)%nlay

! -- Open the system state file.

       call utl_addquote(infile,afile1)
       inunit=utl_nextunit()
       open(unit=inunit,file=infile,status='old',form='unformatted',access='stream',iostat=ierr)
       if(ierr.ne.0) then
         write(amessage,160) trim(afile1)
160      format('Cannot open binary simulator output file ',a,'.')
         go to 9890
       end if

! -- Allocate memory

       if(iprec.eq.1)then
         allocate(rarray(0:ncol+1,0:nrow+1),stat=ierr)
         if(ierr.ne.0) go to 9200
       else
         allocate(darray(0:ncol+1,0:nrow+1),stat=ierr)
         if(ierr.ne.0) go to 9200
       end if
       allocate(icellno(npts),jcellno(npts),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(fac1(npts),fac2(npts),fac3(npts),fac4(npts),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- Calculate interpolation factors

       do ipts=1,npts
         call uth_strucfactors(igrid,ecoord(ipts),ncoord(ipts),fac1(ipts),fac2(ipts),  &
         fac3(ipts),fac4(ipts),icellno(ipts),jcellno(ipts))
       end do
       if(all(icellno.eq.-999)) then
         write(amessage,170) trim(strucmodgrid(igrid)%name)
170      format('None of the user-supplied points are within the bounds of ',    &
         'the "',a,'" structured grid.')
         go to 9890
       end if

! -- Pre-fill the cells of the array that are not in the model grid.

       if(iprec.eq.1)then
         rgt_thresh=rinterpthresh+2.0*(spacing(rinterpthresh))
         rarray(0,:)=rgt_thresh
         rarray(ncol+1,:)=rgt_thresh
         rarray(:,0)=rgt_thresh
         rarray(:,nrow+1)=rgt_thresh
       else
         gt_thresh=interpthresh+2.0d0*spacing(interpthresh)
         darray(0,:)=gt_thresh
         darray(ncol+1,:)=gt_thresh
         darray(:,0)=gt_thresh
         darray(:,nrow+1)=gt_thresh
       end if

! -- Read the input file

       iarray=0
       itime=0
       read_an_array: do
         iarray=iarray+1
         mrow=0
         mcol=0
         if(isim.eq.1)then
           if(iprec.eq.1)then
             read(inunit,err=9000,end=1000) kstp,kper,pertim,totim, &
             text,mcol,mrow,ilay
           else
             read(inunit,err=9000,end=1000) kstp,kper,dpertim,dtotim, &
             text,mcol,mrow,ilay
           end if
         else if(isim.eq.-1)then
           if(iprec.eq.1)then
             read(inunit,err=9000,end=1000) ntrans,kstp,kper,&
             totim,text,mcol,mrow,ilay
           else
             read(inunit,err=9000,end=1000) ntrans,kstp,kper,&
             dtotim,text,mcol,mrow,ilay
           end if
         end if
         if((mrow.lt.1).or.(mcol.lt.1).or.(ilay.lt.1).or.               &
            (mrow.gt.ibig).or.(mcol.gt.ibig).or.(ilay.gt.ibig))then
            write(amessage,180) trim(afile1)
180         format('An array header in file ',a,' does not make sense. Is this ', &
            'file being read using the correct precision?')
            go to 9890
          end if
         if((mrow.ne.nrow).or.(mcol.ne.ncol))then
           write(amessage,185) trim(chargridname),trim(afile1)
185        format('There is a NCOL or NROW mismatch between structured model ',  &
           'grid "',a,'" and arrays in file ',a,'. Alternatively this file is ', &
           'being read using the wrong precision.')
           go to 9890
         end if
         if(ilay.gt.nlay)then
           call utl_num2char(ilay,alay)
           write(amessage,190) trim(afile1),trim(alay),trim(chargridname)
190        format('An array in file ',a,' pertains to layer number ',a,'. This is ', &
           'larger than the number of layers in structured model grid "',a,'".')
           go to 9890
         end if
         text=adjustl(text)
         call utl_casetrans(text,'lo')
         if(iarray.ne.1)then
           if((kstp.ne.kstpold).or.(ntrans.ne.ntransold).or. &
             (kper.ne.kperold)) then
             itime=itime+1
             if(itime.gt.ntime) then
               itime=itime-1
               go to 1000
             end if
             if(iprec.eq.1)then
               simtime(itime)=totim
             else
               simtime(itime)=dtotim
             end if
           end if
         else
           itime=itime+1
           if(iprec.eq.1)then
             simtime(itime)=totim
           else
             simtime(itime)=dtotim
           end if
         end if
         if(iprec.eq.1)then
           read(inunit,err=9000,end=9050) ((rarray(icol,irow),icol=1,ncol), &
           irow=1,nrow)
           darray=rarray      ! arrays
         else
           read(inunit,err=9000,end=9050) ((darray(icol,irow),icol=1,ncol), &
           irow=1,nrow)
         end if
         kstpold=kstp
         kperold=kper
         ntransold=ntrans
         if(index(text,trim(atext)).ne.0)then
           icount=icount+1
           do ipts=1,npts
             if(layer(ipts).eq.ilay) then
                call uth_point_interp(ncol,nrow,interpthresh,nointerpval,fac1(ipts),    &
                fac2(ipts),fac3(ipts),fac4(ipts),icellno(ipts),jcellno(ipts),           &
                simstate(itime,ipts),darray)
             end if
           end do
         end if
       end do read_an_array
1000   if(iarray.eq.1) then
         write(amessage,1010) trim(afile1)
1010     format('No arrays were found in file ',a,'.')
         go to 9890
       end if
       nproctime=itime
       if(icount.eq.0)then
         write(amessage,1020) trim(atext),trim(afile1)
1020     format('No "',a,'" arrays found in file ',a,'.')
         go to 9890
       end if
       go to 9900

9000   continue
       if(isim.eq.-1)then
         write(amessage,9010) 'MT3D',trim(afile1)
       else
         write(amessage,9010) 'MODFLOW',trim(afile1)
       end if
9010   format('An error was encountered in reading ',a,' binary output file ',a,'.')
       go to 9890

9050   continue
       if(isim.eq.-1)then
         write(amessage,9060) 'MT3D',trim(afile1)
       else
         write(amessage,9060) 'MODFLOW',trim(afile1)
       end if
9060   format('An unexpected end was encountered when reading ',a,    &
       ' binary output file ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in call to function ',a,'.')
       go to 9890

9890   continue
       interp_from_structured_grid=1

9900   continue

! -- Tidy up

       if(inunit.ne.0)then
         close(unit=inunit,iostat=ierr)
       end if
       if(allocated(rarray))deallocate(rarray,stat=ierr)
       if(allocated(darray))deallocate(darray,stat=ierr)
       if(allocated(icellno))deallocate(icellno,stat=ierr)
       if(allocated(jcellno))deallocate(jcellno,stat=ierr)
       if(allocated(fac1))deallocate(fac1,stat=ierr)
       if(allocated(fac2))deallocate(fac2,stat=ierr)
       if(allocated(fac3))deallocate(fac3,stat=ierr)
       if(allocated(fac4))deallocate(fac4,stat=ierr)

       return

end function interp_from_structured_grid



integer (kind=c_int) function interp_to_obstime(                               &
                             nsimtime,nproctime,npts,simtime,simval,           &
                             interpthresh,how_extrap,time_extrap,nointerpval,  &
                             nobs,obspoint,obstime,obssimval)                  &
                             bind(c,name="interp_to_obstime_")

! -- This function performs temporal interpolation for simulation times to observation times.
! -- Note that observation times do not need to be supplied in increasing order for each
!    observation point. However simulation times in the SIMTIME array must be in increasing order.

       use iso_c_binding, only: c_int,c_double,c_char
       use utilities
       implicit none

       integer(kind=c_int), intent(in)         :: nsimtime           ! first dimension of simtime and simval arrays
       integer(kind=c_int), intent(in)         :: nproctime          ! number of times in simtime and simval arrays
       integer(kind=c_int), intent(in)         :: npts               ! number of points in simval array (second dimension)
       real(kind=c_double), intent(in)         :: simtime(nsimtime)      ! simulation times
       real(kind=c_double), intent(in)         :: simval(nsimtime,npts)  ! simulated values
       real(kind=c_double), intent(in)         :: interpthresh       ! values equal or above this in simval have no meaning
       character(kind=c_char,len=1),intent(in) :: how_extrap         ! L=linear; C=constant
       real(kind=c_double), intent(in)         :: time_extrap        ! permitted extrapolation time
       real(kind=c_double), intent(in)         :: nointerpval        ! dummy output value if interpolation impossible
       integer(kind=c_int), intent(in)         :: nobs               ! number of elements in obspoint, obstime and obssimval arrays
       integer(kind=c_int), intent(in)         :: obspoint(nobs)     ! indices of observation points (start at 0; -1 means no index)
       real(kind=c_double), intent(in)         :: obstime(nobs)      ! observation times
       real(kind=c_double), intent(out)        :: obssimval(nobs)    ! time-interpolated simulation values

       character (len=1)                :: aextrap
       character (len=20)               :: atemp20
       integer                          :: istime,ipts,iobs
       double precision                 :: ttime,dtime,dsim1,dsim2

! -- Initialisation

       interp_to_obstime=0
       function_name='interp_to_obstime()'
       obssimval=nointerpval      ! default value

! -- Checks are made

       if(interpthresh.le.0.0d0)then
         write(amessage,110) 'INTERPTHRESH', trim(function_name)
         go to 9890
       end if
       if(nsimtime.le.0)then
         write(amessage,110) 'NSIMTIME',trim(function_name)
110      format('The ',a,' argument of function ',a,' must be greater than zero.')
         go to 9890
       end if
       if(npts.le.0)then
         write(amessage,110) 'NPTS',trim(function_name)
         go to 9890
       end if
       if(nobs.le.0)then
         write(amessage,110) 'NOBS',trim(function_name)
         go to 9890
       end if
       if(nproctime.le.0)then
         write(amessage,110) 'NPROCTIME',trim(function_name)
         go to 9890
       end if
       if(nproctime.gt.nsimtime)then
         write(amessage,115)
115      format('Value for NPROCTIME argument must not exceed that of NSIMTIME argument.')
         go to 9890
       end if
       aextrap=how_extrap
       call utl_casetrans(aextrap,'lo')
       if((aextrap.ne.'l').and.(aextrap.ne.'c'))then
         write(amessage,120) trim(function_name)
120      format('The value supplied for the HOW_EXTRAP argument of function ',a,  &
         ' must be "L" or "C".')
         go to 9890
       end if
       if(nproctime.eq.1) aextrap='c'
       if(time_extrap.lt.0.0d0)then
         write(amessage,130) trim(function_name)
130      format('The TIME_EXTRAP argument of function ',a,' must be nonnegative.')
         go to 9890
       end if

       if(nproctime.gt.1)then
         do istime=2,nproctime
           if(simtime(istime).le.simtime(istime-1))then
             write(amessage,140)
140          format('Times provided in the SIMTIME array must be in increasing order.')
             go to 9890
           end if
         end do
       end if

! -- Interpolation is now performed.

       do iobs=1,nobs
         ipts=obspoint(iobs)+1              ! Recall that indices supplied by users begin at zero
         if(ipts.gt.0)then
           ttime=obstime(iobs)
           if(ttime.le.simtime(1))then
             dtime=simtime(1)-ttime
             if(dtime.le.time_extrap)then
               dsim1=simval(1,ipts)
               if(abs(dsim1).lt.interpthresh)then
                 if(aextrap.eq.'c')then
                   obssimval(iobs)=dsim1
                 else
                   dsim2=simval(2,ipts)
                   if(abs(dsim2).lt.interpthresh)then
                     obssimval(iobs)=dsim1-(dsim2-dsim1)/(simtime(2)-simtime(1))*dtime
                   else
                     obssimval(iobs)=dsim1
                   end if
                 end if
               end if
             end if
           else
             if(nproctime.gt.1)then
               do istime=2,nproctime
                 if(ttime.le.simtime(istime))then
                   dsim2=simval(istime,ipts)
                   if(ttime.eq.simtime(istime))then
                     if(abs(dsim2).lt.interpthresh) obssimval(iobs)=dsim2
                     go to 300
                   else
                     dsim1=simval(istime-1,ipts)
                     if(abs(dsim1).lt.interpthresh)then
                       if(abs(dsim2).lt.interpthresh)then
                         obssimval(iobs)=dsim1+(dsim2-dsim1)/(simtime(istime)-simtime(istime-1))  &
                                             *(ttime-simtime(istime-1))
                       end if
                     end if
                   end if
                   go to 300
                 end if
               end do
             end if
             dtime=ttime-simtime(nproctime)
             if(dtime.le.time_extrap)then
               dsim2=simval(nproctime,ipts)
               if(abs(dsim2).lt.interpthresh)then
                 if(aextrap.eq.'c')then
                   obssimval(iobs)=dsim2
                 else
                   dsim1=simval(nproctime-1,ipts)
                   if(abs(dsim1).lt.interpthresh)then
                     obssimval(iobs)=dsim2+(dsim2-dsim1)/(simtime(nproctime)-simtime(nproctime-1))*dtime
                   else
                     obssimval(iobs)=dsim2
                   end if
                 end if
               end if
             end if
300          continue
           end if
         end if
       end do

       go to 9900

9890   continue
       interp_to_obstime=1

9900   continue

end function interp_to_obstime



integer (kind=c_int) function install_mf6_grid_from_file(gridname,grbfile,        &
                              idis,ncells,ndim1,ndim2,ndim3)                      &
                              bind(c,name="install_mf6_grid_from_file_")

! -- This function installs specifications for a MF6 grid from the contents of a GRB file.

       use iso_c_binding, only: c_int,c_char
       use dimvar
       use deftypes
       use utilities
       use high_level_utilities
       implicit none

       character (kind=c_char,len=1), intent(in)  :: gridname(LENGRIDNAME)   ! Name of installed grid
       character (kind=c_char,len=1), intent(in)  :: grbfile(LENFILENAME)    ! MF6 GRB file from which grid specs read
       integer(kind=c_int), intent(out)           :: idis                    ! 1=DIS; 2=DISV
       integer(kind=c_int), intent(out)           :: ncells                  ! Number of cells in the grid
       integer(kind=c_int), intent(out)           :: ndim1,ndim2,ndim3       ! Grid dimensions

       integer                        :: i,ierr,itype
       integer                        :: igrid,gridunit,jgrid,itemp
       integer                        :: nlay,nrow,ncol,ncpl,nvert
       integer                        :: ntxt,lentxt
       integer                        :: ndimgridname,ndimgrbfile
       integer                        :: icol,irow,ilay,icpl,nja,njavert

       character (len=10)             :: atemp10
       character (len=20)             :: atype,anum,avar,aword
       character (len=LENGRIDNAME)    :: aname
       character (len=LENFILENAME)    :: agrbfile

! -- Initialisation

       function_name='install_mf6_grid_from_file()'
       install_mf6_grid_from_file=0
       igrid=0
       idis=0
       ncells=0; ndim1=0; ndim2=0; ndim3=0
       gridunit=0
!       ndimgridname=size(gridname)           ! Inherited from declaration
!       ndimgrbfile=size(grbfile)             ! Inherited from declaration
       ndimgridname=LENGRIDNAME
       ndimgrbfile=LENFILENAME

! -- Text strings are converted to character variables.

       call utl_string2char(ndimgridname,gridname,aname)
       aname=adjustl(aname)
       call utl_casetrans(aname,'lo')
       if(aname.eq.' ')then
         write(amessage,50) 'gridname', trim(function_name)
50       format('The ',a,' argument of function ',a,' is supplied as blank.')
         go to 9890
       end if

       call utl_string2char(ndimgrbfile,grbfile,agrbfile)
       agrbfile=adjustl(agrbfile)
       if(agrbfile.eq.' ')then
         write(amessage,50) 'grbfile',trim(function_name)
         go to 9890
       end if
       call utl_addquote(agrbfile,afile1)

! -- An empty array element is found.

       if(nummf6modgrid.ge.MAXMF6MODGRID)then
         call utl_num2char(MAXMF6MODGRID,atemp10)
         write(amessage,40) trim(atemp10)
40       format('A maximum of ',a,' MODFLOW 6 model grid specifications ',  &
         'can be installed. You must uninstall at least one MODFLOW 6 model ', &
         'grid specification before you can install a new one.')
         go to 9890
       end if
       if(nummf6modgrid.eq.0)then
         igrid=1
       else
         do igrid=1,MAXMF6MODGRID
           if(mf6modgrid(igrid)%distype.eq.0) go to 60
         end do
         write(amessage,45) trim(function_name)
45       format('Programming error in function ',a,': contact programmer.')
         go to 9890
60       continue
       end if

       if(nummf6modgrid.gt.0)then
         do jgrid=1,MAXMF6MODGRID
           if(mf6modgrid(jgrid)%distype.ne.0)then
             if(aname.eq.mf6modgrid(jgrid)%name)then
               write(amessage,110) trim(aname)
110            format('The name "',a,'" belongs to an already-installed ',  &
               'MODFLOW 6 grid.')
               go to 9890
             end if
           endif
         enddo
       end if
       mf6modgrid(igrid)%name=aname

! -- Open the GRB file.

       gridunit=utl_nextunit()
       open(unit=gridunit,file=agrbfile,status='old',form='unformatted',    &
       access='stream',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,150) trim(afile1)
150      format('Cannot open file ',a,'.')
         go to 9890
       end if

! -- We read the 4 header lines.

        mf6header=' '   ! An array
        atype='header'
        do i=1,4
          read(gridunit,err=9000,end=9050) mf6header(i)
        end do

! -- We obtain data from these header lines.

        mf6header(1)=adjustl(mf6header(1))
        if(mf6header(1)(1:9).eq.'GRID DIS ')then
          mf6modgrid(igrid)%distype=1
        else if(mf6header(1)(1:9).eq.'GRID DISV')then
          mf6modgrid(igrid)%distype=2
        else if(mf6header(1)(1:9).eq.'GRID DISU')then
          mf6modgrid(igrid)%distype=3
        else
          write(amessage,120) trim(mf6header(1)(1:49)),trim(afile1)
120       format('Unknown grid type: "',a,'" cited in file ',a,'.')
          go to 9890
        end if
        if(mf6modgrid(igrid)%distype.eq.3)then
          write(amessage,121)
121       format('As presently programmed, a DISU GRB file cannot be read.')
          go to 9890
        end if
        mf6header(3)=adjustl(mf6header(3))
        if(mf6header(3)(1:4).ne.'NTXT')then
          write(amessage,130) trim(afile1)
130       format('Third header line of file ',a,' expeced to begin with "NTXT".')
          go to 9890
        end if
        anum=adjustl(mf6header(3)(5:))
        if(utl_char2num(anum,ntxt).ne.0)then
          avar='NTXT'
          write(amessage,140) trim(avar),trim(afile1)
140       format('Cannot read ',a,' variable from header line of file ',a,'.')
          go to 9890
        end if
        mf6header(4)=adjustl(mf6header(4))
        if(mf6header(4)(1:6).ne.'LENTXT')then
          write(amessage,145) trim(afile1)
145       format('Fourth header line of file ',a,' expected to begin with "LENTXT".')
          go to 9890
        end if
        anum=adjustl(mf6header(4)(7:))
        if(utl_char2num(anum,lentxt).ne.0)then
          avar='LENTXT'
          write(amessage,140) trim(avar),trim(afile1)
          go to 9890
        end if
        if(lentxt.gt.MAXLENTXT)then
          write(amessage,160) trim(afile1)
160       format('Cannot read definition strings from file ',a,'. Increase MAXLENTXT ',   &
          'and re-compile program.')
          go to 9890
        end if

        idis=mf6modgrid(igrid)%distype
        if(idis.eq.1)then
          if(ntxt.ne.NTXT_DIS) go to 9070
        else if(idis.eq.2)then
          if(ntxt.ne.NTXT_DISV) go to 9070
        end if

! -- Next we read the definition lines.

        atype='definition'
        do i=1,ntxt
161       continue
          read(gridunit,err=9000,end=9050) definition(i)(1:lentxt)
          if(definition(i)(1:1).eq.'#') go to 161
          definition(i)=adjustl(definition(i)(1:lentxt-1))
        end do

! -- We now check that the definition lines are what we expect.

        do i=1,ntxt
          cline=definition(i)
          if(utl_wordsplit(2,lw,rw,cline).ne.0) go to 9150
          aword=cline(lw(2):rw(2))
          if(aword.eq.'INTEGER')then
            itype=1
          else if(aword.eq.'DOUBLE')then
            itype=2
          else
            itype=3
          end if
          if(idis.eq.1)then
            if(cline(lw(1):rw(1)).ne.expectdefn_dis(i)) go to 9150
            if(itype.ne.expect_dis_type(i)) go to 9150
          else if (idis.eq.2)then
            if(cline(lw(1):rw(1)).ne.expectdefn_disv(i)) go to 9150
            if(itype.ne.expect_disv_type(i)) go to 9150
          end if
        end do

! -- We now read the data and dimension arrays.

        if(idis.eq.1)then
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%ncells
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%nlay
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%nrow
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%ncol
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%nja
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%xorigin
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%yorigin
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%angrot
          nlay=mf6modgrid(igrid)%nlay
          ncol=mf6modgrid(igrid)%ncol
          nrow=mf6modgrid(igrid)%nrow
          nja=mf6modgrid(igrid)%nja
          ncells=mf6modgrid(igrid)%ncells
          itemp=nlay*nrow*ncol
          if(mf6modgrid(igrid)%ncells.ne.itemp)then
            write(amessage,170) trim(afile1)
170         format('NCELLS expected to be NCOL*NROW*NLAY in file ',a,'.')
            go to 9890
          end if
          allocate(mf6modgrid(igrid)%delr(0:ncol+1),mf6modgrid(igrid)%delc(0:nrow+1),stat=ierr)
          if(ierr.ne.0) go to 9200
          mf6modgrid(igrid)%ncr=nrow*ncol
          allocate(mf6modgrid(igrid)%botm(ncol,nrow,0:nlay),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%idomain(ncol,nrow,nlay),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%icelltype(ncol,nrow,nlay),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%ia(ncells+1),mf6modgrid(igrid)%ja(nja),stat=ierr)
          if(ierr.ne.0) go to 9200

          read(gridunit,err=9220,end=9240) (mf6modgrid(igrid)%delr(icol),icol=1,ncol)
          read(gridunit,err=9220,end=9240) (mf6modgrid(igrid)%delc(irow),irow=1,nrow)
          read(gridunit,err=9220,end=9240) ((mf6modgrid(igrid)%botm(icol,irow,0),icol=1,ncol),irow=1,nrow)
          read(gridunit,err=9220,end=9240) (((mf6modgrid(igrid)%botm(icol,irow,ilay),   &
                                              icol=1,ncol),irow=1,nrow),ilay=1,nlay)
          read(gridunit,err=9220,end=9240) (mf6modgrid(igrid)%ia(i),i=1,ncells+1)
          read(gridunit,err=9220,end=9240) (mf6modgrid(igrid)%ja(i),i=1,nja)
          read(gridunit,err=9220,end=9240) (((mf6modgrid(igrid)%idomain(icol,irow,ilay), &
                                              icol=1,ncol),irow=1,nrow),ilay=1,nlay)
          read(gridunit,err=9220,end=9240) (((mf6modgrid(igrid)%icelltype(icol,irow,ilay),  &
                                              icol=1,ncol),irow=1,nrow),ilay=1,nlay)
          mf6modgrid(igrid)%delr(0)=mf6modgrid(igrid)%delr(1)
          mf6modgrid(igrid)%delr(ncol+1)=mf6modgrid(igrid)%delr(ncol)
          mf6modgrid(igrid)%delc(0)=mf6modgrid(igrid)%delc(1)
          mf6modgrid(igrid)%delc(nrow+1)=mf6modgrid(igrid)%delc(nrow)
          if(any(mf6modgrid(igrid)%delr.le.0.0d0))then
            write(amessage,171) trim(afile1)
171         format('At least one member of the DELR array is zero or negative in file ',a,'.')
            go to 9890
          end if
          if(any(mf6modgrid(igrid)%delc.le.0.0d0))then
            write(amessage,172) trim(afile1)
172         format('At least one member of the DELC array is zero or negative in file ',a,'.')
            go to 9890
          end if

        else if (idis.eq.2)then

          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%ncells
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%nlay
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%ncpl
          ncpl=mf6modgrid(igrid)%ncpl
          nlay=mf6modgrid(igrid)%nlay
          ncells=mf6modgrid(igrid)%ncells
          if(mf6modgrid(igrid)%ncells.ne.nlay*ncpl)then
            write(amessage,180) trim(afile1)
180         format('NCELLS should equal NLAY*NCPL in file ',a,'.')
            go to 9890
          end if
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%nvert
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%njavert
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%nja
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%xorigin
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%yorigin
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%angrot
          nja=mf6modgrid(igrid)%nja
          nvert=mf6modgrid(igrid)%nvert
          njavert=mf6modgrid(igrid)%njavert

          allocate(mf6modgrid(igrid)%botmv(ncpl,0:nlay),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%vertices(2,nvert),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%cellx(ncpl),mf6modgrid(igrid)%celly(ncpl),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%iavert(ncpl+1),mf6modgrid(igrid)%javert(njavert),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%idomainv(ncpl,nlay),     &
          mf6modgrid(igrid)%icelltypev(ncpl,nlay),stat=ierr)
          if(ierr.ne.0) go to 9200
          allocate(mf6modgrid(igrid)%ia(ncells+1),mf6modgrid(igrid)%ja(nja),stat=ierr)
          if(ierr.ne.0) go to 9200

          read(gridunit,err=9220,end=9240) (mf6modgrid(igrid)%botmv(icpl,0),icpl=1,ncpl)
          read(gridunit,err=9220,end=9240) ((mf6modgrid(igrid)%botmv(icpl,ilay),icpl=1,ncpl),ilay=1,nlay)
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%vertices
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%cellx
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%celly
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%iavert
          read(gridunit,err=9220,end=9240) mf6modgrid(igrid)%javert
          read(gridunit,err=9220,end=9240) (mf6modgrid(igrid)%ia(i),i=1,ncells+1)
          read(gridunit,err=9220,end=9240) (mf6modgrid(igrid)%ja(i),i=1,nja)
          read(gridunit,err=9220,end=9240) ((mf6modgrid(igrid)%idomainv(icpl,ilay),icpl=1,ncpl),ilay=1,nlay)
          read(gridunit,err=9220,end=9240) ((mf6modgrid(igrid)%icelltypev(icpl,ilay),icpl=1,ncpl),ilay=1,nlay)

        end if
        nummf6modgrid=nummf6modgrid+1
        idis=mf6modgrid(igrid)%distype
        ncells=mf6modgrid(igrid)%ncells
        if(idis.eq.1)then
          ndim1=ncol
          ndim2=nrow
          ndim3=nlay
        else if(idis.eq.2)then
          ndim1=ncpl
          ndim2=1
          ndim3=nlay
        end if

        go to 9900

9000    write(amessage,9010) trim(atype),trim(afile1)
9010    format('Error encountered in reading ',a,' lines from file ',a,'.')
        go to 9890

9050    write(amessage,9060) trim(afile1),trim(atype)
9060    format('Premature end encountered to file ',a,' while reading ',a,' lines.')
        go to 9890

9070    write(amessage,9080) trim(afile1)
9080    format('Unexpected value for NTXT variable in file ',a,'.')
        go to 9890

9150    write(amessage,9160) trim(definition(i)),trim(afile1)
9160    format('Unexpected definition line "',a,'" in file ',a,'.')
        go to 9890

9200    write(amessage,9210)
9210    format('Cannot allocate memory required to store grid specifications.')
        go to 9890

9220    write(amessage,9230) trim(afile1)
9230    format('Error encountered in reading file ',a,'.')
        go to 9890

9240    write(amessage,9250) trim(afile1)
9250    format('Premature end encountered to file ',a,'.')
        go to 9890

9890   continue
       install_mf6_grid_from_file=1
       if(igrid.ne.0)then
         itemp=uth_mf6modgrid_deallocate(igrid)
       end if

9900   continue
       if(gridunit.ne.0) close(unit=gridunit,iostat=ierr)

       return

end function install_mf6_grid_from_file



integer (kind=c_int) function uninstall_mf6_grid(gridname)            &
                     bind(c,name="uninstall_mf6_grid_")

! -- This function uninstalls a previously installed mf6 grid specification.

       use iso_c_binding, only: c_int,c_char
       use dimvar
       use deftypes
       use utilities
       use high_level_utilities
       implicit none

       character (kind=c_char,len=1), intent(in)  :: gridname(LENGRIDNAME)  ! Grid name to uninstall

       integer                        :: igrid,ierr
       character (len=LENGRIDNAME)    :: aname

! -- Initialisation

       uninstall_mf6_grid=0

! -- The name is translated to a character variable.

       call utl_string2char(LENGRIDNAME,gridname,aname)
       aname=adjustl(aname)
       call utl_casetrans(aname,'lo')
       if(aname.eq.' ')then
         write(amessage,100)
100      format('GRIDNAME argument is supplied as blank.')
         go to 9890
       end if

! -- Does this correspond to a stored mf6 grid?

       if(nummf6modgrid.eq.0) go to 9000
       do igrid=1,MAXMF6MODGRID
         if(mf6modgrid(igrid)%distype.ne.0)then
           if(mf6modgrid(igrid)%name.eq.aname) go to 200
         end if
       end do
       go to 9000
200    continue

! -- The grid is now deallocated.

       ierr=uth_mf6modgrid_deallocate(igrid)
       if(ierr.ne.0) go to 9890
       go to 9990

9000   write(amessage,9010) trim(aname)
9010   format('The name "',a,'" does not correspond to an installed MODFLOW 6 grid.')
       go to 9890

9890   continue
       uninstall_mf6_grid=1

9990   continue
       return

end function uninstall_mf6_grid



integer (kind=c_int) function calc_mf6_interp_factors(gridname,        &
                              npts,ecoord,ncoord,layer,                &
                              factorfile, factorfiletype,              &
                              blnfile,interp_success)                  &
                              bind(c,name="calc_mf6_interp_factors_")

! -- This function calculates interpolation factors from a MODFLOW 6 DIS or DISV
!    grid to a set of user-supplied points.

       use iso_c_binding, only: c_int,c_double,c_char
       use dimvar
       use deftypes
       use utilities
       implicit none

       character (kind=c_char,len=1), intent(in)   :: gridname(LENGRIDNAME)      ! name of installed MF6 grid
       integer(kind=c_int), intent(in)             :: npts                       ! number of points for which interpolation required
       real(kind=c_double), intent(in)             :: ecoord(npts),ncoord(npts)  ! eastings and northing of points
       integer(kind=c_int), intent(in)             :: layer(npts)                ! layers of points
       character(kind=c_char,len=1), intent(in)    :: factorfile(LENFILENAME)    ! name of factor file
       integer(kind=c_int), intent(in)             :: factorfiletype             ! 0=binary; 1=text
       character(kind=c_char,len=1), intent(in)    :: blnfile(LENFILENAME)       ! name of bln file to write
       integer(kind=c_int), intent(out)            :: interp_success(npts)       ! 1=success; 0=failure

       integer                        :: ierr,i
       integer                        :: outunit1,outunit2
       integer                        :: igrid,nlay,ncol,nrow,ilay,icol,irow,ncr
       integer                        :: ncpl,icpl,distype
       integer                        :: ipos,ivert,ipts
       integer                        :: iicol,iirow,iilay,iicpl
       integer                        :: ndim1,ndim2,ndim3
       integer                        :: in1,in2,in3,in4
       integer                        :: nvcell,nvert,jvert,itemp,l,m
       integer                        :: numtri
       integer                        :: jlayoffset,mincon,icell,icon
       integer                        :: ic1,ic2,ic3,ic4
       integer                        :: i1,i2,i3
       integer                        :: ncon1,ncon2,ncon3,icon1,icon2,icon3,numcon,ncon
       integer                        :: icell_keep,ic1_keep,ic2_keep,ic3_keep

       double precision               :: xxmin,xxmax,yymin,yymax
       double precision               :: eee,nnn
       double precision               :: x,y,xorigin,yorigin,cosang,sinang
       double precision               :: ee,nn,xx,yy
       double precision               :: dx,dy,ddx,ddy,den
       double precision               :: fac1,fac2,fac3,fac4,dtemp,eps
       double precision               :: y23,x13,x32,y13
       double precision               :: b1,b2,b3
       double precision               :: bb1,bb2,bb3,bb4
       double precision               :: xpoly(MAXINTERPVERT+1),ypoly(MAXINTERPVERT+1)
       double precision               :: xpoly_keep(MAXINTERPVERT+1),ypoly_keep(MAXINTERPVERT+1)

       character (len=20)             :: apts,anum
       character (len=LENGRIDNAME)    :: chargridname
       character (len=LENFILENAME)    :: outfile1,outfile2

! -- Pointers

       integer, pointer, dimension(:)            :: iavert, javert,ia,ja
       double precision, pointer, dimension(:)   :: delr,delc
       double precision, pointer, dimension(:)   :: cellx,celly
       double precision, pointer, dimension(:,:) :: vertices

! -- Allocatable arrays

       double precision, allocatable  :: xcell(:),ycell(:)
       double precision, allocatable  :: cellxmax(:),cellxmin(:),cellymax(:),cellymin(:)

! -- Initialisation

       calc_mf6_interp_factors=0
       function_name='calc_mf6_interp_factors()'
       outunit1=0
       outunit2=0
       eps=1.0d-7              !arbitrary


! -- Character arrays are translated to character variables.

       call utl_string2char(LENGRIDNAME,gridname,chargridname)
       chargridname=adjustl(chargridname)
       call utl_casetrans(chargridname,'lo')
       call utl_string2char(LENFILENAME,factorfile,outfile1)
       outfile1=adjustl(outfile1)
       call utl_string2char(LENFILENAME,blnfile,outfile2)
       outfile2=adjustl(outfile2)

! -- Other checks of input arguments are made.

       if(npts.le.0)then
         write(amessage,110) trim(function_name)
110      format('The NPTS argument of function ',a,' must be greater than zero.')
         go to 9890
       end if

       do igrid=1,MAXMF6MODGRID
         if(mf6modgrid(igrid)%name.eq.chargridname) go to 150
       end do
       write(amessage,140) trim(chargridname)
140    format('"',a,'" is not an installed grid.')
       go to 9890
150    continue
       distype=mf6modgrid(igrid)%distype
       nlay=mf6modgrid(igrid)%nlay
       if(any(layer.le.0))then
         write(amessage,160)
160      format('At least one element of the LAYER array argument is zero or less.')
         go to 9890
       end if
       if(any(layer.gt.nlay))then
         write(amessage,170) trim(chargridname)
170      format('At least one element of the LAYER array argument has a value that is ',  &
         'greater than NLAY for MF6 grid "',a,'".')
         go to 9890
       end if
       if(outfile1.eq.' ')then
         write(amessage,180)
180      format('The value of the factorfile argument must not be blank.')
         go to 9890
       end if
       if((factorfiletype.ne.0).and.(factorfiletype.ne.1))then
         write(amessage,185) trim(function_name)
185      format('The factorfiletype argument of function ',a,' must be supplied as 0 or 1.')
         go to 9890
       end if
       call utl_addquote(outfile1,afile1)
       if(outfile2.ne.' ') call utl_addquote(outfile2,afile2)

! -- We find the maximum and minimum local x and y coordinates in the grid.

        if(distype.eq.1)then
          nrow=mf6modgrid(igrid)%nrow
          ncol=mf6modgrid(igrid)%ncol
          ncr=mf6modgrid(igrid)%ncr
          delr=>mf6modgrid(igrid)%delr
          delc=>mf6modgrid(igrid)%delc
          xxmin=0.0d0
          xxmax=0.0d0
          do icol=1,ncol
            xxmax=xxmax+delr(icol)
          end do
          yymin=0.0d0
          yymax=0.0d0
          do irow=nrow,1,-1
            yymax=yymax+delc(irow)
          end do
! -- Find cell centre coords in local coordinate system.
          allocate(xcell(ncol),ycell(nrow),stat=ierr)
          if(ierr.ne.0) go to 9200
          xcell(1)=delr(1)*0.5
          do icol=2,ncol
            xcell(icol)=xcell(icol-1)+(delr(icol-1)+delr(icol))*0.5
          end do
          ycell(nrow)=delc(nrow)*0.5
          do irow=nrow-1,1,-1
            ycell(irow)=ycell(irow+1)+(delc(irow+1)+delc(irow))*0.5
          end do
        else if(distype.eq.2)then    ! The following assumes that there are no "stray vertices".
          xxmin=1.0d300
          xxmax=-1.0d300
          yymin=1.0d300
          yymax=-1.0d300
          ncpl=mf6modgrid(igrid)%ncpl
          nvert=mf6modgrid(igrid)%nvert
          vertices=>mf6modgrid(igrid)%vertices
          iavert=>mf6modgrid(igrid)%iavert
          javert=>mf6modgrid(igrid)%javert
          ia=>mf6modgrid(igrid)%ia
          ja=>mf6modgrid(igrid)%ja
          cellx=>mf6modgrid(igrid)%cellx
          celly=>mf6modgrid(igrid)%celly
          do ivert=1,nvert
            if(vertices(1,ivert).gt.xxmax)xxmax=vertices(1,ivert)
            if(vertices(1,ivert).lt.xxmin)xxmin=vertices(1,ivert)
            if(vertices(2,ivert).gt.yymax)yymax=vertices(2,ivert)
            if(vertices(2,ivert).lt.yymin)yymin=vertices(2,ivert)
          end do
! -- To make the following search easier, we find max coordinates of each grid cell.
          allocate(cellxmax(ncpl),cellxmin(ncpl),cellymax(ncpl),cellymin(ncpl),stat=ierr)
          if(ierr.ne.0) go to 9200
          cellxmax=-1.0d300          !an array
          cellymax=-1.0d300          !an array
          cellxmin=1.0d300           !an array
          cellymin=1.0d300           !an array
          do icpl=1,ncpl
            do ipos=iavert(icpl),iavert(icpl+1)-1
              ivert=javert(ipos)
              x=vertices(1,ivert)
              y=vertices(2,ivert)
              if(x.gt.cellxmax(icpl))cellxmax(icpl)=x
              if(x.lt.cellxmin(icpl))cellxmin(icpl)=x
              if(y.gt.cellymax(icpl))cellymax(icpl)=y
              if(y.lt.cellymin(icpl))cellymin(icpl)=y
            end do
          end do
        end if

! -- The factor file is opened.

        outunit1=utl_nextunit()
        if(factorfiletype.eq.0)then
          open(unit=outunit1,file=outfile1,form='unformatted',access='stream',   &
          action='write',iostat=ierr)
        else
          open(unit=outunit1,file=outfile1,action='write',iostat=ierr)
        end if
        if(ierr.ne.0)then
          write(amessage,190) trim(afile1)
190       format('Cannot write to file ',a,'.')
          go to 9890
        end if
        if(outfile2.ne.' ')then
          outunit2=utl_nextunit()
          open(unit=outunit2,file=outfile2,action='write',iostat=ierr)
          if(ierr.ne.0)then
            write(amessage,190) trim(afile2)
            go to 9890
          end if
        end if

! -- Record data at the top of the file.

        if(distype.eq.1)then
          ndim1=ncol
          ndim2=nrow
        else if(distype.eq.2)then
          ndim1=ncpl
          ndim2=1
        end if
        ndim3=nlay
        if(factorfiletype.eq.0)then
          write(outunit1,err=9300) npts,distype,ndim1,ndim2,ndim3
          do ipts=1,npts
            write(outunit1,err=9300) ecoord(ipts),ncoord(ipts),layer(ipts)
          end do
        else
          write(outunit1,220,err=9300) npts,distype,ndim1,ndim2,ndim3
220       format(5i10)
          do ipts=1,npts
            write(outunit1,230,err=9300) ecoord(ipts),ncoord(ipts),layer(ipts)
230         format(2(' ',1pg23.16),1x,i10)
          end do
        end if

! -- The points are now processed one by one.

        xorigin=mf6modgrid(igrid)%xorigin
        yorigin=mf6modgrid(igrid)%yorigin
        cosang=cos(mf6modgrid(igrid)%angrot*pi/180.0d0)
        sinang=sin(mf6modgrid(igrid)%angrot*pi/180.0d0)
        do ipts=1,npts
          interp_success(ipts)=1
          ilay=layer(ipts)
! -- Express point coordinates in local coordinates if necessary.
          ee=ecoord(ipts)
          nn=ncoord(ipts)
          xx=(ee-xorigin)*cosang+(nn-yorigin)*sinang
          yy=-(ee-xorigin)*sinang+(nn-yorigin)*cosang
          if((xx.lt.xxmin).or.(xx.gt.xxmax).or.(yy.lt.yymin).or.(yy.gt.yymax)) go to 459
! -- Now we locate the cell (DISV grid) or the cell centres surrounding the bore (DIS grid).
          if(distype.eq.1)then
            if(xx.lt.xcell(1)) go to 459
            do icol=2,ncol
              if(xx.le.xcell(icol))then
                iicol=icol-1
                go to 300
              end if
            end do
            go to 459
300         continue
            if(yy.gt.ycell(1)) go to 459
            do irow=2,nrow
              if(yy.ge.ycell(irow))then
                iirow=irow-1
                go to 310
              end if
            end do
            go to 459
310         continue
          else if(distype.eq.2)then
            do icpl=1,ncpl
              if((xx.le.cellxmax(icpl)).and.(xx.ge.cellxmin(icpl)))then
                if((yy.le.cellymax(icpl)).and.(yy.ge.cellymin(icpl)))then
                  nvcell = iavert(icpl+1) - iavert(icpl)
                  if(nvcell+1.gt.MAXINTERPVERT)then
                    call utl_num2char(nvcell,anum)
                    write(amessage,320) trim(anum)
320                 format(' At least one cell in the MODFLOW 6 grid has ',a,' or more vertices. ',   &
                    'Increase MAXINTERPVERT to greater than this and re-compile program.')
                    go to 9890
                  end if
                  i=0
                  do ipos = iavert(icpl), iavert(icpl + 1) - 1
                    ivert=javert(ipos)
                    i=i+1
                    xpoly(i)=vertices(1,ivert)
                    ypoly(i)=vertices(2,ivert)
                  end do
                  jvert=ivert
                  ipos = iavert(icpl)
                  ivert=javert(ipos)
                  itemp=nvcell
                  if(ivert.ne.jvert)then
                    i=i+1
                    xpoly(i)=vertices(1,ivert)
                    ypoly(i)=vertices(2,ivert)
                    itemp=nvcell+1
                  end if
                  call utl_locpt(xx,yy,xpoly,ypoly,itemp,l,m)
                  if(l.ge.0) then
                    iicpl=icpl
                    go to 340
                  end if
                end if
              end if
            end do
            go to 459
340         continue
          end if
! -- We record interpolation factors.
350       continue
          if(distype.eq.1)then
            iilay=layer(ipts)
            in1=(iilay-1)*ncr+(iirow-1)*ncol+iicol
            in2=in1+1
            in3=in2+ncol
            in4=in3-1
            dx=xx-xcell(iicol)
            dy=ycell(iirow)-yy
            ddx=xcell(iicol+1)-xx
            ddy=yy-ycell(iirow+1)
            den=(dx+ddx)*(dy+ddy)
            fac1=ddx*ddy/den
            fac2=dx*ddy/den
            fac3=dx*dy/den
            fac4=ddx*dy/den
! -- Quality assurance
            if((fac1.lt.-eps).or.(fac2.lt.-eps).or.(fac3.lt.-eps).or.(fac4.lt.-eps)) go to 9170
            dtemp=1.0d0+eps
            if((fac1.gt.dtemp).or.(fac2.gt.dtemp).or.(fac3.gt.dtemp).or.(fac4.gt.dtemp)) go to 9170
            dtemp=fac1+fac2+fac3+fac4
            if((dtemp.lt.1.0d0-3.0d0*eps).or.(dtemp.gt.1.0d0+3.0d0*eps)) go to 9170
            if(factorfiletype.eq.0)then
              write(outunit1) 4,in1,fac1,in2,fac2,in3,fac3,in4,fac4
            else
              write(outunit1,360) 4,in1,fac1,in2,fac2,in3,fac3,in4,fac4
360           format(i6,100(' ',i10,' ',1pg23.16))
            end if
            if(outunit2.ne.0)then
              call utl_num2char(ipts,apts)
              write(outunit2,440) 5,0,trim(apts)
              eee=xcell(iicol)*cosang-ycell(iirow)*sinang+xorigin
              nnn=xcell(iicol)*sinang+ycell(iirow)*cosang+yorigin
              write(outunit2,450) eee,nnn
              eee=xcell(iicol+1)*cosang-ycell(iirow)*sinang+xorigin
              nnn=xcell(iicol+1)*sinang+ycell(iirow)*cosang+yorigin
              write(outunit2,450) eee,nnn
              eee=xcell(iicol+1)*cosang-ycell(iirow+1)*sinang+xorigin
              nnn=xcell(iicol+1)*sinang+ycell(iirow+1)*cosang+yorigin
              write(outunit2,450) eee,nnn
              eee=xcell(iicol)*cosang-ycell(iirow+1)*sinang+xorigin
              nnn=xcell(iicol)*sinang+ycell(iirow+1)*cosang+yorigin
              write(outunit2,450) eee,nnn
              eee=xcell(iicol)*cosang-ycell(iirow)*sinang+xorigin
              nnn=xcell(iicol)*sinang+ycell(iirow)*cosang+yorigin
              write(outunit2,450) eee,nnn
            end if
          else if(distype.eq.2)then
            iilay=layer(ipts)
            jlayoffset=(iilay-1)*ncpl
            mincon=99999999
! -- We look for a three or four connection path back to the cell.
! -- Make sure that this works for layers other than the first.
            icell=(iilay-1)*ncpl+iicpl
            ncon=ia(icell+1)-ia(icell)-1
            do icon=1,ncon
              ic1=ja(ia(icell)+icon)
              if((ic1.eq.icell-ncpl).or.(ic1.eq.icell+ncpl))cycle
              ncon1=ia(ic1+1)-ia(ic1)-1
              do icon1=1,ncon1
                ic2=ja(ia(ic1)+icon1)
                if(ic2.eq.ic1)cycle
                if((ic2.eq.ic1-ncpl).or.(ic2.eq.ic1+ncpl))cycle
                ncon2=ia(ic2+1)-ia(ic2)-1
                do icon2=1,ncon2
                  ic3=ja(ia(ic2)+icon2)
                  if(ic3.eq.ic2)cycle
                  if((ic3.eq.ic2-ncpl).or.(ic3.eq.ic2+ncpl))cycle
                  if(ic3.eq.icell)then
                    numcon=3
                    xpoly(1)=cellx(icell-jlayoffset)
                    ypoly(1)=celly(icell-jlayoffset)
                    xpoly(2)=cellx(ic1-jlayoffset)
                    ypoly(2)=celly(ic1-jlayoffset)
                    xpoly(3)=cellx(ic2-jlayoffset)
                    ypoly(3)=celly(ic2-jlayoffset)
                    xpoly(4)=cellx(icell-jlayoffset)
                    ypoly(4)=celly(icell-jlayoffset)
                    call utl_locpt(xx,yy,xpoly,ypoly,4,l,m)
                    if(l.ge.0) then
                      mincon=3
                      go to 430
                    end if
                  end if
                  ncon3=ia(ic3+1)-ia(ic3)-1
                  do icon3=1,ncon3
                    ic4=ja(ia(ic3)+icon3)
                    if(ic4.eq.ic3)cycle
                    if((ic4.eq.ic3-ncpl).or.(ic4.eq.ic3+ncpl))cycle
                    if(ic4.eq.icell)then
                      numcon=4
                      xpoly(1)=cellx(icell-jlayoffset)
                      ypoly(1)=celly(icell-jlayoffset)
                      xpoly(2)=cellx(ic1-jlayoffset)
                      ypoly(2)=celly(ic1-jlayoffset)
                      xpoly(3)=cellx(ic2-jlayoffset)
                      ypoly(3)=celly(ic2-jlayoffset)
                      xpoly(4)=cellx(ic3-jlayoffset)
                      ypoly(4)=celly(ic3-jlayoffset)
                      xpoly(5)=cellx(icell-jlayoffset)
                      ypoly(5)=celly(icell-jlayoffset)
                      call utl_locpt(xx,yy,xpoly,ypoly,5,l,m)
                      if(l.ge.0) then
                        if(numcon.lt.mincon)then
                          mincon=numcon
                          xpoly_keep(1)=xpoly(1)
                          ypoly_keep(1)=ypoly(1)
                          xpoly_keep(2)=xpoly(2)
                          ypoly_keep(2)=ypoly(2)
                          xpoly_keep(3)=xpoly(3)
                          ypoly_keep(3)=ypoly(3)
                          xpoly_keep(4)=xpoly(4)
                          ypoly_keep(4)=ypoly(4)
                          xpoly_keep(5)=xpoly(5)
                          ypoly_keep(5)=ypoly(5)
                          icell_keep=icell
                          ic1_keep=ic1
                          ic2_keep=ic2
                          ic3_keep=ic3
                        end if
                      end if
                    end if
                  end do
                end do
              end do
            end do
            if(mincon.lt.99999999)then
              numcon=mincon
              xpoly(1)=xpoly_keep(1)
              ypoly(1)=ypoly_keep(1)
              xpoly(2)=xpoly_keep(2)
              ypoly(2)=ypoly_keep(2)
              xpoly(3)=xpoly_keep(3)
              ypoly(3)=ypoly_keep(3)
              xpoly(4)=xpoly_keep(4)
              ypoly(4)=ypoly_keep(4)
              icell=icell_keep
              ic1=ic1_keep
              ic2=ic2_keep
              if(numcon.eq.4)then
                xpoly(5)=xpoly_keep(5)
                ypoly(5)=ypoly_keep(5)
                ic3=ic3_keep
              end if
            else
              go to 459
            end if
430         continue

! -- Record interpolation factors
            if(numcon.eq.3)then
              y23=ypoly(2)-ypoly(3)
              x13=xpoly(1)-xpoly(3)
              x32=xpoly(3)-xpoly(2)
              y13=ypoly(1)-ypoly(3)
              den=y23*x13+x32*y13
              if(den.eq.0.0d0) go to 459
              b1=(y23*(xx-xpoly(3))+x32*(yy-ypoly(3)))/den
              b2=(-y13*(xx-xpoly(3))+x13*(yy-ypoly(3)))/den
              b3=1.0d0-b1-b2
              dtemp=0.0d0-eps
              if((b1.lt.dtemp).or.(b2.lt.dtemp).or.(b3.lt.dtemp)) go to 9000
              dtemp=1.0d0+eps
              if((b1.gt.dtemp).or.(b2.gt.dtemp).or.(b3.gt.dtemp)) go to 9000
              dtemp=b1+b2+b3
              if((dtemp.lt.1.0d0-eps).or.(dtemp.gt.1.0d0+eps)) go to 9100
              if(factorfiletype.eq.0)then
                write(outunit1,err=9300)3,icell,b1,ic1,b2,ic2,b3
              else
                write(outunit1,360,err=9300) 3,icell,b1,ic1,b2,ic2,b3
              end if
            else if(numcon.eq.4)then
! -- We average over all possible triangles; normally there will be 2.
              numtri=0
              bb1=0.0d0
              bb2=0.0d0
              bb3=0.0d0
              bb4=0.0d0
              do i=1,4
                if(i.eq.1)then
                  i1=1
                  i2=2
                  i3=4
                else if(i.eq.2)then
                  i1=2
                  i2=3
                  i3=4
                else if(i.eq.3)then
                  i1=1
                  i2=2
                  i3=3
                else if(i.eq.4)then
                  i1=1
                  i2=3
                  i3=4
                end if
                y23=ypoly(i2)-ypoly(i3)
                x13=xpoly(i1)-xpoly(i3)
                x32=xpoly(i3)-xpoly(i2)
                y13=ypoly(i1)-ypoly(i3)
                den=y23*x13+x32*y13
                if(den.eq.0.0d0) cycle
                b1=(y23*(xx-xpoly(i3))+x32*(yy-ypoly(i3)))/den
                b2=(-y13*(xx-xpoly(i3))+x13*(yy-ypoly(i3)))/den
                b3=1.0d0-b1-b2
                dtemp=0.0d0-eps
                if((b1.lt.dtemp).or.(b2.lt.dtemp).or.(b3.lt.dtemp)) cycle
                dtemp=1.0d0+eps
                if((b1.gt.dtemp).or.(b2.gt.dtemp).or.(b3.gt.dtemp)) cycle
                numtri=numtri+1
                if(i.eq.1)then
                  bb1=bb1+b1
                  bb2=bb2+b2
                  bb4=bb4+b3
                else if(i.eq.2)then
                  bb2=bb2+b1
                  bb3=bb3+b2
                  bb4=bb4+b3
                else if(i.eq.3)then
                  bb1=bb1+b1
                  bb2=bb2+b2
                  bb3=bb3+b3
                else if(i.eq.4)then
                  bb1=bb1+b1
                  bb3=bb3+b2
                  bb4=bb4+b3
                end if
              end do
              if(numtri.eq.0) go to 9050
              if(numtri.gt.1)then
                bb1=bb1/numtri
                bb2=bb2/numtri
                bb3=bb3/numtri
                bb4=bb4/numtri
              end if
              dtemp=bb1+bb2+bb3+bb4
              if((dtemp.lt.1.0d0-3.0d0*eps).or.(dtemp.gt.1.0d0+3.0d0*eps)) go to 9150
              if(factorfiletype.eq.0)then
                write(outunit1,err=9300) 4,icell,bb1,ic1,bb2,ic2,bb3,ic3,bb4
              else
                write(outunit1,360,err=9300) 4,icell,bb1,ic1,bb2,ic2,bb3,ic3,bb4
              end if
            end if
! -- Record the subtending polygon.
            if(outunit2.ne.0)then
              call utl_num2char(ipts,apts)
              write(outunit2,440) numcon+1,0,trim(apts)
440           format(i10,i5,5x,'"',a,'"')
              do i=1,numcon+1
                eee=xpoly(i)*cosang-ypoly(i)*sinang+xorigin
                nnn=xpoly(i)*sinang+ypoly(i)*cosang+yorigin
                write(outunit2,450) eee,nnn
450             format(1x,f20.4,3x,f20.4)
              end do
            end if
          end if
          cycle
459       continue
          interp_success(ipts)=0
          if(factorfiletype.eq.0)then
            write(outunit1,err=9300) 0
          else
            write(outunit1,361,err=9300) 0
361         format(i6)
          end if
        end do

        go to 9900

9000    continue
        call utl_num2char(1,anum)
        write(amessage,9010) trim(anum),trim(function_name)
9010    format('Error type ',a,' encountered in function ',a,': contact programmer.')
        go to 9890

9050    continue
        call utl_num2char(2,anum)
        write(amessage,9010) trim(anum),trim(function_name)
        go to 9890

9100    continue
        call utl_num2char(3,anum)
        write(amessage,9010) trim(anum),trim(function_name)
        go to 9890

9150    continue
        call utl_num2char(4,anum)
        write(amessage,9010) trim(anum),trim(function_name)
        go to 9890

9170    continue
        call utl_num2char(5,anum)
        write(amessage,9010) trim(anum),trim(function_name)
        go to 9890

9200    write(amessage,9210)
9210    format('Cannot allocate sufficient memory to continue execution.')
        go to 9890

9300    write(amessage,9310) trim(afile1)
9310    format('Error writing to file ',a,'.')
        go to 9890

9890    continue
        calc_mf6_interp_factors=1

9900    continue
        if(outunit1.ne.0) close(unit=outunit1,iostat=ierr)
        if(outunit2.ne.0) close(unit=outunit2,iostat=ierr)

        if(allocated(xcell)) deallocate(xcell,stat=ierr)
        if(allocated(ycell)) deallocate(ycell,stat=ierr)
        if(allocated(cellxmax)) deallocate(cellxmax,stat=ierr)
        if(allocated(cellxmin)) deallocate(cellxmin,stat=ierr)
        if(allocated(cellymax)) deallocate(cellymax,stat=ierr)
        if(allocated(cellymin)) deallocate(cellymax,stat=ierr)

        if(associated(iavert)) nullify(iavert)
        if(associated(javert)) nullify(javert)
        if(associated(ia)) nullify(ia)
        if(associated(ja)) nullify(ja)
        if(associated(delr)) nullify(delr)
        if(associated(delc)) nullify(delc)
        if(associated(cellx)) nullify(cellx)
        if(associated(celly)) nullify(celly)
        if(associated(vertices)) nullify(vertices)

end function calc_mf6_interp_factors



integer (kind=c_int) function interp_from_mf6_depvar_file(             &
                 depvarfile,factorfile,factorfiletype,                 &
                 ntime,vartype,interpthresh,reapportion,nointerpval,   &
                 npts,nproctime,simtime,simstate)                      &
                 bind(c,name="interp_from_mf6_depvar_file_")

! -- This function interpolates to a set of points using previously-calculated
!    interpolation factors.

       use iso_c_binding, only: c_int,c_char,c_double
       use dimvar
       use utilities
       implicit none

       character(kind=c_char,len=1), intent(in)   :: depvarfile(LENFILENAME)   ! Dependent variable file written by MODFLOW 6
       character(kind=c_char,len=1), intent(in)   :: factorfile(LENFILENAME)   ! File containing spatial interpolation factors
       integer(kind=c_int), intent(in)            :: factorfiletype       ! 0 for binary; 1 for ascii
       integer(kind=c_int), intent(in)            :: ntime                ! First dimension of simtime and simstate arrays
       character (kind=c_char,len=1), intent(in)  :: vartype(17)           ! Only read arrays of this type
       real(kind=c_double), intent(in)            :: interpthresh         ! Abs threshold for dry or inactive
       integer(kind=c_int), intent(in)            :: reapportion          ! 0 for no; 1 for yes
       real(kind=c_double), intent(in)            :: nointerpval          ! No-interpolation-possible value
       integer(kind=c_int), intent(in)            :: npts                 ! Second dimension of simstate
       integer(kind=c_int), intent(out)           :: nproctime            ! Number of processed simulation times
       real(kind=c_double), intent(out)           :: simtime(ntime)       ! Simulation times
       real(kind=c_double), intent(out)           :: simstate(ntime,npts) ! Interpolated system states

       integer                        :: ierr,itemp
       integer                        :: dvunit,facunit
       integer                        :: mpts,distype,ndim1,ndim2,ndim3
       integer                        :: ipts,icell,itime,inode
       integer                        :: ncol,nrow,ncpl,nlay,mcol,mrow,mlay,mcpl,icpl,ilay
       integer                        :: kstpold,kperold,kstp,kper
       double precision               :: dtemp,dpertim,dtotim,sum,dtotimold,den
       character (len=16)             :: atext,text,textold
       character (len=256)            :: dvfile,facfile

       integer, allocatable           :: ncell(:),ifac(:,:)
       double precision, allocatable  :: dfac(:,:)
       double precision, allocatable  :: darray(:,:)

! -- Initialisation

       interp_from_mf6_depvar_file=0
       function_name='interp_from_mf6_depvar_file()'
       dvunit=0
       facunit=0
       simtime=nointerpval     ! an array
       simstate=nointerpval    ! an array
       kstpold=-99999999
       kperold=-99999999
       textold=' '
       dtotimold=-1.0d30
       nproctime=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,depvarfile,dvfile)
       dvfile=adjustl(dvfile)
       call utl_string2char(LENFILENAME,factorfile,facfile)
       facfile=adjustl(facfile)
       call utl_string2char(17,vartype,atext)
       atext=adjustl(atext)
       call utl_casetrans(atext,'lo')

! -- Check input arguments.

       if(ntime.le.0)then
         write(amessage,110) 'NTIME', trim(function_name)
110      format('The ', a,' argument of function ',a,' must be positive.')
         go to 9890
       end if
       if(npts.le.0)then
         write(amessage,110) 'NPTS', trim(function_name)
         go to 9890
       end if
       if(dvfile.eq.' ')then
         write(amessage,120) 'DEPVARFILE',trim(function_name)
120      format('The ',a,' argument of function ',a,' must not be blank.')
         go to 9890
       end if
       if(facfile.eq.' ')then
         write(amessage,120) 'FACTORFILE',trim(function_name)
         go to 9890
       end if
       if(atext.eq.' ')then
         write(amessage,120) 'VARTYPE',trim(function_name)
         go to 9890
       end if
       if((factorfiletype.ne.0).and.(factorfiletype.ne.1))then
         write(amessage, 130) trim(function_name)
130      format('The FACTORFILETYPE argument of function ',a,' must be supplied as 0 or 1.')
         go to 9890
       end if
       if(interpthresh.le.0.0d0)then
         write(amessage,140) trim(function_name)
140      format('The INTERPTHRESH argument of function ',a,' must be positive.')
         go to 9890
       end if

! -- Open the dependent variable file.

       call utl_addquote(dvfile,afile1)
       dvunit=utl_nextunit()
       open(unit=dvunit,file=dvfile,status='old',form='unformatted',access='stream',iostat=ierr)
       if(ierr.ne.0) then
         write(amessage,160) trim(afile1)
160      format('Cannot open binary MODFLOW 6 output file ',a,'.')
         go to 9890
       end if

! -- Open the factor file.

       call utl_addquote(facfile,afile2)
       facunit=utl_nextunit()
       if(factorfiletype.eq.0)then
         open(unit=facunit,file=facfile,status='old',form='unformatted',access='stream',iostat=ierr)
         if(ierr.ne.0) then
           write(amessage,180) trim(afile2)
180        format('Cannot open binary interpolation factor file ',a,'.')
           go to 9890
         end if
       else
         open(unit=facunit,file=facfile,status='old',iostat=ierr)
         if(ierr.ne.0) then
           write(amessage,185) trim(afile2)
185        format('Cannot open ASCII interpolation factor file ',a,'.')
           go to 9890
         end if
       end if

! -- Read the first line of the factor file to ascertain the number of points and some model details.

       if(factorfiletype.eq.0)then
         read(facunit,err=9000,end=9000) mpts,distype,ndim1,ndim2,ndim3
       else
         read(facunit,*,err=9000,end=9000) mpts,distype,ndim1,ndim2,ndim3
       end if
       if(mpts.ne.npts)then
         write(amessage,190) trim(afile2)
190      format('The value for NPTS recorded in the header to file ',a,' does not ', &
         'agree with the value of the user-supplied NPTS function argument.')
         go to 9890
       end if
       if(distype.eq.1)then
         ncol=ndim1
         nrow=ndim2
         ncpl=nrow*ncol
       else
         ncpl=ndim1
       end if
       nlay=ndim3

! -- Arrays are allocated

       allocate(darray(ncpl,nlay),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(ncell(npts),ifac(MAXINTERPVERT,npts),dfac(MAXINTERPVERT,npts),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- The first NPTS lines of the factor file are read.

       if(factorfiletype.eq.0)then
         do ipts=1,npts
           read(facunit,err=9000,end=9000) dtemp,dtemp,itemp
         end do
       else
         do ipts=1,npts
           read(facunit,*,err=9000,end=9000) dtemp,dtemp,itemp
         end do
       end if

! -- Interpolation factors are read.

       if(factorfiletype.eq.0)then
         do ipts=1,npts
           read(facunit,err=9000,end=9000) ncell(ipts),                &
               (ifac(icell,ipts),dfac(icell,ipts),icell=1,ncell(ipts))
         end do
       else
         do ipts=1,npts
           read(facunit,*,err=9000,end=9000) ncell(ipts),              &
               (ifac(icell,ipts),dfac(icell,ipts),icell=1,ncell(ipts))
         end do
       end if
       close(unit=facunit)

! -- The dependent variable file is read.

       itime=0
       do
         if(distype.eq.1)then
           read(dvunit,err=9100,end=500) kstp,kper,dpertim,dtotim,text,mcol,mrow,mlay
           if((mcol.ne.ncol).or.(mrow.ne.nrow)) go to 9300
         else
           read(dvunit,err=9100,end=500) kstp,kper,dpertim,dtotim,text,mcpl,itemp,mlay
           if(mcpl.ne.ncpl) go to 9300
         end if
         if((mlay.le.0).or.(mlay.gt.nlay)) go to 9300
         if((kstp.ne.kstpold).or.(kper.ne.kperold))then
           textold=adjustl(textold)
           call utl_casetrans(textold,'lo')
           if(index(textold,trim(atext)).ne.0) then
             itime=itime+1
             if(itime.gt.ntime) go to 500
             simtime(itime)=dtotimold
             do ipts=1,npts
               if(ncell(ipts).gt.0)then
                 sum=0.0d0
                 den=0.0d0
                 do icell=1,ncell(ipts)
                   inode=ifac(icell,ipts)
                   ilay=(inode-1)/ncpl+1
                   icpl=inode-(ilay-1)*ncpl
                   dtemp=darray(icpl,ilay)
                   if(abs(dtemp).ge.interpthresh)then
                     if(reapportion.eq.0) go to 210
                   end if
                   sum=sum+dtemp*dfac(icell,ipts)
                   den=den+dfac(icell,ipts)
                 end do
                 if(den.ne.0.0d0) simstate(itime,ipts)=sum/den
210              continue
               end if
             end do
           end if
           kstpold=kstp
           kperold=kper
           dtotimold=dtotim
           textold=text
           darray=interpthresh+2.0d0*spacing(interpthresh)
         end if
         read(dvunit,err=9100,end=9150) (darray(icpl,mlay),icpl=1,ncpl)
       end do

500    continue
       textold=adjustl(textold)
       call utl_casetrans(textold,'lo')
       if(index(textold,trim(atext)).ne.0) then
         if(itime+1.le.ntime) then
           itime=itime+1
           simtime(itime)=dtotimold
           do ipts=1,npts
             if(ncell(ipts).gt.0)then
               sum=0.0d0
               den=0.0d0
               do icell=1,ncell(ipts)
                 inode=ifac(icell,ipts)
                 ilay=(inode-1)/ncpl+1
                 icpl=inode-(ilay-1)*ncpl
                 dtemp=darray(icpl,ilay)
                 if(abs(dtemp).ge.interpthresh) then
                   if(reapportion.eq.0) go to 520
                 end if
                 sum=sum+dtemp*dfac(icell,ipts)
                 den=den+dfac(icell,ipts)
               end do
               if(den.ne.0.0d0) simstate(itime,ipts)=sum/den
520            continue
             end if
           end do
         end if
       end if

       if(itime.eq.0)then
         write(amessage,510) trim(atext),trim(afile1)
510      format('No dependent variable arrays characterized by text "',a,   &
         '" were found in file ',a,'.')
         go to 9890
       end if
       nproctime=itime
       go to 9900

9000   continue
       if(factorfiletype.eq.0)then
         write(amessage,9010) trim(afile2)
9010     format('Error encountered when reading binary interpolation factor ', &
         'file ',a,'.')
       else
         write(amessage,9011) trim(afile2)
9011     format('Error encountered when reading ASCII interpolation factor ', &
         'file ',a,'.')
       end if
       go to 9890

9100   write(amessage,9110) trim(afile1)
9110   format('Error encountered when reading binary MODFLOW 6 dependent variable file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(afile1)
9160   format('Premature end encountered to binary MODFLOW 6 dependent variable file ',a,'.')
       go to 9890


9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in function ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(afile1),trim(afile2)
9310   format('Grid dimensions listed in array headers in file ',a,' are not ', &
       'in accordance with those of model grid for which interpolation factors ', &
       'are recorded in file ',a,'.')
       go to 9890

9890   continue
       interp_from_mf6_depvar_file=1

9900   continue
       if(dvunit.ne.0) close(unit=dvunit,iostat=ierr)
       if(facunit.ne.0) close(unit=facunit,iostat=ierr)

       if(allocated(ncell))deallocate(ncell,stat=ierr)
       if(allocated(ifac))deallocate(ifac,stat=ierr)
       if(allocated(dfac))deallocate(dfac,stat=ierr)
       if(allocated(darray))deallocate(darray,stat=ierr)

end function interp_from_mf6_depvar_file



integer (kind=c_int) function extract_flows_from_cbc_file(     &
                 cbcfile,flowtype,isim,iprec,                  &
                 ncell,izone,nzone,                            &
                 numzone,zonenumber,                           &
                 ntime,nproctime,                              &
                 timestep,stressperiod,simtime,simflow)        &
                 bind(c,name="extract_flows_from_cbc_file_")

! -- This function reads and accumulates flows (as read from a cell-by-cell flow term file)
!    to a user-specified boundary condition.

       use iso_c_binding, only: c_int,c_char,c_double
       use dimvar
       use utilities
       implicit none

       character (kind=c_char,len=1), intent(in)  :: cbcfile(LENFILENAME)  ! Cell-by-cell flow term file written by any MF version.
       character (kind=c_char,len=1), intent(in)  :: flowtype(17)           ! Type of flow to read.
       integer(kind=c_int), intent(in)            :: isim                  ! Simulator type
       integer(kind=c_int), intent(in)            :: iprec                 ! Precision used to record real variables in cbc file
       integer(kind=c_int), intent(in)            :: ncell                 ! Number of cells in the model
       integer(kind=c_int), intent(in)            :: izone(ncell)          ! Zonation of model domain
       integer(kind=c_int), intent(in)            :: nzone                 ! Equals or exceeds number of zones; zone 0 doesn't count
       integer(kind=c_int), intent(out)           :: numzone               ! Number of non-zero-valued zones
       integer(kind=c_int), intent(out)           :: zonenumber(nzone)     ! Zone numbers (processed from izone)
       integer(kind=c_int), intent(in)            :: ntime                 ! Equals or exceed number of model output times for flow type
       integer(kind=c_int), intent(out)           :: nproctime             ! Number of processed simulation times
       integer(kind=c_int), intent(out)           :: timestep(ntime)       ! Simulation time step
       integer(kind=c_int), intent(out)           :: stressperiod(ntime)   ! Simulation stress period
       real(kind=c_double), intent(out)           :: simtime(ntime)        ! Simulation time; a time of -1.0 indicates unknown
       real(kind=c_double), intent(out)           :: simflow(ntime,nzone)  ! Interpolated flows

! -- Note that for older versions of MODFLOW the simulation time may not be recorded in the cbc flow term file.
!    This is why time step and stress period are also reported.

       integer                        :: inunit,ibig,itemp,iflag,ierr,ndimtemp
       integer                        :: ialloci,iallocr
       integer                        :: icell,iz
       integer                        :: kstp,kper,ndim1,ndim2,ndim3,imeth
       integer                        :: ilist,nlist,idat,ndat,numread,ilay,jcell
       real                           :: rbig,delt,pertim,totim
       real                           :: rtemp
       double precision               :: dbig,ddelt,dpertim,dtotim
       double precision               :: dtemp

       character (len=16)             :: atext,text
       character (len=16)             :: txt1id1,txt2id1,txt1id2,txt2id2,atemp16
       character (len=20)             :: atemp20
       character (len=LENFILENAME)    :: infile

! -- Allocatable arrays

       integer, allocatable           :: cell2zone(:)  ! Could be automatic, but worried about stack.
       integer, allocatable           :: iarray(:)
       real, allocatable              :: rarray(:)
       double precision, allocatable  :: darray(:)

! -- Initialisation

       extract_flows_from_cbc_file=0
       function_name='extract_flows_from_cbc_file()'
       inunit=0
       timestep=0       ! an array
       stressperiod=0   ! an array
       simtime=0.0d0
       simflow=0.0d0    ! an array
       numzone=0
       zonenumber=0     ! an array
       ibig=huge(ibig)/2                   ! arbitrary
       rbig=huge(rbig)*0.5                 ! arbitrary
       dbig=huge(dbig)*0.5d0               ! arbitrary
       ialloci=0
       iallocr=0
       nproctime=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,cbcfile,infile)
       infile=adjustl(infile)
       call utl_string2char(17,flowtype,atext)
       atext=adjustl(atext)
       call utl_casetrans(atext,'lo')

! -- Check input arguments.

       if((isim.ne.0).and.(isim.ne.1).and.(isim.ne.21).and.(isim.ne.22).and.   &
          (isim.ne.31).and.(isim.ne.32).and.(isim.ne.33))then
          write(amessage,90)
90         format('The isim argument must be 0, 1, 21, 22, 31, 32 or 33')
         go to 9890
       end if
       if(ntime.le.0)then
         write(amessage,110) 'NTIME', trim(function_name)
110      format('The ', a,' argument of function ',a,' must be positive.')
         go to 9890
       end if
       if(nzone.le.0)then
         write(amessage,110) 'NZONE', trim(function_name)
         go to 9890
       end if
       if(ncell.le.0)then
         write(amessage,110) 'NCELL',trim(function_name)
         go to 9890
       end if
       if(infile.eq.' ')then
         write(amessage,120) 'CBCFILE',trim(function_name)
120      format('The ',a,' argument of function ',a,' must not be blank.')
         go to 9890
       end if
       if((atext.eq.'flow right face').or.    &
          (atext.eq.'flow front face').or.    &
          (atext.eq.'flow lower face').or.    &
          (atext.eq.'flow-ja-face'))then
          write(amessage,125)
125       format('Flowtypes of FLOW RIGHT FACE, FLOW-JA-FACE etc ',  &
          'are not allowed. Flows must be to/from a boundary condition.')
          go to 9890
       end if

! -- Open the cell-by-cell flow term file.

       call utl_addquote(infile,afile1)
       inunit=utl_nextunit()
       open(unit=inunit,file=infile,status='old',form='unformatted',access='stream',iostat=ierr)
       if(ierr.ne.0) then
         write(amessage,160) trim(afile1)
160      format('Cannot open binary cell-by-cell flow term file ',a,'.')
         go to 9890
       end if

! -- Extract zones from izone array

       iz=0
       do icell=1,ncell
         if(izone(icell).ne.0)then
           if(numzone.eq.0)then
             numzone=numzone+1
             zonenumber(numzone)=izone(icell)
           else
             itemp=utl_whichone(numzone,iz,zonenumber,izone(icell))
             if(itemp.ne.0)then
               numzone=numzone+1
               if(numzone.gt.nzone)then
                 write(amessage,180)
180              format('The number of different non-zero integers that are featured ',  &
                 'in the IZONE input array exceeds the user-supplied values for NZONE.')
                 go to 9890
               end if
               zonenumber(numzone)=izone(icell)
             end if
           end if
         end if
       end do
       if(numzone.eq.0)then
         write(amessage,190)
190      format('Input array IZONE features no nonzero zone numbers.')
         go to 9890
       end if

! -- Zones within the zonenumber array are now sorted.

       call utl_sort(numzone,zonenumber)

! -- Create an indexing array.

       allocate(cell2zone(ncell),stat=ierr)
       if(ierr.ne.0) go to 9200
       iz=0
       do icell=1,ncell
         if(izone(icell).eq.0)then
           cell2zone(icell)=0
         else
           itemp=utl_whichone(numzone,iz,zonenumber,izone(icell))
           if(itemp.ne.0)then
             write(amessage,210) trim(function_name)
210          format('Programming error encountered in function ',a,': - contact programmer.')
             go to 9890
           end if
           cell2zone(icell)=iz
         end if
       end do

! -- Progress through the budget file.

       do
         if(nproctime+1.gt.ntime) go to 1000
         read(inunit,err=9000,end=1000) kstp,kper,text,ndim1,ndim2,ndim3
         if((kstp.lt.0).or.(kper.lt.0).or.(ndim1.lt.0).or.(ndim2.lt.0)) go to 9000
         if(utl_textcheck(text).ne.0) go to 9000
         if((kstp.gt.ibig).or.(kper.gt.ibig).or.(ndim1.gt.ibig).or.(ndim2.gt.ibig)     &
                          .or.(abs(ndim3).gt.ibig))go to 9000
         call utl_casetrans(text,'lo')
         text=adjustl(text)
         iflag=0
         if(index(text,trim(atext)).ne.0) iflag=1
         if(ndim3.gt.0)then
           if(iflag.eq.0)then
             if(iprec.eq.1)then
               read(inunit,err=9100,end=9100) (rtemp,icell=1,ndim1*ndim2*ndim3)
             else
               read(inunit,err=9100,end=9100) (dtemp,icell=1,ndim1*ndim2*ndim3)
             end if
           else
             itemp=ndim1*ndim2*ndim3
             if(itemp.ne.ncell)go to 9050
             if(iallocr.eq.0)then
               iallocr=ncell
               if(iprec.eq.1)then
                 allocate(rarray(ncell),stat=ierr)
               else
                 allocate(darray(ncell),stat=ierr)
               end if
               if(ierr.ne.0) go to 9200
             else
               if(iallocr.lt.ncell)then
                 if(iprec.eq.1)then
                   deallocate(rarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(rarray(ncell),stat=ierr)
                 else
                   deallocate(darray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(darray(ncell),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
               end if
             end if
             nproctime=nproctime+1
             timestep(nproctime)=kstp
             stressperiod(nproctime)=kper
             simtime(nproctime)=-1.0d0
             if(iprec.eq.1)then
               read(inunit,err=9100,end=9100) (rarray(icell),icell=1,ncell)
             else
               read(inunit,err=9100,end=9100) (darray(icell),icell=1,ncell)
             end if
             do icell=1,ncell
               iz=cell2zone(icell)
               if(iz.ne.0)then
                 if(iprec.eq.1)then
                   simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(icell)
                 else
                   simflow(nproctime,iz)=simflow(nproctime,iz)+darray(icell)
                 end if
               end if
             end do
           end if
         else
           ndim3=-ndim3
           if(iprec.eq.1)then
             read(inunit,err=9000,end=9000) imeth,delt,pertim,totim
             dtotim=totim
           else
             read(inunit,err=9000,end=9000) imeth,ddelt,dpertim,dtotim
           end if
           if((imeth.lt.1).or.(imeth.gt.ibig))go to 9000
           if(imeth.gt.6)then
             write(amessage,217) trim(afile1)
217          format('An array header in file ',a,' cites an IMETH value that is ',  &
             'greater than 6.')
             go to 9890
           end if
           if(imeth.eq.6)then
             read(inunit,err=9000,end=9000) txt1id1
             if(utl_textcheck(txt1id1).ne.0) go to 9000
             read(inunit,err=9000,end=9000) txt2id1
             if(utl_textcheck(txt2id1).ne.0) go to 9000
             read(inunit,err=9000,end=9000) txt1id2
             if(utl_textcheck(txt1id2).ne.0) go to 9000
             read(inunit,err=9000,end=9000) txt2id2
             if(utl_textcheck(txt2id2).ne.0) go to 9000
           end if
           if(iprec.eq.1)then
             if(delt.lt.0.0) go to 9000
             if(pertim.lt.0.0) go to 9000
             if(totim.lt.0.0) go to 9000
             if(delt.gt.rbig) go to 9000
             if(pertim.gt.rbig) go to 9000
             if(totim.gt.rbig) go to 9000
           else
             if(ddelt.lt.0.0d0) go to 9000
             if(dpertim.lt.0.0d0) go to 9000
             if(dtotim.lt.0.0d0) go to 9000
             if(ddelt.gt.dbig) go to 9000
             if(dpertim.gt.dbig) go to 9000
             if(dtotim.gt.dbig) go to 9000
           end if
           if(iflag.ne.0)then
             nproctime=nproctime+1
             timestep(nproctime)=kstp
             stressperiod(nproctime)=kper
             simtime(nproctime)=dtotim
           end if
           if(imeth.eq.1)then
             if(iflag.eq.0)then
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) (rtemp,icell=1,ndim1*ndim2*ndim3)
               else
                 read(inunit,err=9100,end=9100) (dtemp,icell=1,ndim1*ndim2*ndim3)
               end if
             else
               itemp=abs(ndim1*ndim2*ndim3)
               if(itemp.ne.ncell)go to 9050
               if(iallocr.eq.0)then
                 if(iprec.eq.1)then
                   allocate(rarray(ncell),stat=ierr)
                 else
                   allocate(darray(ncell),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ncell
               else
                 if(iallocr.lt.ncell)then
                   if(iprec.eq.1)then
                     deallocate(rarray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(rarray(ncell),stat=ierr)
                   else
                     deallocate(darray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(darray(ncell),stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=ncell
                 end if
               end if
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) (rarray(icell),icell=1,ncell)
               else
                 read(inunit,err=9100,end=9100) (darray(icell),icell=1,ncell)
               end if
               do icell=1,ncell
                 iz=cell2zone(icell)
                 if(iz.ne.0)then
                   if(iprec.eq.1)then
                     simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(icell)
                   else
                     simflow(nproctime,iz)=simflow(nproctime,iz)+darray(icell)
                   end if
                 end if
               end do
             end if
           else if(imeth.eq.2)then
             read(inunit,err=9100,end=9100) nlist
             if(nlist.lt.0) go to 9000
             if(nlist.gt.ibig) go to 9000
             if(iflag.eq.0)then
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) ((itemp,rtemp),ilist=1,nlist)
               else
                 read(inunit,err=9100,end=9100) ((itemp,dtemp),ilist=1,nlist)
               end if
             else
               ndimtemp=max(nlist,ncell)
               if(iallocr.eq.0)then
                 if(iprec.eq.1)then
                   allocate(rarray(ndimtemp),stat=ierr)
                 else
                   allocate(darray(ndimtemp),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ndimtemp
               else if(iallocr.lt.nlist)then
                 if(iprec.eq.1)then
                   deallocate(rarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(rarray(ndimtemp),stat=ierr)
                 else
                   deallocate(darray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(darray(ndimtemp),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ndimtemp
               end if
               if(ialloci.eq.0)then
                 allocate(iarray(ndimtemp),stat=ierr)
                 if(ierr.ne.0) go to 9200
                 ialloci=ndimtemp
               else
                 if(ialloci.lt.nlist)then
                   deallocate(iarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(iarray(ndimtemp),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=ndimtemp
                 end if
               end if
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) ((iarray(ilist),rarray(ilist)),ilist=1,nlist)
               else
                 read(inunit,err=9100,end=9100) ((iarray(ilist),darray(ilist)),ilist=1,nlist)
               end if
               do ilist=1,nlist
                 icell=iarray(ilist)
                 if((icell.le.0).or.(icell.gt.ncell)) go to 9300
                 iz=cell2zone(icell)
                 if(iz.ne.0)then
                   if(iprec.eq.1)then
                     simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(ilist)
                   else
                     simflow(nproctime,iz)=simflow(nproctime,iz)+darray(ilist)
                   end if
                 end if
               end do
             end if
           else if(imeth.eq.3)then   ! There may be problems with this if cell numbers
                                     ! are different in different layers for an unstructured
                                     ! mfusg grid.
             numread=ndim1*ndim2
             if(iflag.eq.0)then
               read(inunit,err=9100,end=9100) (itemp,icell=1,numread)
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) (rtemp,icell=1,numread)
               else
                 read(inunit,err=9100,end=9100) (dtemp,icell=1,numread)
               end if
             else
               ndimtemp=max(ncell,numread)
               if(iallocr.eq.0)then
                 if(iprec.eq.1)then
                   allocate(rarray(ndimtemp),stat=ierr)
                 else
                   allocate(darray(ndimtemp),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ndimtemp
               else
                 if(iallocr.lt.numread)then
                   if(iprec.eq.1)then
                     deallocate(rarray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(rarray(ndimtemp),stat=ierr)
                   else
                     deallocate(darray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(darray(ndimtemp),stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=ndimtemp
                 end if
               end if
               if(ialloci.eq.0)then
                 allocate(iarray(ndimtemp),stat=ierr)
                 if(ierr.ne.0) go to 9200
                 ialloci=ndimtemp
               else
                 if(ialloci.lt.numread)then
                   deallocate(iarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(iarray(ndimtemp),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=ndimtemp
                 end if
               end if
               read(inunit,err=9100,end=9100) (iarray,icell=1,numread)
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) (rarray,icell=1,numread)
               else
                 read(inunit,err=9000,end=9000) (darray,icell=1,numread)
               end if
               if(iprec.eq.1)then
                 do icell=1,numread
                   if(rarray(icell).ne.0.0d0)then
                     if(isim.eq.22)then
                       jcell=iarray(icell)
                     else
                       ilay=iarray(icell)
                       jcell=icell+(ilay-1)*numread
                     end if
                     if((jcell.lt.0).or.(jcell.gt.ncell))then
                       call utl_num2char(jcell,atemp20)
                       write(amessage,250) trim(atemp20),trim(afile1)
                       go to 9890
                     end if
                     iz=cell2zone(jcell)
                     if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(icell)
                   end if
                 end do
               else
                 do icell=1,numread
                   if(darray(icell).ne.0.0d0)then
                     if(isim.eq.22)then
                       jcell=iarray(icell)
                     else
                       ilay=iarray(icell)
                       jcell=icell+(ilay-1)*numread
                     end if
                     if((jcell.lt.0).or.(jcell.gt.ncell))then
                       call utl_num2char(jcell,atemp20)
                       write(amessage,250) trim(atemp20),trim(afile1)
250                    format('Out of range cell number ',a,' enountered when reading flow ', &
                       'terms from file ',a,' using IMETH=3 method.')
                       go to 9890
                     end if
                     iz=cell2zone(jcell)
                     if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+darray(icell)
                   end if
                 end do
               end if
             end if
           else if(imeth.eq.4)then
             numread=ndim1*ndim2
             if(iflag.eq.0)then
               if(iprec.eq.1)then
                 read(inunit,err=9000,end=9000) (rtemp,icell=1,numread)
               else
                 read(inunit,err=9000,end=9000) (dtemp,icell=1,numread)
               end if
             else
               ndimtemp=max(numread,ncell)
               if(iallocr.eq.0)then
                 if(iprec.eq.1)then
                   allocate(rarray(ndimtemp),stat=ierr)
                 else
                   allocate(darray(ndimtemp),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ndimtemp
               else
                 if(iallocr.lt.numread)then
                   if(iprec.eq.1)then
                     deallocate(rarray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(rarray(ndimtemp),stat=ierr)
                   else
                     deallocate(darray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(darray(ndimtemp),stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=ndimtemp
                 end if
               end if
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) (rarray,icell=1,numread)
               else
                 read(inunit,err=9000,end=9000) (darray,icell=1,numread)
               end if
               if(iprec.eq.1)then
                 do icell=1,numread
                   if(rarray(icell).ne.0.0d0)then
                     iz=cell2zone(icell)
                     if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(icell)
                   end if
                 end do
               else
                 do icell=1,numread
                   if(darray(icell).ne.0.0d0)then
                     iz=cell2zone(icell)
                     if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+darray(icell)
                   end if
                 end do
               end if
             end if
           else if(imeth.eq.5)then
             read(inunit,err=9100,end=9100) ndat
             if(ndat.lt.-1) go to 9000
             if(ndat.gt.ibig) go to 9000
             if(ndat.gt.1)then
               read(inunit,err=9100,end=9100) (atemp16,idat=1,ndat-1)
             end if
             read(inunit,err=9100,end=9100) nlist
             if(nlist.lt.0) go to 9000
             if(nlist.gt.ibig) go to 9000
             if(iflag.eq.0)then
               if((nlist.gt.0).and.(ndat.gt.0))then
                 if(iprec.eq.1)then
                   read(inunit,err=9400,end=9400)((itemp,(rtemp,idat=1,ndat)),ilist=1,nlist)
                 else
                   read(inunit,err=9400,end=9400)((itemp,(dtemp,idat=1,ndat)),ilist=1,nlist)
                 end if
               endif
             else
               ndimtemp=max(nlist,ncell)
               if(iallocr.eq.0)then
                 if(iprec.eq.1)then
                   allocate(rarray(ndimtemp),stat=ierr)
                 else
                   allocate(darray(ndimtemp),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ndimtemp
               else
                 if(iallocr.lt.nlist)then
                   if(iprec.eq.1)then
                     deallocate(rarray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(rarray(ndimtemp),stat=ierr)
                   else
                     deallocate(darray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(darray(ndimtemp),stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=ndimtemp
                 end if
               end if
               if(ialloci.eq.0)then
                 allocate(iarray(ndimtemp),stat=ierr)
                 if(ierr.ne.0) go to 9200
                 ialloci=ndimtemp
               else
                 if(ialloci.lt.nlist)then
                   deallocate(iarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(iarray(ndimtemp),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=ndimtemp
                 end if
               end if
               if(iprec.eq.1)then
                 read(inunit,err=9400,end=9400)    &
                 ((iarray(ilist),rarray(ilist),(rtemp,idat=1,ndat-1)),ilist=1,nlist)
               else
                 read(inunit,err=9400,end=9400)    &
                 ((iarray(ilist),darray(ilist),(dtemp,idat=1,ndat-1)),ilist=1,nlist)
               end if
             end if
             if(iprec.eq.1)then
               do ilist=1,nlist
                 if(rarray(ilist).ne.0.0d0)then
                   icell=iarray(ilist)
                   if((icell.lt.0).or.(icell.gt.ncell)) go to 9450
                   iz=cell2zone(icell)
                   if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(ilist)
                 end if
               end do
             else
               do ilist=1,nlist
                 if(darray(ilist).ne.0.0d0)then
                   icell=iarray(ilist)
                   if((icell.lt.0).or.(icell.gt.ncell)) go to 9450
                   iz=cell2zone(icell)
                   if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+darray(ilist)
                 end if
               end do
             end if
           else if(imeth.eq.6)then
             if(iprec.eq.1)then
               write(amessage,270) trim(afile1),trim(function_name)
270            format('Reading of data from file ',a,' requires use of the IMETH=6 protocol. This ',   &
               'means that it is a MODFLOW 6 file. MODFLOW 6 budget files are always double ',         &
               'precision. However the IPREC argument in function ',a,' has been set to 1.')
               go to 9890
             end if
             read(inunit,err=9100,end=9100) ndat
             if(ndat.gt.ibig) go to 9000
             if(ndat.lt.0) go to 9000
             if(ndat-1.ge.1)then
               read(inunit,err=9100,end=9100) (atemp16,idat=1,ndat-1)
             end if
             read(inunit,err=9100,end=9100) nlist
             if(nlist.lt.0) go to 9000
             if(nlist.gt.ibig) go to 9000
             if(nlist.gt.0)then
               if(iflag.eq.0)then
                 if(iprec.eq.1)then
                   read(inunit,err=9100,end=9100) ((itemp,itemp,(rtemp,idat=1,ndat)),ilist=1,nlist)
                 else
                   read(inunit,err=9100,end=9100) ((itemp,itemp,(dtemp,idat=1,ndat)),ilist=1,nlist)
                 end if
               else
                 ndimtemp=max(nlist,ncell)
                 if(iallocr.eq.0)then
                   if(iprec.eq.1)then
                     allocate(rarray(ndimtemp),stat=ierr)
                   else
                     allocate(darray(ndimtemp),stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=ndimtemp
                 else
                   if(iallocr.lt.nlist)then
                     if(iprec.eq.1)then
                       deallocate(rarray,stat=ierr)
                       if(ierr.ne.0) go to 9200
                       allocate(rarray(ndimtemp),stat=ierr)
                     else
                       deallocate(darray,stat=ierr)
                       if(ierr.ne.0) go to 9200
                       allocate(darray(ndimtemp),stat=ierr)
                     end if
                     if(ierr.ne.0) go to 9200
                     iallocr=ndimtemp
                   end if
                 end if
                 if(ialloci.eq.0)then
                   allocate(iarray(ndimtemp),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=ndimtemp
                 else
                   if(ialloci.lt.nlist)then
                     deallocate(iarray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(iarray(ndimtemp),stat=ierr)
                     if(ierr.ne.0) go to 9200
                     ialloci=ndimtemp
                   end if
                 end if
                 if(iprec.eq.1)then
                   read(inunit,err=9500,end=9500)    &
                   ((iarray(ilist),itemp,rarray(ilist),(rtemp,idat=1,ndat-1)),ilist=1,nlist)
                 else
                   read(inunit,err=9500,end=9500)    &
                   ((iarray(ilist),itemp,darray(ilist),(dtemp,idat=1,ndat-1)),ilist=1,nlist)
                 end if
                 if(iprec.eq.1)then
                   do ilist=1,nlist
                     if(rarray(ilist).ne.0.0d0)then
                       icell=iarray(ilist)
                       if((icell.lt.1).or.(icell.gt.ncell)) go to 9550
                       iz=cell2zone(icell)
                       if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(ilist)
                     end if
                   end do
                 else
                   do ilist=1,nlist
                     if(darray(ilist).ne.0.0d0)then
                       icell=iarray(ilist)
                       if((icell.lt.1).or.(icell.gt.ncell)) go to 9550
                       iz=cell2zone(icell)
                       if(iz.ne.0)simflow(nproctime,iz)=simflow(nproctime,iz)+darray(ilist)
                     end if
                   end do
                 end if
               end if
             end if
           end if
         end if
       end do

1000   continue

       go to 9900

9000   continue
       write(amessage,9010) trim(afile1)
9010   format('Some of the numbers/text read from file ',a,' do not make sense. ',   &
       'Has the precision-type of this file been supplied correctly?')
       go to 9890

9050   write(amessage,9060) trim(function_name)
9060   format('Function ',a,' has been asked to read an array whose dimensions ', &
       'differ from input argument NCELL. This can mean that the model does not ', &
       'possess NCELL cells, or that inter-cell flows (e.g. FLOW-JA-FACE) terms ', &
       'are being sought. The latter is not allowed. The former comprises ',     &
       'an error condition.')
       go to 9890

9100   write(amessage,9110) trim(afile1)
9110   format('An error or premature end-of-file was encountered when reading ',  &
       'file ',a,'. Has the precision type of this file been supplied correctly?')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation/deallocate error condition endountered in function ',a,'.')
       go to 9890

9300   continue
       call utl_num2char(icell,atemp20)
       write(amessage,9310) trim(atemp20),trim(afile1)
9310   format('A cell index number of ',a,' was encountered in reading a ',     &
       'flow dataset from file ',a,' with an IMETH value of 2. This could be ', &
       'a model error. Alternatively it may indicate that you ',                &
       'need to call this function again with an altered IPREC precision setting.')
       go to 9890

9400   write(amessage,9410) trim(afile1)
9410   format('Error reading data from file ',a,' for IMETH=5 array reading option.')
       go to 9890

9450   call utl_num2char(icell,atemp20)
       write(amessage,9460) trim(atemp20)
9460   format('Illegal cell number ',a,' encountered when reading an array ',  &
       'with IMETH=5 array reading option.')
       go to 9890

9500   write(amessage,9510) trim(afile1)
9510   format('Error reading data from file ',a,' for IMETH=6 array reading option.')
       go to 9890

9550   call utl_num2char(icell,atemp20)
       write(amessage,9560) trim(atemp20)
9560   format('Illegal cell number ',a,' encountered when reading an array ',  &
       'with IMETH=6 array reading option.')
       go to 9890

9890   continue
       extract_flows_from_cbc_file=1
       nproctime=0
       timestep=0           ! an array
       stressperiod=0       ! an array
       simtime=0.0d0        ! an array
       simflow=0.0d0        ! an array
       numzone=0
       zonenumber=0         ! an array

9900   continue
       if(inunit.ne.0) close(unit=inunit,iostat=ierr)
       if(allocated(cell2zone)) deallocate(cell2zone,stat=ierr)
       if(allocated(iarray)) deallocate(iarray,stat=ierr)
       if(allocated(rarray)) deallocate(rarray,stat=ierr)
       if(allocated(darray)) deallocate(darray,stat=ierr)

end function extract_flows_from_cbc_file



