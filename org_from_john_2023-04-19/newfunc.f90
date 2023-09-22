integer function extract_flows_from_cbc_file(             &
                 cbcfile,flowtype,                        &
                 ncell,iprec,                             &
                 ntime,nzone,                             &
                 izone,                                   &
                 numzone,zonenumber,                      &
                 zonenum,proctime,                        &
                 timestep,stressperiod,                   &
                 simtime,simbud)

! -- This function reads and accumulates flows (as read from a cell-by-cell flow term file)
!    to a user-specified boundary condition.

       use dimvar
       use utilities
       implicit none

       character (len=1), intent(in)  :: cbcfile(LENGRIDNAME)       ! cell-by-cell flow term file written by any MF version.
       character (len=1), intent(in)  :: flowtype(17)               ! type of flow to read.
       integer, intent(in)            :: ncell                      ! number of cells in the model
       integer, intent(in)            :: iprec                      ! precision used to record real variables in cbc file
       integer, intent(in)            :: ntime                      ! equals or exceed number of model output times for flow type
       integer, intent(in)            :: nzone                      ! equals or exceeds number of zones; zone 0 doesn't count
                                                                    ! second dimension of simbud.
       integer, intent(in)            :: izone(ncell)               ! zonation of model domain
       integer, intent(out)           :: numzone                    ! number of non-zero-valued zones
       integer, intent(out)           :: zonenumber(nzone)          ! zone numbers (processed from izone)
       integer, intent(out)           :: nproctime                  ! number of processed simulation times
       integer, intent(out)           :: timestep(ntime)            ! simulation time step
       integer, intent(out)           :: stressperiod(ntime)        ! simulation stress period
       double precision, intent(out)  :: simtime(ntime)             ! simulation time; a time of -1.0 indicates unknown
       double precision, intent(out)  :: simflow(ntime,nzone)       ! interpolated flows

! -- Note that for older versions of MODFLOW the simulation time may not be recorded.
!    This is why ISTP and IPER are also reported.

       integer                        :: inunit,ibig,itemp,iflag
       integer                        :: ialloci,iallocr
       integer                        :: icell,iz
       integer                        :: kstp,kper,ndim1,ndim2,ndim3,imeth
       integer                        :: ilist,nlist,idat,ndat,numreal,ilay,jcell
       real                           :: rbig,delt,pertim,totim
       real                           :: rtemp
       double precision               :: dbig,ddelt,dpertim,dtotim
       double precision               :: dtemp

       character (len=16)             :: atext,text
       character (len=16)             :: txt1id1,txt2id1,txt1id2,txt2id2,atemp16
       character (len=256)            :: infile

! -- Automatic arrays

       integer                        :: cell2zone(ncell)

! -- Allocatable arrays

       integer, allocatable           :: iarray(:)
       real, allocatable              :: rarray(:)
       double precision, allocatable  :: darray(:)

! -- Initialisation

       extract_flows_from_cbc_file=0
       function_name='extract_flows_from_cbc_file()'
       inunit=0
       istp=0           ! an array
       iper=0           ! an array
       simtime=0.0d0
       simbud=0.0       ! an array
       zonenumber=0     ! an array
       ibig=huge(ibig)/2                   ! arbitrary
       rbig=huge(rbig)*0.5                 ! arbitrary
       dbig=huge(dbig)*0.5d0               ! arbitrary
       ialloci=0
       iallocr=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENGRIDNAME,cbcfile,infile)
       infile=adjustl(infile)
       call utl_string2char(17,flowtype,atext)
       atext=adjustl(atext)
       call utl_casetrans(atext,'lo')

! -- Check input arguments.

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
       if(infile.eq.' ')then
         write(amessage,120) 'CBCFILE',trim(function_name)
120      format('The ',a,' argument of function ',a,' must not be blank.')
         go to 9890
       end if

! -- Open the dependent variable file.

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
             itemp=call utl_whichone(numzone,iz,zonenumber,izone(icell))
             if(itemp.ne.0)then
               numzone=numzone+1
               if(numzone.gt.nzone)then
                 write(amessage,180)
180              format('The number of different non-zero integers that are featured ',  &
                 'in the IZONE input array exceeds the user-supplied value for NZONE.')
                 go to 9890
               end if
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
           itemp=call utl_whichone(numzone,iz,zonenumber,izone(icell))
           if(itemp.eq.0)then
             write(amessage,210) trim(function_name)
210          format('Programming error encountered in function ',a,': - contact programmer.')
             go to 9890
           end if
           cell2zone(icell)=iz
         end if
       end do

! -- Progress through the budget file.

       do
         read(inunit,err=9000,end=1000) kstp,kper,text,ndim1,ndim2,ndim3
         if((kstp.lt.0).or.(kper.lt.0).or.(ndim1.lt.0).or.(ndim2.lt.0)) go to 9000
         if(utl_textcheck(text).ne.0) go to 9000
         if((kstp.gt.ibig).or.(kper.gt.ibig).or.(ndim1.gt.ibig).or.(ndim2.gt.ibig)     &
                          .or.(abs(ndim3).gt.ibig))go to 9000
         call utl_casetrans(text,'lo')
         text=adjustl(text)
         iflag=0
         if(index(text,trim(atext)).eq.0)then
           if(nproctime+1.gt.ntime) go to 9900
           iflag=1
         end if
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
                 if(prec.eq.1)then
                   simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(cell)
                 else
                   simflow(nproctime,iz)=simflow(nproctime,iz)+darray(cell)
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
           if(ibig.gt.6)then
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
                 iallocr=1
                 if(iprec.eq.1)then
                   allocate(rarray(ncell),stat=ierr)
                 else
                   allocate(darray(ncell),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ncell
               else
                 if(allocr.lt.ncell)then
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
               nproctime=nproctime+1
               timestep(nproctime)=kstp
               stressperiod(nproctime)=kper
               simtime(nproctime)=dtotim
               do iz=1,numzone
                 simflow(nproctime,iz)=0.0d0
               end do
               if(iprec.eq.1)then
                 read(inunit,err=9100,end=9100) (rarray(icell),icell=1,ncell)
               else
                 read(inunit,err=9100,end=9100) (darray(icell),icell=1,ncell)
               end if
               do icell=1,ncell
                 iz=cell2zone(icell)
                 if(iz.ne.0)then
                   if(prec.eq.1)then
                     simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(cell)
                   else
                     simflow(nproctime,iz)=simflow(nproctime,iz)+darray(cell)
                   end if
                 end if
               end do
             end if
           else if(imeth.eq.2)then
             read(inunit,err=9000,end=9000) nlist
             if(nlist.lt.0) go to 9000
             if(nlist.gt.ibig) go to 9000
             if(iflag.eq.0)then
               if(iprec.eq.1)then
                 read(inunit,err=9000,end=9000) ((itemp,rtemp),ilist=1,nlist)
               else
                 read(inunit,err=9000,end=9000) ((itemp,dtemp),ilist=1,nlist)
               end if
             else
               nproctime=nproctime+1
               timestep(nproctime)=kstp
               stressperiod(nproctime)=kper
               simtime(nproctime)=dtotim
               if(iallocr.eq.0)then
                 if(iprec.eq.1)then
                   allocate(rarray(ncell),stat=ierr)
                 else
                   allocate(darray(ncell),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=ncell
               else if(allocr.lt.nlist)then
                 if(iprec.eq.1)then
                   deallocate(rarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(rarray(max(nlist,ncell)),stat=ierr)
                 else
                   deallocate(darray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(darray(max(nlist,ncell)),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=max(nlist,ncell)
               end if
               if(ialloci.eq.0)then
                 allocate(iarray(max(nlist,ncell)),stat=ierr)
                 if(ierr.ne.0) go to 9200
                 ialloci=max(nlist,ncell)
               else
                 if(alloci.lt.nlist)then
                   deallocate(rarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(iarray(max(nlist,ncell)),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=max(nlist,ncell)
                 end if
               end if
               if(iprec.eq.1)then
                 read(inunit,err=9000,end=9000) ((iarray(ilist),rarray(ilist)),ilist=1,nlist)
               else
                 read(inunit,err=9000,end=9000) ((iarray(ilist),darray(ilist)),ilist=1,nlist)
               end if
               do ilist=1,nlist
                 icell=iarray(ilist)
                 if((icell.le.0).or.(icell.gt.ncell)) go to 9300
                 iz=cell2zone(icell)
                 if(iz.ne.0)then
                   if(prec.eq.1)then
                     simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(ilist)
                   else
                     simflow(nproctime,iz)=simflow(nproctime,iz)+darray(ilist)
                   end if
                 end if
               end do
           else if(imeth.eq.3)then   ! There may be problems with this if cell numbers
                                     ! are different in different layers for an unstructured
                                     ! mfusg grid.
             numread=ndim1*ndim2
             if(iflag.eq.0)then
               read(inunit,err=9000,end=9000) (itemp,icell=1,numread)
               if(iprec.eq.1)then
                 read(inunit,err=9000,end=9000) (rtemp,icell=1,numread)
               else
                 read(inunit,err=9000,end=9000) (dtemp,icell=1,numread)
               end if
             else
               nproctime=nproctime+1
               timestep(nproctime)=kstp
               stressperiod(nproctime)=kper
               simflow(nproctime)=dtotim
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
               if(ialloci.eq.0)then
                 allocate(iarray(ncell),stat=ierr)
                 if(ierr.ne.0) go to 9200
                 ialloci=ncell
               else
                 if(ialloci.lt.ncell)then
                   deallocate(iarray,stat=ierr)
                   if(ierr.ne.0) go to 9200
                   allocate(iarray(ncell),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=ncell
                 end if
               end if
               read(inunit,err=9000,end=9000) (iarray,icell=1,numread)
               if(iprec.eq.1)then
                 read(inunit,err=9000,end=9000) (rarray,icell=1,numread)
                 darray=rarray       ! arrays
               else
                 read(inunit,err=9000,end=9000) (darray,icell=1,numread)
               end if
               do icell=1,numread
                 if(darray(icell).ne.0.0d0)then
                   ilay=iarray(icell)
                   jcell=icell+(ilay-1)*numread
                   if((jcell.lt.0).or.(jcell.gt.ncell))then
                     call utl_num2char(jcell,atemp20)
                     write(amessage,250) trim(atemp20),trim(afile1)
250                  format('Out of range cell number ',a,' enountered when reading flow ', &
                     'terms from file ',a,' using IMETH=3 method.')
                     go to 9890
                   end if
                   iz=cell2zone(jcell)
                   simflow(nproctime,iz)=simflow(nproctime,iz)+darray(icell)
                 end if
               end do
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
                     allocate(rarray,stat=ierr)
                   else
                     deallocate(darray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(darray,stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=ncell
                 end if
               end if
               if(iprec.eq.1)then
                 read(inunit,err=9000,end=9000) (rarray,icell=1,numread)
                 darray=rarray       ! arrays
               else
                 read(inunit,err=9000,end=9000) (darray,icell=1,numread)
               end if
               do icell=1,numread
                 if(darray(icell).ne.0.0d0)then
                   iz=cell2zone(icell)
                   simflow(nproctime,iz)=simflow(nproctime,iz)+darray(icell)
                 end if
               end do
             end if
           else if(imeth.eq.5)then
             read(inunit,err=9000,end=9000) ndat
             if(ndat.lt.-1) go to 9000
             if(ndat.gt.ibig) go to 9000
             if(ndat.gt.1)then
               read(inunit,err=9000,end=9000) (atemp16,idat=1,ndat-1)
             end if
             read(inunit,err=9000,end=9000) nlist
             if(nlist.lt.0) go to 9000
             if(nlist.gt.ibig) go to 9000
             if(iflag.eq.0)then
             if((nlist.gt.0).and.(ndat.gt.0))then
               if(iprec.eq.1)then
                 read(inunit,err=9000,end=9000)((itemp,(rtemp,idat=1,ndat)),ilist=1,nlist)
               else
                 read(inunit,err=9000,end=9000)((itemp,(dtemp,idat=1,ndat)),ilist=1,nlist)
               end if
             else
               itemp=max(nlist,ncell)
               if(iallocr.eq.0)then
                 if(iprec.eq.1)then
                   allocate(rarray(itemp),stat=ierr)
                 else
                   allocate(darray(itemp),stat=ierr)
                 end if
                 if(ierr.ne.0) go to 9200
                 iallocr=itemp
               else
                 if(iallocr.lt.nlist)then
                   if(iprec.eq.1)then
                     deallocate(rarray,stat=ierr)
                     allocate(rarray(itemp),stat=ierr)
                   else
                     deallocate(darray,stat=ierr)
                     allocate(darray(itemp),stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=itemp
                 end if
               end if
               if(ialloci.eq.0)then
                 allocate(iarray(itemp),stat=ierr)
                 if(ierr.ne.0) go to 9200
                 ialloci=itemp
               else
                 if(ialloci.lt.nlist)then
                   deallocate(iarray,stat=ierr)
                   allocate(iarray(itemp),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=itemp
                 end if
               end if
               if(iprec.eq.1)then
                 read(inunit,,err=9400,end=9400)    &
                 ((iarray(ilist),rarray(ilist),(rtemp,idat=1,ndat-1)),ilist=1,nlist)
               else
                 read(inunit,,err=9400,end=9400)    &
                 ((iarray(ilist),darray(ilist),(rtemp,idat=1,ndat-1)),ilist=1,nlist)
               end if
             end if
             if(iprec.eq.0)then
               do ilist=1,nlist
                 if(rarray(ilist).ne.0.0d0)then
                   iz=cell2zone(iarray(ilist))
                   simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(ilist)
                 end if
               end do
             else
               do ilist=1,nlist
                 if(darray(ilist).ne.0.0d0)then
                   iz=cell2zone(iarray(ilist))
                   simflow(nproctime,iz)=simflow(nproctime,iz)+darray(ilist)
                 end if
               end do
             end if
           else if(imeth.eq.6)then
             if(iprec.eq.1)then
               write(amessage,270) trim(afile1),trim(function_name)
270            format('A section of file ',a,' requires reading with IMETH=6 protocol. This ', &
               'means that it is a MODFLOW 6 file. MODFLOW 6 budget files are always double ',      &
               'precision. However the IPREC argument in function ',a,' has been set to 1.')
               go to 9890
             end if
             read(inunit,err=9000,end=9000) ndat
             if(ndat.gt.ibig) go to 9000
             if(ndat.lt.0) go to 9000
             if(ndat-1.ge.1)then
               read(inunit,err=9000,end=9000) (atemp16,idat=1,ndat-1)
             end if
             read(inunit,err=9000,end=9000) nlist
             if(nlist.lt.0) go to 9000
             if(nlist.gt.ibig) go to 9000
             if(nlist.gt.0)then
               if(iflag.eq.0)then
                 if(iprec.eq.1)then
                   read(inunit,err=9000,end=9000) ((itemp,itemp,(rtemp,idat=1,ndat)),ilist=1,nlist)
                 else
                   read(inunit,err=9000,end=9000) ((itemp,itemp,(dtemp,idat=1,ndat)),ilist=1,nlist)
                 end if
               else
                 itemp=max(nlist,ncell)
                 if(iallocr.eq.0)then
                   if(iprec.eq.1)then
                     allocate(rarray(itemp),stat=ierr)
                   else
                     allocate(darray(itemp),stat=ierr)
                   end if
                   if(ierr.ne.0) go to 9200
                   iallocr=itemp
                 else
                   if(iallocr.lt.nlist)then
                     if(iprec.eq.1)then
                       deallocate(rarray,stat=ierr)
                       if(ierr.ne.0) go to 9200
                       allocate(rarray(itemp),stat=ierr)
                     else
                       deallocate(darray,stat=ierr)
                       if(ierr.ne.0) go to 9200
                       allocate(darray(itemp),stat=ierr)
                     end if
                     if(ierr.ne.0) go to 9200
                     iallocr=itemp
                   end if
                 end if
                 if(ialloci.eq.0)then
                   allocate(iarray(itemp),stat=ierr)
                   if(ierr.ne.0) go to 9200
                   ialloci=itemp
                 else
                   if(ialloci.lt.nlist)then
                     deallocate(iarray,stat=ierr)
                     if(ierr.ne.0) go to 9200
                     allocate(iarray(itemp),stat=ierr)
                     if(ierr.ne.0) go to 9200
                     ialloci=itemp
                   end if
                 end if
                 if(iprec.eq.1)then
                   read(inunit,,err=9500,end=9500)    &
                   ((iarray(ilist),itemp,rarray(ilist),(rtemp,idat=1,ndat-1)),ilist=1,nlist)
                 else
                   read(inunit,,err=9500,end=9500)    &
                   ((iarray(icell),itemp,darray(ilist),(dtemp,idat=1,ndat-1)),ilist=1,nlist)
                 end if
               end if
               if(iprec.eq.0)then
                 do ilist=1,nlist
                   if(rarray(list).ne.0.0d0)then
                     iz=cell2array(iarray(ilist))
                     simflow(nproctime,iz)=simflow(nproctime,iz)+rarray(ilist)
                   end if
                 end do
               else
                 do ilist=1,nlist
                   if(darray(ilist).ne.0.0d0)then
                     iz=cell2array(iarray(ilist))
                     simflow(nproctime,iz)=simflow(nproctime,iz)+darray(ilist)
                   end if
                 end do
               end if
             end if
           end if
         end if
       end do

1000   continue

       go to 9900

9000   continue
       write(amessage,9010) trim(afile1)
9010   format('Some of the numbers/text read from file do not make sense. ',   &
       'Has the precision-type of this file been supplied correctly?')
       go to 9890

9050   write(amessage,9060) trim(function_name)
9060   format('Function ',a,' has been asked to read an array whose dimensions ', &
       'differ from input argument NCELL. This can mean that the model does not ', &
       'possess NCELL cells, or that inter-cell-flows (e.g. FLOW-JA-FACE) terms ', &
       'are being sought. The latter is not allowed. The former comprises ',     &
       'an error condition.')
       go to 9890

9100   write(amessage,9110) trim(afile1)
9110   format('An error or premature end-of-file was encountered when reading ',  &
       'file ',a,'. Has the precision type of this file been supplied correctly?')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation/deallocate error condition in function ',a,'.')
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

9500   write(amessage,9510) trim(afile1)
9510   format('Error reading data from file ',a,' for IMETH=6 array reading option.')
       go to 9890

9890   continue
       extract_flows_from_cbc_file=1
       nproctime=0
       timestep=0           ! an array
       stressperiod=0       ! an array
       simtime=0.0d0        ! an array
       simflow=0.0d0        ! an array
       zonenumber=0         ! an array

9900   continue
       if(inunit.ne.0) close(unit=inunit,iostat=ierr)
       if(allocated(iarray)) deallocate(iarray,stat=ierr)
       if(allocated(rarray)) deallocate(rarray,stat=ierr)
       if(allocated(darray)) deallocate(darray,stat=ierr)

end function extract_flows_from_cbc_file




