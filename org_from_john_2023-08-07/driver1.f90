       program driver1

! -- This program is used as a driver for functions which read and report the contents of
!    MODFLOW binary output files.

       use function_interfaces
       use iso_c_binding, only : c_int,c_char
       implicit none

       integer (kind=c_int)  :: isim,itype,iprec
       integer (kind=c_int)  :: narray,ntime,ifail
       integer               :: ibeg,iend
       character (kind=c_char,len=1)     :: infilestring(256),outfilestring(256)
       character (kind=c_char,len=1)     :: messagestring(1500)
       character (len=30)                :: atype,aprec
       character (len=256)               :: infile,afile,outfile
       character (len=1500)              :: amessage

! -- Record what it does.

       write(6,*)
       write(6,20)
20     format(' Driver for function inquire_modflow_binary_file_specs()')
       write(6,*)

! -- Initialization

       amessage=' '

50     write(6,60)
60     format(' A simulation code is required.')
       write(6,70)
70     format(' (1=mf; 21=mfusg_s; 22=mfusg_us; 31=mf6_dis; 32=mfusg_disv; 33=mfusg_disu)')
79     write(6,80,advance='no')
80     format(' Enter simulation code: ')
       read(5,*,err=79) isim
       write(6,*)

! -- Acquire the name of the MODFLOW binary file that must be read.

100    write(6,110,advance='no')
110    format(' Enter name of MODFLOW binary output file (<Enter> if no more): ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 9990
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,infile,ibeg,iend)
       if(ifail.ne.0) go to 100
120    write(6,125,advance='no')
125    format(' Enter file type code (1=state; 2=flow): ')
       read(5,*,err=120) itype
!       if((itype.ne.1).and.(itype.ne.2)) go to 120

! -- Acquire the name of the file to which contents of this file must be recorded.

       write(6,*)
130    write(6,140,advance='no')
140    format(' Enter name for file details file (<Enter> if none): ')
       read(5,'(a)') afile
       if(afile.eq.' ')then
         outfile=' '
       else
         ibeg=1
         iend=len_trim(afile)
         call getfile(ifail,afile,outfile,ibeg,iend)
         if(ifail.ne.0) go to 130
       end if

! -- Convert to C-type character strings

       call char2string(256,infile,infilestring)
       call char2string(256,outfile,outfilestring)

! -- Call the function.

       write(6,*)
       write(6,150)
150    format(' Calling function inquire_modflow_binary_file_specs...')
       write(6,*)
       ifail=inquire_modflow_binary_file_specs(infilestring,outfilestring,  &
       isim,itype,iprec,narray,ntime)

! -- Report success or failure.

       write(6,160) ifail
160    format(' IFAIL = ',i1)
       if(ifail.ne.0)then
         ifail=retrieve_error_message(messagestring)
         call string2char(1500,messagestring,amessage)
         amessage=' '//trim(amessage)
         call writmess(6,amessage)
       else
         atype='xxxx'
         aprec='xxxx'
         if(itype.eq.1)then
           atype='dependent-variable'
         else if(itype.eq.2)then
           atype='cell-by-cell flow term'
         end if
         if(iprec.eq.1)then
           aprec='single'
         else if(iprec.eq.2)then
           aprec='double'
         endif
         write(6,170) trim(atype)
170      format(' FILE TYPE              = ',a)
         write(6,180) trim(aprec)
180      format(' PRECISION              = ',a)
         write(6,190) narray
190      format(' Number of arrays       =',i5)
         write(6,195) ntime
195      format(' Number of output times =',i5)

       end if
       write(6,*)
       go to 100

9990   end

