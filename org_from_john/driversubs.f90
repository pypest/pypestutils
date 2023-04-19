       subroutine char2string(ndim,textchar,textstring)

! -- Converts a FORTRAN character variable to a C type string.

       implicit none
       integer, intent(in)            :: ndim
       character (len=*), intent(in)  :: textchar
       character (len=1), intent(out) :: textstring(ndim)

       integer    :: ns,is

       ns=min(len_trim(textchar),ndim-1)
       do is=1,ns
         textstring(is)=textchar(is:is)
       end do
       textstring(ns+1)=achar(0)
       return

       end



       subroutine string2char(ndim,textstring,textchar)

! -- Converts a C type character string to a FORTRAN character variable.

       implicit none
       integer, intent(in)            :: ndim
       character (len=1), intent(in)  :: textstring(ndim)
       character (len=*), intent(out) :: textchar

       integer       :: ns,is

       ns=len(textchar)
       textchar=' '
       do is=1,min(ns,ndim)
         if(textstring(is).eq.char(0))exit
         textchar(is:is)=textstring(is)
       end do
       return

       end

        subroutine getfile(ifail,cline,filename,ibeg,iend)

!-- Subroutine getfile extracts a filename from a string.

! -- Arguments are as follows:-
!       ifail: returned as zero if filename successfully read
!       cline: a character string containing the file name
!       filename: the name of the file read from the string
!       ibeg: character position at which to begin search for filename
!       iend: on input  - character position at which to end search for filename
!             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
          do j=i+1,iend
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.(cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
        return

        end


      subroutine writmess(iunit,amessage)

        implicit none

        integer iunit,jend,i,nblc,junit,leadblank,itake,j
        character*(*) amessage
        character (len=20) ablank

        ablank=' '
        itake=0
        j=0
        junit=iunit

        if(amessage.eq.' ')then
          write(junit,*)
          return
        end if
        write(junit,*)
        do i=1,min(20,len(amessage))
          if(amessage(i:i).ne.' ')go to 21
20      end do
21      leadblank=i-1
        nblc=len_trim(amessage)
5       jend=j+78-itake
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
        if(amessage(i:i).eq.' ') then
          if(itake.eq.0) then
             write(junit,'(a)') amessage(j+1:i)
             itake=2+leadblank
          else
             write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:i)
          end if
          j=i
          go to 5
        end if
        end do
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
          itake=2+leadblank
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        j=jend
        go to 5
100     jend=nblc
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        return

      end


        SUBROUTINE LOWCASE(ASTRNG)

! -- Subroutine LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END


        subroutine writint(atemp,ival)

!       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
        end



SUBROUTINE intREAD(IFAIL,CLINE,iTEMP)

! -- Subroutine intREAD reads a real number from a string.

        INTEGER IFAIL
        integer iTEMP
        CHARACTER*6 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(i   )'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) iTEMP
        RETURN

100     IFAIL=1
        RETURN

END SUBROUTINE INTREAD

