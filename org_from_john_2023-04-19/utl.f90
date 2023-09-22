module utilities

       use dimvar
       implicit none

public

! -- General reuseable variables

        integer                        :: lw(100),rw(100)
        double precision               :: pi=3.14159265358979
        character (len=LENFUNCNAME)    :: function_name
        character (len=LENFILENAME)    :: afile1,afile2
        character (len=LENCLINE)       :: cline
        character (len=LENMESSAGE)     :: amessage

! -- Geometry utilities

public   utl_locpt

! -- File utilities

public  utl_addquote,        &
        utl_MfBinOpSpecs,    &
        utl_nextunit

! -- Text utilities

public  utl_casetrans,       &
        utl_char2string,     &
        utl_num2char,        &
        utl_int2char,        &
        utl_real2char,       &
        utl_dbl2char,        &
        utl_char2num,        &
        utl_char2int,        &
        utl_char2real,       &
        utl_char2dbl,        &
        utl_string2char,     &
        utl_textcheck,       &
        utl_wordsplit

! -- Array utilities

public  utl_whichone,        &
        utl_sort

! -- Interfaces for generic functions.

interface utl_num2char
        module procedure utl_int2char
        module procedure utl_real2char
        module procedure utl_dbl2char
end interface

interface utl_char2num
        module procedure utl_char2int
        module procedure utl_char2real
        module procedure utl_char2dbl
end interface

interface utl_whichone
        module procedure utl_whichone_int
        module procedure utl_whichone_char
end interface

interface utl_sort
        module procedure utl_sort_int
        module procedure utl_sort_real
        module procedure utl_sort_dbl
end interface

contains


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                               ARRAY UTILITES                                     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine utl_sort_int(n,iarray)            ! Check this when a really low item occurs late in the array.

        integer, intent(in)     :: n
        integer, intent(inout)  :: iarray(n)

        integer                 :: iw,i,j

        if(n.eq.1) then
          return
        else if(n.eq.2)then
          if(iarray(2).lt.iarray(1))then
            iw=iarray(2)
            iarray(2)=iarray(1)
            iarray(1)=iw
          end if
        else
          do i = 2, n
            iw = iarray(i)
            do j =i,2,-1
              if(iw.ge.iarray(j-1)) go to 100
              iarray(j) = iarray(j-1)
            end do
            j=1
100         continue
            iarray(j) = iw
          end do
        end if

        return

end subroutine utl_sort_int



subroutine utl_sort_real(n,rarray)            ! Check this when a really low item occurs late in the array.

        integer, intent(in)     :: n
        real, intent(inout)     :: rarray(n)

        integer                 :: i,j
        real                    :: rw

        if(n.eq.1) then
          return
        else if(n.eq.2)then
          if(rarray(2).lt.rarray(1))then
            rw=rarray(2)
            rarray(2)=rarray(1)
            rarray(1)=rw
          end if
        else
          do i = 2, n
            rw = rarray(i)
            do j =i,2,-1
              if(rw.ge.rarray(j-1)) go to 100
              rarray(j) = rarray(j-1)
            end do
            j=1
100         continue
            rarray(j) = rw
          end do
        end if

        return

end subroutine utl_sort_real



subroutine utl_sort_dbl(n,rarray)            ! Check this when a really low item occurs late in the array.

        integer, intent(in)                 :: n
        double precision, intent(inout)     :: rarray(n)

        integer                             :: i,j
        double precision                    :: rw

        if(n.eq.1) then
          return
        else if(n.eq.2)then
          if(rarray(2).lt.rarray(1))then
            rw=rarray(2)
            rarray(2)=rarray(1)
            rarray(1)=rw
          end if
        else
          do i = 2, n
            rw = rarray(i)
            do j =i,2,-1
              if(rw.ge.rarray(j-1)) go to 100
              rarray(j) = rarray(j-1)
            end do
            j=1
100         continue
            rarray(j) = rw
          end do
        end if

        return

end subroutine utl_sort_dbl


integer function utl_whichone_char(npar,ipar,apar,tpar)

! -- Function UTL_WHICHONE_CHAR locates a string in an array of strings.

        implicit none
        integer, intent(in)               :: npar
        integer, intent(inout)            :: ipar
        character (len=*), intent(in)     :: apar(npar)
        character (len=*), intent(inout)  :: tpar

        integer i

        utl_whichone_char=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        call utl_casetrans(tpar,'lo')
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do i=ipar+1,npar
            if(tpar.eq.apar(i))then
              ipar=i
              return
            end if
          end do
        end if
        if(ipar.ne.1)then
          do i=ipar-1,1,-1
            if(tpar.eq.apar(i)) then
              ipar=i
              return
            end if
          end do
        end if
        utl_whichone_char=1
        return

end function utl_whichone_char



integer function utl_whichone_int(npar,ipar,apar,tpar)

! -- Function UTL_WHICHONE_INT locates an integer in an array of integers.

        implicit none
        integer, intent(in)           :: npar
        integer, intent(inout)        :: ipar
        integer, intent(in)           :: apar(npar)
        integer, intent(in)           :: tpar

        integer i

        utl_whichone_int=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do i=ipar+1,npar
            if(tpar.eq.apar(i))then
              ipar=i
              return
            end if
          end do
        end if
        if(ipar.ne.1)then
          do i=ipar-1,1,-1
            if(tpar.eq.apar(i)) then
              ipar=i
              return
            end if
          end do
        end if
        utl_whichone_int=1
        return

end function utl_whichone_int



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                             GEOMETRY UTILITES                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE UTL_LOCPT (X0, Y0, X, Y, N, L, M)
      DOUBLE PRECISION, intent(in) :: X(N), Y(N)
!-----------------------------------------------------------------------
!     GIVEN A POLYGONAL LINE CONNECTING THE VERTICES (X(I),Y(I))
!     (I = 1,...,N) TAKEN IN THIS ORDER. IT IS ASSUMED THAT THE
!     POLYGONAL PATH IS A LOOP, WHERE (X(N),Y(N)) = (X(1),Y(1))
!     OR THERE IS AN ARC FROM (X(N),Y(N)) TO (X(1),Y(1)).
!
!     (X0,Y0) IS AN ARBITRARY POINT AND L AND M ARE VARIABLES.
!     L AND M ARE ASSIGNED THE FOLLOWING VALUES ...
!
!        L = -1   IF (X0,Y0) IS OUTSIDE THE POLYGONAL PATH
!        L =  0   IF (X0,Y0) LIES ON THE POLYGONAL PATH
!        L =  1   IF (X0,Y0) IS INSIDE THE POLYGONAL PATH
!
!     M = 0 IF (X0,Y0) IS ON OR OUTSIDE THE PATH. IF (X0,Y0)
!     IS INSIDE THE PATH THEN M IS THE WINDING NUMBER OF THE
!     PATH AROUND THE POINT (X0,Y0).
!
!-----------------------
!
!     ****** EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE
!            SMALLEST NUMBER SUCH THAT 1.0 + EPS .GT. 1.0
!
!jed      EXTERNAL   DPMPAR

      INTEGER            N,L,M,N0,I
      DOUBLE PRECISION   ANGLE  ,DPMPAR ,EPS    ,PI     ,PI2    ,SUM
      DOUBLE PRECISION   THETA  ,THETAI ,THETA1 ,TOL    ,U      ,V
      DOUBLE PRECISION   X0     ,Y0
!jed                     EPS = DPMPAR(1)
                      eps=1.0d-10   ! a rough guess, sufficient for double prec
!
!-----------------------------------------------------------------------
      N0 = N
      IF (X(1) .EQ. X(N) .AND. Y(1) .EQ. Y(N)) N0 = N - 1
      PI = ATAN2(0.0D0, -1.0D0)
      PI2 = 2.0D0*PI
      TOL = 4.0D0*EPS*PI
      L = -1
      M = 0

      U = X(1) - X0
      V = Y(1) - Y0
      IF (U .EQ. 0.0D0 .AND. V .EQ. 0.0D0) GO TO 20
      IF (N0 .LT. 2) RETURN
      THETA1 = ATAN2(V, U)

      SUM = 0.0D0
      THETA = THETA1
      DO 10 I = 2,N0
         U = X(I) - X0
         V = Y(I) - Y0
         IF (U .EQ. 0.0D0 .AND. V .EQ. 0.0D0) GO TO 20
         THETAI = ATAN2(V, U)

         ANGLE = ABS(THETAI - THETA)
         IF (ABS(ANGLE - PI) .LT. TOL) GO TO 20
         IF (ANGLE .GT. PI) ANGLE = ANGLE - PI2
         IF (THETA .GT. THETAI) ANGLE = -ANGLE
         SUM = SUM + ANGLE
         THETA = THETAI
   10 CONTINUE

      ANGLE = ABS(THETA1 - THETA)
      IF (ABS(ANGLE - PI) .LT. TOL) GO TO 20
      IF (ANGLE .GT. PI) ANGLE = ANGLE - PI2
      IF (THETA .GT. THETA1) ANGLE = -ANGLE
      SUM = SUM + ANGLE

!     SUM = 2*PI*M WHERE M IS THE WINDING NUMBER

      M = ABS(SUM)/PI2 + 0.2D0
      IF (M .EQ. 0) RETURN
      L = 1
      IF (SUM .LT. 0.0D0) M = -M
      RETURN

!     (X0, Y0) IS ON THE BOUNDARY OF THE PATH

   20 L = 0
      RETURN
END SUBROUTINE UTL_LOCPT



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                               FILE UTILITES                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine utl_addquote(afile,aqfile)

! -- Subroutine UTL_ADDQUOTE adds quotes to a filename if it has a space in it.

        implicit none

        character (len=*), intent(in)   :: afile
        character (len=*), intent(out)  :: aqfile
        integer nbb

        if(index(trim(afile),' ').eq.0)then
          aqfile=afile
        else
          aqfile(1:1)='"'
          aqfile(2:)=trim(afile)
          nbb=len_trim(aqfile)+1
          aqfile(nbb:nbb)='"'
        end if

        return

end subroutine utl_addquote



integer function utl_MfBinOpSpecs(inunit,itype,infile,iprec)

! -- This function tries to establish the precision of a MODFLOW binary output file
!    by reading the first lineor two of this file.
! -- It probably works well in identifying the precision of a system state file. It may not
!    work so well in identifying the precision of a budget file. The calling program should
!    be aware of this.

       implicit none

       integer, intent(in)             :: inunit  ! The file is already open
       integer, intent(in)             :: itype   ! 1 = system states;  2 = cell-by-cell flow
       character (len=*), intent(in)   :: infile
       integer, intent(out)            :: iprec   ! 1 = single precision; 2 = double precision

       integer                         :: kstp,kper,ilay,int1,int2,int3
       integer                         :: ibig,imeth
       real                            :: rbig
       real                            :: pertim,totim,delt
       double precision                :: dbig
       double precision                :: dpertim,dtotim,ddelt
       character (len=16)              :: text

! -- Initialisation

       utl_mfbinopspecs=0
       call utl_addquote(infile,afile1)
       ibig=huge(ibig)/2                   ! arbitrary
       rbig=huge(rbig)*0.5                 ! arbitrary
       dbig=huge(dbig)*0.5d0               ! arbitrary
       iprec=0

! -- Try to establish file details by reading its first line. (The file is assumed to
!    be already open.)

       if(itype.eq.1)then
         read(inunit,err=9000,end=9000) kstp,kper,pertim,totim,text,int1,int2,ilay
         if((kstp.lt.0).or.(kper.lt.0).or.(pertim.lt.0.0).or.(totim.lt.0.0).or.         &
           (int1.lt.0).or.(int2.lt.0).or.(ilay.lt.0).or.                                &
           (kstp.gt.ibig).or.(kper.gt.ibig).or.(int1.gt.ibig).or.(int2.gt.ibig).or.     &
           (ilay.gt.ibig).or.(pertim.gt.rbig).or.(totim.gt.rbig).or.                  &
           (utl_textcheck(text).ne.0)) go to 100
         iprec=1
         go to 200
100      continue
         rewind(unit=inunit)
         read(inunit,err=9000,end=9000) kstp,kper,dpertim,dtotim,text,int1,int2,ilay
         if((kstp.lt.0).or.(kper.lt.0).or.(dpertim.lt.0.0d0).or.(dtotim.lt.0.0d0).or.      &
           (int1.lt.0).or.(int2.lt.0).or.(ilay.lt.0).or.                                   &
           (kstp.gt.ibig).or.(kper.gt.ibig).or.(int1.gt.ibig).or.(int2.gt.ibig).or.        &
           (ilay.gt.ibig).or.(dpertim.gt.dbig).or.(dtotim.gt.dbig).or.                     &
           (utl_textcheck(text).ne.0)) go to 9000
         iprec=2
         go to 200
       else if(itype.eq.2)then
         read(inunit,err=9000,end=9000) kstp,kper,text,int1,int2,int3
         if((int1.lt.0).or.(int2.lt.0).or.(int1.gt.ibig).or.(int2.gt.ibig).or.     &
         (abs(int3).gt.ibig).or.(utl_textcheck(text).ne.0)) go to 130
         if(int3.lt.0)then
           read(inunit,err=9000,end=9000) imeth,delt,pertim,totim
           if((imeth.lt.0).or.(imeth.gt.ibig)) go to 130
           if((delt.lt.0.0).or.(pertim.lt.0.0).or.(totim.lt.0.0)) go to 130
           if((delt.gt.rbig).or.(pertim.gt.rbig).or.(totim.gt.rbig)) go to 130
           if(totim.lt.pertim) go to 130
         end if
         iprec=1          ! we cannot be 100% sure
         go to 200
130      continue
         rewind(unit=inunit)
         read(inunit,err=9000,end=9000) kstp,kper,text,int1,int2,int3
         read(inunit,err=9000,end=9000) imeth,ddelt,dpertim,dtotim
         if((ddelt.lt.0.0d0).or.(dpertim.lt.0.0d0).or.(dtotim.lt.0.0d0)) go to 9000
         if((ddelt.gt.dbig).or.(dpertim.gt.dbig).or.(dtotim.gt.dbig)) go to 9000
         if(dtotim.lt.dpertim) go to 9000
         iprec=2          ! we cannot be 100% sure
         go to 200
       end if
200    continue
       go to 9900

9000   write(amessage,9010) trim(afile1)
9010   format('Unable to obtain specifications of file ',a,'.')
       go to 9890

9890   continue
       utl_mfbinopspecs=1

9900   continue
       return

end function utl_MfBinOpSpecs




integer function utl_nextunit()

! -- Function UTL_NEXTUNIT determines the lowest unit number available for
! -- opening.

        implicit none
        logical::lopen

        do utl_nextunit=10,200
          inquire(unit=utl_nextunit,opened=lopen)
          if(.not.lopen) return
        end do

        utl_nextunit=0

end function utl_nextunit


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                               TEXT UTILITES                                      !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine utl_casetrans(string,hi_or_lo)

! -- Subroutine UTL_CASETRANS converts a string to upper or lower case.

! -- Arguments are as follows:-
!      string:    contains the string whose case must be changed
!      hi_or_lo:  must be either 'lo' or 'hi' to indicate
!                 change of case direction.

        implicit none

        character (len=*), intent(inout)        :: string
        character (len=*), intent(in)           :: hi_or_lo
        character                               :: alo, ahi
        integer                                 :: inc,i

        if(hi_or_lo.eq.'lo') then
          alo='A'; ahi='Z'; inc=iachar('a')-iachar('A')
        else if(hi_or_lo.eq.'hi') then
          alo='a'; ahi='z'; inc=iachar('A')-iachar('a')
        else
          return
        endif

        do i=1,len_trim(string)
          if((string(i:i).ge.alo).and.(string(i:i).le.ahi)) &
          string(i:i)=achar(iachar(string(i:i))+inc)
        end do

        return

end subroutine utl_casetrans



subroutine utl_int2char(inum,anum)

! -- Subroutine UTL_INT2CHAR converts an integer to a string.

        implicit none
        integer, intent(in)             :: inum
        character (len=*), intent(out)  :: anum

        character (len=12)              :: afmt
        integer                         :: llen

        afmt='(i    )'
        llen=min(40,len(anum))
        write(afmt(3:6),'(i4)') llen
        write(anum(1:llen),afmt,err=100) inum
        anum=adjustl(anum)
        if(index(anum,'*').ne.0) go to 100
        return

100     continue
        anum=' '
        return

end subroutine utl_int2char


subroutine utl_real2char(rnum,anum)

! -- Subroutine UTL_REAL2CHAR converts a real number to a string.

        implicit none
        real, intent(in)                :: rnum
        character (len=*), intent(out)  :: anum

        integer                         :: llen,n1,n2,i
        character (len=15)              :: afmt

        llen=len(anum)
        n1=min(13,llen)
        if(rnum.lt.0)then
          i=7
        else
          i=6
        end if
        n2=n1-i
        if(n2.lt.0) go to 100
        afmt='(1pg  .  )'
        write(afmt(5:6),'(i2)') n1
        write(afmt(8:9),'(i2)') n2
        write(anum,afmt,err=100) rnum
        anum=adjustl(anum)
        return

100     continue
        anum=' '
	return

end subroutine utl_real2char



subroutine utl_dbl2char(dnum,anum)

! -- Subroutine UTL_DBL2CHAR converts a double precision number to a string.

        implicit none
        double precision, intent(in)    :: dnum
        character (len=*), intent(out)  :: anum

        integer                         :: llen,n1,n2,i
        character (len=15)              :: afmt

        llen=len(anum)
        n1=min(13,llen)
        if(dnum.lt.0)then
          i=7
        else
          i=6
        end if
        n2=n1-i
        if(n2.lt.0) go to 100
        afmt='(1pg  .  )'
        write(afmt(5:6),'(i2)') n1
        write(afmt(8:9),'(i2)') n2
        write(anum,afmt,err=100) dnum
        anum=adjustl(anum)
        return

100     continue
        anum=' '
	return

end subroutine utl_dbl2char



integer function utl_char2int(astring,inum)

! -- Function UTL_CHAR2INT reads an integer from a string.

        implicit none
        character (len=*), intent(in)   :: astring
        integer, intent(out)            :: inum

        integer                         :: i
        character (len=6)               :: afmt

        if(astring.eq.' ') go to 100
        utl_char2int=0
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(astring)
        read(astring,afmt,err=100) inum
        return

100     utl_char2int=1
        return

end function utl_char2int



integer function utl_char2real(astring,rnum)

! -- Function UTL_CHAR2REAL reads a real number from a string.

        implicit none
        character (len=*), intent(in)    :: astring
        real, intent(out)                :: rnum

        character (len=8)                :: afmt

        if(astring.eq.' ') go to 100
        utl_char2real=0
        afmt='(f   .0)'
        write(afmt(3:5),'(i3)') len(astring)
        read(astring,afmt,err=100) rnum
        return

100     utl_char2real=1
        return

end function utl_char2real



integer function utl_char2dbl(astring,dnum)

! -- Function UTL_CHAR2DBL reads a double precision number from a string.

        implicit none
        character (len=*), intent(in)    :: astring
        double precision, intent(out)    :: dnum

        character (len=8)                :: afmt

        if(astring.eq.' ') go to 100
        utl_char2dbl=0
        afmt='(f   .0)'
        write(afmt(3:5),'(i3)') len(astring)
        read(astring,afmt,err=100) dnum
        return

100     utl_char2dbl=1
        return

end function utl_char2dbl



subroutine utl_char2string(ndim,textchar,textstring)

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

end subroutine utl_char2string



subroutine utl_string2char(ndim,textstring,textchar)

! -- Converts a C type character string to a FORTRAN character variable.

       implicit none
       integer, intent(in)            :: ndim
       character (len=1), intent(in)  :: textstring(ndim)
       character (len=*), intent(out) :: textchar

       integer       :: ns,is

       ns=len(textchar)
       textchar=' '
       do is=1,min(ns,ndim)
         if(textstring(is).eq.achar(0))exit
         textchar(is:is)=textstring(is)
       end do
       return

end subroutine utl_string2char



integer function utl_textcheck(astring)

! -- This function tests for ASCII reasonable values in a text string.

       implicit none
       character (len=*), intent(in) :: astring
       integer i,ii

! -- Initialisation

       utl_textcheck=0

       do i=1,len_trim(astring)
         ii=ichar(astring(i:i))
         if((ii.le.31).or.(ii.ge.127)) go to 9890
       end do
       return

9890   continue
       utl_textcheck=1
       return

end function utl_textcheck



integer function utl_wordsplit(nword,ls,rs,aline)

! -- Function UTL_WORDSPLIT splits a character string into words.

        implicit none

        integer, intent(in)            :: nword
        integer, intent(out)           :: ls(nword),rs(nword)
        character (len=*),intent(in)   :: aline

        integer                        :: istart,ifin,iword
        integer                        :: i,j
        character (len=1)              :: atab,aa

        utl_wordsplit=0
        atab=char(9)

        if(nword.eq.0) return
        if(aline.eq.' ')then
          utl_wordsplit=-1
          return
        end if

        istart=0
        ifin=len_trim(aline)
        if(ifin.eq.0)then
          utl_wordsplit=-1
          return
        end if

        do iword=1,nword
          if(istart.eq.ifin) go to 9890
          do i=istart+1,ifin
            if((aline(i:i).ne.' ').and.(aline(i:i).ne.atab))exit
          end do
          aa=aline(i:i)
          if((aa.ne.'''').and.(aa.ne.'"'))aa=' '
          if(aa.eq.' ')then
            ls(iword)=i
            do j=i,ifin
              if((aline(j:j).eq.' ').or.(aline(j:j).eq.atab)) exit
            end do
          else
            ls(iword)=i+1
            do j=i+1,ifin
              if(aline(j:j).eq.aa) go to 20
            end do
            go to 9891
20          continue
          end if
          if(aa.eq.' ')then
            if(j.eq.ifin)then
              rs(iword)=ifin
            else
              rs(iword)=j-1
            end if
            istart=rs(iword)
          else
            istart=j
            if(i+1.eq.j)go to 9890
            rs(iword)=j-1
          end if
        end do

        return
9890    utl_wordsplit=1
        go to 9999
9891    utl_wordsplit=2
        go to 9999

9999    continue
        return

end function utl_wordsplit



end module utilities