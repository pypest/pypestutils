module used_by_kb2d_1
  integer                       :: maxsam=0
  real, allocatable             :: xa(:),ya(:),dist(:)
  double precision, allocatable :: r(:),rr(:),s(:),a(:)
  integer, allocatable          :: nums(:)
end module used_by_kb2d_1


module geostat_3d

      integer             :: MAXDAT
      integer,allocatable :: nisb(:),ixsbtosr(:),iysbtosr(:),izsbtosr(:)
      real,allocatable    :: x(:),y(:),z(:),vr(:),ve(:),dh(:),tmp(:),                   &
               close(:),xa(:),ya(:),za(:),vra(:),vea(:),xdb(:),ydb(:),                  &
               zdb(:),cut(:),cdf(:)
      real*8,allocatable  :: r(:),rr(:),s(:),a(:)

      real, allocatable    :: sec3(:)                               !jd

end module geostat_3d


!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 2-D KRIGGING FOLLOWS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine kb2d_1(n_ndmin,n_ndmax,r_radius,k_ktype,s_skmean,    &
      n_nst,c_c0,i_it,c_cc,a_ang,a_aa,a_anis,                   &
      unestimated,n_npts,n_ndat,inumdat,icellno,epoint,npoint,  &
      aoutfile,outunit,pmx,x,y,af1,af2)

      use used_by_kb2d_1

      implicit integer(i-n), real(a-h, o-z)                    !jd

!-----------------------------------------------------------------------
!
!           Ordinary/Simple Kriging of a 2-D Rectangular Grid
!           *************************************************
!
! This subroutine estimates point or block values of one variable by
! ordinary kriging.  All of the samples are rescanned for each block
! estimate; this makes the program simple but inefficient.  The data
! should NOT contain any missing values.  Unestimated points are
! returned as -1.0e21
!
!
!
! Original:  A.G. Journel                                           1978
! Revisions: B.E. Buxton                                       Apr. 1983
!-----------------------------------------------------------------------
!      include  'kb2d.inc'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Here is kb2d.inc!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! The following Parameters control static dimensioning within okb2d:
!
!   MAXSAM    maximum number of data points to use in one kriging system
!   MAXDIS    maximum number of discretization points per block
!   MAXNST    maximum number of nested structures
!
!-----------------------------------------------------------------------
!
! User Adjustable:
!
!      parameter(MAXSAM =  3600,  &
!                MAXDIS =   64,  &
!                MAXNST =    5)
      parameter(MAXDIS =   64,MAXNST =    5)

!
! Fixed
!
!      parameter(MAXKD=MAXSAM+1,MAXKRG=MAXKD*MAXKD,UNEST=-999.,  &
!                EPSLON=0.0000001,VERSION=2.000)
      parameter(UNEST=-999.,EPSLON=0.0000001,VERSION=2.000)
!
! Variable Declaration:
!
      real      aa(MAXNST),cc(MAXNST),ang(MAXNST),anis(MAXNST)
      integer   it(MAXNST)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!      real      xdb(MAXDIS),ydb(MAXDIS),xa(MAXSAM),ya(MAXSAM), &
!                vra(MAXSAM),dist(MAXSAM)
!!                dist(MAXSAM)
      real      xdb(MAXDIS),ydb(MAXDIS)
!!      real*8    r(MAXSAM+1),rr(MAXSAM+1),s(MAXSAM+1),a(MAXKRG)
!!      integer   nums(MAXSAM)
      logical   first

! -- The following dimension statement was added by myself for the
!    subroutine arguments.



     integer n_npts,n_ndat,outunit                                  !jd
     integer icellno(n_npts),inumdat(n_ndat)                        !jd
     real pmx                                                       !jd
     real epoint(n_npts),npoint(n_npts)                             !jd
     character*(*) aoutfile                                         !jd
     character*(*)  af1,af2                                         !jd
     character*150 aformat                                          !jd
     real x(n_ndat),y(n_ndat)                                       !jd

     integer unestimated

!!!!!!!! OLD READPARM !!!!!!!!!!
! -- The following used to be in subroutine READPARM. This allows us to
!    get rid of the common block.

      integer n_nst
      integer i_it(n_nst)
      real c_c0
      real c_cc(n_nst),a_ang(n_nst),a_aa(n_nst),a_anis(n_nst)
      lout = 2
      ldbg = 3
      idbg=0                    !debugging level
      nxdis=1
      nydis=1

! -- Here are the dummy grid specifications.

      nx=1
      xmn=0.0
      xsiz=1.0
      ny=1
      ymn=0.0
      ysiz=1.0

! -- The following variables were supplied through the subroutine argument.

      ndmin=n_ndmin
      ndmax=n_ndmax
      radius=r_radius
      ktype=k_ktype
      skmean=s_skmean
      nst=n_nst
      c0=c_c0

            do i=1,nst
                  it(i)=i_it(i)
                  cc(i)=c_cc(i)
                  ang(i)=a_ang(i)
                  aa(i)=a_aa(i)
                  anis(i)=a_anis(i)
                  if(it(i).eq.4) then
                        if(aa(i).lt.0.0) stop ' INVALID power variogram'
                        if(aa(i).gt.2.0) stop ' INVALID power variogram'
                  end if
            end do

! -- The following code was added for Damian so that we can dynamically allocate some arrays

      if(ndmax.gt.maxsam)then
        if(maxsam.gt.0)then
          deallocate(xa,ya,dist,stat=ierr)
          if(ierr.ne.0)then
            write(6,12)
12          format(' Deallocation error in subroutine kb2d_1.')
            stop
          end if
          deallocate(r,rr,s,a,nums,stat=ierr)
          if(ierr.ne.0) then
            write(6,12)
            stop
          end if
          maxsam=0
        end if
      end if
      if(maxsam.eq.0)then
!        maxsam=max(ndmax,3600)
        maxsam=max(ndmax,7000)
!        maxsam=max(ndmax,15000)
        maxkd=maxsam+1
        maxkrg=maxkd*maxkd
        allocate(xa(maxsam),ya(maxsam),dist(maxsam),stat=ierr)
        if(ierr.ne.0)then
          write(6,11)
11        format(' Allocation error in subroutine kb2d_1.')
          stop
        end if
        allocate(r(maxsam+1),rr(maxsam+1),s(maxsam+1),a(maxkrg),nums(maxsam),stat=ierr)
        if(ierr.ne.0)then
          write(6,11)
          stop
        end if
      end if

! -- Testing

      if(nst.gt.MAXNST)   stop ' nst is too big - contact programmer'
      if(ndmax.gt.MAXSAM) stop ' ndmax is too big - contact programmer'

!!!!!!!! OLD READPARM !!!!!!!!!!

! -- We work out the formatting so that we get good compression if
!    writing an ascii file.

     nd=n_ndat
     aformat='('//trim(af2)//','//trim(af1)//',1x,1pg14.7,1000('//trim(af1)//',1x,1pg14.7))' !jd

      first=.true.                                    !jd
      unestimated=0
!
! Echo the input parameters if debugging flag is >2:
!
      if(idbg.gt.2) then
            write(ldbg,*) 'KB2D Parameters'
            write(ldbg,*)
            write(ldbg,*) 'Variogram Parameters for ',nst,' structures:'
            write(ldbg,*) '  Nugget effect:         ',c0
            write(ldbg,*) '  Types of variograms:   ',(it(i),i=1,nst)
            write(ldbg,*) '  Contribution cc        ',(cc(i),i=1,nst)
            write(ldbg,*) '  Ranges:                ',(aa(i),i=1,nst)
            write(ldbg,*) '  Angle for Continuity:  ',(ang(i),i=1,nst)
            write(ldbg,*) '  Anisotropy Factors:    ',(anis(i),i=1,nst)
            write(ldbg,*) ' '
            write(ldbg,*) 'Grid for Kriging:'
            write(ldbg,*) '  Number of X and Y Blocks:',nx,ny
            write(ldbg,*) '  Origin of X and Y Blocks:',xmn,ymn
            write(ldbg,*) '  Size   of X and Y Blocks:',xsiz,ysiz
            write(ldbg,*) ' '
            write(ldbg,*) 'Discretization of blocks:  ',nxdis,nydis
            write(ldbg,*) 'Search Radius:             ',radius
            write(ldbg,*) 'Minimum number of samples: ',ndmin
            write(ldbg,*) 'Maximum number of samples: ',ndmax
            write(ldbg,*) ' '
      endif
!
! Echo the input data if debugging flag >1:
!
!      if(idbg.ge.4) then
!            do id=1,nd
!                  write(ldbg,99) id,x(id),y(id),vr(id)
! 99               format('Data: ',i5,' at ',2f12.3,' value: ',f12.5)
!            end do
!      endif
!
! Set up the discretization points per block.  Figure out how many
! are needed, the spacing, and fill the xdb and ydb arrays with the
! offsets relative to the block center (this only gets done once):
!
      ndb  = nxdis * nydis
      if(ndb.gt.MAXDIS) then
            write(*,*) ' ERROR KB2D: Too many discretization points '
            write(*,*) '             Increase MAXDIS or lower n[xy]dis'
            stop
      endif
      xdis = xsiz  / max(real(nxdis),1.0)
      ydis = ysiz  / max(real(nydis),1.0)
      xloc = -0.5*(xsiz+xdis)
      i    = 0
      do ix =1,nxdis
            xloc = xloc + xdis
            yloc = -0.5*(ysiz+ydis)
            do iy=1,nydis
                  yloc = yloc + ydis
                  i = i+1
                  xdb(i) = xloc
                  ydb(i) = yloc
            end do
      end do
!
! Initialize accumulators:
!
      cbb  = 0.0
      rad2 = radius*radius
!
! Calculate Block Covariance. Check for point kriging.
!
      cov   = cova2(xdb(1),ydb(1),xdb(1),ydb(1),nst,c0,PMX,cc, &
                    aa,it,ang,anis,first,passmaxcov)
!
! Keep this value to use for the unbiasedness constraint:
!
      unbias = cov
      first  = .false.
      if (ndb.le.1) then
            cbb = cov
      else
            do i=1,ndb
                  do j=1,ndb
                        cov = cova2(xdb(i),ydb(i),xdb(j),ydb(j),nst,c0, &
                                    PMX,cc,aa,it,ang,anis,first,passmaxcov)
                        if(i.eq.j) cov = cov - c0
                        cbb = cbb + cov
                  end do
            end do
            cbb = cbb/real(ndb*ndb)
      endif
      if(idbg.gt.1) then
            write(ldbg,*) ' '
            write(ldbg,*) 'Block Covariance: ',cbb
            write(ldbg,*) ' '
      endif
!
! MAIN LOOP OVER ALL THE BLOCKS IN THE GRID:
!
      nk = 0
      ak = 0.0
      vk = 0.0
!jd      do 4 iy=1,ny                                               !jd
!jd      yloc = ymn + (iy-1)*ysiz                                   !jd
!jd      do 4 ix=1,nx                                               !jd
!jd            xloc = xmn + (ix-1)*xsiz                             !jd

         do 4 i_ipts=1,n_npts                                       !jd
            xloc=epoint(i_ipts)                                     !jd
            yloc=npoint(i_ipts)                                     !jd
            icell=icellno(i_ipts)                                   !jd
!
! Find the nearest samples within each octant: First initialize
! the counter arrays:
!
            na = 0
            do isam=1,ndmax
                  dist(isam) = 1.0e+20
                  nums(isam) = 0
            end do
!
! Scan all the samples (this is inefficient and the user with lots of
! data should move to ktb3d):
!
            do 6 id=1,nd
                  dx = x(id) - xloc
                  dy = y(id) - yloc
                  h2 = dx*dx + dy*dy
                  if(h2.gt.rad2) go to 6
!
! Do not consider this sample if there are enough close ones:
!
                  if(na.ne.0)then                          !jd
                  if(na.eq.ndmax.and.h2.gt.dist(na)) go to 6
                  end if                                  !jd
!
! Consider this sample (it will be added in the correct location):
!
                  if(na.lt.ndmax) na = na + 1
                  nums(na)           = id
                  dist(na)           = h2
                  if(na.eq.1) go to 6
!
! Sort samples found thus far in increasing order of distance:
!
                  n1 = na-1
                  do ii=1,n1
                        k=ii
                        if(h2.lt.dist(ii)) then
                              jk = 0
                              do jj=k,n1
                                    j  = n1-jk
                                    jk = jk+1
                                    j1 = j+1
                                    dist(j1) = dist(j)
                                    nums(j1) = nums(j)
                              end do
                              dist(k) = h2
                              nums(k) = id
                              go to 6
                        endif
                  end do
 6          continue
!
! Is there enough samples?
!
            if(na.lt.ndmin) then
                  if(idbg.ge.2) &
                  write(ldbg,*) 'Block ',ix,iy, 'not estimated'
                  est  = UNEST
                  estv = UNEST
                  go to 8888
!                  go to 1
            endif
!
! Put coordinates and values of neighborhood samples into xa,ya,vra:
!
            do ia=1,na
                  jj      = nums(ia)
                  xa(ia)  = x(jj)
                  ya(ia)  = y(jj)
!                  vra(ia) = vr(jj)
            end do
!
! Handle the situation of only one sample:
!
            if(na.eq.1) then
                  cb1 = cova2(xa(1),ya(1),xa(1),ya(1),nst,c0,   &
                              PMX,cc,aa,it,ang,anis,first,passmaxcov)
                  xx  = xa(1) - xloc
                  yy  = ya(1) - yloc
!
! Establish Right Hand Side Covariance:
!
                  if(ndb.le.1) then
                        cb = cova2(xx,yy,xdb(1),ydb(1),nst,c0,  &
                                   PMX,cc,aa,it,ang,anis,first,passmaxcov)
                  else
                        cb  = 0.0
                        do i=1,ndb
                              cb = cb + cova2(xx,yy,xdb(i),ydb(i),nst, &
                                        c0,PMX,cc,aa,it,ang,anis,first,passmaxcov)
                              dx = xx - xdb(i)
                              dy = yy - ydb(i)
                              if((dx*dx+dy*dy).lt.EPSLON) &
                              cb = cb - c0
                        end do
                        cb = cb / real(ndb)
                  end if
                  if(ktype.eq.0) then
                        s(1) = cb/cbb
!                        est  = s(1)*vra(1) + (1.0-s(1))*skmean
!                        estv = cbb - s(1) * cb
                  else
                        s(1) = 1.0                                      !jd
!                        est  = vra(1)
!                        estv = cbb - 2.0*cb + cb1
                  end if

                  if(ktype.eq.0)then                                    !jd
!!                    rrtemp=(1.0-s(1))*skmean                            !jd
                    rrtemp=(1.0-s(1))                                  !jd
                  else                                                  !jd
                    rrtemp=0.0                                          !jd
                  end if                                                !jd
                  if(aoutfile.eq.'f')then                               !jd
                    write(outunit,aformat) icell,1,rrtemp, &            !jd
                    inumdat(nums(1)),s(1)                                 !jd
                  else                                                    !jd
                    i_one=1                                               !jd
                    write(outunit)   icell,i_one,rrtemp, &                    !jd
                    inumdat(nums(1)),real(s(1))                              !jd
                  end if                                                  !jd

            else
!
! Solve the Kriging System with more than one sample:
!
                  neq = na + ktype
                  nn  = (neq + 1)*neq/2
!
! Set up kriging matrices:
!
                  in=0
                  do j=1,na
!
! Establish Left Hand Side Covariance Matrix:
!
                        do i=1,j
                              in = in + 1
                              a(in) = dble( cova2(xa(i),ya(i),xa(j),  &
                                            ya(j),nst,c0,PMX,cc,aa,it, &
                                            ang,anis,first,passmaxcov) )
                        end do
                        xx = xa(j) - xloc
                        yy = ya(j) - yloc
!
! Establish Right Hand Side Covariance:
!
                        if(ndb.le.1) then
                              cb = cova2(xx,yy,xdb(1),ydb(1),nst,c0,  &
                                         PMX,cc,aa,it,ang,anis,first,passmaxcov)
                        else
                              cb  = 0.0
                              do j1=1,ndb
                                    cb = cb + cova2(xx,yy,xdb(j1),  &
                                         ydb(j1),nst,c0,PMX,cc,aa,  &
                                         it,ang,anis,first,passmaxcov)
                                    dx = xx - xdb(j1)
                                    dy = yy - ydb(j1)
                                    if((dx*dx+dy*dy).lt.EPSLON)  &
                                          cb = cb - c0
                              end do
                              cb = cb / real(ndb)
                        end if
                        r(j)  = dble(cb)
                        rr(j) = r(j)
                  end do
!
! Set the unbiasedness constraint:
!
                  if(ktype.eq.1) then
                        do i=1,na
                              in    = in + 1
                              a(in) = dble(unbias)
                        end do
                        in      = in + 1
                        a(in)   = 0.0
                        r(neq)  = dble(unbias)
                        rr(neq) = r(neq)
                  end if
!
! Write out the kriging Matrix if Seriously Debugging:
!
                  if(idbg.ge.3) then
                        write(ldbg,101) ix,iy
                        is = 1
                        do i=1,neq
                              ie = is + i - 1
                              write(ldbg,102) i,r(i),(a(j),j=is,ie)
                              is = is + i
                        end do
 101                    format(/,'Kriging Matrices for Node: ',2i4,  &
                                 ' RHS first')
 102                    format('    r(',i2,') =',f7.4,'  a= ',9(10f7.4))
                  endif
!
! Solve the Kriging System:
!
                  call ksol(1,neq,1,a,r,s,ising)
!
! Write a warning if the matrix is singular:
!
                  if(ising.ne.0) then
        write(6,450) icell                                            !jd
450     format(' WARNING: singular kriging matrix for cell',i5)      !jd
                        est  = UNEST
                        estv = UNEST
                        go to 8888
!                        go to 1
                  endif
!
! Write the kriging weights and data if requested:
!
!                  if(idbg.ge.2) then
!                        write(ldbg,*) '       '
!                        write(ldbg,*) 'BLOCK: ',ix,iy
!                        write(ldbg,*) '       '
!                        if(ktype.eq.1) write(ldbg,*)    &
!                        '  Lagrange multiplier: ',s(neq)*unbias
!                        write(ldbg,*) '  BLOCK EST: x,y,vr,wt '
!                        do i=1,na
!                        write(ldbg,'(4f12.3)') xa(i),ya(i),vra(i),s(i)
!                        end do
!                  endif
!
! Compute the estimate and the kriging variance:
!
                  est  = 0.0
                  estv = cbb
                  sumw = 0.0
                  if(ktype.eq.1) estv = estv - real(s(na+1))
                  do i=1,na
                        sumw = sumw + real(s(i))
!                        est  = est  + real(s(i))*vra(i)
!                        estv = estv - real(s(i)*rr(i))
                  end do
!                  if(ktype.eq.0) est = est + (1.0-sumw)*skmean

                  if(ktype.eq.0)then                                    !jd
!!                    rrtemp=(1.0-sumw)*skmean                            !jd
                    rrtemp=(1.0-sumw)                                   !jd
                  else                                                  !jd
                    rrtemp=0.0                                          !jd
                  end if                                                !jd
                  if(aoutfile.eq.'f')then                               !jd
                    write(outunit,aformat) icell,na,rrtemp, &           !jd
                    ((inumdat(nums(i)),real(s(i))),i=1,na)              !jd
                  else                                                  !jd
                    write(outunit)   icell,na,rrtemp, &                 !jd
                    ((inumdat(nums(i)),real(s(i))),i=1,na)              !jd
                  end if                                                !jd
            endif
!            if(idbg.ge.2) then
!                  write(ldbg,*) '  est  ',est
!                  write(ldbg,*) '  estv ',estv
!                  write(ldbg,*) ' '
!            endif
!
! Write the result to the output file:
!
! 1          write(lout,'(f8.3,1x,f8.3)') est,estv

1           continue

!            if(est.gt.UNEST)then
!              iirow=(icell-1)/n_ncol+1
!              iicol=icell-((iirow-1)*n_ncol)
!              rrtemp=estv
!              if(rrtemp.lt.0.0)rrtemp=0.0
!              var_arr(iicol,iirow)=sqrt(rrtemp)
!            end if

!            if(est.gt.UNEST) then
!                  nk = nk + 1
!                  ak = ak + est
!                  vk = vk + est*est
!            end if
!
! END OF MAIN LOOP OVER ALL THE BLOCKS:
!
 4    continue
!      if(nk.ge.1) then
!            ak = ak / real(nk)
!            vk = vk/real(nk) - ak*ak
!             write(6,105) nk
105          format('   No. of grid points for which factors were calculated = ',i5)
!             write(6,106) ak
!106          format('   Average interpolated value at these points           = ',1pg12.5)
!             if(vk.lt.0.0)vk=0.0
!             write(6,107) sqrt(vk)
!107          format('   Standard deviation of interpolated grid point values = ',1pg12.5)

!            write(ldbg,105) nk,ak,vk
!            write(*,   105) nk,ak,vk
! 105        format(/,'Estimated   ',i8,' blocks ',/,  &
!                     '  average   ',f9.4,/,'  variance  ',f9.4,/)
!      else
!        write(6,105) nk
!      end if
      return

! Error condition arising from an unestimable point.

8888  unestimated=icell
      return


end subroutine kb2d_1



      real function cova2(x1,y1,x2,y2,nst,c0,PMX,cc,aa,it,  &
                          ang,anis,first,passmaxcov)              !jd

      implicit integer(i-n), real(a-h, o-z)                    !jd

!-----------------------------------------------------------------------
!
!              Covariance Between Two Points (2-D Version)
!              *******************************************
!
! This function returns the covariance associated with a variogram model
! that is specified by a nugget effect and possibly four different
! nested varigoram structures.  The anisotropy definition can be
! different for each of the nested structures (spherical, exponential,
! gaussian, or power).
!
!
!
! INPUT VARIABLES:
!
!   x1,y1            Coordinates of first point
!   x2,y2            Coordinates of second point
!   nst              Number of nested structures (max. 4).
!   c0               Nugget constant (isotropic).
!   PMX              Maximum variogram value needed for kriging when
!                      using power model.  A unique value of PMX is
!                      used for all nested structures which use the
!                      power model.  therefore, PMX should be chosen
!                      large enough to account for the largest single
!                      structure which uses the power model.
!   cc(nst)          Multiplicative factor of each nested structure.
!   aa(nst)          Parameter "a" of each nested structure.
!   it(nst)          Type of each nested structure:
!                      1. spherical model of range a;
!                      2. exponential model of parameter a;
!                           i.e. practical range is 3a
!                      3. gaussian model of parameter a;
!                           i.e. practical range is a*sqrt(3)
!                      4. power model of power a (a must be gt. 0  and
!                           lt. 2).  if linear model, a=1,c=slope.
!   ang(nst)         Azimuth angle for the principal direction of
!                      continuity (measured clockwise in degrees from Y)
!   anis(nst)        Anisotropy (radius in minor direction at 90 degrees
!                      from "ang" divided by the principal radius in
!                      direction "ang")
!   first            A logical variable which is set to true if the
!                      direction specifications have changed - causes
!                      the rotation matrices to be recomputed.
!
!
!
! OUTPUT VARIABLES: returns "cova2" the covariance obtained from the
!                   variogram model.
!
!
!
!-----------------------------------------------------------------------
      parameter(DTOR=3.14159265/180.0,EPSLON=0.0000001)
      real      aa(*),cc(*),ang(*),anis(*),rotmat(4,4),maxcov
      integer   it(*)
      logical   first
      save      rotmat,maxcov
!
! The first time around, re-initialize the cosine matrix for the
! variogram structures:
!
      if(first) then
            maxcov = c0
            do is=1,nst
                  azmuth       = (90.0-ang(is))*DTOR
                  rotmat(1,is) =  cos(azmuth)
                  rotmat(2,is) =  sin(azmuth)
                  rotmat(3,is) = -sin(azmuth)
                  rotmat(4,is) =  cos(azmuth)
                  if(it(is).eq.4) then
                        maxcov = maxcov + PMX
                  else
                        maxcov = maxcov + cc(is)
                  endif
            end do
      endif
      passmaxcov=maxcov                                            !jd
!
! Check for very small distance:
!
      dx = x2-x1
      dy = y2-y1
      if((dx*dx+dy*dy).lt.EPSLON) then
            cova2 = maxcov
            return
      endif
!
! Non-zero distance, loop over all the structures:
!
      cova2 = 0.0
      do is=1,nst
!
! Compute the appropriate structural distance:
!
            dx1 = (dx*rotmat(1,is) + dy*rotmat(2,is))
            dy1 = (dx*rotmat(3,is) + dy*rotmat(4,is))/anis(is)
            h   = sqrt(max((dx1*dx1+dy1*dy1),0.0))
            if(it(is).eq.1) then
!
! Spherical model:
!
                  hr = h/aa(is)
                  if(hr.lt.1.0) cova2 = cova2    &
                                      + cc(is)*(1.-hr*(1.5-.5*hr*hr))
            else if(it(is).eq.2) then
!
! Exponential model:
!
                  cova2 = cova2 +cc(is)*exp(-h/aa(is))
            else if(it(is).eq. 3) then
!
! Gaussian model:
!
                  hh=-(h*h)/(aa(is)*aa(is))
                  cova2 = cova2 +cc(is)*exp(hh)
            else
!
! Power model:
!
                  cov1  = PMX - cc(is)*(h**aa(is))
                  cova2 = cova2 + cov1
            endif
      end do
      return
      end





      subroutine ksol(nright,neq,nsb,a,r,s,ising)

!-----------------------------------------------------------------------
!
!                Solution of a System of Linear Equations
!                ****************************************
!
!
!
! INPUT VARIABLES:
!
!   nright,nsb       number of columns in right hand side matrix.
!                      for KB2D: nright=1, nsb=1
!   neq              number of equations
!   a()              upper triangular left hand side matrix (stored
!                      columnwise)
!   r()              right hand side matrix (stored columnwise)
!                      for kb2d, one column per variable
!
!
!
! OUTPUT VARIABLES:
!
!   s()              solution array, same dimension as  r  above.
!   ising            singularity indicator
!                      0,  no singularity problem
!                     -1,  neq .le. 1
!                      k,  a null pivot appeared at the kth iteration
!
!
!
! PROGRAM NOTES:
!
!   1. Requires the upper triangular left hand side matrix.
!   2. Pivots are on the diagonal.
!   3. Does not search for max. element for pivot.
!   4. Several right hand side matrices possible.
!   5. USE for ok and sk only, NOT for UK.
!
!
!-----------------------------------------------------------------------
      implicit integer (i-n)
      implicit real*8 (a-h,o-z)
      real*8   a(*),r(*),s(*)
!
! If there is only one equation then set ising and return:
!
      if(neq.le.1) then
            ising = -1
            return
      endif
!
! Initialize:
!
      tol   = 0.1e-06
      ising = 0
      nn    = neq*(neq+1)/2
      nm    = nsb*neq
      m1    = neq-1
      kk    = 0
!
! Start triangulation:
!
      do k=1,m1
            kk=kk+k
            ak=a(kk)
            if(abs(ak).lt.tol) then
                  ising=k
                  return
            endif
            km1=k-1
            do iv=1,nright
                  nm1=nm*(iv-1)
                  ii=kk+nn*(iv-1)
                  piv=1./a(ii)
                  lp=0
                  do i=k,m1
                        ll=ii
                        ii=ii+i
                        ap=a(ii)*piv
                        lp=lp+1
                        ij=ii-km1
                        do j=i,m1
                              ij=ij+j
                              ll=ll+j
                              a(ij)=a(ij)-ap*a(ll)
                        end do
                        do llb=k,nm,neq
                              in=llb+lp+nm1
                              ll1=llb+nm1
                              r(in)=r(in)-ap*r(ll1)
                        end do
                  end do
            end do
      end do
!
! Error checking - singular matrix:
!
      ijm=ij-nn*(nright-1)
      if(abs(a(ijm)).lt.tol) then
            ising=neq
            return
      endif
!
! Finished triangulation, start solving back:
!
      do iv=1,nright
            nm1=nm*(iv-1)
            ij=ijm+nn*(iv-1)
            piv=1./a(ij)
            do llb=neq,nm,neq
                  ll1=llb+nm1
                  s(ll1)=r(ll1)*piv
            end do
            i=neq
            kk=ij
            do ii=1,m1
                  kk=kk-i
                  piv=1./a(kk)
                  i=i-1
                  do llb=i,nm,neq
                        ll1=llb+nm1
                        in=ll1
                        ap=r(in)
                        ij=kk
                        do j=i,m1
                              ij=ij+j
                              in=in+1
                              ap=ap-a(ij)*s(in)
                        end do
                        s(ll1)=ap*piv
                  end do
            end do
      end do
!
! Finished solving back, return:
!
      return
      end

!Notes:-
! standard deviations computed for gaussian case seem to be in error when a is large.



!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 3-D KRIGGING FOLLOWS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine readparm_1(MAXDIS,MAXSBX,MAXSBY,MAXSBZ,MAXEQ,n_ndmin,n_ndmax,     &
      r_radius1,r_radius2,r_radius3,k_ktype,s_skmean,n_nst,c_c0,           &
      i_it,c_cc,a_ang1,a_ang2,a_ang3,a_hmax,a_hmin,a_vert,                 &
      n_nd,ncol_range,nrow_range,icall)

!      use       msflib
      use       geostat_3d

      implicit integer(i-n), real(a-h, o-z)                    !jd

!!!!!!!!!!!!!!!!!!kt3d.inc!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      parameter(MAXDT  =   9, MAXNST =   4)
      parameter(UNEST = -999.0, EPSLON = 0.000001, MAXROT = MAXNST + 1,VERSION = 3.000)
      integer   idrif(MAXDT),nst(1),it(MAXNST),iktype,ncut,test
      real      bv(9),c0(1),cc(MAXNST),aa(MAXNST),ang1(MAXNST),	         &
                ang2(MAXNST),ang3(MAXNST),anis1(MAXNST),anis2(MAXNST)
      real*8    rotmat(MAXROT,3,3)
      common /datcom/ nd,tmin,tmax,nx,ny,nz,xmn,ymn,zmn,test,            &
                      xsiz,ysiz,zsiz,ndmax,ndmin,radius,noct,nxdis,      &
                      nydis,nzdis,idrif,itrend,ktype,skmean,koption,     &
                      idbg,ldbg,iout,lout,iext,lext,iextve,ljack,        &
                      idhlj,ixlj,iylj,izlj,ivrlj,iextvj,nvarij
      common /vargdt/ nst,it,c0,cc,aa,ang1,ang2,ang3,anis1,anis2,rotmat
      common /srccom/ sang1,sang2,sang3,sanis1,sanis2,isrot,na,ndb,bv,unbias,iktype,ncut

!!!!!!!!!!!!!!!!!!kt3d.inc!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      parameter(MV=100)
      real      var(MV)
      character datafl*512,jackfl*512,extfl*512,outfl*512,dbgfl*512,str*512,title*80
      logical   testfl

! -- Dimension statements added by myself for subroutine arguments.

      integer ncol_range,nrow_range
      integer i_it(n_nst)
      integer icall
      real c_c0
      real c_cc(n_nst),a_ang1(n_nst),a_ang2(n_nst),a_ang3(n_nst)
      real a_hmax(n_nst),a_hmin(n_nst),a_vert(n_nst)

!
! FORTRAN Units:
!
      lin   = 81                       !jd
      ldbg  = 83                       !jd
      lout  = 84                       !jd
      lext  = 87                       !jd
      ljack = 88                       !jd

!
! Read Input Parameters:
!
!jd      read(lin,'(a512)',err=98) datafl
!jd      call chknam(datafl,512)
!jd      write(*,*) ' data file = ',datafl(1:40)

!jd      read(lin,*,err=98) idhl,ixl,iyl,izl,ivrl,iextv
!jd      write(*,*) ' columns = ',idhl,ixl,iyl,izl,ivrl,iextv

!jd      read(lin,*,err=98) tmin,tmax
!jd      write(*,*) ' trimming limits = ',tmin,tmax

!jd      read(lin,*,err=98) koption
!jd      write(*,*) ' kriging option = ',koption

      datafl=' '                 !jd
      idhl=-9999                 !jd
      ixl=-9999                  !jd
      iyl=-9999                  !jd
      izl=-9999                  !jd
      ivrl=-9999                 !jd
      iextv=-9999                !jd
      tmax=0.0                   !jd
      tmin=0.0                   !jd
      koption=0                  !jd
!
! This is an undocumented feature to have kt3d construct an IK-type
! distribution:
!
      iktype = 0
!jd      if(koption.lt.0) then
!jd            iktype  = 1
!jd            koption = -koption
!jd      end if
!jd      if(iktype.eq.1) then

!jd            read(lin,*,err=98) ncut
!jd            write(*,*) ' number of cutoffs = ',ncut
!
! Find the needed parameter:
!
!jd            MAXCUT = ncut
!
! Allocate the needed memory:
!21
!jd            allocate(cut(MAXCUT),stat = test)
!jd                  if(test.ne.0)then
!jd                        write(*,*)'ERROR: Allocation failed due to',
!jd     +                        ' insufficient memory.'
!jd                        stop
!jd                  end if
!22
!jd            allocate(cdf(MAXCUT),stat = test)
!jd                  if(test.ne.0)then
!jd                        write(*,*)'ERROR: Allocation failed due to',
!jd     +                        ' insufficient memory.'
!jd                        stop
!jd                  end if
!
!jd            read(lin,*,err=98) (cut(i),i=1,ncut)
!jd            write(*,*) ' cutoffs = ',(cut(i),i=1,ncut)

!jd      end if

!jd      read(lin,'(a512)',err=98) jackfl
!jd      call chknam(jackfl,512)
!jd      write(*,*) ' jackknife data file = ',jackfl(1:40)

         jackfl=' '                     !jd

!jd      read(lin,*,err=98) ixlj,iylj,izlj,ivrlj,iextvj
!jd      write(*,*) ' columns = ',ixlj,iylj,izlj,ivrlj,iextvj

       ixlj=0           !jd
       iylj=0           !jd
       izlj=0           !jd
       ivrlj=0          !jd
       iextvj=0         !jd

!jd      read(lin,*,err=98) idbg
!jd      write(*,*) ' debugging level = ',idbg

      idbg=0                                            !jd

!jd      read(lin,'(a512)',err=98) dbgfl
!jd      call chknam(dbgfl,512)
!jd      write(*,*) ' debugging file = ',dbgfl(1:40)

      dbgfl='debug_kt3d.dat'                            !jd

!jd      read(lin,'(a512)',err=98) outfl
!jd      call chknam(outfl,512)
!jd      write(*,*) ' output file = ',outfl(1:40)


      outfl='output_kt3d.dat'                           !jd

!jd      read(lin,*,err=98) nx,xmn,xsiz
!jd      write(*,*) ' nx, xmn, xsiz = ',nx,xmn,xsiz

      nx=1                                             !jd
      xmn=0.0                                          !jd
      xsiz=1.0                                         !jd

!jd      read(lin,*,err=98) ny,ymn,ysiz
!jd      write(*,*) ' ny, ymn, ysiz = ',ny,ymn,ysiz

      ny=1                                             !jd
      ymn=0.0                                          !jd
      ysiz=1.0                                         !jd

!jd      read(lin,*,err=98) nz,zmn,zsiz
!jd      write(*,*) ' nz, zmn, zsiz = ',nz,zmn,zsiz

      nz=1                                             !jd
      zmn=0.0                                          !jd
      zsiz=1.0                                         !jd

!jd      read(lin,*,err=98) nxdis,nydis,nzdis
!jd      write(*,*) ' block discretization:',nxdis,nydis,nzdis

      nxdis=1                                          !jd
      nydis=1                                          !jd
      nzdis=1                                          !jd

!jd      read(lin,*,err=98) ndmin,ndmax
!jd      write(*,*) ' ndmin,ndmax = ',ndmin,ndmax

      ndmin=n_ndmin                                    !jd
      ndmax=n_ndmax                                    !jd

!jd      read(lin,*,err=98) noct
!jd      write(*,*) ' max per octant = ',noct

      noct=0                                           !jd

!jd      read(lin,*,err=98) radius,radius1,radius2
!jd      write(*,*) ' search radii = ',radius,radius1,radius2
!jd      if(radius.lt.EPSLON) stop 'radius must be greater than zero'

      radius=r_radius1                                !jd
      radius1=r_radius2                                !jd
      radius2=r_radius3                                !jd

      radsqd = radius  * radius
      sanis1 = radius1 / radius
      sanis2 = radius2 / radius

!jd      read(lin,*,err=98) sang1,sang2,sang3
!jd      write(*,*) ' search anisotropy angles = ',sang1,sang2,sang3

!jd      read(lin,*,err=98) ktype,skmean
!jd      write(*,*) ' ktype, skmean =',ktype,skmean

      ktype=k_ktype                                    !jd
      skmean=s_skmean                                  !jd

!jd      read(lin,*,err=98) (idrif(i),i=1,9)
!jd      write(*,*) ' drift terms = ',(idrif(i),i=1,9)

      do i=1,9                                         !jd
        idrif(i)=0                                     !jd
      end do                                           !jd

!jd      read(lin,*,err=98) itrend
!jd      write(*,*) ' itrend = ',itrend

      itrend=0                                         !jd

!jd      read(lin,'(a512)',err=98) extfl
!jd      call chknam(extfl,40)
!jd      write(*,*) ' external drift file = ',extfl(1:40)

      extfl=' '                                        !jd

!jd      read(lin,*,err=98) iextve
!jd      write(*,*) ' variable in external drift file = ',iextve

      iextve=0                                         !jd

!jd      read(lin,*,err=98) nst(1),c0(1)
!jd      write(*,*) ' nst, c0 = ',nst(1),c0(1)

      nst(1)=n_nst                                     !jd
      c0(1)=c_c0                                       !jd

!jd      if(nst(1).le.0) then
!jd            write(*,9997) nst(1)
!jd 9997       format(' nst must be at least 1, it has been set to ',i4,/,
!jd     +             ' The c or a values can be set to zero')
!jd            stop
!jd      endif

      do i=1,nst(1)
!jd            read(lin,*,err=98) it(i),cc(i),ang1(i),ang2(i),ang3(i)

            it(i)=i_it(i)                               !jd
            cc(i)=c_cc(i)                               !jd
            ang1(i)=a_ang1(i)                           !jd
            ang2(i)=a_ang2(i)                           !jd
            ang3(i)=a_ang3(i)                           !jd

!jd            read(lin,*,err=98) aa(i),aa1,aa2

            aa(i)=a_hmax(i)                             !jd
            aa1  =a_hmin(i)                             !jd
            aa2  =a_vert(i)                             !jd

            anis1(i) = aa1 / max(aa(i),EPSLON)
            anis2(i) = aa2 / max(aa(i),EPSLON)
!jd            write(*,*) ' it,cc,ang[1,2,3]; ',it(i),cc(i),
!jd     +                   ang1(i),ang2(i),ang3(i)
!jd            write(*,*) ' a1 a2 a3: ',aa(i),aa1,aa2
            if(it(i).eq.4) then
                  if(aa(i).lt.0.0) stop ' INVALID power variogram'
                  if(aa(i).gt.2.0) stop ' INVALID power variogram'
            end if
      end do

      if(nst(1).eq.1)then                            !jd
        itemp=1                                      !jd
      else                                           !jd
        rtemp=-1.035                                 !jd
        do i=1,nst(1)                                !jd
          if(cc(i).gt.rtemp)then                     !jd
            rtemp=cc(i)                              !jd
            itemp=i                                  !jd
          end if                                     !jd
        end do                                       !jd
      end if                                         !jd
      sang1=ang1(itemp)                              !jd
      sang2=ang2(itemp)                              !jd
      sang3=ang3(itemp)                              !jd

!jd      close(lin)
!
! Find the needed parameters:
!
      MAXDIS = nxdis*nydis*nzdis
      MAXSAM = ndmax + 1
      MAXEQ = MAXSAM + MAXDT + 2
      MAXSBX = 1
!jd      if(nx.gt.1)then
!jd            MAXSBX = int(nx/2.00)
!jd            if(MAXSBX.gt.50)MAXSBX=50
!jd      end if

!!      if(ncol_range.gt.1)then                         !jd
!!            MAXSBX = int(ncol_range/2.00)             !jd
!!            if(MAXSBX.gt.50)MAXSBX=50                 !jd
!!      end if                                          !jd
!
      MAXSBY = 1
!jd      if(ny.gt.1)then
!jd            MAXSBY = int(ny/2.00)
!jd            if(MAXSBY.gt.50)MAXSBY=50
!jd      end if

!!      if(nrow_range.gt.1)then                         !jd
!!            MAXSBY = int(nrow_range/2.00)             !jd
!!            if(MAXSBY.gt.50)MAXSBY=50                 !jd
!!      end if                                          !jd
!
      MAXSBZ = 1
!jd      if(nz.gt.1)then
!jd            MAXSBZ = int(nz/2.00)
!jd            if(MAXSBZ.gt.50)MAXSBZ=50
!jd      end if
!
      MAXSB = MAXSBX*MAXSBY*MAXSBZ
      MXSXY = 4 * MAXSBX * MAXSBY
      MXSX  = 2 * MAXSBX
!
! Allocate the needed memory:
!1
      if(icall.eq.1)then
      allocate(nisb(MAXSB),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!2
      if(icall.eq.1)then
      allocate(ixsbtosr(8 * MAXSB),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!3
      if(icall.eq.1)then
      allocate(iysbtosr(8 * MAXSB),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!4
      if(icall.eq.1)then
      allocate(izsbtosr(8 * MAXSB),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!13
      if(icall.eq.1)then
      allocate(xa(MAXSAM),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!14
      if(icall.eq.1)then
      allocate(ya(MAXSAM),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!15
      if(icall.eq.1)then
      allocate(za(MAXSAM),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!16
      if(icall.eq.1)then
      allocate(vra(MAXSAM),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!17
      if(icall.eq.1)then
      allocate(vea(MAXSAM),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!18
      if(icall.eq.1)then
      allocate(xdb(MAXDIS),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!19
      if(icall.eq.1)then
      allocate(ydb(MAXDIS),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!20
      if(icall.eq.1)then
      allocate(zdb(MAXDIS),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!23
      if(icall.eq.1)then
      allocate(r(MAXEQ),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!24
      if(icall.eq.1)then
      allocate(rr(MAXEQ),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!25

      if(icall.eq.1)then
      allocate(s(MAXEQ),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!26
      if(icall.eq.1)then
      allocate(a(MAXEQ * MAXEQ),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!
! Perform some quick error checking:
!
      if(ndmax.gt.MAXSAM) stop 'ndmax is too big - modify .inc file'
      if(ktype.eq.3.and.iextv.le.0) stop 'must have external variable'
      if(ixl.le.0.and.nx.gt.1) write(*,*) ' WARNING: ixl=0 and nx>1 ! '
      if(iyl.le.0.and.ny.gt.1) write(*,*) ' WARNING: iyl=0 and ny>1 ! '
      if(izl.le.0.and.nz.gt.1) write(*,*) ' WARNING: izl=0 and nz>1 ! '
!
! Check to make sure the data file exists, then either read in the
! data or write an error message and stop:
!

!jd      inquire(file=datafl,exist=testfl)
!jd      if(.not.testfl) then
!jd            write(*,*) 'ERROR data file ',datafl,' does not exist!'
!jd            stop
!jd      endif
!
! The data file exists so open the file and read in the header
! information. Initialize the storage that will be used to summarize
! the data found in the file:
!
      title(1:22) = 'KT3D ESTIMATES WITH: '
!jd      open(lin,file=datafl,status='OLD')
!jd      read(lin,*)
!jd      read(lin,*,err=99)       nvari
!jd      do i=1,nvari
!jd            read(lin,*)
!jd      end do
!jd      MAXDAT = 0
!jd 22   read(lin,*,end=33,err=99) (var(j),j=1,nvari)
!jd      if(var(ivrl).lt.tmin.or.var(ivrl).ge.tmax) go to 22
!jd      MAXDAT = MAXDAT + 1
!jd      go to 22
!jd 33   continue
!
! Allocate the needed memory:
!5

!jd      MAXDAT=n_nd+1                                !jd

!jd      if(icall.eq.1)then
!jd      allocate(x(MAXDAT),stat = test)
!jd            if(test.ne.0)then
!jd                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
!jd                  stop
!jd            end if
!jd      end if
!6
!jd      if(icall.eq.1)then
!jd      allocate(y(MAXDAT),stat = test)
!jd            if(test.ne.0)then
!jd                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
!jd                  stop
!jd            end if
!jd      end if
!7
!jd      if(icall.eq.1)then
!jd      allocate(z(MAXDAT),stat = test)
!jd            if(test.ne.0)then
!jd                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
!jd                  stop
!jd            end if
!jd      end if

      if(icall.eq.1)then
      allocate(sec3(MAXDAT),stat=ierr)                                                    !jd
            if(test.ne.0)then                                                             !jd
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'        !jd
                  stop                                                                    !jd
            end if                                                                        !jd
      end if
!8
!jd      if(icall.eq.1)then
!jd      allocate(vr(MAXDAT),stat = test)
!jd            if(test.ne.0)then
!jd                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
!jd                  stop
!jd            end if
!jd      end if
!9

      if(icall.eq.1)then
      allocate(ve(MAXDAT),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!10
      if(icall.eq.1)then
      allocate(dh(MAXDAT),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!11
      if(icall.eq.1)then
      allocate(tmp(MAXDAT),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!12
      if(icall.eq.1)then
      allocate(close(MAXDAT),stat = test)
            if(test.ne.0)then
                  write(*,*)'ERROR: Allocation failed due to insufficient memory.'
                  stop
            end if
      end if
!
!jd      rewind(lin)
!jd      read(lin,'(a58)') title(23:80)
!jd      read(lin,*,err=99)       nvari
      nd = 0
      av = 0.0
      ss = 0.0
!jd      do i=1,nvari
!jd            read(lin,'(a40)',err=99) str
!jd      end do
!
! Some tests on column numbers:
!
!jd      if(ixl.gt.nvari.or.iyl.gt.nvari.or.izl.gt.nvari.or.ivrl.gt.nvari)
!jd     +      then
!jd            write(*,*) 'There are only ',nvari,' columns in input data'
!jd            write(*,*) '  your specification is out of range'
!jd            stop
!jd      end if
!
! Read all the data until the end of the file:
!
!jd 2    read(lin,*,end=3,err=99) (var(j),j=1,nvari)
2     continue
!jd      vrt = var(ivrl)
!jd      if(vrt.lt.tmin.or.vrt.ge.tmax) go to 2
      nd = nd + 1
      if(nd.gt.n_nd) go to 3                         !jd
!jd      if(nd.gt.MAXDAT) then
!jd            write(*,*) ' ERROR: Exceeded available memory for data'
!jd            stop
!jd      end if
!
! Establish the location of this datum:
!
!jd      if(idhl.le.0) then
!jd            dh(nd) = -99
!jd      else
!jd            dh(nd) = var(idhl)
!jd      endif
!jd      if(ixl.le.0) then
!jd            x(nd) = xmn
!jd      else
!jd            x(nd) = var(ixl)
!jd      endif
!jd      if(iyl.le.0) then
!jd            y(nd) = ymn
!jd      else
!jd            y(nd) = var(iyl)
!jd      endif
!jd      if(izl.le.0) then
!jd            z(nd) = zmn
!jd      else
!jd            z(nd) = var(izl)
!jd      endif

!jd         vrt=valdat(nd)                                 !jd
         vrt=vr(nd)                                     !jd
         dh(nd)=-99                                     !jd
         sec3(nd)=real(nd)                              !jd
!
! Establish the external drift variable (if needed):
!
      ve(nd) = 1.0
!jd      if(ktype.eq.3.or.ktype.eq.2) then
!jd            ve(nd) = var(iextv)
!jd            if(ve(nd).lt.tmin.or.ve(nd).ge.tmax) then
!jd                  write(*,*) ' External drift variable must be present at all data locations!'
!jd                  write(*,*) ' Encountered at data number ',nd
!jd                  stop
!jd            end if
!jd      end if
!jd      vr(nd) = vrt
      av     = av + vrt
      ss     = ss + vrt*vrt
      go to 2
!jd 3    close(lin)
3     continue                     !jd
      nd=n_nd
!
! Compute the averages and variances as an error check for the user:
!
      av = av / max(real(nd),1.0)
      ss =(ss / max(real(nd),1.0)) - av * av
!jd      write(*,*) 'Data for KT3D: Variable number ',ivrl
!jd      write(*,*) '  Number   = ',nd
!jd      write(*,*) '  Average  = ',av
!jd      write(*,*) '  Variance = ',ss

!       write(6,*)
!       write(6,900) nd
!900    format('   Number of pilot points for this zone     = ',i6)
!       write(6,901) av
!901    format('   Mean data value for these pilot points   = ',1pg12.5)
!       write(6,902) sqrt(max(ss,0.0))
!902    format('   Data standard deviation for these points = ',1pg12.5)
!       write(6,903)
!903    format('   Working....')

      if(nd.lt.1) then
            write(*,*) ' ERROR: there are no data'
            stop
      end if
!
! Open the debugging and output files:
!
!jd      open(ldbg,file=dbgfl,status='UNKNOWN')
!jd      open(lout,file=outfl,status='UNKNOWN')
!jd      write(lout,'(a80)') title

!jd      if(iktype.eq.0.and.koption.eq.0) then
!jd           write(lout,201) 2,nx,ny,nz
!jd           write(lout,102)
!jd 102       format('Estimate',/,'EstimationVariance')
!jd      end if
!jd      if(iktype.eq.0.and.koption.ge.1) then
!jd           write(lout,201) 7
!jd           write(lout,103)
!jd 103       format('X',/,'Y',/,'Z',/,'True',/,'Estimate',/,'EstimationVariance',/,'Error: est-true')
!jd      end if
!jd 201  format(4(1x,i4))

!jd      if(iktype.eq.1) then
!jd            if(koption.eq.0) then
!jd                  write(lout,201) ncut,nx,ny,nz
!jd            else
!jd                  write(lout,201) ncut+1
!jd            end if
!jd            do i=1,ncut
!jd                  write(lout,104) i,cut(i)
!jd 104              format('Threshold: ',i2,' = ',f12.5)
!jd            end do
!jd            if(koption.eq.1) write(lout,105)
!jd 105        format('true value')
!jd      end if
!
! Open the external drift file if needed and position it at the
! first grid node in the file:
!
!jd      if((ktype.eq.2.or.ktype.eq.3).and.koption.eq.0) then
!jd            inquire(file=extfl,exist=testfl)
!jd            if(.not.testfl) then
!jd                  write(*,*) 'ERROR file ',extfl,' does not exist!'
!jd                  stop
!jd            endif
!jd            open(lext,file=extfl,status='UNKNOWN')
!jd            read(lext,'(a40)',err=97) str
!jd            read(lext,*,err=97)       nvari
!jd            do i=1,nvari
!jd                  read(lext,'(a40)',err=97) str
!jd            end do
!jd            if(idbg.ge.3) write(ldbg,100) iextve
!jd 100        format('A secondary variable is being used.  The gridded '     &
!jd                   'file',/,'must have the same grid specifications '      &
!jd                   'as the grid you are kriging.',/,'The external '        &
!jd                   'drift variable was taken from column ',i2)
!jd      endif
!
! Set up for cross validation:
!
!jd      if(koption.eq.1) then
!jd            jackfl = datafl
!jd            idhlj  = idhl
!jd            ixlj   = ixl
!jd            iylj   = iyl
!jd            izlj   = izl
!jd            ivrlj  = ivrl
!jd            iextvj = iextv
!jd      end if
!
! Open the file with the jackknife data?
!
!jd      if(koption.gt.0) then
!jd            inquire(file=jackfl,exist=testfl)
!jd            if(.not.testfl) then
!jd                  write(*,*) 'ERROR file ',jackfl,' does not exist!'
!jd                  stop
!jd            endif
!jd            open(ljack,file=jackfl,status='OLD')
!jd            read(ljack,*,err=96)
!jd            read(ljack,*,err=96) nvarij
!jd            do i=1,nvarij
!jd                  read(ljack,*,err=96)
!jd            end do
!jd      end if
!
! Finished here:
!
      return
!
! Error in an Input File Somewhere:
!
 96   stop 'ERROR in jackknife file!'
 97   stop 'ERROR in external drift file!'
 98   stop 'ERROR in parameter file!'
 99   stop 'ERROR in data file!'
      end



      subroutine kt3d_1(MAXDIS,MAXSBX,MAXSBY,MAXSBZ,MAXEQ,unestimated,                   &
                      n_npts,n_ndat,inumdat,icellno,epoint,npoint,zpoint,                &
                      outfile,aoutfile,outunit,pmx,                                      &
                      ncol_range,nrow_range,e_min,e_max,n_min,n_max,elev_min,elev_max,   &
                      af1,af2)
!-----------------------------------------------------------------------
!
!                Krige a 3-D Grid of Rectangular Blocks
!                **************************************
!
! This subroutine estimates point or block values of one variable by
! simple, ordinary, or kriging with a trend model.  It is also possible
! to estimate the trend directly.
!
!
!
! PROGRAM NOTES:
!
!   1. The data and parameters are passed in common blocks defined
!      in kt3d.inc.  Local storage is allocated in the subroutine
!      for kriging matrices, i.e.,
!         - xa,ya,za,vra   arrays for data within search neighborhood
!         - a,r,rr,s       kriging arrays
!         - xdb,ydb,zdb    relative position of discretization points
!         - cbb            block covariance
!   2. The kriged value and the kriging variance is written to Fortran
!      unit number "lout".
!
!
!
!
! Original:  A.G. Journel and C. Lemmer                             1981
! Revisions: A.G. Journel and C. Kostov                             1984
!-----------------------------------------------------------------------

      use geostat_3d

      implicit integer(i-n), real(a-h, o-z)                          !jd


!!!!!!!!!!!!!!!!!!kt3d.inc!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      parameter(MAXDT  =   9, MAXNST =   4)
      parameter(UNEST = -999.0, EPSLON = 0.000001, MAXROT = MAXNST + 1,VERSION = 3.000)
      integer   idrif(MAXDT),nst(1),it(MAXNST),iktype,ncut,test
      real      bv(9),c0(1),cc(MAXNST),aa(MAXNST),ang1(MAXNST),	         &
                ang2(MAXNST),ang3(MAXNST),anis1(MAXNST),anis2(MAXNST)
      real*8    rotmat(MAXROT,3,3)
      common /datcom/ nd,tmin,tmax,nx,ny,nz,xmn,ymn,zmn,test,            &
                      xsiz,ysiz,zsiz,ndmax,ndmin,radius,noct,nxdis,      &
                      nydis,nzdis,idrif,itrend,ktype,skmean,koption,     &
                      idbg,ldbg,iout,lout,iext,lext,iextve,ljack,        &
                      idhlj,ixlj,iylj,izlj,ivrlj,iextvj,nvarij
      common /vargdt/ nst,it,c0,cc,aa,ang1,ang2,ang3,anis1,anis2,rotmat
      common /srccom/ sang1,sang2,sang3,sanis1,sanis2,isrot,na,ndb,bv,unbias,iktype,ncut

!!!!!!!!!!!!!!!!!!kt3d.inc!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      real*8     cbb
      real       var(20)
      logical    first,fircon,accept

      integer    n_npts,n_ndat,outunit                               !jd
      integer    inumdat(n_ndat),icellno(n_npts)                     !jd
      integer    ncol_range,nrow_range                               !jd
      real       epoint(n_npts),npoint(n_npts),zpoint(n_npts)        !jd
      real       e_min,e_max,n_min,n_max,elev_min,elev_max           !jd
      real       pmx                                                 !jd
      character*(*) outfile,aoutfile                                 !jd
      integer    unestimated                                         !jd

      character*(*)  af1,af2                                         !jd
      character*150 aformat                                          !jd

      data       fircon/.true./

! -- We work out the formatting so that we get good compression if
!    writing an ascii file.

     nd=n_ndat

     aformat='('//trim(af2)//','//trim(af1)//',1x,1pg14.7,1000('//trim(af1)//',1x,1pg14.7))' !jd

!
! Set up the rotation/anisotropy matrices that are needed for the
! variogram and search.  Also compute the maximum covariance for
! the rescaling factor:
!
!jd      write(*,*) 'Setting up rotation matrices for variogram and search'
      radsqd = radius * radius
!jd      PMX    = 999.0
      covmax = c0(1)
      do is=1,nst(1)
            call setrot_new(ang1(is),ang2(is),ang3(is),anis1(is),anis2(is),is,MAXROT,rotmat)
            if(it(is).eq.4) then
                  covmax = covmax + PMX
            else
                  covmax = covmax + cc(is)
            endif
      end do
      isrot = MAXNST + 1
      call setrot_new(sang1,sang2,sang3,sanis1,sanis2,isrot,MAXROT,rotmat)
!
! Finish computing the rescaling factor and stop if unacceptable:
!
      if(radsqd.lt.1.0) then
            resc = 2.0 * radius / max(covmax,0.0001)
      else
            resc =(4.0 * radsqd)/ max(covmax,0.0001)
      endif
      if(resc.le.0.0) then
            write(*,*) 'ERROR KT3D: The rescaling value is wrong ',resc
            write(*,*) '            Maximum covariance: ',covmax
            write(*,*) '            search radius:      ',radius
            stop
      endif
      resc = 1.0 / resc

!
! Set up for super block searching:
!
!jd      write(*,*) 'Setting up super block search strategy'
!jd      nsec = 2
         nsec = 3                                                               !jd
!jd      call setsupr(nx,xmn,xsiz,ny,ymn,ysiz,nz,zmn,zsiz,nd,x,y,z,
!jd     +             vr,tmp,nsec,ve,dh,sec3,MAXSBX,MAXSBY,MAXSBZ,nisb,
!jd     +             nxsup,xmnsup,xsizsup,nysup,ymnsup,ysizsup,nzsup,
!jd     +             zmnsup,zsizsup)

      call setsupr_jd(nx,xmn,xsiz,ny,ymn,ysiz,nz,zmn,zsiz,nd,x,y,z,  &          !jd
                   vr,tmp,nsec,ve,dh,sec3,MAXSBX,MAXSBY,MAXSBZ,nisb, &          !jd
                   nxsup,xmnsup,xsizsup,nysup,ymnsup,ysizsup,nzsup,  &          !jd
                   zmnsup,zsizsup,                                   &          !jd
                   ncol_range,nrow_range,e_min,e_max,n_min,n_max,    &          !jd
                   elev_min,elev_max)                                           !jd
      call picksup_new(nxsup,xsizsup,nysup,ysizsup,nzsup,zsizsup,        &
                   isrot,MAXROT,rotmat,radsqd,nsbtosr,ixsbtosr,      &
                   iysbtosr,izsbtosr)
!
! Compute the number of drift terms, if an external drift is being
! considered then it is one more drift term, if SK is being considered
! then we will set all the drift terms off and mdt to 0):
!
      mdt = 1
      do i=1,9
            if(ktype.eq.0.or.ktype.eq.2) idrif(i) = 0
            if(idrif(i).lt.0.or.idrif(i).gt.1) then
                  write(*,*) 'ERROR KT3D: invalid drift term',idrif(i)
                  stop
            endif
            mdt = mdt + idrif(i)
      end do
      if(ktype.eq.3) mdt = mdt + 1
      if(ktype.eq.0) mdt = 0
      if(ktype.eq.2) mdt = 0
!
! Set up the discretization points per block.  Figure out how many
! are needed, the spacing, and fill the xdb,ydb, and zdb arrays with
! the offsets relative to the block center (this only gets done once):
!
! In all cases the offsets are relative to the lower left corner.
! This is done for rescaling the drift terms in the kriging matrix.
!
      if(nxdis.lt.1) nxdis = 1
      if(nydis.lt.1) nydis = 1
      if(nzdis.lt.1) nzdis = 1
      ndb = nxdis * nydis * nzdis
      if(ndb.gt.MAXDIS) then
            write(*,*) 'ERROR KT3D: Too many discretization points',ndb
            write(*,*) '            Increase MAXDIS or lower n[xyz]dis'
            stop
      endif
      xdis = xsiz  / max(real(nxdis),1.0)
      ydis = ysiz  / max(real(nydis),1.0)
      zdis = zsiz  / max(real(nzdis),1.0)
      i    = 0
      xloc = -0.5*(xsiz+xdis)
      do ix =1,nxdis
            xloc = xloc + xdis
            yloc = -0.5*(ysiz+ydis)
            do iy=1,nydis
                  yloc = yloc + ydis
                  zloc = -0.5*(zsiz+zdis)
                  do iz=1,nzdis
                        zloc = zloc + zdis
                        i = i+1
                        xdb(i) = xloc + 0.5*xsiz
                        ydb(i) = yloc + 0.5*ysiz
                        zdb(i) = zloc + 0.5*zsiz
                  end do
            end do
      end do
!
! Initialize accumulators:
!
      nk    = 0
      xk    = 0.0
      vk    = 0.0
      xkmae = 0.0
      xkmse = 0.0
!
! Calculate Block Covariance. Check for point kriging.
!
      call cova3_jd(xdb(1),ydb(1),zdb(1),xdb(1),ydb(1),zdb(1),1,nst,MAXNST,c0,it,cc,aa,1,MAXROT,rotmat,cmax,cov,PMX)
!
! Set the ``unbias'' variable so that the matrix solution is more stable
!

      unbias = cov
      cbb    = dble(cov)
      if(ndb.gt.1) then
            cbb = 0.0
            do i=1,ndb
               do j=1,ndb
                  call cova3_jd(xdb(i),ydb(i),zdb(i),xdb(j),ydb(j),zdb(j),                  &
                     1,nst,MAXNST,c0,it,cc,aa,1,MAXROT,rotmat,cmax,cov,PMX)
                  if(i.eq.j) cov = cov - c0(1)
                  cbb = cbb + dble(cov)
               end do
            end do
            cbb = cbb/dble(real(ndb*ndb))
      end if
      if(idbg.gt.1) then
            write(ldbg,*) ' '
            write(ldbg,*) 'Block Covariance: ',cbb
            write(ldbg,*) ' '
      end if
!
! Mean values of the drift functions:
!
      do i=1,9
            bv(i) = 0.0
      end do
      do i=1,ndb
            bv(1) = bv(1) + xdb(i)
            bv(2) = bv(2) + ydb(i)
            bv(3) = bv(3) + zdb(i)
            bv(4) = bv(4) + xdb(i)*xdb(i)
            bv(5) = bv(5) + ydb(i)*ydb(i)
            bv(6) = bv(6) + zdb(i)*zdb(i)
            bv(7) = bv(7) + xdb(i)*ydb(i)
            bv(8) = bv(8) + xdb(i)*zdb(i)
            bv(9) = bv(9) + ydb(i)*zdb(i)
      end do
      do i=1,9
            bv(i) = (bv(i) / real(ndb)) * resc
      end do
!
! Report on progress from time to time:
!
      if(koption.eq.0) then
            nxy   = nx*ny
            nxyz  = nx*ny*nz
            nloop = nxyz
            irepo = max(1,min((nxyz/10),10000))
      else
            nloop = 10000000
            irepo = max(1,min((nd/10),10000))
      end if
      ddh = 0.0
!jd      write(*,*)
!jd      write(*,*) 'Working on the kriging '

      nloop=n_npts                 !jd
!
! MAIN LOOP OVER ALL THE BLOCKS IN THE GRID:
!
!jd      do index=1,nloop
      do i_ipts=1,n_npts
!jd      if((int(index/irepo)*irepo).eq.index) write(*,103) index
!jd103   format('   currently on estimate ',i9)
!
! Where are we making an estimate?
!
      if(koption.eq.0) then
!jd            iz   = int((index-1)/nxy) + 1
!jd            iy   = int((index-(iz-1)*nxy-1)/nx) + 1
!jd            ix   = index - (iz-1)*nxy - (iy-1)*nx
!jd            xloc = xmn + real(ix-1)*xsiz
!jd            yloc = ymn + real(iy-1)*ysiz
!jd            zloc = zmn + real(iz-1)*zsiz

          xloc=epoint(i_ipts)                     !jd
          yloc=npoint(i_ipts)                     !jd
          zloc=zpoint(i_ipts)                     !jd
          icell=icellno(i_ipts)                   !jd
      else
            read(ljack,*,err=96,end=2) (var(i),i=1,nvarij)
            ddh  = 0.0
            xloc = xmn
            yloc = ymn
            zloc = zmn
            true = UNEST
            secj = UNEST
            if(idhlj.gt.0)  ddh    = var(idhlj)
            if(ixlj.gt.0)   xloc   = var(ixlj)
            if(iylj.gt.0)   yloc   = var(iylj)
            if(izlj.gt.0)   zloc   = var(izlj)
            if(ivrlj.gt.0)  true   = var(ivrlj)
            if(iextvj.gt.0) extest = var(iextvj)
            if(true.lt.tmin.or.true.ge.tmax) true = UNEST
      end if

!
! Read in the external drift variable for this grid node if needed:
!
      if(ktype.eq.2.or.ktype.eq.3) then
            if(koption.eq.0) then
                  read(lext,*) (var(i),i=1,iextve)
                  extest = var(iextve)
            end if
            if(extest.lt.tmin.or.extest.ge.tmax) then
                  est  = UNEST
                  estv = UNEST
                  go to 8888
!jd                  go to 1
            end if
            resce  = covmax / max(extest,0.0001)
      endif
!
! Find the nearest samples:
!
      call srchsupr_new(xloc,yloc,zloc,radsqd,isrot,MAXROT,rotmat,nsbtosr,            &
                    ixsbtosr,iysbtosr,izsbtosr,noct,nd,x,y,z,tmp,                 &
                    nisb,nxsup,xmnsup,xsizsup,nysup,ymnsup,ysizsup,               &
                    nzsup,zmnsup,zsizsup,nclose,close,infoct)

!       write(6,*) ' nclose = ',nclose     !debug
!
! Load the nearest data in xa,ya,za,vra,vea:
!
      na = 0
      do i=1,nclose
            ind    = int(close(i)+0.5)
            accept = .true.
            if(koption.ne.0.and.(abs(x(ind)-xloc)+abs(y(ind)-yloc)+ abs(z(ind)-zloc))  &
                                 .lt.EPSLON) accept = .false.
            if(koption.ne.0.and.(abs(dh(ind)-ddh)).lt.EPSLON) accept = .false.
            if(accept) then
                  if(na.lt.ndmax) then
                        na = na + 1
!jd                        xa(na)  = x(ind) - xloc + 0.5*xsiz
!jd                        ya(na)  = y(ind) - yloc + 0.5*ysiz
!jd                        za(na)  = z(ind) - zloc + 0.5*zsiz
                        xa(na)  = x(ind) - xloc
                        ya(na)  = y(ind) - yloc
                        za(na)  = z(ind) - zloc
                        vra(na) = vr(ind)
                        vea(na) = ve(ind)
                  end if
            end if
      end do
!
! Test number of samples found:
!
      if(na.lt.ndmin) then
            est  = UNEST
            estv = UNEST
            go to 8888
!jd            go to 1
      end if
!
! Test if there are enough samples to estimate all drift terms:
!
!jd      if(na.ge.1.and.na.le.mdt) then
!jd            if(fircon) then
!jd                  write(ldbg,999)
!jd                  write(6,999)
!jd                  fircon = .false.
!jd            end if
!jd            est  = UNEST
!jd            estv = UNEST
!jd            go to 1
!jd      end if
 999  format(' Encountered a location where there were too few data ',/,     &
             ' to estimate all of the drift terms but there would be',/,     &
             ' enough data for OK or SK.   KT3D currently leaves ',/,        &
             ' these locations unestimated.',/,                              &
             ' This message is only written once - the first time.',/)
!
! There are enough samples - proceed with estimation.
!
      if(na.le.1) then
!
! Handle the situation of only one sample:
!
            call cova3_jd(xa(1),ya(1),za(1),xa(1),ya(1),za(1),1,nst,MAXNST,     &
                       c0,it,cc,aa,1,MAXROT,rotmat,cmax,cb1,PMX)
!
! Establish Right Hand Side Covariance:
!
            if(ndb.le.1) then
                  zero=0.0                                              !jd
!jd                  call cova3_jd(xa(1),ya(1),za(1),xdb(1),ydb(1),zdb(1),1,       &
!jd                       nst,MAXNST,c0,it,cc,aa,1,MAXROT,rotmat,cmax,cb,PMX)
                  call cova3_jd(xa(1),ya(1),za(1),zero,zero,zero,1,       &          !jd
                       nst,MAXNST,c0,it,cc,aa,1,MAXROT,rotmat,cmax,cb,PMX)          !jd

            else
                  cb  = 0.0
                  do i=1,ndb
                        call cova3_jd(xa(1),ya(1),za(1),xdb(i),ydb(i),          &
                                   zdb(i),1,nst,MAXNST,c0,it,cc,aa,1,        &
                                   MAXROT,rotmat,cmax,cov,PMX)
                        cb = cb + cov
                        dx = xa(1) - xdb(i)
                        dy = ya(1) - ydb(i)
                        dz = za(1) - zdb(i)
                        if((dx*dx+dy*dy+dz*dz).lt.EPSLON) cb=cb-c0(1)
                  end do
                  cb = cb / real(ndb)
            end if
!vd
!
! Early bug - always did OK in presence of one data.
!
!vd
            if(ktype.eq.2) skmean = extest
            if(ktype.eq.0.or.ktype.eq.2) then
                  wt   = cb / cb1
                  est  = wt * vra(1) + (1.0-wt) * skmean
                  estv = real(cbb) - wt*cb
            else
                  est  = vra(1)
                  estv = real(cbb) - 2.0*cb + cb1
                  wt=1.0                                         !jd
            end if

            if(ktype.eq.0)then                                    !jd
!!              rrtemp=(1.0-wt)*skmean                              !jd
              rrtemp=(1.0-wt)                                     !jd
            else                                                  !jd
              rrtemp=0.0                                          !jd
            end if                                                !jd
            if(aoutfile.eq.'f')then                               !jd
              write(outunit,aformat) icell,1,rrtemp, &            !jd
              inumdat(nint(sec3(nint(close(1))))),wt              !jd
            else                                                  !jd
              write(outunit)   icell,1,rrtemp, &                  !jd
              inumdat(nint(sec3(nint(close(1))))),wt              !jd
            end if                                                !jd

            nk   = nk + 1
            xk   = xk + est
            vk   = vk + est*est
            go to 1
      end if
!
! Go ahead and set up the OK portion of the kriging matrix:
!
      neq = mdt+na
!
! Initialize the main kriging matrix:
!
      first = .false.
      do i=1,neq*neq
            a(i) = 0.0
      end do
!
! Fill in the kriging matrix:
!
      do i=1,na
      do j=i,na
            call cova3_jd(xa(i),ya(i),za(i),xa(j),ya(j),za(j),1,nst,MAXNST,   &
                       c0,it,cc,aa,1,MAXROT,rotmat,cmax,cov,PMX)
            a(neq*(i-1)+j) = dble(cov)
            a(neq*(j-1)+i) = dble(cov)
      end do
      end do
!
! Fill in the OK unbiasedness portion of the matrix (if not doing SK):
!
      if(neq.gt.na) then
            do i=1,na
                  a(neq*(i-1)+na+1) = dble(unbias)
                  a(neq*na+i)       = dble(unbias)
            end do
      endif
!
! Set up the right hand side:
!
      do i=1,na
            if(ndb.le.1) then
                  zero=0.0
!jd                  call cova3_jd(xa(i),ya(i),za(i),xdb(1),ydb(1),zdb(1),1,     &
!jd                       nst,MAXNST,c0,it,cc,aa,1,MAXROT,rotmat,cmax,cb,PMX)
                  call cova3_jd(xa(i),ya(i),za(i),zero,zero,zero,1,     &
                       nst,MAXNST,c0,it,cc,aa,1,MAXROT,rotmat,cmax,cb,PMX)
            else
                  cb  = 0.0
                  do j=1,ndb
                        call cova3_jd(xa(i),ya(i),za(i),xdb(j),ydb(j),        &
                                   zdb(j),1,nst,MAXNST,c0,it,cc,aa,1,        &
                                   MAXROT,rotmat,cmax,cov,PMX)
                        cb = cb + cov
                        dx = xa(i) - xdb(j)
                        dy = ya(i) - ydb(j)
                        dz = za(i) - zdb(j)
                        if((dx*dx+dy*dy+dz*dz).lt.EPSLON) cb=cb-c0(1)
                  end do
                  cb = cb / real(ndb)
            end if
            r(i) = dble(cb)
      end do
      if(neq.gt.na) r(na+1) = dble(unbias)
!
! Add the additional unbiasedness constraints:
!
      im = na + 1
!
! First drift term (linear in "x"):
!
      if(idrif(1).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(xa(k)*resc)
                  a(neq*(k-1)+im) = dble(xa(k)*resc)
            end do
            r(im) = dble(bv(1))
      endif
!
! Second drift term (linear in "y"):
!
      if(idrif(2).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(ya(k)*resc)
                  a(neq*(k-1)+im) = dble(ya(k)*resc)
            end do
            r(im) = dble(bv(2))
      endif
!
! Third drift term (linear in "z"):
!
      if(idrif(3).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(za(k)*resc)
                  a(neq*(k-1)+im) = dble(za(k)*resc)
            end do
            r(im) = dble(bv(3))
      endif
!
! Fourth drift term (quadratic in "x"):
!
      if(idrif(4).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(xa(k)*xa(k)*resc)
                  a(neq*(k-1)+im) = dble(xa(k)*xa(k)*resc)
            end do
            r(im) = dble(bv(4))
      endif
!
! Fifth drift term (quadratic in "y"):
!
      if(idrif(5).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(ya(k)*ya(k)*resc)
                  a(neq*(k-1)+im) = dble(ya(k)*ya(k)*resc)
            end do
            r(im) = dble(bv(5))
      endif
!
! Sixth drift term (quadratic in "z"):
!
      if(idrif(6).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(za(k)*za(k)*resc)
                  a(neq*(k-1)+im) = dble(za(k)*za(k)*resc)
            end do
            r(im) = dble(bv(6))
      endif
!
! Seventh drift term (quadratic in "xy"):
!
      if(idrif(7).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(xa(k)*ya(k)*resc)
                  a(neq*(k-1)+im) = dble(xa(k)*ya(k)*resc)
            end do
            r(im) = dble(bv(7))
      endif
!
! Eighth drift term (quadratic in "xz"):
!
      if(idrif(8).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(xa(k)*za(k)*resc)
                  a(neq*(k-1)+im) = dble(xa(k)*za(k)*resc)
            end do
            r(im) = dble(bv(8))
      endif
!
! Ninth drift term (quadratic in "yz"):
!
      if(idrif(9).eq.1) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(ya(k)*za(k)*resc)
                  a(neq*(k-1)+im) = dble(ya(k)*za(k)*resc)
            end do
            r(im) = dble(bv(9))
      endif
!
! External drift term (specified by external variable):
!
      if(ktype.eq.3) then
            im=im+1
            do k=1,na
                  a(neq*(im-1)+k) = dble(vea(k)*resce)
                  a(neq*(k-1)+im) = dble(vea(k)*resce)
            end do
            r(im) = dble(extest*resce)
      endif
!
! Copy the right hand side to compute the kriging variance later:
!
      do k=1,neq
            rr(k) = r(k)
      end do
      kadim = neq * neq
      ksdim = neq
      nrhs  = 1
      nv    = 1
!
! If estimating the trend then reset all the right hand side terms=0.0:
!
      if(itrend.ge.1) then
            do i=1,na
                  r(i)  = 0.0
                  rr(i) = 0.0
            end do
      endif
!
! Write out the kriging Matrix if Seriously Debugging:
!
      if(idbg.eq.3) then
            write(ldbg,*) 'Estimating node index : ',ix,iy,iz
            is = 1 - neq
            do i=1,neq
                  is = 1 + (i-1)*neq
                  ie = is + neq - 1
                  write(ldbg,100) i,r(i),(a(j),j=is,ie)
 100              format('    r(',i2,') =',f7.4,'  a= ',9(10f7.4))
            end do
      endif
!
! Solve the kriging system:
!
      call ktsol_new(neq,nrhs,nv,a,r,s,ising,maxeq)
!
! Compute the solution:
!
      if(ising.ne.0) then
            write(6,*)                                                             !jd
            write(6,450) icell                                                     !jd
450         format(' *** ERROR: singular kriging matrix for cell',i5,' ***')       !jd
            write(6,451)                                                           !jd
451         format(' *** Check for coincidence of points in source CLIST ***')     !jd
            if(idbg.ge.3) write(ldbg,*) ' Singular Matrix ',ix,iy,iz
            est  = UNEST
            estv = UNEST
            go to 8888
      else
            est  = 0.0
            estv = real(cbb)
            sumw=0.0                                                 !jd
            if(ktype.eq.2) skmean = extest
            do j=1,neq
                  estv = estv - real(s(j))*rr(j)
                  if(j.le.na) then
                        if(ktype.eq.0) then
                              est = est + real(s(j))*(vra(j)-skmean)
                              sumw=sumw+real(s(j))                   !jd
                        else if(ktype.eq.2) then
                              est = est + real(s(j))*(vra(j)-vea(j))
                        else
                              est = est + real(s(j))*vra(j)
                        endif
                  endif
            end do
            if(ktype.eq.0.or.ktype.eq.2) est = est + skmean
            nk   = nk + 1
            xk   = xk + est
            vk   = vk + est*est
            if(ktype.eq.0)then                                       !jd
!!              rrtemp=(1.0-sumw)*skmean                               !jd
              rrtemp=(1.0-sumw)                                      !jd
            else                                                     !jd
              rrtemp=0.0                                             !jd
            end if                                                   !jd
            if(aoutfile.eq.'f')then                                  !jd
              write(outunit,aformat) icell,na,rrtemp, &              !jd
              ((inumdat(nint(sec3(nint(close(i))))),real(s(i))),i=1,na)         !jd
            else                                                     !jd
              write(outunit)   icell,na,rrtemp, &                    !jd
              ((inumdat(nint(sec3(nint(close(i))))),real(s(i))),i=1,na)         !jd
            end if                                                   !jd

!
! Write the kriging weights and data if debugging level is above 2:
!
            if(idbg.ge.2) then
                  write(ldbg,*) '       '
                  write(ldbg,*) 'BLOCK: ',ix,iy,iz,' at ',xloc,yloc,zloc
                  write(ldbg,*) '       '
                  if(ktype.ne.0)write(ldbg,*) '  Lagrange : ',s(na+1)*unbias
                  write(ldbg,*) '  BLOCK EST: x,y,z,vr,wt '
                  do i=1,na
                        xa(i) = xa(i) + xloc - 0.5*xsiz
                        ya(i) = ya(i) + yloc - 0.5*ysiz
                        za(i) = za(i) + zloc - 0.5*zsiz
                        write(ldbg,'(5f12.3)') xa(i),ya(i),za(i),vra(i),s(i)
                  end do
                  write(ldbg,*) '  estimate, variance  ',est,estv
            endif
      endif
!
! END OF MAIN KRIGING LOOP:
!
 1          continue
            if(iktype.eq.0) then
                  if(koption.eq.0) then
!jd                        write(lout,'(g14.8,1x,g14.8)') est,estv
                  else
                        err = UNEST
                        if(true.ne.UNEST.and.est.ne.UNEST) then
                              err=est-true
                              xkmae = xkmae + abs(err)
                              xkmse = xkmse + err*err
                        end if
!jd                        write(lout,'(7(g14.8,1x))') xloc,yloc,zloc,true,est,estv,err
                  end if
            else
!
! Work out the IK-type distribution implicit to this data configuration
! and kriging weights:
!
                  do icut=1,ncut
                        cdf(icut) = -1.0
                  end do
                  wtmin = 1.0
                  do i=1,na
                        if(s(i).lt.wtmin) wtmin = s(i)
                  end do
                  sumwt = 0.0
                  do i=1,na
                        s(i)  = s(i) - wtmin
                        sumwt = sumwt + s(i)
                  end do
                  do i=1,na
                        s(i) = s(i) / max(0.00001,sumwt)
                  end do
                  if(na.gt.1.and.sumwt.gt.0.00001) then
                        do icut=1,ncut
                              cdf(icut) = 0.0
                              do i=1,na
                                    if(vra(i).le.cut(icut))cdf(icut)=cdf(icut)+s(i)
                              end do
                        end do
                  end if
                  if(koption.eq.0) then
!jd                        write(lout,'(30(f8.4))') (cdf(i),i=1,ncut)
                  else
!jd                        write(lout,'(30(f8.4))') (cdf(i),i=1,ncut),true
                  end if
            end if
      end do
 2    continue
      if(koption.gt.0) close(ljack)
!
! Write statistics of kriged values:
!

!jd      if(nk.gt.0.and.idbg.gt.0) then
!jd            xk    = xk/real(nk)
!jd            vk    = vk/real(nk) - xk*xk
!jd            xkmae = xkmae/real(nk)
!jd            xkmse = xkmse/real(nk)
!jd            write(ldbg,105) nk,xk,vk
!jd            write(*,   105) nk,xk,vk
!jd 105        format(/,'Estimated   ',i8,' blocks ',/,
!jd     +               '  average   ',g14.8,/,'  variance  ',g14.8,/)
!jd            if(koption.ne.0) then
!jd                  write(*,106) xkmae,xkmse
!jd 106              format(/,'  mean error',g14.8,/,'  mean sqd e',g14.8)
!jd            end if
!jd      endif
!
! All finished the kriging:
!
      return
 96   stop 'ERROR in jackknife file!'

8888  unestimated=icell
      return

      end




      subroutine setsupr_jd(nx,xmn,xsiz,ny,ymn,ysiz,nz,zmn,zsiz,nd,x,y,z,     &
                         vr,tmp,nsec,sec1,sec2,sec3,MAXSBX,MAXSBY,            &
                         MAXSBZ,nisb,nxsup,xmnsup,xsizsup,nysup,ymnsup,       &
                         ysizsup,nzsup,zmnsup,zsizsup,                        &
                         ncol_range,nrow_range,e_min,e_max,n_min,n_max,       &
                         elev_min,elev_max)
!-----------------------------------------------------------------------
!
!           Establish Super Block Search Limits and Sort Data
!           *************************************************
!
! This subroutine sets up a 3-D "super block" model and orders the data
! by super block number.  The limits of the super block is set to the
! minimum and maximum limits of the grid; data outside are assigned to
! the nearest edge block.
!
! The idea is to establish a 3-D block network that contains all the
! relevant data.  The data are then sorted by their index location in
! the search network, i.e., the index location is given after knowing
! the block index in each coordinate direction (ix,iy,iz):
!          ii = (iz-1)*nxsup*nysup + (iy-1)*nxsup + ix
! An array, the same size as the number of super blocks, is constructed
! that contains the cumulative number of data in the model.  With this
! array it is easy to quickly check what data are located near any given
! location.
!
!
!
! INPUT VARIABLES:
!
!   nx,xmn,xsiz      Definition of the X grid being considered
!   ny,ymn,ysiz      Definition of the Y grid being considered
!   nz,zmn,zsiz      Definition of the Z grid being considered
!   nd               Number of data
!   x(nd)            X coordinates of the data
!   y(nd)            Y coordinates of the data
!   z(nd)            Z coordinates of the data
!   vr(nd)           Variable at each location.
!   tmp(nd)          Temporary storage to keep track of the super block
!                      index associated to each data (uses the same
!                      storage already allocated for the simulation)
!   nsec             Number of secondary variables to carry with vr
!   sec1(nd)         First secondary variable (if nsec >= 1)
!   sec2(nd)         Second secondary variable (if nsec >= 2)
!   sec3(nd)         Third secondary variable (if nsec = 3)
!   MAXSB[X,Y,Z]     Maximum size of super block network
!
!
!
! OUTPUT VARIABLES:
!
!   nisb()                Array with cumulative number of data in each
!                           super block.
!   nxsup,xmnsup,xsizsup  Definition of the X super block grid
!   nysup,ymnsup,ysizsup  Definition of the Y super block grid
!   nzsup,zmnsup,zsizsup  Definition of the Z super block grid
!
!
!
! EXTERNAL REFERENCES:
!
!   sortem           Sorting routine to sort the data
!
!
!
!-----------------------------------------------------------------------

      implicit integer(i-n), real(a-h, o-z)                    !jd

      real    x(*),y(*),z(*),vr(*),tmp(*),sec1(*),sec2(*),sec3(*)
      integer nisb(*)
      logical inflag

      integer ncol_range,nrow_range                            !jd
      real e_min,e_max,n_min,n_max,elev_min,elev_max           !jd
!
! Establish the number and size of the super blocks:
!
!jd      nxsup   = min(nx,MAXSBX)
!jd      nysup   = min(ny,MAXSBY)
      nxsup=min(ncol_range,MAXSBX)                             !jd
      nysup=min(nrow_range,MAXSBY)                             !jd
      nzsup   = min(nz,MAXSBZ)
!jd      xsizsup = real(nx)*xsiz/real(nxsup)
!jd      ysizsup = real(ny)*ysiz/real(nysup)
!jd      zsizsup = real(nz)*zsiz/real(nzsup)

      xsizsup = (e_max-e_min)*1.1/real(nxsup)                      !jd
      ysizsup = (n_max-n_min)*1.1/real(nysup)                      !jd
      zsizsup = (elev_max-elev_min)*1.1                            !jd
      if(xsizsup.le.0.0d0)xsizsup=1.0              !jd  arbitrary
      if(ysizsup.le.0.0d0)ysizsup=1.0              !jd  arbitrary
      if(zsizsup.le.0.0d0)zsizsup=1.0              !jd  arbitrary

!jd      xmnsup  = (xmn-0.5*xsiz)+0.5*xsizsup
!jd      ymnsup  = (ymn-0.5*ysiz)+0.5*ysizsup
!jd      zmnsup  = (zmn-0.5*zsiz)+0.5*zsizsup

      xmnsup  = e_min-0.05*(e_max-e_min)                           !jd
      ymnsup  = n_min-0.05*(n_max-n_min)                           !jd
      zmnsup  = elev_min-0.05*(elev_max-elev_min)                  !jd

!
! Initialize the extra super block array to zeros:
!
      do i=1,nxsup*nysup*nzsup
            nisb(i) = 0
      end do
!
! Loop over all the data assigning the data to a super block and
! accumulating how many data are in each super block:
!
      do i=1,nd
            call getindx_new(nxsup,xmnsup,xsizsup,x(i),ix,inflag)
            call getindx_new(nysup,ymnsup,ysizsup,y(i),iy,inflag)
            call getindx_new(nzsup,zmnsup,zsizsup,z(i),iz,inflag)
            ii = ix + (iy-1)*nxsup + (iz-1)*nxsup*nysup
            tmp(i)   = ii
            nisb(ii) = nisb(ii) + 1
      end do
!
! Sort the data by ascending super block number:
!
      nsort = 4 + nsec
      call sortem_new(1,nd,tmp,nsort,x,y,z,vr,sec1,sec2,sec3)
!
! Set up array nisb with the starting address of the block data:
!
      do i=1,(nxsup*nysup*nzsup-1)
            nisb(i+1) = nisb(i) + nisb(i+1)
      end do
!
! Finished:
!
      return
      end



      subroutine cova3_jd(x1,y1,z1,x2,y2,z2,ivarg,nst,MAXNST,c0,it,cc,aa,   &
                       irot,MAXROT,rotmat,cmax,cova,PMX)
!-----------------------------------------------------------------------
!
!                    Covariance Between Two Points
!                    *****************************
!
! This subroutine calculated the covariance associated with a variogram
! model specified by a nugget effect and nested varigoram structures.
! The anisotropy definition can be different for each nested structure.
!
!
!
! INPUT VARIABLES:
!
!   x1,y1,z1         coordinates of first point
!   x2,y2,z2         coordinates of second point
!   nst(ivarg)       number of nested structures (maximum of 4)
!   ivarg            variogram number (set to 1 unless doing cokriging
!                       or indicator kriging)
!   MAXNST           size of variogram parameter arrays
!   c0(ivarg)        isotropic nugget constant
!   it(i)            type of each nested structure:
!                      1. spherical model of range a;
!                      2. exponential model of parameter a;
!                           i.e. practical range is 3a
!                      3. gaussian model of parameter a;
!                           i.e. practical range is a*sqrt(3)
!                      4. power model of power a (a must be gt. 0  and
!                           lt. 2).  if linear model, a=1,c=slope.
!                      5. hole effect model
!   cc(i)            multiplicative factor of each nested structure.
!                      (sill-c0) for spherical, exponential,and gaussian
!                      slope for linear model.
!   aa(i)            parameter "a" of each nested structure.
!   irot             index of the rotation matrix for the first nested
!                    structure (the second nested structure will use
!                    irot+1, the third irot+2, and so on)
!   MAXROT           size of rotation matrix arrays
!   rotmat           rotation matrices
!
!
! OUTPUT VARIABLES:
!
!   cmax             maximum covariance
!   cova             covariance between (x1,y1,z1) and (x2,y2,z2)
!
!
!
! EXTERNAL REFERENCES: sqdist    computes anisotropic squared distance
!                      rotmat    computes rotation matrix for distance
!-----------------------------------------------------------------------

      implicit integer(i-n), real(a-h, o-z)                    !jd

!jd      parameter(PI=3.14159265,PMX=999.,EPSLON=1.e-5)
      parameter(PI=3.14159265,EPSLON=1.e-5)

      integer   nst(*),it(*)
      real      c0(*),cc(*),aa(*)
      real*8    rotmat(MAXROT,3,3),hsqd,sqdist_new

      real      PMX                                         !jd
!
! Calculate the maximum covariance value (used for zero distances and
! for power model covariance):
!
      istart = 1 + (ivarg-1)*MAXNST
      cmax   = c0(ivarg)
      do is=1,nst(ivarg)
            ist = istart + is - 1
            if(it(ist).eq.4) then
                  cmax = cmax + PMX
            else
                  cmax = cmax + cc(ist)
            endif
      end do
!
! Check for "zero" distance, return with cmax if so:
!
      hsqd = sqdist_new(x1,y1,z1,x2,y2,z2,irot,MAXROT,rotmat)
      if(real(hsqd).lt.EPSLON) then
            cova = cmax
            return
      endif
!
! Loop over all the structures:
!
      cova = 0.0
      do is=1,nst(ivarg)
            ist = istart + is - 1
!
! Compute the appropriate distance:
!
            if(ist.ne.1) then
                  ir = min((irot+is-1),MAXROT)
                  hsqd=sqdist_new(x1,y1,z1,x2,y2,z2,ir,MAXROT,rotmat)
            end if
            h = real(dsqrt(hsqd))
!
! Spherical Variogram Model?
!
            if(it(ist).eq.1) then
                  hr = h/aa(ist)
                  if(hr.lt.1.) cova=cova+cc(ist)*(1.-hr*(1.5-.5*hr*hr))
!
! Exponential Variogram Model?
!
            else if(it(ist).eq.2) then
!jd                  cova = cova + cc(ist)*exp(-3.0*h/aa(ist))
                  cova = cova + cc(ist)*exp(-1.0*h/aa(ist))                   !jd
!
! Gaussian Variogram Model?
!
            else if(it(ist).eq.3) then
!jd                  cova = cova + cc(ist)*exp(-3.*(h/aa(ist))*(h/aa(ist)))
                  cova = cova + cc(ist)*exp(-1.*(h/aa(ist))*(h/aa(ist)))      !jd
!
! Power Variogram Model?
!
            else if(it(ist).eq.4) then
                  cova = cova + cmax - cc(ist)*(h**aa(ist))
!
! Hole Effect Model?
!
            else if(it(ist).eq.5) then
!                 d = 10.0 * aa(ist)
!                 cova = cova + cc(ist)*exp(-3.0*h/d)*cos(h/aa(ist)*PI)
                  cova = cova + cc(ist)*cos(h/aa(ist)*PI)
            endif
      end do
!
! Finished:
!
      return
      end


      subroutine ktsol_new(n,ns,nv,a,b,x,ktilt,maxeq)
!-----------------------------------------------------------------------
!
! Solution of a system of linear equations by gaussian elimination with
! partial pivoting.  Several right hand side matrices and several
! variables are allowed.
!
!
!         NOTE: All input matrices must be in double precision
!
!
! INPUT/OUTPUT VARIABLES:
!
!   n                Number of equations
!   ns               Number of right hand side matrices
!   nv               Number of variables.
!   a(n*n*nv)        left hand side matrices versus columnwise.
!   b(n*ns*nv)       input right hand side matrices.
!   x(n*ns*nv)       solution matrices.
!   ktilt            indicator of singularity
!                      =  0  everything is ok.
!                      = -1 n.le.1
!                      =  k  a null pivot appeared at the kth iteration.
!   tol              used in test for null pivot. depends on machine
!                      precision and can also be set for the tolerance
!                      of an ill-defined kriging system.
!
!
!-----------------------------------------------------------------------


!jd      implicit real*8 (a-h,o-z)

      implicit integer(i-n), real*8(a-h, o-z)                    !jd

      real*8 x(maxeq),a(maxeq*maxeq),b(maxeq)
!
! Make sure there are equations to solve:
!
      if(n.le.1) then
            ktilt = -1
            return
      endif
!
! Initialization:
!
      tol   = 0.1e-10
      ktilt = 0
      ntn   = n*n
      nm1   = n-1
!
! Triangulation is done variable by variable:
!
      do iv=1,nv
!
! Indices of location in vectors a and b:
!
            nva = ntn*(iv-1)
            nvb = n*ns*(iv-1)
!
! Gaussian elimination with partial pivoting:
!
            do k=1,nm1
                  kp1 = k+1
!
! Indice of the diagonal element in the kth row:
!
                  kdiag = nva+(k-1)*n+k
!
! Find the pivot - interchange diagonal element/pivot:
!
                  npiv = kdiag
                  ipiv = k
                  i1   = kdiag
                  do i=kp1,n
                        i1 = i1+1
                        if(abs(a(i1)).gt.abs(a(npiv))) then
                              npiv = i1
                              ipiv = i
                        endif
                  end do
                  t        = a(npiv)
                  a(npiv)  = a(kdiag)
                  a(kdiag) = t
!
! Test for singularity:
!
                  if(abs(a(kdiag)).lt.tol) then
                        ktilt=k
                        return
                  endif
!
! Compute multipliers:
!
                  i1 = kdiag
                  do i=kp1,n
                        i1    = i1+1
                        a(i1) = -a(i1)/a(kdiag)
                  end do
!
! Interchange and eliminate column per column:
!
                  j1 = kdiag
                  j2 = npiv
                  do j=kp1,n
                        j1    = j1+n
                        j2    = j2+n
                        t     = a(j2)
                        a(j2) = a(j1)
                        a(j1) = t
                        i1    = j1
                        i2    = kdiag
                        do i=kp1,n
                              i1    = i1+1
                              i2    = i2+1
                              a(i1) = a(i1)+a(i2)*a(j1)
                        end do
                  end do
!
! Interchange and modify the ns right hand matrices:
!
                  i1 = nvb+ipiv
                  i2 = nvb+k
                  do i=1,ns
                        t     = b(i1)
                        b(i1) = b(i2)
                        b(i2) = t
                        j1    = i2
                        j2    = kdiag
                        do j=kp1,n
                              j1    = j1+1
                              j2    = j2+1
                              b(j1) = b(j1)+b(i2)*a(j2)
                        end do
                        i1 = i1+n
                        i2 = i2+n
                  end do
            end do
!
! Test for singularity for the last pivot:
!
            kdiag = ntn*iv
            if(abs(a(kdiag)).lt.tol) then
                  ktilt = n
                  return
            endif
      end do
!
! End of triangulation. Now, solve back variable per variable:
!
      do iv=1,nv
!
! Indices of location in vectors a and b:
!
            nva  = ntn*iv
            nvb1 = n*ns*(iv-1)+1
            nvb2 = n*ns*iv
!
! Back substitution with the ns right hand matrices:
!
            do il=1,ns
                  do k=1,nm1
                        nmk = n-k
!
! Indice of the diagonal element of the (n-k+1)th row and of
! the (n-k+1)th element of the left hand side.
!
                        kdiag = nva-(n+1)*(k-1)
                        kb    = nvb2-(il-1)*n-k+1
                        b(kb) = b(kb)/a(kdiag)
                        t     = -b(kb)
                        i1    = kb
                        i2    = kdiag
                        do i=1,nmk
                              i1    = i1-1
                              i2    = i2-1
                              b(i1) = b(i1)+a(i2)*t
                        end do
                  end do
                  kdiag = kdiag-n-1
                  kb    = kb-1
                  b(kb) = b(kb)/a(kdiag)
            end do
!
! End of back substitution:
!
      end do
!
! Restitution of the solution:
!
      itot = n*ns*nv
      do i=1,itot
            x(i) = b(i)
      end do
!
! Finished:
!
      return
      end



      subroutine srchsupr_new(xloc,yloc,zloc,radsqd,irot,MAXROT,rotmat,    &
                          nsbtosr,ixsbtosr,iysbtosr,izsbtosr,noct,nd,      &
                          x,y,z,tmp,nisb,nxsup,xmnsup,xsizsup,             &
                          nysup,ymnsup,ysizsup,nzsup,zmnsup,zsizsup,       &
                          nclose,close,infoct)
!-----------------------------------------------------------------------
!
!              Search Within Super Block Search Limits
!              ***************************************
!
!
! This subroutine searches through all the data that have been tagged in
! the super block subroutine.  The close data are passed back in the
! index array "close".  An octant search is allowed.
!
!
!
! INPUT VARIABLES:
!
!   xloc,yloc,zloc   location of point being estimated/simulated
!   radsqd           squared search radius
!   irot             index of the rotation matrix for searching
!   MAXROT           size of rotation matrix arrays
!   rotmat           rotation matrices
!   nsbtosr          Number of super blocks to search
!   ixsbtosr         X offsets for super blocks to search
!   iysbtosr         Y offsets for super blocks to search
!   izsbtosr         Z offsets for super blocks to search
!   noct             If >0 then data will be partitioned into octants
!   nd               Number of data
!   x(nd)            X coordinates of the data
!   y(nd)            Y coordinates of the data
!   z(nd)            Z coordinates of the data
!   tmp(nd)          Temporary storage to keep track of the squared
!                      distance associated with each data
!   nisb()                Array with cumulative number of data in each
!                           super block.
!   nxsup,xmnsup,xsizsup  Definition of the X super block grid
!   nysup,ymnsup,ysizsup  Definition of the X super block grid
!   nzsup,zmnsup,zsizsup  Definition of the X super block grid
!
!
!
! OUTPUT VARIABLES:
!
!   nclose           Number of close data
!   close()          Index of close data
!   infoct           Number of informed octants (only computes if
!                      performing an octant search)
!
!
!
! EXTERNAL REFERENCES:
!
!   sqdist           Computes anisotropic squared distance
!   sortem           Sorts multiple arrays in ascending order
!
!
!
!-----------------------------------------------------------------------

      implicit integer(i-n), real(a-h, o-z)                    !jd

      real    x(*),y(*),z(*),tmp(*),close(*)
      real*8  rotmat(MAXROT,3,3),hsqd,sqdist_new
      integer nisb(*),inoct(8)
      integer ixsbtosr(*),iysbtosr(*),izsbtosr(*)
      logical inflag
!
! Determine the super block location of point being estimated:
!
      call getindx_new(nxsup,xmnsup,xsizsup,xloc,ix,inflag)
      call getindx_new(nysup,ymnsup,ysizsup,yloc,iy,inflag)
      call getindx_new(nzsup,zmnsup,zsizsup,zloc,iz,inflag)
!
! Loop over all the possible Super Blocks:
!
      nclose = 0
      do 1 isup=1,nsbtosr
!
! Is this super block within the grid system:
!
            ixsup = ix + ixsbtosr(isup)
            iysup = iy + iysbtosr(isup)
            izsup = iz + izsbtosr(isup)
            if(ixsup.le.0.or.ixsup.gt.nxsup.or.                      &
               iysup.le.0.or.iysup.gt.nysup.or.			     &
               izsup.le.0.or.izsup.gt.nzsup) go to 1
!
! Figure out how many samples in this super block:
!
            ii = ixsup + (iysup-1)*nxsup + (izsup-1)*nxsup*nysup
            if(ii.eq.1) then
                  nums = nisb(ii)
                  i    = 0
            else
                  nums = nisb(ii) - nisb(ii-1)
                  i    = nisb(ii-1)
            endif
!
! Loop over all the data in this super block:
!
            do 2 ii=1,nums
                  i = i + 1
!
! Check squared distance:
!
                  hsqd = sqdist_new(xloc,yloc,zloc,x(i),y(i),z(i),irot,    &
                                MAXROT,rotmat)
                  if(real(hsqd).gt.radsqd) go to 2
!
! Accept this sample:
!
                  nclose = nclose + 1
                  close(nclose) = real(i)
                  tmp(nclose)  = real(hsqd)
 2          continue
 1    continue
!
! Sort the nearby samples by distance to point being estimated:
!
      call sortem_new(1,nclose,tmp,1,close,c,d,e,f,g,h)
!
! If we aren't doing an octant search then just return:
!
      if(noct.le.0) return
!
! PARTITION THE DATA INTO OCTANTS:
!
      do i=1,8
            inoct(i) = 0
      end do
!
! Now pick up the closest samples in each octant:
!
      nt = 8*noct
      na = 0
      do j=1,nclose
            i  = int(close(j))
            h  = tmp(j)
            dx = x(i) - xloc
            dy = y(i) - yloc
            dz = z(i) - zloc
            if(dz.lt.0.) go to 5
            iq=4
            if(dx.le.0.0 .and. dy.gt.0.0) iq=1
            if(dx.gt.0.0 .and. dy.ge.0.0) iq=2
            if(dx.lt.0.0 .and. dy.le.0.0) iq=3
            go to 6
 5          iq=8
            if(dx.le.0.0 .and. dy.gt.0.0) iq=5
            if(dx.gt.0.0 .and. dy.ge.0.0) iq=6
            if(dx.lt.0.0 .and. dy.le.0.0) iq=7
 6          continue
            inoct(iq) = inoct(iq) + 1
!
! Keep this sample if the maximum has not been exceeded:
!
            if(inoct(iq).le.noct) then
                  na = na + 1
                  close(na) = i
                  tmp(na)   = h
                  if(na.eq.nt) go to 7
            endif
      end do
!
! End of data selection. Compute number of informed octants and return:
!
 7    nclose = na
      infoct = 0
      do i=1,8
            if(inoct(i).gt.0) infoct = infoct + 1
      end do
!
! Finished:
!
      return
      end



      subroutine getindx_new(n,min,siz,loc,index,inflag)
!-----------------------------------------------------------------------
!
!     Gets the coordinate index location of a point within a grid
!     ***********************************************************
!
!
! n       number of "nodes" or "cells" in this coordinate direction
! min     origin at the center of the first cell
! siz     size of the cells
! loc     location of the point being considered
! index   output index within [1,n]
! inflag  true if the location is actually in the grid (false otherwise
!         e.g., if the location is outside then index will be set to
!         nearest boundary
!
!
!
!-----------------------------------------------------------------------
      integer   n,index
      real      min,siz,loc
      logical   inflag
!
! Compute the index of "loc":
!
      index = int( (loc-min)/siz + 1.5 )
!
! Check to see if in or out:
!
      if(index.lt.1) then
            index  = 1
            inflag = .false.
      else if(index.gt.n) then
            index  = n
            inflag = .false.
      else
            inflag = .true.
      end if
!
! Return to calling program:
!
      return
      end


      subroutine setrot_new(ang1,ang2,ang3,anis1,anis2,ind,MAXROT,rotmat)
!-----------------------------------------------------------------------
!
!              Sets up an Anisotropic Rotation Matrix
!              **************************************
!
! Sets up the matrix to transform cartesian coordinates to coordinates
! accounting for angles and anisotropy (see manual for a detailed
! definition):
!
!
! INPUT PARAMETERS:
!
!   ang1             Azimuth angle for principal direction
!   ang2             Dip angle for principal direction
!   ang3             Third rotation angle
!   anis1            First anisotropy ratio
!   anis2            Second anisotropy ratio
!   ind              matrix indicator to initialize
!   MAXROT           maximum number of rotation matrices dimensioned
!   rotmat           rotation matrices
!
!
! NO EXTERNAL REFERENCES
!
!
!-----------------------------------------------------------------------
      implicit integer(i-n), real(a-h, o-z)                    !jd

      parameter(DEG2RAD=3.141592654/180.0,EPSLON=1.e-20)
      real*8    rotmat(MAXROT,3,3),afac1,afac2,sina,sinb,sint,cosa,cosb,cost
!
! Converts the input angles to three angles which make more
!  mathematical sense:
!
!         alpha   angle between the major axis of anisotropy and the
!                 E-W axis. Note: Counter clockwise is positive.
!         beta    angle between major axis and the horizontal plane.
!                 (The dip of the ellipsoid measured positive down)
!         theta   Angle of rotation of minor axis about the major axis
!                 of the ellipsoid.
!
      if(ang1.ge.0.0.and.ang1.lt.270.0) then
            alpha = (90.0   - ang1) * DEG2RAD
      else
            alpha = (450.0  - ang1) * DEG2RAD
      endif
      beta  = -1.0 * ang2 * DEG2RAD
      theta =        ang3 * DEG2RAD
!
! Get the required sines and cosines:
!
      sina  = dble(sin(alpha))
      sinb  = dble(sin(beta))
      sint  = dble(sin(theta))
      cosa  = dble(cos(alpha))
      cosb  = dble(cos(beta))
      cost  = dble(cos(theta))
!
! Construct the rotation matrix in the required memory:
!
      afac1 = 1.0 / dble(max(anis1,EPSLON))
      afac2 = 1.0 / dble(max(anis2,EPSLON))
      rotmat(ind,1,1) =       (cosb * cosa)
      rotmat(ind,1,2) =       (cosb * sina)
      rotmat(ind,1,3) =       (-sinb)
      rotmat(ind,2,1) = afac1*(-cost*sina + sint*sinb*cosa)
      rotmat(ind,2,2) = afac1*(cost*cosa + sint*sinb*sina)
      rotmat(ind,2,3) = afac1*( sint * cosb)
      rotmat(ind,3,1) = afac2*(sint*sina + cost*sinb*cosa)
      rotmat(ind,3,2) = afac2*(-sint*cosa + cost*sinb*sina)
      rotmat(ind,3,3) = afac2*(cost * cosb)
!
! Return to calling program:
!
      return
      end




      subroutine picksup_new(nxsup,xsizsup,nysup,ysizsup,nzsup,zsizsup,    &
                         irot,MAXROT,rotmat,radsqd,nsbtosr,ixsbtosr,	   &
                         iysbtosr,izsbtosr)
!-----------------------------------------------------------------------
!
!             Establish Which Super Blocks to Search
!             **************************************
!
! This subroutine establishes which super blocks must be searched given
! that a point being estimated/simulated falls within a super block
! centered at 0,0,0.
!
!
!
! INPUT VARIABLES:
!
!   nxsup,xsizsup    Definition of the X super block grid
!   nysup,ysizsup    Definition of the Y super block grid
!   nzsup,zsizsup    Definition of the Z super block grid
!   irot             index of the rotation matrix for searching
!   MAXROT           size of rotation matrix arrays
!   rotmat           rotation matrices
!   radsqd           squared search radius
!
!
!
! OUTPUT VARIABLES:
!
!   nsbtosr          Number of super blocks to search
!   ixsbtosr         X offsets for super blocks to search
!   iysbtosr         Y offsets for super blocks to search
!   izsbtosr         Z offsets for super blocks to search
!
!
!
! EXTERNAL REFERENCES:
!
!   sqdist           Computes anisotropic squared distance
!
!
!
!-----------------------------------------------------------------------
      implicit integer(i-n), real(a-h, o-z)                    !jd

      real*8  rotmat(MAXROT,3,3),hsqd,sqdist_new,shortest
      integer ixsbtosr(*),iysbtosr(*),izsbtosr(*)
!
! MAIN Loop over all possible super blocks:
!
      nsbtosr = 0
      do i=-(nxsup-1),(nxsup-1)
      do j=-(nysup-1),(nysup-1)
      do k=-(nzsup-1),(nzsup-1)
            xo = real(i)*xsizsup
            yo = real(j)*ysizsup
            zo = real(k)*zsizsup
!
! Find the closest distance between the corners of the super blocks:
!
            shortest = 1.0e21
            do i1=-1,1
            do j1=-1,1
            do k1=-1,1
                  do i2=-1,1
                  do j2=-1,1
                  do k2=-1,1
                        if(i1.ne.0.and.j1.ne.0.and.k1.ne.0.and.          &
                           i2.ne.0.and.j2.ne.0.and.k2.ne.0) then
                              xdis = real(i1-i2)*0.5*xsizsup + xo
                              ydis = real(j1-j2)*0.5*ysizsup + yo
                              zdis = real(k1-k2)*0.5*zsizsup + zo
                              hsqd = sqdist_new(0.0,0.0,0.0,xdis,ydis,zdis,   &
                                            irot,MAXROT,rotmat)
                              if(hsqd.lt.shortest) shortest = hsqd
                        end if
                  end do
                  end do
                  end do
            end do
            end do
            end do
!
! Keep this super block if it is close enoutgh:
!
            if(real(shortest).le.radsqd) then
                  nsbtosr = nsbtosr + 1
                  ixsbtosr(nsbtosr) = i
                  iysbtosr(nsbtosr) = j
                  izsbtosr(nsbtosr) = k
            end if
      end do
      end do
      end do
!
! Finished:
!
      return
      end



      real*8 function sqdist_new(x1,y1,z1,x2,y2,z2,ind,MAXROT,rotmat)
!-----------------------------------------------------------------------
!
!    Squared Anisotropic Distance Calculation Given Matrix Indicator
!    ***************************************************************
!
! This routine calculates the anisotropic distance between two points
!  given the coordinates of each point and a definition of the
!  anisotropy.
!
!
! INPUT VARIABLES:
!
!   x1,y1,z1         Coordinates of first point
!   x2,y2,z2         Coordinates of second point
!   ind              The rotation matrix to use
!   MAXROT           The maximum number of rotation matrices dimensioned
!   rotmat           The rotation matrices
!
!
!
! OUTPUT VARIABLES:
!
!   sqdist           The squared distance accounting for the anisotropy
!                      and the rotation of coordinates (if any).
!
!
! NO EXTERNAL REFERENCES
!
!
!-----------------------------------------------------------------------
      implicit integer(i-n), real(a-h, o-z)                    !jd
      real*8 rotmat(MAXROT,3,3),cont,dx,dy,dz
!
! Compute component distance vectors and the squared distance:
!
      dx = dble(x1 - x2)
      dy = dble(y1 - y2)
      dz = dble(z1 - z2)
      sqdist_new = 0.0
      do i=1,3
            cont   = rotmat(ind,i,1) * dx      &
                   + rotmat(ind,i,2) * dy      &
                   + rotmat(ind,i,3) * dz
            sqdist_new = sqdist_new + cont * cont
      end do
      return
      end




      subroutine sortem_new(ib,ie,a,iperm,b,c,d,e,f,g,h)
!-----------------------------------------------------------------------
!
!                      Quickersort Subroutine
!                      **********************
!
! This is a subroutine for sorting a real array in ascending order. This
! is a Fortran translation of algorithm 271, quickersort, by R.S. Scowen
! in collected algorithms of the ACM.
!
! The method used is that of continually splitting the array into parts
! such that all elements of one part are less than all elements of the
! other, with a third part in the middle consisting of one element.  An
! element with value t is chosen arbitrarily (here we choose the middle
! element). i and j give the lower and upper limits of the segment being
! split.  After the split a value q will have been found such that
! a(q)=t and a(l)<=t<=a(m) for all i<=l<q<m<=j.  The program then
! performs operations on the two segments (i,q-1) and (q+1,j) as follows
! The smaller segment is split and the position of the larger segment is
! stored in the lt and ut arrays.  If the segment to be split contains
! two or fewer elements, it is sorted and another segment is obtained
! from the lt and ut arrays.  When no more segments remain, the array
! is completely sorted.
!
!
! INPUT PARAMETERS:
!
!   ib,ie        start and end index of the array to be sorteda
!   a            array, a portion of which has to be sorted.
!   iperm        0 no other array is permuted.
!                1 array b is permuted according to array a
!                2 arrays b,c are permuted.
!                3 arrays b,c,d are permuted.
!                4 arrays b,c,d,e are permuted.
!                5 arrays b,c,d,e,f are permuted.
!                6 arrays b,c,d,e,f,g are permuted.
!                7 arrays b,c,d,e,f,g,h are permuted.
!               >7 no other array is permuted.
!
!   b,c,d,e,f,g,h  arrays to be permuted according to array a.
!
! OUTPUT PARAMETERS:
!
!    a      = the array, a portion of which has been sorted.
!
!    b,c,d,e,f,g,h  =arrays permuted according to array a (see iperm)
!
! NO EXTERNAL ROUTINES REQUIRED:
!
!-----------------------------------------------------------------------
      implicit integer(i-n), real(a-h, o-z)                    !jd

      dimension a(*),b(*),c(*),d(*),e(*),f(*),g(*),h(*)
!
! The dimensions for lt and ut have to be at least log (base 2) n
!
      integer   lt(64),ut(64),i,j,k,m,p,q
!
! Initialize:
!
      j     = ie
      m     = 1
      i     = ib
      iring = iperm+1
      if (iperm.gt.7) iring=1
!
! If this segment has more than two elements  we split it
!
 10   if (j-i-1) 100,90,15
!
! p is the position of an arbitrary element in the segment we choose the
! middle element. Under certain circumstances it may be advantageous
! to choose p at random.
!
 15   p    = (j+i)/2
      ta   = a(p)
      a(p) = a(i)
      go to (21,19,18,17,16,161,162,163),iring
 163     th   = h(p)
         h(p) = h(i)
 162     tg   = g(p)
         g(p) = g(i)
 161     tf   = f(p)
         f(p) = f(i)
 16      te   = e(p)
         e(p) = e(i)
 17      td   = d(p)
         d(p) = d(i)
 18      tc   = c(p)
         c(p) = c(i)
 19      tb   = b(p)
         b(p) = b(i)
 21   continue
!
! Start at the beginning of the segment, search for k such that a(k)>t
!
      q = j
      k = i
 20   k = k+1
      if(k.gt.q)     go to 60
      if(a(k).le.ta) go to 20
!
! Such an element has now been found now search for a q such that a(q)<t
! starting at the end of the segment.
!
 30   continue
      if(a(q).lt.ta) go to 40
      q = q-1
      if(q.gt.k)     go to 30
      go to 50
!
! a(q) has now been found. we interchange a(q) and a(k)
!
 40   xa   = a(k)
      a(k) = a(q)
      a(q) = xa
      go to (45,44,43,42,41,411,412,413),iring
 413     xh   = h(k)
         h(k) = h(q)
         h(q) = xh
 412     xg   = g(k)
         g(k) = g(q)
         g(q) = xg
 411     xf   = f(k)
         f(k) = f(q)
         f(q) = xf
 41      xe   = e(k)
         e(k) = e(q)
         e(q) = xe
 42      xd   = d(k)
         d(k) = d(q)
         d(q) = xd
 43      xc   = c(k)
         c(k) = c(q)
         c(q) = xc
 44      xb   = b(k)
         b(k) = b(q)
         b(q) = xb
 45   continue
!
! Update q and search for another pair to interchange:
!
      q = q-1
      go to 20
 50   q = k-1
 60   continue
!
! The upwards search has now met the downwards search:
!
      a(i)=a(q)
      a(q)=ta
      go to (65,64,63,62,61,611,612,613),iring
 613     h(i) = h(q)
         h(q) = th
 612     g(i) = g(q)
         g(q) = tg
 611     f(i) = f(q)
         f(q) = tf
 61      e(i) = e(q)
         e(q) = te
 62      d(i) = d(q)
         d(q) = td
 63      c(i) = c(q)
         c(q) = tc
 64      b(i) = b(q)
         b(q) = tb
 65   continue
!
! The segment is now divided in three parts: (i,q-1),(q),(q+1,j)
! store the position of the largest segment in lt and ut
!
      if (2*q.le.i+j) go to 70
      lt(m) = i
      ut(m) = q-1
      i = q+1
      go to 80
 70   lt(m) = q+1
      ut(m) = j
      j = q-1
!
! Update m and split the new smaller segment
!
 80   m = m+1
      go to 10
!
! We arrive here if the segment has  two elements we test to see if
! the segment is properly ordered if not, we perform an interchange
!
 90   continue
      if (a(i).le.a(j)) go to 100
      xa=a(i)
      a(i)=a(j)
      a(j)=xa
      go to (95,94,93,92,91,911,912,913),iring
 913     xh   = h(i)
         h(i) = h(j)
         h(j) = xh
 912     xg   = g(i)
         g(i) = g(j)
         g(j) = xg
 911     xf   = f(i)
         f(i) = f(j)
         f(j) = xf
   91    xe   = e(i)
         e(i) = e(j)
         e(j) = xe
   92    xd   = d(i)
         d(i) = d(j)
         d(j) = xd
   93    xc   = c(i)
         c(i) = c(j)
         c(j) = xc
   94    xb   = b(i)
         b(i) = b(j)
         b(j) = xb
   95 continue
!
! If lt and ut contain more segments to be sorted repeat process:
!
 100  m = m-1
      if (m.le.0) go to 110
      i = lt(m)
      j = ut(m)
      go to 10
 110  continue
      return
      end



      subroutine geostat_3d_dealloc()

      use geostat_3d

      if(allocated(nisb)) deallocate(nisb,stat=ierr)
      if(allocated(ixsbtosr)) deallocate(ixsbtosr,stat=ierr)
      if(allocated(iysbtosr)) deallocate(iysbtosr,stat=ierr)
      if(allocated(izsbtosr)) deallocate(izsbtosr,stat=ierr)
      if(allocated(x)) deallocate(x,stat=ierr)
      if(allocated(y)) deallocate(y,stat=ierr)
      if(allocated(z)) deallocate(z,stat=ierr)
      if(allocated(vr)) deallocate(vr,stat=ierr)
      if(allocated(ve)) deallocate(ve,stat=ierr)
      if(allocated(dh)) deallocate(dh,stat=ierr)
      if(allocated(tmp)) deallocate(tmp,stat=ierr)
      if(allocated(close)) deallocate(close,stat=ierr)
      if(allocated(xa)) deallocate(xa,stat=ierr)
      if(allocated(ya)) deallocate(ya,stat=ierr)
      if(allocated(za)) deallocate(za,stat=ierr)
      if(allocated(vra)) deallocate(vra,stat=ierr)
      if(allocated(vea)) deallocate(vea,stat=ierr)
      if(allocated(xdb)) deallocate(xdb,stat=ierr)
      if(allocated(ydb)) deallocate(ydb,stat=ierr)
      if(allocated(zdb)) deallocate(zdb,stat=ierr)
      if(allocated(cut)) deallocate(cut,stat=ierr)
      if(allocated(cdf)) deallocate(cdf,stat=ierr)
      if(allocated(r)) deallocate(r,stat=ierr)
      if(allocated(rr)) deallocate(rr,stat=ierr)
      if(allocated(s)) deallocate(s,stat=ierr)
      if(allocated(a)) deallocate(a,stat=ierr)
      if(allocated(sec3)) deallocate(sec3,stat=ierr)

      return
      end subroutine geostat_3d_dealloc

