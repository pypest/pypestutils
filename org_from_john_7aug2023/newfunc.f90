integer (kind=c_int) function fieldgen3d_sva(                &
                              nnode,                         &
                              ec,nc,zc,                      &
                              area,height,active,            &
                              mean,var,                      &
                              ahmax,ahmin,avert,             &
                              bearing,dip,rake,              &
                              transtype,avetype,power,       &
                              ldrand,nreal,randfield)

! -- This function generates 3D stochastic fields based on a spatially varying variogram.
! -- It does this by spatial convolution using an averaging function.

       use iso_c_binding, only: c_int,c_double
       use dimvar
       use utilities
       implicit none

       integer(kind=c_int), intent(in)   :: nnode                       ! Nodes in model grid
       real(kind=c_double), intent(in)   :: ec(nnode),nc(nnode),zc(node)! x,y, z coordinates of nodes
       real(kind=c_double), intent(in)   :: area(nnode)            ! Areas of grid cells
       real(kind=c_double), intent(in)   :: height(nnode)          ! Height of grid cells
       integer(kind=c_int), intent(in)   :: active(nnode)          ! 0=inactive cell
       real(kind=c_double), intent(in)   :: mean(nnode)            ! Mean value of stochastic field
       real(kind=c_double), intent(in)   :: var(nnode)             ! Variance of stochastic field
       real(kind=c_double), intent(in)   :: ahmax(nnode),ahmin(nnode),avert(nnode) ! Ave func. correlation lengths
       real(kind=c_double), intent(in)   :: bearing(nnode)         ! Bearing of ahmax direction
       real(kind=c_double), intent(in)   :: dip(nnode)             ! Dip of ahmax direction
       real(kind=c_double), intent(in)   :: rake(nnode)            ! Rotation of ahmin direction
       integer(kind=c_int), intent(in)   :: transtype              ! Stochastic field pertains to natural(0) or log(1) props
       integer(kind=c_int), intent(in)   :: avetype                ! 1:spher,2:exp,3:gauss,4:pow
       real(kind=c_double), intent(in)   :: power                  ! Power used in power AVETYPE
       integer(kind=c_int), intent(in)   :: ldrand                 ! Leading dimension of RANDFIELD
       integer(kind=c_int), intent(in)   :: nreal                  ! Number of realisations to generate
       real(kind=c_double), intent(out)  :: randfield(ldrand,nreal)! Realisations

       integer                 :: jnode,knode,ierr
       integer                 :: ireal
       double precision        :: xi,yi,zi
       double precision        :: aavert
       double precision        :: vvar,along,amid,cosang1,sinang1,ang1
       double precision        :: den_along,den_along2,den_amid,den_amid2,den
       double precision        :: den_avert,den_avert2
       double precision        :: xd,yd,zd,xdd,ydd,zdd
       double precision        :: xdiff,ydiff,zdiff,dlong,dmid,dvert
       double precision        :: dtemp,dtemp1,dtemp2,dtemp3
       double precision        :: dlim,pidiv180
       character(len=25)       :: varname

       double precision, allocatable :: diid(:,:)

! -- Initialisation

       fieldgen3d_sva=0
       function_name='fieldgen3d_sva()'
       pidiv180=3.1415926535898d0/180.0d0
       dlim=1.0d-5

! -- Check that the random number generator has been seeded.

       if(.not.allocated(seed))then
         write(amessage,90) trim(function_name)
90       format(' The random number generator must be seeded before calling function ',a,'.')
         go to 9890
       end if

! -- Check input arguments.

       if((transtype.ne.0).and.(transtype.ne.1))then
         write(amessage,100) 'TRANSTYPE',trim(function_name)
100      format('The ',a,' argument of function ',a,' must be ',  &
         'supplied as 0 or 1.')
         go to 9890
       end if
       if((avetype.ne.1).and.(avetype.ne.2).and.(avetype.ne.3).and.(avetype.ne.4))then
         write(amessage,105)  trim(function_name)
105      format('The AVETYPE argument of function ',a,' must be ',  &
         'supplied as 1, 2, 3 or 4.')
         go to 9890
       end if
       if(avetype.eq.4)then
         if(power.le.0.0d0)then
           write(amessage,106) trim(function_name)
106        format('If AVETYPE is set to 4, then the POWER argument of function ',a,    &
           ' must be greater than zero.')
           go to 9890
         end if
       end if
       if(nnode.le.0)then
         varname='NNODE'
         go to 9000
       end if
       if(nreal.le.0)then
         varname='NREAL'
         go to 9000
       end if
       if(ldrand.lt.nreal)then
         write(amessage,107) trim(function_name)
107      format(' LDRAND must equal or exceed NNODE in call to function ',a,'.')
         go to 9890
       end if
       if(all(active.eq.0))then
         varname='ACTIVE'
         go to 9100
       end if
       do jnode=1,nnode
         if(active(jnode).ne.0)then
           if(area(jnode).le.0.0d0)then
             write(amessage,170) 'AREA'
170          format('At least one ',a,' value is zero or negative for an active model node.')
             go to 9890
           end if
           if(height(jnode).le.0.0d0)then
             write(amessage,170) 'HEIGHT'
             go to 9890
           end if
           if(var(jnode).lt.0.0d0)then
             write(amessage,171) 'VAR'
171          format('At least one ',a,' value is negative for an active model node.')
             go to 9890
           end if
           if(ahmax(jnode).le.0.0d0)then
             write(amessage,170) 'AHMAX'
             go to 9890
           end if
           if(ahmin(jnode).le.0.0d0)then
             write(amessage,170) 'AHMIN'
             go to 9890
           end if
           if(avert(jnode).le.0.0d0)then
             write(amessage,170) 'AVERT'
             go to 9890
           end if
           if((bearing(jnode).lt.-360.0d0).or.(bearing(jnode).gt.360.0d0))then
             write(amessage,180)
180          format('At least one BEARING value is less than -360 degrees or greater than ',  &
             '360 degrees for an active model node.')
             go to 9890
           end if
           if((dip(jnode).lt.-180.0d0).or.(dip(jnode).gt.180.0d0))then
             write(amessage,181)
181          format('At least one DIP value is less than -180 degrees or greater than ',  &
             '180 degrees for an active model node.')
             go to 9890
           end if
           if((rake(jnode).lt.-90.0d0).or.(rake(jnode).gt.90.0d0))then
             write(amessage,182)
182          format('At least one RAKE value is less than -90 degrees or greater than ',  &
             '90 degrees for an active model node.')
             go to 9890
           end if
         end if
       end do

! -- Allocate the array that stores random numbers.

       allocate(diid(nnode,nreal),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(utl_allocate_vector('d',1,nreal).ne.0) go to 9200

! -- Fill the diid array with random numbers.

       do ireal=1,nreal
         do jnode=1,nnode
           diid(jnode,ireal)=utl_random_normal()
         end do
       end do

! -- Evaluate limit threshold

       if(dlim.eq.0.0d0)then
         dlim=1.0d30
       else
         if(avetype.eq.2)then
           dlim=-log(dlim)
           dlim=dlim*dlim              ! applies to x^2
         else if(avetype.eq.3)then
           dlim=-log(dlim)             ! applies to x^2
         else if(avetype.eq.1)then
           dlim=1.0d0
         else if(avetype.eq.4)then
           dlim=1.0d0
         end if
       end if

! -- Do the convolution

       do jnode=1,nnode
         if(iarray(jnode).ne.0)then
           xi=xx(jnode)
           yi=yy(jnode)
           zi=zz(jnode)
           var=sill(jnode)
           along=ahmax(jnode)
           amid=ahmin(jnode)
           aavert=a_vert(jnode)
           ang1=90.0d0-bearing(jnode)
           cosang1=cos(ang1*pidiv180)
           sinang1=sin(ang1*pidiv180)
           cosang2=cos(dip(jnode)*pidiv180)
           sinang2=sin(dip(jnode)*pidiv180)
           cosang3=cos(rake(jnode)*pidiv180)
           sinang3=sin(rake(jnode)*pidiv180)
           den_along=1.0d0/along
           den_along2=den_along*den_along
           den_amid=1.0d0/amid
           den_amid2=den_amid*den_amid
           den_aavert=1.0d0/aavert
           den_avert2=den_avert*den_avert
           den=0.0d0
           dvector1=0.0d0   ! an array
           do knode=1,nnode
             if(active(knode).eq.0))cycle
             xdiff=xx(knode)-xi
             ydiff=yy(knode)-yi
             zdiff=zz(knode)-zi
             xd=xdiff*cosang1+ydiff*sinang1
             yd=-xdiff*sinang1+ydiff*cosang1
             zd=zdiff
             xdd=xd*cosang2+zd*sinang2
             ydd=yd
             zdd=-xd*sinang2+zd*cosang2
             dlong=xdd
             dmid=ydd*cosang3+zdd*sinang3
             dvert=-ydd*sinang3+zdd*cosang3
             if(transtype.eq.2)then
               dtemp1=dlong*dlong*den_along2
               if(dtemp1.gt.dlim) cycle
               dtemp2=dmid*dmid*den_amid2
               if(dtemp2.gt.dlim) cycle
               dtemp3=dvert*dvert*den_avert2
               if(dtemp3.gt.dlim) cycle
               dtemp=dtemp1+dtemp2+dtemp3
               if(dtemp.gt.dlim) cycle
               dtemp=sqrt(dtemp)
               dtemp=exp(-dtemp)
             else if(transtype.eq.3)then
               dtemp1=dlong*dlong*den_along2
               if(dtemp1.gt.dlim) cycle
               dtemp2=dmid*dmid*den_amid2
               if(dtemp2.gt.dlim) cycle
               dtemp3=dvert*dvert*den_avert2
               if(dtemp3.gt.dlim) cycle
               dtemp=dtemp1+dtemp2+dtemp3
               if(dtemp.gt.dlim) cycle
               dtemp=exp(-dtemp)
             else if(transtype.eq.1)then
               dtemp1=abs(dlong*den_along)
               if(dtemp1.gt.1.0d0)cycle
               dtemp2=abs(dmid*den_amid)
               if(dtemp2.gt.1.0d0) cycle
               dtemp3=abs(dvert*den_avert)
               if(dtemp3.gt.1.0d0) cycle
               dtemp=dtemp1*dtemp1+dtemp2*dtemp2+dtemp3*dtemp3
               dtemp=sqrt(dtemp)
               dtemp3=dtemp*dtemp*dtemp
               dtemp=1.0d0-1.5*dtemp+0.5*dtemp3
               if(dtemp.lt.0.0d0) cycle
             else if(transtype.eq.4)then
               dtemp1=abs(dlong*den_along)
               if(dtemp1.gt.1.0d0) cycle
               dtemp2=abs(dmid*den_amid)
               if(dtemp2.gt.1.0d0) cycle
               dtemp3=abs(dvert*den_avert)
               if(dtemp3.gt.1.0d0) cycle
               dtemp=dtemp1*dtemp1+dtemp2*dtemp2+dtemp3*dtemp3
               dtemp=sqrt(dtemp)
               dtemp=1.0d0-(dtemp**power)
               if(dtemp.lt.0.0d0) cycle
             end if
             dtemp=dtemp*area(knode)*height(knode)
             do ireal=1,nreal
               dvector1(ireal)=dvector1(ireal)+dtemp*diid(knode,ireal)
             end do
             den=den+dtemp*dtemp
           end do
           do ireal=1,nreal
             dtemp=dvector1(ireal)*sqrt(var/den)          ! Ensures that it has the correct variance
             if(transtype.eq.0)then
               darray(jnode,ireal)=meanarray(jnode)+dtemp
             else
               darray(jnode,ireal)=meanarray(jnode)*(10**dtemp)
             end if
           end do
         end if
       end do

       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array are supplied as zero.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory management error encountered in function ',a,'.')
       go to 9890

9890   fieldgen3d_sva=1

9900   continue
       if(allocated(diid)) deallocate(diid,stat=ierr)

       return

end function fieldgen3d_sva


