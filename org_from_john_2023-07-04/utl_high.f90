module high_level_utilities

public
contains

integer function uth_strucmodgrid_deallocate(igrid)

! -- This function deallocates one incidence of structured grid specifications.

       use deftypes
       use utilities
       implicit none

       integer, intent(in)   :: igrid
       integer               :: ierr

! -- Initialization

       uth_strucmodgrid_deallocate=0

! -- Do the work

       if((igrid.ge.1).and.(igrid.le.MAXSTRUCMODGRID))then
         if(strucmodgrid(igrid)%nrow.gt.0)then
           deallocate(strucmodgrid(igrid)%delr,strucmodgrid(igrid)%delc,stat=ierr)
           strucmodgrid(igrid)%ncol=0
           strucmodgrid(igrid)%nrow=0
           strucmodgrid(igrid)%nlay=0
           strucmodgrid(igrid)%name=' '
           numstrucmodgrid=numstrucmodgrid-1
         end if
       end if
9990   continue
       return

end function uth_strucmodgrid_deallocate


integer function uth_mf6modgrid_deallocate(igrid)

! -- This function deallocates one incidence of a MF6 grid specification.

       use deftypes
       use utilities
       implicit none

       integer, intent(in)   :: igrid
       integer               :: ierr

! -- Initialization

       uth_mf6modgrid_deallocate=0

! -- Do the work

       if((igrid.ge.1).and.(igrid.le.MAXMF6MODGRID))then
         if(mf6modgrid(igrid)%distype.gt.0)then
           mf6modgrid(igrid)%distype=0
           mf6modgrid(igrid)%name=' '
           nummf6modgrid=nummf6modgrid-1
           if(associated(mf6modgrid(igrid)%idomain))deallocate(mf6modgrid(igrid)%idomain,stat=ierr)
           if(associated(mf6modgrid(igrid)%icelltype))deallocate(mf6modgrid(igrid)%icelltype,stat=ierr)
           if(associated(mf6modgrid(igrid)%iavert))deallocate(mf6modgrid(igrid)%iavert,stat=ierr)
           if(associated(mf6modgrid(igrid)%javert))deallocate(mf6modgrid(igrid)%javert,stat=ierr)
           if(associated(mf6modgrid(igrid)%idomainv))deallocate(mf6modgrid(igrid)%idomainv,stat=ierr)
           if(associated(mf6modgrid(igrid)%icelltypev))deallocate(mf6modgrid(igrid)%icelltypev,stat=ierr)
           if(associated(mf6modgrid(igrid)%ivv))deallocate(mf6modgrid(igrid)%ivv,stat=ierr)
           if(associated(mf6modgrid(igrid)%ibound))deallocate(mf6modgrid(igrid)%ibound,stat=ierr)
           if(associated(mf6modgrid(igrid)%nvertcon))deallocate(mf6modgrid(igrid)%nvertcon,stat=ierr)
           if(associated(mf6modgrid(igrid)%vertconcell))deallocate(mf6modgrid(igrid)%vertconcell,stat=ierr)
           if(associated(mf6modgrid(igrid)%identvert))deallocate(mf6modgrid(igrid)%identvert,stat=ierr)
           if(associated(mf6modgrid(igrid)%ia))deallocate(mf6modgrid(igrid)%ia,stat=ierr)
           if(associated(mf6modgrid(igrid)%ja))deallocate(mf6modgrid(igrid)%ja,stat=ierr)
           if(associated(mf6modgrid(igrid)%delc))deallocate(mf6modgrid(igrid)%delc,stat=ierr)
           if(associated(mf6modgrid(igrid)%delr))deallocate(mf6modgrid(igrid)%delr,stat=ierr)
           if(associated(mf6modgrid(igrid)%botm))deallocate(mf6modgrid(igrid)%botm,stat=ierr)
           if(associated(mf6modgrid(igrid)%botmv))deallocate(mf6modgrid(igrid)%botmv,stat=ierr)
           if(associated(mf6modgrid(igrid)%vertices))deallocate(mf6modgrid(igrid)%vertices,stat=ierr)
           if(associated(mf6modgrid(igrid)%cellx))deallocate(mf6modgrid(igrid)%cellx,stat=ierr)
           if(associated(mf6modgrid(igrid)%celly))deallocate(mf6modgrid(igrid)%celly,stat=ierr)
           if(associated(mf6modgrid(igrid)%bottom))deallocate(mf6modgrid(igrid)%bottom,stat=ierr)
         end if
       end if
9990   continue
       return

end function uth_mf6modgrid_deallocate



subroutine uth_strucfactors(igrid,east,north,fac1,fac2,fac3,fac4,&
       icellno,jcellno)

! -- Subroutine uth_strucfactors calculates interpolation factors from
!    a structured grid to a point. There are four factors, one for each
!    of the four cell centres surrounding the point. When interpolation
!    takes place, system states at the respective cell centres are
!    multiplied by these factors and added together to determine the state
!    at the point.

! -- Subroutine arguments are as follows:-
!      igrid                the index of the installed structured grid
!      east, north          east and north coordinates of point
!      fac1,fac2,fac3,fac4  interpolation factors for point
!      icellno              identifier for cell in which point is situated
!      jcellno              identifier for the cell centre to the top left
!                           of the point

       use deftypes
       use utilities
       implicit none

       integer, intent(in)              :: igrid
       double precision, intent(in)     :: east,north
       double precision, intent(out)    :: fac1,fac2,fac3,fac4
       integer, intent(out)             :: icellno,jcellno

       integer          :: i,irow,icol,jrow,jcol,nrow,ncol
       double precision :: x,y,etemp,ntemp,rtemp1,rtemp2, &
                           x1,y1,x2,y2,delx,dely
       double precision, dimension(:),pointer :: delr,delc

       nrow=strucmodgrid(igrid)%nrow
       ncol=strucmodgrid(igrid)%ncol
       delr=>strucmodgrid(igrid)%delr
       delc=>strucmodgrid(igrid)%delc

! -- First the point coordinates are expressed as local grid coordinates.

       etemp=east-strucmodgrid(igrid)%e0
       ntemp=north-strucmodgrid(igrid)%n0
       x=etemp*strucmodgrid(igrid)%cosang+ntemp*strucmodgrid(igrid)%sinang
       y=ntemp*strucmodgrid(igrid)%cosang-etemp*strucmodgrid(igrid)%sinang
       if((x.lt.0.0).or.(y.gt.0.0))then
         icellno=-999
         return
       end if

! -- The location of the point within the finite-difference grid is next
!    determined.

       rtemp1=0.0
       rtemp2=delr(1)/2.0
       do i=1,ncol+1
       if(x.le.rtemp2)then
         x1=x-rtemp1
         x2=rtemp2-x
         delx=rtemp2-rtemp1
         jcol=i-1
         if(i.eq.1)then
           icol=1
         else if(i.eq.ncol+1)then
           icol=ncol
         else
           if(x1.le.delr(i-1)/2.0)then
             icol=i-1
           else
             icol=i
           end if
         end if
         go to 100
       end if
       rtemp1=rtemp2
       if(i.lt.ncol)then
         rtemp2=rtemp2+(delr(i)+delr(i+1))/2.0
       else if(i.eq.ncol) then
         rtemp2=rtemp2+delr(i)/2.0
       end if
       end do
       icellno=-999
       return

100    rtemp1=0.0
       rtemp2=-delc(1)/2.0
       do i=1,nrow+1
         if(y.ge.rtemp2)then
           y1=rtemp1-y
           y2=y-rtemp2
           dely=rtemp1-rtemp2
           jrow=i-1
           if(i.eq.1)then
             irow=1
           else if(i.eq.nrow+1)then
             irow=nrow
           else if(y1.le.delc(i-1)/2.0)then
             irow=i-1
           else
             irow=i
           end if
           go to 200
         end if
         rtemp1=rtemp2
         if(i.lt.nrow)then
           rtemp2=rtemp2-(delc(i)+delc(i+1))/2.0
         else if(i.eq.nrow) then
           rtemp2=rtemp2-delc(i)/2.0
         end if
       end do
       icellno=-999
       return

! -- The interpolation factors are calculated.

200    jcellno=(ncol+1)*jrow+jcol+1
       icellno=ncol*(irow-1)+icol
       rtemp1=1.0/delx/dely
       fac1=x2*y2*rtemp1
       fac2=x1*y2*rtemp1
       fac3=x2*y1*rtemp1
       fac4=x1*y1*rtemp1

       return

end subroutine uth_strucfactors


subroutine uth_point_interp(ncol,nrow,thresh,nointerpval,    &
       fac1,fac2,fac3,fac4,icellno,jcellno,bhead,rarray)

! -- This subroutine multiplies calculated states at grid cell centres by
!    factors determined in subroutine uth_strucfactors to implement interpolation
!    to point locations. If a point is near the edge of the grid, or borders
!    one or more dry or inactive cells, dummy heads are assigned to the
!    cell centres or pseudo cell centres surrounding the point.

! -- Arguments are as follows:-
!       ncol, nrow:     number of columns and rows in finite-difference grid
!       thresh:         abs. value threshold above which cells are inactive
!       noninterpval:   value to assign to points to which interpolation cannot take place
!       fac1,fac2,fac3,fac4:  interpolation factors from grid to point
!       icellno:        index of grid cell containing point
!       jcellno:        index of cell centre to top right of point
!       bhead:          interpolated value at point
!       rarray:         array from which interpolation takes place


       integer, intent(in)                     :: ncol,nrow
       double precision, intent(in)            :: thresh
       double precision, intent(in)            :: nointerpval
       double precision, intent(in)            :: fac1,fac2,fac3,fac4
       integer, intent(in)                     :: icellno,jcellno
       double precision, intent(out)           :: bhead
       double precision, dimension(0:ncol+1,0:nrow+1),intent(in)  :: rarray

       integer          :: i,j,irow,icol,jrow,jcol,k
       integer          :: iact(2,2)
       double precision :: head(2,2)

       if(icellno.eq.-999) then
         bhead=nointerpval
         return
       end if
       irow=(icellno-1)/ncol+1
       icol=icellno-(irow-1)*ncol
       jrow=(jcellno-1)/(ncol+1)
       jcol=jcellno-jrow*(ncol+1)-1
       if(abs(rarray(icol,irow)).gt.thresh) then
         bhead=nointerpval
         return
       end if

       head(1,1)=rarray(jcol,jrow)
       head(1,2)=rarray(jcol+1,jrow)
       head(2,1)=rarray(jcol,jrow+1)
       head(2,2)=rarray(jcol+1,jrow+1)
       do k=1,2
         do j=1,2
           iact(j,k)=1
         end do
       end do
       if(abs(head(1,1)).gt.thresh) iact(1,1)=0
       if(abs(head(1,2)).gt.thresh) iact(1,2)=0
       if(abs(head(2,1)).gt.thresh) iact(2,1)=0
       if(abs(head(2,2)).gt.thresh) iact(2,2)=0

       if(iact(1,1).eq.0)then
         if(iact(1,2).eq.0)then
           if(iact(2,1).eq.0)then
             head(1,1)=head(2,2)
             head(1,2)=head(2,2)
             head(2,1)=head(2,2)
           else if(iact(2,2).eq.0)then
             head(1,1)=head(2,1)
             head(1,2)=head(2,1)
             head(2,2)=head(2,1)
           else
             head(1,1)=head(2,1)
             head(1,2)=head(2,2)
           end if
         else if(iact(2,1).eq.0)then
           if(iact(2,2).eq.0)then
             head(1,1)=head(1,2)
             head(2,1)=head(1,2)
             head(2,2)=head(1,2)
           else
             head(1,1)=head(1,2)
             head(2,1)=head(2,2)
           end if
         else if(iact(2,2).eq.0)then
           if(icol.eq.jcol+1)then
             head(1,1)=head(1,2)
             head(2,1)=head(1,2)
             head(2,2)=head(1,2)
           else
             head(1,1)=head(2,1)
             head(1,2)=head(2,1)
             head(2,2)=head(2,1)
          end if
         else
           if(fac2+fac3.le.1.0e-6)then
             head(1,1)=(head(1,2)+head(2,1))/2.0
           else
             head(1,1)=(head(1,2)*fac2+head(2,1)*fac3)/(fac2+fac3)
           end if
         end if
       else if(iact(1,2).eq.0)then
         if(iact(2,2).eq.0)then
           if(iact(2,1).eq.0)then
             head(1,2)=head(1,1)
             head(2,1)=head(1,1)
             head(2,2)=head(1,1)
           else
             head(1,2)=head(1,1)
             head(2,2)=head(2,1)
           end if
         else if(iact(2,1).eq.0)then
           if(icol.eq.jcol)then
             head(1,2)=head(1,1)
             head(2,1)=head(1,1)
             head(2,2)=head(1,1)
           else
             head(1,1)=head(2,2)
             head(1,2)=head(2,2)
             head(2,1)=head(2,2)
           end if
         else
           if(fac1+fac4.le.1.0e-6)then
             head(1,2)=(head(1,1)+head(2,2))/2.0
           else
             head(1,2)=(head(1,1)*fac1+head(2,2)*fac4)/(fac1+fac4)
           end if
         end if
       else if(iact(2,1).eq.0)then
         if(iact(2,2).eq.0)then
           head(2,1)=head(1,1)
           head(2,2)=head(1,2)
         else
           if(fac1+fac4.le.1.0e-6)then
             head(2,1)=(head(1,1)+head(2,2))/2.0
           else
             head(2,1)=(head(1,1)*fac1+head(2,2)*fac4)/(fac1+fac4)
           end if
         end if
       else if(iact(2,2).eq.0)then
         if(fac2+fac3.le.1.0e-6)then
           head(2,2)=(head(1,2)+head(2,1))/2.0
         else
           head(2,2)=(head(1,2)*fac2+head(2,1)*fac3)/(fac2+fac3)
         end if
       end if

       bhead=head(1,1)*fac1+head(1,2)*fac2+head(2,1)*fac3+ &
       head(2,2)*fac4

       return
end subroutine uth_point_interp


end module high_level_utilities
