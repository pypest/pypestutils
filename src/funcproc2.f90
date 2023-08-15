integer (kind=c_int) function calc_kriging_factors_2d(npts,ecs,ncs,zns,     &
                                   mpts,ect,nct,znt,                        &
                                   vartype,krigtype,                        &
                                   aa,anis,bearing,                         &
                                   searchrad,maxpts,minpts,                 &
                                   factorfile,factorfiletype,               &
                                   icount_interp)                           &
                 bind(C,name="calc_kriging_factors_2d")
!DIR$ ATTRIBUTES DLLEXPORT :: calc_kriging_factors_2d

! -- This function calculates 2D kriging factors. Anisotropy, and anisotropy bearing
!    can be spatially variable.

       use iso_c_binding, only: c_int,c_char,c_double
       use dimvar
       use utilities
       implicit none

       integer(kind=c_int), intent(in)           :: npts                    ! number of source points
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)     ! source point coordinates
       integer(kind=c_int), intent(in)           :: zns(npts)               ! source point zones
       integer(kind=c_int), intent(in)           :: mpts                    ! number of target points
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)     ! target point coords
       integer(kind=c_int), intent(in)           :: znt(mpts)               ! target point zones
       integer(kind=c_int), intent(in)           :: vartype                 ! 1:spher,2:exp,3:gauss,4:pow
       integer(kind=c_int), intent(in)           :: krigtype                ! 0:simple,1:ordinary
       real(kind=c_double), intent(in)           :: aa(mpts)                ! variogram "a" value
       real(kind=c_double), intent(in)           :: anis(mpts)              ! variogram anisotropies
       real(kind=c_double), intent(in)           :: bearing(mpts)           ! anisotropy bearings
       real(kind=c_double), intent(in)           :: searchrad               ! search radius
       integer(kind=c_int), intent(in)           :: maxpts, minpts          ! search specifications
       character (kind=c_char,len=1), intent(in) :: factorfile(LENFILENAME) ! file for kriging factors
       integer(kind=c_int), intent(in)           :: factorfiletype          ! 0:binary, 1:text
       integer(kind=c_int), intent(out)          :: icount_interp           ! number of interp points

       integer, parameter  :: MAXZONE=20

       integer                     :: izone,numzone,iizone
       integer                     :: ipt,jpt,itemp,ierr,nb
       integer                     :: nc_npts,nc_mpts
       integer                     :: zone(MAXZONE)
       double precision            :: dtor
       double precision            :: nugget,sill,mean
       double precision            :: xmin,ymin
       character (len=20)          :: anum
       character (len=25)          :: varname
       character (len=LENFACCODE)  :: acode
       character (len=LENFILENAME) :: facfile

! -- Required by SGSIM function.

       logical                       :: lexist
       integer                       :: nnpts,iunit
       integer                       :: unestimated,idummy_num2
       integer                       :: n_nst
       integer                       :: i_it(1)
       integer                       :: minpoints,maxpoints,krtype
       integer                       :: idummy_ivector2(1)
       real                          :: pmx,sr,smean,c_c0
       real                          :: c_cc(1),a_ang(1),a_anis(1),a_aa(1)
       real                          :: dummy_rvector3(1),dummy_rvector4(1) ! PLPROC names
       character (len=1)             :: outfiletype
       character (len=10)            :: af1,af2

! -- Initialisation

       calc_kriging_factors_2d=0
       function_name='calc_kriging_factors_2d()'
       dtor=3.1415926535898d0/180.0d0
       nugget=0.0d0
       sill=1.0d0
       mean=0.0d0
       iunit=0
       unestimated=0
       icount_interp=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,factorfile,facfile)
       facfile=adjustl(facfile)

! -- Check input arguments.

       if(npts.le.0)then
         varname='NPTS'
         go to 9000
       end if
       if(mpts.le.0)then
         varname='MPTS'
         go to 9000
       end if
       if(minpts.le.0)then
         varname='MINPTS'
         go to 9000
       end if
       if(maxpts.le.0)then
         varname='MAXPTS'
         go to 9000
       end if
       if(searchrad.le.0.0d0)then
         varname='SEARCHRAD'
         go to 9000
       end if
       if(maxpts.lt.minpts) then
         write(amessage,110) trim(function_name)
110      format('MAXPTS must not be less than MINPTS in call to function ',a,'.')
         go to 9890
       end if
       if((vartype.lt.1).or.(vartype.gt.4))then
         write(amessage,115) trim(function_name)
115      format('The VARTYPE argument of function ',a,' must be 1,2,3 or 4.')
         go to 9890
       end if

       if((factorfiletype.ne.0).and.(factorfiletype.ne.1))then
         varname='FACTORFILETYPE'
         go to 9050
       end if
       if((krigtype.ne.0).and.(krigtype.ne.1))then
         varname='KRIGTYPE'
         go to 9050
       end if

       if(all(zns.eq.0))then
         varname='ZNS'
         go to 9100
       end if
       if(all(znt.eq.0))then
         varname='ZNT'
         go to 9100
       end if

! -- Build up an array of zone numbers.

       izone=1
       numzone=0
       do ipt=1,mpts
         itemp=znt(ipt)
         if(itemp.ne.0)then
           if(numzone.eq.0)then
             numzone=1
             zone(numzone)=itemp
           else
             if(utl_whichone(numzone,izone,zone,itemp).ne.0)then
               numzone=numzone+1
               if(numzone.gt.MAXZONE)then
                 call utl_num2char(MAXZONE,anum)
                 write(amessage,150) trim(anum)
150              format('A maximum of only ',a,' different zones can feature in ZNT array.')
                 go to 9890
               end if
               zone(numzone)=itemp
             end if
           end if
         end if
       end do
       if(numzone.eq.0)then
         write(amessage,151)
151      format('All zone numbers in the ZNT array are zero.')
         go to 9890
       end if

! -- Check that all zones in the target array are respected in the source array.

       do izone=1,numzone
         itemp=zone(izone)
         do ipt=1,npts
           if(zns(ipt).eq.itemp) go to 165
         end do
         call utl_num2char(itemp,anum)
         write(amessage,160) trim(anum)
160      format('Zone ',a,' from the ZNT array is not represented in the ZNS array.')
         go to 9890
165      continue
       end do

! -- Establish legitimacy of anisotropy specifications.

       do ipt=1,mpts
         if(znt(ipt).ne.0)then
           if(anis(ipt).le.0.0d0)then
             write(amessage,170)
170          format('At least one ANIS value is zero or negative at a point to ',  &
             'which interpolation is required.')
             go to 9890
           end if
           if((bearing(ipt).lt.-360.0d0).or.(bearing(ipt).gt.360.0d0))then
             write(amessage,180)
180          format('At least one BEARING value is less than -360 or greater than ',  &
             '360 at a point to which interpolation is required.')
             go to 9890
           end if
         end if
       end do

! -- Establish acode

       if(krigtype.eq.0)then
         acode='2dks'
       else if(krigtype.eq.1)then
         acode='2dko'
       end if
       acode=adjustl(acode)

! -- We open the factor file. However if a binary file is requested, and the file is present
!    it is deleted.

       iunit=utl_nextunit()
       if(factorfiletype.eq.0)then
         inquire(file=facfile,exist=lexist)
         if(lexist)then
           open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
           action='write',iostat=ierr)
           close(unit=iunit,status='delete',iostat=ierr)
         end if
         open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
         action='write',err=9150)
       else
         open(unit=iunit,file=facfile,action='write',err=9170)
       end if

! -- Write a header to the file.

       nc_npts=npts             ! just in case of problems where a file is binary
       nc_mpts=mpts
       if(factorfiletype.eq.0)then
         write(iunit,err=9150) acode
         write(iunit,err=9150) nc_npts,nc_mpts
       else
         write(iunit,220,err=9170) trim(acode)
220      format(a)
         write(iunit,230,err=9170) nc_npts,nc_mpts
230      format(2i10)
       end if

! -- Allocate some work arrays. (Use same names as PLPROC.)

       if(utl_allocate_vector('r',1,npts).ne.0) go to 9200
       if(utl_allocate_vector('r',2,npts).ne.0) go to 9200
       if(utl_allocate_vector('i',1,npts).ne.0) go to 9200

! -- Evaluate reference coords.

       xmin=1.0d300
       ymin=1.0d300
       do ipt=1,mpts
         if(ect(ipt).lt.xmin)xmin=ect(ipt)
         if(nct(ipt).lt.ymin)ymin=nct(ipt)
       end do

! -- Give value to some variables.

       krtype=krigtype
       n_nst=1
       c_c0=nugget
       i_it(1)=vartype
       c_cc(1)=sill
       pmx=10000.0
       sr=min(searchrad,1.0e15)
       smean=mean
       if(factorfiletype.eq.0)then
         outfiletype='u'
       else
         outfiletype='f'
       end if
       idummy_num2=1

       call utl_num2char(npts,anum)
       anum=adjustl(anum)
       nb=len_trim(anum)
       nb=nb+1
       call utl_num2char(nb,af1)
       af1='i'//trim(af1)
       call utl_num2char(mpts,anum)
       anum=adjustl(anum)
       nb=len_trim(anum)
       nb=nb+1
       call utl_num2char(nb,af2)
       af2='i'//trim(af2)

! -- The zone loop starts here

       icount_interp=0
       zones: do izone=1,numzone
         iizone=zone(izone)

! -- Store the source points that we need in a dedicated array.
         nnpts=0
         do ipt=1,npts
           if(zns(ipt).eq.iizone)then
             nnpts=nnpts+1
             rvector1(nnpts)=ecs(ipt)-xmin
             rvector2(nnpts)=ncs(ipt)-ymin
             ivector1(nnpts)=ipt
           end if
         end do
         maxpoints=maxpts
         if(maxpoints.gt.nnpts)maxpoints=nnpts

! -- We now cycle through the individual points to which interpolation must take place.

         targpoints: do ipt=1,mpts
           if(znt(ipt).eq.iizone)then
             icount_interp=icount_interp+1
             dummy_rvector3(1)=ect(ipt)-xmin
             dummy_rvector4(1)=nct(ipt)-ymin
             idummy_ivector2(1)=ipt
             a_aa(1)=aa(ipt)
             a_ang(1)=bearing(ipt)
             a_anis(1)=1.0/anis(ipt)
             minpoints=minpts
             if(minpoints.gt.nnpts)minpoints=nnpts
             call kb2d_1(minpoints,maxpoints,sr,krtype,smean,                &
             n_nst,c_c0,i_it,c_cc,a_ang,a_aa,a_anis,                         &
             unestimated,idummy_num2,nnpts,ivector1,idummy_ivector2,dummy_rvector3,dummy_rvector4,  &
             outfiletype,iunit,pmx,rvector1,rvector2,af1,af2)
             if(unestimated.ne.0)then
               call utl_num2char(unestimated,anum)
               write(amessage,300) trim(anum)
300            format('Interpolation cannot take place to all target points. The ',     &
               'first element for which interpolation cannot take place is element ',   &
               'number ',a,'. Check that no interpolation source points are ',          &
               'coincident. Check that the search radius is large enough. ',            &
               'If you are using a Gaussian variogram, try using another.')
               go to 9890
             end if
           end if
         end do targpoints
       end do zones
       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9050   write(amessage,9060) trim(varname),trim(function_name)
9060   format('The ',a,' argument of function ',a,' must be supplied as 0 or 1.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array are supplied as zero.')
       go to 9890

9150   write(amessage,9160) trim(facfile)
9160   format('Cannot write to binary factor file ',a,'.')
       go to 9890

9170   write(amessage,9180) trim(facfile)
9180   format('Cannot write to text factor file ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in function ',a,'.')
       go to 9890

9890   calc_kriging_factors_2d=1
       icount_interp=0

9900   continue
       if(iunit.ne.0)then
         close(unit=iunit,iostat=ierr)
       end if

       return

end function calc_kriging_factors_2d



integer (kind=c_int) function calc_kriging_factors_auto_2d(npts,ecs,ncs,zns,  &
                                   mpts,ect,nct,znt,                          &
                                   krigtype,                                  &
                                   anis,bearing,                              &
                                   factorfile,factorfiletype,                 &
                                   icount_interp)                             &
                 bind(C,name="calc_kriging_factors_auto_2d")
!DIR$ ATTRIBUTES DLLEXPORT :: calc_kriging_factors_auto_2d

! -- This function calculates 2D kriging factors. Anisotropy, and anisotropy bearing
!    can be spatially variable. Some variogram properties are determined automatically.

       use iso_c_binding, only: c_int,c_char,c_double
       use dimvar
       use utilities
       implicit none

       integer(kind=c_int), intent(in)           :: npts                    ! number of source points
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)     ! source point coordinates
       integer(kind=c_int), intent(in)           :: zns(npts)               ! source point zones
       integer(kind=c_int), intent(in)           :: mpts                    ! number of target points
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)     ! target point coords
       integer(kind=c_int), intent(in)           :: znt(mpts)               ! target point zones
       integer(kind=c_int), intent(in)           :: krigtype                ! 0:simple,1:ordinary
       real(kind=c_double), intent(in)           :: anis(mpts)              ! variogram anisotropies
       real(kind=c_double), intent(in)           :: bearing(mpts)           ! anisotropy bearings
       character (kind=c_char,len=1), intent(in) :: factorfile(LENFILENAME) ! file for kriging factors
       integer(kind=c_int), intent(in)           :: factorfiletype          ! 0:binary, 1:text
       integer(kind=c_int), intent(out)          :: icount_interp           ! number of interp points

       integer, parameter  :: MAXZONE=20

       integer                     :: i,j
       integer                     :: vartype
       integer                     :: izone,numzone,iizone
       integer                     :: ipt,itemp,ierr,nb
       integer                     :: nc_npts,nc_mpts
       integer                     :: zone(MAXZONE)
       real                        :: rtemp
       double precision            :: nugget,sill,mean
       double precision            :: xmin,ymin
       double precision            :: distmin,dist2,sum,minmindist
       double precision            :: avemindist,basedist,basedist2
       double precision            :: den,dtemp,aa,xx1,yy1
       character (len=20)          :: anum
       character (len=25)          :: varname
       character (len=LENFACCODE)  :: acode
       character (len=LENFILENAME) :: facfile

! -- Required by SGSIM function.

       logical                       :: lexist
       integer                       :: nnpts,iunit
       integer                       :: unestimated,idummy_num2
       integer                       :: n_nst
       integer                       :: i_it(1)
       integer                       :: minpoints,maxpoints,krtype
       integer                       :: idummy_ivector2(1)
       real                          :: pmx,sr,smean,c_c0
       real                          :: c_cc(1),a_ang(1),a_anis(1),a_aa(1)
       real                          :: dummy_rvector3(1),dummy_rvector4(1) ! PLPROC names
       character (len=1)             :: outfiletype
       character (len=10)            :: af1,af2

! -- Initialisation

       calc_kriging_factors_auto_2d=0
       function_name='calc_kriging_factors_auto_2d()'
       vartype=2
       nugget=0.0d0
       sill=1.0d0
       mean=0.0d0
       iunit=0
       unestimated=0
       icount_interp=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,factorfile,facfile)
       facfile=adjustl(facfile)

! -- Check input arguments.

       if(npts.le.0)then
         varname='NPTS'
         go to 9000
       end if
       if(mpts.le.0)then
         varname='MPTS'
         go to 9000
       end if
       if((factorfiletype.ne.0).and.(factorfiletype.ne.1))then
         varname='FACTORFILETYPE'
         go to 9050
       end if
       if((krigtype.ne.0).and.(krigtype.ne.1))then
         varname='KRIGTYPE'
         go to 9050
       end if

       if(all(zns.eq.0))then
         varname='ZNS'
         go to 9100
       end if
       if(all(znt.eq.0))then
         varname='ZNT'
         go to 9100
       end if

! -- Build up an array of zone numbers.

       izone=1
       numzone=0
       do ipt=1,mpts
         itemp=znt(ipt)
         if(itemp.ne.0)then
           if(numzone.eq.0)then
             numzone=1
             zone(numzone)=itemp
           else
             if(utl_whichone(numzone,izone,zone,itemp).ne.0)then
               numzone=numzone+1
               if(numzone.gt.MAXZONE)then
                 call utl_num2char(MAXZONE,anum)
                 write(amessage,150) trim(anum)
150              format('A maximum of only ',a,' different zones can feature in ZNT array.')
                 go to 9890
               end if
               zone(numzone)=itemp
             end if
           end if
         end if
       end do
       if(numzone.eq.0)then
         write(amessage,151)
151      format('All zone numbers in the ZNT array are zero.')
         go to 9890
       end if

! -- Check that all zones in the target array are respected in the source array.

       do izone=1,numzone
         itemp=zone(izone)
         do ipt=1,npts
           if(zns(ipt).eq.itemp) go to 165
         end do
         call utl_num2char(itemp,anum)
         write(amessage,160) trim(anum)
160      format('Zone ',a,' from the ZNT array is not represented in the ZNS array.')
         go to 9890
165      continue
       end do

! -- Establish legitimacy of anisotropy specifications.

       do ipt=1,mpts
         if(znt(ipt).ne.0)then
           if(anis(ipt).le.0.0d0)then
             write(amessage,170)
170          format('At least one ANIS value is zero or negative at a point to ',  &
             'which interpolation is required.')
             go to 9890
           end if
           if((bearing(ipt).lt.-360.0d0).or.(bearing(ipt).gt.360.0d0))then
             write(amessage,180)
180          format('At least one BEARING value is less than -360 or greater than ',  &
             '360 at a point to which interpolation is required.')
             go to 9890
           end if
         end if
       end do

! -- Establish acode

       if(krigtype.eq.0)then
         acode='2dks'
       else if(krigtype.eq.1)then
         acode='2dko'
       end if
       acode=adjustl(acode)

! -- We open the factor file. However if a binary file is requested, and the file is present
!    it is deleted.

       iunit=utl_nextunit()
       if(factorfiletype.eq.0)then
         inquire(file=facfile,exist=lexist)
         if(lexist)then
           open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
           action='write',iostat=ierr)
           close(unit=iunit,status='delete',iostat=ierr)
         end if
         open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
         action='write',err=9150)
       else
         open(unit=iunit,file=facfile,action='write',err=9170)
       end if

! -- Write a header to the file.

       nc_npts=npts             ! just in case of KIND problems where a file is binary
       nc_mpts=mpts
       if(factorfiletype.eq.0)then
         write(iunit,err=9150) acode
         write(iunit,err=9150) nc_npts,nc_mpts
       else
         write(iunit,220,err=9170) trim(acode)
220      format(a)
         write(iunit,230,err=9170) nc_npts,nc_mpts
230      format(2i10)
       end if

! -- Allocate some work arrays. (Use same names as PLPROC.)

       if(utl_allocate_vector('r',1,npts).ne.0) go to 9200
       if(utl_allocate_vector('r',2,npts).ne.0) go to 9200
       if(utl_allocate_vector('i',1,npts).ne.0) go to 9200
       if(utl_allocate_vector('r',3,npts).ne.0) go to 9200

! -- Evaluate reference coords.

       xmin=1.0d300
       ymin=1.0d300
       do ipt=1,mpts
         if(ect(ipt).lt.xmin)xmin=ect(ipt)
         if(nct(ipt).lt.ymin)ymin=nct(ipt)
       end do

! -- Give values to some variables.

       krtype=krigtype
       n_nst=1
       c_c0=nugget
       i_it(1)=vartype
       c_cc(1)=sill
       pmx=10000.0
       smean=mean
       if(factorfiletype.eq.0)then
         outfiletype='u'
       else
         outfiletype='f'
       end if
       idummy_num2=1

       call utl_num2char(npts,anum)
       anum=adjustl(anum)
       nb=len_trim(anum)
       nb=nb+1
       call utl_num2char(nb,af1)
       af1='i'//trim(af1)
       call utl_num2char(mpts,anum)
       anum=adjustl(anum)
       nb=len_trim(anum)
       nb=nb+1
       call utl_num2char(nb,af2)
       af2='i'//trim(af2)

! -- The zone loop starts here

       icount_interp=0
       zones: do izone=1,numzone
         iizone=zone(izone)

! -- Store the source points that we need in a dedicated array.

         nnpts=0
         do ipt=1,npts
           if(zns(ipt).eq.iizone)then
             nnpts=nnpts+1
             rvector1(nnpts)=ecs(ipt)-xmin
             rvector2(nnpts)=ncs(ipt)-ymin
             ivector1(nnpts)=ipt
           end if
         end do
         maxpoints=nnpts

! -- We find the distance between each pilot point and its closest neighbour.
! -- This is assigned to rvector3.

         if(nnpts.gt.2)then
           do i=1,nnpts
             xx1=rvector1(i)
             yy1=rvector2(i)
             distmin=1.0d300
             do j=1,nnpts
               if(j.eq.i) cycle
               dist2=(xx1-rvector1(j))*(xx1-rvector1(j))+(yy1-rvector2(j))*(yy1-rvector2(j))
               if(dist2.lt.distmin)distmin=dist2
             end do
             rvector3(i)=sqrt(distmin)
           end do
         else if(nnpts.eq.2)then
           rvector3(1)=sqrt((rvector1(1)-rvector1(2))*(rvector1(1)-rvector1(2))+    &
                            (rvector2(1)-rvector2(2))*(rvector2(1)-rvector2(2)))
           rvector3(2)=rvector3(1)
         else if(nnpts.eq.1)then
           rvector3(1)=0.0
         end if

! -- We find the minimum minimum distance between any two points and the average
!    minimum distance between any two points.

         if(npts.le.2)then
           minmindist=rvector3(1)
           avemindist=rvector3(1)
         else
           sum=0.0d0
           minmindist=huge(rtemp)
           do i=1,nnpts
             if(minmindist.gt.rvector3(i))minmindist=rvector3(i)
             sum=sum+rvector3(i)
           end do
           avemindist=sum/nnpts
         end if
         basedist=minmindist
         if(avemindist.ne.0.0)then
           if(basedist/avemindist.lt.0.01) basedist=avemindist*0.01   ! arbitrary
         end if
         basedist2=basedist*basedist

! -- We now cycle through the individual points to which interpolation must take place.

         targpoints: do ipt=1,mpts
           if(znt(ipt).eq.iizone)then

! -- For each point to which we must interpolate we assign the variogram range a value as
!    equal to either the distance to the closest pilot point or weighted average of inter-point
!    distances, with weighting being inverse distance squared, whichever is greater.
! -- Actually the weights in determining average interpoint distance are maximized to be
!    equal to the minimum distance between any two points (just so the weight doesn't
!    become infinite as we approach a point.)

             xx1=ect(ipt)-xmin
             yy1=nct(ipt)-ymin
             distmin=1.0d300
             sum=0.0d0
             den=0.0d0
             do j=1,nnpts
               dist2=(xx1-rvector1(j))*(xx1-rvector1(j))+(yy1-rvector2(j))*(yy1-rvector2(j))
               if(dist2.lt.distmin) distmin=dist2
               dtemp=dist2
               if(dtemp.lt.basedist2)dtemp=basedist2
               if(dtemp.gt.0.0)then
                 dtemp=1.0/dtemp
               else
                 dtemp=1.0e30
               end if
!               dtemp=sqrt(dtemp)
               sum=sum+rvector3(j)*dtemp
               den=den+dtemp
             end do
             aa=sum/den
             distmin=sqrt(distmin)
             if(aa.lt.distmin)aa=distmin
             a_aa(1)=aa

! -- The search radius is assigned as a 4 times the variogram value.

             sr=3.0d0*aa                             ! arbitrary
             if(sr.gt.1.0e15)sr=1.0e15
             dummy_rvector3(1)=xx1
             dummy_rvector4(1)=yy1
             icount_interp=icount_interp+1
             idummy_ivector2(1)=ipt
             a_ang(1)=bearing(ipt)
             a_anis(1)=1.0/anis(ipt)
             minpoints=1                             ! may need review
             call kb2d_1(minpoints,maxpoints,sr,krtype,smean,                &
             n_nst,c_c0,i_it,c_cc,a_ang,a_aa,a_anis,                         &
             unestimated,idummy_num2,nnpts,ivector1,idummy_ivector2,dummy_rvector3,dummy_rvector4,  &
             outfiletype,iunit,pmx,rvector1,rvector2,af1,af2)
             if(unestimated.ne.0)then
               call utl_num2char(unestimated,anum)
               write(amessage,300) trim(anum)
300            format('Interpolation cannot take place to all target points. The ',     &
               'first element for which interpolation cannot take place is element ',   &
               'number ',a,'. Check that no interpolation source points are ',          &
               'coincident. Check that the search radius is large enough and ',         &
               'that MINPTS is large enough. If you are using a Gaussian variogram, ',  &
               ' then try using another.')
               go to 9890
             end if
           end if
         end do targpoints
       end do zones
       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9050   write(amessage,9060) trim(varname),trim(function_name)
9060   format('The ',a,' argument of function ',a,' must be supplied as 0 or 1.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array are supplied as zero.')
       go to 9890

9150   write(amessage,9160) trim(facfile)
9160   format('Cannot write to binary factor file ',a,'.')
       go to 9890

9170   write(amessage,9180) trim(facfile)
9180   format('Cannot write to text factor file ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in function ',a,'.')
       go to 9890

9890   calc_kriging_factors_auto_2d=1
       icount_interp=0

9900   continue
       if(iunit.ne.0)then
         close(unit=iunit,iostat=ierr)
       end if

       return

end function calc_kriging_factors_auto_2d



integer (kind=c_int) function calc_kriging_factors_3d(            &
                                   npts,ecs,ncs,zcs,zns,          &
                                   mpts,ect,nct,zct,znt,          &
                                   krigtype,                      &
                                   nzone,zonenum,                 &
                                   vartype,                       &
                                   ahmax,ahmin,avert,             &
                                   bearing,dip,rake,              &
                                   srhmax,srhmin,srvert,          &
                                   maxpts,minpts,                 &
                                   factorfile,factorfiletype,     &
                                   icount_interp)                 &
                 bind(C,name="calc_kriging_factors_3d")
!DIR$ ATTRIBUTES DLLEXPORT :: calc_kriging_factors_3d

! -- This function calculates 3D kriging factors.

       use iso_c_binding, only: c_int,c_char,c_double
       use dimvar
       use geostat_3d, only: MAXDAT,x,y,z,vr
       use utilities
       implicit none

       integer(kind=c_int), intent(in)         :: npts                          ! number of source points
       real(kind=c_double), intent(in)         :: ecs(npts),ncs(npts),zcs(npts) ! source point coords
       integer(kind=c_int), intent(in)         :: zns(npts)                     ! source point zones
       integer(kind=c_int), intent(in)         :: mpts                          ! number of target points
       real(kind=c_double), intent(in)         :: ect(mpts),nct(mpts),zct(mpts) ! target point coords
       integer(kind=c_int), intent(in)         :: znt(mpts)                     ! target point zones
       integer(kind=c_int), intent(in)         :: krigtype                      ! 0:simple,1:ordinary
       integer(kind=c_int), intent(in)         :: nzone                         ! number of zones
       integer(kind=c_int), intent(in)         :: zonenum(nzone)                ! zone numbers
       integer(kind=c_int), intent(in)         :: vartype(nzone)                ! 1:spher,2:exp,3:gauss,4:pow
       real(kind=c_double), intent(in)         :: ahmax(nzone),ahmin(nzone),avert(nzone)  ! variogram "a" values in 3 orthog dirns
       real(kind=c_double), intent(in)         :: bearing(nzone)                ! bearing of hmax
       real(kind=c_double), intent(in)         :: dip(nzone)                    ! dip of hmax
       real(kind=c_double), intent(in)         :: rake(nzone)                   ! twist about hmax axis
       real(kind=c_double), intent(in)         :: srhmax                        ! search radius in hmax dirn
       real(kind=c_double), intent(in)         :: srhmin                        ! search radius in hmin dirn
       real(kind=c_double), intent(in)         :: srvert                        ! search radius in vert dirn
       integer(kind=c_int), intent(in)         :: maxpts, minpts                ! search specifications
       character (kind=c_char,len=1), intent(in) :: factorfile(LENFILENAME)     ! file for kriging factors
       integer(kind=c_int), intent(in)         :: factorfiletype                ! 0:binary, 1:text
       integer(kind=c_int), intent(out)        :: icount_interp                 ! number of interp points

       integer                     :: izone,iizone,lastzone
       integer                     :: ipt,jpt,itemp,ierr,nb
       integer                     :: nc_npts,nc_mpts
       double precision            :: nugget,sill,mean
       double precision            :: xmin,ymin,zmin,dtemp
       character (len=20)          :: anum
       character (len=25)          :: varname
       character (len=LENFACCODE)  :: acode
       character (len=LENFILENAME) :: facfile

! -- Required by SGSIM function.

       logical                       :: lexist
       integer                       :: nnpts,mmpts,iunit
       integer                       :: unestimated
       integer                       :: n_nst
       integer                       :: i_it(1)
       integer                       :: minpoints,maxpoints,krtype
       integer                       :: ncol_range,nrow_range
       integer                       :: iparmcall
       integer                       :: MAXDIS,MAXSBX,MAXSBY,MAXSBZ,MAXEQ
       real                          :: pmx,sr,s_skmean,c_c0
       real                          :: emin,emax,nmin,nmax,elevmin,elevmax
       real                          :: sradmax_hmax,sradmax_hmin,sradmax_vert
       real                          :: c_cc(1)
       real                          :: a_ang1(1),a_ang2(1),a_ang3(1)
       real                          :: a_a_hmax(1),a_a_hmin(1),a_a_vert(1)
       character (len=1)             :: aoutfile
       character (len=10)            :: af1,af2

! -- Initialisation

       calc_kriging_factors_3d=0
       function_name='calc_kriging_factors_3d()'
       nugget=0.0d0
       sill=1.0d0
       mean=0.0d0
       iunit=0
       unestimated=0
       icount_interp=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,factorfile,facfile)
       facfile=adjustl(facfile)

! -- Check input arguments.

       if(npts.le.0)then
         varname='NPTS'
         go to 9000
       end if
       if(mpts.le.0)then
         varname='MPTS'
         go to 9000
       end if
       if(minpts.le.0)then
         varname='MINPTS'
         go to 9000
       end if
       if(maxpts.le.0)then
         varname='MAXPTS'
         go to 9000
       end if
       if(maxpts.lt.minpts) then
         write(amessage,110) trim(function_name)
110      format('MAXPTS must not be less than MINPTS in call to function ',a,'.')
         go to 9890
       end if
       do izone=1,nzone
         if(zonenum(izone).ne.0)then
           if(ahmax(izone).le.0.0d0)then
             varname='AHMAX'
             go to 9070
           end if
         end if
       end do
       do izone=1,nzone
         if(zonenum(izone).ne.0)then
           if(ahmin(izone).le.0.0d0)then
             varname='AHMIN'
             go to 9070
           end if
         end if
       end do
       do izone=1,nzone
         if(zonenum(izone).ne.0)then
           if(avert(izone).le.0.0d0)then
             varname='AVERT'
             go to 9070
           end if
         end if
       end do
       if(srhmax.le.0.0d0)then
         varname='SRHMAX'
         go to 9000
       end if
       if(srhmin.le.0.0d0)then
         varname='SRHMIN'
         go to 9000
       end if
       if(srvert.le.0.0d0)then
         varname='SRVERT'
         go to 9000
       end if
       do izone=1,nzone
         if(zonenum(izone).ne.0)then
           if((vartype(izone).lt.1).or.(vartype(izone).gt.4))then
             call utl_num2char(zonenum(izone),anum)
             write(amessage,115) trim(anum)
115          format('The value of VARTYPE supplied for zone ',a,' must be 1,2,3 or 4.')
             go to 9890
           end if
         end if
       end do
       if((krigtype.ne.0).and.(krigtype.ne.1))then
         varname='KRIGTYPE'
         go to 9050
       end if

       if((factorfiletype.ne.0).and.(factorfiletype.ne.1))then
         varname='FACTORFILETYPE'
         go to 9050
       end if

       if(all(zns.eq.0))then
         varname='ZNS'
         go to 9100
       end if
       if(all(znt.eq.0))then
         varname='ZNT'
         go to 9100
       end if

       do izone=1,nzone
         if(zonenum(izone).ne.0)then
           if((bearing(izone).lt.-360.0d0).or.(bearing(izone).gt.360.0d0))then
             call utl_num2char(zonenum(izone),anum)
             write(amessage,180) trim(anum)
180          format('The value of BEARING for zone ',a,' must be greater than -360 ',  &
             'degrees and less than 360 degrees.')
             go to 9890
           end if
         end if
       end do
       do izone=1,nzone
         if(zonenum(izone).ne.0)then
           if((dip(izone).lt.-180.0d0).or.(dip(izone).gt.180.0d0))then
             call utl_num2char(zonenum(izone),anum)
             write(amessage,181) trim(anum)
181          format('The value of DIP for zone ',a,' must be greater than -180 degrees ',  &
            'and less than 180 degrees.')
             go to 9890
           end if
         end if
       end do
       do izone=1,nzone
         if(zonenum(izone).ne.0)then
           if((rake(izone).lt.-90.0d0).or.(rake(izone).gt.90.0d0))then
             call utl_num2char(zonenum(izone),anum)
             write(amessage,182) trim(anum)
182          format('The value of RAKE for zone ',a,' must be greater than -90 degrees ', &
             'and less than 90 degrees.')
             go to 9890
           end if
         endif
       end do

! -- Ensure that all zones are present in source and target arrays.

       lastzone=0
       izone=0
       do ipt=1,npts
         itemp=zns(ipt)
         if(itemp.ne.0)then
           if(itemp.ne.lastzone)then
             if(utl_whichone(nzone,izone,zonenum,itemp).ne.0)then
               call utl_num2char(itemp,anum)
               write(amessage,190) trim(anum),'ZNS','ZONENUM'
190            format('Zone ',a,' features in the ',a,' array, but does not ',  &
               'feature in the ',a,' array.')
               go to 9890
             end if
             lastzone=itemp
           end if
         end if
       end do
       lastzone=0
       do ipt=1,mpts
         itemp=znt(ipt)
         if(itemp.ne.0)then
           if(itemp.ne.lastzone)then
             if(utl_whichone(nzone,izone,zonenum,itemp).ne.0)then
               call utl_num2char(itemp,anum)
               write(amessage,190) trim(anum),'ZNT','ZONENUM'
               go to 9890
             end if
             lastzone=itemp
           end if
         end if
       end do
       do izone=1,nzone
         itemp=zonenum(izone)
         if(itemp.ne.0)then
           do ipt=1,npts
             if(zns(ipt).eq.itemp) go to 200
           end do
           call utl_num2char(itemp,anum)
           write(amessage,190) trim(anum),'ZONENUM','ZNS'
           go to 9890
200        continue
           do ipt=1,mpts
             if(znt(ipt).eq.itemp) go to 210
           end do
           call utl_num2char(itemp,anum)
           write(amessage,190) trim(anum),'ZONENUM','ZNT'
           go to 9890
210        continue
         end if
       end do

! -- Establish acode

       if(krigtype.eq.0)then
         acode='3dks'
       else if(krigtype.eq.1)then
         acode='3dko'
       end if
       acode=adjustl(acode)

! -- We open the factor file. However if a binary file is requested, and the file is present
!    it is deleted.

       iunit=utl_nextunit()
       if(factorfiletype.eq.0)then
         inquire(file=facfile,exist=lexist)
         if(lexist)then
           open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
           action='write',iostat=ierr)
           close(unit=iunit,status='delete',iostat=ierr)
         end if
         open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
         action='write',err=9150)
       else
         open(unit=iunit,file=facfile,action='write',err=9170)
       end if

! -- Write a header to the file.

       nc_npts=npts             ! just in case of problems where a file is binary
       nc_mpts=mpts
       if(factorfiletype.eq.0)then
         write(iunit,err=9150) acode
         write(iunit,err=9150) nc_npts,nc_mpts
       else
         write(iunit,220,err=9170) trim(acode)
220      format(a)
         write(iunit,230,err=9170) nc_npts,nc_mpts
230      format(2i10)
       end if

! -- Allocate some work arrays. (Use same names as PLPROC.)

       if(utl_allocate_vector('r',1,mpts).ne.0) go to 9200
       if(utl_allocate_vector('r',2,mpts).ne.0) go to 9200
       if(utl_allocate_vector('r',3,mpts).ne.0) go to 9200
       if(utl_allocate_vector('i',1,npts).ne.0) go to 9200
       if(utl_allocate_vector('i',2,mpts).ne.0) go to 9200
       MAXDAT=npts+1
       allocate(x(MAXDAT),y(MAXDAT),z(MAXDAT),vr(MAXDAT),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- Evaluate reference coords.

       xmin=1.0d300
       ymin=1.0d300
       zmin=1.0d300
       do ipt=1,mpts
         if(znt(ipt).ne.0)then
           if(ect(ipt).lt.xmin)xmin=ect(ipt)
           if(nct(ipt).lt.ymin)ymin=nct(ipt)
           if(zct(ipt).lt.zmin)zmin=zct(ipt)
         end if
       end do

! -- Give value to some variables.

       krtype=krigtype
       n_nst=1
       c_c0=nugget
       c_cc(1)=sill
       dtemp=mpts
       ncol_range=sqrt(dtemp*0.5)
       if(ncol_range.lt.1)ncol_range=1
       nrow_range=sqrt(dtemp*0.5)
       if(nrow_range.lt.1)nrow_range=1
       iparmcall=1
       pmx=10000.0
       if(factorfiletype.eq.0)then
         aoutfile='u'
       else
         aoutfile='f'
       end if
       emin=1.0e35
       emax=-1.0e35
       nmin=1.0e35
       nmax=-1.0e35
       elevmin=1.0e35
       elevmax=-1.0e35
       sradmax_hmax=min(srhmax,1.0e15)
       sradmax_hmin=min(srhmin,1.0e15)
       sradmax_vert=min(srvert,1.0e15)
       s_skmean=0.0                                   ! a dummy value
       minpoints=minpts

! -- Evaluate formats for factor file storage

       call utl_num2char(npts,anum)
       anum=adjustl(anum)
       nb=len_trim(anum)
       nb=nb+1
       call utl_num2char(nb,af1)
       af1='i'//trim(af1)
       call utl_num2char(mpts,anum)
       anum=adjustl(anum)
       nb=len_trim(anum)
       nb=nb+1
       call utl_num2char(nb,af2)
       af2='i'//trim(af2)

! -- The zone loop starts here

       icount_interp=0
       zones: do izone=1,nzone
         iizone=zonenum(izone)
         if(iizone.eq.0) cycle

! -- Store some variables.

         i_it(1)=vartype(izone)
         a_ang1(1)=bearing(izone)
         a_ang2(1)=dip(izone)
         a_ang3(1)=rake(izone)
         a_a_hmax(1)=ahmax(izone)
         a_a_hmin(1)=ahmin(izone)
         a_a_vert(1)=avert(izone)

! -- Store the source and target points for the pertinent zone in dedicated arrays.

         mmpts=0
         do ipt=1,mpts
           if(znt(ipt).eq.iizone)then
             mmpts=mmpts+1
             icount_interp=icount_interp+1
             rvector1(mmpts)=ect(ipt)-xmin
             rvector2(mmpts)=nct(ipt)-ymin
             rvector3(mmpts)=zct(ipt)-zmin
             ivector2(mmpts)=ipt
           end if
         end do
         nnpts=0
         do ipt=1,npts
           if(zns(ipt).eq.iizone)then
             nnpts=nnpts+1
             x(nnpts)=ecs(ipt)-xmin
             y(nnpts)=ncs(ipt)-ymin
             z(nnpts)=zcs(ipt)-zmin
             ivector1(nnpts)=ipt
           end if
         end do
         maxpoints=maxpts
         if(maxpoints.gt.nnpts)maxpoints=nnpts
         vr=z             ! Pragmatic; achieves nothing but to have values in this array.

         do ipt=1,mmpts
           if(rvector1(ipt).lt.emin)emin=rvector1(ipt)
           if(rvector1(ipt).gt.emax)emax=rvector1(ipt)
         end do
         do ipt=1,mmpts
           if(rvector2(ipt).lt.nmin)nmin=rvector2(ipt)
           if(rvector2(ipt).gt.nmax)nmax=rvector2(ipt)
         end do
         do ipt=1,mmpts
           if(rvector3(ipt).lt.elevmin)elevmin=rvector3(ipt)
           if(rvector3(ipt).gt.elevmax)elevmax=rvector3(ipt)
         end do
         do ipt=1,nnpts
           if(x(ipt).lt.emin)emin=x(ipt)
           if(x(ipt).gt.emax)emax=x(ipt)
         end do
         do ipt=ipt,nnpts
           if(y(ipt).lt.nmin)nmin=y(ipt)
           if(y(ipt).gt.nmax)nmax=y(ipt)
         end do
         do ipt=1,nnpts
           if(z(ipt).lt.elevmin)elevmin=z(ipt)
           if(z(ipt).gt.elevmax)elevmax=z(ipt)
         end do
         unestimated=0

         call readparm_1(MAXDIS,MAXSBX,MAXSBY,MAXSBZ,MAXEQ,minpoints,maxpoints,  &
         sradmax_hmax,sradmax_hmin,sradmax_vert,krtype,s_skmean,                 &
         n_nst,c_c0,i_it,c_cc,a_ang1,a_ang2,a_ang3,a_a_hmax,a_a_hmin,a_a_vert,   &
         nnpts,ncol_range,nrow_range,iparmcall)
         iparmcall=0

         call kt3d_1(MAXDIS,MAXSBX,MAXSBY,MAXSBZ,MAXEQ,unestimated,              &
         mmpts,nnpts,ivector1,ivector2,rvector1,rvector2,rvector3,               &
         factorfile,aoutfile,iunit,pmx,                                          &
         ncol_range,nrow_range,emin,emax,nmin,nmax,elevmin,elevmax,af1,af2)
         if(unestimated.ne.0)then
           call utl_num2char(unestimated,anum)
           write(amessage,300) trim(anum)
300        format('Interpolation cannot take place to all target points. The ',     &
           'first element for which interpolation cannot take place is element ',   &
           'number ',a,'. Check that no interpolation source points are ',          &
           'coincident. Check that the search radius is large enough and ',         &
           'that MINPTS is large enough. If you are using a Gaussian variogram, ',  &
           ' then try using another.')
           go to 9890
         end if

       end do zones
       close(unit=iunit)
       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9050   write(amessage,9060) trim(varname),trim(function_name)
9060   format('The ',a,' argument of function ',a,' must be supplied as 0 or 1.')
       go to 9890

9070   call utl_num2char(zonenum(izone),anum)
       write(amessage,9080) trim(varname),trim(anum)
9080   format('The value of ',a,' supplied for zone ',a,' is not greater than zero.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array are supplied as zero.')
       go to 9890

9150   write(amessage,9160) trim(facfile)
9160   format('Cannot write to binary factor file ',a,'.')
       go to 9890

9170   write(amessage,9180) trim(facfile)
9180   format('Cannot write to text factor file ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in function ',a,'.')
       go to 9890


9890   calc_kriging_factors_3d=1
       icount_interp=0

9900   continue
       if(iunit.ne.0)then
         close(unit=iunit,iostat=ierr)
       end if

       call geostat_3d_dealloc()
       return

end function calc_kriging_factors_3d



integer (kind=c_int) function krige_using_file(factorfile,factorfiletype,      &
                                               npts,mpts,                      &
                                               krigtype,transtype,             &
                                               sourceval,targval,              &
                                               icount_interp,                  &
                                               meanval)                        &
                 bind(C,name="krige_using_file")
!DIR$ ATTRIBUTES DLLEXPORT :: krige_using_file

! -- This function applies interpolation factors that are calculated by other functions.

       use iso_c_binding, only: c_int,c_char,c_double
       use utilities
       implicit none

       character (kind=c_char,len=1), intent(in) :: factorfile(LENFILENAME) ! file for kriging factors
       integer(kind=c_int), intent(in)           :: factorfiletype          ! 0:binary, 1:text
       integer(kind=c_int), intent(in)           :: npts                    ! number of source points
       integer(kind=c_int), intent(in)           :: mpts                    ! number of target points
       integer(kind=c_int), intent(in)           :: krigtype                ! 0:simple, 1:ordinary  (just a check)
       integer(kind=c_int), intent(in)           :: transtype               ! 1:log,0:none
       real(kind=c_double), intent(in)           :: sourceval(npts)         ! values at sources
       real(kind=c_double), intent(out)          :: targval(mpts)           ! values calculated for targets
       integer(kind=c_int), intent(out)          :: icount_interp           ! number of interpolation pts
       real(kind=c_double), intent(in), optional :: meanval(mpts)           ! mean values (if simple kriging)

! -- Note that mean values are supplied in a dedicated INTENT(IN) array so that they are not over-written
!    if an error condition is encountered midway through the interpolation process.

       integer                     :: iunit,ierr
       integer                     :: nnpts,mmpts
       integer                     :: icellno,j,na
       real                        :: rtemp
       double precision            :: sum,dtemp
       character (len=15)          :: afiletype
       character (len=20)          :: anum,anum1
       character (len=LENFACCODE)  :: acode
       character (len=LENFILENAME) :: facfile

! -- Initialisation

       krige_using_file=0
       function_name='krige_using_file()'
       iunit=0
       icount_interp=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,factorfile,facfile)
       facfile=adjustl(facfile)

! -- Check input arguments.

       if((transtype.ne.0).and.(transtype.ne.1))then
         write(amessage,100) 'TRANSTYPE',trim(function_name)
100      format('The ',a,' argument of function ',a,' must be ',  &
         'supplied as 0 or 1.')
         go to 9890
       end if
       if((npts.le.0).or.(mpts.le.0))then
         write(amessage,110) trim(function_name)
110      format('The NPTS and MPTS arguments of function ',a,  &
         ' must exceed zero.')
         go to 9890
       end if
       if((krigtype.ne.0).and.(krigtype.ne.1))then
         write(amessage,100) 'KRIGTYPE',trim(function_name)
         go to 9890
       end if
       if(krigtype.eq.0)then
         if(.not.present(meanval))then
           write(amessage,117) trim(function_name)
117        format('If kriging is simple, then a MEANVAL argument must be ',     &
           'supplied for function ',a,'.')
           go to 9890
         end if
       end if

! -- Open the input file.

       iunit=utl_nextunit()
       if(factorfiletype.eq.0)then
         afiletype='binary'
         open(unit=iunit,file=facfile,form='unformatted',access='stream',         &
         status='old',err=9000)
         read(10,err=9050,end=9100)acode
         read(10,err=9050,end=9100)nnpts,mmpts
       else
         afiletype='text'
         open(unit=iunit,file=facfile,status='old',err=9000)
         read(10,'(a)',err=9050,end=9100) acode
         read(10,*,err=9050,end=9100)nnpts,mmpts
       end if

! -- Read the code at the top.

       if((acode(1:2).ne.'2d').and.(acode(1:2).ne.'3d')) go to 9150
       if(acode(3:4).eq.'ks')then
         if(krigtype.ne.0)then
           write(amessage,130) trim(function_name),'1',trim(facfile),'simple'
130        format('The KRIGTYPE argument of function ',a,' is supplied as ',a,    &
           '. However the interpolation factor file ',a,' specifies ',a,' kriging.')
           go to 9890
         end if
       else if(acode(3:4).eq.'ko')then
         if(krigtype.ne.1)then
           write(amessage,130) trim(function_name),'0',trim(facfile),'ordinary'
           go to 9890
         end if
       else
         go to 9150
       end if

! -- Check dimensions

       if((npts.ne.nnpts).or.(mpts.ne.mmpts))then
         write(amessage,140) trim(facfile),trim(function_name)
140      format('The dimensions of the source and target arrays that ', &
         'are specified in file ',a,' are not in agreement with the ',  &
         'NPTS and MPTS arguments supplied to function ',a,'.')
         go to 9890
       end if

! -- Allocate some memory.

       if(utl_allocate_vector('i',1,npts).ne.0) go to 9200
       if(utl_allocate_vector('r',1,npts).ne.0) go to 9200

! -- Now we do the interpolation.

       do
         if(factorfiletype.eq.0)then
           read(iunit,err=9300,end=700) icellno,na,rtemp,(ivector1(j),rvector1(j),j=1,na)
         else
           read(iunit,*,err=9300,end=700) icellno,na,rtemp,(ivector1(j),rvector1(j),j=1,na)
         end if
         if((icellno.lt.0).or.(icellno.gt.mpts))then
           call utl_num2char(icellno,anum)
           write(amessage,142) trim(anum),'TARGVAL',trim(facfile)
142        format('Element number ',a,' of ',a,' array cited in file ',a,' is out of bounds.')
           go to 9890
         end if
         icount_interp=icount_interp+1
         if(krigtype.eq.1)then
           sum=0.0d0
         else
           dtemp=meanval(icellno)
           if(transtype.eq.1)then
             if(dtemp.le.0.0d0) then
               call utl_num2char(icellno,anum)
               call utl_num2char(dtemp,anum1)
               write(amessage,160) trim(function_name),trim(anum1),trim(anum)
160            format('Error in function ',a,': kriging is simple and transformation is ',  &
               'logarithmic. The value of ',a,' supplied for element number ',a,            &
               ' of MEANVAL array is illegal as it must be positive.')
               go to 9890
             end if
             dtemp=log10(dtemp)
           end if
           sum=dtemp*rtemp
         end if
         do j=1,na
           if((ivector1(j).lt.0).or.(ivector1(j).gt.npts))then
             call utl_num2char(ivector1(j),anum)
             write(amessage,161) trim(facfile),trim(anum)
161          format('File ',a,' cites element number ',a,' of SOURCEVAL array. This index ', &
             'is out of bounds.')
             go to 9890
           end if
           if(transtype.eq.0)then
             sum=sum+sourceval(ivector1(j))*rvector1(j)
           else
             dtemp=sourceval(ivector1(j))
             if(dtemp.le.0.0)then
               call utl_num2char(ivector1(j),anum)
               call utl_num2char(dtemp,anum1)
               write(amessage,280) trim(anum),trim(anum1)
280            format('Element number ',a,' of the SOURCEVAL array is ',a,'. This should be ',  &
               'positive as it is used in log-based interpolation.')
               go to 9890
             end if
             sum=sum+log10(dtemp)*rvector1(j)
           end if
         end do
         if(transtype.eq.0)then
           dtemp=sum
         else
           dtemp=10**(sum)
         end if
         targval(icellno)=dtemp
       end do
700    continue

       go to 9900

9000   write(amessage,9010) trim(afiletype),trim(facfile)
9010   format('Cannot open ',a,' factor file ',a,'.')
       go to 9890

9050   write(amessage,9060) trim(afiletype),trim(facfile)
9060   format('Error reading ',a,' factor file ',a,'.')
       go to 9890

9100   write(amessage,9110) trim(afiletype),trim(facfile)
9110   format('Premature end encountered to ',a,' factor file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(facfile),trim(function_name)
9160   format('File ',a,' is incompatible with function ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in function ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(afiletype),trim(facfile)
9310   format('Error encountered while reading interpolation factors from ',a,' interpolation ',   &
       'factor file ',a,'.')
       go to 9890

9890   krige_using_file=1

9900   continue
       if(iunit.ne.0)then
         close(unit=iunit,iostat=ierr)
       end if

       return

end function krige_using_file



integer (kind=c_int) function build_covar_matrix_2d(        &
                              npts,ec,nc,zn,                &
                              vartype,                      &
                              nugget,aa,sill,anis,bearing,  &
                              ldcovmat,covmat)              &
                 bind(C,name="build_covar_matrix_2d")
!DIR$ ATTRIBUTES DLLEXPORT :: build_covar_matrix_2d

! -- This function calculates a covariance matrix for a set of 2D pilot points.
!    Variogram specifications can change with pilot point location.
!    Pilot points are assigned to zones. A zone number of zero is not allowed.

       use iso_c_binding, only: c_int,c_double
       use utilities
       implicit none

       integer(kind=c_int), intent(in)         :: npts                  ! number of pilot points
       real(kind=c_double), intent(in)         :: ec(npts),nc(npts)     ! pilot point coords
       integer(kind=c_int), intent(in)         :: zn(npts)              ! pilot point zones
       integer(kind=c_int), intent(in)         :: vartype               ! 1:spher,2:exp,3:gauss,4:pow
       real(kind=c_double), intent(in)         :: nugget(npts)          ! nugget
       real(kind=c_double), intent(in)         :: aa(npts)              ! variogram a
       real(kind=c_double), intent(in)         :: sill(npts)            ! variogram sill
       real(kind=c_double), intent(in)         :: anis(npts)            ! variogram anisotropy
       real(kind=c_double), intent(in)         :: bearing(npts)         ! variogram bearing
       integer(kind=c_int), intent(in)         :: ldcovmat              ! leading dimension of COVMAT
       real(kind=c_double), intent(out)        :: covmat(ldcovmat,npts) ! covariance matrix

       integer, parameter                      :: MAXZONE=10
       logical                                 :: uniform
       integer                                 :: ipt,jpt,kpt,izone,ierr,info
       integer                                 :: m,n,lda,ldu,ldvt,lwork
       integer                                 :: numzone,itemp
       integer                                 :: zone(MAXZONE)
       double precision                        :: e0,n0,dtemp,ecc,ncc
       double precision                        :: vt(1,1)
       character (len=1)                       :: jobu,jobvt
       character (len=10)                      :: anum,anum1,anum2
       character (len=25)                      :: varname

       double precision, allocatable           :: u(:,:)

! -- Variables used by GSLIB library functions (as well as related variables).

       logical                :: first
       integer                :: nst,it(1)
       real                   :: c0,pmx,x1,y1,x2,y2,rtemp1,rtemp2,passmaxcov,cova2
       real                   :: cc(1),ang(1),aaa(1),aanis(1)
       real                   :: oldcc,oldaaa,oldang,oldaanis,oldc0

! -- Initialisation

       build_covar_matrix_2d=0
       function_name='build_covar_matrix_2d()'

! -- Check input arguments.

       if(npts.le.0)then
         varname='NPTS'
         go to 9000
       end if
       if((vartype.lt.1).or.(vartype.gt.4))then
         write(amessage,115) trim(function_name)
115      format('The VARTYPE argument of function ',a,' must be 1,2,3 or 4.')
         go to 9890
       end if
       if(any(zn.eq.0))then
         varname='ZN'
         go to 9100
       end if
       if(ldcovmat.lt.npts)then
         write(amessage,116)
116      format('The value of the LDCOVMAT argument must not be less than that of the NPTS argument.')
         go to 9890
       end if

! -- Establish legitimacy of variogram specifications.

       do ipt=1,npts
         if(anis(ipt).le.0.0d0)then
           write(amessage,170) 'ANIS'
170        format('At least one value in the ',a,' array is zero or negative.')
           go to 9890
         end if
         if((bearing(ipt).lt.-360.0d0).or.(bearing(ipt).gt.360.0d0))then
           write(amessage,175)
175        format('At least one value in the BEARING array is less than -360 or ',  &
           'greater than 360.')
           go to 9890
         end if
         if(nugget(ipt).lt.0.0d0)then
           write(amessage,176) 'NUGGET'
176        format('At least one value in the ',a,' array is negative.')
           go to 9890
         end if
         if(aa(ipt).le.0.0d0)then
           write(amessage,170) 'variogram "a" value'
           go to 9890
         end if
         if(sill(ipt).lt.0.0d0)then
           write(amessage,176) 'SILL'
           go to 9890
         end if
       end do

! -- Build up an array of zone numbers.

       izone=1
       numzone=0
       do ipt=1,npts
         itemp=zn(ipt)
         if(numzone.eq.0)then
           numzone=1
           zone(numzone)=itemp
         else
           if(utl_whichone(numzone,izone,zone,itemp).ne.0)then
             numzone=numzone+1
             if(numzone.gt.MAXZONE)then
               call utl_num2char(MAXZONE,anum)
               write(amessage,150) trim(anum)
150            format('A maximum of only ',a,' different zones can feature in ZN array.')
               go to 9890
             end if
             zone(numzone)=itemp
           end if
         end if
       end do

! -- Test for uniformity

       uniform=.FALSE.
       do izone=1,numzone
         if(.not.utl_uniform_dvector(npts,nugget,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,aa,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,sill,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,anis,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,bearing,zn,zone(izone))) go to 200
       end do
       uniform=.TRUE.
200    continue

! -- See whether any two pilot points are in the same location.

       do ipt=1,npts-1
        ecc=ec(ipt)
        ncc=nc(ipt)
        izone=zn(ipt)
        do jpt=ipt+1,npts
          if(zn(jpt).eq.izone)then
            if(utl_equals(ec(jpt),ecc).and.utl_equals(nc(jpt),ncc))then
              call utl_num2char(ipt,anum1)
              call utl_num2char(jpt,anum2)
              write(amessage,210) trim(anum1),trim(anum2)
210           format('Points ',a,' and ',a,' are at the same location and are ',  &
              'in the same zone.')
              go to 9890
            end if
          end if
        end do
      end do

! -- Fill the covariance matrix.

        pmx=10000.0
        nst=1
        it(1)=vartype
        first=.TRUE.
        do ipt=1,npts
          e0=ec(ipt)
          n0=nc(ipt)
          covmat(ipt,ipt)=sill(ipt)+nugget(ipt)
          if(ipt.ne.npts)then
            izone=zn(ipt)
            x1=0.0
            y1=0.0
            do jpt=ipt+1,npts
              if(zn(jpt).ne.izone)then
                  covmat(jpt,ipt)=0.0
              else
                x2=ec(jpt)-e0
                y2=nc(jpt)-n0
                cc(1)=sill(ipt)
                aaa(1)=aa(ipt)
                ang(1)=bearing(ipt)
                aanis(1)=1.0d0/anis(ipt)
                c0=nugget(ipt)
                oldcc=cc(1)
                oldaaa=aaa(1)
                oldang=ang(1)
                oldaanis=aanis(1)
                oldc0=c0
                rtemp1 = cova2(x1,y1,x2,y2,nst,c0,PMX,cc,aaa,it,  &
                          ang,aanis,first,passmaxcov)
                cc(1)=sill(jpt)
                aaa(1)=aa(jpt)
                ang(1)=bearing(jpt)
                aanis(1)=1.0d0/anis(jpt)
                c0=nugget(jpt)
                if((cc(1).ne.oldcc).or.(aaa(1).ne.oldaaa).or.(ang(1).ne. oldang).or.   &
                  (aanis(1).ne.oldaanis).or.(c0.ne.oldc0))then
                  rtemp2 = cova2(x1,y1,x2,y2,nst,c0,PMX,cc,aaa,it,  &
                            ang,aanis,first,passmaxcov)
                else
                  rtemp2=rtemp1
                end if
                covmat(jpt,ipt)=min(rtemp1,rtemp2)
              end if
            end do
          end if
        end do
        if(npts.gt.1)then
          do ipt=2,npts
            do jpt=1,ipt-1
              covmat(jpt,ipt)=covmat(ipt,jpt)
            end do
          end do
        end if

! -- If necessary, the covariance matrix is now made into a positive definite matrix.

        if(.not.uniform)then
          jobu='A'
          jobvt='N'
          m=npts
          n=npts
          lda=ldcovmat
          ldu=npts
          ldvt=1
          lwork=8*npts
          allocate(u(npts,npts),stat=ierr)
          if(ierr.ne.0) go to 9200
          if(utl_allocate_vector('d',1,npts).ne.0) go to 9200
          if(utl_allocate_vector('d',2,lwork).ne.0) go to 9200
          call dgesvd(jobu,jobvt,m,n,covmat,lda,dvector1,u,ldu,vt,ldvt,dvector2,lwork,info )
          if(info.ne.0)then
            write(amessage,180)
180         format('Unable to conduct singular value decomposition on covariance matrix.')
            go to 9890
          end if
          do ipt=1,npts
            do jpt=1,ipt
              dtemp=0.0d0
              do kpt=1,npts
                dtemp=dtemp+u(ipt,kpt)*u(jpt,kpt)*dvector1(kpt)
              end do
              covmat(ipt,jpt)=dtemp
            end do
          end do
          if(npts.gt.1)then
            do ipt=1,npts-1
              do jpt=ipt+1,npts
                covmat(ipt,jpt)=covmat(jpt,ipt)
              end do
            end do
          end if
        end if

        go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array must be supplied as nonzero.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error encountered in function ',a,'.')
       go to 9890

9890   build_covar_matrix_2d=1

9900   continue
       if(allocated(u))deallocate(u,stat=ierr)

       return

end function build_covar_matrix_2d



integer (kind=c_int) function build_covar_matrix_3d(        &
                              npts,ec,nc,zc,zn,             &
                              vartype,                      &
                              nugget,sill,                  &
                              ahmax,ahmin,avert,            &
                              bearing,dip,rake,             &
                              ldcovmat,covmat)              &
                 bind(C,name="build_covar_matrix_3d")
!DIR$ ATTRIBUTES DLLEXPORT :: build_covar_matrix_3d

! -- This function calculates a covariance matrix for a set of 3D pilot points.
!    Variogram specifications can change with pilot point location.
!    Pilot points are assigned to zones. A zone number of zero is not allowed.

       use iso_c_binding, only: c_int,c_double
       use utilities
       implicit none

       integer(kind=c_int), intent(in)         :: npts                       ! number of pilot points
       real(kind=c_double), intent(in)         :: ec(npts),nc(npts),zc(npts) ! pilot point coords
       integer(kind=c_int), intent(in)         :: zn(npts)              ! pilot point zones
       integer(kind=c_int), intent(in)         :: vartype               ! 1:spher,2:exp,3:gauss,4:pow
       real(kind=c_double), intent(in)         :: nugget(npts)          ! nugget
       real(kind=c_double), intent(in)         :: sill(npts)            ! sill
       real(kind=c_double), intent(in)         :: ahmax(npts),ahmin(npts),avert(npts) ! variogram "a" valies
       real(kind=c_double), intent(in)         :: bearing(npts),dip(npts),rake(npts)  ! variogram angles
       integer(kind=c_int), intent(in)         :: ldcovmat              ! leading dimension of COVMAT
       real(kind=c_double), intent(out)        :: covmat(ldcovmat,npts) ! covariance matrix

       integer, parameter                      :: MAXZONE=10
       logical                                 :: uniform
       integer                                 :: ipt,jpt,kpt,izone,ierr,info,lastzone
       integer                                 :: m,n,lda,ldu,ldvt,lwork
       integer                                 :: numzone,itemp
       integer                                 :: zone(MAXZONE)
       double precision                        :: e0,n0,z0,ecc,ncc,zcc
       double precision                        :: aa1,aa2
       double precision                        :: dtemp,dtemp1,dtemp2
       double precision                        :: vt(1,1)
       double precision                        :: rotmat(1,3,3)
       character (len=1)                       :: jobu,jobvt
       character (len=10)                      :: anum,anum1,anum2
       character (len=25)                      :: varname

       double precision, allocatable           :: u(:,:)

! -- Variables used by GSLIB library functions (as well as related variables).

       integer                :: one
       integer                :: n_nst(1),i_it(1)
       real, parameter        :: EPSLON = 0.000001
       real                   :: pmx
       real                   :: x1,y1,z1,x2,y2,z2
       real                   :: c_c0(1),c_cc(1)
       real                   :: a_ang1(1),a_ang2(1),a_ang3(1)
       real                   :: a_ahmax(1),a_ahmin(1),a_avert(1),aa(1)
       real                   :: anis1(1),anis2(1)
       real                   :: rtemp,covmax

! -- Initialisation

       build_covar_matrix_3d=0
       function_name='build_covar_matrix_3d()'

! -- Check input arguments.

       if(npts.le.0)then
         varname='NPTS'
         go to 9000
       end if
       if((vartype.lt.1).or.(vartype.gt.4))then
         write(amessage,115) trim(function_name)
115      format('The VARTYPE argument of function ',a,' must be 1,2,3 or 4.')
         go to 9890
       end if
       if(any(zn.eq.0))then
         varname='ZN'
         go to 9100
       end if
       if(ldcovmat.lt.npts)then
         write(amessage,116)
116      format('The value of the LDCOVMAT argument must not be less than that of the NPTS argument.')
         go to 9890
       end if

! -- Establish legitimacy of variogram specifications.

       do ipt=1,npts
         if(nugget(ipt).lt.0.0d0)then
           write(amessage,176) 'NUGGET'
176        format('At least one value in the user-supplied ',a,' array is negative.')
           go to 9890
         end if
         if(sill(ipt).lt.0.0d0)then
           write(amessage,176) 'SILL'
           go to 9890
         end if
         if(ahmax(ipt).le.0.0d0)then
           varname='AHMAX'
           go to 9300
         end if
         if(ahmin(ipt).le.0.0d0)then
           varname='AHMIN'
           go to 9300
         end if
         if(avert(ipt).le.0.0d0)then
           varname='AVERT'
           go to 9300
         end if
         if((bearing(ipt).lt.-360.0d0).or.(bearing(ipt).gt.360.0d0))then
           write(amessage,175)
175        format('At least one value in the user-supplied BEARING array is less than -360 '  &
           'degrees or greater than 360 degrees.')
           go to 9890
         end if
         if((dip(ipt).lt.-180.0d0).or.(dip(ipt).gt.180.0d0))then
           write(amessage,181)
181        format('At least one value in the user-supplied DIP array is less than -180 ',  &
           'degrees or greater than 180 degrees.')
           go to 9890
         end if
         if((rake(ipt).lt.-90.0d0).or.(rake(ipt).gt.90.0d0))then
           write(amessage,182)
182        format('At least one value in the user-supplied RAKE array is less -90 ', &
           'degrees or greater than 90 degrees.')
           go to 9890
         end if
       end do

! -- Build up an array of zone numbers.

       izone=1
       numzone=0
       lastzone=huge(lastzone)
       do ipt=1,npts
         itemp=zn(ipt)
         if(itemp.eq.lastzone)cycle
         if(numzone.eq.0)then
           numzone=1
           zone(numzone)=itemp
         else
           if(utl_whichone(numzone,izone,zone,itemp).ne.0)then
             numzone=numzone+1
             if(numzone.gt.MAXZONE)then
               call utl_num2char(MAXZONE,anum)
               write(amessage,150) trim(anum)
150            format('A maximum of only ',a,' different zones can feature in ZN array.')
               go to 9890
             end if
             zone(numzone)=itemp
           end if
         end if
         lastzone=itemp
       end do

! -- Test for uniformity

       uniform=.FALSE.
       do izone=1,numzone
         if(.not.utl_uniform_dvector(npts,nugget,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,sill,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,ahmax,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,ahmin,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,avert,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,bearing,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,dip,zn,zone(izone))) go to 200
         if(.not.utl_uniform_dvector(npts,rake,zn,zone(izone))) go to 200
       end do
       uniform=.TRUE.
200    continue

! -- See whether any two pilot points are in the same location.

       do ipt=1,npts-1
         ecc=ec(ipt)
         ncc=nc(ipt)
         zcc=zc(ipt)
         izone=zn(ipt)
         do jpt=ipt+1,npts
           if(zn(jpt).eq.izone)then
             if(utl_equals(ec(jpt),ecc).and.utl_equals(nc(jpt),ncc).and.   &
                utl_equals(zc(jpt),zcc))then
               call utl_num2char(ipt,anum1)
               call utl_num2char(jpt,anum2)
               write(amessage,210) trim(anum1),trim(anum2)
210            format('Points ',a,' and ',a,' are at the same location and are ',  &
               'in the same zone.')
               go to 9890
             end if
           end if
         end do
       end do

! -- Elements of the covariance matrix are now evaluated.

       one=1
       pmx=10000.0
       n_nst(1)=1
       i_it(1)=vartype
       x1=0.0
       y1=0.0
       z1=0.0
       do ipt=1,npts
         e0=ec(ipt)
         n0=nc(ipt)
         z0=zc(ipt)
         covmat(ipt,ipt)=sill(ipt)+nugget(ipt)
         izone=zn(ipt)
         c_c0(1)=nugget(ipt)
         c_cc(1)=sill(ipt)
         a_ang1(1)=bearing(ipt)
         a_ang2(1)=dip(ipt)
         a_ang3(1)=rake(ipt)
         a_ahmax(1)=ahmax(ipt)
         a_ahmin(1)=ahmin(ipt)
         a_avert(1)=avert(ipt)
         aa(1)=a_ahmax(1)
         aa1  =a_ahmin(1)
         aa2  =a_avert(1)
         anis1(1) = aa1 / max(aa(1),EPSLON)
         anis2(1) = aa2 / max(aa(1),EPSLON)
         call setrot_new(a_ang1(1),a_ang2(1),a_ang3(1),anis1(1),anis2(1),one,one,rotmat)
         do jpt=1,npts
           if(zn(jpt).ne.izone)then
             covmat(jpt,ipt)=0.0d0
           else if(ipt.ne.jpt)then
             x2=ec(jpt)-e0
             y2=nc(jpt)-n0
             z2=zc(jpt)-z0
             call cova3_jd(x1,y1,z1,x2,y2,z2,one,n_nst,one,c_c0,i_it,c_cc,aa,   &
                  one,one,rotmat,covmax,rtemp,PMX)
             covmat(jpt,ipt)=rtemp
           end if
         end do
       end do

! -- We select the smaller of the two covariances calculated between pairs of pilot points.

       if(npts.ne.1)then
         do ipt=1,npts
           do jpt=1,npts-1
             dtemp1=covmat(ipt,jpt)
             dtemp2=covmat(jpt,ipt)
             dtemp=min(dtemp1,dtemp2)
             covmat(ipt,jpt)=dtemp
             covmat(jpt,ipt)=dtemp
           end do
         end do

! -- If necessary,the covariance matrix is now made into a positive definite matrix.

         if(.not.uniform)then
           jobu='A'
           jobvt='N'
           m=npts
           n=npts
           lda=ldcovmat
           ldu=npts
           ldvt=1
           lwork=8*npts
           allocate(u(npts,npts),stat=ierr)
           if(ierr.ne.0) go to 9200
           if(utl_allocate_vector('d',1,npts).ne.0) go to 9200
           if(utl_allocate_vector('d',2,lwork).ne.0) go to 9200
           call dgesvd( jobu,jobvt,m,n,covmat,lda,dvector1,u,ldu,vt,ldvt,dvector2,lwork,info )
           if(info.ne.0)then
             write(amessage,180)
180          format('Unable to conduct singular value decomposition on covariance matrix.')
             go to 9890
           end if
           do ipt=1,npts
             do jpt=1,ipt
               dtemp=0.0d0
               do kpt=1,npts
                 dtemp=dtemp+u(ipt,kpt)*u(jpt,kpt)*dvector1(kpt)
               end do
               covmat(ipt,jpt)=dtemp
             end do
           end do
           do ipt=1,npts-1
             do jpt=ipt+1,npts
               covmat(ipt,jpt)=covmat(jpt,ipt)
             end do
           end do

! -- If two points are in different zones, then we re-instate zero correlation.

           if(numzone.gt.1)then
             do ipt=1,npts-1
               izone=zn(ipt)
               do jpt=ipt+1,npts
                 if(zn(jpt).ne.izone)then
                   covmat(ipt,jpt)=0.0d0
                   covmat(jpt,ipt)=0.0d0
                 end if
               end do
             end do
           end if
         end if
       end if

       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array must be supplied as nonzero.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error encountered in function ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(varname)
9310   format('At least one value in the user-supplied ',a,' array is zero or negative.')
       go to 9890

9890   build_covar_matrix_3d=1

9900   continue
       if(allocated(u))deallocate(u,stat=ierr)

       return

end function build_covar_matrix_3d



integer (kind=c_int) function calc_structural_overlay_factors(     &
                     npts,                                         &
                     ecs,ncs,ids,                                  &
                     conwidth,aa,                                  &
                     structype,inverse_power,                      &
                     mpts,                                         &
                     ect,nct,active,                               &
                     factorfile,factorfiletype,                    &
                     icount_interp)                                &
                 bind(C,name="calc_structural_overlay_factors")
!DIR$ ATTRIBUTES DLLEXPORT :: calc_structural_overlay_factors

! -- This function calculates interpolation/blending factors for structural overlay parameters.

       use iso_c_binding, only: c_int,c_char,c_double
       use utilities
       implicit none

       integer(kind=c_int), intent(in)           :: npts                    ! number of source points
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)     ! source point coordinates
       integer(kind=c_int), intent(in)           :: ids(npts)               ! source point structure number
       real(kind=c_double), intent(in)           :: conwidth(npts),aa(npts) ! blending parameters
       integer(kind=c_int), intent(in)           :: structype               ! 0=polylinear;1=polygonal
       real(kind=c_double), intent(in)           :: inverse_power           ! inverse power of distance
       integer(kind=c_int), intent(in)           :: mpts                    ! number of target points
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)     ! target point coords
       integer(kind=c_int), intent(in)           :: active(mpts)            ! target point activity
       character (kind=c_char,len=1), intent(in) :: factorfile(LENFILENAME) ! file for kriging factors
       integer(kind=c_int), intent(in)           :: factorfiletype          ! 0:binary, 1:text
       integer(kind=c_int), intent(out)          :: icount_interp           ! number of interp points

       logical                       :: lexist,lopened
       integer                       :: i_one,i_two
       integer                       :: iunit,iii_old,iii,jjj,iflag,ierr,i,ii
       integer                       :: ipt,jpt,nc_npts,nc_mpts
       integer                       :: num2,numinlist1
       integer                       :: numstruc,icountmax,icount,istruc,numelem1
       integer                       :: iend1,istart1
       integer                       :: inside,m
       integer                       :: minsector,ielem1,ielem2
       integer                       :: structtype

       real                          :: r_zero
       double precision              :: xx,yy
       double precision              :: mindist,dist,uu,minuu
       double precision              :: cw1,cw2,cw
       double precision              :: alp1,alp2,alp,dist_no_inform,gg,recip_alpha
       double precision              :: sum,dtemp2,dtemp,dtemp1

       character (len=LENFACCODE)    :: acode
       character (len=10)            :: anum
       character (len=25)            :: varname
       character (len=LENFILENAME)   :: facfile

! -- Initialisation

       calc_structural_overlay_factors=0
       function_name='calc_structural_overlay_factors()'
       iunit=0
       i_one=1
       i_two=2
       r_zero=0.0
       icount_interp=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,factorfile,facfile)
       facfile=adjustl(facfile)

! -- Check input arguments.

       if(npts.le.1)then
         varname='NPTS'
         go to 9000
       end if
       if(mpts.le.1)then
         varname='MPTS'
         go to 9000
       end if
       if(any(conwidth.lt.0.0d0))then
         write(amessage,100) 'CONWIDTH'
100      format('At least one element of the user-supplied ',a,' array is negative.')
         go to 9890
       end if
       if(any(aa.lt.0.0d0))then
         write(amessage,100) 'AA'
         go to 9890
       end if
       if((structype.ne.0).and.(structype.ne.1))then
         write(amessage,110) 'STRUCTYPE'
110      format(a,' must be supplied as 0 or 1.')
         go to 9890
       end if
       if((factorfiletype.ne.0).and.(factorfiletype.ne.1))then
         write(amessage,110) 'FACTORFILETYPE'
         go to 9890
       end if
       if(inverse_power.lt.0.0d0)then
           write(amessage,115)
115        format('The INVERSE_POWER argument must not be negative.')
           go to 9890
       end if
       if(all(active.eq.0))then
         write(amessage,120)
120      format('All elements of the ACTIVE array are zero.')
         go to 9890
       end if

       iii_old=huge(iii_old)
       do ipt=1,npts-1
         iii=ids(ipt)
         if(iii.eq.iii_old)then
           cycle
         else
           iii_old=iii
         end if
         iflag=0
         do jpt=ipt+1,npts
           jjj=ids(jpt)
           if(jjj.ne.iii)then
             iflag=1
           else
             if(iflag.eq.1)then
               write(amessage,130)
130            format('Entries of the same value provided in the IDS array ', &
               'must not be separated by entries of different value.')
               go to 9890
             end if
           end if
         end do
       end do

! -- For conformity with PLPROC.

       structtype=structype+1

! -- We find the number of elements to which interpolation/blending will take place.

       num2=0
       do ipt=1,mpts
         if(active(ipt).ne.0) num2=num2+1
       end do

! -- We find the maximum number of elements in any structure and count the number of structures.

       numstruc=0
       icountmax=0
       icount=0
       jjj=huge(jjj)
       do ipt=1,npts
         iii=ids(ipt)
         if(iii.ne.jjj)then
           numstruc=numstruc+1
           if(icount.gt.icountmax)icountmax=icount
           icount=0
           jjj=iii
         end if
         icount=icount+1
       end do
       if(icount.gt.icountmax)icountmax=icount
       num2=num2*numstruc        ! The number of lines that will appear in the factor file.
                                 ! Don't forget that interpolation is repeated for each overlay structure.

! -- Establish acode.

       acode='2dbl'

! -- We open the factor file. However if a binary file is requested, and the file is present
!    it is deleted.

       iunit=utl_nextunit()
       if(factorfiletype.eq.0)then
         inquire(file=facfile,exist=lexist)
         if(lexist)then
           open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
           action='write',iostat=ierr)
           close(unit=iunit,status='delete',iostat=ierr)
         end if
         open(unit=iunit,file=facfile,form='unformatted',access='stream',   &
         action='write',err=9150)
       else
         open(unit=iunit,file=facfile,action='write',err=9170)
       end if

! -- Write a header to the file.

       nc_npts=npts             ! just in case of problems where a file is binary
       nc_mpts=mpts
       if(factorfiletype.eq.0)then
         write(iunit,err=9150) acode
         write(iunit,err=9150) nc_npts,nc_mpts,num2
       else
         write(iunit,220,err=9170) trim(acode)
220      format(a)
         write(iunit,230,err=9170) nc_npts,nc_mpts,num2
230      format(3i10)
       end if

! -- Allocate some memory.

       if(utl_allocate_vector('d',1,icountmax+2).ne.0) go to 9200
       if(utl_allocate_vector('d',2,icountmax+2).ne.0) go to 9200
       if(utl_allocate_vector('d',3,icountmax+2).ne.0) go to 9200
       if(utl_allocate_vector('i',1,icountmax+2).ne.0) go to 9200

! -- The big loop starts here.

       iend1=0
       do istruc=1,numstruc
         istart1=iend1+1
         if(numstruc.eq.1)then
           iend1=npts
         else
           iii=ids(istart1)
           if(istruc.eq.numstruc)then
             iend1=npts
           else
             do ipt=istart1,npts
               if(ids(ipt).ne.iii)then
                 iend1=ipt-1
                 go to 269
               end if
             end do
           end if
269        continue
         end if
         numelem1=iend1-istart1+1

         if(utl_equals(ecs(istart1),ecs(iend1)).and.      &
            utl_equals(ncs(istart1),ncs(iend1)))then
            call utl_num2char(iii,anum)
            write(amessage,270) trim(anum)
270         format('The first and last vertices of structure with index ',a,' have ',   &
            'the same east and north coordinates. This is not allowed.')
            go to 9890
         end if
         if(structtype.eq.2)then
           if(numelem1.lt.3)then
             call utl_num2char(iii,anum)
             write(amessage,285) trim(anum)
285          format('The polygonal structure with index ',a,' has only two points. ',    &
             'A polygon cannot be formed.')
             go to 9890
           end if
         end if

! -- Source coordinates are stored in DVECTORs.

         numinlist1=iend1-istart1+1
         if(structtype.eq.2)numinlist1=numinlist1+1
         dvector3=0.0d0           ! an array
         ii=0
         do ipt=istart1,iend1
           ii=ii+1
           dvector1(ii)=ecs(ipt)
           dvector2(ii)=ncs(ipt)
           ivector1(ii)=ipt
         end do
         if(structtype.eq.2)then
           dvector1(numinlist1)=ecs(istart1)
           dvector2(numinlist1)=ncs(istart1)
           ivector1(numinlist1)=istart1
         end if

! -- The work is now done.

         do ipt=1,mpts
           if(active(ipt).ne.0)then
             xx=ect(ipt)
             yy=nct(ipt)

! -- Is the target point inside the polygon?

             inside=-1
             if(structtype.eq.2)then
               call utl_locpt(xx,yy,dvector1,dvector2,numinlist1,inside,m)
             end if

             if(inside.lt.0)then
! -- So either structtype is 1 or the point is outside the polygon.
! -- We find the segment that the point is closest to.
               mindist=1.0d300
               do i=1,numinlist1-1
                 dist=utl_distance_to_segment(     &
                 dvector1(i),dvector2(i),dvector1(i+1),dvector2(i+1),xx,yy,uu)
                 if(dist.lt.mindist)then
                   mindist=dist
                   minsector=i
                   minuu=uu
                 end if
               end do
               if(minuu.lt.0.0d0)minuu=0.0d0
               if(minuu.gt.1.0d0)minuu=1.0d0
               ielem1=ivector1(minsector)
               ielem2=ivector1(minsector+1)
               cw1=conwidth(ielem1)
               cw2=conwidth(ielem2)
               cw=(1.0d0-minuu)*cw1+minuu*cw2
               cw=cw*0.5
               alp1=aa(ielem1)
               alp2=aa(ielem2)
               alp=(1.0d0-minuu)*alp1+minuu*alp2
               dist_no_inform=cw+alp*sqrt(-log(1.0d-30))       !   1e-30 is arbitrary
               if(dist_no_inform.eq.0.0d0)then
                 if(mindist.eq.0.0d0)then
                   gg=1.0d0
                 else
                   gg=0.0d0
                 end if
               else
                 if(mindist.gt.dist_no_inform)then
                   gg=0.0d0
                 else if(mindist.lt.cw)then
                   gg=1.0d0
                 else
                   if(alp.gt.0.0d0)then
                     recip_alpha=1.0d0/alp
                     dtemp=(mindist-cw)*recip_alpha
                     gg=exp(-dtemp*dtemp)
                   else
                     gg=0.0d0
                   end if
                 end if
               end if
             else
               gg=1.0d0
             end if
             if(structtype.eq.1)then
! -- We do a secondary inverse power of distance interpolation to supplement the interpolation that has already been done.
!    This gets rid of discontinuties on lines that bisect the inside of bends.
               dvector3=0.0 ! for safety
               sum=0.0d0
               do i=1,numelem1
                 dtemp1=xx-dvector1(i)
                 dtemp2=yy-dvector2(i)
                 dist=sqrt(dtemp1*dtemp1+dtemp2*dtemp2)
                 if(abs(dist).lt.1.0d-30) then
                   dvector3(i)=1.0d30
                 else
                   dvector3(i)=min(1.0d0/(dist**inverse_power),1.0e30)
                 end if
                 sum=sum+dvector3(i)
               end do
               if(abs(mindist).lt.1.0d-30)then
                 dtemp=1.0d30
               else
                 dtemp=min(1.0d0/(mindist**inverse_power),1.0d30)
               end if
               dvector3(minsector)=dvector3(minsector)+(1.0d0-minuu)*dtemp
               dvector3(minsector+1)=dvector3(minsector+1)+minuu*dtemp
               sum=sum+dtemp
               if(sum.gt.0.0d0)sum=1.0d0/sum
               do i=1,numelem1
                 dvector3(i)=dvector3(i)*sum
               end do
               do i=1,numelem1
                 dvector3(i)=dvector3(i)*gg
               end do
               if(factorfiletype.eq.0)then
                 write(iunit,err=9600) ipt,numelem1,r_zero,       &
                 (ivector1(i),dvector3(i),i=1,numelem1)
               else
                 write(iunit,310,err=9600) ipt,numelem1,r_zero,   &
                 (ivector1(i),dvector3(i),i=1,numelem1)
               end if
             else if(structtype.eq.2)then
! -- Undertake inverse power of distance interpolation regardless of whether we are
!    inside or outside of the polygon.
               sum=0.0d0
               do i=1,numelem1
                 dtemp1=xx-dvector1(i)
                 dtemp2=yy-dvector2(i)
                 dist=sqrt(dtemp1*dtemp1+dtemp2*dtemp2)
                 if(abs(dist).lt.1.0d-30) then
                   dvector3(i)=1.0d30
                 else
                   dvector3(i)=min(1.0d0/(dist**inverse_power),1.0d30)
                 end if
                 sum=sum+dvector3(i)
               end do
               if(sum.gt.0.0d0)sum=1.0d0/sum
               do i=1,numelem1
                 dvector3(i)=dvector3(i)*sum
               end do
               do i=1,numelem1
                 dvector3(i)=dvector3(i)*gg
               end do
               if(factorfiletype.eq.0)then
                 write(iunit,err=9600) ipt,numelem1,r_zero,       &
                 (ivector1(i),dvector3(i),i=1,numelem1)
               else
                 write(iunit,310,err=9600) ipt,numelem1,r_zero,   &
                 (ivector1(i),dvector3(i),i=1,numelem1)
310              format(2i10,1pg14.7,10000(i10,2x,1pg17.10))
               end if
             end if
           end if
         end do

       end do
       icount_interp=num2
       close(unit=iunit)
       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than one.')
       go to 9890

9150   write(amessage,9160) trim(facfile)
9160   format('Cannot write to binary factor file ',a,'.')
       go to 9890

9170   write(amessage,9180) trim(facfile)
9180   format('Cannot write to text factor file ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in function ',a,'.')
       go to 9890

9600   continue
       write(amessage,9650) trim(facfile)
9650   format('Cannot write interpolation factors to file ',a,'.')
       go to 9890

9890   calc_structural_overlay_factors=1
       if(iunit.ne.0)then
         inquire(unit=iunit,opened=lopened)
         if(lopened) then
           close(unit=iunit,iostat=ierr,status='delete')
         end if
       end if

9900   continue
       return

end function calc_structural_overlay_factors




integer (kind=c_int) function interpolate_blend_using_file(                    &
                                               factorfile,factorfiletype,      &
                                               npts,mpts,                      &
                                               transtype,                      &
                                               lt_target,gt_target,            &
                                               sourceval,targval,              &
                                               icount_interp)                  &
                 bind(C,name="interpolate_blend_using_file")
!DIR$ ATTRIBUTES DLLEXPORT :: interpolate_blend_using_file

! -- This function applies interpolation factors calculated by function calc_structural_overlay_factors.

       use iso_c_binding, only: c_int,c_char,c_double
       use utilities
       implicit none

       character (kind=c_char,len=1), intent(in) :: factorfile(LENFILENAME) ! file for kriging factors
       integer(kind=c_int), intent(in)           :: factorfiletype          ! 0:binary, 1:text
       integer(kind=c_int), intent(in)           :: npts                    ! dimension of sourceval array
       integer(kind=c_int), intent(in)           :: mpts                    ! dimension of targval array
       integer(kind=c_int), intent(in)           :: transtype               ! 1:log,0:none
       character (kind=c_char,len=1), intent(in) :: lt_target,gt_target     ! whether to undercut or exceed target
       real(kind=c_double), intent(in)           :: sourceval(npts)         ! values at sources
       real(kind=c_double), intent(inout)        :: targval(mpts)           ! values at targets
       integer(kind=c_int), intent(out)          :: icount_interp           ! number of interpolation pts

       integer                     :: iunit,ierr
       integer                     :: nnpts,mmpts
       integer                     :: icellno,i,j,na,ivj
       integer                     :: ilow,ihigh,num2
       real                        :: rtemp
       double precision            :: sum,rtemp1,rtemp2,dval0
       character (len=15)          :: afiletype
       character (len=LENFACCODE)  :: acode
       character (len=LENFILENAME) :: facfile

! -- Initialisation

       interpolate_blend_using_file=0
       function_name='interpolate_blend_using_file()'
       iunit=0
       icount_interp=0

! -- Character arrays are translated to character variables.

       call utl_string2char(LENFILENAME,factorfile,facfile)
       facfile=adjustl(facfile)

! -- Check input arguments.

       if((transtype.ne.0).and.(transtype.ne.1))then
         write(amessage,100) 'TRANSTYPE',trim(function_name)
100      format('The ',a,' argument of function ',a,' must be ',  &
         'supplied as 0 or 1.')
         go to 9890
       end if
       if((factorfiletype.ne.0).and.(factorfiletype.ne.1))then
         write(amessage,100) 'FACTORFILETYPE',trim(function_name)
         go to 9890
       end if
       if((npts.le.0).or.(mpts.le.0))then
         write(amessage,110) trim(function_name)
110      format('The NPTS and MPTS arguments of function ',a,  &
         ' must exceed zero.')
         go to 9890
       end if
       if((lt_target.eq.'y').or.(lt_target.eq.'Y'))then
         ilow=1
       else if((lt_target.eq.'n').or.(lt_target.eq.'N'))then
         ilow=0
       else
         write(amessage,120) 'LT_TARGET',trim(function_name)
120      format('The value supplied for the ',a,' argument of function ',a,    &
         ' must be "y" or "n".')
         go to 9890
       end if
       if((gt_target.eq.'y').or.(gt_target.eq.'Y'))then
         ihigh=1
       else if((gt_target.eq.'n').or.(gt_target.eq.'N'))then
         ihigh=0
       else
         write(amessage,120) 'GT_TARGET',trim(function_name)
         go to 9890
       end if

! -- Open the input file.

       iunit=utl_nextunit()
       if(factorfiletype.eq.0)then
         afiletype='binary'
         open(unit=iunit,file=facfile,form='unformatted',access='stream',         &
         status='old',err=9000)
         read(iunit,err=9050,end=9100)acode
       else
         afiletype='text'
         open(unit=iunit,file=facfile,status='old',err=9000)
         read(iunit,'(a)',err=9050,end=9100) acode
       end if

! -- Read the code at the top.

       if((acode(1:2).ne.'2d').and.(acode(1:2).ne.'3d')) go to 9150
       if(acode(3:4).ne.'bl') go to 9150

! -- Read and check dimensions

       if(factorfiletype.eq.0)then
         read(iunit,err=9050,end=9100)nnpts,mmpts,num2
       else
         read(iunit,*,err=9050,end=9100)nnpts,mmpts,num2
       end if
       if((npts.ne.nnpts).or.(mpts.ne.mmpts))then
         write(amessage,140) trim(facfile),trim(function_name)
140      format('The dimensions of the source and/or target arrays that ', &
         'are specified in file ',a,' are not in agreement with the ',     &
         'NPTS and MPTS arguments supplied to function ',a,'.')
         go to 9890
       end if

! -- Allocate some memory.

       if(utl_allocate_vector('i',1,npts).ne.0) go to 9200
       if(utl_allocate_vector('d',3,npts).ne.0) go to 9200

! -- Now we do the interpolation.

       do i=1,num2
         if(factorfiletype.eq.0)then
           read(iunit,err=9300,end=9350) icellno,na,rtemp,(ivector1(j),dvector3(j),j=1,na)
         else
           read(iunit,*,err=9300,end=9350) icellno,na,rtemp,(ivector1(j),dvector3(j),j=1,na)
         end if
         if((icellno.lt.1).or.(icellno.gt.mpts)) go to 9400
         dval0=targval(icellno)
         if(transtype.eq.1)then
           if(dval0.le.0.0d0)then
             write(amessage,280) 'TARGVAL'
             go to 9890
           end if
           dval0=log10(dval0)
         end if
         sum=0.0d0
         do j=1,na
           ivj=ivector1(j)
           if((ivj.lt.1).or.(ivj.gt.npts)) go to 9400
           if(transtype.eq.0)then
             sum=sum+(sourceval(ivj)-dval0)*dvector3(j)
           else
             rtemp2=sourceval(ivj)
             if(rtemp2.le.0.0)then
               write(amessage,280) 'SOURCEVAL'
280            format('At least one element value of the ',a,' array is negative. ', &
               'TRANSTYPE cannot therefore be supplied as 1; it must be supplied as 0.')
               go to 9890
             end if
             sum=sum+(log10(rtemp2)-dval0)*dvector3(j)
           end if
         end do
         if(ihigh.eq.0)then
           rtemp1=min(sum+dval0,dval0)
         else if(ilow.eq.0)then
           rtemp1=max(sum+dval0,dval0)
         else
           rtemp1=sum+dval0
         end if
         if(transtype.eq.1)then
           if(rtemp1.gt.300)then
             write(amessage,290)
290          format('Out of range value calculated for target array.')
             go to 9890
           end if
           rtemp1=10**(rtemp1)
         end if
         targval(icellno)=rtemp1
       end do
       icount_interp=num2
       go to 9900

9000   write(amessage,9010) trim(afiletype),trim(facfile)
9010   format('Cannot open ',a,' factor file ',a,'.')
       go to 9890

9050   write(amessage,9060) trim(afiletype),trim(facfile)
9060   format('Error reading ',a,' factor file ',a,'.')
       go to 9890

9100   write(amessage,9110) trim(afiletype),trim(facfile)
9110   format('Premature end encountered to ',a,' factor file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(facfile),trim(function_name)
9160   format('File ',a,' is incompatible with function ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory allocation error in function ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(afiletype),trim(facfile)
9310   format('Error encountered while reading interpolation factors from ',a,' interpolation ',   &
       'factor file ',a,'.')
       go to 9890

9350   write(amessage,9360) trim(afiletype),trim(facfile)
9360   format('Premature end encountered while reading interpolation factors from ',a,   &
       ' interpolation factor file ',a,'.')
       go to 9890

9400   write(amessage,9410) trim(afiletype),trim(facfile)
9410   format('Out-of-range array index encounted while reading ',a,' factor file ',a,'.')
       go to 9890

9890   interpolate_blend_using_file=1

9900   continue
       if(iunit.ne.0)then
         close(unit=iunit,iostat=ierr)
       end if

       return

end function interpolate_blend_using_file


integer (kind=c_int) function ipd_interpolate_2d(npts,             &
                                        ecs,ncs,zns,sourceval,     &
                                        mpts,                      &
                                        ect,nct,znt,targval,       &
                                        transtype,                 &
                                        anis,bearing,invpow)       &
                 bind(C,name="ipd_interpolate_2d")
!DIR$ ATTRIBUTES DLLEXPORT :: ipd_interpolate_2d

! -- This function undertakes 2D inverse-power-of-distance spatial interpolation.

       use iso_c_binding, only: c_int,c_double
       use dimvar
       use utilities
       implicit none

       integer(kind=c_int), intent(in)           :: npts                    ! number of source points
       real(kind=c_double), intent(in)           :: ecs(npts),ncs(npts)     ! source point coordinates
       integer(kind=c_int), intent(in)           :: zns(npts)               ! source point zones
       real(kind=c_double), intent(in)           :: sourceval(npts)         ! source values
       integer(kind=c_int), intent(in)           :: mpts                    ! number of target points
       real(kind=c_double), intent(in)           :: ect(mpts),nct(mpts)     ! target point coords
       integer(kind=c_int), intent(in)           :: znt(mpts)               ! target point zones
       real(kind=c_double), intent(out)          :: targval(mpts)           ! target (i.e. interpolated) values
       integer(kind=c_int), intent(in)           :: transtype               ! 1:log,0:none
       real(kind=c_double), intent(in)           :: anis(mpts)              ! local anisotropy
       real(kind=c_double), intent(in)           :: bearing(mpts)           ! local anisotropy bearing
       real(kind=c_double), intent(in)           :: invpow(mpts)            ! local inverse power of distance

       integer, parameter  :: MAXZONE=20

       integer                     :: zone(MAXZONE)
       integer                     :: ipt,jpt
       integer                     :: izone,numzone
       integer                     :: iii,itemp
       double precision            :: dtor,power
       double precision            :: x1,y1,x2,y2
       double precision            :: aan,angle,cosang,sinang
       double precision            :: den,num
       double precision            :: dx,dy,dtempx,dtempy,dxs,dys,dists,dtemp,fac
       character (len=25)          :: anum,varname

! -- Initialisation

       ipd_interpolate_2d=0
       function_name='ipd_interpolate_2d()'
       dtor=3.1415926535898d0/180.0d0

! -- Check input arguments.

       if((transtype.ne.0).and.(transtype.ne.1))then
         write(amessage,100) 'TRANSTYPE',trim(function_name)
100      format('The ',a,' argument of function ',a,' must be ',  &
         'supplied as 0 or 1.')
         go to 9890
       end if
       if(npts.le.0)then
         varname='NPTS'
         go to 9000
       end if
       if(mpts.le.0)then
         varname='MPTS'
         go to 9000
       end if
       if(all(zns.eq.0))then
         varname='ZNS'
         go to 9100
       end if
       if(all(znt.eq.0))then
         varname='ZNT'
         go to 9100
       end if
       do ipt=1,mpts
         if(znt(ipt).ne.0)then
           if(anis(ipt).le.0.0d0)then
             write(amessage,170)
170          format('At least one ANIS value is zero or negative for a target point to ',  &
             'which interpolation is required.')
             go to 9890
           end if
           if((bearing(ipt).lt.-360.0d0).or.(bearing(ipt).gt.360.0d0))then
             write(amessage,180)
180          format('At least one BEARING value is less than -360 or greater than ',  &
             '360 for a target point to which interpolation is required.')
             go to 9890
           end if
           if(invpow(ipt).lt.0.0d0)then
             write(amessage,181)
181          format('At least one INVPOW value is less than zero ',                   &
             'for a target point to which interpolation is required.')
             go to 9890
           end if
         end if
       end do
       if(transtype.eq.1)then
         do ipt=1,npts
           if(zns(ipt).ne.0)then
             if(sourceval(ipt).le.0.0d0)then
               write(amessage,190)
190            format('If TRANSTYPE is set to 1 then all source values in non-zero ',  &
               'zones must be positive.')
               go to 9890
             end if
           end if
         end do
       end if

! -- Build up an array of zone numbers.

       izone=1
       numzone=0
       do ipt=1,mpts
         itemp=znt(ipt)
         if(itemp.ne.0)then
           if(numzone.eq.0)then
             numzone=1
             zone(numzone)=itemp
           else
             if(utl_whichone(numzone,izone,zone,itemp).ne.0)then
               numzone=numzone+1
               if(numzone.gt.MAXZONE)then
                 call utl_num2char(MAXZONE,anum)
                 write(amessage,150) trim(anum)
150              format('A maximum of only ',a,' different zones can feature in the ZNT array.')
                 go to 9890
               end if
               zone(numzone)=itemp
             end if
           end if
         end if
       end do

! -- Check that all zones in the target array are respected in the source array.

       do izone=1,numzone
         itemp=zone(izone)
         do ipt=1,npts
           if(zns(ipt).eq.itemp) go to 165
         end do
         call utl_num2char(itemp,anum)
         write(amessage,160) trim(anum)
160      format('Zone ',a,' from the ZNT array is not represented in the ZNS array.')
         go to 9890
165      continue
       end do

! -- Now we do the interpolation.

       zones: do izone=1,numzone
         iii=zone(izone)
         targpoints: do ipt=1,mpts
           if(znt(ipt).eq.iii)then
             x1=ect(ipt)
             y1=nct(ipt)
             aan=anis(ipt)
             angle=90.0d0-bearing(ipt)+90.0d0
             cosang=cos(angle*dtor)
             sinang=sin(angle*dtor)
             power=sqrt(invpow(ipt))
             den=0.0d0
             num=0.0d0
             do jpt=1,npts
               if(zns(jpt).eq.iii)then
                 x2=ecs(jpt)
                 y2=ncs(jpt)
                 dx=x2-x1
                 dy=y2-y1
                 dtempx=dx*cosang+dy*sinang
                 dtempy=-dx*sinang+dy*cosang
                 dtempx=dtempx*aan
                 dxs=dtempx*dtempx
                 dys=dtempy*dtempy
                 dists=dxs+dys
                 if(dists.lt.1.0d-30)then
                   fac=1.0d30
                 else
                   fac=1.0d0/(dists**power)
                 end if
                 if(transtype.eq.0)then
                   num=num+fac*sourceval(jpt)
                 else
                   num=num+fac*log(sourceval(jpt))
                 end if
                 den=den+fac
               end if
             end do
             dtemp=num/den
             if(transtype.eq.1)dtemp=exp(dtemp)
             targval(ipt)=dtemp
           end if
         end do targpoints
       end do zones

       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array are supplied as zero.')
       go to 9890

9890   ipd_interpolate_2d=1

9900   continue

       return

end function ipd_interpolate_2d



integer (kind=c_int) function ipd_interpolate_3d(npts,                &
                                        ecs,ncs,zcs,zns,sourceval,    &
                                        mpts,                         &
                                        ect,nct,zct,znt,targval,      &
                                        transtype,                    &
                                        ahmax,ahmin,avert,            &
                                        bearing,dip,rake,             &
                                        invpow)                       &
                 bind(C,name="ipd_interpolate_3d")
!DIR$ ATTRIBUTES DLLEXPORT :: ipd_interpolate_3d

! -- This function undertakes 2D inverse-power-of-distance spatial interpolation.

       use iso_c_binding, only: c_int,c_double
       use dimvar
       use utilities
       implicit none

       integer(kind=c_int), intent(in)    :: npts                                ! number of source points
       real(kind=c_double), intent(in)    :: ecs(npts),ncs(npts),zcs(npts)       ! source point coordinates
       integer(kind=c_int), intent(in)    :: zns(npts)                           ! source point zones
       real(kind=c_double), intent(in)    :: sourceval(npts)                     ! source values
       integer(kind=c_int), intent(in)    :: mpts                                ! number of target points
       real(kind=c_double), intent(in)    :: ect(mpts),nct(mpts),zct(mpts)       ! target point coords
       integer(kind=c_int), intent(in)    :: znt(mpts)                           ! target point zones
       real(kind=c_double), intent(out)   :: targval(mpts)                       ! target (i.e. interpolated) values
       integer(kind=c_int), intent(in)    :: transtype                           ! 1:log,0:none
       real(kind=c_double), intent(in)    :: ahmax(mpts),ahmin(mpts),avert(mpts) ! relative correlation lengths
       real(kind=c_double), intent(in)    :: bearing(mpts),dip(mpts),rake(mpts)  ! correlation directions
       real(kind=c_double), intent(in)    :: invpow(mpts)                        ! local inverse power of distance

       integer, parameter  :: MAXZONE=10

       integer                     :: zone(MAXZONE)
       integer                     :: ipt,jpt
       integer                     :: izone,numzone
       integer                     :: iii,itemp
       double precision            :: pidiv180,power,angle
       double precision            :: x1,y1,z1
       double precision            :: cosang1,cosang2,cosang3
       double precision            :: sinang1,sinang2,sinang3
       double precision            :: den_along,den_along2,den_amid,den_amid2, &
                                      den_avert,den_avert2
       double precision            :: xdiff,ydiff,zdiff
       double precision            :: xd,yd,zd,xdd,ydd,zdd
       double precision            :: dlong,dmid,dvert
       double precision            :: dtemp,dtemp1,dtemp2,dtemp3
       double precision            :: den,num,fac
       character (len=25)          :: anum,varname

! -- Initialisation

       ipd_interpolate_3d=0
       function_name='ipd_interpolate_3d()'
       pidiv180=3.1415926535898d0/180.0d0

! -- Check input arguments.

       if((transtype.ne.0).and.(transtype.ne.1))then
         write(amessage,100) 'TRANSTYPE',trim(function_name)
100      format('The ',a,' argument of function ',a,' must be ',  &
         'supplied as 0 or 1.')
         go to 9890
       end if
       if(npts.le.0)then
         varname='NPTS'
         go to 9000
       end if
       if(mpts.le.0)then
         varname='MPTS'
         go to 9000
       end if
       if(all(zns.eq.0))then
         varname='ZNS'
         go to 9100
       end if
       if(all(znt.eq.0))then
         varname='ZNT'
         go to 9100
       end if
       do ipt=1,mpts
         if(znt(ipt).ne.0)then
           if(ahmax(ipt).le.0.0d0)then
             write(amessage,170) 'AHMAX'
170          format('At least one ',a,' value is zero or negative for a target point to ',  &
             'which interpolation is required.')
             go to 9890
           end if
           if(ahmin(ipt).le.0.0d0)then
             write(amessage,170) 'AHMIN'
             go to 9890
           end if
           if(avert(ipt).le.0.0d0)then
             write(amessage,170) 'AVERT'
             go to 9890
           end if
           if((bearing(ipt).lt.-360.0d0).or.(bearing(ipt).gt.360.0d0))then
             write(amessage,180)
180          format('At least one BEARING value is less than -360 degrees or greater than ',  &
             '360 degrees for a target point to which interpolation is required.')
             go to 9890
           end if
           if((dip(ipt).lt.-180.0d0).or.(dip(ipt).gt.180.0d0))then
             write(amessage,185)
185          format('At least one DIP value is less than -180 degrees or greater than ',  &
             '180 degrees for a target point to which interpolation is required.')
             go to 9890
           end if
           if((rake(ipt).lt.-90.0d0).or.(rake(ipt).gt.90.0d0))then
             write(amessage,186)
186          format('At least one RAKE value is less than -90 degrees or greater than ',  &
             '90 degrees for a target point to which interpolation is required.')
             go to 9890
           end if
           if(invpow(ipt).lt.0.0d0)then
             write(amessage,181)
181          format('At least one INVPOW value is less than zero ',                   &
             'for a target point to which interpolation is required.')
             go to 9890
           end if
         end if
       end do
       if(transtype.eq.1)then
         do ipt=1,npts
           if(zns(ipt).ne.0)then
             if(sourceval(ipt).le.0.0d0)then
               write(amessage,190)
190            format('If TRANSTYPE is set to 1 then all SOURCEVAL values in non-zero ',  &
               'zones must be positive.')
               go to 9890
             end if
           end if
         end do
       end if

! -- Build up an array of zone numbers.

       izone=1
       numzone=0
       do ipt=1,mpts
         itemp=znt(ipt)
         if(itemp.ne.0)then
           if(numzone.eq.0)then
             numzone=1
             zone(numzone)=itemp
           else
             if(utl_whichone(numzone,izone,zone,itemp).ne.0)then
               numzone=numzone+1
               if(numzone.gt.MAXZONE)then
                 call utl_num2char(MAXZONE,anum)
                 write(amessage,150) trim(anum)
150              format('A maximum of only ',a,' different zones can feature in the ZNT array.')
                 go to 9890
               end if
               zone(numzone)=itemp
             end if
           end if
         end if
       end do

! -- Check that all zones in the target array are respected in the source array.

       do izone=1,numzone
         itemp=zone(izone)
         do ipt=1,npts
           if(zns(ipt).eq.itemp) go to 165
         end do
         call utl_num2char(itemp,anum)
         write(amessage,160) trim(anum)
160      format('Zone ',a,' from the ZNT array is not represented in the ZNS array.')
         go to 9890
165      continue
       end do

! -- Now we do the interpolation.

       zones: do izone=1,numzone
         iii=zone(izone)
         targpoints: do ipt=1,mpts
           if(znt(ipt).eq.iii)then
             x1=ect(ipt)
             y1=nct(ipt)
             z1=zct(ipt)
             angle=90.0d0-bearing(ipt)
             cosang1=cos(angle*pidiv180)
             sinang1=sin(angle*pidiv180)
             cosang2=cos(dip(ipt)*pidiv180)
             sinang2=sin(dip(ipt)*pidiv180)
             cosang3=cos(rake(ipt)*pidiv180)
             sinang3=sin(rake(ipt)*pidiv180)
             den_along=1.0d0
             den_along2=1.0d0
             den_amid=ahmax(ipt)/ahmin(ipt)
             den_amid2=den_amid*den_amid
             den_avert=ahmax(ipt)/avert(ipt)
             den_avert2=den_avert*den_avert
             power=sqrt(invpow(ipt))
             den=0.0d0
             num=0.0d0
             do jpt=1,npts
               if(zns(jpt).eq.iii)then
                 xdiff=ecs(jpt)-x1
                 ydiff=ncs(jpt)-y1
                 zdiff=zcs(jpt)-z1
                 xd=xdiff*cosang1+ydiff*sinang1
                 yd=-xdiff*sinang1+ydiff*cosang1
                 zd=zdiff
                 xdd=xd*cosang2+zd*sinang2
                 ydd=yd
                 zdd=-xd*sinang2+zd*cosang2
                 dlong=xdd
                 dmid=ydd*cosang3+zdd*sinang3
                 dvert=-ydd*sinang3+zdd*cosang3
                 dtemp1=dlong*dlong*den_along2
                 dtemp2=dmid*dmid*den_amid2
                 dtemp3=dvert*dvert*den_avert2
                 dtemp=dtemp1+dtemp2+dtemp3
                 if(dtemp.lt.1.0d-30)then
                   fac=1.0d30
                 else
                   fac=1.0d0/(dtemp**power)
                 end if
                 if(transtype.eq.0)then
                   num=num+fac*sourceval(jpt)
                 else
                   num=num+fac*log(sourceval(jpt))
                 end if
                 den=den+fac
               end if
             end do
             dtemp=num/den
             if(transtype.eq.1)dtemp=exp(dtemp)
             targval(ipt)=dtemp
           end if
         end do targpoints
       end do zones
       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9100   write(amessage,9110) trim(varname)
9110   format('All elements of the ',a,' array are supplied as zero.')
       go to 9890

9070   write(amessage,9080) trim(varname)
9080   format('The value supplied for ',a,' for at least one target point is zero or less.')
       go to 9890


9890   ipd_interpolate_3d=1

9900   continue

       return

end function ipd_interpolate_3d



integer (kind=c_int) function initialize_randgen(iseed) &
                 bind(C,name="initialize_randgen")
!DIR$ ATTRIBUTES DLLEXPORT :: initialize_randgen

! -- Initialize the random number generator.

       use iso_c_binding, only: c_int
       use utilities
       integer(kind=c_int), intent(in)   :: iseed
       integer                           :: seed_size

! -- Initialization

       function_name='initialize_randgen()'
       initialize_randgen=0

       call random_seed(size=seed_size) ! find out size of seed
       if(.not.allocated(seed))allocate(seed(seed_size))
       seed=iseed                 ! an array
       call random_seed(put=seed) ! set current seed
       return

end function initialize_randgen



integer (kind=c_int) function fieldgen2d_sva(                &
                              nnode,                         &
                              ec,nc,area,active,             &
                              mean,var,aa,anis,bearing,      &
                              transtype,avetype,power,       &
                              ldrand,nreal,randfield)        &
                 bind(C,name="fieldgen2d_sva")
!DIR$ ATTRIBUTES DLLEXPORT :: fieldgen2d_sva

! -- This function generates stochastic fields based on a spatially varying variogram.
! -- It does this by 2D spatial convolution of an averaging function.

       use iso_c_binding, only: c_int,c_double
       use dimvar
       use utilities
       implicit none

       integer(kind=c_int), intent(in)   :: nnode                  ! Nodes in model grid
       real(kind=c_double), intent(in)   :: ec(nnode),nc(nnode)    ! x and y coordinates of nodes
       real(kind=c_double), intent(in)   :: area(nnode)            ! Areas of grid cells
       integer(kind=c_int), intent(in)   :: active(nnode)          ! 0=inactive cell
       real(kind=c_double), intent(in)   :: mean(nnode)            ! Mean value of stochastic field
       real(kind=c_double), intent(in)   :: var(nnode)             ! Variance of stochastic field
       real(kind=c_double), intent(in)   :: aa(nnode)              ! Averaging function spatial dimension
       real(kind=c_double), intent(in)   :: anis(nnode)            ! Anisotropy ratio
       real(kind=c_double), intent(in)   :: bearing(nnode)         ! Bearing of principal anisotropy axis
       integer(kind=c_int), intent(in)   :: transtype              ! Stochastic field pertains to natural(0) or log(1) props
       integer(kind=c_int), intent(in)   :: avetype                ! 1:spher,2:exp,3:gauss,4:pow
       real(kind=c_double), intent(in)   :: power                  ! Power used in power AVETYPE
       integer(kind=c_int), intent(in)   :: ldrand                 ! Leading dimension of RANDFIELD
       integer(kind=c_int), intent(in)   :: nreal                  ! Number of realisations to generate
       real(kind=c_double), intent(out)  :: randfield(ldrand,nreal)! Realisations

       integer                 :: jnode,knode,ierr
       integer                 :: ireal
       double precision        :: xi,yi
       double precision        :: vvar,along,amid,cosang1,sinang1,ang1
       double precision        :: den_along,den_along2,den_amid,den_amid2,den
       double precision        :: xdiff,ydiff,dlong,dmid
       double precision        :: dtemp,dtemp1,dtemp2,dtemp3
       double precision        :: dlim,pidiv180
       character(len=25)       :: varname

       double precision, allocatable :: diid(:,:)

! -- Initialisation

       fieldgen2d_sva=0
       function_name='fieldgen2d_sva()'
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
           if(var(jnode).lt.0.0d0)then
             write(amessage,171) 'VAR'
171          format('At least one ',a,' value is negative for an active model node.')
             go to 9890
           end if
           if(aa(jnode).le.0.0d0)then
             write(amessage,170) 'AA'
             go to 9890
           end if
           if(anis(jnode).le.0.0d0)then
             write(amessage,170) 'ANIS'
             go to 9890
           end if
           if((bearing(jnode).lt.-360.0d0).or.(bearing(jnode).gt.360.0d0))then
             write(amessage,180)
180          format('At least one BEARING value is less than -360 degrees or greater than ',  &
             '360 degrees for an active model node.')
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
         if(active(jnode).ne.0)then
           xi=ec(jnode)
           yi=nc(jnode)
           vvar=var(jnode)
           along=aa(jnode)
           amid=aa(jnode)/anis(jnode)
           ang1=90.0d0-bearing(jnode)
           cosang1=cos(ang1*pidiv180)
           sinang1=sin(ang1*pidiv180)
           den_along=1.0d0/along
           den_along2=den_along*den_along
           den_amid=1.0d0/amid
           den_amid2=den_amid*den_amid
           den=0.0d0
           dvector1=0.0d0   ! an array
           do knode=1,nnode
             if(active(knode).eq.0) cycle
             xdiff=ec(knode)-xi
             ydiff=nc(knode)-yi
             dlong=xdiff*cosang1+ydiff*sinang1
             dmid=-xdiff*sinang1+ydiff*cosang1
             if(avetype.eq.2)then
               dtemp1=dlong*dlong*den_along2
               if(dtemp1.gt.dlim) cycle
               dtemp2=dmid*dmid*den_amid2
               if(dtemp2.gt.dlim) cycle
               dtemp=dtemp1+dtemp2
               if(dtemp.gt.dlim) cycle
               dtemp=sqrt(dtemp)
               dtemp=exp(-dtemp)
             else if(avetype.eq.3)then
               dtemp1=dlong*dlong*den_along2
               if(dtemp1.gt.dlim) cycle
               dtemp2=dmid*dmid*den_amid2
               if(dtemp2.gt.dlim) cycle
               dtemp=dtemp1+dtemp2
               if(dtemp.gt.dlim) cycle
               dtemp=exp(-dtemp)
             else if(avetype.eq.1)then
               dtemp1=abs(dlong*den_along)
               if(dtemp1.gt.1.0d0)cycle
               dtemp2=abs(dmid*den_amid)
               if(dtemp2.gt.1.0d0) cycle
               dtemp=dtemp1*dtemp1+dtemp2*dtemp2
               dtemp=sqrt(dtemp)
               dtemp3=dtemp*dtemp*dtemp
               dtemp=1.0d0-1.5*dtemp+0.5*dtemp3
               if(dtemp.lt.0.0d0) cycle
             else if(avetype.eq.4)then
               dtemp1=abs(dlong*den_along)
               if(dtemp1.gt.1.0d0) cycle
               dtemp2=abs(dmid*den_amid)
               if(dtemp2.gt.1.0d0) cycle
               dtemp=dtemp1*dtemp1+dtemp2*dtemp2
               dtemp=sqrt(dtemp)
               dtemp=1.0d0-(dtemp**power)
               if(dtemp.lt.0.0d0) cycle
             end if
             dtemp=dtemp*area(knode)
             do ireal=1,nreal
                dvector1(ireal)=dvector1(ireal)+dtemp*diid(knode,ireal)
             end do
             den=den+dtemp*dtemp
           end do
           do ireal=1,nreal
             dtemp=dvector1(ireal)*sqrt(vvar/den)              ! Ensures that it has the correct variance
             if(transtype.eq.0)then
               randfield(jnode,ireal)=mean(jnode)+dtemp
             else
               if(dtemp.gt.100.0d0) dtemp=100.0d0
               randfield(jnode,ireal)=mean(jnode)*(10**dtemp)
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

9890   fieldgen2d_sva=1

9900   continue
       if(allocated(diid)) deallocate(diid,stat=ierr)

       return

end function fieldgen2d_sva



integer (kind=c_int) function fieldgen3d_sva(                &
                              nnode,                         &
                              ec,nc,zc,                      &
                              area,height,active,            &
                              mean,var,                      &
                              ahmax,ahmin,avert,             &
                              bearing,dip,rake,              &
                              transtype,avetype,power,       &
                              ldrand,nreal,randfield)        &
                 bind(C,name="fieldgen3d_sva")
!DIR$ ATTRIBUTES DLLEXPORT :: fieldgen3d_sva

! -- This function generates 3D stochastic fields based on a spatially varying variogram.
! -- It does this by spatial convolution using an averaging function.

       use iso_c_binding, only: c_int,c_double
       use dimvar
       use utilities
       implicit none

       integer(kind=c_int), intent(in)   :: nnode                       ! Nodes in model grid
       real(kind=c_double), intent(in)   :: ec(nnode),nc(nnode),zc(nnode)! x,y,z coordinates of nodes
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
       double precision        :: cosang2,sinang2,cosang3,sinang3
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
107      format('LDRAND must equal or exceed NNODE in call to function ',a,'.')
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
         if(active(jnode).ne.0)then
           xi=ec(jnode)
           yi=nc(jnode)
           zi=zc(jnode)
           vvar=var(jnode)
           along=ahmax(jnode)
           amid=ahmin(jnode)
           aavert=avert(jnode)
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
           den_avert=1.0d0/aavert
           den_avert2=den_avert*den_avert
           den=0.0d0
           dvector1=0.0d0   ! an array
           do knode=1,nnode
             if(active(knode).eq.0)cycle
             xdiff=ec(knode)-xi
             ydiff=nc(knode)-yi
             zdiff=zc(knode)-zi
             xd=xdiff*cosang1+ydiff*sinang1
             yd=-xdiff*sinang1+ydiff*cosang1
             zd=zdiff
             xdd=xd*cosang2+zd*sinang2
             ydd=yd
             zdd=-xd*sinang2+zd*cosang2
             dlong=xdd
             dmid=ydd*cosang3+zdd*sinang3
             dvert=-ydd*sinang3+zdd*cosang3
             if(avetype.eq.2)then
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
             else if(avetype.eq.3)then
               dtemp1=dlong*dlong*den_along2
               if(dtemp1.gt.dlim) cycle
               dtemp2=dmid*dmid*den_amid2
               if(dtemp2.gt.dlim) cycle
               dtemp3=dvert*dvert*den_avert2
               if(dtemp3.gt.dlim) cycle
               dtemp=dtemp1+dtemp2+dtemp3
               if(dtemp.gt.dlim) cycle
               dtemp=exp(-dtemp)
             else if(avetype.eq.1)then
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
             else if(avetype.eq.4)then
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
             dtemp=dvector1(ireal)*sqrt(vvar/den)          ! Ensures that it has the correct variance
             if(transtype.eq.0)then
               randfield(jnode,ireal)=mean(jnode)+dtemp
             else
               if(dtemp.gt.100.0d0) dtemp=100.0d0
               randfield(jnode,ireal)=mean(jnode)*(10**dtemp)
             end if
           end do
         end if
       end do

       go to 9900

9000   write(amessage,9010) trim(varname),trim(function_name)
9010   format('The ',a,' argument of function ',a,' must be greater than zero.')
       go to 9890

9100   write(amessage,9110) trim(varname),trim(function_name)
9110   format('All elements of the ',a,' array are supplied as zero in call to ',  &
       'function ',a,'.')
       go to 9890

9200   write(amessage,9210) trim(function_name)
9210   format('Memory management error encountered in function ',a,'.')
       go to 9890

9890   fieldgen3d_sva=1

9900   continue
       if(allocated(diid)) deallocate(diid,stat=ierr)

       return

end function fieldgen3d_sva



subroutine free_param_memory1()
       use used_by_kb2d_1
       implicit none
       integer  :: ierr
       if(allocated(xa))deallocate(xa,stat=ierr)
       if(allocated(ya))deallocate(ya,stat=ierr)
       if(allocated(dist))deallocate(dist,stat=ierr)
       if(allocated(r))deallocate(r,stat=ierr)
       if(allocated(rr))deallocate(rr,stat=ierr)
       if(allocated(s))deallocate(s,stat=ierr)
       if(allocated(a))deallocate(a,stat=ierr)
       if(allocated(nums))deallocate(nums,stat=ierr)
       return
end subroutine free_param_memory1


subroutine free_param_memory2
       use geostat_3d
       implicit none
       integer ierr
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
end subroutine free_param_memory2


