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



