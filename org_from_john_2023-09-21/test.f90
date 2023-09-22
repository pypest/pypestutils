       program test

       implicit none

       character *256 :: facfile

! -- Required by SGSIM function.

       integer                       :: nnpts,iunit
       integer                       :: unestimated,idummy_num2
       integer                       :: n_nst
       integer                       :: i_it(1)
       integer                       :: minpoints,maxpoints,krtype
       integer                       :: idummy_ivector2
       real                          :: pmx,sr,smean,c_c0
       real                          :: c_cc(1),a_ang(1),a_anis(1),a_aa(1)
       real                          :: dummy_rvector3(1),dummy_rvector4(1) ! PLPROC names
       character (len=10)            :: af1,af2
       integer, allocatable          :: ivector1(:)                    ! PLPROC names
       real, allocatable             :: rvector1(:),rvector2(:)        ! PLPROC names
       character (len=1)             :: outfiletype


       facfile='test.dat'
       iunit=20
       nnpts=5

! -- We open the factor file.

       open(unit=iunit,file=facfile,action='write')
       write(iunit,220) 'dfsdf'
220    format(a)
       write(iunit,230) nnpts,1
230    format(2i10)


! -- Allocate some work arrays. (Use same names as PLPROC.)

       allocate(rvector1(nnpts),rvector2(nnpts))
       allocate(ivector1(nnpts))
       rvector1(1)=1.0
       rvector1(2)=1.5
       rvector1(3)=1.0
       rvector1(4)=2.0
       rvector1(5)=1.0
       rvector2(1)=2.0
       rvector2(2)=2.5
       rvector2(3)=0.0
       rvector2(4)=1.0
       rvector2(5)=1.5
       ivector1=1  ! an array

       minpoints=1
       maxpoints=4
       krtype=1
       n_nst=1
       c_c0=0.0
       i_it(1)=2
       c_cc(1)=1.0
       pmx=10000.0
       sr=20.0
       smean=0.0
       outfiletype='f'
       idummy_num2=1
       af1='i10'
       af2='i10'
       a_ang=0.0
       a_aa=5.0
       a_anis=1.0
       unestimated=0
       idummy_ivector2=2
       dummy_rvector3(1)=0.5
       dummy_rvector4(1)=0.7


             call kb2d_1(minpoints,maxpoints,sr,krtype,smean,                &
             n_nst,c_c0,i_it,c_cc,a_ang,a_aa,a_anis,                         &
             unestimated,idummy_num2,nnpts,ivector1,idummy_ivector2,dummy_rvector3,dummy_rvector4,  &
             outfiletype,iunit,pmx,rvector1,rvector2,af1,af2)

       write(6,*) 'unestimated = ',unestimated

end
