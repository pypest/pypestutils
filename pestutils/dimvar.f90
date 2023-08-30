module dimvar

! -- Dimensioning and size

       integer, parameter :: LENCLINE=1000
       integer, parameter :: LENFILENAME=257
       integer, parameter :: LENMESSAGE=1500
       integer, parameter :: LENGRIDNAME=201
       integer, parameter :: LENFUNCNAME=150
       integer, parameter :: MAXSTRUCMODGRID=5
       integer, parameter :: MAXMF6MODGRID=5
       integer, parameter :: MAXINTERPVERT=10  ! Maximum no. of vertices for interpolation
       integer, parameter :: LENFACCODE=20     ! Characters in factor file code

end module dimvar
