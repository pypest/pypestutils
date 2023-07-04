MODULE DEFTYPES

! -- General dimensioning parameters.

       use dimvar

! -- MF6-specific parameters and grid-independent data

       integer, parameter           :: MF6HEADERLEN=50
       integer, parameter           :: MAXLENTXT=120
       integer, parameter           :: NTXT_DIS=16
       integer, parameter           :: NTXT_DISV=20
       integer                      :: expect_dis_type(NTXT_DIS),expect_disv_type(NTXT_DISV)
       character (len=10)           :: expectdefn_dis(NTXT_DIS),expectdefn_disv(NTXT_DISV)
       character (len=MF6HEADERLEN) :: mf6header(4)
       character (len=MAXLENTXT)    :: definition(max(NTXT_DIS,NTXT_DISV))

       data expectdefn_dis  /'NCELLS    ','NLAY      ','NROW      ','NCOL      ',              &
                             'NJA       ','XORIGIN   ','YORIGIN   ','ANGROT    ',              &
                             'DELR      ','DELC      ','TOP       ','BOTM      ',              &
                             'IA        ','JA        ','IDOMAIN   ','ICELLTYPE ' /

       data expect_dis_type /1,1,1,1,1,2,2,2,2,2,2,2,1,1,1,1/


       data expectdefn_disv /'NCELLS    ','NLAY      ','NCPL      ','NVERT     ',              &
                             'NJAVERT   ','NJA       ','XORIGIN   ','YORIGIN   ',              &
                             'ANGROT    ','TOP       ','BOTM      ','VERTICES  ',              &
                             'CELLX     ','CELLY     ','IAVERT    ','JAVERT    ',              &
                             'IA        ','JA        ','IDOMAIN   ','ICELLTYPE ' /

       data expect_disv_type /1,1,1,1,1,1,2,2,2,2,2,2,2,2,1,1,1,1,1,1/

! -- Structured grid type

       type strucgrid
         character (len=LENGRIDNAME)              :: name=' '
         integer                                  :: ncol=0
         integer                                  :: nrow=0
         integer                                  :: nlay=0
         double precision                         :: e0,n0    ! top left corner
         double precision                         :: rotation,cosang,sinang
         double precision, dimension(:), pointer  :: delr,delc
       end type strucgrid

! -- MF6 grid type

       type mf6grid
         integer                     :: distype=0
         integer                     :: ncells=0
         integer                     :: ncrl=0
         integer                     :: ncr=0
         integer                     :: ncol=0
         integer                     :: nrow=0
         integer                     :: nlay=0
         integer                     :: ncpl=0
         integer                     :: nvert=0
         integer                     :: njavert=0
         integer                     :: nja=0
         double precision            :: xorigin,yorigin,angrot
         character (len=LENGRIDNAME) :: name=' '

         integer, pointer                   :: idomain(:,:,:),icelltype(:,:,:)
         integer, pointer                   :: iavert(:),javert(:)
         integer, pointer                   :: idomainv(:,:),icelltypev(:,:)
         integer, pointer                   :: ivv(:,:,:)
         integer, pointer                   :: ibound(:,:)
         integer, pointer                   :: nvertcon(:),vertconcell(:,:),identvert(:,:)
         integer, pointer                   :: ia(:),ja(:)
         double precision, pointer          :: delc(:),delr(:)
         double precision, pointer          :: botm(:,:,:)
         double precision, pointer          :: botmv(:,:)
         double precision, pointer          :: vertices(:,:),cellx(:),celly(:)
         double precision, pointer          :: bottom(:,:)
       end type mf6grid

! -- Incidences of grids.

       type (strucgrid), dimension(MAXSTRUCMODGRID) :: strucmodgrid
       integer                                      :: numstrucmodgrid=0
       type (mf6grid), dimension(MAXMF6MODGRID)     :: mf6modgrid
       integer                                      :: nummf6modgrid=0

END MODULE DEFTYPES