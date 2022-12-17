!                 ****************
                  MODULE UTILS_MED
!                 ****************
!
!***********************************************************************
! HERMES  V7P0                                                 2015
!***********************************************************************
!
!brief    INTERFACES OF MED PUBLIC SUBROUTINES
!
!warning  NOTE THAT THIS INTERFACE IS FOR THE LIBRARY INCLUDED IN MED
!+     V3.0.4 CHANGES MIGHT BE REQUIRED WITH FUTURE VERSIONS OF MED.
!
!history YOANN AUDOUIN
!+       25/05/2015
!+       V7P0
!+       ADAPTING CODE TO MED V3.0.4
!
!history VINCENT STOBIAC
!+       23/05/2014
!+       V6P3
!+       ADDING GENERAL MED SUBROUTINES
!
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated mesh
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE HASH_TABLE
!
      IMPLICIT NONE
#if defined (HAVE_MED)
      INCLUDE 'med.hf'
#endif
      INTEGER, PARAMETER :: KID=K8
#if defined (HAVE_MED64)
      INTEGER, PARAMETER :: KMED_INT=8
#else
      INTEGER, PARAMETER :: KMED_INT=4
#endif
      ! Forcing conversion to KMED_INT because all med integer must be
      ! kind_KMED_INT
#if defined (HAVE_MED)
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NO_DT = MED_NO_DT
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NO_IT = MED_NO_IT
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_ACC_RDONLY = MED_ACC_RDONLY
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_ACC_RDWR = MED_ACC_RDWR
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_ACC_CREAT = MED_ACC_CREAT
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_CONNECTIVITY = MED_CONNECTIVITY
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NODAL = MED_NODAL
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_CELL = MED_CELL
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_FULL_INTERLACE = MED_FULL_INTERLACE
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_NO_INTERLACE = MED_NO_INTERLACE
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_COORDINATE = MED_COORDINATE
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NODE = MED_NODE
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NONE = MED_NONE
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_POINT1 = MED_POINT1
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_SEG2 = MED_SEG2
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_TRIA3 = MED_TRIA3
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_QUAD4 = MED_QUAD4
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_TETRA4 = MED_TETRA4
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_PENTA6 = MED_PENTA6
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_SORT_DTIT = MED_SORT_DTIT
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_UNSTRUCTURED_MESH = MED_UNSTRUCTURED_MESH
      INTEGER(KIND=KMED_INT), PARAMETER ::
     &         KMED_CARTESIAN = MED_CARTESIAN
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_INT32 = MED_INT32
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_FLOAT64 = MED_FLOAT64
#else
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NO_DT = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NO_IT = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_ACC_RDONLY = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_ACC_RDWR = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_ACC_CREAT = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_CONNECTIVITY = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NODAL = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_CELL = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_FULL_INTERLACE = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NO_INTERLACE = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_COORDINATE = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NODE = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_NONE = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_POINT1 = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_SEG2 = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_TRIA3 = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_QUAD4 = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_TETRA4 = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_PENTA6 = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_SORT_DTIT = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_UNSTRUCTURED_MESH = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_CARTESIAN = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_INT32 = 0
      INTEGER(KIND=KMED_INT), PARAMETER :: KMED_FLOAT64 = 0
#endif
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NEW OBJECT RELATED TO MESHES IN A MED FILE
      TYPE MED_INFO
        INTEGER(KIND=KID) :: ID
        CHARACTER(LEN=250) :: FILE_NAME
        INTEGER :: NCLI
        INTEGER, ALLOCATABLE :: NBOR(:)
        ! Numbering for boundary segment
        INTEGER, ALLOCATABLE :: NBOR_SEG(:)
        ! For each boundary point gives segments containing it
        INTEGER, ALLOCATABLE :: PT2SEG(:,:)
        ! Contains for each boundary element type if it is indeed a
        ! boundary
        LOGICAL, ALLOCATABLE :: IS_BND(:)
        LOGICAL :: NO_BND
        ! Number of boundary groups
        INTEGER :: NBND_GRP, NBND_USED_GRP
        ! Contains for each boundary group its values for h u v tracer
        INTEGER, ALLOCATABLE :: BND_GRP_VAL(:,:)
        ! Contains for each family its number and the index of the
        ! boundary group associated
        ! 0 if it is not a boundary family
        INTEGER, ALLOCATABLE :: BND_FAM(:,:)
#if defined HAVE_MED
        CHARACTER(LEN=MED_NAME_SIZE) :: MESH_NAME
        ! Contains for boundary group its name
        CHARACTER(LEN=MED_LNAME_SIZE), ALLOCATABLE :: BND_GRP_NAME(:)
#endif
        !NUMBER OF OUR PART OF THE MESH, USED IN THE MED FUNCTION TO
        !ACCESS THE RIGHT MESH IN THE FILE
        INTEGER :: MESH_NUMBER
        CHARACTER(LEN=50) :: MESH_NUMBER_STR
        !CLI INDEX FILE
        INTEGER :: CLI_IDX_ID
        INTEGER :: CLI_LINE_BEGIN
      END TYPE MED_INFO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
!     HASH TABLE FOR FILES
      INTEGER :: HASH(MAX_FILE) = 0
!
      TYPE(MED_INFO) :: MY_OBJ_TAB(MAX_FILE)
      ! Value of lihbor for a solid boundary
      INTEGER, PARAMETER :: SOL_BND = 2
      ! OFFSET for families on points when in parallel
      INTEGER,PARAMETER :: OFFSET = 1000
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!
      SUBROUTINE OPEN_MESH_MED(FILE_NAME,FILE_ID,OPEN_MODE,IERR,
     &                         MESH_NUMBER)
!
!BRIEF    OPENS A MESH FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |<--| NAME OF THE MED FILE
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| OPEN_MODE      |<--| OPENING MODE (READ/READWRITE/WRITE)
!| IERR           |-->| ERROR TAG
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE  WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*),  INTENT(IN)  :: FILE_NAME
      INTEGER,           INTENT(OUT) :: FILE_ID
      CHARACTER(LEN=9),  INTENT(IN)  :: OPEN_MODE
      INTEGER,           INTENT(OUT) :: IERR
      INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
!
#if defined (HAVE_MED)
      INTEGER(KIND=KMED_INT) :: HDFOK, MEDOK ! CHECK COMPATIBILITY
      INTEGER(KIND=KMED_INT) :: KMED_MODE ! MED OPENING MODE
      ! MED VERSION OF THE FILE
      INTEGER(KIND=KMED_INT) :: MAJOR, MINOR, REL
      INTEGER :: MY_ID
      INTEGER(KIND=KID) :: FID
      CHARACTER(LEN=200) :: CFILE_NAME
      CHARACTER(LEN=MED_NAME_SIZE)    :: TITLE_MED
      CHARACTER(LEN=MED_SNAME_SIZE)   :: DT_UNIT
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_NAME(:)
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_UNIT(:)
      CHARACTER(LEN=MED_COMMENT_SIZE) :: COMMENT
!
      INTEGER(KIND=KMED_INT) :: IMESH             ! MESH DESCRIPTOR
      ! NUMBER OF PHYSICAL DIMENSIONS
      INTEGER(KIND=KMED_INT) :: NB_DIM_PB
      ! NUMBER OF MESH DIMENSIONS
      INTEGER(KIND=KMED_INT) :: NB_DIM_MESH
      INTEGER(KIND=KMED_INT) :: MESH_TYPE         ! TYPE OF THE MESH
      INTEGER(KIND=KMED_INT) :: ORDER ! SORTING ORDER FOR IT AND DT
      INTEGER(KIND=KMED_INT) :: NCOMP ! NUMBER OF COMPUTATION STEPS
      INTEGER(KIND=KMED_INT) :: COOR_TYPE         ! TYPE OF COORDINATES
      INTEGER(KIND=KMED_INT) :: NAXIS             ! NUMBER OF AXES
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     CHECK THE MODE. POSSIBLE MODES ARE READONLY, WRITEONLY OR
!     READWRITE. OTHER MODES ARE INVALID
      SELECT CASE(OPEN_MODE)
        CASE('READ     ')
          KMED_MODE = KMED_ACC_RDONLY
        CASE('READWRITE')
          KMED_MODE = KMED_ACC_RDWR
        CASE('WRITE    ')
          IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
            KMED_MODE = KMED_ACC_RDWR
          ELSE
            KMED_MODE = KMED_ACC_CREAT
          ENDIF
        CASE DEFAULT
!         OPEN MODE IS INVALID
          IERR = HERMES_INVALID_OPEN_MODE_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(FILE_NAME)//': '//
     &           'OPEN_MESH_MED'
          RETURN
      END SELECT
      CFILE_NAME = TRIM(FILE_NAME)//CHAR(0)

      HDFOK = 1
      MEDOK = 1
      IF(KMED_MODE.EQ.KMED_ACC_RDONLY) THEN
!       CHECK IF THE FILE IS BOTH A MED & HDF5 FILE
        CALL MFICOM(CFILE_NAME, HDFOK, MEDOK, KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(FILE_NAME)//': '//
     &         'OPEN_MESH_MED:MFICOM'
          RETURN
        ENDIF
      ENDIF
!
!     MESH FILE NOT COMPATIBLE WITH HDF5 OR MED
      IF (HDFOK.EQ.0) THEN
        IERR = HERMES_WRONG_HDF_FORMAT_ERR
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(FILE_NAME)//': '//
     &         'OPEN_MESH_MED'
        RETURN
      ENDIF
      IF (MEDOK.EQ.0) THEN
        IERR = HERMES_WRONG_MED_FORMAT_ERR
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(FILE_NAME)//': '//
     &         'OPEN_MESH_MED'
        RETURN
      ENDIF
!
!     OPEN THE MED FILE AND CHECK RETURN CODE
      CALL MFIOPE(FID, CFILE_NAME, KMED_MODE, KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(FILE_NAME)//': '//
     &         'OPEN_MESH_MED:MFIOPE'
        RETURN
      ENDIF
      FILE_ID = MAXVAL(HASH) + 100

!     CREATE A NEW OBJECT FOR THE NEW MED FILE
      CALL ADD_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'OPEN_MESH_MED:ADD_OBJ'
        RETURN
      ENDIF
      MY_OBJ_TAB(MY_ID)%FILE_NAME = FILE_NAME
      MY_OBJ_TAB(MY_ID)%ID = FID
!
!
      IF(KMED_MODE.EQ.KMED_ACC_RDONLY) THEN
!       CHECK COMPATIBILITY (EXPECTED)
        CALL MFINVR (FID, MAJOR, MINOR, REL, KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH_MED:MFINVR'
          RETURN
        ENDIF
!       MED FILE IS TOO OLD AND SHOULD BE CONVERTED WITH MEDIMPORT
        IF (MAJOR.LT.3) THEN
          IERR = HERMES_WRONG_MED_VERSION_ERR
          RETURN
        ENDIF
      ENDIF
!
      !
      IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
        MY_OBJ_TAB(MY_ID)%MESH_NUMBER=MESH_NUMBER
        !CONVERT INT TO STRING
        WRITE(MY_OBJ_TAB(MY_ID)%MESH_NUMBER_STR,'(I0)')MESH_NUMBER
      ELSE
        MY_OBJ_TAB(MY_ID)%MESH_NUMBER=1
        MY_OBJ_TAB(MY_ID)%MESH_NUMBER_STR=''
      ENDIF
!
!     Identify the name of the mesh if in read only
      IF(KMED_MODE.EQ.KMED_ACC_RDONLY.OR.
     &   (KMED_MODE.EQ.KMED_ACC_RDWR.AND..NOT.(PARTEL_CONCAT))) THEN
!       ONLY ONE MESH PER FILE FOR NOW
        IMESH = MY_OBJ_TAB(MY_ID)%MESH_NUMBER
!
        CALL MMHNAX(FID,IMESH,NAXIS,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH_MED:MMHNAX'
          RETURN
        ENDIF
        ALLOCATE(COOR_NAME(INT(NAXIS)),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING OPEN_MESH_MED:COOR_NAME'
          RETURN
        ENDIF
        ALLOCATE(COOR_UNIT(INT(NAXIS)),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING OPEN_MESH_MED:COOR_UNIT'
          RETURN
        ENDIF
!
!       READ MESH TITLE
        CALL MMHMII(FID,IMESH,TITLE_MED,NB_DIM_PB,NB_DIM_MESH,
     &    MESH_TYPE,COMMENT,DT_UNIT,ORDER,NCOMP,COOR_TYPE,COOR_NAME,
     &    COOR_UNIT,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH_MED:MMHMII'
          RETURN
        ENDIF
        DEALLOCATE(COOR_NAME)
        DEALLOCATE(COOR_UNIT)
!
        MY_OBJ_TAB(MY_ID)%MESH_NAME = TRIM(TITLE_MED)
      ENDIF
!
#else
!
      FILE_ID = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CLOSE_MESH_MED (FILE_ID, IERR)
!
!BRIEF    CLOSES A MESH FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: IERR
!
      INTEGER MY_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
#if defined (HAVE_MED)
!
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'CLOSE_MESH_MED:GET_OBJ'
        RETURN
      ENDIF
!
      ! Clearing id in the hash table
      HASH(MY_ID) = 0
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     CLOSE MED FILE
      CALL MFICLO(FID, KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH_MED:MFICLO'
        RETURN
      ENDIF
!
      ! CLEAN UP OF STRUCTURE
      MY_OBJ_TAB(MY_ID)%ID = 0
      MY_OBJ_TAB(MY_ID)%NCLI = 0
      MY_OBJ_TAB(MY_ID)%NO_BND = .TRUE.
      MY_OBJ_TAB(MY_ID)%NBND_GRP = 0
      MY_OBJ_TAB(MY_ID)%NBND_USED_GRP = 0
      MY_OBJ_TAB(MY_ID)%MESH_NAME = REPEAT(' ',MED_NAME_SIZE)
      IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%BND_GRP_VAL)) THEN
        DEALLOCATE(MY_OBJ_TAB(MY_ID)%BND_GRP_VAL)
      ENDIF
      IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%BND_GRP_NAME)) THEN
        DEALLOCATE(MY_OBJ_TAB(MY_ID)%BND_GRP_NAME)
      ENDIF
      MY_OBJ_TAB(MY_ID)%MESH_NUMBER=-1
      MY_OBJ_TAB(MY_ID)%MESH_NUMBER_STR=''
      MY_OBJ_TAB(MY_ID)%CLI_LINE_BEGIN=-1
      MY_OBJ_TAB(MY_ID)%CLI_IDX_ID=-1
      MY_OBJ_TAB(MY_ID)%ID = 0
      MY_OBJ_TAB(MY_ID)%NCLI = 0
      IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%NBOR)) THEN
        DEALLOCATE(MY_OBJ_TAB(MY_ID)%NBOR)
      ENDIF
      IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%NBOR_SEG)) THEN
        DEALLOCATE(MY_OBJ_TAB(MY_ID)%NBOR_SEG)
      ENDIF
      IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%IS_BND)) THEN
        DEALLOCATE(MY_OBJ_TAB(MY_ID)%IS_BND)
      ENDIF
      IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%BND_FAM)) THEN
        DEALLOCATE(MY_OBJ_TAB(MY_ID)%BND_FAM)
      ENDIF
      IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%PT2SEG)) THEN
        DEALLOCATE(MY_OBJ_TAB(MY_ID)%PT2SEG)
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE OPEN_BND_MED(FILE_NAME,FILE_ID,OPEN_MODE,IERR,
     &                        MESH_NUMBER)
!
!BRIEF    OPEN BOUNDARY FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |<--| NAME OF THE BOUNDARY FILE
!| FILE_ID        |<--| ID OF THE FILE
!| OPEN_MODE      |<--| OPENING MODE (READ/READWRITE/WRITE)
!| IERR           |-->| ERROR TAG
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE  WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE HERMES_INDEX_FILE
        IMPLICIT NONE
!
        CHARACTER(LEN=*),  INTENT(IN)  :: FILE_NAME
        INTEGER,           INTENT(IN)  :: FILE_ID
        CHARACTER(LEN=9),  INTENT(IN)  :: OPEN_MODE
        INTEGER,           INTENT(OUT) :: IERR
        INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
!
#if defined (HAVE_MED)
        INTEGER :: MY_ID,NCLI,NBND_GRP,I,DUMMY
        LOGICAL :: ISOPENED
        CHARACTER(LEN=9) :: REAL_OPENMODE
!
!-----------------------------------------------------------------------
!
        CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(FILE_NAME)//': '//
     &       'OPEN_MED_BND:GET_OBJ'
          RETURN
        ENDIF
        !
        REAL_OPENMODE=OPEN_MODE
        !
        MY_OBJ_TAB(MY_ID)%CLI_LINE_BEGIN=1
        IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
          !If we concatenate we need to be able to move into the file,
          !which can only be done by reading, so we need readwrite
          !access
          IF(OPEN_MODE(1:5)=='WRITE')THEN
            REAL_OPENMODE='READWRITE'
          ENDIF
          CALL OPEN_INDEX(FILE_NAME,MY_OBJ_TAB(MY_ID)%CLI_IDX_ID)
          CALL READ_INDEX(MY_OBJ_TAB(MY_ID)%CLI_IDX_ID,
     &                    OPEN_MODE,MESH_NUMBER,
     &                    MY_OBJ_TAB(MY_ID)%CLI_LINE_BEGIN,DUMMY)
        ENDIF
!
        ! First we check if the file is already opened
        ! Telemac is using one boundary file for all the mesh file
        ! so it could have been opened by another mesh before hand
        INQUIRE(FILE=FILE_NAME,OPENED=ISOPENED)
        IF(ISOPENED) THEN
          ! Id the file is already opened get its id
          INQUIRE(FILE=FILE_NAME,NUMBER=MY_OBJ_TAB(MY_ID)%NCLI)
        ELSE
          ! Otherwise open the file
          CALL GET_FREE_ID(MY_OBJ_TAB(MY_ID)%NCLI)
          OPEN(UNIT=MY_OBJ_TAB(MY_ID)%NCLI,FILE=FILE_NAME,
     &         FORM='FORMATTED',ACTION=REAL_OPENMODE,IOSTAT=IERR)
        ENDIF
        IF(OPEN_MODE(1:4).EQ.'READ') THEN
          ! Getting the name of the boundary groups
          NCLI = MY_OBJ_TAB(MY_ID)%NCLI
          REWIND(NCLI)
          !If the file is a concatenation, we need to move to the
          !begining of our part
          IF(PARTEL_CONCAT)THEN
            DO I=1,MY_OBJ_TAB(MY_ID)%CLI_LINE_BEGIN-1
              READ(MY_OBJ_TAB(MY_ID)%NCLI,*)
            ENDDO
          ENDIF
!         LOOP ON THE INFO IN THE FILE
          READ(UNIT=NCLI,FMT=*,IOSTAT=IERR) NBND_GRP
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'OPEN_BND_MED:READ'
            RETURN
          ENDIF
          MY_OBJ_TAB(MY_ID)%NBND_GRP = NBND_GRP
          ALLOCATE(MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(NBND_GRP,4),
     &             STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING BND_GRP_VAL'
            RETURN
          ENDIF
          ALLOCATE(MY_OBJ_TAB(MY_ID)%BND_GRP_NAME(NBND_GRP),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING BND_GRP_NAME'
            RETURN
          ENDIF
          DO I=1,NBND_GRP
            READ(UNIT=NCLI,FMT=*,IOSTAT=IERR)
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,1),
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,2),
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,3),
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,4),
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_NAME(I)
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'OPEN_BND_MED:READ'
              RETURN
            ENDIF
          ENDDO
        ELSE
          MY_OBJ_TAB(MY_ID)%NBND_GRP = 0
        ENDIF

#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CLOSE_BND_MED (FILE_ID, IERR, MESH_NUMBER)
!
!BRIEF    CLOSES A MESH BOUNDARY FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| IERR           |-->| ERROR TAG
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE  WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE HERMES_INDEX_FILE
        IMPLICIT NONE
!
        INTEGER,           INTENT(IN)  :: FILE_ID
        INTEGER,           INTENT(OUT) :: IERR
        INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
!
        INTEGER MY_ID, LINE_BEGIN, LINE_END
        LOGICAL ISOPENED
!
!-----------------------------------------------------------------------
!
        CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'CLOSE_BND_MED:GET_OBJ'
          RETURN
        ENDIF
!
        ISOPENED = .FALSE.
        ! CHECK IF THE FILE IS STILL OPENED AS IT COULD HAVE BEEN CLOSED
        ! BY ANOTHER MESH FILE (SEE OPEN_BND_SRF FOR MORE INFORMATION)
        INQUIRE(UNIT=MY_OBJ_TAB(MY_ID)%NCLI,OPENED=ISOPENED)
        IERR = 0
        IF(ISOPENED) THEN
          !If we manage a concatenated cli file, then we need to write
          !the current offsets
          IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
            LINE_BEGIN = MY_OBJ_TAB(MY_ID)%CLI_LINE_BEGIN
            LINE_END = MY_OBJ_TAB(MY_ID)%NBND_USED_GRP+1+ LINE_BEGIN
            !
            CALL WRITE_INDEX(MY_OBJ_TAB(MY_ID)%CLI_IDX_ID,
     &                       MESH_NUMBER, LINE_BEGIN, LINE_END)
            CLOSE(MY_OBJ_TAB(MY_ID)%CLI_IDX_ID)
          ENDIF
          CLOSE(MY_OBJ_TAB(MY_ID)%NCLI,IOSTAT=IERR)
        ENDIF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'CLOSE_BND_MED:CLOSE'
          RETURN
        ENDIF
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_TITLE_MED (FILE_ID, TITLE, IERR)
!
!BRIEF    READS THE MESH TITLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TITLE          |-->| MESH TITLE OR NAME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,           INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=80), INTENT(OUT) :: TITLE
      INTEGER,           INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
!
      INTEGER :: MY_ID
      CHARACTER(LEN=MED_COMMENT_SIZE) :: TITLE_MED
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     STORE THE NAME OF THE MESH IN THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_TITLE_MED:GET_OBJ'
        RETURN
      ENDIF
      TITLE_MED = REPEAT(' ',MED_COMMENT_SIZE)
      FID = MY_OBJ_TAB(MY_ID)%ID
!     READS THE TITLE OF THE MESH IF ther is one
      CALL MFICOR(FID,TITLE_MED,KIERR)
      IERR = INT(KIERR)
      IF (IERR.LT.0) THEN
        TITLE = 'NO TITLE'
        IERR = 0
      ELSE
        TITLE = TITLE_MED(1:80)
      ENDIF
!
#else
!
      TITLE = REPEAT(' ', 80)
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NELEM_MED (FILE_ID, TYPE_ELEM, NELEM, IERR)
!
!BRIEF    READS TOTAL NUMBER OF ELEMENTS FOR A GIVEN TYPE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(OUT) :: NELEM
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
!     ELEMENT TYPE IN MED NUMBERING
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
!     INDICATORS OF MESH MODIF
      INTEGER(KIND=KMED_INT) :: CHGT,TSF
      INTEGER :: MY_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KNELEM
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NELEM_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NELEM_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
!     READ TOTAL NUMBER OF ELEMENTS
      KNELEM = 0
      CALL MMHNME(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &  TYPE_ELEM_MED,KMED_CONNECTIVITY,KMED_NODAL,CHGT,TSF,KNELEM,
     &  KIERR)
      IERR = INT(KIERR)
      NELEM = INT(KNELEM)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NELEM_MED:MMHNME'
        RETURN
      ENDIF
!
#else
!
      NELEM = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NPOIN_PER_ELEMENT_MED (TYPE_ELEM, NDP, IERR)
!
!BRIEF    READS NUMBER OF NODES PER ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_ELEM      |<--| TYPE OF ELEMENT
!| NDP            |-->| NUMBER OF NODES PER ELEMENT
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(OUT) :: NDP
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
!
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED   ! ELEMENT TYPE IN MED NUMBERING
!
!-----------------------------------------------------------------------
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) RETURN
!
!     DEDUCES NUMBER OF NODES PER ELEMENT FROM THE ELEMENT TYPE
      CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
      IF(IERR.NE.0) RETURN
!
#else
!
      NDP = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_CONNECTIVITY_MED (FILE_ID,TYPE_ELEM,IKLE,
     &                              NELEM,NDP,IERR)
!
!BRIEF    READS THE CONNECTIVITY TABLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF THE ELEMENT IN FORMAT SLF
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |<--| NUMBER OF ELEMENTS
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(IN)  :: NELEM
      INTEGER, INTENT(IN)  :: NDP
      INTEGER, INTENT(INOUT) :: IKLE(NELEM*NDP)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER(KIND=KID) :: FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
      INTEGER I
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_CONNECTIVITY_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_CONNECTIVITY_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
!     READ THE CONNECTIVITY TABLE
#if defined (HAVE_MED64)
      ALLOCATE(TMP(NELEM*NDP))
      CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &  TYPE_ELEM_MED,KMED_NODAL,KMED_FULL_INTERLACE,TMP,KIERR)
      IERR = INT(KIERR)
      DO I=1,NELEM*NDP
        IKLE(I) = INT(TMP(I))
      ENDDO
      DEALLOCATE(TMP)
#else
      CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &  TYPE_ELEM_MED,KMED_NODAL,KMED_FULL_INTERLACE,IKLE,IERR)
#endif
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_CONNECTIVITY_MED:MMHCYR'
        RETURN
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NPOIN_MED (FILE_ID,TYPE_ELEM,NPOIN,IERR)
!
!BRIEF    READS TOTAL NUMBER OF NODES IN A MED FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| ELEMENT TYPE IN SLF FORMAT
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(OUT) :: NPOIN
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER(KIND=KMED_INT) :: CHGT,TSF     ! INDICATORS OF MESH MODIF
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KNPOIN
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPOIN_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPOIN_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
!     READ THE TOTAL NUMBER OF NODES
      CALL MMHNME(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &  TYPE_ELEM_MED,KMED_COORDINATE,KMED_NODAL,CHGT,TSF,KNPOIN,KIERR)
      IERR = INT(KIERR)
      NPOIN=INT(KNPOIN)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPOIN_MED:MMHNME'
        RETURN
      ENDIF
!
#else
!
      NPOIN = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_DIMENSION_MED (FILE_ID, NDIM, IERR)
!
!BRIEF    READS THE NUMBER OF DIMENSION OF THE MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NDIM           |-->| NUMBER OF DIMENSION OF THE MESH
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: NDIM
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE)    :: TITLE_MED
      CHARACTER(LEN=MED_NAME_SIZE)    :: DT_UNIT
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_NAME(:)
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_UNIT(:)
      CHARACTER(LEN=MED_COMMENT_SIZE) :: COMMENT
!
      INTEGER(KIND=KMED_INT) :: IMESH             ! MESH DESCRIPTOR
      ! NUMBER OF PHYSICAL DIMENSIONS
      INTEGER(KIND=KMED_INT) :: NB_DIM_PB
      INTEGER(KIND=KMED_INT) :: NB_DIM_MESH ! NUMBER OF MESH DIMENSIONS
      INTEGER(KIND=KMED_INT) :: MESH_TYPE         ! TYPE OF THE MESH
      INTEGER(KIND=KMED_INT) :: ORDER ! SORTING ORDER FOR IT AND DT
      INTEGER(KIND=KMED_INT) :: NCOMP ! NUMBER OF COMPUTATION STEPS
      INTEGER(KIND=KMED_INT) :: COOR_TYPE         ! TYPE OF COORDINATES
      INTEGER(KIND=KMED_INT) :: NAXIS             ! NUMBER OF AXES
      INTEGER :: MY_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR
!
!----------------------------------------------------------------------
!
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_DIMENSION:GET_MESH_OBJ_FILE'
        RETURN
      ENDIF
      IMESH = MY_OBJ_TAB(MY_ID)%MESH_NUMBER
      FID = MY_OBJ_TAB(MY_ID)%ID
!
      CALL MMHNAX(FID,IMESH,NAXIS,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_DIMENSION_MED:MMHNAX'
        RETURN
      ENDIF
      ALLOCATE(COOR_NAME(INT(NAXIS)),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_MESH_DIMENSION_MED:COOR_NAME'
        RETURN
      ENDIF
      ALLOCATE(COOR_UNIT(INT(NAXIS)),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_MESH_DIMENSION_MED:COOR_UNIT'
        RETURN
      ENDIF
!
!     READ MESH INFORMATIONS
      CALL MMHMII(FID,IMESH,TITLE_MED,NB_DIM_PB,NB_DIM_MESH,MESH_TYPE,
     &  COMMENT,DT_UNIT,ORDER,NCOMP,COOR_TYPE,COOR_NAME,COOR_UNIT,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_DIMENSION_MED:MMHMII'
        RETURN
      ENDIF
      NDIM = INT(NB_DIM_MESH)
      DEALLOCATE(COOR_NAME)
      DEALLOCATE(COOR_UNIT)
!
#else
!
      NDIM = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_COORD_MED(FILE_ID,JDIM,NDIM,NPOIN,COORD_AXE,
     &  IERR)
!
!BRIEF    READS THE COORDINATES OF THE MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| JDIM           |<--| DIMENSION NUMBER (X, Y OR Z)
!| NDIM           |<--| NUMBER OF DIMENSION OF THE MESH
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| COORD_AXE      |-->| COORDINATES OF THE MESH
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID, JDIM, NDIM, NPOIN
      INTEGER, INTENT(OUT) :: IERR
      DOUBLE PRECISION, INTENT(INOUT) :: COORD_AXE(NPOIN)
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: MY_ID
      DOUBLE PRECISION, ALLOCATABLE :: COORD(:)
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR
!
!----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'COORD_MED:GET_MESH_OBJ_FILE'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     TEMPORARY TABLE ALLOCATION
      ALLOCATE(COORD(3*NPOIN),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_MESH_COORD_MED:COORD'
        RETURN
      ENDIF
!
!     READ THE COORDINATES OF THE MESH
      CALL MMHCOR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NO_INTERLACE,
     &  COORD,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_COORD_MED:MMHCOR'
        RETURN
      ENDIF
!
!     STORE ONLY THE DIMENSION DEFINED BY JDIM
      IF ((JDIM.GE.0).AND.(JDIM.LE.NDIM)) THEN
        COORD_AXE(1:NPOIN)=COORD((JDIM-1)*NPOIN+1:JDIM*NPOIN)
      ELSE
!       ERROR ON JDIM
        IERR = HERMES_WRONG_AXE_ERR
      ENDIF
      DEALLOCATE(COORD)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_L2G_NUMBERING_MED(FILE_ID, KNOLG, NPOIN, IERR)
!
!BRIEF    READS THE INDEX TABLE OF NODES WHICH CONVERT THE LOCAL
!         NUMBERING TO THE GLOBAL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| KNOLG          |-->| NODES INDEX TABLE FROM LOCAL TO GLOBAL
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: NPOIN
      INTEGER, INTENT(OUT) :: IERR
      INTEGER, INTENT(INOUT) :: KNOLG(NPOIN)
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: MY_ID
      INTEGER(KIND=KID) :: FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
      INTEGER I
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
!----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_L2G_NUMBERING_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     READ THE NODES INDEX TABLE FROM LOCAL TO GLOBAL
#if defined (HAVE_MED64)
      ALLOCATE(TMP(NPOIN))
      CALL MMHGNR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,KMED_NONE,
     &  TMP,KIERR)
      IERR = INT(KIERR)
      DO I=1,NPOIN
        KNOLG(I) = INT(TMP(I))
      ENDDO
      DEALLOCATE(TMP)
#else
      CALL MMHGNR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,KMED_NONE,
     &  KNOLG,IERR)
#endif
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_MESH_L2G_NUMBERING_MED:MMHGNR'
        RETURN
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NPTIR_MED (FILE_ID, NPTIR, IERR)
!
!BRIEF    READS TOTAL NUMBER OF INTERFACE NODES IN A MED FILE
!         ONLY FOR PARALLEL MODE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NPTIR          |-->| NUMBER OF INTERFACE NODES FOR THE SUB-DOMAIN
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: NPTIR
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      INTEGER :: MY_ID
      CHARACTER(LEN=MED_NAME_SIZE) :: PNAME
      INTEGER(KIND=KMED_INT) :: ITYP,I
      INTEGER(KIND=KMED_INT) :: NPARAM, NSTEP
      CHARACTER(LEN=MED_NAME_SIZE) :: DTUNIT
      CHARACTER(LEN=MED_COMMENT_SIZE) :: DESC
      LOGICAL :: FOUND
      CHARACTER(LEN=MED_NAME_SIZE) :: NPTIR_NAME
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR, TMP
!
!-----------------------------------------------------------------------
!
      IERR = -1
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_MESH_NPTIR_MED:GET_OBJ'
        RETURN
      ENDIF
!
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     INITIALISATION
      NPTIR = 0
      NPTIR_NAME='NPTIR'//TRIM(MY_OBJ_TAB(MY_ID)%MESH_NUMBER_STR)
      ! Looping on all parameter to check if nplan is in
      CALL MPRNPR(FID,NPARAM,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_MESH_NPTIR_MED:MPRNPR'
        RETURN
      ENDIF
      FOUND = .FALSE.
      DO I=1,INT(NPARAM)
        CALL MPRPRI(FID,I,PNAME,ITYP,DESC,DTUNIT,NSTEP,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPTIR_MED:MPRNPR'
          RETURN
        ENDIF
        IF(TRIM(PNAME).EQ.TRIM(NPTIR_NAME)) THEN
          FOUND = .TRUE.
        ENDIF
      ENDDO
      IF(FOUND) THEN
        PNAME = NPTIR_NAME
        CALL MPRIVR(FID,PNAME,KMED_NO_DT,KMED_NO_IT,TMP,KIERR)
        IERR = INT(KIERR)
        NPTIR = INT(TMP)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPTIR_MED:MPRIVR'
          RETURN
        ENDIF
      ELSE
        NPTIR = 0
      ENDIF
!
#else
!
      NPTIR = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_ORIG_MED (FILE_ID, X_ORIG, Y_ORIG, IERR)
!
!BRIEF    READS ORIGIN OF COORDINATES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| X_ORIG         |<->| Off set of the X coordinates
!| Y_ORIG         |<->| Off set of the Y coordinates
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: X_ORIG, Y_ORIG
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      INTEGER :: MY_ID
      CHARACTER(LEN=MED_NAME_SIZE) :: PNAME,DTUNIT
      CHARACTER(LEN=MED_COMMENT_SIZE) :: DESC
      LOGICAL :: FOUND
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: NPARAM, NSTEP, TMP
      INTEGER(KIND=KMED_INT) :: KIERR,ITYP,I
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_MESH_ORIG_MED:GET_OBJ'
        RETURN
      ENDIF
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     INITIALISATION
      ! Looping on all parameter to check if nplan is in
      CALL MPRNPR(FID,NPARAM,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_MESH_ORIG_MED:MPRNPR'
        RETURN
      ENDIF
      FOUND = .FALSE.
      DO I=1,INT(NPARAM)
        CALL MPRPRI(FID,I,PNAME,ITYP,DESC,DTUNIT,NSTEP,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_ORIG_MED:MPRNPR'
          RETURN
        ENDIF
        IF(PNAME(1:5).EQ.'X_ORIG') THEN
          FOUND = .TRUE.
        ENDIF
      ENDDO
      IF(FOUND) THEN
        PNAME = 'X_ORIG'//CHAR(0)
        CALL MPRIVR(FID,PNAME,KMED_NO_DT,KMED_NO_IT,TMP,KIERR)
        IERR = INT(KIERR)
        X_ORIG = INT(TMP)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_ORIG_MED:MPRIVR'
          RETURN
        ENDIF
        PNAME = 'Y_ORIG'//CHAR(0)
        CALL MPRIVR(FID,PNAME,KMED_NO_DT,KMED_NO_IT,TMP,KIERR)
        IERR = INT(KIERR)
        Y_ORIG = INT(TMP)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_ORIG_MED:MPRIVR'
          RETURN
        ENDIF
      ELSE
        X_ORIG = 0
        Y_ORIG = 0
      ENDIF
!
#else
!
      X_ORIG = 0
      Y_ORIG = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_NPLAN_MED (FILE_ID, NPLAN, IERR)
!
!BRIEF    READS NUMBER OF PLANES (3D)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NPLAN          |-->| NUMBER OF PLANES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: NPLAN
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      INTEGER :: MY_ID
      CHARACTER(LEN=MED_NAME_SIZE) :: PNAME,DTUNIT
      CHARACTER(LEN=MED_COMMENT_SIZE) :: DESC
      LOGICAL :: FOUND
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: NPARAM, NSTEP, TMP
      INTEGER(KIND=KMED_INT) :: KIERR,ITYP,I
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_MESH_NPLAN_MED:GET_OBJ'
        RETURN
      ENDIF
!
      NPLAN = 0
      FID = MY_OBJ_TAB(MY_ID)%ID
!     INITIALISATION
      ! Looping on all parameter to check if nplan is in
      CALL MPRNPR(FID,NPARAM,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_MESH_NPLAN_MED:MPRNPR'
        RETURN
      ENDIF
      FOUND = .FALSE.
      DO I=1,INT(NPARAM)
        CALL MPRPRI(FID,I,PNAME,ITYP,DESC,DTUNIT,NSTEP,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPLAN_MED:MPRNPR'
          RETURN
        ENDIF
        IF(PNAME(1:5).EQ.'NPLAN') THEN
          FOUND = .TRUE.
        ENDIF
      ENDDO
      IF(FOUND) THEN
        PNAME = 'NPLAN'//CHAR(0)
        CALL MPRIVR(FID,PNAME,KMED_NO_DT,KMED_NO_IT,TMP,KIERR)
        IERR = INT(KIERR)
        NPLAN = INT(TMP)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPLAN_MED:MPRIVR'
          RETURN
        ENDIF
      ELSE
        NPLAN = 0
      ENDIF
!
#else
!
      NPLAN = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_MESH_DATE_MED (FILE_ID, DATE, IERR)
!
!BRIEF    READS NUMBER OF PLANES (3D)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NPLAN          |-->| NUMBER OF PLANES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: DATE(6)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      INTEGER :: MY_ID
      CHARACTER(LEN=MED_NAME_SIZE) :: PNAME,DTUNIT
      CHARACTER(LEN=MED_COMMENT_SIZE) :: DESC
      LOGICAL :: FOUND
      INTEGER(KIND=KMED_INT) :: MYDATE
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: NPARAM, NSTEP
      INTEGER(KIND=KMED_INT) :: KIERR,ITYP,I
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_MESH_NPLAN_MED:GET_OBJ'
        RETURN
      ENDIF
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     INITIALISATION
      ! Looping on all parameter to check if nplan is in
      CALL MPRNPR(FID,NPARAM,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_MESH_NPLAN_MED:MPRNPR'
        RETURN
      ENDIF
      FOUND = .FALSE.
      DO I=1,INT(NPARAM)
        CALL MPRPRI(FID,I,PNAME,ITYP,DESC,DTUNIT,NSTEP,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPLAN_MED:MPRNPR'
          RETURN
        ENDIF
        IF(PNAME(1:5).EQ.'DATE') THEN
          FOUND = .TRUE.
        ENDIF
      ENDDO
      IF(FOUND) THEN
        PNAME = 'DATE'//CHAR(0)
        CALL MPRIVR(FID,PNAME,KMED_NO_DT,KMED_NO_IT,MYDATE,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPLAN_MED:MPRIVR'
          RETURN
        ENDIF
        DATE(1) = INT(MYDATE)/10000
        MYDATE = MYDATE - DATE(1)*10000
        DATE(2) = INT(MYDATE)/100
        MYDATE = MYDATE - DATE(2)*100
        DATE(3) = INT(MYDATE)
        PNAME = 'TIME'//CHAR(0)
        CALL MPRIVR(FID,PNAME,KMED_NO_DT,KMED_NO_IT,MYDATE,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_MESH_NPLAN_MED:MPRIVR'
          RETURN
        ENDIF
        DATE(4) = INT(MYDATE)/10000
        MYDATE = MYDATE - DATE(4)*10000
        DATE(5) = INT(MYDATE)/100
        MYDATE = MYDATE - DATE(5)*100
        DATE(6) = INT(MYDATE)
      ELSE
        DATE(:) = 0
      ENDIF
!
#else
!
      DATE(:) = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_IPOBO_MED(FILE_ID,TYPE_ELEM_BND,NPOIN,
     &  IPOBO,IERR)
!
!BRIEF    DETERMINES THE TABLE IPOBO: 1 IF BOUNDARY NODE, 0 OTHERWISE
!
!WARNING  ONLY WORKS FOR ONE TYPE OF BOUNDARY ELEMENTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM_BND  |<--| BOUNDARY ELEMENT TYPE IN SLF FORMAT
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| IPOBO          |-->| LOGICAL TABLE FOR BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, TYPE_ELEM_BND, NPOIN
      INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
!
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: I,J, MY_ID
      INTEGER :: NDP_BND_ELE  ! NUMBER OF NODES PER ELEMENT
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER, ALLOCATABLE :: BND_IKLE(:)
      INTEGER, ALLOCATABLE :: NBOR(:)
      INTEGER :: NELEM
      INTEGER :: NPTFR
      INTEGER(KIND=KID) :: FID

#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     INITIALISATION
      DO I = 1, NPOIN
        IPOBO(I) = 0
      ENDDO
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_IPOBO_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      IF(MY_OBJ_TAB(MY_ID)%NO_BND) THEN
        IERR = 0
        RETURN
      ENDIF
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM_BND, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_IPOBO_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM_BND,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_IPOBO_MED:IDENTIFY_BND_ELMT'
        RETURN
      ENDIF
!
!     DETERMINE THE NUMBER OF NODES PER ELEMENT FROM THE TYPE
      CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP_BND_ELE,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_IPOBO_MED:NDP_FROM_ELT_TYPE_MED'
        RETURN
      ENDIF
!
!     Get the number of element of the boundary type
      CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_ELEM_BND,NELEM,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_IPOBO_MED:GET_MESH_NELEM_MED'
        RETURN
      ENDIF
!
!
      IF(TYPE_ELEM_BND.EQ.POINT_BND_ELT_TYPE) THEN
        CALL GET_BND_NPOIN_MED(FILE_ID,TYPE_ELEM_BND,NPTFR,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_IPOBO_MED:GET_BND_NPOIN'
          RETURN
        ENDIF
        ALLOCATE(NBOR(NPTFR),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING NBOR'
          RETURN
        ENDIF

        CALL GET_BND_NUMBERING_MED(FILE_ID,TYPE_ELEM_BND,NPTFR,
     &                             NBOR,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_IPOBO_MED:GET_BND_NPOIN'
          RETURN
        ENDIF
        DO I = 1, NPTFR
          IPOBO(NBOR(I)) = 1
        ENDDO
        DEALLOCATE(NBOR)
      ELSE
!       READ THE ONNECTIVITY TABLE OF THE BOUNDARY ELEMENTS
        ALLOCATE(BND_IKLE(NDP_BND_ELE*NELEM),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_IPOBO_MED:BND_IKLE'
          RETURN
        ENDIF
!
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NDP_BND_ELE*NELEM))
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NELEM*NDP_BND_ELE
          BND_IKLE(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,BND_IKLE,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_IPOBO_MED:MMHCYR'
          RETURN
        ENDIF
!       LOOP ON THE BOUNDARY ELEMENTS TO MODIFY IPOBO
        DO I = 1, NELEM
          ! Skipping non boundary elements
          IF(.NOT.MY_OBJ_TAB(MY_ID)%IS_BND(I)) CYCLE
          DO J=1,NDP_BND_ELE
            IPOBO(BND_IKLE((J-1)*NELEM+I)) = 1
          ENDDO
        ENDDO
!
!       FREE MEMORY
        DEALLOCATE(BND_IKLE)
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_NUMBERING_MED(FILE_ID,TYPE_ELEM_BND,NPTFR,
     & NBOR,IERR)
!
!BRIEF    GET THE TABLE OF THE BOUNDARY NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM_BND  |<--| BOUNDARY ELEMENT TYPE IN SLF FORMAT
!| NPTFR          |<--| TOTAL NUMBER OF BOUNDARY NODES
!| NBOR           |-->| LOGICAL TABLE FOR BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID,NPTFR,TYPE_ELEM_BND
      INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined HAVE_MED
!
      INTEGER :: NELEBD,NDP,I,J
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER, ALLOCATABLE :: IKLE(:)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER :: ISUIV, NOEUD1, NOEUD2
      INTEGER :: IILE,NILE,NPOIN,NELEM
      INTEGER, ALLOCATABLE :: TRAV1(:,:)
      INTEGER, ALLOCATABLE :: KP1BOR(:)
      DOUBLE PRECISION :: SOM1,SOM2,Y2
      DOUBLE PRECISION, PARAMETER :: EPSILO = 1.D-6
      DOUBLE PRECISION, ALLOCATABLE :: X(:),Y(:)
      LOGICAL :: SWAP
      INTEGER(KIND=KID) :: FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_NUMBERING_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
      IF(MY_OBJ_TAB(MY_ID)%NO_BND) THEN
        IERR = 0
        RETURN
      ENDIF

      IF(TYPE_ELEM_BND.EQ.POINT_BND_ELT_TYPE) THEN
!
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NPTFR))
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    KMED_POINT1,KMED_NODAL,KMED_NO_INTERLACE,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NPTFR
          NBOR(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    KMED_POINT1,KMED_NODAL,KMED_NO_INTERLACE,NBOR,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_CONNECTIVITY_MED:MMHCYR'
          RETURN
        ENDIF
!
      ELSE IF(ALLOCATED(MY_OBJ_TAB(MY_ID)%NBOR)) THEN
!
        DO I=1,NPTFR
          NBOR(I) = MY_OBJ_TAB(MY_ID)%NBOR(I)
        ENDDO
!
      ELSE
!
!       OTHERWISE COMPUTING NBOR
!       CONVERTS TYPE OF ELEMENTS
        CALL CONVERT_ELEM_TYPE(TYPE_ELEM_BND, TYPE_ELEM_MED, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_MED:CONVERT_ELEM_TYPE'
          RETURN
        ENDIF
!
!       Identify bnd element if necessary
        CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM_BND,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_IPOBO_MED:IDENTIFY_BND_ELMT'
          RETURN
        ENDIF
!
!       DETERMINE THE NUMBER OF NODES PER ELEMENT FROM THE TYPE
        CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_MED:NDP_FROM_ELT_TYPE_MED'
          RETURN
        ENDIF
!
!       Get the number of element of the boundary type
        CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_ELEM_BND,NELEM,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_MED:GET_MESH_NELEM_MED'
          RETURN
        ENDIF
!
!       READ THE CONNECTIVITY TABLE OF THE ELEMENTS
        ALLOCATE(IKLE(NDP*NELEM),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_NUMBERING_MED:BND_IKLE'
          RETURN
        ENDIF
!
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NDP*NELEM))
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NDP*NELEM
          IKLE(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,IKLE,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_MED:MMHCYR'
          RETURN
        ENDIF
!
!       REORDER THE BOUNDARY POINT TO COMPLY WITH TELEMAC CONVENTION
!       (SEE DOCUMENTATION)
        CALL GET_MESH_NPOIN_MED(FILE_ID,TYPE_ELEM_BND,NPOIN,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_MED:GET_MESH_COORD_MED:X'
          RETURN
        ENDIF
!
        ALLOCATE(TRAV1(NPTFR,2),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_NUMBERING_MED:TRAV1'
          RETURN
        ENDIF

        ALLOCATE(KP1BOR(NPTFR*2),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_NUMBERING_MED:KP1BOR'
          RETURN
        ENDIF

        ALLOCATE(MY_OBJ_TAB(MY_ID)%PT2SEG(NPTFR,2),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_NUMBERING_MED:PT2SEG'
          RETURN
        ENDIF
!
!       Only take into account the real boundary elements
        NELEBD = 0
        DO I=1,NELEM
          ! Skipping non boundary elements
          IF(.NOT.MY_OBJ_TAB(MY_ID)%IS_BND(I)) CYCLE
          NELEBD = NELEBD + 1
          IF (NELEBD.GT.NPTFR) THEN
            IERR = 1
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ISSUE WITH BOUNDARY CONDITION: '//NEW_LINE('(A)')//
     &         'MISSING A GROUP '//NEW_LINE('(A)')//
     &         'OR ELEMENTS ARE IN MULTIPLE GROUPS '//NEW_LINE('(A)')//
     &         'OR NON BOUNDARY ELEMENTS IN BND FILE'
            RETURN
          ENDIF

          TRAV1(NELEBD,1) = IKLE(I)
          TRAV1(NELEBD,2) = IKLE(I+NELEM)
        ENDDO
        ! Initialising boundary element renumbering
        ALLOCATE(MY_OBJ_TAB(MY_ID)%NBOR_SEG(NELEBD))
        J = 0
        DO I=1,NELEM
          ! Skipping non boundary elements
          IF(.NOT.MY_OBJ_TAB(MY_ID)%IS_BND(I)) CYCLE
          J = J + 1
          MY_OBJ_TAB(MY_ID)%NBOR_SEG(J) = J
        ENDDO
!
!       GET MESH COORDINATES
        ALLOCATE(X(NPOIN),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_NUMBERING_MED:X'
          RETURN
        ENDIF
        ALLOCATE(Y(NPOIN),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_NUMBERING_MED:Y'
          RETURN
        ENDIF
        CALL GET_MESH_COORD_MED(FILE_ID,1,2,NPOIN,X,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_MED:GET_MESH_COORD_MED:X'
          RETURN
        ENDIF
        CALL GET_MESH_COORD_MED(FILE_ID,2,2,NPOIN,Y,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_MED:GET_MESH_COORD_MED:Y'
          RETURN
        ENDIF
!
!       EDGE ARE STORED CONTINUOUSLY
!       IT STARTS WITH SOUTH-WESTERNMOST POINT (OR SOUTHMORE POINT IN
!       CASE OF CONFLICT) IN ORDER TO START ON THE BOUNDARY AND NOT ON A
!       ISLAND
        SOM2 = X(1) + Y(1)
        Y2   = Y(1)
!
        DO I=1,NPTFR
!
          SOM1 = X(TRAV1(I,1)) + Y(TRAV1(I,1))
          IF (ABS(SOM1-SOM2).LE.ABS(EPSILO*SOM1)) THEN
            IF (Y(TRAV1(I,1)).LE.Y2) THEN
              Y2    = Y(TRAV1(I,1))
              SOM2  = SOM1
              ISUIV = I
            ENDIF
          ELSEIF (SOM1.LE.SOM2) THEN
            Y2    = Y(TRAV1(I,1))
            SOM2  = SOM1
            ISUIV = I
          ENDIF
!
        ENDDO
!
        NOEUD1 = TRAV1(ISUIV,1)
        NOEUD2 = TRAV1(ISUIV,2)
        TRAV1(ISUIV,1) = TRAV1(1,1)
        TRAV1(ISUIV,2) = TRAV1(1,2)
        TRAV1(1,1) = NOEUD1
        TRAV1(1,2) = NOEUD2
        NOEUD1 = MY_OBJ_TAB(MY_ID)%NBOR_SEG(ISUIV)
        MY_OBJ_TAB(MY_ID)%NBOR_SEG(ISUIV) =
     &    MY_OBJ_TAB(MY_ID)%NBOR_SEG(1)
        MY_OBJ_TAB(MY_ID)%NBOR_SEG(1) = NOEUD1
!
        IILE = 0
        NILE = 1
!
        DO I=2,NPTFR
          SWAP = .FALSE.
          MY_OBJ_TAB(MY_ID)%PT2SEG(I,1) =
     &      MY_OBJ_TAB(MY_ID)%NBOR_SEG(I-1)
!
!         SEACH OF THE EDGE WHOSE FIRST NODE IS IDENTICAL TO THE SECOND
!         ONE
!         OF THE PREVIOUS EDGE
          DO ISUIV=I,NPTFR
!
            IF (TRAV1(ISUIV,1).EQ.TRAV1(I-1,2)) THEN
!
!             PERMUTATION OF EDGE WITH NUMBER I+1 AND ISUIV
              NOEUD1 = TRAV1(ISUIV,1)
              NOEUD2 = TRAV1(ISUIV,2)
              TRAV1(ISUIV,1) = TRAV1(I,1)
              TRAV1(ISUIV,2) = TRAV1(I,2)
              TRAV1(I,1) = NOEUD1
              TRAV1(I,2) = NOEUD2
              NOEUD1 = MY_OBJ_TAB(MY_ID)%NBOR_SEG(ISUIV)
              MY_OBJ_TAB(MY_ID)%NBOR_SEG(ISUIV) =
     &          MY_OBJ_TAB(MY_ID)%NBOR_SEG(I)
              MY_OBJ_TAB(MY_ID)%NBOR_SEG(I) = NOEUD1
              KP1BOR(I+NPTFR) = I-1
              KP1BOR(I-1) = I
              SWAP = .TRUE.
              EXIT
!
            ENDIF
!
          ENDDO! ISUIV
          IF(.NOT.SWAP) THEN
            DO ISUIV=I,NPTFR
!
              IF (TRAV1(ISUIV,2).EQ.TRAV1(I-1,2)) THEN
!
!               PERMUTATION OF EDGE WITHIN ITSELF
                NOEUD1 = TRAV1(ISUIV,2)
                NOEUD2 = TRAV1(ISUIV,1)
                TRAV1(ISUIV,1) = TRAV1(I,1)
                TRAV1(ISUIV,2) = TRAV1(I,2)
                TRAV1(I,1) = NOEUD1
                TRAV1(I,2) = NOEUD2
                NOEUD1 = MY_OBJ_TAB(MY_ID)%NBOR_SEG(ISUIV)
                MY_OBJ_TAB(MY_ID)%NBOR_SEG(ISUIV) =
     &            MY_OBJ_TAB(MY_ID)%NBOR_SEG(I)
                MY_OBJ_TAB(MY_ID)%NBOR_SEG(I) = NOEUD1
                KP1BOR(I+NPTFR) = I-1
                KP1BOR(I-1) = I
                SWAP = .TRUE.
                EXIT
!
              ENDIF
!
            ENDDO! ISUIV
          ENDIF
          MY_OBJ_TAB(MY_ID)%PT2SEG(I,2) =
     &          MY_OBJ_TAB(MY_ID)%NBOR_SEG(I)
          IF(SWAP) CYCLE
!
!         IF NO FOLLOWING POINT IS FOUND, WE VERIFY THAT THE LAST AND
!         THE FIRST POINTS ARE IDENTICAL. IT MEANS WE FOUND AN ISLAND,
!         SO WE RETURN TO THE GLOBAL LOOP
          IF (TRAV1(NILE,1).NE.TRAV1(I-1,2)) THEN
!
!           OTHERWISE IT IS AN ERROR
            WRITE(ERROR_MESSAGE,4500) TRAV1(I-1,2)
4500        FORMAT('ERROR IN THE EDGE SEGMENTS MISSING SEGMENT',
     &             ' FOR THE NODE ',I0)
            IERR = -1
            RETURN
          ENDIF
!
          KP1BOR(NILE+NPTFR) = I-1
          KP1BOR(I-1) = NILE
          MY_OBJ_TAB(MY_ID)%PT2SEG(NILE,1) =
     &      MY_OBJ_TAB(MY_ID)%NBOR_SEG(I-1)
          MY_OBJ_TAB(MY_ID)%PT2SEG(NILE,2) =
     &      MY_OBJ_TAB(MY_ID)%NBOR_SEG(NILE)
          IILE = IILE+1
          NILE = I
!
        ENDDO! I
!
!       WE VERIFY THAT THE LAST ISLAND IS CLOSED
        IF (TRAV1(NILE,1).NE.TRAV1(NPTFR,2)) THEN
          WRITE(ERROR_MESSAGE,5000) TRAV1(NILE,1),TRAV1(NPTFR,2)
5000      FORMAT('ERROR, THE BOUNDARY IS NOT CLOSED :',
     &           'FIRST POINT :',I0,'LAST POINT : ',I0)
          IERR = -1
          RETURN
        ENDIF
!
        KP1BOR(NILE+NPTFR) = NPTFR
        KP1BOR(NPTFR) = NILE
        MY_OBJ_TAB(MY_ID)%PT2SEG(NILE,1) =
     &    MY_OBJ_TAB(MY_ID)%NBOR_SEG(NPTFR)
        MY_OBJ_TAB(MY_ID)%PT2SEG(NILE,2) =
     &    MY_OBJ_TAB(MY_ID)%NBOR_SEG(NILE)
!
!       THE TABLE NBOR IS FILLED AND THE COLOR OF THE BOUNDARY NODES IS
!       STORED IN THE TABLE NCOLFR
        ALLOCATE(MY_OBJ_TAB(MY_ID)%NBOR(NPTFR))
        DO I=1,NPTFR
          NBOR(I      ) = TRAV1(I,1)
          MY_OBJ_TAB(MY_ID)%NBOR(I) = NBOR(I)
        ENDDO

        DEALLOCATE(IKLE)
        DEALLOCATE(X,Y)
        DEALLOCATE(TRAV1)
        DEALLOCATE(KP1BOR)
      ENDIF
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_NELEM_MED (FILE_ID, TYPE_ELEM, BND_NELEM, IERR)
!
!BRIEF    READS THE TOTAL NUMBER OF BOUNDARY ELEMENTS FOR A GIVEN TYPE
!         OF BOUNDARY ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF ELEMENT IN SLF FORMAT
!| BND_NELEM      |-->| NUMBER OF BOUNDARY ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(OUT) :: BND_NELEM
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_NELEM_MED:GET_OBJ'
        RETURN
      ENDIF
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_NELEM_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_NELEM_MED:IDENTIFY_BND_ELMT'
        RETURN
      ENDIF
!
      IF(MY_OBJ_TAB(MY_ID)%NO_BND) THEN
        IERR = 0
        BND_NELEM = 0
        RETURN
      ENDIF
!
!     Compute the total number of element
      BND_NELEM = COUNT(MY_OBJ_TAB(MY_ID)%IS_BND)
!
#else
!
      BND_NELEM = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_CONNECTIVITY_MED (FILE_ID, TYPE_ELEM, NELEBD,
     &                              NDP, BND_IKLE, IERR)
!
!BRIEF    READS THE CONNECTIVITY TABLE OF BOUNDARY ELEMENTS FOR A GIVEN
!         TYPE OF BOUNDARY ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_ELEM      |<--| TYPE OF THE ELEMENT IN MED FORMAT
!| NELEBD         |<--| TOTAL NUMBER OF ELEMENTS
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| BND_IKLE       |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: NELEBD
      INTEGER, INTENT(IN)  :: NDP
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER, INTENT(INOUT) :: BND_IKLE(NELEBD*NDP)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER :: MY_ID
      INTEGER,ALLOCATABLE :: IKLE(:)
      INTEGER :: IBND,NELEM,I
      INTEGER(KIND=KID) ::FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
      INTEGER J
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_CONNECTIVITY_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME

!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     CONVERT
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_CONNECTIVITY_MED:CONVERT'
        RETURN
      ENDIF
!
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_ELEM,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_CONNECTIVITY_MED:IDENTIFY_BND_ELMT'
        RETURN
      ENDIF

      IF(MY_OBJ_TAB(MY_ID)%NO_BND) THEN
        IERR = 0
        RETURN
      ENDIF
!
      CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_ELEM,NELEM,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_CONNECTIVITY_MED:GET_MESH_NELEM'
        RETURN
      ENDIF
      IF(NELEM.GT.0) THEN
        ALLOCATE(IKLE(NELEM*NDP),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_CONNECTIVITY_MED:IKLE'
          RETURN
        ENDIF
!       Read the connectivity table of the elements
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEM*NDP))
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,TMP,KIERR)
        IERR = INT(KIERR)
        DO J=1,NELEM*NDP
          IKLE(J) = INT(TMP(J))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHCYR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,IKLE,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_CONNECTIVITY_MED:MMHCYR'
          RETURN
        ENDIF
!
        ! Compute the boundary elements connectivity table
        IBND = 0
        DO I=1,NELEM
          ! Skipping no boundary elements
          IF(.NOT.MY_OBJ_TAB(MY_ID)%IS_BND(I)) CYCLE
          IBND = IBND + 1
          BND_IKLE(IBND) = IKLE(I)
          BND_IKLE(IBND+NELEBD) = IKLE(I+NELEM)
        ENDDO
        DEALLOCATE(IKLE)
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_GRP_VALUE_MED (FILE_ID, GRP_NAME, VALUE, IERR)
!
!BRIEF    GIVES THE VALUE OF THE BOUNDARY CONDITIONS ASSOCIATED TO A
!         FAMILY NAME
!
!WARNING  THIS FUNCTION COULD BE OPTIMIZED BY READING ALL THE INFO AT
!         THE BEGINNING. HOWEVER THE COST IT LOW AS THE SIZE OF THE FILE
!         IS LIMITED (I.E. TOTAL NUMBER OF GROUP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| GRP_NAME       |<--| NAME OF THE FAMILY
!| VALUE          |-->| VALUE OF THE BOUNDARY CONDITIONS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID
      CHARACTER(LEN=16), INTENT(IN)  :: GRP_NAME
      INTEGER, INTENT(INOUT) :: VALUE
      INTEGER, INTENT(OUT) :: IERR
!
      INTEGER :: NCLI,MY_ID,I1,I2,I3,I4,I
      INTEGER :: NGROUP
      CHARACTER(LEN=16) :: TEMP_NAME
      LOGICAL :: FOUND
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_GRP_VALUE_MED:GET_OBJ'
        RETURN
      ENDIF
      NCLI = MY_OBJ_TAB(MY_ID)%NCLI
      IF(MY_OBJ_TAB(MY_ID)%NO_BND) THEN
        IERR = 0
        RETURN
      ENDIF
!
!     INITIALIZATION
      FOUND = .FALSE.
      IERR = 0
      REWIND(NCLI)
!
!     LOOP ON THE INFO IN THE FILE
      READ(UNIT=NCLI,FMT=*,IOSTAT=IERR) NGROUP
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_GRP_VALUE:READ'
        RETURN
      ENDIF
      DO I=1,NGROUP
        READ(UNIT=NCLI,FMT=*,IOSTAT=IERR) I1,I2,I3,I4,TEMP_NAME
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_GRP_VALUE:READ'
          RETURN
        ENDIF
!
!       IF THE FAMILY IS FOUND, THE VALUE IS STORED
        IF (TEMP_NAME .EQ. GRP_NAME) THEN
          FOUND = .TRUE.
          VALUE = I1*1000+I2*100+I3*10+I4
          EXIT
        ENDIF
      ENDDO
!
      IF(.NOT.FOUND) IERR = HERMES_UNKNOWN_GROUP_ERR
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_FAMILY_MED (FILE_ID, TYPE_BND_ELEM, NELEBD,
     &  FAMILY, IERR)
!
!BRIEF    CREATE THE FAMILY NAME FOR EACH BOUNDARY ELEMENT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELEM  |<--| TYPE OF THE BOUNDARY ELEMENT
!| NELEBD         |<--| TOTAL NUMBER OF BOUNDARY ELEMENTS
!| FAMILY         |-->| TABLE OF BOUNDARY ELEMENT'S FAMILY NAME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID, TYPE_BND_ELEM, NELEBD
      INTEGER, DIMENSION(NELEBD), INTENT(INOUT) :: FAMILY
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER ::  IELEM, IGRP
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER(KIND=KMED_INT) :: NB_FAMILY
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUM_FAMILY
      INTEGER :: NELEM, IBND
      INTEGER(KIND=KID) :: FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
      INTEGER I
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_FAMILY_MED:GET_OBJ_FILE'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_FAMILY_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
!     Identify bnd element if necessary
      CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_BND_ELEM,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_FAMILY_MED:IDENTIFY_BND_ELMT'
        RETURN
      ENDIF
!
      IF(TYPE_BND_ELEM.EQ.POINT_BND_ELT_TYPE) THEN
!       Get the number of elements
        CALL GET_BND_NELEM_MED(FILE_ID,TYPE_BND_ELEM,NELEM,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_FAMILY_MED:GET_MESH_NELEM'
          RETURN
        ENDIF
      ELSE
!       Get the number of elements
        CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_BND_ELEM,NELEM,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_FAMILY_MED:GET_MESH_NELEM'
          RETURN
        ENDIF
      ENDIF
!
!     READ THE FAMILY NUMBER FOR EACH ELEMENT
      ALLOCATE(NUM_FAMILY(NELEM),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'ALLOCATING NUM_FAMILY'
        RETURN
      ENDIF

#if defined (HAVE_MED64)
      ALLOCATE(TMP(NELEM))
      CALL MMHFNR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &            TYPE_ELEM_MED,TMP,KIERR)
      IERR = INT(KIERR)
      DO I=1,NELEM
        NUM_FAMILY(I) = INT(TMP(I))
      ENDDO
      DEALLOCATE(TMP)
#else
      CALL MMHFNR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &            TYPE_ELEM_MED,NUM_FAMILY,IERR)
#endif
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_FAMILY_MED:MMHFNR'
        RETURN
      ENDIF
!
!     READ THE TOTAL NUMBER OF FAMILY
      CALL MFANFA(FID,MNAME,NB_FAMILY,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_FAMILY_MED:MFANFA'
        RETURN
      ENDIF
!
      IBND = 0
      DO IELEM = 1, NELEM
!
        IF(.NOT.MY_OBJ_TAB(MY_ID)%IS_BND(IELEM)) CYCLE
!       LOOP ON FAMILY
        IBND = IBND + 1
        DO IGRP = 1, INT(NB_FAMILY)
          ! Skipping non boundary families
          IF(MY_OBJ_TAB(MY_ID)%BND_FAM(IGRP,2).EQ.0) CYCLE
          IF (NUM_FAMILY(IELEM).EQ.
     &        MY_OBJ_TAB(MY_ID)%BND_FAM(IGRP,1)) THEN
            FAMILY(IBND) = MY_OBJ_TAB(MY_ID)%BND_FAM(IGRP,2)
          ENDIF
        ENDDO
      ENDDO
!
!     FREE MEMORY
      DEALLOCATE(NUM_FAMILY)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_NPOIN_MED(FILE_ID,TYPE_BND_ELEM,NPTFR,IERR)
!
!BRIEF    DETERMINE THE NUMBER OF BOUNDARY POINTS
!
!WARNING  FIND ANOTHER SOLUTION THIS ONE IS WAY TOO EXPENSIVE
!         SEE WITH MED PEOPLE IF THER IS ANOTHER WAY
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELEM  |<--| TYPE OF THE BOUNDARY ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: FILE_ID, TYPE_BND_ELEM
        INTEGER, INTENT(OUT) :: NPTFR
        INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
!
      INTEGER(KIND=KMED_INT) :: CHGT,TSF     ! INDICATORS OF MESH MODIF
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: TYPE_BND_ELEM_MED
      INTEGER :: NELEBD, NPOIN, I, J, NDP
      INTEGER, ALLOCATABLE :: IKLE(:)
      LOGICAL, ALLOCATABLE :: IS_BND(:)
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: TMP
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_NPOIN_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_BND_ELEM_MED, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_NPOIN_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
      NELEBD = 0
!     GET THE NUMBER OF BOUNDARY ELEMENTS
      CALL GET_BND_NELEM_MED(FILE_ID,TYPE_BND_ELEM,NELEBD,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_BND_NPOIN_MED:GET_BND_NELEM_MED'
        RETURN
      ENDIF
      IF(TYPE_BND_ELEM.EQ.POINT_BND_ELT_TYPE) THEN
        NPTFR = NELEBD
        RETURN
      ENDIF
!
!     CONTINUE ONLY IF BOUNDARY ELEMENTS EXIST
      IF(NELEBD.GT.0) THEN
!
!       GET THE NUMBER OF POINT IN THE MESH
        CALL MMHNME(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &            TYPE_BND_ELEM_MED,KMED_COORDINATE,KMED_NODAL,
     &            CHGT,TSF,TMP,KIERR)
        IERR = INT(KIERR)
        NPOIN = INT(TMP)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NPOIN_MED:MMHNME'
          RETURN
        ENDIF
!
!       GET THE NUMBER OF POINTS PER ELEMENT
        CALL GET_NODES_PER_ELEMENT(TYPE_BND_ELEM,NDP)
!
!       ALLOCATE CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
        ALLOCATE(IKLE(NELEBD*NDP),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCAING GET_BND_NPOIN_MED:IKLE'
          RETURN
        ENDIF
!
!       GET THE CONNECTIVITY TABLE FOR THE BOUNDARY ELEMENTS
        CALL GET_BND_CONNECTIVITY_MED(FILE_ID, TYPE_BND_ELEM, NELEBD,
     &                        NDP, IKLE, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_NPOIN_MED:GET_BND_CONNECTIVITY_MED'
          RETURN
        ENDIF
!
        ALLOCATE(IS_BND(NPOIN),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING GET_BND_NPOIN_MED:IKLE'
          RETURN
        ENDIF
!
!       INITIALIZE
        DO I=1,NPOIN
          IS_BND(I)  = .FALSE.
        ENDDO
!
!       Loop on all the points of the boundary elements set their value
!       to true
        DO I=1,NELEBD
          DO J=1,NDP
            IS_BND(IKLE(I+(J-1)*NELEBD)) = .TRUE.
          ENDDO
        ENDDO
!
!       COUNT THE NUMBER OF TRUE THIS WILL GIVE US THE NUMBER OF
!       BOUNDARY POINTS
        NPTFR = COUNT(IS_BND.EQV..TRUE.)
        DEALLOCATE(IKLE)
        DEALLOCATE(IS_BND)
!
      ELSE
        NPTFR = 0
      ENDIF
!
#else
!
      NPTFR = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_BND_VALUE_MED(FILE_ID,TYPE_BND_ELEM,NELEBD,
     &                             LIHBOR,LIUBOR,LIVBOR,TRAC,LITBOR,
     &                             NPTFR,IERR)
!
!BRIEF    DETERMINE THE NUMBER OF BOUNDARY POINTS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELEM  |<--| TYPE OF THE BOUNDARY ELEMENT
!| NELEBD         |<--| NUMBER OF BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| NPTFR          |<--| NUMBER OF BOUNDARY NODES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(IN) :: NELEBD
        INTEGER, INTENT(IN) :: NPTFR
        INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
        INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(*)
        LOGICAL, INTENT(IN)    :: TRAC
        INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
        CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
        INTEGER, ALLOCATABLE :: FAMILY(:)
        INTEGER, ALLOCATABLE :: IKLE(:)
        INTEGER, ALLOCATABLE :: NBOR(:)
        INTEGER :: NDP,IGRP,MY_ID
        INTEGER :: INODE,IPTFR
        INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
        INTEGER :: IGRP_CUR, IGRP_NXT
        INTEGER :: BND_TYP_CUR, BND_TYP_NXT
        INTEGER :: CURRENT, NEXT
        INTEGER :: NPTIR
        INTEGER :: IELEB
!
!-----------------------------------------------------------------------
!
!       GET INFO FROM THE MED FILE OBJECT
        CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_VALUE_MED:GET_OBJ'
          RETURN
        ENDIF
        MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
!       CONVERTS TYPE OF ELEMENTS
        CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_ELEM_MED, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_VALUE_MED:CONVERT_ELEM_TYPE'
          RETURN
        ENDIF
!
!       DETERMINE THE NUMBER OF NODES PER ELEMENT FROM THE TYPE
        CALL NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_VALUE_MED:NDP_FROM_ELT_TYPE_MED'
          RETURN
        ENDIF
!
!       Identify bnd element if necessary
        CALL IDENTIFY_BND_ELMT(FILE_ID,TYPE_BND_ELEM,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_VALUE_MED:IDENTIFY_BND_ELMT'
          RETURN
        ENDIF
!
        CALL GET_MESH_NPTIR_MED(FILE_ID, NPTIR, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_BND_VALUE_MED:GET_MESH_NPTIR_MED'
          RETURN
        ENDIF

        ! If no boundary doing nothing
        IF(MY_OBJ_TAB(MY_ID)%NO_BND) THEN
          IERR = 0
          RETURN
        ENDIF
!
        DO IPTFR=1,NPTFR
          LIHBOR(IPTFR) = 0
          LIUBOR(IPTFR) = 0
          LIVBOR(IPTFR) = 0
          IF (TRAC) THEN
            LITBOR(IPTFR) =  0
          ENDIF
        ENDDO

        ! IF we have a partitionned file groups are on points
        ! otherwise they are on elements
        IF(NPTIR.NE.0) THEN
          ALLOCATE(FAMILY(NPTFR),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'ALLOCATING GET_BND_VALUE_MED:FAMILY'
            RETURN
          ENDIF
          CALL GET_BND_FAMILY_MED(FILE_ID, TYPE_BND_ELEM, NPTFR,
     &                        FAMILY,IERR)
!         WRITE FAMILY NUMBER IN THE MED FILE

          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'GET_BND_VALUE_MED:GET_BND_FAMILY_MED'
            RETURN
          ENDIF
          DO IPTFR=1,NPTFR
            IGRP = FAMILY(IPTFR)
            LIHBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,1)
            LIUBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,2)
            LIVBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,3)
            IF (TRAC) THEN
              LITBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,4)
            ENDIF
          ENDDO
          DEALLOCATE(FAMILY)
        ELSE
          ALLOCATE(FAMILY(NELEBD),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'ALLOCATING GET_BND_VALUE_MED:FAMILY'
            RETURN
          ENDIF
          CALL GET_BND_FAMILY_MED(FILE_ID,TYPE_BND_ELEM,NELEBD,
     &                            FAMILY,IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'GET_BND_VALUE_MED:GET_BND_FAMILY_MED'
            RETURN
          ENDIF
!
          ALLOCATE(IKLE(NELEBD*NDP),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'ALLOCATING GET_BND_VALUE_MED:IKLE'
            RETURN
          ENDIF
!
!         GET THE CONNECTIVITY TABLE
          CALL GET_BND_CONNECTIVITY_MED(FILE_ID, TYPE_BND_ELEM, NELEBD,
     &                                  NDP, IKLE, IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'GET_BND_VALUE_MED:GET_BND_CONN_MED'
            RETURN
          ENDIF
!
          ! In case nbor was not build before
          IF(.NOT. ALLOCATED(MY_OBJ_TAB(MY_ID)%NBOR))THEN
            ALLOCATE(NBOR(NPTFR))
            CALL GET_BND_NUMBERING_MED(FILE_ID,TYPE_BND_ELEM,NPTFR,
     &                                 NBOR,IERR)
            DEALLOCATE(NBOR)
          ENDIF
!         LOOP ON ALL BOUNDARY POINTS
          DO IPTFR=1,NPTFR
            CURRENT = MY_OBJ_TAB(MY_ID)%PT2SEG(IPTFR,1)
            NEXT = MY_OBJ_TAB(MY_ID)%PT2SEG(IPTFR,2)
            INODE = MY_OBJ_TAB(MY_ID)%NBOR(IPTFR)
!
!           STORE VALUE OF THE GIVEN NODE
            IGRP_CUR = FAMILY(CURRENT)
            IGRP_NXT = FAMILY(NEXT)
            BND_TYP_CUR = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP_CUR,1)
            BND_TYP_NXT = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP_NXT,1)
            ! Identifying which boundary to take
            CALL SEG2POINT(CURRENT,NEXT,BND_TYP_CUR,BND_TYP_NXT,IELEB)
            IGRP = FAMILY(IELEB)
            ! Defining values
            LIHBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,1)
            LIUBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,2)
            LIVBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,3)
            IF (TRAC) THEN
              LITBOR(IPTFR) = MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IGRP,4)
            ENDIF
          ENDDO
!
!         FREE MEMORY
          DEALLOCATE(FAMILY)
          DEALLOCATE(IKLE)
        ENDIF
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_NVAR_MED (FILE_ID, NVAR, IERR)
!
!BRIEF    READS THE NUMBER OF DATA IN A MED FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NVAR           |-->| NUMBER OF DATA
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: NVAR
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
      INTEGER(KIND=KMED_INT) :: TMP
!
!-----------------------------------------------------------------------
!
#if defined (HAVE_MED)
!
      INTEGER MY_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_BND_VALUE_MED:GET_OBJ'
        RETURN
      ENDIF
      FID = MY_OBJ_TAB(MY_ID)%ID
!     READ THE NUMBER OF DATA
      CALL MFDNFD(FID,TMP,KIERR)
      IERR = INT(KIERR)
      NVAR = INT(TMP)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_DATA_NVAR_MED:MFDNFD'
        RETURN
      ENDIF
!
#else
!
      NVAR = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_VAR_LIST_MED (FILE_ID, NVAR, VAR_LIST,
     &                                  UNIT_LIST, IERR)
!
!BRIEF    READS THE LIST OF THE DATA NAME
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NVAR           |<--| NUMBER OF DATA
!| VAR_LIST       |-->| LIST OF THE DATA NAME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: NVAR
      CHARACTER(LEN=16), DIMENSION(:), INTENT(OUT) :: VAR_LIST
      CHARACTER(LEN=16), DIMENSION(:), INTENT(OUT) :: UNIT_LIST
      INTEGER, INTENT(OUT) :: IERR
      INTEGER(KIND=KID) :: FID
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME, MNAME
      CHARACTER(LEN=MED_SNAME_SIZE) :: CUNIT, CNAME, DTUNIT
!
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: I, ITYPE, IMESH
      INTEGER(KIND=KMED_INT) :: NSTEP
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_DATA_VAR_LIST_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     LOOP ON THE VARIABLES
      DO I = 1, NVAR
!
!       READ THE NAME OF THE VARIABLE
        CALL MFDFDI(FID,I,VAR_NAME,MNAME,IMESH,ITYPE,
     &    CNAME,CUNIT,DTUNIT,NSTEP,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_DATA_VAR_LIST_MED:MFDFDI'
          RETURN
        ENDIF
!
!       CONVERT MED FORMAT TO SLF
        VAR_LIST(I) = VAR_NAME(1:16)
        UNIT_LIST(I) = CUNIT
      ENDDO
!
#else
!
      VAR_LIST = ' '
      UNIT_LIST = ' '
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_NTIMESTEP_MED (FILE_ID, NTIMESTEP, IERR)
!
!BRIEF    READS THE NUMBER OF TIME STEP FOR A GIVEN VARIABLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NTIMESTEP      |-->| NUMBER OF TIME STEP
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: NTIMESTEP
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME_MED, MNAME
      CHARACTER(LEN=MED_SNAME_SIZE) :: CUNIT,CNAME,DTUNIT
!
      INTEGER(KIND=KMED_INT) :: ITYPE, IMESH
      INTEGER :: MY_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: NVAR, TMP
      INTEGER(KIND=KMED_INT) :: IVAR
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
      IERR = -1
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_DATA_NTIMESTEP_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     READ THE NUMBER OF VARIABLES
      CALL MFDNFD(FID,NVAR,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_DATA_NTIMESTEP_MED:MFDNFD'
        RETURN
      ENDIF
!
      IF(NVAR.NE.0) THEN
!
!       READ THE NUMBER OF TIME STEP FROM THE FIRST VARIABLE AS ALL
!       VARIABLE HAVE THE SAME NUMBER OF TIMESTEP
        IVAR=1
        CALL MFDFDI(FID,IVAR,VAR_NAME_MED,MNAME,IMESH,
     &    ITYPE,CUNIT,CNAME,DTUNIT,TMP,KIERR)
        IERR = INT(KIERR)
        NTIMESTEP = INT(TMP)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'GET_DATA_NTIMESTEP_MED:MFDFDI'
          RETURN
        ENDIF
      ELSE

        NTIMESTEP = 0
      ENDIF
!
#else
!
      NTIMESTEP = 0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_TIME_MED (FILE_ID, RECORD, TIME, IERR)
!
!BRIEF    READS PHYSICAL TIME FOR A GIVEN ITERATION NUMBER
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| RECORD         |<--| ITERATION NUMBER
!| TIME           |-->| PHYSICAL TIME
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID, RECORD
      DOUBLE PRECISION, INTENT(OUT) :: TIME
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME_MED
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
      CHARACTER(LEN=MED_SNAME_SIZE) :: CNAME,CUNIT,DTUNIT
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) LMESH, FTYPE
      INTEGER(KIND=KMED_INT) :: DT,IT
      INTEGER(KIND=KMED_INT) :: KMED_ITER
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: N
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_DATA_TIME_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     GET THE NAME OF THE FIRST FIELD TO HAVE ACCESS TO THE TIME
      CALL MFDFDI(FID,1,VAR_NAME_MED,MNAME,LMESH,FTYPE,
     &            CNAME,CUNIT,DTUNIT,N,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_DATA_TIME_MED:MFDFDI'
        RETURN
      ENDIF
!
!     READ THE PHYSICAL TIME TIME
      DT=KMED_NO_DT
      IT=KMED_NO_IT
      ! Iterations start from 1 and records start from 0
      KMED_ITER = RECORD + 1
      CALL MFDCSI(FID,VAR_NAME_MED,KMED_ITER,DT,IT,
     &            TIME,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_DATA_TIME_MED:MFDCSI'
        RETURN
      ENDIF
!
#else
!
      TIME = 0.0
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_DATA_VALUE_MED (FILE_ID,RECORD,VAR_NAME,
     &  RES_VALUE,N,IERR)
!
!BRIEF    READS DATA VALUES FOR A GIVEN ITERATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| RECORD         |<--| ITERATION NUMBER
!| VAR_NAME       |<--| NAME OF THE DATA
!| RES_VALUE      |-->| VECTOR WITH THE VALUE OF THE VARIABLE
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, RECORD, N
      CHARACTER(LEN=16), INTENT(IN) :: VAR_NAME
      DOUBLE PRECISION, INTENT(INOUT) :: RES_VALUE(N)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: VAR_NAME_MED
!
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: LMESH, ITYPE
      CHARACTER(LEN=MED_SNAME_SIZE) :: CNAME,CUNIT,DTUNIT
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
      INTEGER(KIND=KMED_INT) :: NCOMP, NSTEP
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: RECORD2, COMP
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'GET_DATA_VALUE_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     CONVERT SLF FORMAT TO MED
      VAR_NAME_MED = TRIM(VAR_NAME)//CHAR(0)
!
!     CHECKING THAT THE VARIABLE IS THE FILE
      CALL MFDNCN(FID,VAR_NAME_MED,NCOMP,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       VAR_NAME_MED//' IS UNKNOWN'
        IERR = HERMES_VAR_UNKNOWN_ERR
        RETURN
      ENDIF
!
!     CHECKING THAT THE RECORD IS A VALID ONE (BETWEEN 0 AND
!     NTIMESTEP-1)
!     READ THE NAME OF THE VARIABLE
      CALL MFDFIN(FID,VAR_NAME_MED,MNAME,LMESH,ITYPE,
     &            CNAME,CUNIT,DTUNIT,NSTEP,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_DATA_VALUE_MED:MFDFIN'
        RETURN
      ENDIF

      IF(RECORD.GE.INT(NSTEP).OR.RECORD.LT.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       I2CHAR(RECORD)//' IS NOT BETWEEN 0 AND'//I2CHAR(INT(NSTEP))
        IERR = HERMES_RECORD_UNKNOWN_ERR
        RETURN
      ENDIF
!
!     READ THE VALUE FOR THIS VARIABLE AT THIS TIMESTEP
      RECORD2 = RECORD
      COMP = 1
      CALL MFDRVR(FID,VAR_NAME_MED,RECORD2,KMED_NO_IT,KMED_NODE,
     &  KMED_NONE,KMED_NO_INTERLACE,COMP,RES_VALUE,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'GET_DATA_VALUE_MED:MFDRVR'
        RETURN
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_HEADER_MED(FILE_ID,TITLE,IERR)
!
!BRIEF    WRITES THE TITLE OF THE MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TITLE          |<--| TITLE OF THE MESH
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=80), INTENT(IN)  :: TITLE
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_COMMENT_SIZE) :: TITLE_MED
      INTEGER :: MY_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'SET_HEADER_MED:GET_OBJ'
        RETURN
      ENDIF
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     CONVERSION FROM SLF FORMAT TO MED
      TITLE_MED = TITLE
!     WRITES THE TITLE OF THE MESH
      CALL MFICOW(FID,TITLE_MED,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_HEADER_MED:MFICOW'
        RETURN
      ENDIF
!
      MY_OBJ_TAB(MY_ID)%MESH_NAME = 'MESH'//
     &   TRIM(MY_OBJ_TAB(MY_ID)%MESH_NUMBER_STR)
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_MESH_MED(FILE_ID,NB_DIM_PB,NB_DIM_MESH,TYPE_ELEM,
     &  NDP,NPTIR,NELEM,NPOIN,IKLE,KNOLG,COORD,NPLAN,
     &  DATE,TIME,X_ORIG,Y_ORIG,IERR)
!
!BRIEF    WRITES A MESH IN A MED FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| NB_DIM_PB      |<--| NUMBER OF DIMENSION OF THE PHYSICAL DOMAIN
!| NB_DIM_MESH    |<--| NUMBER OF DIMENSION OF THE MESH
!| TYPE_ELEM      |<--| TYPE OF THE ELEMENT IN SLF FORMAT
!| NDP            |<--| NUMBER OF POINTS PER ELEMENT
!| NPTIR          |<--| NUMBER OF INTERFACE NODES FOR THE SUB-DOMAIN
!| NELEM          |<--| TOTAL NUMBER OF ELEMENTS
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| IKLE           |<--| CONNECTIVITY TABLE
!| KNOLG          |<--| NODES INDEX TABLE FROM LOCAL TO GLOBAL
!| COORD          |<--| COORDINATES OF THE NODES
!| NPLAN          |<--| NUMBER OF PLANES
!| DATE           |<--| DATE OF THE FILE
!| TIME           |<--| TIME OF THE FILE
!| X_ORIG         |<--| Off set of the X coordinates
!| Y_ORIG         |<--| Off set of the Y coordinates
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, NB_DIM_PB, NB_DIM_MESH
      INTEGER, INTENT(IN) :: NELEM, NPOIN, TYPE_ELEM, NDP, NPTIR
      INTEGER, INTENT(IN) :: NPLAN, X_ORIG, Y_ORIG
      INTEGER, INTENT(IN) :: IKLE(NDP*NELEM), KNOLG(*)
      DOUBLE PRECISION, INTENT(IN) :: COORD(NB_DIM_MESH*NPOIN)
      INTEGER, INTENT(IN) :: DATE(3), TIME(3)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE)    :: MNAME
      CHARACTER(LEN=MED_NAME_SIZE)    :: DT_UNIT_MED
      CHARACTER(LEN=MED_NAME_SIZE)    :: FAM_ZERO
      CHARACTER(LEN=MED_NAME_SIZE)    :: PNAME
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_NAME_MED(:)
      CHARACTER(LEN=MED_SNAME_SIZE),ALLOCATABLE :: COOR_UNIT_MED(:)
      CHARACTER(LEN=MED_SNAME_SIZE)   :: COOR_X, COOR_Y, COOR_Z
      CHARACTER(LEN=MED_SNAME_SIZE)   :: COOR_UNIT
      CHARACTER(LEN=MED_COMMENT_SIZE) :: COMMENT
!
      INTEGER :: MY_ID, I
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER(KIND=KMED_INT) :: IFAM, MYDATE
      INTEGER(KIND=KMED_INT) :: NPLAN_TMP
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: NB_DIM_PB2, NB_DIM_MESH2
      INTEGER(KIND=KMED_INT) :: KNPOIN, KNELEM
      INTEGER(KIND=KMED_INT) :: TMP2
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     INITIALISE COORDINATE UNIT AND NAME
      COMMENT = 'TELEMAC GENERATED MESH'//CHAR(0)
      COOR_X = 'X'//CHAR(0)
      COOR_Y = 'Y'//CHAR(0)
      COOR_Z = 'Z'//CHAR(0)
      COOR_UNIT = 'M'//CHAR(0)
      ALLOCATE(COOR_NAME_MED(NB_DIM_PB),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'ALLOCATING COOR_NAME_MED'
        RETURN
      ENDIF
      ALLOCATE(COOR_UNIT_MED(NB_DIM_PB),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'ALLOCATING COOR_UNIT_MED'
        RETURN
      ENDIF
      COOR_NAME_MED(1) = COOR_X
      COOR_NAME_MED(2) = COOR_Y
      IF (NB_DIM_PB.EQ.3) COOR_NAME_MED(3) = COOR_Z
      DO I=1,NB_DIM_PB
        COOR_UNIT_MED(I) = COOR_UNIT
      ENDDO
!
!     INITIALISE TIME UNIT
      DT_UNIT_MED = 'S'//CHAR(0)
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
!
      FID = MY_OBJ_TAB(MY_ID)%ID
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_ELEM,TYPE_ELEM_MED,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
!     CREATES A MESH IN THE MED FILE
      NB_DIM_PB2 = NB_DIM_PB
      NB_DIM_MESH2 = NB_DIM_MESH
      CALL MMHCRE(FID,MNAME,NB_DIM_PB2,NB_DIM_MESH2,
     &  KMED_UNSTRUCTURED_MESH,COMMENT,DT_UNIT_MED,KMED_SORT_DTIT,
     &  KMED_CARTESIAN,COOR_NAME_MED,COOR_UNIT_MED,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MMHCRE'
        RETURN
      ENDIF
      DEALLOCATE(COOR_NAME_MED)
      DEALLOCATE(COOR_UNIT_MED)
!
!     WRITES NODE COORDINATES
      KNPOIN = NPOIN
      CALL MMHCOW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,0.D0,
     &  KMED_NO_INTERLACE,KNPOIN,COORD,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MMHCOW'
        RETURN
      ENDIF
!
!     WRITES CONNECTIVITY TABLE
#if defined (HAVE_MED64)
      ALLOCATE(TMP(NELEM*NDP))
      DO I=1,NELEM*NDP
        TMP(I) = IKLE(I)
      ENDDO
      KNELEM = NELEM
      CALL MMHCYW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,0.0,KMED_CELL,
     &  TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,KNELEM,TMP,KIERR)
      IERR = INT(KIERR)
      DEALLOCATE(TMP)
#else
      CALL MMHCYW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,0.0,KMED_CELL,
     &  TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,NELEM,IKLE,IERR)
#endif
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MMHCYW'
        RETURN
      ENDIF
!
!     WRITES THE NUMBER OF PMANES
      PNAME = 'NPLAN'//char(0)
      COMMENT = 'Number of planes'//char(0)
!
!     CREATES THE PARAMETER
      CALL MPRCRE(FID,PNAME,KMED_INT32,COMMENT,DT_UNIT_MED,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRCRE'
        RETURN
      ENDIF
!
!     SETS THE VALUE
      IF(NPLAN.LE.1) THEN
        NPLAN_TMP = 0
      ELSE
        NPLAN_TMP = NPLAN
      ENDIF
      CALL MPRIVW(FID,PNAME,KMED_NO_DT,KMED_NO_IT,0.0,NPLAN_TMP,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRIVW'
        RETURN
      ENDIF
!     WRITES THE ORIGIN
      PNAME = 'X_ORIG'//char(0)
      COMMENT = 'X origin'//char(0)
!
!     CREATES THE PARAMETER
      CALL MPRCRE(FID,PNAME,KMED_INT32,COMMENT,DT_UNIT_MED,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRCRE'
        RETURN
      ENDIF
      ! write value
      TMP2 = X_ORIG
      CALL MPRIVW(FID,PNAME,KMED_NO_DT,KMED_NO_IT,0.0,TMP2,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRIVW'
        RETURN
      ENDIF
      PNAME = 'Y_ORIG'//char(0)
      COMMENT = 'Y origin'//char(0)
!
!     CREATES THE PARAMETER
      CALL MPRCRE(FID,PNAME,KMED_INT32,COMMENT,DT_UNIT_MED,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRCRE'
        RETURN
      ENDIF
!
      ! write value
      TMP2 = Y_ORIG
      CALL MPRIVW(FID,PNAME,KMED_NO_DT,KMED_NO_IT,0.0,TMP2,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRIVW'
        RETURN
      ENDIF
!
!     WRITES THE DATE
      PNAME = 'DATE'//char(0)
      COMMENT = 'Date of the file'//char(0)
!
!     CREATES THE PARAMETER
      CALL MPRCRE(FID,PNAME,KMED_INT32,COMMENT,DT_UNIT_MED,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRCRE'
        RETURN
      ENDIF
!
!     SETS THE VALUE
      MYDATE = DATE(1)*10000 +
     &         DATE(2)*100 +
     &         DATE(3)
      CALL MPRIVW(FID,PNAME,KMED_NO_DT,KMED_NO_IT,0.0,MYDATE,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRIVW'
        RETURN
      ENDIF
!
!     WRITES THE TIME
      PNAME = 'TIME'//char(0)
      COMMENT = 'Time of the file'//char(0)
!
!     CREATES THE PARAMETER
      CALL MPRCRE(FID,PNAME,KMED_INT32,COMMENT,DT_UNIT_MED,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRCRE'
        RETURN
      ENDIF
!
!     SETS THE VALUE
      MYDATE = TIME(1)*10000 +
     &         TIME(2)*100 +
     &         TIME(3)
      CALL MPRIVW(FID,PNAME,KMED_NO_DT,KMED_NO_IT,0.0,MYDATE,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MPRIVW'
        RETURN
      ENDIF
!
!     IF PARTITIONNED FILE
      IF(NPTIR.NE.0) THEN
!
!       WRITES THE GLOBAL NUMBERING
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NPOIN))
        DO I=1,NPOIN
          TMP(I) = KNOLG(I)
        ENDDO
        KNPOIN = NPOIN
        CALL MMHGNW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,KNPOIN,TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        CALL MMHGNW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,NPOIN,KNOLG,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'SET_MESH_MED:MMHGNW'
          RETURN
        ENDIF
!
!       WRITES THE NUMBER OF INTERFACE POINT AS A SCALAR PARAMETER
        PNAME = 'NPTIR'//TRIM(MY_OBJ_TAB(MY_ID)%MESH_NUMBER_STR)
     &           //CHAR(0)
        COMMENT = 'Number of interface points'//char(0)
!
!       CREATES THE PARAMETER
        CALL MPRCRE(FID,PNAME,KMED_INT32,COMMENT,DT_UNIT_MED,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'SET_MESH_MED:MPRCRE'
          RETURN
        ENDIF
!
!       SETS THE VALUE
        TMP2 = NPTIR
        CALL MPRIVW(FID,PNAME,KMED_NO_DT,KMED_NO_IT,0.0,TMP2,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'SET_MESH_MED:MPRIVW'
          RETURN
        ENDIF
      ENDIF
!
!     CREATE FAMILLY ZERO
!     DEFAULT FAMILY NEEDED BY MED FILE
      FAM_ZERO = 'FAMILY_ZERO'//CHAR(0)
      IFAM = 0
      CALL MFACRE(FID,MNAME,FAM_ZERO,IFAM,IFAM,' ',KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_MESH_MED:MFACRE'
        RETURN
      ENDIF
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ADD_DATA_MED (FILE_ID, VAR_NAME, TIME, RECORD,
     &  VAR_VALUE, N, IERR)
!
!BRIEF    WRITE DATA VALUES FOR A GIVEN VARIABLE
!         ONLY FOR VALUES WITH DOUBLE PRECISION, ONE COMPONENT AND VALUE
!         ON THE NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| VAR_NAME       |<--| NAME OF THE DATA
!| TIME           |<--| PHYSICAL TIME
!| RECORD         |<--| ITERATION NUMBER
!| VAR_VALUE      |<--| TABLE OF VARIABLE VALUES
!| N              |<--| NUMBER OF ELEMENTS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, N
      INTEGER, INTENT(IN) :: RECORD
      CHARACTER(LEN=32), INTENT(IN) :: VAR_NAME
      DOUBLE PRECISION, INTENT(IN) :: TIME
      DOUBLE PRECISION, INTENT(IN) :: VAR_VALUE(N)
      INTEGER, INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME, VAR_NAME_MED
      CHARACTER(LEN=MED_SNAME_SIZE) :: VAR_COMP_MED, UNIT_VAR_MED
      CHARACTER(LEN=MED_SNAME_SIZE) :: UNIT_TIME_MED
      INTEGER(KIND=KMED_INT) :: NB_COMP
      INTEGER :: MY_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT) :: KIERR, N2, KRECORD
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'ADD_DATA_MED:GET_MED_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      NB_COMP = 1! ONLY ONE COMPONENT
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     CONVERT SLF FORMAT TO MED
      VAR_NAME_MED  = TRIM(VAR_NAME(1:16))//CHAR(0)
      VAR_COMP_MED  = TRIM(VAR_NAME(1:16))//CHAR(0)
      UNIT_VAR_MED  = TRIM(VAR_NAME(17:32))//CHAR(0)
      UNIT_TIME_MED = 'S'//CHAR(0)
!
      IF(RECORD.EQ.0) THEN
!       CREATE A NEW VARIABLE (DOUBLE PRECISION WITH ONE COMPONENT)
        CALL MFDCRE(FID,VAR_NAME_MED,KMED_FLOAT64,NB_COMP,
     &    VAR_COMP_MED,UNIT_VAR_MED,UNIT_TIME_MED,MNAME,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ADD_DATA_MED:MFDCRE'
          RETURN
        ENDIF
      ENDIF
!
!     WRITE DATA VALUES FOR A GIVEN VARIABLE
      N2 = N
      KRECORD = RECORD
      CALL MFDRVW(FID,VAR_NAME_MED,KRECORD,KMED_NO_IT,TIME,KMED_NODE,
     &  KMED_NONE,KMED_NO_INTERLACE,NB_COMP,N2,VAR_VALUE,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'ADD_DATA_MED:MFDRVW'
        RETURN
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
#if defined HAVE_MED
      SUBROUTINE TRANSFER_GROUP_INFO_COMMON_MED(SOURCE_ID,DEST_ID,
     &                    MNAME_SRC,MNAME_DST,HAS_FAM_ON_POINT,IERR)
!BRIEF    Transfer group and families definition from source_id into
!+        dest_id
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SOURCE_ID       |-->| FILE DESCRIPTOR OF INPUT FILE
!| DEST_ID         |-->| FILE DESCRIPTOR OF OUTPUT FILE
!| MNAME_SRC       |<->| MESH NAME FOR INPUT FILE
!| MNAME_DST       |<->| MESH NAME FOR OUTPUT FILE
!| HAS_FAM_ON_POINT|-->| CONNECTIVITY FOR OUTPUT FILE BOUNDARY ELEMENTS
!| IERR            |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,          INTENT(IN)  :: SOURCE_ID
      INTEGER,          INTENT(IN)  :: DEST_ID
      CHARACTER(LEN=MED_NAME_SIZE), INTENT(INOUT):: MNAME_SRC, MNAME_DST
      LOGICAL, INTENT(OUT) :: HAS_FAM_ON_POINT
      INTEGER,          INTENT(OUT) :: IERR
!
      INTEGER :: MY_ID_SRC, MY_ID_DST
      INTEGER :: NBND_GRP
      CHARACTER(LEN=MED_LNAME_SIZE),ALLOCATABLE :: GRP_NAME(:)
      CHARACTER(LEN=MED_NAME_SIZE) FAM_NAME
      INTEGER :: I
      INTEGER(KIND=KMED_INT) :: IFAM
      INTEGER(KIND=KMED_INT) :: NB_FAMILY, NB_GRP,INUM
      INTEGER :: NPTIR
      INTEGER(KIND=KID) :: SOURCE_FID, DEST_FID
      INTEGER(KIND=KMED_INT) :: KIERR, TMP

!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,SOURCE_ID,MY_ID_SRC,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH '//I2CHAR(SOURCE_ID)//': '//
     &       'TRANSFER_GROUP_INFO_COMMON_MED:GET_OBJ'
        RETURN
      ENDIF
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,DEST_ID,MY_ID_DST,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID'//I2CHAR(DEST_ID)//': '//
     &       'TRANSFER_GROUP_INFO_COMMON_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME_SRC = MY_OBJ_TAB(MY_ID_SRC)%MESH_NAME
      MNAME_DST = MY_OBJ_TAB(MY_ID_DST)%MESH_NAME
      SOURCE_FID = MY_OBJ_TAB(MY_ID_SRC)%ID
      DEST_FID = MY_OBJ_TAB(MY_ID_DST)%ID
!
      CALL MFANFA(SOURCE_FID,MNAME_SRC,NB_FAMILY,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &       'TRANSFER_GROUP_INFO_COMMON_MED:MFANFA'
        RETURN
      ENDIF
!
      HAS_FAM_ON_POINT = .FALSE.
      ! Creating all the families that are in src into dest
      DO IFAM = 1, NB_FAMILY
!
!       READ THE NUMBER OF GROUP PER FAMILY
        CALL MFANFG(SOURCE_FID,MNAME_SRC,IFAM,NB_GRP,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO_COMMON_MED:MFANFG'
          RETURN
        ENDIF
!
        ALLOCATE(GRP_NAME(MAX(INT(NB_GRP),1)),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_INFO_COMMON_MED:GRP_NAME'
          RETURN
        ENDIF
        GRP_NAME(:) = REPEAT(' ',MED_LNAME_SIZE)

!       READ THE TYPE OF FAMILY
        CALL MFAFAI(SOURCE_FID,MNAME_SRC,IFAM,FAM_NAME,INUM,
     &              GRP_NAME,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO_COMMON_MED:MFAFAI'
          RETURN
        ENDIF
        ! Skipping famille_zero
        IF(INT(INUM).EQ.0) THEN
          DEALLOCATE(GRP_NAME)
          CYCLE
        ENDIF
        ! Families on point have a positive number
        IF(INUM.GT.0) HAS_FAM_ON_POINT = .TRUE.
!
        ! WRITE THE FAMILY
        TMP = MAX(NB_GRP,1)
        CALL MFACRE(DEST_FID,MNAME_DST,FAM_NAME,INUM,TMP,
     &              GRP_NAME,KIERR)
        IERR = INT(KIERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO_COMMON_MED:MFACRE'
          RETURN
        ENDIF
!
        DEALLOCATE(GRP_NAME)
      ENDDO

      CALL GET_MESH_NPTIR_MED(DEST_ID, NPTIR, IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &       'TRANSFER_GROUP_INFO_COMMON_MED:GET_MESH_NPTIR_MED'
        RETURN
      ENDIF

      ! Only writing grp info if not in a partitionned file
      IF(NPTIR.EQ.0) THEN
        ! Transfering group values from SRC_ID
        MY_OBJ_TAB(MY_ID_DST)%NBND_GRP =
     &          MY_OBJ_TAB(MY_ID_SRC)%NBND_GRP
        NBND_GRP = MY_OBJ_TAB(MY_ID_DST)%NBND_GRP
        ALLOCATE(MY_OBJ_TAB(MY_ID_DST)%BND_GRP_VAL(NBND_GRP,4),
     &           STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_INFO_COMMON_MED:BND_GRP_VAL'
          RETURN
        ENDIF
        ALLOCATE(MY_OBJ_TAB(MY_ID_DST)%BND_GRP_NAME(NBND_GRP),
     &           STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_INFO_COMMON_MED:BND_GRP_NAME'
          RETURN
        ENDIF
        DO I=1,NBND_GRP
          MY_OBJ_TAB(MY_ID_DST)%BND_GRP_VAL(I,1) =
     &           MY_OBJ_TAB(MY_ID_SRC)%BND_GRP_VAL(I,1)
          MY_OBJ_TAB(MY_ID_DST)%BND_GRP_VAL(I,2) =
     &           MY_OBJ_TAB(MY_ID_SRC)%BND_GRP_VAL(I,2)
          MY_OBJ_TAB(MY_ID_DST)%BND_GRP_VAL(I,3) =
     &           MY_OBJ_TAB(MY_ID_SRC)%BND_GRP_VAL(I,3)
          MY_OBJ_TAB(MY_ID_DST)%BND_GRP_VAL(I,4) =
     &           MY_OBJ_TAB(MY_ID_SRC)%BND_GRP_VAL(I,4)
          MY_OBJ_TAB(MY_ID_DST)%BND_GRP_NAME(I)(:) =
     &           MY_OBJ_TAB(MY_ID_SRC)%BND_GRP_NAME(I)
        ENDDO
      ENDIF
      END SUBROUTINE
#endif
!
      SUBROUTINE TRANSFER_GROUP_PART_INFO_MED(
     &     SOURCE_ID, DEST_ID,
     &     TYP_BND_ELEM, IKLE_BND_DEST, NELEBD_DEST,
     &     NDP_DEST, NELEBD_SRC, KNOGL_BND,
     &     TRANS_POINT, NPOIN_SRC, NPOIN_DEST, KNOLG, IERR)
!BRIEF    Transfer group information from source_id into dest_id
!+        Writes boundary elements as well where dest_id is a partition
!+        of source_id
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SOURCE_ID      |-->| FILE DESCRIPTOR OF INPUT FILE
!| DEST_ID        |-->| FILE DESCRIPTOR OF OUTPUT FILE
!| TYPE_ELEM      |-->| TYPE OF THE ELEMENTS
!| TYPE_BND_ELEM  |-->| TYPE OF THE BOUNDARY ELEMENTS
!| IKLE_BND_DEST  |-->| CONNECTIVITY FOR OUTPUT FILE BOUNDARY ELEMENTS
!| NELEBD_DEST    |-->| NUMBER OF BOUNDARY ELEMENTS IN OUTPUT FILE
!| NDP_DEST       |-->| Number of node per element in output file
!| NELEBD_SRC     |-->| Number of noudary elements in the input file
!| KNOGL_BND      |-->| Local to global numbering for boundary elements
!| TRANS_POINT    |-->| IF TRUE TRANSFERING GROUP ON POINTS AS WELL
!| NPOIN_SRC      |-->| Number of points in input file
!| NPOIN_DEST     |-->| Number if points in output file
!| KNOLG          |-->| Local to global numbering
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      IMPLICIT NONE
!
      INTEGER,          INTENT(IN)  :: SOURCE_ID
      INTEGER,          INTENT(IN)  :: DEST_ID
      INTEGER,          INTENT(IN)  :: NELEBD_DEST
      INTEGER,          INTENT(IN)  :: NDP_DEST
      INTEGER,      INTENT(IN)  :: IKLE_BND_DEST(NELEBD_DEST*NDP_DEST)
      INTEGER,          INTENT(IN)  :: TYP_BND_ELEM
      INTEGER,          INTENT(IN)  :: NELEBD_SRC
      INTEGER,          INTENT(IN)  :: KNOGL_BND(NELEBD_SRC)
      LOGICAL,          INTENT(IN)  :: TRANS_POINT
      INTEGER,          INTENT(IN)  :: NPOIN_SRC
      INTEGER,          INTENT(IN)  :: NPOIN_DEST
      INTEGER,          INTENT(IN)  :: KNOLG(NPOIN_DEST)
      INTEGER,          INTENT(OUT) :: IERR
!
#if defined HAVE_MED
      CHARACTER(LEN=MED_NAME_SIZE)    :: MNAME, MNAME2
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER, ALLOCATABLE :: FAM_NUM(:)
      INTEGER, ALLOCATABLE :: FAM_NUM_DEST(:)
      INTEGER :: I,J
      INTEGER :: NELEM_SRC
      LOGICAL :: HAS_FAM_ON_POINT
      INTEGER :: MY_ID_SRC, MY_ID_DST
      INTEGER(KIND=KID) :: SOURCE_FID, DEST_FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
      INTEGER(KIND=KMED_INT) :: KNELEBD_DEST
      INTEGER(KIND=KMED_INT) :: KNPOIN_DEST
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!

      ! Transferring families and group info
      CALL TRANSFER_GROUP_INFO_COMMON_MED(SOURCE_ID, DEST_ID, MNAME,
     &                 MNAME2, HAS_FAM_ON_POINT, IERR)
      IF(IERR.NE.0) RETURN
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,SOURCE_ID,MY_ID_SRC,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(SOURCE_ID)//': '//
     &       'TRANSFER_GROUP_PART_INFO_MED:GET_OBJ'
        RETURN
      ENDIF

      CALL GET_OBJ(HASH,DEST_ID,MY_ID_DST,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(SOURCE_ID)//': '//
     &       'TRANSFER_GROUP_PART_INFO_MED:GET_OBJ'
        RETURN
      ENDIF
      SOURCE_FID = MY_OBJ_TAB(MY_ID_SRC)%ID
      DEST_FID = MY_OBJ_TAB(MY_ID_DST)%ID

      ! For boundary elements
      IF(TYP_BND_ELEM.NE.0.AND.NELEBD_DEST.NE.0) THEN
!       READ THE FAMILY NUMBER FOR EACH ELEMENT
        CALL CONVERT_ELEM_TYPE(TYP_BND_ELEM, TYPE_ELEM_MED, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &       'ALLOCATING TRANSFER_GROUP_PART_INFO_MED:CONVERT_ELEM_TYPE'
          RETURN
        ENDIF

!       WRITES CONNECTIVITY TABLE
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEBD_DEST*NDP_DEST))
        DO I=1,NELEBD_DEST*NDP_DEST
          TMP(I) = IKLE_BND_DEST(I)
        ENDDO
        KNELEBD_DEST = NELEBD_DEST
        CALL MMHCYW(DEST_FID,MNAME2,KMED_NO_DT,KMED_NO_IT,0.0,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,KNELEBD_DEST,
     &    TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        CALL MMHCYW(DEST_FID,MNAME2,KMED_NO_DT,KMED_NO_IT,0.0,KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,NELEBD_DEST,
     &    IKLE_BND_DEST,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_PART_INFO_MED:MMHCYW:BND'
          RETURN
        ENDIF

        ! Number of element of type typ_bnd_elem
        CALL GET_MESH_NELEM_MED(SOURCE_ID, TYP_BND_ELEM,
     &                          NELEM_SRC, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_PART_INFO_MED:GET_MESH_NELEM_MED'
          RETURN
        ENDIF

        ! Getting family number of every boundary element in input file
        ALLOCATE(FAM_NUM(NELEM_SRC),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_PART_INFO_MED:FAM_NUM:BND'
          RETURN
        ENDIF
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEM_SRC))
        CALL MMHFNR(SOURCE_FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &              TYPE_ELEM_MED,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NELEM_SRC
          FAM_NUM(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHFNR(SOURCE_FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &              TYPE_ELEM_MED,FAM_NUM,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_PART_INFO_MED:MMHFNR:BND'
          RETURN
        ENDIF

        ! Building family number for ouput file using knolg_bnd
        ALLOCATE(FAM_NUM_DEST(NELEBD_DEST),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'ALLOCATING FAM_NUM_DEST:BND'
          RETURN
        ENDIF
        J = 0
        DO I=1,NELEM_SRC
          IF(MY_OBJ_TAB(MY_ID_SRC)%IS_BND(I)) THEN
            J = J+1
            IF(KNOGL_BND(J).EQ.0) THEN
              CYCLE
            ENDIF
            FAM_NUM_DEST(KNOGL_BND(J)) = FAM_NUM(I)
          ENDIF
        ENDDO
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEBD_DEST))
        DO I=1,NELEBD_DEST
          TMP(I) = FAM_NUM_DEST(I)
        ENDDO
        KNELEBD_DEST = NELEBD_DEST
        ! WRITTING FAMILY NUMBER FOR BOUNDARY ELEMENT
        CALL MMHFNW(DEST_FID,MNAME2,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &              TYPE_ELEM_MED,KNELEBD_DEST,TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        ! Writting family number for boundary element
        CALL MMHFNW(DEST_FID,MNAME2,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &              TYPE_ELEM_MED,NELEBD_DEST,FAM_NUM_DEST,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_PART_INFO_MED:MMHFNW:BND'
          RETURN
        ENDIF

        DEALLOCATE(FAM_NUM)
        DEALLOCATE(FAM_NUM_DEST)
      ENDIF

      ! For nodes
      IF(TRANS_POINT.AND.HAS_FAM_ON_POINT) THEN
!       READ THE FAMILY NUMBER FOR EACH ELEMENT

        ALLOCATE(FAM_NUM(NPOIN_SRC),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_PART_INFO_MED:FAM_NUM'
          RETURN
        ENDIF

#if defined (HAVE_MED64)
        ALLOCATE(TMP(NPOIN_SRC))
        CALL MMHFNR(SOURCE_FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NPOIN_SRC
          FAM_NUM(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHFNR(SOURCE_FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,FAM_NUM,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_PART_INFO_MED:MMHFNR'
          RETURN
        ENDIF

        ! Building family number for ouput file using knolg
        ALLOCATE(FAM_NUM_DEST(NPOIN_DEST),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'ALLOCATING FAM_NUM_DEST'
          RETURN
        ENDIF
        DO I=1,NPOIN_DEST
          FAM_NUM_DEST(I) = FAM_NUM(KNOLG(I))
        ENDDO
        ! Writting family number for point
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NPOIN_DEST))
        DO I=1,NPOIN_DEST
          TMP(I) = FAM_NUM_DEST(I)
        ENDDO

        KNPOIN_DEST = NPOIN_DEST
        CALL MMHFNW(DEST_FID,MNAME2,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,KNPOIN_DEST,TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        CALL MMHFNW(DEST_FID,MNAME2,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,NPOIN_DEST,FAM_NUM_DEST,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_PART_INFO_MED:MMHFNW'
          RETURN
        ENDIF

        DEALLOCATE(FAM_NUM)
        DEALLOCATE(FAM_NUM_DEST)
      ENDIF
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!
      SUBROUTINE TRANSFER_GROUP_INFO_MED(SOURCE_ID, DEST_ID, TYP_ELEM,
     &                               TYP_BND_ELEM, IKLE_BND, NELEBD,
     &                               NDP, TRANS_ELEM,
     &                               TRANS_POINT, IERR)
!BRIEF    Transfer group information from source_id into dest_id
!         Writes boundary elements as well
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| SOURCE_ID      |-->| FILE DESCRIPTOR OF INPUT FILE
!| DEST_ID        |-->| FILE DESCRIPTOR OF OUTPUT FILE
!| TYPE_ELT       |-->| TYPE OF THE ELEMENTS
!| TYPE_BND_ELT   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| TRANS_ELEM     |-->| IF TRUE TRANSFERING GROUP ON TYP_ELT AS WELL
!| TRANS_POINT    |-->| IF TRUE TRANSFERING GROUP ON POINTS AS WELL
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      IMPLICIT NONE
!
      INTEGER,          INTENT(IN)  :: SOURCE_ID
      INTEGER,          INTENT(IN)  :: DEST_ID
      INTEGER,          INTENT(IN)  :: NELEBD
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: IKLE_BND(NELEBD*NDP)
      INTEGER,          INTENT(IN)  :: TYP_ELEM
      INTEGER,          INTENT(IN)  :: TYP_BND_ELEM
      LOGICAL,          INTENT(IN)  :: TRANS_ELEM
      LOGICAL,          INTENT(IN)  :: TRANS_POINT
      INTEGER,          INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE)    :: MNAME_SRC, MNAME_DST
      INTEGER(KIND=KMED_INT) :: TYPE_ELEM_MED
      INTEGER, ALLOCATABLE :: FAM_NUM_SRC(:), FAM_NUM_DST(:)
      INTEGER :: NPOIN
      INTEGER :: NELEM,NELEBD_SRC
      LOGICAL :: HAS_FAM_ON_POINT
      INTEGER :: I,J
      INTEGER :: MY_ID_SRC, MY_ID_DST
      INTEGER(KIND=KID) :: SOURCE_FID, DEST_FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
      INTEGER(KIND=KMED_INT) :: KNELEBD, KNPOIN, KNELEM
#endif
      INTEGER(KIND=KMED_INT) :: KIERR
!
      CALL GET_OBJ(HASH,SOURCE_ID,MY_ID_SRC,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(SOURCE_ID)//': '//
     &       'TRANSFER_GROUP_INFO:GET_OBJ'
        RETURN
      ENDIF

      CALL GET_OBJ(HASH,DEST_ID,MY_ID_DST,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(SOURCE_ID)//': '//
     &       'TRANSFER_GROUP_PART_INFO_MED:GET_OBJ'
        RETURN
      ENDIF
      SOURCE_FID = MY_OBJ_TAB(MY_ID_SRC)%ID
      DEST_FID = MY_OBJ_TAB(MY_ID_DST)%ID

      ! Transferring families and group info
      CALL TRANSFER_GROUP_INFO_COMMON_MED(SOURCE_ID, DEST_ID, MNAME_SRC,
     &                 MNAME_DST, HAS_FAM_ON_POINT, IERR)
      IF(IERR.NE.0) RETURN
      ! For boundary elements
      IF(TYP_BND_ELEM.NE.0.AND.NELEBD.NE.0) THEN
!       READ THE FAMILY NUMBER FOR EACH ELEMENT
        CALL CONVERT_ELEM_TYPE(TYP_BND_ELEM, TYPE_ELEM_MED, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:CONVERT_ELEM_TYPE'
          RETURN
        ENDIF

!       WRITES CONNECTIVITY TABLE
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEBD*NDP))
        DO I=1,NELEBD*NDP
          TMP(I) = IKLE_BND(I)
        ENDDO
        KNELEBD = NELEBD
        CALL MMHCYW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,0.0,
     &    KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,KNELEBD,
     &    TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        CALL MMHCYW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,0.0,
     &    KMED_CELL,
     &    TYPE_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,NELEBD,
     &    IKLE_BND,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:MMHCYW'
          RETURN
        ENDIF

        CALL GET_MESH_NELEM_MED(SOURCE_ID, TYP_BND_ELEM,
     &                          NELEBD_SRC, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:GET_MESH_NELEMT_MED'
          RETURN
        ENDIF

        ALLOCATE(FAM_NUM_SRC(NELEBD_SRC),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_INFO:FAM_NUM_DST'
          RETURN
        ENDIF

#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEBD_SRC))
        CALL MMHFNR(SOURCE_FID,MNAME_SRC,KMED_NO_DT,KMED_NO_IT,
     &              KMED_CELL,TYPE_ELEM_MED,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NELEBD_SRC
          FAM_NUM_SRC(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHFNR(SOURCE_FID,MNAME_SRC,KMED_NO_DT,KMED_NO_IT,
     &              KMED_CELL,TYPE_ELEM_MED,FAM_NUM_SRC,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:MMHFNR'
          RETURN
        ENDIF

        ALLOCATE(FAM_NUM_DST(NELEBD),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_INFO:FAM_NUM_DST'
          RETURN
        ENDIF
        J = 0
        DO I=1,NELEBD_SRC
          IF(MY_OBJ_TAB(MY_ID_SRC)%IS_BND(I)) THEN
            J = J+1
            FAM_NUM_DST(J) = FAM_NUM_SRC(I)
          ENDIF
        ENDDO

#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEBD))
        DO I=1,NELEBD
          TMP(I) = FAM_NUM_DST(I)
        ENDDO
        KNELEBD = NELEBD
        CALL MMHFNW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &              TYPE_ELEM_MED,KNELEBD,TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        CALL MMHFNW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &              TYPE_ELEM_MED,NELEBD,FAM_NUM_DST,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:MMHFNW'
          RETURN
        ENDIF
        DEALLOCATE(FAM_NUM_DST)
        DEALLOCATE(FAM_NUM_SRC)
      ENDIF

      IF(TRANS_ELEM) THEN
        ! For elements
        CALL GET_MESH_NELEM_MED(SOURCE_ID, TYP_ELEM, NELEM, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:GET_MESH_NELEMT_MED'
          RETURN
        ENDIF
!       READ THE FAMILY NUMBER FOR EACH ELEMENT
        CALL CONVERT_ELEM_TYPE(TYP_ELEM, TYPE_ELEM_MED, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:CONVERT_ELEM_TYPE'
          RETURN
        ENDIF

        ALLOCATE(FAM_NUM_DST(NELEM),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_INFO:FAM_NUM:ELEM'
          RETURN
        ENDIF
        FAM_NUM_DST(:) = 0

#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEM))
        CALL MMHFNR(SOURCE_FID,MNAME_SRC,KMED_NO_DT,KMED_NO_IT,
     &              KMED_CELL,TYPE_ELEM_MED,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NELEM
          FAM_NUM_DST(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHFNR(SOURCE_FID,MNAME_SRC,KMED_NO_DT,KMED_NO_IT,
     &              KMED_CELL,TYPE_ELEM_MED,FAM_NUM_DST,IERR)
#endif
        ! If the function crashed that means we do not have
        ! families on the elements
        IF(IERR.GE.0) THEN
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &           'TRANSFER_GROUP_INFO:MMHFNR:ELEM'
            RETURN
          ENDIF

#if defined (HAVE_MED64)
          ALLOCATE(TMP(NELEM))
          DO I=1,NELEM
            TMP(I) = FAM_NUM_DST(I)
          ENDDO
          KNELEM = NELEM
          CALL MMHFNW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,
     &                KMED_CELL,TYPE_ELEM_MED,KNELEM,TMP,KIERR)
          IERR = INT(KIERR)
          DEALLOCATE(TMP)
#else
          CALL MMHFNW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,
     &                KMED_CELL,TYPE_ELEM_MED,NELEM,FAM_NUM_DST,IERR)
#endif
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &           'TRANSFER_GROUP_INFO:MMHFNW:ELEM'
            RETURN
          ENDIF
        ELSE
          IERR = 0
        ENDIF
        DEALLOCATE(FAM_NUM_DST)
      ENDIF

      ! For nodes
      IF(TRANS_POINT.AND.HAS_FAM_ON_POINT) THEN
        CALL GET_MESH_NPOIN_MED(SOURCE_ID, TYP_ELEM, NPOIN, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:GET_MESH_NPOIN_MED'
          RETURN
        ENDIF
!       READ THE FAMILY NUMBER FOR EACH ELEMENT

        ALLOCATE(FAM_NUM_DST(NPOIN),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'ALLOCATING TRANSFER_GROUP_INFO:FAM_NUM_DST'
          RETURN
        ENDIF

#if defined (HAVE_MED64)
        ALLOCATE(TMP(NPOIN))
        CALL MMHFNR(SOURCE_FID,MNAME_SRC,KMED_NO_DT,KMED_NO_IT,
     &              KMED_NODE,KMED_NONE,TMP,KIERR)
        IERR = INT(KIERR)
        DO I=1,NPOIN
          FAM_NUM_DST(I) = INT(TMP(I))
        ENDDO
        DEALLOCATE(TMP)
#else
        CALL MMHFNR(SOURCE_FID,MNAME_SRC,KMED_NO_DT,KMED_NO_IT,
     &              KMED_NODE,KMED_NONE,FAM_NUM_DST,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_SRC)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:MMHFNR'
          RETURN
        ENDIF

#if defined (HAVE_MED64)
        ALLOCATE(TMP(NPOIN))
        DO I=1,NPOIN
          TMP(I) = FAM_NUM_DST(I)
        ENDDO
        KNPOIN = NPOIN
        CALL MMHFNW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,KNPOIN,TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        CALL MMHFNW(DEST_FID,MNAME_DST,KMED_NO_DT,KMED_NO_IT,KMED_NODE,
     &              KMED_NONE,NPOIN,FAM_NUM_DST,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID_DST)%FILE_NAME)//': '//
     &         'TRANSFER_GROUP_INFO:MMHFNW'
          RETURN
        ENDIF
        DEALLOCATE(FAM_NUM_DST)
      ENDIF
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_BND_MED(FILE_ID,TYPE_BND_ELT,NELEBD,NDP,IKLE_BND,
     &                       NPTFR,LIHBOR,LIUBOR,LIVBOR,LITBOR,IERR)
!
!BRIEF    WRITE DATA VALUES FOR A GIVEN VARIABLE
!         ONLY FOR VALUES WITH DOUBLE PRECISION, ONE COMPONENT AND VALUE
!         ON THE NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| MED FILE DESCRIPTOR
!| TYPE_BND_ELT   |<--| TYPE OF THE BOUNDARY ELEMENT
!| NELEBD         |<--| NUMBER OF BOUNDARY ELEMENT
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| IKLE           |<--| CONNECTIVITY TABLE OF THE BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,          INTENT(IN)  :: FILE_ID
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
      INTEGER,          INTENT(IN)  :: NELEBD
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: IKLE_BND(NELEBD*NDP)
      INTEGER,          INTENT(IN)  :: NPTFR
      INTEGER,          INTENT(IN)  :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER,          INTENT(IN)  :: LIHBOR(NPTFR),LITBOR(NPTFR)
      INTEGER,          INTENT(OUT) :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE)    :: MNAME
      INTEGER :: MY_ID
      INTEGER(KIND=KMED_INT) :: TYPE_BND_ELEM_MED
      CHARACTER(LEN=MED_LNAME_SIZE) GRP_NAME
      CHARACTER(LEN=MED_NAME_SIZE) FAM_NAME
      INTEGER :: NCLI,I,J,K,IPTFR
      INTEGER,ALLOCATABLE :: FAM_NUM(:)
      INTEGER, PARAMETER :: NVAL(3) = (/5,6,4/)
      INTEGER :: H_VAL(NVAL(1))
      INTEGER :: U_VAL(NVAL(2))
      INTEGER :: T_VAL(NVAL(3))
      INTEGER :: LIST_VAL(NVAL(1)*NVAL(2)*NVAL(3))
      INTEGER :: IFAM
      INTEGER(KIND=KID) :: FID
      LOGICAL, ALLOCATABLE :: WRITE_GRP(:)
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
      INTEGER(KIND=KMED_INT) :: KNELEBD, KNPTFR
#endif
      INTEGER(KIND=KMED_INT) :: IFAM2, NGRP
      INTEGER(KIND=KMED_INT) :: KIERR
      INTEGER :: MULTI
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'SET_BND_MED:GET_OBJ'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      NCLI = MY_OBJ_TAB(MY_ID)%NCLI
      FID = MY_OBJ_TAB(MY_ID)%ID
!
!     CONVERTS TYPE OF ELEMENTS
      CALL CONVERT_ELEM_TYPE(TYPE_BND_ELT,TYPE_BND_ELEM_MED,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &       'SET_BND_MED:CONVERT_ELEM_TYPE'
        RETURN
      ENDIF
!
      ! If we do not have families in the file creating somes
      IF(MY_OBJ_TAB(MY_ID)%NBND_GRP.EQ.0) THEN
!       WRITES CONNECTIVITY TABLE
#if defined (HAVE_MED64)
        ALLOCATE(TMP(NELEBD*NDP))
        DO I=1,NELEBD*NDP
          TMP(I) = IKLE_BND(I)
        ENDDO
        KNELEBD = NELEBD
        CALL MMHCYW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,0.0,KMED_CELL,
     &    TYPE_BND_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,KNELEBD,
     &    TMP,KIERR)
        IERR = INT(KIERR)
        DEALLOCATE(TMP)
#else
        CALL MMHCYW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,0.0,KMED_CELL,
     &    TYPE_BND_ELEM_MED,KMED_NODAL,KMED_NO_INTERLACE,NELEBD,
     &    IKLE_BND,IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'SET_BND_MED:MMHCYW'
          RETURN
        ENDIF
!
        ! Create all the types of boundary conditions
        H_VAL = (/ 1,4,5,2,8/)
        U_VAL = (/ 6,0,4,5,2,1/)
        T_VAL = (/ 4,5,2,1/)
        MY_OBJ_TAB(MY_ID)%NBND_GRP = NVAL(1)*NVAL(2)*NVAL(3)
        ALLOCATE(MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(
     &               MY_OBJ_TAB(MY_ID)%NBND_GRP,4), STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING MEDOBJ%BND_GRP_VAL'
          RETURN
        ENDIF
        ALLOCATE(MY_OBJ_TAB(MY_ID)%BND_GRP_NAME(
     &               MY_OBJ_TAB(MY_ID)%NBND_GRP), STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING MEDOBJ%GRP_NAME'
          RETURN
        ENDIF
        ALLOCATE(WRITE_GRP(MY_OBJ_TAB(MY_ID)%NBND_GRP), STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING WRITE_GRP'
          RETURN
        ENDIF
        WRITE_GRP = .FALSE.

        IFAM = 1
        DO I=1,NVAL(1)
          DO J=1,NVAL(2)
            DO K=1,NVAL(3)
              MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IFAM,1) = H_VAL(I)
              MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IFAM,2) = U_VAL(J)
              MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IFAM,3) = U_VAL(J)
              MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(IFAM,4) = T_VAL(K)
              LIST_VAL(IFAM) =
     &              H_VAL(I)*1000 + U_VAL(J)*100
     &              + U_VAL(J)*10 + T_VAL(K)
              GRP_NAME = REPEAT(' ',MED_LNAME_SIZE)
              GRP_NAME = 'CONLIM_'//TRIM(I2CHAR(H_VAL(I)))//
     &                   TRIM(I2CHAR(U_VAL(J)))//
     &                   TRIM(I2CHAR(U_VAL(J)))//
     &                   TRIM(I2CHAR(T_VAL(K)))
              MY_OBJ_TAB(MY_ID)%BND_GRP_NAME(IFAM) = GRP_NAME
              IFAM = IFAM + 1
            ENDDO
          ENDDO
        ENDDO
!
!       DEFINE FAMILY NUMBER
        IF(NPTFR.GT.0) THEN
          IF(TYPE_BND_ELT.EQ.POINT_BND_ELT_TYPE) THEN
            MULTI = 1
          ELSE
            MULTI = -1
          ENDIF
          ALLOCATE(FAM_NUM(NPTFR),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'ALLOCATING FAM_NUM'
            RETURN
          ENDIF
          FAM_NUM = 0
          DO IPTFR=1,NPTFR
            FAM_NUM(IPTFR) = 0
            DO J=1,MY_OBJ_TAB(MY_ID)%NBND_GRP
              IF(
     &      LIHBOR(IPTFR).EQ.MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(J,1).AND.
     &      LIUBOR(IPTFR).EQ.MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(J,2).AND.
     &      LIVBOR(IPTFR).EQ.MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(J,3).AND.
     &      LITBOR(IPTFR).EQ.MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(J,4))
     &        THEN
                FAM_NUM(IPTFR) = (J + OFFSET)*MULTI
                ! Counting families as used
                WRITE_GRP(J) = .TRUE.
                EXIT
              ENDIF
            ENDDO
            IF(FAM_NUM(IPTFR).EQ.0) THEN
              IERR = HERMES_UNKNOWN_BND_CONDITION
              ERROR_MESSAGE = 'ERROR IN '//
     &             TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &             'UNKNOW BOUNDARY CONDITION LI[HUVT]BOR : '//
     &             I2CHAR(LIHBOR(IPTFR))//' '//
     &             I2CHAR(LIUBOR(IPTFR))//' '//
     &             I2CHAR(LIVBOR(IPTFR))//' '//
     &             I2CHAR(LITBOR(IPTFR))
              RETURN
            ENDIF
          ENDDO

          ! Creating families for each boundary type used
          DO I=1,MY_OBJ_TAB(MY_ID)%NBND_GRP
            IF(WRITE_GRP(I)) THEN
              GRP_NAME = MY_OBJ_TAB(MY_ID)%BND_GRP_NAME(I)
              FAM_NAME = 'FAM_'//TRIM(GRP_NAME)
!             CREATING THE FAMILIES FOR EACH BOUNDARY TYPES
              IFAM2 = (I+OFFSET)*MULTI
              ! family number < 0 for elements
              NGRP = 1
              CALL MFACRE(FID,MNAME,FAM_NAME,IFAM2,NGRP,
     &                    GRP_NAME,KIERR)
              IERR = INT(KIERR)
              IF(IERR.NE.0) THEN
                ERROR_MESSAGE = 'ERROR IN '//
     &               TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &               'SET_BND_MED:MFACRE:'
                RETURN
              ENDIF
            ENDIF
          ENDDO
!
!         WRITE FAMILY NUMBER IN THE MED FILE
#if defined (HAVE_MED64)
          ALLOCATE(TMP(NPTFR))
          DO I=1,NPTFR
            TMP(I) = FAM_NUM(I)
          ENDDO
          KNPTFR=NPTFR
          CALL MMHFNW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &                TYPE_BND_ELEM_MED,KNPTFR,TMP,KIERR)
          IERR = INT(KIERR)
          DEALLOCATE(TMP)
#else
          CALL MMHFNW(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &                TYPE_BND_ELEM_MED,NPTFR,FAM_NUM,IERR)
#endif
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'FAM_NUM'
            RETURN
          ENDIF
          DEALLOCATE(FAM_NUM)
        ENDIF
!
      ENDIF

      !If the file is a concatenation, we need to move to the
      !begining of our part
      IF(PARTEL_CONCAT)THEN
        DO I=1,MY_OBJ_TAB(MY_ID)%CLI_LINE_BEGIN-1
          READ(MY_OBJ_TAB(MY_ID)%NCLI,*)
        ENDDO
      ENDIF
!     WRITING THE BOUNDARY FILE
      MY_OBJ_TAB(MY_ID)%NBND_USED_GRP = COUNT(WRITE_GRP)
      WRITE(NCLI,*) MY_OBJ_TAB(MY_ID)%NBND_USED_GRP
      DO I=1,MY_OBJ_TAB(MY_ID)%NBND_GRP
        IF(WRITE_GRP(I)) THEN
          WRITE(NCLI,*)
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,1),
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,2),
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,3),
     &               MY_OBJ_TAB(MY_ID)%BND_GRP_VAL(I,4),
     &               TRIM(MY_OBJ_TAB(MY_ID)%BND_GRP_NAME(I))
        ENDIF
      ENDDO

#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE UPDATE_DATA_MESH_MED (FILE_ID,TIME,RECORD,NB_DIM_MESH,
     &                                 NPOIN,COORD,IERR)
!
!BRIEF    UPDATE MESH COORDINATES
!         ONLY FOR MOVING MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| MED FILE DESCRIPTOR
!| TIME           |-->| PHYSICAL TIME
!| RECORD         |-->| ITERATION NUMBER
!| NB_DIM_MESH    |-->| DIMENSION OF THE MESH
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| COORD          |-->| COORDINATES TABLE
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)          :: FILE_ID
      INTEGER, INTENT(IN)          :: RECORD
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER, INTENT(IN)          :: NB_DIM_MESH, NPOIN
      DOUBLE PRECISION, INTENT(IN) :: COORD(NB_DIM_MESH*NPOIN)
      INTEGER, INTENT(OUT)         :: IERR
!
#if defined (HAVE_MED)
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME
      INTEGER :: MED_ID
      INTEGER(KIND=KID) :: FID
      INTEGER(KIND=KMED_INT)       :: KIERR, KRECORD, KNPOIN
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MED_ID,IERR)
      MNAME = MY_OBJ_TAB(MED_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MED_ID)%ID
!
!     UPDATE MESH COORDINATES IF NECESSARY
      KRECORD = RECORD
      KNPOIN = NPOIN
      CALL MMHCOW(FID,MNAME,KRECORD,KMED_NO_IT,TIME,
     &            KMED_NO_INTERLACE,KNPOIN,COORD,KIERR)
      IERR = INT(KIERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(MY_OBJ_TAB(MED_ID)%FILE_NAME)//': '//
     &       'UPDATE_DATA_MESH_MED:MMHCOW'
        RETURN
      ENDIF
!
#else
!
!     MED LIBRARY NOT LOADED
      IERR = HERMES_MED_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE IFVECTOR_(STRING,COMP_NUM,ISVECTOR)
!
!BRIEF    FIND ' U ' ' V ' ' W ' ' X ' ' Y ' ' Z ' IN NAME FIELD OF
!+        SCALAR OR VECTOR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| STRING         |<->| THE NAME OF THE VARIABLE TO TEST
!| COMP_NUM       |<--| DIRECTION OF VECTOR
!| ISVECTOR       |<--| TRUE IF VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CHARACTER(LEN=32), INTENT(INOUT) :: STRING
      INTEGER, INTENT(INOUT)             :: COMP_NUM
      LOGICAL, INTENT(INOUT)             :: ISVECTOR
!
      INTEGER :: J
!
!-----------------------------------------------------------------------
!
      ISVECTOR = .FALSE.
      COMP_NUM = 0
      RETURN

      IF (STRING(1:6)/='COTE Z') THEN
      DO J = 2,31
        IF (STRING(J-1:J+1) .EQ. ' U ') THEN
          STRING(J:J) = '*'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. ' V ') THEN
          STRING(J:J) = '*'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. ' W ') THEN
          STRING(J:J) = '*'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. ' X ') THEN
          STRING(J:J) = '*'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. ' Y ') THEN
          STRING(J:J) = '*'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. ' Z ') THEN
          STRING(J:J) = '*'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'QX ') THEN
          STRING(J-1:J) = 'Q*'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'QY ') THEN
          STRING(J-1:J) = 'Q*'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'QZ ') THEN
          STRING(J-1:J) = 'Q*'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'U0 ') THEN
          STRING(J-1:J) = '*0'
          COMP_NUM = 1
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'V0 ') THEN
          STRING(J-1:J) = '*0'
          COMP_NUM = 2
          ISVECTOR = .TRUE.
        ELSEIF (STRING(J-1:J+1) .EQ. 'W0 ') THEN
          STRING(J-1:J) = '*0'
          COMP_NUM = 3
          ISVECTOR = .TRUE.
        ENDIF
      ENDDO
      ENDIF
!
      RETURN
      END SUBROUTINE IFVECTOR_
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CONVERT_ELEM_TYPE(TYPE_ELEM,TYPE_ELEM_MED,IERR)
!
!BRIEF    CONVERTS ELEMENT TYPE FROM SLF FORMAT TO MED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_ELEM         |<--| ELEMENT TYPE WITH SLF FORMAT
!| TYPE_ELEM_MED     |-->| ELEMENT TYPE WITH MED FORMAT
!| IERR              |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTEGER, INTENT(IN)  :: TYPE_ELEM
      INTEGER(KIND=KMED_INT), INTENT(INOUT) :: TYPE_ELEM_MED
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
#if defined HAVE_MED
!     INITIALISATION
      IERR = 0
!
!     CONVERSION FROM BIEF NUMBERING TO MED
      SELECT CASE(TYPE_ELEM)
!
!       1 NODES OR 1 SEGMENT WITH 1 NODES
        CASE(POINT_BND_ELT_TYPE)
          TYPE_ELEM_MED = KMED_POINT1
!
!       SEGMENT WITH 2 NODES
        CASE(EDGE_BND_ELT_TYPE)
          TYPE_ELEM_MED = KMED_SEG2
!
!       TRIANGLE WITH 3 NODES
        CASE(TRIANGLE_ELT_TYPE, TRIANGLE_BND_ELT_TYPE,
     &       TRIANGLE_3D_BND_ELT_TYPE)
          TYPE_ELEM_MED = KMED_TRIA3
!
!       QUADRANGLE WITH 4 NODES
        CASE(QUADRANGLE_ELT_TYPE, QUADRANGLE_BND_ELT_TYPE)
          TYPE_ELEM_MED = KMED_QUAD4
!
!       TETRAHEDRA WITH 4 NODES
        CASE(TETRAHEDRON_ELT_TYPE)
          TYPE_ELEM_MED = KMED_TETRA4
!
!       PRISM WITH 6 NODES
        CASE(PRISM_ELT_TYPE, SPLIT_PRISM_ELT_TYPE)
          TYPE_ELEM_MED = KMED_PENTA6
!
!       ERROR CASE
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_ELEMENT_TYPE_ERR
!
      END SELECT
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE NDP_FROM_ELEMENT_TYPE_MED(TYPE_ELEM_MED,NDP,IERR)
!
!brief    DEDUCES NUMBER OF NODES FROM ELEMENT TYPE IN MED NUMBERING
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_ELEM_MED     |-->| ELEMENT TYPE WITH MED NUMBERING
!| NDP               |-->| NUMBER OF NODES PER ELEMENT
!| IERR              |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      INTEGER(KIND=KMED_INT), INTENT(IN)  :: TYPE_ELEM_MED
      INTEGER, INTENT(INOUT) :: NDP
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
!     INITIALISATION
      IERR = 0
      NDP  = -99
#if defined HAVE_MED
!
!     CONVERSION FROM BIEF NUMBERING TO MED
!
!     1D ELEMENTS
      IF (TYPE_ELEM_MED .EQ. KMED_POINT1) NDP = 1 ! SEGM WITH 1 NODES
      IF (TYPE_ELEM_MED .EQ. KMED_SEG2)   NDP = 2 ! SEGM WITH 2 NODES
!
!     2D ELEMENTS
      IF (TYPE_ELEM_MED .EQ. KMED_TRIA3)  NDP = 3 ! TRIA WITH 3 NODES
      IF (TYPE_ELEM_MED .EQ. KMED_QUAD4)  NDP = 4 ! QUAD WITH 4 NODES
!
!     3D ELEMENTS
      IF (TYPE_ELEM_MED .EQ. KMED_TETRA4) NDP = 4 ! TETRA WITH 4 NODES
      IF (TYPE_ELEM_MED .EQ. KMED_PENTA6) NDP = 6 ! PRISM WITH 6 NODES
!
!     ERROR IF UNKNOWN ELEMNT TYPE
      IF (NDP .EQ. -99) IERR = HERMES_UNKNOWN_ELEMENT_TYPE_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE IDENTIFY_BND_ELMT(FILE_ID,TYPE_BND_ELEM,IERR)
!
!brief    DEDUCES NUMBER OF NODES FROM ELEMENT TYPE IN MED NUMBERING
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| TYPE_BND_ELEM     |-->| ELEMENT TYPE WITH BIEF NUMBERING
!| FILE_ID           |-->| ID OF THE MED FILE
!| IERR              |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: TYPE_BND_ELEM
      INTEGER, INTENT(OUT) :: IERR
!
!-----------------------------------------------------------------------
!
#if defined HAVE_MED
      CHARACTER(LEN=MED_NAME_SIZE) :: MNAME, TEMP_FAM
      CHARACTER(LEN=MED_LNAME_SIZE),ALLOCATABLE :: GRP_NAME(:)
      INTEGER ::  INUM,I,MY_ID
      INTEGER(KIND=KMED_INT) :: IFAM,TYPE_ELEM_MED
      INTEGER :: J
      INTEGER :: NELEM,NB_FAMILY,NB_GRP
      INTEGER, ALLOCATABLE :: NUM_FAMILY(:)
      LOGICAL :: FOUND
      INTEGER(KIND=KID) :: FID
#if defined (HAVE_MED64)
      INTEGER(KIND=KMED_INT), ALLOCATABLE :: TMP(:)
#endif
      INTEGER(KIND=KMED_INT) :: KNFAM, KINUM, KNB_GRP
      INTEGER(KIND=KMED_INT) :: KIERR
!
!-----------------------------------------------------------------------
!
!     GET INFO FROM THE MED FILE OBJECT
      CALL GET_OBJ(HASH,FILE_ID,MY_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'IDENTIFY_BND_ELMT:GET_OBJ_FILE'
        RETURN
      ENDIF
      MNAME = MY_OBJ_TAB(MY_ID)%MESH_NAME
      FID = MY_OBJ_TAB(MY_ID)%ID
!     Only computing the array if it was not done before
      IF(.NOT.ALLOCATED(MY_OBJ_TAB(MY_ID)%IS_BND)) THEN
!       CONVERTS TYPE OF ELEMENTS
        CALL CONVERT_ELEM_TYPE(TYPE_BND_ELEM, TYPE_ELEM_MED, IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'IDENTIFY_BND_ELMT:CONVERT_ELEM_TYPE'
          RETURN
        ENDIF
!
        CALL GET_MESH_NELEM_MED(FILE_ID,TYPE_BND_ELEM,NELEM,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'IDENTIFY_BND_ELMT:GET_BND_NELEM_MED'
          RETURN
        ENDIF
        ! In case we do not have boundary element in the mesh
        ALLOCATE(MY_OBJ_TAB(MY_ID)%IS_BND(MAX(NELEM,1)),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING IDENTIFY_BND_ELMT:IS_BND'
          RETURN
        ENDIF

        IF(NELEM.NE.0) THEN
!
!         READ THE FAMILY NUMBER FOR EACH ELEMENT
          MY_OBJ_TAB(MY_ID)%NO_BND = .FALSE.
          ALLOCATE(NUM_FAMILY(NELEM),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'IDENTIFY_BND_ELMT:NUM_FAMILY'
            RETURN
          ENDIF
!
#if defined (HAVE_MED64)
          ALLOCATE(TMP(NELEM))
          CALL MMHFNR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &                TYPE_ELEM_MED,TMP,KIERR)
          IERR = INT(KIERR)
          DO I=1,NELEM
            NUM_FAMILY(I) = INT(TMP(I))
          ENDDO
          DEALLOCATE(TMP)
#else
          CALL MMHFNR(FID,MNAME,KMED_NO_DT,KMED_NO_IT,KMED_CELL,
     &                TYPE_ELEM_MED,NUM_FAMILY,IERR)
#endif
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'IDENTIFY_BND_ELMT:MMHFNR'
            RETURN
          ENDIF
        ELSE
          MY_OBJ_TAB(MY_ID)%NO_BND = .TRUE.
          MY_OBJ_TAB(MY_ID)%IS_BND(:) = .FALSE.
          RETURN
        ENDIF
!
!       READ THE TOTAL NUMBER OF FAMILY
        CALL MFANFA(FID,MNAME,KNFAM,KIERR)
        IERR = INT(KIERR)
        NB_FAMILY = INT(KNFAM)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'IDENTIFY_BND_ELMT:MFANFA'
          RETURN
        ENDIF
        ALLOCATE(MY_OBJ_TAB(MY_ID)%BND_FAM(NB_FAMILY,2),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &         'ALLOCATING BND_FAM'
          RETURN
        ENDIF
!
        DO IFAM = 1, NB_FAMILY
!
!         READ THE NUMBER OF GROUP PER FAMILY
          CALL MFANFG(FID,MNAME,IFAM,KNB_GRP,KIERR)
          IERR = INT(KIERR)
          NB_GRP = INT(KNB_GRP)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'IDENTIFY_BND_ELMT:MFANFG'
            RETURN
          ENDIF
!
          ALLOCATE(GRP_NAME(MAX(NB_GRP,1)),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'ALLOCATING IDENTIFY_BND_ELMT:GRP_NAME'
            RETURN
          ENDIF

!         READ THE TYPE OF FAMILY
          CALL MFAFAI(FID,MNAME,IFAM,TEMP_FAM,KINUM,GRP_NAME,KIERR)
          IERR = INT(KIERR)
          INUM = INT(KINUM)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(MY_OBJ_TAB(MY_ID)%FILE_NAME)//': '//
     &           'IDENTIFY_BND_ELMT:MFAFAI'
            RETURN
          ENDIF
          MY_OBJ_TAB(MY_ID)%BND_FAM(INT(IFAM),1) = INUM
          MY_OBJ_TAB(MY_ID)%BND_FAM(INT(IFAM),2) = 0
          IF(TYPE_BND_ELEM.EQ.POINT_BND_ELT_TYPE) THEN
            ! Only check for families on points i.e. num > 0
            IF(INUM.LE.0) THEN
              DEALLOCATE(GRP_NAME)
              CYCLE
            ENDIF
          ELSE
            ! Only check for families on elements i.e. num < 0
            IF(INUM.GE.0) THEN
              DEALLOCATE(GRP_NAME)
              CYCLE
            ENDIF
          ENDIF
          FOUND = .FALSE.
          DO J=1,MY_OBJ_TAB(MY_ID)%NBND_GRP
            IF(FOUND) EXIT
            ! looping on the family groups and if one is in bnd_grp
            ! adding it to bnd_fam
            IF(NB_GRP.EQ.0) EXIT
            DO I=1,NB_GRP
              IF(GRP_NAME(I).EQ.
     &           MY_OBJ_TAB(MY_ID)%BND_GRP_NAME(J)) THEN
                FOUND = .TRUE.
                MY_OBJ_TAB(MY_ID)%BND_FAM(INT(IFAM),2) = J
                EXIT
              ENDIF
            ENDDO
          ENDDO
          DEALLOCATE(GRP_NAME)
        ENDDO
        IF(NELEM.NE.0) THEN
          ! Looping on all element and checking if their family is in
          ! bnd_fam
          DO I=1,NELEM
            MY_OBJ_TAB(MY_ID)%IS_BND(I) = .FALSE.
            DO IFAM=1,NB_FAMILY
              IF(NUM_FAMILY(I).EQ.
     &           MY_OBJ_TAB(MY_ID)%BND_FAM(INT(IFAM),1)) THEN
                MY_OBJ_TAB(MY_ID)%IS_BND(I) =
     &                MY_OBJ_TAB(MY_ID)%BND_FAM(INT(IFAM),2).NE.0
                EXIT
              ENDIF
            ENDDO
          ENDDO
          DEALLOCATE(NUM_FAMILY)
        ENDIF
      ENDIF
!
#endif
      RETURN
      END SUBROUTINE

      SUBROUTINE SEG2POINT(P1,P2,BND_TYP_P1,BND_TYP_P2,POINT)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: P1,P2
      INTEGER, INTENT(IN) :: BND_TYP_P1
      INTEGER, INTENT(IN) :: BND_TYP_P2
      INTEGER, INTENT(OUT):: POINT

      IF (BND_TYP_P1.EQ.SOL_BND.AND.
     &    BND_TYP_P2.EQ.SOL_BND) THEN
        POINT = P1
      ELSE IF(BND_TYP_P1.EQ.SOL_BND) THEN
        POINT = P2
      ELSE IF(BND_TYP_P2.EQ.SOL_BND) THEN
        POINT = P1
      ELSE
        IF(BND_TYP_P1.LT.BND_TYP_P2) THEN
          POINT = P1
        ELSE
          POINT = P2
        ENDIF
      ENDIF
      END SUBROUTINE

      SUBROUTINE POINT2SEG(P1,P2,BND_TYP_P1,BND_TYP_P2,SEG)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: P1,P2
      INTEGER, INTENT(IN) :: BND_TYP_P1
      INTEGER, INTENT(IN) :: BND_TYP_P2
      INTEGER, INTENT(OUT):: SEG

      IF(BND_TYP_P1.NE.SOL_BND .AND.
     &   BND_TYP_P2.NE.SOL_BND) THEN
        IF(BND_TYP_P1.LT.BND_TYP_P2) THEN
          SEG = P1
        ELSE
          SEG = P2
        ENDIF
      ELSE IF (BND_TYP_P1.EQ.SOL_BND) THEN
        SEG = P1
      ELSE
        SEG = P2
      ENDIF
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      END MODULE UTILS_MED
