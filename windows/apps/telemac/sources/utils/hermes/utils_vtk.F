!                 ************
                  MODULE UTILS_VTK
!                 ************
!
!***********************************************************************
! HERMES  V7P0                                                 2015
!***********************************************************************
!
!brief    INTERFACES OF VTK PUBLIC SUBROUTINES
!
!history YOANN AUDOUIN
!+       13/11/2016
!+       V7PR2
!+       First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE HASH_TABLE
      USE LIB_VTK_IO
!
      IMPLICIT NONE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NEW OBJECT RELATED TO MESHES IN A VTK FILE
      TYPE VTK_INFO
        INTEGER :: FILE_ID
        CHARACTER(LEN=MAXLENFILE) :: FILE_NAME
        CHARACTER(LEN=80) :: TITLE
        INTEGER, ALLOCATABLE :: CELL_TYPE(:)
        INTEGER, ALLOCATABLE :: CONNECT(:)
        INTEGER :: NELEM, NPOIN
        INTEGER :: IVAR, NVAR
        INTEGER :: MESH_DIM
        DOUBLE PRECISION, ALLOCATABLE :: X(:)
        DOUBLE PRECISION, ALLOCATABLE :: Y(:)
        DOUBLE PRECISION, ALLOCATABLE :: Z(:)
      END TYPE VTK_INFO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTEGER :: NFILES = 0
!
!     HASH TABLE FOR FILES
      INTEGER :: HASH(MAX_FILE) = 0
!
      TYPE(VTK_INFO) :: VTK_OBJ_TAB(MAX_FILE)
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!
!
      ! CONVERT AN INT INTO A STRING WITH ZEROS
      SUBROUTINE NUMBER2STRING(RES,VAL)
!
!brief    Fill a 00000000 string with val
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| RES         |-->| OUTPUT STRING
!| VAL         |<--| INTEGER TO CONVERT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: VAL
!
      CHARACTER(LEN=8), INTENT(OUT) :: RES
!
      IF(VAL.LT.10) THEN
        WRITE(RES,'(A7,I1)') '0000000',VAL
      ELSE IF(VAL.LT.100) THEN
        WRITE(RES,'(A6,I2)') '000000',VAL
      ELSE IF(VAL.LT.1000) THEN
        WRITE(RES,'(A5,I3)') '00000',VAL
      ELSE IF(VAL.LT.10000) THEN
        WRITE(RES,'(A4,I4)') '0000',VAL
      ELSE IF(VAL.LT.100000) THEN
        WRITE(RES,'(A3,I5)') '000',VAL
      ELSE IF(VAL.LT.1000000) THEN
        WRITE(RES,'(A2,I6)') '00',VAL
      ELSE IF(VAL.LT.10000000) THEN
        WRITE(RES,'(A1,I7)') '0',VAL
      ELSE
        WRITE(RES,'(I8)') VAL
      ENDIF
      END SUBROUTINE NUMBER2STRING
!
!-----------------------------------------------------------------------
!
      SUBROUTINE OPEN_MESH_VTK(FILE_NAME,FILE_ID,OPEN_MODE,IERR)
!
!BRIEF    OPENS A MESH FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |<--| NAME OF THE VTK FILE
!| FILE_ID        |<--| VTK FILE DESCRIPTOR
!| OPEN_MODE      |<--| OPENING MODE (READ/READWRITE/WRITE)
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*), INTENT(IN)  :: FILE_NAME
      INTEGER,          INTENT(OUT)  :: FILE_ID
      CHARACTER(LEN=9), INTENT(IN)  :: OPEN_MODE
      INTEGER,          INTENT(OUT) :: IERR
!
#if !defined (DISABLE_VTK)
      INTEGER :: VTK_ID
!
!-----------------------------------------------------------------------
!
      SELECT CASE(OPEN_MODE)
        CASE('READ     ')
          IERR = HERMES_WRITE_ONLY_ERR
        CASE('READWRITE')
        CASE('WRITE    ')
        CASE DEFAULT
!         OPEN MODE IS INVALID
          IERR = HERMES_INVALID_OPEN_MODE_ERR
      END SELECT
!
      CALL GET_FREE_ID(FILE_ID)
!     CREATE A NEW OBJECT FOR THE NEW VTK FILE
      CALL ADD_OBJ(HASH,FILE_ID,VTK_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(FILE_NAME)//': '//
     &       'OPEN_MESH_MED:ADD_OBJ'
        RETURN
      ENDIF
!
      VTK_OBJ_TAB(VTK_ID)%FILE_NAME = FILE_NAME
!     Opening bogus file to secure the id
      OPEN(UNIT=FILE_ID,FILE='bogus.txt',
     &     ACTION='WRITE',IOSTAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &       'OPEN_MESH_VTK:BOGUS'
        RETURN
      ENDIF
!
#else
!
!     VTK LIBRARY NOT LOADED
      IERR = HERMES_VTK_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CLOSE_MESH_VTK (FILE_ID, IERR)
!
!BRIEF    CLOSES A MESH FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| VTK FILE DESCRIPTOR
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(OUT) :: IERR
!
      INTEGER VTK_ID
!
!-----------------------------------------------------------------------
!
#if !defined (DISABLE_VTK)
!
      CALL GET_OBJ(HASH,FILE_ID,VTK_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'CLOSE_MESH_VTK:GET_OBJ'
        RETURN
      ENDIF
!
      ! Clearing id in the hash table
      HASH(VTK_ID) = 0
      NFILES = NFILES - 1
!
      ! CLEAN UP OF STRUCTURE
      DEALLOCATE(VTK_OBJ_TAB(VTK_ID)%CONNECT)
      DEALLOCATE(VTK_OBJ_TAB(VTK_ID)%CELL_TYPE)
      DEALLOCATE(VTK_OBJ_TAB(VTK_ID)%X)
      DEALLOCATE(VTK_OBJ_TAB(VTK_ID)%Y)
      CLOSE(FILE_ID)
!
#else
!
!     VTK LIBRARY NOT LOADED
      IERR = HERMES_VTK_NOT_LOADED_ERR
!
#endif
!
      RETURN
      END SUBROUTINE
!
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_HEADER_VTK(FILE_ID,TITLE,NVAR,IERR)
!
!BRIEF    WRITES THE TITLE OF THE MESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| VTK FILE DESCRIPTOR
!| TITLE          |<--| TITLE OF THE MESH
!| NVAR           |<--| Number of variable
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)  :: FILE_ID
      INTEGER, INTENT(IN)  :: NVAR
      CHARACTER(LEN=80), INTENT(IN)  :: TITLE
      INTEGER, INTENT(OUT) :: IERR
!
#if !defined (DISABLE_VTK)
      INTEGER :: VTK_ID
!
!-----------------------------------------------------------------------
!
      CALL GET_OBJ(HASH,FILE_ID,VTK_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'SET_HEADER_VTK:GET_OBJ'
        RETURN
      ENDIF
!
      VTK_OBJ_TAB(VTK_ID)%TITLE = TITLE
      VTK_OBJ_TAB(VTK_ID)%NVAR = NVAR
!
#else
!
!     VTK LIBRARY NOT LOADED
      IERR = HERMES_VTK_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_MESH_VTK(FILE_ID,NB_DIM_MESH,TYPE_ELEM,
     &  NDP,NELEM,NPOIN,IKLE,X,Y,IERR)
!
!BRIEF    WRITES A MESH IN A VTK FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| VTK FILE DESCRIPTOR
!| NB_DIM_PB      |<--| NUMBER OF DIMENSION OF THE PHYSICAL DOMAIN
!| NB_DIM_MESH    |<--| NUMBER OF DIMENSION OF THE MESH
!| TYPE_ELEM      |<--| TYPE OF THE ELEMENT IN SLF FORMAT
!| NDP            |<--| NUMBER OF POINTS PER ELEMENT
!| NPTFR          |<--| NUMBER OF BOUNDARY NODES
!| NPTIR          |<--| NUMBER OF INTERFACE NODES FOR THE SUB-DOMAIN
!| NELEM          |<--| TOTAL NUMBER OF ELEMENTS
!| NPOIN          |<--| TOTAL NUMBER OF NODES
!| IKLE           |<--| CONNECTIVITY TABLE
!| IPOBO          |<--| LOGICAL TABLE FOR BOUNDARY NODES
!| KNOLG          |<--| NODES INDEX TABLE FROM LOCAL TO GLOBAL
!| X              |<--| X COORDINATES OF THE NODES
!| Y              |<--| Y COORDINATES OF THE NODES
!| NPLAN          |<--| NUMBER OF PLANES
!| IERR           |-->| ERROR TAG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: FILE_ID, NB_DIM_MESH
      INTEGER, INTENT(IN) :: NELEM, NPOIN, TYPE_ELEM, NDP
      INTEGER, INTENT(IN) :: IKLE(NDP*NELEM)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: Y(NPOIN)
      INTEGER, INTENT(OUT) :: IERR
!
#if !defined (DISABLE_VTK)
!
      INTEGER :: VTK_ID
      INTEGER :: I,J
!
!-----------------------------------------------------------------------
!
      CALL GET_OBJ(HASH,FILE_ID,VTK_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'SET_MESH_VTK:GET_OBJ'
        RETURN
      ENDIF
!
      ! DEFINING THE CELLS TYPE
      ALLOCATE(VTK_OBJ_TAB(VTK_ID)%CELL_TYPE(NELEM),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &       'ALLOCATING CELL_TYPE'
        RETURN
      ENDIF
      ! VALUE CHOSEN IN PDF VTK FILE FORMAT (GOOGLE)
      SELECT CASE(TYPE_ELEM)
      CASE(TRIANGLE_ELT_TYPE)
        VTK_OBJ_TAB(VTK_ID)%CELL_TYPE(:) = 5
      CASE(QUADRANGLE_ELT_TYPE)
        VTK_OBJ_TAB(VTK_ID)%CELL_TYPE(:) = 9
      CASE(TETRAHEDRON_ELT_TYPE)
        VTK_OBJ_TAB(VTK_ID)%CELL_TYPE(:) = 10
      CASE(PRISM_ELT_TYPE)
        VTK_OBJ_TAB(VTK_ID)%CELL_TYPE(:) = 13
      END SELECT
      ! BUILDING THE CONNECTION TABLE FOLLOWING THE VTK STANDARD
      ! FOR EACH CELL :
      !   FIRST THE NUMBER OF NODES COMPOSING THE CELL
      !   THEN THE NUMBER OF EACH NODE
      ! WE NEED TO RENUMBER TO NODES 0 TO NELEM-1
      ALLOCATE(VTK_OBJ_TAB(VTK_ID)%CONNECT(NELEM*(NDP+1)),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &       'ALLOCATING CONNECT'
        RETURN
      ENDIF
      DO I=1,NELEM
        VTK_OBJ_TAB(VTK_ID)%CONNECT((I-1)*(NDP+1)+1) = NDP
        DO J=1,NDP
          VTK_OBJ_TAB(VTK_ID)%CONNECT((I-1)*(NDP+1)+J+1) =
     &            IKLE((J-1)*NELEM+I)-1
        ENDDO
      ENDDO
      VTK_OBJ_TAB(VTK_ID)%NELEM = NELEM
      VTK_OBJ_TAB(VTK_ID)%NPOIN = NPOIN

      ALLOCATE(VTK_OBJ_TAB(VTK_ID)%X(NPOIN),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &       'ALLOCATING X'
        RETURN
      ENDIF
      VTK_OBJ_TAB(VTK_ID)%X = X
      ALLOCATE(VTK_OBJ_TAB(VTK_ID)%Y(NPOIN),STAT=IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &       'ALLOCATING Y'
        RETURN
      ENDIF
      VTK_OBJ_TAB(VTK_ID)%Y = Y

      VTK_OBJ_TAB(VTK_ID)%MESH_DIM = NB_DIM_MESH
!
#endif
!
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ADD_DATA_VTK (FILE_ID, VAR_NAME, RECORD,
     &  FIRST_VAR, VAR_VALUE, N, IERR)
!
!BRIEF    WRITE DATA VALUES FOR A GIVEN VARIABLE
!         ONLY FOR VALUES WITH DOUBLE PRECISION, ONE COMPONENT AND VALUE
!         ON THE NODES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |<--| VTK FILE DESCRIPTOR
!| VAR_NAME       |<--| NAME OF THE DATA
!| RECORD         |<--| ITERATION NUMBER
!| FIRST_VAR      |<--| TRUE IF FIRST VARIABLE
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
      DOUBLE PRECISION, INTENT(IN) :: VAR_VALUE(N)
      LOGICAL, INTENT(IN) :: FIRST_VAR
      INTEGER, INTENT(OUT) :: IERR
!
#if !defined (DISABLE_VTK)
      INTEGER :: VTK_ID
      DOUBLE PRECISION, ALLOCATABLE :: Z(:)
      CHARACTER(LEN=MAXLENFILE) :: FILENAME
      CHARACTER(LEN=8) :: TIMESTR
      CHARACTER(LEN=32) :: VAR_NAME_VTK
!
!-----------------------------------------------------------------------
!
      CALL GET_OBJ(HASH,FILE_ID,VTK_ID,IERR)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &       'ADD_DATA:GET_OBJ'
        RETURN
      ENDIF
!
      IF(FIRST_VAR) THEN
        FILENAME = REPEAT(' ',MAXLENFILE)
        CALL NUMBER2STRING(TIMESTR,RECORD)
        FILENAME = TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//TIMESTR
        ! INITIALISING THE VTK FILE
        IERR=VTK_INI('ASCII',FILENAME,VTK_OBJ_TAB(VTK_ID)%TITLE,
     &               'UNSTRUCTURED_GRID')
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &         'ADD_DATA:VTK_INI'
          RETURN
        ENDIF
        ! WRITTING THE NODES COORDINATES
        ALLOCATE(Z(VTK_OBJ_TAB(VTK_ID)%NPOIN),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &         'ALLOCATING Z'
          RETURN
        ENDIF
        IF(VTK_OBJ_TAB(VTK_ID)%MESH_DIM.EQ.3) THEN
          Z = VAR_VALUE
        ELSE
          Z = 0.D0
        ENDIF
        IERR=VTK_GEO(VTK_OBJ_TAB(VTK_ID)%NPOIN,
     &               VTK_OBJ_TAB(VTK_ID)%X,
     &               VTK_OBJ_TAB(VTK_ID)%Y,
     &               Z)
        DEALLOCATE(Z)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &         'ADD_DATA:VTK_GEO'
          RETURN
        ENDIF
        IERR=VTK_CON(VTK_OBJ_TAB(VTK_ID)%NELEM,
     &               VTK_OBJ_TAB(VTK_ID)%CONNECT,
     &               VTK_OBJ_TAB(VTK_ID)%CELL_TYPE)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &         'ADD_DATA:VTK_CON'
          RETURN
        ENDIF
        ! ADDING THE NUMBER OF NODES ON WHICH THERE IS GONNA BE A
        ! VARAIBLE HERE EVERY ONE
        IERR=VTK_DAT(VTK_OBJ_TAB(VTK_ID)%NPOIN,'node')
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &         'ADD_DATA:VTK_DAT'
          RETURN
        ENDIF
        ! Initializing variable counter
        VTK_OBJ_TAB(VTK_ID)%IVAR = 0
      ENDIF

      ! REMOVE WHITE SPACES IN VAR NAMES
      ! TO BE READABLE IN PARAVIEW 5.0
      VTK_OBJ_TAB(VTK_ID)%IVAR = VTK_OBJ_TAB(VTK_ID)%IVAR + 1
      VAR_NAME_VTK = VAR_NAME
      CALL BLANC2USCORE(VAR_NAME_VTK, LEN(VAR_NAME_VTK))
      IERR=VTK_VAR(VTK_OBJ_TAB(VTK_ID)%NPOIN, VAR_NAME_VTK, VAR_VALUE)
      IF(IERR.NE.0) THEN
        ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &       'ADD_DATA:VTK_VAR '
        RETURN
      ENDIF

      ! Closing the file after the last varialbe was written
      IF(VTK_OBJ_TAB(VTK_ID)%IVAR.EQ.VTK_OBJ_TAB(VTK_ID)%NVAR) THEN
!       CLOSE VTK FILE
        ! CLOSING THE FILE
        IERR=VTK_END()
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(VTK_OBJ_TAB(VTK_ID)%FILE_NAME)//': '//
     &         'ADD_DATA:VTK_END'
          RETURN
        ENDIF
      ENDIF
!
#else
!
!     VTK LIBRARY NOT LOADED
      IERR = HERMES_VTK_NOT_LOADED_ERR
!
#endif
      RETURN
      END SUBROUTINE
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      END MODULE UTILS_VTK
