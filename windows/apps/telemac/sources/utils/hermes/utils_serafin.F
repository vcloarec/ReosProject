!                 ********************
                  MODULE UTILS_SERAFIN
!                 ********************
!
!***********************************************************************
! HERMES  V7P1
!***********************************************************************
!
!brief    a number of subroutines dedicated to the serafin format.
!
!history YOANN AUDOUIN
!+       29/10/2011
!+       V7P1
!+       Creation of the file
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+       12/05/2015
!+       V7P1
!+       Correcting an old mistake on serafin files in prisms when the
!+       computation is done with tetrahedra. See SET_MESH_SRF.
!
!history S.E. BOURBAN (HRW)
!+       11/11/2016
!+       V7P2
!+       Replacement of K4 (which is compiler depend and therefore not
!+       necessary equals to 4) by KS in the SRF_INFO structure.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE HASH_TABLE
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: VAR_SIZE = 32 ! SIZE OF A VARIABLE TEXT
      INTEGER, PARAMETER :: TITLE_SIZE = 80 ! SIZE OF A TITLE
!
      TYPE SRF_INFO
        CHARACTER(LEN=250) :: FILE_NAME
        ! SIZE OF ELEMENTS
        INTEGER :: KS ! INTEGER SIZE (4 OR 8)
        INTEGER :: RS ! REAL SIZE (4 OR 8)
        ! POSITION IN FILE
        INTEGER(KIND=K8) :: POS_TITLE
        INTEGER(KIND=K8) :: POS_NVAR != POS_TITLE + 4 + TITLE_SIZE + 4
        INTEGER(KIND=K8) :: POS_VARINFO != POS_NVAR + 4 + 2*K4 + 4
        INTEGER(KIND=K8) :: POS_IB != POS_VARINFO + 4 + NVAR*VAR_SIZE + 4
        INTEGER(KIND=K8) :: POS_DATE != POS_IB + 4 + 10*K4 + 4
        INTEGER(KIND=K8) :: POS_NUM != POS_DATE + (IB(10).NE.0)*(4 + 6*K4 + 4)
        INTEGER(KIND=K8) :: POS_IKLE != POS_NUM + 4 + 4*K4 + 4
        INTEGER(KIND=K8) :: POS_IPOBO != POS_IKLE + 4 + NELEM*NDP*K4 + 4
        INTEGER(KIND=K8) :: POS_COORD != POS_IPOBO + 4 + NPOIN*K4 + 4
        INTEGER(KIND=K8) :: POS_DATA != POS_COORD + (4 + NPOIN*RS + 4)*NDIM
        ! COMPUTED INFORMATIONS
        INTEGER :: SIZE_DATA != 4 + NPOIN*RS + 4
        INTEGER :: SIZE_DATA_SET != 4 + RS + 4 + NVAR*(4 + NPOIN*RS + 4)
        ! STOCKED QUANTITIES AND SMALL VARIABLES
        INTEGER :: NTIMESTEP
        INTEGER :: NPOIN
        INTEGER :: NVAR
        INTEGER :: NELEM
        INTEGER :: NDP
        INTEGER :: NPLAN
        INTEGER :: NPTIR
        INTEGER :: NDIM
        INTEGER :: TYP_ELT
        CHARACTER(LEN=VAR_SIZE),ALLOCATABLE :: VAR_LIST(:)
        ! BOUNDARY INFORMATIONS
        INTEGER :: TYP_BND_ELT
        INTEGER :: NPTFR
        INTEGER :: NCLI
        ! COORDINATES OFFSET
        INTEGER :: X_ORIG
        INTEGER :: Y_ORIG
        !FILE ID OF THE INDEX FILE
        INTEGER :: MESH_IDX_ID
        INTEGER :: CLI_IDX_ID
        !LINE NUMBER TO OUR PART IN THE CONCATENATED CLI FILE
        INTEGER :: CLI_LINE_BEGIN
        INTEGER :: CLI_LINE_END
        ! ENDIAN of the file
        CHARACTER(LEN=13) :: ENDIAN
      END TYPE SRF_INFO

      ! HASH TABLE FOR SERAFIN FILES
      INTEGER :: HASH(MAX_FILE) = 0
      TYPE(SRF_INFO) :: SRF_OBJ_TAB(MAX_FILE)
!
!-----------------------------------------------------------------------
!
      CONTAINS
!***********************************************************************
      SUBROUTINE IDENTIFY_TYP_ELT
!***********************************************************************
!
     &(NDP,NDIM,TYP_ELT)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of point per element and dimension
!+        for the given element type
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NDP            |<--| NUMBER OF POINT PER ELEMENT
!| NDIM           |-->| DIMENSION OF THE ELEMENT (2D OR 1D)
!| TYP_ELT        |-->| TYPE OF THE ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !TODO: Merge with the one in special
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN)  :: NDP
        INTEGER, INTENT(IN)  :: NDIM
        INTEGER, INTENT(OUT) :: TYP_ELT
        !
        TYP_ELT = 0
        ! Returns the element type by checking the couple
        ! (dimension, number of point per element)
        IF(NDIM.EQ.3) THEN
          IF(NDP.EQ.4) TYP_ELT = TETRAHEDRON_ELT_TYPE
          IF(NDP.EQ.6) TYP_ELT = PRISM_ELT_TYPE
        ELSE
          IF(NDP.EQ.3) TYP_ELT = TRIANGLE_ELT_TYPE
          IF(NDP.EQ.4) TYP_ELT = QUADRANGLE_ELT_TYPE
        ENDIF
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE IDENTIFY_ENDIAN_TYPE
!***********************************************************************
!
     &(FILE_NAME,ENDIAN,FILE_ID,IERR)
!
!***********************************************************************
! HERMES   V7P2
!***********************************************************************
!
!brief    Read the first tag of a Serafin file to detect if it is in
!+        little or big Endian
!
!history  Y AUDOUIN (LNHE)
!+        11/05/2016
!+        V7P2
!+    First version.
!
!history  S.E. BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+        Compiler specific directive added because not all compilers
!+        support OPEN( CONVERT= )
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        08/07/2016
!+        V7P2
!+    Adding FILE_ID in the list of arguments.
!
!history S.E. BOURBAN (HRW)
!+       11/11/2016
!+       V7P2
!+       ENDIAN has now an INOUT intent to add flexibility when reading
!+       mixed binaries from various files.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |<--| Name of the file
!| ENDIAN         |<->| Encoding of integer and real in the file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
!
        INTEGER, INTENT(IN)           :: FILE_ID
        CHARACTER(LEN=*), INTENT(IN)  :: FILE_NAME
        CHARACTER(LEN=13),INTENT(INOUT) :: ENDIAN
        INTEGER, INTENT(OUT) :: IERR
!
        INTEGER(KIND=K4) :: I
        LOGICAL :: FILE_EXIST
!
        ! IF THE FILE DOES NOT EXIST RETURN BIG OR LITTLE ENDIAN
        IERR = 0
        INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXIST)
        IF(.NOT.FILE_EXIST) RETURN
!
#if defined NO_CONVERT_ENDIAN
!       CONVERT= NOT IMPLEMENTED WITH SOME COMPILERS
        OPEN(FILE=FILE_NAME, ACTION='READ', UNIT=FILE_ID,
     &       FORM='UNFORMATTED', ACCESS='STREAM', IOSTAT=IERR)
#else
        OPEN(FILE=FILE_NAME, ACTION='READ', UNIT=FILE_ID,
     &       FORM='UNFORMATTED', ACCESS='STREAM',CONVERT=ENDIAN,
     &       IOSTAT=IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(FILE_NAME)//': '//
     &         'IDENTIFY_ENDIAN_TYPE:OPEN:'//ENDIAN
          RETURN
        ENDIF
!
        READ(FILE_ID,POS=1,IOSTAT=IERR) I
        IF(IERR.LT.0) THEN
          ! WE'VE REACHED THE END OF THE FILE I.E. THE FILE IS EMPTY
          CLOSE(FILE_ID)
          RETURN
        ENDIF
        IF(I.EQ.80) THEN
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(FILE_NAME)//': '//
     &           'IDENTIFY_ENDIAN_TYPE:READ:'//ENDIAN
            RETURN
          ENDIF
          CLOSE(FILE_ID)
        ELSE
#if defined NO_CONVERT_ENDIAN
!       IF YOU DO NOT HAVE THE CONVERT OPTION AND YOU WERE NOT ABLE
!         TO OPEN THE FILE, THEN THE FILE IS NOT OF THE RIGHT FORMAT.
          IERR = HERMES_INVALID_SERAFIN_FILE
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(FILE_NAME)//': '//
     &           'IDENTIFY_ENDIAN_TYPE: Wrong endian of the file'
            RETURN
          ENDIF
          CLOSE(FILE_ID)
#else
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(FILE_NAME)//': '//
     &           'IDENTIFY_ENDIAN_TYPE:READ:'//ENDIAN
            RETURN
          ENDIF
          CLOSE(FILE_ID)
          IF( ENDIAN.EQ.'LITTLE_ENDIAN' ) THEN
            ENDIAN = 'BIG_ENDIAN   '
          ELSE
            ENDIAN = 'LITTLE_ENDIAN'
          ENDIF
          OPEN(FILE=FILE_NAME, ACTION='READ', UNIT=FILE_ID,
     &         FORM='UNFORMATTED', ACCESS='STREAM',CONVERT=ENDIAN,
     &         IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(FILE_NAME)//': '//
     &           'IDENTIFY_ENDIAN_TYPE:OPEN:'//ENDIAN
            RETURN
          ENDIF
          READ(FILE_ID,POS=1,IOSTAT=IERR) I
          IF(I.EQ.80) THEN
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &             TRIM(FILE_NAME)//': '//
     &             'IDENTIFY_ENDIAN_TYPE:READ:'//ENDIAN
              RETURN
            ENDIF
            CLOSE(FILE_ID)
          ELSE
            IERR = HERMES_INVALID_SERAFIN_FILE
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &             TRIM(FILE_NAME)//': '//
     &             'IDENTIFY_ENDIAN_TYPE:READ:'//ENDIAN
              RETURN
            ENDIF
            CLOSE(FILE_ID)
          ENDIF
#endif
        ENDIF
!
      END SUBROUTINE

!***********************************************************************
      SUBROUTINE OPEN_MESH_SRF
!***********************************************************************
!
     &(FILE_NAME,FILE_ID,OPENMODE,FFORMAT,IERR,MESH_NUMBER)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    OPENS A MESH FILE
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!history  S.E. BOURBAN (HRW)
!+        20/06/2016
!+        V7P2
!+        Compiler specific directive added because not all compilers
!+        support OPEN( CONVERT= )
!
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated mesh
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |-->| NAME OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','READWRITE'
!| FFORMAT        |<->| FORMAT OF THE FILE, CAN BE MODIFIED IF THE USER
!|                |   |  MADE A MISTAKE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE  WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE HERMES_INDEX_FILE
        IMPLICIT NONE
        !
        INTEGER,           INTENT(OUT)   :: FILE_ID
        CHARACTER(LEN=9),  INTENT(IN)    :: OPENMODE
        CHARACTER(LEN=*),  INTENT(IN)    :: FILE_NAME
        CHARACTER(LEN=8),  INTENT(INOUT) :: FFORMAT
        INTEGER,           INTENT(OUT)   :: IERR
        INTEGER, OPTIONAL, INTENT(IN)    :: MESH_NUMBER
        !
        INTEGER(KIND=K8) :: MY_POS, I, POS, FSIZE
        INTEGER(KIND=K8) :: OFFSET_BEGIN, OFFSET_END
        INTEGER(KIND=K4) :: B1, B2, IB(10), IDUM, TAG
        INTEGER :: SRF_ID, NTIMESTEP, FD
        CHARACTER(LEN=9) :: SRF_OPENMODE
        CHARACTER(LEN=200) :: MSG
#if defined NO_INQUIRE_SIZE
        REAL :: TIME
#endif
        ! Check if the file is already opened and get its id if that is
        ! the case
        ! This will return -1 if the file is not opened
        INQUIRE(FILE=FILE_NAME, NUMBER=FD)
        IF(FD.NE.-1) THEN
          ! File already opened so just setting file_id
          IERR = 0
          FILE_ID = FD
          RETURN
        ELSE
          ! First time opening the file
          CALL GET_FREE_ID(FILE_ID)
          ! ADD A NEW FILE TO THE HASH TABLE
          CALL ADD_OBJ(HASH,FILE_ID,SRF_ID,IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(FILE_NAME)//': '//
     &           'OPEN_MESH_SRF:ADD_OBJ'
            RETURN
          ENDIF
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%FILE_NAME = FILE_NAME
        !
        ! BECAUSE of the stream mode in write only we need to have read access
        ! to position the file pointer so if the file is in write only
        ! we open it in readwrite
        IF(OPENMODE(1:5).EQ.'WRITE') THEN
          SRF_OPENMODE = 'READWRITE'
        ELSE
          SRF_OPENMODE = OPENMODE
        ENDIF
        ! Get the Endian for the file
        CALL IDENTIFY_ENDIAN_TYPE(FILE_NAME,ENDIAN,FILE_ID, IERR)
        SRF_OBJ_TAB(SRF_ID)%ENDIAN = ENDIAN
        IF(IERR.NE.0) RETURN
        ! OPEN THE FILE IN STREAM MODE
#if defined NO_CONVERT_ENDIAN
        OPEN(FILE=FILE_NAME, ACTION=SRF_OPENMODE, UNIT=FILE_ID,
     &       FORM='UNFORMATTED', ACCESS='STREAM',
     &       IOMSG=MSG, IOSTAT=IERR)
#else
        OPEN(FILE=FILE_NAME, ACTION=SRF_OPENMODE, UNIT=FILE_ID,
     &       FORM='UNFORMATTED', ACCESS='STREAM',CONVERT=ENDIAN,
     &       IOMSG=MSG, IOSTAT=IERR)
#endif
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(FILE_NAME)//': '//
     &         'OPEN_MESH_SRF:OPEN'//'\n'//MSG
          RETURN
        ENDIF
        !
        IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
          CALL OPEN_INDEX(FILE_NAME, SRF_OBJ_TAB(SRF_ID)%MESH_IDX_ID)
          CALL READ_INDEX(SRF_OBJ_TAB(SRF_ID)%MESH_IDX_ID, OPENMODE,
     &                    MESH_NUMBER, OFFSET_BEGIN, OFFSET_END)
        ELSE
          OFFSET_BEGIN=1
        ENDIF
        ! IT DOES NOT MATTER IF SERAFIN IS DOUBLE PRECISION OR NOT
        SRF_OBJ_TAB(SRF_ID)%KS = 4
        ! ONLY DO THE SCAN OF THE FILE IF THE FILE IS NOT WRITE ONLY
        IF(OPENMODE(1:4).EQ.'READ') THEN
!
          ! IDENTIFY THE POSITION OF THE DIFFERENT MARKER OF THE FILE
          ! EACH "SET" IS DELIMITED BETWEEN TWO 4 BYTE INTEGER
          ! INDICATING THE SIZE OF THE SET
          ! SEE COMMENT FOR TYPE SRF_INFO FOR A DESCRIPTION OF EVERY VARIABLE
          SRF_OBJ_TAB(SRF_ID)%POS_TITLE = OFFSET_BEGIN
          SRF_OBJ_TAB(SRF_ID)%POS_NVAR = SRF_OBJ_TAB(SRF_ID)%POS_TITLE
     &                            + 4 + TITLE_SIZE + 4
          SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = SRF_OBJ_TAB(SRF_ID)%POS_NVAR
     &                            + 4 + 2*SRF_OBJ_TAB(SRF_ID)%KS + 4
!
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_NVAR+4
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR)
     &              B1,B2
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'OPEN_MESH_SRF:READNVAR'
            RETURN
          ENDIF
          SRF_OBJ_TAB(SRF_ID)%NVAR = B1 + B2
          ALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(B1+B2),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'OPEN_MESH_SRF:VAR_LIST'
            RETURN
          ENDIF
          DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
            POS = SRF_OBJ_TAB(SRF_ID)%POS_VARINFO + (I-1)*(4+VAR_SIZE+4)
            READ(FILE_ID,POS=POS+4,IOMSG=MSG,IOSTAT=IERR)
     &                      SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &             TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &             'OPEN_MESH_SRF:READ_VAR_LIST'//'\N'//MSG
              RETURN
            ENDIF
          ENDDO
!
          SRF_OBJ_TAB(SRF_ID)%POS_IB = SRF_OBJ_TAB(SRF_ID)%POS_VARINFO
     &                     + SRF_OBJ_TAB(SRF_ID)%NVAR*(4 + VAR_SIZE + 4)
!
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IB+4
          READ(FILE_ID, POS=MY_POS, IOSTAT=IERR) IB(1:10)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'OPEN_MESH_SRF:READ_IB'
            RETURN
          ENDIF
          SRF_OBJ_TAB(SRF_ID)%X_ORIG = IB(3)
          SRF_OBJ_TAB(SRF_ID)%Y_ORIG = IB(4)
          SRF_OBJ_TAB(SRF_ID)%NPLAN = IB(7)
          SRF_OBJ_TAB(SRF_ID)%NPTFR = IB(8)
          SRF_OBJ_TAB(SRF_ID)%NPTIR = IB(9)
          IF(IB(7).GT.1) THEN
            SRF_OBJ_TAB(SRF_ID)%NDIM = 3
          ELSE
            SRF_OBJ_TAB(SRF_ID)%NDIM = 2
          ENDIF
!
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_IB
     &                          + 4 + 10*SRF_OBJ_TAB(SRF_ID)%KS + 4
!
          ! IF IB(10).NE.0  THEN WE HAVE A DATE OF 6 INTEGER AFTER IB
          IF(IB(10).NE.0) THEN

            SRF_OBJ_TAB(SRF_ID)%POS_DATE = SRF_OBJ_TAB(SRF_ID)%POS_NUM
            SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_NUM
     &                            + 4 + 6*SRF_OBJ_TAB(SRF_ID)%KS + 4
          ELSE
            SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
          ENDIF
!
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_NUM+4
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR) IB(1:4)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'OPEN_MESH_SRF:READ_NELEM*'
            RETURN
          ENDIF

          SRF_OBJ_TAB(SRF_ID)%NELEM = IB(1)
          SRF_OBJ_TAB(SRF_ID)%NPOIN = IB(2)
          SRF_OBJ_TAB(SRF_ID)%NDP = IB(3)
!         Identify the type of element:
          CALL IDENTIFY_TYP_ELT(SRF_OBJ_TAB(SRF_ID)%NDP,
     &                           SRF_OBJ_TAB(SRF_ID)%NDIM,
     &                           SRF_OBJ_TAB(SRF_ID)%TYP_ELT)
          SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = POINT_BND_ELT_TYPE
!
!
          SRF_OBJ_TAB(SRF_ID)%POS_IKLE = SRF_OBJ_TAB(SRF_ID)%POS_NUM
     &                            + (4 + 4*SRF_OBJ_TAB(SRF_ID)%KS + 4)
          SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = SRF_OBJ_TAB(SRF_ID)%POS_IKLE
     &        + (4 +
     &             SRF_OBJ_TAB(SRF_ID)%NELEM*SRF_OBJ_TAB(SRF_ID)%NDP
     &            * SRF_OBJ_TAB(SRF_ID)%KS
     &             + 4)
          SRF_OBJ_TAB(SRF_ID)%POS_COORD = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO
     &      + (4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%KS + 4)
          !
          !Check if we are indeed in single or double precision
          ! If we are in single precision the tag for the coordiantes should be:
          ! npoin*4 if we are in double it will be npoin*8
          !
          READ(FILE_ID,POS=SRF_OBJ_TAB(SRF_ID)%POS_COORD) TAG
          IF(TAG.EQ.SRF_OBJ_TAB(SRF_ID)%NPOIN*8) THEN
            FFORMAT = 'SERAFIND'
            SRF_OBJ_TAB(SRF_ID)%RS = 8
          ELSEIF(TAG.EQ.SRF_OBJ_TAB(SRF_ID)%NPOIN*4) THEN
            FFORMAT = 'SERAFIN '
            SRF_OBJ_TAB(SRF_ID)%RS = 4
          ELSE
            IERR = HERMES_INVALID_SERAFIN_FILE
            RETURN
          ENDIF
!
          ! EVEN IN 3D THE SERAFON ONLY CONTAINS THE X AND Y COORDINATES AS THE Y VARIES WITH TIME
          SRF_OBJ_TAB(SRF_ID)%POS_DATA = SRF_OBJ_TAB(SRF_ID)%POS_COORD
     &        + (4 +
     &             SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS
     &             + 4)*2
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA =
     &         4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS + 4
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET =
     &         4 + SRF_OBJ_TAB(SRF_ID)%RS + 4
     &         + SRF_OBJ_TAB(SRF_ID)%NVAR*SRF_OBJ_TAB(SRF_ID)%SIZE_DATA
!
          ! IF NPTFR IS NOT IN IB
          ! WE COMPUTE IT BY COUNTING THE NUMBER OF POINT FOR WHICH IPOBO == 1
          IF((SRF_OBJ_TAB(SRF_ID)%NPTFR.EQ.0)
     &       .AND. (SRF_OBJ_TAB(SRF_ID)%NPTIR.EQ.0)) THEN
            ! POSITION OF THE IPOBO ARRAY IN THE FILE
            MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO + 4
            ! Positionning the file pointer
            READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) IDUM
            ! LOOP ON ALL THE POINTS
            DO I=1,SRF_OBJ_TAB(SRF_ID)%NPOIN
              READ(FILE_ID,IOSTAT=IERR) IDUM
              IF(IERR.NE.0) THEN
                ERROR_MESSAGE = 'ERROR IN '//
     &               TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &               'OPEN_MESH_SRF:READ_IPOBO_VAL'
                RETURN
              ENDIF
              IF(IDUM.NE.0) SRF_OBJ_TAB(SRF_ID)%NPTFR =
     &                          SRF_OBJ_TAB(SRF_ID)%NPTFR + 1
            ENDDO
          ENDIF

          ! COUTING THE NUMBER OF TIME STEPS
          NTIMESTEP = 0
          IERR = 0
          IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
            NTIMESTEP =
     &         INT((OFFSET_END-(SRF_OBJ_TAB(SRF_ID)%POS_DATA-1))
     &             /SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET, KIND=K4)
          ELSE
#if defined NO_INQUIRE_SIZE
          ! Need this solution for some compilers in which inquire size
          ! is not implemented
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA
          DO
            READ(FILE_ID,POS=MY_POS+4,IOSTAT=IERR) TIME
            IF(IERR.LT.0) EXIT
            NTIMESTEP = NTIMESTEP + 1
            MY_POS = MY_POS + SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET
          ENDDO
          ! THE LAST READ IS GOING TO CRASH ON PURPOSE BECAUSE
          ! WE'VE REACHED THE END OF THE FILE
          IERR = 0
#else
          ! Get the size of the file
          INQUIRE(UNIT=FILE_ID,SIZE=FSIZE)
          ! Ntimestep = (size_of_fil - size_of_geom)/size_of_a_timestep
          NTIMESTEP = INT((FSIZE - (SRF_OBJ_TAB(SRF_ID)%POS_DATA - 1) )
     &             /SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET, KIND=K4)
#endif
          ENDIF
          SRF_OBJ_TAB(SRF_ID)%NTIMESTEP = NTIMESTEP
        ELSE
          ! NOT READ ONLY
          ! INTIALIZE SRF_OBJ_TAB(SRF_ID) TO ZEROS
          ! POSITION IN FILE
          SRF_OBJ_TAB(SRF_ID)%POS_TITLE = OFFSET_BEGIN
          SRF_OBJ_TAB(SRF_ID)%POS_NVAR = 0
          SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = 0
          SRF_OBJ_TAB(SRF_ID)%POS_IB = 0
          SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = 0
          SRF_OBJ_TAB(SRF_ID)%POS_IKLE = 0
          SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = 0
          SRF_OBJ_TAB(SRF_ID)%POS_COORD = 0
          SRF_OBJ_TAB(SRF_ID)%POS_DATA =0
          ! COMPUTED INFORMATIONS
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA = 0
          SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET = 0
          ! STOCKED QUANTITIES AND SMALL VARIABLES
          SRF_OBJ_TAB(SRF_ID)%NTIMESTEP = 0
          SRF_OBJ_TAB(SRF_ID)%NPOIN = 0
          SRF_OBJ_TAB(SRF_ID)%NVAR = 0
          SRF_OBJ_TAB(SRF_ID)%NELEM = 0
          SRF_OBJ_TAB(SRF_ID)%NDP = 0
          SRF_OBJ_TAB(SRF_ID)%NPLAN = 0
          SRF_OBJ_TAB(SRF_ID)%NPTFR = 0
          SRF_OBJ_TAB(SRF_ID)%NPTIR = 0
          SRF_OBJ_TAB(SRF_ID)%NDIM = 0
          SRF_OBJ_TAB(SRF_ID)%X_ORIG = 0
          SRF_OBJ_TAB(SRF_ID)%Y_ORIG = 0
          SRF_OBJ_TAB(SRF_ID)%TYP_ELT = 0
          SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = 0
          SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN = 0
          SRF_OBJ_TAB(SRF_ID)%CLI_LINE_END = 0
          ! DEFINES THE SIZE OF REAL IN THE FILE
          IF(FFORMAT.EQ.'SERAFIND') THEN
            SRF_OBJ_TAB(SRF_ID)%RS = 8
          ELSE
            SRF_OBJ_TAB(SRF_ID)%RS = 4
          ENDIF
        ENDIF
        ! Initialising boundary unit to the same as mesh file
        SRF_OBJ_TAB(SRF_ID)%NCLI = 0

        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE OPEN_BND_SRF
!***********************************************************************
!
     &(FILE_NAME,FILE_ID,OPENMODE,IERR,MESH_NUMBER)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    OPENS A BOUNDARY FILE
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated mesh
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |-->| NAME OF THE BOUNDARY FILE
!| FILE_ID        |-->| FILE DESCRIPTOR OF THE "MESH" FILE
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','WRITE','READWRITE'
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE  WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE HERMES_INDEX_FILE
        IMPLICIT NONE
        !
        CHARACTER(LEN=*),  INTENT(IN)  :: FILE_NAME
        INTEGER,           INTENT(IN)  :: FILE_ID
        CHARACTER(LEN=9),  INTENT(IN)  :: OPENMODE
        INTEGER,           INTENT(OUT) :: IERR
        INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
        !
        INTEGER SRF_ID,I,NPTFR
        LOGICAL :: ISOPENED
        CHARACTER(LEN=9) :: REAL_OPENMODE
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH IF '//I2CHAR(FILE_ID)//': '//
     &         'OPEN_BND_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        REAL_OPENMODE=OPENMODE
        SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN=1
        IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
          !If we concatenate we need to be able to move into the file,
          !which can only be done by reading, so we need readwrite access
          IF(OPENMODE(1:5)=='WRITE')THEN
            REAL_OPENMODE='READWRITE'
          ENDIF
          CALL OPEN_INDEX(FILE_NAME, SRF_OBJ_TAB(SRF_ID)%CLI_IDX_ID)
          CALL READ_INDEX(SRF_OBJ_TAB(SRF_ID)%CLI_IDX_ID,OPENMODE,
     &                    MESH_NUMBER,
     &                    SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN,
     &                    SRF_OBJ_TAB(SRF_ID)%CLI_LINE_END)
        ENDIF
        !
        ! Open the boundary file with a set id available
        ! First we check if the file is already opened
        ! Telemac is using one boundary file for all the mesh file
        ! so it could have been opened by another mesh before hand
        INQUIRE(FILE=FILE_NAME,OPENED=ISOPENED)
        IF(ISOPENED) THEN
            INQUIRE(FILE=FILE_NAME,NUMBER=SRF_OBJ_TAB(SRF_ID)%NCLI)
        ELSE
          ! Otherwise open the file
          CALL GET_FREE_ID(SRF_OBJ_TAB(SRF_ID)%NCLI)
          OPEN(UNIT=SRF_OBJ_TAB(SRF_ID)%NCLI,FILE=FILE_NAME,
     &         FORM='FORMATTED',ACTION=REAL_OPENMODE,IOSTAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'OPEN_BND_SRF:OPEN'
            RETURN
          ENDIF
        ENDIF
        !
        ! If we are not in write only
        ! We compute the number of boundary point i.e. number of line in the file
        IF(REAL_OPENMODE(1:5).NE.'WRITE') THEN
          !OFFSET_END SHOULD BE SET TO -1 BY THE SET_BND SUBROUTINE WHEN
          !WE DON'T HAVE ANY BOUNDARY OR THE FILE IS NOT CONCATENATE
          IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
            IF(SRF_OBJ_TAB(SRF_ID)%CLI_LINE_END >
     &         SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN)THEN
              NPTFR=SRF_OBJ_TAB(SRF_ID)%CLI_LINE_END
     &             - SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN
            ELSE
              NPTFR=0
            ENDIF
          ELSE
            REWIND(SRF_OBJ_TAB(SRF_ID)%NCLI)
            DO I=1,SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN-1
              READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*)
            ENDDO
            NPTFR = 0
            DO
              READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR)
              IF (IERR.LT.0) THEN
                ! END OF FILE REACHED
                EXIT
              ELSE IF (IERR.GT.0) THEN
                ! ERROR DURING READ
                IF(IERR.NE.0) THEN
                  ERROR_MESSAGE = 'ERROR IN '//
     &                 TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &                 'OPEN_BND_SRF:READ'
                  RETURN
                ENDIF
              ENDIF
              NPTFR = NPTFR + 1
            ENDDO
            IERR = 0
          ENDIF
          SRF_OBJ_TAB(SRF_ID)%NPTFR = NPTFR
        ENDIF
        RETURN
        !
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE CLOSE_BND_SRF
!***********************************************************************
!
     &(FILE_ID,IERR,MESH_NUMBER)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!BRIEF    CLOSES A BOUNDARY FILE
!         IF MESH_NUMBER IS PROVIDED, THE FUNCTION WILL WRITE THE
!         CURRENT SIZE OF THE OFFSET INTO THE INDEX FILE ASSOCIATED TO THE
!         CONCATENATED CLI
!
!HISTORY  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated mesh
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE HERMES_INDEX_FILE
        IMPLICIT NONE
        !
        INTEGER,           INTENT(IN)  :: FILE_ID
        INTEGER,           INTENT(OUT) :: IERR
        INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
        !
        INTEGER SRF_ID, LINE_BEGIN, LINE_END
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'CLOSE_BND_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Check if the file is still opened as it could have been closed
        ! by another mesh file (see open_bnd_srf for more information)
        IF(ISOPENED(SRF_OBJ_TAB(SRF_ID)%NCLI)) THEN
          IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
            LINE_BEGIN = SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN
            LINE_END = SRF_OBJ_TAB(SRF_ID)%NPTFR + LINE_BEGIN
            !
            CALL WRITE_INDEX(SRF_OBJ_TAB(SRF_ID)%CLI_IDX_ID,
     &                       MESH_NUMBER, LINE_BEGIN, LINE_END)
            CLOSE(SRF_OBJ_TAB(SRF_ID)%CLI_IDX_ID)
          ENDIF
          CLOSE(SRF_OBJ_TAB(SRF_ID)%NCLI,IOSTAT=IERR)
        ENDIF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'CLOSE_BND_SRF:CLOSE'
          RETURN
        ENDIF
        !
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE CLOSE_MESH_SRF
!***********************************************************************
!
     &(FILE_ID,IERR,MESH_NUMBER)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!BRIEF    CLOSES A MESH FILE
!         IF MESH_NUMBER IS PROVIDED, THE FUNCTION WILL WRITE THE
!         CURRENT SIZE OF THE MESH INTO THE INDEX FILE ASSOCIATED TO THE
!         CONCATENATED MESH
!
!HISTORY  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!history  J. GRASSET (Daresbury Lab & EDF)
!+        01/05/2018
!+        Add code for managing concatenated mesh
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE HERMES_INDEX_FILE
        IMPLICIT NONE
        !
        INTEGER,           INTENT(IN)  :: FILE_ID
        INTEGER,           INTENT(OUT) :: IERR
        INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
        !
        INTEGER SRF_ID
        INTEGER(KIND=K8) :: OFFSET
        LOGICAL :: ISOPEN
        !
        INQUIRE(UNIT=FILE_ID, OPENED=ISOPEN)
        IF(.NOT.ISOPEN) THEN
          IERR = 0
          RETURN
        ENDIF
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'CLOSE_MESH:GET_OBJ'
          RETURN
        ENDIF
        !
        !If mesh_number is present that means we should write the index
        !at the right place in the index file
        IF(PRESENT(MESH_NUMBER).AND.PARTEL_CONCAT)THEN
          INQUIRE(FILE_ID,POS=OFFSET)
          CALL WRITE_INDEX(SRF_OBJ_TAB(SRF_ID)%MESH_IDX_ID,MESH_NUMBER,
     &                     SRF_OBJ_TAB(SRF_ID)%POS_TITLE,OFFSET)
        ENDIF
        !If an index file was open with the mesh we must close it too
        IF(ISOPENED(SRF_OBJ_TAB(SRF_ID)%MESH_IDX_ID))THEN
          CLOSE(SRF_OBJ_TAB(SRF_ID)%MESH_IDX_ID)
        ENDIF

        ! Clearing id in the hash table
        HASH(SRF_ID) = 0
        ! Closing the file
        CLOSE(FILE_ID,IOSTAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH_SRF:CLOSE'
          RETURN
        ENDIF
        ! RESET SRF_OBJ_TAB(SRF_ID) TO ZEROS
        ! POSITION IN FILE
        SRF_OBJ_TAB(SRF_ID)%POS_TITLE = 1
        SRF_OBJ_TAB(SRF_ID)%POS_NVAR = 0
        SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = 0
        SRF_OBJ_TAB(SRF_ID)%POS_IB = 0
        SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
        SRF_OBJ_TAB(SRF_ID)%POS_NUM = 0
        SRF_OBJ_TAB(SRF_ID)%POS_IKLE = 0
        SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = 0
        SRF_OBJ_TAB(SRF_ID)%POS_COORD = 0
        SRF_OBJ_TAB(SRF_ID)%POS_DATA =0
        ! COMPUTED INFORMATIONS
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA = 0
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET = 0
        ! STOCKED QUANTITIES AND SMALL VARIABLES
        SRF_OBJ_TAB(SRF_ID)%NTIMESTEP = 0
        SRF_OBJ_TAB(SRF_ID)%NPOIN = 0
        SRF_OBJ_TAB(SRF_ID)%NVAR = 0
        SRF_OBJ_TAB(SRF_ID)%NELEM = 0
        SRF_OBJ_TAB(SRF_ID)%NDP = 0
        SRF_OBJ_TAB(SRF_ID)%NPLAN = 0
        SRF_OBJ_TAB(SRF_ID)%NPTFR = 0
        SRF_OBJ_TAB(SRF_ID)%NPTIR = 0
        SRF_OBJ_TAB(SRF_ID)%NDIM = 0
        SRF_OBJ_TAB(SRF_ID)%X_ORIG = 0
        SRF_OBJ_TAB(SRF_ID)%Y_ORIG = 0
        SRF_OBJ_TAB(SRF_ID)%TYP_ELT = 0
        SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = 0
        SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN = 0
        SRF_OBJ_TAB(SRF_ID)%CLI_LINE_END = 0
        IF(ALLOCATED(SRF_OBJ_TAB(SRF_ID)%VAR_LIST))
     &    DEALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST)
        !
        RETURN
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE GET_ENDIANESS_SRF
!***********************************************************************
!
     &(FILE_ID,ENDIANESS,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the title from a mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TITLE          |<->| TITLE OF THE MESH FILE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        CHARACTER(LEN=13), INTENT(OUT) :: ENDIANESS
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER          :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_ENDIANESS:GET_OBJ'
          RETURN
        ENDIF
        !
        ENDIANESS = SRF_OBJ_TAB(SRF_ID)%ENDIAN
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_FILE_FORMAT_SRF
!***********************************************************************
!
     &(FFORMAT,FILE_ID,REAL_FFORMAT,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the format from a mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| REAL_FFORMAT   |<--| REAL FORMAT OF THE FILE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
        CHARACTER(LEN=8), INTENT(OUT) :: REAL_FFORMAT
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER          :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_FILE_FORMAT:GET_OBJ'
          RETURN
        ENDIF
        !
        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.8) THEN
          REAL_FFORMAT = 'SERAFIND'
        ELSEIF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          REAL_FFORMAT = 'SERAFIN '
        ELSE
          REAL_FFORMAT = FFORMAT
        ENDIF
        !
        RETURN
      END SUBROUTINE
!
!     Mesh functions
!
!***********************************************************************
      SUBROUTINE GET_MESH_TITLE_SRF
!***********************************************************************
!
     &(FILE_ID,TITLE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the title from a mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TITLE          |<->| TITLE OF THE MESH FILE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        CHARACTER(LEN=TITLE_SIZE), INTENT(OUT) :: TITLE
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_TITLE:GET_OBJ'
          RETURN
        ENDIF
        !
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_TITLE + 4
        READ(FILE_ID,POS=MY_POS,IOSTAT=IERR)
     &           TITLE(1:TITLE_SIZE)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_MESH_TITLE_SRF:READ'
          RETURN
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_DATE_SRF
!***********************************************************************
!
     &(FILE_ID,DATE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the date of the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| DATE           |<->| THE DATE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: DATE(6)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: I
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_DATE_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Default value
        DATE(1:6) = (/ 0,0,0,0,0,0 /)
        ! POS_DATE=0 <=> NO DATE
        IF(SRF_OBJ_TAB(SRF_ID)%POS_DATE.NE.0) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATE + 4
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR) (DATE(I),I=1,6)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'GET_MESH_DATE_SRF:READ'
            RETURN
          ENDIF
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NELEM_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,NELEM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of elements of type typ_elem in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NELEM          |<->| THE NUMBER OF ELEMENTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(OUT) :: NELEM
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NELEM_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        IF(TYP_ELT.EQ.SRF_OBJ_TAB(SRF_ID)%TYP_ELT) THEN
          NELEM = SRF_OBJ_TAB(SRF_ID)%NELEM
        ELSE
          NELEM = 0
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPOIN_PER_ELEMENT_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,NDP,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of point per element of type typ_elem
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NDP            |<->| THE NUMBER OF POINT PER ELEMENT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(OUT) :: NDP
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPOIN_PER_ELEMENT_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NDP = 0
        IF(TYP_ELT.EQ.SRF_OBJ_TAB(SRF_ID)%TYP_ELT) THEN
          NDP = SRF_OBJ_TAB(SRF_ID)%NDP
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_CONNECTIVITY_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,IKLE,NELEM,NDP,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the connectivity table for
!+        the element of type typ_elem in the mesh
!+        will do nothing if there are no element of typ_elem in the mesh
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| IKLE           |<->| THE CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(IN) :: NELEM
        INTEGER, INTENT(IN) :: NDP
        INTEGER, INTENT(INOUT) :: IKLE(NELEM*NDP)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, ARRAY_SIZE
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: I
        INTEGER(KIND=K4) :: TMP
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_CONNECTIVITY_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NELEM * SRF_OBJ_TAB(SRF_ID)%NDP
        !
        IF(TYP_ELT.EQ.SRF_OBJ_TAB(SRF_ID)%TYP_ELT) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IKLE + 4
          ! Positioning the file pointer
          READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TMP
          ! WE NEED TO CERTIFY THAT THE INTEGER READ IS ON 4 BYTES
          READ(FILE_ID,IOSTAT=IERR) (IKLE(I),I=1,ARRAY_SIZE)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'GET_MESH_CONNECTIVITY_SRF:READ'
            RETURN
          ENDIF
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPOIN_SRF
!***********************************************************************
!
     &(FILE_ID,NPOIN,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of point for the given element type in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NPOIN          |<->| THE NUMBER OF POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: NPOIN
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPOIN_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NPOIN = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPLAN_SRF
!***********************************************************************
!
     &(FILE_ID,NPLAN,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of layers
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NPLAN          |<->| THE NUMBER OF LAYERS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: NPLAN
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPLAN_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NPLAN = SRF_OBJ_TAB(SRF_ID)%NPLAN
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_ORIG_SRF
!***********************************************************************
!
     &(FILE_ID,X_ORIG,Y_ORIG,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the X,Y origin of the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| X_ORIG         |<->| Off set of the X coordinates
!| Y_ORIG         |<->| Off set of the Y coordinates
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(INOUT) :: X_ORIG, Y_ORIG
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER          :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_ORIG_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        X_ORIG = SRF_OBJ_TAB(SRF_ID)%X_ORIG
        Y_ORIG = SRF_OBJ_TAB(SRF_ID)%Y_ORIG
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_DIMENSION_SRF
!***********************************************************************
!
     &(FILE_ID,NDIM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of dimensions of the space
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NDIM           |<->| NUMBER OF DIMENSION
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: NDIM
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_DIMENSION_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NDIM = SRF_OBJ_TAB(SRF_ID)%NDIM
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_COORD_SRF
!***********************************************************************
!
     &(FILE_ID,JDIM,NPOIN,COORD,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the coordinates for the given dimension
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| JDIM           |-->| DIMENSION NUMBER
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| COORD          |<->| LOCAL TO GLOBAL NUMBERING ARRAY
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: JDIM
        INTEGER, INTENT(IN) :: NPOIN
        DOUBLE PRECISION, INTENT(INOUT) :: COORD(NPOIN)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: I, ARRAY_SIZE
        INTEGER :: DBL_TYP
        INTEGER(KIND=K4) :: TAG
        REAL(KIND=R4),ALLOCATABLE :: COORD_2(:)
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_COORD_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        DBL_TYP = SRF_OBJ_TAB(SRF_ID)%RS
        !
        ! Move to the position of the coordinates in the mesh file
        IF ((JDIM.GT.0).AND.(JDIM.LE.2)) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_COORD
     &             + INT((JDIM-1)*(4 + ARRAY_SIZE*DBL_TYP + 4),K8)
     &             + 4
        ELSE
!         ERROR ON JDIM
          IERR = HERMES_WRONG_AXE_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'COORDINATE NUMBER ('//TRIM(I2CHAR(JDIM))//
     &         ') NOT BETWEEN 0 AND '//I2CHAR(2)
          RETURN
        ENDIF
        ! Positioning the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG
        ! Loop on all value depending on real precision (single or double)
        IF(DBL_TYP.EQ.4) THEN
          ALLOCATE(COORD_2(ARRAY_SIZE),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'COORD_2'
            RETURN
          ENDIF
          READ(FILE_ID,IOSTAT=IERR) (COORD_2(I),I=1,ARRAY_SIZE)
          COORD = DBLE(COORD_2)
          DEALLOCATE(COORD_2)
        ELSE
          READ(FILE_ID,IOSTAT=IERR) (COORD(I),I=1,ARRAY_SIZE)
        ENDIF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_MESH_COORD_SRF:READ'
          RETURN
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_L2G_NUMBERING_SRF
!***********************************************************************
!
     &(FILE_ID,KNOLG,NPOIN,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the local to global numbering array
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| KNOLG          |<->| LOCAL TO GLOBAL NUMBERING ARRAY
!| NPOIN          |-->| NUMBER OF NODES
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: NPOIN
        INTEGER, INTENT(INOUT) :: KNOLG(NPOIN)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: ARRAY_SIZE,I
        INTEGER(KIND=K4) :: TMP
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_L2G_NUMBERING_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO + 4
        ! Positionning the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TMP
        READ(FILE_ID,IOSTAT=IERR) (KNOLG(I),I=1,ARRAY_SIZE)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_MESH_L2G_NUMBERING_SRF:READ'
          RETURN
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPTIR_SRF
!***********************************************************************
!
     &(FILE_ID,NPTIR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of interface point
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NPTIR          |<->| NUMBER OF INTERFACE POINT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: NPTIR
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        !TODO: Remove that function ??
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPTIR_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NPTIR = SRF_OBJ_TAB(SRF_ID)%NPTIR
        !
        RETURN
      END SUBROUTINE
!
!     Boundary functions
!
!***********************************************************************
      SUBROUTINE GET_BND_IPOBO_SRF
!***********************************************************************
!
     &(FILE_ID,NPOIN,IPOBO,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing
!+        1 if a point is a boundary point 0 otherwise
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| IPOBO          |<->| AN ARRAY CONTAINING
!|                |   | 1 IF A POINT IS A BOUNDARY POINT 0 OTHERWISE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: NPOIN
        INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: SRF_ID
        INTEGER :: ARRAY_SIZE, I
        INTEGER(KIND=K4) TMP
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_IPOBO_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO + 4
        ! Positionning the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TMP
        READ(FILE_ID,IOSTAT=IERR) (IPOBO(I),I=1,ARRAY_SIZE)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_MESH_IPOBO_SRF:READ'
          RETURN
        ENDIF
        !
        RETURN
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE GET_BND_NUMBERING_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_ELEM_BND,NPTFR,NBOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing
!+        The association of boundary numbering to mesh numbering
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NBOR           |<->| AN ARRAY CONTAINING THE NUMBERING IN THE MESH
!|                |   | OF ALL BOUNDARY POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
        INTEGER, INTENT(IN) :: FILE_ID,NPTFR,TYP_ELEM_BND
        INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_IPOBO_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        CALL GET_BND_CONNECTIVITY_SRF(FILE_ID,TYP_ELEM_BND,NPTFR,
     &                                POINT_BND_ELT_TYPE,NBOR,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_SRF:GET_BND_CONNECTIVITY_SRF'
          RETURN
        ENDIF
        !
        RETURN
      END SUBROUTINE
      SUBROUTINE GET_BND_NELEM_SRF(FILE_ID,TYPE_BND_ELEM, NELEM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Reads the number of boundary elements
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYPE_BND_ELEM  |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEM          |<->| NUMBER OF BOUNDARY ELEMENTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(INOUT) :: NELEM
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_NELEM_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.EQ.TYPE_BND_ELEM) THEN
          NELEM = SRF_OBJ_TAB(SRF_ID)%NPTFR
        ELSE
          NELEM = 0
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_CONNECTIVITY_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_BND_ELT,NELEBD,NDP,IKLE,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Reads the connectivity of the boundary elements
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| IKLE           |<->| THE CONNECTIVITY OF THE BOUNDARY ELEMENTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_BND_ELT
        INTEGER, INTENT(IN) :: NELEBD
        INTEGER, INTENT(IN) :: NDP
        INTEGER, INTENT(INOUT) :: IKLE(NELEBD*NDP)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I
        DOUBLE PRECISION :: DDUM
        INTEGER :: IDUM
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_CONNECTIVITY_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.NE.TYP_BND_ELT) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          ERROR_MESSAGE = 'GIVEN BOUNDARY ELEMENT TYPE :'//
     &              I2CHAR(TYP_BND_ELT)//
     &         ' IS NOT THE SAME AS THE ONE IN THE FILE: '//
     &         I2CHAR(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT)
          RETURN
        ENDIF
        ! Read the boundary file we only care about the connectivity
        REWIND(SRF_OBJ_TAB(SRF_ID)%NCLI)
        !If the file is a concatenation, we need to move to the
        !begining of our part
        DO I=1,SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN-1
          READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*)
        ENDDO
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NPTFR
          READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR) IDUM,IDUM,IDUM,
     &                         DDUM  ,DDUM  ,DDUM,
     &                         DDUM ,IDUM,DDUM,DDUM,DDUM,
     &                         IKLE(I),IDUM
          IF(IERR.LT.0) THEN
            ! End of file reached
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'GET_BND_CONNECTIVITY_SRF:READ:END OF FILE'
            RETURN
          ELSE IF (IERR.GE.0) THEN
            ! Error during read
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &             TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &             'GET_BND_CONNECTIVITY_SRF:READ'
              RETURN
            ENDIF
          ENDIF
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_COLOR_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_BND_ELT,NELEBD,COLOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Reads the connectivity of the boundary elements
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| COLOR          |<->| Boundary color
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_BND_ELT
        INTEGER, INTENT(IN) :: NELEBD
        INTEGER, INTENT(INOUT) :: COLOR(NELEBD)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I
        DOUBLE PRECISION :: DDUM
        INTEGER :: IDUM
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_BND_CONNECTIVITY_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.NE.TYP_BND_ELT) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          ERROR_MESSAGE = 'GIVEN BOUNDARY ELEMENT TYPE :'//
     &              I2CHAR(TYP_BND_ELT)//
     &         ' IS NOT THE SAME AS THE ONE IN THE FILE: '//
     &         I2CHAR(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT)
          RETURN
        ENDIF
        ! Read the boundary file we only care about the connectivity
        REWIND(SRF_OBJ_TAB(SRF_ID)%NCLI)
        !If the file is a concatenation, we need to move to the
        !begining of our part
        DO I=1,SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN-1
          READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*)
        ENDDO
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NPTFR
          READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR) IDUM,IDUM,IDUM,
     &                         DDUM  ,DDUM  ,DDUM,
     &                         DDUM ,IDUM,DDUM,DDUM,DDUM,
     &                         IDUM, COLOR(I)
          IF(IERR.LT.0) THEN
            ! End of file reached
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'GET_BND_CONNECTIVITY_SRF:READ:END OF FILE'
            RETURN
          ELSE IF (IERR.GT.0) THEN
            ! Error during read
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &             TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &             'GET_BND_CONNECTIVITY_SRF:READ'
              RETURN
            ENDIF
          ENDIF
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_VALUE_SRF
!***********************************************************************
!
     &(FILE_ID,TYP_BND_ELEM,NPTFR,LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,TRAC,
     & LITBOR,TBOR,ATBOR,BTBOR, IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing the boundary type for each
!+        boundary point
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_BND_ELEM
        INTEGER, INTENT(IN) :: NPTFR
        INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
        INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(*)
        DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
        DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
        DOUBLE PRECISION, INTENT(INOUT) :: TBOR(*),ATBOR(*)
        DOUBLE PRECISION, INTENT(INOUT) :: BTBOR(*)
        LOGICAL, INTENT(IN)    :: TRAC
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I
        INTEGER :: IDUM
        DOUBLE PRECISION :: DDUM
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_VALUE_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.NE.TYP_BND_ELEM) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          ERROR_MESSAGE = 'GIVEN BOUNDARY ELEMENT TYPE :'//
     &              I2CHAR(TYP_BND_ELEM)//
     &         ' IS NOT THE SAME AS THE ONE IN THE FILE: '//
     &         I2CHAR(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT)
          RETURN
        ENDIF
        ! Reading the boundary file informations we only care
        ! about the boundary type li[huvt]bor
        REWIND(SRF_OBJ_TAB(SRF_ID)%NCLI)
        !If the file is a concatenation, we need to move to the
        !begining of our part
        DO I=1,SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN-1
          READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*)
        ENDDO
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NPTFR
          IF(TRAC) THEN
            READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR)
     &             LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &             HBOR(I)  ,UBOR(I)  ,VBOR(I),
     &             CHBORD(I) ,LITBOR(I),
     &             TBOR(I),ATBOR(I),BTBOR(I),
     &             IDUM,IDUM
          ELSE
            READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*,IOSTAT=IERR)
     &             LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &             HBOR(I)  ,UBOR(I)  ,VBOR(I),
     &             CHBORD(I) ,IDUM,DDUM,DDUM,DDUM,
     &             IDUM,IDUM
          ENDIF
          IF(IERR.LT.0) THEN
            ! End of file reached
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'GET_BND_VALUE_SRF:READ:END OF FILE'
            RETURN
          ELSE IF (IERR.GT.0) THEN
            ! Error during read
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &             TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &             'GET_BND_VALUE_SRF:READ'
              RETURN
            ENDIF
          ENDIF
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_NPOIN_SRF
!***********************************************************************
!
     &(FILE_ID,TYPE_BND_ELEM,NPTFR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of boundary points
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYPE_BND_ELEM  |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NPTFR          |<->| NUMBER OF BOUNDARY POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(OUT) :: NPTFR
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_NPOIN_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        IF(SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT.EQ.TYPE_BND_ELEM) THEN
          NPTFR = SRF_OBJ_TAB(SRF_ID)%NPTFR
        ELSE
          NPTFR = 0
        ENDIF
        !
        !
        RETURN
      END SUBROUTINE
!
!     Data functions
!
!***********************************************************************
      SUBROUTINE GET_DATA_NVAR_SRF
!***********************************************************************
!
     &(FILE_ID,NVAR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of varaibles in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NVAR           |<->| NUMBER OF VARIABLE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: NVAR
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_NVAR_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NVAR = SRF_OBJ_TAB(SRF_ID)%NVAR
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_VAR_LIST_SRF
!***********************************************************************
!
     &(FILE_ID,NVAR,VAR_LIST,UNIT_LIST,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns a list of all the name of the variables in the mesh file
!+        and a list of their units
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| VARLIST        |<->| LIST OF VARIABLE NAME
!| UNTILIST       |<->| LIST OF VARIABLE UNIT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: NVAR
        CHARACTER(LEN=16), INTENT(INOUT) :: VAR_LIST(NVAR)
        CHARACTER(LEN=16), INTENT(INOUT) :: UNIT_LIST(NVAR)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        INTEGER :: I
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_DATA_VAR_LIST_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Test if the number of variable given as argument is the same
        ! as the one in the file
        IF(NVAR.NE.SRF_OBJ_TAB(SRF_ID)%NVAR) THEN
          IERR = HERMES_WRONG_ARRAY_SIZE_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'WRON NUMBER OF VARIABLE GIVEN: '//I2CHAR(NVAR)//
     &         'INSTEAD OF '//I2CHAR(SRF_OBJ_TAB(SRF_ID)%NVAR)
          RETURN
        ENDIF
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
          VAR_LIST(I) = TRIM(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(1:16))
          UNIT_LIST(I) = TRIM(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(17:32))
        ENDDO
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_NTIMESTEP_SRF
!***********************************************************************
!
     &(FILE_ID,NTIMESTEP,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of time step in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| NTIMESTEP      |<->| THE NUMBER OF TIME STEPS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: NTIMESTEP
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_NTIMSTEP_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NTIMESTEP = SRF_OBJ_TAB(SRF_ID)%NTIMESTEP
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_TIME_SRF
!***********************************************************************
!
     &(FILE_ID,RECORD,TIME,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the time value of a given time step
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!history  R ATA (EDF R&D, LNHE)
!+        24/05/2016
!+        V7P2
!+   The case with no record in the file was not treated.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| RECORD         |-->| NUMBER OF THE TIME STEP
!| TIME           |<->| TIME IN SECOND OF THE TIME STEP
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: RECORD
        DOUBLE PRECISION, INTENT(INOUT) :: TIME
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: SRF_ID
        REAL :: W
        INTEGER :: IREC,NTIMESTEP
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_TIME_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NTIMESTEP = SRF_OBJ_TAB(SRF_ID)%NTIMESTEP
        ! CHECK IF THE RECORD IS IN THE FILE
        IF((RECORD.GE.NTIMESTEP.OR.RECORD.LT.0).AND.
     &     NTIMESTEP.GT.0) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'RECORD: '//I2CHAR(RECORD)//' IS NOT WITHIN [0, '//
     &         I2CHAR(NTIMESTEP-1)//']'
          RETURN
        ELSE
          IREC = RECORD
        ENDIF
        ! POSITION OF THE TIME TO READ
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA + 4
     &           + INT(IREC,K8)*SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET
        ! DIFFERENCE BETWEEN REAL AND DOUBLE PRECISION
        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR) W
          TIME = DBLE(W)
        ELSE
          READ(FILE_ID,POS=MY_POS,IOSTAT=IERR) TIME
        ENDIF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_DATA_TIME_SRF:READ'
          RETURN
        ENDIF
        !
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_VALUE_SRF
!***********************************************************************
!
     &(FILE_ID,RECORD,VAR_NAME,RES_VALUE,N,IERR)
!
!***********************************************************************
! HERMES   V7P2                                               01/05/2014
!***********************************************************************
!
!brief    Returns The value for each point of a given variable
!+        for a given time step
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+   First version.
!
!history  R ATA (EDF R&D, LNHE)
!+        24/05/2016
!+        V7P2
!+   The case with no record in the file was not treated.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| RECORD         |-->| TIME STEP TO READ IN THE FILE
!| VAR_NAME       |-->| VARIABLE FOR WHICH WE NEED THE VALUE
!| RES_VALUE      |<->| VALUE FOR EACH POINT AT TIME STEP RECORD
!|                |   | FOR THE VARIABLE VAR_NAME
!| N              |-->| SIZE OF RES_VALUE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: RECORD, N
        CHARACTER(LEN=16), INTENT(IN) :: VAR_NAME
        DOUBLE PRECISION, INTENT(INOUT) :: RES_VALUE(N)
        INTEGER, INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: SRF_ID, IVAR
        INTEGER :: I, ARRAY_SIZE, NTIMESTEP, IREC
        REAL(KIND=R4),ALLOCATABLE :: TMP(:)
        INTEGER(KIND=K4) :: TAG
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_VALUE_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NTIMESTEP = SRF_OBJ_TAB(SRF_ID)%NTIMESTEP
        ! CHECK IF THE RECORD IS IN THE FILE
        IF((RECORD.GE.NTIMESTEP.OR.RECORD.LT.0).AND.
     &     NTIMESTEP.GT.0) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'RECORD: '//I2CHAR(RECORD)//' IS NOT WITHIN [0, '//
     &         I2CHAR(NTIMESTEP-1)//']'
          RETURN
        ELSE
          IREC = RECORD
        ENDIF
        !
        ! GET THE POSITION OF THE VARIABLE
        IVAR = 0
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
          IF(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(1:16).EQ.VAR_NAME) THEN
            IVAR = I
            EXIT
          ENDIF
        ENDDO
        ! NO VARIABLE WAS FOUND
        IF(IVAR.EQ.0) THEN
          IERR = HERMES_VAR_UNKNOWN_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'COULD NOT FIND YOUR VARIABLE: '//VAR_NAME
          RETURN
        ENDIF
        !
        ! Check the size of the array compare to the number of points
        IF (SRF_OBJ_TAB(SRF_ID)%NPOIN.LT.N) THEN
          IERR = HERMES_WRONG_SIZE
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'WRONG SIZE FOR ARRAY IS: '//I2CHAR(N)//' SHOULD BE '//
     &         I2CHAR(SRF_OBJ_TAB(SRF_ID)%NPOIN)
          RETURN
        ENDIF
        ARRAY_SIZE = SRF_OBJ_TAB(SRF_ID)%NPOIN
        !
        ! READ THE VARIABLES RESULT FOR THE GIVEN RECORD
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA + 4
     &          + INT(IREC,K8)*(SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET)
     &          + 4 + SRF_OBJ_TAB(SRF_ID)%RS + 4 ! THE TIME VALUE
     &          + INT(IVAR-1,K8) * SRF_OBJ_TAB(SRF_ID)%SIZE_DATA
        ! Quick read to position the file pointer
        READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG
        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          ALLOCATE(TMP(N),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'ALLOCATING TMP'
            RETURN
          ENDIF
          READ(FILE_ID,IOSTAT=IERR) (TMP(I),I=1,N)
          RES_VALUE = DBLE(TMP)
          DEALLOCATE(TMP)
        ELSE
          READ(FILE_ID,IOSTAT=IERR) (RES_VALUE(I),I=1,N)
        ENDIF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'GET_DATA_VALUE_SRF:READ'
          RETURN
        ENDIF
        READ(FILE_ID,IOSTAT=IERR) TAG
        !
        RETURN
      END SUBROUTINE
!
!     Writing functions
!
!***********************************************************************
      SUBROUTINE SET_HEADER_SRF
!***********************************************************************
!
     &(FILE_ID,TITLE,NVAR,VAR_NAME,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the Title and the name and units of the variables
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TITLE          |-->| TITLE OF THE MESH
!| NVAR           |-->| NUMBER OF VARIABLES
!| VAR_NAME       |-->| NAME AND UNITS OF THE VARIABLES
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER,                          INTENT(IN)  :: FILE_ID
        CHARACTER(LEN=TITLE_SIZE),        INTENT(IN)  :: TITLE
        INTEGER,                          INTENT(IN)  :: NVAR
        CHARACTER(LEN=VAR_SIZE),          INTENT(IN)  :: VAR_NAME(NVAR)
        INTEGER,                          INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER ::  TAG,I,SRF_ID
        INTEGER(KIND=K4) :: TMP, TMP2
        !
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'SET_HEADER_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! WRITING} THE TITLE RECORD
        TAG = TITLE_SIZE ! 80 CHARCATERS

        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_TITLE
        WRITE(FILE_ID,POS=MY_POS,IOSTAT=IERR) TAG,TITLE,TAG
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'SET_HEADER_SRF:WRITE:TITLE'
          RETURN
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%POS_NVAR = SRF_OBJ_TAB(SRF_ID)%POS_TITLE
     &                          + 4 + TITLE_SIZE + 4
        !
        ! WRINTING THE NUMBER OF VARIABLE RECORD
        TAG = 2*SRF_OBJ_TAB(SRF_ID)%KS ! TWO INTEGERS
        TMP = NVAR
        TMP2 = 0
        WRITE(FILE_ID,IOSTAT=IERR) TAG,TMP,TMP2,TAG
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'SET_HEADER_SRF:WRITE'
          RETURN
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%POS_VARINFO = SRF_OBJ_TAB(SRF_ID)%POS_NVAR
     &                          + 4 + 2*SRF_OBJ_TAB(SRF_ID)%KS + 4
        !
        ! WRITING THE NAME AND UNITS FOR EACH VARIABLE
        ! I.E. NVAR RECORD OF VAR_SIZE CHARACTERS
        IF(.NOT.ALLOCATED(SRF_OBJ_TAB(SRF_ID)%VAR_LIST)) THEN
          ALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(NVAR),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'ALLOCATING SET_HEADER:SRF_OBJ_TAB%VARLIST'
            RETURN
          ENDIF
        ! Case were user modified nvar between read and write
        ELSE IF(LEN(SRF_OBJ_TAB(SRF_ID)%VAR_LIST).NE.NVAR) THEN
          DEALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST)
          ALLOCATE(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(NVAR),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'ALLOCATING SET_HEADER:SRF_OBJ_TAB%VARLIST'
            RETURN
          ENDIF
        ENDIF
        TAG = VAR_SIZE
        DO I=1, NVAR
          WRITE(FILE_ID,IOSTAT=IERR) TAG,VAR_NAME(I),TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_HEADER_SRF:WRITE'
            RETURN
          ENDIF
          SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I) = VAR_NAME(I)
        ENDDO
        ! NOW WE UPDATE THE POSITION INFORMATION IN SRF_OBJ_TAB
!
        SRF_OBJ_TAB(SRF_ID)%NVAR = NVAR
      END SUBROUTINE
!***********************************************************************
      PURE FUNCTION COMPUTE_NEXT
!***********************************************************************
!
     &(N,H,W) RESULT(NX)
!
!***********************************************************************
!brief    Helper function for IGETMI. Compute the next element to move.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| N              |-->| CURRENT ELEMENT
!| H              |-->| HEIGHT
!| W              |-->| WIDTH
!| NX             |<--| NEXT ELEMENT TO MOVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              INTEGER,INTENT(IN) :: N,H,W
              INTEGER :: NX
              NX = (MOD((N-1),H)*W + (N-1)/H)+1
      END FUNCTION
!***********************************************************************
      SUBROUTINE IGETMI
!***********************************************************************
!
     &(M, H, W)
!
!***********************************************************************
!brief    Integer GEneral Transpose Matrix In-place
!
!         Perform an in-place transpose of a non-square matrix of
!         integers stored in a single dimension array.
!         The subroutine will use h*w bits of auxilary memory but that
!         will be to the detriment of the execution time.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| M              |<->| MATRIX TO TRANSPOSE
!| H              |-->| HEIGHT
!| W              |-->| WIDTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        IMPLICIT NONE
!
        INTEGER, INTENT(INOUT) :: M(:)
        INTEGER, INTENT(IN) :: H,W
!
        INTEGER INTEGER_SIZE,NUM_BYTES,BYTE_INDEX,BIT_INDEX
        INTEGER NUM_BITS, NUM_INTS
        INTEGER I,J,NX,START,TMP,TMP2
        INTEGER, ALLOCATABLE :: MOVED(:)

!
#if defined NAGFOR
        INTEGER_SIZE=4
#else
        INTEGER_SIZE=BIT_SIZE(I)/8!SHOULD RETURN A VALUE BETWEEN
                                  !4 OR 8,MAYBE MORE LATER, BUT IT WILL
                                  !FIT INTO AN INT ANYWAY
#endif
        NUM_BITS=H*W
        NUM_BYTES=NUM_BITS/8
        NUM_INTS=NUM_BYTES/INTEGER_SIZE
        NUM_INTS=NUM_INTS+1
!
        ALLOCATE(MOVED(NUM_INTS))
        MOVED = 0
!
        DO J=2, (H*W)-1
          !FIRST CHECK IF WE HAVE ALREADY MOVED THE NUMBER
          BYTE_INDEX=J/BIT_SIZE(I) +1
          BIT_INDEX=MOD(J,BIT_SIZE(I))
          IF (BTEST(MOVED(BYTE_INDEX), BIT_INDEX )) CYCLE
!
          START=J
          TMP=M(START)
          I=COMPUTE_NEXT(START,H,W)
          NX=COMPUTE_NEXT(I,H,W)
          DO
            TMP2=M(I)
            M(I)=TMP
!
            !MARK THE NUMBER AS MOVED
            BYTE_INDEX=I/BIT_SIZE(I) +1
            BIT_INDEX=MOD(I,BIT_SIZE(I))
            MOVED(BYTE_INDEX)=IBSET(MOVED(BYTE_INDEX),BIT_INDEX)
!
            I=NX
            NX=COMPUTE_NEXT(NX,H,W)
            TMP=TMP2
            IF(I==START) EXIT
          END DO
          M(I)=TMP
        END DO
        DEALLOCATE(MOVED)
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE SET_MESH_SRF
!***********************************************************************
!
     &(FFORMAT,FILE_ID,MESH_DIM,TYPELT,NDP,NPTFR,NPTIR,NELEM,NPOIN,
     & IKLE,IPOBO,KNOLG,X,Y,NPLAN,DATE,TIME,X_ORIG,Y_ORIG,IERR,IN_PLACE)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the mesh geometry in the file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| MESH_DIM       |-->| DIMENSION OF THE MESH
!| TYPELM         |-->| TYPE OF THE MESH ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY POINT
!| NPTIR          |-->| NUMBER OF INTERFACE POINT
!| NELEM          |-->| NUMBER OF ELEMENT IN THE MESH
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| IKLE           |<->| CONNECTIVITY ARRAY FOR THE MAIN ELEMENT
!| IPOBO          |-->| IS A BOUNDARY POINT ? ARRAY
!| KNOLG          |-->| LOCAL TO GLOBAL NUMBERING ARRAY
!| X              |-->| X COORDINATES OF THE MESH POINTS
!| Y              |-->| Y COORDINATES OF THE MESH POINTS
!| NPLAN          |-->| NUMBER OF PLANES
!| DATE           |-->| DATE OF THE CREATION OF THE MESH
!| TIME           |-->| TIME OF THE CREATION OF THE MESH
!| X_ORIG         |-->| Off set of the X coordinates
!| Y_ORIG         |-->| Off set of the Y coordinates
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| Z  (OPTIONAL)  |-->| Z COORDINATES OF THE MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        CHARACTER(LEN=8) ,     INTENT(IN) :: FFORMAT
        INTEGER          ,     INTENT(IN) :: FILE_ID,NPLAN
        INTEGER, DIMENSION(3), INTENT(IN) :: DATE
        INTEGER, DIMENSION(3), INTENT(IN) :: TIME
        INTEGER,               INTENT(IN) :: MESH_DIM
        INTEGER,               INTENT(IN) :: TYPELT
        INTEGER,               INTENT(IN) :: NDP
        INTEGER,               INTENT(IN) :: NPTFR
        INTEGER,               INTENT(IN) :: NPTIR
        INTEGER,               INTENT(IN) :: NELEM
        INTEGER,               INTENT(IN) :: NPOIN
        INTEGER,               INTENT(INOUT) :: IKLE(NDP*NELEM)
        INTEGER,               INTENT(IN) :: IPOBO(*)
        INTEGER,               INTENT(IN) :: KNOLG(*)
        DOUBLE PRECISION,      INTENT(IN) :: X(NPOIN),Y(NPOIN)
        INTEGER,               INTENT(IN) :: X_ORIG,Y_ORIG
        INTEGER,               INTENT(OUT) :: IERR
        LOGICAL, OPTIONAL,     INTENT(IN) :: IN_PLACE
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER :: I,SRF_ID,IKLES_SIZE,IELEM,IPLAN
        INTEGER :: NELEM2
        INTEGER :: IELEMP,IELEMT,NPOIN2,IELEM2
        !
        INTEGER(KIND=K4) :: TMP(10), TAG
        INTEGER(KIND=K4), ALLOCATABLE :: IKLES(:)
        LOGICAL :: TRANSPOSE_IN_PLACE
        !
        IF(PRESENT(IN_PLACE)) THEN
          TRANSPOSE_IN_PLACE=IN_PLACE
        ELSE
          TRANSPOSE_IN_PLACE=.FALSE.
        END IF
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! ASSOCIATE THE ELEMENT TYPE WITH THE NUMBER OF POINT PER ELEMENTS
        SRF_OBJ_TAB(SRF_ID)%TYP_ELT = (TYPELT/10)*10
        SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = 0
        SELECT CASE (SRF_OBJ_TAB(SRF_ID)%TYP_ELT)
          CASE (TRIANGLE_ELT_TYPE) ! TRIANGLES
            SRF_OBJ_TAB(SRF_ID)%NDP = 3
          CASE (QUADRANGLE_ELT_TYPE) ! QUADRANGLES
            SRF_OBJ_TAB(SRF_ID)%NDP = 4
          CASE (PRISM_ELT_TYPE) ! PRISMS
            SRF_OBJ_TAB(SRF_ID)%NDP = 6
          CASE (SPLIT_PRISM_ELT_TYPE) ! PRISMS SPLIT IN TETRAS
            SRF_OBJ_TAB(SRF_ID)%NDP = 6
          CASE DEFAULT
            IERR = UNKNOWN_ELT_TYPE_ERR
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'UNKNOWN ELEMENT TYPE: '//
     &           I2CHAR(SRF_OBJ_TAB(SRF_ID)%TYP_ELT)
            RETURN
        END SELECT
        !
        ! BUILDING THE 10 INTEGERS ARRAY
        !
        IF(NPLAN.LE.1) THEN
          SRF_OBJ_TAB(SRF_ID)%NPLAN = 0
        ELSE
          SRF_OBJ_TAB(SRF_ID)%NPLAN = NPLAN
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%NPTIR = NPTIR
        SRF_OBJ_TAB(SRF_ID)%NPTFR = NPTFR
        SRF_OBJ_TAB(SRF_ID)%NDIM = MESH_DIM
        TMP(1) = 1
        TMP(2) = 0
        TMP(3) = X_ORIG
        TMP(4) = Y_ORIG
        TMP(5) = 0
        TMP(6) = 0
        TMP(7) = SRF_OBJ_TAB(SRF_ID)%NPLAN
        TMP(8) = NPTFR
        TMP(9) = NPTIR
        TMP(10) = 0
        IF((DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3)).NE.0) THEN
          TMP(10) = 1
        ENDIF
        ! WRITING THE RECORD WITH THE TAG
        SRF_OBJ_TAB(SRF_ID)%POS_IB = SRF_OBJ_TAB(SRF_ID)%POS_VARINFO
     &                   + SRF_OBJ_TAB(SRF_ID)%NVAR*(4 + VAR_SIZE + 4)
        MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_IB
        ! Set position for writing
        READ(FILE_ID,POS=MY_POS-4) TAG
        TAG = 10*SRF_OBJ_TAB(SRF_ID)%KS
        WRITE(FILE_ID,IOSTAT=IERR) TAG,TMP,TAG
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'SET_MESH_SRF:WRITE:10 INTEGERS'
          RETURN
        ENDIF

        ! WRITING THE DATE RECORD
        IF(TMP(10).NE.0) THEN
          SRF_OBJ_TAB(SRF_ID)%POS_DATE =
     &           SRF_OBJ_TAB(SRF_ID)%POS_IB
     &          + 4 + 10*SRF_OBJ_TAB(SRF_ID)%KS + 4
          TAG = 6*SRF_OBJ_TAB(SRF_ID)%KS
          WRITE(FILE_ID,IOSTAT=IERR) TAG,DATE,TIME,TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_MESH_SRF:WRITE:DATE'
            RETURN
          ENDIF
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_DATE
     &                           + 4 + 6*SRF_OBJ_TAB(SRF_ID)%KS + 4
        ELSE
          SRF_OBJ_TAB(SRF_ID)%POS_DATE = 0
          SRF_OBJ_TAB(SRF_ID)%POS_NUM = SRF_OBJ_TAB(SRF_ID)%POS_IB
     &                          + 4 + 10*SRF_OBJ_TAB(SRF_ID)%KS + 4
        ENDIF
        !
        ! BUILDING THE 4 INTEGER RECORD
        !
        IF(TYPELT.EQ.50) THEN
          SRF_OBJ_TAB(SRF_ID)%NELEM = NELEM/3
        ELSE
          SRF_OBJ_TAB(SRF_ID)%NELEM = NELEM
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%NPOIN = NPOIN
        TMP(1) = SRF_OBJ_TAB(SRF_ID)%NELEM
        TMP(2) = SRF_OBJ_TAB(SRF_ID)%NPOIN
        TMP(3) = SRF_OBJ_TAB(SRF_ID)%NDP
        TMP(4) = 1
        TAG = 4*SRF_OBJ_TAB(SRF_ID)%KS
        WRITE(FILE_ID,IOSTAT=IERR) TAG,TMP(1:4),TAG
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'SET_MESH_SRF:WRITE:4 INTEGERS'
          RETURN
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%POS_IKLE = SRF_OBJ_TAB(SRF_ID)%POS_NUM
     &                          + (4 + 4*SRF_OBJ_TAB(SRF_ID)%KS + 4)
        !
        ! Writing ikle
        !
        ! Converting ikle->  ikles
        ! Building ikles
        IF(TYPELT.NE.50) THEN
          IKLES_SIZE = NELEM*SRF_OBJ_TAB(SRF_ID)%NDP
        ELSE
!         tetrahedrons regrouped into prisms
!         nelem*2=(nelem/3)*6
          IKLES_SIZE = NELEM*2
        ENDIF
        IF(.NOT. TRANSPOSE_IN_PLACE) THEN
          ALLOCATE(IKLES(IKLES_SIZE),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_MESH_SRF:IKLES'
            RETURN
          ENDIF
        END IF
!       inversion of ikle in ikles
        IF(TYPELT.NE.50) THEN
          IF(.NOT. TRANSPOSE_IN_PLACE) THEN
            DO I = 1,SRF_OBJ_TAB(SRF_ID)%NDP
              DO IELEM = 1,NELEM
                IKLES((IELEM-1)*SRF_OBJ_TAB(SRF_ID)%NDP+I) =
     &                                       IKLE((I-1)*NELEM+IELEM)
              ENDDO
            ENDDO
          ELSE
            CALL IGETMI(IKLE,NELEM,NDP)
          END IF
        ELSE
!         tetrahedrons regrouped into prisms
!         the first tetrahedrons in a layer have the first 3 points like the bottom of the prism
!         ikle is ikle(NELEM,4)
!         ikles is ikles(6,nelem/3)
          NELEM2=NELEM/3/(NPLAN-1)
          NPOIN2=NPOIN/NPLAN
!         loop on layers
          DO IPLAN=1,NPLAN-1
            DO IELEM2=1,NELEM2
!             prism number
              IELEMP=(IPLAN-1)*NELEM2+IELEM2
!             tetrahedron number (the first of the 3 in the prism ielemp)
              IELEMT=(IPLAN-1)*NELEM2*3+IELEM2
!             ikles of the prism
              IKLES((IELEMP-1)*6+1) = IKLE(IELEMT)
              IKLES((IELEMP-1)*6+2) = IKLE(NELEM+IELEMT)
              IKLES((IELEMP-1)*6+3) = IKLE(2*NELEM+IELEMT)
              IKLES((IELEMP-1)*6+4) = IKLE(IELEMT)+NPOIN2
              IKLES((IELEMP-1)*6+5) = IKLE(NELEM+IELEMT)+NPOIN2
              IKLES((IELEMP-1)*6+6) = IKLE(2*NELEM+IELEMT)+NPOIN2
            ENDDO
          ENDDO
        ENDIF
        !
        TAG = IKLES_SIZE*SRF_OBJ_TAB(SRF_ID)%KS
        IF(.NOT. TRANSPOSE_IN_PLACE) THEN
          WRITE(FILE_ID,IOSTAT=IERR)TAG,IKLES(1:IKLES_SIZE),TAG
        ELSE
          WRITE(FILE_ID,IOSTAT=IERR)TAG,IKLE(1:IKLES_SIZE),TAG
        END IF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'SET_MESH_SRF:WRITE:IKLES'
          RETURN
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%POS_IPOBO = SRF_OBJ_TAB(SRF_ID)%POS_IKLE
     &      + (4 +
     &           SRF_OBJ_TAB(SRF_ID)%NELEM*SRF_OBJ_TAB(SRF_ID)%NDP
     &          * SRF_OBJ_TAB(SRF_ID)%KS
     &           + 4)
        IF(.NOT. TRANSPOSE_IN_PLACE) DEALLOCATE(IKLES)
        !
        ! Write ipobo or knolg depending if serial or parallel
        !
        ! We are in serial if both nptfr and nptir are equal to 0
        TAG = NPOIN*SRF_OBJ_TAB(SRF_ID)%KS
        IF(NPTIR.EQ.0) THEN
          WRITE(FILE_ID,IOSTAT=IERR) TAG,IPOBO(1:NPOIN),TAG
        ELSE
          WRITE(FILE_ID,IOSTAT=IERR) TAG,KNOLG(1:NPOIN),TAG
        ENDIF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'SET_MESH_SRF:WRITE:IPOBO/KNOLG'
          RETURN
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%POS_COORD = SRF_OBJ_TAB(SRF_ID)%POS_IPOBO
     &      + (4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%KS + 4)
        !
        ! Writing coordinates
        !
        TAG = SRF_OBJ_TAB(SRF_ID)%RS*NPOIN
        ! convert in real if file in single precision
        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          ! X COORDINATES
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(REAL(X(I)),I=1,NPOIN),TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_MESH_SRF:WRITE:X'
            RETURN
          ENDIF
          ! Y COORDINATES
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(REAL(Y(I)),I=1,NPOIN),TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_MESH_SRF:WRITE:Y'
            RETURN
          ENDIF
        ELSE
          ! X Y COORDINATES
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(X(I),I=1,NPOIN),TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_MESH_SRF:WRITE:X'
            RETURN
          ENDIF
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(Y(I),I=1,NPOIN),TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_MESH_SRF:WRITE:Y'
            RETURN
          ENDIF
        ENDIF
        SRF_OBJ_TAB(SRF_ID)%POS_DATA = SRF_OBJ_TAB(SRF_ID)%POS_COORD
     &      + (4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS + 4)
     &       *2
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA =
     &       4 + SRF_OBJ_TAB(SRF_ID)%NPOIN*SRF_OBJ_TAB(SRF_ID)%RS + 4
        SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET =
     &       4 + SRF_OBJ_TAB(SRF_ID)%RS + 4
     &       + SRF_OBJ_TAB(SRF_ID)%NVAR*SRF_OBJ_TAB(SRF_ID)%SIZE_DATA

      END SUBROUTINE
!***********************************************************************
      SUBROUTINE ADD_DATA_SRF
!***********************************************************************
!
     &(FILE_ID,VAR_NAME,TIME,RECORD,FIRST_VAR,VAR_VALUE,N,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Add data information for a given variable and a given time on
!+        all points of the mesh
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| VAR_NAME       |-->| NAME OF THE VARIABLE
!| TIME           |-->| TIME OF THE DATA
!| RECORD         |-->| TIME STEP OF THE DATA
!| FIRST_VAR      |-->| TRUE IF IT IS THE FIRST VARIABLE OF THE DATASET
!| VAR_VALUE      |-->| THE VALUE FOR EACH POINT OF THE MESH
!| N              |-->| SIZE OF VAR_VALUE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER,          INTENT(IN)  :: FILE_ID,N
        CHARACTER(LEN=VAR_SIZE), INTENT(IN)  :: VAR_NAME
        DOUBLE PRECISION, INTENT(IN)  :: TIME
        INTEGER,          INTENT(IN)  :: RECORD
        LOGICAL,          INTENT(IN)  :: FIRST_VAR
        DOUBLE PRECISION, INTENT(IN)  :: VAR_VALUE(N)
        INTEGER,          INTENT(OUT) :: IERR
        !
        INTEGER(KIND=K8) :: MY_POS
        INTEGER :: SRF_ID,I,IVAR, IREC
        INTEGER(KIND=K4) :: TAG
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'ADD_DATA_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        ! Get the position of the variable
        IVAR = 0
        DO I=1,SRF_OBJ_TAB(SRF_ID)%NVAR
          IF(SRF_OBJ_TAB(SRF_ID)%VAR_LIST(I)(1:16).EQ.VAR_NAME(1:16))
     &    THEN
            IVAR = I
            EXIT
          ENDIF
        ENDDO
        IF(IVAR.EQ.0) THEN
          IERR = HERMES_VAR_UNKNOWN_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'COULD NOT FIND YOUR VARIABLE: '//VAR_NAME
          RETURN
        ENDIF
        IREC = RECORD
        IF(IREC.LT.0) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &         'RECORD: '//I2CHAR(RECORD)//' IS LOWER THAN 0'
          RETURN
        ENDIF
        ! Write time of the dataset if it is the first variable
        IF(FIRST_VAR) THEN
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA
     &            + INT(IREC,K8)*(SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET)
          READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG
          TAG = SRF_OBJ_TAB(SRF_ID)%RS
          IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
            WRITE(FILE_ID,IOSTAT=IERR,IOMSG=ERROR_MESSAGE)
     &                         TAG,REAL(TIME),TAG
          ELSE
            WRITE(FILE_ID,IOSTAT=IERR,IOMSG=ERROR_MESSAGE) TAG,TIME,TAG
          ENDIF
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'ADD_DATA_SRF:WRITE:TIME'//
     &           NEW_LINE('A')//TRIM(ERROR_MESSAGE)
            RETURN
          ENDIF
          ! We only increase the number of timesteps if we add a new done
          ! i.e. irec is greater than ntimesteps
          IF(IREC.EQ.SRF_OBJ_TAB(SRF_ID)%NTIMESTEP) THEN
            SRF_OBJ_TAB(SRF_ID)%NTIMESTEP =
     &                   SRF_OBJ_TAB(SRF_ID)%NTIMESTEP + 1
          ENDIF
        ELSE
          MY_POS = SRF_OBJ_TAB(SRF_ID)%POS_DATA
     &            + INT(IREC,K8)*(SRF_OBJ_TAB(SRF_ID)%SIZE_DATA_SET)
     &            + 4 + SRF_OBJ_TAB(SRF_ID)%RS + 4 ! THE TIME VALUE
     &            + INT(IVAR-1,K8) * SRF_OBJ_TAB(SRF_ID)%SIZE_DATA
          ! Using a read to go to position in file
          READ(FILE_ID,POS=MY_POS-4,IOSTAT=IERR) TAG

        ENDIF
        ! If the file is in single precision we convert the data

        IF(SRF_OBJ_TAB(SRF_ID)%RS.EQ.4) THEN
          TAG = SRF_OBJ_TAB(SRF_ID)%RS*N
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(REAL(VAR_VALUE(I)),I=1,N),TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'ADD_DATA_SRF:WRITE:VALUE'
            RETURN
          ENDIF
        ELSE
          ! write time of the dataset if it is the first variable
          ! write the data for that variable
          TAG = SRF_OBJ_TAB(SRF_ID)%RS*N
          WRITE(FILE_ID,IOSTAT=IERR) TAG,(VAR_VALUE(I),I=1,N),TAG
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'ADD_DATA_SRF:WRITE'
            RETURN
          ENDIF
        ENDIF
        !
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE SET_BND_SRF
!***********************************************************************
!
     &(FILE_ID,TYPE_BND_ELT,NELEBD,NDP,IKLE,
     & LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     & LITBOR,TBOR,ATBOR,BTBOR,COLOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the boundary information into the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TYPE_BND_ELT   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER BOUNDARY ELEMENT
!| IKLE           |-->| CONNECTIVITY ARRAY FOR THE BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER,          INTENT(IN)  :: FILE_ID
        INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
        INTEGER,          INTENT(IN)  :: NELEBD
        INTEGER,          INTENT(IN)  :: NDP
        INTEGER,          INTENT(IN)  :: IKLE(NELEBD*NDP)
        INTEGER,          INTENT(IN)  :: LIUBOR(NELEBD),LIVBOR(NELEBD)
        INTEGER,          INTENT(IN)  :: LIHBOR(NELEBD),LITBOR(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: UBOR(NELEBD),VBOR(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: HBOR(NELEBD),CHBORD(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: TBOR(NELEBD),ATBOR(NELEBD)
        DOUBLE PRECISION, INTENT(IN)  :: BTBOR(NELEBD)
        INTEGER,          INTENT(IN)  :: COLOR(NELEBD)
        INTEGER,          INTENT(OUT) :: IERR
        !
        INTEGER :: SRF_ID, I, NCLI
        !
        CALL GET_OBJ(HASH,FILE_ID,SRF_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'SET_BND_SRF:GET_OBJ'
          RETURN
        ENDIF
        !
        NCLI = SRF_OBJ_TAB(SRF_ID)%NCLI
        SRF_OBJ_TAB(SRF_ID)%TYP_BND_ELT = TYPE_BND_ELT
        REWIND(NCLI)
        !If the file is a concatenation, we need to move to the
        !begining of our part
        IF(PARTEL_CONCAT)THEN
          DO I=1,SRF_OBJ_TAB(SRF_ID)%CLI_LINE_BEGIN-1
            READ(SRF_OBJ_TAB(SRF_ID)%NCLI,*)
          ENDDO
        ENDIF
        DO I=1,NELEBD
          ! Write connectivity and bnoundary value the rest is set to 0.D0
          WRITE(NCLI,4000,IOSTAT=IERR) LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &                         HBOR(I),UBOR(I),VBOR(I),
     &                         CHBORD(I),LITBOR(I),
     &                         TBOR(I),ATBOR(I),BTBOR(I),
     &                         IKLE(I),COLOR(I)
 4000       FORMAT (1X,I2,1X,2(I1,1X),3(F24.12,1X),1X,
     &           F24.12,3X,I1,1X,3(F24.12,1X),1I9,1X,1I9,
     &           1X,I10,1X,2(F27.15,1X),I8)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &           TRIM(SRF_OBJ_TAB(SRF_ID)%FILE_NAME)//': '//
     &           'SET_BND_SRF:WRITE:NCLI'
            RETURN
          ENDIF
          SRF_OBJ_TAB(SRF_ID)%NPTFR=NELEBD
        ENDDO

      END SUBROUTINE
!
!***********************************************************************
      FUNCTION ISOPENED
!***********************************************************************
     &(FILE_ID)
!
!***********************************************************************
! HERMES   V8P1                                               15/11/2019
!***********************************************************************
!
!brief    Check is a file descriptor is opened.
!         Return False if the file descriptor happened to be stdin,
!         stdout or stderr
!
!history  Judicael Grasset (STFC-DL)
!+        15/11/2019
!+        V8P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| ISOPENED       |<--| RETURN VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE ISO_FORTRAN_ENV, ONLY : INPUT_UNIT,OUTPUT_UNIT,ERROR_UNIT
        INTEGER, INTENT(IN) :: FILE_ID
        LOGICAL :: ISOPENED
        IF(FILE_ID==INPUT_UNIT.OR.FILE_ID==OUTPUT_UNIT
     &       .OR.FILE_ID==ERROR_UNIT)THEN
          ISOPENED=.FALSE.
        ELSE
          INQUIRE(UNIT=FILE_ID,OPENED=ISOPENED)
        ENDIF
      END FUNCTION ISOPENED
!
      END MODULE UTILS_SERAFIN
