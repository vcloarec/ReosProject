!                 *****************
                  MODULE UTILS_CGNS
!                 *****************
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE HASH_TABLE
!
      IMPLICIT NONE
#if defined(HAVE_CGNS)
      INCLUDE 'cgnslib_f.h'
#endif
!
      INTEGER, PARAMETER :: VAR_SIZE = 32 ! SIZE OF A VARIABLE TEXT
      INTEGER, PARAMETER :: TITLE_SIZE = 80 ! SIZE OF A TITLE
!
      TYPE CGNS_INFO
        ! SIZE OF ELEMENTS
        CHARACTER(LEN=250) :: FILE_NAME
        CHARACTER(LEN=32) :: TITLE
        INTEGER :: INDEX_ZONE
        INTEGER :: INDEX_BASE
        INTEGER :: INDEX_SECTION
        INTEGER :: INDEX_FLOW
        INTEGER :: INDEX_FIELD
        INTEGER :: NELEM
        INTEGER :: NPOIN
        INTEGER :: NDP
        INTEGER :: TYPE_ELEM
        INTEGER :: NDIM
        INTEGER :: NVAR
        INTEGER :: NTIMESTEP
        DOUBLE PRECISION, ALLOCATABLE :: TIMES(:)
        CHARACTER(LEN=VAR_SIZE), ALLOCATABLE :: VARNAME(:)
        LOGICAL :: WRITE_AT_END
        INTEGER :: NCLI
        INTEGER :: NPTFR
      END TYPE CGNS_INFO

      ! HASH TABLE FOR CGNS FILES
      INTEGER :: HASH(MAX_FILE) = 0
      TYPE(CGNS_INFO) :: CGNS_OBJ_TAB(MAX_FILE)
!
!-----------------------------------------------------------------------
!
      CONTAINS
!***********************************************************************

!***********************************************************************
      SUBROUTINE OPEN_MESH_CGNS
!***********************************************************************
!
     &(FILE_NAME,FILE_ID,OPENMODE,IERR)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |-->| NAME OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','READWRITE'
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(OUT) :: FILE_ID
        CHARACTER(LEN=9), INTENT(IN)     :: OPENMODE
        CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        CHARACTER(LEN=32) :: CGNSNAME
        INTEGER :: IPHYSDIM
        INTEGER :: ISIZE(3,3)
        INTEGER :: NBDYELEM
        INTEGER :: TYPGEO
        INTEGER :: PARENT_FLAG
        INTEGER :: ISTART, IEND
        INTEGER CGNS_OPENMODE
        !
        ! ADD A NEW FILE TO THE HASH TABLE
        !
        IF(OPENMODE(1:5).EQ.'WRITE') THEN
          CGNS_OPENMODE = CG_MODE_WRITE
        ELSE IF(OPENMODE.EQ.'READWRITE') THEN
          CGNS_OPENMODE = CG_MODE_MODIFY
        ELSE
          CGNS_OPENMODE = CG_MODE_READ
        ENDIF
        !
        CALL CG_OPEN_F(FILE_NAME,CGNS_OPENMODE,FILE_ID,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//TRIM(FILE_NAME)//': '//
     &                    'OPEN_MESH_CGNS:CG_OPEN_F'
          RETURN
        ENDIF
        !
        CALL ADD_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//TRIM(FILE_NAME)//': '//
     &         'OPEN_MESH_CGNS:ADD_CGNS_FILE'
          RETURN
        ENDIF
        CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME = FILE_NAME
        !
        CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP = 0
        IF(OPENMODE(1:5).EQ.'WRITE') THEN
          CGNS_OBJ_TAB(CGNS_ID)%WRITE_AT_END = .TRUE.
        ELSE IF(OPENMODE.EQ.'READWRITE') THEN
          CGNS_OBJ_TAB(CGNS_ID)%WRITE_AT_END = .TRUE.
        ELSE
          CGNS_OBJ_TAB(CGNS_ID)%WRITE_AT_END = .FALSE.
        ENDIF
        IF(OPENMODE(1:5).EQ.'READ ') THEN
          !READ BASE
          CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE = 1
          CALL CG_BASE_READ_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                        CGNSNAME,CGNS_OBJ_TAB(CGNS_ID)%NDIM,
     &                        IPHYSDIM,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH_CGNS:CG_BASE_READ_F'
            RETURN
          ENDIF
          ! READ ZONE INFORMATION
          CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE = 1
          CALL CG_ZONE_READ_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,CGNSNAME,
     &                        ISIZE(1,:),IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH_CGNS:CG_ZONE_READ_F'
            RETURN
          ENDIF
          CGNS_OBJ_TAB(CGNS_ID)%NPOIN = ISIZE(1,1)
          CGNS_OBJ_TAB(CGNS_ID)%TITLE = CGNSNAME
          ! IDENTIFY THE ELEMENT TYPE
          CGNS_OBJ_TAB(CGNS_ID)%INDEX_SECTION = 1
          CALL CG_SECTION_READ_F(FILE_ID,
     &       CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &       CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &       CGNS_OBJ_TAB(CGNS_ID)%INDEX_SECTION,CGNSNAME,TYPGEO,
     &       ISTART,IEND,NBDYELEM,
     &       PARENT_FLAG,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH_CGNS:CG_SECTION_READ_F'
            RETURN
          ENDIF
          ! COUNTING THE NUMBER OF ELEMENTS
          CGNS_OBJ_TAB(CGNS_ID)%NELEM = IEND
          SELECT CASE (TYPGEO)
          CASE(TRI_3)
            CGNS_OBJ_TAB(CGNS_ID)%TYPE_ELEM = TRIANGLE_ELT_TYPE
            CGNS_OBJ_TAB(CGNS_ID)%NDP = 3
          CASE(QUAD_4)
            CGNS_OBJ_TAB(CGNS_ID)%TYPE_ELEM = QUADRANGLE_ELT_TYPE
            CGNS_OBJ_TAB(CGNS_ID)%NDP = 4
          CASE(TETRA_4)
            CGNS_OBJ_TAB(CGNS_ID)%TYPE_ELEM = TETRAHEDRON_ELT_TYPE
            CGNS_OBJ_TAB(CGNS_ID)%NDP = 4
          CASE(PENTA_6)
            CGNS_OBJ_TAB(CGNS_ID)%TYPE_ELEM = PRISM_ELT_TYPE
            CGNS_OBJ_TAB(CGNS_ID)%NDP = 6
          END SELECT
          ! READING THE NUMBER OF TIMESTEP
          CALL CG_BITER_READ_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &             CGNSNAME,CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH:CG_BITER_READ_F'
            RETURN
          ENDIF
          ! IF WE HAVE RESULTS
          IF(CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP.NE.0) THEN
            CALL CG_NFIELDS_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,1,
     &                        CGNS_OBJ_TAB(CGNS_ID)%NVAR,IERR)
            IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH:CG_NFIELDS_F'
              RETURN
            ENDIF
            ! READING THE TIME TABLE
            ! GO TO BASEITERATIVEDATA LEVEL
            CALL CG_GOTO_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                     IERR,'BaseIterativeData_t',
     &                     1,'end')
            IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH:CG_GOTO_F'
              RETURN
            ENDIF
            ALLOCATE(CGNS_OBJ_TAB(CGNS_ID)%TIMES(
     &                    CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP),STAT=IERR)
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'ALLOCATING TIMES'
              RETURN
            ENDIF
            ! THERE IS NORAMLY ONLY ONE TABLE UNDER THE BASE ITERATIVE NODE
            CALL CG_ARRAY_READ_F(1,CGNS_OBJ_TAB(CGNS_ID)%TIMES,IERR)
            IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_MESH:CG_ARRAY_READ_F'
              RETURN
            ENDIF
          ENDIF
        ELSE
          CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE = 1
          CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE = 1
          CGNS_OBJ_TAB(CGNS_ID)%INDEX_SECTION = 1
        ENDIF
#else
!
!     CGNS LIBRARY NOT LOADED
      FILE_ID = 0
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif

        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE OPEN_BND_CGNS
!***********************************************************************
!
     &(FILE_NAME,FILE_ID,OPENMODE,IERR)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_NAME      |-->| NAME OF THE BOUNDARY FILE
!| FILE_ID        |-->| FILE DESCRIPTOR OF THE "MESH" FILE
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','WRITE','READWRITE'
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        CHARACTER(LEN=*), INTENT(IN) :: FILE_NAME
        INTEGER,          INTENT(IN) :: FILE_ID
        CHARACTER(LEN=9), INTENT(IN) :: OPENMODE
        INTEGER, INTENT(OUT)         :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER CGNS_ID,I
        INTEGER :: NPTFR
        LOGICAL :: ISOPENED, IS_USED
        !
        ISOPENED = .FALSE.
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(FILE_NAME)//': '//
     &         'OPEN_BND_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! Open the boundary file with a set id available
        ! First we check if the file is already opened
        ! Telemac is using one boundary file for all the mesh file
        ! so it could have been opened by another mesh before hand
        INQUIRE(FILE=FILE_NAME,OPENED=ISOPENED)
        IF(ISOPENED) THEN
          ! Id the file is already opened get its id
          INQUIRE(FILE=FILE_NAME,NUMBER=CGNS_OBJ_TAB(CGNS_ID)%NCLI)
        ELSE
          ! Otherwise open the file
          CALL GET_FREE_ID(CGNS_OBJ_TAB(CGNS_ID)%NCLI)
          OPEN(UNIT=CGNS_OBJ_TAB(CGNS_ID)%NCLI,FILE=FILE_NAME,
     &         FORM='FORMATTED',ACTION=OPENMODE,IOSTAT=IERR)
        ENDIF
        ! If we are not in write only
        ! We compute the number of boundary point i.e. number of line in the file
        IF(OPENMODE(1:5).NE.'WRITE') THEN
          REWIND(CGNS_OBJ_TAB(CGNS_ID)%NCLI)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_BND_CGNS:OPEN'
            RETURN
          ENDIF
          NPTFR = 0
          DO
            READ(CGNS_OBJ_TAB(CGNS_ID)%NCLI,*,IOSTAT=IERR)
            IF (IERR.LT.0) THEN
              ! END OF FILE REACHED
              EXIT
            ELSE IF (IERR.GT.0) THEN
              ! Error during read
              IF(IERR.NE.0) THEN
                ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'OPEN_BND_CGNS:READ'
                RETURN
              ENDIF
            ENDIF
            NPTFR = NPTFR + 1
          ENDDO
          IERR = 0
          CGNS_OBJ_TAB(CGNS_ID)%NPTFR = NPTFR
        ENDIF
        RETURN
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE CLOSE_BND_CGNS
!***********************************************************************
!
     &(FILE_ID,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!BRIEF    CLOSES A BOUNDARY FILE
!
!HISTORY  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER,          INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT)         :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER CGNS_ID
        LOGICAL ISOPENED
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH FILE ID '//I2CHAR(FILE_ID)//': '//
     &         'CLOSE_BND_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ISOPENED = .FALSE.
        ! Check if the file is still opened as it could have been closed
        ! by another mesh file (see open_bnd_CGNS for more information)
        INQUIRE(UNIT=CGNS_OBJ_TAB(CGNS_ID)%NCLI,OPENED=ISOPENED)
        IERR = 0
        IF(ISOPENED) THEN
          CLOSE(CGNS_OBJ_TAB(CGNS_ID)%NCLI,IOSTAT=IERR)
        ENDIF
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_BND_CGNS:CLOSE'
          RETURN
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE CLOSE_MESH_CGNS
!***********************************************************************
!
     &(FILE_ID,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!BRIEF    CLOSES A MESH FILE
!
!HISTORY  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_ID        |-->| FILE DESCRIPTOR
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined HAVE_CGNS
        INTEGER CGNS_ID,I
        INTEGER :: IDATA(2)
        CHARACTER(LEN=32), ALLOCATABLE :: SOLNAME(:)
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'CLOSE_MESH:GET_CGNS_OBJ'
          RETURN
        ENDIF
        ! If we are writing a file we need to output this at the end
        IF (CGNS_OBJ_TAB(CGNS_ID)%WRITE_AT_END) THEN
          ! CREATE BASEITERATIVEDATA
          CALL CG_BITER_WRITE_F(FILE_ID,
     &         CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,'TimeIterValues',
     &         CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_BITER_WRITE_F'
            RETURN
          ENDIF
          ! GO TO BASEITERATIVEDATA LEVEL AND WRITE TIME VALUES
          CALL CG_GOTO_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,IERR,
     &                   'BaseIterativeData_t',
     &                   1,'end')
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_GOTO_F'
            RETURN
          ENDIF
          CALL CG_ARRAY_WRITE_F('TimeValues',REALDOUBLE,1,
     &               CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP,
     &               CGNS_OBJ_TAB(CGNS_ID)%TIMES,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_ARRAY_WRITE_F'
            RETURN
          ENDIF
          ! CREATE ZONEITERATIVEDATA
          CALL CG_ZITER_WRITE_F(FILE_ID,
     &              CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &              CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &              'ZoneIterativeData',IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_ZITER_WRITE_F'
            RETURN
          ENDIF
          ! GO TO ZONEITERATIVEDATA LEVEL AND GIVE INFO TELLING WHICH
          ! FLOW SOLUTION CORRESPONDS WITH WHICH TIME (SOLNAME(1) CORRESPONDS
          ! WITH TIME(1), SOLNAME(2) WITH TIME(2), AND SOLNAME(3) WITH TIME(3))

          ALLOCATE(SOLNAME(CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'ALLOCATING SOLNAME'
            RETURN
          ENDIF
          DO I=1,CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP
            SOLNAME = REPEAT(' ',32)
            SOLNAME(I) = 'Time'//I2CHAR(I)
          ENDDO
          CALL CG_GOTO_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &             IERR,'Zone_t',
     &             CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &             'ZoneIterativeData_t',1,'end')
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_GOTO_F BIS'
            RETURN
          ENDIF
          IDATA(1)=32
          IDATA(2)=CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP
          CALL CG_ARRAY_WRITE_F('FlowSolutionPointers',CHARACTER,
     &                       2,IDATA,SOLNAME,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_ARRAY_WRITE_F BIS'
            RETURN
          ENDIF
          ! ADD SIMULATIONTYPE
          CALL CG_SIMULATION_TYPE_WRITE_F(FILE_ID,
     &                 CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                 TIMEACCURATE,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_SIMULATION_TYPE_WRITE_F'
            RETURN
          ENDIF
          DEALLOCATE(SOLNAME)
        ENDIF
        ! CLOSE CGNS CFILE
        CALL CG_CLOSE_F(FILE_ID,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'CLOSE_MESH:CG_CLOSE_F'
          RETURN
        ENDIF
        ! Deallocating arrays
        IF(ALLOCATED(CGNS_OBJ_TAB(CGNS_ID)%TIMES)) THEN
          DEALLOCATE(CGNS_OBJ_TAB(CGNS_ID)%TIMES)
        ENDIF
        IF(ALLOCATED(CGNS_OBJ_TAB(CGNS_ID)%VARNAME)) THEN
          DEALLOCATE(CGNS_OBJ_TAB(CGNS_ID)%VARNAME)
        ENDIF
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!     Mesh functions
!
!***********************************************************************
      SUBROUTINE GET_MESH_TITLE_CGNS
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
        CHARACTER(LEN=TITLE_SIZE), INTENT(INOUT) :: TITLE
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER(KIND=K8) :: MY_POS
        INTEGER          :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'CLOSE_MESH:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        TITLE = REPEAT(' ',TITLE_SIZE)
        TITLE(1:32) = CGNS_OBJ_TAB(CGNS_ID)%TITLE
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NELEM_CGNS
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
        INTEGER, INTENT(INOUT) :: NELEM
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NELEM_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        NELEM = CGNS_OBJ_TAB(CGNS_ID)%NELEM
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPOIN_PER_ELEMENT_CGNS
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
        INTEGER, INTENT(INOUT) :: NDP
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPOIN_PER_ELEMENT_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        NDP = CGNS_OBJ_TAB(CGNS_ID)%NDP
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_CONNECTIVITY_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        INTEGER, ALLOCATABLE :: IELEM(:,:)
        INTEGER :: PARENT(1)
        INTEGER :: I,J
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_CONNECTIVITY_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ALLOCATE(IELEM(CGNS_OBJ_TAB(CGNS_ID)%NDP,
     &                 CGNS_OBJ_TAB(CGNS_ID)%NELEM),
     &           STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'ALLOCATING IELEM'
          RETURN
        ENDIF
        ! TODO: Check unallocated parent will cause bug in debug
        ! READING THE CONNECTIVITY TABLE
        CALL CG_ELEMENTS_READ_F(FILE_ID,
     &            CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &            CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &            CGNS_OBJ_TAB(CGNS_ID)%INDEX_SECTION,
     &            IELEM,PARENT,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_MESH_CONNECTIVITY_CGNS:CG_ELEMENTS_READ_F'
          RETURN
        ENDIF
        ! BUILDING THE CONNECTIVITY TABLE
        DO I=1,CGNS_OBJ_TAB(CGNS_ID)%NELEM
          DO J=1,CGNS_OBJ_TAB(CGNS_ID)%NDP
            IKLE((I-1)*CGNS_OBJ_TAB(CGNS_ID)%NDP+J) = IELEM(J,I)
          ENDDO
        ENDDO
        DEALLOCATE(IELEM)
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPOIN_CGNS
!***********************************************************************
!
     &(FILE_ID,TYP_ELT,NPOIN,IERR)
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
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NPOIN          |<->| THE NUMBER OF POINTS
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: FILE_ID
        INTEGER, INTENT(IN) :: TYP_ELT
        INTEGER, INTENT(INOUT) :: NPOIN
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPOIN_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        NPOIN = CGNS_OBJ_TAB(CGNS_ID)%NPOIN
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPLAN_CGNS
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
        INTEGER, INTENT(INOUT) :: NPLAN
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPLAN_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! TODO: See to that
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_DIMENSION_CGNS
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
        INTEGER, INTENT(INOUT) :: NDIM
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_DIMENSION_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        NDIM = CGNS_OBJ_TAB(CGNS_ID)%NDIM
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_COORD_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER          :: CGNS_ID
        CHARACTER(LEN=11) :: COORDNAME
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_COORD_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        SELECT CASE(JDIM)
          CASE(1)
            COORDNAME = 'CoordinateX'
          CASE(2)
            COORDNAME = 'CoordinateY'
          CASE(3)
            COORDNAME = 'CoordinateZ'
        END SELECT
        CALL CG_COORD_READ_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &                        COORDNAME,
     &                        REALDOUBLE,1,NPOIN,COORD,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_MESH_COORD_CGNS:CG_COORD_READ_F'
          RETURN
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_L2G_NUMBERING_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER          :: CGNS_ID
        INTEGER :: DISCR_NO, INDEXU
        INTEGER :: INDEX_DIM
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_L2G_NUMBERING_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        DISCR_NO = 1

        INDEXU = 1

        CALL CG_GOTO_F(FILE_ID, CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                 IERR, 'Zone_t', CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &                 'DiscreteData_t', DISCR_NO,
     &                 'UserDefinedData_t', INDEXU, 'end')
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_MESH_L2G_NUMBERING:CG_GOTO_F'
          RETURN
        ENDIF

        INDEX_DIM = 1
        CALL CG_ARRAY_READ_F(KNOLG, INTEGER, IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_MESH_L2G_NUMBERING_CGNS:CG_ARRAY_READ_F'
          RETURN
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_MESH_NPTIR_CGNS
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
        INTEGER, INTENT(INOUT) :: NPTIR
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_MESH_NPTIR_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! TODO: Get how to add this in the format
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!     Boundary functions
!
!***********************************************************************
      SUBROUTINE GET_BND_IPOBO_CGNS
!***********************************************************************
!
     &(FILE_ID,TYPE_BND_ELEM,NPOIN,IPOBO,IERR)
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
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENT
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
        INTEGER, INTENT(IN) :: TYPE_BND_ELEM
        INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER          :: CGNS_ID
        INTEGER          :: IPTFR
        INTEGER :: IDUM, I
        DOUBLE PRECISION :: DDUM
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_IPOBO_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(POINT_BND_ELT_TYPE.NE.TYPE_BND_ELEM) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          RETURN
        ENDIF
        ! Read the boundary file we only care about the connectivity
        REWIND(CGNS_OBJ_TAB(CGNS_ID)%NCLI)
        DO I=1,CGNS_OBJ_TAB(CGNS_ID)%NPTFR
          READ(CGNS_OBJ_TAB(CGNS_ID)%NCLI,*,IOSTAT=IERR) IDUM,IDUM,IDUM,
     &                         DDUM  ,DDUM  ,DDUM,
     &                         DDUM ,IDUM,DDUM,DDUM,DDUM,
     &                         IPTFR,IDUM
          IPOBO(IPTFR) = I
          IF(IERR.LT.0) THEN
            ! End of file reached
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_BND_CONNECTIVITY_CGNS:READ:END OF FILE'
              RETURN
            ENDIF
          ELSE IF (IERR.GT.0) THEN
            ! Error during read
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_BND_CONNECTIVITY_CGNS:READ'
              RETURN
            ENDIF
          ENDIF
        ENDDO
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE GET_BND_NUMBERING_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER :: TYP_ELEM
        INTEGER :: CGNS_ID
        !
        TYP_ELEM = 1
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_IPOBO_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        CALL GET_BND_CONNECTIVITY_CGNS(FILE_ID,TYP_ELEM_BND,NPTFR,
     &                                TYP_ELEM,NBOR,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_BND_NUMBERING_CGNS:GET_BND_CONNECTIVITY_CGNS'
          RETURN
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
      SUBROUTINE GET_BND_NELEM_CGNS(FILE_ID,TYPE_BND_ELEM, NELEM,IERR)
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
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_NELEM_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(POINT_BND_ELT_TYPE.NE.TYPE_BND_ELEM) THEN
          NELEM = 0
          RETURN
        ELSE
          NELEM = CGNS_OBJ_TAB(CGNS_ID)%NPTFR
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_CONNECTIVITY_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        INTEGER :: IDUM, I
        DOUBLE PRECISION :: DDUM
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_CONNECTIVITY_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(POINT_BND_ELT_TYPE.NE.TYP_BND_ELT) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          RETURN
        ENDIF
        ! Read the boundary file we only care about the connectivity
        REWIND(CGNS_OBJ_TAB(CGNS_ID)%NCLI)
        DO I=1,CGNS_OBJ_TAB(CGNS_ID)%NPTFR
          READ(CGNS_OBJ_TAB(CGNS_ID)%NCLI,*,IOSTAT=IERR) IDUM,IDUM,IDUM,
     &                         DDUM  ,DDUM  ,DDUM,
     &                         DDUM ,IDUM,DDUM,DDUM,DDUM,
     &                         IKLE(I),IDUM
          IF(IERR.LT.0) THEN
            ! End of file reached
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_BND_CONNECTIVITY_CGNS:READ:END OF FILE'
              RETURN
            ENDIF
          ELSE IF (IERR.GT.0) THEN
            ! Error during read
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_BND_CONNECTIVITY_CGNS:READ'
              RETURN
            ENDIF
          ENDIF
        ENDDO
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_VALUE_CGNS
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
        DOUBLE PRECISION, INTENT(INOUT) :: TBOR(NPTFR),ATBOR(NPTFR)
        DOUBLE PRECISION, INTENT(INOUT) :: BTBOR(NPTFR)
        LOGICAL, INTENT(IN)    :: TRAC
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        INTEGER :: I, IDUM
        DOUBLE PRECISION :: DDUM
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_VALUE_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! Checking that we have the right element type
        IF(POINT_BND_ELT_TYPE.NE.TYP_BND_ELEM) THEN
          IERR = HERMES_WRONG_ELEMENT_TYPE_ERR
          RETURN
        ENDIF
        ! Reading the boundary file informations we only care
        ! about the boundary type li[huvt]bor
        REWIND(CGNS_OBJ_TAB(CGNS_ID)%NCLI)
        DO I=1,CGNS_OBJ_TAB(CGNS_ID)%NPTFR
          IF(TRAC) THEN
            READ(CGNS_OBJ_TAB(CGNS_ID)%NCLI,*,IOSTAT=IERR)
     &             LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &             HBOR(I)  ,UBOR(I)  ,VBOR(I),
     &             CHBORD(I) ,LITBOR(I),
     &             TBOR(I),ATBOR(I),BTBOR(I),
     &             IDUM,IDUM
          ELSE
            READ(CGNS_OBJ_TAB(CGNS_ID)%NCLI,*,IOSTAT=IERR)
     &             LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &             HBOR(I)  ,UBOR(I)  ,VBOR(I),
     &             CHBORD(I) ,IDUM,DDUM,DDUM,DDUM,
     &             IDUM,IDUM
          ENDIF
          IF(IERR.LT.0) THEN
            ! End of file reached
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_BND_VALUE_CGNS:READ:END OF FILE'
              RETURN
            ENDIF
          ELSE IF (IERR.GT.0) THEN
            ! Error during read
            IF(IERR.NE.0) THEN
              ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_BND_VALUE_CGNS:READ'
              RETURN
            ENDIF
          ENDIF
        ENDDO
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_BND_NPOIN_CGNS
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
        INTEGER, INTENT(INOUT) :: NPTFR
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_BND_NPOIN_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        IF(POINT_BND_ELT_TYPE.EQ.TYPE_BND_ELEM) THEN
          NPTFR = CGNS_OBJ_TAB(CGNS_ID)%NPTFR
        ELSE
          NPTFR = 0
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!     Data functions
!
!***********************************************************************
      SUBROUTINE GET_DATA_NVAR_CGNS
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
        INTEGER, INTENT(INOUT) :: NVAR
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_NVAR_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        NVAR = CGNS_OBJ_TAB(CGNS_ID)%NVAR
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_VAR_LIST_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        INTEGER :: IVAR
        CHARACTER(LEN=32) :: VARNAME
        INTEGER :: DATATYPE
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_VAR_LIST_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        ! TODO: Move into open
        DO IVAR=1,NVAR
          CALL CG_FIELD_INFO_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                         CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,1,
     &                         IVAR,DATATYPE,VARNAME,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_DATA_VAR_LIST:CG_FIELD_INFO_F '//I2CHAR(IVAR)
            RETURN
          ENDIF
          VAR_LIST(IVAR) = VARNAME(1:16)
          UNIT_LIST(IVAR) = VARNAME(16:32)
        ENDDO
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_NTIMESTEP_CGNS
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
        INTEGER, INTENT(INOUT) :: NTIMESTEP
        INTEGER, INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_NTIMESTEP_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        NTIMESTEP = CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_TIME_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER          :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_TIME_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        IF (RECORD.LT.0) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &       'GET_DATA_VAR_TIME_CGNS'
          RETURN
        ENDIF
        IF (RECORD.GE.CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          ERROR_MESSAGE = 'ERROR IN '//
     &       TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &       'GET_DATA_VAR_TIME_CGNS'
          RETURN
        ENDIF
        TIME = CGNS_OBJ_TAB(CGNS_ID)%TIMES(RECORD+1)
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE GET_DATA_VALUE_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER          :: CGNS_ID
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'GET_DATA_VALUE_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        ! TODO: Check on record and var_name
        !
        CALL CG_FIELD_READ_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                       CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,RECORD+1,
     &                       VAR_NAME,REALDOUBLE,1,N,
     &                       RES_VALUE,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'GET_DATA_VALUE_CGNS:CG_FIELD_READ_F'
          RETURN
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
        RETURN
      END SUBROUTINE
!
!     Writing functions
!
!***********************************************************************
      SUBROUTINE SET_HEADER_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        INTEGER :: I
        !
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'SET_HEADER_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        CGNS_OBJ_TAB(CGNS_ID)%TITLE = TITLE(1:32)
        CGNS_OBJ_TAB(CGNS_ID)%NVAR = NVAR
        ALLOCATE(CGNS_OBJ_TAB(CGNS_ID)%VARNAME(NVAR),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'ALLOCATING VARNAME'
          RETURN
        ENDIF
        DO I=1,NVAR
          CGNS_OBJ_TAB(CGNS_ID)%VARNAME(I) = VAR_NAME(I)
        ENDDO
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif

      END SUBROUTINE
!***********************************************************************
      SUBROUTINE SET_MESH_CGNS
!***********************************************************************
!
     &(FILE_ID,MESH_DIM,TYPELT,NDP,NPTFR,NPTIR,NELEM,NPOIN,
     & IKLE,IPOBO,KNOLG,X,Y,NPLAN,IERR,Z)
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
!| FILE_ID        |-->| FILE DESCRIPTOR
!| MESH_DIM       |-->| DIMENSION OF THE MESH
!| TYPELM         |-->| TYPE OF THE MESH ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY POINT
!| NPTIR          |-->| NUMBER OF INTERFACE POINT
!| NELEM          |-->| NUMBER OF ELEMENT IN THE MESH
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| IKLE           |-->| CONNECTIVITY ARRAY FOR THE MAIN ELEMENT
!| IPOBO          |-->| IS A BOUNDARY POINT ? ARRAY
!| KNOLG          |-->| LOCAL TO GLOBAL NUMBERING ARRAY
!| X              |-->| X COORDINATES OF THE MESH POINTS
!| Y              |-->| Y COORDINATES OF THE MESH POINTS
!| NPLAN          |-->| NUMBER OF PLANES
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| Z  (OPTIONAL)  |-->| Z COORDINATES OF THE MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        INTEGER          ,     INTENT(IN) :: FILE_ID,NPLAN
        INTEGER,               INTENT(IN) :: MESH_DIM
        INTEGER,               INTENT(IN) :: TYPELT
        INTEGER,               INTENT(IN) :: NDP
        INTEGER,               INTENT(IN) :: NPTFR
        INTEGER,               INTENT(IN) :: NPTIR
        INTEGER,               INTENT(IN) :: NELEM
        INTEGER,               INTENT(IN) :: NPOIN
        INTEGER,               INTENT(IN) :: IKLE(NDP*NELEM)
        INTEGER,               INTENT(IN) :: IPOBO(*)
        INTEGER,               INTENT(IN) :: KNOLG(*)
        DOUBLE PRECISION,      INTENT(IN) :: X(NPOIN),Y(NPOIN)
        INTEGER,               INTENT(OUT) :: IERR
        DOUBLE PRECISION, INTENT(IN), OPTIONAL :: Z(*)
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        INTEGER :: ISIZE(3,3)
        CHARACTER(LEN=32) :: BASENAME, ZONENAME
        INTEGER :: ICELLDIM, IPHYSDIM
        INTEGER, ALLOCATABLE :: IELEM(:,:)
        INTEGER :: NBDYELEM
        INTEGER :: CGNS_TYPE_ELEM
        INTEGER :: I,J
        INTEGER :: INDEX_DIM, INDEX_COORD
        INTEGER :: DISCR_NO
        INTEGER :: INDEXU
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'SET_MESH_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        !CREATE BASE (USER CAN GIVE ANY NAME)
        BASENAME='BASE'
        ICELLDIM=MESH_DIM
        IPHYSDIM=MESH_DIM
        CALL CG_BASE_WRITE_F(FILE_ID,BASENAME,ICELLDIM,IPHYSDIM,
     &                       CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'SET_MESH_CGNS:CG_BASE_WRITE_F'
          RETURN
        ENDIF
        !  DEFINE ZONE NAME (USER CAN GIVE ANY NAME)
        ZONENAME = CGNS_OBJ_TAB(CGNS_ID)%TITLE(1:32)
        CGNS_OBJ_TAB(CGNS_ID)%NELEM = NELEM
        CGNS_OBJ_TAB(CGNS_ID)%NPOIN = NPOIN
        CGNS_OBJ_TAB(CGNS_ID)%NDP = NDP
        ISIZE = 0
        ! NUMBER OF POINTS
        ISIZE(1,1) = NPOIN
        ! NUMBER OF ELEMENTS
        ISIZE(1,2) = NELEM
        ! CREATE ZONE
        CALL CG_ZONE_WRITE_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                       ZONENAME,ISIZE,
     &                       UNSTRUCTURED,
     &                       CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'SET_MESH_CGNS:CG_ZONE_WRITE_F'
          RETURN
        ENDIF
        ! WRITE GRID COORDINATES (USER MUST USE SIDS-STANDARD NAMES HERE)
        CALL CG_COORD_WRITE_F(FILE_ID,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &                        REALDOUBLE,
     &                        'CoordinateX',X,INDEX_COORD,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'SET_MESH_CGNS:CG_COORD_WRITE_F X'
          RETURN
        ENDIF
        CALL CG_COORD_WRITE_F(FILE_ID,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &                        REALDOUBLE,
     &                        'CoordinateY',Y,INDEX_COORD,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'SET_MESH_CGNS:CG_COORD_WRITE_F Y'
          RETURN
        ENDIF
        IF(MESH_DIM.EQ.3) THEN
          CALL CG_COORD_WRITE_F(FILE_ID,
     &                          CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                          CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &                          REALDOUBLE,
     &                          'CoordinateZ',Z,INDEX_COORD,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'SET_MESH_CGNS:CG_COORD_WRITE_F Z'
            RETURN
          ENDIF
        ENDIF
        ! SET ELEMENT CONNECTIVITY:
        ALLOCATE(IELEM(NDP,NELEM),STAT=IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'ALLOCATING IELEM'
          RETURN
        ENDIF
        ! BUILDING THE CONNECTIVITY TABLE
        DO I=1,NELEM
          DO J=1,NDP
            IELEM(J,I) = IKLE((J-1)*NELEM+I)
          ENDDO
        ENDDO
        ! UNSORTED BOUNDARY ELEMENTS
        NBDYELEM=0
        ! DEFINES THE CGNS TYPE
        ! WRITE ELEMENT CONNECTIVITY
        SELECT CASE (TYPELT)
        CASE(TRIANGLE_ELT_TYPE)
          CGNS_TYPE_ELEM = TRI_3
        CASE(QUADRANGLE_ELT_TYPE)
          CGNS_TYPE_ELEM = QUAD_4
        CASE(TETRAHEDRON_ELT_TYPE)
          CGNS_TYPE_ELEM = TETRA_4
        CASE(PRISM_ELT_TYPE)
          CGNS_TYPE_ELEM = PENTA_6
        END SELECT
        !
        CALL CG_SECTION_WRITE_F(FILE_ID,
     &   CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &   CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &   'Elem',CGNS_TYPE_ELEM,1,NELEM,
     &   NBDYELEM,IELEM,
     &   CGNS_OBJ_TAB(CGNS_ID)%INDEX_SECTION,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'SET_MESH_CGNS:CG_SECTION_WRITE_F'
          RETURN
        ENDIF
        DEALLOCATE(IELEM)

        ! Writing knolg/ipobo under a
        ! UserDefinedData_t node
        ! *** discrete data
!       call cg_discrete_write_f(file_id,
!    &   CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
!    &   CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE, 'discrete#1',
!    &                               discr_no, ierr)
!       IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
!       IF(IERR.NE.0) THEN
!         ERROR_MESSAGE = 'ERROR IN '//
!    &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
!    &         'set_mesh_cgns:cg_discrete_write_f'
!         RETURN
!       ENDIF

!       ! *** discrete data arrays, defined on
!       ! vertices:
!       call cg_goto_f(file_id, CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE, ierr,
!    &                 'Zone_t', CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
!    &                'DiscreteData_t', discr_no, 'end')
!       INDEX_DIM = 1
!       IF(NPTIR.GT.0) THEN
!         CALL CG_ARRAY_WRITE_F('knolgIpobo', INTEGER, INDEX_DIM,
!    &                          NPOIN, KNOLG, IERR)
!       ELSE
!         CALL CG_ARRAY_WRITE_F('knolgIpobo', INTEGER, INDEX_DIM,
!    &                          NPOIN, IPOBO, IERR)
!       ENDIF
!       IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
!       IF(IERR.NE.0) THEN
!         ERROR_MESSAGE = 'ERROR IN '//
!    &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
!    &         'SET_MESH_CGNS:CG_array_write_f'
!         RETURN
!       ENDIF

#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE ADD_DATA_CGNS
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
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID
        CHARACTER(LEN=32) :: SOLNAME
        CHARACTER(LEN=32) :: SIDS_NAME
        DOUBLE PRECISION, ALLOCATABLE :: TMP(:)
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'ADD_DATA_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        SOLNAME = REPEAT(' ',32)
        SOLNAME = 'Time'//I2CHAR(RECORD+1)
        ! LOOPING FOR EACH TIME STEP
        IF(FIRST_VAR) THEN
          CGNS_OBJ_TAB(CGNS_ID)%INDEX_FLOW = RECORD + 1
          !   CREATE FLOW SOLUTION NODE
          CALL CG_SOL_WRITE_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &                        SOLNAME,VERTEX,
     &                        CGNS_OBJ_TAB(CGNS_ID)%INDEX_FLOW,IERR)
          IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'ADD_DATA_CGNS:CG_SOL_WRITE_F '//I2CHAR(RECORD)
            RETURN
          ENDIF
          ! Update times
          IF(.NOT.ALLOCATED(CGNS_OBJ_TAB(CGNS_ID)%TIMES)) THEN
            ALLOCATE(CGNS_OBJ_TAB(CGNS_ID)%TIMES(1))
            CGNS_OBJ_TAB(CGNS_ID)%TIMES(1) = TIME
          ELSE
            ALLOCATE(TMP(CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP))
            TMP = CGNS_OBJ_TAB(CGNS_ID)%TIMES
            DEALLOCATE(CGNS_OBJ_TAB(CGNS_ID)%TIMES)
            ALLOCATE(CGNS_OBJ_TAB(CGNS_ID)%TIMES(
     &                        CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP+1))
            CGNS_OBJ_TAB(CGNS_ID)%TIMES(1:CGNS_OBJ_TAB(
     &                       CGNS_ID)%NTIMESTEP) = TMP
            CGNS_OBJ_TAB(CGNS_ID)%TIMES(CGNS_OBJ_TAB(
     &                       CGNS_ID)%NTIMESTEP+1) = TIME
            DEALLOCATE(TMP)
          ENDIF
          CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP =
     &       CGNS_OBJ_TAB(CGNS_ID)%NTIMESTEP + 1
        ENDIF
        !   WRITE FLOW SOLUTION (USER MUST USE SIDS-STANDARD NAMES HERE)
        CALL VARNAME2SIDS(VAR_NAME,SIDS_NAME)
        ! DECLARING A NEW VARIABLE
        CALL CG_FIELD_WRITE_F(FILE_ID,CGNS_OBJ_TAB(CGNS_ID)%INDEX_BASE,
     &           CGNS_OBJ_TAB(CGNS_ID)%INDEX_ZONE,
     &           CGNS_OBJ_TAB(CGNS_ID)%INDEX_FLOW,REALDOUBLE,SIDS_NAME,
     &           VAR_VALUE,CGNS_OBJ_TAB(CGNS_ID)%INDEX_FIELD,IERR)
        IF(IERR.NE.CG_OK) CALL CG_ERROR_PRINT_F
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'ADD_DATA_CGNS:CG_FIELD_WRITE_F '//VAR_NAME
          RETURN
        ENDIF
        !
#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!
!***********************************************************************
      SUBROUTINE SET_BND_CGNS
!***********************************************************************
!
     &(FILE_ID,TYPE_BND_ELT,NELEBD,NDP,IKLE,
     & LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     & LITBOR,TBOR,ATBOR,BTBOR,IERR)
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
        INTEGER,          INTENT(OUT) :: IERR
        !
#if defined (HAVE_CGNS)
        INTEGER :: CGNS_ID, I, NCLI
        !
        CALL GET_OBJ(HASH,FILE_ID,CGNS_ID,IERR)
        IF(IERR.NE.0) THEN
          ERROR_MESSAGE = 'ERROR WITH ID '//I2CHAR(FILE_ID)//': '//
     &         'SET_BND_CGNS:GET_CGNS_OBJ'
          RETURN
        ENDIF
        !
        NCLI = CGNS_OBJ_TAB(CGNS_ID)%NCLI
        REWIND(NCLI)
        DO I=1,NELEBD
          ! Write connectivity and bnoundary value the rest is set to 0.D0
          WRITE(NCLI,4000,IOSTAT=IERR) LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &                         HBOR(I),UBOR(I),VBOR(I),
     &                         CHBORD(I),LITBOR(I),
     &                         TBOR(I),ATBOR(I),BTBOR(I),
     &                         IKLE(I),I
 4000       FORMAT (1X,I2,1X,2(I1,1X),3(F24.12,1X),1X,
     &           F24.12,3X,I1,1X,3(F24.12,1X),1I9,1X,1I9,
     &           1X,I10,1X,2(F27.15,1X),I8)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN '//
     &         TRIM(CGNS_OBJ_TAB(CGNS_ID)%FILE_NAME)//': '//
     &         'SET_BND_CGNS:WRITE:NCLI'
            RETURN
          ENDIF
          CGNS_OBJ_TAB(CGNS_ID)%NPTFR=NELEBD
        ENDDO

#else
!
!     CGNS LIBRARY NOT LOADED
      IERR = HERMES_CGNS_NOT_LOADED_ERR
!
#endif
      END SUBROUTINE
!***********************************************************************
      SUBROUTINE VARNAME2SIDS
!***********************************************************************
!
     &(VAR_NAME, SIDS_NAME)
!
!***********************************************************************
! HERMES   V7P3
!***********************************************************************
!
!brief    Converts variable name into sids-standard name
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| VAR_NAME       |-->| Variable name
!| SIDS_NAME      |<--| SIDS-standard name
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        !
        IMPLICIT NONE
        !
        CHARACTER(LEN=VAR_SIZE), INTENT(IN) :: VAR_NAME
        CHARACTER(LEN=32), INTENT(OUT) :: SIDS_NAME
        !
        CHARACTER(LEN=16) VAR
        INTEGER :: I

        VAR = VAR_NAME(1:16)
        SIDS_NAME = REPEAT(' ',32)
        ! TODO: Add more correspondance
        SELECT CASE(VAR)
          CASE("VITESSE U","VELOCITY U")
            SIDS_NAME = "VelocityX"
          CASE("VITESSE V","VELOCITY V")
            SIDS_NAME = "VelocityY"
          CASE("VITESSE W","VELOCITY W")
            SIDS_NAME = "VelocityZ"
          CASE DEFAULT
            DO I=1,LEN(TRIM(VAR))
              IF(VAR(I:I).EQ.' '.OR.VAR(I:I).EQ."'") THEN
                SIDS_NAME(I:I) = '_'
              ELSE
                SIDS_NAME(I:I) = VAR(I:I)
              ENDIF
            ENDDO
        END SELECT
      END SUBROUTINE
!
      END MODULE UTILS_CGNS
