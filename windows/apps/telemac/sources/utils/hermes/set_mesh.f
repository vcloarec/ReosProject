!                   *******************
                    SUBROUTINE SET_MESH
!                   *******************
!
     &(FFORMAT,FILE_ID,MESH_DIM,TYPELM,NDP,NPTFR,
     & NPTIR,NELEM,NPOIN,IKLE,IPOBO,
     & KNOLG,X,Y,NPLAN,DATE,TIME,X_ORIG,Y_ORIG,IERR,Z,IN_PLACE)
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
      USE UTILS_SERAFIN
      USE UTILS_MED
      USE UTILS_VTK
      USE UTILS_CGNS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FILE_ID,NPLAN
      INTEGER,          INTENT(IN)  :: DATE(3)
      INTEGER,          INTENT(IN)  :: TIME(3)
      INTEGER,          INTENT(IN)  :: MESH_DIM
      INTEGER,          INTENT(IN)  :: TYPELM
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: NPTFR
      INTEGER,          INTENT(IN)  :: NPTIR
      INTEGER,          INTENT(IN)  :: NELEM
      INTEGER,          INTENT(IN)  :: NPOIN
      INTEGER,          INTENT(IN)  :: X_ORIG
      INTEGER,          INTENT(IN)  :: Y_ORIG
      INTEGER,          INTENT(INOUT)  :: IKLE(NELEM*NDP)
      INTEGER,          INTENT(IN)  :: IPOBO(*)
      INTEGER,          INTENT(IN)  :: KNOLG(*)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN),Y(NPOIN)
      INTEGER,          INTENT(OUT) :: IERR
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: Z(*)
      LOGICAL,          INTENT(IN), OPTIONAL :: IN_PLACE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, ALLOCATABLE :: COORD(:)
      INTEGER :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL SET_MESH_SRF(FFORMAT,FILE_ID,MESH_DIM,TYPELM,NDP,NPTFR,
     &                      NPTIR,NELEM,NPOIN,IKLE,IPOBO,
     &                      KNOLG,X,Y,NPLAN,DATE,TIME,X_ORIG,Y_ORIG,
     &                      IERR,
     &                      IN_PLACE=IN_PLACE)
        CASE ('MED    ')
!         STORE COORDINATES
          ALLOCATE(COORD(NPOIN*MESH_DIM),STAT=IERR)
          IF(IERR.NE.0) THEN
            ERROR_MESSAGE = 'ERROR IN ALLOCATING SET_MESH:COORD'
            RETURN
          ENDIF
          DO I=1,NPOIN
            COORD(I) = X(I)
            COORD(I+NPOIN) = Y(I)
          ENDDO
          IF (PRESENT(Z).AND.(MESH_DIM.EQ.3)) THEN
            DO I=1,NPOIN
              COORD(I+2*NPOIN) = Z(I)
            ENDDO
          ENDIF
!
          CALL SET_MESH_MED(FILE_ID,MESH_DIM,MESH_DIM,TYPELM,NDP,
     &      NPTIR,NELEM,NPOIN,IKLE,KNOLG,COORD,NPLAN,
     &      DATE,TIME,X_ORIG,Y_ORIG,IERR)
          DEALLOCATE(COORD)
        CASE ('VTK    ')
          CALL SET_MESH_VTK(FILE_ID,MESH_DIM,TYPELM,NDP,
     &      NELEM,NPOIN,IKLE,X,Y,IERR)
        CASE ('CGNS   ')
          IF (PRESENT(Z).AND.(MESH_DIM.EQ.3)) THEN
            CALL SET_MESH_CGNS(FILE_ID,MESH_DIM,TYPELM,NDP,NPTFR,
     &        NPTIR,NELEM,NPOIN,IKLE,IPOBO,KNOLG,X,Y,NPLAN,IERR,Z)
          ELSE
            CALL SET_MESH_CGNS(FILE_ID,MESH_DIM,TYPELM,NDP,NPTFR,
     &        NPTIR,NELEM,NPOIN,IKLE,IPOBO,KNOLG,X,Y,NPLAN,IERR)
          ENDIF
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*) 'SET_MESH: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END


