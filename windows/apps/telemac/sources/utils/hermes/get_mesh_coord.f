!                   *************************
                    SUBROUTINE GET_MESH_COORD
!                   *************************
!
     &(FFORMAT,FID,JDIM,NDIM,NPOIN,COORD,IERR)
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
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| JDIM           |-->| DIMENSION NUMBER
!| NDIM           |-->| NUMBER OF DIMENSION OF THE MESH
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| COORD          |<->| LOCAL TO GLOBAL NUMBERING ARRAY
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN
      USE UTILS_MED
      USE UTILS_CGNS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID, JDIM, NDIM, NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: COORD(NPOIN)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_MESH_COORD_SRF(FID, JDIM, NPOIN, COORD, IERR)
        CASE ('MED    ')
          CALL GET_MESH_COORD_MED(FID, JDIM, NDIM, NPOIN, COORD, IERR)
        CASE ('CGNS   ')
          CALL GET_MESH_COORD_CGNS(FID, JDIM, NPOIN, COORD, IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &      'GET_MESH_COORD: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
