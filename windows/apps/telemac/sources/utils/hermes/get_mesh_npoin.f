!                   *************************
                    SUBROUTINE GET_MESH_NPOIN
!                   *************************
!
     &(FFORMAT,FID,TYP_ELEM,NPOIN,IERR)
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
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NPOIN          |<->| THE NUMBER OF POINTS
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
      INTEGER,          INTENT(IN)  :: FID
      INTEGER,          INTENT(IN)  :: TYP_ELEM
      INTEGER,          INTENT(OUT) :: NPOIN
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      NPOIN = 0
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_MESH_NPOIN_SRF(FID, NPOIN, IERR)
        CASE ('MED    ')
          CALL GET_MESH_NPOIN_MED(FID, TYP_ELEM, NPOIN, IERR)
        CASE ('CGNS   ')
          CALL GET_MESH_NPOIN_CGNS(FID, TYP_ELEM, NPOIN, IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &       'GET_MESH_NPOIN: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
