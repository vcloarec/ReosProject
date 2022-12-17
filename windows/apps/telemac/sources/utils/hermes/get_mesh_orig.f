!                   ************************
                    SUBROUTINE GET_MESH_ORIG
!                   ************************
!
     &(FFORMAT,FID,X_ORIG,Y_ORIG,IERR)
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
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| X_ORIG         |<--| Off set of the X coordinates
!| Y_ORIG         |<--| Off set of the Y coordinates
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
      INTEGER,          INTENT(OUT) :: X_ORIG, Y_ORIG
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      X_ORIG = 0
      Y_ORIG = 0
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_MESH_ORIG_SRF(FID, X_ORIG, Y_ORIG, IERR)
        CASE ('MED    ')
          CALL GET_MESH_ORIG_MED(FID, X_ORIG, Y_ORIG, IERR)
        CASE ('CGNS   ')
          CONTINUE
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &        'GET_MESH_ORIG: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
