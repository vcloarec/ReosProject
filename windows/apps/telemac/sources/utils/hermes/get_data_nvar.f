!                   ************************
                    SUBROUTINE GET_DATA_NVAR
!                   ************************
!
     &(FFORMAT,FID,NVAR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of variables in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| NVAR           |<->| NUMBER OF VARIABLE
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
      INTEGER,          INTENT(OUT) :: NVAR
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      NVAR = 0
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_DATA_NVAR_SRF(FID, NVAR,IERR)
        CASE ('MED    ')
          CALL GET_DATA_NVAR_MED(FID, NVAR,IERR)
        CASE ('CGNS   ')
          CALL GET_DATA_NVAR_CGNS(FID, NVAR,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &          'GET_DATA_NVAR: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END


