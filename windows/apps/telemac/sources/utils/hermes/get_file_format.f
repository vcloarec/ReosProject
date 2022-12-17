!**********************************************************************
                    SUBROUTINE GET_FILE_FORMAT
!**********************************************************************
!
     &(FFORMAT,FILE_ID,REAL_FFORMAT,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns file format (usefull only for SERAFIN/SERAFIND)
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| REAL_FFORMAT   |<--| REAL FORMAT OF THE FILE
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN, ONLY : GET_FILE_FORMAT_SRF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      INTEGER,          INTENT(IN)    :: FILE_ID
      CHARACTER(LEN=8), INTENT(OUT)   :: REAL_FFORMAT
      INTEGER,          INTENT(OUT)   :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IERR = 0
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_FILE_FORMAT_SRF(FFORMAT,FILE_ID,REAL_FFORMAT,IERR)
        CASE ('MED    ')
          REAL_FFORMAT = FFORMAT
        CASE ('VTK    ')
          REAL_FFORMAT = FFORMAT
        CASE ('CGNS   ')
          REAL_FFORMAT = FFORMAT
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*) 'GET_FILE_FORMAT: BAD FILE FORMAT: ',
     &                           FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

