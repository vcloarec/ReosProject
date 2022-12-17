!                   *******************
                    SUBROUTINE SET_HEADER
!                   *******************
!
     &(FFORMAT,FILE_ID,TITLE,NVAR,VAR_NAME,IERR)
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
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TITLE          |-->| TITLE OF THE MESH
!| NVAR           |-->| NUMBER OF VARIABLES
!| VAR_NAME       |-->| NAME AND UNITS OF THE VARIABLES
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
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
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      INTEGER,           INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=80), INTENT(IN)  :: TITLE
      INTEGER,           INTENT(IN)  :: NVAR
      CHARACTER(LEN=32), INTENT(IN)  :: VAR_NAME(NVAR)
      INTEGER,           INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          ! Forcing end of title to be format
          CALL SET_HEADER_SRF(FILE_ID,TITLE(1:72)//FFORMAT,
     &                        NVAR,VAR_NAME,IERR)
        CASE ('MED    ')
          CALL SET_HEADER_MED(FILE_ID,TITLE,IERR)
        CASE ('VTK    ')
          CALL SET_HEADER_VTK(FILE_ID,TITLE,NVAR,IERR)
        CASE ('CGNS   ')
          CALL SET_HEADER_CGNS(FILE_ID,TITLE,NVAR,VAR_NAME,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &       'SET_HEADER: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

