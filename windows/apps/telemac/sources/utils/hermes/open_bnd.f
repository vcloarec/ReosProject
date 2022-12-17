!                   ********************
                    SUBROUTINE OPEN_BND
!                   ********************
!
     &(FFORMAT,FILE_NAME,FILE_ID,OPENMODE,IERR,MESH_NUMBER)
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
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_NAME      |-->| NAME OF THE BOUNDARY FILE
!| FILE_ID        |-->| FILE DESCRIPTOR OF THE "MESH" FILE
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','WRITE','READWRITE'
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE  WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN, ONLY : OPEN_BND_SRF
      USE UTILS_MED, ONLY : OPEN_BND_MED
      USE UTILS_CGNS, ONLY : OPEN_BND_CGNS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      CHARACTER(LEN=*),  INTENT(IN)  :: FILE_NAME
      INTEGER,           INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=9),  INTENT(IN)  :: OPENMODE
      INTEGER,           INTENT(OUT) :: IERR
      INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL OPEN_BND_SRF(FILE_NAME,FILE_ID,OPENMODE,IERR,MESH_NUMBER)
        CASE ('MED    ')
          CALL OPEN_BND_MED(FILE_NAME,FILE_ID,OPENMODE,IERR,MESH_NUMBER)
        CASE ('CGNS   ')
          CALL OPEN_BND_CGNS(FILE_NAME, FILE_ID, OPENMODE, IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &       'OPEN_BND: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

