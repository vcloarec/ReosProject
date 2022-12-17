!                   ********************
                    SUBROUTINE CLOSE_BND
!                   ********************
!
     &(FFORMAT,FILE_ID,IERR,MESH_NUMBER)
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
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!                       THE CONCATENATED FILE  WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN, ONLY: CLOSE_BND_SRF
      USE UTILS_MED, ONLY : CLOSE_BND_MED
      USE UTILS_CGNS, ONLY : CLOSE_BND_CGNS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      INTEGER,           INTENT(IN)  :: FILE_ID
      INTEGER,           INTENT(OUT) :: IERR
      INTEGER, OPTIONAL, INTENT(IN)  :: MESH_NUMBER
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL CLOSE_BND_SRF(FILE_ID,IERR,MESH_NUMBER)
        CASE ('MED    ')
          CALL CLOSE_BND_MED(FILE_ID,IERR,MESH_NUMBER)
        CASE ('CGNS   ')
          CALL CLOSE_BND_CGNS(FILE_ID,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*) 'CLOSE_BND: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

