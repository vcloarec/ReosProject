!**********************************************************************
                    SUBROUTINE OPEN_MESH
!**********************************************************************
!
     &(FFORMAT,FILE_NAME,FILE_ID,OPENMODE,IERR,MESH_NUMBER)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |<->| FORMAT OF THE FILE
!| FILE_NAME      |-->| NAME OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| OPENMODE       |-->| ONE OF THE FOLLOWING VALUE 'READ','WRITE','READWRITE'
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| MESH_NUMBER    |-->| IF PRESENT, THIS IS THE NUMBER OF THE PART OF
!|                |   |    THE CONCATENATED FILE WE WANT TO ACCESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN, ONLY : OPEN_MESH_SRF
      USE UTILS_MED, ONLY : OPEN_MESH_MED
      USE UTILS_VTK, ONLY : OPEN_MESH_VTK
      USE UTILS_CGNS, ONLY : OPEN_MESH_CGNS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(INOUT) :: FFORMAT
      CHARACTER(LEN=*), INTENT(IN)    :: FILE_NAME
      INTEGER,          INTENT(OUT)   :: FILE_ID
      CHARACTER(LEN=9), INTENT(IN)    :: OPENMODE
      INTEGER,          INTENT(OUT)   :: IERR
      INTEGER, OPTIONAL,INTENT(IN)    :: MESH_NUMBER
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL OPEN_MESH_SRF(FILE_NAME, FILE_ID, OPENMODE, FFORMAT,
     &                       IERR, MESH_NUMBER)
        CASE ('MED    ')
          CALL OPEN_MESH_MED(FILE_NAME, FILE_ID, OPENMODE,IERR,
     &                       MESH_NUMBER)
        CASE ('VTK    ')
          CALL OPEN_MESH_VTK(FILE_NAME, FILE_ID, OPENMODE,IERR)
        CASE ('CGNS   ')
          CALL OPEN_MESH_CGNS(FILE_NAME, FILE_ID, OPENMODE,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*) 'OPEN_MESH: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

