!                   *******************
                    SUBROUTINE ADD_DATA
!                   *******************
!
     &(FFORMAT,FILE_ID,VAR_NAME,TIME,RECORD,
     & FIRST_VAR,VAR_VALUE,N,IERR)
!
!***********************************************************************
! HERMES   V7P0
!***********************************************************************
!
!brief    Add data information for a given variable and a given time on
!+        all points of the mesh
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| VAR_NAME       |-->| NAME OF THE VARIABLE
!| TIME           |-->| TIME OF THE DATA
!| RECORD         |-->| TIME STEP OF THE DATA
!| FIRST_VAR      |-->| TRUE IF IT IS THE FIRST VARIABLE OF THE DATASET
!| VAR_VALUE      |-->| THE VALUE FOR EACH POINT OF THE MESH
!| N              |-->| SIZE OF VAR_VALUE
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
      INTEGER,           INTENT(IN)  :: FILE_ID,N
      CHARACTER(LEN=32), INTENT(IN)  :: VAR_NAME
      DOUBLE PRECISION,  INTENT(IN)  :: TIME
      INTEGER,           INTENT(IN)  :: RECORD
      LOGICAL,           INTENT(IN)  :: FIRST_VAR
      DOUBLE PRECISION,  INTENT(IN)  :: VAR_VALUE(N)
      INTEGER,           INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL ADD_DATA_SRF(FILE_ID,VAR_NAME,TIME,RECORD,
     &                    FIRST_VAR,VAR_VALUE,N,IERR)
        CASE ('MED    ')
          CALL ADD_DATA_MED(FILE_ID,VAR_NAME,TIME,RECORD,VAR_VALUE,
     &                      N,IERR)
        CASE ('VTK    ')
          CALL ADD_DATA_VTK(FILE_ID,VAR_NAME,RECORD,
     &                    FIRST_VAR,VAR_VALUE,N,IERR)
        CASE ('CGNS   ')
          CALL ADD_DATA_CGNS(FILE_ID,VAR_NAME,TIME,RECORD,
     &                    FIRST_VAR,VAR_VALUE,N,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*) 'ADD_DATA: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

