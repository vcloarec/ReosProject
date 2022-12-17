!                   ****************************
                    SUBROUTINE GET_DATA_VAR_LIST
!                   ****************************
!
     &(FFORMAT,FID,NVAR,VARLIST,UNITLIST,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns a list of all the name of the variables in the mesh file
!+        and a list of their units
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| VARLIST        |<->| LIST OF VARIABLE NAME
!| UNTILIST       |<->| LIST OF VARIABLE UNIT
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
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      INTEGER,           INTENT(IN)  :: FID
      INTEGER,           INTENT(IN)  :: NVAR
      CHARACTER(LEN=16), INTENT(INOUT) :: VARLIST(NVAR)
      CHARACTER(LEN=16), INTENT(INOUT) :: UNITLIST(NVAR)
      INTEGER,           INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_DATA_VAR_LIST_SRF(FID, NVAR, VARLIST, UNITLIST, IERR)
        CASE ('MED    ')
          CALL GET_DATA_VAR_LIST_MED(FID, NVAR, VARLIST, UNITLIST,IERR)
        CASE ('CGNS   ')
          CALL GET_DATA_VAR_LIST_CGNS(FID, NVAR, VARLIST, UNITLIST,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &      'GET_DATA_VAR_LIST: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END


