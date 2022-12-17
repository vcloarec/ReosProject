!                   *****************************
                    SUBROUTINE GET_DATA_VAR_LIST2
!                   *****************************
!
     &(FFORMAT,FID,NVAR,VARLIST2,UNITLIST2,IERR)
!
!***********************************************************************
! HERMES   V7P0 01/05/2014
!***********************************************************************
!
!brief    For Python API we need string arrays as character array
!         So we are calling get_data_var_list and creating the array
!
!history  Y AUDOUIN (LNHE)
!+        15/02/2017
!+        V7P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| VARLIST2       |<->| LIST OF VARIABLE NAME
!| UNTILIST2      |<->| LIST OF VARIABLE UNIT
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      INTEGER,           INTENT(IN)  :: FID
      INTEGER,           INTENT(IN)  :: NVAR
      CHARACTER, INTENT(OUT) :: VARLIST2(NVAR*16)
      CHARACTER, INTENT(OUT) :: UNITLIST2(NVAR*16)
      INTEGER,           INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16) :: VARLIST(NVAR)
      CHARACTER(LEN=16) :: UNITLIST(NVAR)
      INTEGER I, J

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL GET_DATA_VAR_LIST(FFORMAT,FID, NVAR, VARLIST, UNITLIST, IERR)
      IF(IERR.NE.0) RETURN

      DO J=1,NVAR
        DO I=1,16
          VARLIST2((J-1)*16+I) = VARLIST(J)(I:I)
          UNITLIST2((J-1)*16+I) = UNITLIST(J)(I:I)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
