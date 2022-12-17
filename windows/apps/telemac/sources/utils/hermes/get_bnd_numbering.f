!                   ************************
                    SUBROUTINE GET_BND_NUMBERING
!                   ************************
!
     &(FFORMAT,FID,TYP_BND_ELEM,NPTFR,NBOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns an array containing
!+        The association of boundary numbering to mesh numbering
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| TYP_BND_ELEM   |-->| TYPE OF THE BOUNDARY ELEMENT
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NBOR           |<->| AN ARRAY CONTAINING THE NUMBERING IN THE MESH
!|                |   | OF ALL BOUNDARY POINTS
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
      INTEGER,          INTENT(IN)  :: FID, NPTFR, TYP_BND_ELEM
      INTEGER,          INTENT(INOUT) :: NBOR(NPTFR)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF((TYP_BND_ELEM.EQ.TYPE_NULL).OR.(NPTFR.EQ.0)) RETURN
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_BND_NUMBERING_SRF(FID,TYP_BND_ELEM,NPTFR,NBOR,IERR)
        CASE ('MED    ')
          CALL GET_BND_NUMBERING_MED(FID,TYP_BND_ELEM,NPTFR,NBOR,IERR)
        CASE ('CGNS   ')
          CALL GET_BND_NUMBERING_CGNS(FID,TYP_BND_ELEM,NPTFR,NBOR,IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &         'GET_BND_NUMBERING: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END

