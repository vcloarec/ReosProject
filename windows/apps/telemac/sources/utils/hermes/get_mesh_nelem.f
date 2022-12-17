!                   *************************
                    SUBROUTINE GET_MESH_NELEM
!                   *************************
!
     &(FFORMAT,FID,TYP_ELEM,NELEM,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the number of elements of type typ_elem in the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| TYP_ELEM       |-->| TYPE OF THE ELEMENT
!| NELEM          |<->| THE NUMBER OF ELEMENTS
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
      INTEGER,          INTENT(IN)  :: TYP_ELEM
      INTEGER,          INTENT(OUT) :: NELEM
      INTEGER,          INTENT(OUT) :: IERR
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      NELEM = 0
      IF(TYP_ELEM.EQ.TYPE_NULL) THEN
        RETURN
      ENDIF
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_MESH_NELEM_SRF(FID, TYP_ELEM, NELEM, IERR)
        CASE ('MED    ')
          CALL GET_MESH_NELEM_MED(FID, TYP_ELEM, NELEM, IERR)
        CASE ('CGNS   ')
          CALL GET_MESH_NELEM_CGNS(FID, TYP_ELEM, NELEM, IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &        'GET_MESH_NELEM: BAD FILE FORMAT: ',FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
