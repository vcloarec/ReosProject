!                   ********************************
                    SUBROUTINE GET_MESH_CONNECTIVITY
!                   ********************************
!
     &(FFORMAT,FID,TYP_ELEM,IKLE,NELEM,NDP,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns the connectivity table for
!+        the element of type typ_elem in the mesh
!+        will do nothing if there are no element of typ_elem in the mesh
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
!| IKLE           |<->| THE CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
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
      INTEGER,          INTENT(IN)  :: NELEM
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(INOUT) :: IKLE(NELEM*NDP)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT(1:7))
        CASE ('SERAFIN')
          CALL GET_MESH_CONNECTIVITY_SRF(FID, TYP_ELEM, IKLE, NELEM,
     &                                   NDP, IERR)
        CASE ('MED    ')
          CALL GET_MESH_CONNECTIVITY_MED(FID, TYP_ELEM, IKLE, NELEM,
     &                                   NDP, IERR)
        CASE ('CGNS   ')
          CALL GET_MESH_CONNECTIVITY_CGNS(FID, TYP_ELEM, IKLE, NELEM,
     &                                   NDP, IERR)
        CASE DEFAULT
          IERR = HERMES_UNKNOWN_FILE_FORMAT_ERR
          WRITE(ERROR_MESSAGE,*)
     &       'GET_MESH_CONNECTIVITY: BAD FILE FORMAT: ', FFORMAT
          RETURN
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
