!                   *************************
                    SUBROUTINE READ_MESH_CONN
!                   *************************
!
     &(FFORMAT,NFIC,NPOIN,TYP_ELEM,NELEM,NDP,TYP_BND_ELEM,NELEBD,IKLE,
     &  IPOBO)
!
!***********************************************************************
! HERMES   V7P0
!***********************************************************************
!
!brief    READS THE CONNECTIVITY TABLE AND NUMBERING FOR THE
!+        BOUNDARY NODES.
!
!history  Y AUDOUIN (LNHE)
!+        21/05/2015
!+        V7P0
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NFIC           |-->| LOGICAL UNIT FOR GEOMETRY FILE
!| IB             |<--| 10 INTEGERS, SEE SELAFIN FILE STANDARD
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| NELEBD         |<--| NUMBER OF BOUNDARY ELEMENTS
!| NELEM          |<--| NUMBER OF ELEMENTS IN THE MESH
!| NPOIN          |<--| NUMBER OF POINTS IN THE MESH
!| NPTFR          |<--| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN)    :: NFIC,NPOIN,NELEM,NDP,TYP_ELEM
      INTEGER, INTENT(IN)    :: TYP_BND_ELEM,NELEBD
      INTEGER, INTENT(INOUT) :: IKLE(NDP*NELEM)
      INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IERR
!
!-----------------------------------------------------------------------
!
      CALL GET_MESH_CONNECTIVITY(FFORMAT,NFIC,TYP_ELEM,IKLE,NELEM,
     &                           NDP,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_CONN:GET_MESH_CONNECTIVITY')
!
      CALL GET_BND_IPOBO(FFORMAT,NFIC,NPOIN,NELEBD,TYP_BND_ELEM,IPOBO,
     &                   IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_CONN:GET_BND_IPOBO')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
