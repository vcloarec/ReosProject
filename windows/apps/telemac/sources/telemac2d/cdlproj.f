!                   ******************
                    SUBROUTINE CDLPROJ
!                   ******************
!
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,UA)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PROJECTS THE SOLUTION ON THE BOUNDARY CONDITIONS.
!
!history  INRIA
!+
!+        V5P3
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KNEU           |-->| CONVENTION FOR NEUMANN POINTS
!| LIMPRO         |-->| TYPES OF BOUNDARY CONDITION
!| NBOR           |-->| GLOBAL INDICES FOR BORD NODES
!| NPTFR          |-->| TOTAL NUMBER OF BOUNDARY NODES
!| NS             |-->| TOTAL NUMBER OF NODES
!| UA             |<->| WORKING TABLE
!| XNEBOR         |-->| OUTWARD UNIT NORMAL (X COMPONENT)
!| YNEBOR         |-->| OUTWARD UNIT NORMAL (Y COMPONENT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NPTFR,KNEU
      INTEGER, INTENT(IN) :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NS)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K
!
      DOUBLE PRECISION VNX,VNY,CEN
!
!-----------------------------------------------------------------------
!
!  SLIPING CONDITION
!  ***********************
!
!
      DO K=1,NPTFR
!
        IF(LIMPRO(K,1).EQ.KNEU) THEN
!
          IS=NBOR(K)
          VNX=XNEBOR(K)
          VNY=YNEBOR(K)
          CEN=UA(2,IS)*VNX+UA(3,IS)*VNY
          UA(2,IS) = UA(2,IS) -CEN*VNX
          UA(3,IS) = UA(3,IS) -CEN*VNY
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
