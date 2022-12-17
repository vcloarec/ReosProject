!                   *****************************
                    DOUBLE PRECISION FUNCTION DOT
!                   *****************************
!
     &(NPOIN,X,Y)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SCALAR PRODUCT OF VECTORS X AND Y OF SIZE NPOIN.
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        18/08/94
!+        V5P1
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
!| NPOIN          |-->| TAILLE DE X ET Y
!| X              |-->| FIRST DOUBLE PRECISION VECTOR
!| Y              |-->| SECOND DOUBLE PRECISION VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DOT = 0.D0
!
      DO I = 1 , NPOIN
        DOT = DOT + X(I) * Y(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
