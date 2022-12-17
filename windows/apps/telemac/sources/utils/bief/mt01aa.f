!                   *****************
                    SUBROUTINE MT01AA
!                   *****************
!
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SURFAC,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX FOR P1 TRIANGLES.
!code
!+    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!+
!+                                 /
!+                    A    = XMUL /  (P *P )*J(X,Y) DXDY
!+                     I J       /S    I  J
!+
!+    BY ELEMENTARY CELL
!+
!+    J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        28/11/94
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT01AA => MT01AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM
      DOUBLE PRECISION SUR12,DET
!
!=======================================================================
!
      SUR12 = XMUL/12.D0
!
!-----------------------------------------------------------------------
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      DET = SURFAC(IELEM) * SUR12
!
!  ELEMENTS OFF THE DIAGONAL
!
      A12(IELEM) = DET
      A13(IELEM) = DET
      A23(IELEM) = DET
!
!  DIAGONAL TERMS
!
      A11(IELEM) = DET + DET
      A22(IELEM) = DET + DET
      A33(IELEM) = DET + DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
