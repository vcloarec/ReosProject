!                   *****************
                    SUBROUTINE MT01OO
!                   *****************
!
     &(A11,A12,A22,XMUL,LGSEG,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX FOR P1 SEGMENTS.
!code
!+    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!+
!+                              /
!+                    A    =   /  (P *P )*J(X,Y) DX
!+                     I J    /L    I  J
!+
!+    BY ELEMENTARY CELL
!+
!+    J(X,Y) : JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        06/12/94
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
!| A22            |<--| ELEMENTS OF MATRIX
!| LGSEG          |-->| LENGTH OF SEGMENTS.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT01OO => MT01OO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(NELMAX),A12(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A22(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: LGSEG(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION XSUR6,DET1
!
!-----------------------------------------------------------------------
!
      XSUR6  = XMUL/6.D0
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        DET1 = LGSEG(IELEM) * XSUR6
        A11(IELEM) = DET1 + DET1
        A12(IELEM) = DET1
        A22(IELEM) = DET1 + DET1
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
