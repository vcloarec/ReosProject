!                   *****************
                    SUBROUTINE VC00BB
!                   *****************
!
     &(XMUL,SURFAC,NELEM,NELMAX,W1,W2,W3,W4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE QUASI-BUBBLE TRIANGLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        05/02/91
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
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION X2SUR9,COEF
!
!-----------------------------------------------------------------------
!
      X2SUR9 = 2 * XMUL / 9.D0
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
      COEF = X2SUR9 * SURFAC(IELEM)
!
      W1(IELEM) = COEF
      W2(IELEM) = COEF
      W3(IELEM) = COEF
      W4(IELEM) = COEF*1.5D0
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
