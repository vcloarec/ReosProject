!                   *****************
                    SUBROUTINE VC01BB
!                   *****************
!
     &(XMUL,SF,F,SURFAC,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     & W1,W2,W3,W4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I) * F  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF QUASI-BUBBLE TYPE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        10/01/95
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC01BB => VC01BB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ),   INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION F1,F2,F3,F4,XSU108,XSUR09,XSUR36,XSUR18
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!     F OF TYPE P1
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.11) THEN
!
      XSU108 = XMUL / 108.D0
      XSUR09 = XMUL / 9.D0
!
      DO IELEM = 1 , NELEM
!
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
!
        W1(IELEM) = SURFAC(IELEM)*(5*F3+5*F2+14*F1)*XSU108
!
        W2(IELEM) = SURFAC(IELEM)*(5*F3+14*F2+5*F1)*XSU108
!
        W3(IELEM) = SURFAC(IELEM)*(14*F3+5*F2+5*F1)*XSU108
!
        W4(IELEM) = SURFAC(IELEM)*(F3+F2+F1)*XSUR09
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!     F QUASI-BUBBLE
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.12) THEN
!
      XSUR36 = XMUL / 36.D0
      XSUR18 = XMUL / 18.D0
!
      DO IELEM = 1 , NELEM
!
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
        F4  = F(IKLE4(IELEM))
!
        W1(IELEM) = SURFAC(IELEM)*(2*F4+  F3+  F2+4*F1)*XSUR36
        W2(IELEM) = SURFAC(IELEM)*(2*F4+  F3+4*F2+  F1)*XSUR36
        W3(IELEM) = SURFAC(IELEM)*(2*F4+4*F3+  F2+  F1)*XSUR36
        W4(IELEM) = SURFAC(IELEM)*(3*F4+  F3+  F2+  F1)*XSUR18
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!      OTHER
!      ELSEIF
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'VC01BB (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &         1X,'REAL NAME: ',A6)
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
