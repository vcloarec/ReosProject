!                   *****************
                    SUBROUTINE VC03AA
!                   *****************
!
     &(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,XEL,YEL,SURFAC,
     & IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W1,W2,W3 )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /                     DF      DF
!+      V  =  XMUL   /   K GRAD(PSII) * ( U --  + V -- )   D(OMEGA)
!+       I          /OMEGA                  DX      DY
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!+
!+    F, U AND V ARE VECTORS
!+    K IS A VECTOR WITH COMPONENTS G AND H
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        09/12/94
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
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SH             |-->| BIEF_OBJ STRUCTURE OF H
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SURFAC         |-->| AREA OF TRIANGLES
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC03AA => VC03AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)   :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT):: W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
!
!     STRUCTURES OF F, G, H, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SH,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMG,IELMU,IELMV,IELMH
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3,U123,V123
      DOUBLE PRECISION WX1,WX2,WX3,WY1,WY2,WY3,XSUR12,COEF
!
!-----------------------------------------------------------------------
!
      XSUR12 = XMUL / 12.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMG=SG%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMH=SH%ELM
!
!-----------------------------------------------------------------------
!
!     F IS LINEAR; G AND H P0; AND U, V LINEAR
!
      IF(       IELMF.EQ.11
     &     .AND.IELMG.EQ.10
     &     .AND.IELMH.EQ.10
     &     .AND.IELMU.EQ.11
     &     .AND.IELMV.EQ.11  ) THEN
!
      DO IELEM = 1 , NELEM
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM)) - F1
      F3 = F(IKLE3(IELEM)) - F1
!
!     F1 NOW =0 (ONLY THE GRADIENT OF F IS USED)
!
      U123 = U(IKLE1(IELEM)) + U(IKLE2(IELEM)) + U(IKLE3(IELEM))
      V123 = V(IKLE1(IELEM)) + V(IKLE2(IELEM)) + V(IKLE3(IELEM))
!
      WX1 = ( - F2*X3*Y2 + F2*X3*Y3 + F3*X2*Y2 - F3*X2*Y3 ) * V123
     &    + ( + F2*Y2*Y3 - F2*Y3*Y3 - F3*Y2*Y2 + F3*Y2*Y3 ) * U123
!
      WY1 = (   F2*X2*X3 - F3*X2*X2 - F2*X3*X3 + F3*X2*X3 ) * V123
     &    + ( - F2*X2*Y3 + F2*X3*Y3 + F3*X2*Y2 - F3*X3*Y2 ) * U123
!
      WX2 = Y3 * ( (F3*X2-F2*X3) * V123 + (F2*Y3-F3*Y2) * U123 )
!
      WY2 = X3 * ( (F2*X3-F3*X2) * V123 + (F3*Y2-F2*Y3) * U123 )
!
      WX3 = Y2 * ( (F2*X3-F3*X2) * V123 + (F3*Y2-F2*Y3) * U123 )
!
      WY3 = X2 * ( (F3*X2-F2*X3) * V123 + (F2*Y3-F3*Y2) * U123 )
!
      COEF = XSUR12 / SURFAC(IELEM)
!
      W1(IELEM) = ( WX1*G(IELEM) + WY1*H(IELEM) ) * COEF
      W2(IELEM) = ( WX2*G(IELEM) + WY2*H(IELEM) ) * COEF
      W3(IELEM) = ( WX3*G(IELEM) + WY3*H(IELEM) ) * COEF
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMF,SF%NAME
        WRITE(LU,111) IELMG,SG%NAME
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC03AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
111     FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
