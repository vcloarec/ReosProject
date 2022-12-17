!                   *****************
                    SUBROUTINE VC16AA
!                   *****************
!
     &( XMUL,SF,SG,SU,SV,F,G,U,V,
     &  XEL,YEL,SURFAC,
     &  IKLE1,IKLE2,IKLE3,NELEM,NELMAX,
     &  W1,W2,W3 )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /  -> -->              ->
!+      V  =  XMUL   /   K GRAD(PSII) * DIV( U )  D(OMEGA)
!+       I          /OMEGA
!+
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!+    ->
!+    U IS A VECTOR WITH COORDINATES U AND V
!+    ->
!+    K IS A VECTOR WITH COMPONENTS F AND G
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)
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
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
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
      USE BIEF, EX_VC16AA => VC16AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMG,IELMV
      DOUBLE PRECISION X2,Y2,X3,Y3,U1,U2,U3,V1,V2,V3,FF,GG
      DOUBLE PRECISION XSUR04,COEF
!
!-----------------------------------------------------------------------
!
      XSUR04 = XMUL / 12.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMG=SG%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
!     F AND G (NOT CHECKED) ARE P0; U AND V (NOT CHECKED) ARE LINEAR
!
      IF(     IELMF.EQ.10.AND.IELMG.EQ.11.
     &    AND.IELMU.EQ.11.AND.IELMV.EQ.11  ) THEN
!
      DO IELEM = 1 , NELEM
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      U1 = U(IKLE1(IELEM))
      U2 = U(IKLE2(IELEM)) - U1
      U3 = U(IKLE3(IELEM)) - U1
      V1 = V(IKLE1(IELEM))
      V2 = V(IKLE2(IELEM)) - V1
      V3 = V(IKLE3(IELEM)) - V1
!
!     U1 AND V1 NOW =0 (ONLY THE GRADIENT OF U IS USED)
!
      COEF = (X2*V3-X3*V2-U3*Y2+U2*Y3) * XSUR04 / SURFAC(IELEM)
!
      FF = F(IELEM)
      GG = G(IELEM)
!
      W1(IELEM) =-( (X2-X3)*GG+(Y3-Y2)*FF ) * COEF
      W2(IELEM) =           (-GG*X3+FF*Y3)  * COEF
      W3(IELEM) =          -(FF*Y2-GG*X2)   * COEF
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
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC16AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
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
