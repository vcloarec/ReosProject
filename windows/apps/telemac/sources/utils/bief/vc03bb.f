!                   *****************
                    SUBROUTINE VC03BB
!                   *****************
!
     &(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,XEL,YEL,
     & IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4 )
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
!+    PSI(I) IS A BASE OF TYPE QUASI-BUBBLE TRIANGLE
!+
!+    F, U AND V ARE VECTORS
!+    K IS A VECTOR WITH COMPONENTS G AND H
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; C MOULIN  (LNH)
!+        13/01/95
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
!| IKLE4          |-->| QUASI-BUBBLE POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SH             |-->| BIEF_OBJ STRUCTURE OF H
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC03BB => VC03BB
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
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURES OF F, G, H, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF,SG,SH,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMG,IELMU,IELMV,IELMH
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3,F4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,GEL,HEL
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMG=SG%ELM
      IELMH=SH%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
!     F IS QUASI-BUBBLE; G AND H (NOT CHECKED) P0; AND U, V (NOT CHECKED) P1
!
      IF(      IELMF.EQ.12.AND.IELMG.EQ.10.AND.IELMH.EQ.10
     &    .AND.IELMU.EQ.11.AND.IELMV.EQ.11                 ) THEN
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
      F4 = F(IKLE4(IELEM)) - F1
!
      U1 = U(IKLE1(IELEM))
      U2 = U(IKLE2(IELEM))
      U3 = U(IKLE3(IELEM))
!
      V1 = V(IKLE1(IELEM))
      V2 = V(IKLE2(IELEM))
      V3 = V(IKLE3(IELEM))
!
      GEL = G(IELEM)
      HEL = H(IELEM)
!
      W1(IELEM) = (-(3*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V3+4
     &   *X2*V2+4*X2*V1-U3*Y2-4*U2*Y2-4*U1*Y2)-(X2*HEL-2*X3*HEL+
     &   2*GEL*Y3-GEL*Y2)*(4*X3*V3+X3*V2+4*X3*V1-4*U3*Y3-U2*Y3-4
     &   *U1*Y3))*F4-(2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V3+4*X2*
     &   V2+4*X2*V1+X3*V3+4*X3*V2+4*X3*V1-U3*Y3-U3*Y2-4*U2*Y3-
     &   4*U2*Y2-4*U1*Y3-4*U1*Y2)*F2+(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL
     &   *Y2)*(4*X2*V3+X2*V2+4*X2*V1+4*X3*V3+X3*V2+4*X3*V1-4*
     &   U3*Y3-4*U3*Y2-U2*Y3-U2*Y2-4*U1*Y3-4*U1*Y2)*F3))
     &   *XMUL/(54*(X2*Y3-X3*Y2))
!
      W2(IELEM) = (F2*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(X2*V3+4*X2*V2+
     &   4*X2*V1+X3*V3+4*X3*V2+4*X3*V1-U3*Y3-U3*Y2-4*U2*Y3-4*
     &   U2*Y2-4*U1*Y3-4*U1*Y2)+(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*Y2)*
     &   (4*X2*V3+4*X2*V2+X2*V1-8*X3*V3-8*X3*V2-2*X3*V1+8*U3
     &   *Y3-4*U3*Y2+8*U2*Y3-4*U2*Y2+2*U1*Y3-U1*Y2))+F3*(X2*HEL
     &   -2*X3*HEL+2*GEL*Y3-GEL*Y2)*(8*X2*V3+8*X2*V2+2*X2*V1-4*
     &   X3*V3-4*X3*V2-X3*V1+4*U3*Y3-8*U3*Y2+4*U2*Y3-8*U2*Y2+
     &   U1*Y3-2*U1*Y2)-3*F4*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(X2*V3+
     &   4*X2*V2+4*X2*V1-U3*Y2-4*U2*Y2-4*U1*Y2)+(X2*HEL-2*X3*HEL
     &   +2*GEL*Y3-GEL*Y2)*(4*X2*V3+4*X2*V2+X2*V1-4*X3*V3-4*X3*
     &   V2-X3*V1+4*U3*Y3-4*U3*Y2+4*U2*Y3-4*U2*Y2+U1*Y3-U1*Y2)
     &   ))*XMUL/(54*(X2*Y3-X3*Y2))
!
      W3(IELEM) = (F2*(2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(4*X2*V3+4
     &   *X2*V2+X2*V1-8*X3*V3-8*X3*V2-2*X3*V1+8*U3*Y3-4*U3*Y2
     &   +8*U2*Y3-4*U2*Y2+2*U1*Y3-U1*Y2)+F3*((2*X2*HEL-X3*HEL+GEL
     &   *Y3-2*GEL*Y2)*(8*X2*V3+8*X2*V2+2*X2*V1-4*X3*V3-4*X3*
     &   V2-X3*V1+4*U3*Y3-8*U3*Y2+4*U2*Y3-8*U2*Y2+U1*Y3-2*U1*
     &   Y2)+(X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(4*X2*V3+X2*V2+4*X2*V1+4
     &   *X3*V3+X3*V2+4*X3*V1-4*U3*Y3-4*U3*Y2-U2*Y3-U2*Y2-4*U1
     &   *Y3-4*U1*Y2))-3*F4*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(4
     &   *X2*V3+4*X2*V2+X2*V1-4*X3*V3-4*X3*V2-X3*V1+4*U3*Y3-4
     &   *U3*Y2+4*U2*Y3-4*U2*Y2+U1*Y3-U1*Y2)+(X2*HEL+X3*HEL-GEL*Y3-
     &   GEL*Y2)*(4*X3*V3+X3*V2+4*X3*V1-4*U3*Y3-U2*Y3-4*U1*Y3))
     &   )*XMUL/(54*(X2*Y3-X3*Y2))
!
      W4(IELEM) = (-(((X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(8*X2*V3+8*X2*V2
     &   +2*X2*V1-4*X3*V3-4*X3*V2-X3*V1+4*U3*Y3-8*U3*Y2+4*U2
     &   *Y3-8*U2*Y2+U1*Y3-2*U1*Y2)+(4*X2*V3+X2*V2+4*X2*V1+4*
     &   X3*V3+X3*V2+4*X3*V1-4*U3*Y3-4*U3*Y2-U2*Y3-U2*Y2-4*U1*
     &   Y3-4*U1*Y2)*(X3*HEL-GEL*Y3))*F3-3*((X2*HEL-X3*HEL+GEL*Y3-GEL*
     &   Y2)*(4*X2*V3+4*X2*V2+X2*V1-4*X3*V3-4*X3*V2-X3*V1+4*
     &   U3*Y3-4*U3*Y2+4*U2*Y3-4*U2*Y2+U1*Y3-U1*Y2)+(X2*HEL-GEL*
     &   Y2)*(X2*V3+4*X2*V2+4*X2*V1-U3*Y2-4*U2*Y2-4*U1*Y2)+(X3
     &   *HEL-GEL*Y3)*(4*X3*V3+X3*V2+4*X3*V1-4*U3*Y3-U2*Y3-4*U1*
     &   Y3))*F4+((X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(4*X2*V3+4*X2*V2+X2*
     &   V1-8*X3*V3-8*X3*V2-2*X3*V1+8*U3*Y3-4*U3*Y2+8*U2*Y3-
     &   4*U2*Y2+2*U1*Y3-U1*Y2)+(X2*HEL-GEL*Y2)*(X2*V3+4*X2*V2+4*
     &   X2*V1+X3*V3+4*X3*V2+4*X3*V1-U3*Y3-U3*Y2-4*U2*Y3-4*U2*
     &   Y2-4*U1*Y3-4*U1*Y2))*F2))*XMUL/(18*(X2*Y3-X3*Y2))
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!     F IS QUASI-B; G AND H (NOT CHECKED) P0; AND U, V (NOT CHECKED) QUASI-B
!
      ELSEIF(IELMF.EQ.12.AND.IELMG.EQ.10.AND.IELMH.EQ.10.
     &                   AND.IELMU.EQ.12.AND.IELMV.EQ.12  ) THEN
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
      F4 = F(IKLE4(IELEM)) - F1
!
      U1 = U(IKLE1(IELEM))
      U2 = U(IKLE2(IELEM))
      U3 = U(IKLE3(IELEM))
      U4 = U(IKLE4(IELEM))
!
      V1 = V(IKLE1(IELEM))
      V2 = V(IKLE2(IELEM))
      V3 = V(IKLE3(IELEM))
      V4 = V(IKLE4(IELEM))
!
      GEL = G(IELEM)
      HEL = H(IELEM)
!
      W1(IELEM) = (-(3*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V4+X2
     &   *V2+X2*V1-U4*Y2-U2*Y2-U1*Y2)-(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*
     &   Y2)*(X3*V3+X3*V4+X3*V1-U3*Y3-U4*Y3-U1*Y3))*F4-(2*X2*HEL-
     &   X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V4+X2*V2+X2*V1+X3*V4+X3*V2+X3*
     &   V1-U4*Y3-U4*Y2-U2*Y3-U2*Y2-U1*Y3-U1*Y2)*F2+(X2*HEL-2*X3*
     &   HEL+2*GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V1+X3*V3+X3*V4+X3*V1-
     &   U3*Y3-U3*Y2-U4*Y3-U4*Y2-U1*Y3-U1*Y2)*F3))
     &   *XMUL/(18*(X2*Y3-X3*Y2))
!
      W2(IELEM) = (F2*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*(X2*V4+X2*V2+X2*
     &   V1+X3*V4+X3*V2+X3*V1-U4*Y3-U4*Y2-U2*Y3-U2*Y2-U1*Y3-U1*Y2)
     &   +(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V2-2*X3
     &   *V3-2*X3*V4-2*X3*V2+2*U3*Y3-U3*Y2+2*U4*Y3-U4*Y2+2*U2
     &   *Y3-U2*Y2))+F3*(X2*HEL-2*X3*HEL+2*GEL*Y3-GEL*Y2)*(2*X2*V3+
     &   2*X2*V4+2*X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-2*U3*Y2+U4*Y3-
     &   2*U4*Y2+U2*Y3-2*U2*Y2)-3*F4*((X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*
     &   (X2*V4+X2*V2+X2*V1-U4*Y2-U2*Y2-U1*Y2)+(X2*HEL-2*X3*HEL+2*
     &   GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-
     &   U3*Y2+U4*Y3-U4*Y2+U2*Y3-U2*Y2)))*XMUL/(18*(X2*Y3-X3*Y2))
!
      W3(IELEM) = (F2*(2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2)*(X2*V3+X2*V4
     &   +X2*V2-2*X3*V3-2*X3*V4-2*X3*V2+2*U3*Y3-U3*Y2+2*U4*Y3
     &   -U4*Y2+2*U2*Y3-U2*Y2)+F3*((2*X2*HEL-X3*HEL+GEL*Y3-2*GEL*Y2
     &   )*(2*X2*V3+2*X2*V4+2*X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-2*
     &   U3*Y2+U4*Y3-2*U4*Y2+U2*Y3-2*U2*Y2)+(X2*HEL+X3*HEL-GEL*Y3-
     &   GEL*Y2)*(X2*V3+X2*V4+X2*V1+X3*V3+X3*V4+X3*V1-U3*Y3-U3*Y2-
     &   U4*Y3-U4*Y2-U1*Y3-U1*Y2))-3*F4*((2*X2*HEL-X3*HEL+GEL*Y3-2
     &   *GEL*Y2)*(X2*V3+X2*V4+X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-U3*Y2+
     &   U4*Y3-U4*Y2+U2*Y3-U2*Y2)+(X2*HEL+X3*HEL-GEL*Y3-GEL*Y2)*
     &   (X3*V3+X3*V4+X3*V1-U3*Y3-U4*Y3-U1*Y3)))
     &   *XMUL/(18*(X2*Y3-X3*Y2))
!
      W4(IELEM) = (-(((X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(2*X2*V3+2*X2*V4
     &   +2*X2*V2-X3*V3-X3*V4-X3*V2+U3*Y3-2*U3*Y2+U4*Y3-2*U4*Y2
     &   +U2*Y3-2*U2*Y2)+(X2*V3+X2*V4+X2*V1+X3*V3+X3*V4+X3*V1-U3*
     &   Y3-U3*Y2-U4*Y3-U4*Y2-U1*Y3-U1*Y2)*(X3*HEL-GEL*Y3))*F3-3*((
     &   X2*HEL-X3*HEL+GEL*Y3-GEL*Y2)*(X2*V3+X2*V4+X2*V2-X3*V3-X3*V4-
     &   X3*V2+U3*Y3-U3*Y2+U4*Y3-U4*Y2+U2*Y3-U2*Y2)+(X2*HEL-GEL*Y2)*
     &   (X2*V4+X2*V2+X2*V1-U4*Y2-U2*Y2-U1*Y2)+(X3*HEL-GEL*Y3)*(X3*
     &   V3+X3*V4+X3*V1-U3*Y3-U4*Y3-U1*Y3))*F4+((X2*HEL-X3*HEL+GEL*Y3
     &   -GEL*Y2)*(X2*V3+X2*V4+X2*V2-2*X3*V3-2*X3*V4-2*X3*V2+2*
     &   U3*Y3-U3*Y2+2*U4*Y3-U4*Y2+2*U2*Y3-U2*Y2)+(X2*HEL-GEL*Y2)*
     &   (X2*V4+X2*V2+X2*V1+X3*V4+X3*V2+X3*V1-U4*Y3-U4*Y2-U2*Y3-U2
     &   *Y2-U1*Y3-U1*Y2))*F2))*XMUL/(6*(X2*Y3-X3*Y2))
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
      ELSE
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMF,SF%NAME
        WRITE(LU,111) IELMG,SG%NAME
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC03BB (BIEF) :',/,
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
