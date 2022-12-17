!                   *****************
                    SUBROUTINE VC15AA
!                   *****************
!
     &( XMUL,SF,SU,SV,F,U,V,
     &  XEL,YEL,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     &  W1,W2,W3)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /                D(FU)    D(FV)
!+      V  =  XMUL   /       PSII  * (   --  +    -- )   D(OMEGA)
!+       I          /OMEGA               DX       DY
!+
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
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
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
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
      USE BIEF, EX_VC15AA => VC15AA
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
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION XSUR24,XSU216
!
!-----------------------------------------------------------------------
!
      XSUR24 = XMUL/24.D0
      XSU216 = XMUL/216.D0
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F AND VECTOR U ARE LINEAR
!
      IF(IELMF.EQ.11.AND.IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        F3 = F(IKLE3(IELEM))
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
!
        W1(IELEM)=(((2*F3+F2+F1)*V3+(F3-F2-4*F1)*V1+(F3-F1)*V2)*X2-((
     &         F3+2*F2+F1)*V2-(F3-F2+4*F1)*V1+(F2-F1)*V3)*X3-((Y3+Y2)*
     &         F3-(Y3+Y2)*F2+4*(Y3-Y2)*F1)*U1+((Y3+Y2)*F1+(Y3-Y2)*F3+2
     &         *F2*Y3)*U2-((Y3+Y2)*F1-(Y3-Y2)*F2+2*F3*Y2)*U3)*XSUR24
        W2(IELEM)=(-(((F3+4*F2-F1)*V2-(F3+F2+2*F1)*V1+(F2-F1)*V3)*X3-
     &         2*((F3+F2)*V3+(F3-F1)*V2-(F2+F1)*V1)*X2+(2*(Y3-Y2)*F1+(
     &         Y3-2*Y2)*F2+F3*Y3)*U1-((Y3-2*Y2)*F3-(Y3-2*Y2)*F1+4*F2
     &         *Y3)*U2-((Y3-2*Y2)*F2-2*F3*Y2-F1*Y3)*U3))*XSUR24
        W3(IELEM)=(((4*F3+F2-F1)*V3-(F3+F2+2*F1)*V1+(F3-F1)*V2)*X2-2
     &         *((F3+F2)*V2-(F3+F1)*V1+(F2-F1)*V3)*X3-((2*Y3-Y2)*F3+2*
     &          (Y3-Y2)*F1-F2*Y2)*U1+((2*Y3-Y2)*F3+2*F2*Y3+F1*Y2)*U2+((
     &            2*Y3-Y2)*F2-(2*Y3-Y2)*F1-4*F3*Y2)*U3)*XSUR24
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F IS LINEAR AND VECTOR U IS QUASI-BUBBLE
!
      ELSEIF(IELMF.EQ.11.AND.IELMU.EQ.12.AND.IELMU.EQ.12) THEN
!
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        F3 = F(IKLE3(IELEM))
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        U4 = U(IKLE4(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
        V4 = V(IKLE4(IELEM))
!
        W1(IELEM)=((14*X2*V3+12*X2*V4+5*X2*V2+5*X2*V1+4*X3*V3-
     &    12*X3*V4-5*X3*V2+13*X3*V1-4*U3*Y3-14*U3*Y2+12*U4*Y3-
     &    12*U4*Y2+5*U2*Y3-5*U2*Y2-13*U1*Y3-5*U1*Y2)*F3+(5*X2*
     &    V3+12*X2*V4-4*X2*V2-13*X2*V1-5*X3*V3-12*X3*V4-14*X3
     &    *V2-5*X3*V1+5*U3*Y3-5*U3*Y2+12*U4*Y3-12*U4*Y2+14*U2
     &    *Y3+4*U2*Y2+5*U1*Y3+13*U1*Y2)*F2+(5*X2*V3+12*X2*V4-
     &    13*X2*V2-40*X2*V1+13*X3*V3-12*X3*V4-5*X3*V2+40*X3*V1
     &    -13*U3*Y3-5*U3*Y2+12*U4*Y3-12*U4*Y2+5*U2*Y3+13*U2*
     &    Y2-40*U1*Y3+40*U1*Y2)*F1)*XSU216
        W2(IELEM)=(18*X2*V3*F3+18*X2*V3*F2+18*X2*V2*F3-18*X2*V2*
     &    F1-18*X2*V1*F2-18*X2*V1*F1-4*X3*V3*F3-13*X3*V3*F2+5*
     &    X3*V3*F1+12*X3*V4*F3+12*X3*V4*F2+12*X3*V4*F1-13*X3*V2
     &    *F3-40*X3*V2*F2+5*X3*V2*F1+5*X3*V1*F3+5*X3*V1*F2+14*
     &    X3*V1*F1+4*U3*Y3*F3+13*U3*Y3*F2-5*U3*Y3*F1-18*U3*Y2*
     &    F3-18*U3*Y2*F2-12*U4*Y3*F3-12*U4*Y3*F2-12*U4*Y3*F1+
     &    13*U2*Y3*F3+40*U2*Y3*F2-5*U2*Y3*F1-18*U2*Y2*F3+18*U2*
     &    Y2*F1-5*U1*Y3*F3-5*U1*Y3*F2-14*U1*Y3*F1+18*U1*Y2*F2+
     &    18*U1*Y2*F1)*XSU216
        W3(IELEM)=(40*X2*V3*F3+13*X2*V3*F2-5*X2*V3*F1-12*X2*V4*F3
     &    -12*X2*V4*F2-12*X2*V4*F1+13*X2*V2*F3+4*X2*V2*F2-5*X2
     &    *V2*F1-5*X2*V1*F3-5*X2*V1*F2-14*X2*V1*F1-18*X3*V3*F2+
     &    18*X3*V3*F1-18*X3*V2*F3-18*X3*V2*F2+18*X3*V1*F3+18*X3
     &    *V1*F1+18*U3*Y3*F2-18*U3*Y3*F1-40*U3*Y2*F3-13*U3*Y2*
     &    F2+5*U3*Y2*F1+12*U4*Y2*F3+12*U4*Y2*F2+12*U4*Y2*F1+18
     &    *U2*Y3*F3+18*U2*Y3*F2-13*U2*Y2*F3-4*U2*Y2*F2+5*U2*Y2*
     &    F1-18*U1*Y3*F3-18*U1*Y3*F1+5*U1*Y2*F3+5*U1*Y2*F2+14*
     &    U1*Y2*F1)*XSU216
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
101     FORMAT(1X,'VC15AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
