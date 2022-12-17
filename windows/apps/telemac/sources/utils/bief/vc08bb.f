!                   *****************
                    SUBROUTINE VC08BB
!                   *****************
!
     &(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     & IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4,FORMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /                  DF      DF
!+      V  =  XMUL   /       PSII  * ( U --  + V -- )   D(OMEGA)
!+       I          /OMEGA               DX      DY
!+
!+    PSI(I) IS A BASE OF QUASI-BUBBLE TYPE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        29/12/05
!+        V5P6
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
!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| STRING WITH FORMULA OF VECTOR
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
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
      USE BIEF, EX_VC08BB => VC08BB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX*3),YEL(NELMAX*3),XMUL
!     W1 IS ALSO USED AS 1-DIMENSIONAL FOR ALL W
      DOUBLE PRECISION, INTENT(INOUT) :: W1(4*NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
!     IKLE1 IS ALSO USED AS A 1-DIMENSIONAL IKLE
      INTEGER, INTENT(IN) :: IKLE1(4*NELMAX)
      INTEGER, INTENT(IN) :: IKLE2(NELMAX),IKLE3(NELMAX),IKLE4(NELMAX)
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMV
      INTEGER IG1,IG2,IG3,IT,IAD1,IAD2,IAD3
!
      DOUBLE PRECISION K1,K2,K3
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3,F4,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION XSUR72,XSU216,X1,Y1,TIERS
      DOUBLE PRECISION PHIT,SUR6,USUR2,VSUR2
      DOUBLE PRECISION L12,L13,L21,L23,L31,L32,SIG,BETAN1,BETAN2,BETAN3
!
      INTRINSIC MAX,MIN,SIGN
!
!-----------------------------------------------------------------------
!
!     FOR A QUASI-BUBBLE TRIANGLE : NODE NUMBERS OF THE SUB-TRIANGLES
!     IN THE INITIAL TRIANGLE
!     IL(NUMBER OF THE SUB-TRIANGLE,LOCAL NUMBER IN THE SUB-TRIANGLE)
!
      INTEGER :: IL(3,3)
      PARAMETER ( IL = RESHAPE( (/
     &          1,2,3,2,3,1,4,4,4 /), SHAPE=(/ 3,3 /) ) )
!
!-----------------------------------------------------------------------
!
      XSUR72 = XMUL/72.D0
      XSU216 = XMUL/216.D0
      TIERS  = 1.D0 / 3.D0
      SUR6   = 1.D0 / 6.D0
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F AND VECTOR U ARE QUASI-BUBBLE
!
      IF(IELMF.EQ.12.AND.IELMU.EQ.12.AND.IELMV.EQ.12) THEN
!
      IF(FORMUL(14:16).EQ.'PSI') THEN
!
!     INITIALISES W
!
      DO IELEM = 1 , NELEM
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = 0.D0
      ENDDO ! IELEM
!
!     PSI SCHEME, LOOP ON THE 3 SUB-TRIANGLES AND
!     PREASSEMBLY
!
      DO IT=1,3
      DO IELEM = 1 , NELEM
!
!     ADDRESSES IN ARRAY (NELMAX,*)
      IAD1= IELEM + (IL(IT,1)-1)*NELMAX
      IAD2= IELEM + (IL(IT,2)-1)*NELMAX
      IAD3= IELEM + (IL(IT,3)-1)*NELMAX
!     GLOBAL NUMBERS IN THE INITIAL TRIANGLE
      IG1 = IKLE1(IAD1)
      IG2 = IKLE1(IAD2)
      IG3 = IKLE1(IAD3)
!     COORDINATES OF THE SUB-TRIANGLE NODES
      X1 = XEL(IAD1)
      X2 = XEL(IAD2) - X1
!     POINT 3 IS ALWAYS THE CENTRE OF THE INITIAL TRIANGLE
      X3=TIERS*(XEL(IELEM)+XEL(IELEM+NELMAX)+XEL(IELEM+2*NELMAX))-X1
      Y1 = YEL(IAD1)
      Y2 = YEL(IAD2) - Y1
!     POINT 3 IS ALWAYS THE CENTRE OF THE INITIAL TRIANGLE
      Y3=TIERS*(YEL(IELEM)+YEL(IELEM+NELMAX)+YEL(IELEM+2*NELMAX))-Y1
!     F VALUES IN THE SUB-TRIANGLE
      F1 = F(IG1)
      F2 = F(IG2)
      F3 = F(IG3)
!
      USUR2 = (U(IG1)+U(IG2)+U(IG3))*SUR6
      VSUR2 = (V(IG1)+V(IG2)+V(IG3))*SUR6
!
      K1 = USUR2 * (Y2-Y3) - VSUR2 * (X2-X3)
      K2 = USUR2 * (Y3   ) - VSUR2 * (X3   )
      K3 = USUR2 * (  -Y2) - VSUR2 * (  -X2)
!
      L12 = MAX(  MIN(K1,-K2) , 0.D0 )
      L13 = MAX(  MIN(K1,-K3) , 0.D0 )
      L21 = MAX(  MIN(K2,-K1) , 0.D0 )
      L23 = MAX(  MIN(K2,-K3) , 0.D0 )
      L31 = MAX(  MIN(K3,-K1) , 0.D0 )
      L32 = MAX(  MIN(K3,-K2) , 0.D0 )
!
      BETAN1 = L12*(F1-F2) + L13*(F1-F3)
      BETAN2 = L21*(F2-F1) + L23*(F2-F3)
      BETAN3 = L31*(F3-F1) + L32*(F3-F2)
!
      PHIT = BETAN1 + BETAN2 + BETAN3
      SIG  = SIGN(1.D0,PHIT)
!
      W1(IAD1)=W1(IAD1)+ XMUL * SIG * MAX(MIN(SIG*BETAN1,SIG*PHIT),0.D0)
      W1(IAD2)=W1(IAD2)+ XMUL * SIG * MAX(MIN(SIG*BETAN2,SIG*PHIT),0.D0)
      W1(IAD3)=W1(IAD3)+ XMUL * SIG * MAX(MIN(SIG*BETAN3,SIG*PHIT),0.D0)
!
      ENDDO ! IELEM
      ENDDO ! IT
!
      ELSE
!
!     CLASSICAL COMPUTATION
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM+NELMAX)
        X3 = XEL(IELEM+2*NELMAX)
        Y2 = YEL(IELEM+NELMAX)
        Y3 = YEL(IELEM+2*NELMAX)
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
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
!
        W1(IELEM)= (((V3+V4+2*V1)*X2+(V3+V4+2*V1)*X3-(Y3+Y2)*U3-(Y3+
     &    Y2)*U4-2*(Y3+Y2)*U1)*F3-3*((V3+V4+2*V1)*X3-(V4+V2+2*
     &    V1)*X2-(Y3-Y2)*U4-2*(Y3-Y2)*U1-U3*Y3+U2*Y2)*F4-((V4+V2+
     &    2*V1)*X2+(V4+V2+2*V1)*X3-(Y3+Y2)*U4-(Y3+Y2)*U2-2*(Y3+Y2
     &    )*U1)*F2)*XSUR72
!
        W2(IELEM)= (-(((2*V3+3*V4+6*V2+V1)*X3-(V3-V1)*X2-(2*Y3-Y2)
     &    *U3-(Y3+Y2)*U1-3*U4*Y3-6*U2*Y3)*F2-(2*(V3+V4+2*V2)*X2
     &    -(V3+V4+2*V2)*X3+(Y3-2*Y2)*U3+(Y3-2*Y2)*U4+2*(Y3-2*
     &    Y2)*U2)*F3-3*((V3+V4+2*V2)*X3-(V3-V1)*X2-(Y3-Y2)*U3-U4*
     &    Y3-2*U2*Y3-U1*Y2)*F4))*XSUR72
!
        W3(IELEM)= (((6*V3+3*V4+2*V2+V1)*X2-(V2-V1)*X3-(Y3+Y2)*U1+(
     &    Y3-2*Y2)*U2-6*U3*Y2-3*U4*Y2)*F3+((2*V3+V4+V2)*X2-2*(
     &    2*V3+V4+V2)*X3+2*(2*Y3-Y2)*U3+(2*Y3-Y2)*U4+(2*Y3-Y2)*
     &    U2)*F2-3*((2*V3+V4+V2)*X2-(V2-V1)*X3+(Y3-Y2)*U2-2*U3*
     &    Y2-U4*Y2-U1*Y3)*F4)*XSUR72
!
        W4(IELEM)= (((3*V3+6*V4+2*V2+V1)*X2-(V2-V1)*X3-(Y3+Y2)*U1+(
     &    Y3-2*Y2)*U2-3*U3*Y2-6*U4*Y2)*F3-((2*V3+6*V4+3*V2+V1
     &    )*X3-(V3-V1)*X2-(2*Y3-Y2)*U3-(Y3+Y2)*U1-6*U4*Y3-3*U2*
     &    Y3)*F2-3*((V3-V1)*X2-(V2-V1)*X3-(Y3-Y2)*U1-U3*Y2+U2*Y3)*
     &    F4)*XSUR72
!
      ENDDO ! IELEM
!
      ENDIF
!
!     FUNCTION F IS QUASI-BUBBLE AND VECTOR U LINEAR
!
      ELSEIF(IELMF.EQ.12.AND.IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
      IF(FORMUL(14:16).EQ.'PSI') THEN
!
        WRITE(LU,401)
401   FORMAT(1X,'VC08BB (BIEF) : PSI NOT IMPLEMENTED FOR QUASI-BUBBLE')
        CALL PLANTE(1)
        STOP
!
      ELSE
!
!     CLASSICAL COMPUTATION
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM+NELMAX)
        X3 = XEL(IELEM+2*NELMAX)
        Y2 = YEL(IELEM+NELMAX)
        Y3 = YEL(IELEM+2*NELMAX)
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
!
        W1(IELEM)=(((4*V3+V2+7*V1)*X2+(4*V3+V2+7*V1)*X3-4*(Y3+Y2
     &   )*U3-(Y3+Y2)*U2-7*(Y3+Y2)*U1)*F3-3*((4*V3+V2+7*V1)*X3
     &   -(V3+4*V2+7*V1)*X2-(4*Y3-Y2)*U3-7*(Y3-Y2)*U1-(Y3-4*
     &   Y2)*U2)*F4-((V3+4*V2+7*V1)*X2+(V3+4*V2+7*V1)*X3-(Y3+
     &   Y2)*U3-4*(Y3+Y2)*U2-7*(Y3+Y2)*U1)*F2)*XSU216
        W2(IELEM)=((2*(4*V3+7*V2+V1)*X2-(4*V3+7*V2+V1)*X3+4*(Y3
     &   -2*Y2)*U3+7*(Y3-2*Y2)*U2+(Y3-2*Y2)*U1)*F3+3*((4*V3+
     &   7*V2+V1)*X3-3*(V3-V1)*X2-(4*Y3-3*Y2)*U3-(Y3+3*Y2)*U1-
     &   7*U2*Y3)*F4-3*((3*V3+7*V2+2*V1)*X3-(V3-V1)*X2-(3*Y3-
     &   Y2)*U3-(2*Y3+Y2)*U1-7*U2*Y3)*F2)*XSU216
        W3(IELEM)=(((7*V3+4*V2+V1)*X2-2*(7*V3+4*V2+V1)*X3+7*(2
     &   *Y3-Y2)*U3+4*(2*Y3-Y2)*U2+(2*Y3-Y2)*U1)*F2-3*((7*V3+
     &   4*V2+V1)*X2-3*(V2-V1)*X3-(3*Y3+Y2)*U1+(3*Y3-4*Y2)*U2-
     &   7*U3*Y2)*F4+3*((7*V3+3*V2+2*V1)*X2-(V2-V1)*X3-(Y3+2*
     &   Y2)*U1+(Y3-3*Y2)*U2-7*U3*Y2)*F3)*XSU216
        W4(IELEM)=(((5*V3+4*V2+3*V1)*X2-(V2-V1)*X3-(Y3+3*Y2)*U1+(
     &   Y3-4*Y2)*U2-5*U3*Y2)*F3-((4*V3+5*V2+3*V1)*X3-(V3-V1)
     &   *X2-(4*Y3-Y2)*U3-(3*Y3+Y2)*U1-5*U2*Y3)*F2-3*((V3-V1)*
     &   X2-(V2-V1)*X3-(Y3-Y2)*U1-U3*Y2+U2*Y3)*F4)*XSUR72
!
      ENDDO ! IELEM
!
      ENDIF
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
101     FORMAT(1X,'VC08BB (BIEF) :',/,
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
