!                   *****************
                    SUBROUTINE MT12AB
!                   *****************
!
     &(  A11 , A12 , A13 , A14 ,
     &   A21 , A22 , A23 , A24 ,
     &   A31 , A32 , A33 , A34 ,
     &   XMUL,SF,SU,SV,F,U,V,
     &   XEL,YEL,SURFAC,
     &   IKLE1,IKLE2,IKLE3,IKLE4,
     &   NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD=1
!+
!+                 /           D       -> -->
!+  A(I,J)= XMUL  /  PSI2(J) * --(F) * U.GRAD(PSI1(I))  D(OMEGA)
!+               /OMEGA        DX
!+
!+
!+  PSI1 : BASES OF TYPE P1 TRIANGLE
!+  PSI2 : BASES OF TYPE QUASI-BUBBLE
!+  F    : FUNCTION OF TYPE P1 TRIANGLE
!+  U    : VECTOR OF TYPE P0 OR QUASI-BUBBLE
!+
!+  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A14            |<--| ELEMENTS OF MATRIX
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SURFAC         |-->| AREA OF TRIANGLES
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| V              |-->| FUNCTION V USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT12AB => MT12AB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*),A34(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
!     STRUCTURES OF F, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION X2,X3,Y2,Y3,F1,F2,F3
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,UX,UY
      DOUBLE PRECISION XSUR18,XSUR12,XSUR72,XSU144
      DOUBLE PRECISION AUX18,AUX12,AUX144,AUX72,UNSURF
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
      XSUR18 = XMUL /  18.D0
      XSUR12 = XMUL /  12.D0
      XSUR72 = XMUL /  72.D0
      XSU144 = XMUL / 144.D0
!
!-----------------------------------------------------------------------
!  CASE WHERE F IS OF TYPE P1
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.11) THEN
!
      IF(IELMU.EQ.10.AND.IELMV.EQ.10) THEN
!
!================================
!  DERIVATIVE WRT X  =
!================================
!
        IF(ICOORD.EQ.1) THEN
!
!   LOOP ON THE ELEMENTS
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM)) - F1
        F3  =  F(IKLE3(IELEM)) - F1
!
        UX  =  U(IELEM)
        UY  =  V(IELEM)
!
        UNSURF= 1.D0 / SURFAC(IELEM)
        AUX12 = XSUR12 * UNSURF
        AUX18 = XSUR18 * UNSURF
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)=-( (Y3-Y2)*UX+X2*UY-X3*UY )*(Y3*F2-Y2*F3)*AUX18
        A14(IELEM)=-( (Y3-Y2)*UX+X2*UY-X3*UY )*(Y3*F2-Y2*F3)*AUX12
        A21(IELEM)=-(X3*UY-UX*Y3)*(Y3*F2-Y2*F3)*AUX18
        A24(IELEM)=-(X3*UY-UX*Y3)*(Y3*F2-Y2*F3)*AUX12
        A31(IELEM)= (X2*UY-UX*Y2)*(Y3*F2-Y2*F3)*AUX18
        A34(IELEM)= (X2*UY-UX*Y2)*(Y3*F2-Y2*F3)*AUX12
        A13(IELEM)= A12(IELEM)
        A23(IELEM)= A21(IELEM)
        A32(IELEM)= A31(IELEM)
!
!   DIAGONAL TERMS
!   THE SUM OF EACH COLUMN IS 0
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
        ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM)) - F1
        F3  =  F(IKLE3(IELEM)) - F1
!
        UX  =  U(IELEM)
        UY  =  V(IELEM)
!
        UNSURF= 1.D0 / SURFAC(IELEM)
        AUX12 = XSUR12 * UNSURF
        AUX18 = XSUR18 * UNSURF
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)=-(X2*UY-X3*UY+UX*Y3-UX*Y2)*(X2*F3-X3*F2)*AUX18
        A14(IELEM)=-(X2*UY-X3*UY+UX*Y3-UX*Y2)*(X2*F3-X3*F2)*AUX12
        A21(IELEM)=-(X2*F3-X3*F2)*(X3*UY-UX*Y3)*AUX18
        A24(IELEM)=-(X2*F3-X3*F2)*(X3*UY-UX*Y3)*AUX12
        A31(IELEM)= (X2*UY-UX*Y2)*(X2*F3-X3*F2)*AUX18
        A34(IELEM)= (X2*UY-UX*Y2)*(X2*F3-X3*F2)*AUX12
        A13(IELEM)= A12(IELEM)
        A23(IELEM)= A21(IELEM)
        A32(IELEM)= A31(IELEM)
!
!   DIAGONAL TERMS
!   THE SUM OF THE MATRIX ROWS IS 0 (VECTOR)
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
        ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
!
!
      ELSEIF(IELMU.EQ.12) THEN
!
!================================
!  DERIVATIVE WRT X  =
!================================
!
        IF(ICOORD.EQ.1) THEN
!
!   LOOP ON THE ELEMENTS
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM)) - F1
        F3  =  F(IKLE3(IELEM)) - F1
!
        U1  =  U(IKLE1(IELEM))
        U2  =  U(IKLE2(IELEM))
        U3  =  U(IKLE3(IELEM))
        U4  =  U(IKLE4(IELEM))
        V1  =  V(IKLE1(IELEM))
        V2  =  V(IKLE2(IELEM))
        V3  =  V(IKLE3(IELEM))
        V4  =  V(IKLE4(IELEM))
!
        UNSURF= 1.D0 / SURFAC(IELEM)
        AUX144= XSU144 * UNSURF
        AUX72 = XSUR72 * UNSURF
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)=(-((V3+V4+2*V2)*X2-(V3+V4+2*V2)*X3+(V4+2*V2+
     &   V1)*X2-(V4+2*V2+V1)*X3+(Y3-Y2)*U3+2*(Y3-Y2)*U4+4*(Y3-
     &   Y2)*U2+(Y3-Y2)*U1)*(Y3*F2-Y2*F3))*AUX144
        A13(IELEM)=(-((2*V3+V4+V2)*X2-(2*V3+V4+V2)*X3+(2*V3+V4+
     &   V1)*X2-(2*V3+V4+V1)*X3+4*(Y3-Y2)*U3+2*(Y3-Y2)*U4+(Y3-
     &   Y2)*U2+(Y3-Y2)*U1)*(Y3*F2-Y2*F3))*AUX144
        A14(IELEM)=(-(X2*V3+3*X2*V4+X2*V2+X2*V1-X3*V3-3*X3*V4-X3
     &   *V2-X3*V1+U3*Y3-U3*Y2+3*U4*Y3-3*U4*Y2+U2*Y3-U2*Y2+U1*Y3
     &   -U1*Y2)*(Y3*F2-Y2*F3))*AUX72
        A21(IELEM)=(-(X3*V3+2*X3*V4+X3*V2+4*X3*V1-U3*Y3-2*U4*Y3
     &   -U2*Y3-4*U1*Y3)*(Y3*F2-Y2*F3))*AUX144
        A23(IELEM)=(-(4*X3*V3+2*X3*V4+X3*V2+X3*V1-4*U3*Y3-2*U4
     &   *Y3-U2*Y3-U1*Y3)*(Y3*F2-Y2*F3))*AUX144
        A24(IELEM)=(-(X3*V3+3*X3*V4+X3*V2+X3*V1-U3*Y3-3*U4*Y3-U2
     &   *Y3-U1*Y3)*(Y3*F2-Y2*F3))*AUX72
        A31(IELEM)=((X2*V3+2*X2*V4+X2*V2+4*X2*V1-U3*Y2-2*U4*Y2-
     &   U2*Y2-4*U1*Y2)*(Y3*F2-Y2*F3))*AUX144
        A32(IELEM)=((X2*V3+2*X2*V4+4*X2*V2+X2*V1-U3*Y2-2*U4*Y2-
     &   4*U2*Y2-U1*Y2)*(Y3*F2-Y2*F3))*AUX144
        A34(IELEM)=((X2*V3+3*X2*V4+X2*V2+X2*V1-U3*Y2-3*U4*Y2-U2*
     &   Y2-U1*Y2)*(Y3*F2-Y2*F3))*AUX72
!
!   DIAGONAL TERMS
!   THE SUM OF EACH COLUMN IS 0
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
        ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM)) - F1
        F3  =  F(IKLE3(IELEM)) - F1
!
        U1  =  U(IKLE1(IELEM))
        U2  =  U(IKLE2(IELEM))
        U3  =  U(IKLE3(IELEM))
        U4  =  U(IKLE4(IELEM))
        V1  =  V(IKLE1(IELEM))
        V2  =  V(IKLE2(IELEM))
        V3  =  V(IKLE3(IELEM))
        V4  =  V(IKLE4(IELEM))
!
        UNSURF= 1.D0 / SURFAC(IELEM)
        AUX144= XSU144 * UNSURF
        AUX72= XSUR72 * UNSURF
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)=(-(X2*V3+2*X2*V4+4*X2*V2+X2*V1-X3*V3-2*X3*V4
     &   -4*X3*V2-X3*V1+U3*Y3-U3*Y2+2*U4*Y3-2*U4*Y2+4*U2*Y3-4
     &   *U2*Y2+U1*Y3-U1*Y2)*(X2*F3-X3*F2))*AUX144
        A13(IELEM)=(-(4*X2*V3+2*X2*V4+X2*V2+X2*V1-4*X3*V3-2*X3
     &   *V4-X3*V2-X3*V1+4*U3*Y3-4*U3*Y2+2*U4*Y3-2*U4*Y2+U2*Y3
     &   -U2*Y2+U1*Y3-U1*Y2)*(X2*F3-X3*F2))*AUX144
        A14(IELEM)=(-(X2*V3+3*X2*V4+X2*V2+X2*V1-X3*V3-3*X3*V4-X3
     &   *V2-X3*V1+U3*Y3-U3*Y2+3*U4*Y3-3*U4*Y2+U2*Y3-U2*Y2+U1*Y3
     &   -U1*Y2)*(X2*F3-X3*F2))*AUX72
        A21(IELEM)=(-(X2*F3-X3*F2)*(X3*V3+2*X3*V4+X3*V2+4*X3*V1-
     &   U3*Y3-2*U4*Y3-U2*Y3-4*U1*Y3))*AUX144
        A23(IELEM)=(-(X2*F3-X3*F2)*(4*X3*V3+2*X3*V4+X3*V2+X3*V1-
     &   4*U3*Y3-2*U4*Y3-U2*Y3-U1*Y3))*AUX144
        A24(IELEM)=(-(X2*F3-X3*F2)*(X3*V3+3*X3*V4+X3*V2+X3*V1-U3*
     &   Y3-3*U4*Y3-U2*Y3-U1*Y3))*AUX72
        A31(IELEM)=((X2*V3+2*X2*V4+X2*V2+4*X2*V1-U3*Y2-2*U4*Y2-
     &   U2*Y2-4*U1*Y2)*(X2*F3-X3*F2))*AUX144
        A32(IELEM)=((X2*V3+2*X2*V4+4*X2*V2+X2*V1-U3*Y2-2*U4*Y2-
     &   4*U2*Y2-U1*Y2)*(X2*F3-X3*F2))*AUX144
        A34(IELEM)=((X2*V3+3*X2*V4+X2*V2+X2*V1-U3*Y2-3*U4*Y2-U2*
     &   Y2-U1*Y2)*(X2*F3-X3*F2))*AUX72
!
!   DIAGONAL TERMS
!   THE SUM OF THE MATRIX ROWS IS 0 (VECTOR)
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
        ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
!
      ELSE
!
        WRITE(LU,301) IELMU
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,101) IELMF
101     FORMAT(1X,'MT12AB (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
201   FORMAT(1X,'MT12AB (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
!
301   FORMAT(1X,'MT12AB (BIEF) :',/,
     &        1X,'DISCRETIZATION OF U : ',1I6,' NOT AVAILABLE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
