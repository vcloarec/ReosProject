!                   *****************
                    SUBROUTINE MT12AC
!                   *****************
!
     &(  A11 , A12 , A13 , A14 , A15, A16,
     &   A21 , A22 , A23 , A24 , A25, A26,
     &   A31 , A32 , A33 , A34 , A35, A36,
     &   XMUL,SF,SU,SV,F,U,V,
     &   XEL,YEL,SURFAC,
     &   IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
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
!+  PSI2 : BASES OF TYPE P2
!+  F    : FUNCTION OF TYPE P1 TRIANGLE
!+  U    : VECTOR OF TYPE P0 OR P2
!+
!+  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  ALGIANE FROEHLY (MATMECA)
!+        19/06/08
!+        V5P9
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
!| ...            |<--| ELEMENTS OF MATRIX
!| A36            |<--| ELEMENTS OF MATRIX
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
      USE BIEF !, EX_MT12AC => MT12AC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
      INTEGER, INTENT(IN) :: IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
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
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6,UX,UY
      DOUBLE PRECISION XSUR12 ,XSUR180,XSUR720
      DOUBLE PRECISION AUX12  ,AUX180 ,AUX720 , UNSURF
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
      XSUR12  = XMUL /  12.D0
      XSUR720 = XMUL / 720.D0
      XSUR180 = XMUL / 180.D0
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
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)= 0.D0
        A13(IELEM)= 0.D0
        A21(IELEM)= 0.D0
        A23(IELEM)= 0.D0
        A24(IELEM)= (Y3*UX-UY*X3) * (Y3*F2-Y2*F3) * AUX12
        A25(IELEM)= A24(IELEM)
        A26(IELEM)= A24(IELEM)
        A31(IELEM)= 0.D0
        A32(IELEM)= 0.D0
        A34(IELEM)= (X2*UY-UX*Y2) * (Y3*F2-Y2*F3) * AUX12
        A35(IELEM)= A34(IELEM)
        A36(IELEM)= A34(IELEM)
!
!   THE SUM OF EACH COLUMN IS 0
!
        A14(IELEM)= - A24(IELEM) - A34(IELEM)
        A15(IELEM)= A14(IELEM)
        A16(IELEM)= A14(IELEM)
!
!C DIAGONAL ELEMENTS
!
        A11(IELEM) = 0.D0
        A22(IELEM) = 0.D0
        A33(IELEM) = 0.D0
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
        UNSURF = 1.D0 / SURFAC(IELEM)
        AUX12 = XSUR12 * UNSURF
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)= 0.D0
        A13(IELEM)= 0.D0
        A21(IELEM)= 0.D0
        A23(IELEM)= 0.D0
        A24(IELEM)= (X3*F2-X2*F3) * (UY*X3-UX*Y3) * AUX12
        A25(IELEM)=  A24(IELEM)
        A26(IELEM)=  A24(IELEM)
        A31(IELEM)= 0.D0
        A32(IELEM)= 0.D0
        A34(IELEM)= (X3*F2-X2*F3) * (UX*Y2-UY*X2) * AUX12
        A35(IELEM)=  A34(IELEM)
        A36(IELEM)=  A34(IELEM)
!
!   THE SUM OF THE MATRIX ROWS IS 0 (VECTOR)
!
        A14(IELEM)= - A24(IELEM) - A34(IELEM)
        A15(IELEM)=   A14(IELEM)
        A16(IELEM)=   A14(IELEM)
!
!C   DIAGONAL TERMS
!
        A11(IELEM) = 0.D0
        A22(IELEM) = 0.D0
        A33(IELEM) = 0.D0
!
        ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(IELMU.EQ.13) THEN
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
        U5  =  U(IKLE5(IELEM))
        U6  =  U(IKLE6(IELEM))
        V1  =  V(IKLE1(IELEM))
        V2  =  V(IKLE2(IELEM))
        V3  =  V(IKLE3(IELEM))
        V4  =  V(IKLE4(IELEM))
        V5  =  V(IKLE5(IELEM))
        V6  =  V(IKLE6(IELEM))
!
        UNSURF = 1.D0 / SURFAC(IELEM)
        AUX720 = XSUR720 * UNSURF
        AUX180 = XSUR180 * UNSURF
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)= (Y3*F2-Y2*F3) * ((U3+U1-6.D0*U2+4.D0*U6) * (Y3-Y2)
     &            + (V3+V1-6.D0*V2+4.D0*V6) * (X2-X3)) * AUX720
!
        A13(IELEM)= (Y3*F2-Y2*F3) * ((4.D0*V4-6.D0*V3+V2+V1) * (X2-X3)
     &            + (6.D0*U3-4.D0*U4-U2-U1) * (Y2-Y3)) * AUX720
!
        A21(IELEM)= (Y3*F2-Y2*F3) * ((4.D0*V5+V3+V2-6.D0*V1) * X3
     &            - (U3-6.D0*U1+4.D0*U5+U2) * Y3     ) * AUX720
!
        A23(IELEM)= (Y3*F2-Y2*F3) * ((V1-6.D0*V3+4.D0*V4+V2) * X3
     &            + (6.D0*U3-4.D0*U4-U2-U1) * Y3     ) * AUX720
!
        A24(IELEM)= (Y3*F2-Y2*F3) * ((V3-4.D0*V6-8.D0*V4-4.D0*V5) * X3
     &            - (U3-8.D0*U4-4.D0*U6-4.D0*U5) * Y3) * AUX180
!
        A25(IELEM)= (Y3*F2-Y2*F3) * ((V1-4.D0*V6-8.D0*V5-4.D0*V4) * X3
     &            + (4.D0*U6-U1+4.D0*U4+8.D0*U5) * Y3) * AUX180
!
        A26(IELEM)= (Y3*F2-Y2*F3) * ((V2-4.D0*V4-4.D0*V5-8.D0*V6) * X3
     &            + (8.D0*U6+4.D0*U4+4.D0*U5-U2) * Y3) * AUX180
!
        A31(IELEM)= (Y3*F2-Y2*F3) * ((6.D0*V1-4.D0*V5-V3-V2) * X2
     &            + (U3-6.D0*U1+4.D0*U5+U2) * Y2         ) * AUX720
!
        A32(IELEM)= (Y3*F2-Y2*F3) * ((-4.D0*V6+6.D0*V2-V1-V3) * X2
     &            + (U3+U1+4.D0*U6-6.D0*U2) * Y2        ) * AUX720
!
        A34(IELEM)= (Y3*F2-Y2*F3) * ((4.D0*V6+8.D0*V4+4.D0*V5-V3) * X2
     &            + (U3-8.D0*U4-4.D0*U6-4.D0*U5) * Y2    ) * AUX180
!
        A35(IELEM)= (Y2*F3-Y3*F2) * ((V1-8.D0*V5-4.D0*V4-4.D0*V6) * X2
     &            + (4.D0*U6-U1+4.D0*U4+8.D0*U5) * Y2   ) * AUX180
!
        A36(IELEM)= (Y2*F3-Y3*F2) * ((V2-4.D0*V4-4.D0*V5-8.D0*V6) * X2
     &            + (8.D0*U6+4.D0*U4+4.D0*U5-U2) * Y2   ) * AUX180
!
!   THE SUM OF THE MATRIX ROWS IS 0 (VECTOR)
!
        A14(IELEM) = - A24(IELEM) - A34(IELEM)
        A15(IELEM) = - A25(IELEM) - A35(IELEM)
        A16(IELEM) = - A26(IELEM) - A36(IELEM)
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
        U5  =  U(IKLE5(IELEM))
        U6  =  U(IKLE6(IELEM))
        V1  =  V(IKLE1(IELEM))
        V2  =  V(IKLE2(IELEM))
        V3  =  V(IKLE3(IELEM))
        V4  =  V(IKLE4(IELEM))
        V5  =  V(IKLE5(IELEM))
        V6  =  V(IKLE6(IELEM))
!
        UNSURF= 1.D0 / SURFAC(IELEM)
        AUX180= XSUR180 * UNSURF
        AUX720= XSUR720 * UNSURF
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)= (X2*F3-X3*F2) * ((4.D0*V6-6.D0*V2+V3+V1) * (X2-X3)
     &            + (U1+U3+4.D0*U6-6.D0*U2) * (Y3-Y2)) * AUX720
!
        A13(IELEM)= (X2*F3-X3*F2) * ((V2-6.D0*V3+4.D0*V4+V1) * (X2-X3)
     &            + (6.D0*U3-U1-U2-4.D0*U4) * (Y2-Y3)) * AUX720
!
        A21(IELEM)= (X3*F2-X2*F3) * ((6.D0*V1-V3-V2-4.D0*V5) * X3
     &            + (U3-6.D0*U1+U2+4.D0*U5) * Y3      ) * AUX720
!
        A23(IELEM)= (X2*F3-X3*F2) * ((V2-6.D0*V3+4.D0*V4+V1) * X3
     &            + (6.D0*U3-U1-U2-4.D0*U4) * Y3      ) * AUX720
!
        A24(IELEM)= (X2*F3-X3*F2) * ((V3-4.D0*V6-4.D0*V5-8.D0*V4) * X3
     &            + (4.D0*U5+8.D0*U4-U3+4.D0*U6) * Y3 ) * AUX180
!
        A25(IELEM)= (X2*F3-X3*F2) * ((V1-8.D0*V5-4.D0*V4-4.D0*V6) * X3
     &            + (4.D0*U4-U1+8.D0*U5+4.D0*U6) * Y3 ) * AUX180
!
        A26(IELEM)= (X2*F3-X3*F2) * ((V2-4.D0*V5-4.D0*V4-8.D0*V6) * X3
     &            + (4.D0*U5-U2+8.D0*U6+4.D0*U4) * Y3 ) * AUX180
!
        A31(IELEM)= (X2*F3-X3*F2) * ((6.D0*V1-V3-V2-4.D0*V5) * X2
     &            + (U3-6.D0*U1+U2+4.D0*U5) * Y2      ) * AUX720
!
        A32(IELEM)= (X3*F2-X2*F3) * ((4.D0*V6-6.D0*V2+V3+V1) * X2
     &            + (6.D0*U2-U1-U3-4.D0*U6) * Y2      ) * AUX720
!
        A34(IELEM)= (X3*F2-X2*F3) * ((V3-4.D0*V6-4.D0*V5-8.D0*V4) * X2
     &            + (4.D0*U5+8.D0*U4-U3+4.D0*U6) * Y2 ) * AUX180
!
        A35(IELEM)= (X3*F2-X2*F3) * ((V1-8.D0*V5-4.D0*V4-4.D0*V6) * X2
     &            + (4.D0*U4-U1+8.D0*U5+4.D0*U6) * Y2 ) * AUX180
!
        A36(IELEM)= (X3*F2-X2*F3) * ((V2-4.D0*V5-4.D0*V4-8.D0*V6) * X2
     &            + (4.D0*U5-U2+8.D0*U6+4.D0*U4) * Y2 ) * AUX180
!
!   THE SUM OF THE MATRIX ROWS IS 0 (VECTOR)
!
        A14(IELEM) = - A24(IELEM) - A34(IELEM)
        A15(IELEM) = - A25(IELEM) - A35(IELEM)
        A16(IELEM) = - A26(IELEM) - A36(IELEM)
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
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
!
        WRITE(LU,301) IELMU
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,101) IELMF
101     FORMAT(1X,'MT12AC (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
201   FORMAT(1X,'MT12AC (BIEF) : IMPOSSIBLE COMPONENT ',
     &          1I6,' CHECK ICOORD')
!
301   FORMAT(1X,'MT12AC (BIEF) :',/,
     &       1X,'DISCRETIZATION OF U : ',1I6,' NOT AVAILABLE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
