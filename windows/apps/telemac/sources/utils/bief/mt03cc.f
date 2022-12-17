!                   *****************
                    SUBROUTINE MT03CC
!                   *****************
!
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &  A21 , A22 , A23 , A24 , A25 , A26 ,
     &  A31 , A32 , A33 , A34 , A35 , A36 ,
     &  A41 , A42 , A43 , A44 , A45 , A46 ,
     &  A51 , A52 , A53 , A54 , A55 , A56 ,
     &  A61 , A62 , A63 , A64 , A65 , A66 ,
     &  XMUL,SF,SG,SU,SV,F,G,U,V,
     &  XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     &  NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                 /  -->   - -->          -->  --->
!+ A(I,J) = XMUL  /   KEL . GRAD(PSI1(I)) * U . GRAD(PSI2(J)) D(OMEGA)
!+               /OMEGA
!+         -->
!+         KEL CONSTANT VECTOR ON THE ELEMENT, WITH COMPONENTS F AND G
!+
!+         PSI1 OF P2 DISCRETISATION
!+         PSI2 OF P2 DISCRETISATION
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
!| A66            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| G              |-->| FUNCTION USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| FOURTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE5          |-->| FIFTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE6          |-->| SIXTH POINTS OF TRIANGLES (QUADRATIC)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SG             |-->| STRUCTURE OF FUNCTIONS G
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| V              |-->| FUNCTION V USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT03CC => MT03CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
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
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A51(*),A52(*),A53(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A54(*),A55(*),A56(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A61(*),A62(*),A63(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A64(*),A65(*),A66(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),U(*),V(*)
!
!     STRUCTURES OF      F, G, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,IELMF,IELMG,IELMU,IELMV
!
      DOUBLE PRECISION X2,X3,Y2,Y3,U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION KXEL,KYEL,ANS1,ANS2
!
!-----------------------------------------------------------------------
!
      IELMF = SF%ELM
      IELMG = SG%ELM
      IELMU = SU%ELM
      IELMV = SV%ELM
!
!-----------------------------------------------------------------------
! CASE WHERE U IS OF P1 DISCRETISATION
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.
     &   IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
!
      U1   =  U(IKLE1(IELEM))
      U2   =  U(IKLE2(IELEM))
      U3   =  U(IKLE3(IELEM))
      V1   =  V(IKLE1(IELEM))
      V2   =  V(IKLE2(IELEM))
      V3   =  V(IKLE3(IELEM))
!
      KXEL =  F(IELEM)
      KYEL =  G(IELEM)
!
!  COMPUTES 30 OF THE 36 TERMS (SELECTED AMONG THE LEAST COMPLEX)
!
      A11(IELEM)= ( KXEL * (Y3-Y2) - KYEL * (X3-X2) ) *
     &            ((X2-X3)*(V3+3.D0*V1+V2) + (Y3-Y2)*(U2+3.D0*U1+U3)) *
     &              XMUL/((X2*Y3-Y2*X3)*10.D0)
!
      A12(IELEM)= ( KXEL * (Y3-Y2) - KYEL * (X3-X2) ) *
     &            ( X3 * (V3+2.D0*(V1+V2)) -  Y3 * (U3+2.D0*(U1+U2))) *
     &             XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A13(IELEM)= ( KXEL * (Y3-Y2) - KYEL * (X3-X2) ) *
     &            ( X2 * (V2+2.D0*(V1+V3)) -  Y2 * (U2+2.D0*(U1+U3))) *
     &             XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A14(IELEM)= ( KXEL * (Y3-Y2) - KYEL * (X3-X2) ) *
     &            ( X2   *( -   V3+ 3.D0*V1- 2.D0*V2) +
     &              X3   *(4.D0*V3+11.D0*V1+ 5.D0*V2) +
     &              Y2   *(     U3+ 2.D0*U2- 3.D0*U1) -
     &              Y3   *(11.D0*U1+5.D0*U2+ 4.D0*U3))*
     &              XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A15(IELEM)= ( KXEL * (Y3-Y2) - KYEL * (X3-X2) ) *
     &            ( X3   *(     V2+2.D0*V3-  3.D0*V1) +
     &              X2   *(3.D0*V1-     V3-  2.D0*V2) +
     &              Y3   *(3.D0*U1-     U2-  2.D0*U3) +
     &              Y2   *(-3.D0*U1+2.D0*U2+      U3))*
     &              XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A21(IELEM)= ( KYEL * X3      - KXEL *    Y3  ) *
     &            ( (X2-X3) * ( V3 + 2.D0*(V1+V2) )  +
     &              (Y3-Y2) * ( U3 + 2.D0*(U1+U2) )) *
     &               XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A22(IELEM)= ( KYEL * X3      - KXEL *    Y3  ) *
     &            ( X3 * (V3+V1+3.D0*V2) - Y3*(U3+U1+3.D0*U2)) *
     &              XMUL/((X2*Y3-Y2*X3)*10.D0)
!
      A23(IELEM)= ( KYEL * X3      - KXEL *    Y3 ) *
     &            ( X2 * (V1+2.D0*(V2+V3)) - Y2 * (U1+2.D0*(U2+U3))) *
     &              XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A24(IELEM)= ( KYEL * X3      - KXEL *    Y3 ) *
     &            ( -X3*(4.D0*V3+ 5.D0*V1+11.D0*V2) +
     &               X2*(3.D0*V1+14.D0*V2+ 3.D0*V3) +
     &               Y3*(5.D0*U1+11.D0*U2+ 4.D0*U3) -
     &               Y2*(3.D0*U1+14.D0*U2+ 3.D0*U3))*
     &               XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A26(IELEM)= ( KXEL *    Y3  -  KYEL * X3    ) *
     &            (X3*(2.D0*V3 +      V1 - 3.D0*V2) +
     &             X2*(V1-V3)  + Y2*(U3-U1)         +
     &             Y3*(3.D0*U2 -      U1 - 2.D0*U3))*
     &             XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A31(IELEM)= ( KYEL * X2      - KXEL *    Y2 ) *
     &            ( (X2-X3) * (2.D0*(V1+V3) + V2  ) +
     &              (Y3-Y2) * (2.D0*(U1+U3) + U2 )) *
     &               XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A32(IELEM)= ( KYEL * X2      - KXEL *    Y2 ) *
     &            ( X3*(2.D0*(V3+V2)+V1)-Y3*(U1+2.D0*(U2+U3))) *
     &              XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A33(IELEM)= ( KYEL*X2        - KXEL *    Y2 ) *
     &            (X2*(V1+3.D0*V3+V2)-Y2*(U1+3.D0*U3+U2)) *
     &             XMUL/(10.D0*(X2*Y3-Y2*X3))
!
      A34(IELEM)= ( KYEL*X2        - KXEL *    Y2 ) *
     &            (X3*(V1-V2)+X2*(V1+2.D0*V2-3.D0*V3) +
     &             Y3*(U2-U1)+Y2*(3.D0*U3-U1-2.D0*U2))*
     &             XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A35(IELEM)=  ( KXEL*Y2       - KYEL *    X2 ) *
     &             (X3*(14.D0*V3+3.D0*(V2+V1)) +
     &              X2*(      V1-3.D0*V3+2.D0*V2) -
     &              Y3*(3.D0*(U1+U2)+14.D0*U3) +
     &              Y2*( 3.D0*U3-     U1- 2.D0*U2 ))*
     &              XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A36(IELEM)= ( KYEL*X2        - KXEL *    Y2 ) *
     &            ( X3*(-14.D0*V3-3.D0*(V1+V2)) +
     &              X2*(  5.D0*V1+4.D0*V2+11.D0*V3) +
     &              Y3*(3.D0*(U1+U2)+14.D0*U3) -
     &              Y2*(5.D0*U1+4.D0*U2+11.D0*U3)) *
     &              XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A42(IELEM)=(-5.D0*KXEL*Y3**2*U1+11.D0*KYEL*X3*Y3*U2+
     &            5.D0*KYEL*X3*Y3*U1+3.D0*KYEL*X3*X2*V1+
     &            5.D0*KXEL*Y3*X3*V1+3.D0*KXEL*Y3*Y2*U1-
     &            5.D0*KYEL*X3**2*V1-3.D0*KXEL*Y2*X3*V3-
     &            3.D0*KYEL*X2*Y3*U3+4.D0*KYEL*X3*Y3*U3+
     &            3.D0*KYEL*X3*X2*V3+3.D0*KXEL*Y3*Y2*U3+
     &            4.D0*KXEL*Y3*X3*V3-4.D0*KXEL*Y3**2*U3-
     &            4.D0*KYEL*X3**2*V3-14.D0*KXEL*Y2*X3*V2-
     &            14.D0*KYEL*X2*Y3*U2+14.D0*KXEL*Y3*Y2*U2+
     &            11.D0*KXEL*Y3*X3*V2+14.D0*KYEL*X3*X2*V2-
     &            11.D0*KYEL*X3**2*V2-11.D0*KXEL*Y3**2*U2-
     &            3.D0*KYEL*X2*Y3*U1-3.D0*KXEL*Y2*X3*V1)*
     &            XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A43(IELEM)=(KYEL*X3*X2*V1-KYEL*X3*Y2*U1-KXEL*Y3*X2*V1+
     &            KXEL*Y3*Y2*U1+KYEL*X2**2*V1+3.D0*KYEL*X2*Y2*U3+
     &            3.D0*KXEL*Y2*X2*V3-3.D0*KYEL*X2**2*V3-
     &            3.D0*KXEL*Y2**2*U3-2.D0*KXEL*Y2*X2*V2+
     &            2.D0*KYEL*X2**2*V2+2.D0*KXEL*Y2**2*U2-
     &            2.D0*KYEL*X2*Y2*U2-KXEL*Y3*Y2*U2+KXEL*Y3*X2*V2+
     &            KYEL*X3*Y2*U2-KYEL*X3*X2*V2+KXEL*Y2**2*U1-
     &            KYEL*X2*Y2*U1-KXEL*Y2*X2*V1)*
     &            XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A44(IELEM)=(-4.D0*KYEL*X3*Y3*U2-4.D0*KYEL*X3*Y3*U1-
     &            4.D0*KXEL*Y3*X3*V1-2.D0*KYEL*X2*Y2*U3+
     &            KXEL*Y2*X3*V3-2.D0*KXEL*Y2*X2*V3+KYEL*X2*Y3*U3+
     &            KYEL*X3*Y2*U3-2.D0*KYEL*X3*X2*V3-
     &            2.D0*KXEL*Y3*Y2*U3+KXEL*Y3*X2*V3+
     &            4.D0*KXEL*Y2*X3*V2-6.D0*KXEL*Y2*X2*V2+
     &            4.D0*KYEL*X2*Y3*U2+2.D0*KYEL*X2**2*V1+
     &            4.D0*KYEL*X3**2*V1+2.D0*KYEL*X2**2*V3+
     &            2.D0*KXEL*Y2**2*U3+6.D0*KYEL*X2**2*V2+
     &            6.D0*KXEL*Y2**2*U2+4.D0*KXEL*Y3**2*U1-
     &            6.D0*KYEL*X2*Y2*U2-8.D0*KXEL*Y3*Y2*U2-
     &            4.D0*KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V2+
     &            4.D0*KYEL*X3*Y2*U2-8.D0*KYEL*X3*X2*V2-
     &            2.D0*KYEL*X2*Y2*U1-2.D0*KXEL*Y2*X2*V1+
     &            4.D0*KYEL*X3**2*V2+2.D0*KXEL*Y2**2*U1+
     &            4.D0*KXEL*Y3**2*U2+2.D0*KXEL*Y3**2*U3+
     &            2.D0*KYEL*X3**2*V3-2.D0*KYEL*X3*Y3*U3-
     &            2.D0*KXEL*Y3*X3*V3)*
     &             XMUL/(15.D0*(X2*Y3-Y2*X3)/2.D0)
!
      A45(IELEM)=(-KYEL*X3*Y3*U2+KYEL*X3*Y3*U1-KYEL*X3*X2*V1+
     &            KXEL*Y3*X3*V1-KXEL*Y3*Y2*U1-2.D0*KYEL*X2*Y2*U3+
     &            2.D0*KXEL*Y2*X3*V3-2.D0*KXEL*Y2*X2*V3+
     &            2.D0*KYEL*X2*Y3*U3+KYEL*X3*Y2*U3-3.D0*KYEL*X3*X2*V3-
     &            3.D0*KXEL*Y3*Y2*U3+KXEL*Y3*X2*V3+2.D0*KXEL*Y2*X3*V2-
     &            6.D0*KXEL*Y2*X2*V2+2.D0*KYEL*X2*Y3*U2+
     &            2.D0*KYEL*X2**2*V1-KYEL*X3**2*V1+2.D0*KYEL*X2**2*V3+
     &            2.D0*KXEL*Y2**2*U3+6.D0*KYEL*X2**2*V2+
     &            6.D0*KXEL*Y2**2*U2-KXEL*Y3**2*U1-6.D0*KYEL*X2*Y2*U2-
     &            6.D0*KXEL*Y3*Y2*U2-KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V2+
     &            4.D0*KYEL*X3*Y2*U2-6.D0*KYEL*X3*X2*V2-
     &            2.D0*KYEL*X2*Y2*U1-2.D0*KXEL*Y2*X2*V1+KYEL*X3**2*V2+
     &            2.D0*KXEL*Y2**2*U1+KXEL*Y3**2*U2+KYEL*X2*Y3*U1+
     &            KXEL*Y2*X3*V1)*XMUL/(15.D0*(X3*Y2-Y3*X2)/2.D0)
!
      A46(IELEM)=(KYEL*X3*Y3*U2-KYEL*X3*Y3*U1+4.D0*KYEL*X3*X2*V1-
     &            3.D0*KYEL*X3*Y2*U1-3.D0*KXEL*Y3*X2*V1-KXEL*Y3*X3*V1+
     &            4.D0*KXEL*Y3*Y2*U1+KYEL*X2*Y2*U3-
     &            2.D0*KXEL*Y2*X3*V3+KXEL*Y2*X2*V3-
     &            2.D0*KYEL*X2*Y3*U3-KYEL*X3*Y2*U3+
     &            3.D0*KYEL*X3*X2*V3+3.D0*KXEL*Y3*Y2*U3-KXEL*Y3*X2*V3-
     &            2.D0*KXEL*Y2*X3*V2-2.D0*KYEL*X2*Y3*U2+KYEL*X2**2*V1+
     &            KYEL*X3**2*V1-KYEL*X2**2*V3-KXEL*Y2**2*U3+
     &            KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U2+KXEL*Y3*X3*V2-
     &            KXEL*Y3*X2*V2-KYEL*X3*Y2*U2+3.D0*KYEL*X3*X2*V2-
     &            KYEL*X2*Y2*U1-KXEL*Y2*X2*V1-KYEL*X3**2*V2+
     &            KXEL*Y2**2*U1-KXEL*Y3**2*U2-KYEL*X2*Y3*U1-
     &            KXEL*Y2*X3*V1)*XMUL/(15.D0*(X3*Y2-Y3*X2)/2.D0)
!
      A52(IELEM)=(KXEL*Y3**2*U1+3.D0*KYEL*X3*Y3*U2-KYEL*X3*Y3*U1+
     &            3.D0*KYEL*X3*X2*V1-KXEL*Y3*X3*V1+3.D0*KXEL*Y3*Y2*U1+
     &            KYEL*X3**2*V1-3.D0*KXEL*Y2*X3*V3-3.D0*KYEL*X2*Y3*U3-
     &            2.D0*KYEL*X3*Y3*U3+3.D0*KYEL*X3*X2*V3+
     &            3.D0*KXEL*Y3*Y2*U3-2.D0*KXEL*Y3*X3*V3+
     &            2.D0*KXEL*Y3**2*U3+2.D0*KYEL*X3**2*V3-
     &            14.D0*KXEL*Y2*X3*V2-14.D0*KYEL*X2*Y3*U2+
     &            14.D0*KXEL*Y3*Y2*U2+3.D0*KXEL*Y3*X3*V2+
     &            14.D0*KYEL*X3*X2*V2-3.D0*KYEL*X3**2*V2-
     &            3.D0*KXEL*Y3**2*U2-3.D0*KYEL*X2*Y3*U1-
     &            3.D0*KXEL*Y2*X3*V1)*XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A53(IELEM)=(3.D0*KYEL*X3*X2*V1-3.D0*KYEL*X3*Y2*U1-
     &            3.D0*KXEL*Y3*X2*V1+3.D0*KXEL*Y3*Y2*U1+
     &            KYEL*X2**2*V1+3.D0*KYEL*X2*Y2*U3+3.D0*KXEL*Y2*X2*V3-
     &            3.D0*KYEL*X2**2*V3-3.D0*KXEL*Y2**2*U3-
     &            14.D0*KYEL*X3*Y2*U3+14.D0*KYEL*X3*X2*V3+
     &            14.D0*KXEL*Y3*Y2*U3-14.D0*KXEL*Y3*X2*V3-
     &            2.D0*KXEL*Y2*X2*V2+2.D0*KYEL*X2**2*V2+
     &            2.D0*KXEL*Y2**2*U2-2.D0*KYEL*X2*Y2*U2+
     &            3.D0*KXEL*Y3*Y2*U2-3.D0*KXEL*Y3*X2*V2-
     &            3.D0*KYEL*X3*Y2*U2+3.D0*KYEL*X3*X2*V2+
     &            KXEL*Y2**2*U1-KYEL*X2*Y2*U1-KXEL*Y2*X2*V1)*
     &            XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A54(IELEM)=(-KYEL*X3*Y3*U2+KYEL*X3*Y3*U1-KYEL*X3*X2*V1+
     &            KYEL*X3*Y2*U1+KXEL*Y3*X2*V1+KXEL*Y3*X3*V1-
     &            KXEL*Y3*Y2*U1-2.D0*KYEL*X2*Y2*U3+KXEL*Y2*X3*V3-
     &            2.D0*KXEL*Y2*X2*V3+KYEL*X2*Y3*U3+2.D0*KYEL*X3*Y2*U3-
     &            3.D0*KYEL*X3*X2*V3-3.D0*KXEL*Y3*Y2*U3+
     &            2.D0*KXEL*Y3*X2*V3+4.D0*KXEL*Y2*X3*V2-
     &            6.D0*KXEL*Y2*X2*V2+4.D0*KYEL*X2*Y3*U2+
     &            2.D0*KYEL*X2**2*V1-KYEL*X3**2*V1+2.D0*KYEL*X2**2*V3+
     &            2.D0*KXEL*Y2**2*U3+6.D0*KYEL*X2**2*V2+
     &            6.D0*KXEL*Y2**2*U2-KXEL*Y3**2*U1-6.D0*KYEL*X2*Y2*U2-
     &            6.D0*KXEL*Y3*Y2*U2-KXEL*Y3*X3*V2+2.D0*KXEL*Y3*X2*V2+
     &            2.D0*KYEL*X3*Y2*U2-6.D0*KYEL*X3*X2*V2-
     &            2.D0*KYEL*X2*Y2*U1-2.D0*KXEL*Y2*X2*V1+
     &            KYEL*X3**2*V2+2.D0*KXEL*Y2**2*U1+KXEL*Y3**2*U2)
     &            *XMUL/(15.D0*(X3*Y2-Y3*X2)/2.D0)
!
      A56(IELEM)=(2.D0*KYEL*X3*Y3*U2+2.D0*KYEL*X3*Y3*U1+KYEL*X3*X2*V1+
     &            2.D0*KXEL*Y3*X3*V1+KXEL*Y3*Y2*U1+KYEL*X2*Y2*U3-
     &            2.D0*KXEL*Y2*X3*V3+KXEL*Y2*X2*V3-2.D0*KYEL*X2*Y3*U3-
     &            4.D0*KYEL*X3*Y2*U3+6.D0*KYEL*X3*X2*V3+
     &            6.D0*KXEL*Y3*Y2*U3-4.D0*KXEL*Y3*X2*V3-
     &            2.D0*KXEL*Y2*X3*V2-2.D0*KYEL*X2*Y3*U2+KYEL*X2**2*V1-
     &            2.D0*KYEL*X3**2*V1-KYEL*X2**2*V3-KXEL*Y2**2*U3-
     &            2.D0*KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U2+
     &            2.D0*KXEL*Y3*X3*V2-KXEL*Y3*X2*V2-KYEL*X3*Y2*U2+
     &            3.D0*KYEL*X3*X2*V2-KYEL*X2*Y2*U1-KXEL*Y2*X2*V1-
     &            2.D0*KYEL*X3**2*V2+KXEL*Y2**2*U1-2.D0*KXEL*Y3**2*U2-
     &            6.D0*KXEL*Y3**2*U3-6.D0*KYEL*X3**2*V3+
     &            6.D0*KYEL*X3*Y3*U3+6.D0*KXEL*Y3*X3*V3-
     &            KYEL*X2*Y3*U1-KXEL*Y2*X3*V1)*
     &            XMUL/(15.D0*(X2*Y3-Y2*X3)/2.D0)
!
      A62(IELEM)=(KXEL*Y3**2*U1+3.D0*KYEL*X3*Y3*U2-KYEL*X3*Y3*U1+
     &            KYEL*X3*X2*V1-KXEL*Y3*X3*V1+KXEL*Y3*Y2*U1+
     &            KYEL*X3**2*V1+KXEL*Y2*X3*V3+KYEL*X2*Y3*U3-
     &            2.D0*KYEL*X3*Y3*U3-KYEL*X3*X2*V3-KXEL*Y3*Y2*U3-
     &            2.D0*KXEL*Y3*X3*V3+2.D0*KXEL*Y3**2*U3+
     &            2.D0*KYEL*X3**2*V3+3.D0*KXEL*Y3*X3*V2-
     &            3.D0*KYEL*X3**2*V2-3.D0*KXEL*Y3**2*U2-
     &            KYEL*X2*Y3*U1-KXEL*Y2*X3*V1)
     &            *XMUL/((X2*Y3-Y2*X3)*30.D0)
!
      A63(IELEM)=(-3.D0*KYEL*X3*X2*V1+3.D0*KYEL*X3*Y2*U1+
     &            3.D0*KXEL*Y3*X2*V1-3.D0*KXEL*Y3*Y2*U1+
     &            5.D0*KYEL*X2**2*V1-11.D0*KYEL*X2*Y2*U3-
     &            11.D0*KXEL*Y2*X2*V3+11.D0*KYEL*X2**2*V3+
     &            11.D0*KXEL*Y2**2*U3+14.D0*KYEL*X3*Y2*U3-
     &            14.D0*KYEL*X3*X2*V3-14.D0*KXEL*Y3*Y2*U3+
     &            14.D0*KXEL*Y3*X2*V3-4.D0*KXEL*Y2*X2*V2+
     &            4.D0*KYEL*X2**2*V2+4.D0*KXEL*Y2**2*U2-
     &            4.D0*KYEL*X2*Y2*U2-3.D0*KXEL*Y3*Y2*U2+
     &            3.D0*KXEL*Y3*X2*V2+3.D0*KYEL*X3*Y2*U2-
     &            3.D0*KYEL*X3*X2*V2+5.D0*KXEL*Y2**2*U1-
     &            5.D0*KYEL*X2*Y2*U1-5.D0*KXEL*Y2*X2*V1)*
     &            XMUL/((X3*Y2-Y3*X2)*30.D0)
!
      A64(IELEM)=(KYEL*X3*Y3*U2-KYEL*X3*Y3*U1+4.D0*KYEL*X3*X2*V1-
     &            KYEL*X3*Y2*U1-KXEL*Y3*X2*V1-KXEL*Y3*X3*V1+
     &            4.D0*KXEL*Y3*Y2*U1+KYEL*X2*Y2*U3-KXEL*Y2*X3*V3+
     &            KXEL*Y2*X2*V3-KYEL*X2*Y3*U3-2.D0*KYEL*X3*Y2*U3+
     &            3.D0*KYEL*X3*X2*V3+3.D0*KXEL*Y3*Y2*U3-
     &            2.D0*KXEL*Y3*X2*V3-KXEL*Y2*X3*V2-KYEL*X2*Y3*U2+
     &            KYEL*X2**2*V1+KYEL*X3**2*V1-KYEL*X2**2*V3-
     &            KXEL*Y2**2*U3+KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U2+
     &            KXEL*Y3*X3*V2-2.D0*KXEL*Y3*X2*V2-
     &            2.D0*KYEL*X3*Y2*U2+3.D0*KYEL*X3*X2*V2-
     &            KYEL*X2*Y2*U1-KXEL*Y2*X2*V1-KYEL*X3**2*V2+
     &            KXEL*Y2**2*U1-KXEL*Y3**2*U2-3.D0*KYEL*X2*Y3*U1-
     &            3.D0*KXEL*Y2*X3*V1)*
     &            XMUL/(15.D0*(X3*Y2-Y3*X2)/2.D0)
!
      A65(IELEM)=(2.D0*KYEL*X3*Y3*U2+2.D0*KYEL*X3*Y3*U1+KYEL*X3*X2*V1-
     &            KYEL*X3*Y2*U1-KXEL*Y3*X2*V1+2.D0*KXEL*Y3*X3*V1+
     &            KXEL*Y3*Y2*U1+KYEL*X2*Y2*U3-4.D0*KXEL*Y2*X3*V3+
     &            KXEL*Y2*X2*V3-4.D0*KYEL*X2*Y3*U3-2.D0*KYEL*X3*Y2*U3+
     &            6.D0*KYEL*X3*X2*V3+6.D0*KXEL*Y3*Y2*U3-
     &            2.D0*KXEL*Y3*X2*V3-KXEL*Y2*X3*V2-KYEL*X2*Y3*U2+
     &            KYEL*X2**2*V1-2.D0*KYEL*X3**2*V1-KYEL*X2**2*V3-
     &            KXEL*Y2**2*U3-2.D0*KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U2+
     &            2.D0*KXEL*Y3*X3*V2-2.D0*KXEL*Y3*X2*V2-
     &            2.D0*KYEL*X3*Y2*U2+3.D0*KYEL*X3*X2*V2-KYEL*X2*Y2*U1-
     &            KXEL*Y2*X2*V1-2.D0*KYEL*X3**2*V2+KXEL*Y2**2*U1-
     &            2.D0*KXEL*Y3**2*U2-6.D0*KXEL*Y3**2*U3-
     &            6.D0*KYEL*X3**2*V3+6.D0*KYEL*X3*Y3*U3+
     &            6.D0*KXEL*Y3*X3*V3)*XMUL/(15.D0*(X2*Y3-Y2*X3)/2.D0)
!
      A66(IELEM)=(-2.D0*KYEL*X3*Y3*U2-2.D0*KYEL*X3*Y3*U1-
     &            2.D0*KXEL*Y3*X3*V1-4.D0*KYEL*X2*Y2*U3+
     &            4.D0*KXEL*Y2*X3*V3-4.D0*KXEL*Y2*X2*V3+
     &            4.D0*KYEL*X2*Y3*U3+4.D0*KYEL*X3*Y2*U3-
     &            8.D0*KYEL*X3*X2*V3-8.D0*KXEL*Y3*Y2*U3+
     &            4.D0*KXEL*Y3*X2*V3+KXEL*Y2*X3*V2-
     &            2.D0*KXEL*Y2*X2*V2+KYEL*X2*Y3*U2+
     &            4.D0*KYEL*X2**2*V1+2.D0*KYEL*X3**2*V1+
     &            4.D0*KYEL*X2**2*V3+4.D0*KXEL*Y2**2*U3+
     &            2.D0*KYEL*X2**2*V2+2.D0*KXEL*Y2**2*U2+
     &            2.D0*KXEL*Y3**2*U1-2.D0*KYEL*X2*Y2*U2-
     &            2.D0*KXEL*Y3*Y2*U2-2.D0*KXEL*Y3*X3*V2+
     &            KXEL*Y3*X2*V2+KYEL*X3*Y2*U2-2.D0*KYEL*X3*X2*V2-
     &            4.D0*KYEL*X2*Y2*U1-4.D0*KXEL*Y2*X2*V1+
     &            2.D0*KYEL*X3**2*V2+4.D0*KXEL*Y2**2*U1+
     &            2.D0*KXEL*Y3**2*U2+6.D0*KXEL*Y3**2*U3+
     &            6.D0*KYEL*X3**2*V3-6.D0*KYEL*X3*Y3*U3-
     &            6.D0*KXEL*Y3*X3*V3)*XMUL/(15.D0*(X2*Y3-Y2*X3)/2.D0)
!
! USES HERE THE 'MAGIC SQUARE' PROPERTIES
! (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
!
      A16(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
     &             - A14(IELEM) - A15(IELEM)
      A25(IELEM) = - A21(IELEM) - A22(IELEM) - A23(IELEM)
     &             - A24(IELEM) - A26(IELEM)
      A41(IELEM) = - A42(IELEM) - A43(IELEM) - A44(IELEM)
     &             - A45(IELEM) - A46(IELEM)
      A61(IELEM) = - A62(IELEM) - A63(IELEM) - A64(IELEM)
     &             - A65(IELEM) - A66(IELEM)
      A51(IELEM) = - A11(IELEM) - A21(IELEM) - A31(IELEM)
     &             - A41(IELEM) - A61(IELEM)
      A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &             - A54(IELEM) - A56(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
! CASE WHERE U IS OF P2 DISCRETISATION
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.
     &       IELMU.EQ.13.AND.IELMV.EQ.13) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
!
      U1   =  U(IKLE1(IELEM))
      U2   =  U(IKLE2(IELEM))
      U3   =  U(IKLE3(IELEM))
      U4   =  U(IKLE4(IELEM))
      U5   =  U(IKLE5(IELEM))
      U6   =  U(IKLE6(IELEM))
      V1   =  V(IKLE1(IELEM))
      V2   =  V(IKLE2(IELEM))
      V3   =  V(IKLE3(IELEM))
      V4   =  V(IKLE4(IELEM))
      V5   =  V(IKLE5(IELEM))
      V6   =  V(IKLE6(IELEM))
!
      KXEL =  F(IELEM)
      KYEL =  G(IELEM)
!
!  COMPUTES 30 OF THE 36 TERMS (SELECTED AMONG THE LEAST COMPLEX)
!
      A11(IELEM)=(KXEL*(Y2-Y3)+KYEL*(X3-X2))*(12.D0*X3*V1+
     &            12.D0*Y2*U1-12.D0*Y3*U1-12.D0*X2*V1+2.D0*Y3*U3+
     &            2.D0*X2*V3+15.D0*X3*V6-2.D0*Y2*U3+15.D0*Y2*U6-
     &            15.D0*X2*V6-15.D0*Y3*U6-2.D0*X3*V3+15.D0*Y2*U4-
     &            7.D0*Y3*U5+15.D0*X3*V4-15.D0*X2*V4-15.D0*Y3*U4+
     &            7.D0*Y2*U5+7.D0*X3*V5-7.D0*X2*V5-2.D0*Y2*U2+
     &            2.D0*X2*V2-2.D0*X3*V2+2.D0*Y3*U2)*
     &            XMUL/(X2*Y3-Y2*X3)/90.D0
!
      A12(IELEM)=(KXEL*(Y3-Y2)+KYEL*(X2-X3))*(3.D0*X3*V1-
     &            3.D0*Y3*U1+2.D0*Y3*U3+5.D0*X3*V6-5.D0*Y3*U6-
     &            2.D0*X3*V3-5.D0*Y3*U5+X3*V4-Y3*U4+5.D0*X3*V5+
     &            3.D0*X3*V2-3.D0*Y3*U2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A13(IELEM)=(KXEL*(Y3-Y2)+KYEL*(X2-X3))*(3.D0*Y2*U1-
     &            3.D0*X2*V1-3.D0*X2*V3+3.D0*Y2*U3+Y2*U6-X2*V6+
     &            5.D0*Y2*U4-5.D0*X2*V4+5.D0*Y2*U5-5.D0*X2*V5-
     &            2.D0*Y2*U2+2.D0*X2*V2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A14(IELEM)=(KXEL*(Y2-Y3)+KYEL*(X3-X2))*(15.D0*X3*V1-
     &            3.D0*Y2*U1-15.D0*Y3*U1+3.D0*X2*V1+4.D0*Y3*U3-
     &            X2*V3+20.D0*X3*V6+Y2*U3-4.D0*Y2*U6+4.D0*X2*V6-
     &            20.D0*Y3*U6-4.D0*X3*V3-8.D0*Y2*U4-12.D0*Y3*U5+
     &            16.D0*X3*V4+8.D0*X2*V4-16.D0*Y3*U4+8.D0*Y2*U5+
     &            12.D0*X3*V5-8.D0*X2*V5+6.D0*Y2*U2-6.D0*X2*V2+
     &            X3*V2-Y3*U2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A15(IELEM)=(KXEL*(Y2-Y3)+KYEL*(X3-X2))*(3.D0*X3*V1+
     &            3.D0*Y2*U1-3.D0*Y3*U1-3.D0*X2*V1+6.D0*Y3*U3+
     &            X2*V3+8.D0*X3*V6-Y2*U3+4.D0*Y2*U6-4.D0*X2*V6-
     &            8.D0*Y3*U6-6.D0*X3*V3+8.D0*Y2*U4+8.D0*Y3*U5+
     &            4.D0*X3*V4-8.D0*X2*V4-4.D0*Y3*U4-8.D0*Y2*U5-
     &            8.D0*X3*V5+8.D0*X2*V5-6.D0*Y2*U2+6.D0*X2*V2-
     &            X3*V2+Y3*U2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A21(IELEM)=(-KXEL*Y3+KYEL*X3)*(3.D0*X3*V1+3.D0*Y2*U1-
     &            3.D0*Y3*U1-3.D0*X2*V1+2.D0*Y3*U3+2.D0*X2*V3+
     &            5.D0*X3*V6-2.D0*Y2*U3+5.D0*Y2*U6-5.D0*X2*V6-
     &            5.D0*Y3*U6-2.D0*X3*V3+Y2*U4-5.D0*Y3*U5+
     &            X3*V4-X2*V4-Y3*U4+5.D0*Y2*U5+5.D0*X3*V5-
     &            5.D0*X2*V5+3.D0*Y2*U2-3.D0*X2*V2+3.D0*X3*V2-
     &            3.D0*Y3*U2)*XMUL/(X2*Y3-Y2*X3)/90.D0
!
      A22(IELEM)=(-KXEL*Y3+KYEL*X3)*(-2.D0*X3*V1+2.D0*Y3*U1+
     &            2.D0*Y3*U3+7.D0*X3*V6-7.D0*Y3*U6-2.D0*X3*V3-
     &            15.D0*Y3*U5+15.D0*X3*V4-15.D0*Y3*U4+
     &            15.D0*X3*V5+12.D0*X3*V2-12.D0*Y3*U2)
     &            *XMUL/(X2*Y3-Y2*X3)/90.D0
!
      A23(IELEM)=(-KXEL*Y3+KYEL*X3)*(2.D0*Y2*U1-2.D0*X2*V1+
     &            3.D0*X2*V3-3.D0*Y2*U3-5.D0*Y2*U6+5.D0*X2*V6-
     &            5.D0*Y2*U4+5.D0*X2*V4-Y2*U5+X2*V5-3.D0*Y2*U2+
     &            3.D0*X2*V2)*XMUL/(X2*Y3-Y2*X3)/90.D0
!
      A24(IELEM)=(-KXEL*Y3+KYEL*X3)*(X3*V1-5.D0*Y2*U1-Y3*U1+
     &            5.D0*X2*V1+4.D0*Y3*U3+5.D0*X2*V3+12.D0*X3*V6-
     &            5.D0*Y2*U3+4.D0*Y2*U6-4.D0*X2*V6-12.D0*Y3*U6-
     &            4.D0*X3*V3+24.D0*Y2*U4-20.D0*Y3*U5+16.D0*X3*V4-
     &            24.D0*X2*V4-16.D0*Y3*U4+24.D0*Y2*U5+20.D0*X3*V5-
     &            24.D0*X2*V5+18.D0*Y2*U2-18.D0*X2*V2+15.D0*X3*V2-
     &            15.D0*Y3*U2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A26(IELEM)=(-KXEL*Y3+KYEL*X3)*(-X3*V1+5.D0*Y2*U1+Y3*U1-
     &            5.D0*X2*V1+6.D0*Y3*U3+5.D0*X2*V3-8.D0*X3*V6-
     &            5.D0*Y2*U3+8.D0*Y3*U6-6.D0*X3*V3-4.D0*Y2*U4-
     &            8.D0*Y3*U5+4.D0*X3*V4+4.D0*X2*V4-4.D0*Y3*U4+
     &            4.D0*Y2*U5+8.D0*X3*V5-4.D0*X2*V5+3.D0*X3*V2-
     &            3.D0*Y3*U2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A31(IELEM)=(KXEL*Y2-KYEL*X2)*(3.D0*X3*V1+3.D0*Y2*U1-
     &            3.D0*Y3*U1-3.D0*X2*V1-3.D0*Y3*U3-3.D0*X2*V3+
     &            X3*V6+3.D0*Y2*U3+Y2*U6-X2*V6-Y3*U6+3.D0*X3*V3+
     &            5.D0*Y2*U4-5.D0*Y3*U5+5.D0*X3*V4-5.D0*X2*V4-
     &            5.D0*Y3*U4+5.D0*Y2*U5+5.D0*X3*V5-5.D0*X2*V5-
     &            2.D0*Y2*U2+2.D0*X2*V2-2.D0*X3*V2+2.D0*Y3*U2)
     &            *XMUL/(X2*Y3-Y2*X3)/90.D0
!
      A32(IELEM)=(KXEL*Y2-KYEL*X2)*(-2.D0*X3*V1+2.D0*Y3*U1-
     &            3.D0*Y3*U3+5.D0*X3*V6-5.D0*Y3*U6+3.D0*X3*V3-
     &            Y3*U5+5.D0*X3*V4-5.D0*Y3*U4+X3*V5+3.D0*X3*V2-
     &            3.D0*Y3*U2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A33(IELEM)=(KXEL*Y2-KYEL*X2)*(2.D0*Y2*U1-2.D0*X2*V1+
     &            12.D0*X2*V3-12.D0*Y2*U3-15.D0*Y2*U6+15.D0*X2*V6-
     &            7.D0*Y2*U4+7.D0*X2*V4-15.D0*Y2*U5+15.D0*X2*V5+
     &            2.D0*Y2*U2-2.D0*X2*V2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A34(IELEM)=(KXEL*Y2-KYEL*X2)*(5.D0*X3*V1-Y2*U1-5.D0*Y3*U1+
     &            X2*V1-3.D0*X2*V3-4.D0*X3*V6+3.D0*Y2*U3+
     &            4.D0*Y2*U6-4.D0*X2*V6+4.D0*Y3*U6-8.D0*Y2*U4-
     &            4.D0*Y3*U5+8.D0*X2*V4+8.D0*Y2*U5+4.D0*X3*V5-
     &            8.D0*X2*V5-6.D0*Y2*U2+6.D0*X2*V2-5.D0*X3*V2+
     &            5.D0*Y3*U2)*XMUL/(-X2*Y3+Y2*X3)/90.D0
!
      A35(IELEM)=(KXEL*Y2-KYEL*X2)*(-5.D0*X3*V1-Y2*U1+5.D0*Y3*U1+
     &            X2*V1-18.D0*Y3*U3-3.D0*X2*V3+24.D0*X3*V6+
     &            3.D0*Y2*U3+4.D0*Y2*U6-4.D0*X2*V6-24.D0*Y3*U6+
     &            18.D0*X3*V3-8.D0*Y2*U4-24.D0*Y3*U5+4.D0*X3*V4+
     &            8.D0*X2*V4-4.D0*Y3*U4+8.D0*Y2*U5+24.D0*X3*V5-
     &            8.D0*X2*V5-6.D0*Y2*U2+6.D0*X2*V2-5.D0*X3*V2+
     &            5.D0*Y3*U2)*XMUL/(X2*Y3-Y2*X3)/90.D0
!
      ANS1 = KYEL*X3*Y2*U2-6.D0*KYEL*X2*Y2*U2-6.D0*KXEL*Y2*X2*V2-
     &       4.D0*KYEL*X2**2*V6+15.D0*KXEL*Y3**2*U1-
     &       12.D0*KXEL*Y3*Y2*U1+15.D0*KXEL*Y3*X2*V1+
     &       6.D0*KYEL*X2**2*V2-KXEL*Y3*X3*V2+16.D0*KXEL*Y3*X2*V4+
     &       6.D0*KXEL*Y2**2*U2+16.D0*KXEL*Y3**2*U4-
     &       8.D0*KXEL*Y3*Y2*U4-4.D0*KXEL*Y2**2*U6+
     &       15.D0*KYEL*X3*Y2*U1-15.D0*KYEL*X3*Y3*U1-
     &       12.D0*KYEL*X3*X2*V1-16.D0*KXEL*Y3*X3*V4-
     &       7.D0*KXEL*Y3*Y2*U2+20.D0*KXEL*Y3**2*U6+
     &       12.D0*KYEL*X3**2*V5+15.D0*KYEL*X3**2*V1-
     &       15.D0*KXEL*Y3*X3*V1+8.D0*KYEL*X2*Y3*U5-
     &       8.D0*KYEL*X2*Y2*U5+8.D0*KYEL*X2*Y2*U4+
     &       KXEL*Y3**2*U2+KYEL*X3**2*V2+20.D0*KYEL*X3**2*V6-
     &       4.D0*KYEL*X3**2*V3-4.D0*KXEL*Y3**2*U3-
     &       8.D0*KXEL*Y2**2*U4+KYEL*X2**2*V3+8.D0*KXEL*Y2**2*U5+
     &       8.D0*KYEL*X2**2*V5+3.D0*KXEL*Y2*X2*V1-
     &       3.D0*KXEL*Y2*X3*V1-KXEL*Y2*X2*V3-3.D0*KXEL*Y2**2*U1+
     &       KXEL*Y2**2*U3-3.D0*KYEL*X2**2*V1-8.D0*KXEL*Y2*X2*V5+
     &       8.D0*KXEL*Y2*X3*V5+8.D0*KXEL*Y2*X2*V4-KYEL*X3*Y3*U2
      ANS2 = 4.D0*KYEL*X3*Y3*U3+3.D0*KYEL*X3*X2*V3+
     &       20.D0*KYEL*X3*Y2*U6-16.D0*KYEL*X3*X2*V6-
     &       20.D0*KYEL*X3*Y3*U6+3.D0*KXEL*Y3*Y2*U3-
     &       20.D0*KXEL*Y3*X3*V6+4.D0*KXEL*Y3*X3*V3-
     &       16.D0*KXEL*Y3*Y2*U6+12.D0*KXEL*Y3**2*U5+
     &       16.D0*KYEL*X3**2*V4-4.D0*KXEL*Y2*X3*V6+
     &       3.D0*KYEL*X2*Y2*U1-3.D0*KYEL*X2*Y3*U1-KYEL*X2*Y2*U3+
     &       KYEL*X2*Y3*U3-4.D0*KYEL*X2*Y3*U6+KXEL*Y2*X3*V3
     &       -8.D0*KYEL*X2**2*V4-7.D0*KYEL*X3*X2*V2-
     &       16.D0*KYEL*X3*Y3*U4+16.D0*KYEL*X3*Y2*U4+
     &       12.D0*KXEL*Y3*X2*V5-20.D0*KXEL*Y3*Y2*U5-
     &       12.D0*KXEL*Y3*X3*V5-8.D0*KYEL*X3*X2*V4+
     &       20.D0*KXEL*Y3*X2*V6-20.D0*KYEL*X3*X2*V5-
     &       12.D0*KYEL*X3*Y3*U5+12.D0*KYEL*X3*Y2*U5+
     &       6.D0*KYEL*X2*Y3*U2+6.D0*KXEL*Y2*X3*V2+KXEL*Y3*X2*V2-
     &       8.D0*KYEL*X2*Y3*U4-8.D0*KXEL*Y2*X3*V4+
     &       4.D0*KXEL*Y2*X2*V6-4.D0*KYEL*X3*Y2*U3+
     &       4.D0*KYEL*X2*Y2*U6-4.D0*KXEL*Y3*X2*V3
      A41(IELEM)= (ANS1+ANS2)*XMUL/(90.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = KXEL*Y3**2*U1+5.D0*KXEL*Y3*Y2*U1-15.D0*KXEL*Y3*X3*V2+
     &       16.D0*KXEL*Y3**2*U4-24.D0*KXEL*Y3*Y2*U4-KYEL*X3*Y3*U1+
     &       5.D0*KYEL*X3*X2*V1-16.D0*KXEL*Y3*X3*V4-
     &       18.D0*KXEL*Y3*Y2*U2+12.D0*KXEL*Y3**2*U6+
     &       20.D0*KYEL*X3**2*V5+KYEL*X3**2*V1-KXEL*Y3*X3*V1+
     &       24.D0*KYEL*X2*Y3*U5+15.D0*KXEL*Y3**2*U2+
     &       15.D0*KYEL*X3**2*V2+12.D0*KYEL*X3**2*V6-
     &       4.D0*KYEL*X3**2*V3-4.D0*KXEL*Y3**2*U3-
     &       5.D0*KXEL*Y2*X3*V1+24.D0*KXEL*Y2*X3*V5-
     &       15.D0*KYEL*X3*Y3*U2+4.D0*KYEL*X3*Y3*U3
      ANS2 = 5.D0*KYEL*X3*X2*V3-4.D0*KYEL*X3*X2*V6-
     &       12.D0*KYEL*X3*Y3*U6+5.D0*KXEL*Y3*Y2*U3-
     &       12.D0*KXEL*Y3*X3*V6+4.D0*KXEL*Y3*X3*V3-
     &       4.D0*KXEL*Y3*Y2*U6+20.D0*KXEL*Y3**2*U5+
     &       16.D0*KYEL*X3**2*V4+4.D0*KXEL*Y2*X3*V6-
     &       5.D0*KYEL*X2*Y3*U1-5.D0*KYEL*X2*Y3*U3+
     &       4.D0*KYEL*X2*Y3*U6-5.D0*KXEL*Y2*X3*V3-
     &       18.D0*KYEL*X3*X2*V2-16.D0*KYEL*X3*Y3*U4-
     &       24.D0*KXEL*Y3*Y2*U5-20.D0*KXEL*Y3*X3*V5-
     &       24.D0*KYEL*X3*X2*V4-24.D0*KYEL*X3*X2*V5-
     &       20.D0*KYEL*X3*Y3*U5+18.D0*KYEL*X2*Y3*U2+
     &       18.D0*KXEL*Y2*X3*V2+24.D0*KYEL*X2*Y3*U4+
     &       24.D0*KXEL*Y2*X3*V4
      A42(IELEM)= (ANS1+ANS2)*XMUL/(90.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = 6.D0*KYEL*X3*Y2*U2-6.D0*KYEL*X2*Y2*U2-
     &       6.D0*KXEL*Y2*X2*V2+4.D0*KYEL*X2**2*V6+
     &       4.D0*KXEL*Y3**2.D0*U1+4.D0*KXEL*Y3*Y2*U1-
     &       2.D0*KXEL*Y3*X2*V1+6.D0*KYEL*X2**2*V2-
     &       4.D0*KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V4+
     &       6.D0*KXEL*Y2**2*U2+8.D0*KXEL*Y3**2.D0*U4-
     &       8.D0*KXEL*Y3*Y2*U4+4.D0*KXEL*Y2**2*U6-
     &       2.D0*KYEL*X3*Y2*U1-4.D0*KYEL*X3*Y3*U1+
     &       4.D0*KYEL*X3*X2*V1-8.D0*KXEL*Y3*X3*V4-
     &       12.D0*KXEL*Y3*Y2*U2+8.D0*KXEL*Y3**2*U6+
     &       8.D0*KYEL*X3**2*V5+4.D0*KYEL*X3**2*V1-
     &       4.D0*KXEL*Y3*X3*V1+8.D0*KYEL*X2*Y3*U5-
     &       12.D0*KYEL*X2*Y2*U5-12.D0*KYEL*X2*Y2*U4+
     &       4.D0*KXEL*Y3**2*U2+4.D0*KYEL*X3**2*V2+
     &       8.D0*KYEL*X3**2*V6-2.D0*KYEL*X3**2*V3-
     &       2.D0*KXEL*Y3**2*U3+12.D0*KXEL*Y2**2*U4-
     &       2.D0*KYEL*X2**2*V3+12.D0*KXEL*Y2**2*U5+
     &       12.D0*KYEL*X2**2*V5+2.D0*KXEL*Y2*X2*V1-
     &       2.D0*KXEL*Y2*X3*V1+2.D0*KXEL*Y2*X2*V3-
     &       2.D0*KXEL*Y2**2*U1
      ANS2 = -2.D0*KXEL*Y2**2*U3-2.D0*KYEL*X2**2*V1-
     &       12.D0*KXEL*Y2*X2*V5+8.D0*KXEL*Y2*X3*V5-
     &       12.D0*KXEL*Y2*X2*V4-4.D0*KYEL*X3*Y3*U2+
     &       2.D0*KYEL*X3*Y3*U3+2.D0*KYEL*X3*X2*V3-
     &       8.D0*KYEL*X3*Y3*U6+2.D0*KXEL*Y3*Y2*U3-
     &       8.D0*KXEL*Y3*X3*V6+2.D0*KXEL*Y3*X3*V3+
     &       8.D0*KXEL*Y3**2*U5+8.D0*KYEL*X3**2*V4+
     &       2.D0*KYEL*X2*Y2*U1-2.D0*KYEL*X2*Y3*U1+
     &       2.D0*KYEL*X2*Y2*U3-KYEL*X2*Y3*U3-KXEL*Y2*X3*V3+
     &       12.D0*KYEL*X2**2*V4-12.D0*KYEL*X3*X2*V2-
     &       8.D0*KYEL*X3*Y3*U4+4.D0*KYEL*X3*Y2*U4+
     &       8.D0*KXEL*Y3*X2*V5-16.D0*KXEL*Y3*Y2*U5-
     &       8.D0*KXEL*Y3*X3*V5-8.D0*KYEL*X3*X2*V4-
     &       16.D0*KYEL*X3*X2*V5-8.D0*KYEL*X3*Y3*U5+
     &       8.D0*KYEL*X3*Y2*U5+6.D0*KYEL*X2*Y3*U2+
     &       6.D0*KXEL*Y2*X3*V2+6.D0*KXEL*Y3*X2*V2+
     &       4.D0*KYEL*X2*Y3*U4+4.D0*KXEL*Y2*X3*V4-
     &       4.D0*KXEL*Y2*X2*V6-KYEL*X3*Y2*U3-
     &       4.D0*KYEL*X2*Y2*U6-KXEL*Y3*X2*V3
      A44(IELEM)= (ANS1+ANS2)*2.D0*XMUL/(45.D0*(X2*Y3-Y2*X3))
!
      ANS1 = 6.D0*KYEL*X3*Y2*U2-6.D0*KYEL*X2*Y2*U2-
     &       6.D0*KXEL*Y2*X2*V2+4.D0*KYEL*X2**2*V6-
     &       KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U1-2.D0*KXEL*Y3*X2*V1+
     &       6.D0*KYEL*X2**2*V2-KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V4+
     &       6.D0*KXEL*Y2**2*U2-8.D0*KXEL*Y3*Y2*U4+
     &       4.D0*KXEL*Y2**2*U6-2.D0*KYEL*X3*Y2*U1+KYEL*X3*Y3*U1+
     &       3.D0*KYEL*X3*X2*V1-6.D0*KXEL*Y3*Y2*U2-
     &       4.D0*KXEL*Y3**2*U6+4.D0*KYEL*X3**2*V5-
     &       KYEL*X3**2*V1+KXEL*Y3*X3*V1+8.D0*KYEL*X2*Y3*U5-
     &       12.D0*KYEL*X2*Y2*U5-12.D0*KYEL*X2*Y2*U4+
     &       KXEL*Y3**2*U2+KYEL*X3**2*V2-4.D0*KYEL*X3**2*V6+
     &       12.D0*KXEL*Y2**2*U4-2.D0*KYEL*X2**2*V3+
     &       12.D0*KXEL*Y2**2*U5+12.D0*KYEL*X2**2*V5+
     &       2.D0*KXEL*Y2*X2*V1-KXEL*Y2*X3*V1+2.D0*KXEL*Y2*X2*V3-
     &       2.D0*KXEL*Y2**2*U1
      ANS2 = -2.D0*KXEL*Y2**2*U3-2.D0*KYEL*X2**2*V1-
     &       12.D0*KXEL*Y2*X2*V5+8.D0*KXEL*Y2*X3*V5-
     &       12.D0*KXEL*Y2*X2*V4-KYEL*X3*Y3*U2+
     &       KYEL*X3*X2*V3-4.D0*KYEL*X3*X2*V6+4.D0*KYEL*X3*Y3*U6+
     &       KXEL*Y3*Y2*U3+4.D0*KXEL*Y3*X3*V6-4.D0*KXEL*Y3*Y2*U6+
     &       4.D0*KXEL*Y3**2*U5+4.D0*KXEL*Y2*X3*V6+
     &       2.D0*KYEL*X2*Y2*U1-KYEL*X2*Y3*U1+2.D0*KYEL*X2*Y2*U3+
     &       4.D0*KYEL*X2*Y3*U6+12.D0*KYEL*X2**2*V4-
     &       6.D0*KYEL*X3*X2*V2+4.D0*KYEL*X3*Y2*U4+
     &       8.D0*KXEL*Y3*X2*V5-16.D0*KXEL*Y3*Y2*U5-
     &       4.D0*KXEL*Y3*X3*V5-8.D0*KYEL*X3*X2*V4-
     &       16.D0*KYEL*X3*X2*V5-4.D0*KYEL*X3*Y3*U5+
     &       8.D0*KYEL*X3*Y2*U5+6.D0*KXEL*Y3*X2*V2+
     &       4.D0*KYEL*X2*Y3*U4+4.D0*KXEL*Y2*X3*V4-
     &       4.D0*KXEL*Y2*X2*V6-KYEL*X3*Y2*U3-
     &       4.D0*KYEL*X2*Y2*U6-KXEL*Y3*X2*V3
      A45(IELEM)=  (ANS1+ANS2)*2.D0*XMUL/(45.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = -KYEL*X3*Y2*U2-KXEL*Y3**2*U1-4.D0*KXEL*Y3*Y2*U1+
     &       5.D0*KXEL*Y3*X2*V1-KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V4-
     &       8.D0*KXEL*Y3*Y2*U4+5.D0*KYEL*X3*Y2*U1+KYEL*X3*Y3*U1-
     &       4.D0*KYEL*X3*X2*V1+KXEL*Y3*Y2*U2-4.D0*KXEL*Y3**2*U6+
     &       4.D0*KYEL*X3**2*V5-KYEL*X3**2*V1+KXEL*Y3*X3*V1+
     &       8.D0*KYEL*X2*Y3*U5-4.D0*KYEL*X2*Y2*U5+
     &       4.D0*KYEL*X2*Y2*U4+KXEL*Y3**2*U2+KYEL*X3**2*V2-
     &       4.D0*KYEL*X3**2*V6-4.D0*KXEL*Y2**2*U4+KYEL*X2**2*V3+
     &       4.D0*KXEL*Y2**2*U5+4.D0*KYEL*X2**2*V5+KXEL*Y2*X2*V1-
     &       KXEL*Y2*X3*V1-KXEL*Y2*X2*V3-KXEL*Y2**2*U1+
     &       KXEL*Y2**2*U3-KYEL*X2**2*V1-4.D0*KXEL*Y2*X2*V5
      ANS2 = 8.D0*KXEL*Y2*X3*V5+4.D0*KXEL*Y2*X2*V4-KYEL*X3*Y3*U2+
     &       KYEL*X3*X2*V3+4.D0*KYEL*X3*Y2*U6-8.D0*KYEL*X3*X2*V6+
     &       4.D0*KYEL*X3*Y3*U6+KXEL*Y3*Y2*U3+4.D0*KXEL*Y3*X3*V6-
     &       8.D0*KXEL*Y3*Y2*U6+4.D0*KXEL*Y3**2*U5+
     &       4.D0*KXEL*Y2*X3*V6+KYEL*X2*Y2*U1-KYEL*X2*Y3*U1-
     &       KYEL*X2*Y2*U3+4.D0*KYEL*X2*Y3*U6-4.D0*KYEL*X2**2*V4+
     &       KYEL*X3*X2*V2+4.D0*KYEL*X3*Y2*U4+4.D0*KXEL*Y3*X2*V5-
     &       12.D0*KXEL*Y3*Y2*U5-4.D0*KXEL*Y3*X3*V5-
     &       8.D0*KYEL*X3*X2*V4+4.D0*KXEL*Y3*X2*V6-
     &       12.D0*KYEL*X3*X2*V5-4.D0*KYEL*X3*Y3*U5+
     &       4.D0*KYEL*X3*Y2*U5-KXEL*Y3*X2*V2+4.D0*KYEL*X2*Y3*U4+
     &       4.D0*KXEL*Y2*X3*V4-KYEL*X3*Y2*U3-KXEL*Y3*X2*V3
      A46(IELEM)=  (ANS1+ANS2)*2.D0*XMUL/(45.D0*(X2*Y3-Y2*X3))
!
      ANS1 = -KYEL*X3*Y2*U2+6.D0*KYEL*X2*Y2*U2+6.D0*KXEL*Y2*X2*V2+
     &       4.D0*KYEL*X2**2*V6+3.D0*KXEL*Y3**2*U1-
     &       6.D0*KXEL*Y3*Y2*U1+3.D0*KXEL*Y3*X2*V1-
     &       6.D0*KYEL*X2**2*V2+KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V4-
     &       6.D0*KXEL*Y2**2*U2+4.D0*KXEL*Y3**2*U4-
     &       12.D0*KXEL*Y3*Y2*U4+4.D0*KXEL*Y2**2*U6+
     &       3.D0*KYEL*X3*Y2*U1-3.D0*KYEL*X3*Y3*U1-
     &       6.D0*KYEL*X3*X2*V1-4.D0*KXEL*Y3*X3*V4+
     &       7.D0*KXEL*Y3*Y2*U2+8.D0*KXEL*Y3**2*U6-
     &       8.D0*KYEL*X3**2*V5+3.D0*KYEL*X3**2*V1-
     &       3.D0*KXEL*Y3*X3*V1-8.D0*KYEL*X2*Y3*U5+
     &       8.D0*KYEL*X2*Y2*U5-8.D0*KYEL*X2*Y2*U4-
     &       KXEL*Y3**2*U2-KYEL*X3**2*V2+8.D0*KYEL*X3**2*V6-
     &       6.D0*KYEL*X3**2*V3-6.D0*KXEL*Y3**2*U3+
     &       8.D0*KXEL*Y2**2*U4-KYEL*X2**2*V3-8.D0*KXEL*Y2**2*U5-
     &       8.D0*KYEL*X2**2*V5-3.D0*KXEL*Y2*X2*V1+
     &       3.D0*KXEL*Y2*X3*V1+KXEL*Y2*X2*V3+3.D0*KXEL*Y2**2*U1-
     &       KXEL*Y2**2*U3+3.D0*KYEL*X2**2*V1+8.D0*KXEL*Y2*X2*V5
      ANS2 = -8.D0*KXEL*Y2*X3*V5-8.D0*KXEL*Y2*X2*V4+
     &       KYEL*X3*Y3*U2+6.D0*KYEL*X3*Y3*U3+7.D0*KYEL*X3*X2*V3+
     &       8.D0*KYEL*X3*Y2*U6-12.D0*KYEL*X3*X2*V6-
     &       8.D0*KYEL*X3*Y3*U6+7.D0*KXEL*Y3*Y2*U3-
     &       8.D0*KXEL*Y3*X3*V6+6.D0*KXEL*Y3*X3*V3-
     &       12.D0*KXEL*Y3*Y2*U6-8.D0*KXEL*Y3**2*U5+
     &       4.D0*KYEL*X3**2*V4+4.D0*KXEL*Y2*X3*V6-
     &       3.D0*KYEL*X2*Y2*U1+3.D0*KYEL*X2*Y3*U1+
     &       KYEL*X2*Y2*U3-KYEL*X2*Y3*U3+4.D0*KYEL*X2*Y3*U6-
     &       KXEL*Y2*X3*V3+8.D0*KYEL*X2**2*V4+7.D0*KYEL*X3*X2*V2-
     &       4.D0*KYEL*X3*Y3*U4+4.D0*KYEL*X3*Y2*U4-
     &       8.D0*KXEL*Y3*X2*V5+16.D0*KXEL*Y3*Y2*U5+
     &       8.D0*KXEL*Y3*X3*V5-12.D0*KYEL*X3*X2*V4+
     &       8.D0*KXEL*Y3*X2*V6+16.D0*KYEL*X3*X2*V5+
     &       8.D0*KYEL*X3*Y3*U5-8.D0*KYEL*X3*Y2*U5-
     &       6.D0*KYEL*X2*Y3*U2-6.D0*KXEL*Y2*X3*V2-
     &       KXEL*Y3*X2*V2+8.D0*KYEL*X2*Y3*U4+8.D0*KXEL*Y2*X3*V4-
     &       4.D0*KXEL*Y2*X2*V6-6.D0*KYEL*X3*Y2*U3-
     &       4.D0*KYEL*X2*Y2*U6-6.D0*KXEL*Y3*X2*V3
      A51(IELEM) = (ANS1+ANS2)*XMUL/(90.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = -KXEL*Y3**2*U1+5.D0*KXEL*Y3*Y2*U1-3.D0*KXEL*Y3*X3*V2+
     &       4.D0*KXEL*Y3**2*U4-24.D0*KXEL*Y3*Y2*U4+KYEL*X3*Y3*U1+
     &       5.D0*KYEL*X3*X2*V1-4.D0*KXEL*Y3*X3*V4-
     &       18.D0*KXEL*Y3*Y2*U2-8.D0*KXEL*Y3**2*U6+
     &       8.D0*KYEL*X3**2*V5-KYEL*X3**2*V1+KXEL*Y3*X3*V1+
     &       24.D0*KYEL*X2*Y3*U5+3.D0*KXEL*Y3**2*U2+
     &       3.D0*KYEL*X3**2*V2-8.D0*KYEL*X3**2*V6-
     &       6.D0*KYEL*X3**2*V3-6.D0*KXEL*Y3**2*U3-
     &       5.D0*KXEL*Y2*X3*V1+24.D0*KXEL*Y2*X3*V5-
     &       3.D0*KYEL*X3*Y3*U2+6.D0*KYEL*X3*Y3*U3
      ANS2 = 5.D0*KYEL*X3*X2*V3 -4.D0*KYEL*X3*X2*V6+
     &       8.D0*KYEL*X3*Y3*U6+5.D0*KXEL*Y3*Y2*U3+
     &       8.D0*KXEL*Y3*X3*V6+6.D0*KXEL*Y3*X3*V3-
     &       4.D0*KXEL*Y3*Y2*U6+8.D0*KXEL*Y3**2*U5+
     &       4.D0*KYEL*X3**2*V4+4.D0*KXEL*Y2*X3*V6-
     &       5.D0*KYEL*X2*Y3*U1-5.D0*KYEL*X2*Y3*U3+
     &       4.D0*KYEL*X2*Y3*U6-5.D0*KXEL*Y2*X3*V3-
     &       18.D0*KYEL*X3*X2*V2-4.D0*KYEL*X3*Y3*U4-
     &       24.D0*KXEL*Y3*Y2*U5-8.D0*KXEL*Y3*X3*V5-
     &       24.D0*KYEL*X3*X2*V4-24.D0*KYEL*X3*X2*V5-
     &       8.D0*KYEL*X3*Y3*U5+18.D0*KYEL*X2*Y3*U2+
     &       18.D0*KXEL*Y2*X3*V2+24.D0*KYEL*X2*Y3*U4+
     &       24.D0*KXEL*Y2*X3*V4
      A52(IELEM)= (ANS1+ANS2)*XMUL/(90.D0*(X2*Y3-Y2*X3))
!
      ANS1 = 5.D0*KYEL*X3*Y2*U2-6.D0*KYEL*X2*Y2*U2-
     &       6.D0*KXEL*Y2*X2*V2-4.D0*KYEL*X2**2*V6-
     &       5.D0*KXEL*Y3*Y2*U1+5.D0*KXEL*Y3*X2*V1+
     &       6.D0*KYEL*X2**2*V2-4.D0*KXEL*Y3*X2*V4+
     &       6.D0*KXEL*Y2**2*U2+4.D0*KXEL*Y3*Y2*U4-
     &       4.D0*KXEL*Y2**2*U6+5.D0*KYEL*X3*Y2*U1-
     &       5.D0*KYEL*X3*X2*V1-5.D0*KXEL*Y3*Y2*U2+
     &       8.D0*KYEL*X2*Y2*U5-8.D0*KYEL*X2*Y2*U4+
     &       8.D0*KXEL*Y2**2*U4-3.D0*KYEL*X2**2*V3-
     &       8.D0*KXEL*Y2**2*U5-8.D0*KYEL*X2**2*V5-
     &       KXEL*Y2*X2*V1+3.D0*KXEL*Y2*X2*V3+KXEL*Y2**2*U1-
     &       3.D0*KXEL*Y2**2*U3
      ANS2 = KYEL*X2**2*V1+8*KXEL*Y2*X2*V5-8.D0*KXEL*Y2*X2*V4+
     &       18.D0*KYEL*X3*X2*V3-24.D0*KYEL*X3*Y2*U6+
     &       24.D0*KYEL*X3*X2*V6+18.D0*KXEL*Y3*Y2*U3+
     &       24.D0*KXEL*Y3*Y2*U6-KYEL*X2*Y2*U1+3.D0*KYEL*X2*Y2*U3+
     &       8.D0*KYEL*X2**2*V4-5.D0*KYEL*X3*X2*V2-
     &       4.D0*KYEL*X3*Y2*U4-24.D0*KXEL*Y3*X2*V5+
     &       24.D0*KXEL*Y3*Y2*U5+4.D0*KYEL*X3*X2*V4-
     &       24.D0*KXEL*Y3*X2*V6+24.D0*KYEL*X3*X2*V5-
     &       24.D0*KYEL*X3*Y2*U5+5.D0*KXEL*Y3*X2*V2+
     &       4.D0*KXEL*Y2*X2*V6-18.D0*KYEL*X3*Y2*U3+
     &       4.D0*KYEL*X2*Y2*U6-18.D0*KXEL*Y3*X2*V3
      A53(IELEM)= (ANS1+ANS2)*XMUL/(90.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = -6.D0*KYEL*X2*Y2*U2-6.D0*KXEL*Y2*X2*V2+
     &       4.D0*KYEL*X2**2*V6-KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U1-
     &       KXEL*Y3*X2*V1+6.D0*KYEL*X2**2*V2-KXEL*Y3*X3*V2+
     &       4.D0*KXEL*Y3*X2*V4+6.D0*KXEL*Y2**2*U2-
     &       8.D0*KXEL*Y3*Y2*U4+4.D0*KXEL*Y2**2*U6-
     &       KYEL*X3*Y2*U1+KYEL*X3*Y3*U1+3.D0*KYEL*X3*X2*V1-
     &       6.D0*KXEL*Y3*Y2*U2-4.D0*KXEL*Y3**2*U6+
     &       4.D0*KYEL*X3**2*V5-KYEL*X3**2*V1+KXEL*Y3*X3*V1+
     &       8.D0*KYEL*X2*Y3*U5-12.D0*KYEL*X2*Y2*U5-
     &       12.D0*KYEL*X2*Y2*U4+KXEL*Y3**2*U2+KYEL*X3**2*V2-
     &       4.D0*KYEL*X3**2*V6+12.D0*KXEL*Y2**2*U4-
     &       2.D0*KYEL*X2**2*V3+12.D0*KXEL*Y2**2*U5+
     &       12.D0*KYEL*X2**2*V5+2.D0*KXEL*Y2*X2*V1-
     &       2.D0*KXEL*Y2*X3*V1+2.D0*KXEL*Y2*X2*V3-
     &       2.D0*KXEL*Y2**2*U1-2.D0*KXEL*Y2**2*U3
      ANS2 = -2.D0*KYEL*X2**2*V1-12.D0*KXEL*Y2*X2*V5+
     &       8.D0*KXEL*Y2*X3*V5-12.D0*KXEL*Y2*X2*V4-
     &       KYEL*X3*Y3*U2+KYEL*X3*X2*V3+4.D0*KYEL*X3*Y2*U6-
     &       4.D0*KYEL*X3*X2*V6+4.D0*KYEL*X3*Y3*U6+KXEL*Y3*Y2*U3+
     &       4.D0*KXEL*Y3*X3*V6-4.D0*KXEL*Y3*Y2*U6+
     &       4.D0*KXEL*Y3**2*U5+2.D0*KYEL*X2*Y2*U1-
     &       2.D0*KYEL*X2*Y3*U1+2.D0*KYEL*X2*Y2*U3-
     &       KYEL*X2*Y3*U3-KXEL*Y2*X3*V3+12.D0*KYEL*X2**2*V4-
     &       6.D0*KYEL*X3*X2*V2+4.D0*KYEL*X3*Y2*U4+
     &       8.D0*KXEL*Y3*X2*V5-16.D0*KXEL*Y3*Y2*U5-
     &       4.D0*KXEL*Y3*X3*V5-8.D0*KYEL*X3*X2*V4+
     &       4.D0*KXEL*Y3*X2*V6-16.D0*KYEL*X3*X2*V5-
     &       4.D0*KYEL*X3*Y3*U5+8.D0*KYEL*X3*Y2*U5+
     &       6.D0*KYEL*X2*Y3*U2+6.D0*KXEL*Y2*X3*V2+
     &       4.D0*KYEL*X2*Y3*U4+4.D0*KXEL*Y2*X3*V4-
     &       4.D0*KXEL*Y2*X2*V6-4.D0*KYEL*X2*Y2*U6
      A54(IELEM)= (ANS1+ANS2)*2.D0*XMUL/(45.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = -KYEL*X3*Y2*U2-2.D0*KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U1-
     &        2.D0*KXEL*Y3*X2*V1+2.D0*KXEL*Y3*X3*V2+
     &        4.D0*KXEL*Y3**2*U4-4.D0*KXEL*Y3*Y2*U4-
     &        2.D0*KYEL*X3*Y2*U1+2.D0*KYEL*X3*Y3*U1+
     &        3.D0*KYEL*X3*X2*V1-4.D0*KXEL*Y3*X3*V4+
     &        KXEL*Y3*Y2*U2+12.D0*KXEL*Y3**2*U6+
     &        12.D0*KYEL*X3**2*V5-2.D0*KYEL*X3**2*V1+
     &        2.D0*KXEL*Y3*X3*V1+8.D0*KYEL*X2*Y3*U5-
     &        4.D0*KYEL*X2*Y2*U5+4.D0*KYEL*X2*Y2*U4-
     &        2.D0*KXEL*Y3**2*U2-2.D0*KYEL*X3**2*V2+
     &        12.D0*KYEL*X3**2*V6+6.D0*KYEL*X3**2*V3+
     &        6.D0*KXEL*Y3**2*U3-4.D0*KXEL*Y2**2*U4+
     &        KYEL*X2**2*V3+4.D0*KXEL*Y2**2*U5+4.D0*KYEL*X2**2*V5+
     &        KXEL*Y2*X2*V1-KXEL*Y2*X3*V1-KXEL*Y2*X2*V3-
     &        KXEL*Y2**2*U1+KXEL*Y2**2*U3-KYEL*X2**2*V1-
     &        4.D0*KXEL*Y2*X2*V5
      ANS2 = 8.D0*KXEL*Y2*X3*V5+4.D0*KXEL*Y2*X2*V4+
     &       2.D0*KYEL*X3*Y3*U2-6.D0*KYEL*X3*Y3*U3-
     &       6.D0*KYEL*X3*X2*V3+4.D0*KYEL*X3*Y2*U6-
     &       8.D0*KYEL*X3*X2*V6-12.D0*KYEL*X3*Y3*U6-
     &       6.D0*KXEL*Y3*Y2*U3-12.D0*KXEL*Y3*X3*V6-
     &       6.D0*KXEL*Y3*X3*V3-8.D0*KXEL*Y3*Y2*U6+
     &       12.D0*KXEL*Y3**2*U5+4.D0*KYEL*X3**2*V4+
     &       4.D0*KXEL*Y2*X3*V6+KYEL*X2*Y2*U1-KYEL*X2*Y3*U1-
     &       KYEL*X2*Y2*U3+4.D0*KYEL*X2*Y3*U6-4.D0*KYEL*X2**2*V4+
     &       KYEL*X3*X2*V2-4.D0*KYEL*X3*Y3*U4+8.D0*KXEL*Y3*X2*V5-
     &       16.D0*KXEL*Y3*Y2*U5-12.D0*KXEL*Y3*X3*V5-
     &       4.D0*KYEL*X3*X2*V4+4.D0*KXEL*Y3*X2*V6-
     &       16.D0*KYEL*X3*X2*V5-12.D0*KYEL*X3*Y3*U5+
     &       8.D0*KYEL*X3*Y2*U5-KXEL*Y3*X2*V2+4.D0*KYEL*X2*Y3*U4+
     &       4.D0*KXEL*Y2*X3*V4+6.D0*KYEL*X3*Y2*U3+6.D0*KXEL*Y3*X2*V3
      A56(IELEM)= (ANS1+ANS2)*2.D0*XMUL/(45.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = -KYEL*X3*Y2*U2-4.D0*KYEL*X2*Y2*U2-4.D0*KXEL*Y2*X2*V2-
     &       16.D0*KYEL*X2**2*V6+3.D0*KXEL*Y3**2*U1+
     &       12.D0*KXEL*Y3*Y2*U1+3.D0*KXEL*Y3*X2*V1+
     &       4.D0*KYEL*X2**2*V2+KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V4+
     &       4.D0*KXEL*Y2**2*U2+4.D0*KXEL*Y3**2*U4+
     &       16.D0*KXEL*Y3*Y2*U4-16.D0*KXEL*Y2**2*U6+
     &       3.D0*KYEL*X3*Y2*U1-3.D0*KYEL*X3*Y3*U1+
     &       12.D0*KYEL*X3*X2*V1-4.D0*KXEL*Y3*X3*V4-
     &       3.D0*KXEL*Y3*Y2*U2+8.D0*KXEL*Y3**2*U6-
     &       8.D0*KYEL*X3**2*V5+3.D0*KYEL*X3**2*V1-
     &       3.D0*KXEL*Y3*X3*V1-12.D0*KYEL*X2*Y3*U5+
     &       12.D0*KYEL*X2*Y2*U5+20.D0*KYEL*X2*Y2*U4-KXEL*Y3**2*U2-
     &       KYEL*X3**2*V2+8.D0*KYEL*X3**2*V6-6.D0*KYEL*X3**2*V3-
     &       6.D0*KXEL*Y3**2*U3-20.D0*KXEL*Y2**2*U4-KYEL*X2**2*V3-
     &       12.D0*KXEL*Y2**2*U5-12.D0*KYEL*X2**2*V5+
     &       15.D0*KXEL*Y2*X2*V1-15.D0*KXEL*Y2*X3*V1+
     &       KXEL*Y2*X2*V3-15.D0*KXEL*Y2**2*U1-KXEL*Y2**2*U3-
     &       15.D0*KYEL*X2**2*V1+12.D0*KXEL*Y2*X2*V5
      ANS2 = -12.D0*KXEL*Y2*X3*V5+20.D0*KXEL*Y2*X2*V4+
     &       KYEL*X3*Y3*U2+6.D0*KYEL*X3*Y3*U3+7.D0*KYEL*X3*X2*V3+
     &       8.D0*KYEL*X3*Y2*U6+8.D0*KYEL*X3*X2*V6-
     &       8.D0*KYEL*X3*Y3*U6+7.D0*KXEL*Y3*Y2*U3-
     &       8.D0*KXEL*Y3*X3*V6+6.D0*KXEL*Y3*X3*V3+
     &       8.D0*KXEL*Y3*Y2*U6-8.D0*KXEL*Y3**2*U5+
     &       4.D0*KYEL*X3**2*V4-16.D0*KXEL*Y2*X3*V6+
     &       15.D0*KYEL*X2*Y2*U1-15.D0*KYEL*X2*Y3*U1+
     &       KYEL*X2*Y2*U3-KYEL*X2*Y3*U3-16.D0*KYEL*X2*Y3*U6-
     &       KXEL*Y2*X3*V3-20.D0*KYEL*X2**2*V4-3.D0*KYEL*X3*X2*V2-
     &       4.D0*KYEL*X3*Y3*U4+4.D0*KYEL*X3*Y2*U4-
     &       8.D0*KXEL*Y3*X2*V5+20.D0*KXEL*Y3*Y2*U5+
     &       8.D0*KXEL*Y3*X3*V5+16.D0*KYEL*X3*X2*V4+
     &       8.D0*KXEL*Y3*X2*V6+20.D0*KYEL*X3*X2*V5+
     &       8.D0*KYEL*X3*Y3*U5-8.D0*KYEL*X3*Y2*U5+
     &       4.D0*KYEL*X2*Y3*U2+4.D0*KXEL*Y2*X3*V2-
     &       KXEL*Y3*X2*V2-20.D0*KYEL*X2*Y3*U4-20.D0*KXEL*Y2*X3*V4+
     &       16.D0*KXEL*Y2*X2*V6-6.D0*KYEL*X3*Y2*U3+16.D0*KYEL*X2*
     &       Y2*U6-6.D0*KXEL*Y3*X2*V3
      A61(IELEM)= (ANS1+ANS2)*XMUL/(90.D0*(X2*Y3-Y2*X3))
!
      ANS1 = -KXEL*Y3**2*U1-5.D0*KXEL*Y3*Y2*U1-
     &       3.D0*KXEL*Y3*X3*V2+4.D0*KXEL*Y3**2*U4+
     &       4.D0*KXEL*Y3*Y2*U4+KYEL*X3*Y3*U1-5.D0*KYEL*X3*X2*V1-
     &       4.D0*KXEL*Y3*X3*V4-8.D0*KXEL*Y3**2*U6+
     &       8.D0*KYEL*X3**2*V5-KYEL*X3**2*V1+KXEL*Y3*X3*V1+
     &       4.D0*KYEL*X2*Y3*U5+3.D0*KXEL*Y3**2*U2+
     &       3.D0*KYEL*X3**2*V2-8.D0*KYEL*X3**2*V6-
     &       6.D0*KYEL*X3**2*V3-6.D0*KXEL*Y3**2*U3+
     &       5.D0*KXEL*Y2*X3*V1+4.D0*KXEL*Y2*X3*V5-
     &       3.D0*KYEL*X3*Y3*U2+6.D0*KYEL*X3*Y3*U3+
     &       5.D0*KYEL*X3*X2*V3+8.D0*KYEL*X3*Y3*U6+
     &       5.D0*KXEL*Y3*Y2*U3+8.D0*KXEL*Y3*X3*V6+
     &       6.D0*KXEL*Y3*X3*V3+8.D0*KXEL*Y3**2*U5+
     &       4.D0*KYEL*X3**2*V4+5.D0*KYEL*X2*Y3*U1-
     &       5.D0*KYEL*X2*Y3*U3-5.D0*KXEL*Y2*X3*V3-
     &       4.D0*KYEL*X3*Y3*U4-4.D0*KXEL*Y3*Y2*U5-
     &       8.D0*KXEL*Y3*X3*V5+4.D0*KYEL*X3*X2*V4-
     &       4.D0*KYEL*X3*X2*V5-8.D0*KYEL*X3*Y3*U5-
     &       4.D0*KYEL*X2*Y3*U4-4.D0*KXEL*Y2*X3*V4
      A62(IELEM)= ANS1*XMUL/(90.D0*(-X2*Y3+Y2*X3))
!
      ANS1 = 5.D0*KYEL*X3*Y2*U2-4.D0*KYEL*X2*Y2*U2-
     &       4.D0*KXEL*Y2*X2*V2-16.D0*KYEL*X2**2*V6-
     &       5.D0*KXEL*Y3*Y2*U1+5.D0*KXEL*Y3*X2*V1+
     &       4.D0*KYEL*X2**2*V2-4.D0*KXEL*Y3*X2*V4+
     &       4.D0*KXEL*Y2**2*U2+4.D0*KXEL*Y3*Y2*U4-
     &       16.D0*KXEL*Y2**2*U6+5.D0*KYEL*X3*Y2*U1-
     &       5.D0*KYEL*X3*X2*V1-5.D0*KXEL*Y3*Y2*U2+
     &       20.D0*KYEL*X2*Y2*U5+12.D0*KYEL*X2*Y2*U4-
     &       12.D0*KXEL*Y2**2*U4-15.D0*KYEL*X2**2*V3-
     &       20.D0*KXEL*Y2**2*U5-20.D0*KYEL*X2**2*V5+
     &       KXEL*Y2*X2*V1+15.D0*KXEL*Y2*X2*V3-KXEL*Y2**2*U1-
     &       15.D0*KXEL*Y2**2*U3-KYEL*X2**2*V1
      ANS2 = 20.D0*KXEL*Y2*X2*V5+12.D0*KXEL*Y2*X2*V4+
     &       18.D0*KYEL*X3*X2*V3-24.D0*KYEL*X3*Y2*U6+
     &       24.D0*KYEL*X3*X2*V6+18.D0*KXEL*Y3*Y2*U3+
     &       24.D0*KXEL*Y3*Y2*U6+KYEL*X2*Y2*U1+
     &       15.D0*KYEL*X2*Y2*U3-12.D0*KYEL*X2**2*V4-
     &       5.D0*KYEL*X3*X2*V2-4.D0*KYEL*X3*Y2*U4-
     &       24.D0*KXEL*Y3*X2*V5+24.D0*KXEL*Y3*Y2*U5+
     &       4.D0*KYEL*X3*X2*V4-24.D0*KXEL*Y3*X2*V6+
     &       24.D0*KYEL*X3*X2*V5-24.D0*KYEL*X3*Y2*U5+
     &       5.D0*KXEL*Y3*X2*V2+16.D0*KXEL*Y2*X2*V6-
     &       18.D0*KYEL*X3*Y2*U3+16.D0*KYEL*X2*Y2*U6-
     &       18.D0*KXEL*Y3*X2*V3
      A63(IELEM)= (ANS1+ANS2)*XMUL/(90.D0*(X2*Y3-Y2*X3))
!
      ANS1 = -KXEL*Y3**2*U1-4.D0*KXEL*Y3*Y2*U1-KXEL*Y3*X2*V1-
     &       KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V4-8.D0*KXEL*Y3*Y2*U4-
     &       KYEL*X3*Y2*U1+KYEL*X3*Y3*U1-4.D0*KYEL*X3*X2*V1+
     &       KXEL*Y3*Y2*U2-4.D0*KXEL*Y3**2*U6+4.D0*KYEL*X3**2*V5-
     &       KYEL*X3**2*V1+KXEL*Y3*X3*V1+4.D0*KYEL*X2*Y3*U5-
     &       4.D0*KYEL*X2*Y2*U5+4.D0*KYEL*X2*Y2*U4+KXEL*Y3**2*U2+
     &       KYEL*X3**2*V2-4.D0*KYEL*X3**2*V6-4.D0*KXEL*Y2**2*U4+
     &       KYEL*X2**2*V3+4.D0*KXEL*Y2**2*U5+4.D0*KYEL*X2**2*V5+
     &       KXEL*Y2*X2*V1+5.D0*KXEL*Y2*X3*V1-KXEL*Y2*X2*V3-
     &       KXEL*Y2**2*U1+KXEL*Y2**2*U3-KYEL*X2**2*V1-
     &       4.D0*KXEL*Y2*X2*V5+4.D0*KXEL*Y2*X3*V5+
     &       4.D0*KXEL*Y2*X2*V4-KYEL*X3*Y3*U2+KYEL*X3*X2*V3+
     &       4.D0*KYEL*X3*Y2*U6-8.D0*KYEL*X3*X2*V6
      ANS2 = 4.D0*KYEL*X3*Y3*U6+KXEL*Y3*Y2*U3+4.D0*KXEL*Y3*X3*V6-
     &       8.D0*KXEL*Y3*Y2*U6+4.D0*KXEL*Y3**2*U5+
     &       4.D0*KXEL*Y2*X3*V6+KYEL*X2*Y2*U1+5.D0*KYEL*X2*Y3*U1-
     &       KYEL*X2*Y2*U3-KYEL*X2*Y3*U3+4.D0*KYEL*X2*Y3*U6-
     &       KXEL*Y2*X3*V3-4.D0*KYEL*X2**2*V4+KYEL*X3*X2*V2+
     &       4.D0*KYEL*X3*Y2*U4+8.D0*KXEL*Y3*X2*V5-
     &       12.D0*KXEL*Y3*Y2*U5-4.D0*KXEL*Y3*X3*V5-
     &       8.D0*KYEL*X3*X2*V4+4.D0*KXEL*Y3*X2*V6-
     &       12.D0*KYEL*X3*X2*V5-4.D0*KYEL*X3*Y3*U5+
     &       8.D0*KYEL*X3*Y2*U5-KYEL*X2*Y3*U2-KXEL*Y2*X3*V2+
     &       4.D0*KYEL*X2*Y3*U4+4.D0*KXEL*Y2*X3*V4
      A64(IELEM)= (ANS1+ANS2)*XMUL*2.D0/(45.D0*(X2*Y3-Y2*X3))
!
      ANS1 = -2.D0*KXEL*Y3**2*U1+3.D0*KXEL*Y3*Y2*U1-KXEL*Y3*X2*V1+
     &       2.D0*KXEL*Y3*X3*V2+4.D0*KXEL*Y3*X2*V4+
     &       4.D0*KXEL*Y3**2*U4-4.D0*KXEL*Y3*Y2*U4-
     &       KYEL*X3*Y2*U1+2.D0*KYEL*X3*Y3*U1+3.D0*KYEL*X3*X2*V1-
     &       4.D0*KXEL*Y3*X3*V4+KXEL*Y3*Y2*U2+12.D0*KXEL*Y3**2*U6+
     &       12.D0*KYEL*X3**2*V5-2.D0*KYEL*X3**2*V1+
     &       2.D0*KXEL*Y3*X3*V1+8.D0*KYEL*X2*Y3*U5-
     &       4.D0*KYEL*X2*Y2*U5+4.D0*KYEL*X2*Y2*U4-
     &       2.D0*KXEL*Y3**2*U2-2.D0*KYEL*X3**2*V2+
     &       12.D0*KYEL*X3**2*V6+6.D0*KYEL*X3**2*V3+
     &       6.D0*KXEL*Y3**2*U3-4.D0*KXEL*Y2**2*U4+
     &       KYEL*X2**2*V3+4.D0*KXEL*Y2**2*U5+4.D0*KYEL*X2**2*V5+
     &       KXEL*Y2*X2*V1-2.D0*KXEL*Y2*X3*V1-KXEL*Y2*X2*V3-
     &       KXEL*Y2**2*U1+KXEL*Y2**2*U3-KYEL*X2**2*V1-
     &       4.D0*KXEL*Y2*X2*V5
      ANS2 = 8.D0*KXEL*Y2*X3*V5+4.D0*KXEL*Y2*X2*V4+
     &       2.D0*KYEL*X3*Y3*U2-6.D0*KYEL*X3*Y3*U3-
     &       6.D0*KYEL*X3*X2*V3+4.D0*KYEL*X3*Y2*U6-
     &       8.D0*KYEL*X3*X2*V6-12.D0*KYEL*X3*Y3*U6-
     &       6.D0*KXEL*Y3*Y2*U3-12.D0*KXEL*Y3*X3*V6-
     &       6.D0*KXEL*Y3*X3*V3-8.D0*KXEL*Y3*Y2*U6+
     &       12.D0*KXEL*Y3**2*U5+4.D0*KYEL*X3**2*V4+
     &       4.D0*KXEL*Y2*X3*V6+KYEL*X2*Y2*U1-2.D0*KYEL*X2*Y3*U1-
     &       KYEL*X2*Y2*U3+6.D0*KYEL*X2*Y3*U3+4.D0*KYEL*X2*Y3*U6+
     &       6.D0*KXEL*Y2*X3*V3-4.D0*KYEL*X2**2*V4+KYEL*X3*X2*V2-
     &       4.D0*KYEL*X3*Y3*U4+4.D0*KYEL*X3*Y2*U4+
     &       8.D0*KXEL*Y3*X2*V5-16.D0*KXEL*Y3*Y2*U5-
     &       12.D0*KXEL*Y3*X3*V5-4.D0*KYEL*X3*X2*V4+
     &       4.D0*KXEL*Y3*X2*V6-16.D0*KYEL*X3*X2*V5-
     &       12.D0*KYEL*X3*Y3*U5+8.D0*KYEL*X3*Y2*U5-KYEL*X2*Y3*U2-
     &       KXEL*Y2*X3*V2
      A65(IELEM)= (ANS1+ANS2)*XMUL*2.D0/(45.D0*(-X2*Y3+Y2*X3))
!
! USES HERE THE 'MAGIC SQUARE' PROPERTIES
! (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
!
      A16(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
     &             - A14(IELEM) - A15(IELEM)
      A25(IELEM) = - A21(IELEM) - A22(IELEM) - A23(IELEM)
     &             - A24(IELEM) - A26(IELEM)
      A36(IELEM) = - A31(IELEM) - A32(IELEM) - A33(IELEM)
     &             - A34(IELEM) - A35(IELEM)
      A43(IELEM) = - A41(IELEM) - A42(IELEM) - A44(IELEM)
     &             - A45(IELEM) - A46(IELEM)
      A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &             - A54(IELEM) - A56(IELEM)
      A66(IELEM) = - A61(IELEM) - A62(IELEM) - A63(IELEM)
     &             - A64(IELEM) - A65(IELEM)
!
      ENDDO ! IELEM
!
!     OTHER TYPES OF FUNCTIONS F AND G
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
        WRITE(LU,111) IELMG,SG%NAME
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'MT03CC (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
111     FORMAT(1X,'DISCRETIZATION OF G:',1I6,
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
