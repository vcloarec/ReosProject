!                   *****************
                    SUBROUTINE MT05CC
!                   *****************
!
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &  A21 , A22 , A23 , A24 , A25 , A26 ,
     &  A31 , A32 , A33 , A34 , A35 , A36 ,
     &  A41 , A42 , A43 , A44 , A45 , A46 ,
     &  A51 , A52 , A53 , A54 , A55 , A56 ,
     &  A61 , A62 , A63 , A64 , A65 , A66 ,
     &  XMUL,SU,SV,U,V,
     &  XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     &  NELEM,NELMAX,FORMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE FOLLOWING MATRIX FOR P2 TRIANGLES:
!code
!+    ->--->
!+    U.GRAD
!
!history  ALGIANE FROEHLY; C MOULIN (LNH)
!+        09/07/2008
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
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| FOURTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE5          |-->| FIFTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE6          |-->| SIXTH POINTS OF TRIANGLES (QUADRATIC)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
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
      USE BIEF!, EX_MT05CC => MT05CC
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
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!
!     STRUCTURES OF      U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
      CHARACTER(LEN=16) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,IELMU,IELMV
!
      DOUBLE PRECISION X2,X3
      DOUBLE PRECISION Y2,Y3
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6
      DOUBLE PRECISION V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION XSU360,XSUR90,XSUR45
      DOUBLE PRECISION XSUR2520,XSUR630
!=======================================================================
!
!     EXTRACTS THE TYPE OF ELEMENT FOR VELOCITY
!
      IELMU = SU%ELM
      IELMV = SV%ELM
!
!-----------------------------------------------------------------------
!
      XSU360   = XMUL / 360.D0
      XSUR45   = XMUL /  45.D0
      XSUR90   = XMUL /  90.D0
      XSUR2520 = XMUL /  2520.D0
      XSUR630  = XMUL /  630.D0
!
!-----------------------------------------------------------------------
!
!     CASE WHERE U AND V ARE CONSTANT BY ELEMENT
!
      IF(IELMU.EQ.10.AND.IELMV.EQ.10) THEN
!
!-----------------------------------------------------------------------
!
!  P1 DISCRETISATION OF THE VELOCITY:
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        U1 = U(IELEM)
        U2 = U(IELEM)
        U3 = U(IELEM)
        V1 = V(IELEM)
        V2 = V(IELEM)
        V3 = V(IELEM)
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)=(( 5.D0*V2+V3+V1*6.D0)*X3 +
     &             ( -U1*6.D0-U3-U2*5.D0)*Y3) * XSU360
        A13(IELEM)=((-V2-V3*5.D0-V1*6.D0)*X2 +
     &             (  U1*6.D0+U3*5.D0+U2)*Y2) * XSU360
        A14(IELEM)=(( V2*2.D0+V3)*X2+(-V2*2.D0-V3-V1*6.D0)*X3 +
     &             ( -U2*2.D0-U3)*Y2+(U1*6.D0+U3+U2*2.D0)*Y3) * XSUR90
        A15(IELEM)=((-V3-V2*2.D0)*X2+(V2+V3*2.D0)*X3 +
     &             (U2*2.D0+U3)*Y2+(-U2-U3*2.D0)*Y3) * XSUR90
        A16(IELEM)=((V2+V3*2.D0+V1*6.D0)*X2+(-V2-V3*2.D0)*X3 +
     &             (-U2-U3*2.D0-U1*6.D0)*Y2+(U2+U3*2.D0)*Y3) * XSUR90
!
        A21(IELEM)=((V2*6.D0+V3+V1*5.D0)*X2 +
     &             (-V2*6.D0-V3-V1*5.D0)*X3 +
     &             (-U2*6.D0-U3-U1*5.D0)*Y2 +
     &             (U1*5.D0+U3+U2*6.D0)*Y3) * XSU360
        A23(IELEM)=((-V2*6.D0-V3*5.D0-V1)*X2 +
     &             (  U1+U3*5.D0+U2*6.D0)*Y2) * XSU360
        A24(IELEM)=(-V2*X2*6.D0+(V2*6.D0+V3+V1*2.D0)*X3+U2*Y2*6.D0 +
     &             (-U2*6.D0-U3-U1*2.D0)*Y3) * XSUR90
        A25(IELEM)=(V2*X2*6.D0+(V3*2.D0+V1)*X3-U2*Y2*6.D0 +
     &             (-U3*2.D0-U1)*Y3) * XSUR90
        A26(IELEM)=((V3-V1)*X2+(-V3*2.D0-V1)*X3+(-U3+U1)*Y2 +
     &             (U1+U3*2.D0)*Y3) * XSUR90
!
        A31(IELEM)=((V2+V3*6.D0+V1*5.D0)*X2 +
     &             (-V2-V3*6.D0-V1*5.D0)*X3 +
     &             (-U2-U3*6.D0-U1*5.D0)*Y2 +
     &             (U1*5.D0+U3*6.D0+U2)*Y3) * XSU360
        A32(IELEM)=((V2*5.D0+V3*6.D0+V1)*X3 +
     &             (-U1-U3*6.D0-U2*5.D0)*Y3) * XSU360
        A34(IELEM)=((V2*2.D0+V1)*X2+(-V2+V1)*X3+(-U1-U2*2.D0)*Y2+
     &             (U2-U1)*Y3)*XSUR90
        A35(IELEM)=((-V2*2.D0-V1)*X2-V3*X3*6.D0 +
     &              ( U2*2.D0+U1)*Y2+U3*Y3*6.D0) * XSUR90
        A36(IELEM)=((-V3*6.D0-V2-V1*2.D0)*X2+V3*X3*6.D0 +
     &              ( U2+U3*6.D0+U1*2.D0)*Y2-U3*Y3*6.D0) * XSUR90
!
        A41(IELEM)=((-V2*2.D0-V3-V1*6.D0)*X2 +
     &              ( V2*2.D0+V3+V1*6.D0)*X3 +
     &              ( U1*6.D0+U3+U2*2.D0)*Y2 +
     &              (-U1*6.D0-U3-U2*2.D0)*Y3) * XSUR90
        A42(IELEM)=((-V2*6.D0-V3-V1*2.D0)*X3 +
     &              ( U1*2.D0+U3+U2*6.D0)*Y3) * XSUR90
        A43(IELEM)=((-V2*2.D0+V3-V1*2.D0)*X2 +
     &              ( U1*2.D0-U3+U2*2.D0)*Y2) * XSUR90
        A45(IELEM)=(( 6.D0*V2+2.D0*V3+4.D0*V1)*X2 +
     &              (-2.D0*V2-2.D0*V3-2.D0*V1)*X3 +
     &              (-6.D0*U2-2.D0*U3-4.D0*U1)*Y2 +
     &              (2.D0*U1+2.D0*U3+2.D0*U2)*Y3) * XSUR45
        A46(IELEM)=((2.D0*V2+4.D0*V1)*X2 +
     &              (2.D0*V3+2.D0*V2+2.D0*V1)*X3 +
     &              (-4.D0*U1-2.D0*U2)*Y2 +
     &              (-2.D0*U2-2.D0*U3-2.D0*U1)*Y3) * XSUR45
!
        A51(IELEM)=((V2*2.D0+V3*2.D0-V1)*X2 +
     &             (-V2*2.D0-V3*2.D0+V1)*X3 +
     &             (-U2*2.D0-U3*2.D0+U1)*Y2 +
     &             (-U1+U3*2.D0+U2*2.D0)*Y3) * XSUR90
        A52(IELEM)=((-V2*6.D0-V3*2.D0-V1)*X3 +
     &             (U1+U3*2.D0+U2*6.D0)*Y3) * XSUR90
        A53(IELEM)=((V2*2.D0+V3*6.D0+V1)*X2 +
     &             (-U1-U3*6.D0-U2*2.D0)*Y2) * XSUR90
        A54(IELEM)=((-6.D0*V2-4.D0*V3-2.D0*V1)*X2 +
     &             (4.D0*V2+2.D0*V3)*X3 +
     &             (6.D0*U2+4.D0*U3+2.D0*U1)*Y2 +
     &             (-4.D0*U2-2.D0*U3)*Y3) * XSUR45
        A56(IELEM)=((-2.D0*V2-4.D0*V3)*X2 +
     &             (4.D0*V2+6.D0*V3+2.D0*V1)*X3 +
     &             (2.D0*U2+4.D0*U3)*Y2 +
     &             (-2.D0*U1-6.D0*U3-4.D0*U2)*Y3) * XSUR45
!
        A61(IELEM)=((-V2-V3*2.D0-V1*6.D0)*X2 +
     &             (V2+V3*2.D0+V1*6.D0)*X3 +
     &             (U2+U3*2.D0+U1*6.D0)*Y2 +
     &             (-U2-U3*2.D0-U1*6.D0)*Y3) * XSUR90
        A62(IELEM)=((-V2+V3*2.D0+V1*2.D0)*X3 +
     &             (-U1*2.D0-U3*2.D0+U2)*Y3) * XSUR90
        A63(IELEM)=((V2+V3*6.D0+V1*2.D0)*X2 +
     &             (-U1*2.D0-U3*6.D0-U2)*Y2) * XSUR90
        A64(IELEM)=((-2.D0*V2-2.D0*V3-2.D0*V1)*X2 +
     &             (-4.D0*V1-2.D0*V3)*X3 +
     &             (2.D0*U1+2.D0*U3+2.D0*U2)*Y2 +
     &             (4.D0*U1+2.D0*U3)*Y3) * XSUR45
        A65(IELEM)=((2.D0*V3+2.D0*V2+2.D0*V1)*X2 +
     &             (-2.D0*V2-6.D0*V3-4.D0*V1)*X3 +
     &             (-2.D0*U2-2.D0*U3-2.D0*U1)*Y2 +
     &             (4.D0*U1+6.D0*U3+2.D0*U2)*Y3) * XSUR45
!
!  DIAGONAL TERMS:
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &               - A15(IELEM) - A16(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
     &               - A25(IELEM) - A26(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
     &               - A35(IELEM) - A36(IELEM)
        A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
     &               - A45(IELEM) - A46(IELEM)
        A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &               - A54(IELEM) - A56(IELEM)
        A66(IELEM) = - A61(IELEM) - A62(IELEM) - A63(IELEM)
     &               - A64(IELEM) - A65(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!     CASE WHERE U AND V ARE LINEAR
!
      ELSEIF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
!-----------------------------------------------------------------------
!
!  P1 DISCRETISATION OF THE VELOCITY:
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)=(( 5.D0*V2+V3+V1*6.D0)*X3 +
     &             ( -U1*6.D0-U3-U2*5.D0)*Y3) * XSU360
        A13(IELEM)=((-V2-V3*5.D0-V1*6.D0)*X2 +
     &             (  U1*6.D0+U3*5.D0+U2)*Y2) * XSU360
        A14(IELEM)=(( V2*2.D0+V3)*X2+(-V2*2.D0-V3-V1*6.D0)*X3 +
     &             ( -U2*2.D0-U3)*Y2+(U1*6.D0+U3+U2*2.D0)*Y3) * XSUR90
        A15(IELEM)=((-V3-V2*2.D0)*X2+(V2+V3*2.D0)*X3 +
     &             (U2*2.D0+U3)*Y2+(-U2-U3*2.D0)*Y3) * XSUR90
        A16(IELEM)=((V2+V3*2.D0+V1*6.D0)*X2+(-V2-V3*2.D0)*X3 +
     &             (-U2-U3*2.D0-U1*6.D0)*Y2+(U2+U3*2.D0)*Y3) * XSUR90
!
        A21(IELEM)=((V2*6.D0+V3+V1*5.D0)*X2 +
     &             (-V2*6.D0-V3-V1*5.D0)*X3 +
     &             (-U2*6.D0-U3-U1*5.D0)*Y2 +
     &             (U1*5.D0+U3+U2*6.D0)*Y3) * XSU360
        A23(IELEM)=((-V2*6.D0-V3*5.D0-V1)*X2 +
     &             (  U1+U3*5.D0+U2*6.D0)*Y2) * XSU360
        A24(IELEM)=(-V2*X2*6.D0+(V2*6.D0+V3+V1*2.D0)*X3+U2*Y2*6.D0 +
     &             (-U2*6.D0-U3-U1*2.D0)*Y3) * XSUR90
        A25(IELEM)=(V2*X2*6.D0+(V3*2.D0+V1)*X3-U2*Y2*6.D0 +
     &             (-U3*2.D0-U1)*Y3) * XSUR90
        A26(IELEM)=((V3-V1)*X2+(-V3*2.D0-V1)*X3+(-U3+U1)*Y2 +
     &             (U1+U3*2.D0)*Y3) * XSUR90
!
        A31(IELEM)=((V2+V3*6.D0+V1*5.D0)*X2 +
     &             (-V2-V3*6.D0-V1*5.D0)*X3 +
     &             (-U2-U3*6.D0-U1*5.D0)*Y2 +
     &             (U1*5.D0+U3*6.D0+U2)*Y3) * XSU360
        A32(IELEM)=((V2*5.D0+V3*6.D0+V1)*X3 +
     &             (-U1-U3*6.D0-U2*5.D0)*Y3) * XSU360
        A34(IELEM)=((V2*2.D0+V1)*X2+(-V2+V1)*X3+(-U1-U2*2.D0)*Y2+
     &             (U2-U1)*Y3)*XSUR90
        A35(IELEM)=((-V2*2.D0-V1)*X2-V3*X3*6.D0 +
     &              ( U2*2.D0+U1)*Y2+U3*Y3*6.D0) * XSUR90
        A36(IELEM)=((-V3*6.D0-V2-V1*2.D0)*X2+V3*X3*6.D0 +
     &              ( U2+U3*6.D0+U1*2.D0)*Y2-U3*Y3*6.D0) * XSUR90
!
        A41(IELEM)=((-V2*2.D0-V3-V1*6.D0)*X2 +
     &              ( V2*2.D0+V3+V1*6.D0)*X3 +
     &              ( U1*6.D0+U3+U2*2.D0)*Y2 +
     &              (-U1*6.D0-U3-U2*2.D0)*Y3) * XSUR90
        A42(IELEM)=((-V2*6.D0-V3-V1*2.D0)*X3 +
     &              ( U1*2.D0+U3+U2*6.D0)*Y3) * XSUR90
        A43(IELEM)=((-V2*2.D0+V3-V1*2.D0)*X2 +
     &              ( U1*2.D0-U3+U2*2.D0)*Y2) * XSUR90
        A45(IELEM)=(( 6.D0*V2+2.D0*V3+4.D0*V1)*X2 +
     &              (-2.D0*V2-2.D0*V3-2.D0*V1)*X3 +
     &              (-6.D0*U2-2.D0*U3-4.D0*U1)*Y2 +
     &              (2.D0*U1+2.D0*U3+2.D0*U2)*Y3) * XSUR45
        A46(IELEM)=((2.D0*V2+4.D0*V1)*X2 +
     &              (2.D0*V3+2.D0*V2+2.D0*V1)*X3 +
     &              (-4.D0*U1-2.D0*U2)*Y2 +
     &              (-2.D0*U2-2.D0*U3-2.D0*U1)*Y3) * XSUR45
!
        A51(IELEM)=((V2*2.D0+V3*2.D0-V1)*X2 +
     &             (-V2*2.D0-V3*2.D0+V1)*X3 +
     &             (-U2*2.D0-U3*2.D0+U1)*Y2 +
     &             (-U1+U3*2.D0+U2*2.D0)*Y3) * XSUR90
        A52(IELEM)=((-V2*6.D0-V3*2.D0-V1)*X3 +
     &             (U1+U3*2.D0+U2*6.D0)*Y3) * XSUR90
        A53(IELEM)=((V2*2.D0+V3*6.D0+V1)*X2 +
     &             (-U1-U3*6.D0-U2*2.D0)*Y2) * XSUR90
        A54(IELEM)=((-6.D0*V2-4.D0*V3-2.D0*V1)*X2 +
     &             (4.D0*V2+2.D0*V3)*X3 +
     &             (6.D0*U2+4.D0*U3+2.D0*U1)*Y2 +
     &             (-4.D0*U2-2.D0*U3)*Y3) * XSUR45
        A56(IELEM)=((-2.D0*V2-4.D0*V3)*X2 +
     &             (4.D0*V2+6.D0*V3+2.D0*V1)*X3 +
     &             (2.D0*U2+4.D0*U3)*Y2 +
     &             (-2.D0*U1-6.D0*U3-4.D0*U2)*Y3) * XSUR45
!
        A61(IELEM)=((-V2-V3*2.D0-V1*6.D0)*X2 +
     &             (V2+V3*2.D0+V1*6.D0)*X3 +
     &             (U2+U3*2.D0+U1*6.D0)*Y2 +
     &             (-U2-U3*2.D0-U1*6.D0)*Y3) * XSUR90
        A62(IELEM)=((-V2+V3*2.D0+V1*2.D0)*X3 +
     &             (-U1*2.D0-U3*2.D0+U2)*Y3) * XSUR90
        A63(IELEM)=((V2+V3*6.D0+V1*2.D0)*X2 +
     &             (-U1*2.D0-U3*6.D0-U2)*Y2) * XSUR90
        A64(IELEM)=((-2.D0*V2-2.D0*V3-2.D0*V1)*X2 +
     &             (-4.D0*V1-2.D0*V3)*X3 +
     &             (2.D0*U1+2.D0*U3+2.D0*U2)*Y2 +
     &             (4.D0*U1+2.D0*U3)*Y3) * XSUR45
        A65(IELEM)=((2.D0*V3+2.D0*V2+2.D0*V1)*X2 +
     &             (-2.D0*V2-6.D0*V3-4.D0*V1)*X3 +
     &             (-2.D0*U2-2.D0*U3-2.D0*U1)*Y2 +
     &             (4.D0*U1+6.D0*U3+2.D0*U2)*Y3) * XSUR45
!
!  DIAGONAL TERMS:
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &               - A15(IELEM) - A16(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
     &               - A25(IELEM) - A26(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
     &               - A35(IELEM) - A36(IELEM)
        A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
     &               - A45(IELEM) - A46(IELEM)
        A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &               - A54(IELEM) - A56(IELEM)
        A66(IELEM) = - A61(IELEM) - A62(IELEM) - A63(IELEM)
     &               - A64(IELEM) - A65(IELEM)
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.13.AND.IELMV.EQ.13) THEN
!
!-----------------------------------------------------------------------
!
!  P2 DISCRETISATION OF THE VELOCITY:
!
      IF(FORMUL(16:16).EQ.'N') THEN
!
!  N SCHEME
!
        WRITE(LU,13)
        CALL PLANTE(1)
      ELSE
!
      DO IELEM = 1 , NELEM
!
!  TRADITIONAL METHOD
!
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        U4 = U(IKLE4(IELEM))
        U5 = U(IKLE5(IELEM))
        U6 = U(IKLE6(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
        V4 = V(IKLE4(IELEM))
        V5 = V(IKLE5(IELEM))
        V6 = V(IKLE6(IELEM))
!
!  EXTRADIAGONAL TERMS
!
      A12(IELEM)= ((V5*20.D0+4.D0*8.D0*V4-11.D0*V3+16.D0*V6 +
     &             V1*18.D0+V2*9.D0)*X3+(-16.D0*U6-U5*20.D0+
     &             11.D0*U3-32.D0*U4-U1*18.D0-U2*9.D0)*Y3) * XSUR2520
      A13(IELEM)= ((-16.D0*V4-V5*20.D0-4.D0*8.D0*V6-V3*9.D0 -
     &             V1*18.D0+11.D0*V2)*X2+(U1*18.D0+U5*20.D0 +
     &             U3*9.D0+32.D0*U6+16.D0*U4-11.D0*U2)*Y2) * XSUR2520
      A14(IELEM)= ((-V3+8.D0*V4+4.D0*V6-V1*6.D0+12.D0*V5       +
     &              2.D0*2.D0*V2)*X2+(-20.D0*V4+V3*5.D0-16.D0*V6-
     &              24.D0*V1-8.D0*V5)*X3+(-4.D0*U2-8.D0*U4-12.D0*U5+
     &              U3+U1*6.D0-4.D0*U6)*Y2+(16.D0*U6+20.D0*U4+
     &              24.D0*U1-U3*5.D0+4.D0*2.D0*U5)*Y3) * XSUR630
      A15(IELEM)= ((V3-12.D0*V5-4.D0*V6+V1*6.D0-8.D0*V4-4.D0*V2)*X2 +
     &             (4.D0*V4-V1*6.D0-V2+12.D0*V5+4.D0*V3+8.D0*V6)*X3 +
     &             (4.D0*U2+12.D0*U5-U1*6.D0+8.D0*U4+4.D0*U6-U3)*Y2 +
     &             (-8.D0*U6-4.D0*U4+U1*6.D0+U2-4.D0*U3-12.D0*U5)*Y3)
     &             * XSUR630
      A16(IELEM)= (( 16.D0*V4+20.D0*V6+24.D0*V1+8.D0*V5-V2*5.D0)*X2 +
     &             (-8.D0*V6-4.D0*V4+V1*6.D0-4.D0*V3-12.D0*V5+V2)*X3+
     &             (-24.D0*U1-8.D0*U5+U2*5.D0-20.D0*U6-16.D0*U4)*Y2 +
     &             (8.D0*U6+4.D0*U4-U1*6.D0+4.D0*U3+12.D0*U5-U2)*Y3)
     &             * XSUR630
!
      A21(IELEM)= ((-11.D0*V3+2.D0*8.D0*V5+V6*20.D0+V1*9.D0+32.D0*V4+
     &             V2*18.D0)*X2+(-4.D0*8.D0*V4-V1*9.D0-V2*18.D0-
     &             2.D0*8.D0*V5+11.D0*V3-V6*20.D0)*X3+(-U2*18.D0-
     &             2.D0*8.D0*U5-U1*9.D0-32.D0*U4-U6*20.D0+11.D0*U3)*Y2+
     &             (U6*20.D0+4.D0*8.D0*U4+U1*9.D0+U2*18.D0-11.D0*U3+
     &             2.D0*8.D0*U5)*Y3) * XSUR2520
      A23(IELEM)= ((-2.D0*8.D0*V4-4.D0*8.D0*V5-V6*20.D0-V3*9.D0 +
     &             11.D0*V1-V2*18.D0)*X2+(-11.D0*U1+4.D0*8.D0*U5 +
     &             U3*9.D0+U6*20.D0+2.D0*8.D0*U4+U2*18.D0)*Y2)
     &             *XSUR2520
      A24(IELEM)=((4.D0*V3-12.D0*V4+4.D0*V1+4.D0*V6-12.D0*V5-
     &            V2*30.D0)*X2+(20.D0*V4-V3*5.D0+8.D0*V6+24.D0*V2+
     &            16.D0*V5)*X3+(U2*30.D0+12.D0*U4+12.D0*U5-4.D0*U3-
     &            4.D0*U1-4.D0*U6)*Y2+(-4.D0*2.D0*U6-20.D0*U4-24.D0*U2+
     &            U3*5.D0-16.D0*U5)*Y3)*XSUR630
      A25(IELEM)=((-4.D0*V3+12.D0*V5-4.D0*V6-4.D0*V1+12.D0*V4+
     &            V2*30.D0)*X2+(4.D0*V4-V1-V2*6.D0+8.D0*V5+4.D0*V3+
     &            12.D0*V6)*X3+(-U2*30.D0-12.D0*U5+4.D0*U1-12.D0*U4+
     &            4.D0*U6+4.D0*U3)*Y2+(-12.D0*U6-4.D0*U4+U1+U2*6.D0-
     &            4.D0*U3-8.D0*U5)*Y3)*XSUR630
      A26(IELEM)=((4.D0*V5-4.D0*V4-V1*5.D0+V3*5.D0)*X2+(-12.D0*V6+
     &            V2*6.D0-4.D0*V4+V1-4.D0*V3-8.D0*V5)*X3+(-4.D0*U5-
     &            U3*5.D0+U1*5.D0+4.D0*U4)*Y2+(12.D0*U6+4.D0*U4-U2*6.D0+
     &            8.D0*U5+4.D0*U3-U1)*Y3)*XSUR630
!
      A31(IELEM)=((V3*18.D0+16.D0*V5+32.D0*V6+V1*9.D0+V4*20.D0-
     &            11.D0*V2)*X2+(-V4*20.D0-V1*9.D0+11.D0*V2-2.D0*8.D0*V5-
     &            V3*18.D0-4.D0*8.D0*V6)*X3+(11.D0*U2-2.D0*8.D0*U5-
     &            U1*9.D0-U4*20.D0-4.D0*8.D0*U6-U3*18.D0)*Y2+
     &            (4.D0*8.D0*U6+U4*20.D0+U1*9.D0-11.D0*U2+U3*18.D0+
     &            16.D0*U5)*Y3)*XSUR2520
      A32(IELEM)=((32.D0*V5+V4*20.D0+V3*18.D0+16.D0*V6-11.D0*V1+
     &            V2*9.D0)*X3+(-16.D0*U6-32.D0*U5-U3*18.D0-U4*20.D0+
     &            11.D0*U1-U2*9.D0)*Y3)*XSUR2520
      A34(IELEM)=((-V3*6.D0+8.D0*V5+4.D0*V6+12.D0*V4-V1+4.D0*V2)*X2+
     &           (-V2*5.D0-4.D0*V5+V1*5.D0+4.D0*V6)*X3+(U1-8.D0*U5+
     &           U3*6.D0-4.D0*U6-4.D0*U2-12.D0*U4)*Y2+(-4.D0*U6+U2*5.D0+
     &           4.D0*U5-U1*5.D0)*Y3)*XSUR630
      A35(IELEM)=((V3*6.D0-8.D0*V5-4.D0*V6+V1-12.D0*V4-4.D0*V2)*X2+
     &           (4.D0*V4+4.D0*V1+4.D0*V2-12.D0*V5-V3*30.D0-
     &           12.D0*V6)*X3+(4.D0*U2+8.D0*U5-U1+12.D0*U4+4.D0*U6-
     &           U3*6.D0)*Y2+(12.D0*U6-4.D0*U4-4.D0*U1-4.D0*U2+U3*30.D0+
     &           12.D0*U5)*Y3)*XSUR630
      A36(IELEM)=((-24.D0*V3-8.D0*V4-20.D0*V6-16.D0*V5+V2*5.D0)*X2+
     &           (-4.D0*V4+V3*30.D0+12.D0*V6-4.D0*V2+12.D0*V5-
     &           4.D0*V1)*X3+(-U2*5.D0+16.D0*U5+24.D0*U3+8.D0*U4+
     &           20.D0*U6)*Y2+(-12.D0*U6+4.D0*U4+4.D0*U2-U3*30.D0+
     &           4.D0*U1-12.D0*U5)*Y3)*XSUR630
!
      A41(IELEM)=(-U3*Y2*5.D0-20.D0*V6*X2-V3*X3*5.D0+12.*V1*X3-
     &           12.D0*V1*X2-12.D0*U1*Y3+12.D0*U1*Y2-20.D0*U6*Y3+
     &           V3*X2*5.D0+U3*Y3*5.D0+20.D0*V6*X3+20.*U6*Y2-
     &           40.D0*U4*Y3+40.*V4*X3-4.D0*U5*Y3+4.D0*V5*X3+
     &           40.D0*U4*Y2+4.D0*U5*Y2-4.D0*V5*X2-40.*V4*X2-
     &           8.D0*V2*X3+8.D0*U2*Y3+8.D0*V2*X2-
     &           8.D0*U2*Y2)*XSUR630
      A42(IELEM)=((-20.D0*V5-40.D0*V4+5.D0*V3-4.D0*V6+8.D0*V1-
     &           12.D0*V2)*X3+(4.D0*U6+20.D0*U5-5.D0*U3+40.D0*U4-
     &           8.D0*U1+12.D0*U2)*Y3) * XSUR630
      A43(IELEM)=((-24.D0*V4+4.D0*V5+4.D0*V6+3.D0*V3-4.D0*V1-
     &           4.D0*V2)*X2+(4.D0*U1-4.D0*U5-3.D0*U3-4.D0*U6+
     &           24.D0*U4+4.D0*U2)*Y2) * XSUR630
      A45(IELEM)=(12.D0*U3*Y2+32.D0*V6*X2+4.D0*V3*X3+4.D0*V1*X3-
     &           8.D0*V1*X2-4.D0*U1*Y3+8.D0*U1*Y2+32.D0*U6*Y3-
     &           12.D0*V3*X2-4.D0*U3*Y3-32.D0*V6*X3-32.D0*U6*Y2+
     &           32.D0*U4*Y3-32.D0*V4*X3+32.D0*U5*Y3-32.D0*V5*X3-
     &           96.D0*U4*Y2-48.D0*U5*Y2+48.D0*V5*X2+96.D0*V4*X2+
     &           4.D0*V2*X3-4.D0*U2*Y3+12.D0*V2*X2-12.D0*U2*Y2)
     &           * XSUR630
      A46(IELEM)=((-8.D0*V3+16.D0*V6+16.D0*V1+64.D0*V4-4.D0*V2)*X2+
     &           (-4.D0*V1+32.D0*V4-4.D0*V3+32.D0*V6+32.D0*V5-
     &           4.D0*V2)*X3+(8.D0*U3-16.D0*U1+4.D0*U2-16.D0*U6-
     &           64.D0*U4)*Y2+(-32.D0*U6+4.D0*U1-32.D0*U5+4.D0*U3-
     &           32.D0*U4+4.D0*U2)*Y3)*XSUR630
!
      A51(IELEM)=((4.D0*V3+24.D0*V5-4.D0*V6-V1*3.D0-4.D0*V4+
     &           4.D0*V2)*X2+(4.D0*V4+V1*3.D0-4.D0*V2-24.D0*V5-
     &           4.D0*V3+4.D0*V6)*X3+(-4.D0*U2-24.D0*U5+U1*3.D0+
     &           4.D0*U4+4.D0*U6-4.D0*U3)*Y2+(-4.D0*U6-4.D0*U4-U1*3.D0+
     &           4.D0*U2+4.D0*U3+24.D0*U5)*Y3)*XSUR630
      A52(IELEM)=((-40.D0*V5-20.D0*V4+8.D0*V3-4.D0*V6+V1*5.D0-
     &            12.D0*V2)*X3+(4.D0*U6+40.D0*U5-8.D0*U3+
     &            20.D0*U4-U1*5.D0+12.D0*U2)*Y3)*XSUR630
      A53(IELEM)=((4.D0*V4+40.D0*V5+20.D0*V6+2.D0*6.D0*V3-V1*5.D0-
     &           8.D0*V2)*X2+(U1*5.D0-40.D0*U5-12.D0*U3-20.D0*U6-
     &           4.D0*U4+8.D0*U2)*Y2)*XSUR630
      A54(IELEM)=((8.D0*V3-96.D0*V5-32.D0*V6+12.D0*V1-48.D0*V4-
     &           12.D0*V2)*X2+(16.D0*V4-8.D0*V1+16.D0*V2+64.D0*V5-
     &           4.D0*V3)*X3+(-12.D0*U1+96.D0*U5-8.D0*U3+12.D0*U2+
     &           32.D0*U6+48.D0*U4)*Y2+(8.D0*U1+4.D0*U3-16.D0*U4-
     &           64.D0*U5-16.D0*U2)*Y3)*XSUR630
      A56(IELEM)=((-16.D0*V3-16.D0*V6+8.D0*V1-64.D0*V5+4.D0*V2)*X2+
     &           (32.D0*V4+12.D0*V3-12.D0*V1+48.D0*V6+96.D0*V5-
     &           8.D0*V2)*X3+(-8.D0*U1-4.D0*U2+16.D0*U3+16.D0*U6+
     &           64.D0*U5)*Y2+(-48.D0*U6+12.D0*U1+8.D0*U2-12.D0*U3-
     &           32.D0*U4-96.D0*U5)*Y3)*XSUR630
!
      A61(IELEM)=((8.D0*V3-4.D0*V5-40.D0*V6-12.D0*V1-20.D0*V4+
     &           V2*5.D0)*X2+(20.D0*V4+12.D0*V1-V2*5.D0+4.D0*V5-
     &           8.D0*V3+40.D0*V6)*X3+(-U2*5.D0+4.D0*U5+12.D0*U1+
     &           20.D0*U4+40.D0*U6-8.D0*U3)*Y2+(-40.D0*U6-20.D0*U4-
     &           12.D0*U1+U2*5.D0+8.D0*U3-4.D0*U5)*Y3)*XSUR630
      A62(IELEM)=((-4.D0*V5-4.D0*V4+4.D0*V3+24.D0*V6+4.D0*V1-
     &           V2*3.D0)*X3+(-24.D0*U6+4.D0*U5-4.D0*U3+4.D0*U4-
     &           4.D0*U1+U2*3.D0)*Y3)*XSUR630
      A63(IELEM)=((4.D0*V4+20.D0*V5+40.D0*V6+12.D0*V3-8.D0*V1-
     &           V2*5.D0)*X2+(8.D0*U1-20.D0*U5-12.D0*U3-40.D0*U6-
     &           4.D0*U4+U2*5.D0)*Y2)*XSUR630
      A64(IELEM)=((-32.D0*V4+4.D0*V1+4.D0*V2-32.D0*V5+4.D0*V3-
     &           32.D0*V6)*X2+(-16.D0*V4-16.D0*V1+8.D0*V2+4.D0*V3-
     &           64.D0*V6)*X3+(32.D0*U6+32.D0*U4-4.D0*U1-4.D0*U2-
     &           4.D0*U3+32.D0*U5)*Y2+(64.D0*U6-8.D0*U2+16.D0*U1-
     &           4.D0*U3+16.D0*U4)*Y3)*XSUR630
      A65(IELEM)=((-4.D0*V1+32.D0*V4-4.D0*V3+32.D0*V6+32.D0*V5-
     &           4.D0*V2)*X2+(-32.D0*V4+8.D0*V1+12.D0*V2-48.D0*V5-
     &           12.D0*V3-96.D0*V6)*X3+(-32.D0*U6+4.D0*U1-32.D0*U5+
     &           4.D0*U3-32.D0*U4+4.D0*U2)*Y2+(96.D0*U6+32.D0*U4-
     &           8.D0*U1-12.D0*U2+12.D0*U3+48.D0*U5)*Y3)*XSUR630
!
!  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &               - A15(IELEM) - A16(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
     &               - A25(IELEM) - A26(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
     &               - A35(IELEM) - A36(IELEM)
        A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
     &               - A45(IELEM) - A46(IELEM)
        A55(IELEM) = - A51(IELEM) - A52(IELEM) - A53(IELEM)
     &               - A54(IELEM) - A56(IELEM)
        A66(IELEM) = - A61(IELEM) - A62(IELEM) - A63(IELEM)
     &               - A64(IELEM) - A65(IELEM)
!
      ENDDO ! IELEM
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,11) IELMU,IELMV
11    FORMAT(1X,
     & 'MT05CC (BIEF) : TYPES OF VELOCITIES NOT AVAILABLE : ',2I6)
13    FORMAT(1X,
     & 'MT05CC (BIEF) : N SCHEMES NOT AVAILABLE  ')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
