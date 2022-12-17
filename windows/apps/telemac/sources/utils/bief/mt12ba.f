!                   *****************
                    SUBROUTINE MT12BA
!                   *****************
!
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   A41 , A42 , A43 ,
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
!+  PSI1 : BASES OF TYPE QUASI-BUBBLE
!+  PSI2 : BASES OF TYPE LINEAR
!+  F    : FUNCTION OF TYPE QUASI-BUBBLE TRIANGLE
!+  U    : VECTOR OF TYPE LINEAR OR P0
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
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A41            |<--| ELEMENTS OF MATRIX
!| A42            |<--| ELEMENTS OF MATRIX
!| A43            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| FOURTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE5          |-->| FIFTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE6          |-->| SIXTH POINTS OF TRIANGLES (QUADRATIC)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
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
      USE BIEF, EX_MT12BA => MT12BA
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
!     STRUCTURES OF F, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
!
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION X2,X3,Y2,Y3,F1,F2,F3,F4
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,UX,UY,AUX108,XSU108,AUX036
      DOUBLE PRECISION AX1296,XS1296,XSUR36,AUX432,XSU432
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
      XSUR36 = XMUL /  36.D0
      XSU108 = XMUL / 108.D0
      XSU432 = XMUL / 432.D0
      XS1296 = XMUL /1296.D0
!
!-----------------------------------------------------------------------
!  CASE WHERE F IS OF TYPE P1
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.12) THEN
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
        F4  =  F(IKLE4(IELEM)) - F1
!
        UX  =  U(IELEM)
        UY  =  V(IELEM)
!
        AUX108 = XSU108 / SURFAC(IELEM)
        AUX036 = XSUR36 / SURFAC(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)=((((F3-3*F4)*Y3+Y2*F3)*X2*UY-2*((F3-3*F4)*Y3
     & +Y2*F3)*X3*UY+8*((3*F4-F2)*Y2-Y3*F2)*X2*UY-4*((3*F4-
     & F2)*Y2-Y3*F2)*X3*UY+(Y3*F3-3*Y3*F4+Y2*F3)*(2*Y3-Y2)*UX-
     & 4*(Y3*F2-3*Y2*F4+Y2*F2)*(Y3-2*Y2)*UX))*AUX108
      A13(IELEM)=((4*((F3-3*F4)*Y3+Y2*F3)*X2*UY-8*((F3-3*F4)
     & *Y3+Y2*F3)*X3*UY+2*((3*F4-F2)*Y2-Y3*F2)*X2*UY-((3*F4-
     & F2)*Y2-Y3*F2)*X3*UY+4*(Y3*F3-3*Y3*F4+Y2*F3)*(2*Y3-Y2)*
     & UX-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3-2*Y2)*UX))*AUX108
      A21(IELEM)=(-(((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X2
     & *UY-2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY-4
     & *((3*F4-F2)*Y2-Y3*F2)*X2*UY-4*((3*F4-F2)*Y2-Y3*F2)*X3*
     & UY-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(2*
     & Y3-Y2)*UX-4*(Y3*F2-3*Y2*F4+Y2*F2)*(Y3+Y2)*UX))*AUX108
      A23(IELEM)=(-(4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *X2*UY-8*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY
     & -((3*F4-F2)*Y2-Y3*F2)*X2*UY-((3*F4-F2)*Y2-Y3*F2)*X3*UY-
     & 4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(2*
     & Y3-Y2)*UX-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3+Y2)*UX))*AUX108
      A31(IELEM)=(-(2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *X2*UY-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY+4
     & *((F3-3*F4)*Y3+Y2*F3)*X2*UY+4*((F3-3*F4)*Y3+Y2*F3)*X3*
     & UY-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-
     & 2*Y2)*UX-4*(Y3*F3-3*Y3*F4+Y2*F3)*(Y3+Y2)*UX))*AUX108
      A32(IELEM)=(-(8*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *X2*UY-4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY
     & +((F3-3*F4)*Y3+Y2*F3)*X2*UY+((F3-3*F4)*Y3+Y2*F3)*X3*UY-
     & 4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-
     & 2*Y2)*UX-(Y3*F3-3*Y3*F4+Y2*F3)*(Y3+Y2)*UX))*AUX108
      A41(IELEM)=(-(X2*UY*Y3*F3-3*X2*UY*Y3*F4-2*X2*UY*Y3*F2-2
     & *X2*UY*Y2*F3+15*X2*UY*Y2*F4-5*X2*UY*Y2*F2-5*X3*UY*Y3*
     & F3+15*X3*UY*Y3*F4-2*X3*UY*Y3*F2-2*X3*UY*Y2*F3-3*X3*UY
     & *Y2*F4+X3*UY*Y2*F2+5*UX*Y3**2*F3-15*UX*Y3**2*F4+2*UX*
     & Y3**2*F2+UX*Y3*Y2*F3+6*UX*Y3*Y2*F4+UX*Y3*Y2*F2+2*UX*Y2
     & **2*F3-15*UX*Y2**2*F4+5*UX*Y2**2*F2))*AUX036
      A42(IELEM)=((4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*
     & X2*UY-4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY+
     & ((F3-3*F4)*Y3+Y2*F3)*X3*UY-((F3-3*F4)*Y3+Y2*F3)*UX*Y3-
     & 4*((3*F4-F2)*Y2-Y3*F2)*X2*UY+4*((3*F4-F2)*Y2-Y3*F2)*UX
     & *Y2-4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*
     & (Y3-Y2)*UX))*AUX036
      A43(IELEM)=((4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*
     & X2*UY-4*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*X3*UY+
     & 4*((F3-3*F4)*Y3+Y2*F3)*X3*UY-4*((F3-3*F4)*Y3+Y2*F3)*UX
     & *Y3-((3*F4-F2)*Y2-Y3*F2)*X2*UY+((3*F4-F2)*Y2-Y3*F2)*UX*
     & Y2-4*(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(
     & Y3-Y2)*UX))*AUX036
!
!   DIAGONAL TERMS
!   THE SUM OF EACH COLUMN IS 0
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
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
        F4  =  F(IKLE4(IELEM)) - F1
!
        UX  =  U(IELEM)
        UY  =  V(IELEM)
!
        AUX108 = XSU108 / SURFAC(IELEM)
        AUX036 = XSUR36 / SURFAC(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)=(-(X2**2*UY*F3+24*X2**2*UY*F4-8*X2**2*UY*F2-
     & X2*X3*UY*F3-15*X2*X3*UY*F4-4*X2*X3*UY*F2+2*X2*UX*Y3*F3
     & +12*X2*UX*Y3*F4-4*X2*UX*Y3*F2-X2*UX*Y2*F3-24*X2*UX*Y2*
     & F4+8*X2*UX*Y2*F2-2*X3**2*UY*F3+6*X3**2*UY*F4+4*X3**2*
     & UY*F2+2*X3*UX*Y3*F3-6*X3*UX*Y3*F4-4*X3*UX*Y3*F2-X3*UX*
     & Y2*F3+3*X3*UX*Y2*F4+8*X3*UX*Y2*F2))*AUX108
      A13(IELEM)=(-(4*X2**2*UY*F3+6*X2**2*UY*F4-2*X2**2*UY*F2
     & -4*X2*X3*UY*F3-15*X2*X3*UY*F4-X2*X3*UY*F2+8*X2*UX*Y3*
     & F3+3*X2*UX*Y3*F4-X2*UX*Y3*F2-4*X2*UX*Y2*F3-6*X2*UX*Y2*
     & F4+2*X2*UX*Y2*F2-8*X3**2*UY*F3+24*X3**2*UY*F4+X3**2*UY
     & *F2+8*X3*UX*Y3*F3-24*X3*UX*Y3*F4-X3*UX*Y3*F2-4*X3*UX*
     & Y2*F3+12*X3*UX*Y2*F4+2*X3*UX*Y2*F2))*AUX108
      A21(IELEM)=((2*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*F2
     & -5*X2*X3*UY*F3-3*X2*X3*UY*F4+4*X2*X3*UY*F2+4*X2*UX*Y3
     & *F3+6*X2*UX*Y3*F4-2*X2*UX*Y3*F2-2*X2*UX*Y2*F3+15*X2*
     & UX*Y2*F4-5*X2*UX*Y2*F2+2*X3**2*UY*F3-6*X3**2*UY*F4+8*
     & X3**2*UY*F2-2*X3*UX*Y3*F3+6*X3*UX*Y3*F4-8*X3*UX*Y3*F2+
     & X3*UX*Y2*F3-3*X3*UX*Y2*F4-2*X3*UX*Y2*F2))*AUX108
      A23(IELEM)=((8*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*F2
     & -20*X2*X3*UY*F3+33*X2*X3*UY*F4-14*X2*X3*UY*F2+16*X2*
     & UX*Y3*F3-21*X2*UX*Y3*F4+7*X2*UX*Y3*F2-8*X2*UX*Y2*F3+
     & 15*X2*UX*Y2*F4-5*X2*UX*Y2*F2+8*X3**2*UY*F3-24*X3**2*UY
     & *F4+17*X3**2*UY*F2-8*X3*UX*Y3*F3+24*X3*UX*Y3*F4-17*X3
     & *UX*Y3*F2+4*X3*UX*Y2*F3-12*X3*UX*Y2*F4+7*X3*UX*Y2*F2))*AUX108
      A31(IELEM)=((8*X2**2*UY*F3-6*X2**2*UY*F4+2*X2**2*UY*F2+
     & 4*X2*X3*UY*F3-3*X2*X3*UY*F4-5*X2*X3*UY*F2-2*X2*UX*Y3*
     & F3-3*X2*UX*Y3*F4+X2*UX*Y3*F2-8*X2*UX*Y2*F3+6*X2*UX*Y2*
     & F4-2*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY*F4+2*X3**2
     & *UY*F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-2*X3*UX*Y3*F2-2*
     & X3*UX*Y2*F3+6*X3*UX*Y2*F4+4*X3*UX*Y2*F2))*AUX108
      A32(IELEM)=((17*X2**2*UY*F3-24*X2**2*UY*F4+8*X2**2*UY*
     & F2-14*X2*X3*UY*F3+33*X2*X3*UY*F4-20*X2*X3*UY*F2+7*X2*
     & UX*Y3*F3-12*X2*UX*Y3*F4+4*X2*UX*Y3*F2-17*X2*UX*Y2*F3+
     & 24*X2*UX*Y2*F4-8*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY
     & *F4+8*X3**2*UY*F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-8*X3*
     & UX*Y3*F2+7*X3*UX*Y2*F3-21*X3*UX*Y2*F4+16*X3*UX*Y2*F2))*AUX108
      A41(IELEM)=(-(2*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*
     & F2+X2*X3*UY*F3+6*X2*X3*UY*F4+X2*X3*UY*F2-2*X2*UX*Y3*F3-
     & 3*X2*UX*Y3*F4+X2*UX*Y3*F2-2*X2*UX*Y2*F3+15*X2*UX*Y2*F4-
     & 5*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY*F4+2*X3**2*UY*
     & F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-2*X3*UX*Y3*F2+X3*UX*Y2
     & *F3-3*X3*UX*Y2*F4-2*X3*UX*Y2*F2))*AUX036
      A42(IELEM)=(-(8*X2**2*UY*F3-24*X2**2*UY*F4+8*X2**2*UY*
     & F2-11*X2*X3*UY*F3+24*X2*X3*UY*F4-8*X2*X3*UY*F2+7*X2*
     & UX*Y3*F3-12*X2*UX*Y3*F4+4*X2*UX*Y3*F2-8*X2*UX*Y2*F3+
     & 24*X2*UX*Y2*F4-8*X2*UX*Y2*F2+5*X3**2*UY*F3-15*X3**2*UY
     & *F4+8*X3**2*UY*F2-5*X3*UX*Y3*F3+15*X3*UX*Y3*F4-8*X3*
     & UX*Y3*F2+4*X3*UX*Y2*F3-12*X3*UX*Y2*F4+4*X3*UX*Y2*F2))*AUX036
      A43(IELEM)=(-(8*X2**2*UY*F3-15*X2**2*UY*F4+5*X2**2*UY*
     & F2-8*X2*X3*UY*F3+24*X2*X3*UY*F4-11*X2*X3*UY*F2+4*X2*
     & UX*Y3*F3-12*X2*UX*Y3*F4+4*X2*UX*Y3*F2-8*X2*UX*Y2*F3+
     & 15*X2*UX*Y2*F4-5*X2*UX*Y2*F2+8*X3**2*UY*F3-24*X3**2*UY
     & *F4+8*X3**2*UY*F2-8*X3*UX*Y3*F3+24*X3*UX*Y3*F4-8*X3*
     & UX*Y3*F2+4*X3*UX*Y2*F3-12*X3*UX*Y2*F4+7*X3*UX*Y2*F2))*AUX036
!
!   DIAGONAL TERMS
!   THE SUM OF EACH COLUMN IS 0
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
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
      ELSEIF(IELMU.EQ.11) THEN
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
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
!
        AX1296 = XS1296 / SURFAC(IELEM)
        AUX432 = XSU432 / SURFAC(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)=((((F3-3*F4)*Y3+Y2*F3)*(5*V3+2*V2+5*V1)*X2-
     & 2*((F3-3*F4)*Y3+Y2*F3)*(5*V3+2*V2+5*V1)*X3+2*((3*F4
     & -F2)*Y2-Y3*F2)*(5*V3+26*V2+17*V1)*X2-((3*F4-F2)*Y2-Y3
     & *F2)*(5*V3+26*V2+17*V1)*X3+(Y3*F3-3*Y3*F4+Y2*F3)*(2*
     & Y3-Y2)*(5*U3+2*U2+5*U1)-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3-2*
     & Y2)*(5*U3+26*U2+17*U1)))*AX1296
      A13(IELEM)=((((F3-3*F4)*Y3+Y2*F3)*(26*V3+5*V2+17*V1)*
     & X2-2*((F3-3*F4)*Y3+Y2*F3)*(26*V3+5*V2+17*V1)*X3+2*(
     & (3*F4-F2)*Y2-Y3*F2)*(2*V3+5*V2+5*V1)*X2-((3*F4-F2)*
     & Y2-Y3*F2)*(2*V3+5*V2+5*V1)*X3+(Y3*F3-3*Y3*F4+Y2*F3)*(
     & 2*Y3-Y2)*(26*U3+5*U2+17*U1)-(Y3*F2-3*Y2*F4+Y2*F2)*(Y3
     & -2*Y2)*(2*U3+5*U2+5*U1)))*AX1296
      A21(IELEM)=(-(((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 5*V3+5*V2+2*V1)*X2-2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2
     & *F2)*Y3)*(5*V3+5*V2+2*V1)*X3-((3*F4-F2)*Y2-Y3*F2)*(5
     & *V3+17*V2+26*V1)*X2-((3*F4-F2)*Y2-Y3*F2)*(5*V3+17*V2
     & +26*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2
     & *F2)*(2*Y3-Y2)*(5*U3+5*U2+2*U1)-(Y3*F2-3*Y2*F4+Y2*F2
     & )*(Y3+Y2)*(5*U3+17*U2+26*U1)))*AX1296
      A23(IELEM)=(-(((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 26*V3+17*V2+5*V1)*X2-2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+
     & 2*F2)*Y3)*(26*V3+17*V2+5*V1)*X3-((3*F4-F2)*Y2-Y3*F2)*
     & (2*V3+5*V2+5*V1)*X2-((3*F4-F2)*Y2-Y3*F2)*(2*V3+5*V2
     & +5*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*
     & F2)*(2*Y3-Y2)*(26*U3+17*U2+5*U1)-(Y3*F2-3*Y2*F4+Y2*
     & F2)*(Y3+Y2)*(2*U3+5*U2+5*U1)))*AX1296
      A31(IELEM)=(-(2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *(5*V3+5*V2+2*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2
     & *F2)*Y3)*(5*V3+5*V2+2*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(
     & 17*V3+5*V2+26*V1)*X2+((F3-3*F4)*Y3+Y2*F3)*(17*V3+5*
     & V2+26*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-
     & Y2*F2)*(Y3-2*Y2)*(5*U3+5*U2+2*U1)-(Y3*F3-3*Y3*F4+Y2*
     & F3)*(Y3+Y2)*(17*U3+5*U2+26*U1)))*AX1296
      A32(IELEM)=(-(2*((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)
     & *(17*V3+26*V2+5*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+
     & 2*F2)*Y3)*(17*V3+26*V2+5*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*
     & (5*V3+2*V2+5*V1)*X2+((F3-3*F4)*Y3+Y2*F3)*(5*V3+2*V2
     & +5*V1)*X3-(Y3*F3-3*Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*
     & F2)*(Y3-2*Y2)*(17*U3+26*U2+5*U1)-(Y3*F3-3*Y3*F4+Y2*
     & F3)*(Y3+Y2)*(5*U3+2*U2+5*U1)))*AX1296
      A41(IELEM)=((((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(5
     & *V3+5*V2+2*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)
     & *Y3)*(5*V3+5*V2+2*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(17*V3
     & +5*V2+26*V1)*X3-((F3-3*F4)*Y3+Y2*F3)*(17*U3+5*U2+26
     & *U1)*Y3-((3*F4-F2)*Y2-Y3*F2)*(5*V3+17*V2+26*V1)*X2+((
     & 3*F4-F2)*Y2-Y3*F2)*(5*U3+17*U2+26*U1)*Y2-(Y3*F3-3*Y3*
     & F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-Y2)*(5*U3+5*U2
     & +2*U1)))*AUX432
      A42(IELEM)=((((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 17*V3+26*V2+5*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*
     & F2)*Y3)*(17*V3+26*V2+5*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(
     & 5*V3+2*V2+5*V1)*X3-((F3-3*F4)*Y3+Y2*F3)*(5*U3+2*U2+
     & 5*U1)*Y3-((3*F4-F2)*Y2-Y3*F2)*(5*V3+26*V2+17*V1)*X2+(
     & (3*F4-F2)*Y2-Y3*F2)*(5*U3+26*U2+17*U1)*Y2-(Y3*F3-3*
     & Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-Y2)*(17*U3+
     & 26*U2+5*U1)))*AUX432
      A43(IELEM)=((((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*F2)*Y3)*(
     & 26*V3+17*V2+5*V1)*X2-((2*F3-3*F4+F2)*Y2-(F3-3*F4+2*
     & F2)*Y3)*(26*V3+17*V2+5*V1)*X3+((F3-3*F4)*Y3+Y2*F3)*(
     & 26*V3+5*V2+17*V1)*X3-((F3-3*F4)*Y3+Y2*F3)*(26*U3+5*
     & U2+17*U1)*Y3-((3*F4-F2)*Y2-Y3*F2)*(2*V3+5*V2+5*V1)*
     & X2+((3*F4-F2)*Y2-Y3*F2)*(2*U3+5*U2+5*U1)*Y2-(Y3*F3-3
     & *Y3*F4+2*Y3*F2-2*Y2*F3+3*Y2*F4-Y2*F2)*(Y3-Y2)*(26*U3+
     & 17*U2+5*U1)))*AUX432
!
!   DIAGONAL TERMS
!   THE SUM OF EACH COLUMN IS 0
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
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
        F4  =  F(IKLE4(IELEM)) - F1
!
        U1  =  U(IKLE1(IELEM))
        U2  =  U(IKLE2(IELEM))
        U3  =  U(IKLE3(IELEM))
        V1  =  V(IKLE1(IELEM))
        V2  =  V(IKLE2(IELEM))
        V3  =  V(IKLE3(IELEM))
!
        AX1296 = XS1296 / SURFAC(IELEM)
        AUX432 = XSU432 / SURFAC(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)=(-(((2*Y3-Y2)*(5*U3+2*U2+5*U1)*F3-(F3+3*F4
     & )*(5*V3+2*V2+5*V1)*X3)*X2+((Y3-2*Y2)*(3*F4-F2)*(5*
     & U3+26*U2+17*U1)-(3*F4+F2)*(5*V3+26*V2+17*V1)*X3)*X2
     & +(2*Y3-Y2)*(F3-3*F4)*(5*U3+2*U2+5*U1)*X3-(Y3-2*Y2)*
     & (5*U3+26*U2+17*U1)*X3*F2-2*(F3-3*F4)*(5*V3+2*V2+5
     & *V1)*X3**2+2*(3*F4-F2)*(5*V3+26*V2+17*V1)*X2**2+(5*
     & V3+26*V2+17*V1)*X3**2*F2+(5*V3+2*V2+5*V1)*X2**2*F3))*AX1296
      A13(IELEM)=(-(((2*Y3-Y2)*(26*U3+5*U2+17*U1)*F3-(F3+3*
     & F4)*(26*V3+5*V2+17*V1)*X3)*X2+((Y3-2*Y2)*(3*F4-F2)*(
     & 2*U3+5*U2+5*U1)-(3*F4+F2)*(2*V3+5*V2+5*V1)*X3)*X2+(
     & 2*Y3-Y2)*(F3-3*F4)*(26*U3+5*U2+17*U1)*X3-(Y3-2*Y2)*(
     & 2*U3+5*U2+5*U1)*X3*F2-2*(F3-3*F4)*(26*V3+5*V2+17*
     & V1)*X3**2+2*(3*F4-F2)*(2*V3+5*V2+5*V1)*X2**2+(26*V3
     & +5*V2+17*V1)*X2**2*F3+(2*V3+5*V2+5*V1)*X3**2*F2))*AX1296
      A21(IELEM)=((((2*Y3-Y2)*(2*F3-3*F4+F2)*(5*U3+5*U2+2*
     & U1)-(5*F3-9*F4+4*F2)*(5*V3+5*V2+2*V1)*X3)*X2+((Y3+
     & Y2)*(3*F4-F2)*(5*U3+17*U2+26*U1)-(3*F4-2*F2)*(5*V3
     & +17*V2+26*V1)*X3)*X2-(2*Y3-Y2)*(F3-3*F4+2*F2)*(5*U3
     & +5*U2+2*U1)*X3-(Y3+Y2)*(5*U3+17*U2+26*U1)*X3*F2+(2*
     & F3-3*F4+F2)*(5*V3+5*V2+2*V1)*X2**2+2*(F3-3*F4+2*F2
     & )*(5*V3+5*V2+2*V1)*X3**2-(3*F4-F2)*(5*V3+17*V2+26*
     & V1)*X2**2+(5*V3+17*V2+26*V1)*X3**2*F2))*AX1296
      A23(IELEM)=((((2*Y3-Y2)*(2*F3-3*F4+F2)*(26*U3+17*U2+
     & 5*U1)-(5*F3-9*F4+4*F2)*(26*V3+17*V2+5*V1)*X3)*X2+((
     & Y3+Y2)*(3*F4-F2)*(2*U3+5*U2+5*U1)-(3*F4-2*F2)*(2*
     & V3+5*V2+5*V1)*X3)*X2-(2*Y3-Y2)*(F3-3*F4+2*F2)*(26*
     & U3+17*U2+5*U1)*X3-(Y3+Y2)*(2*U3+5*U2+5*U1)*X3*F2+(2
     & *F3-3*F4+F2)*(26*V3+17*V2+5*V1)*X2**2+2*(F3-3*F4+2
     & *F2)*(26*V3+17*V2+5*V1)*X3**2-(3*F4-F2)*(2*V3+5*V2+
     & 5*V1)*X2**2+(2*V3+5*V2+5*V1)*X3**2*F2))*AX1296
      A31(IELEM)=(-(((Y3+Y2)*(17*U3+5*U2+26*U1)*F3-(2*F3-3*
     & F4)*(17*V3+5*V2+26*V1)*X3)*X2-((Y3-2*Y2)*(2*F3-3*F4
     & +F2)*(5*U3+5*U2+2*U1)-(4*F3-9*F4+5*F2)*(5*V3+5*V2
     & +2*V1)*X3)*X2+(Y3+Y2)*(F3-3*F4)*(17*U3+5*U2+26*U1)*
     & X3+(Y3-2*Y2)*(F3-3*F4+2*F2)*(5*U3+5*U2+2*U1)*X3-2*
     & (2*F3-3*F4+F2)*(5*V3+5*V2+2*V1)*X2**2-(F3-3*F4+2*
     & F2)*(5*V3+5*V2+2*V1)*X3**2-(F3-3*F4)*(17*V3+5*V2+
     & 26*V1)*X3**2-(17*V3+5*V2+26*V1)*X2**2*F3))*AX1296
      A32(IELEM)=(-(((Y3+Y2)*(5*U3+2*U2+5*U1)*F3-(2*F3-3*F4
     & )*(5*V3+2*V2+5*V1)*X3)*X2-((Y3-2*Y2)*(2*F3-3*F4+F2)
     & *(17*U3+26*U2+5*U1)-(4*F3-9*F4+5*F2)*(17*V3+26*V2
     & +5*V1)*X3)*X2+(Y3+Y2)*(F3-3*F4)*(5*U3+2*U2+5*U1)*X3+
     & (Y3-2*Y2)*(F3-3*F4+2*F2)*(17*U3+26*U2+5*U1)*X3-2*(
     & 2*F3-3*F4+F2)*(17*V3+26*V2+5*V1)*X2**2-(F3-3*F4+2*
     & F2)*(17*V3+26*V2+5*V1)*X3**2-(F3-3*F4)*(5*V3+2*V2+
     & 5*V1)*X3**2-(5*V3+2*V2+5*V1)*X2**2*F3))*AX1296
      A41(IELEM)=(-(((Y3-Y2)*(2*F3-3*F4+F2)*(5*U3+5*U2+2*U1
     & )-3*(F3-2*F4+F2)*(5*V3+5*V2+2*V1)*X3)*X2+((3*F4-F2)
     & *(5*U3+17*U2+26*U1)*Y2+(5*V3+17*V2+26*V1)*X3*F2)*X2
     & +((17*V3+5*V2+26*V1)*X3-(17*U3+5*U2+26*U1)*Y3)*X2*
     & F3-(Y3-Y2)*(F3-3*F4+2*F2)*(5*U3+5*U2+2*U1)*X3+(2*F3
     & -3*F4+F2)*(5*V3+5*V2+2*V1)*X2**2+(F3-3*F4+2*F2)*(5
     & *V3+5*V2+2*V1)*X3**2+(F3-3*F4)*(17*V3+5*V2+26*V1)*
     & X3**2-(F3-3*F4)*(17*U3+5*U2+26*U1)*X3*Y3-(3*F4-F2)*(
     & 5*V3+17*V2+26*V1)*X2**2-(5*U3+17*U2+26*U1)*X3*Y2*F2))*AUX432
      A42(IELEM)=(-(((Y3-Y2)*(2*F3-3*F4+F2)*(17*U3+26*U2+5*
     & U1)-3*(F3-2*F4+F2)*(17*V3+26*V2+5*V1)*X3)*X2+((3*F4
     & -F2)*(5*U3+26*U2+17*U1)*Y2+(5*V3+26*V2+17*V1)*X3*F2
     & )*X2+((5*V3+2*V2+5*V1)*X3-(5*U3+2*U2+5*U1)*Y3)*X2*
     & F3-(Y3-Y2)*(F3-3*F4+2*F2)*(17*U3+26*U2+5*U1)*X3+(2*
     & F3-3*F4+F2)*(17*V3+26*V2+5*V1)*X2**2+(F3-3*F4+2*F2)
     & *(17*V3+26*V2+5*V1)*X3**2+(F3-3*F4)*(5*V3+2*V2+5*
     & V1)*X3**2-(F3-3*F4)*(5*U3+2*U2+5*U1)*X3*Y3-(3*F4-F2)
     & *(5*V3+26*V2+17*V1)*X2**2-(5*U3+26*U2+17*U1)*X3*Y2*
     & F2))*AUX432
      A43(IELEM)=(-(((Y3-Y2)*(2*F3-3*F4+F2)*(26*U3+17*U2+5*
     & U1)-3*(F3-2*F4+F2)*(26*V3+17*V2+5*V1)*X3)*X2+((3*F4
     & -F2)*(2*U3+5*U2+5*U1)*Y2+(2*V3+5*V2+5*V1)*X3*F2)*X2
     & +((26*V3+5*V2+17*V1)*X3-(26*U3+5*U2+17*U1)*Y3)*X2*
     & F3-(Y3-Y2)*(F3-3*F4+2*F2)*(26*U3+17*U2+5*U1)*X3+(2*
     & F3-3*F4+F2)*(26*V3+17*V2+5*V1)*X2**2+(F3-3*F4+2*F2)
     & *(26*V3+17*V2+5*V1)*X3**2+(F3-3*F4)*(26*V3+5*V2+17
     & *V1)*X3**2-(F3-3*F4)*(26*U3+5*U2+17*U1)*X3*Y3-(3*F4-
     & F2)*(2*V3+5*V2+5*V1)*X2**2-(2*U3+5*U2+5*U1)*X3*Y2*F2))*AUX432
!
!   DIAGONAL TERMS
!   THE SUM OF EACH COLUMN IS 0
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A43(IELEM)
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
101     FORMAT(1X,'MT12BA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
201       FORMAT(1X,'MT12BA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
!
301    FORMAT(1X,'MT12BA (BIEF) :',/,
     &        1X,'DISCRETIZATION OF U : ',1I6,' NOT AVAILABLE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
