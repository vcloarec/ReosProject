!                   *****************
                    SUBROUTINE MT04BB
!                   *****************
!
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SU,SV,U,V,XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE SUPG MATRIX:
!code
!+            ->--->        ->--->
!+           (U.GRAD(PI))* (U.GRAD(PJ))  WITH
!+
!+          PI OF QUASI-BUBBLE DISCRETISATION
!+          PJ OF QUASI-BUBBLE DISCRETISATION
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        10/01/95
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
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| A44            |<--| ELEMENTS OF MATRIX
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT
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
      USE BIEF, EX_MT04BB => MT04BB
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) ::                      A44(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!     STRUCTURES OF      U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMU,IELMV,IELEM
!
      DOUBLE PRECISION X2,X3,Y2,Y3,ANS1,ANS2
      DOUBLE PRECISION U1,U2,U3,U4
      DOUBLE PRECISION V1,V2,V3,V4
!
!=======================================================================
!
!     EXTRACTS THE TYPE OF ELEMENT FOR VELOCITY
!
      IELMU = SU%ELM
      IELMV = SV%ELM
!
!-----------------------------------------------------------------------
!
      IF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
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
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
!
!  COMPUTES 6 OF THE 16 TERMS (SELECTED AMONG THE LEAST COMPLEX)
!
        A11(IELEM)=
     &  (X2**2*(17*V3**2+25*V3*V2+37*V3*V1+53*V2**2
     &  +73*V2*V1+65*V1**2)+8*X2*X3*(-7*V3**2-5*V3*V2-11*V3
     &  *V1-7*V2**2-11*V2*V1-13*V1**2)+X2*Y2*(-34*V3*U3-25*
     &  V3*U2-37*V3*U1-25*V2*U3-106*V2*U2-73*V2*U1-37*V1*U3-
     &  73*V1*U2-130*V1*U1)+4*X2*Y3*(14*V3*U3+5*V3*U2+11*V3*
     &  U1+5*V2*U3+14*V2*U2+11*V2*U1+11*V1*U3+11*V1*U2+26*
     &  V1*U1)+X3**2*(53*V3**2+25*V3*V2+73*V3*V1+17*V2**2+37
     &  *V2*V1+65*V1**2)+4*X3*Y2*(14*V3*U3+5*V3*U2+11*V3*U1+
     &  5*V2*U3+14*V2*U2+11*V2*U1+11*V1*U3+11*V1*U2+26*V1*U1
     &  )+X3*Y3*(-106*V3*U3-25*V3*U2-73*V3*U1-25*V2*U3-34*V2
     &  *U2-37*V2*U1-73*V1*U3-37*V1*U2-130*V1*U1)+Y2**2*(17*
     &  U3**2+25*U3*U2+37*U3*U1+53*U2**2+73*U2*U1+65*U1**2)+
     &  8*Y2*Y3*(-7*U3**2-5*U3*U2-11*U3*U1-7*U2**2-11*U2*U1-
     &  13*U1**2)+Y3**2*(53*U3**2+25*U3*U2+73*U3*U1+17*U2**2+
     &  37*U2*U1+65*U1**2))*XMUL/(324*(X2*Y3-X3*Y2))
!
        A12(IELEM)=
     &  (4*X2**2*(5*(V2+V1)*V3+V3**2+13*V2**2+17*V2
     &  *V1+13*V1**2)+X2*(2*(5*(V2+V1)*V3+V3**2+13*V2**2+17*
     &  V2*V1+13*V1**2)*X3-(5*U3+26*U2+17*U1)*(Y3+4*Y2)*V2-(
     &  5*U3+17*U2+26*U1)*(Y3+4*Y2)*V1-(2*U3+5*U2+5*U1)*(Y3
     &  +4*Y2)*V3)-2*X3**2*(5*(V2+V1)*V3+V3**2+13*V2**2+17*
     &  V2*V1+13*V1**2)+X3*((5*U3+26*U2+17*U1)*V2+(5*U3+17*
     &  U2+26*U1)*V1+(2*U3+5*U2+5*U1)*V3)*(2*Y3-Y2)+2*(Y3+
     &  Y2)*(Y3-2*Y2)*(-5*(U2+U1)*U3-U3**2-13*U2**2-17*U2*U1-
     &  13*U1**2))*XMUL/(648*(X2*Y3-X3*Y2))
!
        A13(IELEM)=
     &  (-2*X2**2*((5*V2+17*V1)*V3+13*V3**2+V2**2+
     &  5*V2*V1+13*V1**2)+X2*(2*((5*V2+17*V1)*V3+13*V3**2+V2
     &  **2+5*V2*V1+13*V1**2)*X3-(26*U3+5*U2+17*U1)*(Y3-2*
     &  Y2)*V3-(17*U3+5*U2+26*U1)*(Y3-2*Y2)*V1-(5*U3+2*U2+
     &  5*U1)*(Y3-2*Y2)*V2)+4*X3**2*((5*V2+17*V1)*V3+13*V3**
     &  2+V2**2+5*V2*V1+13*V1**2)-X3*((26*U3+5*U2+17*U1)*V3+
     &  (17*U3+5*U2+26*U1)*V1+(5*U3+2*U2+5*U1)*V2)*(4*Y3+
     &  Y2)+2*(2*Y3-Y2)*(Y3+Y2)*((5*U2+17*U1)*U3+13*U3**2+U2
     &  **2+5*U2*U1+13*U1**2))*XMUL/(648*(X2*Y3-X3*Y2))
!
        A22(IELEM)=
     &  (2*X2**2*(7*V3**2+11*V3*V2+5*V3*V1+13*V2**
     &  2+11*V2*V1+7*V1**2)+2*X2*X3*(-25*V3**2-29*V3*V2-5*
     &  V3*V1-13*V2**2+7*V2*V1+11*V1**2)+2*X2*Y2*(-14*V3*U3-
     &  11*V3*U2-5*V3*U1-11*V2*U3-26*V2*U2-11*V2*U1-5*V1*U3-
     &  11*V1*U2-14*V1*U1)+X2*Y3*(50*V3*U3+29*V3*U2+5*V3*U1+
     &  29*V2*U3+26*V2*U2-7*V2*U1+5*V1*U3-7*V1*U2-22*V1*U1)+
     &  X3**2*(53*V3**2+73*V3*V2+25*V3*V1+65*V2**2+37*V2*V1+
     &  17*V1**2)+X3*Y2*(50*V3*U3+29*V3*U2+5*V3*U1+29*V2*U3+
     &  26*V2*U2-7*V2*U1+5*V1*U3-7*V1*U2-22*V1*U1)+X3*Y3*(-
     &  106*V3*U3-73*V3*U2-25*V3*U1-73*V2*U3-130*V2*U2-37*V2
     &  *U1-25*V1*U3-37*V1*U2-34*V1*U1)+2*Y2**2*(7*U3**2+11
     &  *U3*U2+5*U3*U1+13*U2**2+11*U2*U1+7*U1**2)+2*Y2*Y3*(-
     &  25*U3**2-29*U3*U2-5*U3*U1-13*U2**2+7*U2*U1+11*U1**2)
     &  +Y3**2*(53*U3**2+73*U3*U2+25*U3*U1+65*U2**2+37*U2*U1
     &  +17*U1**2))*XMUL/(324*(X2*Y3-X3*Y2))
!
        A23(IELEM)=
     &  (-((10*((17*V2+5*V1)*V3+13*V3**2+13*V2**2+
     &  5*V2*V1+V1**2)*X3-(26*U3+17*U2+5*U1)*(5*Y3-4*Y2)*V3-
     &  (17*U3+26*U2+5*U1)*(5*Y3-4*Y2)*V2-(5*U3+5*U2+2*U1
     &  )*(5*Y3-4*Y2)*V1)*X2-4*((17*V2+5*V1)*V3+13*V3**2+
     &  13*V2**2+5*V2*V1+V1**2)*X2**2-4*((17*V2+5*V1)*V3+13*
     &  V3**2+13*V2**2+5*V2*V1+V1**2)*X3**2+((26*U3+17*U2+5*
     &  U1)*V3+(17*U3+26*U2+5*U1)*V2+(5*U3+5*U2+2*U1)*V1)*(
     &  4*Y3-5*Y2)*X3-2*(17*U2+5*U1)*(2*Y3-Y2)*(Y3-2*Y2)*U3
     &  -26*(2*Y3-Y2)*(Y3-2*Y2)*U3**2-26*(2*Y3-Y2)*(Y3-2*Y2
     &  )*U2**2-10*(2*Y3-Y2)*(Y3-2*Y2)*U2*U1-2*(2*Y3-Y2)*(Y3
     &  -2*Y2)*U1**2))*XMUL/(648*(X2*Y3-X3*Y2))
!
        A33(IELEM)=
     &  (X2**2*(65*V3**2+73*V3*V2+37*V3*V1+53*V2**2
     &  +25*V2*V1+17*V1**2)+2*X2*X3*(-13*V3**2-29*V3*V2+7*
     &  V3*V1-25*V2**2-5*V2*V1+11*V1**2)+X2*Y2*(-130*V3*U3-
     &  73*V3*U2-37*V3*U1-73*V2*U3-106*V2*U2-25*V2*U1-37*V1*
     &  U3-25*V1*U2-34*V1*U1)+X2*Y3*(26*V3*U3+29*V3*U2-7*V3*
     &  U1+29*V2*U3+50*V2*U2+5*V2*U1-7*V1*U3+5*V1*U2-22*V1*
     &  U1)+2*X3**2*(13*V3**2+11*V3*V2+11*V3*V1+7*V2**2+5*
     &  V2*V1+7*V1**2)+X3*Y2*(26*V3*U3+29*V3*U2-7*V3*U1+29*
     &  V2*U3+50*V2*U2+5*V2*U1-7*V1*U3+5*V1*U2-22*V1*U1)+2*
     &  X3*Y3*(-26*V3*U3-11*V3*U2-11*V3*U1-11*V2*U3-14*V2*U2
     &  -5*V2*U1-11*V1*U3-5*V1*U2-14*V1*U1)+Y2**2*(65*U3**2+
     &  73*U3*U2+37*U3*U1+53*U2**2+25*U2*U1+17*U1**2)+2*Y2*
     &  Y3*(-13*U3**2-29*U3*U2+7*U3*U1-25*U2**2-5*U2*U1+11*
     &  U1**2)+2*Y3**2*(13*U3**2+11*U3*U2+11*U3*U1+7*U2**2+
     &  5*U2*U1+7*U1**2))*XMUL/(324*(X2*Y3-X3*Y2))
!
!
! USES HERE THE 'MAGIC SQUARE' PROPERTIES
! (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
!
        A14(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
!
        A24(IELEM) = - A12(IELEM) - A22(IELEM) - A23(IELEM)
!
        A34(IELEM) = - A13(IELEM) - A23(IELEM) - A33(IELEM)
!
        A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.12.AND.IELMV.EQ.12) THEN
!
!-----------------------------------------------------------------------
!
!  QUASI-BUBBLE DISCRETISATION OF THE VELOCITY:
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
        U4 = U(IKLE4(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
        V4 = V(IKLE4(IELEM))
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
!
!  COMPUTES 6 OF THE 16 TERMS (SELECTED AMONG THE LEAST COMPLEX)
!
        A12(IELEM)=
     &    ((2*((V2+V1)*V4+V4**2+V2**2+V2*V1+V1**2)*X3-(
     &    2*U4+U2+U1)*(Y3+4*Y2)*V4-(U4+2*U2+U1)*(Y3+4*Y2)*V2-(U4
     &    +U2+2*U1)*(Y3+4*Y2)*V1)*X2+2*((V2+V1)*V4+V4**2+V2**2+
     &    V2*V1+V1**2)*(2*X2**2-X3**2)+((2*U4+U2+U1)*V4+(U4+2*U2
     &    +U1)*V2+(U4+U2+2*U1)*V1)*(2*Y3-Y2)*X3-2*(U4**2+U2**2+
     &    U2*U1+U1**2)*(Y3+Y2)*(Y3-2*Y2)-2*(U2+U1)*(Y3+Y2)*(Y3-2
     &    *Y2)*U4)*XMUL/(72*(X2*Y3-X3*Y2))
!
        A13(IELEM)=
     &  (-(2*X2**2)*(V3**2+V3*V4+V3*V1+V4**2+V4*V1+V1
     &  **2)+2*X2*X3*(V3**2+V3*V4+V3*V1+V4**2+V4*V1+V1**2)+2*X2
     &  *Y2*(2*V3*U3+V3*U4+V3*U1+V4*U3+2*V4*U4+V4*U1+V1*U3+V1*
     &  U4+2*V1*U1)+X2*Y3*(-2*V3*U3-V3*U4-V3*U1-V4*U3-2*V4*U4-
     &  V4*U1-V1*U3-V1*U4-2*V1*U1)+4*X3**2*(V3**2+V3*V4+V3*V1+
     &  V4**2+V4*V1+V1**2)+X3*Y2*(-2*V3*U3-V3*U4-V3*U1-V4*U3-2*
     &  V4*U4-V4*U1-V1*U3-V1*U4-2*V1*U1)+4*X3*Y3*(-2*V3*U3-V3*
     &  U4-V3*U1-V4*U3-2*V4*U4-V4*U1-V1*U3-V1*U4-2*V1*U1)-(2*
     &  Y2**2)*(U3**2+U3*U4+U3*U1+U4**2+U4*U1+U1**2)+2*Y2*Y3*(U3
     &  **2+U3*U4+U3*U1+U4**2+U4*U1+U1**2)+4*Y3**2*(U3**2+U3*U4+
     &  U3*U1+U4**2+U4*U1+U1**2))*XMUL/(72*X2*Y3-72*X3*Y2)
!
        A14(IELEM)=
     &  (-(4*X2**2)*(V4**2+V4*V2+V4*V1+V2**2+V2*V1+V1
     &  **2)+2*X2*X3*(V3**2+V3*V4+V3*V1+2*V4**2+V4*V2+2*V4*V1+
     &  V2**2+V2*V1+2*V1**2)+4*X2*Y2*(2*V4*U4+V4*U2+V4*U1+V2*
     &  U4+2*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)+X2*Y3*(-2*V3*U3-
     &  V3*U4-V3*U1-V4*U3-4*V4*U4-V4*U2-2*V4*U1-V2*U4-2*V2*U2-
     &  V2*U1-V1*U3-2*V1*U4-V1*U2-4*V1*U1)-(4*X3**2)*(V3**2+V3
     &  *V4+V3*V1+V4**2+V4*V1+V1**2)+X3*Y2*(-2*V3*U3-V3*U4-V3*U1
     &  -V4*U3-4*V4*U4-V4*U2-2*V4*U1-V2*U4-2*V2*U2-V2*U1-V1*U3
     &  -2*V1*U4-V1*U2-4*V1*U1)+4*X3*Y3*(2*V3*U3+V3*U4+V3*U1+
     &  V4*U3+2*V4*U4+V4*U1+V1*U3+V1*U4+2*V1*U1)-(4*Y2**2)*(U4
     &  **2+U4*U2+U4*U1+U2**2+U2*U1+U1**2)+2*Y2*Y3*(U3**2+U3*U4+
     &  U3*U1+2*U4**2+U4*U2+2*U4*U1+U2**2+U2*U1+2*U1**2)-(4*
     &  Y3**2)*(U3**2+U3*U4+U3*U1+U4**2+U4*U1+U1**2))
     &  *XMUL/(24*X2*Y3-24*X3*Y2)
!
      A23(IELEM)=
     &  (-((10*((V4+V2)*V3+V3**2+V4**2+V4*V2+V2**2)*X3
     &  -(2*U3+U4+U2)*(5*Y3-4*Y2)*V3-(U3+2*U4+U2)*(5*Y3-4*
     &  Y2)*V4-(U3+U4+2*U2)*(5*Y3-4*Y2)*V2)*X2-4*((V4+V2)*V3+
     &  V3**2+V4**2+V4*V2+V2**2)*X2**2-4*((V4+V2)*V3+V3**2+V4**2
     &  +V4*V2+V2**2)*X3**2+((2*U3+U4+U2)*V3+(U3+2*U4+U2)*V4+(
     &  U3+U4+2*U2)*V2)*(4*Y3-5*Y2)*X3-2*(U4+U2)*(2*Y3-Y2)*(
     &  Y3-2*Y2)*U3-2*(2*Y3-Y2)*(Y3-2*Y2)*U3**2-2*(2*Y3-Y2)
     &  *(Y3-2*Y2)*U4**2-2*(2*Y3-Y2)*(Y3-2*Y2)*U4*U2-2*(2*
     &  Y3-Y2)*(Y3-2*Y2)*U2**2))*XMUL/(72*(X2*Y3-X3*Y2))
!
      ANS2=-(4*Y3**2)*(U3**2+U3*U4+U3*U2+U4**2+U4*U2+U2**2)
!
      ANS1=2*X2**2*(-V3**2-V3*V4-V3*V2-2*V4**2-2*V4*V2-V4*V1-
     & 2*V2**2-V2*V1-V1**2)+2*X2*X3*(3*V3**2+3*V3*V4+3*V3*V2
     & +2*V4**2+2*V4*V2-V4*V1+2*V2**2-V2*V1-V1**2)+2*X2*Y2*(
     & 2*V3*U3+V3*U4+V3*U2+V4*U3+4*V4*U4+2*V4*U2+V4*U1+V2*U3+
     & 2*V2*U4+4*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)+X2*Y3*(-6*
     & V3*U3-3*V3*U4-3*V3*U2-3*V4*U3-4*V4*U4-2*V4*U2+V4*U1-
     & 3*V2*U3-2*V2*U4-4*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)-(4
     & *X3**2)*(V3**2+V3*V4+V3*V2+V4**2+V4*V2+V2**2)+X3*Y2*(-6*
     & V3*U3-3*V3*U4-3*V3*U2-3*V4*U3-4*V4*U4-2*V4*U2+V4*U1-
     & 3*V2*U3-2*V2*U4-4*V2*U2+V2*U1+V1*U4+V1*U2+2*V1*U1)+4*
     & X3*Y3*(2*V3*U3+V3*U4+V3*U2+V4*U3+2*V4*U4+V4*U2+V2*U3+V2
     & *U4+2*V2*U2)+2*Y2**2*(-U3**2-U3*U4-U3*U2-2*U4**2-2*U4
     & *U2-U4*U1-2*U2**2-U2*U1-U1**2)+2*Y2*Y3*(3*U3**2+3*U3*
     & U4+3*U3*U2+2*U4**2+2*U4*U2-U4*U1+2*U2**2-U2*U1-U1**2)
     & +ANS2
!
        A24(IELEM)= ANS1*XMUL/(24*X2*Y3-24*X3*Y2)
!
      ANS2=2*Y3**2*(-2*U3**2-2*U3*U4-U3*U2-U3*U1-2*U4**2-U4*
     & U2-U4*U1-U2**2-U1**2)
!
      ANS1=-(4*X2**2)*(V3**2+V3*V4+V3*V2+V4**2+V4*V2+V2**2)+2*
     & X2*X3*(2*V3**2+2*V3*V4+3*V3*V2-V3*V1+2*V4**2+3*V4*V2
     & -V4*V1+3*V2**2-V1**2)+4*X2*Y2*(2*V3*U3+V3*U4+V3*U2+V4*
     & U3+2*V4*U4+V4*U2+V2*U3+V2*U4+2*V2*U2)+X2*Y3*(-4*V3*U3-
     & 2*V3*U4-3*V3*U2+V3*U1-2*V4*U3-4*V4*U4-3*V4*U2+V4*U1-
     & 3*V2*U3-3*V2*U4-6*V2*U2+V1*U3+V1*U4+2*V1*U1)+2*X3**2*
     & (-2*V3**2-2*V3*V4-V3*V2-V3*V1-2*V4**2-V4*V2-V4*V1-V2**
     & 2-V1**2)+X3*Y2*(-4*V3*U3-2*V3*U4-3*V3*U2+V3*U1-2*V4*
     & U3-4*V4*U4-3*V4*U2+V4*U1-3*V2*U3-3*V2*U4-6*V2*U2+V1*
     & U3+V1*U4+2*V1*U1)+2*X3*Y3*(4*V3*U3+2*V3*U4+V3*U2+V3*
     & U1+2*V4*U3+4*V4*U4+V4*U2+V4*U1+V2*U3+V2*U4+2*V2*U2+V1*
     & U3+V1*U4+2*V1*U1)-(4*Y2**2)*(U3**2+U3*U4+U3*U2+U4**2+U4
     & *U2+U2**2)+2*Y2*Y3*(2*U3**2+2*U3*U4+3*U3*U2-U3*U1+2*
     & U4**2+3*U4*U2-U4*U1+3*U2**2-U1**2)+ANS2
!
        A34(IELEM)=ANS1*XMUL/(24*X2*Y3-24*X3*Y2)
!
!
! USES HERE THE 'MAGIC SQUARE' PROPERTIES TO GET THE DIAGONAL TERMS
! (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
!
        A22(IELEM) = - A12(IELEM) - A23(IELEM) - A24(IELEM)
!
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A34(IELEM)
!
        A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
        IF(IELMU.EQ.IELMV) THEN
        WRITE(LU,101) IELMU
101     FORMAT(1X,'MT04BB (BIEF) :',/,
     &         1X,'DISCRETIZATION OF U AND V : ',1I6,' NOT AVAILABLE')
        ELSE
        WRITE(LU,201) IELMU,IELMV
201     FORMAT(1X,'MT04BB (BIEF) :',/,
     &         1X,'U AND V OF A DIFFERENT DISCRETISATION:',1I6,3X,1I6)
        ENDIF
!
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
