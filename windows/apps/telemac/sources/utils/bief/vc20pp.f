!                   *****************
                    SUBROUTINE VC20PP
!                   *****************
!
     &( XMUL,SURFAC,SU,SV,SW,U,V,W,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     &  W1,W2,W3,W4,W5,W6 )
!
!***********************************************************************
! BIEF   V8P0                                    21/07/2018
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!          STRAIN RATE TENSOR NORM
!+    PSI(I) IS A BASE OF TYPE P1 PRISM
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH
!
!
!history  A. BOURGOIN (EDF R&D LNHE) & R. ATA
!+        07/01/2018
!+
!+   creation.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF PRISMS
!| IKLE2          |-->| SECOND POINT OF PRISMS
!| IKLE3          |-->| THIRD POINT OF PRISMS
!| IKLE4          |-->| FOURTH POINT OF PRISMS
!| IKLE5          |-->| FIFTH POINT OF PRISMS
!| IKLE6          |-->| SIXTH POINT OF PRISMS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| W5             |<--| RESULT IN NON ASSEMBLED FORM
!| W6             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Z              |-->| ELEVATIONS OF POINTS ,PER POINT !!!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC20PP => VC20PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: X(NELMAX,6),Y(NELMAX,6),Z(*)
      DOUBLE PRECISION, INTENT(IN) :: XMUL,SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT)::W4(NELMAX),W5(NELMAX),W6(NELMAX)
!
!     STRUCTURES OF F, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV,SW
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION X2,X3,Y2,Y3,COEF,XS24,XS144
      DOUBLE PRECISION Z2,Z3,Z4,Z5,Z6
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION Q1,Q2,Q3,Q4,Q5,Q6,H1,H2,H3,SHT,DIFFU,DIFFV,DIFFW
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,IELMU,IELMV,IELMW
!
!**********************************************************************
!
      XS24  = XMUL/24.D0
      XS144 = XMUL/144.D0
!
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F AND VECTOR U ARE LINEAR
!
      IF(IELMU.EQ.41.AND.IELMV.EQ.41.AND.IELMW.EQ.41) THEN
!
!       LOOP ON THE ELEMENTS
!
        DO IELEM = 1,NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          X2 = X(IELEM,2)
          X3 = X(IELEM,3)
          Y2 = Y(IELEM,2)
          Y3 = Y(IELEM,3)
!
          U1  =  U(I1)
          U2  =  U(I2)
          U3  =  U(I3)
          U4  =  U(I4)
          U5  =  U(I5)
          U6  =  U(I6)
          V1  =  V(I1)
          V2  =  V(I2)
          V3  =  V(I3)
          V4  =  V(I4)
          V5  =  V(I5)
          V6  =  V(I6)
          Q1  =  W(I1)
          Q2  =  W(I2)
          Q3  =  W(I3)
          Q4  =  W(I4)
          Q5  =  W(I5)
          Q6  =  W(I6)

          H1 = Z(IKLE4(IELEM)) - Z(IKLE1(IELEM))
          H2 = Z(IKLE5(IELEM)) - Z(IKLE2(IELEM))
          H3 = Z(IKLE6(IELEM)) - Z(IKLE3(IELEM))
          SHT = H1 + H2 + H3
          COEF=XS24*SURFAC(IELEM)
!
          Z2  =  Z(I2) - Z(I1)
          Z3  =  Z(I3) - Z(I1)
          Z4  =  Z(I4) - Z(I1)
          Z5  =  Z(I5) - Z(I1)
          Z6  =  Z(I6) - Z(I1)


!      2(du/dx)^2

      W1(IELEM)=2*(((2*U1-U6)*Y2*( Z5+3*Z4-3*Z3  -Z2)
     &           +(2*U1-U5)*Y3*(-Z6-3*Z4  +Z3+3*Z2)
     &                  +U2*Y3*(2*Z6+3*Z5+3*Z4-2*Z3)
     &                  +U3*Y2*(-3*Z6-2*Z5-3*Z4+2*Z2)
     &             +(U3-U6)*Y3*(Z5-Z4+2*Z2)
     &                  +U4*Y2*(3*Z6+Z5+3*Z3-Z2)
     &                  +U4*Y3*(-Z6-3*Z5+Z3-3*Z2)
     &             +(U5-U2)*Y2*(Z6-Z4+2*Z3))*XS144)**2

      W2(IELEM)=2*((U1*Y2*(Z6+4*Z5+3*Z4-4*Z3-4*Z2)
     &             +U1*Y3*(-2*Z6-3*Z5-3*Z4+2*Z3+6*Z2)
     &      +(2*U2-U4)*Y3*(Z6+3*Z5-Z3)
     &             +U3*Y2*(-3*Z6-4*Z5-Z4+4*Z2)
     &             +U4*Y2*(2*Z6+2*Z5+Z3-2*Z2)
     &      +2*(U5-U2)*Y2*(Z6-Z4+2*Z3)
     &             +U5*Y3*(Z6+3*Z4-Z3-6*Z2)
     &             +U6*Y2*(-2*Z5-2*Z4+3*Z3+2*Z2)
     &        +(U6-U3)*Y3*(-Z5+Z4-2*Z2) )*XS144)**2

      W3(IELEM)=2*((U1*Y2*(3*Z6+2*Z5+3*Z4-6*Z3-2*Z2)
     &             +U1*Y3*(-4*Z6-Z5-3*Z4+4*Z3+4*Z2)
     &             +U2*Y3*(4*Z6+3*Z5+Z4-4*Z3)
     &      +(2*U3-U4)*Y2*(-3*Z6-Z5+Z2)
     &             +U4*Y3*(-2*Z6-2*Z5+2*Z3-Z2)
     &        +(U5-U2)*Y2*(Z6-Z4+2*Z3)
     &             +U5*Y3*(2*Z6+2*Z4-2*Z3-3*Z2)
     &             +U6*Y2*(-Z5-3*Z4+6*Z3+Z2)
     &      +2*(U6-U3)*Y3*(-Z5+Z4-2*Z2) )*XS144)**2

      W4(IELEM)=2*((U1*Y2*(-3*Z6+Z5+6*Z4-3*Z3-Z2)
     &             +U1*Y3*(-Z6+3*Z5-6*Z4+Z3+3*Z2)
     &             +U2*Y3*(Z6+3*Z5-Z3)
     &      +(2*U4-U3)*Y2*(3*Z6+Z5-Z2)
     &           +2*U4*Y3*(-Z6-3*Z5+Z3)
     &        +(U5-U2)*Y2*(2*Z6-2*Z4+Z3)
     &             +U5*Y3*(2*Z6+6*Z4-2*Z3-3*Z2)
     &             +U6*Y2*(-2*Z5-6*Z4+3*Z3+2*Z2)
     &        +(U6-U3)*Y3*(-2*Z5+2*Z4-Z2)  )*XS144)**2

      W5(IELEM)=2*((U1*Y2*(-Z6+2*Z5+3*Z4-2*Z3-2*Z2)
     &             +U2*Y3*(Z6+6*Z5-3*Z4-Z3)
     &             +U3*Y2*(-3*Z6-2*Z5+Z4+2*Z2)
     &             +U4*Y2*(4*Z6+4*Z5-Z3-4*Z2)
     &             +U4*Y3*(-2*Z6-6*Z5+2*Z3+3*Z2)
     &      +2*(U5-U2)*Y2*(2*Z6-2*Z4+Z3)
     &      +(2*U5-U1)*Y3*(Z6+3*Z4-Z3-3*Z2)
     &             +U6*Y2*(-4*Z5-4*Z4+3*Z3+4*Z2)
     &        +(U6-U3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144)**2

      W6(IELEM)=2*((U1*Y3*(-2*Z6+Z5-3*Z4+2*Z3+2*Z2)
     &             +U2*Y3*(2*Z6+3*Z5-Z4-2*Z3)
     &             +U3*Y2*(-6*Z6-Z5+3*Z4+Z2)
     &             +U4*Y2*(6*Z6+2*Z5-3*Z3-2*Z2)
     &             +U4*Y3*(-4*Z6-4*Z5+4*Z3+Z2)
     &        +(U5-U2)*Y2*(2*Z6-2*Z4+Z3)
     &             +U5*Y3*(4*Z6+4*Z4-4*Z3-3*Z2)
     &      +(2*U6-U1)*Y2*(-Z5-3*Z4+3*Z3+Z2)
     &      +2*(U6-U3)*Y3*(-2*Z5+2*Z4-Z2))*XS144)**2

!     +2(dv/dy)^2

      W1(IELEM)=W1(IELEM)+2*(((2*V1-V6)*X2*(-Z5-3*Z4+3*Z3+Z2)
     &                +2*V1*X3*(Z6+3*Z4-Z3-3*Z2)
     &                  +V2*X3*(-2*Z6-3*Z5-3*Z4+2*Z3)
     &                  +V3*X2*(3*Z6+2*Z5+3*Z4-2*Z2)
     &                  +V4*X2*(-3*Z6-Z5-3*Z3+Z2)
     &                  +V4*X3*(Z6+3*Z5-Z3+3*Z2)
     &             +(V5-V2)*X2*(-Z6+Z4-2*Z3)
     &                  +V5*X3*(-Z6-3*Z4+Z3+3*Z2)
     &             +(V6-V3)*X3*(Z5-Z4+2*Z2))*XS144)**2

      W2(IELEM)=W2(IELEM)+2*((V1*X2*(-Z6-4*Z5-3*Z4+4*Z3+4*Z2)
     &           +V1*X3*(2*Z6+3*Z5+3*Z4-2*Z3-6*Z2)
     &    +(2*V2-V4)*X3*(-Z6-3*Z5+Z3)
     &           +V3*X2*(3*Z6+4*Z5+Z4-4*Z2)
     &           +V4*X2*(-2*Z6-2*Z5-Z3+2*Z2)
     &    +2*(V5-V2)*X2*(-Z6+Z4-2*Z3)
     &           +V5*X3*(-Z6-3*Z4+Z3+6*Z2)
     &           +V6*X2*(2*Z5+2*Z4-3*Z3-2*Z2)
     &      +(V6-V3)*X3*(Z5-Z4+2*Z2))*XS144)**2

      W3(IELEM)=W3(IELEM)+2*((V1*X2*(-3*Z6-2*Z5-3*Z4+6*Z3+2*Z2)
     &           +V1*X3*(4*Z6+Z5+3*Z4-4*Z3-4*Z2)
     &           +V2*X3*(-4*Z6-3*Z5-Z4+4*Z3)
     &    +(2*V3-V4)*X2*(3*Z6+Z5-Z2)
     &           +V4*X3*(2*Z6+2*Z5-2*Z3+Z2)
     &      +(V5-V2)*X2*(-Z6+Z4-2*Z3)
     &           +V5*X3*(-2*Z6-2*Z4+2*Z3+3*Z2)
     &           +V6*X2*(Z5+3*Z4-6*Z3-Z2)
     &    +2*(V6-V3)*X3*(Z5-Z4+2*Z2))*XS144)**2

      W4(IELEM)=W4(IELEM)+2*((V1*X2*(3*Z6-Z5-6*Z4+3*Z3+Z2)
     &           +V1*X3*(Z6-3*Z5+6*Z4-Z3-3*Z2)
     &    +(2*V4-V3)*X2*(-3*Z6-Z5+Z2)
     &    +(2*V4-V2)*X3*(Z6+3*Z5-Z3)
     &      +(V5-V2)*X2*(-2*Z6+2*Z4-Z3)
     &           +V5*X3*(-2*Z6-6*Z4+2*Z3+3*Z2)
     &           +V6*X2*(2*Z5+6*Z4-3*Z3-2*Z2)
     &      +(V6-V3)*X3*(2*Z5-2*Z4+Z2))*XS144)**2

      W5(IELEM)=W5(IELEM)+2*((V1*X2*(Z6-2*Z5-3*Z4+2*Z3+2*Z2)
     &           +V2*X3*(-Z6-6*Z5+3*Z4+Z3)
     &           +V3*X2*(3*Z6+2*Z5-Z4-2*Z2)
     &           +V4*X2*(-4*Z6-4*Z5+Z3+4*Z2)
     &           +V4*X3*(2*Z6+6*Z5-2*Z3-3*Z2)
     &    +2*(V5-V2)*X2*(-2*Z6+2*Z4-Z3)
     &    +(2*V5-V1)*X3*(-Z6-3*Z4+Z3+3*Z2)
     &           +V6*X2*(4*Z5+4*Z4-3*Z3-4*Z2)
     &      +(V6-V3)*X3*(2*Z5-2*Z4+Z2))*XS144)**2

      W6(IELEM)=W6(IELEM)+2*((V1*X3*(2*Z6-Z5+3*Z4-2*Z3-2*Z2)
     &           +V2*X3*(-2*Z6-3*Z5+Z4+2*Z3)
     &           +V3*X2*(6*Z6+Z5-3*Z4-Z2)
     &           +V4*X2*(-6*Z6-2*Z5+3*Z3+2*Z2)
     &           +V4*X3*(4*Z6+4*Z5-4*Z3-Z2)
     &      +(V5-V2)*X2*(-2*Z6+2*Z4-Z3)
     &           +V5*X3*(-4*Z6-4*Z4+4*Z3+3*Z2)
     &    +(2*V6-V1)*X2*(Z5+3*Z4-3*Z3-Z2)
     &    +2*(V6-V3)*X3*(2*Z5-2*Z4+Z2))*XS144)**2

!!     +2(dw/dz)^2

      DIFFU = (U4+U5+U6)-(U1+U2+U3)
      DIFFV = (V4+V5+V6)-(V1+V2+V3)
      DIFFW = (Q4+Q5+Q6)-(Q1+Q2+Q3)


      W1(IELEM)=W1(IELEM)+2*((Q4-Q1+DIFFW)*COEF)**2
      W2(IELEM)=W2(IELEM)+2*((Q5-Q2+DIFFW)*COEF)**2
      W3(IELEM)=W3(IELEM)+2*((Q6-Q3+DIFFW)*COEF)**2
      W4(IELEM)=W4(IELEM)+2*((Q4-Q1+DIFFW)*COEF)**2
      W5(IELEM)=W5(IELEM)+2*((Q5-Q2+DIFFW)*COEF)**2
      W6(IELEM)=W6(IELEM)+2*((Q6-Q3+DIFFW)*COEF)**2


!      +(DU/DY+DV/DX)^2
!
!
      W1(IELEM)=W1(IELEM)+
     &          (((Z5+3*Z4-3*Z3-Z2)*((2*V1-V6)*Y2-(2*U1-U6)*X2)
     &          +(-Z6-3*Z4+Z3+3*Z2)*((2*V1-V5)*Y3-(2*U1-U5)*X3)
     &          +(2*Z6+3*Z5+3*Z4-2*Z3)*(V2*Y3-U2*X3)
     &          +(-3*Z6-2*Z5-3*Z4+2*Z2)*(V3*Y2-U3*X2)
     &          +(Z5-Z4+2*Z2)*((V3-V6)*Y3-(U3-U6)*X3)
     &          +(3*Z6+Z5+3*Z3-Z2)*(V4*Y2-U4*X2)
     &          +(-Z6-3*Z5+Z3-3*Z2)*(V4*Y3-U4*X3)
     &          +(Z6-Z4+2*Z3)*((V5-V2)*Y2-(U5-U2)*X2))*XS144)**2
!

      W2(IELEM)=W2(IELEM)+
     &          (((Z6+4*Z5+3*Z4-4*Z3-4*Z2)*(V1*Y2-U1*X2)
     &          +(-2*Z6-3*Z5-3*Z4+2*Z3+6*Z2)*(V1*Y3-U1*X3)
     &          +(Z6+3*Z5-Z3)*((2*V2-V4)*Y3-(2*U2-U4)*X3)
     &          +(-3*Z6-4*Z5-Z4+4*Z2)*(V3*Y2-U3*X2)
     &          +(2*Z6+2*Z5+Z3-2*Z2)*(V4*Y2-U4*X2)
     &          +(Z6-Z4+2*Z3)*(2*(V5-V2)*Y2-2*(U5-U2)*X2)
     &          +(Z6+3*Z4-Z3-6*Z2)*(V5*Y3-U5*X3)
     &          +(-2*Z5-2*Z4+3*Z3+2*Z2)*(V6*Y2-U6*X2)
     &          +(-Z5+Z4-2*Z2)*((V6-V3)*Y3-(U6-U3)*X3))*XS144)**2


      W3(IELEM)=W3(IELEM)+
     &          (((3*Z6+2*Z5+3*Z4-6*Z3-2*Z2)*(V1*Y2-U1*X2)
     &          +(-4*Z6-Z5-3*Z4+4*Z3+4*Z2)*(V1*Y3-U1*X3)
     &          +(4*Z6+3*Z5+Z4-4*Z3)*(V2*Y3-U2*X3)
     &          +(-3*Z6-Z5+Z2)*((2*V3-V4)*Y2-(2*U3-U4)*X2)
     &          +(-2*Z6-2*Z5+2*Z3-Z2)*(V4*Y3-U4*X3)
     &          +(Z6-Z4+2*Z3)*((V5-V2)*Y2-(U5-U2)*X2)
     &          +(2*Z6+2*Z4-2*Z3-3*Z2)*(V5*Y3-U5*X3)
     &          +(-Z5-3*Z4+6*Z3+Z2)*(V6*Y2-U6*X2)
     &          +(-Z5+Z4-2*Z2)*(2*(V6-V3)*Y3-2*(U6-U3)*X3))*XS144)**2


      W4(IELEM)=W4(IELEM)+
     &           (((-3*Z6+Z5+6*Z4-3*Z3-Z2)*(V1*Y2-U1*X2)
     &          +(-Z6+3*Z5-6*Z4+Z3+3*Z2)*(V1*Y3-U1*X3)
     &          +(3*Z6+Z5-Z2)*((2*V4-V3)*Y2-(2*U4-U3)*X2)
     &          +(-Z6-3*Z5+Z3)*((2*V4-V2)*Y3-(2*U4-U2)*X3)
     &          +(2*Z6-2*Z4+Z3)*(2*(V5-V2)*Y2-2*(U5-U2)*X2)
     &          +(2*Z6+6*Z4-2*Z3-3*Z2)*(V5*Y3-U5*X3)
     &          +(-2*Z5-6*Z4+3*Z3+2*Z2)*(V6*Y2-U6*X2)
     &          +(-2*Z5+2*Z4-Z2)*((V6-V3)*Y3-(U6-U3)*X3))*XS144)**2


      W5(IELEM)=W5(IELEM)+
     &          (((-Z6+2*Z5+3*Z4-2*Z3-2*Z2)*(V1*Y2-U1*X2)
     &          +(Z6+6*Z5-3*Z4-Z3)*(V2*Y3-U2*X3)
     &          +(-3*Z6-2*Z5+Z4+2*Z2)*(V3*Y2-U3*X2)
     &          +(4*Z6+4*Z5-Z3-4*Z2)*(V4*Y2-U4*X2)
     &          +(-2*Z6-6*Z5+2*Z3+3*Z2)*(V4*Y3-U4*X3)
     &          +(2*Z6-2*Z4+Z3)*(2*(V5-V2)*Y2-2*(U5-U2)*X2)
     &          +(Z6+3*Z4-Z3-3*Z2)*((2*V5-V1)*Y3-(2*U5-U1)*X3)
     &          +(-4*Z5-4*Z4+3*Z3+4*Z2)*(V6*Y2-U6*X2)
     &          +(-2*Z5+2*Z4-Z2)*((V6-V3)*Y3-(U6-U3)*X3))*XS144)**2


      W6(IELEM)=W6(IELEM)+
     &          (((-2*Z6+Z5-3*Z4+2*Z3+2*Z2)*(V1*Y3-U1*X3)
     &          +(2*Z6+3*Z5-Z4-2*Z3)*(V2*Y3-U2*X3)
     &          +(-6*Z6-Z5+3*Z4+Z2)*(V3*Y2-U3*X2)
     &          +(6*Z6+2*Z5-3*Z3-2*Z2)*(V4*Y2-U4*X2)
     &          +(-4*Z6-4*Z5+4*Z3+Z2)*(V4*Y3-U4*X3)
     &          +(2*Z6-2*Z4+Z3)*((V5-V2)*Y2-(U5-U2)*X2)
     &          +(4*Z6+4*Z4-4*Z3-3*Z2)*(V5*Y3-U5*X3)
     &          +(-Z5-3*Z4+3*Z3+Z2)*((2*V6-V1)*Y2-(2*U6-U1)*X2)
     &          +(-2*Z5+2*Z4-Z2)*(2*(V6-V3)*Y3-2*(U6-U3)*X3))*XS144)**2


!!      +(DU/DZ+DW/DX)^2

      W1(IELEM)=W1(IELEM)+
     &           ((U4-U1+DIFFU)*COEF
     &           +((2*Q1-Q6)*Y2*(Z5+3*Z4-3*Z3  -Z2)
     &           +(2*Q1-Q5)*Y3*(-Z6-3*Z4  +Z3+3*Z2)
     &                  +Q2*Y3*(2*Z6+3*Z5+3*Z4-2*Z3)
     &                  +Q3*Y2*(-3*Z6-2*Z5-3*Z4+2*Z2)
     &             +(Q3-Q6)*Y3*(Z5-Z4+2*Z2)
     &                  +Q4*Y2*(3*Z6+Z5+3*Z3-Z2)
     &                  +Q4*Y3*(-Z6-3*Z5+Z3-3*Z2)
     &             +(Q5-Q2)*Y2*(Z6-Z4+2*Z3))*XS144)**2


      W2(IELEM)=W2(IELEM)+
     &           ((U5-U2+DIFFU)*COEF
     &           +(Q1*Y2*(Z6+4*Z5+3*Z4-4*Z3-4*Z2)
     &             +Q1*Y3*(-2*Z6-3*Z5-3*Z4+2*Z3+6*Z2)
     &      +(2*Q2-Q4)*Y3*(Z6+3*Z5-Z3)
     &             +Q3*Y2*(-3*Z6-4*Z5-Z4+4*Z2)
     &             +Q4*Y2*(2*Z6+2*Z5+Z3-2*Z2)
     &      +2*(Q5-Q2)*Y2*(Z6-Z4+2*Z3)
     &             +Q5*Y3*(Z6+3*Z4-Z3-6*Z2)
     &             +Q6*Y2*(-2*Z5-2*Z4+3*Z3+2*Z2)
     &        +(Q6-Q3)*Y3*(-Z5+Z4-2*Z2))*XS144)**2


      W3(IELEM)=W3(IELEM)+
     &            ((U6-U3+DIFFU)*COEF
     &           +(Q1*Y2*(3*Z6+2*Z5+3*Z4-6*Z3-2*Z2)
     &             +Q1*Y3*(-4*Z6-Z5-3*Z4+4*Z3+4*Z2)
     &             +Q2*Y3*(4*Z6+3*Z5+Z4-4*Z3)
     &      +(2*Q3-Q4)*Y2*(-3*Z6-Z5+Z2)
     &             +Q4*Y3*(-2*Z6-2*Z5+2*Z3-Z2)
     &        +(Q5-Q2)*Y2*(Z6-Z4+2*Z3)
     &             +Q5*Y3*(2*Z6+2*Z4-2*Z3-3*Z2)
     &             +Q6*Y2*(-Z5-3*Z4+6*Z3+Z2)
     &      +2*(Q6-Q3)*Y3*(-Z5+Z4-2*Z2) )*XS144)**2


      W4(IELEM)=W4(IELEM)+
     &           ((U4-U1+DIFFU)*COEF
     &           +(Q1*Y2*(-3*Z6+Z5+6*Z4-3*Z3-Z2)
     &             +Q1*Y3*(-Z6+3*Z5-6*Z4+Z3+3*Z2)
     &             +Q2*Y3*(Z6+3*Z5-Z3)
     &      +(2*Q4-Q3)*Y2*(3*Z6+Z5-Z2)
     &           +2*Q4*Y3*(-Z6-3*Z5+Z3)
     &        +(Q5-Q2)*Y2*(2*Z6-2*Z4+Z3)
     &             +Q5*Y3*(2*Z6+6*Z4-2*Z3-3*Z2)
     &             +Q6*Y2*(-2*Z5-6*Z4+3*Z3+2*Z2)
     &        +(Q6-Q3)*Y3*(-2*Z5+2*Z4-Z2)  )*XS144)**2


      W5(IELEM)=W5(IELEM)+
     &           ((U5-U2+DIFFU)*COEF
     &           +(Q1*Y2*(-Z6+2*Z5+3*Z4-2*Z3-2*Z2)
     &             +Q2*Y3*(Z6+6*Z5-3*Z4-Z3)
     &             +Q3*Y2*(-3*Z6-2*Z5+Z4+2*Z2)
     &             +Q4*Y2*(4*Z6+4*Z5-Z3-4*Z2)
     &             +Q4*Y3*(-2*Z6-6*Z5+2*Z3+3*Z2)
     &      +2*(Q5-Q2)*Y2*(2*Z6-2*Z4+Z3)
     &      +(2*Q5-Q1)*Y3*(Z6+3*Z4-Z3-3*Z2)
     &             +Q6*Y2*(-4*Z5-4*Z4+3*Z3+4*Z2)
     &        +(Q6-Q3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144)**2


      W6(IELEM)=W6(IELEM)+
     &           ((U6-U3+DIFFU)*COEF
     &           +(Q1*Y3*(-2*Z6+Z5-3*Z4+2*Z3+2*Z2)
     &             +Q2*Y3*(2*Z6+3*Z5-Z4-2*Z3)
     &             +Q3*Y2*(-6*Z6-Z5+3*Z4+Z2)
     &             +Q4*Y2*(6*Z6+2*Z5-3*Z3-2*Z2)
     &             +Q4*Y3*(-4*Z6-4*Z5+4*Z3+Z2)
     &        +(Q5-Q2)*Y2*(2*Z6-2*Z4+Z3)
     &             +Q5*Y3*(4*Z6+4*Z4-4*Z3-3*Z2)
     &      +(2*Q6-Q1)*Y2*(-Z5-3*Z4+3*Z3+Z2)
     &      +2*(Q6-Q3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144)**2

!!      +(DV/DZ+DW/DY)^2



      W1(IELEM)=W1(IELEM)+
     &           ((V4-V1+DIFFV)*COEF
     &           +((2*Q1-Q6)*X2*(Z5+3*Z4-3*Z3  -Z2)
     &           +(2*Q1-Q5)*X3*(-Z6-3*Z4  +Z3+3*Z2)
     &                  +Q2*X3*(2*Z6+3*Z5+3*Z4-2*Z3)
     &                  +Q3*X2*(-3*Z6-2*Z5-3*Z4+2*Z2)
     &             +(Q3-Q6)*X3*(Z5-Z4+2*Z2)
     &                  +Q4*X2*(3*Z6+Z5+3*Z3-Z2)
     &                  +Q4*X3*(-Z6-3*Z5+Z3-3*Z2)
     &             +(Q5-Q2)*X2*(Z6-Z4+2*Z3))*XS144)**2


      W2(IELEM)=W2(IELEM)+
     &           ((V5-V2+DIFFV)*COEF
     &           +(Q1*X2*(Z6+4*Z5+3*Z4-4*Z3-4*Z2)
     &             +Q1*X3*(-2*Z6-3*Z5-3*Z4+2*Z3+6*Z2)
     &      +(2*Q2-Q4)*X3*(Z6+3*Z5-Z3)
     &             +Q3*X2*(-3*Z6-4*Z5-Z4+4*Z2)
     &             +Q4*X2*(2*Z6+2*Z5+Z3-2*Z2)
     &      +2*(Q5-Q2)*X2*(Z6-Z4+2*Z3)
     &             +Q5*X3*(Z6+3*Z4-Z3-6*Z2)
     &             +Q6*X2*(-2*Z5-2*Z4+3*Z3+2*Z2)
     &        +(Q6-Q3)*X3*(-Z5+Z4-2*Z2))*XS144)**2


      W3(IELEM)=W3(IELEM)+
     &           ((V6-V3+DIFFV)*COEF
     &           +(Q1*X2*(3*Z6+2*Z5+3*Z4-6*Z3-2*Z2)
     &             +Q1*X3*(-4*Z6-Z5-3*Z4+4*Z3+4*Z2)
     &             +Q2*X3*(4*Z6+3*Z5+Z4-4*Z3)
     &      +(2*Q3-Q4)*X2*(-3*Z6-Z5+Z2)
     &             +Q4*X3*(-2*Z6-2*Z5+2*Z3-Z2)
     &        +(Q5-Q2)*X2*(Z6-Z4+2*Z3)
     &             +Q5*X3*(2*Z6+2*Z4-2*Z3-3*Z2)
     &             +Q6*X2*(-Z5-3*Z4+6*Z3+Z2)
     &      +2*(Q6-Q3)*X3*(-Z5+Z4-2*Z2) )*XS144)**2


      W4(IELEM)=W4(IELEM)+
     &           ((V4-V1+DIFFV)*COEF
     &           +(Q1*X2*(-3*Z6+Z5+6*Z4-3*Z3-Z2)
     &             +Q1*X3*(-Z6+3*Z5-6*Z4+Z3+3*Z2)
     &             +Q2*X3*(Z6+3*Z5-Z3)
     &      +(2*Q4-Q3)*X2*(3*Z6+Z5-Z2)
     &           +2*Q4*X3*(-Z6-3*Z5+Z3)
     &        +(Q5-Q2)*X2*(2*Z6-2*Z4+Z3)
     &             +Q5*X3*(2*Z6+6*Z4-2*Z3-3*Z2)
     &             +Q6*X2*(-2*Z5-6*Z4+3*Z3+2*Z2)
     &        +(Q6-Q3)*X3*(-2*Z5+2*Z4-Z2)  )*XS144)**2


      W5(IELEM)=W5(IELEM)+
     &           ((V5-V2+DIFFV)*COEF
     &           +(Q1*X2*(-Z6+2*Z5+3*Z4-2*Z3-2*Z2)
     &             +Q2*X3*(Z6+6*Z5-3*Z4-Z3)
     &             +Q3*X2*(-3*Z6-2*Z5+Z4+2*Z2)
     &             +Q4*X2*(4*Z6+4*Z5-Z3-4*Z2)
     &             +Q4*X3*(-2*Z6-6*Z5+2*Z3+3*Z2)
     &      +2*(Q5-Q2)*X2*(2*Z6-2*Z4+Z3)
     &      +(2*Q5-Q1)*X3*(Z6+3*Z4-Z3-3*Z2)
     &             +Q6*X2*(-4*Z5-4*Z4+3*Z3+4*Z2)
     &        +(Q6-Q3)*X3*(-2*Z5+2*Z4-Z2) )*XS144)**2


      W6(IELEM)=W6(IELEM)+
     &            ((V6-V3+DIFFV)*COEF
     &           +(Q1*X3*(-2*Z6+Z5-3*Z4+2*Z3+2*Z2)
     &             +Q2*X3*(2*Z6+3*Z5-Z4-2*Z3)
     &             +Q3*X2*(-6*Z6-Z5+3*Z4+Z2)
     &             +Q4*X2*(6*Z6+2*Z5-3*Z3-2*Z2)
     &             +Q4*X3*(-4*Z6-4*Z5+4*Z3+Z2)
     &        +(Q5-Q2)*X2*(2*Z6-2*Z4+Z3)
     &             +Q5*X3*(4*Z6+4*Z4-4*Z3-3*Z2)
     &      +(2*Q6-Q1)*X2*(-Z5-3*Z4+3*Z3+Z2)
     &      +2*(Q6-Q3)*X3*(-2*Z5+2*Z4-Z2) )*XS144)**2


      W1(IELEM)=W1(IELEM)/(COEF*(SHT+H1))
      W2(IELEM)=W2(IELEM)/(COEF*(SHT+H2))
      W3(IELEM)=W3(IELEM)/(COEF*(SHT+H3))
      W4(IELEM)=W4(IELEM)/(COEF*(SHT+H1))
      W5(IELEM)=W5(IELEM)/(COEF*(SHT+H2))
      W6(IELEM)=W6(IELEM)/(COEF*(SHT+H3))


!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!

        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)

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

