!                   *****************
                    SUBROUTINE VC21PP
!                   *****************
!
     &( XMUL,SF,F,X,Y,Z,SURFAC,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     &  W1,W2,W3,W4,W5,W6)
!
!***********************************************************************
! BIEF   V8P0                                    21/06/2018
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I) * F  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 PRISM
!+
!+    F IS A VECTOR OF TYPE IELMF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH
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
!| IKLE1          |-->| FIRST POINT OF PRISMS
!| IKLE2          |-->| SECOND POINT OF PRISMS
!| IKLE3          |-->| THIRD POINT OF PRISMS
!| IKLE4          |-->| FOURTH POINT OF PRISMS
!| IKLE5          |-->| FIFTH POINT OF PRISMS
!| IKLE6          |-->| SIXTH POINT OF PRISMS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| W5             |<--| RESULT IN NON ASSEMBLED FORM
!| W6             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Z              |-->| ELEVATIONS OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC21PP => VC21PP
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
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
      DOUBLE PRECISION,INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION,INTENT(INOUT)::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ),   INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION COEF,H1,H2,H3,SHT,DIFF,X2,Y2,X3,Y3,XS24,XS144
      DOUBLE PRECISION Z2,Z3,Z4,Z5,Z6
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6
!
!***********************************************************************
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!
!   F IS LINEAR

      XS24  = XMUL/24.D0
      XS144 = XMUL/144.D0
!
      IF(IELMF.EQ.41) THEN
      DO IELEM = 1 , NELEM
!
        I1 = IKLE1(IELEM)
        I2 = IKLE2(IELEM)
        I3 = IKLE3(IELEM)
        I4 = IKLE4(IELEM)
        I5 = IKLE5(IELEM)
        I6 = IKLE6(IELEM)
!
        F1 = F(I1)
        F2 = F(I2)
        F3 = F(I3)
        F4 = F(I4)
        F5 = F(I5)
        F6 = F(I6)
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
!       Y2  =  Y(I2) - Y(I1)
!       Y3  =  Y(I3) - Y(I1)
        X2  =  X(IELEM,2)
        X3  =  X(IELEM,3)
        Y2  =  Y(IELEM,2)
        Y3  =  Y(IELEM,3)
!
        Z2  =  Z(I2) - Z(I1)
        Z3  =  Z(I3) - Z(I1)
        Z4  =  Z(I4) - Z(I1)
        Z5  =  Z(I5) - Z(I1)
        Z6  =  Z(I6) - Z(I1)


        H1 = Z(IKLE4(IELEM)) - Z(IKLE1(IELEM))
        H2 = Z(IKLE5(IELEM)) - Z(IKLE2(IELEM))
        H3 = Z(IKLE6(IELEM)) - Z(IKLE3(IELEM))
        SHT = H1 + H2 + H3
!!
      W1(IELEM)=(((2*F1-F6)*Y2*( Z5+3*Z4-3*Z3  -Z2)
     &           +(2*F1-F5)*Y3*(-Z6-3*Z4  +Z3+3*Z2)
     &                  +F2*Y3*(2*Z6+3*Z5+3*Z4-2*Z3)
     &                  +F3*Y2*(-3*Z6-2*Z5-3*Z4+2*Z2)
     &             +(F3-F6)*Y3*(Z5-Z4+2*Z2)
     &                  +F4*Y2*(3*Z6+Z5+3*Z3-Z2)
     &                  +F4*Y3*(-Z6-3*Z5+Z3-3*Z2)
     &             +(F5-F2)*Y2*(Z6-Z4+2*Z3))*XS144)**2

      W2(IELEM)=((F1*Y2*(Z6+4*Z5+3*Z4-4*Z3-4*Z2)
     &             +F1*Y3*(-2*Z6-3*Z5-3*Z4+2*Z3+6*Z2)
     &      +(2*F2-F4)*Y3*(Z6+3*Z5-Z3)
     &             +F3*Y2*(-3*Z6-4*Z5-Z4+4*Z2)
     &             +F4*Y2*(2*Z6+2*Z5+Z3-2*Z2)
     &      +2*(F5-F2)*Y2*(Z6-Z4+2*Z3)
     &             +F5*Y3*(Z6+3*Z4-Z3-6*Z2)
     &             +F6*Y2*(-2*Z5-2*Z4+3*Z3+2*Z2)
     &        +(F6-F3)*Y3*(-Z5+Z4-2*Z2) )*XS144)**2

      W3(IELEM)=((F1*Y2*(3*Z6+2*Z5+3*Z4-6*Z3-2*Z2)
     &             +F1*Y3*(-4*Z6-Z5-3*Z4+4*Z3+4*Z2)
     &             +F2*Y3*(4*Z6+3*Z5+Z4-4*Z3)
     &      +(2*F3-F4)*Y2*(-3*Z6-Z5+Z2)
     &             +F4*Y3*(-2*Z6-2*Z5+2*Z3-Z2)
     &        +(F5-F2)*Y2*(Z6-Z4+2*Z3)
     &             +F5*Y3*(2*Z6+2*Z4-2*Z3-3*Z2)
     &             +F6*Y2*(-Z5-3*Z4+6*Z3+Z2)
     &      +2*(F6-F3)*Y3*(-Z5+Z4-2*Z2) )*XS144)**2

      W4(IELEM)=((F1*Y2*(-3*Z6+Z5+6*Z4-3*Z3-Z2)
     &             +F1*Y3*(-Z6+3*Z5-6*Z4+Z3+3*Z2)
     &             +F2*Y3*(Z6+3*Z5-Z3)
     &      +(2*F4-F3)*Y2*(3*Z6+Z5-Z2)
     &           +2*F4*Y3*(-Z6-3*Z5+Z3)
     &        +(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &             +F5*Y3*(2*Z6+6*Z4-2*Z3-3*Z2)
     &             +F6*Y2*(-2*Z5-6*Z4+3*Z3+2*Z2)
     &        +(F6-F3)*Y3*(-2*Z5+2*Z4-Z2)  )*XS144)**2

      W5(IELEM)=((F1*Y2*(-Z6+2*Z5+3*Z4-2*Z3-2*Z2)
     &             +F2*Y3*(Z6+6*Z5-3*Z4-Z3)
     &             +F3*Y2*(-3*Z6-2*Z5+Z4+2*Z2)
     &             +F4*Y2*(4*Z6+4*Z5-Z3-4*Z2)
     &             +F4*Y3*(-2*Z6-6*Z5+2*Z3+3*Z2)
     &      +2*(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &      +(2*F5-F1)*Y3*(Z6+3*Z4-Z3-3*Z2)
     &             +F6*Y2*(-4*Z5-4*Z4+3*Z3+4*Z2)
     &        +(F6-F3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144)**2

      W6(IELEM)=((F1*Y3*(-2*Z6+Z5-3*Z4+2*Z3+2*Z2)
     &             +F2*Y3*(2*Z6+3*Z5-Z4-2*Z3)
     &             +F3*Y2*(-6*Z6-Z5+3*Z4+Z2)
     &             +F4*Y2*(6*Z6+2*Z5-3*Z3-2*Z2)
     &             +F4*Y3*(-4*Z6-4*Z5+4*Z3+Z2)
     &        +(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &             +F5*Y3*(4*Z6+4*Z4-4*Z3-3*Z2)
     &      +(2*F6-F1)*Y2*(-Z5-3*Z4+3*Z3+Z2)
     &      +2*(F6-F3)*Y3*(-2*Z5+2*Z4-Z2))*XS144)**2



      W1(IELEM)=W1(IELEM)+(((2*F1-F6)*X2*(-Z5-3*Z4+3*Z3+Z2)
     &                +2*F1*X3*(Z6+3*Z4-Z3-3*Z2)
     &                  +F2*X3*(-2*Z6-3*Z5-3*Z4+2*Z3)
     &                  +F3*X2*(3*Z6+2*Z5+3*Z4-2*Z2)
     &                  +F4*X2*(-3*Z6-Z5-3*Z3+Z2)
     &                  +F4*X3*(Z6+3*Z5-Z3+3*Z2)
     &             +(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &                  +F5*X3*(-Z6-3*Z4+Z3+3*Z2)
     &             +(F6-F3)*X3*(Z5-Z4+2*Z2))*XS144)**2

      W2(IELEM)=W2(IELEM)+((F1*X2*(-Z6-4*Z5-3*Z4+4*Z3+4*Z2)
     &           +F1*X3*(2*Z6+3*Z5+3*Z4-2*Z3-6*Z2)
     &    +(2*F2-F4)*X3*(-Z6-3*Z5+Z3)
     &           +F3*X2*(3*Z6+4*Z5+Z4-4*Z2)
     &           +F4*X2*(-2*Z6-2*Z5-Z3+2*Z2)
     &    +2*(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &           +F5*X3*(-Z6-3*Z4+Z3+6*Z2)
     &           +F6*X2*(2*Z5+2*Z4-3*Z3-2*Z2)
     &      +(F6-F3)*X3*(Z5-Z4+2*Z2))*XS144)**2

      W3(IELEM)=W3(IELEM)+((F1*X2*(-3*Z6-2*Z5-3*Z4+6*Z3+2*Z2)
     &           +F1*X3*(4*Z6+Z5+3*Z4-4*Z3-4*Z2)
     &           +F2*X3*(-4*Z6-3*Z5-Z4+4*Z3)
     &    +(2*F3-F4)*X2*(3*Z6+Z5-Z2)
     &           +F4*X3*(2*Z6+2*Z5-2*Z3+Z2)
     &      +(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &           +F5*X3*(-2*Z6-2*Z4+2*Z3+3*Z2)
     &           +F6*X2*(Z5+3*Z4-6*Z3-Z2)
     &    +2*(F6-F3)*X3*(Z5-Z4+2*Z2))*XS144)**2

      W4(IELEM)=W4(IELEM)+((F1*X2*(3*Z6-Z5-6*Z4+3*Z3+Z2)
     &           +F1*X3*(Z6-3*Z5+6*Z4-Z3-3*Z2)
     &    +(2*F4-F3)*X2*(-3*Z6-Z5+Z2)
     &    +(2*F4-F2)*X3*(Z6+3*Z5-Z3)
     &      +(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &           +F5*X3*(-2*Z6-6*Z4+2*Z3+3*Z2)
     &           +F6*X2*(2*Z5+6*Z4-3*Z3-2*Z2)
     &      +(F6-F3)*X3*(2*Z5-2*Z4+Z2))*XS144)**2

      W5(IELEM)=W5(IELEM)+((F1*X2*(Z6-2*Z5-3*Z4+2*Z3+2*Z2)
     &           +F2*X3*(-Z6-6*Z5+3*Z4+Z3)
     &           +F3*X2*(3*Z6+2*Z5-Z4-2*Z2)
     &           +F4*X2*(-4*Z6-4*Z5+Z3+4*Z2)
     &           +F4*X3*(2*Z6+6*Z5-2*Z3-3*Z2)
     &    +2*(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &    +(2*F5-F1)*X3*(-Z6-3*Z4+Z3+3*Z2)
     &           +F6*X2*(4*Z5+4*Z4-3*Z3-4*Z2)
     &      +(F6-F3)*X3*(2*Z5-2*Z4+Z2))*XS144)**2

      W6(IELEM)=W6(IELEM)+((F1*X3*(2*Z6-Z5+3*Z4-2*Z3-2*Z2)
     &           +F2*X3*(-2*Z6-3*Z5+Z4+2*Z3)
     &           +F3*X2*(6*Z6+Z5-3*Z4-Z2)
     &           +F4*X2*(-6*Z6-2*Z5+3*Z3+2*Z2)
     &           +F4*X3*(4*Z6+4*Z5-4*Z3-Z2)
     &      +(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &           +F5*X3*(-4*Z6-4*Z4+4*Z3+3*Z2)
     &    +(2*F6-F1)*X2*(Z5+3*Z4-3*Z3-Z2)
     &    +2*(F6-F3)*X3*(2*Z5-2*Z4+Z2))*XS144)**2

      DIFF = (F4+F5+F6)-(F1+F2+F3)
      COEF=XS24*SURFAC(IELEM)

      W1(IELEM)=W1(IELEM)+((F4-F1+DIFF)*COEF)**2
      W2(IELEM)=W2(IELEM)+((F5-F2+DIFF)*COEF)**2
      W3(IELEM)=W3(IELEM)+((F6-F3+DIFF)*COEF)**2
      W4(IELEM)=W4(IELEM)+((F4-F1+DIFF)*COEF)**2
      W5(IELEM)=W5(IELEM)+((F5-F2+DIFF)*COEF)**2
      W6(IELEM)=W6(IELEM)+((F6-F3+DIFF)*COEF)**2

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
        WRITE(LU,102) IELMF,SF%NAME
102     FORMAT(1X,'VC01PP (BIEF) :',/,
     &         1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &         1X,'REAL NAME OF F: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
