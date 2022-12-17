!                   *****************
                    SUBROUTINE VC13PP
!                   *****************
!
     &(XMUL,SF,F,X,Y,Z,SURFAC,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     & W1,W2,W3,W4,W5,W6,ICOORD,FORMUL)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)
!+
!+                       /            DF
!+    VEC(I)  =  XMUL   /     ( P  *( --  )) D(OMEGA)
!+                     /OMEGA    I    DX
!+
!+    P   IS A LINEAR BASE
!+     I
!+
!+    F IS A VECTOR OF TYPE P1 OR OTHER
!
!note     IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!+                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!+                     P1, BUT GIVEN BY ELEMENTS.
!+                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,3).
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH
!
!history  J-M HERVOUET (LNH)
!+        19/05/2010
!+        V6P0
!+   NEW FILTER FOR PARTLY CRUSHED ELEMENTS
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
!history  J-M HERVOUET (EDF R&D LNHE)
!+        07/01/2013
!+        V6P3
!+   X and Y are now given per element.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| SEE AT THE END OF THE SUBROUTINE
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
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
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC13PP => VC13PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!                                                               NPOIN
      DOUBLE PRECISION, INTENT(IN) :: X(NELMAX,6),Y(NELMAX,6),Z(*)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) ::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XS24,XS144,F1,F2,F3,F4,F5,F6,XMU
      DOUBLE PRECISION X2,X3,Y2,Y3,Z1,Z2,Z3,Z4,Z5,Z6
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,IELMF
!
      INTRINSIC MAX,MIN
!
!-----------------------------------------------------------------------
!
      XS24  = XMUL/24.D0
      XS144 = XMUL/144.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!=======================================================================
!
!     F IS LINEAR
!
      IF(IELMF.EQ.41) THEN
!
      IF(ICOORD.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!  DERIVATIVE WRT X
!
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
        Y2  =  Y(IELEM,2)
        Y3  =  Y(IELEM,3)
!
        Z2  =  Z(I2) - Z(I1)
        Z3  =  Z(I3) - Z(I1)
        Z4  =  Z(I4) - Z(I1)
        Z5  =  Z(I5) - Z(I1)
        Z6  =  Z(I6) - Z(I1)
!
      W1(IELEM)=( (2*F1-F6)*Y2*( Z5+3*Z4-3*Z3  -Z2)
     &           +(2*F1-F5)*Y3*(-Z6-3*Z4  +Z3+3*Z2)
     &                  +F2*Y3*(2*Z6+3*Z5+3*Z4-2*Z3)
     &                  +F3*Y2*(-3*Z6-2*Z5-3*Z4+2*Z2)
     &             +(F3-F6)*Y3*(Z5-Z4+2*Z2)
     &                  +F4*Y2*(3*Z6+Z5+3*Z3-Z2)
     &                  +F4*Y3*(-Z6-3*Z5+Z3-3*Z2)
     &             +(F5-F2)*Y2*(Z6-Z4+2*Z3) )*XS144
      W2(IELEM)=(   F1*Y2*(Z6+4*Z5+3*Z4-4*Z3-4*Z2)
     &             +F1*Y3*(-2*Z6-3*Z5-3*Z4+2*Z3+6*Z2)
     &      +(2*F2-F4)*Y3*(Z6+3*Z5-Z3)
     &             +F3*Y2*(-3*Z6-4*Z5-Z4+4*Z2)
     &             +F4*Y2*(2*Z6+2*Z5+Z3-2*Z2)
     &      +2*(F5-F2)*Y2*(Z6-Z4+2*Z3)
     &             +F5*Y3*(Z6+3*Z4-Z3-6*Z2)
     &             +F6*Y2*(-2*Z5-2*Z4+3*Z3+2*Z2)
     &        +(F6-F3)*Y3*(-Z5+Z4-2*Z2) )*XS144
      W3(IELEM)=(   F1*Y2*(3*Z6+2*Z5+3*Z4-6*Z3-2*Z2)
     &             +F1*Y3*(-4*Z6-Z5-3*Z4+4*Z3+4*Z2)
     &             +F2*Y3*(4*Z6+3*Z5+Z4-4*Z3)
     &      +(2*F3-F4)*Y2*(-3*Z6-Z5+Z2)
     &             +F4*Y3*(-2*Z6-2*Z5+2*Z3-Z2)
     &        +(F5-F2)*Y2*(Z6-Z4+2*Z3)
     &             +F5*Y3*(2*Z6+2*Z4-2*Z3-3*Z2)
     &             +F6*Y2*(-Z5-3*Z4+6*Z3+Z2)
     &      +2*(F6-F3)*Y3*(-Z5+Z4-2*Z2) )*XS144
      W4(IELEM)=(   F1*Y2*(-3*Z6+Z5+6*Z4-3*Z3-Z2)
     &             +F1*Y3*(-Z6+3*Z5-6*Z4+Z3+3*Z2)
     &             +F2*Y3*(Z6+3*Z5-Z3)
     &      +(2*F4-F3)*Y2*(3*Z6+Z5-Z2)
     &           +2*F4*Y3*(-Z6-3*Z5+Z3)
     &        +(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &             +F5*Y3*(2*Z6+6*Z4-2*Z3-3*Z2)
     &             +F6*Y2*(-2*Z5-6*Z4+3*Z3+2*Z2)
     &        +(F6-F3)*Y3*(-2*Z5+2*Z4-Z2)  )*XS144
      W5(IELEM)=(   F1*Y2*(-Z6+2*Z5+3*Z4-2*Z3-2*Z2)
     &             +F2*Y3*(Z6+6*Z5-3*Z4-Z3)
     &             +F3*Y2*(-3*Z6-2*Z5+Z4+2*Z2)
     &             +F4*Y2*(4*Z6+4*Z5-Z3-4*Z2)
     &             +F4*Y3*(-2*Z6-6*Z5+2*Z3+3*Z2)
     &      +2*(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &      +(2*F5-F1)*Y3*(Z6+3*Z4-Z3-3*Z2)
     &             +F6*Y2*(-4*Z5-4*Z4+3*Z3+4*Z2)
     &        +(F6-F3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144
      W6(IELEM)=(  +F1*Y3*(-2*Z6+Z5-3*Z4+2*Z3+2*Z2)
     &             +F2*Y3*(2*Z6+3*Z5-Z4-2*Z3)
     &             +F3*Y2*(-6*Z6-Z5+3*Z4+Z2)
     &             +F4*Y2*(6*Z6+2*Z5-3*Z3-2*Z2)
     &             +F4*Y3*(-4*Z6-4*Z5+4*Z3+Z2)
     &        +(F5-F2)*Y2*(2*Z6-2*Z4+Z3)
     &             +F5*Y3*(4*Z6+4*Z4-4*Z3-3*Z2)
     &      +(2*F6-F1)*Y2*(-Z5-3*Z4+3*Z3+Z2)
     &      +2*(F6-F3)*Y3*(-2*Z5+2*Z4-Z2) )*XS144
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!-----------------------------------------------------------------------
!
!  DERIVATIVE WRT Y
!
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
!       X2  =  X(I2) - X(I1)
!       X3  =  X(I3) - X(I1)
        X2  =  X(IELEM,2)
        X3  =  X(IELEM,3)
!
        Z2  =  Z(I2) - Z(I1)
        Z3  =  Z(I3) - Z(I1)
        Z4  =  Z(I4) - Z(I1)
        Z5  =  Z(I5) - Z(I1)
        Z6  =  Z(I6) - Z(I1)
!
      W1(IELEM)=( (2*F1-F6)*X2*(-Z5-3*Z4+3*Z3+Z2)
     &                +2*F1*X3*(Z6+3*Z4-Z3-3*Z2)
     &                  +F2*X3*(-2*Z6-3*Z5-3*Z4+2*Z3)
     &                  +F3*X2*(3*Z6+2*Z5+3*Z4-2*Z2)
     &                  +F4*X2*(-3*Z6-Z5-3*Z3+Z2)
     &                  +F4*X3*(Z6+3*Z5-Z3+3*Z2)
     &             +(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &                  +F5*X3*(-Z6-3*Z4+Z3+3*Z2)
     &             +(F6-F3)*X3*(Z5-Z4+2*Z2) )*XS144
      W2(IELEM)=( F1*X2*(-Z6-4*Z5-3*Z4+4*Z3+4*Z2)
     &           +F1*X3*(2*Z6+3*Z5+3*Z4-2*Z3-6*Z2)
     &    +(2*F2-F4)*X3*(-Z6-3*Z5+Z3)
     &           +F3*X2*(3*Z6+4*Z5+Z4-4*Z2)
     &           +F4*X2*(-2*Z6-2*Z5-Z3+2*Z2)
     &    +2*(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &           +F5*X3*(-Z6-3*Z4+Z3+6*Z2)
     &           +F6*X2*(2*Z5+2*Z4-3*Z3-2*Z2)
     &      +(F6-F3)*X3*(Z5-Z4+2*Z2) )*XS144
      W3(IELEM)=( F1*X2*(-3*Z6-2*Z5-3*Z4+6*Z3+2*Z2)
     &           +F1*X3*(4*Z6+Z5+3*Z4-4*Z3-4*Z2)
     &           +F2*X3*(-4*Z6-3*Z5-Z4+4*Z3)
     &    +(2*F3-F4)*X2*(3*Z6+Z5-Z2)
     &           +F4*X3*(2*Z6+2*Z5-2*Z3+Z2)
     &      +(F5-F2)*X2*(-Z6+Z4-2*Z3)
     &           +F5*X3*(-2*Z6-2*Z4+2*Z3+3*Z2)
     &           +F6*X2*(Z5+3*Z4-6*Z3-Z2)
     &    +2*(F6-F3)*X3*(Z5-Z4+2*Z2) )*XS144
      W4(IELEM)=( F1*X2*(3*Z6-Z5-6*Z4+3*Z3+Z2)
     &           +F1*X3*(Z6-3*Z5+6*Z4-Z3-3*Z2)
     &    +(2*F4-F3)*X2*(-3*Z6-Z5+Z2)
     &    +(2*F4-F2)*X3*(Z6+3*Z5-Z3)
     &      +(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &           +F5*X3*(-2*Z6-6*Z4+2*Z3+3*Z2)
     &           +F6*X2*(2*Z5+6*Z4-3*Z3-2*Z2)
     &      +(F6-F3)*X3*(2*Z5-2*Z4+Z2) )*XS144
      W5(IELEM)=( F1*X2*(Z6-2*Z5-3*Z4+2*Z3+2*Z2)
     &           +F2*X3*(-Z6-6*Z5+3*Z4+Z3)
     &           +F3*X2*(3*Z6+2*Z5-Z4-2*Z2)
     &           +F4*X2*(-4*Z6-4*Z5+Z3+4*Z2)
     &           +F4*X3*(2*Z6+6*Z5-2*Z3-3*Z2)
     &    +2*(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &    +(2*F5-F1)*X3*(-Z6-3*Z4+Z3+3*Z2)
     &           +F6*X2*(4*Z5+4*Z4-3*Z3-4*Z2)
     &      +(F6-F3)*X3*(2*Z5-2*Z4+Z2)  )*XS144
      W6(IELEM)=(+F1*X3*(2*Z6-Z5+3*Z4-2*Z3-2*Z2)
     &           +F2*X3*(-2*Z6-3*Z5+Z4+2*Z3)
     &           +F3*X2*(6*Z6+Z5-3*Z4-Z2)
     &           +F4*X2*(-6*Z6-2*Z5+3*Z3+2*Z2)
     &           +F4*X3*(4*Z6+4*Z5-4*Z3-Z2)
     &      +(F5-F2)*X2*(-2*Z6+2*Z4-Z3)
     &           +F5*X3*(-4*Z6-4*Z4+4*Z3+3*Z2)
     &    +(2*F6-F1)*X2*(Z5+3*Z4-3*Z3-Z2)
     &    +2*(F6-F3)*X3*(2*Z5-2*Z4+Z2)  )*XS144
!
!
        ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.3) THEN
!
!-----------------------------------------------------------------------
!
!  DERIVATIVE WRT Z
!
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
!       X2  =  X(I2) - X(I1)
!       X3  =  X(I3) - X(I1)
!       Y2  =  Y(I2) - Y(I1)
!       Y3  =  Y(I3) - Y(I1)
!
!       XMU  = XS48*(X2*Y3-X3*Y2)
        XMU  = XS24*SURFAC(IELEM)
!
!       NOT LUMPED VERSION
!       DIFF = (F4+F5+F6) - (F1+F2+F3)
!       W1(IELEM)=(F4-F1+DIFF)*XMU
!       W2(IELEM)=(F5-F2+DIFF)*XMU
!       W3(IELEM)=(F6-F3+DIFF)*XMU
!       LUMPED VERSION (LIKE THE DIFFUSION MATRIX)
!       SEE W COMPUTATION IN TELEMAC-3D IN PROVEL
        W1(IELEM)=4*(F4-F1)*XMU
        W2(IELEM)=4*(F5-F2)*XMU
        W3(IELEM)=4*(F6-F3)*XMU
!
        W4(IELEM)=W1(IELEM)
        W5(IELEM)=W2(IELEM)
        W6(IELEM)=W3(IELEM)
!
      ENDDO ! IELEM
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,201) ICOORD
201     FORMAT(1X,'VC13PP (BIEF) : IMPOSSIBLE COMPONENT ',
     &            1I6,' CHECK ICOORD')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!=======================================================================
!
      ELSE
!
!=======================================================================
!
        WRITE(LU,102) IELMF,SF%NAME
102     FORMAT(1X,'VC13PP (BIEF) :',/,
     &         1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &         1X,'REAL NAME OF F: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!=======================================================================
!
!     HYDROSTATIC INCONSISTENCIES
!
!     COMMON TREATMENT FOR FILTERS 2,3 AND 4
!
      IF(FORMUL(6:6).EQ.'2'.OR.FORMUL(6:6).EQ.'3'.OR.
     &   FORMUL(6:6).EQ.'4'     ) THEN
!
        DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6))) THEN
            W1(IELEM)=0.D0
            W2(IELEM)=0.D0
            W3(IELEM)=0.D0
            W4(IELEM)=0.D0
            W5(IELEM)=0.D0
            W6(IELEM)=0.D0
          ENDIF
!
        ENDDO
!
      ENDIF
!
!     FILTER 3
!
      IF(FORMUL(6:6).EQ.'3'.AND.(ICOORD.EQ.1.OR.ICOORD.EQ.2)) THEN
!
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
!         IF THERE IS A POSSIBILITY OF STRATIFICATION
!         GRADIENTS CANCELLED
!
          IF( MIN(MAX(F1,F4),MAX(F2,F5),MAX(F3,F6)).GE.
     &        MAX(MIN(F1,F4),MIN(F2,F5),MIN(F3,F6))     ) THEN
            W1(IELEM)=0.D0
            W2(IELEM)=0.D0
            W3(IELEM)=0.D0
            W4(IELEM)=0.D0
            W5(IELEM)=0.D0
            W6(IELEM)=0.D0
          ENDIF
!
        ENDDO
!
      ENDIF
!
!     FILTER 4
!
      IF(FORMUL(6:6).EQ.'4'.AND.(ICOORD.EQ.1.OR.ICOORD.EQ.2)) THEN
!
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
          Z1 = Z(I1)
          Z2 = Z(I2)
          Z3 = Z(I3)
          Z4 = Z(I4)
          Z5 = Z(I5)
          Z6 = Z(I6)
!
!         CHECKS IF A STRATIFICATION IS IMPOSSIBLE
!         IN THIS CASE (GO TO 1000) GRADIENTS ARE KEPT
!
!         1 IN BETWEEN 3 AND 6
          IF(Z1.GE.Z3.AND.Z1.LE.Z6) THEN
            IF(F1.LT.MIN(F3,F6).OR.F1.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
!         1 IN BETWEEN 2 AND 5
          IF(Z1.GE.Z2.AND.Z1.LE.Z5) THEN
            IF(F1.LT.MIN(F2,F5).OR.F1.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
!         2 IN BETWEEN 1 AND 4
          IF(Z2.GE.Z1.AND.Z2.LE.Z4) THEN
            IF(F2.LT.MIN(F1,F4).OR.F2.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
!         2 IN BETWEEN 3 AND 6
          IF(Z2.GE.Z3.AND.Z2.LE.Z6) THEN
            IF(F2.LT.MIN(F3,F6).OR.F2.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
!         3 IN BETWEEN 1 AND 4
          IF(Z3.GE.Z1.AND.Z3.LE.Z4) THEN
            IF(F3.LT.MIN(F1,F4).OR.F3.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
!         3 IN BETWEEN 2 AND 5
          IF(Z3.GE.Z2.AND.Z3.LE.Z5) THEN
            IF(F3.LT.MIN(F2,F5).OR.F3.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
!         4 IN BETWEEN 2 AND 5
          IF(Z4.GE.Z2.AND.Z4.LE.Z5) THEN
            IF(F4.LT.MIN(F2,F5).OR.F4.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
!         4 IN BETWEEN 3 AND 6
          IF(Z4.GE.Z3.AND.Z4.LE.Z6) THEN
            IF(F4.LT.MIN(F3,F6).OR.F4.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
!         5 IN BETWEEN 1 AND 4
          IF(Z5.GE.Z1.AND.Z5.LE.Z4) THEN
            IF(F5.LT.MIN(F1,F4).OR.F5.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
!         5 IN BETWEEN 3 AND 6
          IF(Z5.GE.Z3.AND.Z5.LE.Z6) THEN
            IF(F5.LT.MIN(F3,F6).OR.F5.GT.MAX(F3,F6)) GO TO 1000
          ENDIF
!         6 IN BETWEEN 1 AND 4
          IF(Z6.GE.Z1.AND.Z6.LE.Z4) THEN
            IF(F6.LT.MIN(F1,F4).OR.F6.GT.MAX(F1,F4)) GO TO 1000
          ENDIF
!         6 IN BETWEEN 2 AND 5
          IF(Z6.GE.Z2.AND.Z6.LE.Z5) THEN
            IF(F6.LT.MIN(F2,F5).OR.F6.GT.MAX(F2,F5)) GO TO 1000
          ENDIF
!
!         SO THERE IS A POSSIBILITY OF STRATIFICATION
!         GRADIENTS CANCELLED
!
          W1(IELEM)=0.D0
          W2(IELEM)=0.D0
          W3(IELEM)=0.D0
          W4(IELEM)=0.D0
          W5(IELEM)=0.D0
          W6(IELEM)=0.D0
!
1000      CONTINUE
!
        ENDDO
!
      ENDIF
!
!     FILTER FOR PARTLY CRUSHED PRISMS
!
      IF(FORMUL(7:7).EQ.'2') THEN
!
        DO IELEM = 1 , NELEM
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
          IF(Z(I4)-Z(I1).LT.1.D-3.OR.
     &       Z(I5)-Z(I2).LT.1.D-3.OR.
     &       Z(I6)-Z(I3).LT.1.D-3     ) THEN
            W1(IELEM)=0.D0
            W2(IELEM)=0.D0
            W3(IELEM)=0.D0
            W4(IELEM)=0.D0
            W5(IELEM)=0.D0
            W6(IELEM)=0.D0
          ENDIF
        ENDDO
!
      ENDIF
!
!=======================================================================
!
      RETURN
      END
