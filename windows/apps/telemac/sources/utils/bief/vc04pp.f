!                   *****************
                    SUBROUTINE VC04PP
!                   *****************
!
     &( XMUL,SU,SV,SW,U,V,W,F,H,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     &  W1,W2,W3,W4,W5,W6,SPECAD,FORMUL,NELEM2)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+     IF FORMUL ENDS WITH 'HOR'
!+     IN THE TRANSFORMED MESH
!+
!+                 /            D(PSII*)           D(PSII*)
!+     V  = XMUL  /    H * U * -------- +  H * V * --------   D(OMEGA*)
!+      I        /OMEGA*          DX                 DY
!+
!+     BEWARE : TRANSFORMED MESH HERE !!!!
!+
!+
!+     IF FORMUL ENDS WITH 'TOT'
!+     IN THE REAL MESH
!+
!+                 /     ->  --->
!+     V  = XMUL  /      U * GRAD(PSI)   D(OMEGA*)
!+      I        /OMEGA
!+
!+     BEWARE : REAL MESH HERE !!!!
!+
!+     PSII IS OF TYPE P1 PRISM
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!warning  IF SPECAD=.TRUE., THE ADVECTING FIELD IS NOT ONLY
!+        U AND V BUT U+DM1*GRAD(ZCONV). ZCONV IS HERE G, DM1 IS F
!+        GRAD(ZCONV) IS HERE H, DM1 IS F
!+
!warning  VERSION 6.1: WITH MASS-LUMPING ON THE VERTICAL
!
!history  J-M HERVOUET (LNHE)
!+        21/06/06
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        07/09/2011
!+        V6P2
!+   Case SPECAD=.TRUE. differently treated, with gradient of ZCONV
!+   now given as argument.
!
!history  J-M HERVOUET (EDF R&D LNHE)
!+        07/01/2013
!+        V6P3
!+   X and Y are now given per element.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| STRING WITH FORMULA OF VECTOR
!| IKLE1          |-->| FIRST POINT OF PRISMS
!| IKLE2          |-->| SECOND POINT OF PRISMS
!| IKLE3          |-->| THIRD POINT OF PRISMS
!| IKLE4          |-->| FOURTH POINT OF PRISMS
!| IKLE5          |-->| FIFTH POINT OF PRISMS
!| IKLE6          |-->| SIXTH POINT OF PRISMS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE MESH OF PRISMS
!| SPECAD         |-->| IF YES, SPECIAL ADVECTION FIELD, SEE ABOVE
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
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH, PER POINT !!!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC04PP => VC04PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NELEM2
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)   ::X(NELMAX,6),Y(NELMAX,6),Z(*)
      DOUBLE PRECISION, INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT)::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   ::XMUL
!
      LOGICAL, INTENT(IN) :: SPECAD
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!     STRUCTURES AND THEIR REAL DATA
!
      TYPE(BIEF_OBJ),   INTENT(IN) :: SU,SV,SW
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*),F(*)
      DOUBLE PRECISION, INTENT(IN) :: H(NELEM2,2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION SUR144,X1,X2,X3,Y1,Y2,Y3,H1,H2,H3,SHT
      DOUBLE PRECISION HU1,HU2,HUINF,HUSUP,HV1,HV2,HVINF,HVSUP
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION Q1,Q2,Q3,Q4,Q5,Q6,Z2,Z3,Z4,Z5,Z6
!     DOUBLE PRECISION GRADGX,GRADGY,DET
      INTEGER I1,I2,I3,I4,I5,I6,IELEM2
      INTEGER IELEM,IELMU,IELMV,IELMW
!
!**********************************************************************
!
      SUR144 = XMUL / 144.D0
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
!
!-----------------------------------------------------------------------
!
!   TERMES HORIZONTAUX
!
      IF(FORMUL(14:16).EQ.'HOR') THEN
!
!   BOUCLE SUR LES ELEMENTS
!
        IF(IELMU.EQ.41.AND.IELMV.EQ.41) THEN
!
!-----------------------------------------------------------------------
!
!  U ET V DISCRETISEES EN PRISME P1 :
!
          IF(.NOT.SPECAD) THEN
!
!     STANDARD CASE
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
              H1 = Z(I4) - Z(I1)
              H2 = Z(I5) - Z(I2)
              H3 = Z(I6) - Z(I3)
              SHT = H1 + H2 + H3
!
              HU1 = (U(I1)+U(I2)+U(I3))*SHT + H1*U(I1) + H2*U(I2)
     &            + H3*U(I3)
              HU2 = (U(I4)+U(I5)+U(I6))*SHT + H1*U(I4) + H2*U(I5)
     &            + H3*U(I6)
              HUINF = (HU1+HU1+HU2) * SUR144
              HUSUP = (HU1+HU2+HU2) * SUR144
!
              HV1 = (V(I1)+V(I2)+V(I3))*SHT + H1*V(I1) + H2*V(I2)
     &            + H3*V(I3)
              HV2 = (V(I4)+V(I5)+V(I6))*SHT + H1*V(I4) + H2*V(I5)
     &            + H3*V(I6)
              HVINF = (HV1+HV1+HV2) * SUR144
              HVSUP = (HV1+HV2+HV2) * SUR144
!
!             Y1 = Y(I2) - Y(I3)
!             Y2 = Y(I3) - Y(I1)
!             Y3 = Y(I1) - Y(I2)
              Y1 = Y(IELEM,2) - Y(IELEM,3)
              Y2 = Y(IELEM,3)
              Y3 =            - Y(IELEM,2)
!
!             X1 = X(I3) - X(I2)
!             X2 = X(I1) - X(I3)
!             X3 = X(I2) - X(I1)
              X1 = X(IELEM,3) - X(IELEM,2)
              X2 =            - X(IELEM,3)
              X3 = X(IELEM,2)
!
              W1(IELEM) = Y1*HUINF + X1*HVINF
              W2(IELEM) = Y2*HUINF + X2*HVINF
              W3(IELEM) = Y3*HUINF + X3*HVINF
              W4(IELEM) = Y1*HUSUP + X1*HVSUP
              W5(IELEM) = Y2*HUSUP + X2*HVSUP
              W6(IELEM) = Y3*HUSUP + X3*HVSUP
!
            ENDDO
!
          ELSE
!
!       CASE WITH SPECIFIC ADVECTING FIELD
!
            DO IELEM = 1 , NELEM
!
!             CORRESPONDING 2D ELEMENT ON THE VERTICAL
!             SEE NUMBERING OF PRISMS
!
              IELEM2 = MOD(IELEM-1,NELEM2) + 1
!
              I1 = IKLE1(IELEM)
              I2 = IKLE2(IELEM)
              I3 = IKLE3(IELEM)
              I4 = IKLE4(IELEM)
              I5 = IKLE5(IELEM)
              I6 = IKLE6(IELEM)
!
              H1 = Z(I4) - Z(I1)
              H2 = Z(I5) - Z(I2)
              H3 = Z(I6) - Z(I3)
              SHT = H1 + H2 + H3
!
              U1=U(I1)+F(I1)*H(IELEM2,1)
              U2=U(I2)+F(I2)*H(IELEM2,1)
              U3=U(I3)+F(I3)*H(IELEM2,1)
              U4=U(I4)+F(I4)*H(IELEM2,1)
              U5=U(I5)+F(I5)*H(IELEM2,1)
              U6=U(I6)+F(I6)*H(IELEM2,1)
              V1=V(I1)+F(I1)*H(IELEM2,2)
              V2=V(I2)+F(I2)*H(IELEM2,2)
              V3=V(I3)+F(I3)*H(IELEM2,2)
              V4=V(I4)+F(I4)*H(IELEM2,2)
              V5=V(I5)+F(I5)*H(IELEM2,2)
              V6=V(I6)+F(I6)*H(IELEM2,2)
!
              HU1 = (U1+U2+U3)*SHT + H1*U1 + H2*U2 + H3*U3
              HU2 = (U4+U5+U6)*SHT + H1*U4 + H2*U5 + H3*U6
              HUINF = (HU1+HU1+HU2) * SUR144
              HUSUP = (HU1+HU2+HU2) * SUR144
!
              HV1 = (V1+V2+V3)*SHT + H1*V1 + H2*V2 + H3*V3
              HV2 = (V4+V5+V6)*SHT + H1*V4 + H2*V5 + H3*V6
              HVINF = (HV1+HV1+HV2) * SUR144
              HVSUP = (HV1+HV2+HV2) * SUR144
!
!
!             Y1 = Y(I2) - Y(I3)
!             Y2 = Y(I3) - Y(I1)
!             Y3 = Y(I1) - Y(I2)
              Y1 = Y(IELEM,2) - Y(IELEM,3)
              Y2 = Y(IELEM,3)
              Y3 =            - Y(IELEM,2)
!
!             X1 = X(I3) - X(I2)
!             X2 = X(I1) - X(I3)
!             X3 = X(I2) - X(I1)
              X1 = X(IELEM,3) - X(IELEM,2)
              X2 =            - X(IELEM,3)
              X3 = X(IELEM,2)
!
              W1(IELEM) = Y1*HUINF + X1*HVINF
              W2(IELEM) = Y2*HUINF + X2*HVINF
              W3(IELEM) = Y3*HUINF + X3*HVINF
              W4(IELEM) = Y1*HUSUP + X1*HVSUP
              W5(IELEM) = Y2*HUSUP + X2*HVSUP
              W6(IELEM) = Y3*HUSUP + X3*HVSUP
!
            ENDDO
!
          ENDIF
!
!     ELSEIF(IELMU.EQ.  ) THEN
!
!-----------------------------------------------------------------------
!
        ELSE
!
!-----------------------------------------------------------------------
!
          WRITE(LU,102) IELMU,SU%NAME
102       FORMAT(1X,'VC04PP (BIEF) :',/,
     &        1X,'DISCRETISATION OF U ET V : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF U : ',A6)
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(14:16).EQ.'TOT') THEN
!
!     TERMES HORIZONTAUX
!
!     BOUCLE SUR LES ELEMENTS
!
!     HERE SIGMAG NOT TAKEN INTO ACCOUNT (IT WOULD BE TO IMPLEMENT IF A
!     COMPATIBILITY WITH 2D CONTINUITY EQUATION REQUIRED)
!
        IF(IELMU.EQ.41.AND.
     &     IELMV.EQ.41.AND.
     &     IELMW.EQ.41) THEN
!
!-----------------------------------------------------------------------
!
!     VITESSE DISCRETISEE EN PRISME P1 :
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
!           X2=X(I2)-X(I1)
!           X3=X(I3)-X(I1)
!           Y2=Y(I2)-Y(I1)
!           Y3=Y(I3)-Y(I1)
!
            X2=X(IELEM,2)
            X3=X(IELEM,3)
            Y2=Y(IELEM,2)
            Y3=Y(IELEM,3)
!
            Z2=Z(I2)-Z(I1)
            Z3=Z(I3)-Z(I1)
            Z4=Z(I4)-Z(I1)
            Z5=Z(I5)-Z(I1)
            Z6=Z(I6)-Z(I1)
!
            U1=U(I1)
            U2=U(I2)
            U3=U(I3)
            U4=U(I4)
            U5=U(I5)
            U6=U(I6)
!
            V1=V(I1)
            V2=V(I2)
            V3=V(I3)
            V4=V(I4)
            V5=V(I5)
            V6=V(I6)
!
            Q1=W(I1)
            Q2=W(I2)
            Q3=W(I3)
            Q4=W(I4)
            Q5=W(I5)
            Q6=W(I6)
!
!           DERIVATIVE IN Z
!
            W1(IELEM) = (-3*Q6-3*Q5-3*Q2-3*Q3-6*Q4-6*Q1)*(Y3*X2-X3*Y2)
            W2(IELEM) = (-3*Q6-6*Q5-6*Q2-3*Q3-3*Q4-3*Q1)*(Y3*X2-X3*Y2)
            W3(IELEM) = (-6*Q6-3*Q1-3*Q4-6*Q3-3*Q2-3*Q5)*(Y3*X2-X3*Y2)
            W4(IELEM) = - W1(IELEM)
            W5(IELEM) = - W2(IELEM)
            W6(IELEM) = - W3(IELEM)
!
!           DERIVATIVE IN X
!
            W1(IELEM) = W1(IELEM) +
     &         (-2*Z2*U1+3*Z6*U3+Z6*U2-4*Z2*U2-Z6*U5+Z5*U4+3*Z4*U5+4*Z5*
     &U2-2*Z2*U5-2*Z3*U5+6*Z4*U4-3*Z6*U4-3*Z3*U6-6*Z3*U1-3*Z3*U4+2*Z5*U3
     &+2*Z5*U5+6*Z4*U1+3*Z4*U2+Z5*U6-2*Z2*U3-4*Z3*U2-Z2*U4-Z2*U6+3*Z4*U3
     &+3*Z4*U6+2*Z5*U1-6*Z3*U3)*Y2+(6*Z2*U1-4*Z6*U3-2*Z6*U6-2*Z6*U2+6*Z2
     &*U2-Z6*U5+3*Z5*U4-3*Z4*U5-3*Z5*U2+3*Z2*U5+Z3*U5-6*Z4*U4-Z6*U4+2*Z3
     &*U6+2*Z3*U1+Z3*U4-Z5*U3-6*Z4*U1-3*Z4*U2+Z5*U6+4*Z2*U3+2*Z3*U2+3*Z2
     &*U4+2*Z2*U6-3*Z4*U3-3*Z4*U6+4*Z3*U3-2*Z6*U1)*Y3
            W2(IELEM) = W2(IELEM) +
     &         (Z4*U3-4*Z3*U2+2*Z4*U2-2*Z6*U4+2*Z4*U6-2*Z3*U3-Z6*U3-2*Z6
     &*U6-2*Z3*U5+2*Z4*U4-2*Z3*U1+Z4*U1-Z3*U6-Z6*U1-2*Z6*U2-4*Z6*U5+4*Z4
     &*U5-Z3*U4)*Y2+(Z4*U3-Z4*U6-2*Z3*U2+6*Z5*U5+4*Z6*U3+2*Z6*U2-2*Z3*U6
     &+3*Z5*U3+3*Z5*U4-Z3*U4+6*Z5*U2+3*Z4*U1+Z6*U4-Z3*U5+2*Z6*U6-4*Z3*U3
     &+2*Z6*U1+3*Z5*U1+3*Z5*U6+Z6*U5-2*Z3*U1-3*Z4*U5)*Y3
            W3(IELEM) = W3(IELEM) +
     &         (-6*Z6*U6-2*Z5*U1-3*Z6*U1-Z5*U6+2*Z2*U5+4*Z2*U2+Z2*U4-2*Z
     &5*U5+2*Z2*U1-2*Z5*U3-3*Z6*U2+3*Z4*U6+Z2*U6-6*Z6*U3-4*Z5*U2+2*Z2*U3
     &-Z5*U4+Z4*U5-Z4*U2-3*Z4*U1-3*Z6*U5-3*Z6*U4)*Y2+(-2*Z4*U3-Z4*U2-4*Z
     &4*U6+2*Z2*U2+2*Z5*U3+4*Z2*U3-2*Z4*U4+2*Z5*U4+2*Z2*U6+Z5*U1+2*Z2*U1
     &-Z4*U1+Z5*U2+Z2*U4+Z2*U5+2*Z5*U5+4*Z5*U6-2*Z4*U5)*Y3
            W4(IELEM) = W4(IELEM) +
     &         (Z3*U2-2*Z2*U6+4*Z5*U5-2*Z2*U2+6*Z6*U6+3*Z6*U1+2*Z6*U2-Z3
     &*U5+2*Z5*U6-2*Z2*U4+2*Z5*U2-3*Z3*U6-Z2*U3+3*Z6*U3-Z2*U1+2*Z5*U4+6*
     &Z6*U4+3*Z3*U1+Z5*U3+4*Z6*U5-4*Z2*U5+Z5*U1)*Y2+(Z3*U2-4*Z5*U6-2*Z6*
     &U3-2*Z5*U3-6*Z5*U5-6*Z5*U4-Z6*U1+4*Z3*U6-Z2*U3+2*Z3*U3-2*Z6*U4+2*Z
     &3*U4-4*Z6*U6+2*Z3*U5+3*Z2*U5-2*Z6*U5+Z2*U6-3*Z5*U2-Z6*U2-3*Z2*U1+Z
     &3*U1-3*Z5*U1)*Y3
            W5(IELEM) = W5(IELEM) +
     &         (2*Z6*U6+Z6*U1-Z4*U1-2*Z4*U6-2*Z4*U4+Z3*U4+2*Z3*U3+2*Z3*U
     &5+4*Z3*U2+Z3*U6-2*Z4*U2+Z6*U3+2*Z6*U4-4*Z4*U5+2*Z6*U2+2*Z3*U1+4*Z6
     &*U5-Z4*U3)*Y2+(2*Z4*U3-Z3*U1-Z3*U2-6*Z2*U2+2*Z6*U3+4*Z4*U6+Z6*U1+3
     &*Z4*U2+6*Z4*U4-4*Z3*U6-3*Z2*U3+3*Z4*U1+2*Z6*U4-2*Z3*U4+6*Z4*U5-2*Z
     &3*U3+Z6*U2-3*Z2*U4-6*Z2*U5+2*Z6*U5-2*Z3*U5+4*Z6*U6-3*Z2*U6-3*Z2*U1
     &)*Y3
            W6(IELEM) = W6(IELEM) +
     &         (3*Z3*U5+2*Z2*U4-2*Z5*U2+3*Z3*U2-Z5*U1-2*Z5*U6+Z2*U1-2*Z4
     &*U2+2*Z2*U2+4*Z2*U5-Z5*U3-6*Z4*U6+2*Z2*U6+3*Z3*U4+6*Z3*U3+6*Z3*U6+
     &Z2*U3-6*Z4*U4+3*Z3*U1-2*Z5*U4-4*Z4*U5-3*Z4*U1-4*Z5*U5-3*Z4*U3)*Y2+
     &(2*Z4*U3-Z5*U1+4*Z4*U6-2*Z2*U2-2*Z5*U3+Z4*U2+Z4*U1-2*Z5*U4-4*Z2*U3
     &-Z5*U2-4*Z5*U6-2*Z5*U5-2*Z2*U6-2*Z2*U1-Z2*U5-Z2*U4+2*Z4*U5+2*Z4*U4
     &)*Y3
!
!            DERIVATIVE IN Y
!
            W1(IELEM) = W1(IELEM) +
     &         (-4*Z5*V2-3*Z4*V5-Z5*V6+Z6*V5+2*Z3*V5+4*Z2*V2+3*Z3*V6+Z2*
     &V4+4*Z3*V2-2*Z5*V3-Z6*V2+Z2*V6+6*Z3*V1-6*Z4*V1-2*Z5*V5+6*Z3*V3+2*Z
     &2*V3-3*Z4*V3-2*Z5*V1+3*Z3*V4-6*Z4*V4-3*Z4*V6+3*Z6*V4-3*Z6*V3-Z5*V4
     &+2*Z2*V1+2*Z2*V5-3*Z4*V2)*X2+(3*Z5*V2+3*Z4*V5-Z5*V6+Z6*V5-Z3*V5-6*
     &Z2*V2-2*Z3*V6-3*Z2*V4-2*Z3*V2+Z5*V3+2*Z6*V2-2*Z2*V6-2*Z3*V1+6*Z4*V
     &1-4*Z3*V3-4*Z2*V3+3*Z4*V3+2*Z6*V6-Z3*V4+6*Z4*V4+3*Z4*V6+Z6*V4+4*Z6
     &*V3-3*Z5*V4-6*Z2*V1+2*Z6*V1-3*Z2*V5+3*Z4*V2)*X3
            W2(IELEM) = W2(IELEM) +
     &         (-2*Z4*V6+2*Z6*V6+Z6*V3+2*Z6*V4-Z4*V3-Z4*V1+2*Z3*V1+Z3*V4
     &-4*Z4*V5+4*Z6*V5+2*Z6*V2+2*Z3*V3+Z3*V6+4*Z3*V2-2*Z4*V4+2*Z3*V5-2*Z
     &4*V2+Z6*V1)*X2+(-3*Z5*V1-6*Z5*V2+2*Z3*V1-3*Z5*V4+2*Z3*V6+4*Z3*V3+Z
     &3*V4+Z3*V5-6*Z5*V5-3*Z4*V1+3*Z4*V5-2*Z6*V2-Z6*V5-Z4*V3-2*Z6*V1-4*Z
     &6*V3-3*Z5*V6-Z6*V4+Z4*V6+2*Z3*V2-3*Z5*V3-2*Z6*V6)*X3
            W3(IELEM) = W3(IELEM) +
     &         (-2*Z2*V5+Z5*V4+6*Z6*V3+6*Z6*V6+3*Z6*V5-2*Z2*V1+3*Z6*V4+2
     &*Z5*V5+Z5*V6+3*Z4*V1-2*Z2*V3-Z4*V5+3*Z6*V1-Z2*V6-3*Z4*V6+2*Z5*V3-4
     &*Z2*V2-Z2*V4+3*Z6*V2+4*Z5*V2+Z4*V2+2*Z5*V1)*X2+(-4*Z5*V6-2*Z2*V2-2
     &*Z2*V1-Z2*V4-Z5*V1+Z4*V1+Z4*V2+2*Z4*V4-2*Z5*V5+4*Z4*V6-Z2*V5-2*Z2*
     &V6+2*Z4*V5-2*Z5*V4-4*Z2*V3-Z5*V2+2*Z4*V3-2*Z5*V3)*X3
            W4(IELEM) = W4(IELEM) +
     &         (-2*Z5*V4-6*Z6*V6-2*Z5*V6+4*Z2*V5-4*Z5*V5-4*Z6*V5+2*Z2*V2
     &+Z2*V3-6*Z6*V4+2*Z2*V6+3*Z3*V6-Z5*V1-2*Z6*V2-Z5*V3-3*Z6*V3+Z2*V1+2
     &*Z2*V4-3*Z6*V1-Z3*V2-2*Z5*V2-3*Z3*V1+Z3*V5)*X2+(3*Z5*V1+2*Z6*V5+3*
     &Z2*V1-Z3*V2-Z3*V1+2*Z6*V4-4*Z3*V6-2*Z3*V3+Z6*V2-2*Z3*V4+2*Z6*V3+6*
     &Z5*V5+Z2*V3-Z2*V6+2*Z5*V3+Z6*V1+4*Z5*V6+6*Z5*V4-2*Z3*V5+3*Z5*V2+4*
     &Z6*V6-3*Z2*V5)*X3
            W5(IELEM) = W5(IELEM) +
     &         (Z4*V3-Z6*V1-2*Z6*V6+4*Z4*V5-2*Z3*V3-Z6*V3+2*Z4*V4-Z3*V4+
     &2*Z4*V6-2*Z6*V4-4*Z6*V5-2*Z3*V1-Z3*V6-4*Z3*V2-2*Z6*V2-2*Z3*V5+2*Z4
     &*V2+Z4*V1)*X2+(-2*Z6*V5+3*Z2*V1-4*Z4*V6+Z3*V1-2*Z4*V3+4*Z3*V6+2*Z3
     &*V3+2*Z3*V4-6*Z4*V5-3*Z4*V1-3*Z4*V2-6*Z4*V4+3*Z2*V4-2*Z6*V3+6*Z2*V
     &2+3*Z2*V3-4*Z6*V6+3*Z2*V6-Z6*V1-Z6*V2+6*Z2*V5-2*Z6*V4+2*Z3*V5+Z3*V
     &2)*X3
            W6(IELEM) = W6(IELEM) +
     &         (6*Z4*V6-2*Z2*V2+2*Z5*V4-4*Z2*V5-6*Z3*V3+2*Z5*V6-Z2*V1+6*
     &Z4*V4+3*Z4*V1+3*Z4*V3+Z5*V1-3*Z3*V1-3*Z3*V4-2*Z2*V6-6*Z3*V6+Z5*V3-
     &Z2*V3-2*Z2*V4+4*Z5*V5-3*Z3*V2+2*Z5*V2-3*Z3*V5+2*Z4*V2+4*Z4*V5)*X2+
     &(2*Z5*V4+2*Z2*V1+Z5*V1-Z4*V2-2*Z4*V4+2*Z5*V5-Z4*V1+4*Z5*V6+2*Z2*V2
     &-2*Z4*V5+2*Z5*V3+Z5*V2+4*Z2*V3+Z2*V5+Z2*V4+2*Z2*V6-4*Z4*V6-2*Z4*V3
     &)*X3
!
            W1(IELEM) = W1(IELEM)*SUR144
            W2(IELEM) = W2(IELEM)*SUR144
            W3(IELEM) = W3(IELEM)*SUR144
            W4(IELEM) = W4(IELEM)*SUR144
            W5(IELEM) = W5(IELEM)*SUR144
            W6(IELEM) = W6(IELEM)*SUR144
!
          ENDDO
!
!-----------------------------------------------------------------------
!
!     ELSEIF(IELMU.EQ.  ) THEN
!
!-----------------------------------------------------------------------
!
        ELSE
!
!-----------------------------------------------------------------------
!
          WRITE(LU,302) IELMU,SU%NAME
302       FORMAT(1X,'VC04PP (BIEF) :',/,
     &         1X,'DISCRETISATION OF U,V,W : ',1I6,' NOT IMPLEMENTED',/,
     &         1X,'REAL NAME OF U : ',A6)
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,202) FORMUL
202     FORMAT(1X,'VC04PP (BIEF) :',/,
     &         1X,'HOR OR TOT LACKING AT THE END OF THE FORMULA : ',A16)
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
