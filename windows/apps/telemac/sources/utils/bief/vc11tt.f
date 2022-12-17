!                   *****************
                    SUBROUTINE VC11TT
!                   *****************
!
     &( XMUL,SF,SG,F,G,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     &  W1,W2,W3,W4,ICOORD )
!
!***********************************************************************
! BIEF   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)
!+
!+                       /            DF
!+    VEC(I)  =  XMUL   /  ( G  P  *( --  )) D(OMEGA)
!+                     /OMEGA    I    DX
!+
!+
!+    P   IS A LINEAR BASE
!+     I
!+
!+    F IS A VECTOR OF TYPE P1 OR OTHER
!
!note     IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!+
!+               HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!+                     P1, BUT GIVEN BY ELEMENTS.
!+
!+               THE SIZE OF F SHOULD THEN BE : F(NELMAX,4).
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH
!
!history  ARNAUD DESITTER - UNIVERSITY OF BRISTOL
!+        **/04/98
!+
!+
!
!history  J-M HERVOUET (LNH)
!+        25/03/02
!+        V5P3
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
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINT OF TETRAHEDRA
!| IKLE2          |-->| SECOND POINT OF TETRAHEDRA
!| IKLE3          |-->| THIRD POINT OF TETRAHEDRA
!| IKLE4          |-->| FOURTH POINT OF TETRAHEDRA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
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
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*),XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
!
!     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! LOCAL VARIABLES
!
      INTEGER IELEM,IELMF,IELMG
      DOUBLE PRECISION F1,F2,F3,F4
      DOUBLE PRECISION G1,G2,G3,G4,X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      INTEGER I1,I2,I3,I4
!
      DOUBLE PRECISION XSUR120,F2MF1,F3MF1,F4MF1,G2MG1,G3MG1,G4MG1
!
!-----------------------------------------------------------------------
! INITIALISES
!
      XSUR120 = XMUL/120.D0
!
      IELMF = SF%ELM
      IELMG = SG%ELM
!
!-----------------------------------------------------------------------
!     F AND G ARE LINEAR
!
      IF ((IELMF.EQ.31.AND.((IELMG.EQ.31).OR.(IELMG.EQ.30))).OR.
     &    (IELMF.EQ.51.AND.IELMG.EQ.51)     ) THEN
!
        IF (ICOORD.EQ.1) THEN
!
!-----------------------------------------------------------------------
!  DERIVATIVE WRT X
!
          DO  IELEM = 1 , NELEM
!
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
!
            F1 = F(I1)
            F2 = F(I2)
            F3 = F(I3)
            F4 = F(I4)
!
            IF (IELMG.EQ.31) THEN
              G1 = G(I1)
              G2 = G(I2)
              G3 = G(I3)
              G4 = G(I4)
            ELSE
              G1 = G(IELEM)
              G2 = G1
              G3 = G1
              G4 = G1
            ENDIF

!
            F2MF1 = F2-F1
            F3MF1 = F3-F1
            F4MF1 = F4-F1
            G2MG1 = G2-G1
            G3MG1 = G3-G1
            G4MG1 = G4-G1
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
            Y2  =  Y(I2) - Y(I1)
            Y3  =  Y(I3) - Y(I1)
            Y4  =  Y(I4) - Y(I1)
            Z2  =  Z(I2) - Z(I1)
            Z3  =  Z(I3) - Z(I1)
            Z4  =  Z(I4) - Z(I1)
!
            W1(IELEM) = (
     & (5*F2MF1*G1+F2MF1*G2MG1+F2MF1*G3MG1+F2MF1*G4MG1)*(Y3*Z4-Y4*Z3)
     &+(5*F3MF1*G1+F3MF1*G2MG1+F3MF1*G3MG1+F3MF1*G4MG1)*(Z2*Y4-Y2*Z4)
     &+(5*F4MF1*G1+F4MF1*G2MG1+F4MF1*G3MG1+F4MF1*G4MG1)*(Y2*Z3-Z2*Y3)
     &               ) * XSUR120
!
            W2(IELEM) = (
     &-F4MF1*Z2*Y3*G4MG1+F4MF1*Y2*Z3*G4MG1+F3MF1*Z2*Y4*G4MG1
     &-F3MF1*Y2*Z4*G4MG1+F2MF1*Y3*Z4*G4MG1-F2MF1*Y4*Z3*G4MG1
     &+5*F2MF1*Y3*Z4*G1+2*F2MF1*Y3*Z4*G2MG1+F2MF1*Y3*Z4*G3MG1
     &-5*F2MF1*Y4*Z3*G1-2*F2MF1*Y4*Z3*G2MG1-F2MF1*Y4*Z3*G3MG1
     &-5*F3MF1*Y2*Z4*G1-2*F3MF1*Y2*Z4*G2MG1-F3MF1*Y2*Z4*G3MG1
     &+5*F3MF1*Z2*Y4*G1+2*F3MF1*Z2*Y4*G2MG1+F3MF1*Z2*Y4*G3MG1
     &+5*F4MF1*Y2*Z3*G1+2*F4MF1*Y2*Z3*G2MG1+F4MF1*Y2*Z3*G3MG1
     &-5*F4MF1*Z2*Y3*G1-2*F4MF1*Z2*Y3*G2MG1-F4MF1*Z2*Y3*G3MG1
     &               ) * XSUR120
            W3(IELEM) = (
     &-(-F2MF1*Y3*Z4+F2MF1*Y4*Z3+F3MF1*Y2*Z4
     &  -F3MF1*Z2*Y4-F4MF1*Y2*Z3+F4MF1*Z2*Y3)
     &               *(2*G3MG1+G2MG1+G4MG1+5*G1)
     &               ) * XSUR120
            W4(IELEM) = (
     &-2*F4MF1*Z2*Y3*G4MG1
     &+2*F4MF1*Y2*Z3*G4MG1
     &+2*F3MF1*Z2*Y4*G4MG1
     &-2*F3MF1*Y2*Z4*G4MG1
     &+2*F2MF1*Y3*Z4*G4MG1
     &-2*F2MF1*Y4*Z3*G4MG1
     &+5*F2MF1*Y3*Z4*G1
     &+F2MF1*Y3*Z4*G2MG1
     &+F2MF1*Y3*Z4*G3MG1-5*F2MF1*Y4*Z3*G1-F2MF1*Y4*Z3*G2MG1
     &-F2MF1*Y4*Z3*G3MG1-5*F3MF1*Y2*Z4*G1-F3MF1*Y2*Z4*G2MG1
     &-F3MF1*Y2*Z4*G3MG1+5*F3MF1*Z2*Y4*G1+F3MF1*Z2*Y4*G2MG1
     &+F3MF1*Z2*Y4*G3MG1+5*F4MF1*Y2*Z3*G1+F4MF1*Y2*Z3*G2MG1
     &+F4MF1*Y2*Z3*G3MG1-5*F4MF1*Z2*Y3*G1-F4MF1*Z2*Y3*G2MG1
     &-F4MF1*Z2*Y3*G3MG1
     &               ) * XSUR120
!
          ENDDO
!
        ELSE IF (ICOORD.EQ.2) THEN
!
!-----------------------------------------------------------------------
!  DERIVATIVE WRT Y
!
          DO   IELEM = 1 , NELEM
!
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
!
            F1 = F(I1)
            F2 = F(I2)
            F3 = F(I3)
            F4 = F(I4)
!
            IF (IELMG.EQ.31) THEN
              G1 = G(I1)
              G2 = G(I2)
              G3 = G(I3)
              G4 = G(I4)
            ELSE
              G1 = G(IELEM)
              G2 = G1
              G3 = G1
              G4 = G1
            ENDIF
!
            F2MF1 = F2-F1
            F3MF1 = F3-F1
            F4MF1 = F4-F1
            G2MG1 = G2-G1
            G3MG1 = G3-G1
            G4MG1 = G4-G1
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
            X2  =  X(I2) - X(I1)
            X3  =  X(I3) - X(I1)
            X4  =  X(I4) - X(I1)
            Z2  =  Z(I2) - Z(I1)
            Z3  =  Z(I3) - Z(I1)
            Z4  =  Z(I4) - Z(I1)
!
            W1(IELEM) = (
     &-F2MF1*X3*Z4*G2MG1+F3MF1*X2*Z4*G3MG1+5*F3MF1*X2*Z4*G1
     &-F2MF1*X3*Z4*G3MG1-5*F2MF1*X3*Z4*G1
     &+F2MF1*X4*Z3*G2MG1+F2MF1*X4*Z3*G3MG1+5*F2MF1*X4*Z3*G1
     &+F3MF1*X2*Z4*G2MG1+F4MF1*Z2*X3*G2MG1+F4MF1*Z2*X3*G3MG1
     &+5*F4MF1*Z2*X3*G1-F4MF1*X2*Z3*G2MG1-F4MF1*X2*Z3*G3MG1
     &-5*F4MF1*X2*Z3*G1-F3MF1*Z2*X4*G2MG1-F3MF1*Z2*X4*G3MG1
     &-5*F3MF1*Z2*X4*G1+F2MF1*X4*Z3*G4MG1+F3MF1*X2*Z4*G4MG1
     &-F4MF1*X2*Z3*G4MG1-F3MF1*Z2*X4*G4MG1-F2MF1*X3*Z4*G4MG1
     &+F4MF1*Z2*X3*G4MG1 ) * XSUR120
            W2(IELEM) = (
     &         -2*F2MF1*X3*Z4*G2MG1+F3MF1*X2*Z4*G3MG1+5*F3MF1*X2*Z4*G1-F
     &2MF1*X3*Z4*G3MG1-5*F2MF1*X3*Z4*G1+2*F2MF1*X4*Z3*G2MG1+F2MF1*X4*Z3*
     &G3MG1+5*F2MF1*X4*Z3*G1+2*F3MF1*X2*Z4*G2MG1+2*F4MF1*Z2*X3*G2MG1+F4M
     &F1*Z2*X3*G3MG1+5*F4MF1*Z2*X3*G1-2*F4MF1*X2*Z3*G2MG1-F4MF1*X2*Z3*G3
     &MG1-5*F4MF1*X2*Z3*G1-2*F3MF1*Z2*X4*G2MG1-F3MF1*Z2*X4*G3MG1-5*F3MF1
     &*Z2*X4*G1+F2MF1*X4*Z3*G4MG1+F3MF1*X2*Z4*G4MG1-F4MF1*X2*Z3*G4MG1-F3
     &MF1*Z2*X4*G4MG1-F2MF1*X3*Z4*G4MG1+F4MF1*Z2*X3*G4MG1 ) * XSUR120
            W3(IELEM) = (
     &         -(F2MF1*X3*Z4-F2MF1*X4*Z3-F3MF1*X2*Z4+F3MF1*Z2*X4+F4MF1*X
     &2*Z3-F4MF1*Z2*X3)*(2*G3MG1+G2MG1+G4MG1+5*G1) ) * XSUR120
            W4(IELEM) = (
     &         -F2MF1*X3*Z4*G2MG1+F3MF1*X2*Z4*G3MG1+5*F3MF1*X2*Z4*G1-F2M
     &F1*X3*Z4*G3MG1-5*F2MF1*X3*Z4*G1+F2MF1*X4*Z3*G2MG1+F2MF1*X4*Z3*G3MG
     &1+5*F2MF1*X4*Z3*G1+F3MF1*X2*Z4*G2MG1+F4MF1*Z2*X3*G2MG1+F4MF1*Z2*X3
     &*G3MG1+5*F4MF1*Z2*X3*G1-F4MF1*X2*Z3*G2MG1-F4MF1*X2*Z3*G3MG1-5*F4MF
     &1*X2*Z3*G1-F3MF1*Z2*X4*G2MG1-F3MF1*Z2*X4*G3MG1-5*F3MF1*Z2*X4*G1+2*
     &F2MF1*X4*Z3*G4MG1+2*F3MF1*X2*Z4*G4MG1-2*F4MF1*X2*Z3*G4MG1-2*F3MF1*
     &Z2*X4*G4MG1-2*F2MF1*X3*Z4*G4MG1+2*F4MF1*Z2*X3*G4MG1 ) * XSUR120
!
          ENDDO
!
        ELSE IF (ICOORD.EQ.3) THEN
!-----------------------------------------------------------------------
!  DERIVATIVE WRT Z
!
          DO   IELEM = 1 , NELEM
!
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
!
            F1 = F(I1)
            F2 = F(I2)
            F3 = F(I3)
            F4 = F(I4)
!
            IF (IELMG.EQ.31) THEN
              G1 = G(I1)
              G2 = G(I2)
              G3 = G(I3)
              G4 = G(I4)
            ELSE
              G1 = G(IELEM)
              G2 = G1
              G3 = G1
              G4 = G1
            ENDIF
!
            F2MF1 = F2-F1
            F3MF1 = F3-F1
            F4MF1 = F4-F1
            G2MG1 = G2-G1
            G3MG1 = G3-G1
            G4MG1 = G4-G1
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT
!
            X2  =  X(I2) - X(I1)
            X3  =  X(I3) - X(I1)
            X4  =  X(I4) - X(I1)
            Y2  =  Y(I2) - Y(I1)
            Y3  =  Y(I3) - Y(I1)
            Y4  =  Y(I4) - Y(I1)
!
            W1(IELEM) = (
     &         5*F2MF1*X3*Y4*G1+F2MF1*X3*Y4*G2MG1+F2MF1*X3*Y4*G3MG1-5*F2
     &MF1*X4*Y3*G1-F2MF1*X4*Y3*G2MG1-F2MF1*X4*Y3*G3MG1-5*F3MF1*X2*Y4*G1-
     &F3MF1*X2*Y4*G2MG1-F3MF1*X2*Y4*G3MG1+5*F3MF1*Y2*X4*G1+F3MF1*Y2*X4*G
     &2MG1+F3MF1*Y2*X4*G3MG1+5*F4MF1*X2*Y3*G1+F4MF1*X2*Y3*G2MG1-5*F4MF1*
     &Y2*X3*G1-F4MF1*Y2*X3*G2MG1-F4MF1*Y2*X3*G3MG1+F4MF1*X2*Y3*G3MG1-F4M
     &F1*Y2*X3*G4MG1-F3MF1*X2*Y4*G4MG1+F4MF1*X2*Y3*G4MG1+F2MF1*X3*Y4*G4M
     &G1+F3MF1*Y2*X4*G4MG1-F2MF1*X4*Y3*G4MG1 ) * XSUR120
            W2(IELEM) = (
     &         5*F2MF1*X3*Y4*G1+2*F2MF1*X3*Y4*G2MG1+F2MF1*X3*Y4*G3MG1-5*
     &F2MF1*X4*Y3*G1-2*F2MF1*X4*Y3*G2MG1-F2MF1*X4*Y3*G3MG1-5*F3MF1*X2*Y4
     &*G1-2*F3MF1*X2*Y4*G2MG1-F3MF1*X2*Y4*G3MG1+5*F3MF1*Y2*X4*G1+2*F3MF1
     &*Y2*X4*G2MG1+F3MF1*Y2*X4*G3MG1+5*F4MF1*X2*Y3*G1+2*F4MF1*X2*Y3*G2MG
     &1-5*F4MF1*Y2*X3*G1-2*F4MF1*Y2*X3*G2MG1-F4MF1*Y2*X3*G3MG1+F4MF1*X2*
     &Y3*G3MG1-F4MF1*Y2*X3*G4MG1-F3MF1*X2*Y4*G4MG1+F4MF1*X2*Y3*G4MG1+F2M
     &F1*X3*Y4*G4MG1+F3MF1*Y2*X4*G4MG1-F2MF1*X4*Y3*G4MG1 ) * XSUR120
            W3(IELEM) = (
     &         -(-F2MF1*X3*Y4+F2MF1*X4*Y3+F3MF1*X2*Y4-F3MF1*Y2*X4-F4MF1*
     &X2*Y3+F4MF1*Y2*X3)*(2*G3MG1+G2MG1+G4MG1+5*G1) ) * XSUR120
            W4(IELEM) = (
     &         5*F2MF1*X3*Y4*G1+F2MF1*X3*Y4*G2MG1+F2MF1*X3*Y4*G3MG1-5*F2
     &MF1*X4*Y3*G1-F2MF1*X4*Y3*G2MG1-F2MF1*X4*Y3*G3MG1-5*F3MF1*X2*Y4*G1-
     &F3MF1*X2*Y4*G2MG1-F3MF1*X2*Y4*G3MG1+5*F3MF1*Y2*X4*G1+F3MF1*Y2*X4*G
     &2MG1+F3MF1*Y2*X4*G3MG1+5*F4MF1*X2*Y3*G1+F4MF1*X2*Y3*G2MG1-5*F4MF1*
     &Y2*X3*G1-F4MF1*Y2*X3*G2MG1-F4MF1*Y2*X3*G3MG1+F4MF1*X2*Y3*G3MG1-2*F
     &4MF1*Y2*X3*G4MG1-2*F3MF1*X2*Y4*G4MG1+2*F4MF1*X2*Y3*G4MG1+2*F2MF1*X
     &3*Y4*G4MG1+2*F3MF1*Y2*X4*G4MG1-2*F2MF1*X4*Y3*G4MG1 ) * XSUR120
!
          ENDDO
!
        ELSE
!
!-----------------------------------------------------------------------
!
          WRITE(LU,201) ICOORD
 201      FORMAT(1X,'VC11TT (BIEF) : IMPOSSIBLE COMPONENT ',
     &         1I6,' CHECK ICOORD')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
! ERROR
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,1101) IELMF,SF%NAME
        WRITE(LU,1201) IELMG,SG%NAME
        WRITE(LU,1301)
        CALL PLANTE(1)
        STOP
 1101   FORMAT(1X,'VC11TT (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
 1201   FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &         1X,'REAL NAME: ',A6)
 1301   FORMAT(1X,'CASE NOT IMPLEMENTED')
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
