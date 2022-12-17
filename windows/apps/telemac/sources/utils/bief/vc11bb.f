!                   *****************
                    SUBROUTINE VC11BB
!                   *****************
!
     &( XMUL,SF,SG,F,G,XEL,YEL,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4 , ICOORD )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING TERMS:
!code
!+   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)
!+
!+                       /            DF
!+    VEC(I)  =  XMUL   /  ( G  P  *( --  )) D(OMEGA)
!+                     /OMEGA    I    DX
!+
!+
!+    P   IS A QUASI-BUBBLE BASE
!+     I
!+
!+    F IS A VECTOR OF TYPE P1 OR OTHER
!+    G IS A VECTOR OF TYPE P1 OR OTHER
!
!note     IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!+
!+               HERE, IF F IS P0, IT REALLY MEANS THAT F IS P1,
!+                     BUT GIVEN BY ELEMENTS.
!+
!+               THE SIZE OF F SHOULD THEN BE : F(NELMAX,3).
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
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
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT OF TRIANGLES
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
      USE BIEF, EX_VC11BB => VC11BB
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
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURES OF F, G AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF,SG
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMG
!
      DOUBLE PRECISION F1,F2,F3,F4,G1,G2,G3,G4,X2,X3,Y2,Y3,AUX
      DOUBLE PRECISION XSUR72 ,XSU216,XSUR18
!
!-----------------------------------------------------------------------
!
      XSUR72= XMUL / 72.D0
      XSU216= XMUL /216.D0
      XSUR18= XMUL / 18.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMG=SG%ELM
!
!-----------------------------------------------------------------------
!
!     F AND G ARE LINEAR
!
      IF(IELMF.EQ.11.AND.IELMG.EQ.11) THEN
!
!  X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO IELEM = 1 , NELEM
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        G1 = G(IKLE1(IELEM))
        G2 = G(IKLE2(IELEM))
        G3 = G(IKLE3(IELEM))
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
        AUX = F2*Y3 - F3*Y2
!
        W1(IELEM)=(5*G3+5*G2+14*G1)*AUX*XSU216
        W2(IELEM)=(5*G3+14*G2+5*G1)*AUX*XSU216
        W3(IELEM)=(14*G3+5*G2+5*G1)*AUX*XSU216
        W4(IELEM)=(G3+G2+G1)       *AUX*XSUR18
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!  Y COORDINATE
!
      DO IELEM = 1 , NELEM
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        G1 = G(IKLE1(IELEM))
        G2 = G(IKLE2(IELEM))
        G3 = G(IKLE3(IELEM))
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        AUX = X2*F3 - X3*F2
!
        W1(IELEM)=AUX*(5*G3+5*G2+14*G1)*XSU216
        W2(IELEM)=AUX*(5*G3+14*G2+5*G1)*XSU216
        W3(IELEM)=AUX*(14*G3+5*G2+5*G1)*XSU216
        W4(IELEM)=AUX*(G3+G2+G1)       *XSUR18
!
      ENDDO ! IELEM
!
      ELSE
!
          WRITE(LU,21) ICOORD
21        FORMAT(1X,'VC11BB (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
          STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     F AND G ARE QUASI-BUBBLE
!
      ELSEIF(IELMF.EQ.12.AND.IELMG.EQ.12) THEN
!
!  X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO IELEM = 1 , NELEM
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
        G1 = G(IKLE1(IELEM))
        G2 = G(IKLE2(IELEM))
        G3 = G(IKLE3(IELEM))
        G4 = G(IKLE4(IELEM))
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        W1(IELEM)=(3*((Y3-Y2)*G4+2*(Y3-Y2)*G1+G3*Y3-G2*Y2)*F4-(G3+
     &   G4+2*G1)*(Y3+Y2)*F3+(G4+G2+2*G1)*(Y3+Y2)*F2)*XSUR72
        W2(IELEM)=(((2*Y3-Y2)*G3+(Y3+Y2)*G1+3*G4*Y3+6*G2*Y3)*F2-3
     &   *((Y3-Y2)*G3+G4*Y3+2*G2*Y3+G1*Y2)*F4+(G3+G4+2*G2)*(Y3-
     &   2*Y2)*F3)*XSUR72
        W3(IELEM)=(-(((Y3+Y2)*G1-(Y3-2*Y2)*G2+6*G3*Y2+3*G4*Y2)*F3+
     &   3*((Y3-Y2)*G2-2*G3*Y2-G4*Y2-G1*Y3)*F4-(2*G3+G4+G2)*(2*
     &   Y3-Y2)*F2))*XSUR72
        W4(IELEM)=(((2*Y3-Y2)*G3+(Y3+Y2)*G1+6*G4*Y3+3*G2*Y3)*F2-((
     &   Y3+Y2)*G1-(Y3-2*Y2)*G2+3*G3*Y2+6*G4*Y2)*F3+3*((Y3-Y2)
     &   *G1+G3*Y2-G2*Y3)*F4)*XSUR72
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!  Y COORDINATE
!
      DO IELEM = 1 , NELEM
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
        G1 = G(IKLE1(IELEM))
        G2 = G(IKLE2(IELEM))
        G3 = G(IKLE3(IELEM))
        G4 = G(IKLE4(IELEM))
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
!
        W1(IELEM)=(-(3*((G3+G4+2*G1)*X3-(G4+G2+2*G1)*X2)*F4-(X2+X3
     &   )*(G3+G4+2*G1)*F3+(X2+X3)*(G4+G2+2*G1)*F2))*XSUR72
        W2(IELEM)=(-(((2*G3+3*G4+6*G2+G1)*X3-(G3-G1)*X2)*F2-3*((
     &   G3+G4+2*G2)*X3-(G3-G1)*X2)*F4-(2*X2-X3)*(G3+G4+2*G2)*
     &   F3))*XSUR72
        W3(IELEM)=(((6*G3+3*G4+2*G2+G1)*X2-(G2-G1)*X3)*F3-3*((2*
     &   G3+G4+G2)*X2-(G2-G1)*X3)*F4+(X2-2*X3)*(2*G3+G4+G2)*F2)*XSUR72
        W4(IELEM)=(((3*G3+6*G4+2*G2+G1)*X2-(G2-G1)*X3)*F3-((2*G3+
     &   6*G4+3*G2+G1)*X3-(G3-G1)*X2)*F2-3*((G3-G1)*X2-(G2-G1)*
     &   X3)*F4)*XSUR72
!
      ENDDO ! IELEM
!
      ELSE
!
          WRITE(LU,21) ICOORD
          CALL PLANTE(0)
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
        WRITE(LU,101) IELMF,SF%NAME
        WRITE(LU,201) IELMG,SG%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC11BB (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
