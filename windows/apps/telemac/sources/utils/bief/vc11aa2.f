!                   ******************
                    SUBROUTINE VC11AA2
!                   ******************
!
     &(XMUL,SF,SG,SH,F,G,H,XEL,YEL,
     & IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W1,W2,W3,ICOORD)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING TERMS:
!code
!+   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)
!+
!+                       /                  DF
!+    VEC(I)  =  XMUL   /  ( G . H .  P  *( --  )) D(OMEGA)
!+                     /OMEGA          I    DX
!+
!+    P   IS A LINEAR BASE
!+     I
!+
!+    F IS A VECTOR OF TYPE P1
!+    G IS A VECTOR OF TYPE DISCONTINUOUS P1
!+    H IS A VECTOR OF TYPE P0
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| H              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SH             |-->| BIEF_OBJ STRUCTURE OF H
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC11AA2 => VC11AA2
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION,INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURES OF F, G AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SH
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMG,IELMH
      DOUBLE PRECISION XSUR24 ,F1,F2,F3,G1,G2,G3,X2,X3,Y2,Y3,KSAT
!
!-----------------------------------------------------------------------
!
      XSUR24= XMUL / 24.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMG=SG%ELM
      IELMH=SH%ELM
!
!-----------------------------------------------------------------------
!
!     F IS P1, G IS DISCONTINUOUS P1, H IS P0
!
      IF(IELMG.EQ.15.AND.IELMF.EQ.11.AND.IELMH.EQ.10) THEN
!
!     X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO IELEM = 1 , NELEM
!
        KSAT=H(IELEM)
        G1 = G(IELEM)
        G2 = G(IELEM+NELEM)
        G3 = G(IELEM+2*NELEM)
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        F3 = F(IKLE3(IELEM))
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        W1(IELEM)=(Y2*(-G3*F3+G3*F1-G2*F3+G2*F1-2*G1*F3+2*G1*F1)+Y3*(
     &             G3*F2-G3*F1+G2*F2-G2*F1+2*G1*F2-2*G1*F1))* XSUR24
        W2(IELEM)=(Y2*(-G3*F3+G3*F1-2*G2*F3+2*G2*F1-G1*F3+G1*F1)+Y3*(
     &             G3*F2-G3*F1+2*G2*F2-2*G2*F1+G1*F2-G1*F1))* XSUR24
        W3(IELEM)=(Y2*(-2*G3*F3+2*G3*F1-G2*F3+G2*F1-G1*F3+G1*F1)+Y3*(
     &             2*G3*F2-2*G3*F1+G2*F2-G2*F1+G1*F2-G1*F1))* XSUR24
!
        W1(IELEM)=KSAT*W1(IELEM)
        W2(IELEM)=KSAT*W2(IELEM)
        W3(IELEM)=KSAT*W3(IELEM)
!
      ENDDO
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!     Y COORDINATE
!
      DO IELEM = 1 , NELEM
!
        KSAT=H(IELEM)
        G1 = G(IELEM)
        G2 = G(IELEM+NELEM)
        G3 = G(IELEM+2*NELEM)
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        F3 = F(IKLE3(IELEM))
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
!
        W1(IELEM)=(X2*(G3*F3-G3*F1+G2*F3-G2*F1+2*G1*F3-2*G1*F1)+X3*(-
     &             G3*F2+G3*F1-G2*F2+G2*F1-2*G1*F2+2*G1*F1)) * XSUR24
        W2(IELEM)=(X2*(G3*F3-G3*F1+2*G2*F3-2*G2*F1+G1*F3-G1*F1)+X3*(-
     &             G3*F2+G3*F1-2*G2*F2+2*G2*F1-G1*F2+G1*F1)) * XSUR24
        W3(IELEM)=(X2*(2*G3*F3-2*G3*F1+G2*F3-G2*F1+G1*F3-G1*F1)+X3*(-
     &             2*G3*F2+2*G3*F1-G2*F2+G2*F1-G1*F2+G1*F1)) * XSUR24
!
        W1(IELEM)=KSAT*W1(IELEM)
        W2(IELEM)=KSAT*W2(IELEM)
        W3(IELEM)=KSAT*W3(IELEM)
!
      ENDDO
!
      ELSE
!
          WRITE(LU,21) ICOORD
21        FORMAT(1X,'VC11AA2 (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
          STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
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
101     FORMAT(1X,'VC11AA2 (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF G:',1I6,
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
