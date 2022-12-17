!                   ******************
                    SUBROUTINE VC13PP2
!                   ******************
!
     &(XMUL,SF,F,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     & W1,W2,W3,W4,W5,W6,ICOORD)
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
!+    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+    HERE F IS A FUNCTION OF X AND Y (NOT OF Z)
!+    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!note     IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!+                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!+                     P1, BUT GIVEN BY ELEMENTS.
!+                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,3).
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
!history  N.DURAND (HRW), S.E. BOURBAN (HRW)
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
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| Z              |-->| Z-COORDINATES OF POINTS IN THE MESH, PER POINT !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC13PP2 => VC13PP2
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: X(NELMAX,6),Y(NELMAX,6),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) ::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSUR48,F1,F2,F3
      DOUBLE PRECISION X2,X3,Y2,Y3,H1,H2,H3,H123,FX6,FY6
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,IELMF
!
!-----------------------------------------------------------------------
!
      XSUR48  = XMUL/48.D0
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
!
!       REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
        Y2  =  Y(IELEM,2)
        Y3  =  Y(IELEM,3)
!
        FX6 = Y2*(F1-F3) + Y3*(F2-F1)
!
        H1 = Z(I4) - Z(I1)
        H2 = Z(I5) - Z(I2)
        H3 = Z(I6) - Z(I3)
        H123 = H1 + H2 + H3
!
        W1(IELEM) = XSUR48 * (H123+H1) * FX6
        W2(IELEM) = XSUR48 * (H123+H2) * FX6
        W3(IELEM) = XSUR48 * (H123+H3) * FX6
!
        W4(IELEM) = W1(IELEM)
        W5(IELEM) = W2(IELEM)
        W6(IELEM) = W3(IELEM)
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
!
!       REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
        X2  =  X(IELEM,2)
        X3  =  X(IELEM,3)
!
        FY6 = X2*(F3-F1) + X3*(F1-F2)
!
        H1 = Z(I4) - Z(I1)
        H2 = Z(I5) - Z(I2)
        H3 = Z(I6) - Z(I3)
        H123 = H1 + H2 + H3
!
        W1(IELEM) = XSUR48 * (H123+H1) * FY6
        W2(IELEM) = XSUR48 * (H123+H2) * FY6
        W3(IELEM) = XSUR48 * (H123+H3) * FY6
!
        W4(IELEM) = W1(IELEM)
        W5(IELEM) = W2(IELEM)
        W6(IELEM) = W3(IELEM)
!
        ENDDO
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,201) ICOORD
201     FORMAT(1X,'VC13PP2 (BIEF) : IMPOSSIBLE COMPONENT ',
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
102     FORMAT(1X,'VC13PP2 (BIEF) :',/,
     &         1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &         1X,'REAL NAME OF F: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!=======================================================================
!
      RETURN
      END
