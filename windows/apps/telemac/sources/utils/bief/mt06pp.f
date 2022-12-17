!                   *****************
                    SUBROUTINE MT06PP
!                   *****************
!
     &( T,XM,XMUL,SF,F,Z,SURFAC,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX FOR P1 PRISMS.
!code
!+     COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!+
!+                              /
!+                    A    =   /  F * (P *P )*J(X,Y) DXDY
!+                     I J    /S        I  J
!+
!+     BY ELEMENTARY CELL; THE ELEMENT IS THE P1 PRISM
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        28/11/94
!+
!+
!
!history  ARNAUD DESITTER - UNIVERSITY OF BRISTOL
!+        **/04/98
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
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| Z              |-->| ELEVATIONS OF POINTS
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT06PP => MT06PP
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6), XM(NELMAX,30)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: Z(*)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      DOUBLE PRECISION SUR2160,COEF,H1,H2,H3,F1,F2,F3,F4,F5,F6
!
      INTEGER IELEM,IELMF
!
!-----------------------------------------------------------------------
!
      SUR2160 = XMUL / 2160.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IF(IELMF.NE.41) THEN
        WRITE(LU,101) IELMF
101     FORMAT(1X,'MT06PP (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
! LOOP ON THE ELEMENTS
!
      DO IELEM = 1,NELEM
!
        COEF = SURFAC(IELEM)* SUR2160
!
        H1 = (Z(IKLE(IELEM,4)) - Z(IKLE(IELEM,1))) * COEF
        H2 = (Z(IKLE(IELEM,5)) - Z(IKLE(IELEM,2))) * COEF
        H3 = (Z(IKLE(IELEM,6)) - Z(IKLE(IELEM,3))) * COEF
!
        F1 = F(IKLE(IELEM,1))
        F2 = F(IKLE(IELEM,2))
        F3 = F(IKLE(IELEM,3))
        F4 = F(IKLE(IELEM,4))
        F5 = F(IKLE(IELEM,5))
        F6 = F(IKLE(IELEM,6))
!
!-----------------------------------------------------------------------
!
!  EXTRA-DIAGONAL TERMS
!
        XM(IELEM,01) =
     &     (9*F1+6*F2+3*F3+3*F4+2*F5+F6)*H1+
     &     (6*F1+9*F2+3*F3+2*F4+3*F5+F6)*H2+
     &     (3*F1+3*F2+3*F3+F4+F5+F6)*H3
        XM(IELEM,02) =
     &     (9*F1+3*F2+6*F3+3*F4+F5+2*F6)*H1+
     &     (3*F1+3*F2+3*F3+F4+F5+F6)*H2+
     &     (6*F1+3*F2+9*F3+2*F4+F5+3*F6)*H3
        XM(IELEM,03) =
     &     (12*F1+3*F2+3*F3+12*F4+3*F5+3*F6)*H1+
     &     (3*F1+2*F2+F3+3*F4+2*F5+F6)*H2+
     &     (3*F1+F2+2*F3+3*F4+F5+2*F6)*H3
        XM(IELEM,04) =
     &     (3*F1+2*F2+F3+3*F4+2*F5+F6)*H1+
     &     (2*F1+3*F2+F3+2*F4+3*F5+F6)*H2+
     &     (F1+F2+F3+F4+F5+F6)*H3
        XM(IELEM,05) =
     &     (3*F1+F2+2*F3+3*F4+F5+2*F6)*H1+
     &     (F1+F2+F3+F4+F5+F6)*H2+
     &     (2*F1+F2+3*F3+2*F4+F5+3*F6)*H3
        XM(IELEM,06) =
     &     (3*F1+3*F2+3*F3+F4+F5+F6)*H1+
     &     (3*F1+9*F2+6*F3+F4+3*F5+2*F6)*H2+
     &     (3*F1+6*F2+9*F3+F4+2*F5+3*F6)*H3
        XM(IELEM,07) =
     &     (3*F1+2*F2+F3+3*F4+2*F5+F6)*H1+
     &     (2*F1+3*F2+F3+2*F4+3*F5+F6)*H2+
     &     (F1+F2+F3+F4+F5+F6)*H3
        XM(IELEM,08) =
     &     (2*F1+3*F2+F3+2*F4+3*F5+F6)*H1+
     &     (3*F1+12*F2+3*F3+3*F4+12*F5+3*F6)*H2+
     &     (F1+3*F2+2*F3+F4+3*F5+2*F6)*H3
        XM(IELEM,09) =
     &     (F1+F2+F3+F4+F5+F6)*H1+
     &     (F1+3*F2+2*F3+F4+3*F5+2*F6)*H2+
     &     (F1+2*F2+3*F3+F4+2*F5+3*F6)*H3
        XM(IELEM,10) =
     &     (3*F1+F2+2*F3+3*F4+F5+2*F6)*H1+
     &     (F1+F2+F3+F4+F5+F6)*H2+
     &     (2*F1+F2+3*F3+2*F4+F5+3*F6)*H3
        XM(IELEM,11) =
     &     (F1+F2+F3+F4+F5+F6)*H1+
     &     (F1+3*F2+2*F3+F4+3*F5+2*F6)*H2+
     &     (F1+2*F2+3*F3+F4+2*F5+3*F6)*H3
        XM(IELEM,12) =
     &     (2*F1+F2+3*F3+2*F4+F5+3*F6)*H1+
     &     (F1+2*F2+3*F3+F4+2*F5+3*F6)*H2+
     &     (3*F1+3*F2+12*F3+3*F4+3*F5+12*F6)*H3
        XM(IELEM,13) =
     &     (3*F1+2*F2+F3+9*F4+6*F5+3*F6)*H1+
     &     (2*F1+3*F2+F3+6*F4+9*F5+3*F6)*H2+
     &     (F1+F2+F3+3*F4+3*F5+3*F6)*H3
        XM(IELEM,14) =
     &     (3*F1+F2+2*F3+9*F4+3*F5+6*F6)*H1+
     &     (F1+F2+F3+3*F4+3*F5+3*F6)*H2+
     &     (2*F1+F2+3*F3+6*F4+3*F5+9*F6)*H3
        XM(IELEM,15) =
     &     (F1+F2+F3+3*F4+3*F5+3*F6)*H1+
     &     (F1+3*F2+2*F3+3*F4+9*F5+6*F6)*H2+
     &     (F1+2*F2+3*F3+3*F4+6*F5+9*F6)*H3
!
!  DIAGONAL TERMS
!
        T(IELEM,1) =
     &     (36*F1+9*F2+9*F3+12*F4+3*F5+3*F6)*H1+
     &     (9*F1+6*F2+3*F3+3*F4+2*F5+F6)*H2+
     &     (9*F1+3*F2+6*F3+3*F4+F5+2*F6)*H3
        T(IELEM,2) =
     &     (6*F1+9*F2+3*F3+2*F4+3*F5+F6)*H1+
     &     (9*F1+36*F2+9*F3+3*F4+12*F5+3*F6)*H2+
     &     (3*F1+9*F2+6*F3+F4+3*F5+2*F6)*H3
        T(IELEM,3) =
     &     (6*F1+3*F2+9*F3+2*F4+F5+3*F6)*H1+
     &     (3*F1+6*F2+9*F3+F4+2*F5+3*F6)*H2+
     &     (9*F1+9*F2+36*F3+3*F4+3*F5+12*F6)*H3
        T(IELEM,4) =
     &     (12*F1+3*F2+3*F3+36*F4+9*F5+9*F6)*H1+
     &     (3*F1+2*F2+F3+9*F4+6*F5+3*F6)*H2+
     &     (3*F1+F2+2*F3+9*F4+3*F5+6*F6)*H3
        T(IELEM,5) =
     &     (2*F1+3*F2+F3+6*F4+9*F5+3*F6)*H1+
     &     (3*F1+12*F2+3*F3+9*F4+36*F5+9*F6)*H2+
     &     (F1+3*F2+2*F3+3*F4+9*F5+6*F6)*H3
        T(IELEM,6) =
     &     (2*F1+F2+3*F3+6*F4+3*F5+9*F6)*H1+
     &     (F1+2*F2+3*F3+3*F4+6*F5+9*F6)*H2+
     &     (3*F1+3*F2+12*F3+9*F4+9*F5+36*F6)*H3
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
