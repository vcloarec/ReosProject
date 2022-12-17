!                   *****************
                    SUBROUTINE VC01FF
!                   *****************
!
     &(XMUL,SF,F,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,NBOR,NELEB,NELEBX,W1,W2,W3,W4,
     & NELBOR,NULONE,NELMAX)
!
!***********************************************************************
! BIEF   V6P3                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I) * F  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 SEGMENT
!+
!+    F IS A VECTOR OF TYPE IELMF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/012013
!+        V6P3
!+   Last 3 arguments added, use of XEL, YEL instead of XPT,YPT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF QUADRILATERAL
!| IKLE2          |-->| SECOND POINT OF QUADRILATERAL
!| IKLE3          |-->| THIRD POINT OF QUADRILATERAL
!| IKLE4          |-->| FOURTH POINT OF QUADRILATERAL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELBOR         |-->| ADJACENT ELEMENT NUMBER
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NULONE         |-->| LOCAL NUMBERING OF BOUNDARY ELEMENT IN ADJACENT
!|                |   | ELEMENT.
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC01FF => VC01FF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEB,NELEBX,NELMAX
      INTEGER, INTENT(IN) :: NELBOR(NELEBX),NULONE(NELEBX,4),NBOR(*)
      INTEGER, INTENT(IN) :: IKLE1(NELEBX),IKLE2(NELEBX)
      INTEGER, INTENT(IN) :: IKLE3(NELEBX),IKLE4(NELEBX)
!
      DOUBLE PRECISION, INTENT(IN)    :: X(NELMAX,6),Y(NELMAX,6),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELEBX),W2(NELEBX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELEBX),W4(NELEBX)
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,I1,I2,I3,I4,IEL,J1,J2
      DOUBLE PRECISION XSUR72,H1,H2,HT,AL,F1,F2,F3,F4
!
      INTRINSIC SQRT
!
!***********************************************************************
!
      IELMF=SF%ELM
!
      XSUR72 = XMUL/72.D0
!
!-----------------------------------------------------------------------
!
!     F IS LINEAR BY BOUNDARY SIDE
!
      IF(IELMF.EQ.71) THEN
!
        DO IELEM = 1,NELEB
!
          IEL=NELBOR(IELEM)
!
          IF(IEL.GT.0) THEN
!
!           ELEMENT IN THE DOMAIN
!
!           LOOP ON THE BOUNDARY SIDES
!
!           GLOBAL NUMBERING OF THE SIDE NODES
!
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
!
            J1=NULONE(IELEM,1)
            J2=NULONE(IELEM,2)
            AL = SQRT((X(IEL,J2)-X(IEL,J1))**2
     &               +(Y(IEL,J2)-Y(IEL,J1))**2) * XSUR72
!
            H1 = Z(NBOR(I4)) - Z(NBOR(I1))
            H2 = Z(NBOR(I3)) - Z(NBOR(I2))
            HT = H1 + H2
            H1 = H1 + H1 + HT
            H2 = H2 + H2 + HT
!
            F1 = F(I1) + F(I1) + F(I4)
            F2 = F(I2) + F(I2) + F(I3)
            F3 = F(I2) + F(I3) + F(I3)
            F4 = F(I1) + F(I4) + F(I4)
!
            W1(IELEM) = (F1*H1+F2*HT)*AL
            W2(IELEM) = (F1*HT+F2*H2)*AL
            W3(IELEM) = (F4*HT+F3*H2)*AL
            W4(IELEM) = (F4*H1+F3*HT)*AL
!
          ELSE
!
!           ELEMENT NOT IN THE DOMAIN (PARALLELISM)
!
            W1(IELEM) = 0.D0
            W2(IELEM) = 0.D0
            W3(IELEM) = 0.D0
            W4(IELEM) = 0.D0
!
          ENDIF
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'VC01FF (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &         1X,'REAL NAME: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
