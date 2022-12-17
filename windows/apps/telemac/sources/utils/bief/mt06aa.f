!                   *****************
                    SUBROUTINE MT06AA
!                   *****************
!
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SF,F,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                              /
!+                    A    =   /  F * (P *P )*J(X,Y) DXDY
!+                     I J    /S        I  J
!+
!+     BY ELEMENTARY CELL; THE ELEMENT IS THE P1 TRIANGLE
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        29/10/99
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION F USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT06AA => MT06AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMF,IELEM
!
      DOUBLE PRECISION SUR12,SUR60,DET1,DET2,F123,F1,F2,F3
!
!-----------------------------------------------------------------------
!
      SUR60 = XMUL/60.D0
      SUR12 = XMUL/12.D0
!
!-----------------------------------------------------------------------
!
      IELMF = SF%ELM
!
!-----------------------------------------------------------------------
!
!  CASE WHERE F IS LINEAR
!
      IF(IELMF.EQ.11) THEN
!
      DO IELEM = 1 , NELEM
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      F123 = F1 + F2 + F3
!
      DET1 = SURFAC(IELEM) * SUR60
      DET2 = DET1 + DET1
!
!***********************************************************************
!
!  ELEMENTS OFF THE DIAGONAL
!
      A12(IELEM) = DET1 * (F123+F123-F3)
      A13(IELEM) = DET1 * (F123+F123-F2)
      A23(IELEM) = DET1 * (F123+F123-F1)
!
!  DIAGONAL TERMS
!
      A11(IELEM) = DET2 * (F123+F1+F1)
      A22(IELEM) = DET2 * (F123+F2+F2)
      A33(IELEM) = DET2 * (F123+F3+F3)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!  CASE WHERE F IS LINEAR
!
      ELSEIF(IELMF.EQ.10) THEN
!
      DO IELEM = 1 , NELEM
!
      F1 = F(IELEM)
!
      DET1 = SURFAC(IELEM) * SUR12
      DET2 = DET1 + DET1
!
!***********************************************************************
!
!  ELEMENTS OFF THE DIAGONAL
!
      A12(IELEM) = DET1 * F1
      A13(IELEM) = DET1 * F1
      A23(IELEM) = DET1 * F1
!
!  DIAGONAL TERMS
!
      A11(IELEM) = DET2 * F1
      A22(IELEM) = DET2 * F1
      A33(IELEM) = DET2 * F1
!
      ENDDO ! IELEM
!
!     OTHER TYPES OF DISCRETISATION OF F
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'MT06AA (BIEF) :',/,
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
