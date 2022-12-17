!                   *****************
                    SUBROUTINE MT06FT
!                   *****************
!
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SF,F,X,Y,Z,IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX)
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
!+     BY ELEMENTARY CELL;
!+  !! THE ELEMENT IS THE P1 TRIANGLE, BUT IN A MESH OF PRISMS !!
!+  !! SPLIT IN TETRAHEDRONS                                   !!
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        26/04/04
!+        V5P5
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
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT06FT => MT06FT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NBOR(*),NELEM,NELMAX
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
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMF,I1,I2,I3,IELEM
!
      DOUBLE PRECISION SUR60,S,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,F1,F2,F3,F123
      DOUBLE PRECISION DET1,DET2
!
!**********************************************************************
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!
!     F LINEAR BY BOUNDARY SIDE
!
      IF(IELMF.EQ.61.OR.IELMF.EQ.81) THEN
!
        SUR60  = XMUL/60.D0
!
!   LOOP ON THE BOUNDARY SIDES
!
        DO IELEM = 1,NELEM
!
!         GLOBAL NUMBERING OF THE SIDE VERTICES
!
          I1 = NBOR(IKLE1(IELEM))
          I2 = NBOR(IKLE2(IELEM))
          I3 = NBOR(IKLE3(IELEM))
!
          X1 = X(I1)
          Y1 = Y(I1)
          Z1 = Z(I1)
!
          X2 = X(I2)-X1
          X3 = X(I3)-X1
          Y2 = Y(I2)-Y1
          Y3 = Y(I3)-Y1
          Z2 = Z(I2)-Z1
          Z3 = Z(I3)-Z1
!
          F1 = F(IKLE1(IELEM))
          F2 = F(IKLE2(IELEM))
          F3 = F(IKLE3(IELEM))
          F123  = F1 + F2 + F3
!
!         COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
!
          S=0.5D0*SQRT(  (Y2*Z3-Y3*Z2)**2
     &                  +(X3*Z2-X2*Z3)**2
     &                  +(X2*Y3-X3*Y2)**2   )
!
          DET1 = S * SUR60
          DET2 = DET1 + DET1
!
!***********************************************************************
!
!         ELEMENTS OFF THE DIAGONAL
!
          A12(IELEM) = DET1 * (F123+F123-F3)
          A13(IELEM) = DET1 * (F123+F123-F2)
          A23(IELEM) = DET1 * (F123+F123-F1)
!
!         DIAGONAL TERMS
!
          A11(IELEM) = DET2 * (F123+F1+F1)
          A22(IELEM) = DET2 * (F123+F2+F2)
          A33(IELEM) = DET2 * (F123+F3+F3)
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!     OTHER TYPES OF DISCRETISATION OF F
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'MT06FT (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &         1X,'REAL NAME: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NOTE: ON A TRIANGULAR MESH IN PLANE (X, Y)
!
!     DO IELEM = 1 , NELEM
!
!     F1 = F(IKLE1(IELEM))
!     F2 = F(IKLE2(IELEM))
!     F3 = F(IKLE3(IELEM))
!
!     F123 = F1 + F2 + F3
!
!     DET1 = SURFAC(IELEM) * SUR60
!     DET2 = DET1 + DET1
!
!***********************************************************************
!
!  ELEMENTS OFF THE DIAGONAL
!
!     A12(IELEM) = DET1 * (F123+F123-F3)
!     A13(IELEM) = DET1 * (F123+F123-F2)
!     A23(IELEM) = DET1 * (F123+F123-F1)
!
!  DIAGONAL TERMS
!
!     A11(IELEM) = DET2 * (F123+F1+F1)
!     A22(IELEM) = DET2 * (F123+F2+F2)
!     A33(IELEM) = DET2 * (F123+F3+F3)
!
!      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
