!                   *****************
                    SUBROUTINE VC00FT
!                   *****************
!
     &(XMUL,X,Y,Z,IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX,W1,W2,W3)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 SEGMENT BUT ON A MESH WITH
!+    VERTICAL TRIANGLES IN THE X,Y,Z SPACE
!+
!+    F IS A VECTOR OF TYPE IELMF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
!+        V5P4
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
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: NBOR(*)
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3
      DOUBLE PRECISION XSUR3,COEF,X1,X2,X3,Y1,Y2
      DOUBLE PRECISION Y3,Z1,Z2,Z3,S
!
      INTRINSIC SQRT
!
!***********************************************************************
!
      XSUR3 = XMUL/3.D0
!
!   LOOP ON THE BOUNDARY SIDES
!
      DO IELEM = 1,NELEM
!
!       GLOBAL NUMBERING OF THE SIDE NODES
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
!       COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
!
        S=0.5D0*SQRT(  (Y2*Z3-Y3*Z2)**2
     &                +(X3*Z2-X2*Z3)**2  )
!    &                +(X2*Y3-X3*Y2)**2  )  THIS TERM IS 0
!
        COEF=XSUR3*S
!
        W1(IELEM) = COEF
        W2(IELEM) = COEF
        W3(IELEM) = COEF
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
!     NOTE: FOR A PLANE TRIANGLE (VC00AA):
!
!     XSUR3 = XMUL / 3.D0
!
!     DO IELEM = 1 , NELEM
!
!
!       COEF = XSUR3 * SURFAC(IELEM)
!
!       W1(IELEM) = COEF
!       W2(IELEM) = COEF
!       W3(IELEM) = COEF
!
!     ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
