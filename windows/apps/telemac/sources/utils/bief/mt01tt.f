!                   *****************
                    SUBROUTINE MT01TT
!                   *****************
!
     &( T,XM,XMUL,X,Y,Z,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX FOR TETRAHEDRONS.
!code
!+     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!+     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!+     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!+     XM(IELEM, 4)  ---->  M(2,3) = M(3,2)
!+     XM(IELEM, 5)  ---->  M(2,4) = M(4,2)
!+     XM(IELEM, 6)  ---->  M(3,4) = M(4,3)
!
!history  J-M HERVOUET (LNH)
!+        04/01/02
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
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| T              |<--| NON ASSEMBLED DIAGONAL
!| XM             |<--| NON ASSEMBLED OFF-DIAGONAL TERMS
!| XMUL           |-->| MULTIPLICATION FACTOR
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT01TT => MT01TT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,4)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,6)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     SPECIFIC DECLARATIONS
!
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,VOLSUR20
      INTEGER I1,I2,I3,I4,IELEM
!
      DOUBLE PRECISION XSUR120
!
!***********************************************************************
!
      XSUR120=XMUL/120.D0
!
!-----------------------------------------------------------------------
!
!     LOOP ON THE TETRAHEDRONS
!
      DO IELEM=1,NELEM
!
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
!
!-----------------------------------------------------------------------
!
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
!
!     EXTRA-DIAGONAL TERMS
!
!     VOLUME OF THE TETRAHEDRON:
!
!     (Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))/6
!
!     XM(IELEM,1) = VOLUME / 20
!
!     SUMS UP THE TERMS (INC. SYMMETRIC ONES) TO YIELD VOLUME OF THE TETRAHEDRON
!
      VOLSUR20 =
     &(Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))*XSUR120
      XM(IELEM,1) = MAX(VOLSUR20,1.D-4)
      XM(IELEM,2) = XM(IELEM,1)
      XM(IELEM,3) = XM(IELEM,1)
      XM(IELEM,4) = XM(IELEM,1)
      XM(IELEM,5) = XM(IELEM,1)
      XM(IELEM,6) = XM(IELEM,1)
!
!     DIAGONAL TERMS
!
      T(IELEM,1) = 2 * XM(IELEM,1)
      T(IELEM,2) = T(IELEM,1)
      T(IELEM,3) = T(IELEM,1)
      T(IELEM,4) = T(IELEM,1)
!
!-----------------------------------------------------------------------
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
