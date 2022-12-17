!                   *****************
                    SUBROUTINE SLOP10
!                   *****************
!
     &(COEF,XEL,YEL,Z,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENT 1 / COS(ALFA)
!+                WHERE ALFA IS THE SLOPE OF A TRIANGULAR ELEMENT.
!+
!+            THIS COEFFICIENT IS USED IN THE BOTTOM FRICTION
!+                TERM.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        27/01/95
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
!| COEF           |<--| RESULT
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| Z              |-->| BOTTOM ELEVATIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: COEF(NELEM)
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*),Z(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3,Z2,Z3,A,B,C
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
!
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        Z2 = Z(IKLE(IELEM,2)) -  Z(IKLE(IELEM,1))
        Z3 = Z(IKLE(IELEM,3)) -  Z(IKLE(IELEM,1))
!
        A = (X2*Y3-X3*Y2)**2
        B = (Y2*Z3-Z2*Y3)**2
        C = (X3*Z2-Z3*X2)**2
!
        COEF(IELEM) = SQRT( (A+B+C)/A )
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
