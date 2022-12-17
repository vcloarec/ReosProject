!                   *****************
                    SUBROUTINE FILP12
!                   *****************
!
     &(F,C,XSOM,YSOM,NSOM,X,Y,NPOIN,NELEM,NELMAX,IKLE)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES A FUNCTION TO A CONSTANT VALUE
!+                INSIDE OF A POLYGON.
!
!history  C MOULIN (LNH)
!+        06/12/94
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
!| C              |-->| THE CONSTANT VALUE
!| F              |<->| THE FUNCTION
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NSOM           |-->| NUMBER OF VERTICES IN THE POLYGON
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XSOM           |-->| ABSCISSAE OF POINTS IN THE POLYGON
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YSOM           |-->| ORDINATES OF POINTS IN THE POLYGON
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FILP12 => FILP12
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM , NELEM , NELMAX , NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: F(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*) , Y(*)
      DOUBLE PRECISION, INTENT(IN) :: XSOM(NSOM) , YSOM(NSOM) , C
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I , I1 , I2 , I3 , IELEM
!
      DOUBLE PRECISION XX , YY
!
!-----------------------------------------------------------------------
!
      DO I = 1 , NPOIN
!
        IF(INPOLY(X(I),Y(I),XSOM,YSOM,NSOM)) F(I) = C
!
      ENDDO ! I
!
      DO IELEM = 1 , NELEM
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        XX = 0.3333333333D0 * ( X(I1)+X(I2)+X(I3) )
        YY = 0.3333333333D0 * ( Y(I1)+Y(I2)+Y(I3) )
        IF(INPOLY(XX,YY,XSOM,YSOM,NSOM)) F(IELEM+NPOIN) = C
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
