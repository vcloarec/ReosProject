!                   *****************
                    SUBROUTINE FILP11
!                   *****************
!
     &( F , C , XSOM , YSOM , NSOM , X , Y , NPOIN )
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
!| NPOIN          |-->| NUMBER OF POINTS
!| NSOM           |-->| NUMBER OF VERTICES IN THE POLYGON
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XSOM           |-->| ABSCISSAE OF POINTS IN THE POLYGON
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YSOM           |-->| ORDINATES OF POINTS IN THE POLYGON
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FILP11 => FILP11
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM , NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: F(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*) , Y(*)
      DOUBLE PRECISION, INTENT(IN) :: XSOM(NSOM) , YSOM(NSOM) , C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DO I = 1 , NPOIN
!
        IF(INPOLY(X(I),Y(I),XSOM,YSOM,NSOM)) F(I) = C
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
