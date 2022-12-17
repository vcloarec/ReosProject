!                   *****************
                    SUBROUTINE PTEL11
!                   *****************
!
     &(XEL,X,IKLE,NELMAX,NELEM)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GOES FROM A VECTOR BY POINTS TO A VECTOR BY ELEMENTS.
!+                CASE OF A P1 TRIANGLE.
!
!history  J-M HERVOUET (LNH)
!+        10/01/95
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
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| X              |-->| VECTOR DEFINED PER POINT
!| XEL            |<--| SAME VECTOR DEFINED PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,IKLE(NELMAX,3)
!
!-----------------------------------------------------------------------
!
!     VECTORS STRUCTURES
!
      DOUBLE PRECISION, INTENT(IN)    :: X(*)
      DOUBLE PRECISION, INTENT(INOUT) :: XEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1,NELEM
!
        XEL(IELEM,1)=X(IKLE(IELEM,1))
        XEL(IELEM,2)=X(IKLE(IELEM,2))
        XEL(IELEM,3)=X(IKLE(IELEM,3))
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
