!                   *****************
                    SUBROUTINE PTEL41
!                   *****************
!
     &(XEL,X,IKLE,NELMAX,NELEM)
!
!***********************************************************************
! BIEF   V6P3                                          03/01/2013
!***********************************************************************
!
!brief    GOES FROM A VECTOR BY POINTS TO A VECTOR BY ELEMENTS.
!+                CASE OF A LINEAR PRISM.
!
!history  J-M HERVOUET (LNH)
!+        03/01/2013
!+        V6P3
!+    First version.
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
      INTEGER, INTENT(IN) :: NELEM,NELMAX,IKLE(NELMAX,6)
!
!-----------------------------------------------------------------------
!
!     VECTORS STRUCTURES
!
      DOUBLE PRECISION, INTENT(IN)    :: X(*)
      DOUBLE PRECISION, INTENT(INOUT) :: XEL(NELMAX,6)
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
        XEL(IELEM,4)=X(IKLE(IELEM,4))
        XEL(IELEM,5)=X(IKLE(IELEM,5))
        XEL(IELEM,6)=X(IKLE(IELEM,6))
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
