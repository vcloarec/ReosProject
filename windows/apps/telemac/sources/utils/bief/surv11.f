!                   *****************
                    SUBROUTINE SURV11
!                   *****************
!
     &(SURFAC, XEL,YEL,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE AREA (VOLUME) OF THE ELEMENTS OF A MESH.
!+                TRIANGLES IN THIS CASE.
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |<--| AREA OR VOLUME OF ELEMENTS
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: SURFAC(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        SURFAC(IELEM) = 0.5D0 * ( X2*Y3 - X3*Y2 )
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
