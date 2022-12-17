!                   ***************
                    SUBROUTINE MINI
!                   ***************
!
     &( XMIN , IMIN , X , NPOIN )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    LOOKS FOR THE LOWEST VALUE IN ARRAY X
!+                OF DIMENSION NPOIN.
!
!history  E. PELTIER   (LNH)
!+        17/08/94
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
!| IMIN           |<--| INDEX OF MINIMUM
!| NPOIN          |-->| DIMENSION OF X
!| X              |-->| ARRAY WITH VALUES TO LOOK AT
!| XMIN           |<--| THE MINIMUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      INTEGER, INTENT(INOUT)          :: IMIN
      DOUBLE PRECISION, INTENT(INOUT) :: XMIN
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      XMIN = X(1)
      IMIN = 1
!
      DO I = 2, NPOIN
!
        IF(X(I).LT.XMIN) THEN
          IMIN = I
          XMIN = X(I)
        ENDIF
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
