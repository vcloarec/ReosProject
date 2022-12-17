!                   ***************
                    SUBROUTINE MAXI
!                   ***************
!
     &( XMAX , IMAX , X , NPOIN )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    LOOKS FOR THE GREATEST VALUE IN ARRAY X
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
!| IMAX           |<--| INDEX OF MAXIMUM
!| NPOIN          |-->| DIMENSION OF ARRAY X
!| X              |-->| ARRAY WITH VALUES TO LOOK AT
!| XMAX           |<--| THE MAXIMUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      INTEGER, INTENT(INOUT)          :: IMAX
      DOUBLE PRECISION, INTENT(INOUT) :: XMAX
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      XMAX = X(1)
      IMAX = 1
!
      DO I = 2 , NPOIN
!
        IF(X(I).GT.XMAX) THEN
          IMAX = I
          XMAX = X(I)
        ENDIF
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
