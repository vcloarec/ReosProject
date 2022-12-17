!                   *****************
                    SUBROUTINE LOIDEN
!                   *****************
!
     &(YAM,YS,PHI,DEB,G)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DISCHARGE LAW FOR A DRY WEIR.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        03/10/1996
!+        V5P2
!+   MODIFIED
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
!| DEB            |<--| DISCHARGE OF WEIR
!| G              |-->| GRAVITY.
!| PHI            |-->| DISCHARGE COEFFICIENT OF WEIR.
!| YAM            |-->| UPSTREAM ELEVATION
!| YS             |-->| ELEVATION OF WEIR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: DEB
      DOUBLE PRECISION, INTENT(IN)    :: G,YAM,PHI,YS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      IF(YAM.GT.YS) THEN
        DEB=PHI*SQRT(2.D0*G)*(YAM-YS)**1.5D0
      ELSE
        DEB=0.D0
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
