!                   *****************
                    SUBROUTINE LOINOY
!                   *****************
!
     &(YAM,YAV,YS,PHI,DEB,G)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DISCHARGE LAW FOR A WET WEIR.
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
!| YAV            |-->| DOWNSTREAM ELEVATION
!| YS             |-->| ELEVATION OF WEIR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: DEB
      DOUBLE PRECISION, INTENT(IN)    :: G,YAM,YAV,PHI,YS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(YAM.LT.YS.AND.YAV.LT.YS) THEN
        DEB=0.D0
      ELSE
        DEB=2.598D0*PHI*SQRT(2.D0*G)*(YAV-YS)*SQRT(YAM-YAV)
!           2.598D0 IS THE INVERSE OF SQRT(1/3)*2/3
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
