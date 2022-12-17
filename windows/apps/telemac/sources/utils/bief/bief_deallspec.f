!                   *************************
                    SUBROUTINE BIEF_DEALLSPEC
!                   *************************
!
     &(SPEC,NSPE)
!
!***********************************************************************
! BIEF   V8P0
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A SPECTRAL DATA STRUCTURE : SPEC.
!
!history  N.DURAND (HRW)
!+        14/01/19
!+
!+   Original
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SPEC           |<->| STRUCTURE TO BE DEALLOCATED
!| NSPE           |<--| NUMBER OF TOMAWAC SPECTRA IN FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SPECTRUM)   , INTENT(INOUT)        :: SPEC
      INTEGER NSPE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO I=1,NSPE
        DEALLOCATE(SPEC%ADR(I)%SOUTER)
        NULLIFY(SPEC%ADR(I)%SOUTER)
      ENDDO
      DEALLOCATE(SPEC%ADR)
      NULLIFY(SPEC%ADR)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(SPEC%NOUTER)
      NULLIFY(SPEC%NOUTER)
      DEALLOCATE(SPEC%XOUTER)
      NULLIFY(SPEC%XOUTER)
      DEALLOCATE(SPEC%YOUTER)
      NULLIFY(SPEC%YOUTER)
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(SPEC%DIR)
      NULLIFY(SPEC%DIR)
      DEALLOCATE(SPEC%FRE)
      NULLIFY(SPEC%FRE)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
