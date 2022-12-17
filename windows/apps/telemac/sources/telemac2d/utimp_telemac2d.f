!                   **************************
                    SUBROUTINE UTIMP_TELEMAC2D
!                   **************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DEPRECATED KEPT FOR RETRO COMPATIBILITY USE
!+        USER_UTIMP_TELEMAC2D INSTEAD
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
!+        V5P4
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
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL USER_UTIMP_TELEMAC2D
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP_TELEMAC2D
