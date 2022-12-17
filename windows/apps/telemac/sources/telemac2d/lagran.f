!                   *****************
                    SUBROUTINE LAGRAN
!                   *****************
!
     &(NLAG,DEBLAG,FINLAG)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES FIRST AND FINAL TIMESTEPS FOR THE LAGRANGIAN DRIFTS.
!
!warning  TWO DRIFTS CANNOT COMPLETE IN THE SAME TIMESTEP (ONLY THE 1ST WILL BE WRITTEN TO FILE)
!warning  THE RESULTS MUST BE SAVED BETWEEN TWO DRIFT COMPUTATION ENDS
!
!history  J-M JANIN (LNH)
!+        17/08/1994
!+        V5P2
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
!| DEBLAG         |<--| TIME STEP AT THE BEGINNING
!| FINLAG         |<--| TIME STEP AT THE END
!| NLAG           |-->| NUMBER OF LAGRANGIAN DRIFTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NLAG
      INTEGER, INTENT(INOUT) :: DEBLAG(NLAG) , FINLAG(NLAG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      CALL USER_LAGRAN(NLAG,DEBLAG,FINLAG)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
