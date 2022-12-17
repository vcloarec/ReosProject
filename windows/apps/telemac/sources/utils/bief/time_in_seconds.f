!                   ********************************
                    INTEGER FUNCTION TIME_IN_SECONDS
!                   ********************************
!
     &()
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MACHINE TIME IN SECONDS.
!
!history  J-M HERVOUET (LNH)
!+        04/08/98
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER TEMPS,PARSEC
!
!----------------------------------------------------------------------
!
      CALL SYSTEM_CLOCK(COUNT=TEMPS,COUNT_RATE=PARSEC)
      TIME_IN_SECONDS = TEMPS / PARSEC
!
!----------------------------------------------------------------------
!
      RETURN
      END
