!                   ***********************
                    SUBROUTINE ATTEND(ISEC)
!                   ***********************
!
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WAITS FOR A PERIOD OF TIME: ISEC IN SECONDS.
!
!note     PLEASE ADD OR REMOVE COMMENTS ACCORDING TO YOUR COMPILER
!
!history  NATHALY BARBRY (UNIVERSITE DE CAEN); J-M HERVOUET (LNHE)
!+        08/02/2001
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
!| ISEC           |-->| THE LAPSE OF TIME TO WAIT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!-----------------------------------------------------------------------
!     VERSION F90 STANDARD
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: ISEC
      INTEGER T1,T2
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
      T1 = TIME_IN_SECONDS()
      T2 = T1
!                            .AND. : WHEN CLOCK RESET TO ZERO
      DO WHILE (T2.LT.T1+ISEC.AND.T2.GE.T1)
        T2 = TIME_IN_SECONDS()
      END DO
!
!-----------------------------------------------------------------------
!     VERSION F95 NAG
!-----------------------------------------------------------------------
!
!     USE F90_UNIX_PROC
!     INTEGER, INTENT(IN) :: ISEC
!     CALL SLEEP(ISEC)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
