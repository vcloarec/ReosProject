!                   ****************
                    MODULE GRACESTOP
!                   ****************
!
!
!***********************************************************************
! SPECIAL
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      LOGICAL :: BREAKER = .FALSE.
!SGI      INTEGER, PARAMETER :: SIGUSR1=16
!LINUX    INTEGER, PARAMETER :: SIGUSR1=10
!IBM      INTEGER, PARAMETER :: SIGUSR1=30
      INTEGER, PARAMETER :: SIGUSR1=16
      CONTAINS
!
      SUBROUTINE HANDLER
        IMPLICIT NONE
!!!        WRITE(*,*) 'SIGNAL ',SIGUSR1,' CAUGHT'
        BREAKER = .TRUE.
      END SUBROUTINE HANDLER
!
      SUBROUTINE GRACE
        IMPLICIT NONE
        WRITE(*,*) 'I STOP GRACEFULLY'
        STOP 0
      END SUBROUTINE GRACE
!
      SUBROUTINE TRAPSIG
      IMPLICIT NONE
!      INTEGER ISIGNAL, SIGNAL
!
!SGI        ISIGNAL = SIGNAL(SIGUSR1, HANDLER, -1)
!LINUX      ISIGNAL = SIGNAL(SIGUSR1, HANDLER)
!IBM        CALL SIGNAL(SIGUSR1, HANDLER)
!
!  CHOICE HERE : NOTHING
!       ISIGNAL = SIGNAL(SIGUSR1, HANDLER, -1)
!
      END SUBROUTINE TRAPSIG
!
      END MODULE GRACESTOP
