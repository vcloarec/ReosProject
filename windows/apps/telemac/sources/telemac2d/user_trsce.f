!                   *********************
                    SUBROUTINE USER_TRSCE
!                   *********************
!
     &(TIME, I, ITRAC, TRSCE)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER PRESCRIBES THE TRACER VALUES AT THE SOURCES.
!+                THIS VALUE MAY VARY IN TIME.
!
!history  J-M HERVOUET (LNHE)
!+        08/04/2008
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| SOURCE RANK
!| ITRAC          |-->| TRACER RANK
!| TIME           |-->| TIME
!| TRSCE          |<->| TRACER AT THE SOURCE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
!     USE DECLARATIONS_TELEMAC2D, ONLY: AT,ENTET,NTRAC,TSCE,NREJET,
!    &                                  T2D_FILES,T2DVEF,OKTRSCE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I,ITRAC
      DOUBLE PRECISION, INTENT(INOUT) :: TRSCE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
