!                   *********************
                    SUBROUTINE USER_VUSCE
!                   *********************
!
     &( VUSCE, TIME , I )
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    GIVES THE VALUE OF VELOCITY ALONG X AT SOURCES.
!+                ALLOWS TO DEFINE VELOCITIES THAT ARE VARIABLE IN
!+                TIME AND IN THE VERTICAL.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF THE SOURCE
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(INOUT) :: VUSCE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!     USCE STEMS FROM THE STEERING FILE, BUT VUSCE MAY BE MODIFIED HERE
!     (READ IN A FILE, ETC.)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
