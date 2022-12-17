!                   **********************
                    SUBROUTINE USER_STRCHE
!                   **********************
!
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM FRICTION COEFFICIENT
!+                IF VARIABLE IN SPACE.
!
!note     IN PARAMETER ESTIMATION WITH A LIST OF TESTS,
!+         THESE VALUES ARE DISCARDED.
!
!history  J-M HERVOUET (LNH)
!+        01/10/96
!+        V5P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
!     DECLARATIONS MUST BE ADAPTED TO EVERY CODE
!     THIS EXAMPLE APPLIES TO TELEMAC2D
!
!     USE DECLARATIONS_TELEMAC2D
!     USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!     HERE A CONSTANT FRICTION VALUE IS GIVEN
!
!     DO I=1,NPOIN
!       CHESTR%R(I) = 60.D0
!     ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMENTS HERE MAY BE CHANGED
!
      WRITE(LU,*) 'STRCHE (BIEF): NO MODIFICATION OF FRICTION'
      WRITE(LU,*)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
