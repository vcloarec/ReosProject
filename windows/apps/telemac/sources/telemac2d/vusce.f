!                   *******************************
                    DOUBLE PRECISION FUNCTION VUSCE
!                   *******************************
!
     &( TIME , I )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE VALUE OF VELOCITY ALONG X AT SOURCES.
!+                ALLOWS TO DEFINE VELOCITIES THAT ARE VARIABLE IN
!+                TIME AND IN THE VERTICAL.
!
!history  J-M HERVOUET (LNH)
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      VUSCE = USCE(I)
!
      CALL USER_VUSCE(VUSCE, TIME, I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
