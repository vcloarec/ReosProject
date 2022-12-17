!                   **********************
                    SUBROUTINE USER_DEBSCE
!                   **********************
!
     &(TIME, I, DISCE, DEBSCE)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    USER GIVES THE PRESCRIBED DISCHARGE OF EVERY SOURCE POINT.
!+
!+            VARIATIONS WRT TIME AND SPACE MAY BE IMPLEMENTED.
!
!note     T2DVEF IS THE SOURCES FILE IN TELEMAC-2D
!
!history  J-M HERVOUET (LNHE)
!+        03/04/2008
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DISCE          |-->| ARRAY OF DISCHARGES OF SOURCES.
!|                |   | READ IN THE PARAMETER FILE.
!|                |   | NAME OF DISCE IS DSCE IN TELEMAC-2D.
!| I              |-->| NUMBER OF THE SOURCE OR OF THE SOURCE REGION
!| TIME           |-->| TIME
!| DEBSCE         |-->| DISCHARGE OF SOURCE POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
!     USE DECLARATIONS_TELEMAC2D, ONLY: MAXSCE,AT,ENTET,NREJET,DT,
!    &                                  T2D_FILES,T2DVEF,OKDEBSCE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME,DISCE(*)
      INTEGER         , INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(INOUT) :: DEBSCE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
