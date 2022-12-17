!                   *****************************
                    SUBROUTINE FRICTION_WHITTAKER
!                   *****************************
!
     &(VA,HA,CD0,AP0,EI,VOGEL,SP,HVEG,ROEAU,CP)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR SUBMERGED AND NON-SUBMERGED
!+        VEGETATION FROM PARAMETERS WITH WHITTAKER (2015) APPROACH
!
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P2
!+
!+   The algorithm was developed by Whittaker
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AP0            |-->| RIGID PROJECTED AREA
!| CD0            |-->| REFERENCE DRAG COEFFICIENT (RIGID)
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| EI             |-->| FLEXURAL RIGIDITY
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| UNDEFLECTED VEGETATION HEIGHT
!| ROEAU          |-->| WATER DENSITY
!| SP             |-->| SPACING BETWEEN ROUGHNESS ELEMENTS
!| VA             |-->| VELOCITY
!| VOGEL          |-->| VOGEL COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_WHITTAKER =>
     &    FRICTION_WHITTAKER
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: VA, HA, ROEAU
      DOUBLE PRECISION, INTENT(IN)  :: CD0, AP0, EI, VOGEL, SP, HVEG
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION CA
!
!-----------------------------------------------------------------------
!
!     CA CAUCHY NUMBER
      CA = ROEAU/EI*AP0*MIN(HA/HVEG,1.D0)*HVEG**2*VA**2
      CP = CD0/SP/SP*AP0*MIN(HA/HVEG,1.D0)*CA**(VOGEL/2.D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
