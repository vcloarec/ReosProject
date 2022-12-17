!                   *****************
                    SUBROUTINE COSASA
!                   *****************
!
     &(SIGMANU,NUMIN,NUMAX)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   31/08/2015
!***********************************************************************
!
!brief    SETS THE CONSTANTS FOR THE SPALART ALLMARAS MODEL.
!
!
!history  A BOURGOIN (LNHE)
!+        31/08/2015
!+        V7p0
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NUMAX           |<--| MAXIMUM VISCSA IF CLIPPING
!| NUMIN           |<--| MINIMUM VISCSA IF CLIPPING
!| SIGMANU         |<--| K-EPSILON CONSTANT FOR DIFFUSION OF VISCSA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(OUT) :: NUMIN,NUMAX
      DOUBLE PRECISION, INTENT(OUT) :: SIGMANU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SIGMANU = 2.D0/3.D0
!
!     RANGE OF VALUES USED TO CLIP SA VISCOSITY
!
      NUMIN = 1.D-6
      NUMAX = 1.D6
!    TO BE CONTINUED : BRING HERE CONSTANTS FROM ROUTINE SPALART-ALMARAS.F
!
!-----------------------------------------------------------------------
!
      RETURN
      END
