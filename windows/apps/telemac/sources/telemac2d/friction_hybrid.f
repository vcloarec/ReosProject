!                   **************************
                    SUBROUTINE FRICTION_HYBRID
!                   **************************
!
     &(VA,HA,CDX,LAI,UREF,VOGEL,HVEG,KARMAN,CP)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR SUBMERGED AND NON-SUBMERGED
!         VEGETATION USING A HYBRID MODEL BETWEEN BAPTIST (2007) APPROACH
!         FOR FREE LAYER AND JAERVELA (2004) APPROACH FOR VEGETATION LAYER
!
!history  MOHAMAD ATTIEH
!+        **/02/2019
!+
!+   The algorithm was developed by Baptist
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CDX            |-->| SPECIES SPECIFIC DRAG COEFFICIENT
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| HEIGHT OF VEGETATION
!| KARMAN         |-->| VON KARMAN CONSTANT
!| LAI            |-->| LAI VEGETATION PARAMETER
!| UREF           |-->| REFERENCE VELOCITY
!| VA             |-->| VELOCITY
!| VOGEL          |-->| VOGEL COEFFICIENT FOR FLEXIBILITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_HYBRID => FRICTION_HYBRID
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: CDX, LAI, UREF, VOGEL, HVEG
      DOUBLE PRECISION, INTENT(OUT) :: CP
      DOUBLE PRECISION, INTENT(IN)  :: VA, HA, KARMAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TERM1, TERM2
!
!-----------------------------------------------------------------------
!
      TERM1 = 1.D0/
     &  SQRT(CDX * LAI * (MAX(VA/UREF,1.D0))**VOGEL * MIN(HA/HVEG,1.D0))
      TERM2 = LOG(MAX(HA/HVEG,1.D0))/KARMAN/SQRT(2.D0)
      CP = 1.D0/(TERM1+TERM2)**2
!
!-----------------------------------------------------------------------
!
      RETURN
      END
