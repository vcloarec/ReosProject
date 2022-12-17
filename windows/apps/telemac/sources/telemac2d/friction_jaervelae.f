!                   *****************************
                    SUBROUTINE FRICTION_JAERVELAE
!                   *****************************
!
     &(VA,HA,CDX,LAI,UREF,VOGEL,HVEG,CP)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR SUBMERGED AND NON-SUBMERGED
!+        VEGETATION FROM PARAMETERS WITH JAERVELAE APPROACH
!
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P2
!+
!+   The algorithm was developed by Jaervelae
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CDX            |-->| SPECIES SPECIFIC DRAG COEFFICIENT
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| HEIGHT OF VEGETATION
!| LAI            |-->| LAI VEGETATION PARAMETER
!| UREF           |-->| REFERENCE VELOCITY
!| VA             |-->| VELOCITY
!| VOGEL          |-->| VOGEL COEFFICIENT FOR FLEXIBILITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_JAERVELAE =>
     &    FRICTION_JAERVELAE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: VA, HA
      DOUBLE PRECISION, INTENT(IN)  :: CDX, LAI, UREF, VOGEL, HVEG
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CP = CDX * LAI * (MAX(VA/UREF,1.D0))**VOGEL * MIN(HA/HVEG,1.D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
