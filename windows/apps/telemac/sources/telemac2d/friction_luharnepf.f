!                   *****************************
                    SUBROUTINE FRICTION_LUHARNEPF
!                   *****************************
!
     &(HA,CD,CV,A,HVEG,CP)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR VEGETATION
!+        WITH LUHAR AND NEPF(2013) APPROACH
!
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P2
!+
!+   The algorithm was developed by Luhar and Nepf
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| FRONTAL AREA PER VOLUME
!| CD             |-->| BULK DRAG COEFFICIENT FOR VEGETATION
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| CV             |-->| FRICTION COEFFICIENT ON TOP OF VEGETATION LAYER
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| HEIGHT OF VEGETATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_LUHARNEPF =>
     &    FRICTION_LUHARNEPF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: HA,CD,CV,A,HVEG
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TERM1, TERM2
!
!-----------------------------------------------------------------------
!
      TERM1 = SQRT(1.D0/CV)*SQRT((1.D0-MIN(HVEG/HA,1.D0))**3)
      TERM2 = MIN(HVEG/HA,1.D0)/SQRT(CD*A*MIN(HA,HVEG))
      CP = 1.D0/(TERM1+TERM2)**2
!
!-----------------------------------------------------------------------
!
      RETURN
      END
