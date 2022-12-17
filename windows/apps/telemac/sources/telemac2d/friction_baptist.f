!                   ***************************
                    SUBROUTINE FRICTION_BAPTIST
!                   ***************************
!
     &(HA,CD,MD,HVEG,KARMAN,CP)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR SUBMERGED AND NON-SUBMERGED
!+        VEGETATION FROM PARAMETERS WITH BAPTIST ET AL.(2007) APPROACH
!
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P2
!+
!+   The algorithm was developed by Baptist
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CD             |-->| BULK DRAG COEFFICIENT FOR VEGETATION
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| HEIGHT OF VEGETATION
!| KARMAN         |-->| VON KARMAN CONSTANT
!| MD             |-->| VEGETATION DENSITY * VEGETATION DIAMETER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_BAPTIST => FRICTION_BAPTIST
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: HA, CD, MD, HVEG, KARMAN
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TERM1, TERM2
!
!-----------------------------------------------------------------------
!
      IF(MIN(HA,HVEG).LE.0.D0) THEN
        CP = 0.D0
      ELSE
        TERM1 = 1.D0/SQRT(CD*MD*MIN(HA,HVEG))
        TERM2 = LOG(MAX(HA/HVEG,1.D0))/KARMAN/SQRT(2.D0)
        CP = 1.D0/(TERM1+TERM2)**2
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
