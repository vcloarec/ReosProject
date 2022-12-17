!                   ***************************
                    SUBROUTINE FRICTION_VASTILA
!                   ***************************
!
     &(VA,HA,CDXF,LAI,UREFF,VOGELF,CDXS,SAI,UREFS,VOGELS,HVEG,CP)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR SUBMERGED AND NON-SUBMERGED
!+        VEGETATION FROM PARAMETERS WITH VASTILA(2014) APPROACH
!
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P2
!+
!+   The algorithm was developed by Jaervelae & Vastila
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| CDXF           |-->| SPECIES SPECIFIC DRAG COEFFICIENT FOR FOLIAGE
!| CDXS           |-->| SPECIES SPECIFIC DRAG COEFFICIENT FOR STEM
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| HEIGHT OF VEGETATION
!| LAI            |-->| LEAF AREA INDEX
!| SAI            |-->| STEM AREA INDEX
!| VOGELF         |-->| FOLIAGE VOGEL COEFFICIENT
!| VOGELS         |-->| STEM VOGEL COEFFICIENT
!| UREFF          |-->| FOLIAGE REFERENCE VELOCITY
!| UREFS          |-->| STEM REFERENCE VELOCITY
!| VA             |-->| VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX FRICTION_VASTILA => FRICTION_VASTILA
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: VA, HA, HVEG
      DOUBLE PRECISION, INTENT(IN) :: CDXF, LAI, UREFF, VOGELF
      DOUBLE PRECISION, INTENT(IN) :: CDXS, SAI, UREFS, VOGELS
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION CPF, CPS
!
!-----------------------------------------------------------------------
!
      CPF = CDXF*LAI*(MAX(VA/UREFF,1.D0))**VOGELF*MIN(HA/HVEG,1.D0)
      CPS = CDXS*SAI*(MAX(VA/UREFS,1.D0))**VOGELS*MIN(HA/HVEG,1.D0)
      CP  = CPS + CPF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
