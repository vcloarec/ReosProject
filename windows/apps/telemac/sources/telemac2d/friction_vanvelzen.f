!                   *****************************
                    SUBROUTINE FRICTION_VANVELZEN
!                   *****************************
!
     &(HA,CD,MD,HVEG,KARMAN,CP)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION COEFFICIENT FOR SUBMERGED AND NON-SUBMERGED
!+        VEGETATION FROM PARAMETERS WITH VAN VELZEN (2003) APPROACH
!
!history  FREDERIK FOLKE (BAW)
!+        07/11/2019
!+        V8P2
!+
!+   The algorithm was developed by Van Velzen
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CD             |-->| BULK DRAG COEFFICIENT FOR VEGETATION
!| CP             |<--| VEGETATION FRICTION COEFFICIENT
!| HA             |-->| WATER DEPTH
!| HVEG           |-->| HEIGHT OF VEGETATION
!| KARMAN         |-->| VON KARMAN CONSTANT
!| MD             |-->| VEGETATION DENSITY* VEGETATION DIAMETER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_VANVELZEN =>
     &    FRICTION_VANVELZEN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: HA, CD, MD, HVEG, KARMAN
      DOUBLE PRECISION, INTENT(OUT) :: CP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ALPHA, K, L, M, HS, LS, Z0, TERM1, TERM2
      DOUBLE PRECISION US_GS, UV_GS, USTAR, UVEG
!
!-----------------------------------------------------------------------
!
! ALPHA    CLOSURE PARAMETER
! K, L, M  HELP VARIABLE
! HS       DISTANCE BETWEEN TOP OF THE VEGETATION AND VIRTUAL BED OF THE
!          SURFACE LAYER
! LS       SCALING LENGTH
! US_GS    DEPTH AVERAGED VELOCITY IN SURFACE LAYER DIVIDED BY GRAVITY
!          AND SLOPE
! USTAR    SHEAR VELOCITY OF SURFACE LAYER
! UV_GS    DEPTH AVERAGED VELOCITY IN VEGETATION LAYER DIVIDED BY
!          GRAVITY AND SLOPE
! UVEG     VELOCITY THROUGH VEGETATION FOR NON-SUBMERGED CONDITIONS
! Z0       ROUGHNESS HEIGHT OF SURFACE LAYER
!
      UVEG = SQRT(2.D0/CD/MD)
      IF(HA.LE.HVEG) THEN
        CP = CD*MD*HA
      ELSE
        ALPHA = 0.0227D0*HVEG**0.7D0
        LS = SQRT(ALPHA/CD/MD)
        K = (HA-HVEG)*LS/ALPHA/COSH(HVEG/LS)

        TERM1 = SQRT(K*EXP(HVEG/LS)+UVEG**2)
        TERM2 = SQRT(K+UVEG**2)

        UV_GS = 2.D0*LS/HVEG*(TERM1-TERM2) + UVEG*LS/HVEG*
     &    LOG((TERM1-UVEG)*(TERM2+UVEG)/(TERM1+UVEG)/(TERM2-UVEG))
        L = K*COSH(HVEG/LS)/LS/SQRT(2.D0*K*SINH(HVEG/LS)+UVEG**2)
        HS = (1.D0+SQRT(1.D0+4.D0*L**2*KARMAN**2*(HA-HVEG)))/
     &    (2.D0*L**2*KARMAN**2)
        USTAR = SQRT(HA-HVEG+HS)
        M = KARMAN*SQRT(2.D0*K*SINH(HVEG/LS)+UVEG**2)/USTAR
        Z0 = HS*EXP(-M)
        US_GS = USTAR/KARMAN/(HA-HVEG)*
     &    (USTAR**2*LOG(USTAR**2/Z0)-HS*LOG(HS/Z0)-HA+HVEG)

        CP = 2.D0*HA / (HVEG/HA*UV_GS + (HA-HVEG)/HA*US_GS)/
     &        (HVEG/HA*UV_GS + (HA-HVEG)/HA*US_GS)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
