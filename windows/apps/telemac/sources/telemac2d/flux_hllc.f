!                     ********************
                      SUBROUTINE FLUX_HLLC
!                     ********************
     &(H1,H2,U1,U2,V1,V2,XNN,YNN,HLLCFLX,ROT)
!
!***********************************************************************
! TELEMAC 2D
!***********************************************************************
!
!>@brief  FUNCTION  : SUBROUTINE COMPUTES HLLC FLUX: THREE HYDRODYNAMICAL
!!                 COMPENENTS
!!            SEE TORO: SHOCK CAPTURING METHODS FOR FREE
!!            SURFACE FLOWS (WILEY 2005)
!
!>@history  RIADH ATA (EDF R&D-LNHE)
!!        07/15/2012
!!        V6P2
!!
!
!>@history  RIADH ATA (EDF R&D-LNHE)
!!        03/20/2013
!!        V6P3
!!  OPTIMIZATION OF THE CODE
!!  AVOID DIVISION BY 0
!
!>@history  RIADH ATA (EDF R&D-LNHE)
!!        10/6/2013
!!        V6P3
!!  BUG FIXED IN COMPUTING U*
!!  THANKS TO L. STADLER (BAW)
!
!>@history  RIADH ATA & S. PAVAN (EDF R&D-LNHE)
!!        10/04/2014
!!        V7P0
!!  IMPROVEMENT OF S* COMPUTATION FOR DRY CASES (GOTO ADDED)
!!  USE OF ANALYTICAL FORMULA FOR THESE CASES
!!  ADD TEST TO CHECK DIVISION BY ZERO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  HLLCFLX    FLUX AT THE INTERFACE
!>@param  [in]      H1         RECONSTRUCTED WATER DEPTH ON LEFT CELL
!>@param  [in]      H2         RECONSTRUCTED WATER DEPTH ON RIGHT CELL
!>@param  [in]      ROT        ROTATION
!>@param  [in]      U1         VELOCITY U ON THE LEFT CELL
!>@param  [in]      U2         VELOCITY U ON THE RIGHT CELL
!>@param  [in]      V1         VELOCITY V ON THE LEFT CELL
!>@param  [in]      V2         VELOCITY V ON THE RIGHT CELL
!>@param  [in]      XNN        X COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      YNN        Y COMPONENT OF THE INTERFACE NORMAL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_FLUX_HLLC => FLUX_HLLC
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV,EPS_FV
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)             :: ROT
      DOUBLE PRECISION, INTENT(IN)    :: H1,H2,U1,U2
      DOUBLE PRECISION, INTENT(IN)    :: V1,V2,XNN,YNN
      DOUBLE PRECISION, INTENT(INOUT) :: HLLCFLX(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,SPY
      DOUBLE PRECISION HL,UL,VL
      DOUBLE PRECISION HR,UR,VR
      DOUBLE PRECISION AL,AR,HSTAR,USTAR
      DOUBLE PRECISION PQL,PQR,SSL,SR
      DOUBLE PRECISION QSTARL(3),QSTARR(3)
      DOUBLE PRECISION QL(3),QR(3),FL(3),FR(3)
      DOUBLE PRECISION FSTARL(3),FSTARR(3)
!
      DOUBLE PRECISION GSUR2,DENOM
      DOUBLE PRECISION FLU2X,FLU2Y
      DOUBLE PRECISION U0,POND,SSTAR
      DOUBLE PRECISION FLX(3)
!
!-----------------------------------------------------------------------
!
      GSUR2 = GRAV/2.0D0
      SPY   = 0
      PQL   = 0.0D0
      PQR   = 0.0D0
      USTAR = 0.0D0
      HSTAR = 0.0D0
      AL    = 0.0D0
      AR    = 0.0D0
!***********************************************************************
! INITIALIZATION OF FLX AND HLLCFLX
      DO I=1,3
        FLX(I)     = 0.0D0
        HLLCFLX(I) = 0.0D0
      ENDDO
!
!-----------------------------------------------------------------------
! DEPTHS, VELOCITIES, TRACERS
      HL    = H1
      UL    = U1
      VL    = V1
!
      HR    = H2
      UR    = U2
      VR    = V2
!
! ROTATION
!
      U0 = UL
      UL  = XNN*U0+YNN*VL
      VL  =-YNN*U0+XNN*VL

      U0 = UR
      UR  = XNN*U0+YNN*VR
      VR  =-YNN*U0+XNN*VR
!
! CASE WITH DRY LEFT AND RIGHT
      IF(HL.LT.EPS_FV.AND.HR.LT.EPS_FV)GOTO 20
!
! CELERITIES
!
      AL = SQRT(GRAV*HL)
      AR = SQRT(GRAV*HR)
! STAR VARIABLES
      HSTAR = 0.5D0*(HL+HR)-0.25D0*(UR-UL)*(HL+HR)/(AL+AR)
!RA BUG FIXED WHEN COMPUTING U STAR
!       USTAR = 0.5D0*(UL+UR)-0.25D0*(HR-HL)*(AL+AR)/(HL+HR)
      USTAR = 0.5D0*(UL+UR)-       (HR-HL)*(AL+AR)/(HL+HR)
! COMPUTE PQL AND PQR:
! IT WILL DEPEND IF WE ARE IN PRESENCE OF SHOCK OR RAREFACTION WAVE
      IF(HSTAR.LT.HL)THEN
!       RAREFACTION
        PQL = 1.0D0
      ELSE
!       SHOCK
        IF(HL.GT.EPS_FV)THEN
          PQL = SQRT(0.5D0*(HSTAR+HL)*HSTAR/HL**2)
        ELSE
          PQL = 0.0D0
        ENDIF
      ENDIF
      IF(HSTAR.LT.HR)THEN
!       RAREFACTION
        PQR = 1.0D0
      ELSE
!       SHOCK
        IF(HR.GT.EPS_FV)THEN
          PQR = SQRT(0.5D0*(HSTAR+HR)*HSTAR/HR**2)
        ELSE
          PQR = 0.0D0
        ENDIF
      ENDIF
!
20    CONTINUE
!
! COMPUTE SSL, SR AND SSTAR  (WE CONSIDER DRY CASES)
      IF(HL.GT.EPS_FV)THEN
        SSL = UL-AL*PQL
      ELSE
        SSL = UR - 2.0D0*AR
        SR = UR + AR
! RA+SP: USE OF ANALYTICAL FORMULA FOR SSTAR
!       SSTAR = SSL
        GOTO 35
      ENDIF
!
      IF(HR.GT.EPS_FV)THEN
        SR = UR + AR*PQR
      ELSE
        SSL = UL - AL
        SR = UL + 2.0D0*AL
! RA+SP: USE OF ANALYTICAL FORMULA FOR SSTAR
!       SSTAR = SR
        GOTO 35
      ENDIF
!RA      SSTAR = USTAR
35    CONTINUE
      DENOM = HR*(UR-SR)-HL*(UL-SSL)
      IF(ABS(DENOM).LT.EPS_FV)THEN
        SSTAR = USTAR
      ELSE
        SSTAR = (SSL*HR*(UR-SR)-SR*HL*(UL-SSL))/DENOM
      ENDIF
!END RA
!
! COMPUTE QL AND QR
      QL(1)     = HL
      QL(2)     = HL*UL
      QL(3)     = HL*VL
!
      QR(1)     = HR
      QR(2)     = HR*UR
      QR(3)     = HR*VR

! COMPUTE QSTARL AND QSTARR
      IF(ABS(SSL-SSTAR).GT.EPS_FV)THEN
        POND = HL*(SSL-UL)/(SSL-SSTAR)
      ELSE
        POND = 0.0D0
      ENDIF
      QSTARL(1) = POND
      QSTARL(2) = POND*SSTAR
      QSTARL(3) = POND*VL
!
      IF(ABS(SR-SSTAR).GT.EPS_FV)THEN
        POND = HR*(SR-UR)/(SR-SSTAR)
      ELSE
        POND = 0.0D0
      ENDIF
      QSTARR(1) = POND
      QSTARR(2) = POND*SSTAR
      QSTARR(3) = POND*VR
!
! COMPUTE FL AND FR
!
      FL(1)     = HL*UL
      FL(2)     = HL*UL**2 +GSUR2*HL**2
      FL(3)     = HL*UL*VL
!
      FR(1)     = HR*UR
      FR(2)     = HR*UR**2 +GSUR2*HR**2
      FR(3)     = HR*UR*VR
!
! COMPUTE FSTARL SFTARR
      FSTARL(1) = FL(1) + SSL*(QSTARL(1)-QL(1))
      FSTARL(2) = FL(2) + SSL*(QSTARL(2)-QL(2))
      FSTARL(3) = FL(3) + SSL*(QSTARL(3)-QL(3))
!
      FSTARR(1) = FR(1) + SR*(QSTARR(1)-QR(1))
      FSTARR(2) = FR(2) + SR*(QSTARR(2)-QR(2))
      FSTARR(3) = FR(3) + SR*(QSTARR(3)-QR(3))
! AND FINALLY THE HLLC FLUX (BEFORE ROTATION)
      IF(SSL.GT.0.D0)THEN
        FLX(1) = FL(1)
        FLX(2) = FL(2)
        FLX(3) = FL(3)
        SPY = 1
      ELSEIF(SSTAR.GT.0.D0.AND.SSL.LT.0.D0) THEN
        FLX(1) = FSTARL(1)
        FLX(2) = FSTARL(2)
        FLX(3) = FSTARL(3)
        SPY = 1
      ELSEIF(SSTAR.LT.0.D0.AND.SR.GT.0.D0) THEN
        FLX(1) = FSTARR(1)
        FLX(2) = FSTARR(2)
        FLX(3) = FSTARR(3)
        SPY = 1
      ELSE
        FLX(1) = FR(1)
        FLX(2) = FR(2)
        FLX(3) = FR(3)
        SPY = 1
      ENDIF
      IF(SPY.EQ.0)THEN
        WRITE(LU,*)'ERROR IN HLLC FLUX ESTIMATION (FLUX_HLLC.F)'
        CALL PLANTE(1)
        STOP
      ENDIF
!
! INVERSE ROTATION AND FINAL FLUX
!
      IF(ROT)THEN
        FLU2X  = XNN*FLX(2) - YNN*FLX(3)
        FLU2Y  = YNN*FLX(2) + XNN*FLX(3)
!
        HLLCFLX(1) = FLX(1)
        HLLCFLX(2) = FLU2X
        HLLCFLX(3) = FLU2Y
      ELSE
! IN THIS CASE, NO ROTATION
!
! FINAL FLUX
!
        HLLCFLX(1) = FLX(1)
        HLLCFLX(2) = FLX(2)
        HLLCFLX(3) = FLX(3)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
