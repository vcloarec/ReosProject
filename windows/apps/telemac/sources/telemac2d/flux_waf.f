!                       *******************
                        SUBROUTINE FLUX_WAF
!                       *******************
     &(H1,H2,U1,U2,V1,V2,HL_UP,HR_UP,VL_UP,VR_UP,XNN,YNN,DX,WAFFLX)
!
!***********************************************************************
! TELEMAC 2D
!***********************************************************************
!
!>@brief  FUNCTION  : SUBROUTINE COMPUTES WAF FLUX: THREE HYDRODYNAMICAL
!                 COMPENENTS
!
!>@history  RIADH ATA (EDF R&D-LNHE)
!!        07/15/2012
!!        V6P2
!
!>@history  RIADH ATA (EDF R&D-LNHE)
!!        03/20/2013
!!        V6P3
!!  OPTIMIZATION OF THE CODE
!!  AVOID DIVISION BY 0
!
!>@history  RIADH ATA (EDF R&D-LNHE)
!!        12/11/2013
!!        V6P3
!!  BUG FIXED IN COMPUTING U*, THANKS TO L. STADLER (BAW)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      DT         TIME STEP
!>@param  [in]      DX         SPACE STEP
!>@param  [in]      H1         RECONSTRUCTED WATER DEPTH ON LEFT CELL
!>@param  [in]      H2         RECONSTRUCTED WATER DEPTH ON RIGHT CELL
!>@param  [in]      HL_UP      WATER DEPTH OF LEFT NEIGHBOR
!>@param  [in]      HR_UP      WATER DEPTH OF RIGHT NEIGHBOR
!>@param  [in]      U1         VELOCITY U ON THE LEFT CELL
!>@param  [in]      U2         VELOCITY U ON THE RIGHT CELL
!>@param  [in]      V1         VELOCITY V ON THE LEFT CELL
!>@param  [in]      V2         VELOCITY V ON THE RIGHT CELL
!>@param  [in]      VL_UP      VELOCITY OF LEFT NEIGHBOR
!>@param  [in]      VR_UP      VELOCITY OF RIGHT NEIGHBOR
!>@param  [in,out]  WAFFLX     FLUX AT THE INTERFACE
!>@param  [in]      XNN        X COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      YNN        Y COMPONENT OF THE INTERFACE NORMAL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FLUX_WAF => FLUX_WAF
      USE DECLARATIONS_TELEMAC2D, ONLY:DT,GRAV,EPS_FV
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: H1,H2,U1,U2
      DOUBLE PRECISION, INTENT(IN)    :: V1,V2,XNN,YNN
      DOUBLE PRECISION, INTENT(IN)    :: HL_UP,HR_UP,VL_UP,VR_UP
      DOUBLE PRECISION, INTENT(IN)    :: DX
      DOUBLE PRECISION, INTENT(INOUT) :: WAFFLX(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ILIM
      LOGICAL ROT,TVD
!
      DOUBLE PRECISION HL,UL,VL
      DOUBLE PRECISION HR,UR,VR
      DOUBLE PRECISION AL,AR,HSTAR,USTAR
      DOUBLE PRECISION PQL,PQR,SLI,SR
      DOUBLE PRECISION FL(3),FR(3)
!
      DOUBLE PRECISION GSUR2,DTDX
      DOUBLE PRECISION CL,CR,CSTAR,WL,WR
      DOUBLE PRECISION WLR,WLSTAR,WRSTAR
      DOUBLE PRECISION FLU2X,FLU2Y
      DOUBLE PRECISION U0,SSTAR
      DOUBLE PRECISION FLX(3),HLLCFLX(3)
!
      DOUBLE PRECISION LIM_RL,LIM_RR,LIM_RSTAR
      DOUBLE PRECISION RL,RR,RSTAR,DELTA
!
      INTRINSIC SIGN
!
!-----------------------------------------------------------------------
!
      GSUR2 = GRAV/2.0D0
      ROT   = .FALSE.
      TVD   = .TRUE.
      ILIM  = 4
      PQL   = 0.D0
      PQR   = 0.D0
      USTAR = 0.D0
      HSTAR = 0.D0
      AL    = 0.D0
      AR    = 0.D0
!
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF FLX, HLLCFLX AND WAFFLX
      DO I=1,3
        FLX(I)     = 0.D0
        HLLCFLX(I) = 0.D0
        WAFFLX(I)  = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     DEPTHS, VELOCITIES, TRACERS
!
      HL    = H1
      UL    = U1
      VL    = V1
!
      HR    = H2
      UR    = U2
      VR    = V2
!
! LET'S START BY COMPUTING GNIHLLC FLUX (WITHOUT INVERSE ROTATION IN THE END)
!
      CALL FLUX_HLLC(HL,HR,UL,UR,VL,VR,
     &               XNN,YNN,HLLCFLX,ROT)
!
! ROTATION
!
      U0  = UL
      UL  = XNN*U0+YNN*VL
      VL  =-YNN*U0+XNN*VL
!
      U0  = UR
      UR  = XNN*U0+YNN*VR
      VR  =-YNN*U0+XNN*VR
!
! CASE WITH DRY LEFT AND RIGHT
      IF(HL.LT.EPS_FV.AND.HR.LT.EPS_FV) GOTO 20
!
! CELERITIES
!
      AL = SQRT(GRAV*HL)
      AR = SQRT(GRAV*HR)
!
! STAR VARIABLES
!
      HSTAR = 0.5D0*(HL+HR)-0.25D0*(UR-UL)*(HL+HR)/(AL+AR)
!RA BUG FIXED WHEN COMPUTING U STAR, THANKS TO L.STADLER (BAW)
!     USTAR = 0.5D0*(UL+UR)-0.25D0*(HR-HL)*(AL+AR)/(HL+HR)
      USTAR = 0.5D0*(UL+UR)-       (HR-HL)*(AL+AR)/(HL+HR)

! COMPUTE PQL AND PQR:
! IT DEPENDS IF WE ARE IN PRESENCE OF SHOCK OR RAREFACTION WAVE
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
! FL AND FR
!
      FL(1)   = HL*UL
      FL(2)   = HL*UL**2 +GSUR2*HL**2
      FL(3)   = HL*UL*VL
!
      FR(1)   = HR*UR
      FR(2)   = HR*UR**2 +GSUR2*HR**2
      FR(3)   = HR*UR*VR
!
!     SLI, SR AND SSTAR  (WE CONSIDER DRY CASES)
!
      IF(HL.GT.EPS_FV) THEN
        SLI   = UL-AL*PQL
      ELSE
        SLI   = UR - 2.D0*AR
        SR    = UR + AR
        SSTAR = SLI
      ENDIF
!
      IF(HR.GT.EPS_FV)THEN
        SR    = UR + AR*PQR
      ELSE
        SLI   = UL - AL
        SR    = UL + 2.D0*AL
        SSTAR = SR
        GOTO 35
      ENDIF
      SSTAR   = USTAR

35    CONTINUE
!
! WEIGHTING COEFFICIENTS WL,WLR, WR WLSTAR AND WRSTAR
!
!     COURANT NUMBERS FOR ALL WAVES
      DTDX  = DT/DX
      CL    = SLI*DTDX
      CR    = SR*DTDX
      CSTAR = SSTAR*DTDX
!
!===================================================
!   NON TVD WAF SCHEME
!===================================================
!
      IF(.NOT.TVD) THEN
!
!     COEFFICIENTS
      WL     = 0.5D0*(1.D0 + CL)
      WR     = 0.5D0*(1.D0 - CR)
      WLR    = 0.5D0*(CR - CL)
      WLSTAR = 0.5D0*(1.D0 + CSTAR)
      WRSTAR = 0.5D0*(1.D0 - CSTAR)
!
!     FINAL FLUX (BEFORE ROTATION)
!
      FLX(1) = WL*FL(1) + WLR*HLLCFLX(1) + WR*FR(1)
      FLX(2) = WL*FL(2) + WLR*HLLCFLX(2) + WR*FR(2)
      FLX(3) = (WLSTAR*VL + WRSTAR*VR)*FLX(1)
!
!===================================================
!    TVD WAF SCHEME
!===================================================
!
      ELSE
!
!     LIMITERS
!     PREPARE rK BEFORE CALLING LIMITER
!     COMPUTE ALL rK (SEE LOUKILI ET AL. PAGE 4)
!       RL
        IF(SLI.GT.0.D0)THEN
          DELTA = HL-HL_UP
        ELSE
          DELTA = HR_UP-HR
        ENDIF
        RL = DELTA/(HR-HL + EPS_FV)
!       RR
        IF(SR.GT.0.0D0)THEN
          DELTA = HL-HL_UP
        ELSE
          DELTA = HR_UP-HR
        ENDIF
        RR = DELTA/(HR-HL + EPS_FV)
!       r*
        IF(SSTAR.GT.0.D0)THEN
          DELTA = VL-VL_UP
        ELSE
          DELTA = VR_UP-VR
        ENDIF
        RSTAR = DELTA/(VR-VL+EPS_FV)
!
        LIM_RL    = LIMITER(ILIM,RL,CL)
        LIM_RR    = LIMITER(ILIM,RR,CR)
        LIM_RSTAR = LIMITER(ILIM,RSTAR,CSTAR)
!
!   TVD COEFFICIENTS
!
      WL     = 0.5D0*(1.D0 + SIGN(1.D0,CL)*LIM_RL) !SIGN(A,B)=|A|*SIGN(B)
      WR     = 0.5D0*(1.D0 - SIGN(1.D0,CR)*LIM_RR)
      WLR    = 0.5D0*(SIGN(1.D0,CR)*LIM_RR - SIGN(1.D0,CL)*LIM_RL)
      WLSTAR = 0.5D0*(1.D0 + SIGN(1.D0,CSTAR)*LIM_RSTAR)
      WRSTAR = 0.5D0*(1.D0 - SIGN(1.D0,CSTAR)*LIM_RSTAR)
!
! FINAL FLUX (BEFORE ROTATION)
!
      FLX(1) = WL*FL(1) + WLR*HLLCFLX(1) + WR*FR(1)
      FLX(2) = WL*FL(2) + WLR*HLLCFLX(2) + WR*FR(2)
      FLX(3) = (WLSTAR*VL    + WRSTAR*VR   )*FLX(1)
!
      ENDIF
!
! INVERSE ROTATION
!
      FLU2X  = XNN*FLX(2) - YNN*FLX(3)
      FLU2Y  = YNN*FLX(2) + XNN*FLX(3)
!
! FINAL WAF FLUX
!
      WAFFLX(1) = FLX(1)
      WAFFLX(2) = FLU2X
      WAFFLX(3) = FLU2Y
!
!-----------------------------------------------------------------------
!
      RETURN
      END
