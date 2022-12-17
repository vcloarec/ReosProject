!                       *******************
                        SUBROUTINE FLUX_ROE
!                       *******************
     &(HI,HJ,UI,UJ,VI,VJ,XN,YN,FLX)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Computes Roe fluxes at the ij interface
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  FLX        FLUX AT THE INTERFACE
!>@param  [in]      HI         WATER DEPTH ON THE LEFT CELL
!>@param  [in]      HJ         WATER DEPTH ON THE RIGHT CELL
!>@param  [in]      UI         VELOCITY U ON THE LEFT CELL
!>@param  [in]      UJ         VELOCITY U ON THE RIGHT CELL
!>@param  [in]      VI         VELOCITY V ON THE LEFT CELL
!>@param  [in]      VJ         VELOCITY V ON THE RIGHT CELL
!>@param  [in]      XN         X COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      YN         Y COMPONENT OF THE INTERFACE NORMAL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FLUX_ROE => FLUX_ROE
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV,EPS_FV
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: HI,HJ,UI,UJ,VI,VJ
      DOUBLE PRECISION, INTENT(IN) :: XN,YN
      DOUBLE PRECISION, INTENT(INOUT) :: FLX(3)

      DOUBLE PRECISION RI,RJ,CT2,UT,VT,RLAMB0
      DOUBLE PRECISION CT,PRII,PRIJ,ALPHA
      DOUBLE PRECISION RLAMBM,PS,SA,RLAMBP,TW(3)
      DOUBLE PRECISION TTR(3),T(3),CI2,CI,CJ,CJ2,RLAMBI,RLAMBJ
!
!-----------------------------------------------------------------------
!
!   --->    COMPUTES THE AVERAGES OF ROE OF U,V,H,C**2 AND C
!           ---------------------------------------------
!
      IF(HI.LE.0.D0) THEN
        RI = 0.D0
      ELSE
        RI = SQRT ( HI )
      ENDIF
      IF (HJ.LE.0.D0) THEN
        RJ = 0.D0
      ELSE
        RJ = SQRT ( HJ )
      ENDIF
!
      UT = ( RI * UI + RJ * UJ ) /MAX((RI + RJ),EPS_FV)
      VT = ( RI * VI + RJ * VJ ) /MAX((RI + RJ),EPS_FV)
      IF (HI+HJ.LE.0.D0)  THEN
        CT2= 0.D0
      ELSE
      CT2 = GRAV*(HI+HJ)*0.5D0
      ENDIF
      CT = SQRT ( CT2 )
!
!   --->  TESTS THE SIGN OF THE EIGENVALUE LAMB0 =
!           ----------------------------------------------------------
!
      RLAMB0 = UT * XN + VT * YN
!
!TBTB BEGINNING: MODIFICATION OF RLAMB0 IF RLAMB0
!C     IT IS NECESSARY TO ADD FLUXES FOR THE DUPLICATED EIGENVALUES
!C     TO BE COMPLETED BY WHOEVER WISHES TO
!C
!TBTB END
!
!---------------------------------------------------------------------
      IF  ( RLAMB0 . GE .-0.000001D0 ) THEN
!     ---- END SEGMENT ---------
!
!   --->    SMALL CALCULATIONS
!
        RLAMBM = RLAMB0 - CT
!
        PRII = GRAV*(HI**2)*0.5D0
        PRIJ = GRAV*(HJ**2)*0.5D0
        ALPHA = UI * XN + VI * YN
!
!TBTB BEGINNING : MODIFICATION OF RLAMBM IF RLAMBM
!
        IF (HI.LE.0.D0) THEN
        CI2 = 0.D0
        PRII = 0.D0
        ELSE
        CI2 =  2.D0*PRII / HI
        ENDIF
        IF (HJ.LE.0.D0) THEN
        CJ2 = 0.D0
        PRIJ = 0.D0
        ELSE
        CJ2 =  2.D0*PRIJ / HJ
        ENDIF
        CI = SQRT (CI2)
        CJ = SQRT (CJ2)
        RLAMBI = ALPHA - CI
        RLAMBJ = UJ * XN + VJ * YN - CJ
!
        IF ( RLAMBI .LT. 0.D0 .AND. RLAMBJ .GT. 0.D0) THEN
          RLAMBM = MIN(0.D0,RLAMBM) - ABS(RLAMBI - RLAMBJ) * 0.25D0
        ENDIF
!     END
!
!   --->    COMPUTES FLUX 1
!
        FLX(1) = ALPHA * HI
        FLX(2) = ALPHA * HI*UI
        FLX(3) = ALPHA * HI*VI
!
        FLX (2) = FLX(2) + PRII * XN
        FLX (3) = FLX(3) + PRII * YN
!
!   --->    TESTS THE SIGN OF LAMBDAM
!           ----------------------------
!
        IF ( RLAMBM . LT . 0.D0 ) THEN
!       - - - - - - - - - - - - - -
!
          T (1) = 1.D0
          T (2) = UT - CT * XN
          T (3) = VT - CT * YN
!
          TTR(1) = HJ-HI
          TTR(2) = HJ*UJ-HI*UI
          TTR(3) = HJ*VJ-HI*VI
!
          TW(1) = (UT*XN + VT*YN)*CT + CT2
          TW(2) = -XN*CT
          TW(3) = -YN*CT
!
          PS = TTR(1)*TW(1)+TTR(2)*TW(2)+TTR(3)*TW(3)
!
!   --->    COMPUTES TOTAL LOCAL FLUX
!           --------------------------
!
          SA = PS * RLAMBM / (2.D0 * CT2 )
          FLX(1)= FLX(1)+SA*T(1)
          FLX(2)= FLX(2)+SA*T(2)
          FLX(3)= FLX(3)+SA*T(3)
!
!
        ENDIF
!           -----
!
!      TESTEST
      ELSE
!      TESTEST
!
!   --->    SMALL CALCULATIONS
!           --------------
!
        RLAMBP = RLAMB0 + CT
!
!
        ALPHA = UJ * XN + VJ* YN
!
        IF (HI.LE.0.D0) THEN
          CI2 = 0.D0
        ELSE
          CI2 = GRAV*HI
        ENDIF
        CI = SQRT (CI2)
        IF (HJ.LE.0.D0) THEN
        CJ2 = 0.D0
        PRIJ = 0.D0
        ELSE
        CJ2 = GRAV*HJ
        PRIJ = GRAV*(HJ**2)*0.5D0
        ENDIF
        CJ = SQRT (CJ2)
        RLAMBI = UI * XN + VI * YN + CI
        RLAMBJ = ALPHA + CJ
!
        IF ( RLAMBI .LT. 0.D0.AND. RLAMBJ .GT. 0.D0) THEN
          RLAMBP = MAX(0.D0,RLAMBP) + ABS(RLAMBI - RLAMBJ) * 0.25D0
        ENDIF
!
!   --->    COMPUTES FLUX 1
!           ----------------
!
        FLX(1) = ALPHA * HJ
        FLX(2) = ALPHA * HJ*UJ
        FLX(3) = ALPHA * HJ*VJ
!
        FLX (2) = FLX(2) + PRIJ * XN
        FLX (3) = FLX(3) + PRIJ * YN
!
!   --->    TESTS THE SIGN OF LAMBDAP
!           ----------------------------
!
        IF ( RLAMBP . GT . 0.D0 ) THEN
!       - - - - - - - - - - - - - -
!
          T(1) = 1.D0
          T(2) = UT + CT * XN
          T(3) = VT + CT * YN
!
          TTR(1) = HJ-HI
          TTR(2) = HJ*UJ-HI*UI
          TTR(3) = HJ*VJ-HI*VI
!
          TW(1) = (-UT*XN - VT*YN)*CT +CT2
          TW(2) = CT*XN
          TW(3) = CT*YN
!
          PS = TTR(1)*TW(1)+TTR(2)*TW(2)+TTR(3)*TW(3)
!
!   --->    COMPUTES TOTAL LOCAL FLUX
!           --------------------------
!
          SA = - PS * RLAMBP / (2.D0 * CT2 )
          FLX(1)= FLX(1)+SA*T(1)
          FLX(2)= FLX(2)+SA*T(2)
          FLX(3)= FLX(3)+SA*T(3)
!
        ENDIF
!           -----
      ENDIF
!
      RETURN
      END
