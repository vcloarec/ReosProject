!                       ************************
                        SUBROUTINE NODALUPV_PUGH
!                       ************************
!
     &(UPVM2,UPVN2,UPVS2,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   23/03/2011
!***********************************************************************
!
!brief    COMPUTES NODAL FACTORS PHASE FROM PUGH FORMULAE
!+
!
!history  C-T PHAM (LNHE)
!+        23/03/2011
!+        V6P1
!+
!
!history  U.H.Merkel
!+        18/07/2012
!+        V6P2
!+   NAG doesn't like DINT
!
!history  C-T PHAM (LNHE)
!+        31/10/2012
!+        V6P3
!+   Bug correction when MARTIM not equal to midnight or noon
!+   Introduction of TT + UPVS2 (new output argument)
!+   New calculation of UPVM2 and UPVN2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| UPVM2          |<--| U+V (ORIGIN + NODAL PHASE) FOR WAVE M2
!| UPVN2          |<--| U+V (ORIGIN + NODAL PHASE) FOR WAVE N2
!| UPVS2          |<--| U+V (ORIGIN + NODAL PHASE) FOR WAVE S2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_NODALUPV_PUGH => NODALUPV_PUGH
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(OUT) :: UPVM2,UPVN2,UPVS2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,DTR
      DOUBLE PRECISION SLUNPUGH,HSOLPUGH,PLUNPUGH,NLUNPUGH
!     DOUBLE PRECISION PSOLPUGH
      DOUBLE PRECISION TJ,TT
      DOUBLE PRECISION VVM2,UUM2,VVN2,UUN2,VVS2
!
      INTEGER YEAR,MONTH,DAY,NDAY,HOUR,MINUTE,SECOND,I
!
      INTRINSIC DBLE,MOD,INT
!
!-----------------------------------------------------------------------
!
      PI  = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
!
      YEAR  = MARDAT(1)
      MONTH = MARDAT(2)
      DAY   = MARDAT(3)
!
      HOUR   = MARTIM(1)
      MINUTE = MARTIM(2)
      SECOND = MARTIM(3)
!
!     NUMBER OF THE DAY IN YEAR YEAR
!
      NDAY = DAY
!
      DO I=MONTH-1,1,-1
        IF((I.EQ.1).OR.(I.EQ.3).OR.(I.EQ.5).OR.(I.EQ.7).OR.(I.EQ.8)
     &  .OR.(I.EQ.10)) THEN
          NDAY = NDAY + 31
        ELSEIF((I.EQ.4).OR.(I.EQ.6).OR.(I.EQ.9).OR.(I.EQ.11)) THEN
          NDAY = NDAY + 30
        ELSEIF(I.EQ.2) THEN
          IF((MOD(YEAR,4).NE.0)
     &    .OR.((MOD(YEAR,100).EQ.0).AND.(MOD(YEAR,400).NE.0))) THEN
            NDAY = NDAY + 28
          ELSE
            NDAY = NDAY + 29
          ENDIF
        ENDIF
      ENDDO
!
      TJ = DBLE(365*(YEAR-1900)+(NDAY-1)
     &         +DBLE(INT(DBLE(YEAR-1901)/4.D0)))/36525.D0
     &   +(DBLE(HOUR)+DBLE(MINUTE)/60.D0+DBLE(SECOND)/3600.D0)/876600.D0
!
      SLUNPUGH = MOD(277.02D0+481267.89D0*TJ+0.0011D0*TJ**2,360.D0)
      HSOLPUGH = MOD(280.19D0+ 36000.77D0*TJ+0.0003D0*TJ**2,360.D0)
      PLUNPUGH = MOD(334.39D0+  4069.04D0*TJ-0.0103D0*TJ**2,360.D0)
      NLUNPUGH = MOD(259.16D0-  1934.14D0*TJ+0.0021D0*TJ**2,360.D0)
!     PSOLPUGH = MOD(281.22D0+     1.72D0*TJ+0.0005D0*TJ**2,360.D0)
!
! TT MEAN GREENWICH SOLAR ANGLE, ORIGIN AT ZENITH
! 15.D0 DEG = PI/12.D0
! PI/12.D0*24.D0 = TWOPI
      TT = 360.D0*(TJ*36525.D0-DBLE(INT(TJ*36525.D0)))+180.D0
!
      VVM2 = MOD(2.D0*(HSOLPUGH-SLUNPUGH+TT),360.D0)
      UUM2 = -2.1D0*SIN(NLUNPUGH*DTR)
      VVN2 = MOD(-3.D0*SLUNPUGH+2.D0*HSOLPUGH+PLUNPUGH+2.D0*TT,360.D0)
      UUN2 = UUM2
      VVS2 = MOD(2.D0*TT,360.D0)
!
      UPVM2 = MOD(UUM2+VVM2,360.D0)
      UPVN2 = MOD(UUN2+VVN2,360.D0)
      UPVS2 = VVS2
!
      IF(UPVM2.LT.0.D0) UPVM2 = UPVM2 + 360.D0
      IF(UPVN2.LT.0.D0) UPVN2 = UPVN2 + 360.D0
      IF(UPVS2.LT.0.D0) UPVS2 = UPVS2 + 360.D0
!
!     DEGREES TO RADIANS CONVERSIONS
!
      UPVM2 = UPVM2*DTR
      UPVN2 = UPVN2*DTR
      UPVS2 = UPVS2*DTR
!
!-----------------------------------------------------------------------
!
      RETURN
      END
