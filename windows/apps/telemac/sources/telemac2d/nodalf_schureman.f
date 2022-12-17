!                       ***************************
                        SUBROUTINE NODALF_SCHUREMAN
!                       ***************************
!
     &(FWAVE,TEMPS,DEJA,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    COMPUTES NODAL FACTORS F FROM SCHUREMAN FORMULAE
!+
!
!history  C-T PHAM (LNHE)
!+        13/01/2012
!+        V6P2
!+
!
!history  C-T PHAM (LNHE)
!+        08/01/2014
!+        V7P0
!+   Adding 7 extra harmonic constituents, for Previmer database.
!+
!
!history  L.LEBALLEUR (ACTIMAR)
!+        30/03/2017
!+        V7P3
!+   Adding 7 extra harmonic constituents, for FES2014 database.
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEJA           |-->| LOGICAL FOR 1ST TIME STEP
!| FWAVE          |<--| NODAL FACTOR FOR 61 WAVES
!| MARDAT         |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR,MINUTE,SECOND)
!| TEMPS          |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_NODALF_SCHUREMAN => NODALF_SCHUREMAN
      USE DECLARATIONS_TELEMAC2D, ONLY : YEAR,NDAY,HOUR,MINUTE,SECOND
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3)
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: FWAVE(*)
      DOUBLE PRECISION, INTENT(IN)  :: TEMPS
      LOGICAL, INTENT(IN)           :: DEJA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,DAY,MONTH
!
      DOUBLE PRECISION PI,TWOPI,DTR
      DOUBLE PRECISION TJ,PLUN,NLUN
      DOUBLE PRECISION XI,NU,TGI2,PPP,X1QA,X1RA,IANG,TGN2,AT1,AT2
      DOUBLE PRECISION F73,F74,F75,F76,F77,F78,F79,F149,F207,F215,
     &                 F227,F235
!
      INTRINSIC INT,MOD,DBLE,COS,ATAN
!
!-----------------------------------------------------------------------
!
      PI  = 4.D0*ATAN(1.D0)
      TWOPI = 2.D0*PI
      DTR = PI/180.D0
!
      IF(.NOT.DEJA) THEN
        YEAR  = MARDAT(1)
        MONTH = MARDAT(2)
        DAY   = MARDAT(3)
!
        HOUR   = MARTIM(1)
        MINUTE = MARTIM(2)
        SECOND = MARTIM(3)
!  NUMBER OF THE DAY IN YEAR YEAR
        NDAY = DAY
!
        DO I=MONTH-1,1,-1
          IF((I.EQ.1).OR.(I.EQ.3).OR.(I.EQ.5).OR.(I.EQ.7).OR.(I.EQ.8)
     &    .OR.(I.EQ.10)) THEN
            NDAY = NDAY + 31
          ELSEIF((I.EQ.4).OR.(I.EQ.6).OR.(I.EQ.9).OR.(I.EQ.11)) THEN
            NDAY = NDAY + 30
          ELSEIF(I.EQ.2) THEN
            IF((MOD(YEAR,4).NE.0)
     &      .OR.((MOD(YEAR,100).EQ.0).AND.(MOD(YEAR,400).NE.0))) THEN
              NDAY = NDAY + 28
            ELSE
              NDAY = NDAY + 29
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
      TJ = DBLE(365*(YEAR-1900)+(NDAY-1)
     &         +DBLE(INT(DBLE(YEAR-1901)/4.D0)))/36525.D0
     &   +(DBLE(HOUR)+DBLE(MINUTE)/60.D0+DBLE(SECOND)/3600.D0)/876600.D0
     &   +TEMPS/3.15576D9
!
!-----------------------------------------------------------------------
!
! SCHUREMAN FORMULAE P. 162, ORDER 2 IS ENOUGH
! IN DEGREES
! TJ TIME ELAPSED SINCE 01/01/1900 AT 0 H, IN JULIAN CENTURY
!
      PLUN = MOD(334.3837215D0+  4069.0322056D0*TJ-1.03444D-2*TJ**2,
     &           360.D0)
      NLUN = MOD(259.1560564D0-  1934.1423972D0*TJ+ 2.1056D-3*TJ**2,
     &           360.D0)
!
! CONVERSION IN RADIANS
! FROM NOW, EVERY ANGLE IS IN RADIAN
      PLUN = MOD(PLUN*DTR,TWOPI)
      NLUN = MOD(NLUN*DTR,TWOPI)
!
      IF (PLUN.LT.0.D0) PLUN = PLUN + TWOPI
      IF (NLUN.LT.0.D0) NLUN = NLUN + TWOPI
!
! SCHUREMAN FORMULAE P. 156
!
! IANG FROM TUGO TOOLS TIDES.CPP (MORE DECIMALS THAN IN SCHUREMAN P. 156)
      IANG = ACOS(0.913694997D0-0.035692561D0*COS(NLUN))
!
      TGN2 = TAN(NLUN/2.D0)
      AT1 = ATAN(1.01883D0*TGN2)
      AT2 = ATAN(0.64412D0*TGN2)
!
      XI = -AT1-AT2+NLUN
      IF (NLUN.GT.PI) THEN
        XI = XI-TWOPI
      ENDIF
      NU = AT1-AT2
!
! FOR CONSTITUENTS L2,K1,K2
!
      TGI2 = TAN(IANG/2.D0)
! SCHUREMAN P. 41 (191)
      PPP  = PLUN-XI
! SCHUREMAN P. 41 (197)
      X1QA = SQRT(2.310D0+1.435D0*COS(2.D0*PPP))
! SCHUREMAN P. 44 (213)
      X1RA = SQRT(1.D0-12.D0*TGI2**2*COS(2.D0*PPP)+36.D0*TGI2**4)
!
!-----------------------------------------------------------------------
!
! NODAL FACTORS, RECURRENT FORMULAE
!
! SCHUREMAN P. 25 (75)-(79)
      F73 = (2.D0/3.D0-SIN(IANG)**2)/0.5021D0
      F74 = SIN(IANG)**2/0.1578D0
      F75 = SIN(IANG)*COS(IANG/2.D0)**2/0.38D0
      F76 = SIN(2.D0*IANG)/0.7214D0
      F77 = SIN(IANG)*SIN(IANG/2.D0)**2/0.0164D0
      F78 = COS(IANG/2.D0)**4/0.9154D0
      F79 = SIN(IANG)**2/0.1565D0
! SCHUREMAN P. 36 (144)
!      F144 = (1.D0-10.D0*SIN(IANG/2.D0)**2+15.D0*SIN(IANG/2.D0)**4)
!     &      *COS(IANG/2.D0)**2/0.5873D0
! SCHUREMAN P. 36 (149)
      F149 = COS(IANG/2.D0)**6/0.8758D0
! SCHUREMAN P. 43 (207)
      F207 = F75*X1QA
! SCHUREMAN P. 44 (215)
      F215 = F78*X1RA
! SCHUREMAN P. 45 (227)
      F227 = SQRT( 0.8965D0*SIN(2.D0*IANG)**2
     &            +0.6001D0*SIN(2.D0*IANG)*COS(NU)+0.1006D0)
! SCHUREMAN P. 46 (235)
      F235 = SQRT( 19.0444D0*SIN(IANG)**4
     &            + 2.7702D0*SIN(IANG)**2*COS(2.D0*NU)+0.0981D0)
!
!-----------------------------------------------------------------------
!
! 2MK6     M2+M2+K2
      FWAVE(1) = F78**2*F235
! 2MN6     TAB 2A
      FWAVE(2) = F78**3
! 2MS6     TAB 2A
      FWAVE(3) = F78**2
! 2N2      A42
      FWAVE(4) = F78
! 2Q1      A17
      FWAVE(5) = F75
! 2SM2     TAB 2A
      FWAVE(6) = F78
! 2SM6     TAB 2A
      FWAVE(7) = F78
! EPSILON2 ( = MNS2)    TAB 2A
      FWAVE(8) = F78**2
! TTA1     A28
      FWAVE(9) = F76
! J1       A24
      FWAVE(10) = F76
! K1       NOTE 2
      FWAVE(11) = F227
! K2       NOTE 4
      FWAVE(12) = F235
! KJ2      A49
      FWAVE(13) = F79
! KQ1      A32
      FWAVE(14) = F77
! L2       NOTE 3
      FWAVE(15) = F215
! LAMBDA2  A44
      FWAVE(16) = F78
! M1       NOTE 1
      FWAVE(17) = F207
! M2       A39
      FWAVE(18) = F78
! M3       A82
      FWAVE(19) = F149
! M4       TAB 2A
      FWAVE(20) = F78**2
! M6       TAB 2A
      FWAVE(21) = F78**3
! M8       TAB 2A
      FWAVE(22) = F78**4
! MF       A6
      FWAVE(23) = F74
! MK3      TAB 2A
      FWAVE(24) = F78*F227
! MK4      TAB 2A
      FWAVE(25) = F78*F235
! MKS2     M2+K2-S2
      FWAVE(26) = F78*F235
! MM       A2
      FWAVE(27) = F73
! MN4      TAB 2A
      FWAVE(28) = F78**2
! MO3      M2+O1
      FWAVE(29) = F78*F75
! MP1      A29, NOT M2-P1
      FWAVE(30) = F76
! MS4      TAB 2A
      FWAVE(31) = F78 ! SCHUREMAN PROBABLY WRONG???
! MSF      A5
      FWAVE(32) = F73
! MSK6     M2+S2+K2
      FWAVE(33) = F78*F235
! MSN2     M2+S2-N2
      FWAVE(34) = F78**2
! MSN6     TAB 2A
      FWAVE(35) = F78**2
! MSQM     A12
      FWAVE(36) = F74
! MTM      A7
      FWAVE(37) = F74
! MU2      A45
      FWAVE(38) = F78
! N2       A40
      FWAVE(39) = F78
! N4       N2+N2
      FWAVE(40) = F78**2
! NU2      A43
      FWAVE(41) = F78
! O1       A14
      FWAVE(42) = F75
! OO1      A31
      FWAVE(43) = F77
! P1       B14
      FWAVE(44) = 1.D0
! PI1      B15
      FWAVE(45) = 1.D0
! Q1       A15
      FWAVE(46) = F75
! R2       B41
      FWAVE(47) = 1.D0
! RHO1     A18
      FWAVE(48) = F75
! S1       B71
      FWAVE(49) = 1.D0
! S2       B39
      FWAVE(50) = 1.D0
! S4       TAB 2A
      FWAVE(51) = 1.D0
! SA       B64
      FWAVE(52) = 1.D0
! SIGMA1   A20
      FWAVE(53) = F75
! SK4      S2+K2
      FWAVE(54) = F235
! SN4      S2+N2
      FWAVE(55) = F78
! SSA      B6
      FWAVE(56) = 1.D0
! T2       B40
      FWAVE(57) = 1.D0
! PHI1     B31
      FWAVE(58) = 1.D0
! KI1      A27
      FWAVE(59) = F76
! PSI1     B24
      FWAVE(60) = 1.D0
! Z0
      FWAVE(61) = 1.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
