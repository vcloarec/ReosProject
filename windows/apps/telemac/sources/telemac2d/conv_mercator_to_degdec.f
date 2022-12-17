!                     **********************************
                      SUBROUTINE CONV_MERCATOR_TO_DEGDEC
!                     **********************************
!
     &(NTAB,XTAB,YTAB,LAMBDATAB,PHITAB,GEOSYST,NUMZONE,LONG0,LAT0)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   22/04/2011
!***********************************************************************
!
!brief    CONVERSION OF COORDINATES METRIC MERCATOR
!+        INTO LATITUDES, LONGITUDES (DECIMAL DEGREES)
!
!history  C-T PHAM (LNHE)
!+        22/04/2011
!+        V6P1
!+
!
!history  C-T PHAM (LNHE)
!+        16/05/2012
!+        V6P2
!+        ADD MERCATOR FOR TELEMAC
!
!history  U.H.Merkel
!+        18/07/2012
!+        V6P2
!+        NAG doesn't like EPSILON -> renamed to CHOUIA
!
!history  S.E.BOURBAN (HRW) and M.S.TURNBULL (HRW)
!+        28/01/2016
!+        V7P2
!+        Replacement of COMPLEX numbers by their imaginary and real parts
!+        (to be compatible with algorithmic differentiation tools)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GEOSYST        |-->| TYPE OF GEOGRAPHIC SYSTEM
!|                |   | 2: UTM NORTH     3: UTM SOUTH
!|                |   | 5: MERCATOR FOR TELEMAC
!| LAMBDATAB      |<--| LONGITUDE (DECIMAL DEGREES)
!| LAT0           |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!|                |   | BEWARE!!! IN TELEMAC PHI0 IS LONGITUDE!!!
!| LONG0          |-->| LONGITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!|                |   | BEWARE!!! IN TELEMAC LAMBD0 IS LATITUDE!!!
!| NTAB           |-->| NUMBER OF COORDINATES
!| NUMZONE        |-->| NUMBER OF UTM ZONE
!| PHITAB         |<--| LATITUDE (DECIMAL DEGREES)
!| XTAB           |-->| METRIC COORDINATES (WGS84 UTM)
!| YTAB           |-->| METRIC COORDINATES (WGS84 UTM)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_CONV_MERCATOR_TO_DEGDEC
     &                         => CONV_MERCATOR_TO_DEGDEC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NTAB,GEOSYST,NUMZONE
      DOUBLE PRECISION, INTENT(IN)  :: LAT0,LONG0
      DOUBLE PRECISION, INTENT(IN)  :: XTAB(NTAB),YTAB(NTAB)
      DOUBLE PRECISION, INTENT(OUT) :: LAMBDATAB(NTAB),PHITAB(NTAB)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,DTR,RTD,CONST,AAA,FFF,EEE,EEE2,EEE4,EEE6,EEE8
      DOUBLE PRECISION LAMBDAC,NNN,XS,YS,PHIM,LATISO,LATISOS,ES2,CHOUIA
      DOUBLE PRECISION X,Y,LAMBDA,PHI
      DOUBLE PRECISION CITM(5)
      DOUBLE PRECISION, PARAMETER :: RADIUS = 6371000.D0
!      COMPLEX(KIND(1.D0)) ZPRIME,ZZZ
      DOUBLE PRECISION ZPRIME_R,ZPRIME_I,ZZZ_R,ZZZ_I
      DOUBLE PRECISION TWOK
!
      INTEGER I,J,K
!
!-----------------------------------------------------------------------
!
      PI  = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI
      CONST = TAN(0.5D0*LAT0*DTR+0.25D0*PI)
!
      CHOUIA = 1.D-11
!
      AAA = 6378137.D0
      FFF = 1.D0/298.257223563D0
      EEE = SQRT(2.D0*FFF-FFF**2)
!     EEE = 0.081991889980000D0
!
      IF(GEOSYST.EQ.2.AND.NUMZONE.EQ.-1) THEN
!     NORTHERN UTM
        WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR UTM ZONE.'
        WRITE(LU,*) 'TO BE CHOSEN BETWEEN 1 AND 60.'
        WRITE(LU,*) 'E.G. BETWEEN 30 AND 32 FOR FRANCE.'
        CALL PLANTE(1)
        STOP
      ELSEIF(GEOSYST.EQ.3.AND.NUMZONE.EQ.-1) THEN
!     SOUTHERN UTM
        WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR UTM ZONE.'
        WRITE(LU,*) 'TO BE CHOSEN BETWEEN 1 AND 60.'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      NNN = 0.9996D0 * AAA
      LAMBDAC = (6.D0*REAL(NUMZONE)-183.D0)*DTR ! RADIANS
      XS = 500000.D0
!
!     NORTHERN UTM
!
      IF(GEOSYST.EQ.2) THEN
        YS = 0.D0
!       SOUTHERN UTM
      ELSEIF(GEOSYST.EQ.3) THEN
        YS = 10000000.D0
      ENDIF
!
      ES2 = EEE/2.D0
!
      EEE2 = EEE**2
      EEE4 = EEE2**2
      EEE6 = EEE2*EEE4
      EEE8 = EEE4**2
!
!  PROJECTION COEFFICIENTS
!  MERCATOR TRANSVERSE PROJECTION "BACKWARD"
!  (IGN: ALG0029)
!
      CITM(1) = 1.D0 - 1.D0/4.D0*EEE2 - 3.D0/64.D0*EEE4
     &               - 5.D0/256.D0*EEE6 - 175.D0/16384.D0*EEE8
      CITM(2) = 1.D0/8.D0*EEE2 + 1.D0/48.D0*EEE4 + 7.D0/2048.D0*EEE6
     &                         + 1.D0/61440.D0*EEE8
      CITM(3) = 1.D0/768.D0*EEE4 + 3.D0/1280.D0*EEE6
     &                           + 559.D0/368640.D0*EEE8
      CITM(4) = 17.D0/30720.D0*EEE6 + 283.D0/430080.D0*EEE8
      CITM(5) = 4397.D0/41287680.D0*EEE8
!
      IF(GEOSYST.EQ.2.OR.GEOSYST.EQ.3) THEN
!
!  BEGINNING OF LOOP ON POINTS
!
      DO J=1,NTAB
        X = XTAB(J)
        Y = YTAB(J)
!       ZPRIME = DCMPLX((Y-YS), (X-XS))/NNN/CITM(1)
!        ZPRIME = CMPLX(Y-YS,X-XS,KIND(1.D0))/NNN/CITM(1)
        ZPRIME_R = (Y-YS)/NNN/CITM(1)
        ZPRIME_I = (X-XS)/NNN/CITM(1)

        ZZZ_R = ZPRIME_R
        ZZZ_I = ZPRIME_I

        DO K=1,4
          TWOK = 2.D0*REAL(K)
!          ZZZ = ZZZ - CITM(K+1)*SIN(2.D0*REAL(K)*ZPRIME)
          ZZZ_R = ZZZ_R
     &          - CITM(K+1)*SIN(TWOK*ZPRIME_R)*COSH(TWOK*ZPRIME_I)
          ZZZ_I = ZZZ_I
     &          - CITM(K+1)*COS(TWOK*ZPRIME_R)*SINH(TWOK*ZPRIME_I)
        ENDDO

!        LATISO  =  REAL(ZZZ)
        LATISO  = ZZZ_R
!        LATISOS = AIMAG(ZZZ)
        LATISOS = ZZZ_I
!
        LAMBDA = LAMBDAC + ATAN(SINH(LATISOS)/COS(LATISO))
        PHI    =           ASIN(SIN(LATISO)/COSH(LATISOS))
!
!  COMPUTATION OF LATITUDE PHI FROM ISOMETRIC LATITUDE LATISO
!  (ELLIPSOID OF 1ST EXCENTRICITY EEE AT POINT OF LATITUDE PHI)
!  (IGN: ALG0001)
!
        LATISO = LOG(TAN(PI/4.D0+PHI/2.D0)) ! EEE = 0.D0 HERE
!
!  COMPUTATION OF LATITUDE PHI FROM ISOMETRIC LATITUDE LATISO
!  (IGN: ALG0002)
!
!  I = 0
        PHIM = 2.D0*ATAN(EXP(LATISO))-PI/2.D0
!  I = 1
        PHI  = 2.D0*ATAN(EXP(LATISO)*( (1.D0+EEE*SIN(PHIM))
     &                                /(1.D0-EEE*SIN(PHIM)))**ES2)
     &        -PI/2.D0
!
        I = 1
!
        DO WHILE (ABS(PHI-PHIM).GE.CHOUIA)
          PHIM = PHI
          PHI  = 2.D0*ATAN(EXP(LATISO)*( (1.D0+EEE*SIN(PHIM))
     &                                  /(1.D0-EEE*SIN(PHIM)))**ES2)
     &          -PI/2.D0
          I = I + 1
        ENDDO
!
!  CONVERSION INTO DECIMAL DEGREES
!
        LAMBDA = LAMBDA*RTD
        PHI    = PHI*RTD
!
        LAMBDATAB(J) = LAMBDA
        PHITAB(J)    = PHI
      ENDDO
!
      ELSEIF(GEOSYST.EQ.5) THEN
        DO J=1,NTAB
          LAMBDATAB(J) = XTAB(J)/RADIUS*RTD + LONG0
          PHITAB(J)    = ( 2.D0*ATAN(CONST*EXP(YTAB(J)/RADIUS))
     &                    -0.5D0*PI)*RTD
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
