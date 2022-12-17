!                   *************************
                    SUBROUTINE BORD_TIDE_MISC
!                   *************************
!
     &(ZF,NBOR,LIHBOR,LIUBOR,NPOIN,NPTFR,TEMPS,DT,
     & NUMLIQ,KENT,KENTU,TIDALTYPE,CTIDE,MSL,
     & CTIDEV,NODALCORR,NFOT,
     & BOUNDARY_COLOUR,HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     & MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   08/01/2014
!***********************************************************************
!
!brief    MODIFIES THE BOUNDARY CONDITIONS ARRAYS FOR TIDES
!+                WHEN THEY VARY IN TIME.
!+
!
!history  C-T PHAM (LNHE)
!+        12/01/2012
!+        V6P2
!+
!
!history  C-T PHAM (LNHE)
!+        08/01/2014
!+        V7P0
!+   Change of the name BORD_TIDE_LEGOS to BORD_TIDE_MISC
!+   (e.g. LEGOS-NEA, Previmer, FES).
!+   Adding 7 extra harmonic constituents.
!
!history  C-T PHAM (LNHE)
!+        30/06/2015
!+        V7P1
!+   Schematic tides
!
!history  C-T PHAM (LNHE)
!+        13/07/2017
!+        V7P3
!+   Adding 7 extra harmonic constituents, for FES2014 (L.LEBALLEUR).
!+   Because of the addition of M3 between M2 and M4, OMEGA_BTM cannot
!+   taken for index 19 for quarter diurnal constituents
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BOUNDARY_COLOUR|-->| AN INTEGER LINKED TO BOUNDARY POINTS
!|                |   | BY DEFAULT THE LAST LINE OF BOUNDARY CONDITIONS
!|                |   | FILE, HENCE THE GLOBAL BOUNDARY NUMBER, BUT CAN
!|                |   | BE CHANGED BY USER.
!| CTIDE          |-->| COEFFICIENT TO CALIBRATE THE TIDAL RANGE
!| CTIDEV         |-->| COEFFICIENT TO CALIBRATE THE VELOCITIES
!| DT             |-->| TIME STEP
!| HBTIDE         |<->| WATER DEPTH ON TIDAL BOUNDARY CONDITIONS
!| ICALHW         |<->| NUMBER THAT MAY BE CHOSEN BY THE USER
!|                |   | TO CALIBRATE HIGH WATER OR AUTOMATICALLY CHOSEN
!|                |   | IN CASE OF THE MODELLING OF A SCHEMATIC TIDE
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MARDAT         |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR,MINUTE,SECOND)
!| MSL            |-->| COEFFICIENT TO CALIBRATE THE SEA LEVEL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NFOT           |-->| LOGICAL UNIT OF HARMONIC CONSTANTS FILE
!| NODALCORR      |-->| OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION F
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NUMTIDE        |<->| NUMBER OF THE TIDAL BOUNDARY
!|                |   | ASSOCIATED TO EACH POINT OF THE BOUNDARY
!| TEMPS          |-->| TIME IN SECONDS
!| TIDALTYPE      |-->| TYPE OF TIDE TO MODEL
!| UBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| VBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD_TIDE_MISC => BORD_TIDE_MISC
      USE DECLARATIONS_TELEMAC2D, ONLY : DEJA_BTM,MISC_NCMX_BTM,
     &                 NTIDE_BTM,NPTFRL_BTM,NWAVES_BTM,FIRSTTIDE_BTM,
     &                 LASTTIDE_BTM,SHIFTTIDE_BTM,INDW_BTM,
     &                 NAMEWAVE_BTM,AH_BTM,PH_BTM,AU_BTM,PU_BTM,
     &                 AV_BTM,PV_BTM,LON_BTM,LAT_BTM,UPV_BTM,FF_BTM,
     &                 OMEGA_BTM,PHCALHW_BTM,MISC_CONSTID_BTM,
     &                 INDW2_BTM,INDW3_BTM,NWAVES2_BTM
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,NFOT
      INTEGER, INTENT(IN)             :: KENT,KENTU,NODALCORR
      INTEGER, INTENT(IN)             :: LIHBOR(NPTFR),LIUBOR(NPTFR)
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: TIDALTYPE,MARDAT(3),MARTIM(3)
      INTEGER, INTENT(INOUT)          :: ICALHW
      DOUBLE PRECISION, INTENT(IN)    :: TEMPS,CTIDE,MSL,CTIDEV,DT
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
      TYPE(BIEF_OBJ), INTENT(IN)      :: BOUNDARY_COLOUR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: NUMTIDE,UBTIDE,VBTIDE,HBTIDE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IERR,I,J,J2,J3
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPTFRL
!
      DOUBLE PRECISION PI,DTR
      DOUBLE PRECISION SUMH,SUMU,SUMV
!
      CHARACTER(LEN=4) TEXT
!
!
!-----------------------------------------------------------------------
!
      PI  = ATAN(1.D0)*4.D0
      DTR = PI/180.D0
!
!  TEST TO CHECK CORRECT VALUES FOR TIDALTYPE
!
      IF(.NOT.DEJA_BTM) THEN
        IF(TIDALTYPE.LT.1.OR.TIDALTYPE.GT.6) THEN
          WRITE(LU,*) 'UNEXPECTED VALUE FOR TIDALTYPE=',TIDALTYPE
          WRITE(LU,*) 'IT MUST BE CHOSEN BETWEEN 1 AND 6'
          WRITE(LU,*) 'CURRENTLY'
          WRITE(LU,*) 'WITH MISC TIDAL DATA BASES'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!  MAGNITUDES AND PHASES ARE READ IN TIDAL FILE
!  TIDAL FILE IS OBTAINED FROM THE TIDAL TOOL BOX (LEGOS)
!
!  NTIDE_BTM:  NUMBER OF THE TIDAL BOUNDARIES
!  NPTFRL_BTM: NUMBERS OF BOUNDARY POINTS WHERE TIDE IS PRESCRIBED
!
      IF(.NOT.DEJA_BTM) THEN
!
        REWIND NFOT
!
        READ(NFOT,*,END=2) NTIDE_BTM
        DO K=1,NTIDE_BTM
          READ(NFOT,*,END=2)
        ENDDO
!
        READ(NFOT,*,END=2) NPTFRL_BTM,NWAVES_BTM,TEXT
!
        IF(NWAVES_BTM.GT.MISC_NCMX_BTM) THEN
          WRITE(LU,*) 'NUMBER OF WAVES IN THE HARMONIC CONSTITUENTS'
          WRITE(LU,*) 'FILE GREATER THAN',MISC_NCMX_BTM
          WRITE(LU,*) 'SOME WAVES ARE NOT EXPECTED. THE FILE IS TO'
          WRITE(LU,*) 'BE ADJUSTED'
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
2     CONTINUE
!
      IF(.NOT.DEJA_BTM) THEN
        ALLOCATE(FIRSTTIDE_BTM(NTIDE_BTM),STAT=IERR)
        ALLOCATE(LASTTIDE_BTM(NTIDE_BTM), STAT=IERR)
        ALLOCATE(SHIFTTIDE_BTM(NTIDE_BTM),STAT=IERR)
!
        ALLOCATE(AH_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(PH_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(AU_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(PU_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(AV_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(PV_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(LON_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(LAT_BTM(NPTFRL_BTM,NWAVES_BTM),STAT=IERR)
        ALLOCATE(NAMEWAVE_BTM(NWAVES_BTM),STAT=IERR)
        ALLOCATE(INDW_BTM(NWAVES_BTM),STAT=IERR)
        IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
          ALLOCATE(PHCALHW_BTM(NWAVES_BTM),STAT=IERR)
        ENDIF
        ALLOCATE(UPV_BTM(MISC_NCMX_BTM),STAT=IERR)
        ALLOCATE(FF_BTM(MISC_NCMX_BTM),STAT=IERR)
        ALLOCATE(OMEGA_BTM(MISC_NCMX_BTM),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'bord_tide_misc')
      ENDIF
!
!  COMPUTE THE FIRST AND LAST INDICES OF THE OPEN LIQUID BOUNDARY WITH TIDE TO PRESCRIBE
!
      IF(.NOT.DEJA_BTM) THEN
        REWIND NFOT
!
        READ(NFOT,*)
        DO I=1,NTIDE_BTM
          READ(NFOT,*,END=4) FIRSTTIDE_BTM(I),LASTTIDE_BTM(I)
        ENDDO
4       CONTINUE
!
!  SHIFTS WHEN CHANGING TIDAL BOUNDARY
!
        SHIFTTIDE_BTM(1) = 0
!
        DO I=2,NTIDE_BTM
          SHIFTTIDE_BTM(I) = LASTTIDE_BTM(I-1) - FIRSTTIDE_BTM(I-1) + 1
     &                 + SHIFTTIDE_BTM(I-1)
        ENDDO
!
        READ(NFOT,*) NPTFRL_BTM,NWAVES_BTM,TEXT
!
!  READING OF TIDAL DATA AT THE FIRST TIME STEP
!
        DO I=1,NWAVES_BTM
          READ(NFOT,*)NAMEWAVE_BTM(I)
          DO IPTFRL = 1,NPTFRL_BTM
            READ(NFOT,*) LON_BTM(IPTFRL,I),LAT_BTM(IPTFRL,I),
     &                   AH_BTM(IPTFRL,I),PH_BTM(IPTFRL,I),
     &                   AU_BTM(IPTFRL,I),PU_BTM(IPTFRL,I),
     &                   AV_BTM(IPTFRL,I),PV_BTM(IPTFRL,I)
          ENDDO
        ENDDO
!
        IF(TIDALTYPE.EQ.1) THEN
          DO K = 1,NWAVES_BTM
            INDW_BTM(K) = 0
            DO I = 1,MISC_NCMX_BTM
              IF(NAMEWAVE_BTM(K).EQ.MISC_CONSTID_BTM(I)) THEN
                INDW_BTM(K) = I
                EXIT
              ENDIF
            ENDDO
            IF(INDW_BTM(K).EQ.0) THEN
              WRITE(LU,*) 'MISC : WARNING:' //
     &         'CONSTITUENT ID ',NAMEWAVE_BTM(K),' IS NOT ALLOWED'
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDDO
        ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
!  BEGINNING: SAME AS FOR TIDALTYPE = 1
!  TO BE SURE THAT EVERY CONSTITUENT IS ALLOWED
          DO K = 1,NWAVES_BTM
            INDW_BTM(K) = 0
            DO I = 1,MISC_NCMX_BTM
              IF(NAMEWAVE_BTM(K).EQ.MISC_CONSTID_BTM(I)) THEN
                INDW_BTM(K) = I
                EXIT
              ENDIF
            ENDDO
            IF(INDW_BTM(K).EQ.0) THEN
              WRITE(LU,*) 'MISC : WARNING:' //
     &         'CONSTITUENT ID ',NAMEWAVE_BTM(K),' IS NOT ALLOWED'
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDDO
!
!  TREATMENTS FOR SCHEMATIC TIDES
!
          I = 0
          WRITE(LU,*) 'AVAILABLE CONSTITUENTS FOR SCHEMATIC TIDES:'
!
          IF(TIDALTYPE.EQ.2.OR.TIDALTYPE.EQ.6) THEN
            DO K = 1,NWAVES_BTM
              IF(NAMEWAVE_BTM(K)(2:2).EQ.'1'
     &       .OR.NAMEWAVE_BTM(K)(3:3).EQ.'1'
     &       .OR.NAMEWAVE_BTM(K)(4:4).EQ.'1'
     &       .OR.NAMEWAVE_BTM(K)(2:2).EQ.'2'
     &       .OR.NAMEWAVE_BTM(K)(3:3).EQ.'2'
     &       .OR.NAMEWAVE_BTM(K)(4:4).EQ.'2'
     &       .OR.NAMEWAVE_BTM(K)(2:2).EQ.'4'
     &       .OR.NAMEWAVE_BTM(K)(3:3).EQ.'4'
     &       .OR.NAMEWAVE_BTM(K)(4:4).EQ.'4') THEN
                I = I + 1
                WRITE(LU,*) 'NAMEWAVE_BTM(',K,') = ',NAMEWAVE_BTM(K)
              ENDIF
            ENDDO
          ELSEIF(TIDALTYPE.EQ.3.OR.TIDALTYPE.EQ.5) THEN
            DO K = 1,NWAVES_BTM
              IF(NAMEWAVE_BTM(K)(1:2).EQ.'M2'
     &       .OR.NAMEWAVE_BTM(K)(1:2).EQ.'S2'
     &       .OR.NAMEWAVE_BTM(K)(1:2).EQ.'M4') THEN
                I = I + 1
                WRITE(LU,*) 'NAMEWAVE_BTM(',K,') = ',NAMEWAVE_BTM(K)
              ENDIF
            ENDDO
          ELSEIF(TIDALTYPE.EQ.4) THEN
            DO K = 1,NWAVES_BTM
              IF(NAMEWAVE_BTM(K)(1:2).EQ.'M2'
     &       .OR.NAMEWAVE_BTM(K)(1:2).EQ.'M4')
     &        THEN
                I = I + 1
                WRITE(LU,*) 'NAMEWAVE_BTM(',K,') = ',NAMEWAVE_BTM(K)
              ENDIF
            ENDDO
          ENDIF
          NWAVES2_BTM = I
          WRITE(LU,*) 'AVAILABLE CONSTITUENTS ' //
     &                'FOR SCHEMATIC TIDES:',NWAVES2_BTM
!  INDW2_BTM: INDICES IN MISC_CONSTID_BTM NUMBER (POSSIBLE CONSTITUENTS)
!  INDW3_BTM: INDICES IN THE HARMONIC CONSTANTS FILE NUMBER
          ALLOCATE(INDW2_BTM(NWAVES2_BTM))
          ALLOCATE(INDW3_BTM(NWAVES2_BTM))
!
          J = 1
!
          IF(TIDALTYPE.EQ.2.OR.TIDALTYPE.EQ.6) THEN
            DO K = 1,NWAVES_BTM
              IF(NAMEWAVE_BTM(K)(2:2).EQ.'1'
     &       .OR.NAMEWAVE_BTM(K)(3:3).EQ.'1'
     &       .OR.NAMEWAVE_BTM(K)(4:4).EQ.'1'
     &       .OR.NAMEWAVE_BTM(K)(2:2).EQ.'2'
     &       .OR.NAMEWAVE_BTM(K)(3:3).EQ.'2'
     &       .OR.NAMEWAVE_BTM(K)(4:4).EQ.'2'
     &       .OR.NAMEWAVE_BTM(K)(2:2).EQ.'4'
     &       .OR.NAMEWAVE_BTM(K)(3:3).EQ.'4'
     &       .OR.NAMEWAVE_BTM(K)(4:4).EQ.'4') THEN
                INDW3_BTM(J) = K
                DO I = 1,MISC_NCMX_BTM
                  IF(NAMEWAVE_BTM(K).EQ.MISC_CONSTID_BTM(I)) THEN
                    INDW2_BTM(J) = I
                    EXIT
                  ENDIF
                ENDDO
                J = J+1
              ENDIF
            ENDDO
          ELSEIF(TIDALTYPE.EQ.3.OR.TIDALTYPE.EQ.5) THEN
            DO K = 1,NWAVES_BTM
              IF(NAMEWAVE_BTM(K)(1:2).EQ.'M2'
     &          .OR.NAMEWAVE_BTM(K)(1:2).EQ.'S2'
     &          .OR.NAMEWAVE_BTM(K)(1:2).EQ.'M4') THEN
                INDW3_BTM(J) = K
                DO I = 1,MISC_NCMX_BTM
                  IF(NAMEWAVE_BTM(K).EQ.MISC_CONSTID_BTM(I)) THEN
                    INDW2_BTM(J) = I
                    EXIT
                  ENDIF
                ENDDO
                J = J+1
              ENDIF
            ENDDO
          ELSEIF(TIDALTYPE.EQ.4) THEN
            DO K = 1,NWAVES_BTM
              IF(NAMEWAVE_BTM(K)(1:2).EQ.'M2'
     &           .OR.NAMEWAVE_BTM(K)(1:2).EQ.'M4')
     &        THEN
                INDW3_BTM(J) = K
                DO I = 1,MISC_NCMX_BTM
                  IF(NAMEWAVE_BTM(K).EQ.MISC_CONSTID_BTM(I)) THEN
                    INDW2_BTM(J) = I
                    EXIT
                  ENDIF
                ENDDO
                J = J+1
              ENDIF
            ENDDO
          ENDIF
!
        ENDIF
!
!       POTENTIAL SPECIFIC TREATMENTS
!
        IF(TIDALTYPE.EQ.1) THEN
!
!         DEGREES TO RADIANS CONVERSIONS
          DO I=1,NWAVES_BTM
            DO IPTFRL = 1,NPTFRL_BTM
              PH_BTM(IPTFRL,I) = PH_BTM(IPTFRL,I)*DTR
              PU_BTM(IPTFRL,I) = PU_BTM(IPTFRL,I)*DTR
              PV_BTM(IPTFRL,I) = PV_BTM(IPTFRL,I)*DTR
            ENDDO
          ENDDO
!
        ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
!
!         ARBITRARY CHOICE
          IF(ICALHW.EQ.0) ICALHW = NPTFRL_BTM/2
!
!  CALIBRATION WITH RESPECT TO HIGH WATER!!!
!  PHASES FOR HEIGHTS ARE READ IN TIDAL FILE
!  EXCEPT M4: 2*PHM2 MOD 360 IS APPLIED
! --------------------------------------------------
!
          DO I=1,NWAVES_BTM
            PHCALHW_BTM(I) = PH_BTM(ICALHW,I)
            DO IPTFRL = 1,NPTFRL_BTM
              PH_BTM(IPTFRL,I) = (PH_BTM(IPTFRL,I) - PHCALHW_BTM(I))*DTR
              PU_BTM(IPTFRL,I) = (PU_BTM(IPTFRL,I) - PHCALHW_BTM(I))*DTR
              PV_BTM(IPTFRL,I) = (PV_BTM(IPTFRL,I) - PHCALHW_BTM(I))*DTR
            ENDDO
          ENDDO
!
        ENDIF
!
!  NUMBER OF THE TIDAL BOUNDARY ASSOCIATED TO EACH POINT OF THE BOUNDARY
!  REMAINS 0 IF POINT IS NOT ON AN OPEN BOUNDARY WITH TIDE
!
        DO K=1,NPTFR
          NUMTIDE%I(K) = 0
          IPTFR=BOUNDARY_COLOUR%I(K)
          DO I=1,NTIDE_BTM
            IF(IPTFR.GE.FIRSTTIDE_BTM(I)
     &         .AND.IPTFR.LE.LASTTIDE_BTM(I)) THEN
              NUMTIDE%I(K) = I
            ENDIF
          ENDDO
        ENDDO
!
!  FOR THE SIMULATION OF REAL TIDES, NODAL FACTOR CORRECTIONS ARE COMPUTED
!  WITH SCHUREMAN FORMULAE
!  SCHUREMAN P. (1971). MANUAL OF HARMONIC ANALYSIS AND PREDICTION OF TIDES
!
        IF(TIDALTYPE.EQ.1) THEN
!
          CALL NODALUPV_SCHUREMAN(UPV_BTM,OMEGA_BTM,MARDAT,MARTIM)
!  TEMPS-DT RATHER THAN TEMPS BECAUSE THE FIRST CALL TO BORD_TIDE_MISC
!  IS AT THE FIRST TIME STEP
          CALL NODALF_SCHUREMAN(FF_BTM,TEMPS-DT,DEJA_BTM,
     &                          MARDAT,MARTIM)
!
!  JUST TO COMPUTE OMEGA_BTM FOR EVERY WAVE FOR SCHEMATIC TIDES ONLY
!  BEWARE!!! 18 IS FOR M2. BE CAREFUL IF AN EXTRA CONSTITUENT IS
!  INTRODUCED BEFORE M2
!
        ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
          CALL NODALUPV_SCHUREMAN(UPV_BTM,OMEGA_BTM,MARDAT,MARTIM)
          DO I = 1,MISC_NCMX_BTM
            IF(    MISC_CONSTID_BTM(I)(2:2).EQ.'2'
     &         .OR.MISC_CONSTID_BTM(I)(3:3).EQ.'2'
     &         .OR.MISC_CONSTID_BTM(I)(4:4).EQ.'2') THEN
              OMEGA_BTM(I) = OMEGA_BTM(18)
            ELSEIF(    MISC_CONSTID_BTM(I)(2:2).EQ.'4'
     &             .OR.MISC_CONSTID_BTM(I)(3:3).EQ.'4'
     &             .OR.MISC_CONSTID_BTM(I)(4:4).EQ.'4') THEN
              OMEGA_BTM(I) = OMEGA_BTM(18)*2.D0
            ELSEIF(    MISC_CONSTID_BTM(I)(2:2).EQ.'1'
     &             .OR.MISC_CONSTID_BTM(I)(3:3).EQ.'1'
     &             .OR.MISC_CONSTID_BTM(I)(4:4).EQ.'1') THEN
              OMEGA_BTM(I) = OMEGA_BTM(18)*0.5D0
            ENDIF
          ENDDO
        ENDIF
!
        DEJA_BTM = .TRUE.
!
      ENDIF
!
      IF(TIDALTYPE.EQ.1.AND.NODALCORR.EQ.0) THEN
        CALL NODALF_SCHUREMAN(FF_BTM,TEMPS,DEJA_BTM,
     &                        MARDAT,MARTIM)
      ENDIF
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO K=1,NPTFR
!
        IPTFR=BOUNDARY_COLOUR%I(K)
!
!     LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
        IF(LIHBOR(K).EQ.KENT) THEN
!         BEGINNING OF PRESCRIBED DEPTHS
          IF(NUMTIDE%I(K).GT.0) THEN
            IPTFRL=IPTFR-FIRSTTIDE_BTM(NUMTIDE%I(K))+1
     &                  +SHIFTTIDE_BTM(NUMTIDE%I(K))
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE (RECOMMENDED METHODOLOGY)
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
            IF(TIDALTYPE.EQ.1) THEN
              SUMH = 0.D0
              DO I=1,NWAVES_BTM
                J = INDW_BTM(I)
                IF(J.NE.0) THEN
                  SUMH = SUMH + FF_BTM(J)*AH_BTM(IPTFRL,I)
     &                         *COS( TEMPS*OMEGA_BTM(J)
     &                               -PH_BTM(IPTFRL,I)+UPV_BTM(J))
                ENDIF
              ENDDO
            ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.4) THEN
              SUMH = 0.D0
              DO I=1,NWAVES2_BTM
                J2 = INDW2_BTM(I)
                J3 = INDW3_BTM(I)
                IF(J2.NE.0.AND.J3.NE.0) THEN
                  SUMH = SUMH + AH_BTM(IPTFRL,J3)
     &                         *COS(TEMPS*OMEGA_BTM(J2)
     &                              -PH_BTM(IPTFRL,J3))
                ENDIF
              ENDDO
            ELSEIF(TIDALTYPE.EQ.5) THEN
              SUMH = 0.D0
              DO I=1,NWAVES2_BTM
                J2 = INDW2_BTM(I)
                J3 = INDW3_BTM(I)
                IF(J2.NE.0.AND.J3.NE.0) THEN
                  IF(MISC_CONSTID_BTM(J2).NE.'S2  ') THEN
                    SUMH = SUMH + AH_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PH_BTM(IPTFRL,J3))
                  ELSE
                    SUMH = SUMH - AH_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PH_BTM(IPTFRL,J3))
                  ENDIF
                ENDIF
              ENDDO
            ELSEIF(TIDALTYPE.EQ.6) THEN
              SUMH = 0.D0
              DO I=1,NWAVES2_BTM
                J2 = INDW2_BTM(I)
                J3 = INDW3_BTM(I)
                IF(J2.NE.0.AND.J3.NE.0) THEN
                  IF(    MISC_CONSTID_BTM(J2).EQ.'M2  '
     &               .OR.MISC_CONSTID_BTM(J2).EQ.'K2  '
     &               .OR.MISC_CONSTID_BTM(J2).EQ.'M4  ') THEN
                    SUMH = SUMH + AH_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PH_BTM(IPTFRL,J3))
                  ELSE
                    SUMH = SUMH - AH_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PH_BTM(IPTFRL,J3))
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
!
            HBTIDE%R(K) = -ZF(NBOR(K)) + CTIDE*SUMH + MSL
          ENDIF
!         ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
        ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
      IF(LIUBOR(K).EQ.KENTU) THEN
!
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
!
!         BEGINNING OF PRESCRIBED VELOCITIES
!
          IF(NUMTIDE%I(K).GT.0) THEN
            IPTFRL=IPTFR-FIRSTTIDE_BTM(NUMTIDE%I(K))+1
     &                  +SHIFTTIDE_BTM(NUMTIDE%I(K))
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE (RECOMMENDED METHODOLOGY)
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
            IF(TIDALTYPE.EQ.1) THEN
              SUMU = 0.D0
              SUMV = 0.D0
              DO I=1,NWAVES_BTM
                J = INDW_BTM(I)
                IF(J.NE.0) THEN
                  SUMU = SUMU + FF_BTM(J)*AU_BTM(IPTFRL,I)
     &                         *COS( TEMPS*OMEGA_BTM(J)
     &                               -PU_BTM(IPTFRL,I)+UPV_BTM(J))
                  SUMV = SUMV + FF_BTM(J)*AV_BTM(IPTFRL,I)
     &                         *COS( TEMPS*OMEGA_BTM(J)
     &                               -PV_BTM(IPTFRL,I)+UPV_BTM(J))
                ENDIF
              ENDDO
            ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.4) THEN
              SUMU = 0.D0
              SUMV = 0.D0
              DO I=1,NWAVES2_BTM
                J2 = INDW2_BTM(I)
                J3 = INDW3_BTM(I)
                IF(J2.NE.0.AND.J3.NE.0) THEN
                  SUMU = SUMU + AU_BTM(IPTFRL,J3)
     &                         *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PU_BTM(IPTFRL,J3))
                  SUMV = SUMV + AV_BTM(IPTFRL,J3)
     &                         *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PV_BTM(IPTFRL,J3))
                ENDIF
              ENDDO
            ELSEIF(TIDALTYPE.EQ.5) THEN
              SUMU = 0.D0
              SUMV = 0.D0
              DO I=1,NWAVES2_BTM
                J2 = INDW2_BTM(I)
                J3 = INDW3_BTM(I)
                IF(J2.NE.0.AND.J3.NE.0) THEN
                  IF(MISC_CONSTID_BTM(J2).NE.'S2  ') THEN
                    SUMU = SUMU + AU_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PU_BTM(IPTFRL,J3))
                    SUMV = SUMV + AV_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PV_BTM(IPTFRL,J3))
                  ELSE
                    SUMU = SUMU - AU_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PU_BTM(IPTFRL,J3))
                    SUMV = SUMV - AV_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PV_BTM(IPTFRL,J3))
                  ENDIF
                ENDIF
              ENDDO
            ELSEIF(TIDALTYPE.EQ.6) THEN
              SUMU = 0.D0
              SUMV = 0.D0
              DO I=1,NWAVES2_BTM
                J2 = INDW2_BTM(I)
                J3 = INDW3_BTM(I)
                IF(J2.NE.0.AND.J3.NE.0) THEN
                  IF(    MISC_CONSTID_BTM(J2).EQ.'M2  '
     &               .OR.MISC_CONSTID_BTM(J2).EQ.'K2  '
     &               .OR.MISC_CONSTID_BTM(J2).EQ.'M4  ') THEN
                    SUMU = SUMU + AU_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PU_BTM(IPTFRL,J3))
                    SUMV = SUMV + AV_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PV_BTM(IPTFRL,J3))
                  ELSE
                    SUMU = SUMU - AU_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PU_BTM(IPTFRL,J3))
                    SUMV = SUMV - AV_BTM(IPTFRL,J3)
     &                           *COS(TEMPS*OMEGA_BTM(J2)
     &                                -PV_BTM(IPTFRL,J3))
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
!
            UBTIDE%R(K) = CTIDEV*SUMU
            VBTIDE%R(K) = CTIDEV*SUMV
          ENDIF
        ENDIF
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
