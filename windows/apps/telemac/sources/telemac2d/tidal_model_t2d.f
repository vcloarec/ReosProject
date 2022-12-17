!                   **************************
                    SUBROUTINE TIDAL_MODEL_T2D
!                   **************************
!
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    FINDS TIDAL BOUNDARY CONDITIONS AT THE OPEN SEA BOUNDARIES
!+
!
!history  C-T PHAM (LNHE)
!+        30/05/2011
!+        V6P1
!+
!
!history  C-T PHAM (LNHE)
!+        02/10/2014
!+        V7P0
!+   BORD_TIDE_LEGOS changed into BORD_TIDE_MISC
!+   (e.g. LEGOS-NEA, FES20XX, Previmer)
!+   Default NODALCORR = 0 (not frozen, computed at each time step)
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
      USE TPXO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NODALCORR,IFRLIQ
      DOUBLE PRECISION XSHIFT,YSHIFT,BETA0
      LOGICAL TIDALBCGEN,TM2S2N2EQUAL
!
!-----------------------------------------------------------------------
!
!     PARAMETERS FOR TIDAL BOUNDARY CONDITIONS
!
!     CTIDEV: COEFFICIENT TO CALIBRATE THE VELOCITIES
!             DEFAULT = SQRT(CTIDE)
!
!     ACCORDING TO DV, IF A CORRECTION COEFFICIENT CTIDE IS APPLIED
!     FOR WATER DEPTHS, ANOTHER ONE MUST BE APPLIED FOR VELOCITIES
!     = SQRT(CTIDE)
!
      IF(CTIDEV.EQ.999999.D0) CTIDEV = SQRT(CTIDE)
!
!     NODALCORR: OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION F
!                IN SUBROUTINES BORD_TIDE AND BORD_TIDE_MISC
!                DEFAULT = 0 (NOT FROZEN, WARNING: CHANGED VALUE, 1 UNTIL V6P3)
!                0: NOT FROZEN, COMPUTED AT EACH TIME STEP
!                1: FROZEN WITH VALUE AT THE BEGINNING OF THE SIMULATION
!                2: FROZEN WITH VALUE AT THE MIDDLE OF THE YEAR IN MARDAT
!                   (SINGLE FORMER POSSIBILITY FOR TIDALTYPE = 7)
!     IN THE STEERING FILE, THE KEYWORDS 'ORIGINAL DATE OF TIME'
!     AND 'ORIGINAL HOUR OF TIME' HAVE TO BE SET
!     WARNING, FORMAT: YEAR, MONTH, DAY
!
      NODALCORR = 0
!
!     TIDALBCGEN: LOGICAL FOR GENERATION OF TIDAL BOUNDARY CONDITIONS OR NOT
!                 CURRENTLY WORKS ONLY FOR SCALAR COMPUTATIONS
!                 FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TIDALBCGEN = .FALSE.
!
!     TM2S2N2EQUAL: LOGICAL TO IMPOSE THE PERIODS OF S2 AND N2 WAVES
!                   TO BE EQUAL TO THE PERIOD OF M2 WAVE
!                   DEFAULT = .TRUE. (WARNING: CHANGED VALUE, .FALSE. IN V6P2)
!                   FOR SCHEMATIC TIDES MODELLING ONLY!
!                   FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TM2S2N2EQUAL = .TRUE.
!
!     OPTIONAL SHIFT OF COORDINATES
!     FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      XSHIFT = 0.D0
      YSHIFT = 0.D0
!
!     BETA: OPTIONAL ANGLE (IN DEGREES) BETWEEN LAMBERT AND MERCATOR-JMJ
!           REFERENCES (EAST OR X AXES, TRIGONOMETRIC)
!           DEFAULT = 0.D0 DEGREES
!           FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      BETA0 = 0.D0
!
!     FILES:
!
!     T2DBDD: TIDE DATA BASE
!     T2DHAR: HARMONIC CONSTANTS FILE
!     T2DTID: TIDAL MODEL FILE
!     T2DL93: CONVERSION GRID FOR LAMBERT 93
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALDB.EQ.1) THEN
        IF(TIDALBCGEN) THEN
          IF(T2D_FILES(T2DBDD)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'TO GENERATE THE HARMONIC CONTANTS FILE'
            WRITE(LU,*) 'FOR JMJ DATA BASE, PLEASE GIVE'
            WRITE(LU,*) 'ASCII DATABASE FOR TIDE FILE.'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(T2D_FILES(T2DTID)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'TO GENERATE THE HARMONIC CONTANTS FILE'
            WRITE(LU,*) 'FOR JMJ DATA BASE, PLEASE GIVE'
            WRITE(LU,*) 'THE TIDAL MODEL FILE'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(T2D_FILES(T2DHAR)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(     GEOSYST.EQ.4.AND.NUMZONE.EQ.93
     &       .AND.T2D_FILES(T2DL93)%NAME(1:1).EQ.' ') THEN
            WRITE(LU,*) 'PLEASE GIVE THE LAMBERT 93 CONVERSION FILE'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL BORD_TIDAL_BC(MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                       NPTFR,KENT,KENTU,
     &                       MESH,GEOSYST,NUMZONE,
     &                       T2D_FILES(T2DL93)%LU,LAMBD0,PHI0,
     &                       TIDALTYPE,BOUNDARY_COLOUR,MAXFRO,
     &                       T2D_FILES(T2DBDD)%LU,
     &                       T2D_FILES(T2DTID)%FMT,T2D_FILES(T2DTID)%LU,
     &                       T2D_FILES(T2DHAR)%LU,XSHIFT,YSHIFT,BETA0,
     &                       I_ORIG,J_ORIG)
        ENDIF
!
        IF(T2D_FILES(T2DHAR)%NAME(1:1).EQ.' ') THEN
          WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                 NPOIN,NPTFR,AT,DT,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 TIDALTYPE,
     &                 CTIDE,MSL,CTIDEV,NODALCORR,T2D_FILES(T2DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHWB,
     &                 MARDAT,MARTIM,TM2S2N2EQUAL)
      ELSEIF(TIDALDB.EQ.2) THEN
        CALL BORD_TIDE_TPXO(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                      NPOIN,NPTFR,AT,
     &                      NUMLIQ%I,KENT,KENTU,
     &                      TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,
     &                      BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,ICALHWG,
     &                      MARDAT,MARTIM,T2D_FILES,T2DBB1,T2DBB2,
     &                      X,Y,GEOSYST,NUMZONE,T2DL93,LAMBD0,PHI0,
     &                      I_ORIG,J_ORIG,INTMICON,HMIN_VIT_BC)
      ELSEIF(TIDALDB.EQ.3) THEN
        IF(T2D_FILES(T2DHAR)%NAME(1:1).EQ.' ') THEN
          WRITE(LU,*) 'PLEASE GIVE THE HARMONIC CONTANTS FILE.'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL BORD_TIDE_MISC(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                      NPOIN,NPTFR,AT,DT,
     &                      NUMLIQ%I,KENT,KENTU,
     &                      TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,NODALCORR,
     &                      T2D_FILES(T2DHAR)%LU,BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHWB,
     &                      MARDAT,MARTIM)
      ELSEIF(TIDALDB.EQ.-1) THEN
        WRITE(LU,*) 'INCORRECT DEFAULT VALUE FOR TIDAL DATA BASE.'
        WRITE(LU,*) 'POSSIBLE CHOICES:'
        WRITE(LU,*) '  1: JMJ,'
        WRITE(LU,*) '  2: TPXO,'
        WRITE(LU,*) '  3: MISC (LEGOS-NEA, FES20XX, PREVIMER...).'
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'TIDAL DATA BASE NOT TAKEN INTO ACCOUNT.'
        WRITE(LU,*) 'POSSIBLE CHOICES:'
        WRITE(LU,*) '  1: JMJ,'
        WRITE(LU,*) '  2: TPXO,'
        WRITE(LU,*) '  3: MISC (LEGOS-NEA, FES20XX, PREVIMER...).'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR
        IFRLIQ=NUMLIQ%I(K)
!       TEST ON NUMTIDE PROBABLY NO LONGER USEFUL
        IF(NUMTIDE%I(K).GT.0.AND.IFRLIQ.GT.0) THEN
          IF(BND_TIDE(IFRLIQ).GT.0) THEN
!           POSSIBLE SMOOTHING AT THE BEGINNING
!           IF(AT.LT.1800.D0) THEN
!             UBTIDE%R(K) = UBTIDE%R(K)*(AT/1800.D0)
!             VBTIDE%R(K) = VBTIDE%R(K)*(AT/1800.D0)
!           ENDIF
            IF(LIUBOR%I(K).EQ.KENTU) THEN
              UBOR%R(K) = UBTIDE%R(K)
              VBOR%R(K) = VBTIDE%R(K)
              U%R(MESH%NBOR%I(K)) = UBOR%R(K)
              V%R(MESH%NBOR%I(K)) = VBOR%R(K)
            ENDIF
            IF(LIHBOR%I(K).EQ.KENT) THEN
              HBOR%R(K) = HBTIDE%R(K)
              H%R(MESH%NBOR%I(K)) = HBOR%R(K)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
