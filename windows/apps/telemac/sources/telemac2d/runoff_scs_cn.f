!                   ************************
                    SUBROUTINE RUNOFF_SCS_CN
!                   ************************
!
!
     & (PLUIE,ACCFA,ACCIA,ACCROFF,ACCROF_OLD,RAIN_MPS,AMC,CN,ZF,ZFSLOP,
     &  RAIN_HDUR,FILES,FO2,NPOIN,MASKEL,MSK,IELM,MESH,T8,T9,T10,RFM)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    RAINFALL-RUNOFF CALCULATION BASED ON THE SCS METHOD FOR
!+        ABSTRACTIONS (REFERENCE: APPLIED HYDROLOGY,
!+        CHOW, MAIDMENT, MAYS, McGraw-Hill Publishing 1988).
!+        SPATIALLY VARIABLE CURVE NUMBER DEFINED IN FORMATTED DATA FILE
!+        OR ON THE MESH.
!+        EXAMPLES OF RAINFALL DEFINED BY:
!+        - IDF PARAMETERS (CDS-TYPE HYETOGRAPH)
!+        - HYETOGRAPH READ IN FORMATTED DATA FILE
!
!
!history  PIERRE-LOUIS LIGIER (SWECO)
!+        24/07/2016
!+        V7P2
!+        First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACCFA          |<->| ACCUMULATED CONTINUING ABSTRACTION
!| ACCIA          |<->| ACCUMULATED INITIAL ABSTRACTION
!| ACCROFF        |<->| ACCUMULATED RUNOFF AT TIME AT
!| ACCROF_OLD     |<->| ACCUMULATED RUNOFF AT LAST TIME STEP
!| AMC            |-->| ANTECEDENT MOISTURE CONDITIONS FOR SCS CN MODEL
!|                |   | OPTIONS FOR ANTECEDENT MOISTURE CONDITIONS:
!|                |   |   +> 1: DRY ANTECEDENT MOISTURE CONDITIONS
!|                |   |   +> 2: NORMAL ANTECEDENT MOISTURE CONDITIONS
!|                |   |   +> 3: WET ANTECEDENT MOISTURE CONDITIONS
!| CN             |-->| CURVE NUMBER
!| COUPLING       |-->| STRING WITH THE LIST OF COUPLED PROGRAMMES
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FO2            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| IELM           |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS.
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| PLUIE          |-->| BIEF_OBJ STRUCTURE WITH RAIN OR EVAPORATION.
!| RAIN_HDUR      |-->| RAIN OR EVAPORATION DURATION IN HOURS
!| RAIN_MPS       |<->| RAIN OR EVAPORATION IN M PER SECONDS
!| RFM            |<->| TOTAL RAINFALL OVER A TIMESTEP
!| T8             |<->| WORKING ARRAY
!| T9             |<->| WORKING ARRAY
!| T10            |<->| WORKING ARRAY
!| ZF             |-->| BOTTOM ELEVATION
!| ZFSLOP         |<->| BOTTOM SLOPE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: DT,LT,AT,HN,POTMAXRET,IABST,
     &                                  ACCR,IASCNOPT,ENTET,T2DFO1
      USE INTERFACE_TELEMAC2D, EX_RUNOFF_SCS_CN => RUNOFF_SCS_CN
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            , INTENT(IN)    :: NPOIN,AMC,FO2,IELM
      LOGICAL            , INTENT(IN)    :: MSK
      DOUBLE PRECISION   , INTENT(IN)    :: RAIN_MPS,RAIN_HDUR
      DOUBLE PRECISION   , INTENT(INOUT) :: ACCIA(NPOIN),ACCFA(NPOIN)
      DOUBLE PRECISION   , INTENT(INOUT) :: ACCROFF(NPOIN),RFM(NPOIN)
      TYPE(BIEF_OBJ)     , INTENT(IN)    :: ZF,MASKEL
      TYPE(BIEF_OBJ)     , INTENT(INOUT) :: PLUIE,ACCROF_OLD,CN,ZFSLOP
      TYPE(BIEF_OBJ)     , INTENT(INOUT) :: T8,T9,T10
      TYPE(BIEF_FILE)    , INTENT(IN)    :: FILES(*)
      TYPE(BIEF_MESH)    , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,UL
!     TO CHANGE TO KEYWORD, IF NECESSARY
      INTEGER, PARAMETER ::RAINDEF=1
      LOGICAL STEEPSLOPECOR
!
      DOUBLE PRECISION RAIN_MPS_GEO,PEAK_TIME,CC,IA_S
      DOUBLE PRECISION, PARAMETER::EPS=1.E-6
      DOUBLE PRECISION A,B,C,R,RELT,IMMH,RF_HDUR
      DOUBLE PRECISION AT1,AT2,MM_AT2,RFMPOIN
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     INITIALIZATION
      IF(LT.EQ.1)THEN
        CALL OV('X=C     ', X=POTMAXRET%R, C=0.D0, DIM1=NPOIN)
        CALL OV('X=C     ', X=IABST%R, C=0.D0, DIM1=NPOIN)
        CALL OV('X=C     ', X=ACCR%R, C=0.D0, DIM1=NPOIN)
        CALL OV('X=C     ', X=ACCROF_OLD%R, C=0.D0, DIM1=NPOIN)
      ENDIF
      CALL OV('X=C     ', X=ACCIA, C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', X=ACCFA, C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', X=ACCROFF, C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', X=RFM, C=0.D0, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     OPTIONS FOR RAINFALL DEFINITION:
!     -------------------------------
!     +> 1: STANDARD RAINFALL (CONSTANT VALUE IN MM/DAY, KEYWORD)
!     +> 2: RAINFALL DEFINED AS A CDS-TYPE HYETOGRAPH BY IDF PARAMETERS
!     +> 3: RAINFALL DEFINED AS A BLOCK-TYPE HYETOGRAPH READ IN A
!           FORMATTED DATA FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     1. STANDARD RAINFALL (CONSTANT VALUE IN MM/DAY GIVEN BY KEYWORD)
!     ================================================================
      IF(RAINDEF.EQ.1) THEN
!       RAINFALL AT TIME AT OVER ONE TIME-STEP, M
        RFMPOIN=RAIN_MPS * DT
        CALL OV('X=C     ', X=RFM, C=RFMPOIN, DIM1=NPOIN)
!
!     2. EXAMPLE A: CDS-TYPE HYETOGRAPH DEFINED BY IDF PARAMETERS
!     ===========================================================
      ELSEIF(RAINDEF.EQ.2) THEN
!
!       EXAMPLE A: IDF CURVE OF TYPE I = A / (T**B + C), MM/H
!       RAINFALL STARTS AT AT = 0.D0 SECONDS
!       RAINFALL DURATION DEFINED IN HOURS BY RAIN_HDUR (KEYWORD)

!       IDF CONSTANTS, MM/H
        A = 59.9916D0
        B = 0.9737D0
        C = 0.2235D0
!
        IF(LT.EQ.1) THEN
          RF_HDUR = RAIN_HDUR * A / (RAIN_HDUR**B + C)
          WRITE(LU,*) 'RUNOFF_SCS_CN: TOTAL RAINFALL VOLUME ACCORDING'
          WRITE(LU,*) '               TO USER DATA:', RF_HDUR, 'MM'
        ENDIF
!
!       PEAK DECENTERING PARAMETER R, 0. =< R =< 1.
!       FOR SYMMETRICAL RAINFALL (PEAK AT RAIN_HDUR/2: R = 0.5)
        R = 0.5D0
        PEAK_TIME=R*RAIN_HDUR*3600.D0
!
!       TIME RELATIVE TO PEAK, SECONDS
        IF(AT.LT.(PEAK_TIME)) THEN
          RELT = ABS(AT-PEAK_TIME) / MAX(R,EPS)
        ELSE
          RELT = ABS(AT-PEAK_TIME) / MAX((1.D0-R),EPS)
        ENDIF
!
!       RAINFALL INTENSITY AT TIME AT, MM/H
!       EQUATION BELOW VALID ONLY FOR IDF CURVE OF TYPE I = A / (T**B + C)
        IF(AT.LE.(RAIN_HDUR*3600.D0)) THEN
          IMMH = A*((1.D0-B)*(RELT/3600.D0)**B + C ) /
     &           ((RELT/3600.D0)**B + C)**2
        ELSE
!         FORCE RAINFALL = 0 FOR AT>RAIN_HDUR, IDF RELATIONSHIP NO MORE VALID
          IMMH = 0.D0
        ENDIF
!       RAINFALL AT TIME AT OVER ONE TIME-STEP RFM, M
        RFMPOIN=(IMMH / 1000.D0 / 3600.D0) * DT
        CALL OV('X=C     ', X=RFM, C=RFMPOIN, DIM1=NPOIN)
!
!
!     3. EXAMPLE B: BLOCK-TYPE HYETOGRAPH READ IN A FORMATTED DATA FILE
!     =================================================================
      ELSEIF(RAINDEF.EQ.3) THEN
!
!       THE HYETOGRAPH IS DEFINED IN A FORMATTED DATA FILE WITH THE
!       FOLLOWING STRUCTURE:
!
!       #HYETOGRAPH FILE
!       #T (s) RAINFALL (mm)
!       0. 0.
!       3600. 10.
!       7200. 20.
!       etc...
!
!       NOTE THAT THE KEYWORD 'DURATION OF RAIN OR EVAPORATION IN HOURS'
!       IS NOT TAKEN INTO ACCOUNT IN THIS EXAMPLE.
!
!       THE BLOCK-TYPE DEFINITION ASSUMES THAT THE RAINFALL RATE IS
!       CONSTANT BETWEEN THE GIVEN TIME STEPS. FOR THE FILE EXAMPLE
!       ABOVE, THE PROGRAM WILL THEN ASSUME THAT THERE IS A 10 MM
!       RAINFALL BETWEEN T = 0 AND 3600 S, THEN 20 MM BETWEEN 3600 AND
!       7200 S AND SO ON.
!
!       THE HYETOGRAPH IS READ FROM FORMATTED DATA FILE 1 (T2DFO1)
        UL = FILES(T2DFO1)%LU
        REWIND(UL)
!
!       JUMPING TWO LINES OF COMMENTS
        READ(UL,*)
        READ(UL,*)
!       READING THE FIRST TWO LINES OF DATA
        READ(UL,*) AT1 !HERE WE DON'T NEED TO READ THE RAINFALL QUANTITY
        READ(UL,*) AT2,MM_AT2
!
        IF(AT.LT.AT1) THEN
          WRITE(LU,*)' '
          WRITE(LU,*)'RUNOFF_SCS_CN : LATE BEGINNING OF HYETOGRAPH'
          WRITE(LU,*)'                FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
!
10      CONTINUE
        IF(AT.GE.AT1.AND.AT.LE.AT2) THEN
!         RAINFALL AT TIME AT OVER ONE TIME-STEP RFM, M
          RFMPOIN=(MM_AT2 / 1000.D0 / MAX((AT2-AT1),EPS)) * DT
          CALL OV('X=C     ', X=RFM, C=RFMPOIN, DIM1=NPOIN)
        ELSE
          AT1=AT2
          READ(UL,*,ERR=100,END=200) AT2,MM_AT2
          GO TO 10
!
100       CONTINUE
          WRITE(LU,*) ' '
          WRITE(LU,*)'RUNOFF_SCS_CN : ERROR IN THE HYETOGRAPH FILE'
          CALL PLANTE(1)
          STOP
!
200       CONTINUE
          WRITE(LU,*) ' '
          WRITE(LU,*)'RUNOFF_SCS_CN : HYETOGRAPH FILE TOO SHORT'
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!     RAINFALL DEFINITION OPTION NOT IMPLEMENTED
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*)'RUNOFF_SCS_CN : OPTION OF RAIN DEFINITION NOT'
        WRITE(LU,*)'                IMPLEMENTED YET              '
        CALL PLANTE(1)
        STOP
      ENDIF
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     CHECK THAT RAINFALL IS POSITIVE (EVAPORATION NOT SUPPORTED)
      DO I=1,NPOIN
        IF(RFM(I).LT.0.D0) THEN
          WRITE(LU,*) ' '
          WRITE(LU,*)'RUNOFF_SCS_CN : NEGATIVE RAINFALL FOUND'
          WRITE(LU,*)'                AT TIME', AT
          WRITE(LU,*)'                EVAPORATION NOT SUPPORTED'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CN PARAMETERS:
!     =============
!
!     - CN VALUE FOR NORMAL ANTECEDENT MOISTURE CONDITIONS (CN2) GIVEN
!       IN FORMATED FILE 2, HEREAFTER AN INTERPOLATION OF THESE CN
!       ON THE MESH IS ACHIEVED
!
      IF(LT.EQ.1)THEN
        CALL HYDROMAP(CN%R,MESH%X%R,MESH%Y%R,MESH%NPOIN,FILES(FO2)%LU,
     &                MESH%NBOR%I,MESH%KP1BOR%I,MESH%NPTFR)
      ENDIF
!
!       NOTE: CN VALUE FOR NORMAL ANTECEDENT MOISTURE CONDITIONS (CN2)
!       ****  CAN ALSO BE READ FROM A USER VARIABLE STORED IN THE
!             GEOMETRY FILE USING THE FOLLOWING KEYWORDS:
!              +>  NUMBER OF PRIVATE VARIABLES
!              +>  NAMES OF PRIVATE VARIABLES
!             IN THE EXAMPLE BELOW CN IS READ FROM PRIVE%ADR(1)%P%R:
!
!      IF(LT.EQ.1) THEN
!       CALL OV('X=Y     ', X=CN%R, Y=PRIVE%ADR(1)%P%R, DIM1=NPOIN)
!    ENDIF
!
!     CHECK THAT CN IS NOT GREATER THAN 100
      DO I=1,NPOIN
        IF((CN%R(I)-100.D0).GT.1.D-6) THEN
          WRITE(LU,*) ' '
          WRITE(LU,*) 'RUNOFF_SCS_CN : AT LEAST ONE NODE WITH'
          WRITE(LU,*) '                CN VALUE > 100 FOUND IN'
          WRITE(LU,*) '                INPUT DATA. FOR INSTANCE:'
          WRITE(LU,*) '                NODE:',I,'WITH CN=',CN%R(I)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     CHECK THAT CN IS NOT NEGATIVE
        IF(CN%R(I).LT.0.D0) THEN
          WRITE(LU,*) ' '
          WRITE(LU,*) 'RUNOFF_SCS_CN : AT LEAST ONE NODE WITH'
          WRITE(LU,*) '                NEGATIVE CN VALUE FOUND IN'
          WRITE(LU,*) '          INPUT DATA. FOR INSTANCE:'
          WRITE(LU,*) '                NODE:',I,'WITH CN=',CN%R(I)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!     - OPTION FOR STEEP SLOPE CORRECTION:
!       +> REFERENCE: Huang, Gallichand, Wang and Goulet. A modification
!                     to the Soil Conservation Service curve number
!                     method for steep slopes in the Loess Plateau of
!                     China. Hydrological Processes 20, 579-589 (2006).
!       +> CORRECTION FOR SLOPES BETWEEN 0.14 AND 1.4 M/M DEFINED BY A
!          CORRECTION FACTOR CN2A/CN2 (VARIABLE TCN2A_CN2=T10%R)
!       +> TERRAIN SLOPE (M/M) COMPUTED BY SUBROUTINE ZSLOPE
!       +> WARNING: THE STEEP SLOPE CORRECTION IS PERFORMED AT THE
!          *******  BEGINNING OF THE COMPUTATION ONLY - DOES NOT TAKE
!                   INTO ACOUNT TERRAIN EVOLUTIONS IN CASE OF COUPLING
!                   WITH SISYPHE.
!
!     TO ACTIVATE OPTION FOR STEEP SLOPE CORRECTION: STEEPSLOPECOR = .TRUE.
      STEEPSLOPECOR = .FALSE. !CAN BE A KEYWORD?
!
      IF(STEEPSLOPECOR) THEN
!
        IF(LT.EQ.1)THEN
!        IF(LT.EQ.1.OR.INCLUS(COUPLING,'SISYPHE'))THEN
!        PL: CORRECTION IN CASE OF COUPLING REMOVED! IN THAT CASE
!            THE CORRECTION MUST BE DONE ON THE INITIAL CN VALUE,
!            NOT ON THE VALUE AT PREVIOUS DT
!         COMPUTE THE BOTTOM SLOPE
          CALL ZSLOPE(ZFSLOP,ZF,T8,T9,MSK,MASKEL,IELM,MESH)
!         COMPUTE STEEP SLOPE CORRECTION COEFFICIENT CN2A_CN2 (STOCKED IN T10)
          CC=(322.79D0+15.63D0*1.4D0)/(1.4D0+323.52D0)
          DO I=1,NPOIN
            IF(ZFSLOP%R(I).GE.0.14D0.AND.ZFSLOP%R(I).LE.1.4D0) THEN
              T10%R(I) = (322.79D0+15.63D0*ZFSLOP%R(I))
     &                    /(ZFSLOP%R(I)+323.52D0)
            ELSEIF(ZFSLOP%R(I).GT.1.4D0) THEN
!             FOR SLOPES > 1.4, CN2A_CN2 = CC (MAX VALUE FOR SLOPE = 1.4)
              T10%R(I) = CC
            ELSE
!             FOR SLOPES < 0.14, NO CORRECTION (CN2A_CN2 = 1)
              T10%R(I) = 1.D0
            ENDIF
          ENDDO
        ENDIF
      ELSE
!       CN2A_CN2 = 1 IF STEEPSLOPECOR = .FALSE.
        CALL OS('X=C     ',X=T10,C=1.D0)
      ENDIF
!
!     - OPTION FOR INITIAL ABSTRACTION RATIO:
!       +> REFERENCE: Woodward, Hawkins, Jiang, Hjelmfelt, Van Mullem
!                     and Quan. Runoff Curve Number Method: Examination
!                     of the initial abstraction ratio. World Water and
!                     Environmental Resources Congress 2003.
!       +> TWO OPTIONS DEFINED IN KEYWORD 'OPTION FOR INITIAL ABSTRACTION
!          RATIO':
!          - OPTION 1: IA/S = 0.2 (STANDARD METHOD) - DEFAULT
!          - OPTION 2: IA/S = 0.05 (FROM ABOVE REFERENCE) WITH
!                      AUTOMATIC CONVERSION OF CN COEFFICIENTS (INPUT CN
!                      VALUES MUST BE GIVEN ACCORDING TO THE STANDARD
!                      METHOD)
!
      IF(IASCNOPT.EQ.1) THEN
        IA_S = 0.2D0
      ELSEIF(IASCNOPT.EQ.2) THEN
        IA_S = 0.05D0
        IF(LT.EQ.1) THEN
          DO I=1,NPOIN
            CN%R(I) = 100.D0 / (1.879D0*
     &                (100.D0/MAX(CN%R(I),EPS)-1.D0)**1.15D0+1.D0)
          ENDDO
        ENDIF
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*) 'RUNOFF_SCS_CN : INVALID OPTION FOR INITIAL'
        WRITE(LU,*) '                ABSTRACTION RATIO: ', IASCNOPT
        WRITE(LU,*) '                AVAILABLE OPTIONS: 1 OR 2'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!***********************************************************************
!
!     COMPUTE CN DEPENDING ON THE CHOSEN AMC OPTION (GIVEN BY KEYWORD)
!     CN IS FORCED TO 100 MAXIMUM IN CASE OF STEEP SLOPE CORRECTION
!
      IF(LT.EQ.1) THEN
!
      IF(AMC.EQ.1) THEN
        DO I=1,NPOIN
          CN%R(I)=4.2D0*MIN(100.D0,CN%R(I)*T10%R(I))/
     &           (10.D0 - 0.058D0 *
     &           MIN(100.D0,CN%R(I)*T10%R(I)))
        ENDDO
      ELSEIF(AMC.EQ.2) THEN
        DO I=1,NPOIN
          CN%R(I) = MIN(100.D0,CN%R(I)*T10%R(I))
        ENDDO
      ELSEIF(AMC.EQ.3) THEN
        DO I=1,NPOIN
          CN%R(I) = 23.D0 * MIN(100.D0,CN%R(I)*T10%R(I))/
     &             (10.D0 + 0.13D0* MIN(100.D0,CN%R(I)*T10%R(I)))
        ENDDO
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*) 'RUNOFF_SCS_CN : INVALID AMC OPTION: ',AMC
        WRITE(LU,*) '                AVAILABLE OPTIONS: 1, 2 OR 3'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDIF
!
!     POTENTIAL MAXIMAL RETENTION (POTMAXRET), M (STOCKED IN POTMAXRET)
!     INITIAL ABSTRACTION IA, M (STOCKED IN IABST)
!
      CC=25.4D0/1000.D0
!
      DO I=1,NPOIN
!       POTMAXRET(I) = 25.4D0*(1000.D0/CN%R(I)-10.D0)/1000.D0
!       IA(I) = POTMAXRET(I) * IA_S
        POTMAXRET%R(I)=CC*(1000.D0/MAX(CN%R(I),EPS)-10.D0)
        IABST%R(I)=IA_S*POTMAXRET%R(I)
      ENDDO
!
!
!-----------------------------------------------------------------------
!
!     ABSTRACTION CALCULATION
!     =======================
!
!     Description of the abstraction calculation (see reference)
!     ----------------------------------------------------------
!
!     In a first step, the (accumulated) rainfall volume ACCRF is
!     entirely stored in the "initial abstraction" reservoir IA, which
!     is defined by the method as 20% of the total maximal retention
!     POTMAXRET (see above).
!     While ACCRF =< IA, all the rainfall volume is stored in the ground
!     (in ACCIA) and there is no runoff.
!     In a second step, ie. when the accumulated rainfall ACCRF has
!     become larger than the initial abstraction IA, the rainfall volume
!     is divided in two parts:
!       1. A first part stored in the ground in a second abstraction
!          step called continuing abstraction (FA). Its accumulated
!          value, ACCFA, tends towards POTMAXRET when ACCRF tends to
!          infinity.
!       2. The remaining volume is not infiltrated and becomes direct
!          runoff (ACCROFF = ACCRF - ACCIA - ACCFA).
!
!
!     ACCUMULATED RAINFALL AT TIME AT (ACCRF), M (ACCRF STOCKED IN  ACCR)
!     ACCRF = ACCRF + RFM
      CALL OV('X=X+Y   ', X=ACCR%R, Y=RFM, DIM1=NPOIN)
!
!     ACCUMULATED INITIAL ABSTRACTION AT TIME AT (ACCIA), M
!
      DO I=1,NPOIN
        IF(ACCR%R(I).LT.IABST%R(I)) THEN !IF ACCRF<IA
!         ACCIA = ACCRF
          ACCIA(I) = ACCR%R(I)
        ELSE
!         ACCIA = IA
          ACCIA(I) = IABST%R(I)
        ENDIF
      ENDDO
!
!     ACCUMULATED FA AT TIME AT (ACCFA), M
      DO I=1,NPOIN
        IF(ACCR%R(I).GT.IABST%R(I)) THEN !IF ACCRF>IA
!         ACCFA = POTMAXRET * (ACCRF - IA) / (ACCRF - IA + POTMAXRET)
          ACCFA(I)=POTMAXRET%R(I)*(ACCR%R(I)-IABST%R(I))/
     &            (ACCR%R(I)-IABST%R(I)+POTMAXRET%R(I))
        ELSE
          ACCFA(I) = 0.D0
        ENDIF
      ENDDO
!
!     ACCUMULATED RUNOFF AT TIME AT (ACCROFF), M
!     ACCROFF = ACCRF (=ACCR) - ACCIA - ACCFA
      CALL OV('X=Y-Z   ', X=ACCROFF, Y=ACCR%R, Z=ACCIA, DIM1=NPOIN)
      CALL OV('X=X-Y   ', X=ACCROFF, Y=ACCFA, DIM1=NPOIN)
!
!     HYETOGRAPH RAIN_MPS_GEO, M/S
      DO I=1,NPOIN
        RAIN_MPS_GEO = (ACCROFF(I) - ACCROF_OLD%R(I))/DT
        PLUIE%R(I)=MAX(RAIN_MPS_GEO,-MAX(HN%R(I),0.D0)/DT)
      ENDDO
!
!     ACCUMULATED RAINFALL PRINTED TO THE LISTING (INDEPENDENT OF NODE NUMBER)
      IF(ENTET) THEN
        WRITE(LU,*) ' '
        WRITE(LU,50)ACCR%R(1)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     KEEP ACCROFF IN ACCROF_OLD
      CALL OV('X=Y     ', X=ACCROF_OLD%R, Y=ACCROFF, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
50    FORMAT(/,80('-'),/,5X,'RUNOFF_SCS_CN : ACCUMULATED RAINFALL : ',
     &        G16.7,' M'/,80('-'),/)
!
!-----------------------------------------------------------------------
!
!
      RETURN
      END
