!                   ************************
                    SUBROUTINE RUNOFF_HORTON
!                   ************************
!
     & (PLUIE,ACCINF,ACCROFF,ACCROF_OLD,RAIN_MPS,AMC,FC,
     &  F0,RAIN_HDUR,FILES,FO2,NPOIN,MESH,RFM,FCAPA)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    RAINFALL-RUNOFF CALCULATION BASED ON THE HORTON
!+        (REFERENCE: THE FIELD, SCOPE, AND STATUS OF THE SCIENCE
!+        OF HYDROLOGY, HORTON
!+        WILEY ONLINE LIBRARY Publishing 1933).
!+        SPATIALLY VARIABLE INFITLTRATION CAPACITY DEFINED
!+        IN FORMATTED DATA FILE OR ON THE MESH.
!+        EXAMPLES OF RAINFALL DEFINED BY:
!+        - IDF PARAMETERS (CDS-TYPE HYETOGRAPH)
!+        - HYETOGRAPH READ IN FORMATTED DATA FILE
!+        ONE MUST CHANGE RAINDEF TO CHOOSE HOW THE RAINFALL IS DEFINED
!
!
!history  J.-P. TRAVERT (EDF, LNHE)
!+        20/05/2022
!+        V8P4
!+        First version based on runoff_scs_cn.f
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACCINF         |<->| ACCUMULATED INFILTRATION AT TIME AT
!| ACCROFF        |<->| ACCUMULATED RUNOFF AT TIME AT
!| ACCROF_OLD     |<->| ACCUMULATED RUNOFF AT LAST TIME STEP
!| AMC            |-->| ANTECEDENT MOISTURE CONDITIONS FOR HORTON MODEL
!|                |   | OPTIONS FOR ANTECEDENT MOISTURE CONDITIONS:
!|                |   |   +> 1: DRY ANTECEDENT MOISTURE CONDITIONS
!|                |   |   +> 2: NORMAL ANTECEDENT MOISTURE CONDITIONS
!|                |   |   +> 3: WET ANTECEDENT MOISTURE CONDITIONS
!| F0             |<->| MAXIMUM CAPACITY OF INFILTRATION OF THE SOIL
!| FC             |<->| INFILTRATION CAPACITY OF SATURATED SOIL
!| FCAPA          |<->| INFILTRATION CAPACITY OF SOIL AT TIME AT
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FO2            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| MESH           |-->| MESH
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| PLUIE          |-->| BIEF_OBJ STRUCTURE WITH RAIN OR EVAPORATION.
!| RAIN_HDUR      |-->| RAIN OR EVAPORATION DURATION IN HOURS
!| RAIN_MPS       |<->| RAIN OR EVAPORATION IN M PER SECONDS
!| RFM            |<->| TOTAL RAINFALL OVER A TIMESTEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: DT,LT,AT,HN,ACCR,ENTET,
     &                                  T2DFO1,KDECAY
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            , INTENT(IN)    :: NPOIN,AMC,FO2
      DOUBLE PRECISION   , INTENT(IN)    :: RAIN_MPS,RAIN_HDUR
      DOUBLE PRECISION   , INTENT(INOUT) :: ACCROFF(NPOIN),RFM(NPOIN)
      DOUBLE PRECISION   , INTENT(INOUT) :: FCAPA(NPOIN)
      TYPE(BIEF_OBJ)     , INTENT(INOUT) :: PLUIE,ACCROF_OLD,FC,F0
      TYPE(BIEF_OBJ)     , INTENT(INOUT) :: ACCINF
      TYPE(BIEF_FILE)    , INTENT(IN)    :: FILES(*)
      TYPE(BIEF_MESH)    , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,UL
!     TO CHANGE TO KEYWORD, IF NECESSARY
      INTEGER, PARAMETER ::RAINDEF=1
!
      DOUBLE PRECISION RAIN_MPS_GEO,PEAK_TIME
      DOUBLE PRECISION, PARAMETER::EPS=1.E-6
      DOUBLE PRECISION A,B,C,R,RELT,IMMH,RF_HDUR,RFMPOIN
      DOUBLE PRECISION AT1,AT2,MM_AT2
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     INITIALIZATION
      IF(LT.EQ.1)THEN
        CALL OV('X=C     ', X=ACCROF_OLD%R, C=0.D0, DIM1=NPOIN)
        CALL OV('X=C     ', X=ACCR%R, C=0.D0, DIM1=NPOIN)
        CALL OV('X=C     ', X=F0%R, C=0.D0, DIM1=NPOIN)
        CALL OV('X=C     ', X=ACCINF%R, C=0.D0, DIM1=NPOIN)
      ENDIF
      CALL OV('X=C     ', X=FCAPA, C=0.D0, DIM1=NPOIN)
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
          WRITE(LU,*) 'RUNOFF_HORTON: TOTAL RAINFALL VOLUME ACCORDING'
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
!       RAINFALL AT TIME AT OVER ONE TIME-STEP T8, M
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
          WRITE(LU,*)'RUNOFF_HORTON: LATE BEGINNING OF HYETOGRAPH'
          WRITE(LU,*)'                FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
!
10      CONTINUE
        IF(AT.GE.AT1.AND.AT.LE.AT2) THEN
!         RAINFALL AT TIME AT OVER ONE TIME-STEP T8, M
          RFMPOIN=(MM_AT2 / 1000.D0 / MAX((AT2-AT1),EPS)) * DT
          CALL OV('X=C     ', X=RFM, C=RFMPOIN, DIM1=NPOIN)
        ELSE
          AT1=AT2
          READ(UL,*,ERR=100,END=200) AT2,MM_AT2
          GO TO 10
!
100       CONTINUE
          WRITE(LU,*) ' '
          WRITE(LU,*)'RUNOFF_HORTON: ERROR IN THE HYETOGRAPH FILE'
          CALL PLANTE(1)
          STOP
!
200       CONTINUE
          WRITE(LU,*) ' '
          WRITE(LU,*)'RUNOFF_HORTON: HYETOGRAPH FILE TOO SHORT'
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!     RAINFALL DEFINITION OPTION NOT IMPLEMENTED
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*)'RUNOFF_HORTON: OPTION OF RAIN DEFINITION NOT'
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
          WRITE(LU,*)'RUNOFF_HORTON: NEGATIVE RAINFALL FOUND'
          WRITE(LU,*)'                AT TIME', AT
          WRITE(LU,*)'                EVAPORATION NOT SUPPORTED'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INFILTRATION CAPACITY PARAMETERS:
!     =============
!
!     - FC VALUE GIVEN
!       IN FORMATED FILE 2, HEREAFTER AN INTERPOLATION OF THESE FC
!       ON THE MESH IS ACHIEVED
!
      IF(LT.EQ.1)THEN
        CALL HYDROMAP(FC%R,MESH%X%R,MESH%Y%R,MESH%NPOIN,FILES(FO2)%LU,
     &                MESH%NBOR%I,MESH%KP1BOR%I,MESH%NPTFR)
      ENDIF
!
!       NOTE: FC VALUE
!       ****  CAN ALSO BE READ FROM A USER VARIABLE STORED IN THE
!             GEOMETRY FILE USING THE FOLLOWING KEYWORDS:
!              +>  NUMBER OF PRIVATE VARIABLES
!              +>  NAMES OF PRIVATE VARIABLES
!             IN THE EXAMPLE BELOW FC IS READ FROM PRIVE%ADR(1)%P%R:
!
!      IF(LT.EQ.1) THEN
!        CALL OV('X=Y     ', X=FC%R, Y=PRIVE%ADR(1)%P%R, DIM1=NPOIN)
!      ENDIF
!
!     CHECK THAT FC IS NOT NEGATIVE
      DO I=1,NPOIN
        IF(FC%R(I).LT.0.D0) THEN
          WRITE(LU,*) ' '
          WRITE(LU,*) 'RUNOFF_HORTON: AT LEAST ONE NODE WITH'
          WRITE(LU,*) '                NEGATIVE FC VALUE FOUND IN'
          WRITE(LU,*) '          INPUT DATA. FOR INSTANCE:'
          WRITE(LU,*) '                NODE:',I,'WITH FC=',FC%R(I)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!***********************************************************************
!
!     COMPUTE F0 DEPENDING ON THE CHOSEN AMC OPTION (GIVEN BY KEYWORD)
!
      IF(LT.EQ.1) THEN
!
        IF(AMC.EQ.1) THEN
          DO I=1,NPOIN
            F0%R(I)=4.D0*FC%R(I)
          ENDDO
        ELSEIF(AMC.EQ.2) THEN
          DO I=1,NPOIN
            F0%R(I)=2.D0*FC%R(I)
          ENDDO
        ELSEIF(AMC.EQ.3) THEN
          DO I=1,NPOIN
            F0%R(I)=1.D0*FC%R(I)
          ENDDO
        ELSE
          WRITE(LU,*) ' '
          WRITE(LU,*) 'RUNOFF_HORTON: INVALID AMC OPTION: ',AMC
          WRITE(LU,*) '                AVAILABLE OPTIONS: 1, 2 OR 3'
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!
!-----------------------------------------------------------------------
!
!     ABSTRACTION CALCULATION
!     =======================
!
!     ACCUMULATED RAINFALL AT TIME AT (ACCR)
!     ACCR = ACCR + RFM
      CALL OV('X=X+Y   ', X=ACCR%R, Y=RFM, DIM1=NPOIN)
!
!     INFILTRATION CAPACITY OF SOIL AT TIME AT (FCAPA)
      DO I=1,NPOIN
        FCAPA(I)=((FC%R(I)+(F0%R(I)-FC%R(I))*EXP(-KDECAY*AT)))/3600.D3
      ENDDO
!
!     ACCUMULATED INFILTRATION AT TIME AT (ACCINF)
      DO I=1,NPOIN
        IF(FCAPA(I)*DT.GE.RFM(I)) THEN
          ACCINF%R(I)=ACCINF%R(I)+RFM(I)
        ELSE
          ACCINF%R(I)=ACCINF%R(I)+FCAPA(I)*DT
        ENDIF
      ENDDO
!
!     ACCUMULATED RUNOFF AT TIME AT (ACCROFF)
!     ACCROFF = ACCR- ACCINF
      CALL OV('X=Y-Z   ', X=ACCROFF, Y=ACCR%R, Z=ACCINF%R, DIM1=NPOIN)
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
50    FORMAT(/,80('-'),/,5X,'RUNOFF_HORTON: ACCUMULATED RAINFALL : ',
     &        G16.7,' M'/,80('-'),/)
!
!-----------------------------------------------------------------------
!
!
      RETURN
      END
