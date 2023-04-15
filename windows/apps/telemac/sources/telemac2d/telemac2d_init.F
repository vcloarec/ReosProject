!                   **************************
                    SUBROUTINE TELEMAC2D_INIT
!                   **************************
!
     &(CODE)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    INITIALIZATION OF THE SUBROUTINE TELEMAC2D.F
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!param code      [in] Name of the code that called this function
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE COUPLE_MOD
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE METEO_TELEMAC !, ONLY: WINDX,WINDY,PATMOS
      USE INTERFACE_TELEMAC2D
      USE INTERFACE_SISYPHE, ONLY: SISYPHE
      USE INTERFACE_GAIA,    ONLY: GAIA_INIT
      USE INTERFACE_TOMAWAC, ONLY: WAC
      USE INTERFACE_KHIONE
!     MODULE SPECIFIC TO COUPLING WITH ESTEL-3D
      USE M_COUPLING_ESTEL3D, ONLY : INFILTRATION_INIT, DEPTH_FILL
      USE INITIAL_DROGUES, ONLY : NDRG_CLSS

!     ALGAE MODEL
      USE ALGAE_TRANSP
!     DELWAQ
      USE TEL4DEL, ONLY: TEL4DELWAQ
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=*), INTENT(IN) :: CODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL CORBOT,FOUND
!
      INTEGER I,ITRAC,IVAR,II,IELMX,NFRLIQ0,IREG,SUM_TNP
      DOUBLE PRECISION MODVIT
!     FRICTION DATA
      INTEGER :: KFROT_TP
      INTEGER :: IERR
!
      CHARACTER(LEN=3), PARAMETER :: CCODE = 'T2D'
!
      INTRINSIC MAX
!
      DOUBLE PRECISION, PARAMETER :: RAD2DEG = 90.0D0/ACOS(0.0D0)
!
!=======================================================================
!
! : 1          READS, PREPARES AND CONTROLS THE DATA
!
!=======================================================================
!
#if defined COMPAD
      CALL AD_TELEMAC2D_INITIALISATION_BEGIN
#endif
!
!=======================================================================
!
!  DROGUES INITIALIZATION
!
      NFLOT=0
!
!  TYPES OF DISCRETISATION: P1 TRIANGLES FOR NOW
!
      IELM=IELM1
!     THE MOST COMPLEX ELEMENT
      IELMX = MAX(IELMH,IELMU,IELMT,IELMK,IELME)
!
!-----------------------------------------------------------------------
!
! READS THE BOUNDARY CONDITIONS AND INDICES OF THE BOUNDARY POINTS
!
      IF(IELMX.EQ.13) THEN
        NPTFR2=2*NPTFR
      ELSE
        NPTFR2=NPTFR
      ENDIF
      IF(REF_T2D.LE.1) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECLIM'
!     UPDATE NPTIR FOR SAFETY
!                                                       WAC2
        IF(INCLUS(COUPLING,'TOMAWAC2')) THEN
          CALL GET_MESH_NPTIR(T2D_FILES(T2DGEO)%FMT,
     &         T2D_FILES(T2DGEO)%LU,NPTIR,IERR)
        ENDIF
        CALL LECLIM(LIHBOR%I   , LIUBOR%I , LIVBOR%I, LITBOR%ADR(1)%P%I,
     &              HBOR%R     , UBOR%R   , VBOR%R  , TBOR%ADR(1)%P%R  ,
     &              CHBORD%R   , ATBOR%ADR(1)%P%R   , BTBOR%ADR(1)%P%R ,
     &              MESH%NPTFR , CCODE     ,NTRAC.GT.0,
     &              T2D_FILES(T2DGEO)%FMT,T2D_FILES(T2DGEO)%LU,
     &              KENT       , KENTU    , KSORT ,  KADH , KLOG , KINC,
     &              NUMLIQ%I   ,MESH,BOUNDARY_COLOUR%I,NPTFR2)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECLIM'
      ENDIF
!
! DUPLICATES THE BOUNDARY CONDITIONS FOR THE TRACERS
!
      IF(NTRAC.GE.2) THEN
        DO ITRAC=2,NTRAC
          DO I=1,NPTFR
            LITBOR%ADR(ITRAC)%P%I(I)=LITBOR%ADR(1)%P%I(I)
            TBOR%ADR(ITRAC)%P%R(I)=  TBOR%ADR(1)%P%R(I)
            ATBOR%ADR(ITRAC)%P%R(I)= ATBOR%ADR(1)%P%R(I)
            BTBOR%ADR(ITRAC)%P%R(I)= BTBOR%ADR(1)%P%R(I)
          ENDDO
        ENDDO
      ENDIF
!
! COEFFICIENTS FOR SECONDARY CURRENTS SECURED TO 0.
!
      IF(SECCURRENTS) THEN
        DO I=1,NPTFR
          TBOR%ADR(NTRAC)%P%R(I)  = 0.D0
          ATBOR%ADR(NTRAC)%P%R(I) = 0.D0
          BTBOR%ADR(NTRAC)%P%R(I) = 0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!  COMPLEMENT OF THE DATA STRUCTURE FOR BIEF
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING INBIEF'
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,LVMAC,IELMX,
     &            LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM INBIEF'
!
      IF(IELMX.EQ.13) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING COMPLIM'
        CALL COMPLIM( LIUBOR%I , LIVBOR%I , LITBOR%ADR(1)%P%I,
     &                UBOR%R   , VBOR%R   , TBOR%ADR(1)%P%R ,
     &                CHBORD%R , ATBOR%ADR(1)%P%R , BTBOR%ADR(1)%P%R ,
     &                MESH%NPTFR , NTRAC.GT.0,
     &                KENT , KENTU , KSORT ,KADH , KLOG ,
     &                IELMU,IELMU,IELMT,
     &                MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM COMPLIM'
      ENDIF
!
!-----------------------------------------------------------------------
!  DEFINITION OF ZONES BY THE USER
!-----------------------------------------------------------------------
!
      IF(DEFZON) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DEF_ZONES'
        CALL DEF_ZONES
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DEF_ZONES'
      ENDIF
!
!-----------------------------------------------------------------------
!  CHANGES FROM GLOBAL TO LOCAL IN LIST OF POINTS IN PARALLEL
!-----------------------------------------------------------------------
!
      IF(NPTS.GT.0.AND.NCSIZE.GT.1) THEN
        DO I=1,NPTS
          LIST_PTS(I)=GLOBAL_TO_LOCAL_POINT(LIST_PTS(I),MESH)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INITIALISES PRIVE
!
      IF(NPRIV.GT.0) CALL OS('X=0     ',X=PRIVE)
!
!-----------------------------------------------------------------------
!  LOOKS FOR VARIABLES BOTTOM AND BOTTOM FRICTION IN THE GEOMETRY FILE:
!-----------------------------------------------------------------------
!
      IF(     .NOT.INCLU2(ESTIME,'FROTTEMENT')
     &   .AND..NOT.INCLU2(ESTIME,'FRICTION'  )
     &   .AND.(REF_T2D.LE.1)) THEN
!       NO PARAMETER ESTIMATION
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FONSTR'
        CALL FONSTR(T1,ZF,T2,CHESTR,T2D_FILES(T2DGEO)%LU,
     &              T2D_FILES(T2DGEO)%FMT,
     &              T2D_FILES(T2DFON)%LU,T2D_FILES(T2DFON)%NAME,
     &              MESH,FFON,LISTIN,
     &              N_NAMES_PRIV,NAMES_PRIVE,PRIVE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FONSTR'
        CORBOT=.TRUE.
      ELSEIF(NITERA.EQ.1.AND..NOT.ADJO
     &   .AND.(REF_T2D.LE.1)) THEN
!       WITH PARAMETER ESTIMATION (HENCE NITERA DEFINED),
!       FONSTR CALLED ONCE TO GET
!       THE BOTTOM TOPOGRAPHY AND THE INITIAL FRICTION (CALL TO STRCHE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FONSTR'
        CALL FONSTR(T1,ZF,T2,CHESTR,T2D_FILES(T2DGEO)%LU,
     &              T2D_FILES(T2DGEO)%FMT,
     &              T2D_FILES(T2DFON)%LU,T2D_FILES(T2DFON)%NAME,
     &              MESH,FFON,LISTIN,
     &              N_NAMES_PRIV,NAMES_PRIVE,PRIVE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FONSTR'
!       IF OPTID=0, VALUES OF SETSTR ARE GIVEN BY FILE, MUST NOT BE ERASED
        IF(OPTID.NE.0) CALL INITSTR(CHESTR,SETSTR,ZONE%I,NZONE,NPOIN,T1)
        CALL ASSIGNSTR(CHESTR,SETSTR,ZONE%I,NZONE,NPOIN)
!       SAVE INITIAL FRICTION COEFF SINCE THEY COULD CHANGE DURING TIME LOOP
        CALL OS('X=Y     ',X=CHESTR0    ,Y=CHESTR)
        CORBOT=.TRUE.
      ELSE
!       IN PARAMETER ESTIMATION, FROM NITERA=2 ON, BOTTOM IS NOT READ
!       AGAIN, SO NO CALL TO CORFON
        CORBOT=.FALSE.
      ENDIF
!
!     INITIALISES FRICTION COEFFICIENT BY ZONE
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FRICTION_CHOICE',0
      CALL FRICTION_CHOICE(0)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FRICTION_CHOICE',0
!
!-----------------------------------------------------------------------
!
!     PREPARES THE RESULTS FILE FOR TELEMAC-2D (OPTIONAL)
!
!      STANDARD SELAFIN
!
      IF(ADJO) THEN
!
        IF(T2D_FILES(T2DRBI)%NAME.NE.' '.AND.
     &     INCLU2(ESTIME,'DEBUG')) THEN
          CALL WRITE_HEADER(T2D_FILES(T2DRBI)%FMT, T2D_FILES(T2DRBI)%LU,
     &                      TITCAS, MAXVAR, TEXTE, SORLEOA)
          CALL WRITE_MESH(T2D_FILES(T2DRBI)%FMT, T2D_FILES(T2DRBI)%LU,
     &                    MESH,1,MARDAT,MARTIM,T1,T2,NCSIZE.GT.1,NPTIR)
        ENDIF
!
      ELSE
!
!       CREATES THE TELEMAC-2D DATA FILE USING A GIVEN FILE FORMAT:
!       T2D_FILES(T2DRES)%FMT
!       THE DATA ARE CREATED IN THE LOGICAL UNIT T2D_FILES(T2DRES)%LU
!       WITH A TITLE AND NAMES OF OUTPUT VARIABLES.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING WRITE_HEADER (TELEMAC-2D)'
        CALL WRITE_HEADER(T2D_FILES(T2DRES)%FMT, ! RESULTS FILE FORMAT
     &                    T2D_FILES(T2DRES)%LU,  ! LU FOR RESULTS FILE
     &                    TITCAS,     ! TITLE
     &                    MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                    TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                    SORLEO)     ! PRINT TO FILE OR NOT
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM WRITE_HEADER (TELEMAC-2D)'
!
!       WRITES THE MESH IN THE OUTPUT FILE :
!       IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
!       THE REST OF THE INFORMATION IS IN MESH.
!       ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
!       ORIGIN.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING WRITE_MESH (TELEMAC-2D)'
        CALL WRITE_MESH(T2D_FILES(T2DRES)%FMT, ! RESULTS FILE FORMAT
     &                  T2D_FILES(T2DRES)%LU,  ! LU FOR RESULTS FILE
     &                  MESH,
     &                  1,             ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  T1,T2,          ! WORKING ARRAYS
     &                  NCSIZE.GT.1, NPTIR,
     &                  NGEO=T2D_FILES(T2DGEO)%LU,
     &                  GEOFORMAT=T2D_FILES(T2DGEO)%FMT,
     &                  LATLONG=PROTYP0.EQ.3.AND.KEEP_LONLAT)!IF LONG-LAT
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM WRITE_MESH (TELEMAC-2D)'
!
!       OUTPUT FOR RESTART
!
        IF(RESTART_MODE.AND.T2D_FILES(T2DRST)%NAME(1:1).NE.' ') THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING WRITE_HEADER EN RESTART'
          CALL WRITE_HEADER(T2D_FILES(T2DRST)%FMT, ! RESULT FILE FORMAT
     &                      T2D_FILES(T2DRST)%LU,  ! RESULT FILE LU
     &                      TITCAS,     ! TITLE
     &                      MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                      SOREST)     ! OUTPUT OR NOT
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM WRITE_HEADER EN RESTART'
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING WRITE_MESH EN RESTART'
          CALL WRITE_MESH(T2D_FILES(T2DRST)%FMT, ! RESULT FILE FORMAT
     &                    T2D_FILES(T2DRST)%LU,  ! RESULT FILE LU
     &                    MESH,
     &                    1,               ! NUMBER OF PLANES /NA/
     &                    MARDAT,          ! START DATE
     &                    MARTIM,          ! START HOUR
     &                    T1,T2,           ! WORKING ARRAYS
     &                    NCSIZE.GT.1, NPTIR,
     &                    NGEO=T2D_FILES(T2DGEO)%LU,
     &                    GEOFORMAT=T2D_FILES(T2DGEO)%FMT,
     &                    LATLONG=PROTYP0.EQ.3.AND.KEEP_LONLAT)!IF LONG-LAT
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM WRITE_MESH EN RESTART'
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PREPARES THE RESULTS FILE FOR KHIONE (OPTIONAL)
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING OUTPUT_KHIONE'
        CALL OUTPUT_KHIONE(
     &    TITCAS,        ! TITLE
     &    NPOIN,         ! LOCAL NUMBER OF POINTS
     &    AT,LT,         ! TIME AND TIME NUMBER
     &    MESH,
     &    VARSOR,        ! POINTERS TO TELEMAC VARIABLES
     &    MARDAT,        ! START DATE
     &    MARTIM,        ! START TIME
     &    .TRUE.,        ! WHETHER TO WRITE THE FILE HEADER
     &    .TRUE.,        ! WHETHER TO WRITE THE FILE MESH
     &    .FALSE.,       ! WHETEHR TO WRITE THE VARIABLES AT AT
     &    T)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM OUTPUT_KHIONE'
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  ENLARGES COSLAT AND SINLAT TO GIVE THEM THE DIMENSION OF U AND V
!  SAME THING FOR FRICTION
!
      IF(IELMU.NE.IELM1) THEN
        IF(SPHERI) CALL CHGDIS(MESH%COSLAT,IELM1,IELMU,MESH)
        IF(SPHERI) CALL CHGDIS(MESH%SINLAT,IELM1,IELMU,MESH)
        CALL CHGDIS(CHESTR,IELM1,IELMU,MESH)
      ENDIF
!
!=======================================================================
!
!  LOCATES THE BOUNDARIES
!
      NFRLIQ0=NFRLIQ
      IF(NCSIZE.GT.1) THEN
        NFRLIQ=0
        DO I=1,NPTFR
          NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
        ENDDO
        NFRLIQ=P_MAX(NFRLIQ)
        WRITE(LU,*) ' '
        WRITE(LU,*) 'NUMBER OF LIQUID BOUNDARIES:',NFRLIQ
        IF(NFRLIQ.GT.MAXFRO) THEN
          WRITE(LU,*) 'INCREASE THE MAXIMUM NUMBER OF BOUNDARIES'
          WRITE(LU,*) 'CURRENTLY AT ',MAXFRO
          WRITE(LU,*) 'TO THE VALUE ',NFRLIQ
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FRONT2'
        CALL FRONT2(NFRLIQ,
     &              LIHBOR%I,LIUBOR%I,
     &              MESH%X%R,MESH%Y%R,MESH%NBOR%I,MESH%KP1BOR%I,
     &              IT1%I,NPOIN,NPTFR,KLOG,LISTIN,NUMLIQ%I,MAXFRO)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FRONT2'
      ENDIF
      IF(NFRLIQ.GT.MAXFRO) THEN
        WRITE(LU,*) 'FRONT2: SIZE OF ARRAYS EXCEEDED'
        WRITE(LU,*) '        INCREASE THE KEYWORD'
        WRITE(LU,*) '        MAXIMUM NUMBER OF BOUNDARIES'
        WRITE(LU,*) '        IN THE CALLING PROGRAM'
        WRITE(LU,*) '        THE CURRENT VALUE IS ',MAXFRO
        WRITE(LU,*) '        THE VALUE SHOULD BE ',NFRLIQ
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(NFRLIQ0.NE.0.AND.NFRLIQ0.NE.NFRLIQ)THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'OPTION FOR TIDAL BOUNDARY CONDITIONS: '
        WRITE(LU,*) 'GIVE THE SAME NUMBER OF VALUES AS THE NUMBER  '
        WRITE(LU,*) 'OF LIQUID BOUNDARIES, I.E.',NFRLIQ
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COMPUTE WALL DISTANCE IF S-A TURBULENCE MODEL IS USED
      IF (ITURB.EQ.6) CALL WALLDIST
     &    (WDIST,T11,T12,T13,T14,T15,FLBOR,TB,AM1,AM2,S,
     &     LIUBOR,IELMNU,NPTFR,MESH)
!
!=======================================================================
!
!  READS THE FILE WITH STAGE-DISCHARGE CURVES
!
      IF(T2D_FILES(T2DMAB)%NAME(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING READ_FIC_CURVES'
        CALL READ_FIC_CURVES(T2D_FILES(T2DMAB)%LU,NFRLIQ,
     &                       STA_DIS_CURVES,PTS_CURVES)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM READ_FIC_CURVES'
      ENDIF
!
!=======================================================================
!
! CORRECTS THE NORMALS TO THE BOUNDARY NODES TO HAVE NORMALS TO
! ADJACENT LIQUID SEGMENT IN THE CASE OF A TRANSITION FROM LIQUID TO SOLID
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CORNOR'
      CALL CORNOR(MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            MESH%XSGBOR%R,MESH%YSGBOR%R,NPTFR,KLOG,LIHBOR%I,
     &            T1,T2,MESH,MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX,
     &            VNX1,VNY1,YESNOFR)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CORNOR'
!
!=======================================================================
!
! FILLS IN MASKEL BY DEFAULT
! (ALL THE ELEMENTS ARE TO BE CONSIDERED)
!
      IF(MSK) CALL OS ( 'X=C     ',X=MASKEL,C=1.D0)
!
!     USER CHOOSES TO HIDE SOME OF THE ELEMENTS
!     THIS SUBROUTINE IS ALSO CALLED AT EVERY TIME STEP
      IF(MSKUSE) THEN
        CALL MASKOB (MASKEL%R,MESH%X%R,MESH%Y%R,
     &               IKLE%I,NELEM,NELMAX,NPOIN,0.D0,0)
      ENDIF
!
!-----------------------------------------------------------------------
!  INTEGRAL OF TEST FUNCTIONS (ONCE FOR ALL AND WITHOUT MASKING)
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING MASBAS2D'
      CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,IELM1,MESH,.FALSE.,
     &              MASKEL,T2,T2)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM MASBAS2D'
!
!=======================================================================
!
! CORRECTS THE BOTTOM WITH USER-SUBROUTINE CORFON
! ZF IS TREATED AS LINEAR IN CORFON
! IF(CORBOT) : SEE CALL FONSTR ABOVE, IN PARAMETER ESTIMATION,
! ZF IS READ ONLY AT THE FIRST RUN
!
      IF(CORBOT) THEN
        IF(IELMH.NE.IELM1) CALL CHGDIS(ZF,IELMH,IELM1,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CORFON'
        CALL CORFON
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CORFON'
        IF(IELMH.NE.IELM1) CALL CHGDIS(ZF,IELM1,IELMH,MESH)
      ENDIF
!
!=======================================================================
!
! IS POSSIBLE TO REDEFINE THE CHARACTERISTICS OF THE SOURCES
!
! STANDARD SUBROUTINE DOES NOT DO ANYTHING
!
      CALL SOURCE_TELEMAC2D
!
!=======================================================================
!
! CAREFULLY ANALYSES TOPOGRAPHY
!
      IF(OPTBAN.EQ.2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING TOPOGR'
        CALL TOPOGR(ZF%R,T1%R,ZFE%R,IKLE%I,MESH%IFABOR%I,
     &              MESH%NBOR%I,MESH%NELBOR%I,MESH%NULONE%I,
     &              IT1%I,IT2%I,IT3%I,
     &              NELEM,NPTFR,NPOIN,MXPTVS)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM TOPOGR'
      ENDIF
!
!=======================================================================
!
! GET OFFSET I_ORIG AND J_ORIG IN THE MESH
!
      CALL GET_MESH_ORIG(T2D_FILES(T2DGEO)%FMT,T2D_FILES(T2DGEO)%LU,
     &                   I_ORIG,J_ORIG,IERR)
!
      IF(I_ORIG.NE.0.OR.J_ORIG.NE.0) THEN
!       IF MODELLING TIDE WITH GEOGRAPHIC SYSTEM DIFFERENT FROM UTM OR
!       LAMBERT PROJECTIONS
        IF(TIDALTYPE.NE.0.AND.
     &     GEOSYST.NE.2.AND.GEOSYST.NE.3.AND.GEOSYST.NE.4) THEN
!         WARNING MESSAGE FOR EVERY TIDAL SOLUTION (I.E. WITH JMJ,
!         POSSIBLE IF HARMONIC CONSTANTS FILE ALREADY GENERATED)
          IF(GEOSYST.EQ.1.OR.(SPHERI.AND..NOT.(PROTYP.EQ.2))) THEN
            WRITE(LU,*) 'WHEN MODELLING TIDE, THE OFFSET _ORIG IS'
            WRITE(LU,*) 'NOT IMPLEMENTED FOR GEOGRAPHIC SYSTEM OTHER'
            WRITE(LU,*) 'THAN UTM OR LAMBERT PROJECTIONS'
!           BUT ERROR MESSAGE IF USING OSU SOLUTIONS LIKE TPXO
            IF(TIDALDB.EQ.2) THEN
              WRITE(LU,*) 'IF USING OSU TIDAL SOLUTIONS LIKE TPXO'
              WRITE(LU,*) 'YOU HAVE TO IMPLEMENT YOUR TRANSFORMATION'
              WRITE(LU,*) 'WITH GEOGRAPHIC SYSTEM = 0 (USER DEFINED)'
              WRITE(LU,*) 'GOOD LUCK!'
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!=======================================================================
!
! : 2                  INITIALISES
!
!=======================================================================
!
! INITIALISES PHYSICAL PARAMETERS
!
!     CONDIN IS CALLED EVEN IN THE EVENT OF A CONTINUATION, SO THAT THE DEFINITION
!     OF C0 DOES NOT CHANGE (CASE OF INCIDENT WAVES)
!
      IF(ADJO) THEN
        CALL CONDIN_ADJ(T2D_FILES(T2DRES)%LU,
     &                  T2D_FILES(T2DRES)%FMT)
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CONDIN'
        CALL CONDIN
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CONDIN'
      ENDIF
!
!     STARTING TIME (AT MAY BE INITIALISED BY USER IN CONDIN)
!
      AT0=AT
!
!     CORRECTS USER ERRORS IF H HAS BEEN USED
!     HERE THE NUMBER OF POINTS IS FORCED TO NPOIN.
      CALL CLIP(H,0.D0,.TRUE.,1.D6,.FALSE.,-NPOIN)
!
! COMPUTES REFERENCE HEIGHT FOR BOUSSINESQ EQUATIONS
!
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        CALL HREF
      ENDIF
!
! SET ALIRE TO 1 WHEN VARIABLE IS RELEVANT OR 0 IF NOT
!
!     FOR READING TRACERS IN SELAFIN FILES
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
!         SEE NOMVAR_TELEMAC2D
          ALIRE(ADR_TRAC+ITRAC) = 1
        ENDDO
      ENDIF
!
!     FOR AVOIDING READING K, EPSILON AND DIFFUSION WHEN NOT RELEVANT
!
      IF(ITURB.NE.3) ALIRE(10) = 0
      IF(ITURB.NE.3) ALIRE(11) = 0
      IF(ITURB.EQ.1) ALIRE(12) = 0
!
!     FOR READING MAXZ,TMXZ,MAXV,TMXV
!
      IF(MAX_PREV) THEN
        IF(SORLEO(27)) THEN
          ALIRE(27) = 1
          IF(SORLEO(28)) ALIRE(28) = 1
        ENDIF
        IF(SORLEO(29)) THEN
          ALIRE(29) = 1
          IF(SORLEO(30)) ALIRE(30) = 1
        ENDIF
      ENDIF
!
!     FOR READING SPALART ALLMARAS VISCOSITY IN SELAFIN FILES
!
      IF(ITURB.EQ.6) ALIRE(40) = 1
!
!     FOR READING 1/R IN SELAFIN FILES
!
      IF(SECCURRENTS) ALIRE(33) = 1
!
!     FOR READING DU,DV,DHN IN SELAFIN FILES
!
      IF(IORDRU.EQ.2) THEN
        ALIRE(37) = 1
        ALIRE(38) = 1
      ENDIF
      IF(IORDRH.EQ.2) ALIRE(39) = 1
!
!     READ THE REFERENCE LEVEL ZRL FOR NESTOR  IN THE GEOMETRY FILE
      IF(NESTOR.AND.DEBU) THEN
        CALL FIND_VARIABLE( T2D_FILES(T2DGEO)%FMT
     &                     ,T2D_FILES(T2DGEO)%LU
     &                     ,'REFERENCE LEVEL '
     &                     ,ZRL%R, NPOIN, IERR    )
      ENDIF
!
!
      IF(.NOT.DEBU.AND..NOT.ADJO) THEN
!
!       BEWARE : READ_DATASET WILL TAKE THE BOTTOM IN THE FILE
!                IF IT IS THERE.
!
!       FRICTION COEFFICIENT ALSO READ IN CASE IT HAS BEEN DONE
!       BY THE USER INTERFACE
        ALIRE(19)=1
!
!       TO READ REFERENCE LEVEL ZRL FOR NESTOR IN THE RESTART FILE
        IF(NESTOR)  ALIRE(35)=1
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING READ_DATASET (TELEMAC-2D)'
        CALL READ_DATASET(T2D_FILES(T2DPRE)%FMT,T2D_FILES(T2DPRE)%LU,
     &                    VARSOR,NPOIN,START_RECORD,AT,TEXTPR,TROUVE,
     &                    ALIRE,LISTIN,START_RECORD.EQ.-1,MAXVAR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM READ_DATASET (TELEMAC-2D)'
        ALIRE(19)=0
        IF(INCLUS(COUPLING,'SISYPHE').AND.TROUVE(6).NE.1) THEN
          WRITE(LU,*) 'COMPUTATION CONTINUED, COUPLING WITH SISYPHE :'
          WRITE(LU,*) 'THE BOTTOM MUST BE IN THE PREVIOUS COMPUTATION'
          WRITE(LU,*) 'FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(INCLUS(COUPLING,'GAIA').AND.TROUVE(6).NE.1) THEN
          WRITE(LU,*) 'COMPUTATION CONTINUED, COUPLING WITH GAIA :'
          WRITE(LU,*) 'THE BOTTOM MUST BE IN THE PREVIOUS COMPUTATION'
          WRITE(LU,*) 'FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       THE ORIGINE OF TIME
!
        IF(RAZTIM) THEN
          AT=0.D0
          WRITE(LU,*) 'ELAPSED TIME RESET TO ZERO'
        ENDIF
        AT0=AT
!
!       RESCUE MISSING VARIABLES
!
        CALL RESCUE(U%R,V%R,H%R,FV%R,ZF%R,T,TRAC0,NTRAC,
     &              ITURB,NPOIN,AKEP,SA,TROUVE,ADR_TRAC)
!
!       DIFFERENTIATED VARIABLES
!
        IF( NADVAR.GT.0 ) THEN
          DO IVAR = 1,NADVAR
            CALL AD_SET_TELEMAC2D(IVAR,ADVAR%ADR(IVAR)%P)
          ENDDO
        ENDIF
!
!       CASE WHERE POSITIVE DEPTHS ARE NECESSARY
        IF(OPTBAN.EQ.1.AND.OPT_HNEG.EQ.2) THEN
          CALL CLIP(H,0.D0,.TRUE.,1.D6,.FALSE.,-NPOIN)
        ENDIF
      ENDIF
!
!     INITIALISES SECONDARY CURRENTS VARIABLES
!
      IF(SECCURRENTS) THEN
        CALL OS('X=0     ',X=SEC_TAU)
        IF(DEBU.OR.TROUVE(33).NE.1) THEN
          CALL OS('X=0     ',X=SEC_R)
        ENDIF
      ENDIF
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CONDICE'
        CALL CONDICE(NPOIN,START_RECORD,AT,LISTIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CONDICE'
      ENDIF
!
!
!-----------------------------------------------------------------------
!
!  INITIALISES PARAMETERS SPECIFIC TO FINITE VOLUMES
!
!-----------------------------------------------------------------------
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
!
        DTINI=DT
        CALL OS('X=YZ    ', X=QU, Y=U, Z=H)
        CALL OS('X=YZ    ', X=QV, Y=V, Z=H)
!
!       PREPARES SIMULATION TIME WHEN DURATION =0
        IF(DUREE.EQ.0.D0) THEN
          IF(DT.GT.0.D0.AND.NIT.GE.1) THEN
            DUREE = NIT*DT
            WRITE(LU,*) 'DESIRED SIMULATION DURATION:',DUREE
          ELSE
            WRITE(LU,*) 'PLEASE GIVE AT LEAST A DURATION'
            WRITE(LU,*) 'OR A NUMBER OF ITERATIONS'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
        TMAX =DUREE+AT0
        DTINI=DT
        EPS_FV=1.D-6
!
!       COMPUTE COORDINATES OF POINTS TO USE FOR FIELD
!       RECONSTRUCTION IN FINITE VOLUME DIFFUSION SOLVER
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FIELD_REC_SEG'
        CALL FIELD_REC_SEG(MESH%X%R,MESH%Y%R,MESH%COORDG%R,
     &                     NPOIN,MESH%ELTSEG%I,NELEM,MESH%NSEG,
     &                     MESH%NUBO%I,MESH%IFABOR%I,MESH%COORDS%R,
     &                     MESH%COORDR%R,ALRTPF%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FIELD_REC_SEG'
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      LT=0
!
!=======================================================================
! EXTENDS THE VARIABLES WHICH ARE NOT LINEAR P1
!=======================================================================
!
      IF(NTRAC.GT.0.AND.IELMT.NE.IELM1) THEN
        DO ITRAC=1,NTRAC
          CALL CHGDIS( T%ADR(ITRAC)%P ,IELM1 , IELMT , MESH )
        ENDDO
      ENDIF
      IF(IELMH.NE.IELM1) THEN
        CALL CHGDIS( H  , IELM1 , IELMH , MESH )
        CALL CHGDIS( ZF , IELM1 , IELMH , MESH )
      ENDIF
      IF(IELMU.NE.IELM1) THEN
        CALL CHGDIS( U , IELM1 , IELMU , MESH )
        CALL CHGDIS( V , IELM1 , IELMU , MESH )
      ENDIF
!
!=======================================================================
! INITIAL CONDITIONS NOT IN CONTINUATION FILE NOR IN CONDIN
!=======================================================================
!
!  CLIPPING (CONDITIONAL) OF H
!
      IF(CLIPH) CALL CLIP( H , HMIN , .TRUE. , 1.D6 , .FALSE. , 0 )
!
!-----------------------------------------------------------------------
! INITIAL WEATHER CONDITIONS
!
      IF(VENT.OR.ATMOS) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING METEO'
        CALL METEO(PATMOS%R,WINDX%R,WINDY%R,
     &             CST_WINDX,CST_WINDY,AT,LT,NPOIN,VENT,ATMOS,
     &             T2ATMA,T2ATMB,
     &             T2D_FILES,LISTIN,CST_PATMOS,
     &             INCLUS(COUPLING,'WAQTEL'),PLUIE,OPTWIND,WIND_SPD)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM METEO'
      ENDIF
!
!-----------------------------------------------------------------------
! INITIAL BREACHES CONDITIONS
!
      IF (BRECHE) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BREACH'
        CALL BREACH
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BREACH'
      ENDIF
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
! READS THE GEOMETRY OF SINGULARITIES
!
      IF(NWEIRS.GT.0) THEN
        IF(TYPSEUIL.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECSNG'
          CALL LECSNG(IOPTAN_T2D,T2D_FILES(T2DSEU)%LU)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECSNG'
        ELSEIF(TYPSEUIL.EQ.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECSNG2'
          CALL LECSNG2(IOPTAN_T2D,T2D_FILES(T2DSEU)%LU)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECSNG2'
        ELSE
          WRITE(LU,*)'TELEMAC-2D_INIT : TYPE OF WEIRS NOT IMPLEMENTED'
        ENDIF
      ENDIF
      IF(NBUSE.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECBUS'
        CALL LECBUS(RELAXB,NBUSE,ENTBUS%I,SORBUS%I,LRGBUS%R,
     &              HAUBUS%R,CLPBUS%I,ALTBUS%R,CSBUS%R,CEBUS%R,
     &              ANGBUS%R,LBUS%R,T2D_FILES(T2DBUS)%LU,MESH,
     &              CV%R,C56%R,CV5%R,C5%R,CTRASH%R,FRICBUS%R,
     &              LONGBUS%R,CIRC%I,DELBUS%R,AABUS%I)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECBUS'
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIAL CONDITIONS FOR K-EPSILON MODEL AND DIFFUSION
!
!   K-EPSILON
!
!     IF AKEP = .FALSE. K AND EPSILON COME FROM SUITE OR CONDIN
      IF(AKEP) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING K-EPS INITIALIZATION'
        CALL FRICTION_CHOICE(1)
        IF(FRICTB) THEN
          KFROT_TP = 0
          IF(KFROT.EQ.NZONES) KFROT_TP = 1 ! NEED A NON ZERO VALUE
        ELSE
          KFROT_TP = KFROT
        ENDIF
!
        CALL AKEPIN(AK%R,EP%R,U%R,V%R,H%R,NPOIN,KFROT_TP,CMU,C2,
     &              ESTAR,SCHMIT,EMIN,CF%R)

        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM K-EPS INITIALIZATION'
!
      ENDIF
!
!     INITIAL CODNITIONS FOR SA MODEL
!
      IF(SA) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING SPALART INITIALIZATION'
        CALL FRICTION_CHOICE(1)
        CALL AKSAIN(VISCSA%R,NPOIN,NUMIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SPALART INITIALIZATION'
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PREPARES BOUNDARY CONDITIONS FOR WEIRS.
!
      IF (NCSIZE.GT.0) THEN
        CALL P_SYNC
      ENDIF
      IF(NWEIRS.GT.0) THEN
!
        IF(TYPSEUIL.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CLSING'
          CALL CLSING(NWEIRS,NPSING,NDGA1,NDGB1,
     &                MESH%X%R,MESH%Y%R,ZF%R,CHESTR%R,NKFROT%I,
     &                KARMAN,ZDIG,PHIDIG,MESH%NBOR%I,
     &                H%R,T,NTRAC,IOPTAN_T2D,T1%R,UBOR%R,VBOR%R,TBOR,
     &                LIHBOR%I,LIUBOR%I,LIVBOR%I,LITBOR,GRAV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CLSING'
        ELSEIF(TYPSEUIL.EQ.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CALCUL_Q_WEIR'
          CALL CALCUL_Q_WEIR
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCUL_Q_WEIR'
        ELSE
          WRITE(LU,*)'TELEMAC-2D_INIT : TYPE OF WEIRS NOT IMPLEMENTED'
        ENDIF
!
      ENDIF
      IF (NCSIZE.GT.0) THEN
        CALL P_SYNC
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TYPES OF CONDITIONS FOR TRACER:
!
      IF(NTRAC.GT.0) THEN
        IF(NWEIRS.GT.0.AND.TYPSEUIL.EQ.1)
     &    CALL CLTRAC(NWEIRS,NPSING,NDGA1,NDGB1,ZF%R,ZDIG,H%R,T,
     &                MESH%NBOR%I,LITBOR,TBOR,NTRAC)
        DO ITRAC=1,NTRAC
          CALL DIFFIN(MASKTR,LIMTRA%I,LITBOR%ADR(ITRAC)%P%I,
     &                IT1%I,U%R,V%R,MESH%XNEBOR%R,MESH%YNEBOR%R,
     &                MESH%NBOR%I,NPTFR,
     &                KENT,KSORT,KLOG,KNEU,KDIR,KDDL,
     &                ICONVFT(ITRAC),
     &                MESH%NELBOR%I,NPOIN,MSK,MASKEL%R,
     &                NFRLIQ,THOMFR,FRTYPE,
     &                TN%ADR(ITRAC)%P,TBOR%ADR(ITRAC)%P,NUMLIQ%I,
     &                MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
        ENDDO
      ENDIF
!
!     TYPES OF CONDITIONS FOR PROPAGATION:
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,
     &             KNEU,KDIR,KDDL,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,NPOIN,
     &             MESH%NELBOR%I,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,NUMLIQ%I,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,MESH%IKLBOR%I,ENTET,
     &             MESH%NELEBX,MESH%NELEB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPIN'
!
!     PROPIN WILL BE CALLED IN THE TIME LOOP AFTER EACH CALL
!     TO BORD
!
!-----------------------------------------------------------------------
!
!     FRICTION COEFFICIENT:
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FRICTION_CHOICE',1
      CALL FRICTION_CHOICE(1)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FRICTION_CHOICE',1
!
!  DIFFUSION OF SPEED (CALLED HERE TO INITIALISE VISC IN CASE
!                           IT IS ONE OF THE OUTPUT VARIABLES)
      IF(ITURB.EQ.1) THEN
!
        CALL OS('X=C     ', X=VISC , C=PROPNU )
!
      ELSEIF(ITURB.EQ.2) THEN
!
        CALL DISPER( VISC , U%R , V%R , H%R , CF%R , ELDER , PROPNU )
!
      ELSEIF(ITURB.EQ.3) THEN
!
        CALL VISTUR(VISC,AK,EP,NPOIN,CMU,PROPNU)
!
      ELSEIF(ITURB.EQ.4) THEN
!
        CALL SMAGOR(VISC,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
!
      ELSEIF(ITURB.EQ.5) THEN
!
        CALL MIXLENGTH(VISC,MESH,T1,T2,T3,T4)
!
      ELSEIF(ITURB.EQ.6) THEN
!
        CALL VISTURSA(VISC, VISCSA, NPOIN, PROPNU)
!
      ELSE
        IF(LISTIN) THEN
          WRITE(LU,16) ITURB
        ENDIF
16      FORMAT(1X,'ITURB=',1I6,'UNKNOWN TURBULENCE MODEL')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!  LAGRANGIAN DRIFT(S)
!
      IF(NLAG.NE.0) CALL LAGRAN(NLAG,DEBLAG%I,FINLAG%I)
!
!-----------------------------------------------------------------------
!  LOCATION OF THE SOURCES
!
      IF(NREJETI.NE.0.OR.NREJET.NE.0.OR.NREJTR.NE.0.OR.NREG.NE.0) THEN
        IF(NREJETI.NE.0) THEN
!       AUTOMATICALLY ASSIGNS COORDINATES OF SOURCES
          DO I=1,NREJETI
            II     = GLOBAL_TO_LOCAL_POINT(ISCE(I),MESH)
            ISCE(I)= II
            IF(II.NE.0)THEN
              XSCE(I)=X(II)
              YSCE(I)=Y(II)
            ENDIF
          ENDDO
        ELSEIF(T2D_FILES(T2DSDN)%NAME(1:1).NE.' ') THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING READ_SOURCE_DATA'
          CALL READ_SOURCE_DATA(T2D_FILES(T2DSDN)%LU,NREG)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM READ_SOURCE_DATA'
          DO IREG=1, NREG
            SUM_TNP = 0
            TNP(IREG) = 0
            AREA_P(IREG) = 0.D0
            DO I=1, NPOIN
              IF(INPOLY(X(I),Y(I),XCOO(1:PTS_REG(IREG),IREG),
     &           YCOO(1:PTS_REG(IREG),IREG),PTS_REG(IREG))) THEN
!               WRITING SOURCE POINTS IN THE LISTING
                IF(NCSIZE.GT.1) THEN
                  II=MESH%KNOLG%I(I)
                ELSE
                  II = I
                ENDIF
                WRITE(LU,26)IREG,II
!
                TNP(IREG)=TNP(IREG)+1
                PT_IN_POLY(IREG,TNP(IREG))=I
                AREA_P(IREG)=AREA_P(IREG)+VOLU2D%R(I)
              ENDIF
            ENDDO
!           ERROR MESSAGES FOR REGIONS OUTSIDE THE DOMAINS
!           OR FOR REGIONS WITHOUT MESH POINTS INSIDE
            IF(NCSIZE.LE.1) THEN
              IF(TNP(IREG).EQ.0) THEN
                WRITE(LU,28)IREG
                CALL PLANTE(1)
                STOP
              ENDIF
            ELSE
              SUM_TNP=P_SUM(TNP(IREG))
              IF(SUM_TNP.EQ.0) THEN
                WRITE(LU,28)IREG
                CALL PLANTE(1)
                STOP
              ENDIF
              AREA_P(IREG)=P_SUM(AREA_P(IREG))
            ENDIF
!
!           COMPUTING FLOWRATE FROM VELOCITIES WHEN IT IS NOT GIVEN BY USER
!
            IF(NREJEU.NE.0.AND.NREJET.EQ.0) THEN
              MODVIT=SQRT(USCE(IREG)**2+VSCE(IREG)**2)
              DSCE(IREG)=MODVIT*AREA_P(IREG)
            ENDIF
          ENDDO
!
        ELSEIF(NREJET.NE.0.AND.NREG.EQ.0) THEN
          CALL PROXIM(ISCE,XSCE,YSCE,
     &                MESH%X%R,MESH%Y%R,
     &                NREJET,NPOIN,
     &                MESH%IKLE%I,NELEM,NELMAX)
        ENDIF
      ENDIF
!
26    FORMAT(1X,'SOURCE REGION:',1I5,' CONTAINS THE NODE:',1I8)
28    FORMAT(/,5X,'THE REGION ',1I5,'IS OUTSIDE THE DOMAIN     ',
     &       /,5X,'OR IT DOES NOT CONTAIN ANY NODE OF THE MESH ')
!
!-----------------------------------------------------------------------
!  INITIALISING THE ALGAE VARIABLES
!
      IF(ALGAE) THEN
!       ALLOCATE THE ALGAE VARIABLES IF NEEDED
        CALL ALLOC_ALGAE(NFLOT_MAX,MESH,DT)
        CALL OS('X=Y     ',X=U_X_AV_0,Y=U_X_AV)
        CALL OS('X=Y     ',X=U_Y_AV_0,Y=U_Y_AV)
        CALL OS('X=Y     ',X=U_Z_AV_0,Y=U_Z_AV)
        CALL OS('X=Y     ',X=K_AV_0  ,Y=K_AV)
        CALL OS('X=Y     ',X=EPS_AV_0,Y=EPS_AV)
        CALL OS('X=Y     ',X=U_X_0   ,Y=U_X)
        CALL OS('X=Y     ',X=U_Y_0   ,Y=U_Y)
        CALL OS('X=Y     ',X=U_Z_0   ,Y=U_Z)
        CALL OS('X=Y     ',X=V_X_0   ,Y=V_X)
        CALL OS('X=Y     ',X=V_Y_0   ,Y=V_Y)
        CALL OS('X=Y     ',X=V_Z_0   ,Y=V_Z)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISING DROGUES (AND SOME OF THE ALGAE VARIABLES)
!
      FOUND = .FALSE.
      IF( NDRG_CLSS.GT.0 ) THEN
!
!       PREVIOUS DROGUES FILE
        IF( T2D_FILES(T2DPLO)%NAME(1:1).NE.' ' ) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING READ_DROGUES'
          CALL READ_DROGUES( NFLOT,NFLOT_MAX, XFLOT%R,YFLOT%R,YFLOT%R,
     &      TAGFLO%I,CLSFLO%I,ELTFLO%I,SHPFLO%R,
     &      NPOIN,NPOIN,NELEM,NELMAX,MESH%IKLE%I,MESH%X%R,MESH%Y%R,
     &      T2D_FILES(T2DPLO) )
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM READ_DROGUES'
          FOUND = .TRUE.
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF( .NOT.FOUND .AND. NDRG_CLSS.GT.0 ) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CONDIN_DROGUES'
        CALL CONDIN_DROGUES
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CONDIN_DROGUES'
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ADDITIONAL USER DEFINED DROGUES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FLOT'
      CALL FLOT(XFLOT%R,YFLOT%R,NFLOT,NFLOT_MAX,MESH%X%R,MESH%Y%R,
     &          MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,MESH%NPOIN,
     &          TAGFLO%I,CLSFLO%I,SHPFLO%R,ELTFLO%I,MESH,LT,NIT,AT)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FLOT'
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_INITIALISATION_END
#endif
!
!=======================================================================
! END OF INITIAL CONDITIONS
!=======================================================================
!
! INITIALISES ADVECTION
! FTILD COMPRISES UTILD,VTILD,HTILD,(TTILD),(AKTILD AND EPTILD),(NUTILD)
!
      CALL OS( 'X=0     ' , X=FTILD )
!
!***********************************************************************
!
! LISTING AND OUTPUT FOR THE INITIAL CONDITIONS.
!
      IF(LISTIN) CALL ENTETE(1,AT,LT)
!
!     NOTE THAT OUTPUTS ARE DONE WITHIN ESTEL3D IN COUPLED MODE)
!
      IF((.NOT.ADJO) .AND. (CODE(1:7).NE.'ESTEL3D') ) THEN
!
! CONTROL SECTIONS (0. IN PLACE OF DT)
!
        IF(NCP.NE.0.AND.(ENTET.OR.CUMFLO)) THEN
          CALL FLUSEC_TELEMAC2D(U,V,H,MESH%IKLE%I,
     &                          MESH%NELMAX,MESH%NELEM,
     &                          MESH%X%R,MESH%Y%R,
     &                          0.D0,NCP,CTRLSC,ENTET,AT,
     &                          MSKSEC,BM1,BM2,H,MESH,S,CV1,
     &                          MESH%IFABOR%I,COMFLU,CUMFLO)
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PRERES_TELEMAC2D'
        CALL PRERES_TELEMAC2D
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PRERES_TELEMAC2D'
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DESIMP FOR TELEMAC-2D'
        CALL BIEF_DESIMP(T2D_FILES(T2DRES)%FMT,VARSOR,
     &                  NPOIN,T2D_FILES(T2DRES)%LU,AT,LT,
     &                  LISPRD,LEOPRD,
     &                  SORLEO,SORIMP,MAXVAR,TEXTE,0,     0,
     &                  IIMP=IMP,ILEO=LEO,COMPGRAPH=COMPLEO)
!                                                  PTINIG,PTINIL
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DESIMP'
!
!     INITIAL CONDITIONS OF 2D OUTPUT FOR RESTART (NOT USED BY NOW MANDATORY)
!
        IF(RESTART_MODE.AND.T2D_FILES(T2DRST)%NAME(1:1).NE.' ') THEN
          IF(DEBU.OR.TROUVE(36).NE.1) THEN
            CALL OS('X=0     ',X=DH)
          ENDIF
          IF(IORDRU.EQ.2) THEN
            IF(DEBU.OR.TROUVE(37).NE.1) THEN
              CALL OS('X=0     ',X=DU)
            ENDIF
            IF(DEBU.OR.TROUVE(38).NE.1) THEN
              CALL OS('X=0     ',X=DV)
            ENDIF
          ENDIF
          IF(IORDRH.EQ.2.AND.(DEBU.OR.TROUVE(39).NE.1)) THEN
            CALL OS('X=0     ',X=DHN)
          ENDIF
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_DESIMP RESTART MODE'
          CALL BIEF_DESIMP(T2D_FILES(T2DRST)%FMT,VARSOR,NPOIN,
     &                     T2D_FILES(T2DRST)%LU,AT,LT,
     &                     1,NIT,SOREST,SORIS2,MAXVAR,TEXTE,0,0)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_DESIMP RESTART'
        ENDIF
!
        IF(INCLUS(COUPLING,'KHIONE')) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING OUTPUT_KHIONE'
          CALL OUTPUT_KHIONE(
     &      TITCAS,        ! TITLE
     &      NPOIN,         ! LOCAL NUMBER OF POINTS
     &      AT,LT,         ! TIME AND TIME NUMBER
     &      MESH,
     &      VARSOR,        ! POINTERS TO TELEMAC VARIABLES
     &      MARDAT,        ! START DATE
     &      MARTIM,        ! START TIME
     &      .FALSE.,       ! WHETHER TO WRITE THE FILE HEADER
     &      .FALSE.,       ! WHETHER TO WRITE THE FILE MESH
     &      .TRUE.,        ! WHETEHR TO WRITE THE VARIABLES AT AT
     &      T)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM OUTPUT_KHIONE'
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!     COUPLING WITH DELWAQ
!
      IF(INCLUS(COUPLING,'DELWAQ')) THEN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING TEL4DELWAQ'
!
!     T3 : MODIFIED DEPTH TO TAKE INTO ACCOUNT MASS-LUMPING
!          IN THE CONTINUITY EQUATION
      IF(ABS(1.D0-AGGLOC).GT.1.D-8) THEN
        CALL VECTOR(T3 ,'=','MASVEC          ',IELMH,
     &              1.D0-AGGLOC,H ,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
        CALL OS('X=XY    ',X=T3 ,Y=UNSV2D)
        CALL OS('X=X+CY  ',X=T3 ,Y=H ,C=AGGLOC)
      ELSE
        CALL OS('X=Y     ',X=T3 ,Y=H )
      ENDIF
!
!     FIRST CALL FOR INITIALISATION, HENCE MESH%W%R IS NOT INITIALISED
!     WITH A CALL TO VECTOR, AS IN THE SECOND CALL
!
      CALL TEL4DELWAQ(MESH%NPOIN,MESH%NPOIN,MESH%NSEG,
     &             MESH%IKLE%I,MESH%ELTSEG%I,
     &             MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &             MESH%X%R,MESH%Y%R,MESH%NPTFR,LIHBOR%I,
     &             MESH%NBOR%I,1,AT,DT,LT,NIT,T3%R,H%R,T3%R,U%R,V%R,
     &             T%ADR(MAX(IND_S,1))%P%R,
     &             T%ADR(MAX(IND_T,1))%P%R,VISC%R,TITCAS,
     &             T2D_FILES(T2DGEO)%NAME,T2D_FILES(T2DCLI)%NAME,WAQPRD,
     &             T2DDL1,T2DDL2,T2DDL3,T2DDL5,T2DDL6,T2DDL7,
     &             T2DL11,T2DDL4,T2DDL8,T2DDL9,T2DL10,
     &             INFOGR,NELEM,SALI_DEL,TEMP_DEL,
     &             VELO_DEL,DIFF_DEL,MARDAT,MARTIM,FLODEL%R,
     &             V2DPAR%R,MESH%KNOLG%I,T2D_FILES)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM TEL4DELWAQ'
!
      ENDIF
!
!=======================================================================
!
!     OPTIONAL USER OUTPUT (COURTESY JACEK JANKOWSKI, BAW)
      CALL UTIMP_TELEMAC2D(LT,AT,PTINIG,LEOPRD,PTINIL,LISPRD)
!
!=======================================================================
!
!     INITIAL DROGUES OUTPUTS
!
      IF( NDRG_CLSS.GT.0 ) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING UTIMP_DROGUES'
        CALL UTIMP_DROGUES( LT,AT,MESH%NPOIN,MESH%NPOIN,
     &              XFLOT%R,YFLOT%R,YFLOT%R,TAGFLO%I,CLSFLO%I,
     &              NFLOT,NFLOT_MAX,FLOPRD,DEJA_DROGUES_UTIMP,
     &              T2D_FILES(T2DFLO),T2D_FILES(T2DBLO), MARDAT,MARTIM )
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM UTIMP_DROGUES'
!
      ENDIF
!
!=======================================================================
!
!  INITIALISES THE ADVECTION AND PROPAGATION FIELDS
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING HPROPA'
      CALL HPROPA(HPROP,H,H,PROLIN,HAULIN,TETAC,NSOUSI)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM HPROPA'
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DE CHPCONV'
      CALL CHPCONV(UCONV,VCONV,U,V,U,V,TETAU)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHPCONV'
      IF(SOLSYS.EQ.2) THEN
        USIS=>UDEL
        VSIS=>VDEL
      ELSE
        USIS=>UCONV
        VSIS=>VCONV
      ENDIF
!
!=======================================================================
!
!     FIRST COMPUTATION OF POROSITY
!
      IF(OPTBAN.EQ.3) THEN
        CALL POROS(TE5,ZF,H,MESH)
        IF(MSK) CALL OS('X=XY    ',X=TE5,Y=MASKEL)
      ENDIF
!
! FIRST COMPUTATIONS FOR BALANCE
!
      IF(BILMAS) THEN
!
        MASSES = 0.D0
        FLUSOR = 0.D0
        FLUENT = 0.D0
        MASS_RAIN=0.D0
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BILAN'
!       INITIALISATION OF FLBOR, PRIOR TO CALLING BILAN, FLBOR
!       IS NORMALLY DONE ON PROPAG.F
        CALL VECTOR(FLBOR,'=','FLUBDF          ',IELBOR(IELMH,1),
     &              1.D0,HPROP,S,S,U,V,S,
     &              MESH,.TRUE.,MASK%ADR(8)%P)
        CALL BILAN(MESH,H,T1,AT,DT,LT,TOTAL_ITER,ENTET,
     &             MASSES,MSK,MASKEL,EQUA,TE5,OPTBAN,
     &             MESH%NPTFR,FLBOR,
     &             FLUX_BOUNDARIES,NUMLIQ%I,NFRLIQ,GAMMA)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BILAN'
!
        IF(NTRAC.GT.0) THEN
!
          IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BILANT'
            DO ITRAC=1,NTRAC
            MASSOU(ITRAC) = 0.D0
            CALL BILANT(H,T2,T3,DT,LT,TOTAL_ITER,LISTIN,
     &                  T%ADR(ITRAC)%P,
     &                  AGGLOT,MASSOU(ITRAC),MASTR0(ITRAC),
     &                  MASTR2(ITRAC),MASTEN(ITRAC),
     &                  MASTOU(ITRAC),MSK,MASKEL,MESH,
     &                  NUMLIQ%I,NFRLIQ,NPTFR,NAMETRAC(ITRAC),
     &                  FLBORTRA,MASS_RAIN,TRAIN(ITRAC),
     &                  MASTRAIN(ITRAC))
            ENDDO
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BILANT'
!
          ELSE
            FLUTSOR = 0.D0
            FLUTENT = 0.D0
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BILANT1'
            DO ITRAC=1,NTRAC
            CALL BILANT1(H,UCONV,VCONV,HPROP,T3,T4,T5,T6,
     &                   LT,TOTAL_ITER,ENTET,MASKTR,
     &                   T%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,TETAT,
     &                   MASSOU(ITRAC),MSK,MASKEL,MESH,
     &                   FLUTSOR(ITRAC),FLUTENT(ITRAC),EQUA,ITRAC)
            ENDDO
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BILANT1'
          ENDIF
!
        ENDIF
!
      ENDIF
!
!=======================================================================
!
      IF(NIT.EQ.0) THEN
        IF(LISTIN) THEN
          WRITE(LU,10)
        ENDIF
10    FORMAT(1X,'STOP IN TELEMAC, NUMBER OF TIME STEP ASKED EQUALS 0')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!=======================================================================
!
!     COUPLING
!
      IF(COUPLING.NE.' ') THEN
        WRITE(LU,*) 'TELEMAC2D COUPLED WITH: ',COUPLING
      ENDIF
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!
        WRITE (LU,*) 'TELEMAC-2D: INTERNAL COUPLING WITH TOMAWAC'
        CALL CONFIG_CODE(3)
        IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE TOMAWAC'

!       SEND DATA TO TOMAWAC
!                                                       WAC2
        IF(INCLUS(COUPLING,'TOMAWAC2')) THEN
          CALL SEND_COUPLE(1,NPOIN,NVARTEL2TOM,TEL2TOM)
        ENDIF

        CALL T2D_WAC_CPL_UPDATE(PART=0)
        CALL WAC(PART=0)
        IF(INCLUS(COUPLING,'TOMAWAC2')) THEN
          CALL GET_MESH_NPTIR(T2D_FILES(T2DGEO)%FMT,
     &       T2D_FILES(T2DGEO)%LU,NPTIR,IERR)

          CALL RECEIVE_COUPLE(2,NPOIN,NVARTOM2TEL,TOM2TEL)
          ! CONVERT DIRECTIONS BACK
          DO I=1,NPOIN
            IF((ABS(SINDIR%R(I)).GE.1.D-14).OR.
     &           (ABS(COSDIR%R(I)).GE.1.D-14)) THEN
              DIRMOY%R(I) = RAD2DEG * ATAN2(SINDIR%R(I),COSDIR%R(I))
            ELSE
!     POINT HAS NO INFORMATION
              DIRMOY%R(I) = 0.D0
            ENDIF
          ENDDO

        ENDIF

        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM TOMAWAC'
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!
!     ==== COUPLING WITH NESTOR ========================================
!
      IF(NESTOR) THEN
!
        IF(INCLUS(COUPLING,'SISYPHE')) THEN
          WRITE (LU,*) 'ERROR:---------------------------------------'
          WRITE (LU,*) 'ERROR:   Must not couple both: '
          WRITE (LU,*) 'ERROR:               Telemac2D with Sisyphe'
          WRITE (LU,*) 'ERROR:       and     Telemac2D with Nestor  !'
          WRITE (LU,*) 'ERROR:    '
          WRITE (LU,*) 'ERROR:   In case you need Sisyphe and Nestor'
          WRITE (LU,*) 'ERROR:   couple Nestor with Sisyphe !'
          WRITE (LU,*) 'ERROR:---------------------------------------'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        CALL NESTOR_T2D_INTERFACE(1)
!
      ENDIF !(NESTOR)
!     ==================================================================
!
!
!
!
!     INITIALISES CONSTANT FLOW DISCHARGE (SEE SISYPHE)
!     ------------------------------------------------------------------
!
      SIS_CPL%SISYPHE_CFD   = .FALSE.
      SIS_CPL%CONSTFLOW = .FALSE.
      SIS_CPL%NSIS_CFD      = 1
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        IF(INCLUS(COUPLING,'FILE-SISYPHE')) THEN
!
          WRITE (LU,*) 'TELEMAC-2D: FILE-COUPLING HAS NOW BEEN'
          WRITE (LU,*) '            SUPPRESSED'
          WRITE (LU,*) '            USE SISYPHE'
          WRITE (LU,*) '            INSTEAD OF FILE-SISYPHE'
          CALL PLANTE(1)
          STOP
!
        ELSEIF(INCLUS(COUPLING,'SISYPHE')) THEN
!
          WRITE (LU,*) 'TELEMAC-2D: INTERNAL COUPLING WITH SISYPHE'
          CALL CONFIG_CODE(2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE SISYPHE'
          CALL SISYPHE(0,LT,LEOPRD,LISPRD,NIT,U,V,H,H,ZF,CF,CF,CHESTR,
     &                 SIS_CPL%CONSTFLOW,SIS_CPL%NSIS_CFD,
     &                 SIS_CPL%SISYPHE_CFD,CODE1,PERCOU,
     &                 U,V,AT,VISC,DT,SIS_CPL%CHARR,SIS_CPL%SUSP,
!                                     CHARR,SUSP : RETURNED BY SISYPHE
!                                                  BUT THEN GIVEN TO IT
     &                 FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV,
     &                 DIRMOY,HM0,TPR5,ORBVEL,.TRUE.,GRCOMP=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SISYPHE'
          CALL CONFIG_CODE(1)
!         AVOIDS TWO OUTPUTS WHEN SISYPHE IS CALLED TWICE
          IF(SIS_CPL%SUSP.AND.SIS_CPL%CHARR.AND.PERCOU.NE.1) THEN
            LEOPRD_CHARR=NIT+PERCOU
          ELSE
            LEOPRD_CHARR=LEOPRD
          ENDIF
!
        ENDIF
!
      ENDIF
!
      IF(INCLUS(COUPLING,'GAIA')) THEN
!
        IF(INCLUS(COUPLING,'FILE-GAIA')) THEN
!
          WRITE (LU,*) 'TELEMAC-2D: FILE-COUPLING HAS NOW BEEN'
          WRITE (LU,*) '            SUPPRESSED'
          WRITE (LU,*) '            USE GAIA'
          WRITE (LU,*) '            INSTEAD OF FILE-GAIA'
          CALL PLANTE(1)
          STOP
!
        ELSEIF(INCLUS(COUPLING,'GAIA')) THEN
!
          WRITE (LU,*) 'TELEMAC-2D: INTERNAL COUPLING WITH GAIA'
          CALL CONFIG_CODE(6)
          GAI_CPL%CHARR = .FALSE.
          GAI_CPL%SUSP = .FALSE.
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIRST CALL TO GAIA_INIT'
          CALL GAIA_INIT(
     &      LEOPRD,LISPRD,NIT,U,V,H,ZF,CF,CF,CF,CHESTR,CODE1,
     &      U,V,AT,DT,GAI_CPL%CHARR,GAI_CPL%SUSP,
     &      DIRMOY,HM0,TPR5,ORBVEL,.TRUE.,
     &      ROEAU,GRAV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM GAIA_INIT'
          CALL CONFIG_CODE(1)
          LEOPRD_CHARR=LEOPRD
!
        ENDIF
!
      ENDIF
!
!=======================================================================
! INITIALISES INFILTRATION STRUCTURES FOR COUPLING WITH ESTEL3D
!
      CALL INFILTRATION_INIT(NPOIN,(CODE(1:7).EQ.'ESTEL3D'))
!
!-----------------------------------------------------------------------
!
!     SAVES THE DEPTH CALCULATED BY TELEMAC2D FOR ESTEL3D
!
      IF(CODE(1:7).EQ.'ESTEL3D') CALL DEPTH_FILL(H%R)
!
!     STORES DT FOR CASE WITH VARIABLE TIME-STEP
!
      DTCAS = DT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
