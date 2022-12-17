!                       ********************
                        MODULE METEO_TELEMAC
!                       ********************
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Module containing all subroutines to deal with atmospheric
!+        exchange, whether its dynamics (wind, pressure, etc.) or its
!+        thermal budget (air temperature, solar radiation, cloud, etc.)
!
!history  S.E. BOURBAN (HRW)
!+        13/06/2017
!+        V7P3
!+        Initial developments for compatibility between all modules
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      PRIVATE
      PUBLIC :: POINT_METEO,SYNC_METEO,DEALLOC_METEO,
     &   METEO_OFFSET,METEO_REF_DATE, 
     &   WINDX,WINDY,WINDZ,WINDS,WINDD,MODELZ, ALPHSD,ALPHRD,
     &   TAIR,TDEW,CLDC,VISBI,RAINFALL,PATMOS,PVAP,RAY3,HREL,
     &   CST_WINDX,CST_WINDY,CST_WINDS,CST_WINDD,CST_PATMOS,
     &   CST_TAIR,CST_TDEW,CST_CLDC,CST_VISBI,CST_RAINFALL,CST_HREL,
     &   CST_RAY3,CST_PVAP
!
!=======================================================================
!
!       1) METEOROLOGICAL DRIVERS
!
!     NOTE: A METEOROLOGICAL VARIBALE (SAY TAIR) IS ASSOCIATED WITH
!     A DOUBLE PRECISION CONSTANT (CST_TAIR, TAKEN FROM THE STEERING
!     FILE, OR THE DICO DEFAULT) AND A LOGICAL, INC_TAIR, WHETHER THE
!     VARIABLE WAS FOUND IN ANY OF THE METEO FILES (ASCII OR BINARY)
!
!-----------------------------------------------------------------------

!     TAIR: AIR TEMPERATURE
!     > Measured in degrees oC
!
      TYPE(BIEF_OBJ), TARGET :: TAIR
      DOUBLE PRECISION       :: CST_TAIR
      LOGICAL                :: INC_TAIR
!
!     TDEW: DEWPOINT TEMPERATURE
!     > Measured in degrees oC
!
!     The dewpoint temperature is the temperature at which the air can
!     no longer "hold" all of the water vapor which is mixed with it,
!     and some of the water vapor must condense into liquid water.
!     The dew point is always lower than (or equal to) the air
!     temperature.
!     If the air temperature cools to the dew point, or if the dew point
!     rises to equal the air temperature, then dew, fog or clouds begin
!     to form. At this point where the dew point temperature equals the
!     air temperature, the relative humidity is 100%.
!
      TYPE(BIEF_OBJ), TARGET :: TDEW
      DOUBLE PRECISION       :: CST_TDEW
      LOGICAL                :: INC_TDEW
!
!     CLDC: CLOUD COVER
!     > Measured in tenth
!
!     Clouds produce significant changes in the evolution of the other
!     meteorological components (the duration of the sun shine, solar
!     radiation, temperature, air humidity, atmospheric precipitation,
!     water  drops  or  ice  crystals  etc.), through  their  size  and
!     form, life duration and their constitution.
!     Cloud cover estimated percentages:
!     -      0%: No clouds
!     -   1-10%: Clear
!     -  11-25%: Isolated
!     -  26-50%: Scattered
!     -  51-90%: Broken
!     -   0>90%: Overcast
!     Cloud cover can also be referred to as nebulosity, or the
!     ambiguous nature of clouds, or cloud-like-ness, and therefore is
!     directly related to the humidity. The more significant the
!     extension and the vertical thickness of the clouds, the higher
!     the value of the nebulosity will be.
!
      TYPE(BIEF_OBJ), TARGET :: CLDC
      DOUBLE PRECISION       :: CST_CLDC
      LOGICAL                :: INC_CLDC
!
!     VISBI: VISIBILITY
!     > Measured in meters
!
!     Visibility is a measure of the distance at which an object or
!     light can be clearly discerned. Note that in the dark,
!     meteorological visibility is still the same as in daylight for
!     the same air.
!
      TYPE(BIEF_OBJ), TARGET :: VISBI
      DOUBLE PRECISION       :: CST_VISBI
      LOGICAL                :: INC_VISBI
!
!     RAINFALL: RAIN
!     > Measured in meters
      TYPE(BIEF_OBJ), TARGET :: RAINFALL
      DOUBLE PRECISION       :: CST_RAINFALL
      LOGICAL                :: INC_RAINFALL
!
!     SNOW: SNOW
!     > Measured in meters
      TYPE(BIEF_OBJ), TARGET :: SNOW
      DOUBLE PRECISION       :: CST_SNOW
      LOGICAL                :: INC_SNOW
!
!     ALPHSD: SUN SET ANGLE
!     > Measured in degrees, 180 degrees for the horizontal
      DOUBLE PRECISION    ALPHSD
!
!     ALPHRD: SUN RISE ANGLE
!     > Measured in degrees, 0 degrees for the horizontal
      DOUBLE PRECISION    ALPHRD
!
!
!     WINDX,WINDY : WIND
!     > Measured in m/s at WINDZ above the water surface
! or  WINDS,WINDD : WIND
!     > Measured in m/s and degree angle clockwise from north
!
      TYPE(BIEF_OBJ), TARGET :: WINDX,WINDY
      DOUBLE PRECISION       :: CST_WINDX,CST_WINDY
      LOGICAL                :: INC_WINDX,INC_WINDY
      TYPE(BIEF_OBJ), TARGET :: WINDS,WINDD
      DOUBLE PRECISION       :: CST_WINDS,CST_WINDD
      LOGICAL                :: INC_WINDS,INC_WINDD
!
!     WINDZ
!     > Heigh above the water surface at which the wind is measured
      DOUBLE PRECISION       :: WINDZ
!
!
      TYPE(BIEF_OBJ), TARGET :: PATMOS
      DOUBLE PRECISION       :: CST_PATMOS
      LOGICAL                :: INC_PATMOS
!
!     PVAP: SATURATED VAPOR PRESSURE
!     > Measured in hPa
      TYPE(BIEF_OBJ), TARGET :: PVAP
      DOUBLE PRECISION       :: CST_PVAP
      LOGICAL                :: INC_PVAP
!
!     RAY3: SOLAR RADIATION
!     > Measured in W/m^2
      TYPE(BIEF_OBJ), TARGET :: RAY3
      DOUBLE PRECISION       :: CST_RAY3
      LOGICAL                :: INC_RAY3
!
!     MODELZ
!     > Elevation of the model domain relative to mean sea levels
      DOUBLE PRECISION       :: MODELZ
!
!     HREL: RELAVITE HUMIDITY
!     > Measured in %
      TYPE(BIEF_OBJ), TARGET :: HREL
      DOUBLE PRECISION       :: CST_HREL
      LOGICAL                :: INC_HREL
!
!
!=======================================================================
!
!       2) METEOROLOGICAL CONSTANTS
!
!-----------------------------------------------------------------------
!
!
!
!=======================================================================
!
!       3) METEOROLOGICAL FILE INPUTS
!
!-----------------------------------------------------------------------
!
!     ONLY TWO METEOROLOGY FILES FOR NOW - A:ASCII AND B:BINARY
!     TO RECORD TEMPORAL AND SPATIAL VARIATIONS
!
!     TYPE BIEF_FILE
!       INTEGER LU               : LOGICAL UNIT TO OPEN THE FILE
!       CHARACTER(LEN=PATH_LEN) NAME  : NAME OF FILE
!       CHARACTER(LEN=6) TELNAME : NAME OF FILE IN TEMPORARY DIRECTORY
!       CHARACTER(LEN=8) FMT     : FORMAT (SERAFIN, MED, ETC.)
!       CHARACTER(LEN=9) ACTION  : READ, WRITE OR READWRITE
!       CHARACTER(LEN=3) BINASC  : ASC FOR ASCII OR BIN FOR BINARY
!       CHARACTER(LEN=12) TYPE   : KIND OF FILE
!     TYPE BIEF_FILE
!
      TYPE(BIEF_FILE)  :: METEO_FILES(2)
!
!     READING THE FILES IN FULL ONLY ONCE
      LOGICAL          :: METEO_DEJA(2)
!
!-----------------------------------------------------------------------
!
!     COMMON TO BOTH ASCII AND BINARY FILES
!
!     MAXIMUM NUMBER OF VALUE ON EACH LINE (VARIABLES FOR EACH POINT)
      INTEGER, PARAMETER :: METEO_MAXVALUE = 210
      CHARACTER(LEN=16), TARGET  :: METEO_CHOIX(2,METEO_MAXVALUE)
      CHARACTER(LEN=16), TARGET  :: METEO_UNITS(2,METEO_MAXVALUE)
!
!-----------------------------------------------------------------------
!
!     SPECIFICS FOR THE ASCII FILE
!
!     INTERPOLATION IN TIME AND IN SPACE FOR MULTIPLE VALUES
      INTEGER          :: NTIMEA,NVALUEA,NPOINA
      INTEGER          :: ITIMEA1,ITIMEA2
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::
     &                    TIMEA, XPOINA,YPOINA
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::
     &                    VALUEA1,VALUEA2
      DOUBLE PRECISION DUMMY
!
!     MAXIMUM NUMBER OF CHARACTERS PER LIGNE (MAY BE CHANGED)
      INTEGER, PARAMETER :: SIZELIGN = 30000
!
!-----------------------------------------------------------------------
!
!     SPECIFICS FOR THE BINARY FILE
!
!     INTERPOLATION IN TIME AND IN SPACE FOR MULTIPLE VALUES
      INTEGER          :: NTIMEB,NVALUEB,NPOINB
      INTEGER          :: ITIMEB1,ITIMEB2
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::
     &                    TIMEB, XPOINB,YPOINB, VALUEB0
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::
     &                    VALUEB1,VALUEB2
!
!
!=======================================================================
!
!       4) WORTH SAVING
!
      INTEGER, DIMENSION(6) :: METEO_REF_DATE
      DOUBLE PRECISION METEO_OFFSET,TEL_OFFSET2
!
!
!-----------------------------------------------------------------------
      SAVE
!
!=======================================================================
!
!       5) METEOROLOGICAL SUBROUTINES
!
      CONTAINS
!
!=======================================================================
!
!                   **********************
                    SUBROUTINE POINT_METEO
!                   **********************
!
     &( FILES,ATMFILEA,ATMFILEB,MESH,IELMT,AVENT,AATMOS,AWATER_QUALITY,
     &  AICE,AFREEATMO)
!
!***********************************************************************
! BIEF   V8P3
!***********************************************************************
!
!brief    Memory allocation of structures, aliases, blocks...
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AATMOS     |-->| YES IF ATMOSPHERIC PRESSURE TAKEN INTO ACCOUNT
!| AFREEATMO  |-->| YES IF FREE FORMAT FOR ATMOSPHERIC FILE
!| AICE       |-->| YES IF ICE IS TAKEN INTO ACCOUNT
!| ATMFILEA   |-->| LOGICAL UNIT OF THE ASCII ATMOSPHERIC FILE
!| ATMFILEB   |-->| LOGICAL UNIT OF THE BINARY ATMOSPHERIC FILE
!| AVENT      |-->| YES IF WIND TAKEN INTO ACCOUNT
!| FILES      |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| IELMT      |-->| NUMBER OF ELEMENTS
!| MESH       |-->| MESH STRUCTURE
!| AWATER_QUALITY |-->| YES IF COUPLED WITH WATER QUALITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      TYPE(BIEF_FILE), INTENT(IN) :: FILES(*)
      INTEGER,         INTENT(IN) :: ATMFILEA,ATMFILEB
      INTEGER,         INTENT(IN) :: IELMT
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
      LOGICAL, INTENT(IN), OPTIONAL :: AVENT,AATMOS,AWATER_QUALITY,AICE,
     &                                 AFREEATMO 
!
      LOGICAL VENT,ATMOS,WATER_QUALITY,ICE,FREEATMO
!
      INTEGER I,J
!
      CHARACTER(LEN=16), ALLOCATABLE  :: CHOIX(:)
!
!-----------------------------------------------------------------------
!
!     DEFAULT VALUES OF PARAMETERS WHEN THEY ARE NOT GIVEN
!
      WATER_QUALITY=.FALSE.
      IF(PRESENT(AWATER_QUALITY)) WATER_QUALITY=AWATER_QUALITY
      VENT=.FALSE.
      IF(PRESENT(AVENT)) VENT=AVENT
      ATMOS=.FALSE.
      IF(PRESENT(AATMOS)) ATMOS=AATMOS
      ICE=.FALSE.
      IF(PRESENT(AICE)) ICE=AICE
      FREEATMO=.FALSE.
      IF(PRESENT(AFREEATMO)) FREEATMO=AFREEATMO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     GATHER THE SKELETON OF THE METEO THERMIC FILES
!
!     A: ASCII FILE
      METEO_DEJA(1) = .FALSE.
      NPOINA = 0
      IF( FILES(ATMFILEA)%NAME(1:1).NE.' ' ) THEN
        IF(.NOT.FREEATMO) CALL INIT_FIC_ASCII( FILES,ATMFILEA )
        METEO_DEJA(1) = .TRUE.
      ENDIF
!
!     B: BINARY FILE
      METEO_DEJA(2) = .FALSE.
      NPOINB = 0
      IF( FILES(ATMFILEB)%NAME(1:1).NE.' ' ) THEN
        IF(.NOT.FREEATMO) CALL INIT_FIC_BINARY( FILES,ATMFILEB )
        METEO_DEJA(2) = .TRUE.
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     ALLOCATE MEMORY
!
!     WORKING ARRAY
!
      IF(ICE) THEN
        CALL BIEF_ALLVEC(1,TDEW  ,'TDEW  ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,VISBI ,'VISBI ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,SNOW  ,'SNOW  ',IELMT,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TDEW  ,'TDEW  ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,VISBI ,'VISBI ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,SNOW  ,'SNOW  ',    0,1,0,MESH)
      ENDIF
!
!     METEOROLOGY
!
      IF(WATER_QUALITY.OR.ICE) THEN
        CALL BIEF_ALLVEC(1,TAIR  ,'TAIR  ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,RAINFALL ,'RAINFA',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,RAY3  ,'RAY3  ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,CLDC  ,'CLDC  ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,PVAP  ,'PVAP  ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,HREL  ,'HREL  ',IELMT,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TAIR  ,'TAIR  ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,RAINFALL ,'RAINFA',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,RAY3  ,'RAY3  ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,CLDC  ,'CLDC  ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,PVAP  ,'PVAP  ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,HREL  ,'HREL  ',    0,1,0,MESH)
      ENDIF
      CALL OS('X=C     ', X=TAIR, C=CST_TAIR )
      INC_TAIR = .FALSE.
      CALL OS('X=C     ', X=RAINFALL, C=CST_RAINFALL )
      INC_RAINFALL = .FALSE.
      CALL OS('X=C     ', X=RAY3, C=CST_RAY3 )
      INC_RAY3 = .FALSE.
      CALL OS('X=C     ', X=CLDC, C=CST_CLDC )
      INC_CLDC = .FALSE.
      CALL OS('X=C     ', X=PVAP, C=CST_PVAP )
      INC_PVAP = .FALSE.
      CALL OS('X=C     ', X=HREL, C=CST_HREL )
      INC_HREL = .FALSE.
!
      CALL OS('X=C     ', X=TDEW, C=CST_TDEW )
      INC_TDEW = .FALSE.
!
      IF(VENT.OR.WATER_QUALITY.OR.ICE) THEN
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,WINDS,'WINDS ',IELMT,1,1,MESH)
        CALL BIEF_ALLVEC(1,WINDD,'WINDD ',IELMT,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,WINDS,'WINDS ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,WINDD,'WINDD ',    0,1,0,MESH)
      ENDIF
      CALL OS('X=C     ', X=WINDX, C=CST_WINDX )
      CALL OS('X=C     ', X=WINDY, C=CST_WINDY )
      INC_WINDX = .FALSE.
      INC_WINDY = .FALSE.
!     ONLY WINDS AND WINDD ARE CONVERTED INOT WINDX AND WINDY
      CALL OS('X=C     ', X=WINDS, C=CST_WINDS )
      CALL OS('X=C     ', X=WINDD, C=CST_WINDD )
      INC_WINDS = .FALSE.
      INC_WINDD = .FALSE.
!
      CALL OS('X=C     ', X=VISBI, C=CST_VISBI )
      INC_VISBI = .FALSE.
!
      CALL OS('X=C     ', X=SNOW, C=CST_SNOW )
      INC_SNOW = .FALSE.
!
      IF(ATMOS.OR.WATER_QUALITY.OR.ICE) THEN
        CALL BIEF_ALLVEC(1,PATMOS,'PATMOS',IELMT,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,PATMOS,'PATMOS',    0,1,0,MESH)
      ENDIF
      CALL OS('X=C     ', X=PATMOS, C=CST_PATMOS )
      INC_PATMOS = .FALSE.
!
!     1: ASCII FILE
!     2: BINARY FILE
!
      ALLOCATE(CHOIX(METEO_MAXVALUE))
      DO I = 1,2
        IF( METEO_DEJA(I) ) THEN
          CHOIX(1:METEO_MAXVALUE) = METEO_CHOIX(I,1:METEO_MAXVALUE)
!
          J = FIND_NAME( 'TAIR', CHOIX, METEO_MAXVALUE )
          INC_TAIR = INC_TAIR .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'TDEW', CHOIX, METEO_MAXVALUE )
          INC_TDEW = INC_TDEW .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'WINDX', CHOIX, METEO_MAXVALUE )
          INC_WINDX = INC_WINDX .OR. ( J.NE.0 )
          J = FIND_NAME( 'WINDY', CHOIX, METEO_MAXVALUE )
          INC_WINDY = INC_WINDY .OR. ( J.NE.0 )
          J = FIND_NAME( 'WINDS', CHOIX, METEO_MAXVALUE )
          INC_WINDS = INC_WINDS .OR. ( J.NE.0 )
          J = FIND_NAME( 'WINDD', CHOIX, METEO_MAXVALUE )
          INC_WINDD = INC_WINDD .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'CLDC', CHOIX, METEO_MAXVALUE )
          INC_CLDC = INC_CLDC .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'VISBI', CHOIX, METEO_MAXVALUE )
          INC_VISBI = INC_VISBI .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'RAINI', CHOIX, METEO_MAXVALUE )
     &      + FIND_NAME( 'RAINC', CHOIX, METEO_MAXVALUE )
          INC_RAINFALL = INC_RAINFALL .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'SNOW', CHOIX, METEO_MAXVALUE )
          INC_SNOW = INC_SNOW .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'PATM', CHOIX, METEO_MAXVALUE )
          INC_PATMOS = INC_PATMOS .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'PVAP', CHOIX, METEO_MAXVALUE )
          INC_PVAP = INC_PVAP .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'HREL', CHOIX, METEO_MAXVALUE )
          INC_HREL = INC_HREL .OR. ( J.NE.0 )
!
          J = FIND_NAME( 'RAY3', CHOIX, METEO_MAXVALUE )
          INC_RAY3 = INC_RAY3 .OR. ( J.NE.0 )
!
        ENDIF
      ENDDO
      DEALLOCATE(CHOIX)

!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   ************************
                    SUBROUTINE DEALLOC_METEO
!                   ************************
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Memory de-allocation of structures, aliases, blocks...
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DE-ALLOCATE FILE RELATED VARIABLES (SEE INIT_FIC_*)
!
!     A: ASCII FILE
      IF( ALLOCATED(TIMEA) )   DEALLOCATE(TIMEA)
      IF( ALLOCATED(XPOINA) )  DEALLOCATE(XPOINA)
      IF( ALLOCATED(YPOINA) )  DEALLOCATE(YPOINA)
      IF( ALLOCATED(VALUEA1) ) DEALLOCATE(VALUEA1)
      IF( ALLOCATED(VALUEA2) ) DEALLOCATE(VALUEA2)
!
!     B: BINARY FILE
      IF( ALLOCATED(TIMEB) )   DEALLOCATE(TIMEB)
      IF( ALLOCATED(XPOINB) )  DEALLOCATE(XPOINB)
      IF( ALLOCATED(YPOINB) )  DEALLOCATE(YPOINB)
      IF( ALLOCATED(VALUEB0) ) DEALLOCATE(VALUEB0)
      IF( ALLOCATED(VALUEB1) ) DEALLOCATE(VALUEB1)
      IF( ALLOCATED(VALUEB2) ) DEALLOCATE(VALUEB2)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DE-ALLOCATE METEOROLOGICAL OBJECTS (SEE POINT_METEO)
!
      CALL BIEF_DEALLOBJ(TAIR)
      CALL BIEF_DEALLOBJ(TDEW)
      CALL BIEF_DEALLOBJ(WINDX)
      CALL BIEF_DEALLOBJ(WINDY)
      CALL BIEF_DEALLOBJ(WINDS)
      CALL BIEF_DEALLOBJ(WINDD)
      CALL BIEF_DEALLOBJ(CLDC)
      CALL BIEF_DEALLOBJ(VISBI)
      CALL BIEF_DEALLOBJ(RAINFALL)
      CALL BIEF_DEALLOBJ(SNOW)
      CALL BIEF_DEALLOBJ(PVAP)
      CALL BIEF_DEALLOBJ(RAY3)
      CALL BIEF_DEALLOBJ(PATMOS)
      CALL BIEF_DEALLOBJ(HREL)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   *********************
                    SUBROUTINE SYNC_METEO
!                   *********************
!
     &( WHEN )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Synchronise the ASCII and the BINARY file for spatial and
!         temporal interpolation
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN) :: WHEN
!
      INTEGER          :: IPOIN
      DOUBLE PRECISION :: DTR
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SYNCHRONISE BOTH THE ASCII AND THE BINARY FILES
!
!     A: ASCII FILE
      IF( METEO_DEJA(1) ) CALL SYNC_FIC_ASCII( WHEN )
!
!     B: BINARY FILE
      IF( METEO_DEJA(2) ) CALL SYNC_FIC_BINARY( WHEN )
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DTR = ATAN(1.D0) / 45.D0
!
!     UPDATE METEOROLOGICAL VARIABLES
!
!     TAIR: AIR TEMPERATURE
      CALL OS('X=C     ', X=TAIR, C=CST_TAIR )
      IF( INC_TAIR ) THEN
        CALL INTERP_METEO(WHEN,'TAIR',TAIR%R,TAIR%DIM1)
      ENDIF
!
!     TDEW: DEWPOINT TEMPERATURE
      CALL OS('X=C     ', X=TDEW, C=CST_TDEW )
      IF( INC_TDEW ) THEN
        CALL INTERP_METEO(WHEN,'TDEW',TDEW%R,TDEW%DIM1)
      ENDIF
!
!     CLDC: CLOUD COVER
      CALL OS('X=C     ', X=CLDC, C=CST_CLDC )
      IF( INC_CLDC ) THEN
        CALL INTERP_METEO(WHEN,'CLDC',CLDC%R,CLDC%DIM1)
      ENDIF
!
!     VISBI: VISIBILITY
      CALL OS('X=C     ', X=VISBI, C=CST_VISBI )
      IF( INC_VISBI ) THEN
        CALL INTERP_METEO(WHEN,'VISBI',VISBI%R,VISBI%DIM1)
      ENDIF
!
!     RAINFALL: RAIN
      CALL OS('X=C     ', X=RAINFALL, C=CST_RAINFALL )
      IF( INC_RAINFALL ) THEN
        CALL INTERP_METEO(WHEN,'RAINI',RAINFALL%R,RAINFALL%DIM1)
        CALL INTERP_METEO(WHEN,'RAINC',RAINFALL%R,RAINFALL%DIM1)
      ENDIF
!
!     SNOW: SNOW
      CALL OS('X=C     ', X=SNOW, C=CST_SNOW )
      IF( INC_SNOW ) THEN
        CALL INTERP_METEO(WHEN,'SNOW',SNOW%R,SNOW%DIM1)
      ENDIF
!
!     WINDX,WINDY, OR WINDS,WINDD : WIND
      CALL OS('X=C     ', X=WINDX, C=CST_WINDX )
      IF( INC_WINDX ) THEN
        CALL INTERP_METEO(WHEN,'WINDX',WINDX%R,WINDX%DIM1)
      ENDIF
      CALL OS('X=C     ', X=WINDY, C=CST_WINDY )
      IF( INC_WINDY ) THEN
        CALL INTERP_METEO(WHEN,'WINDY',WINDY%R,WINDY%DIM1)
      ENDIF
      CALL OS('X=C     ', X=WINDS, C=CST_WINDS )
      IF( INC_WINDS ) THEN
        CALL INTERP_METEO(WHEN,'WINDS',WINDS%R,WINDS%DIM1)
      ENDIF
      CALL OS('X=C     ', X=WINDD, C=CST_WINDD )
      IF( INC_WINDD ) THEN
        CALL INTERP_METEO(WHEN,'WINDD',WINDD%R,WINDD%DIM1)
      ENDIF
      IF( ( INC_WINDS.AND.INC_WINDD ).AND.
     &    .NOT.( INC_WINDX.AND.INC_WINDY ) ) THEN
        CALL INTERP_WINDXY(WHEN,WINDX%R,WINDY%R,WINDD%DIM1)
        DO IPOIN = 1,WINDS%DIM1
          WINDS%R(IPOIN) = SQRT(WINDY%R(IPOIN)**2+WINDY%R(IPOIN)**2)
        ENDDO
      ELSEIF( ( INC_WINDS.OR.INC_WINDD ).AND.
     &    .NOT.( INC_WINDX.AND.INC_WINDY ) ) THEN
        DO IPOIN = 1,WINDX%DIM1
          WINDX%R(IPOIN) = - WINDS%R(IPOIN)*SIN( WINDD%R(IPOIN)*DTR )
          WINDY%R(IPOIN) = - WINDS%R(IPOIN)*COS( WINDD%R(IPOIN)*DTR )
        ENDDO
      ENDIF
!
!     ATMOSPHERIC PRESSURE
      CALL OS('X=C     ', X=PATMOS, C=CST_PATMOS )
      IF( INC_PATMOS ) THEN
        CALL INTERP_METEO(WHEN,'PATM',PATMOS%R,PATMOS%DIM1)
      ENDIF
!
!     PVAP: PVAP
      CALL OS('X=C     ', X=PVAP, C=CST_PVAP )
      IF( INC_PVAP ) THEN
        CALL INTERP_METEO(WHEN,'PVAP',PVAP%R,PVAP%DIM1)
      ENDIF
!
!     HREL: HREL
      CALL OS('X=C     ', X=HREL, C=CST_HREL )
      IF( INC_HREL ) THEN
        CALL INTERP_METEO(WHEN,'HREL',HREL%R,HREL%DIM1)
      ENDIF
!
!     RAY3: RAY3
      CALL OS('X=C     ', X=RAY3, C=CST_RAY3 )
      IF( INC_RAY3 ) THEN
        CALL INTERP_METEO(WHEN,'RAY3',RAY3%R,RAY3%DIM1)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                     ***********************
                      SUBROUTINE INTERP_METEO
!                     ***********************
!
     &( WHEN,WHAT,VALEURS,NPOIN )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Spatial and temporal interpolation of variables from either
!+        the ASCII or the BINARY file
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF NODES
!| VALEURS        |<->| VALUES CONTAINED IN THE VARIABLE
!| WHAT           |-->| VARIABLE TO CONSIDER
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,             INTENT(IN) :: NPOIN
      CHARACTER(LEN=*),    INTENT(IN) :: WHAT
      DOUBLE PRECISION,    INTENT(IN) :: WHEN
      DOUBLE PRECISION, INTENT(INOUT) :: VALEURS(NPOIN)
!
      INTEGER           J,IPOIN
      DOUBLE PRECISION  ALPHA,DELTA
      DOUBLE PRECISION  X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,NX,NY,NZ,MX,MY
      CHARACTER(LEN=16), ALLOCATABLE  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     A: ASCII FILE
      ALLOCATE(CHOIX(METEO_MAXVALUE))
      IF( METEO_DEJA(1) ) THEN
        CHOIX(1:METEO_MAXVALUE) = METEO_CHOIX(1,1:METEO_MAXVALUE)
        J = FIND_NAME( WHAT, CHOIX, METEO_MAXVALUE )
        IF( J.NE.0 .AND. WHAT.NE.'RAINC') THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEA(ITIMEA2)-TIMEA(ITIMEA1) )
          ALPHA = ( WHEN+TEL_OFFSET2-TIMEA(ITIMEA1) )/DELTA
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          IF( NPOINA.EQ.1 ) THEN
            DO IPOIN = 1,NPOIN
              VALEURS(IPOIN) =
     &          ( 1.D0-ALPHA )*VALUEA1(J,1) + ALPHA*VALUEA2(J,1)
!             INTERPOLATION LIKE IN OLD INTERPMETEO
!     &          VALUEA1(J,1) + (VALUEA2(J,1)-VALUEA1(J,1))*ALPHA
            ENDDO
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - TWO POINTS
          ELSEIF( NPOINA.EQ.2 ) THEN
!           THE PROJECTION OF THE POINT TO THE LINE WILL GIVE YOU THE Z
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
!           BEFORE
            Z1 = VALUEA1(J,1)
            Z2 = VALUEA1(J,2)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = ( 1.D0-ALPHA )*( Z1 )
              ENDDO
            ENDIF
!           AFTER
            Z1 = VALUEA2(J,1)
            Z2 = VALUEA2(J,2)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = VALEURS(IPOIN)
     &                         + ALPHA*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = VALEURS(IPOIN)
     &                         + ALPHA*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = VALEURS(IPOIN) + ALPHA*( Z1 )
              ENDDO
            ENDIF
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - THREE POINTS
          ELSEIF( NPOINA.EQ.3 ) THEN
!           THE NORMAL VECTOR AND THE FIRST POINT DEFINE THE PLAN
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
            X3 = XPOINA(3)
            Y3 = YPOINA(3)
!           BEFORE
            Z1 = VALUEA1(J,1)
            Z2 = VALUEA1(J,2)
            Z3 = VALUEA1(J,3)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VALEURS(IPOIN) = ( 1.D0-ALPHA )*( Z1-(MX+MY)/NZ )
            ENDDO
!           AFTER
            Z1 = VALUEA2(J,1)
            Z2 = VALUEA2(J,2)
            Z3 = VALUEA2(J,3)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VALEURS(IPOIN) = VALEURS(IPOIN) + ALPHA*( Z1-(MX+MY)/NZ )
            ENDDO
          ENDIF
!       ________________________________________________________________
        ELSEIF( J.NE.0 .AND. WHAT.EQ.'RAINC') THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEA(ITIMEA2)-TIMEA(ITIMEA1) )
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          IF( NPOINA.EQ.1 ) THEN
            DO IPOIN = 1,NPOIN
              VALEURS(IPOIN) = VALUEA2(J,1)/DELTA
            ENDDO
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - TWO POINTS
          ELSEIF( NPOINA.EQ.2 ) THEN
!           THE PROJECTION OF THE POINT TO THE LINE WILL GIVE YOU THE Z
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
!           AFTER
            Z1 = VALUEA2(J,1)
            Z2 = VALUEA2(J,2)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = ( Z1+Z3*(XPOINA(IPOIN)-X1) ) / DELTA
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = ( Z1+Z3*(YPOINA(IPOIN)-Y1) ) / DELTA
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VALEURS(IPOIN) = Z1 / DELTA
              ENDDO
            ENDIF
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - THREE POINTS
          ELSEIF( NPOINA.EQ.3 ) THEN
!           THE NORMAL VECTOR AND THE FIRST POINT DEFINE THE PLAN
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
            X3 = XPOINA(3)
            Y3 = YPOINA(3)
!           AFTER
            Z1 = VALUEA2(J,1)
            Z2 = VALUEA2(J,2)
            Z3 = VALUEA2(J,3)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VALEURS(IPOIN) = ( Z1-(MX+MY)/NZ ) / DELTA
            ENDDO
          ENDIF
!       ________________________________________________________________
        ENDIF
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     B: BINARY FILE
      IF( METEO_DEJA(2) ) THEN
        CHOIX(1:METEO_MAXVALUE) = METEO_CHOIX(2,1:METEO_MAXVALUE)
        J = FIND_NAME( WHAT, CHOIX, METEO_MAXVALUE )
        IF( J.NE.0 .AND. WHAT.NE.'RAINC') THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEB(ITIMEB2)-TIMEB(ITIMEB1) )
          ALPHA = ( WHEN+TEL_OFFSET2-TIMEB(ITIMEB1) )/DELTA
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          DO IPOIN = 1,NPOIN
            VALEURS(IPOIN) =
     &         ( 1.D0-ALPHA )*VALUEB1(J,IPOIN) + ALPHA*VALUEB2(J,IPOIN)
          ENDDO
!
        ELSEIF( J.NE.0 .AND. WHAT.EQ.'RAINC') THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEB(ITIMEB2)-TIMEB(ITIMEB1) )
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          DO IPOIN = 1,NPOIN
            VALEURS(IPOIN) = VALUEB2(J,IPOIN) / DELTA
          ENDDO
!
        ENDIF
      ENDIF
      DEALLOCATE(CHOIX)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                     ************************
                      SUBROUTINE INTERP_WINDXY
!                     ************************
!
     &( WHEN,VITX,VITY,NPOIN )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Spatial and temporal interpolation of X and Y wind components
!+        from either the ASCII or the BINARY file + magnitude+direction
!+        of velocity
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF NODES
!| VITX           |<->| VALUES CONTAINED IN THE VARIABLE X WIND COMPONENT
!| VITY           |<->| VALUES CONTAINED IN THE VARIABLE Y WIND COMPONENT
!| WHAT           |-->| VARIABLE TO CONSIDER
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER,             INTENT(IN) :: NPOIN
      DOUBLE PRECISION,    INTENT(IN) :: WHEN
      DOUBLE PRECISION, INTENT(INOUT) :: VITX(NPOIN),VITY(NPOIN)
!
      INTEGER           J1,J2,IPOIN
      DOUBLE PRECISION  DTR,TMPX1(3),TMPY1(3),TMPX2(3),TMPY2(3)
      DOUBLE PRECISION  ALPHA,DELTA
      DOUBLE PRECISION  X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,NX,NY,NZ,MX,MY
      CHARACTER(LEN=16), ALLOCATABLE  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DTR = ATAN(1.D0)/45.D0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     A: ASCII FILE
      ALLOCATE(CHOIX(METEO_MAXVALUE))
      IF( METEO_DEJA(1) ) THEN
        CHOIX(1:METEO_MAXVALUE) = METEO_CHOIX(1,1:METEO_MAXVALUE)
        J1 = FIND_NAME( 'WINDS', CHOIX, METEO_MAXVALUE )
        J2 = FIND_NAME( 'WINDD', CHOIX, METEO_MAXVALUE )
        IF( J1.NE.0 .AND. J2.NE.0) THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEA(ITIMEA2)-TIMEA(ITIMEA1) )
          ALPHA = ( WHEN+TEL_OFFSET2-TIMEA(ITIMEA1) )/DELTA
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          IF( NPOINA.EQ.1 ) THEN
            TMPX1(1) = -VALUEA1(J1,1)*SIN(VALUEA1(J2,1)*DTR)
            TMPY1(1) = -VALUEA1(J1,1)*COS(VALUEA1(J2,1)*DTR)
            TMPX2(1) = -VALUEA2(J1,1)*SIN(VALUEA2(J2,1)*DTR)
            TMPY2(1) = -VALUEA2(J1,1)*COS(VALUEA2(J2,1)*DTR)
            DO IPOIN = 1,NPOIN
              VITX(IPOIN) = ( 1.D0-ALPHA )*TMPX1(1) + ALPHA*TMPX2(1)
              VITY(IPOIN) = ( 1.D0-ALPHA )*TMPY1(1) + ALPHA*TMPY2(1)
!             INTERPOLATION LIKE IN OLD INTERPMETEO
!              VITX(IPOIN) = TMPX1(1) + (TMPX2(1)-TMPX1(1)) *ALPHA
!              VITY(IPOIN) = TMPY1(1) + (TMPY2(1)-TMPY1(1)) *ALPHA
            ENDDO
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - TWO POINTS
          ELSEIF( NPOINA.EQ.2 ) THEN
!           THE PROJECTION OF THE POINT TO THE LINE WILL GIVE YOU THE Z
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
! X WIND VELOCITY COMPONENT
!           BEFORE
            Z1 = -VALUEA1(J1,1)*SIN(VALUEA1(J2,1)*DTR)
            Z2 = -VALUEA1(J1,2)*SIN(VALUEA1(J2,2)*DTR)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VITX(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VITX(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VITX(IPOIN) = ( 1.D0-ALPHA )*( Z1 )
              ENDDO
            ENDIF
!           AFTER
            Z1 = -VALUEA2(J1,1)*SIN(VALUEA2(J2,1)*DTR)
            Z2 = -VALUEA2(J1,2)*SIN(VALUEA2(J2,2)*DTR)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VITX(IPOIN) = VITX(IPOIN)
     &                         + ALPHA*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VITX(IPOIN) = VITX(IPOIN)
     &                         + ALPHA*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VITX(IPOIN) = VITX(IPOIN) + ALPHA*( Z1 )
              ENDDO
            ENDIF
! Y WIND VELOCITY COMPONENT
!           BEFORE
            Z1 = -VALUEA1(J1,1)*COS(VALUEA1(J2,1)*DTR)
            Z2 = -VALUEA1(J1,2)*COS(VALUEA1(J2,2)*DTR)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VITY(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VITY(IPOIN) =
     &            ( 1.D0-ALPHA )*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VITY(IPOIN) = ( 1.D0-ALPHA )*( Z1 )
              ENDDO
            ENDIF
!           AFTER
            Z1 = -VALUEA2(J1,1)*COS(VALUEA2(J2,1)*DTR)
            Z2 = -VALUEA2(J1,2)*COS(VALUEA2(J2,2)*DTR)
            Z3 = 0.D0
            IF( ABS(X2-X1).GT.ABS(Y2-Y1) ) THEN
              Z3 = ( Z2-Z1 )/( X2-X1 )
              DO IPOIN = 1,NPOIN
                VITY(IPOIN) = VITY(IPOIN)
     &                         + ALPHA*( Z1+Z3*(XPOINA(IPOIN)-X1) )
              ENDDO
            ELSEIF( ABS(Y2-Y1).GT.ABS(X2-X1) ) THEN
              Z3 = ( Z2-Z1 )/( Y2-Y1 )
              DO IPOIN = 1,NPOIN
                VITY(IPOIN) = VITY(IPOIN)
     &                         + ALPHA*( Z1+Z3*(YPOINA(IPOIN)-Y1) )
              ENDDO
            ELSE
              DO IPOIN = 1,NPOIN
                VITY(IPOIN) = VITY(IPOIN) + ALPHA*( Z1 )
              ENDDO
            ENDIF
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - THREE POINTS
          ELSEIF( NPOINA.EQ.3 ) THEN
!           THE NORMAL VECTOR AND THE FIRST POINT DEFINE THE PLAN
            X1 = XPOINA(1)
            Y1 = YPOINA(1)
            X2 = XPOINA(2)
            Y2 = YPOINA(2)
            X3 = XPOINA(3)
            Y3 = YPOINA(3)
! X WIND VELOCITY COMPONENT
!           BEFORE
            Z1 = -VALUEA1(J1,1)*SIN(VALUEA1(J2,1)*DTR)
            Z2 = -VALUEA1(J1,2)*SIN(VALUEA1(J2,2)*DTR)
            Z3 = -VALUEA1(J1,3)*SIN(VALUEA1(J2,3)*DTR)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VITX(IPOIN) = ( 1.D0-ALPHA )*( Z1-(MX+MY)/NZ )
            ENDDO
!           AFTER
            Z1 = -VALUEA2(J1,1)*SIN(VALUEA2(J2,1)*DTR)
            Z2 = -VALUEA2(J1,2)*SIN(VALUEA2(J2,2)*DTR)
            Z3 = -VALUEA2(J1,3)*SIN(VALUEA2(J2,3)*DTR)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VITX(IPOIN) = VITX(IPOIN) + ALPHA*( Z1-(MX+MY)/NZ )
            ENDDO
! Y WIND VELOCITY COMPONENT
!           BEFORE
            Z1 = -VALUEA1(J1,1)*COS(VALUEA1(J2,1)*DTR)
            Z2 = -VALUEA1(J1,2)*COS(VALUEA1(J2,2)*DTR)
            Z3 = -VALUEA1(J1,3)*COS(VALUEA1(J2,3)*DTR)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VITY(IPOIN) = ( 1.D0-ALPHA )*( Z1-(MX+MY)/NZ )
            ENDDO
!           AFTER
            Z1 = -VALUEA2(J1,1)*COS(VALUEA2(J2,1)*DTR)
            Z2 = -VALUEA2(J1,2)*COS(VALUEA2(J2,2)*DTR)
            Z3 = -VALUEA2(J1,3)*COS(VALUEA2(J2,3)*DTR)
            NX = ( Y2-Y1 )*( Z3-Z1 ) - ( Z2-Z1 )*( Y3-Y1 )
            NY = ( Z2-Z1 )*( X3-X1 ) - ( X2-X1 )*( Z3-Z1 )
            NZ = ( X2-X1 )*( Y3-Y1 ) - ( Y2-Y1 )*( X3-X1 )
            DO IPOIN = 1,NPOIN
              MX = ( XPOINA(IPOIN)-X1 )*NX
              MY = ( YPOINA(IPOIN)-Y1 )*NY
              VITY(IPOIN) = VITY(IPOIN) + ALPHA*( Z1-(MX+MY)/NZ )
            ENDDO
          ENDIF
!       ________________________________________________________________
        ENDIF
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     B: BINARY FILE
      IF( METEO_DEJA(2) ) THEN
        CHOIX(1:METEO_MAXVALUE) = METEO_CHOIX(2,1:METEO_MAXVALUE)
        J1 = FIND_NAME( 'WINDS', CHOIX, METEO_MAXVALUE )
        J2 = FIND_NAME( 'WINDD', CHOIX, METEO_MAXVALUE )
        IF( J1.NE.0 .AND. J2.NE.0) THEN
!         ______________________________________________________________
!         TEMPORAL INTERPOLATION
          DELTA = ( TIMEB(ITIMEB2)-TIMEB(ITIMEB1) )
          ALPHA = ( WHEN+TEL_OFFSET2-TIMEB(ITIMEB1) )/DELTA
!         ______________________________________________________________
!         SPATIAL INTERPOLATION - NO INTERPOLATION
          DO IPOIN = 1,NPOIN
            TMPX1(1) = -VALUEB1(J1,IPOIN)*SIN(VALUEB1(J2,IPOIN)*DTR)
            TMPY1(1) = -VALUEB1(J1,IPOIN)*COS(VALUEB1(J2,IPOIN)*DTR)
            TMPX2(1) = -VALUEB2(J1,IPOIN)*SIN(VALUEB2(J2,IPOIN)*DTR)
            TMPY2(1) = -VALUEB2(J1,IPOIN)*COS(VALUEB2(J2,IPOIN)*DTR)
            VITX(IPOIN) = ( 1.D0-ALPHA )*TMPX1(1) + ALPHA*TMPX2(1)
            VITY(IPOIN) = ( 1.D0-ALPHA )*TMPY1(1) + ALPHA*TMPY2(1)
          ENDDO
!
        ENDIF
      ENDIF
      DEALLOCATE(CHOIX)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   **************************
                    INTEGER FUNCTION FIND_NAME
!                   **************************
!
     &( NAME,CHOIX,MAXVALUE )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Search for NAME in a list of CHOIX (variables found in the
!+        METEO files). Return 0 if not found, the index in CHOIX
!+        otherwise.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHOIX          |-->| LIST OF VARIABLES PRESENT IN THE METEO FILE
!| MAXVALUE       |-->| MAXIMUM SIZE OF THE LIST CHOIX
!| NAME           |-->| MNEMO OF THE VARIABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*), INTENT(IN) :: NAME
      INTEGER,          INTENT(IN) :: MAXVALUE
      CHARACTER(LEN=*), INTENT(IN) :: CHOIX(MAXVALUE)
!
      INTEGER :: I
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     DEFAULT IF NOT FOUND
      FIND_NAME = 0
!     LOOP THROUGH
      DO I = 1,MAXVALUE
        IF( INCLUS( CHOIX(I), NAME ) ) FIND_NAME = I
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION
!
!=======================================================================
!
!                   **************************
                    SUBROUTINE INIT_FIC_BINARY
!                   **************************
!
     &( FILES,ATMFILEB )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Scan the ASCII file and prepare skeleton for future calls
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATMFILEA       |-->| LOGICAL UNIT OF ASCII FILE FOR METEO
!| FILES          |-->| ARRAYS OF ALL FILES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      IMPLICIT NONE
!
      TYPE(BIEF_FILE),     INTENT(IN) :: FILES(*)
      INTEGER,             INTENT(IN) :: ATMFILEB
!
      INTEGER :: I,J, NFIC, IERR
      CHARACTER(LEN=8)  :: NFMT
!
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
      CHARACTER(LEN=16), POINTER  :: UNITS(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SETTING LOCAL ASCII FILE
      METEO_FILES(2)%LU      = FILES(ATMFILEB)%LU
      METEO_FILES(2)%NAME    = FILES(ATMFILEB)%NAME
      METEO_FILES(2)%TELNAME = FILES(ATMFILEB)%TELNAME
      METEO_FILES(2)%FMT     = FILES(ATMFILEB)%FMT
      METEO_FILES(2)%ACTION  = FILES(ATMFILEB)%ACTION
      METEO_FILES(2)%BINASC  = FILES(ATMFILEB)%BINASC
      METEO_FILES(2)%TYPE    = FILES(ATMFILEB)%TYPE
!
!     SIMPLIFYING NOTATIONS
      NFMT     = METEO_FILES(2)%FMT
      NFIC     = METEO_FILES(2)%LU
      UNITS =>   METEO_UNITS(2,1:METEO_MAXVALUE)
      CHOIX =>   METEO_CHOIX(2,1:METEO_MAXVALUE)
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SCAN THE SKELETON
!
!     __________________________________________________________________
!     GET THE NUMBER OF POINTS IN THE METEO FILE
      CALL GET_MESH_NPOIN(NFMT,NFIC,TRIANGLE_ELT_TYPE,NPOINB,IERR)
      CALL CHECK_CALL
     &  (IERR,"METEO,INIT_FIC_BINARY: GET_MESH_NPOIN")
!
!     __________________________________________________________________
!     FIND OUT WHAT VARIABLES ARE GIVEN IN THE FILE
!
      CALL GET_DATA_NVAR(NFMT,NFIC,NVALUEB,IERR)
      CALL CHECK_CALL
     &  (IERR, 'METEO,INIT_FIC_BINARY:GET_DATA_NVAR')
!
      CALL GET_DATA_VAR_LIST(NFMT,NFIC,NVALUEB,CHOIX,UNITS,IERR)
      CALL CHECK_CALL
     &  (IERR, 'METEO,INIT_FIC_BINARY:GET_DATA_VAR_LIST')
!
!     __________________________________________________________________
!     GET THE TIME PROFILE FOR FUTURE INTERPOLATION
      CALL GET_DATA_NTIMESTEP(NFMT,NFIC,NTIMEB,IERR)
      CALL CHECK_CALL
     &  (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_NTIMESTEP')
!
      ALLOCATE(TIMEB(NTIMEB))
      DO I = 1,NTIMEB
        CALL GET_DATA_TIME(NFMT,NFIC,I-1,TIMEB(I),IERR)
        CALL CHECK_CALL
     &    (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_TIME:I')
        TIMEB(I) = TIMEB(I) + METEO_OFFSET
      ENDDO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     PREPARE FOR NEXT ACCESS TO THE FILE
!
      ALLOCATE(VALUEB1(NVALUEB,NPOINB),VALUEB2(NVALUEB,NPOINB))
      ALLOCATE(VALUEB0(NPOINB))
!     __________________________________________________________________
!     READ ONE TIME FRAME AT LEAST
      ITIMEB1 = 1
      ITIMEB2 = 1
      DO I = 1,NVALUEB
        CALL GET_DATA_VALUE
     &    (NFMT,NFIC,ITIMEB1,CHOIX(I),VALUEB0,NPOINB,IERR)
        CALL CHECK_CALL
     &    (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_VALUE:CHOIX')
        DO J = 1,NPOINB
          VALUEB1(I,J) = VALUEB0(J)
          VALUEB2(I,J) = VALUEB0(J)
        ENDDO
      ENDDO
!     __________________________________________________________________
!     READ A SECOND TIME FRAME IF THERE
      IF( NTIMEB.GT.1 )THEN
        ITIMEB2 = 1
        DO I = 1,NVALUEB
          CALL GET_DATA_VALUE
     &      (NFMT,NFIC,ITIMEB2,CHOIX(I),VALUEB0,NPOINB,IERR)
          CALL CHECK_CALL
     &      (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_VALUE:CHOIX')
          DO J = 1,NPOINB
            VALUEB2(I,J) = VALUEB0(J)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   *************************
                    SUBROUTINE INIT_FIC_ASCII
!                   *************************
!
     &( FILES,ATMFILEA )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Scan the ASCII file and prepare skeleton for future calls
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATMFILEA       |-->| LOGICAL UNIT OF ASCII FILE FOR METEO
!| FILES          |-->| ARRAYS OF ALL FILES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      TYPE(BIEF_FILE),     INTENT(IN) :: FILES(*)
      INTEGER,             INTENT(IN) :: ATMFILEA
!
      INTEGER :: I,J,K, NFIC, IDEB,IFIN, NPOINX,NPOINY
      CHARACTER(LEN=SIZELIGN) :: LIGNE
      DOUBLE PRECISION :: X1,X2,X3, Y1,Y2,Y3
!
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SETTING LOCAL ASCII FILE
      METEO_DEJA(1) = .FALSE.
      METEO_FILES(1)%LU      = FILES(ATMFILEA)%LU
      METEO_FILES(1)%NAME    = FILES(ATMFILEA)%NAME
      METEO_FILES(1)%TELNAME = FILES(ATMFILEA)%TELNAME
      METEO_FILES(1)%FMT     = FILES(ATMFILEA)%FMT
      METEO_FILES(1)%ACTION  = FILES(ATMFILEA)%ACTION
      METEO_FILES(1)%BINASC  = FILES(ATMFILEA)%BINASC
      METEO_FILES(1)%TYPE    = FILES(ATMFILEA)%TYPE
!
!     SIMPLIFYING NOTATIONS
      NFIC     = METEO_FILES(1)%LU
      CHOIX => METEO_CHOIX(1,1:METEO_MAXVALUE)
!
!     DEFAULT NUMBER OF LOCATIONS
      NPOINX = 1
      NPOINY = 1
      X1 = 0.D0
      Y1 = 0.D0
!
      METEO_REF_DATE(1) = 0
      METEO_REF_DATE(2) = 0
      METEO_REF_DATE(3) = 0
      METEO_REF_DATE(4) = 0
      METEO_REF_DATE(5) = 0
      METEO_REF_DATE(6) = 0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     READ THE FILE ONE TIME IN FULL TO SIZE UP ITS CONTENT
!     ( TODO: REPLACE GOTO STATEMENTS BY WHILE STATEMENT )
      REWIND(NFIC)
!
!  READS THE HEADLINE OF THE DATA FILE
!
!       JUMPING TWO LINES OF COMMENTS
      READ(NFIC,FMT='(A)',ERR=103) LIGNE
      IF(LIGNE(1:8).EQ.'#REFDATE') THEN
        CALL READ_REF_DATE(LIGNE, METEO_REF_DATE)
      ELSE
        BACKSPACE(NFIC)
      ENDIF
!     __________________________________________________________________
!     SKIP COMMENTS
!
 101  READ(NFIC,FMT='(A)',ERR=103) LIGNE
      GOTO 102
!
 103  CONTINUE
      WRITE(LU,*) 'ERROR WHILE READING THE ASCII FILE'
      WRITE(LU,*) 'USED FOR THE METEO (THERMAL PROCESSES)'
      WRITE(LU,*) 'PROBABLY A PROBLEM OF FORMAT'
      WRITE(LU,*) 'ANY WINDOWS CARRIAGE RETURNS ON UNIX OR LINUX'
      WRITE(LU,*) 'GUILTY LINE:'
      WRITE(LU,*) LIGNE
      CALL PLANTE(1)
      STOP
!
 102  CONTINUE
!     IF( LIGNE(1:36).EQ.'#SPECIAL ASCII ATMOSPHERIC DATA FILE' ) THEN
!       WRITE(LU,*) 'SPECIAL ASCII ATMOSPHERIC DATA FILE'
!       WRITE(LU,*) 'TREATMENT TO IMPLEMENTED BY THE USER'
!       RETURN
!     ENDIF
!
      IF( LIGNE(1:1).EQ.':' ) THEN
!       DEFINING X-LOCATIONS IF MORE THAN ONE (UP TO THREE)
        IF( INCLUS(LIGNE,':X') )THEN
          IDEB = 2
 151      IF( IDEB.GE.SIZELIGN ) GOTO 152
          IDEB = IDEB + 1
          IF( (LIGNE(IDEB:IDEB).NE.' ') .AND.
     &         LIGNE(IDEB:IDEB).NE.CHAR(9) ) GOTO 151
!         THE REST OF THE LINE SHOULD HAVE AT MOST THREE VALUES
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=152) X1,X2
          NPOINX = NPOINX + 1
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=152) X1,X2,X3
          NPOINX = NPOINX + 1
 152      CONTINUE
!       DEFINING Y-LOCATIONS IF MORE THAN ONE (UP TO THREE)
        ELSEIF( INCLUS(LIGNE,':Y') )THEN
          IDEB = 2
 153      IF( IDEB.GE.SIZELIGN ) GOTO 154
          IDEB = IDEB + 1
          IF( (LIGNE(IDEB:IDEB).NE.' ') .AND.
     &         LIGNE(IDEB:IDEB).NE.CHAR(9) ) GOTO 153
!         THE REST OF THE LINE SHOULD HAVE AT MOST THREE VALUES
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=154) Y1,Y2
          NPOINY = NPOINY + 1
          READ(LIGNE(IDEB:SIZELIGN),*,ERR=154) Y1,Y2,Y3
          NPOINY = NPOINY + 1
 154      CONTINUE
        ENDIF
      ENDIF
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 101
!
      IF(ANY(METEO_REF_DATE.NE.0)) THEN
        IF(TEL_OFFSET.LE.1.D-16) THEN
          WRITE(LU,*) 'REFERENCE DATE IN ASCII ATMOSPHERIC DATA FILE'
          WRITE(LU,*) 'MISSING ORIGINAL DATE OF TIME IN '//
     &                'STEERING FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
        METEO_OFFSET = DATE_MJD2SEC(METEO_REF_DATE(1:3),
     &                              METEO_REF_DATE(4:6))
        TEL_OFFSET2 = TEL_OFFSET
        WRITE(LU,*) 'USING REFERENCE DATE FOR METEO:'
        WRITE(LU,666) METEO_REF_DATE
666     FORMAT(5X,1I4,'-',1I0.2,'-',1I0.2,' ',
     &         1I0.2,':',1I0.2,':',1I0.2)
      ELSE
        TEL_OFFSET2 = 0.D0
        METEO_OFFSET = 0.D0
      ENDIF
!
!     __________________________________________________________________
!     FINALISING LOCATIONS
!
      IF( NPOINX.NE.NPOINY )THEN
        WRITE(LU,*) 'NUMBER OF LOCATIONS X AND Y LOCATIONS READ'
        WRITE(LU,*) 'IN THE ASCII FILE USED FOR THE METEO (THERMAL'
        WRITE(LU,*) 'PROCESSES) DIFFERENT.'
      ENDIF
      NPOINA = NPOINX
      ALLOCATE( XPOINA(NPOINA),YPOINA(NPOINA) )
      IF( NPOINA.GE.1 ) THEN
        XPOINA(1) = X1
        YPOINA(1) = Y1
      ENDIF
      IF( NPOINA.GE.2 ) THEN
        XPOINA(2) = X2
        YPOINA(2) = Y2
      ENDIF
      IF( NPOINA.EQ.3 ) THEN
        XPOINA(3) = X3
        YPOINA(3) = Y3
      ENDIF
!     __________________________________________________________________
!     FIND OUT WHAT AND HOW MANY VALUES ARE GIVEN IN THE FILE
!
      NVALUEA = -1
      IFIN = 1
 104  IDEB = IFIN
!
!     IDENTIFY FIRST CHARACTER OF NAME
 105  IF((LIGNE(IDEB:IDEB).EQ.' '.OR.LIGNE(IDEB:IDEB).EQ.CHAR(9))
     &   .AND.IDEB.LT.SIZELIGN) THEN
        IDEB = IDEB + 1
        GOTO 105
      ENDIF
!     IDENTIFY LAST CHARACTER OF NAME ( TODO: USE A WHILE STATEMENT )
      IFIN = IDEB
 106  IF( LIGNE(IFIN:IFIN).NE.' '.AND.LIGNE(IFIN:IFIN).NE.CHAR(9)
     &   .AND.IFIN.LT.SIZELIGN) THEN
        IFIN = IFIN + 1
        GOTO 106
      ENDIF
!
      IF( IDEB.EQ.IFIN ) GOTO 140  ! IDEB .EQ. IFIN .EQ. SIZELIGN
!
      NVALUEA = NVALUEA + 1
      IF( NVALUEA.EQ.0 ) THEN
!       SPECIAL CASE FOR TIME
        IF(LIGNE(IDEB:IFIN-1).NE.'T') THEN
          WRITE(LU,*) 'THE FIRST VARIABLE MUST BE TIME T IN THE'
          WRITE(LU,*) 'ASCII FILE USED FOR THE METEO (THERMAL'
          WRITE(LU,*) 'PROCESSES). OTHER POSSIBLE CAUSE:'
          WRITE(LU,*) 'THERE ARE TABS IN THE FILE'
          WRITE(LU,*) 'CHANGE TABS INTO SPACES'
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSEIF( NVALUEA.LE.METEO_MAXVALUE ) THEN
        CHOIX(NVALUEA) = '                '
        CHOIX(NVALUEA)(1:IFIN-IDEB+1) = LIGNE(IDEB:IFIN-1)
      ELSE
        WRITE(LU,*) 'INCREASE MAXVALUE FOR READ_FIC_ASCII'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(IFIN.LT.SIZELIGN) GO TO 104
!
      IF( INT(NVALUEA/NPOINA)*NPOINA .EQ. NVALUEA )THEN
        NVALUEA = INT(NVALUEA/NPOINA)
      ELSE
        WRITE(LU,*) 'NUMBER OF LOCATIONS AND TOTAL NUMBER OF VALUES'
        WRITE(LU,*) 'FOUND IN THE ASCCI FILE FOR METEO (THERMAL'
        WRITE(LU,*) 'PROCESSES) ARE INCOMPATIBLE.'
        WRITE(LU,*) 'ONE VALUE PER VARIABLE SHOULD BE THERE FOR EACH'
        WRITE(LU,*) 'LOCATION.'
        CALL PLANTE(1)
        STOP
      ENDIF
!     __________________________________________________________________
!     SKIP THE LINE WITH UNITS AS WELL AS COMMENTS
 140  READ(NFIC,FMT='(A)',ERR=103) LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 140
!     __________________________________________________________________
!     COUNT LINES OF DATA
      NTIMEA = 0
 201  READ(NFIC,*,END=202,ERR=203) LIGNE
      IF( LIGNE(1:1).NE.'#' .AND.
     &    LIGNE(1:1).NE.'!' .AND.
     &    LIGNE(1:1).NE.':' ) NTIMEA = NTIMEA + 1
      GOTO 201
!
 203  CONTINUE
      WRITE(LU,*) 'ERROR READING THE ASCII FILE USED FOR THE METEO'
      WRITE(LU,*) '(THERMAL PROCESSES) AT LINE: ',NTIMEA
      WRITE(LU,*) '(COMMENTS EXCLUDED)'
      CALL PLANTE(1)
      STOP
!
 202  CONTINUE
      IF( NTIMEA.LE.1 ) THEN
        WRITE(LU,*) 'TWO TIME STEP AT LEAST SHOULD BE PRESENT IN'
        WRITE(LU,*) 'THE ASCII FILE USED FOR THE METEO (THERMAL'
        WRITE(LU,*) 'PROCESSES)'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     READ THE FILE A SECOND TIME IN FULL TO KEEP ITS SCKELETON
!     ( TODO: REPLACE GOTO STATEMENTS BY WHILE STATEMENT )
      REWIND(NFIC)
!     __________________________________________________________________
!     ALLOCATE TIME SCKELETON IN MEMORY
      ALLOCATE(TIMEA(NTIMEA))
      ALLOCATE(VALUEA1(NVALUEA,NPOINA),VALUEA2(NVALUEA,NPOINA))
!     __________________________________________________________________
!     SKIP COMMENTS AND FIRST TWO MANDATORY LINES
 111  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 111
 112  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 112
!     __________________________________________________________________
!     SAVE ALL TIMES FOR TEMPORAL INTERPOLATION
      DO I = 1,NTIMEA
 113    READ(NFIC,FMT='(A)') LIGNE
        IF( LIGNE(1:1).EQ.'#' .OR.
     &      LIGNE(1:1).EQ.'!' .OR.
     &      LIGNE(1:1).EQ.':' ) GOTO 113
        READ(LIGNE,*) TIMEA(I),((VALUEA1(J,K),J=1,NVALUEA),K=1,NPOINA)
        TIMEA(I) = TIMEA(I) + METEO_OFFSET
      ENDDO
!
      IF(METEO_OFFSET.LE.1.D-16) THEN
        TEL_OFFSET2 = 0.D0
      ELSE
        TEL_OFFSET2 = TEL_OFFSET
      ENDIF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     REWIND AND PREPARE FOR NEXT ACCESS TO THE FILE
      REWIND(NFIC)
!     __________________________________________________________________
!     SKIP COMMENTS AND FIRST TWO MANDATORY LINES
 121  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 121
 122  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 122
!     __________________________________________________________________
!     INITIAL TIMES AND VALUES TO THE FIRST TWO LINES
      ITIMEA1 = 1
 123  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 123
      READ(LIGNE,*) DUMMY,   ! TIMEA(ITIMEA1), ALREADY STORED + SHIFTED
     &  ((VALUEA1(J,K),J=1,NVALUEA),K=1,NPOINA)
      ITIMEA2 = 2
 124  READ(NFIC,FMT='(A)') LIGNE
      IF( LIGNE(1:1).EQ.'#' .OR.
     &    LIGNE(1:1).EQ.'!' .OR.
     &    LIGNE(1:1).EQ.':' ) GOTO 124
      READ(LIGNE,*) DUMMY,   ! TIMEA(ITIMEA2), ALREADY STORED + SHIFTED
     &  ((VALUEA2(J,K),J=1,NVALUEA),K=1,NPOINA)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   **************************
                    SUBROUTINE SYNC_FIC_BINARY
!                   **************************
!
     &( WHEN )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Synchronise the BINARY file for spatial and temporal
!         interpolation
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN)    :: WHEN
!
      INTEGER :: I,J, IERR, NFIC
      CHARACTER(LEN=8)  :: NFMT
!
      CHARACTER(LEN=16), POINTER  :: CHOIX(:)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SIMPLIFYING NOTATIONS
      NFMT     = METEO_FILES(2)%FMT
      NFIC     = METEO_FILES(2)%LU
      CHOIX =>   METEO_CHOIX(2,1:METEO_MAXVALUE)
!
!
!     INTERPOLATE IN TIME FOR ONE PARTICULAR VARIABLE
!     __________________________________________________________________
!     TOO EARLY
      IF( WHEN+TEL_OFFSET2.LT.TIMEB(1) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO EARLY COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF BINARY FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
!     __________________________________________________________________
!     TOO LATE
      ELSEIF( WHEN+TEL_OFFSET2.GT.TIMEB(NTIMEB) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO LATE COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF BINARY FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
      ENDIF
!     __________________________________________________________________
!     FIND WHEN
      IF( WHEN+TEL_OFFSET2.GT.TIMEB(ITIMEB2) )THEN
 132    ITIMEB1 = ITIMEB2
        DO I = 1,NVALUEB
          DO J = 1,NPOINB
            VALUEB1(I,J) = VALUEB2(I,J)
          ENDDO
        ENDDO
        ITIMEB2 = ITIMEB2 + 1
        DO I = 1,NVALUEB
          CALL GET_DATA_VALUE
     &      (NFMT,NFIC,ITIMEB2,CHOIX(I),VALUEB0,NPOINB,IERR)
          CALL CHECK_CALL
     &      (IERR,'METEO,INIT_FIC_BINARY:GET_DATA_VALUE:CHOIX')
          DO J = 1,NPOINB
            VALUEB2(I,J) = VALUEB0(J)
          ENDDO
        ENDDO
        IF( WHEN+TEL_OFFSET2.GT.TIMEB(ITIMEB2) ) GOTO 132
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!                   *************************
                    SUBROUTINE SYNC_FIC_ASCII
!                   *************************
!
     &( WHEN )
!
!***********************************************************************
! BIEF   V8P2
!***********************************************************************
!
!brief    Synchronise the ASCII file for spatial and temporal
!         interpolation
!
!history  S.E. BOURBAN (HRW)
!+        11/06/2017
!+        V7P3
!+        Initial implementation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WHEN           |-->| CURRENT TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(IN)    :: WHEN
!
      INTEGER :: J,K, NFIC
      CHARACTER(LEN=SIZELIGN) :: LIGNE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SIMPLIFYING NOTATIONS
      NFIC     = METEO_FILES(1)%LU
!
!
!     INTERPOLATE IN TIME FOR ONE PARTICULAR VARIABLE
!     __________________________________________________________________
!     TOO EARLY
      IF( WHEN+TEL_OFFSET2.LT.TIMEA(1) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO EARLY COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF ASCII FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
!     __________________________________________________________________
!     TOO LATE
      ELSEIF( WHEN+TEL_OFFSET2.GT.TIMEA(NTIMEA) ) THEN
        WRITE(LU,*) 'THE TIME REQUESTED IS TOO LATE COMPARED TO'
        WRITE(LU,*) 'THE PROFIL OF ASCII FILE USED FOR THE METEO'
        WRITE(LU,*) '(THERMAL PROCESSES)'
        CALL PLANTE(1)
        STOP
      ENDIF
!     __________________________________________________________________
!     FIND WHEN
      IF( WHEN+TEL_OFFSET2.GE.TIMEA(ITIMEA2) )THEN
!
 132    ITIMEA1 = ITIMEA2
        DO J = 1,NVALUEA
          DO K = 1,NPOINA
            VALUEA1(J,K) = VALUEA2(J,K)
          ENDDO
        ENDDO
        ITIMEA2 = ITIMEA2 + 1
 133    READ(NFIC,FMT='(A)') LIGNE
        IF( LIGNE(1:1).EQ.'#' .OR.
     &      LIGNE(1:1).EQ.'!' .OR.
     &      LIGNE(1:1).EQ.':' ) GOTO 133
        READ(LIGNE,*) DUMMY,   ! TIMEA(ITIMEA2), ALREADY STORED + SHIFTED 
     &      ((VALUEA2(J,K),J=1,NVALUEA),K=1,NPOINA)
        IF( WHEN+TEL_OFFSET2.GE.TIMEA(ITIMEA2) ) GOTO 132
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!
!=======================================================================
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      END MODULE METEO_TELEMAC
