!                   ****************
                    SUBROUTINE METEO
!                   ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,AT,LT,NPOIN,VENT,ATMOS,
     & ATMFILEA,ATMFILEB,FILES,LISTIN,
     & PATMOS_VALUE,AWATER_QUALITY,PLUIE,AOPTWIND,AWIND_SPD)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES ATMOSPHERIC PRESSURE AND WIND VELOCITY FIELDS
!+               (IN GENERAL FROM INPUT DATA FILES).
!
!warning  CAN BE ADAPTED BY USER
!
!history  J-M HERVOUET (LNHE)
!+        02/01/2004
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/01/2013
!+        V6P3
!+   Now 2 options with an example for reading a file. Extra arguments.
!
!history  C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Reading a file of meteo data for exchange with atmosphere
!+   Only the wind is used here
!
!history R.ATA (LNHE)
!+        09/11/2014
!+        V7P0
!+  introducion of water quality option + pluie is introduced as
!+   an optional parameter + remove of my_option which is replaced
!+   by a new keyword + value of patmos managed also with a new keyword
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        07/01/2015
!+        V7P0
!+  Adding optional arguments to remove USE DECLARATIONS_TELEMAC2D.
!
!history R.ATA (LNHE)
!+        16/11/2015
!+        V7P0
!+  Adding USE WAQTEL...
!
!history A. LEROY (LNHE)
!+        25/11/2015
!+        V7P1
!+  INTERPMETEO now writes directly in variables of WAQTEL which
!+  can be used by the other modules. This makes it possible to
!+  remove subsequent calls to INTERPMETEO in TELEMAC3D
!
!history J.-M. HERVOUET (RETIRED)
!+        01/07/2017
!+        V7P2
!+  Setting of UL moved outside the test IF(LT.EQ.0)... After a post by
!+  Qilong Bi (thanks Qilong...).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| ATMFILEA       |-->| LOGICAL UNIT OF THE ASCII ATMOSPHERIC FILE
!| ATMFILEB       |-->| LOGICAL UNIT OF THE BINARY ATMOSPHERIC FILE
!| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FUAIR          |<->| VELOCITY OF WIND ALONG X, IF CONSTANT
!| FVAIR          |<->| VELOCITY OF WIND ALONG Y, IF CONSTANT
!| LISTIN         |-->| IF YES, PRINTS INFORMATION
!| LT             |-->| ITERATION NUMBER
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PATMOS         |<--| ATMOSPHERIC PRESSURE
!| PATMOS_VALUE   |-->| VALUE OF ATMOSPHERIC PRESSURE IS CONSTANT
!| VENT           |-->| YES IF WIND TAKEN INTO ACCOUNT
!| WINDX          |<--| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |<--| SECOND COMPONENT OF WIND VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: TAIR_VALUE,ATMOSEXCH,RAYAED2
      USE METEO_TELEMAC, ONLY: TAIR,SYNC_METEO,RAY3,RAINFALL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LT,NPOIN,ATMFILEA,ATMFILEB
      LOGICAL, INTENT(IN)             :: ATMOS,VENT,LISTIN
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(*)
      DOUBLE PRECISION, INTENT(IN)    :: AT,PATMOS_VALUE
      DOUBLE PRECISION, INTENT(INOUT) :: FUAIR,FVAIR
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
!     OPTIONAL
      LOGICAL, INTENT(IN)          ,OPTIONAL :: AWATER_QUALITY
      TYPE(BIEF_OBJ), INTENT(INOUT),OPTIONAL :: PLUIE
      INTEGER, INTENT(IN)          ,OPTIONAL :: AOPTWIND
      DOUBLE PRECISION, INTENT(IN) ,OPTIONAL :: AWIND_SPD(2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL WATER_QUALITY
      INTEGER OPTWIND
      DOUBLE PRECISION WIND_SPD(2)
!
!-----------------------------------------------------------------------
!
!     DEFAULT VALUES OF PARAMETERS WHEN THEY ARE NOT GIVEN
!
      WATER_QUALITY=.FALSE.
      IF(PRESENT(AWATER_QUALITY)) WATER_QUALITY=AWATER_QUALITY
      OPTWIND=1
      IF(PRESENT(AOPTWIND)) OPTWIND=AOPTWIND
      WIND_SPD(1)=0.D0
      WIND_SPD(2)=0.D0
      IF(PRESENT(AWIND_SPD)) THEN
        WIND_SPD(1)=AWIND_SPD(1)
        WIND_SPD(2)=AWIND_SPD(2)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AT FIRST TIMESTEP
!
      IF(LT.EQ.0) THEN
!
!       ATMOSPHERIC PRESSURE AND AIR TEMPERATURE
!
        IF(ATMOS.OR.WATER_QUALITY) THEN
          CALL OV('X=C     ', X=PATMOS, C=PATMOS_VALUE, DIM1=NPOIN)
        ENDIF
        IF(WATER_QUALITY) THEN
          CALL OV('X=C     ', X=TAIR%R, C=TAIR_VALUE, DIM1=NPOIN)
        ENDIF
!
!       WIND AND/OR OTHER METEO VARIABLES
!
        IF(VENT.OR.WATER_QUALITY) THEN
          IF(OPTWIND.EQ.1.AND.FILES(ATMFILEA)%NAME(1:1).EQ.' ') THEN
!           IN THIS CASE THE WIND IS CONSTANT, VALUE GIVEN IN STEERING FILE
            CALL OV( 'X=C     ' , X=WINDX, C=FUAIR, DIM1=NPOIN)
            CALL OV( 'X=C     ' , X=WINDY, C=FVAIR, DIM1=NPOIN)
          ELSEIF(FILES(ATMFILEA)%NAME(1:1).NE.' ') THEN
!           WATER QUALITY
            IF(WATER_QUALITY) THEN
!             TIME VARYING WATER QUALITY OTHER THAN THERMIC IN 3D
              IF(ATMOSEXCH.EQ.0) THEN
                CALL SYNC_METEO(AT)
!             TIME VARYING WATER QUALITY WITH HEAT EXCHANGE WITH ATMOSPHERE
              ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
                CALL SYNC_METEO(AT)
                CALL OS('X=C     ', X=RAYAED2, C=RAY3%R(1))
              ENDIF
            ELSEIF(OPTWIND.EQ.2) THEN
              CALL SYNC_METEO(AT)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR THE REMAINING TIME STEPS
!
      IF(VENT.OR.WATER_QUALITY) THEN
!       WATER QUALITY
        IF(FILES(ATMFILEA)%NAME(1:1).NE.' ') THEN
          IF(WATER_QUALITY) THEN
!           TIME VARYING WATER QUALITY
            IF(ATMOSEXCH.EQ.0) THEN
              CALL SYNC_METEO(AT)
              IF(PRESENT(PLUIE)) THEN
                CALL OS('X=Y     ',X=PLUIE, Y=RAINFALL) ! MM/S
              ENDIF
!           TIME VARYING WATER QUALITY WITH HEAT EXCHANGE WITH ATMOSPHERE
            ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
              CALL SYNC_METEO(AT)
              CALL OS('X=C     ', X=RAYAED2, C=RAY3%R(1))
            ENDIF
          ELSEIF(VENT) THEN
!           WIND VARYING IN TIME CONSTANT IN SPACE
            IF(OPTWIND.EQ.2) THEN
              CALL SYNC_METEO(AT)
!           WIND VARYING IN TIME AND SPACE
            ELSEIF(OPTWIND.EQ.3) THEN
              WRITE(LU,*) 'THIS OPTION IS NOT IMPLEMENTED YET'
              WRITE(LU,*) 'SEE VALIDATION CASE WIND_TXY '
              WRITE(LU,*) 'LOCATED AT THE FOLDER EXAMPLES/TELEMAC2D'
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
!
!       WIND AND/OR OTHER METEO VARIABLES
!       VARYING IN SPACE AND TIME, FROM A BINARY FILE
!
        IF(FILES(ATMFILEB)%NAME(1:1).NE.' ') THEN
          IF(FILES(ATMFILEA)%NAME(1:1).NE.' ') THEN
            WRITE(LU,*) 'METEO: THE DATA FROM THE ASCII METEO'
            WRITE(LU,*) 'FILE WILL BE OVERWRITTEN BY THE'
            WRITE(LU,*) 'CORRESPONDING BINARY FILE DATA'
          ENDIF
          CALL METEO_FROM_BINARY_FILE(PATMOS,WINDX,WINDY,AT,
     &                                NPOIN,
     &                                ATMOS,ATMFILEB,FILES,LISTIN,
     &                                OPTWIND,WIND_SPD)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
