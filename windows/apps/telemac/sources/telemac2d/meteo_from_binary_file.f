!                   **********************************
                    SUBROUTINE METEO_FROM_BINARY_FILE
!                   **********************************
     &(PATMOS,WINDX,WINDY,AT,NPOIN,ATMOS,
     & ATMFILE,FILES,LISTIN,OPTWIND,WIND_SPD)
!
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    READS ATMOSPHERIC DATA FROM A BINARY FILE
!
!warning  IF AN ASCII ATMOSPHERIC DATA FILE IS ALSO GIVEN,
!         THE VARIABLES PRESENT IN THE BINARY FILE WILL
!         OVERWRITE THOSE OF THE ASCII FILE
!         THIS ROUTINE CAN BE ADAPTED BY THE USERS TO ADD MORE VARIABLES
!
!history  A. LEROY (LNHE)
!+        01/07/2016
!+        V7P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| LISTIN         |-->| IF YES, PRINTS INFORMATION
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PATMOS         |<--| ATMOSPHERIC PRESSURE
!| WINDX          |<--| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |<--| SECOND COMPONENT OF WIND VELOCITY
!| ATMFILEA       |-->| LOGICAL UNIT OF THE ASCII ATMOSPHERIC FILE
!| ATMFILEB       |-->| LOGICAL UNIT OF THE BINARY ATMOSPHERIC FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: ATMOSEXCH
      USE METEO_TELEMAC, ONLY: TAIR
      USE DECLARATIONS_KHIONE, ONLY: ATMOEXCH
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,ATMFILE
      LOGICAL, INTENT(IN)             :: ATMOS,LISTIN
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(*)
      DOUBLE PRECISION, INTENT(IN)    :: AT
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
      INTEGER, INTENT(IN)             :: OPTWIND
      DOUBLE PRECISION, INTENT(IN)    :: WIND_SPD(2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16) U_NAME,V_NAME,P_NAME,T_NAME
      LOGICAL READ_BIN_WU,READ_BIN_WV,READ_BIN_AIRT, READ_BIN_AIRP
!
!-----------------------------------------------------------------------
!
!     SET THE METEO VARIABLES' NAMES - TO BE MODIFIED BY THE USER
!     IF NECESSARY
!
      CALL METEO_SET_VAR_NAMES(U_NAME,V_NAME,P_NAME,T_NAME)
!
!-----------------------------------------------------------------------
!
!     SET THE WIND VELOCITY ALONG X AND Y
!
      IF (OPTWIND.EQ.1) THEN
!       SET THE WIND VELOCITY FROM THE CAS FILE
        CALL OV('X=C     ' , X=WINDX, C=WIND_SPD(1), DIM1=NPOIN)
        CALL OV('X=C     ' , X=WINDY, C=WIND_SPD(2), DIM1=NPOIN)
        IF(LISTIN) THEN
          WRITE(LU,*) "METEO: OPTION 1 FOR THE WIND, CONSTANT"
          WRITE(LU,*) "WIND VELOCITY SET IN THE CAS FILE"
        ENDIF
      ELSE IF(OPTWIND.EQ.2) THEN
!       SET THE WIND VELOCITY FROM THE ASCII FILE
        IF(LISTIN) THEN
          WRITE(LU,*) "METEO: OPTION 2 FOR THE WIND, THE WIND VELOCITY"
          WRITE(LU,*) "MUST BE GIVEN IN AN ASCII ATMOSPHERIC"
          WRITE(LU,*) "DATA FILE"
        ENDIF
      ELSEIF(OPTWIND.EQ.3) THEN
!       SET THE WIND VELOCITY FROM THE ATMOSPHERIC BINARY FILE
        CALL READ_BIN_2D
     &     (WINDX,U_NAME,AT,FILES(ATMFILE)%LU,
     &      FILES(ATMFILE)%FMT,NPOIN,READ_BIN_WU,TEL_OFFSET)
        CALL READ_BIN_2D
     &     (WINDY,V_NAME,AT,FILES(ATMFILE)%LU,
     &      FILES(ATMFILE)%FMT,NPOIN,READ_BIN_WV,TEL_OFFSET)
        IF(.NOT.READ_BIN_WU.OR..NOT.READ_BIN_WV) THEN
          IF(LISTIN) THEN
            WRITE(LU,*) "METEO: OPTION 3 FOR THE WIND,"
            WRITE(LU,*) "WIND VELOCITY U OR V IS MISSING"
            WRITE(LU,*) "IN THE BINARY ATMOSPHERIC DATA FILE"
            WRITE(LU,*) "THE VARIABLE NAME MAY NOT BE RECOGNIZED"
            WRITE(LU,*) "IT CAN BE SET IN THE SUBROUTINE"
            WRITE(LU,*) "METEO_SET_VAR_NAMES OF TELEMAC2D"
          ENDIF
        ENDIF
      ENDIF
      IF(ATMOS.OR.ATMOSEXCH.GT.0.OR.ATMOEXCH.GT.0) THEN
        ! GET THE ATMOSPHERIC PRESSURE FROM THE
        ! BINARY ATMOSPHERIC FILE
        CALL READ_BIN_2D
     &  (PATMOS,P_NAME,AT,FILES(ATMFILE)%LU,
     &   FILES(ATMFILE)%FMT,NPOIN,READ_BIN_AIRP,TEL_OFFSET)
        IF(.NOT.READ_BIN_AIRP) THEN
          IF(LISTIN) THEN
            WRITE(LU,*) "METEO: "
            WRITE(LU,*) "AIR PRESSURE IS MISSING"
            WRITE(LU,*) "IN THE BINARY ATMOSPHERIC DATA FILE"
            WRITE(LU,*) "THE VARIABLE NAME MAY NOT BE RECOGNIZED"
            WRITE(LU,*) "IT CAN BE SET IN THE SUBROUTINE"
            WRITE(LU,*) "METEO_SET_VAR_NAMES OF TELEMAC2D"
          ENDIF
        ENDIF
      ENDIF
      IF(ATMOSEXCH.GT.0.OR.ATMOEXCH.GT.0) THEN
        ! GET THE AIR TEMPERATURE FROM THE
        ! BINARY ATMOSPHERIC FILE
        CALL READ_BIN_2D
     &  (TAIR%R,T_NAME,AT,FILES(ATMFILE)%LU,
     &   FILES(ATMFILE)%FMT,NPOIN,READ_BIN_AIRT,TEL_OFFSET)
        IF(.NOT.READ_BIN_AIRT) THEN
          IF(LISTIN) THEN
            WRITE(LU,*) "METEO: "
            WRITE(LU,*) "AIR TEMPERATURE IS MISSING"
            WRITE(LU,*) "IN THE BINARY ATMOSPHERIC DATA FILE"
            WRITE(LU,*) "THE VARIABLE NAME MAY NOT BE RECOGNIZED"
            WRITE(LU,*) "IT CAN BE SET IN THE SUBROUTINE"
            WRITE(LU,*) "METEO_SET_VAR_NAMES OF TELEMAC2D"
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
