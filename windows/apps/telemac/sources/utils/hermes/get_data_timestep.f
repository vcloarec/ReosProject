!                   ****************************
                    SUBROUTINE GET_DATA_TIMESTEP
!                   ****************************
!
     &(FFORMAT,FID,RECORD,TIME,IERR)
!
!***********************************************************************
! HERMES   V7P3
!***********************************************************************
!
!brief    Returns the time step of a given time value
!
!history  N.DURAND (HRW)
!+        September 2017
!+        V7P3
!+   First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| RECORD         |-->| NUMBER OF THE TIME STEP
!| TIME           |<->| TIME IN SECOND OF THE TIME STEP
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES, TMP => GET_DATA_TIMESTEP
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID
      INTEGER,          INTENT(OUT) :: RECORD
      DOUBLE PRECISION, INTENT(IN)  :: TIME
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: NTIMESTEP
      DOUBLE PRECISION :: TIME1,FIRSTTIME,LASTTIME,EPS
!
!-----------------------------------------------------------------------
!
      RECORD = 0
      EPS=0.D0
      CALL GET_DATA_TIME(FFORMAT,FID,0,FIRSTTIME,IERR)
      IF(IERR.NE.0) RETURN
      CALL GET_DATA_NTIMESTEP(FFORMAT,FID,NTIMESTEP,IERR)
      IF(IERR.NE.0) RETURN
      CALL GET_DATA_TIME(FFORMAT,FID,NTIMESTEP-1,LASTTIME,IERR)
      IF(IERR.NE.0) RETURN
      !
      ! FIRST CHECK IF THE TIME VALUE IS IN THE FILE
      IF((TIME.GT.LASTTIME.OR.TIME.LT.FIRSTTIME).AND.
     &   NTIMESTEP.GT.0) THEN
        IERR = HERMES_RECORD_UNKNOWN_ERR
        WRITE(ERROR_MESSAGE, *) 'TIME FOR GET_DATA_TIMESTEP: ', TIME,
     &      ' IS NOT IN THE FILE [',FIRSTTIME,', ',LASTTIME,']'
        RETURN
      !
      ELSE
        RECORD = 0
        TIME1 = FIRSTTIME
        DO WHILE ((TIME1.LT.TIME).AND.(ABS(TIME1-TIME).GT.EPS) )
          RECORD = RECORD + 1
          CALL GET_DATA_TIME(FFORMAT,FID,RECORD,TIME1,IERR)
          IF(IERR.NE.0) RETURN
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END


