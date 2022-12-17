!                   ************************
                    SUBROUTINE FIND_VARIABLE
!                   ************************
!
     &(FFORMAT,FID,VAR_NAME,RES,N,IERR,TIME,EPS_TIME,RECORD,
     & TIME_RECORD, OFFSET)
!
!***********************************************************************
! HERMES   V7P2
!***********************************************************************
!
!brief    Returns the value for each point of a given variable,
!+        for a given time or a given record.
!+        This subroutine can interpolate from two time steps
!+        if the time does not exist in the file with an accuracy of EPS.
!+
!+        If no optional parameter is given, the last record is chosen.
!+
!+        Accepted formats so far: 'SERAFIN ', 'SERAFIND', 'MED     '
!
!history  Y. AUDOUIN (EDF LAB, LNHE)
!+        28/07/2015
!+        V7P1
!+   First version.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2016
!+        V7P2
!+   Just a message corrected.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EPS_TIME       |-->| EPSILON TO DETERMINE IF TWO TIMES ARE EQUAL
!|                |   | IF NOT PRESENT, CONSIDERED TO BE 0.D0
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE LOGICAL UNIT
!| RECORD         |-->| RECORD OF RESULTS IN THE FILE THAT SHOULD BE
!|                |   | TAKEN. RECORD = -1 TAKES THE LAST RECORD.
!| RES            |<->| VALUE FOR EACH POINT AT TIME STEP RECORD
!|                |   | FOR THE VARIABLE VAR_NAME
!| TIME           |-->| TIME TO INTERPOLATE FROM THE FILE
!| TIME_RECORD    |<--| TIME OF THE RECORD THAT HAS BEEN CHOSEN
!| VAR_NAME       |-->| VARIABLE FOR WHICH WE NEED THE VALUE
!| N              |-->| SIZE OF EXPECTED RES_VALUE
!| OFFSET         |-->| OFFSET FOR TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY : DATE_MJD2SEC
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8),  INTENT(IN)    :: FFORMAT
      INTEGER,           INTENT(IN)    :: FID
      CHARACTER(LEN=16), INTENT(IN)    :: VAR_NAME
      INTEGER,           INTENT(IN)    :: N
      DOUBLE PRECISION,  INTENT(INOUT) :: RES(N)
      INTEGER,           INTENT(INOUT) :: IERR
      DOUBLE PRECISION,  INTENT(IN)   , OPTIONAL :: EPS_TIME,TIME
      INTEGER,           INTENT(IN)   , OPTIONAL :: RECORD
      DOUBLE PRECISION,  INTENT(INOUT), OPTIONAL :: TIME_RECORD
      DOUBLE PRECISION,  INTENT(IN), OPTIONAL :: OFFSET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER RECORD1,RECORD2
      DOUBLE PRECISION :: TIME1,TIME2,COEF,EPS
      DOUBLE PRECISION,ALLOCATABLE :: RES2(:)
      INTEGER NTIMESTEP,I
      INTEGER :: RRECORD
      CHARACTER(LEN=16), ALLOCATABLE :: VAR_LIST(:)
      CHARACTER(LEN=16), ALLOCATABLE :: UNIT_LIST(:)
      INTEGER :: NVAR
      DOUBLE PRECISION :: TEL_OFFSET1
      INTEGER, DIMENSION(6) :: DATE
      DOUBLE PRECISION :: OFFSET_FILE
      DOUBLE PRECISION :: TIME_FULL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(PRESENT(OFFSET)) THEN
        TEL_OFFSET1 = OFFSET
      ELSE
        TEL_OFFSET1 = 0.D0
      ENDIF

      IF(PRESENT(TIME)) THEN
!
!       TIME ASKED
!
!       EPSILON TO BE CONSIDERED
        IF(PRESENT(EPS_TIME))THEN
          EPS=EPS_TIME
        ELSE
          EPS=0.D0
        ENDIF
!
!       GET THE NUMBER OF TIME STEPS
!
        CALL GET_DATA_NTIMESTEP(FFORMAT,FID,NTIMESTEP,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
!
!       IDENTIFY THE TWO RECORDS IN BETWEEN OUR TIME
!
        RECORD1 = 0
        RECORD2 = NTIMESTEP-1
        CALL GET_DATA_TIME(FFORMAT,FID,RECORD1,TIME1,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
        CALL GET_DATA_TIME(FFORMAT,FID,RECORD2,TIME2,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC2')

        IF (TEL_OFFSET1.GE.1.D-16) THEN
          CALL GET_MESH_DATE(FFORMAT, FID, DATE, IERR)
          IF (ANY(DATE.NE.0)) THEN
            OFFSET_FILE = DATE_MJD2SEC(DATE(1:3),DATE(4:6))
            TIME_FULL = TIME + TEL_OFFSET1
          ELSE
            TIME_FULL = TIME
            OFFSET_FILE = 0.D0
          ENDIF
        ELSE
          TIME_FULL = TIME
          OFFSET_FILE = 0.D0
        ENDIF

        TIME1 = TIME1+OFFSET_FILE
        TIME2 = TIME2+OFFSET_FILE

!
!       Quick check to see if the time is indeed within the first
!       and last time step of the file
!
        IF((TIME1.GT.TIME_FULL.AND.ABS(TIME1-TIME_FULL).GT.EPS)
     & .OR.(TIME2.LT.TIME_FULL.AND.ABS(TIME2-TIME_FULL).GT.EPS)) THEN
          WRITE(LU,*) 'TIME: ',TIME_FULL,
     &                'IS NOT WITHIN THE RANGE',
     &                ' OF THE FILE I.E.[',
     &                TIME1,'-',TIME2,']'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO WHILE ((TIME1.LT.TIME_FULL).AND.
     &       (ABS(TIME1-TIME_FULL).GT.EPS) )
          RECORD1 = RECORD1 + 1
          CALL GET_DATA_TIME(FFORMAT,FID,RECORD1,TIME1,IERR)
          TIME1 = TIME1+OFFSET_FILE
          CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
        ENDDO
!       If the time is on the file return the value for that record
!       otherwise we interpolate between the two time step
        IF(ABS(TIME1-TIME_FULL).LE.EPS) THEN
          CALL GET_DATA_VALUE(FFORMAT,FID,RECORD1,VAR_NAME,RES,N,IERR)
          CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_VALUE:REC1')
        ELSE
!         Get the time and values for the record before and after time
          RECORD2 = RECORD1
          RECORD1 = RECORD1 -1
          CALL GET_DATA_TIME(FFORMAT,FID,RECORD1,TIME1,IERR)
          CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
          CALL GET_DATA_TIME(FFORMAT,FID,RECORD2,TIME2,IERR)
          TIME1 = TIME1+OFFSET_FILE
          TIME2 = TIME2+OFFSET_FILE
          CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC2')
!         Get the value for each time step
          CALL GET_DATA_VALUE(FFORMAT,FID,RECORD1,VAR_NAME,RES,N,IERR)
          CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_VALUE:REC1')
          ALLOCATE(RES2(N),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'FIND_VARIABLE:RES2')
          CALL GET_DATA_VALUE(FFORMAT,FID,RECORD2,VAR_NAME,RES2,N,IERR)
          CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_VALUE:REC1')
!
!         Interpolates in time
          COEF=(TIME_FULL-TIME1)/(TIME2-TIME1)
!
          DO I=1,N
            RES(I)=(RES2(I)-RES(I))*COEF+RES(I)
          ENDDO
          DEALLOCATE(RES2)
        ENDIF
!
      ELSE
        CALL GET_DATA_NTIMESTEP(FFORMAT,FID,NTIMESTEP,IERR)
        CALL CHECK_CALL(IERR, 'READ_DATA:GET_DATA_NTIMESTEP')
        ! Check if record was given if not 0 is taken
        IF (PRESENT(RECORD)) THEN
          ! Taking last time step
          IF(RECORD.EQ.-1) THEN
            RRECORD = NTIMESTEP - 1
          ELSE
            RRECORD = RECORD
          ENDIF
        ELSE
          RRECORD = 0
        ENDIF
        ! WE CHECK IF THE RECORD IS IN THE FILE BY GETTING THE NUMBER OF
        ! TIMESTEPS AND CHECKING THAT THE RECORD IS INDEED
        ! BETWEEN 0 AND NTIMESTEP - 1
        IF((RRECORD.LT.0).OR.(RRECORD.GE.NTIMESTEP)) THEN
          IERR = HERMES_RECORD_UNKNOWN_ERR
          RETURN
        ENDIF
        ! CHECKING THAT THE VARIABLE IS INDEED IN THE FILE
        !  BY LOOPING ON THE LIST OF VARIABLE
        CALL GET_DATA_NVAR(FFORMAT,FID,NVAR,IERR)
        CALL CHECK_CALL(IERR, 'FIND_VARIABLE:GET_DATA_NVAR')
        ALLOCATE(VAR_LIST(NVAR),STAT=IERR)
        ALLOCATE(UNIT_LIST(NVAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'VAR_LIST')
        CALL GET_DATA_VAR_LIST(FFORMAT,FID,NVAR,VAR_LIST,
     &                         UNIT_LIST,IERR)
        CALL CHECK_CALL(IERR, 'FIND_VARIABLE:GET_DATA_VAR_LIST')
        I=1
        DO WHILE(I.LE.NVAR)
          IF (VAR_LIST(I).EQ.VAR_NAME) EXIT
          I = I +1
        ENDDO
        IF(I.EQ.NVAR+1) THEN
          IERR = HERMES_VAR_UNKNOWN_ERR
          RETURN
        ENDIF
        DEALLOCATE(VAR_LIST)
        DEALLOCATE(UNIT_LIST)

        ! Getting the data value by record number
!
        ! If asked for giving the time value associated with the record
        IF(PRESENT(TIME_RECORD)) THEN
          CALL GET_DATA_TIME(FFORMAT,FID,RRECORD,TIME_RECORD,IERR)
          IF(IERR.NE.0) THEN
            WRITE(LU,*) 'ERROR WHILE READING TIME VALUE ',
     &                  'FOR RECORD:',RRECORD
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        CALL GET_DATA_VALUE(FFORMAT,FID,RRECORD,VAR_NAME,RES,
     &                      N,IERR)
        IF(IERR.NE.0) THEN
          WRITE(LU,*) 'ERROR WHILE READING VALUE ',
     &                'FOR VARIABLE:',VAR_NAME,
     &                'FOR RECORD:',RRECORD
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

