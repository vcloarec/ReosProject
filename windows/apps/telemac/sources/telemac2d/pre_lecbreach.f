!                   ************************
                    SUBROUTINE PRE_LECBREACH
!                   ************************
     &(IFIC)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    Read a breaches data file to identify the number of points per
!         breach
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC    [in]  File id
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC2D, ONLY : NBLS,LOGINB
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IFIC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NBREACH, OPTNBR, OPTERO, IDUM, N, IERR, M
      DOUBLE PRECISION DDUM
      CHARACTER(LEN=3) :: COMMENT = "#"
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) 'PRE READING THE BREACH FILE'
      CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
      IF(IERR.NE.0) GOTO 900
      READ(IFIC,*,ERR=999) NBREACH

      ALLOCATE(NBLS(NBREACH))

      DO N = 1, NBREACH
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=998) DDUM

        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=997) OPTNBR

        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900

        IF(OPTNBR.EQ.1) THEN
          READ(IFIC,*,ERR=996) DDUM
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF

        READ(IFIC,*,ERR=995) DDUM
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=810) OPTERO

        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
        READ(IFIC,*,ERR=994) DDUM

        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900

        IF(OPTNBR.EQ.3) THEN
          READ(IFIC,*,ERR=993) IDUM
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        ENDIF

        IF(OPTNBR.NE.1) THEN
          READ(IFIC,*,ERR=992) DDUM
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF

        IF(OPTERO.EQ.3) THEN
          READ(IFIC,*,ERR=801) DDUM, DDUM, DDUM
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ELSEIF(OPTERO.EQ.9) THEN
          READ(IFIC,*,ERR=802) DDUM, DDUM, DDUM
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ELSEIF(OPTERO.EQ.10) THEN
          READ(IFIC,*,ERR=803) DDUM
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF
!
!       INITIAL LENGTH OF BREACH
        IF(LOGINB) THEN
          READ(IFIC,*,ERR=804) DDUM
          CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
          IF(IERR.NE.0) GOTO 900
        ENDIF
!
!       NBL: NUMBER OF POINTS OF THE POLYLINE
        READ(IFIC,*,ERR=991) NBLS(N)
        CALL SKIP_COMMENT_LINE(IFIC, COMMENT, IERR)
        IF(IERR.NE.0) GOTO 900
!
!       ALLOCATION OF LOCAL VARIABLE TO READ BREACH DEFINITION
        DO M = 1, NBLS(N)
          READ(IFIC,*,ERR=990) DDUM, DDUM
        ENDDO
      ENDDO ! NBREACH

      GOTO 1000

804   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               INIBRW CANNOT BE READ'
      GOTO 2000
!
803   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               DF CANNOT BE READ'
      GOTO 2000
!
802   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               CRITICAL FLOW VELOCITY OR'
      WRITE(LU,*) '               EMPIRICAL FACTOR 1 OR'
      WRITE(LU,*) '               EMPIRICAL FACTOR 2 CANNOT BE READ'
      GOTO 2000
!
801   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               FINAL TIME STAGE 1 OR'
      WRITE(LU,*) '               VELOCITY STAGE 1 OR'
      WRITE(LU,*) '               VELOCITY STAGE 2 CANNOT BE READ'
      GOTO 2000
!
810   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               EROSION OPTION CANNOT BE READ'
      GOTO 2000
!
999   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE BREACHES DATA FILE'
      WRITE(LU,*) '               AT LINE 2'
      WRITE(LU,*) '               NUMBER OF BREACHES CANNOT BE READ'
      GOTO 2000
!
998   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               BREACH POLYGON WIDTH CANNOT BE READ'
      GOTO 2000
!
997   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               OPTION FOR BREACH INIT CANNOT BE READ'
      GOTO 2000
!
996   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               THE STARTING TIME CANNOT BE READ'
      GOTO 2000
!
995   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               THE OPENING DURATION CANNOT BE READ'
      GOTO 2000
!
994   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               THE FINAL LEVEL CANNOT BE READ'
      GOTO 2000
!
993   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               NUMBER OF TEST POINT CANNOT BE READ'
      GOTO 2000
!
992   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               THE STARTING LEVEL CANNOT BE READ'
      GOTO 2000
!
991   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               POINT NUMBER OF LINE CANNOT BE READ'
      GOTO 2000
!
990   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               FOR THE BREACH ',N
      WRITE(LU,*) '               THE COORDINATE OF POINT ',M
      WRITE(LU,*) '               CANNOT BE READ'
      GOTO 2000
!
900   CONTINUE
      WRITE(LU,*) 'PRE_LECBREACH: READ ERROR ON THE'
      WRITE(LU,*) '               BREACHES DATA FILE'
      WRITE(LU,*) '               UNEXPECTED END OF FILE'
!
2000  CONTINUE
!
      CALL PLANTE(1)
      STOP
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
