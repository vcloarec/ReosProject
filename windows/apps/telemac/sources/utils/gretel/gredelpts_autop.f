!                   ***********************
                    PROGRAM GREDELPTS_AUTOP
!                   ***********************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION (COUPLING
!+                WITH DELWAQ) TO WRITE A SINGLE FILE IN DELWAQ FORMAT.
!
!history  JAJ
!+        2001/2
!+
!+   SLIGHTLY CHANGED TO DEAL WITH:
!
!history  HW, BAW-HAMBURG
!+        20/02/2003
!+
!+   IMPROVED READING OF DATASETS
!
!history  JAJ
!+        14/03/2003
!+
!+   ADDED EXIT CODES
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      CHARACTER(LEN=30) GEO
!
      INTEGER IPID,ERR,FU
      INTEGER NELEM,ECKEN,NDUM,I,J,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPOIN2LOC,NPLANLOC
      INTEGER NPROC,NRESU,NPOINMAX
      INTEGER I_S, I_SP, I_LEN
      INTEGER IT
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,VERIF
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOLG
!
!
      REAL   , DIMENSION(:)  , ALLOCATABLE :: GLOBAL_VALUE
      REAL   , DIMENSION(:)  , ALLOCATABLE :: LOCAL_VALUE
!
      LOGICAL IS,ENDE
!
      CHARACTER(LEN=30) RES
      CHARACTER(LEN=50) RESPAR
      CHARACTER(LEN=11) EXTENS
      EXTERNAL    EXTENS
      INTRINSIC MAXVAL
!
!-------------------------------------------------------------------------
!
      LI=5
      LU=6
      LNG=2
!HW
!JAJ INTRODUCE YOURSELF WITH THE RELEASE DATE
!
      WRITE(LU,*) 'I AM GREDELPTS, COUSIN OF GRETEL FROM BAW HAMBURG'
      WRITE(LU,*)
!
! READS FILENAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
!
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
!      REWIND(LI)
      READ(LI,*) GEO
      WRITE(LU,*) GEO
!
      WRITE (LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
      WRITE(LU,*) RES
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE(LU,*) NPROC
!
      INQUIRE (FILE=GEO,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', GEO
        CALL PLANTE(1)
        STOP
      END IF
!
      I_S  = LEN (RES)
      I_SP = I_S + 1
      DO I=1,I_S
        IF(RES(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
!
!     GEOMETRY FILE, READ UNTIL 10 PARAMETERS:
!
      OPEN(2,FILE=GEO,FORM='UNFORMATTED',STATUS='OLD',ERR=990)
      READ(2,ERR=990)
      READ(2,ERR=990) NBV1,NBV2
      DO I=1,NBV1+NBV2
        READ(2,ERR=990)
      ENDDO ! I
      GO TO 992
990   WRITE(LU,*) 'ERROR WHEN OPENING OR READING FILE: ',GEO
      CALL PLANTE(1)
      STOP
992   CONTINUE
!     READS THE 10 PARAMETERS AND THE DATE
      READ(2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(2) (PARAM(I),I=1,6)
!
!     RESULTS FILE:
!
      OPEN(3,FILE=RES,FORM='UNFORMATTED',ERR=991)
      GO TO 993
991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RES
      CALL PLANTE(1)
      STOP
993   CONTINUE
!
!     1) READS THE BEGINNING OF THE FIRST RESULTS FILE
!
!CC      RESPAR=RES // EXTENS(2**IDIMS-1,0)
!
      RESPAR=RES(1:I_LEN) // EXTENS(NPROC-1,0)
!
      INQUIRE (FILE=RESPAR,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RESPAR
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL PLANTE(1)
        STOP
      END IF
!
      OPEN(4,FILE=RESPAR,FORM='UNFORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(1)
      STOP
995   CONTINUE
!
      READ(4) NPOIN2
      READ(4) NPLAN
      IF(NPLAN.EQ.1) NPLAN = 0
!
      CLOSE(4)
!
!  5 : 4 PARAMETERS
!
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',  NELEM
      WRITE(LU,*) 'NPOIN2=', NPOIN2
      WRITE(LU,*) 'ECKEN=',  ECKEN
      WRITE(LU,*) 'NDUM=',   NDUM
!
!  DYNAMICALLY ALLOCATES THE ARRAYS
!
      ALLOCATE(NPOIN(NPROC),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NPOIN')
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(VERIF(NPOIN2)    ,STAT=ERR)
      ELSE
        ALLOCATE(VERIF(NPOIN2*NPLAN)    ,STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'VERIF')
!  GLOBAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(GLOBAL_VALUE(NPOIN2)       ,STAT=ERR)
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN) ,STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'GLOBAL_VALUE')
!
!  END OF ALLOCATION ...
!
!------------------------------------------------------------------------------
!
! OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
!
      DO IPID = 0,NPROC-1
        FU = IPID +10
        RESPAR=RES(1:I_LEN) // EXTENS(NPROC-1,IPID)
        OPEN (FU,FILE=RESPAR,FORM='UNFORMATTED',ERR=998)
        GO TO 999
998     WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR,
     &                     ' USING FILE UNIT: ', FU
        CALL PLANTE(1)
        STOP
999     REWIND(FU)
        READ(FU) NPOIN(IPID+1)
        READ(FU) NPLANLOC
      END DO
!
      NPOINMAX = MAXVAL(NPOIN)
! ARRAY FOR LOCAL-GLOBAL NUMBERS, 2D-FIELD
      IF(NPLAN.EQ.0) THEN
        ALLOCATE (KNOLG(NPOINMAX,NPROC),STAT=ERR)
      ELSE
        ALLOCATE (KNOLG(NPOINMAX/NPLAN,NPROC),STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'KNOLG')
!  LOCAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      ALLOCATE(LOCAL_VALUE(NPOINMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LOCAL_VALUE')
!
! READS KNOLG(NPOIN,NPROC)
!
      DO IPID = 0,NPROC-1
        FU = IPID +10
        IF(NPLAN.EQ.0) THEN
          READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1))
        ELSE
          READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1)/NPLAN)
        ENDIF
      END DO
!
! READS DATASETS
!
      NRESU = 0
!
2000  NRESU = NRESU + 1
!
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          VERIF(I)=0
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          VERIF(I)=0
        ENDDO
      ENDIF
!
      WRITE(LU,*)'TRY TO READ DATASET NO.',NRESU
!
      DO IPID = 0,NPROC-1
        FU = IPID +10
        CALL GREDELPTS_READ_DATASET
     &  (LOCAL_VALUE,NPOINMAX,NPOIN(IPID+1),IT,FU,ENDE)
        IF (ENDE) GOTO 3000
! STORES EACH DATASET
        IF(NPLAN.EQ.0) THEN
          DO I=1,NPOIN(IPID+1)
            GLOBAL_VALUE(KNOLG(I,IPID+1)) = LOCAL_VALUE(I)
            VERIF(KNOLG(I,IPID+1))   = 1
          END DO
        ELSE
          NPOIN2LOC = NPOIN(IPID+1)/NPLAN
          DO I=1,NPOIN2LOC
          DO J=1,NPLAN
          GLOBAL_VALUE(KNOLG(I,IPID+1) + NPOIN2   *(J-1)) =
     &     LOCAL_VALUE(      I         + NPOIN2LOC*(J-1))
          VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1)) = 1
          END DO
          END DO
        ENDIF
      END DO
! WRITES GLOBAL DATASET
      WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',IT
!
      IF(NPLAN.EQ.0) THEN
        WRITE(3) IT, (GLOBAL_VALUE(I),I=1,NPOIN2)
      ELSE
        WRITE(3) IT, (GLOBAL_VALUE(I),I=1,NPOIN2*NPLAN)
      ENDIF
! CHECKS ...
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR NRESU=',NRESU
          ENDIF
        END DO
      ELSE
        DO I=1,NPOIN2*NPLAN
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR NRESU=',NRESU
          ENDIF
        END DO
      ENDIF
!
      GO TO 2000
!
3000  WRITE(LU,*) 'END OF PROGRAM, ',NRESU-1,' DATASETS FOUND'
!
      CLOSE(2)
      CLOSE(3)
!
      DO IPID = 0,NPROC-1
        FU = IPID +10
        CLOSE (FU)
      END DO
!
      STOP 0
      END PROGRAM GREDELPTS_AUTOP
