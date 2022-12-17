!                     ************************
                      SUBROUTINE UTIMP_DROGUES
!                     ************************
!
     &( LTL,ATL,NPOIN2,NPOIN3, XFLOT,YFLOT,ZFLOT,TAGFLO,CLSFLO,
     & NFLOT,NFLOT_MAX,FLOPRD,DEJA, T2DFLO,T2DBLO, MARDAT,MARTIM )
!
!***********************************************************************
! TELEMAC2D   V8P1
!***********************************************************************
!
!brief    Specific to drogues.
!+        Allow multiple type of file outputs for drogues
!
!history  S.E.BOURBAN (HRW)
!+        13/07/2018
!+        V8P0
!+   Initial implementation to support TecPlot and BlueKenue parcel
!+     files.
!
!history  M.S.TURNBULL (HRW)
!+        04/11/2019
!+        V8P2
!    Corrections
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| CLSFLO         |<->| CLASS OF FLOATS
!| DEJA           |<->| INDICATES FIRST CALL OF SUBROUTINE
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| FLOPRD         |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!| LUFLO          |-->| LOGICAL UNIT FOR THE ASCII OUTPUT FILE
!| LUBLO          |-->| LOGICAL UNIT FOR THE BINARY OUTPUT FILE
!| FMTBLO         |-->| FORMAT FOR THE OUTPUT FILE
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NUMBER OF POINTS
!| NFLOT          |-->| NUMBER OF FLOATS.
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS.
!| TAGFLO         |<->| TAGS OF FLOATS
!| XFLOT          |<->| ABSCISSAE OF FLOATS
!| YFLOT          |<->| ORDINATES OF FLOATS
!| ZFLOT          |<->| ELEVATIONS OF FLOATS
!| T2DFLO         |-->| ASCII DROGUES FILE
!| T2DBLO         |-->| BINARY DROGUES FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC, ONLY: IFRAME
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: ATL
      INTEGER         , INTENT(IN)    :: FLOPRD,LTL
      INTEGER         , INTENT(IN)    :: MARDAT(3),MARTIM(3)
      INTEGER         , INTENT(IN)    :: NFLOT,NFLOT_MAX,NPOIN2,NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: TAGFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: CLSFLO(NFLOT_MAX)
      TYPE(BIEF_FILE) , INTENT(IN)    :: T2DFLO,T2DBLO
      LOGICAL         , INTENT(INOUT) :: DEJA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           ID, LUFLO,LUBLO, IPROC,IFLOT, NFLOTG
      INTEGER           I1,I2
      INTEGER           NFLOT_START, NFLOTG_START
      DOUBLE PRECISION  V1
      LOGICAL           YESITDOES
      CHARACTER(LEN=32) TEXTE(3)
      REAL              R1,R2,R3
      INTEGER           CURDAT(3),CURTIM(3),MILSEC
      DOUBLE PRECISION  JAT
!
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!     INITIALISING HEADER OF THE OUTPUT FILE
!
      IF(.NOT.DEJA) THEN
!
!       WRITING / COMBINING IN ONLY ONE OUTPUT FILE
        IF(IPID.EQ.0) THEN
!
!         HEADER OF TECPLOT FILE
          IF( T2DBLO%FMT .EQ.'TECPLOT ' .AND. T2DFLO%NAME.NE.'' ) THEN
            LUFLO = T2DFLO%LU
            TEXTE(1)='X                               '
            TEXTE(2)='Y                               '
            TEXTE(3)='ELEVATION Z     M               '
            WRITE(LUFLO,100) 'TITLE = "DROGUES FILE"'
            IF( NPOIN2.EQ.NPOIN3 ) THEN
              WRITE(LUFLO,100) 'VARIABLES = "LABELS","'//
     &          TEXTE(1)//'","'//TEXTE(2)//
     &          '","COLOUR"'
            ELSE
              WRITE(LUFLO,100) 'VARIABLES = "LABELS","'//
     &          TEXTE(1)//'","'//TEXTE(2)//'","'//TEXTE(3)//
     &          '","COLOUR"'
            ENDIF
          ENDIF
!
!         HEADER OF BLUE KENUE PARCEL FILE IN ASCII FORM
          IF( T2DBLO%FMT .EQ.'BKBINPCL' .AND. T2DBLO%NAME.NE.'' ) THEN
            IFRAME = 0
            LUBLO = T2DBLO%LU
            CLOSE(LUBLO)
            OPEN( LUBLO, FILE=TRIM(T2DBLO%TELNAME),FORM='FORMATTED',
     &        ACCESS='STREAM', ACTION='WRITE')
            WRITE(LUBLO,100) ':FileType pcl  BINARY  EnSim 1.0'
            WRITE(LUBLO,100) ':WrittenBy TELEMAC'
            WRITE(LUBLO,100) ':CreationTime 2018/08/21 14:16:28.000'
            WRITE(LUBLO,100) ':Name ' // 'DROGUES_FILE'
            IF( NPOIN2.EQ.NPOIN3 ) THEN
              WRITE(LUBLO,100) ':AttributeCount 1 TAG'
              WRITE(LUBLO,100) ':AttributeCount 2 CLASS'
              WRITE(LUBLO,100) ':AttributeUnits 1 #'
              WRITE(LUBLO,100) ':AttributeUnits 2 #'
            ELSE
              WRITE(LUBLO,100) ':AttributeCount 1 Z'
              WRITE(LUBLO,100) ':AttributeCount 2 TAG'
              WRITE(LUBLO,100) ':AttributeCount 3 CLASS'
              WRITE(LUBLO,100) ':AttributeUnits 1 M'
              WRITE(LUBLO,100) ':AttributeUnits 2 #'
              WRITE(LUBLO,100) ':AttributeUnits 3 #'
            ENDIF
            WRITE(LUBLO,100) ':EndHeader'
            CLOSE(LUBLO)
!
!         CORE OF BLUE KENUE PARCEL FILE IN BINARY LITTLE ENDIAN FORM
            OPEN( LUBLO, FILE=TRIM(T2DBLO%TELNAME),FORM='UNFORMATTED',
     &            ACTION='WRITE',CONVERT='LITTLE_ENDIAN',
     &            ACCESS='STREAM', STATUS='OLD',POSITION='APPEND')
!
          ENDIF
!
        ENDIF
!
        DEJA = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
      NFLOT_START = NFLOT_MAX
      NFLOTG_START = NFLOT_START
!
      IF( (LTL/FLOPRD)*FLOPRD.NE.LTL ) RETURN
!
      CURDAT = MARDAT
      CURTIM = MARTIM
      MILSEC = INT( 1000.D0*( ATL - INT(ATL) ) )
      JAT = JULTIM(CURDAT(1),CURDAT(2),CURDAT(3),
     &  CURTIM(1),CURTIM(2),CURTIM(3),(1.D0*INT(ATL)) )
      CALL GREGTIM(JAT,CURDAT(1),CURDAT(2),CURDAT(3),
     &  CURTIM(1),CURTIM(2),CURTIM(3))
!
!-----------------------------------------------------------------------
!
!     TECPLOT AND BLUE KENUE PARCEL FILES
!
      IF( NCSIZE.GT.1 ) THEN
!
!       WAITING ALL PROCESSORS (SO THAT NFLOT IS UPDATED FOR ALL
!                               BEFORE CALLING P_SUM)
!
!        CALL P_SYNC
!
!       1) PARALLEL WRITE UP
!
        NFLOTG = NFLOT
        NFLOTG = P_SUM(NFLOT)
        IF( NFLOTG.GT.0 ) THEN
!
!         ONE TIME STEP OF THE TECPLOT FILE
          IF( T2DBLO%FMT .EQ.'TECPLOT ' .AND. T2DFLO%NAME.NE.'' ) THEN
            LUFLO = T2DFLO%LU
!
!       1) EVERY PROCESSOR WRITES ITS OWN DATA IN A FILE
!
            CALL GET_FREE_ID(ID)
            IF(NFLOT.GT.0) THEN
              OPEN(ID,FILE=EXTENS(NCSIZE,IPID+1),FORM='UNFORMATTED',
     &          STATUS='NEW',ACTION='WRITE',ACCESS='STREAM')
              IF( NPOIN2.EQ.NPOIN3 ) THEN
                DO IFLOT = 1,NFLOT
                  WRITE(ID) TAGFLO(IFLOT),REAL(XFLOT(IFLOT)),
     &              REAL(YFLOT(IFLOT)),1
                ENDDO
              ELSE
                DO IFLOT = 1,NFLOT
                  WRITE(ID) TAGFLO(IFLOT),REAL(XFLOT(IFLOT)),
     &              REAL(YFLOT(IFLOT)),REAL(ZFLOT(IFLOT)),1
                ENDDO
              ENDIF
              CLOSE(ID)
            ENDIF
!
!       2) WAITING ALL PROCESSORS
!
            CALL P_SYNC
!
!       3) PROCESSOR 0 READS ALL EXISTING FILES AND MERGES
!          THEM IN THE FINAL FILE
!
            IF(IPID.EQ.0) THEN
!
              WRITE(LUFLO,200) 'ZONE DATAPACKING=POINT, T="G_',ATL,
     &          ' SECONDS"',', I=',NFLOTG,', SOLUTIONTIME=',ATL
              DO IPROC = 1,NCSIZE
                INQUIRE(FILE=EXTENS(NCSIZE,IPROC),EXIST=YESITDOES)
                IF( YESITDOES ) THEN
                  OPEN(ID,FILE=EXTENS(NCSIZE,IPROC),FORM='UNFORMATTED',
     &                 STATUS='OLD',ACTION='READ',ACCESS='STREAM')
                  IF( NPOIN2.EQ.NPOIN3 ) THEN
 22                 CONTINUE
                    READ(ID,ERR=24,END=24) I1,R1,R2,I2
                    WRITE(LUFLO,300) I1,R1,R2,I2
                    GO TO 22
                  ELSE
 23                 CONTINUE
                    READ(ID,ERR=24,END=24) I1,R1,R2,R3,I2
                    WRITE(LUFLO,301) I1,R1,R2,R3,I2
                    GO TO 23
                  ENDIF
 24               CONTINUE
                  CLOSE(ID,STATUS='DELETE')
                ENDIF
              ENDDO
            ENDIF
!
          ENDIF
!
!         HEADER OF BLUE KENUE PARCEL FILE IN ASCII FORM
          IF( T2DBLO%FMT .EQ.'BKBINPCL' .AND. T2DBLO%NAME.NE.'' ) THEN
            LUBLO = T2DBLO%LU
!
!       1) EVERY PROCESSOR WRITES ITS OWN DATA IN A FILE
!
            CALL GET_FREE_ID(ID)
            IF(NFLOT.GT.0) THEN
              OPEN(ID,FILE=EXTENS(NCSIZE,IPID+1),FORM='UNFORMATTED',
     &          STATUS='NEW',ACTION='WRITE',ACCESS='STREAM')
              WRITE(ID) NFLOT
              WRITE(ID) ( TAGFLO(IFLOT), IFLOT = 1,NFLOT )
              WRITE(ID) ( CLSFLO(IFLOT), IFLOT = 1,NFLOT )
              WRITE(ID) ( XFLOT(IFLOT), IFLOT = 1,NFLOT )
              WRITE(ID) ( YFLOT(IFLOT), IFLOT = 1,NFLOT )
              IF( NPOIN2.NE.NPOIN3 ) THEN
                WRITE(ID) ( ZFLOT(IFLOT), IFLOT = 1,NFLOT )
              ENDIF
              CLOSE(ID)
            ENDIF
!
!       2) WAITING ALL PROCESSORS
!
            CALL P_SYNC
!
!       3) PROCESSOR 0 READS ALL EXISTING FILES AND MERGES
!          THEM IN ORDER OF THEIR TAGS TO THE FINAL FILE
!
!      /!\ IT IS IMPORTANT TO NOTE THAT XFLOT,YFLOT, ETC.
!          FOR IPID=0 REMAIN UNCHANGED, EVEN IF VALUES
!          FROM OTHER PROCESSORS GET COPIED OVER.
!
            IF(IPID.EQ.0) THEN
!
              IFRAME = IFRAME + 1
              WRITE(LUBLO) IFRAME,IFRAME,
     &          CURDAT(1),CURDAT(2),CURDAT(3),
     &          CURTIM(1),CURTIM(2),CURTIM(3),MILSEC
              WRITE(LUBLO) NFLOTG_START
              I2 = 0
              DO IPROC = 1,NCSIZE
                INQUIRE(FILE=EXTENS(NCSIZE,IPROC),EXIST=YESITDOES)
                IF( YESITDOES ) THEN
                  OPEN(ID,FILE=EXTENS(NCSIZE,IPROC),FORM='UNFORMATTED',
     &                 STATUS='OLD',ACTION='READ',ACCESS='STREAM')
                  READ(ID) I1
                  READ(ID) ( TAGFLO(I2+IFLOT), IFLOT = 1,I1 )
                  READ(ID) ( CLSFLO(I2+IFLOT), IFLOT = 1,I1 )
                  READ(ID) ( XFLOT(I2+IFLOT), IFLOT = 1,I1 )
                  READ(ID) ( YFLOT(I2+IFLOT), IFLOT = 1,I1 )
                  IF( NPOIN2.NE.NPOIN3 ) THEN
                    READ(ID) ( ZFLOT(I2+IFLOT), IFLOT = 1,I1 )
                  ENDIF
                  CLOSE(ID,STATUS='DELETE')
                  I2 = I2 + I1
                ENDIF
              ENDDO
!
              DO IFLOT = NFLOTG+1,NFLOTG_START
                V1 = -1.D0
                XFLOT(IFLOT) = V1
                YFLOT(IFLOT) = V1
                TAGFLO(IFLOT) = IFLOT
                CLSFLO(IFLOT) = 1
              END DO
!
              WRITE(LUBLO)
     &          ( REAL(XFLOT(IFLOT)), IFLOT = 1,NFLOTG_START )
              WRITE(LUBLO)
     &          ( REAL(YFLOT(IFLOT)), IFLOT = 1,NFLOTG_START )
              IF( NPOIN2.NE.NPOIN3 ) THEN
                WRITE(LUBLO)
     &          ( REAL(ZFLOT(IFLOT)), IFLOT = 1,NFLOTG_START )
              ENDIF
              WRITE(LUBLO)
     &          ( REAL(TAGFLO(IFLOT)), IFLOT = 1,NFLOTG_START )
              WRITE(LUBLO)
     &          ( REAL(CLSFLO(IFLOT)), IFLOT = 1,NFLOTG_START )
              WRITE(LUBLO) NFLOTG_START
!
            ENDIF
!
          ENDIF
!
        ENDIF
!
      ELSE
!
!       SCALAR VERSION
!
        IF( NFLOT.GT.0 ) THEN
!
!         ONE TIME STEP OF THE TECPLOT FILE
          IF( T2DBLO%FMT .EQ.'TECPLOT ' .AND. T2DFLO%NAME.NE.'' ) THEN
            LUFLO = T2DFLO%LU
!
            WRITE(LUFLO,200) 'ZONE DATAPACKING=POINT, T="G_',ATL,
     &        ' SECONDS"',', I=',NFLOT,', SOLUTIONTIME=',ATL
            IF( NPOIN2.EQ.NPOIN3 ) THEN
              DO IFLOT = 1,NFLOT
                WRITE(LUFLO,300) TAGFLO(IFLOT),
     &            XFLOT(IFLOT),YFLOT(IFLOT),1
              ENDDO
            ELSE
              DO IFLOT = 1,NFLOT
                WRITE(LUFLO,301) TAGFLO(IFLOT),
     &            XFLOT(IFLOT),YFLOT(IFLOT),ZFLOT(IFLOT),1
              ENDDO
            ENDIF
!
          ENDIF
!
!         ONE TIME STEP OF THE BLUE KENUE PARCEL FILE IN ASCII FORM
          IF( T2DBLO%FMT .EQ.'BKBINPCL' .AND. T2DBLO%NAME.NE.'' ) THEN
            LUBLO = T2DBLO%LU
!
            IFRAME = IFRAME + 1
            WRITE(LUBLO) IFRAME,IFRAME,
     &          CURDAT(1),CURDAT(2),CURDAT(3),
     &          CURTIM(1),CURTIM(2),CURTIM(3),MILSEC
            WRITE(LUBLO) NFLOT_START
!
            DO IFLOT = NFLOT+1,NFLOT_START
              V1 = -1.D0
              XFLOT(IFLOT) = V1
              YFLOT(IFLOT) = V1
            END DO
!
            WRITE(LUBLO)
     &        ( REAL(XFLOT(IFLOT)), IFLOT = 1,NFLOT_START )
            WRITE(LUBLO)
     &        ( REAL(YFLOT(IFLOT)), IFLOT = 1,NFLOT_START )
            IF( NPOIN2.NE.NPOIN3 ) THEN
              WRITE(LUBLO)
     &        ( REAL(ZFLOT(IFLOT)), IFLOT = 1,NFLOT_START )
            ENDIF
            WRITE(LUBLO)
     &        ( REAL(TAGFLO(IFLOT)), IFLOT = 1,NFLOT_START )
            WRITE(LUBLO)
     &        ( REAL(CLSFLO(IFLOT)), IFLOT = 1,NFLOT_START )
            WRITE(LUBLO) NFLOT_START
!
          ENDIF
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
 100  FORMAT(A)
 200  FORMAT(A,F12.4,A,A,I4,A,F12.4)
 300  FORMAT(I6,',',F16.8,',',F16.8,',',I2)
 301  FORMAT(I6,',',F16.8,',',F16.8,',',F16.8,',',I2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP_DROGUES
