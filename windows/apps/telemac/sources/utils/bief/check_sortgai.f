!                 ************************
                  SUBROUTINE CHECK_SORTGAI
!                 ************************
!
     &( CHAINE , NBRE , CHAINEHYD, IND_SED, YES2D, GRAP_PRINT2D)
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief   ADDS CORRESPONDIG GRAPHICAL PRINTOUT IN SORT3D/SORT2D/VARDES
!        FOR VARIABLES LIKE CS,SVX,SVY,SVZ,2DCS WHICH ARE ASKED IN THE
!        GAIA STEERING FILE AS GRAPHICAL PRINTOUT
!        (COPY-PASTE OF BEGINNING OF SUBROUTINE SORTIE)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHAINE         |<->| STRING OF VARIABLES FOR GAIA GRAPHIC OUTPUTS
!| CHAINEHYD      |<->| STRING OF VARIABLES FOR HYDRO GRAPHIC OUTPUTS
!| IND_SED        |-->| RANK OF SEDIMENTS IN TRACERS
!| NBRE           |-->| NUMBER OF VARIABLES
!| YES2D          |-->| LOGICAL TO CHECK IF T2D OR T3D
!| GRAP_PRINT2D   |-->| LOGICAL TO CHECK IF SORT2D OR SORT3D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NBRE
      INTEGER, INTENT(IN) :: IND_SED
      LOGICAL, INTENT(IN) :: YES2D
      LOGICAL, INTENT(IN), OPTIONAL :: GRAP_PRINT2D
!
      CHARACTER(LEN=72), INTENT(INOUT) :: CHAINE
      CHARACTER(LEN=72), INTENT(INOUT) :: CHAINEHYD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! INTERNAL VARIABLES:
!
      CHARACTER C(2)
      CHARACTER(LEN=8) MOT(100)
      CHARACTER(LEN=2)  :: TOBER
      CHARACTER(4) :: MYSTRING
      INTEGER I,J,LONG,I1,I2,NMOT,L,NUMS,NUMS_TO_NUMTR,LONG2,LONG3
!
      INTRINSIC LEN
      LOGICAL LGRAP_PRINT2D
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(GRAP_PRINT2D)) THEN
        LGRAP_PRINT2D = GRAP_PRINT2D
      ELSE
        LGRAP_PRINT2D = .FALSE.
      ENDIF

      LONG2 = 0
!
!  1) COUNTING HOW MANY CHARACTERS IN CHAINEHYD
!  RECOGNISED SEPARATORS IN 'CHAINEHYD'
!
      DO I=1,100
        MOT(I) = '        '
      ENDDO
      C(1) = ','
      C(2) = ';'
      LONG = LEN(CHAINEHYD)
      IF (LONG.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1002)
        IF(LNG.EQ.2) WRITE(LU,1003)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,LONG
        DO J=1,2
          IF(CHAINEHYD(I:I).EQ.C(J)) THEN
            CHAINEHYD(I:I) = ' '
!           COUNTING NUMBER OF SEPARATORS
            LONG2=LONG2+1
          ENDIF
        ENDDO
      ENDDO
!
! 'CHAINEHYD' NOW IS MADE UP OF WORDS SEPARATED BY WHITE SPACES
!
      I1 = 0
      NMOT=0
!
 10   CONTINUE
      IF (I1.GE.LONG) GOTO 30
      I1=I1+1
      IF (CHAINEHYD(I1:I1).EQ.' ') GOTO 10
!
      I2=0
!
 20   CONTINUE
      I2=I2+1
      IF (CHAINEHYD(I1+I2:I1+I2).NE.' ') GOTO 20
!
      NMOT=NMOT+1
      IF (I2.GT.8) THEN
        IF(LNG.EQ.1) WRITE(LU,1004) CHAINEHYD
        IF(LNG.EQ.2) WRITE(LU,1005) CHAINEHYD
        CALL PLANTE(1)
        STOP
      ENDIF
!     ADDING NUMBER OF CHARACTERS OF GRAPH OUTPUTS TO THE NUMBER
!     OF SEPARATORS IN ORDER TO KNOW THE TOTAL LENGTH
      LONG2=LONG2+I2
      MOT(NMOT)=CHAINEHYD(I1:I1+I2)
      I1=I1+I2
      GOTO 10
!
30    CONTINUE
!
!  2)LOOKING FOR CS OR SVX,SVY,SVZ IN CHAINEGAIA
!  RECOGNISED SEPARATORS IN 'CHAINE'
!
      DO I=1,100
        MOT(I) = '        '
      ENDDO
      LONG = LEN(CHAINE)
      IF (LONG.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1002)
        IF(LNG.EQ.2) WRITE(LU,1003)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,LONG
        DO J=1,2
          IF(CHAINE(I:I).EQ.C(J)) CHAINE(I:I) = ' '
        ENDDO
      ENDDO
!
! 'CHAINE' NOW IS MADE UP OF WORDS SEPARATED BY WHITE SPACES
!
      I1 = 0
      NMOT=0
!
 40   CONTINUE
      IF (I1.GE.LONG) GOTO 60
      I1=I1+1
      IF (CHAINE(I1:I1).EQ.' ') GOTO 40
!
      I2=0
!
 50   CONTINUE
      I2=I2+1
      IF (CHAINE(I1+I2:I1+I2).NE.' ') GOTO 50
!
      NMOT=NMOT+1
      IF (I2.GT.8) THEN
        IF(LNG.EQ.1) WRITE(LU,1004) CHAINE
        IF(LNG.EQ.2) WRITE(LU,1005) CHAINE
        CALL PLANTE(1)
        STOP
      ENDIF
      MOT(NMOT)=CHAINE(I1:I1+I2)
      I1=I1+I2
      GOTO 40
!
60    CONTINUE
!     LONG2: LENGTH OF ORIGINAL STRING FOR GR OUTPUT
!     LONG3: WILL BE THE LENGTH OF THE NEW STRING FOR GR OUTPUT
      LONG3=LONG2
!
      IF(YES2D)THEN
!       TELEMAC2D CASE
        DO J=1,NMOT
          L=1
          IF( MOT(J)(L:L+1).EQ.'CS' ) THEN
!           CHECK THE THIRD CHARACTER
            IF( MOT(J)(L+2:L+2).EQ.'*' ) THEN
              CHAINEHYD(LONG3+1:LONG3+3)=' T*'
              LONG3=LONG3+3
            ELSE
!             NUMBER OF SEDIMENT FOR WHICH GR OUTPUT IS ASKED
              TOBER=MOT(J)(L+2:L+3)
              READ(TOBER,*) NUMS
              IF(IND_SED.EQ.1) THEN
                NUMS_TO_NUMTR=NUMS
              ELSE
                NUMS_TO_NUMTR=NUMS+IND_SED
              ENDIF
!             FROM INTEGER TO STRING
              WRITE(MYSTRING,'(i4)') NUMS_TO_NUMTR
              CHAINEHYD(LONG3+1:LONG3+2)=' T'
!
              CHAINEHYD(LONG3+3:LONG3+3+(LEN(ADJUSTL(MYSTRING))-1))=
     &                        ADJUSTL(MYSTRING)
!             INCREMENT OF LENGTH OF STRING FOR GR OUTPUT
              LONG3=LONG3+4
            ENDIF
          ENDIF
        ENDDO
!
      ELSE
!
!       TELEMAC3D CASE
!
        IF(.NOT.LGRAP_PRINT2D) THEN
!         CASE OF 3D GRAPHIC PRINTOUT
          DO J=1,NMOT
            L=1
            IF( MOT(J)(L:L+1).EQ.'CS' ) THEN
!             CHECK THE THIRD CHARACTER
              IF( MOT(J)(L+2:L+2).EQ.'*' ) THEN
                CHAINEHYD(LONG3+1:LONG3+4)=' TA*'
                LONG3=LONG3+4
              ELSE
!               NUMBER OF SEDIMENT FOR WHICH GR OUTPUT IS ASKED
                TOBER=MOT(J)(L+2:L+3)
                READ(TOBER,*) NUMS
                IF(IND_SED.EQ.1) THEN
                  NUMS_TO_NUMTR=NUMS
                ELSE
                  NUMS_TO_NUMTR=NUMS+IND_SED
                ENDIF
!               FROM INTEGER TO STRING
                WRITE(MYSTRING,'(i4)') NUMS_TO_NUMTR
                CHAINEHYD(LONG3+1:LONG3+3)=' TA'
!
                CHAINEHYD(LONG3+4:LONG3+4+(LEN(ADJUSTL(MYSTRING))-1))=
     &                          ADJUSTL(MYSTRING)
!               INCREMENT OF LENGTH OF STRING FOR GR OUTPUT
                LONG3=LONG3+5
              ENDIF
            ELSEIF( MOT(J)(L:L+2).EQ.'SVX' ) THEN
!             CHECK THE FOURTH CHARACTER
              IF( MOT(J)(L+3:L+3).EQ.'*' ) THEN
                CHAINEHYD(LONG3+1:LONG3+5)=' NAX*'
                LONG3=LONG3+5
              ELSE
!               NUMBER OF SEDIMENT FOR WHICH GR OUTPUT IS ASKED
                TOBER=MOT(J)(L+3:L+4)
                READ(TOBER,*) NUMS
!               DISTINCTION BETWEEN CASE WHERE THERE ARE ONLY SEDIMENTS
!               AND CASE WHERE THERE ARE TRACERS+SEDIMENTS
                IF(IND_SED.EQ.1) THEN
                  NUMS_TO_NUMTR=NUMS
                ELSE
                  NUMS_TO_NUMTR=NUMS+IND_SED
                ENDIF
!               FROM INTEGER TO STRING
                WRITE(MYSTRING,'(i4)') NUMS_TO_NUMTR
                CHAINEHYD(LONG3+1:LONG3+4)=' NAX'
!
                CHAINEHYD(LONG3+5:LONG3+5+(LEN(ADJUSTL(MYSTRING))-1))=
     &                          ADJUSTL(MYSTRING)
!               INCREMENT OF LENGTH OF STRING FOR GR OUTPUT
                LONG3=LONG3+6
              ENDIF
            ELSEIF( MOT(J)(L:L+2).EQ.'SVY' ) THEN
!             CHECK THE FOURTH CHARACTER
              IF( MOT(J)(L+3:L+3).EQ.'*' ) THEN
                CHAINEHYD(LONG3+1:LONG3+5)=' NAY*'
                LONG3=LONG3+5
              ELSE
!               NUMBER OF SEDIMENT FOR WHICH GR OUTPUT IS ASKED
                TOBER=MOT(J)(L+3:L+4)
                READ(TOBER,*) NUMS
!               DISTINCTION BETWEEN CASE WHERE THERE ARE ONLY SEDIMENTS
!               AND CASE WHERE THERE ARE TRACERS+SEDIMENTS
                IF(IND_SED.EQ.1) THEN
                  NUMS_TO_NUMTR=NUMS
                ELSE
                  NUMS_TO_NUMTR=NUMS+IND_SED
                ENDIF
!               FROM INTEGER TO STRING
                WRITE(MYSTRING,'(i4)') NUMS_TO_NUMTR
                CHAINEHYD(LONG3+1:LONG3+4)=' NAY'
!
                CHAINEHYD(LONG3+5:LONG3+5+(LEN(ADJUSTL(MYSTRING))-1))=
     &                          ADJUSTL(MYSTRING)
!               INCREMENT OF LENGTH OF STRING FOR GR OUTPUT
                LONG3=LONG3+6
              ENDIF
            ELSEIF( MOT(J)(L:L+2).EQ.'SVZ' ) THEN
!             CHECK THE FOURTH CHARACTER
              IF( MOT(J)(L+3:L+3).EQ.'*' ) THEN
                CHAINEHYD(LONG3+1:LONG3+5)=' NAZ*'
                LONG3=LONG3+5
              ELSE
!               NUMBER OF SEDIMENT FOR WHICH GR OUTPUT IS ASKED
                TOBER=MOT(J)(L+3:L+4)
                READ(TOBER,*) NUMS
!               DISTINCTION BETWEEN CASE WHERE THERE ARE ONLY SEDIMENTS
!               AND CASE WHERE THERE ARE TRACERS+SEDIMENTS
                IF(IND_SED.EQ.1) THEN
                  NUMS_TO_NUMTR=NUMS
                ELSE
                  NUMS_TO_NUMTR=NUMS+IND_SED
                ENDIF
!               FROM INTEGER TO STRING
                WRITE(MYSTRING,'(i4)') NUMS_TO_NUMTR
                CHAINEHYD(LONG3+1:LONG3+4)=' NAZ'
!
                CHAINEHYD(LONG3+5:LONG3+5+(LEN(ADJUSTL(MYSTRING))-1))=
     &                          ADJUSTL(MYSTRING)
!               INCREMENT OF LENGTH OF STRING FOR GR OUTPUT
                LONG3=LONG3+6
              ENDIF
            ENDIF
          ENDDO
        ELSE
!         CASE OF 2D GRAPHIC PRINTOUT
          DO J=1,NMOT
            L=1
            IF( MOT(J)(L:L+3).EQ.'C2DS' ) THEN
!             CHECK THE FIFTH CHARACTER
              IF( MOT(J)(L+4:L+4).EQ.'*' ) THEN
                CHAINEHYD(LONG3+1:LONG3+4)=' TA*'
                LONG3=LONG3+4
              ELSE
!               NUMBER OF SEDIMENT FOR WHICH GR OUTPUT IS ASKED
                TOBER=MOT(J)(L+4:L+5)
                READ(TOBER,*) NUMS
                IF(IND_SED.EQ.1) THEN
                  NUMS_TO_NUMTR=NUMS
                ELSE
                  NUMS_TO_NUMTR=NUMS+IND_SED
                ENDIF
!               FROM INTEGER TO STRING
                WRITE(MYSTRING,'(i4)') NUMS_TO_NUMTR
                CHAINEHYD(LONG3+1:LONG3+3)=' TA'
!
                CHAINEHYD(LONG3+4:LONG3+4+(LEN(ADJUSTL(MYSTRING))-1))=
     &                          ADJUSTL(MYSTRING)
!               INCREMENT OF LENGTH OF STRING FOR GR OUTPUT
                LONG3=LONG3+5
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!
1002    FORMAT(1X,'SORTIE (BIEF) : CHAINEHYD VIDE')
1003    FORMAT(1X,'SORTIE (BIEF): EMPTY STRING')
1004    FORMAT(1X,'SORTIE (BIEF) : PLUS DE 8 CARACTERES PAR MOT',/,1X,
     &            '                 DANS LA CHAINE :',A)
1005    FORMAT(1X,'SORTIE (BIEF): MORE THAN 8 LETTERS PER WORD',/,1X,
     &            '                 IN THE CHAIN: ',A)
!----------------------------------------------------------------
!
      RETURN
      END


