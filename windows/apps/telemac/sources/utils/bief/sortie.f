!                   *****************
                    SUBROUTINE SORTIE
!                   *****************
!
     &( CHAINE , MNEMO , NBRE , SORLEO )
!
!***********************************************************************
! BIEF   V7P3                                   29/08/2018
!***********************************************************************
!
!brief    SETS VARIABLES SORLEO AND SORIMP.
!
!history  J-M HERVOUET (LNHE)
!+        03/11/2009
!+        V6P0
!+   JOKER '*' ALLOWED IN NAMES
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
!history  S.E.BOURBAN (HRW)
!+        11/11/2016
!+        V7P2
!+   The * symbol now only represent a number, such as T* for all
!+   tracers (T1, T2, ... T10, T11, etc.) but not for TAU_S and others
!+   Note that G* will not pick up G, but only G1, G2, etc.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHAINE         |<->| STRING OF VARIABLES FOR GRAPHIC OUTPUTS
!| MNEMO          |<->| MNEMO OF VARIABLES
!| NBRE           |-->| NUMBER OF VARIABLES
!| SORLEO         |<->| LOGICAL ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NBRE
!
      CHARACTER(LEN=*), INTENT(INOUT) :: CHAINE
      CHARACTER(LEN=8), INTENT(IN) :: MNEMO(NBRE)
!
      LOGICAL, INTENT(INOUT) :: SORLEO(NBRE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! INTERNAL VARIABLES:
!
      CHARACTER C(2)
      CHARACTER(LEN=8) MOT(100)
      CHARACTER(LEN=10) :: KS
      CHARACTER(LEN=26) :: KL
      INTEGER I,J,LONG,I1,I2,NMOT,L,KI
      LOGICAL OK,FOUND
!
      INTRINSIC LEN
!
!-----------------------------------------------------------------------
!
      PARAMETER ( KS = '0123456789' )
      PARAMETER ( KL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
!
!-----------------------------------------------------------------------
!
!  RECOGNISED SEPARATORS IN 'CHAINE'
!
      DO I=1,100
        MOT(I) = '        '
      ENDDO
      C(1) = ','
      C(2) = ';'
      LONG = LEN(CHAINE)
      IF (LONG.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1002)
        IF(LNG.EQ.2) WRITE(LU,1003)
1002    FORMAT(1X,'SORTIE (BIEF) : CHAINE VIDE')
1003    FORMAT(1X,'SORTIE (BIEF): EMPTY STRING')
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
 10   CONTINUE
      IF (I1.GE.LONG) GOTO 30
      I1=I1+1
      IF (CHAINE(I1:I1).EQ.' ') GOTO 10
!
      I2=0
!
 20   CONTINUE
      I2=I2+1
      IF (CHAINE(I1+I2:I1+I2).NE.' ') GOTO 20
!
      NMOT=NMOT+1
      IF (I2.GT.8) THEN
        IF(LNG.EQ.1) WRITE(LU,1004) CHAINE
        IF(LNG.EQ.2) WRITE(LU,1005) CHAINE
1004    FORMAT(1X,'SORTIE (BIEF) : PLUS DE 8 CARACTERES PAR MOT',/,1X,
     &            '                 DANS LA CHAINE :',A)
1005    FORMAT(1X,'SORTIE (BIEF): MORE THAN 8 LETTERS PER WORD',/,1X,
     &            '                 IN THE CHAIN: ',A)
        CALL PLANTE(1)
        STOP
      ENDIF
      MOT(NMOT)=CHAINE(I1:I1+I2)
      I1=I1+I2
      GOTO 10
!
30    CONTINUE
!
!     COMPARES 'MOT' AND 'MNEMO'
!
      DO I=1,NBRE
        DO J=1,NMOT
          OK=.TRUE.
          DO L=1,8
            IF( MOT(J)(L:L).NE.MNEMO(I)(L:L) ) THEN
!           A JOKER '*' IS ALLOWED BUT ONLY TO REPLACE A NUMBER
              IF( MOT(J)(L:L).EQ.'*'.AND.MNEMO(I)(L:L).NE.' ' ) THEN
                FOUND = .FALSE.
                DO KI = 1,10
                  IF( MNEMO(I)(L:L).EQ.KS(KI:KI) ) THEN
!                   CHECK ON THE NEXT CHARACTER TO AVOID
!                   SORLEO.EQ.TRUE FOR ALL VARIABLES STARTING WITH '*'
                    IF(MNEMO(I)(L+1:L+1).EQ.MOT(J)(L+1:L+1).OR.
     &                 MNEMO(I)(L+2:L+2).EQ.MOT(J)(L+1:L+1)) THEN
                      FOUND = .TRUE.
                    ENDIF
                  ENDIF
                ENDDO
                IF( FOUND ) EXIT
!           A TILDA '~' IS ALLOWED BUT ONLY TO REPLACE CHARACTERS
              ELSEIF( MOT(J)(L:L).EQ.'~'.AND.MNEMO(I)(L:L).NE.' ' ) THEN
                FOUND = .FALSE.
                DO KI = 1,26
                  IF( MNEMO(I)(L:L).EQ.KL(KI:KI) ) FOUND = .TRUE.
                ENDDO
                IF( FOUND ) EXIT
              ENDIF
              OK=.FALSE.
              EXIT
            ENDIF
            IF( MOT(J)(L:L).EQ.' '.AND.MNEMO(I)(L:L).EQ.' ') EXIT
          ENDDO
          SORLEO(I)=OK
          IF(SORLEO(I)) EXIT
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
