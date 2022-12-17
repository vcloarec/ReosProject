!                   ***************************
                    SUBROUTINE READ_SOURCE_DATA
!                   ***************************
!
     &(NFIC,NREG)
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief   READS COORINATES OF POLYGONES FOR CASES WHERE SOURCES AREC
!        DEFINED WITH REGIONS (NOT WITH NODES)
!
!history  SARA PAVAN (LNHE)
!+        05/03/2018
!+
!+   FIRST VERSION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NFIC           |-->| LOGICAL UNIT OF FILE
!| NREG           |-->| NUMBER OF SOURCE REGIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE DECLARATIONS_TELEMAC2D, ONLY : PTS_REG,XCOO,YCOO
      USE INTERFACE_TELEMAC2D, EX_READ_SOURCE_DATA =>READ_SOURCE_DATA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      INTEGER         , INTENT(IN)    :: NFIC,NREG
!      INTEGER         , INTENT(INOUT) :: PTS_REG(NREG)
!      DOUBLE PRECISION, INTENT(INOUT) :: XCOO(NREG,*),YCOO(NREG,*)
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDEB,IFIN,IREG,IND
      CHARACTER(LEN=144) :: LIGNE
      CHARACTER(LEN=1)   :: WHAT
      LOGICAL FIRST
!
      INTRINSIC CHAR
!
!-----------------------------------------------------------------------
!
!
      FIRST=.TRUE.
!
      REWIND(NFIC)
      IND=0
!     SKIPS COMMENTS
1     CONTINUE
      IF(FIRST) THEN
        READ(NFIC,FMT='(A)',END=1000,ERR=999) LIGNE
      ELSE
!       FOR SOME COMPILERS READING AFTER END IS AN ERROR
!       SO HERE ERROR IS TREATED AS END
        READ(NFIC,FMT='(A)',END=1000,ERR=1000) LIGNE
      ENDIF
      IF(LIGNE(1:1).EQ.'#') GO TO 1
!
!     IDENTIFIES FIRST CHARACTER OF NAME
2     CONTINUE
      IDEB=1
!     SKIPPING SPACES OR TABS
      IF(LIGNE(IDEB:IDEB).EQ.' '.OR.LIGNE(IDEB:IDEB).EQ.CHAR(9)) THEN
        IDEB=IDEB+1
        IF(IDEB.EQ.145) THEN
          READ(NFIC,FMT='(A)') LIGNE
          IDEB=1
        ENDIF
        GO TO 2
      ENDIF
      IF(LIGNE(IDEB:IDEB+1).EQ.'X(') THEN
        WHAT=LIGNE(IDEB:IDEB)
!       WHICH REGION NUMBER?
        IDEB=IDEB+2
        IFIN=IDEB+1
3       IF(LIGNE(IFIN:IFIN).NE.')') THEN
          IFIN=IFIN+1
          IF(IFIN.GT.144) THEN
            WRITE(LU,*) 'ERROR IN THE SOURCE FILE'
            WRITE(LU,*) 'MISSING PARENTHESIS IN LINE:',LIGNE
            CALL PLANTE(1)
            STOP
          ENDIF
          GO TO 3
        ENDIF
        READ(LIGNE(IDEB:IFIN-1),*) IREG
        PTS_REG(IREG)=0
        WRITE(LU,*) 'INITIALISATION IREG=',IREG
        IND=IND+1
4       CONTINUE
        READ(NFIC,FMT='(A)',END=1001,ERR=999) LIGNE
        IF(LIGNE(1:1).NE.'#'.AND.LIGNE.NE.'') THEN
          PTS_REG(IREG)=PTS_REG(IREG)+1
!         READS AND STORES
          IF(WHAT.EQ.'X') THEN
            READ(LIGNE,*,ERR=999) XCOO(PTS_REG(IREG),IREG),
     &                            YCOO(PTS_REG(IREG),IREG)
          ENDIF
          GO TO 4
        ENDIF
!       END OF BLOCK FOR REGION IREG
!       TREATS THE NEXT REGION
1001    FIRST=.FALSE.
        GO TO 1
      ELSE
        WRITE(LU,*) 'ERROR IN THE SOURCE FILE'
        WRITE(LU,*) 'THE FIRST LINE AFTER COMMENTS:',LIGNE
        WRITE(LU,*) 'MUST ANNOUNCE X(..) AND Y(..)'
        CALL PLANTE(1)
        STOP
      ENDIF
999   CONTINUE
      WRITE(LU,*) 'ERROR IN THE ASCII SOURCE DATA FILE'
      CALL PLANTE(1)
      STOP
1000  CONTINUE
!     CHECKING IF NUMBER OF REGIONS EQUALS NUMBER OF DISCHARGES AT SOURCES
      IF(IND.NE.NREG) THEN
        WRITE(LU,*)NREG,' DISCHARGE (OR VELOCITIES) OF SOURCES GIVEN'
        WRITE(LU,*)IND,' SOURCES REGIONS FOUND'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
