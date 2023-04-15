!                   **********************************
                    SUBROUTINE LECDON_TELEMAC2D_WAQTEL
!                   **********************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CAS_FILE,DICO_FILE)
!
!***********************************************************************
! TELEMAC-2D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |<--| STORES STRINGS 'SUBMIT' OF DICTIONARY
!| MOTCAR         |<--| VALUES OF KEY-WORDS OF TYPE CHARACTER
!| NCAR           |-->| NUMBER OF LETTERS IN STRING PATH
!| PATH           |-->| FULL PATH TO CODE DICTIONARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY: NAMETRAC, NTRAC,MAXTRA
      USE DECLARATIONS_WAQTEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     ARGUMENTS
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: PATH
!     API
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: DICO_FILE
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CAS_FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      INTEGER          K,I
!
      CHARACTER(LEN=PATH_LEN) :: NOM_CAS
      CHARACTER(LEN=PATH_LEN) :: NOM_DIC
!-----------------------------------------------------------------------
!
! ARRAYS USED IN THE DAMOCLES CALL
!
      INTEGER            ADRESS(4,MAXKEYWORD),DIMEN(4,MAXKEYWORD)
      DOUBLE PRECISION   MOTREA(MAXKEYWORD)
      INTEGER            MOTINT(MAXKEYWORD)
      LOGICAL            MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=72)  MOTCLE(4,MAXKEYWORD,2)
      INTEGER            TROUVE(4,MAXKEYWORD)
      LOGICAL            DOC
      INTEGER :: ID_DICO, ID_CAS
!
! END OF DECLARATIONS FOR DAMOCLES CALL
!
!***********************************************************************
!
      WRITE(LU,2)
2     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*        SUBROUTINE LECDON_WAQTEL          *',/,
     &            19X, '*           CALL OF DAMOCLES               *',/,
     &            19X, '*        VERIFICATION OF READ DATA         *',/,
     &            19X, '*            ON STEERING FILE              *',/,
     &            19X, '********************************************',/)
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
!
      DO K=1,MAXKEYWORD
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
!
        DIMEN(1,K) = 0
        DIMEN(2,K) = 0
        DIMEN(3,K) = 0
        DIMEN(4,K) = 0
      ENDDO
!
!     WRITES OUT INFO
      DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OPENS DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'WAQDICO'
        NOM_CAS=PATH(1:NCAR)//'WAQCAS'
!
      ELSE
!
        NOM_DIC='WAQDICO'
        NOM_CAS='WAQCAS'
!
      ENDIF
      IF((CAS_FILE(1:1).NE.' ').AND.(DICO_FILE(1:1).NE.' ')) THEN
        NOM_DIC=DICO_FILE
        NOM_CAS=CAS_FILE
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
      CALL DAMOCLE
     &( ADRESS, DIMEN , MAXKEYWORD  , DOC    , LNG   , LU    , MOTINT,
     &  MOTREA, MOTLOG, MOTCAR, MOTCLE , TROUVE, ID_DICO, ID_CAS,
     &  .FALSE.,FILE_DESC)
!-----------------------------------------------------------------------
!     CLOSES DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!     DECODES 'SUBMIT' CHAINS
!
      CALL READ_SUBMIT(WAQ_FILES,MAXLU_WAQ,FILE_DESC,MAXKEYWORD)
!
!-----------------------------------------------------------------------
!
!     KINETIC MODEL FOR MICROPOL
      KIN_MICROPOL = MOTINT( ADRESS(1,  14) )
!
!----------------------------------------------------------------
!
      CALL NAMETRAC_WAQTEL(NAMETRAC,NTRAC,MAXTRA,WAQPROCESS)
!
!----------------------------------------------------------------
!
      RETURN
      END
