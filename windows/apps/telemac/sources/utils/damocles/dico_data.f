      ! brief set of function to read/write a dictionary
      MODULE DICO_DATA
      USE DECLARATIONS_DAMOCLES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! brief Max size of aide
      INTEGER, PARAMETER :: AIDE_LEN = 2400
      ! brief Size for the CHOIX string
      INTEGER, PARAMETER :: CHOIX_LEN = 2500
      ! brief Size for the DEFAUT string
      INTEGER, PARAMETER :: DEFAUT_LEN = 1400
      ! brief Size for the Condition string
      INTEGER, PARAMETER :: COND_LEN = 700
      ! brief Size for the Consigne string
      INTEGER, PARAMETER :: CONSIGNE_LEN = 500
      ! brief Size for the Mnemo string
      INTEGER, PARAMETER :: STRING_LEN = PATH_LEN
      ! brief Size for the Keyword name
      INTEGER, PARAMETER :: KEYWORD_LEN = 72
      ! brief Maximum number of dependencies
      INTEGER, PARAMETER :: MAXDEP = 18
      ! brief Maximum number of conditions
      INTEGER, PARAMETER :: MAXCOND = 9
      ! brief Maximum number of conditions
      INTEGER, PARAMETER :: MAXENUM = 70
      ! brief type for a keyword
      TYPE KEYWORD
        ! param Name of the ley in French and English
        CHARACTER(LEN=KEYWORD_LEN) :: KNOM(2)
        ! param Type fo the key word 1:integer 2:real  3:logical  4: String
        INTEGER         :: KTYPE
        ! param Index of the keyword
        INTEGER         :: KINDEX
        ! param Name of the variable containing the keyword in the Fortran code
        CHARACTER(LEN=STRING_LEN) :: MNEMO
        ! To Be defined
        ! param Size of the keyword 0: 1: 2:
        INTEGER         :: TAILLE
        ! param String containing information on file keyword
        CHARACTER(LEN=STRING_LEN) :: SUBMIT
        ! param Default value in frecnh and in english
        CHARACTER(LEN=DEFAUT_LEN) :: DEFAUT(2)
        ! param List of values for the keyword
        CHARACTER(LEN=CHOIX_LEN) :: CHOIX(2)
        ! param Hash table when choix is in form 'id'='name'
        CHARACTER(LEN=KEYWORD_LEN) :: HASH_ID(MAXENUM,2)
        CHARACTER(LEN=KEYWORD_LEN) :: HASH_VAL(MAXENUM,2)
        ! param Classification of the keyword
        CHARACTER(LEN=STRING_LEN) :: RUBRIQUE(2,3)
        ! param
        CHARACTER(LEN=STRING_LEN) :: COMPOSE
        ! param
        CHARACTER(LEN=STRING_LEN) :: COMPORT
        ! param
        CHARACTER(LEN=STRING_LEN) :: CONTROLE
        ! param
        CHARACTER(LEN=STRING_LEN) :: APPARENCE
        ! param Level of the keyword
        INTEGER :: NIVEAU
        ! param help on the keyword in french and in english
        CHARACTER(LEN=AIDE_LEN) :: AIDE(2)
        ! param list of ikey that depend on cond(i)
        INTEGER DEPEN(MAXCOND,MAXDEP)
        ! param string conition for displaying of keyword
        CHARACTER(LEN=COND_LEN) COND(MAXCOND)
        ! param string conition for displaying of keyword
        CHARACTER(LEN=CONSIGNE_LEN) CONSIGNE(MAXCOND,2)
        ! param Hash table for lists
        CHARACTER(LEN=AIDE_LEN) :: PATHNODE
        ! param pathnode for MASCARET and COURLIS XML steering file
        CHARACTER(LEN=AIDE_LEN) :: ALLOC
        ! param allocate for MASCARET and COURLIS dynamic XML parameters
        CHARACTER(LEN=AIDE_LEN) :: ALLOCED
        ! param allocated for MASCARET and COURLIS dynamic XML
        ! parameters
        CHARACTER(LEN=AIDE_LEN) :: ALLOCED2
        ! param allocated for MASCARET and COURLIS dynamic XML
        ! parameters (two dimensions)
      END TYPE KEYWORD
!
      ! brief Max number of keyword per type
      INTEGER, PARAMETER :: NMAX=121
      ! brief Max number of rubrique
      INTEGER, PARAMETER :: RMAX=50
!
      ! brief array of keyword contains the dictionary info once read
      TYPE(KEYWORD) :: MYDICO(NMAX*4)
      ! brief Number of keywords in mydico
      INTEGER :: NKEY=0
!
      ! Contains the name of each rubriques for each level and each
      ! language
      CHARACTER(LEN=STRING_LEN) :: RUBRIQUE(2,RMAX,3)
      ! brief Dependancies between rubriques
      LOGICAL, ALLOCATABLE :: RUB1_DEP(:,:),RUB2_DEP(:,:,:)
      INTEGER :: NRUB(2,3)
      CHARACTER(LEN=1) :: RUBRIQUE_INFO(RMAX,3)
!
      CONTAINS
      !
      ! brief write an integer into a string
      !
      ! param string The output string
      ! param i the integer to write
      ! param pos the position of the first non blank character
      SUBROUTINE INT2STR(STRING,I,POS)
      IMPLICIT NONE
      !
      CHARACTER(LEN=3),INTENT(INOUT) :: STRING
      INTEGER,INTENT(IN) :: I
      INTEGER,INTENT(INOUT) :: POS
      !
      WRITE(STRING,'(I3)') I
      POS=1
      DO
        IF(STRING(POS:POS).NE.' ') EXIT
        POS=POS+1
      ENDDO
      END SUBROUTINE
      !
      ! brief Fill the array rubrique that contains the list of the rtubriques
      !
      SUBROUTINE CHECK_INDEX(NFIC)
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: NFIC
      !
      INTEGER IKEY,ITYP,I,IERR,J
      INTEGER NTYP(4),OLD_NTYP(4)
      INTEGER MAX_IDX(4)
      INTEGER,ALLOCATABLE :: ITYP2KEY(:,:)
      LOGICAL,ALLOCATABLE :: IDX_USED(:,:)
      CHARACTER(LEN=PATH_LEN) :: USED_IDX(4)
      CHARACTER(LEN=3) :: I2S
      INTEGER :: LAST_TRUE,IDX
      !
      ! Count the number of keyword by type
      !
      NTYP = 0
      MAX_IDX = 0
      DO IKEY=1,NKEY
        ITYP = MYDICO(IKEY)%KTYPE
        NTYP(ITYP) = NTYP(ITYP) + 1
        MAX_IDX(ITYP) = MAX(MAX_IDX(ITYP),MYDICO(IKEY)%KINDEX)
      ENDDO
      IF(NFIC.EQ.6) THEN
        WRITE(*,*) '---- INDEX INFORMATIONS ----'
        WRITE(*,*) 'NUMBER OF KEY WORD BY TYPE AND MAX INDEX:'
        WRITE(*,*) 'FOR INTEGER: ',NTYP(1),MAX_IDX(1)
        WRITE(*,*) 'FOR REAL   : ',NTYP(2),MAX_IDX(2)
        WRITE(*,*) 'FOR LOGICAL: ',NTYP(3),MAX_IDX(3)
        WRITE(*,*) 'FOR STRING : ',NTYP(4),MAX_IDX(4)
      ENDIF
      ALLOCATE(ITYP2KEY(MAXVAL(NTYP),4),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ITYP2KEY')
      OLD_NTYP = NTYP
      NTYP = 0
      ! Computting the ityp2key array
      ! Loop on all types
      DO I=1,4
        ! Loop on all indexes
        DO IDX=1,MAX_IDX(I)
          ! Identifying the key associated to the idx
          DO IKEY=1,NKEY
            ITYP = MYDICO(IKEY)%KTYPE
            IF(I.NE.ITYP) CYCLE
            IF(MYDICO(IKEY)%KINDEX.NE.IDX) CYCLE
            NTYP(ITYP) = NTYP(ITYP) + 1
            ITYP2KEY(NTYP(ITYP),ITYP) = IKEY
            EXIT
          ENDDO
        ENDDO
      ENDDO
!
      DO I=1,4
        IF(NTYP(I).NE.OLD_NTYP(I)) THEN
          WRITE(*,*) 'ERROR ON INDEX FOR TYPE',I
          CALL PLANTE(1)
        ENDIF
      ENDDO
      !
      ALLOCATE(IDX_USED(MAXVAL(MAX_IDX),4),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IDX_USED')
      IDX_USED = .FALSE.
      DO IKEY=1,NKEY
        IDX_USED(MYDICO(IKEY)%KINDEX,MYDICO(IKEY)%KTYPE) = .TRUE.
      ENDDO
      DO ITYP=1,4
        USED_IDX(ITYP) = REPEAT(' ',PATH_LEN)
        IF(NTYP(ITYP).EQ.0) CYCLE
        LAST_TRUE = 1
        LAST_TRUE = MYDICO(ITYP2KEY(1,ITYP))%KINDEX
        CALL INT2STR(I2S,LAST_TRUE,I)
        USED_IDX(ITYP)(1:1) = I2S(I:3)
        DO J=2,NTYP(ITYP)
          IKEY = ITYP2KEY(J,ITYP)
          IDX = MYDICO(IKEY)%KINDEX
          IF(IDX.EQ.LAST_TRUE+1) THEN
            LAST_TRUE = IDX
          ELSE
            CALL INT2STR(I2S,LAST_TRUE,I)
            USED_IDX(ITYP) = TRIM(USED_IDX(ITYP)) // '-' // I2S(I:3)
            LAST_TRUE = IDX
            CALL INT2STR(I2S,LAST_TRUE,I)
            USED_IDX(ITYP) = TRIM(USED_IDX(ITYP)) // ',' // I2S(I:3)
          ENDIF
        ENDDO
        IF(IDX_USED(MAX_IDX(ITYP),ITYP)
     &     .AND.IDX_USED(MAX_IDX(ITYP)-1,ITYP)) THEN
          CALL INT2STR(I2S,MAX_IDX(ITYP),I)
          USED_IDX(ITYP) = TRIM(USED_IDX(ITYP)) // '-' // I2S(I:3)
        ENDIF
      ENDDO
      WRITE(NFIC,'(A)') '/'
      WRITE(NFIC,'(3A,I3)') '/ INTEGER INDEX USED: ',TRIM(USED_IDX(1)),
     &           ' OUT OF ',NTYP(1)
      WRITE(NFIC,'(3A,I3)') '/ REAL INDEX USED: ',TRIM(USED_IDX(2)),
     &           ' OUT OF ',NTYP(2)
      WRITE(NFIC,'(3A,I3)') '/ LOGICAL INDEX USED: ',TRIM(USED_IDX(3)),
     &           ' OUT OF ',NTYP(3)
      WRITE(NFIC,'(3A,I3)') '/ STRING INDEX USED: ',TRIM(USED_IDX(4)),
     &           ' OUT OF ',NTYP(4)
      WRITE(NFIC,'(A)') '/'
      DEALLOCATE(IDX_USED)
      DEALLOCATE(ITYP2KEY)
      !
      END SUBROUTINE
      !
      ! brief Fill the array rubrique that contains the list of the rtubriques
      !
      SUBROUTINE IDENTIFY_RUBRIQUE()
      IMPLICIT NONE
      !
      INTEGER :: I,J,IKEY,LNG,RKEY
      LOGICAL :: ALREADY_IN
      !
      ! Check that a rubrique has not the same name as a keyword
      !  Loop on all languge
      DO LNG=1,2
        NRUB(LNG,:) = 0
        ! Get the first rubriques
        DO I=1,3
          IF(MYDICO(1)%RUBRIQUE(LNG,I).NE.' ') THEN
            NRUB(LNG,I) = NRUB(LNG,I) + 1
            RUBRIQUE(LNG,NRUB(LNG,I),I) = MYDICO(1)%RUBRIQUE(LNG,I)
            RKEY = IDENTIFY_KEYWORD(
     &               MYDICO(1)%RUBRIQUE(LNG,I),LNG)
            IF (RKEY.NE.-1) THEN
              WRITE(*,*) 'ERROR RUBRIQUE: ',
     &                    TRIM(MYDICO(1)%RUBRIQUE(LNG,I))
              WRITE(*,*) 'IS ALSO A KEYWORD PLEASE RENAME RUBRIQUE'
              CALL PLANTE(1)
            ENDIF
          ENDIF
        ENDDO
        DO IKEY=1,NKEY
          DO I=1,3
            IF(MYDICO(IKEY)%RUBRIQUE(LNG,I).NE.' ') THEN
              ! Check if keyword already found
              ALREADY_IN = .FALSE.
              DO J=1,NRUB(LNG,I)
                IF(MYDICO(IKEY)%RUBRIQUE(LNG,I)
     &             .EQ.RUBRIQUE(LNG,J,I)) THEN
                  ALREADY_IN = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              ! If new rubrique adding it to the rubrique array
              IF(.NOT.ALREADY_IN) THEN
                NRUB(LNG,I) = NRUB(LNG,I) + 1
                RUBRIQUE(LNG,NRUB(LNG,I),I) =
     &                        MYDICO(IKEY)%RUBRIQUE(LNG,I)
                RKEY = IDENTIFY_KEYWORD(
     &                   MYDICO(IKEY)%RUBRIQUE(LNG,I),LNG)
                IF (RKEY.NE.-1) THEN
                  WRITE(*,*) 'ERROR RUBRIQUE: ',
     &                        TRIM(MYDICO(IKEY)%RUBRIQUE(LNG,I))
                  WRITE(*,*) 'IS ALSO A KEYWORD PLEASE RENAME RUBRIQUE'
                  CALL PLANTE(1)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO

      ENDDO ! LNG
      END SUBROUTINE
      !
      ! brief Fill the array rubrique_tree that contains dependencies of
      ! each rubriques
      !
      SUBROUTINE IDENTIFY_RUBRIQUE_DEPENDS()
      IMPLICIT NONE
      !
      INTEGER :: IRUB1,IRUB2,IRUB3,IKEY
      INTEGER :: LNG
      INTEGER :: IERR
      !
      !  Loop on all languge
      LNG = 1
      ALLOCATE(RUB1_DEP(NRUB(LNG,1),MAX(NRUB(LNG,2),1)),
     &         STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'RUB1_DEP')
      ALLOCATE(RUB2_DEP(NRUB(LNG,1),MAX(NRUB(LNG,2),1),
     &                  MAX(NRUB(LNG,3),1)),
     &         STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'RUB2_DEP')
      RUB1_DEP(:,:) = .FALSE.
      RUB2_DEP(:,:,:) = .FALSE.
      DO IRUB1 = 1,NRUB(LNG,1)
        DO IRUB2 = 1,NRUB(LNG,2)
        ! If we have no level 3 the loop is not done
          IF (NRUB(LNG,3).NE.0) THEN
            DO IRUB3 = 1,NRUB(LNG,3)
              DO IKEY=1,NKEY
                IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &             HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &             HAS_RUBRIQUE(IKEY,IRUB3,3,LNG)) THEN
                  ! Link between 2 and 3
                  RUB2_DEP(IRUB1,IRUB2,IRUB3) = .TRUE.
                ENDIF
                IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &             HAS_RUBRIQUE(IKEY,IRUB2,2,LNG)) THEN
                  ! Link between 1 and 2
                  RUB1_DEP(IRUB1,IRUB2) = .TRUE.
                ENDIF
              ENDDO
            ENDDO
          ELSE
            DO IKEY=1,NKEY
              IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &           HAS_RUBRIQUE(IKEY,IRUB2,2,LNG)) THEN
                ! Link between 1 and 2
                RUB1_DEP(IRUB1,IRUB2) = .TRUE.
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      DO IRUB1 = 1,NRUB(LNG,1)
        WRITE(*,*) REPEAT('--',1),TRIM(RUBRIQUE(LNG,IRUB1,1))
        DO IRUB2 = 1,NRUB(LNG,2)
          IF(RUB1_DEP(IRUB1,IRUB2)) THEN
            WRITE(*,*) REPEAT('--',2),TRIM(RUBRIQUE(LNG,IRUB2,2))
          ENDIF
          DO IRUB3 = 1,NRUB(LNG,3)
            IF(RUB2_DEP(IRUB1,IRUB2,IRUB3)) THEN
              WRITE(*,*) REPEAT('--',3),TRIM(RUBRIQUE(LNG,IRUB3,3))
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      END SUBROUTINE
      !
      ! brief Identify the index and level of a rubrique
      !
      ! param rub Name of the rubrique
      ! param idx index of the rubrique
      ! param level Level of the rubrique
      !
      SUBROUTINE IDENTIFY_RUBRIQUE_IDX(RUB,IDX,LEVEL)
      IMPLICIT NONE
      !
      CHARACTER(LEN=STRING_LEN) :: RUB
      INTEGER, INTENT(OUT) :: IDX
      INTEGER, INTENT(OUT) :: LEVEL
      !
      INTEGER :: I,ILEVEL
      !
      ! Check that a rubrique has not the same name as a keyword
      !  Loop on all languge
      IDX = -1
      LEVEL = -1
      DO ILEVEL=1,3
        DO I=1,NRUB(LNG_EN,ILEVEL)
          IF(RUBRIQUE(LNG_EN,I,ILEVEL).EQ.RUB) THEN
            IDX = I
            LEVEL = ILEVEL
            RETURN
          ENDIF
        ENDDO
      ENDDO
      END SUBROUTINE
      ! brief Return the hash value linked to the hash_id
      !
      ! param ikey Id of the keyword
      ! param hash_id value in the hash_id array
      ! param lng Language to take into account
      CHARACTER(LEN=KEYWORD_LEN) FUNCTION GET_HASH_VALUE(IKEY,ID,LNG)
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: IKEY
      CHARACTER(LEN=KEYWORD_LEN) :: ID
      INTEGER, INTENT(IN) :: LNG
      !
      INTEGER I
      GET_HASH_VALUE = REPEAT(' ',KEYWORD_LEN)
      DO I=1,MAXENUM
        ! If we reach an empty id it is the end of ids
        IF(MYDICO(IKEY)%HASH_ID(I,LNG)(1:1).EQ.' ') EXIT
        IF (ID.EQ.MYDICO(IKEY)%HASH_ID(I,LNG)) THEN
          GET_HASH_VALUE = MYDICO(IKEY)%HASH_VAL(I,LNG)
          EXIT
        ENDIF
      ENDDO
      END FUNCTION
      ! brief write in canal a list of values from a keyword
      !+      as neatly as possible
      !
      ! param nfic file canal
      ! param ktype Type of the keyword
      ! param str Language of the ouput
      ! param str_len Length of the keyword value
      ! param cmd Command to be printed at the beginning of the list
      ! param cmd_len Length of the cmd string
      SUBROUTINE DUMP_LIST(NFIC,KTYPE,STR,STR_LEN,CMD,CMD_LEN)
      IMPLICIT NONE
      !
      INTEGER,INTENT(IN) :: NFIC
      INTEGER, INTENT(IN) :: KTYPE
      INTEGER, INTENT(IN) :: STR_LEN
      CHARACTER(LEN=STR_LEN) :: STR
      INTEGER, INTENT(IN) :: CMD_LEN
      CHARACTER(LEN=CMD_LEN),INTENT(IN) :: CMD
      !
      INTEGER LENGTH, IDB, IDE, IDX
!
      LENGTH = LEN(TRIM(STR))
      ! If the default values are strings we add quote
      IF(KTYPE.EQ.4.OR.CMD(1:5).EQ.'CHOIX') THEN
        ! If they are too many values printing each on a new line
        IF(LENGTH.GT.60.OR.CMD(1:5).EQ.'CHOIX') THEN
          ! First value printing additional text
          IDB = 1
          IDE = INDEX(STR(1:LENGTH),';')
          WRITE(NFIC,'(2A)') CMD," ="
          WRITE(NFIC,'(3A)') "'",STR(IDB:IDE-1),"';"
          DO
            IDB = IDE + 1
            IDX = INDEX(STR(IDB:LENGTH),';')
            IDE = IDX + IDB - 1
            IF(IDX.EQ.0.OR.IDB.GT.LENGTH) EXIT
            WRITE(NFIC,'(3A)') "'",
     &                       STR(IDB:IDE-1),"';"
          ENDDO
            WRITE(NFIC,'(3A)') "'",
     &                       STR(IDB:LENGTH),"'"
        ELSE
          ! Normal one line printing with quote
          WRITE(NFIC,'(4A)') CMD," = '",TRIM(STR),"'"
        ENDIF
      ELSE
        IF(LENGTH.GT.60) THEN
          ! First value printing additional text
          IDB = 1
          IDE = INDEX(STR(1:LENGTH),';')
          WRITE(NFIC,'(2A)') CMD," ="
          WRITE(NFIC,'(2A)') STR(IDB:IDE-1),";"
          DO
            IDB = IDE + 1
            IDX = INDEX(STR(IDB:LENGTH),';')
            IDE = IDX + IDB - 1
            IF(IDX.EQ.0.OR.IDB.GT.LENGTH) EXIT
            WRITE(NFIC,'(2A)') STR(IDB:IDE-1),";"
          ENDDO
            WRITE(NFIC,'(A)') STR(IDB:LENGTH)
        ! Normal one line printing without quote
        ELSE
          WRITE(NFIC,'(3A)') CMD," = ",TRIM(STR)
        ENDIF
      ENDIF
      !
      END SUBROUTINE
      ! brief write in canal the default value in Latex
      !+      form as neatly as possible
      !
      ! param nfic file canal
      ! param ikey key index in myDico
      ! param lng language of the key (1:french,2:english)
      SUBROUTINE WRITE_DEFAULT(NFIC,IKEY,LNG)
      IMPLICIT NONE
      !
      INTEGER,INTENT(IN) :: IKEY
      INTEGER,INTENT(IN) :: NFIC
      INTEGER, INTENT(IN) :: LNG
      !
      INTEGER LENGTH, IDB, IDE, IDX
      CHARACTER(LEN=DEFAUT_LEN) :: STRING
!
      ! If the default values are strings we add quote
      IF(MYDICO(IKEY)%KTYPE.EQ.4) THEN
        STRING = MYDICO(IKEY)%DEFAUT(LNG)
        LENGTH = LEN(TRIM(MYDICO(IKEY)%DEFAUT(LNG)))
        ! If they are too many values printing each on a new line
        IF(LENGTH.GT.70) THEN
          ! First value printing additional text
          IDB = 1
          IDE = INDEX(STRING(1:LENGTH),';')
          WRITE(NFIC,'(3A)') "DEFAULT VALUE : & '",
     &                     STRING(IDB:IDE),"\\"
          DO
            IDB = IDE + 1
            IDX = INDEX(STRING(IDB:LENGTH),';')
            IDE = IDX + IDB - 1
            IF(IDX.EQ.0.OR.IDB.GT.LENGTH) EXIT
            WRITE(NFIC,'(3A)') "                & ",
     &                       STRING(IDB:IDE),"\\"
          ENDDO
            WRITE(NFIC,'(3A)') "                & ",
     &                       STRING(IDB:LENGTH),"'\\"
        ELSE
          ! Normal one line printing with quote
          WRITE(NFIC,'(3A)') "DEFAULT VALUE : & '",
     &                     TRIM(MYDICO(IKEY)%DEFAUT(LNG)),"'\\"
        ENDIF
      ELSE
        ! Normal one line printing without quote
        WRITE(NFIC,'(3A)') "DEFAULT VALUE : & ",
     &                     TRIM(MYDICO(IKEY)%DEFAUT(LNG)),"\\"
      ENDIF
      !
      END SUBROUTINE
      ! brief Return true if rubrique(irub,level) is in keyword ikey rubrique
      !
      ! param ikey key index in myDico
      ! param level level of the rubrique (1,2,3)
      ! param irub rubrique in rubrique
      ! param lng language of the key (1:french,2:english)
      !
      ! return A logical
      LOGICAL FUNCTION HAS_RUBRIQUE(IKEY,IRUB,LEVEL,LNG)
      IMPLICIT NONE
      !
      INTEGER,INTENT(IN) :: IKEY
      INTEGER,INTENT(IN) :: IRUB
      INTEGER,INTENT(IN) :: LEVEL
      INTEGER, INTENT(IN) :: LNG
      !
      HAS_RUBRIQUE = .FALSE.
      ! Looping on each sub rubrique
      IF(MYDICO(IKEY)%RUBRIQUE(LNG,LEVEL).EQ.
     &   RUBRIQUE(LNG,IRUB,LEVEL)) THEN
        HAS_RUBRIQUE = .TRUE.
        RETURN
      ENDIF
      END FUNCTION
      ! brief Identify what reserved key is the ligne associated to
      !
      ! param chaine key to identify
      ! param ilong length of chaine
      ! param numero number of the key
      ! param lng language of the key (1:french,2:english)
      SUBROUTINE IDENTIFY_KEY(CHAINE,ILONG,NUMERO,LNG)
      IMPLICIT NONE
      !
      CHARACTER(LEN=*),INTENT(IN) :: CHAINE
      INTEGER,INTENT(INOUT) :: ILONG
      INTEGER, INTENT(INOUT) :: NUMERO
      INTEGER, INTENT(INOUT) :: LNG
      !
      CHARACTER(LEN=10) :: MOTPRO(19)
      PARAMETER ( MOTPRO = (/
     & 'NOM       ','TYPE      ','INDEX     ','TAILLE    ','DEFAUT    ',
     & 'AIDE      ','CHOIX     ','RUBRIQUE  ','NIVEAU    ','MNEMO     ',
     & 'COMPOSE   ','COMPORT   ','CONTROLE  ','APPARENCE ','SUBMIT    ',
     & 'PATHNODE  ','ALLOCATE  ','ALLOCATED ','ALLOCATED2' /) )
!     LENGTH OF THE PROTECTED WORDS
      INTEGER :: LONPRO(19),I
      PARAMETER ( LONPRO = (/ 3,4,5,6,6,4,5,8,6,5,7,7,8,9,6,8,8,9,10 /))
      !
      LNG = 1
      NUMERO = 0
      ! Check if it is an english key 1 at the end
      IF (CHAINE(ILONG:ILONG).EQ.'1') THEN
        ILONG = ILONG - 1
        LNG = 2
      ENDIF
      ! Search in the list of known keyword
      DO I=1,19
        IF (ILONG.EQ.LONPRO(I)) THEN
          IF (CHAINE(1:ILONG).EQ.MOTPRO(I)(1:ILONG)) THEN
            NUMERO = I
            EXIT
          ENDIF
        ENDIF
      ENDDO ! I
      !
      END SUBROUTINE
      ! brief Return the id of the given keyword -1 if it is not in the
      ! dictionary
      !
      ! param key_name Name of the keyword
      ! param lng language of the key (1:french,2:english)
      !
      ! return The key of the keyword -1 otherwise
      INTEGER FUNCTION IDENTIFY_KEYWORD(KEY_NAME,LNG)
      IMPLICIT NONE
      !
      CHARACTER(LEN=KEYWORD_LEN),INTENT(IN) :: KEY_NAME
      INTEGER, INTENT(IN) :: LNG
      !
      INTEGER I
      !
      ! Looping on each sub rubrique
      IDENTIFY_KEYWORD = -1
      DO I=1,NKEY
        IF(MYDICO(I)%KNOM(LNG).EQ.KEY_NAME) THEN
          IDENTIFY_KEYWORD = I
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION
      ! brief Fill the myDico structure by reading the condition
      ! dependencies file
      !
      ! param filename name of the dependencies file
      SUBROUTINE READ_DEPENDENCIES(FILENAME)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: FILENAME
      !
      INTEGER NFIC, NDEP, IKEY, IKEY_DEP, I, ICOND, IERR
      CHARACTER(LEN=KEYWORD_LEN) KEY_NAME
      CHARACTER(LEN=COND_LEN) COND
      CHARACTER(LEN=CONSIGNE_LEN) CONSIGNE
      CHARACTER(LEN=STRING_LEN) RUB
      INTEGER LEVEL
      !
      NFIC = 667
      WRITE(*,*) '---- READING DEPENDENCIES ----'
      WRITE(*,*) 'READING: ',TRIM(FILENAME)
      OPEN(NFIC,FILE=FILENAME,IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'OPEN:DEPEN')
      !
      DO
        READ(NFIC,*,IOSTAT=IERR) NDEP, ICOND
        IF(IERR.LT.0) EXIT
        IF(NDEP.EQ.666) EXIT
        READ(NFIC,'(A)') COND
        READ(NFIC,'(A)') KEY_NAME
        IKEY =  IDENTIFY_KEYWORD(KEY_NAME,2)
        IF(IKEY.EQ.-1) THEN
          WRITE(*,*) 'UNKNOWN KEYWORD:'
          WRITE(*,*) TRIM(KEY_NAME)
          CALL PLANTE(1)
          STOP
        ENDIF
        ! If condition number is negative its a consigne
        IF(ICOND.LT.0) THEN
          READ(NFIC,'(A)') CONSIGNE
          MYDICO(IKEY)%COND(-ICOND) = COND
          MYDICO(IKEY)%CONSIGNE(-ICOND,LNG_FR) = CONSIGNE
          READ(NFIC,'(A)') CONSIGNE
          MYDICO(IKEY)%CONSIGNE(-ICOND,LNG_EN) = CONSIGNE
          MYDICO(IKEY)%DEPEN(-ICOND,1) = -1
        ! If it is positive its a dependencies
        ELSE IF(ICOND.NE.0) THEN
          MYDICO(IKEY)%COND(ICOND) = COND
          DO I=1,NDEP-1
            READ(NFIC,'(A)') KEY_NAME
            IKEY_DEP =  IDENTIFY_KEYWORD(KEY_NAME,2)
            IF(IKEY_DEP.EQ.-1) THEN
              WRITE(*,*) 'UNKNOWN KEYWORD:'
              WRITE(*,*) TRIM(KEY_NAME)
              CALL PLANTE(1)
              STOP
            ENDIF
            MYDICO(IKEY)%DEPEN(ICOND,I) = IKEY_DEP
          ENDDO
        ! If it is zero it is an internal dependancies
        ELSE
          MYDICO(IKEY)%COND(1) = COND
        ENDIF
      ENDDO
      ! Reading RUBRIQUE status information
      IF(NDEP.EQ.666) THEN
        RUBRIQUE_INFO(:,:) = 'o'
        DO
          READ(NFIC,'(A)',IOSTAT=IERR) RUB
          IF(IERR.LT.0) EXIT
          CALL IDENTIFY_RUBRIQUE_IDX(RUB,I,LEVEL)
          IF(I.EQ.-1) THEN
            WRITE(LU,*) 'RUBRIQUE UNKNOWN IN DEPENDANCIES FILE: ',
     &                  TRIM(RUB)
            CALL PLANTE(1)
          ENDIF
          READ(NFIC,'(A)') RUBRIQUE_INFO(I,LEVEL)
        ENDDO
      ENDIF
      END SUBROUTINE READ_DEPENDENCIES
      ! brief Fill the myDico structure by reading the dictionary
      !
      ! param filename name of the dictionary file
      SUBROUTINE READ_DICTIONARY(FILENAME)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: FILENAME
      CHARACTER(LEN=AIDE_LEN) :: TMP
      INTEGER :: IKEY,IERR,LNG,I,J
      !
      INTEGER          LCAR,ICOL,JCOL,ILONG,NUMERO,I2
      INTEGER          NBMOT,LONGU
      INTEGER          ORDRE
      INTEGER          NIGN
      LOGICAL          DYNAM,AIDLNG,VUMOT
      LOGICAL          ARRET,EXECMD
      CHARACTER(LEN=1)      PTVIRG,QUOTE
      CHARACTER(LEN=9)      TYPE
      CHARACTER(LEN=72)    LIGNE
      CHARACTER(LEN=PATH_LEN)    TYPE2
      CHARACTER(LEN=AIDE_LEN) :: TEMP
      !
!
      CHARACTER(LEN=PATH_LEN),EXTERNAL :: MYCARLU
      CHARACTER(LEN=AIDE_LEN), EXTERNAL :: MYAIDELU
      INTEGER, EXTERNAL :: INTLU
      LOGICAL, EXTERNAL :: LOGLU
      DOUBLE PRECISION, EXTERNAL  :: REALU
      INTEGER, EXTERNAL :: NEXT,PREV,PREVAL,LONGLU
      LOGICAL FAIL
      INTEGER ERR_LEVEL, ERR_LNG
      !
!
      NFIC = 666
      WRITE(*,*) '---- READING PROCESS ----'
      WRITE(*,*) 'READING: ',TRIM(FILENAME)
      OPEN(NFIC,FILE=FILENAME,IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'OPEN:DICO')
      !
      ARRET   = .FALSE.
      ERREUR  = .FALSE.
      RETOUR  = .FALSE.
      DYNAM   = .FALSE.
      EXECMD  = .FALSE.
      AIDLNG  = .FALSE.
      VUMOT   = .FALSE.
      LONGLI  = 72
      PTVIRG  = ';'
      QUOTE   = ''''
!     TABUL   = CHAR(9)
      NBMOT   = 0
      NIGN    = 0
      ORDRE   = 0
      PARAM  = ' '
      LONGU  = 0
      NTYP   = -100
      INDX   =  123456
      ITAI   = -100
      DEFLU  = 0
!
      ICOL   = LONGLI
      NLIGN = 0
      FAIL = .FALSE.
!
! SEEKS THE FIRST NON-WHITE CHARACTER (IGNORES COMMENTED LINES) :
!
      ICOL = NEXT(ICOL+1,LIGNE)
      IKEY = 0
!
100   CONTINUE
      ! New keyword
!
! IF REACHED THE END OF FILE :
!
      IF(RETOUR) GO TO 900
!
! LOCATES THE COMMANDS STARTING WITH &
!
      IF ( LIGNE(ICOL:ICOL).EQ.'&' ) THEN
        ICOL = PREVAL(ICOL+1,LIGNE,' ',CHAR(10),' ')
        ICOL = NEXT(ICOL+1,LIGNE)
        GO TO 100
      ENDIF
!
      I2 = PREVAL(ICOL+1,LIGNE,'=',':','=')
!     CASE WHERE '=' IS ON THE FOLLOWING LINE
      IF(I2.GT.LONGLI) I2=LONGLI
      JCOL = PREV  (I2,LIGNE)
      ILONG = JCOL - ICOL + 1
!
      ! Identify the type of keyword
      CALL IDENTIFY_KEY(LIGNE,ILONG,NUMERO,LNG)
!
!     STOPS IF THE WORD IS UNKNOWN
      IF(NUMERO.LE.0) THEN
        WRITE(*,*) 'UNKNOWN KEY: ',LIGNE(ICOL:JCOL)
        CALL PLANTE(1)
        STOP
      ENDIF
!     New keyword
      IF((NUMERO.EQ.1).AND.(LNG.EQ.LNG_FR)) THEN
        IKEY = IKEY + 1
        ! Initialising the keyword structure
        MYDICO(IKEY)%KNOM(1) = REPEAT(' ',KEYWORD_LEN)
        MYDICO(IKEY)%KNOM(2) = REPEAT(' ',KEYWORD_LEN)
        MYDICO(IKEY)%KTYPE = 0
        MYDICO(IKEY)%KINDEX = -1
        MYDICO(IKEY)%MNEMO = REPEAT(' ',STRING_LEN)
        MYDICO(IKEY)%TAILLE = 1
        MYDICO(IKEY)%SUBMIT = REPEAT(' ',STRING_LEN)
        MYDICO(IKEY)%DEFAUT(1) = REPEAT(' ',DEFAUT_LEN)
        MYDICO(IKEY)%DEFAUT(2) = REPEAT(' ',DEFAUT_LEN)
        MYDICO(IKEY)%DEFAUT(1) = 'OBLIGATOIRE'
        MYDICO(IKEY)%DEFAUT(2) = 'MANDATORY'
        MYDICO(IKEY)%CHOIX(1) = REPEAT(' ',CHOIX_LEN)
        MYDICO(IKEY)%CHOIX(2) = REPEAT(' ',CHOIX_LEN)
        DO I=1,MAXENUM
          MYDICO(IKEY)%HASH_ID(I,1) = REPEAT(' ',KEYWORD_LEN)
          MYDICO(IKEY)%HASH_VAL(I,1) = REPEAT(' ',KEYWORD_LEN)
          MYDICO(IKEY)%HASH_ID(I,2) = REPEAT(' ',KEYWORD_LEN)
          MYDICO(IKEY)%HASH_VAL(I,2) = REPEAT(' ',KEYWORD_LEN)
        ENDDO
        DO I=1,3
          MYDICO(IKEY)%RUBRIQUE(1,I) = REPEAT(' ',STRING_LEN)
          MYDICO(IKEY)%RUBRIQUE(2,I) = REPEAT(' ',STRING_LEN)
        ENDDO
        DO I=1,MAXCOND
          MYDICO(IKEY)%COND(I) = REPEAT(' ',COND_LEN)
          MYDICO(IKEY)%CONSIGNE(I,1) = REPEAT(' ',CONSIGNE_LEN)
          MYDICO(IKEY)%CONSIGNE(I,2) = REPEAT(' ',CONSIGNE_LEN)
        ENDDO
        MYDICO(IKEY)%SUBMIT = REPEAT(' ',STRING_LEN)
        MYDICO(IKEY)%COMPOSE = REPEAT(' ',STRING_LEN)
        MYDICO(IKEY)%COMPORT = REPEAT(' ',STRING_LEN)
        MYDICO(IKEY)%CONTROLE = REPEAT(' ',STRING_LEN)
        MYDICO(IKEY)%APPARENCE = REPEAT(' ',STRING_LEN)
        MYDICO(IKEY)%NIVEAU = 0
        MYDICO(IKEY)%AIDE(1) = REPEAT(' ',AIDE_LEN)
        MYDICO(IKEY)%AIDE(2) = REPEAT(' ',AIDE_LEN)
        MYDICO(IKEY)%DEPEN(:,:) = 0
        MYDICO(IKEY)%PATHNODE = REPEAT(' ',AIDE_LEN)
        MYDICO(IKEY)%ALLOC = REPEAT(' ',AIDE_LEN)
        MYDICO(IKEY)%ALLOCED = REPEAT(' ',AIDE_LEN)
        MYDICO(IKEY)%ALLOCED2 = REPEAT(' ',AIDE_LEN)
      ENDIF
!
      ICOL = PREVAL(ICOL+1,LIGNE,'=',':','=')
!     CASE WHERE '=' IS ON THE FOLLOWING LINE
      IF(ICOL.GT.LONGLI) THEN
        ICOL  = NEXT(LONGLI,LIGNE)
        IF(RETOUR) GO TO 900
      ENDIF
!
!
!    2) RESERVED KEYWORDS:
!
!    RESERVED KEYWORDS CURRENTLY ARE:
!
!           'NOM'       :NUMERO = 1  (DE TYPE CARACTERE)
!           'TYPE'      :NUMERO = 2  (DE TYPE CARACTERE)
!           'INDEX'     :NUMERO = 3  (DE TYPE ENTIER)
!           'TAILLE'    :NUMERO = 4  (DE TYPE ENTIER)
!           'DEFAUT'    :NUMERO = 5  (DE TYPE VARIABLE)
!           'AIDE'      :NUMERO = 6  (DE TYPE CARACTERE)
!           'CHOIX'     :NUMERO = 7  (DE TYPE VARIABLE)
!           'RUBRIQUE'  :NUMERO = 8  (DE TYPE CARACTERE)
!           'NIVEAU'    :NUMERO = 9  (DE TYPE ENTIER)
!           'MNEMO'     :NUMERO = 10 (DE TYPE CARACTERE)
!           'COMPOSE'   :NUMERO = 11 (DE TYPE CARACTERE)
!           'COMPORT'   :NUMERO = 12 (DE TYPE CARACTERE)
!           'CONTROLE'  :NUMERO = 13 (DE TYPE ENTIER)
!           'APPARENCE' :NUMERO = 14 (DE TYPE CARACTERE)
!           'SUBMIT'    :NUMERO = 15 (DE TYPE CARACTERE)
!           'PATHNODE'  :NUMERO = 16 (DE TYPE CARACTERE)
!
!      NAME
!
          IF(NUMERO.EQ.1) THEN
!
! SHOULD NOT COUNT THE SAME WORD IN SEVERAL LANGUAGES SEVERAL TIMES
! COUNTED ONLY ONCE IN FIRST FOUND LANGUAGE
!
            IF (.NOT.(VUMOT)) NBMOT = NBMOT + 1
!
            ORDRE = 1
!
! SIGNALS THAT THIS NEW KEYWORD WAS ALREADY ENCOUNTERED IN ANOTHER LANGUAGE
            IF (.NOT.(VUMOT)) VUMOT=.TRUE.
!
!           NAME OF THE KEYWORD
            TMP= MYCARLU(LCAR,ICOL,LIGNE,QUOTE,LEN(PARAM))
            LONGU = LCAR
            MYDICO(IKEY)%KNOM(LNG)=TMP(1:MIN(72,LONGU))
!
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    TYPE
!
          ELSE IF(NUMERO.EQ.2) THEN
            VUMOT = .FALSE.
            IF (ORDRE.NE.1) GOTO 1500
            ORDRE=2
            TYPE2= MYCARLU(LCAR,ICOL,LIGNE,QUOTE,LEN(TYPE))
            TYPE=TYPE2(1:MIN(LCAR,9))
            IF(TYPE(1:6).EQ.'ENTIER'
     &      .OR.TYPE(1:8).EQ.'INTEGER') THEN
              NTYP = 1
            ELSEIF(TYPE(1:4).EQ.'REEL'
     &      .OR.TYPE(1:4).EQ.'REAL') THEN
              NTYP = 2
            ELSEIF(TYPE(1:7).EQ.'LOGIQUE'
     &      .OR.TYPE(1:7).EQ.'LOGICAL') THEN
              NTYP = 3
            ELSEIF(TYPE(1:9).EQ.'CARACTERE'
     &      .OR.TYPE(1:6).EQ.'STRING') THEN
              NTYP = 4
            ELSE
!           ERROR: UNKNOWN TYPE
              WRITE (LU,1003) LIGNE
1003          FORMAT(1X,A72,/,1X,'UNKNOWN TYPE ON THIS LINE')
              CALL PLANTE(1)
              STOP
            ENDIF
            MYDICO(IKEY)%KTYPE = NTYP
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    INDEX
!
          ELSE IF(NUMERO.EQ.3) THEN
            IF (ORDRE.NE.2) GOTO 1500
            ORDRE=3
            INDX = INTLU(ICOL,LIGNE)
            ICOL = NEXT(ICOL+1,LIGNE)
!
! CASE INDEX=-1 : WORD FOR EDAMOX CONSTRUCTION, TO KEEP
!
            IF (INDX.EQ.-1) THEN
              NIGN = NIGN + 1
              IF (NIGN.GT.100) THEN
                WRITE(LU,*) 'TOO MANY WORDS FOR EDAMOX',
     &                      ' (MAX=100)'
                ERREUR = .TRUE.
                GO TO 900
              ENDIF
            ENDIF
            MYDICO(IKEY)%KINDEX = INDX
!
!    SIZE
!
          ELSE IF(NUMERO.EQ.4) THEN
            IF (ORDRE.NE.3) GOTO 1500
            ORDRE=4
            MYDICO(IKEY)%TAILLE = INTLU(ICOL,LIGNE)
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    DEFAULT VALUE
!    FOR ARRAYS, IT IS NOT NECESSARY TO SET ALL VALUES
!
          ELSE IF(NUMERO.EQ.5) THEN
!
!
            IF (ORDRE.LT.3.OR.ORDRE.GT.6) GOTO 1500
            ORDRE=6
            MYDICO(IKEY)%DEFAUT(LNG) = MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%DEFAUT(LNG)))

!
            ICOL = NEXT(ICOL+1,LIGNE)

            DO
              IF(MYDICO(IKEY)%TAILLE.LE.1) EXIT
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%DEFAUT(LNG) = TRIM(MYDICO(IKEY)%DEFAUT(LNG))
     &            //PTVIRG//   MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%DEFAUT(LNG)))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
!    HELP
!
          ELSE IF(NUMERO.EQ.6) THEN
!
            MYDICO(IKEY)%AIDE(LNG) =
     &               MYAIDELU(ICOL,LIGNE)
!
!
!    'CHOIX' 'RUBRIQUE' 'NIVEAU' 'MNEMO' 'COMPOSE' 'COMPORT' 'CONTROLE' 'APPARENCE'
!    NUMBER 7 TO 14 INCLUDED
!
          ELSE IF(NUMERO.EQ.7) THEN
!
            MYDICO(IKEY)%CHOIX(LNG) = MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%CHOIX(LNG)))

!
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%CHOIX(LNG) =
     &            TRIM(MYDICO(IKEY)%CHOIX(LNG))
     &            //PTVIRG//MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%CHOIX(LNG)))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
          ELSE IF(NUMERO.EQ.8) THEN
!
            I = 1
            MYDICO(IKEY)%RUBRIQUE(LNG,I)= MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%RUBRIQUE(LNG,I)))

!
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              I = I + 1
              MYDICO(IKEY)%RUBRIQUE(LNG,I) =
     &            MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%RUBRIQUE(LNG,I)))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
          ELSE IF(NUMERO.EQ.9) THEN
!
            MYDICO(IKEY)%NIVEAU = INTLU(ICOL,LIGNE)
            ICOL = NEXT(ICOL+1,LIGNE)
!
          ELSE IF(NUMERO.EQ.10) THEN
!
            MYDICO(IKEY)%MNEMO =
     &               MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%MNEMO))
            ICOL = NEXT(ICOL+1,LIGNE)
!
          ELSE IF(NUMERO.EQ.11) THEN
!
            MYDICO(IKEY)%COMPOSE =
     &               MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%COMPOSE))
            ICOL = NEXT(ICOL+1,LIGNE)
!
          ELSE IF(NUMERO.EQ.12) THEN
!
            TEMP = MYAIDELU(ICOL,LIGNE)
            MYDICO(IKEY)%COMPORT = TEMP(1:STRING_LEN)
!
          ELSE IF(NUMERO.EQ.13) THEN
!
            MYDICO(IKEY)%CONTROLE = MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%CONTROLE))

!
            ICOL = NEXT(ICOL+1,LIGNE)

            ! Reading second value
            DO
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%CONTROLE = TRIM(MYDICO(IKEY)%CONTROLE)
     &            //PTVIRG//   MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%CONTROLE))
            ENDDO

!
            ICOL = NEXT(ICOL+1,LIGNE)
!
          ELSE IF(NUMERO.EQ.14) THEN
!
            MYDICO(IKEY)%APPARENCE =
     &               MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%APPARENCE))
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%APPARENCE =
     &            TRIM(MYDICO(IKEY)%SUBMIT)
     &            //PTVIRG//MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%APPARENCE))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
!    DEFINES A SUBMIT TYPE
          ELSE IF (NUMERO .EQ. 15) THEN
            IF (ORDRE.NE.3.AND.ORDRE.NE.4) GOTO 1500
            ORDRE=5
!
!
            MYDICO(IKEY)%SUBMIT = MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                     LEN(MYDICO(IKEY)%SUBMIT))

!
            ICOL = NEXT(ICOL+1,LIGNE)
            DO
              ! Check if there are multiple values (array)
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).NE.PTVIRG) THEN
                  EXIT
                ENDIF
              ENDIF
              MYDICO(IKEY)%SUBMIT =
     &            TRIM(MYDICO(IKEY)%SUBMIT)
     &            //PTVIRG//MYCARLU(LCAR,ICOL,LIGNE,QUOTE,
     &                       LEN(MYDICO(IKEY)%SUBMIT))

!
              ICOL = NEXT(ICOL+1,LIGNE)
            ENDDO
!
          ELSE IF(NUMERO.EQ.16) THEN
!
            MYDICO(IKEY)%PATHNODE =
     &               MYAIDELU(ICOL,LIGNE)
!
          ELSE IF(NUMERO.EQ.17) THEN
!
            MYDICO(IKEY)%ALLOC =
     &               MYAIDELU(ICOL,LIGNE)
!
          ELSE IF(NUMERO.EQ.18) THEN
!
            MYDICO(IKEY)%ALLOCED =
     &               MYAIDELU(ICOL,LIGNE)
!
          ELSE IF(NUMERO.EQ.19) THEN
!
            MYDICO(IKEY)%ALLOCED2 =
     &               MYAIDELU(ICOL,LIGNE)
!
        ENDIF
!
!
      GO TO 100
900   CONTINUE
      IF(ERREUR) THEN
        WRITE(LU,*)' '
        WRITE(LU,*)'-------------------------------'
        WRITE(LU,*)'- ERROR IN THE DICTIONARY     -'
        WRITE(LU,*)'-------------------------------'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     TRUE END: 2 FILES READ OR 2 FILES IN 1 READ
      CLOSE(NFIC)
      WRITE(*,*) ''
      WRITE(*,*) 'TOTAL NUMBER OF KEY IN THE DICTIONARY: ',IKEY
      WRITE(*,*) ''
      NKEY = IKEY

      ! List all rubriques
      CALL IDENTIFY_RUBRIQUE()
      ! Verification for user
      WRITE(*,*) '---- CHECKING RUBRIQUES ----'
      WRITE(*,*) 'CHECK OF TRANSLATION FOR RUBRIQUE'
      WRITE(*,*) 'NRUB',NRUB
      WRITE(*,*) 'LIST OF RUBRIQUES IN BOTH LANGUAGES'
      DO J=1,3
        WRITE(*,*) 'LEVEL:',J
        IF(NRUB(1,J).NE.NRUB(2,J)) THEN
          WRITE(*,*) 'WARNING: NOT THE SAME NUMBER OF RUBRIQUES ',
     &               'IN FRENCH AND ENGLISH'
          ERR_LEVEL = J
          IF(NRUB(1,ERR_LEVEL).GT.NRUB(2,ERR_LEVEL)) THEN
            ERR_LNG = 1
          ELSE
            ERR_LNG = 2
          ENDIF
          FAIL = .TRUE.
        ENDIF
        DO I=1,MINVAL(NRUB(:,J))
          WRITE(*,*) REPEAT('-',J*2),
     &          TRIM(RUBRIQUE(1,I,J))," = ",TRIM(RUBRIQUE(2,I,J))
        ENDDO
      ENDDO
      IF(FAIL) THEN
        WRITE(*,*) 'ERROR IN RUBRIQUE LEVEL: ',ERR_LEVEL
        DO I=1,NRUB(ERR_LNG, ERR_LEVEL)
          WRITE(*,*) TRIM(RUBRIQUE(ERR_LNG, I, ERR_LEVEL))
        ENDDO
        CALL PLANTE(1)
        STOP
      ENDIF
      WRITE(*,*) ''
      CALL CHECK_INDEX(6)
      CALL IDENTIFY_RUBRIQUE_DEPENDS()
!
      RETURN
!
! TREATS ERRORS OF DECLARATION ORDER IN THE DICTIONARY
!
1500  ERREUR=.TRUE.
      WRITE(LU,'(/,1X,A72,/)') LIGNE
      WRITE(LU,*) 'AT LINE ',NLIGN,', PRIORITY ORDER NOT RESPECTED'
      WRITE(LU,*)
      WRITE(LU,*) 'EXPECTED ORDER IS :'
      WRITE(LU,*) 'NOM, TYPE, INDEX, (TAILLE), (SUBMIT), (DEFAUT)'
      CALL PLANTE(1)
      STOP
      GOTO 900

      END SUBROUTINE
      ! brief transform single quote into double quotes
      !
      ! param strIn Input string
      ! return String with quotes instead of double quotes
      CHARACTER(LEN=KEYWORD_LEN) FUNCTION DBLE_QUOTE(STRIN)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=KEYWORD_LEN), INTENT(IN) :: STRIN
      !
      INTEGER I,J
      !
      J = 1
      DBLE_QUOTE = REPEAT(' ',KEYWORD_LEN)
      DO I=1,LEN(TRIM(STRIN))
        IF (STRIN(I:I).EQ."'") THEN
          DBLE_QUOTE(J:J) = "'"
          J=J+1
        ENDIF
        DBLE_QUOTE(J:J) = STRIN(I:I)
        J=J+1
      ENDDO
      !
      END FUNCTION DBLE_QUOTE
      ! brief Dump a keyword structure
      !
      ! param ndic Id of the file
      ! param ikey Id of the keyword
      SUBROUTINE DUMP_KEYWORD(NFIC,IKEY)
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: NFIC
      INTEGER, INTENT(IN) :: IKEY
      !
      INTEGER :: J
      CHARACTER(LEN=10), EXTERNAL :: I2STR
      !
      WRITE(NFIC,'(3A)') "NOM = '",
     &                   TRIM(DBLE_QUOTE(MYDICO(IKEY)%KNOM(1))),"'"
      WRITE(NFIC,'(3A)') "NOM1 = '",TRIM(MYDICO(IKEY)%KNOM(2)),"'"
      SELECT CASE(MYDICO(IKEY)%KTYPE)
      CASE(1) ! INTEGER
      WRITE(NFIC,'(A)') "TYPE = INTEGER"
      CASE(2) ! REAL
      WRITE(NFIC,'(A)') "TYPE = REAL"
      CASE(3) ! LOGICAL
      WRITE(NFIC,'(A)') "TYPE = LOGICAL"
      CASE(4) ! CHARACTER
      WRITE(NFIC,'(A)') "TYPE = STRING"
      END SELECT
      WRITE(NFIC,'(A)') "INDEX = "//TRIM(I2STR(MYDICO(IKEY)%KINDEX))
      IF(MYDICO(IKEY)%TAILLE.NE.-1) THEN
        WRITE(NFIC,'(A)') "TAILLE = "
     &                       //TRIM(I2STR(MYDICO(IKEY)%TAILLE))
      ENDIF
      IF(MYDICO(IKEY)%SUBMIT(1:1).NE." ") THEN
        WRITE(NFIC,'(3A)') "SUBMIT = '",TRIM(MYDICO(IKEY)%SUBMIT),"'"
      ENDIF
      ! If default = obligatoire IKEY.e no default value
      IF(MYDICO(IKEY)%DEFAUT(1)(1:11).NE.'OBLIGATOIRE') THEN
        CALL DUMP_LIST(NFIC,MYDICO(IKEY)%KTYPE,MYDICO(IKEY)%DEFAUT(1),
     &                 DEFAUT_LEN,'DEFAUT',6)
        CALL DUMP_LIST(NFIC,MYDICO(IKEY)%KTYPE,MYDICO(IKEY)%DEFAUT(2),
     &                 DEFAUT_LEN,'DEFAUT1',7)
      ENDIF
      WRITE(NFIC,'(3A)') "MNEMO = '",TRIM(MYDICO(IKEY)%MNEMO),"'"
      IF(MYDICO(IKEY)%CONTROLE(1:1).NE.' ') THEN
        WRITE(NFIC,'(A,A)') "CONTROLE = ",
     &                     TRIM(MYDICO(IKEY)%CONTROLE)
      ENDIF
      IF(MYDICO(IKEY)%CHOIX(1)(1:1).NE.' ') THEN
        ! If missing english choix crashing
        IF(MYDICO(IKEY)%CHOIX(2)(1:1).EQ.' ') THEN
          WRITE(*,*) 'MISSING CHOIX FOR',MYDICO(IKEY)%KNOM(1)
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL DUMP_LIST(NFIC,MYDICO(IKEY)%KTYPE,MYDICO(IKEY)%CHOIX(1),
     &                 CHOIX_LEN,'CHOIX',5)
        CALL DUMP_LIST(NFIC,MYDICO(IKEY)%KTYPE,MYDICO(IKEY)%CHOIX(2),
     &                 CHOIX_LEN,'CHOIX1',6)
      ENDIF
      IF(MYDICO(IKEY)%APPARENCE(1:1).NE.' ') THEN
        WRITE(NFIC,'(A)') "APPARENCE ="
        WRITE(NFIC,'(3A)') "'",TRIM(MYDICO(IKEY)%APPARENCE),"'"
      ENDIF
      WRITE(NFIC,'(A)') "RUBRIQUE ="
      WRITE(NFIC,'(2(3A),3A)')
     &           ("'",TRIM(MYDICO(IKEY)%RUBRIQUE(1,J)),"';",J=1,2),
     &           "'",TRIM(MYDICO(IKEY)%RUBRIQUE(1,3)),"'"
      WRITE(NFIC,'(A)') "RUBRIQUE1 ="
      WRITE(NFIC,'(2(3A),3A)')
     &           ("'",TRIM(MYDICO(IKEY)%RUBRIQUE(2,J)),"';",J=1,2),
     &           "'",TRIM(MYDICO(IKEY)%RUBRIQUE(2,3)),"'"
      IF(MYDICO(IKEY)%COMPOSE(1:1).NE." ") THEN
        WRITE(NFIC,'(3A)') "COMPOSE = '",
     &        TRIM(MYDICO(IKEY)%COMPOSE),"'"
      ENDIF
      IF(MYDICO(IKEY)%COMPORT(1:1).NE." ") THEN
        WRITE(NFIC,'(A)') "COMPORT ="
        WRITE(NFIC,'(3A)') "'",TRIM(MYDICO(IKEY)%COMPORT),"'"
      ENDIF
      WRITE(NFIC,'(A,A)') "NIVEAU = ",TRIM(I2STR(MYDICO(IKEY)%NIVEAU))
      WRITE(NFIC,'(A)') "AIDE ="
      WRITE(NFIC,'(3A)') "'",TRIM(MYDICO(IKEY)%AIDE(1)),"'"
      WRITE(NFIC,'(A)') "AIDE1 ="
      WRITE(NFIC,'(3A)') "'",TRIM(MYDICO(IKEY)%AIDE(2)),"'"
      WRITE(NFIC,'(A)') "/"
      !
      END SUBROUTINE
      ! brief Dump the myDico structure in index order
      !
      ! param filename name of the output file
      ! param
      SUBROUTINE DUMP_DICTIONARY_INDEX(FILENAME)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: FILENAME
      INTEGER :: NFIC,IERR
      INTEGER :: ITYP, IKEY
      INTEGER :: IDX(1)
      INTEGER, ALLOCATABLE :: INDEX_TYP(:,:)
      INTEGER :: LNG
!
      NFIC = 666
      LNG = 2
      WRITE(*,*) '---- DUMPING PROCESS ----'
      WRITE(*,*) 'DUMPING IN : ',TRIM(FILENAME)
      OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'DUMP_DICTIONARY')
      WRITE(*,*) ''
      WRITE(*,*) 'TOTAL NUMBER OF KEY IN THE DICTIONARY: ',NKEY
      WRITE(*,*) ''
      ! Loop on all the keywords
      WRITE(NFIC,'(A)') '&DYN'
      ! Loop on rubriques
      ALLOCATE(INDEX_TYP(NKEY,4),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'INDEX_TYP')
      INDEX_TYP(:,:) = NKEY*2
      DO IKEY=1,NKEY
        INDEX_TYP(IKEY,MYDICO(IKEY)%KTYPE) = MYDICO(IKEY)%KINDEX
      ENDDO
      DO ITYP=1,4
        IDX = MINLOC(INDEX_TYP(:,ITYP))
        DO WHILE(INDEX_TYP(IDX(1),ITYP).LT.NKEY*2)
          CALL DUMP_KEYWORD(NFIC,IDX(1))
          INDEX_TYP(IDX(1),ITYP) = NKEY*2
          IDX = MINLOC(INDEX_TYP(:,ITYP))
        ENDDO
      ENDDO
      CLOSE(NFIC)
      DEALLOCATE(INDEX_TYP)
      END SUBROUTINE
      ! brief Dump the myDico structure
      !
      ! param filename name of the output file
      ! param
      SUBROUTINE DUMP_DICTIONARY_RUB(FILENAME)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: FILENAME
      INTEGER :: NFIC,IERR
      INTEGER :: IRUB1, IRUB2, IRUB3
      INTEGER :: IDX_RUB2, IDX_RUB3
      INTEGER :: LNG,IKEY
      CHARACTER(LEN=10), EXTERNAL :: I2STR
!
      NFIC = 666
      LNG = 2
      WRITE(*,*) '---- DUMPING PROCESS ----'
      WRITE(*,*) 'DUMPING IN : ',TRIM(FILENAME)
      OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'DUMP_DICTIONARY')
      WRITE(*,*) ''
      WRITE(*,*) 'TOTAL NUMBER OF KEY IN THE DICTIONARY: ',NKEY
      WRITE(*,*) ''
      ! Loop on all the keywords
      WRITE(NFIC,'(A)') '&DYN'
      WRITE(NFIC,'(A)') "/ Description of a keyword"
      WRITE(NFIC,'(A)') "/ NOM : French name"
      WRITE(NFIC,'(A)') "/ NOM1 : English name"
      WRITE(NFIC,'(A)') "/ TYPE : STRING, INTEGER, REAL, LOGICAL"
      WRITE(NFIC,'(A)') "/ INDEX : Index of the keyword"
      WRITE(NFIC,'(A)') "/ TAILLE : Number of values "//
     &                  "(0 means between 1 and n)"
      WRITE(NFIC,'(A)') "/ (opt) SUBMIT : Chain for files"
      WRITE(NFIC,'(A)') "/ DEFAUT : Defaut value in French"
      WRITE(NFIC,'(A)') "/ DEFAUT1 : Defaut value in English"
      WRITE(NFIC,'(A)') "/ MNEMO : Name of the variable in the code"
      WRITE(NFIC,'(A)') "/ (opt) CONTROLE : min;max for the "//
     &                  "keyword value"
      WRITE(NFIC,'(A)') "/ (opt) CHOIX : List of possible values"
      WRITE(NFIC,'(A)') "/ (opt) CHOIX1 : CHOIX in french"
      WRITE(NFIC,'(A)') "/ (opt) APPARENCE : Defined how the keyword "//
     &                  "is filled"
      WRITE(NFIC,'(A)') "/   LIST, DYNLIST, TUPLE"
      WRITE(NFIC,'(A)') "/ RUBRIQUE : Classification of the "//
     &                  "keyword 3 level max"
      WRITE(NFIC,'(A)') "/ RUBRIQUE1 : RUBRIQUE but in English"
      WRITE(NFIC,'(A)') "/ (opt) COMPOSE : Used for fudaa"
      WRITE(NFIC,'(A)') "/ (opt) COMPORT : Used for fudaa"
      WRITE(NFIC,'(A)') "/ NIVEAU : Level of the keyword "//
     &                  "(Level 0 is a mandatory keyword)"
      WRITE(NFIC,'(A)') "/ AIDE : Help in French (LaTeX syntax)"
      WRITE(NFIC,'(A)') "/ AIDE1 : Help in English (LaTeX syntax)"
      CALL CHECK_INDEX(NFIC)
      ! Loop on rubriques
      DO IRUB1=1,NRUB(LNG,1)
        IDX_RUB2 = 0
        WRITE(NFIC,'(2A)') '/',REPEAT('/',71)
        WRITE(NFIC,'(3A,A,2A)')'/',REPEAT('//',1),
     &                  ' ',TRIM(I2STR(IRUB1)),'-',
     &                  TRIM(RUBRIQUE(2,IRUB1,1))
        WRITE(NFIC,'(2A)') '/',REPEAT('/',71)
        DO IKEY=1,NKEY
          ! Identifying keywwords that are 1 1
          IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &       (MYDICO(IKEY)%RUBRIQUE(LNG,2)(1:1).EQ.' ')) THEN
            CALL DUMP_KEYWORD(NFIC,IKEY)
          ENDIF
        ENDDO
        ! LEVEL 2
        ! Loop on rubriques
        DO IRUB2=1,NRUB(LNG,2)
          IDX_RUB3 = 0
          IF(RUB1_DEP(IRUB1,IRUB2)) THEN
            IDX_RUB2 = IDX_RUB2 + 1
            WRITE(NFIC,'(2A)') '/',REPEAT('/',71)
            WRITE(NFIC,'(3A,A,A,A,2A)')'/',REPEAT('//',2),
     &                  ' ',TRIM(I2STR(IRUB1)),'.',
     &                      TRIM(I2STR(IDX_RUB2)),'-',
     &                       TRIM(RUBRIQUE(2,IRUB2,2))
            WRITE(NFIC,'(2A)') '/',REPEAT('/',71)
            DO IKEY=1,NKEY
              ! Identifying keywwords that are 2 1
              IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &           HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &           (MYDICO(IKEY)%RUBRIQUE(LNG,3)(1:1).EQ.' ')) THEN
                CALL DUMP_KEYWORD(NFIC,IKEY)
              ENDIF
            ENDDO
            ! LEVEL 3
            ! Loop on rubriques
            DO IRUB3=1,NRUB(LNG,3)
              IF(RUB2_DEP(IRUB1,IRUB2,IRUB3)) THEN
                IDX_RUB3 = IDX_RUB3 + 1
                WRITE(NFIC,'(2A)') '/',REPEAT('/',71)
                WRITE(NFIC,'(3A,A,A,A,A,A,2A)')
     &                  '/',REPEAT('//',3),
     &                  ' ',TRIM(I2STR(IRUB1)),'.',
     &                      TRIM(I2STR(IDX_RUB2)),'.',
     &                      TRIM(I2STR(IDX_RUB3)),'-',
     &                           TRIM(RUBRIQUE(2,IRUB3,3))
                WRITE(NFIC,'(2A)') '/',REPEAT('/',71)
                DO IKEY=1,NKEY
                  ! Identifying keywwords that are 3 1
                  IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &               HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &               HAS_RUBRIQUE(IKEY,IRUB3,3,LNG)) THEN
                    CALL DUMP_KEYWORD(NFIC,IKEY)
                  ENDIF
                ENDDO
              ENDIF
            ENDDO ! LEVEL 3
          ENDIF
        ENDDO ! LEVEL 2
      ENDDO ! LEVEL 1
      CLOSE(NFIC)
!
      END SUBROUTINE
      !
      END MODULE
