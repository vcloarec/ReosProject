!                       ***********************
                        PROGRAM HOMERE_DAMOCLES
!                       ***********************
      USE DICO_DATA
      USE UTILS_LATEX
      USE UTILS_CATA
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=PATH_LEN) :: DICTIONARY
      CHARACTER(LEN=PATH_LEN) :: DEPENDENCIES
      CHARACTER(LEN=PATH_LEN) :: LATEX_FILE
      CHARACTER(LEN=PATH_LEN) :: ENUM_FILE
      CHARACTER(LEN=PATH_LEN) :: TS_PATH
      CHARACTER(LEN=5) :: TODO
      CHARACTER(LEN=10) :: CODE_NAME
      INTEGER LLNG
      LOGICAL :: FILE_EXIST
!
      WRITE(6,*) 'ENTER ACTION [LATEX,CATA,DUMP]'
      READ(5,'(A)') TODO

      IF(TODO.EQ.'LATEX') THEN
        WRITE(6,*) 'ENTER DICTIONARY FILE: '
        READ(5,'(A)') DICTIONARY
        WRITE(6,*) 'DICTIONARY: ',TRIM(DICTIONARY)
        WRITE(6,*) 'ENTER LATEX FILE: '
        READ(5,'(A)') LATEX_FILE
        WRITE(6,*) 'LATEX_FILE: ',TRIM(LATEX_FILE)
        WRITE(6,*) 'ENTER LANGUAGE [1: FRENCH, 2: ENGLISH]: '
        READ(5,'(I1)') LLNG
        WRITE(6,*) 'LANGUAGE: ',LLNG
        ! Checking that the two file exist and that lng is 1 or 2
        INQUIRE(FILE=DICTIONARY,EXIST=FILE_EXIST)
        IF(.NOT.FILE_EXIST) THEN
          WRITE(6,*) 'ERROR FILE: ',TRIM(DICTIONARY),' DOES NOT EXIST'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(LNG.NE.LNG_EN.AND.LNG.NE.LNG_FR) THEN
          WRITE(6,*) 'WRONG LANGUAGE: ',LLNG
        ENDIF
        ! Reading dictionary
        CALL READ_DICTIONARY(DICTIONARY)
        ! Writing Latex file
        CALL WRITE2LATEX(LATEX_FILE,LLNG)
      ELSE IF(TODO(1:4).EQ.'CATA') THEN
        WRITE(6,*) 'ENTER CODE NAME: '
        READ(5,'(A)') CODE_NAME
        WRITE(6,*) 'ENTER DICTIONARY FILE: '
        READ(5,'(A)') DICTIONARY
        WRITE(6,*) 'DICTIONARY: ',TRIM(DICTIONARY)
        WRITE(6,*) 'ENTER DEPENDENCIES FILE: '
        READ(5,'(A)') DEPENDENCIES
        WRITE(6,*) 'DEPENDENCIES: ',TRIM(DEPENDENCIES)
        WRITE(6,*) 'ENTER CATA FILE: '
        READ(5,'(A)') LATEX_FILE
        WRITE(6,*) 'CATA: ',TRIM(LATEX_FILE)
        WRITE(6,*) 'ENTER ENUM FILE: '
        READ(5,'(A)') ENUM_FILE
        WRITE(6,*) 'ENUM: ',TRIM(ENUM_FILE)
        WRITE(6,*) 'ENTER TS PATH FILE: '
        READ(5,'(A)') TS_PATH
        WRITE(6,*) 'TS PATH: ',TRIM(TS_PATH)
        ! Checking that the two file exist and that llng is 1 or 2
        INQUIRE(FILE=DICTIONARY,EXIST=FILE_EXIST)
        IF(.NOT.FILE_EXIST) THEN
          WRITE(6,*) 'ERROR FILE: ',TRIM(DICTIONARY),' DOES NOT EXIST'
          CALL PLANTE(1)
          STOP
        ENDIF
        ! Reading dictionary
        CALL READ_DICTIONARY(DICTIONARY)
        ! Reading dependencies
        CALL READ_DEPENDENCIES(DEPENDENCIES)
        ! Writing Latex file
        CALL WRITE2CATA(LATEX_FILE, CODE_NAME)
        ! WRITING Enumerate for CHOIX and keywords
        CALL WRITE_ENUM(ENUM_FILE)
        ! WRITING TS
        CALL WRITE_TS(CODE_NAME, TS_PATH)
      ELSE IF(TODO(1:5).EQ.'DUMP2') THEN
        WRITE(6,*) 'ENTER DICTIONARY FILE: '
        READ(5,'(A)') DICTIONARY
        WRITE(6,*) 'DICTIONARY: ',TRIM(DICTIONARY)
        WRITE(6,*) 'ENTER OUTPUT FILE: '
        READ(5,'(A)') LATEX_FILE
        WRITE(6,*) 'CATA: ',TRIM(LATEX_FILE)
        ! Checking that the two file exist and that llng is 1 or 2
        INQUIRE(FILE=DICTIONARY,EXIST=FILE_EXIST)
        IF(.NOT.FILE_EXIST) THEN
          WRITE(6,*) 'ERROR FILE: ',TRIM(DICTIONARY),' DOES NOT EXIST'
          CALL PLANTE(1)
          STOP
        ENDIF
        ! Reading dictionary
        CALL READ_DICTIONARY(DICTIONARY)
        ! Writing Latex file
        CALL DUMP_DICTIONARY_INDEX(LATEX_FILE)
      ELSE IF(TODO(1:4).EQ.'DUMP') THEN
        WRITE(6,*) 'ENTER DICTIONARY FILE: '
        READ(5,'(A)') DICTIONARY
        WRITE(6,*) 'DICTIONARY: ',TRIM(DICTIONARY)
        WRITE(6,*) 'ENTER OUTPUT FILE: '
        READ(5,'(A)') LATEX_FILE
        WRITE(6,*) 'CATA: ',TRIM(LATEX_FILE)
        ! Checking that the two file exist and that llng is 1 or 2
        INQUIRE(FILE=DICTIONARY,EXIST=FILE_EXIST)
        IF(.NOT.FILE_EXIST) THEN
          WRITE(6,*) 'ERROR FILE: ',TRIM(DICTIONARY),' DOES NOT EXIST'
          CALL PLANTE(1)
          STOP
        ENDIF
        ! Reading dictionary
        CALL READ_DICTIONARY(DICTIONARY)
        ! Writing Latex file
        CALL DUMP_DICTIONARY_RUB(LATEX_FILE)
      ELSE
        WRITE(6,*) 'UNKNOWN ACTION',TODO
        CALL PLANTE(1)
        STOP
      ENDIF
      STOP 0
      END

