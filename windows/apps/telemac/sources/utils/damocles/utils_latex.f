      ! brief Contains function to write a list of keywords into a LaTeX
      !+ file (For the reference manual)
      MODULE UTILS_LATEX
      USE DICO_DATA
      IMPLICIT NONE
      CONTAINS
      ! brief Write in Latex format a Rubrique
      !
      ! param nfic File descriptor
      ! param lng Language (1: French 2:English)
      !
      SUBROUTINE WRITE_RUBRIQUE2LATEX(NFIC,LNG,ORDERED_KEY1)
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: LNG
      INTEGER, INTENT(IN) :: NFIC
      !
      INTEGER :: I1,I2,I3,J,IRUB,IKEY,IERR
      INTEGER :: IRUB1
      INTEGER :: IRUB2
      INTEGER :: IRUB3
      CHARACTER(LEN=PATH_LEN), ALLOCATABLE :: TO_SORT1(:)
      INTEGER :: ORDERED_KEY1(*)
      INTEGER, ALLOCATABLE :: ORDERED_RUB1(:)
      INTEGER, ALLOCATABLE :: ORDERED_RUB2(:)
      INTEGER, ALLOCATABLE :: ORDERED_RUB3(:)
      ALLOCATE(ORDERED_RUB1(NRUB(LNG,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_RUB')
      ALLOCATE(ORDERED_RUB2(NRUB(LNG,2)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_RUB')
      ALLOCATE(ORDERED_RUB3(NRUB(LNG,3)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_RUB')
      ! LEVEL 1
      ALLOCATE(TO_SORT1(NRUB(LNG,1)),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TO_SORT1')
      DO IRUB=1,NRUB(LNG,1)
        TO_SORT1(IRUB) = REPEAT(' ',PATH_LEN)
        TO_SORT1(IRUB) = RUBRIQUE(LNG,IRUB,1)
      ENDDO
      CALL SHELL_STRING(NRUB(LNG,1),TO_SORT1,ORDERED_RUB1)
      DEALLOCATE(TO_SORT1)
      ! Loop on rubriques
      DO I1=1,NRUB(LNG,1)
        IRUB1 = ORDERED_RUB1(I1)
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(3A)') "\section{",TRIM(RUBRIQUE(LNG,IRUB1,1)),"}"
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(A)') ' '
        DO J=1,NKEY
          ! Identifying keywwords that are level 1
          IKEY = ORDERED_KEY1(J)
          IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &       (MYDICO(IKEY)%RUBRIQUE(LNG,2)(1:1).EQ.' ')) THEN
            WRITE(NFIC,'(3a)') "\telkey{",
     &               TRIM(MYDICO(IKEY)%KNOM(LNG)),"}\\"
          ENDIF
        ENDDO
        ! LEVEL 2
        ALLOCATE(TO_SORT1(NRUB(LNG,2)),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'TO_SORT1')
        DO IRUB=1,NRUB(LNG,2)
          TO_SORT1(IRUB) = REPEAT(' ',PATH_LEN)
          TO_SORT1(IRUB) = RUBRIQUE(LNG,IRUB,2)
        ENDDO
        CALL SHELL_STRING(NRUB(LNG,2),TO_SORT1,ORDERED_RUB2)
        DEALLOCATE(TO_SORT1)
        ! Loop on rubriques
        DO I2=1,NRUB(LNG,2)
          IRUB2 = ORDERED_RUB2(I2)
          IF(RUB1_DEP(IRUB1,IRUB2)) THEN
            WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
            WRITE(NFIC,'(3A)') "\subsection{",
     &                         TRIM(RUBRIQUE(LNG,IRUB2,2)),"}"
            WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
            WRITE(NFIC,'(A)') ' '
            DO J=1,NKEY
              ! Identifying keywwords that are level 1
              IKEY = ORDERED_KEY1(J)
              IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &           HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &           (MYDICO(IKEY)%RUBRIQUE(LNG,3)(1:1).EQ.' ')) THEN
                WRITE(NFIC,'(3a)') "\telkey{",
     &                   TRIM(MYDICO(IKEY)%KNOM(LNG)),"}\\"
              ENDIF
            ENDDO
            ! LEVEL 3
            ALLOCATE(TO_SORT1(NRUB(LNG,3)),STAT=IERR)
            CALL CHECK_ALLOCATE(IERR,'TO_SORT1')
            DO IRUB=1,NRUB(LNG,3)
              TO_SORT1(IRUB) = REPEAT(' ',PATH_LEN)
              TO_SORT1(IRUB) = RUBRIQUE(LNG,IRUB,3)
            ENDDO
            CALL SHELL_STRING(NRUB(LNG,3),TO_SORT1,ORDERED_RUB3)
            DEALLOCATE(TO_SORT1)
            ! Loop on rubriques
            DO I3=1,NRUB(LNG,3)
              IRUB3 = ORDERED_RUB3(I3)
              IF(RUB2_DEP(IRUB1,IRUB2,IRUB3)) THEN
                WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
                WRITE(NFIC,'(3A)') "\subsubsection{",
     &                             TRIM(RUBRIQUE(LNG,IRUB3,3)),"}"
                WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
                WRITE(NFIC,'(A)') ' '
                DO J=1,NKEY
                  ! Identifying keywwords that are level 1
                  IKEY = ORDERED_KEY1(J)
                  IF(HAS_RUBRIQUE(IKEY,IRUB1,1,LNG).AND.
     &               HAS_RUBRIQUE(IKEY,IRUB2,2,LNG).AND.
     &               HAS_RUBRIQUE(IKEY,IRUB3,3,LNG)) THEN
                    WRITE(NFIC,'(3a)') "\telkey{",
     &                       TRIM(MYDICO(IKEY)%KNOM(LNG)),"}\\"
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO ! LEVEL 1

      END SUBROUTINE
      ! brief Writes a latex documentation from the dictionary
      !
      ! param filename name of the output file
      ! param lng Language of the output
      SUBROUTINE WRITE2LATEX(FILENAME,LNG)
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: FILENAME
      INTEGER, INTENT(IN) :: LNG
      !
      INTEGER :: I,NFIC,IERR,IKEY
      CHARACTER(LEN=PATH_LEN), ALLOCATABLE :: TO_SORT1(:),TO_SORT2(:)
      INTEGER, ALLOCATABLE :: ORDERED_KEY1(:),ORDERED_KEY2(:)
      INTEGER, ALLOCATABLE :: ORDERED_RUB(:)
!
      NFIC = 666
      WRITE(*,*) '---- LATEX PROCESS ----'
      WRITE(*,*) 'WRITING IN : ',TRIM(FILENAME)
      OPEN(NFIC,FILE=TRIM(FILENAME),IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'WRITE2LATEX')
      WRITE(*,*) ''
      WRITE(*,*) 'TOTAL NUMBER OF KEY IN THE DICTIONARY: ',NKEY
      WRITE(*,*) ''
      !
      ! First Chapter the list of all keywords with informations
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.LNG_FR) THEN
        WRITE(NFIC,'(A)') '\chapter{Liste detaillée des mots clefs}'
      ELSE
        WRITE(NFIC,'(A)') '\chapter{Detail list of keywords}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      !
      ! Sorting key words by alpahbetical order for each language
      !
      ALLOCATE(TO_SORT1(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TO_SORT1')
      ALLOCATE(TO_SORT2(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TO_SORT2')
      ALLOCATE(ORDERED_KEY1(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_KEY1')
      ALLOCATE(ORDERED_KEY2(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_KEY2')
      ALLOCATE(ORDERED_RUB(NKEY),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ORDERED_RUB')
      ! Initialising list of keywords
      DO IKEY=1,NKEY
        TO_SORT1(IKEY) = REPEAT(' ',PATH_LEN)
        TO_SORT2(IKEY) = REPEAT(' ',PATH_LEN)
        TO_SORT1(IKEY)(1:72) = MYDICO(IKEY)%KNOM(LNG)
        TO_SORT2(IKEY)(1:72) = MYDICO(IKEY)%KNOM(3-LNG)
      ENDDO
      ! Sorting
      CALL SHELL_STRING(NKEY,TO_SORT1,ORDERED_KEY1)
      CALL SHELL_STRING(NKEY,TO_SORT2,ORDERED_KEY2)
      DEALLOCATE(TO_SORT1)
      DEALLOCATE(TO_SORT2)
      ! Looping on ordered keywords
      DO I=1,NKEY
        IKEY = ORDERED_KEY1(I)
        ! Name of the keywords
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(3A)') "\section{",TRIM(MYDICO(IKEY)%KNOM(LNG)),"}"
        WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
        WRITE(NFIC,'(A)') " "
        ! The other informations are in an array
        WRITE(NFIC,'(A)') "\begin{tabular}{ll}"
        ! Type
        SELECT CASE(MYDICO(IKEY)%KTYPE)
        CASE(1) ! INTEGER
          IF(LNG.EQ.LNG_FR) THEN
            WRITE(NFIC,'(A)') "Type : & Entier\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & Integer\\"
          ENDIF
        CASE(2) ! REAL
          IF(LNG.EQ.LNG_FR) THEN
            WRITE(NFIC,'(A)') "Type : & Réel\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & Real\\"
          ENDIF
        CASE(3) ! LOGICAL
          IF(LNG.EQ.LNG_FR) THEN
            WRITE(NFIC,'(A)') "Type : & Logique\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & Logical\\"
          ENDIF
        CASE(4) ! CHARACTER
          IF(LNG.EQ.LNG_FR) THEN
            WRITE(NFIC,'(A)') "Type : & Caractère\\"
          ELSE
            WRITE(NFIC,'(A)') "Type : & String\\"
          ENDIF
        END SELECT
        ! Size
        IF(LNG.EQ.LNG_FR) THEN
          WRITE(NFIC,'(A,I2,A)') "Taille : & ",MYDICO(IKEY)%TAILLE,"\\"
        ELSE
          WRITE(NFIC,'(A,I2,A)')
     &           "Dimension : & ",MYDICO(IKEY)%TAILLE,"\\"
        ENDIF
        ! Mnemo
        WRITE(NFIC,'(A)') "Mnemo & ",TRIM(MYDICO(IKEY)%MNEMO),"\\"
        ! Default values
        CALL WRITE_DEFAULT(NFIC,IKEY,LNG)
        ! And the name of the keyword in the other language
        IF(LNG.EQ.LNG_FR) THEN
          WRITE(NFIC,'(3A)') "Mot clef anglais : & ",
     &                         TRIM(MYDICO(IKEY)%KNOM(2)),"\\"
        ELSE
          WRITE(NFIC,'(3A)') "French keyword : & \telkey{",
     &                         TRIM(MYDICO(IKEY)%KNOM(1)),"}\\"
        ENDIF
        ! Pathnode if Mascaret-Tracer-Courlis dico
        IF (TRIM(MYDICO(IKEY)%PATHNODE) .NE. "") THEN
          WRITE(NFIC,'(A)') "XML pathnode : &",
     &                      TRIM(MYDICO(IKEY)%PATHNODE),"\\"
        ENDIF
        WRITE(NFIC,'(a)') "\end{tabular}"
        WRITE(NFIC,'(A)') "\\"
        ! The help informations
        IF(MYDICO(IKEY)%AIDE(LNG)(1:3).EQ.'  ') THEN
          WRITE(NFIC,'(A)') 'TODO: WRITE HELP FOR THAT KEYWORD'
        ELSE
          WRITE(NFIC,'(A)') TRIM(MYDICO(IKEY)%AIDE(LNG))
        ENDIF
        WRITE(NFIC,'(A)') "%"
      ENDDO
!
!
      !
      ! Keywsords by rubriques
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.LNG_FR) THEN
        WRITE(NFIC,'(A)') '\chapter{Liste des mots clefs par rubrique}'
      ELSE
        WRITE(NFIC,'(A)')
     &    '\chapter{List of keywords classified according to type}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      CALL WRITE_RUBRIQUE2LATEX(NFIC,LNG,ORDERED_KEY1)

!
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.LNG_FR) THEN
        WRITE(NFIC,'(A)') '\chapter{Glossaire}'
      ELSE
        WRITE(NFIC,'(A)') '\chapter{Glossary}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      !
      ! Section list of keywords lng -> 3-lng
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.LNG_FR) THEN
        WRITE(NFIC,'(A)') '\section{Glossaire francais/anglais}'
      ELSE
        WRITE(NFIC,'(A)') '\section{English/French glossary}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      ! Keywords are written in a longatble
      WRITE(NFIC,'(A)') '\begin{longtable}'//
     &                 '{|p{0.5\linewidth}|p{0.5\linewidth}|}'
      WRITE(NFIC,'(A)') '\hline'
      DO I=1,NKEY
        IKEY = ORDERED_KEY1(I)
        WRITE(NFIC,'(5A)') "\telkey{",TRIM(MYDICO(IKEY)%KNOM(LNG)),
     &                  "} & \telkey{",TRIM(MYDICO(IKEY)%KNOM(3-LNG)),
     &                  "}\\"
        WRITE(NFIC,'(A)') '\hline'
      ENDDO
      WRITE(NFIC,'(A)') '\end{longtable}'
      WRITE(NFIC,'(A)') '%'
      !
      ! Section list of keywords 3-lng -> lng
      !
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      IF(LNG.EQ.LNG_FR) THEN
        WRITE(NFIC,'(A)') '\section{Glossaire anglais/francais}'
      ELSE
        WRITE(NFIC,'(A)') '\section{French/English glossary}'
      ENDIF
      WRITE(NFIC,'(A,A)') '%',REPEAT('-',80)
      WRITE(NFIC,'(A)') ' '
      ! Keywords are written in a longatble
      WRITE(NFIC,'(A)') '\begin{longtable}'//
     &                 '{|p{0.5\linewidth}|p{0.5\linewidth}|}'
      WRITE(NFIC,'(A)') '\hline'
      DO I=1,NKEY
        IKEY = ORDERED_KEY2(I)
        WRITE(NFIC,'(5A)') "\telkey{",TRIM(MYDICO(IKEY)%KNOM(3-LNG)),
     &                  "} & \telkey{",TRIM(MYDICO(IKEY)%KNOM(LNG)),
     &                  "}\\"
        WRITE(NFIC,'(A)') '\hline'
      ENDDO
      WRITE(NFIC,'(A)') '\end{longtable}'

      CLOSE(NFIC)
!
      END SUBROUTINE
      !
      END MODULE UTILS_LATEX
