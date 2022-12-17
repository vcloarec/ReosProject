!                   *****************
                    SUBROUTINE AIDELU
!                   *****************
!
     &( ICOL , LIGNE, DOC )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    DECODES A CHARACTER STRING FROM COLUMN ICOL+1
!+             OF A LINE  (80 CHARACTERS MAXIMUM PER LINE).
!+             THIS STRING CAN RUN OVER SEVERAL LINES.
!+             AIDELU IS USED TO DECODE THE HELP SECTION OF THE
!+             DICTIONARY ONLY, AND THE WORDS IGNORED FOR EDAMOX.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!warning  FOLLOWS THE FORTRAN CONVENTION : '' IS READ AS
!+            ' WHEN WITHIN A CHARACTER STRING IF THE STRING
!+            IS WRITTEN BETWEEN QUOTES
!+
!warning  QUOTES AT THE BEGINNING AND END OF LINES ARE POSSIBLE
!+            SOURCES OF ERRORS
!
!history  J.M. HERVOUET (LNH); A. YESSAYAN; L. LEGUE
!+        14/01/2008
!+        V5P8
!+   BETTER CONTROL OF 'LONG' LINES
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
!| DOC            |-->| LOGIQUE DE DOCUMENTATION DE LA SORTIE
!|                |   | = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
!|                |   | = FAUX : N'IMPRIME PAS L'AIDE
!| ICOL           |<->| INDICE DU CARACTERE COURANT DANS LA LIGNE
!| LIGNE          |<->| LIGNE EN COURS DE DECODAGE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_DAMOCLES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER       ICOL
      LOGICAL       DOC
      CHARACTER(LEN=*) LIGNE
!
      INTEGER  NEXT,PRECAR
      EXTERNAL NEXT,PRECAR
!
!-----------------------------------------------------------------------
!
      INTEGER       IDEB,IFIN,JCOL
      CHARACTER(LEN=1)   QUOTE,TABUL,PTVIRG
!
!-----------------------------------------------------------------------
!
      INTRINSIC CHAR
!
!***********************************************************************
!                                   MARKS RCS AND SCCS
!
!***********************************************************************
!
      QUOTE  = ''''
      PTVIRG = ';'
      TABUL =CHAR(9)
9     ICOL   = NEXT( ICOL+1 , LIGNE )
!
!        //// FINDS THE ENDS OF THE STRING ////
!
!    NOTE: THE STRING CAN BE BETWEEN QUOTES OR NOT.
!          IF NOT, IT CANNOT CONTAIN WHITE CHARACTERS.
!
!
!
      IF ( LIGNE(ICOL:ICOL).NE.QUOTE ) THEN
        IDEB = ICOL
!              PRECAR: SAME FUNCTION AS PREVAL, BUT DOES NOT JUMP
!                      OVER COMMENTED LINES
        ICOL = PRECAR (ICOL+1,LIGNE,' ',PTVIRG,TABUL) - 1
        IFIN = ICOL
        IF (DOC) WRITE(LU,10) LIGNE(IDEB:IFIN)
10      FORMAT(1X,A)
      ELSE
!
! IF THE STRING IS BETWEEN QUOTES
!
        IDEB = ICOL + 1
!
! WHILE THERE IS NO QUOTE ON THE LINE
!
100     ICOL = PRECAR(ICOL+1,LIGNE,QUOTE,QUOTE,QUOTE)
        IF (ICOL.GT.LONGLI) THEN
!         NO QUOTE ON THE LINE, IT'S WRITTEN OUT AND GOES TO NEXT
          IF (DOC) WRITE(LU,10) LIGNE(IDEB:LONGLI)
!         READS NEXT LINE
          READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE
          NLIGN = NLIGN + 1
          ICOL = 1
          IDEB = 1
          GO TO 100
        ELSEIF(ICOL.EQ.LONGLI) THEN
!       QUOTE AT THE END OF THE LINE, THE LINE IS WRITTEN OUT (EXCEPT
!       THE QUOTE) AND THAT'S IT
          IF (DOC) WRITE(LU,10) LIGNE(IDEB:ICOL-1)
        ELSE
!         NEXT QUOTE
          JCOL = PRECAR(ICOL+1,LIGNE,QUOTE,QUOTE,QUOTE)
!         IF THERE IS A DOUBLE QUOTE, IT IS DELETED
          IF ((JCOL-ICOL).EQ.1) THEN
            ICOL=JCOL
            LIGNE(JCOL:LONGLI)=LIGNE(JCOL+1:LONGLI) // ' '
            GO TO 100
          ELSE
!           PRINTS OUT THE 'HELP' WHEN DELETING THE LAST QUOTE
            IF (DOC) WRITE(LU,10) LIGNE(IDEB:ICOL-1)
          ENDIF
        ENDIF
      ENDIF
      ICOL = NEXT(ICOL+1,LIGNE)
      IF(ICOL.LE.LONGLI) THEN
        IF(LIGNE(ICOL:ICOL).EQ.PTVIRG(1:1)) GO TO 9
      ENDIF
      GO TO 1000
!
! WRITES OUT ERRORS
!
998   CONTINUE
      WRITE(LU,1999) NFIC, NLIGN
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR ON LINE ',1I6)
900   CONTINUE
      RETOUR = .TRUE.
!
! END OF THE WRITING OF ERRORS
!
1000  CONTINUE
!
! TWO EMPTY LINES FOR THE PAGE LAYOUT
!
      IF (DOC) WRITE(LU,*) ' '
      IF (DOC) WRITE(LU,*) ' '
!
!-----------------------------------------------------------------------
!
      RETURN
      END
