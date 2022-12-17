!                   *****************
                    SUBROUTINE CLASSE
!                   *****************
!
     &(DIMENS , SIZE   , MOTCLE , UTINDX , NMAX   ,
     & OFFSET , ADRESS , INDIC  , LUIGN  ,
     & MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     & DEFCAR , DEFINT , DEFLOG , DEFREA , DEFATT )
!
!***********************************************************************
! DAMOCLES   V7P1
!***********************************************************************
!
!brief    STORES IN ARRAYS MOTINT, MOTREA, MOTLOG, MOTCAR AND
!+             MOTATT THE VALUES READ FOR A KEYWORD.
!+
!+             DISCARDS THE WORDS RETURNED BY EDAMOX IN THE DATA FILE.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!history  O. QUIQUEMPOIX (LNH)
!+        14/12/1993
!+
!+
!
!history  L. LEGUE
!+        16/08/1994
!+        V5P1
!+
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        20/11/2015
!+        V7P1
!+   An error message was printing the wrong line number. Now printing
!+   the correct keyword instead.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADRESS         |<->| TABLEAU DES ADRESSES DES MOTS CLES
!| DEFATT         |<->| TABLEAU DES SUBMITS PAR DEFAUT
!| DEFCAR         |<->| TABLEAU DES VALEURS CARACTERES PAR DEFAUT
!| DEFINT         |<->| TABLEAU DES VALEURS ENTIERES PAR DEFAUT
!| DEFLOG         |<->| TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
!| DEFREA         |<->| TABLEAU DES VALEURS REELLES PAR DEFAUT
!| DIMENS         |<->| TABLEAU DES DIMENSIONS DES MOTS CLES
!| INDIC          |<->| TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
!|                |   | = 0 : PAS DE SUBMIT & NON TABLEAU
!|                |   | = 1 : PAS DE SUBMIT & TABLEAU
!|                |   | = 2 : AVEC   SUBMIT & NON TABLEAU
!|                |   | = 3 : AVEC   SUBMIT & NON TABLEAU
!| LUIGN          |-->| LOGIQUE POUR LES MOTS A NE PAS CLASSER
!| MOTATT         |<->| TABLEAU DES SUBMITS
!| MOTCAR         |<->| TABLEAU DES VALEURS CARACTERES
!| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
!| MOTINT         |<->| TABLEAU DES VALEURS ENTIERES
!| MOTLOG         |<->| TABLEAU DES VALEURS LOGIQUES
!| MOTREA         |<->| TABLEAU DES VALEURS REELLES
!| NMAX           |-->| TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
!| OFFSET         |<->| TABLEAUX DES PROCHAINES ADRESSES LIBRES
!| SIZE           |<->| TABLEAU DES LONGUEURS DES MOTS CLES
!| UTINDX         |<->| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE DECLARATIONS_DAMOCLES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER          NMAX,MOTINT(*),ADRESS(4,*),DIMENS(4,*)
      INTEGER          SIZE(4,*),OFFSET(4),DEFINT(*),INDIC(4,*)
      LOGICAL          UTINDX(4,*),DEFLOG(*),MOTLOG(*),LUIGN
      CHARACTER(LEN=72)     MOTCLE(4,*)
      CHARACTER(LEN=PATH_LEN)    MOTCAR(*),DEFCAR(*)
      CHARACTER(LEN=PATH_LEN)    MOTATT(4,*),DEFATT(*)
      DOUBLE PRECISION MOTREA(*),DEFREA(*)
!
!-----------------------------------------------------------------------
!
      INTEGER          I
!
!-----------------------------------------------------------------------
!
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      IF (LUIGN) GO TO 1600
!
!     GLOBAL TREATMENT OF THE KEYWORD
!
      IF (INDX .GT. NMAX) THEN
        WRITE(LU,*) '****************************************'
        WRITE(LU,*) 'ERROR AT LINE:',NLIGN,
     &              ' OF THE DICTIONARY'
        WRITE(LU,*) 'INVALID INDEX: ',INDX,' MAX = ',NMAX
        WRITE(LU,*) '****************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF (NMOT(NTYP) .GT. NMAX) THEN
        WRITE(LU,*)'*****************************************'
        WRITE(LU,*) 'ERROR AT LINE:',NLIGN,
     &              ' OF THE DICTIONARY'
        WRITE(LU,*) 'TOO MANY KEY-WORDS, MAXIMUM : ',NMAX
        WRITE(LU,*)'*****************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
! REDUNDANT WITH LUIGN? KEPT BY DEFAULT - TO BE CHECKED
      IF (INDX .LE. 0) GO TO 1600
!
      IF (UTINDX(NTYP,INDX)) THEN
        WRITE(LU,*)'*****************************'
        WRITE(LU,*) 'ERROR AT LINE: ',NLIGN
        WRITE(LU,*) 'THE INDEX: ',INDX,
     &   ' IS USED TWO TIMES FOR THE TYPE : ',NTYP
        WRITE(LU,*)'*****************************'
        CALL PLANTE(1)
        STOP
      ELSE
        UTINDX(NTYP,INDX) = .TRUE.
      ENDIF
!
      IF(ITAI.LE.0) THEN
        ITAI = 1
      ELSE
!       PREVENTS DYNAMIC ALLOCATION FOR SOMETHING ELSE THAN AN ARRAY
        INDIC(NTYP,INDX)=INDIC(NTYP,INDX)+1
      ENDIF
!
! ISSUES A WARNING WHEN THE DEFAULT VALUES ARE DEFINED
! IN INSUFFICIENT NUMBER COMPARED TO THE DIMENSIONS
!
      IF(DEFLU.GT.0.AND.DEFLU.NE.ITAI) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'WARNING IN DICTIONARY:'
        WRITE(LU,*) 'FOR KEYWORD: ',PARAM(1:LONGU)
        WRITE(LU,*) 'THE NUMBER OF DEFAULT VALUES ',DEFLU,
     &              ' IS DIFFERENT FROM THE DECLARED SIZE ',ITAI
        WRITE(LU,*) ' '
      ENDIF
!
      IF(DEFLU .EQ. 0) THEN
        IF     (NTYP .EQ. 1) THEN
          DEFINT(1) = 0
        ELSEIF (NTYP .EQ. 2) THEN
          DEFREA(1) = 0.0
        ELSEIF (NTYP .EQ. 3)THEN
          DEFLOG(1) = .FALSE.
        ELSEIF (NTYP .EQ. 4) THEN
          DEFCAR(1) = ' '
        ENDIF
      ENDIF
!
      IF (ITAI .NE. DEFLU) THEN
        IF (ITAI .GT. DEFLU) THEN
          DO I = DEFLU + 1 , ITAI
            IF     (NTYP .EQ. 1) THEN
              DEFINT(I) = DEFINT(MAX(1,DEFLU))
            ELSEIF (NTYP .EQ. 2) THEN
              DEFREA(I) = DEFREA(MAX(1,DEFLU))
            ELSEIF (NTYP .EQ. 3) THEN
              DEFLOG(I) = DEFLOG(MAX(1,DEFLU))
            ELSEIF (NTYP .EQ. 4) THEN
              DEFCAR(I) = DEFCAR(MAX(1,DEFLU))
            ENDIF
!           DEFATT(NYTP,I) = DEFATT(NYTP,MAX(1,DEFLU))
          ENDDO ! I
        ENDIF
        DEFLU = ITAI
      ENDIF
!
!   STORES THE KEYWORD ATTRIBUTES IN THE ARRAYS
!   NUMBER OF KEYWORDS OF TYPE NTYP
!
      NMOT(NTYP) = NMOT(NTYP) + 1
!
!   NEXT FREE ADDRESS FOR THE KEYWORD OF TYPE NTYP
!
      ADRESS(NTYP,INDX) = OFFSET(NTYP)
!
!   STORED KEYWORD
!
      MOTCLE(NTYP,INDX) = PARAM(1:LONGU)
!
!   NUNBER OF VALUES ASSOCIATED WITH THE KEYWORD OF TYPE NTYP
!
      DIMENS(NTYP,INDX) = ITAI
!
!   LENGTH OF THE KEYWORD (CHARACTERS)
!
      SIZE(NTYP,INDX) = LONGU
!
!   STORES THE VALUES IN THE ARRAYS
!
      IF (((ADRESS(NTYP,INDX)+ITAI-1) .GT. NMAX)
     &   .OR. (OFFSET(NTYP) .GT. NMAX)) THEN
        WRITE(LU,*) 'ADRESS GREATER THAN NMAX = ',NMAX
        WRITE(LU,*) 'TOO MANY VALUES OF TYPE : ',NTYP
     &              ,' DECLARED.'
        WRITE(LU,*) 'STOP AT KEY-WORD OF INDEX: ',INDX
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I = 1 , ITAI
        IF (NTYP .EQ. 1) THEN
          MOTINT(ADRESS(NTYP,INDX)+I-1) = DEFINT(I)
        ELSE IF (NTYP .EQ. 2) THEN
          MOTREA(ADRESS(NTYP,INDX)+I-1) = DEFREA(I)
        ELSE IF (NTYP .EQ. 3) THEN
          MOTLOG(ADRESS(NTYP,INDX)+I-1) = DEFLOG(I)
        ELSE IF (NTYP .EQ. 4) THEN
          MOTCAR(ADRESS(NTYP,INDX)+I-1) = DEFCAR(I)
        ENDIF
        IF (INDIC(NTYP,INDX).GE.2)
     &    MOTATT(NTYP,ADRESS(NTYP,INDX)+I-1) = DEFATT(I)
      ENDDO ! I
!
!   UPDATES THE NEXT FREE ADDRESS
!
      OFFSET(NTYP) = OFFSET(NTYP) + ITAI
!
!   INITIALISES THE TEMPORARY VARIABLES
!
1600  CONTINUE
      PARAM  = ' '
      LONGU  = 0
      NTYP   = -100
      INDX   = 123456
      ITAI   = -100
      DEFLU  = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END

