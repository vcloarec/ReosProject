!                   ****************
                    SUBROUTINE INFLU
!                   ****************
!
     &( ICOL   , LIGNE  , DEFATT , TROUVE , LUIGN , MOTCLE , SIZE,
     &  MOTIGN , LONIGN , NMAXR  , NFICDA , GESTD )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    DECODES THE SUBMIT FIELD FROM COLUMN ICOL+1 OF THE
!+             CURRENT LINE. TESTS THE PRESENCE OF THE 4 FIELDS.
!+             RECOGNISES CHAMP2.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!warning  IF THE 1ST FIELD IS NOT KNOWN, THERE ARE NO CHECKS OTHER
!+            THAN ON THE 2ND FIELD, WHICH CHARACTERISES THE BEHAVIOUR
!+            OF THE KEYWORD FOR DAMOCLES. THIS EXTENDS THE
!+            COMPATIBILITY OF DAMOCLES WITHOUT DIRECT MODIFICATIONS
!+            TO THE FORTRAN
!
!history  O. QUIQUEMPOIX (LNH)
!+        16/08/1994
!+
!+
!
!history  J-M HERVOUET (LNH)
!+        14/01/2008
!+        V5P8
!+   CORRECTION: IN MOTCH1(1:LCAR), LCAR SHOULD NOT EXCEED 10
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
!| DEFATT         |<--| TABLEAU DES SUBMITS PAR DEFAUT
!| GESTD          |-->| LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
!| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
!| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
!| LONIGN         |-->| TABLEAU DES LONGUEURS DES MOTS EDAMOX
!| LUIGN          |-->| LOGIQUE POUR LES MOTS A NE PAS CLASSER
!| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
!| MOTIGN         |-->| TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
!| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
!| NMAXR          |-->| TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
!| SIZE           |-->| TABLEAU DES LONGUEURS DES MOTS CLES
!| TROUVE         |<->| INDICATEUR D'ETAT DES MOTS CLES
!|                |   | = 0 : AUCUNE VALEUR TROUVEE
!|                |   | = 1 : VALEUR PAR DEFAUT TROUVEE
!|                |   | = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)
!|                |   | = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)
!|                |   | = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE
!|                |   | = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE
!|                |   | = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)
!|                |   | = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)
!|                |   | = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR
!|                |   | =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_DAMOCLES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      EXTERNAL NEXT,PRECAR,CARLU,LONGLU
!
      INTEGER       TROUVE(4,*),ICOL,NMAXR(4),NFICDA,SIZE(4,*)
      INTEGER       LONIGN(100)
      LOGICAL       LUIGN,GESTD
      CHARACTER(LEN=72)  MOTIGN(100),MOTCLE(4,*)
      CHARACTER(LEN=PATH_LEN) DEFATT(*)
      CHARACTER(LEN=*) LIGNE
!
      INTEGER       NEXT,PRECAR,LONGLU
      CHARACTER(LEN=PATH_LEN) CARLU
!
!-----------------------------------------------------------------------
!
      INTEGER   ::    NBCHP1
      PARAMETER (NBCHP1=12)
!
      INTEGER       I,LCAR,ICOLA,JCOLA,CHAMP(4),LGA,II
      INTEGER       :: LGMOTG(NBCHP1),GECHP1(NBCHP1)
      CHARACTER(LEN=1)   PTVIRG,QUOTE,GUILLT
      CHARACTER(LEN=53)  :: MESERR(2*NBCHP1)
      CHARACTER(LEN=10)  :: MOTCH1(NBCHP1)
      CHARACTER(LEN=PATH_LEN) NULATT,ANALYS,FIELD,FIELD0
!
!-----------------------------------------------------------------------
!
! ******************* DATABASE FOR THE SUBROUTINE **********************
!
! DEFINITION OF FIELDS 1
      PARAMETER ( MOTCH1 = (/
     &  'IN        ','OUT       ','CAS       ','DIC       ',
     &  'QSUB      ','LIB       ','FORTRAN   ','DIROUT    ',
     &  'USER      ','ACCT      ','PRE       ','POST      ' /) )
! LENGTHS OF THE STRINGS FOR FIELDS 1 DEFINED ABOVE
      PARAMETER ( LGMOTG = (/ 2,3,3,3,4,3,7,6,4,4,3,4 /) )
! CHANGE TO 'NUL;FOR' IF GESTD=.TRUE. ? : 1-YES, 0-NO
      PARAMETER ( GECHP1 = (/ 1,1,0,0,0,0,1,1,0,0,0,0 /) )
! NUMBER OF THE FIELDS TO BE GIVEN TO THESE WORDS --> ERROR MESSAGES
!      DATA NOCHMP /1,2,3,4,5,6,7,8,9,10,11,12/
! ERROR MESSAGES ASSOCIATED WITH THE FIELD NUMBERS
      PARAMETER ( MESERR = (/
     &  'PAS D''ALLOCATION DE FICHIER D''ENTREE !!              ',
     &  'NO ALLOCATION FOR INPUT FILE !!                      ',
     &  'PAS D''ALLOCATION DE FICHIER DE SORTIE !!             ',
     &  'NO ALLOCATION FOR OUTPUT FILE !!                     ',
     &  'PAS D''ALLOCATION POUR LE FICHIER CAS !!              ',
     &  'NO ALLOCATION FOR THE STEERING FILE !!               ',
     &  'PAS D''ALLOCATION POUR LE DICTIONNAIRE !!             ',
     &  'NO ALLOCATION FOR THE DICTIONARY !!                  ',
     &  'PAS DE COMMANDE CRAY !!                              ',
     &  'NO INSTRUCTION FOR CRAY !!                           ',
     &  'PAS DE LIBRAIRIE !!                                  ',
     &  'NO LIBRARY !!                                        ',
     &  'PAS DE VALEUR POUR LE REPERTOIRE FORTRAN !!          ',
     &  'NO VALUE FOR THE FORTRAN DIRECTORY !!                ',
     &  'PAS DE VALEUR POUR LE REPERTOIRE DE SORTIE !!        ',
     &  'NO VALUE FOR THE OUTPUT DIRECTORY !!                 ',
     &  'PAS DE COMMANDE CRAY !!                              ',
     &  'NO INSTRUCTION FOR CRAY !!                           ',
     &  'PAS DE COMMANDE CRAY !!                              ',
     &  'NO INSTRUCTION FOR CRAY !!                           ',
     &  'PAS DE COMMANDE PRE !!                               ',
     &  'NO INSTRUCTION FOR PRE !!                            ',
     &  'PAS DE COMMANDE POST !!                              ',
     &  'NO INSTRUCTION FOR POST !!                           ' /) )
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
!  INITIALISES
!
      PTVIRG = ';'
      QUOTE  = ''''
      GUILLT = '"'
      DEFLU  = 0
!
100   DEFLU = DEFLU +1
      IF(.NOT.(LUIGN)) THEN
        DEFATT(DEFLU)=CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                      LONIGN,NMAXR,NFICDA,LEN(DEFATT(DEFLU)))
      ELSE
        NULATT = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                 LONIGN,NMAXR,NFICDA,LEN(NULATT))
      ENDIF
!
      ICOL = NEXT(ICOL+1,LIGNE)
!
      IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) GO TO 100
!
! NO ANALYSIS IF TO BE IGNORED ...
      IF (LUIGN) GO TO 1300
!
      IF (DEFLU .LT. ITAI) THEN
        ERREUR = .TRUE.
        WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
        WRITE(LU,*)'NOT ENOUGH DATAS DEFINED FOR SUBMIT...'
        WRITE(LU,*)' '
        GO TO 1300
      ENDIF
!
!  EXAMINES THE SUBMIT FIELDS
!
      DO I = 1 , DEFLU
 200    ICOLA = 0
        ANALYS = DEFATT(I)
!
!   *** FIELD 1 ***
!
        LGA = MAX(LONGLU(ANALYS),1)
        IF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
          LCAR = 0
        ELSE
          JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
          LCAR = LONGLU(ANALYS(ICOLA+1:JCOLA-1))
          IF (LCAR.GT.0) THEN
            FIELD0 = CARLU(LCAR,ICOLA,ANALYS,GUILLT,MOTCLE,SIZE,MOTIGN,
     &                     LONIGN,NMAXR,NFICDA,LEN(FIELD0))
            LCAR = LONGLU(FIELD0(1:LCAR))
          ENDIF
        ENDIF
        IF (LCAR.LE.0) THEN
          WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
          WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
          WRITE(LU,*)'NO FIRST FIELD !!'
          ERREUR = .TRUE.
          GO TO 1300
        ENDIF
        IF (ERREUR) GO TO 1300
        FIELD = FIELD0
        CALL MAJUS(FIELD)
!
        CHAMP(1)=100
        DO II=1,NBCHP1
          IF (LCAR.EQ.LGMOTG(II).AND.
     &        FIELD(1:MIN(LCAR,10)).EQ.MOTCH1(II)(1:MIN(LCAR,10))) THEN
            IF (GESTD.AND.GECHP1(II).EQ.1) THEN
              DEFATT(I) = 'NUL;FOR'//DEFATT(I)(JCOLA+4:MAX(LGA,JCOLA+4))
              GO TO 200
            ELSE
!             CHAMP(1)=NOCHMP(II)
              CHAMP(1)=II
              GOTO 400
            ENDIF
          ENDIF
        ENDDO ! II
!
!   *** FIELD 2 ***
!
400     ICOLA = JCOLA
        IF (ICOLA.GE.LONGLI) THEN
          LCAR = 0
        ELSEIF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
          LCAR = 0
        ELSE
          JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
          LCAR = LONGLU(ANALYS(ICOLA+1:JCOLA-1))
          IF (LCAR.GT.0) THEN
            FIELD0 = CARLU(LCAR,ICOLA,ANALYS,GUILLT,MOTCLE,SIZE,MOTIGN,
     &                     LONIGN,NMAXR,NFICDA,LEN(FIELD0))
            LCAR = LONGLU(FIELD0(1:LCAR))
          ENDIF
        ENDIF
        IF (LCAR.LE.0) THEN
          WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
          WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
          WRITE(LU,*)'NO SECOND FIELD !! '
          ERREUR = .TRUE.
          GO TO 1300
        ENDIF
!
        IF (ERREUR) GO TO 1300
        FIELD = FIELD0
        CALL MAJUS(FIELD)
!
!  SUPPRESSION OF AN OBSOLETE CONTROL
!  THE 2ND SUBMIT FIELD CAN BE DIFFERENT FROM FOR...
!  (CASE OF SUBIEF)
!
        IF (FIELD(1:3).EQ.'OPT') THEN
          CHAMP(2) = 1
        ELSEIF (FIELD(1:3).EQ.'REQ') THEN
          CHAMP(2) = 2
        ELSE
          CHAMP(2) = 3
        ENDIF
!
! ASSIGNS THE INITIAL VALUE TO TROUVE ACCORDING TO CHAMP(1)
        IF (ITAI.LE.1.AND.I.LE.MAX(ITAI,1)) THEN
          IF (CHAMP(2) .EQ. 1) TROUVE(NTYP,INDX)=3
          IF (CHAMP(2) .EQ. 3) TROUVE(NTYP,INDX)=6
          IF (CHAMP(1) .EQ. 4) TROUVE(NTYP,INDX)=9
        ENDIF
!
! IF THE 1ST FIELD IS NOT KNOWN, IGNORES THE REST
! TO BE COMPATIBLE WITH EVOLUTIONS OF THE LAUNCHER
        IF (CHAMP(1).EQ.100) CYCLE
!
!   *** FIELD 3 ***
!
        ICOLA = JCOLA
        IF (JCOLA.GE.LONGLI) THEN
          WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
          WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
          WRITE(LU,*)'NO THIRD FIELD !! '
          ERREUR = .TRUE.
          GO TO 1300
        ENDIF
        JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
!
!   *** FIELD 4 ***
!
        ICOLA = JCOLA
        IF (ICOLA.GE.LONGLI) THEN
          LCAR = 0
        ELSEIF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
          LCAR = 0
        ELSE
          JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
          LCAR = LONGLU(ANALYS(ICOLA+1:JCOLA-1))
        ENDIF
        IF (LCAR.LE.0) THEN
          WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
          WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
          ERREUR = .TRUE.
!
! WRITES THE CORRESPONDING ERROR MESSAGE
          WRITE(LU,*) MESERR(2*(CHAMP(1)-1)+LNG)
          GO TO 1300
        ENDIF
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
1300  CONTINUE
      RETURN
      END
