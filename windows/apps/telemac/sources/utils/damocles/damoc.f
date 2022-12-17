!                   ****************
                    SUBROUTINE DAMOC
!                   ****************
!
     &( ADRESS , DIMENS , NMAX   , DOC    , LLNG   , LLU    ,
     &  MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     &  DEFINT , DEFREA , DEFLOG , DEFCAR , DEFATT ,
     &  USRINT , USRREA , USRLOG , USRCAR , USRATT ,
     &  MOTCLE , TAILLE , TROUVE , UTINDX , NFICMO , NFICDA ,
     &  INDIC  , GESTD  , NBLANG , RETRY )
!
!***********************************************************************
! DAMOCLES   V7P2
!***********************************************************************
!
!brief    MAIN ROUTINE OF THE DAMOCLES LIBRARY
!+             CALLED BY THE DAMOCLES EXECUTABLE (DAMOCLE.F)
!+             CALLED BY THE LNH COMPUTATIONAL CODES.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!history  J-M HERVOUET (LNH); A. YESSAYAN; L. LEGUE
!+        10/11/2008
!+        V5P9
!+   First version
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
!+        07/03/2016
!+        V7P2
!+   Changing array SIZE into TAILLE (SIZE is a Fortran function).
!+   Declarations CHARACTER*... replaced by CHARACTER(LEN=...).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADRESS         |<--| TABLEAU DES ADRESSES DES MOTS CLES
!| DEFATT         |<--| TABLEAU DES SUBMITS PAR DEFAUT
!| DEFCAR         |<--| TABLEAU DES VALEURS CARACTERES PAR DEFAUT
!| DEFINT         |<--| TABLEAU DES VALEURS ENTIERES PAR DEFAUT
!| DEFLOG         |<--| TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
!| DEFREA         |<--| TABLEAU DES VALEURS REELLES PAR DEFAUT
!| DIMENS         |<--| TABLEAU DES DIMENSIONS DES MOTS CLES
!| DOC            |-->| LOGIQUE DE DOCUMENTATION DE LA SORTIE
!|                |   | = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
!|                |   | = FAUX : N'IMPRIME PAS L'AIDE
!| GESTD          |-->| LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
!| INDIC          |<--| TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
!|                |   | = 0 : PAS DE SUBMIT & NON TABLEAU
!|                |   | = 1 : PAS DE SUBMIT & TABLEAU
!|                |   | = 2 : AVEC   SUBMIT & NON TABLEAU
!|                |   | = 3 : AVEC   SUBMIT & TABLEAU
!| LLNG           |-->| NUMERO DE LA LANGUE DE DECODAGE
!| LLU            |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
!| MOTATT         |<--| TABLEAU DES SUBMITS
!| MOTCAR         |<--| TABLEAU DES VALEURS CARACTERES
!| MOTCLE         |<--| TABLEAU DES MOTS CLES ACTIFS
!| MOTINT         |<--| TABLEAU DES VALEURS ENTIERES
!| MOTLOG         |<--| TABLEAU DES VALEURS LOGIQUES
!| MOTREA         |<--| TABLEAU DES VALEURS REELLES
!| NBLANG         |-->| NOMBRE DE LANGUES CONNUES
!| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
!| NFICMO         |-->| NUMERO DE CANAL DU FICHIER DES MOTS-CLES
!| NMAX           |-->| TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
!| RETRY          |---|
!| TAILLE         |<--| TABLEAU DES LONGUEURS DES MOTS CLES
!| TROUVE         |<--| INDICATEUR D'ETAT DES MOTS CLES
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
!| USRATT         |<--| TABLEAU DES SUBMITS A USAGE LOCAL
!| USRCAR         |<--| TABLEAU DES VALEURS CARACTERES A USAGE LOCAL
!| USRINT         |<--| TABLEAU DES VALEURS ENTIERES A USAGE LOCAL
!| USRLOG         |<--| TABLEAU DES VALEURS LOGIQUES A USAGE LOCAL
!| USRREA         |<--| TABLEAU DES VALEURS REELLES A USAGE LOCAL
!| UTINDX         |<--| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_DAMOCLES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER            NMAX,LLNG,LLU,NFICMO,NFICDA,NBLANG,RETRY
      INTEGER            MOTINT(*),DEFINT(*),USRINT(*)
      INTEGER            TAILLE(4,*),ADRESS(4,*),DIMENS(4,*)
      INTEGER            INDIC(4,*),TROUVE(4,*)
      LOGICAL            MOTLOG(*),DEFLOG(*),USRLOG(*),UTINDX(4,*),DOC
      CHARACTER(LEN=72)  MOTCLE(4,*)
      CHARACTER(LEN=PATH_LEN) MOTATT(4,*),DEFATT(*),USRATT(*)
      CHARACTER(LEN=PATH_LEN) MOTCAR(*),DEFCAR(*),USRCAR(*)
      DOUBLE PRECISION   MOTREA(*),DEFREA(*),USRREA(*)
!
      INTEGER            INTLU,NEXT,PREV,PREVAL,LONGLU
      LOGICAL            LOGLU
      CHARACTER(LEN=PATH_LEN) CARLU,PARAM2
      DOUBLE PRECISION   REALU
!
!-----------------------------------------------------------------------
!
      INTEGER            I,K,IVAL,LCAR,ICOL,JCOL,ILONG,ITYP,NUMERO,I2
      INTEGER            DEPLAC,ADD,J,OFFSET(4),NBMOT
      INTEGER            TYPIGN(100),LONIGN(100),NMAXR(4),ORDRE
      INTEGER         ::  ADSRC,ADDES,NULINT,NVAL,NIGN,L1,LONPRO(15)
      LOGICAL            DYNAM,LANGUE,NULLOG,LUIGN,AIDLNG,VUMOT
      LOGICAL            ARRET,VUCMD(5),VUCMD0(5),EXECMD,GESTD
      CHARACTER(LEN=1)   PTVIRG,QUOTE
      CHARACTER(LEN=9) :: MOTPRO(15),TYPE
      CHARACTER(LEN=72)  MOTIGN(100),LIGNE
      CHARACTER(LEN=PATH_LEN) NULCAR,TYPE2
      DOUBLE PRECISION   NULREA
!
!-----------------------------------------------------------------------
!
      EXTERNAL CARLU,INTLU,LOGLU,REALU,NEXT,PREV,PREVAL,LONGLU
!
!-----------------------------------------------------------------------
!
      PARAMETER ( MOTPRO = (/
     &  'NOM      ','TYPE     ','INDEX    ','TAILLE   ','DEFAUT   ',
     &  'AIDE     ','CHOIX    ','RUBRIQUE ','NIVEAU   ','MNEMO    ',
     &  'COMPOSE  ','COMPORT  ','CONTROLE ','APPARENCE','SUBMIT   ' /) )
!     LENGTH OF THE PROTECTED WORDS
      PARAMETER ( LONPRO =(/ 3,4,5,6,6,4,5,8,6,5,7,7,8,9,6 /) )
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
!     TYPE NUMBERING :    1 : INTEGER
!                         2 : REAL
!                         3 : LOGICAL
!                         4 : CHARACTER
!
!     MOTPRO(I)   : IEME MOT RESERVE POUR LE PROGRAMME (NOM,TYPE,...)
!     MOTCLE(I,J) : NOM DU JIEME MOT-CLE DE TYPE I
!     DIMENS(I,J) : DIMENSION DU JIEME MOT-CLE DE TYPE I
!     ADRESS(I,J) : ADRESSE DU JIEME MOT-CLE DE TYPE I DANS LE TABLEAU
!     MOTINT(I)   : IEME RESULTAT ENTIER
!     MOTREA(I)   : IEME RESULTAT REEL
!     MOTLOG(I)   : IEME RESULTAT LOGIQUE
!     MOTCAR(I)   : IEME RESULTAT CARACTERE
!     MOTATT(I,J) : JEME SUBMIT DU TYPE I
!     TAILLE(I,J) : LONGUEUR DU JIEME MOT-CLE DE TYPE I
!     TROUVE(I,J) : CONCERNE LE JIEME MOT-CLE DE TYPE I
!     INDIC(I,J)  : CONCERNE LE JIEME MOT-CLE DE TYPE I
!     LUIGN       : INDIQUE SI C'EST UN MOT POUR EDAMOX SEULEMENT
!     MOTIGN(I)   : IEME MOT LU DANS LE FICHIER CAS DONNE PAR EDAMOX
!                   ET LU COMME IGNORE DANS LE DICTIONNAIRE
!     DYNAM       : LOGIQUE POUR LE DYNAMIQUE (.TRUE. SI MODE DYNAMIQUE)
!     VUCMD(I)    : TABLEAU DE LOGIQUES (MEMORISATION DES COMMANDES)
!                   I=1->&LIS;I=2->&ETA;I=3->&IND;I=4->&STO;I=5->&FIN
!     EXECMD      : LOGIQUE D'ACTIVATION DES COMMANDES MEMORISEES
!     NMAXR(I)    : INDEX MAXIMUM REELLEMENT UTILISE POUR LE TYPE I
!
!-----------------------------------------------------------------------
!
! INITIALISES :
!
      LU      = LLU
      LNG     = LLNG
      ARRET   = .FALSE.
      ERREUR  = .FALSE.
      RETOUR  = .FALSE.
      DYNAM   = .FALSE.
      EXECMD  = .FALSE.
      AIDLNG  = .FALSE.
      VUMOT   = .FALSE.
      LONGLI  = 72
      NFIC    = NFICMO
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
      DO K=1, 5
        VUCMD(K) = .FALSE.
        VUCMD0(K) = .FALSE.
      ENDDO ! K
!
      DO K=1,100
        MOTIGN(K)= ' '
        TYPIGN(K)=1
        LONIGN(K)=0
      ENDDO ! K
!
      DO K=1, 4
        NMOT(K) = 0
        NMAXR(K) = 0
        OFFSET(K) = 1
        DO I=1,NMAX
          ADRESS(K,I)  = 0
          DIMENS(K,I)  = 1
          TROUVE(K,I)  = 0
          TAILLE(K,I)  = 0
          UTINDX(K,I)  = .FALSE.
          MOTINT(I)    = 0
          MOTREA(I)    = 0.
          MOTLOG(I)    = .FALSE.
          MOTCAR(I)    = ' '
          MOTATT(K,I)  = ' '
          DEFINT(I)    = 0
          DEFREA(I)    = 0.
          DEFLOG(I)    = .FALSE.
          DEFCAR(I)    = ' '
          DEFATT(I)    = ' '
          USRINT(I)    = 0
          USRREA(I)    = 0.
          USRLOG(I)    = .FALSE.
          USRCAR(I)    = ' '
          USRATT(I)    = ' '
          MOTCLE(K,I)  = ' '
          INDIC(K,I)   = 0
        ENDDO ! I
      ENDDO ! K
!
! CHECKS THE LANGUAGE
!
      IF(LNG.LT.1.OR.LNG.GT.NBLANG) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' CHOICE FOR LANGAGE = ',LNG,' INVALID.'
        WRITE(LU,*) ' DAMOCLES STOPS'
        WRITE(LU,*) ' '
        CALL PLANTE(1)
        STOP
      ENDIF
!
! 99 : NEW FILE   100 : NEW KEYWORD
!
  99  CONTINUE
!
      ICOL   = LONGLI
      NLIGN = 0
!
! SEEKS THE FIRST NON-WHITE CHARACTER (IGNORES COMMENTED LINES) :
!
      ICOL = NEXT(ICOL+1,LIGNE)
!
100   CONTINUE
!
! IF REACHED THE END OF FILE :
!
      IF(RETOUR) GO TO 900
!
! LOCATES THE COMMANDS STARTING WITH &
!
      IF ( LIGNE(ICOL:ICOL).EQ.'&' ) THEN
        CALL CMD (ICOL,LIGNE,ADRESS,DIMENS,TROUVE,MOTCLE,NMOT,
     &       MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT,INDIC,TAILLE,
     &       UTINDX,DYNAM,VUCMD,EXECMD,NFICDA,NMAXR)
!
!     IF FOUND &FIN, ENDS AFTER COMPACTING :
        IF (VUCMD(5)) GO TO 900
!     IF FOUND &STO, ENDS FILE READING :
        IF (VUCMD(4)) GO TO 1000
!
        ICOL = NEXT(ICOL+1,LIGNE)
        IF(RETOUR) GO TO 900
      ELSE
!
        I2 = PREVAL(ICOL+1,LIGNE,'=',':','=')
!       CASE WHERE '=' IS ON THE FOLLOWING LINE
        IF(I2.GT.LONGLI) I2=LONGLI
        JCOL = PREV  (I2,LIGNE)
        ILONG = JCOL - ICOL + 1
!
        LUIGN = .FALSE.
        IF (NFIC.EQ.NFICMO.AND.INDX.LE.0) LUIGN = .TRUE.
!
        CALL DICO(ITYP,NUMERO,ILONG,LIGNE(ICOL:JCOL),
     &       MOTCLE,MOTPRO,LONPRO,TAILLE,UTINDX,LANGUE,
     &       AIDLNG,MOTIGN,NIGN,LUIGN,TYPIGN,LONIGN,NFICDA,
     &       NBLANG,NMAXR)
!
        IF(ERREUR) THEN
          WRITE(LU,*)
          WRITE(LU,*)'************************'
          WRITE(LU,*)'* DAMOCLES STOPPED     *'
          WRITE(LU,*)'************************'
          GO TO 900
        ENDIF
!
! STOPS IF THE WORD IS UNKNOWN
        IF(ITYP.EQ.0) THEN
          ARRET=.TRUE.
          GOTO 1300
        ENDIF
!
        ICOL = PREVAL(ICOL+1,LIGNE,'=',':','=')
!       CASE WHERE '=' IS ON THE FOLLOWING LINE
        IF(ICOL.GT.LONGLI) THEN
          ICOL  = NEXT(LONGLI,LIGNE)
          IF(RETOUR) GO TO 900
        ENDIF
!
! 1) READS AND ASSIGNS A VALUE:
!
        IF(ITYP.LE.4) THEN
!
! A PRIORI THE NUMBER OF VALUES TO READ IS DIMENS(ITYP,NUMERO)
! BUT MISSING OR ADDITIONAL VALUES ARE TOLERATED
!
! WHEN THIS IS IDENTIFIED THE VARIOUS ARRAYS ARE UPDATED
!
!
          IF (.NOT.(LUIGN)) THEN
            NTYP = ITYP
            INDX = NUMERO
            ITAI = DIMENS(NTYP,INDX)
            ADD  = ADRESS(NTYP,INDX)
!           PARAM = MOTCLE(NTYP,INDX)
!           LONGU = LONGLU(PARAM)
            IVAL = 1
            IF (TROUVE(NTYP,INDX).EQ.2.OR.TROUVE(NTYP,INDX).EQ.8) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'THE KEY-WORD: ',MOTCLE(NTYP,INDX)(1:ILONG)
              WRITE(LU,*) 'APPPEARS AT LEAST TWICE , THE LAST',
     &                    ' VALUE WILL BE KEPT...'
              WRITE(LU,*) ' '
            ENDIF
          ENDIF
 10       CONTINUE
          IF (.NOT.(LUIGN)) THEN
            IF     (NTYP.EQ.1) THEN
              DEFINT(IVAL) = INTLU(ICOL,LIGNE)
            ELSEIF (NTYP.EQ.2) THEN
              DEFREA(IVAL) = REALU(ICOL,LIGNE)
            ELSEIF (NTYP.EQ.3) THEN
              DEFLOG(IVAL) = LOGLU(ICOL,LIGNE)
            ELSEIF (NTYP.EQ.4) THEN
              DEFCAR(IVAL) = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     &                             TAILLE,MOTIGN,LONIGN,NMAXR,
     &                             NFICDA,LEN(DEFCAR(IVAL)))
            ENDIF
!
!           SUBMIT FOR A CHARACTER ARRAY
!           THERE IS ONLY ONE; WHEREAS IT WAS PREVIOUSLY ASSUMED
!           THAT THERE WERE AS MANY AS CHARACTER STRINGS ?
            DEFATT(IVAL) = MOTATT(NTYP,ADD)
!
!           CASE OF THE OPTIONAL EMPTY SUBMIT: REMAINS OPTIONAL
            IF (ITAI.LE.1.AND.INDIC(NTYP,INDX).GE.2.AND.
     &        TROUVE(NTYP,INDX).EQ.3) THEN
              L1 = LONGLU(DEFCAR(IVAL))
              IF (L1.GT.0) TROUVE(NTYP,INDX)=2
!
            ELSEIF(TROUVE(NTYP,INDX).LT.6) THEN
              TROUVE(NTYP,INDX)=2
!
            ELSEIF (TROUVE(NTYP,INDX).EQ.6.OR.
     &        TROUVE(NTYP,INDX).EQ.7) THEN
              TROUVE(NTYP,INDX)=8
            ENDIF
!
          ELSE
            NTYP = ITYP
            IF     (NTYP .EQ. 1) THEN
                    NULINT = INTLU(ICOL,LIGNE)
            ELSEIF (NTYP .EQ. 2) THEN
                    NULREA = REALU(ICOL,LIGNE)
            ELSEIF (NTYP .EQ. 3) THEN
                    NULLOG = LOGLU(ICOL,LIGNE)
            ELSEIF (NTYP .EQ. 4) THEN
                    NULCAR = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     &                             TAILLE,MOTIGN,LONIGN,NMAXR,NFICDA,
     &                             LEN(NULCAR))
            ENDIF
          ENDIF
!
          ICOL = NEXT(ICOL+1,LIGNE)
          IF(ICOL.LE.LONGLI) THEN
            IF(LIGNE(ICOL:ICOL).EQ.PTVIRG) THEN
              IVAL = IVAL + 1
              GO TO 10
            ENDIF
          ENDIF
!
          IF (LUIGN) GO TO 100
!
! ALL THE VALUES FOR A KEYWORD HAVE BEEN READ
!
! PARTICULAR CASE: KEYWORDS WITH A SUBMIT
! PREVENTS DYNAMIC ALLOCATION WHEN VALUES ARE GREATER THAN TAILLE (SEE SUBMIT)
! OR FOR KEYWORDS NOT ASSOCIATED WITH ARRAYS
!
          IF (INDIC(NTYP,INDX).NE.1.AND.IVAL.GT.ITAI) IVAL = ITAI
!
!
! IF THE KEYWORD &DYN IS NOT IN THE STEERING FILE AND IF THERE ARE
! MORE VALUES THAN THE PARAMETER TAILLE THEN TRUNCATES TO ITAI;
! ELSE (&DYN IN THE STEERING FILE) READS ALL THE VALUES OF THE STEERING FILE
!
! IS IT REALLY NECESSARY TO SET BACK TO DYNAMIC MODE EVERY TIME ???
! IS MODIFYING DIMENS NOT ENOUGH ?
! IF LECDON IS WELL WRITTEN : IT DOES NOT MATTER IF THERE ARE HOLES
! IN THE ARRAYS SENT ... THINK ABOUT THIS
!
!
          IF (.NOT.(DYNAM)) THEN
            DO I=1 , MIN(IVAL,ITAI)
              IF     (NTYP.EQ.1) THEN
                MOTINT(ADD+I-1) = DEFINT(I)
              ELSEIF (NTYP.EQ.2) THEN
                MOTREA(ADD+I-1) = DEFREA(I)
              ELSEIF (NTYP.EQ.3)THEN
                MOTLOG(ADD+I-1) = DEFLOG(I)
              ELSEIF (NTYP.EQ.4)THEN
                MOTCAR(ADD+I-1) = DEFCAR(I)
              ENDIF
            ENDDO ! I
          ELSE
            DO I=1 ,NMAXR(NTYP)
              IF (UTINDX(NTYP,I)) THEN
                IF(ADRESS(NTYP,I) .NE. ADD) THEN
                  IF (ADRESS(NTYP,I) .LT. ADD) DEPLAC = 0
                  IF (ADRESS(NTYP,I) .GT. ADD) DEPLAC = IVAL - ITAI
                  DO J=1 , DIMENS(NTYP,I)
                    ADSRC = ADRESS(NTYP,I)+J-1
                    ADDES = ADRESS(NTYP,I)+J-1+DEPLAC
                    IF (ADDES.GT. NMAX) GO TO 1515
                    IF     (NTYP.EQ.1) THEN
                      USRINT(ADDES) = MOTINT(ADSRC)
                    ELSEIF (NTYP.EQ.2) THEN
                      USRREA(ADDES) = MOTREA(ADSRC)
                    ELSEIF (NTYP.EQ.3) THEN
                      USRLOG(ADDES) = MOTLOG(ADSRC)
                    ELSEIF (NTYP.EQ.4) THEN
                      USRCAR(ADDES) = MOTCAR(ADSRC)
                    ENDIF
                    USRATT(ADDES) = MOTATT(NTYP,ADSRC)
                  ENDDO ! J
                  IF (ADRESS(NTYP,I) .GT. ADD) THEN
                    ADRESS(NTYP,I) = ADRESS(NTYP,I) + DEPLAC
                    IF (ADRESS(NTYP,I) .GT. NMAX) GO TO 1515
                  ENDIF
!
                ELSE IF (ADRESS(NTYP,I) .EQ. ADD) THEN
                  DO J=1 ,IVAL
                    IF     (NTYP.EQ.1) THEN
                      USRINT(ADD+J-1) = DEFINT(J)
                    ELSEIF (NTYP.EQ.2) THEN
                      USRREA(ADD+J-1) = DEFREA(J)
                    ELSEIF (NTYP.EQ.3) THEN
                      USRLOG(ADD+J-1) = DEFLOG(J)
                    ELSEIF (NTYP.EQ.4)THEN
                      USRCAR(ADD+J-1) = DEFCAR(J)
                    ENDIF
                    USRATT(ADD+J-1) = DEFATT(J)
                  ENDDO ! J
                  DIMENS(NTYP,I) = IVAL
                ENDIF
              ENDIF
            ENDDO ! I
!           SORTS IN FINAL ARRAYS
            DO I=1 ,NMAXR(NTYP)
              IF (UTINDX(NTYP,I)) THEN
                ADSRC = ADRESS(NTYP,I)
                DO J=1 ,DIMENS(NTYP,I)
                  IF     (NTYP.EQ.1) THEN
                    MOTINT(ADSRC+J-1)=USRINT(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.2) THEN
                    MOTREA(ADSRC+J-1)=USRREA(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.3) THEN
                    MOTLOG(ADSRC+J-1)=USRLOG(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.4)  THEN
                    MOTCAR(ADSRC+J-1)=USRCAR(ADSRC+J-1)
                  ENDIF
                  MOTATT(NTYP,ADSRC+J-1) = USRATT(ADSRC+J-1)
                ENDDO ! J
              ENDIF
            ENDDO ! I
          ENDIF
!
!
!       ICOL = NEXT(ICOL,LIGNE)
!
! ENDIF DU IF(ITYP.LE.4) ...
        ENDIF
!
!
! 2) RESERVED KEYWORDS:
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
!
        IF(ITYP.EQ.5) THEN
!
!    NAME
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
! COMING FROM THE PRECEDING WORD, SORTS IT BEFORE READING THE FOLLOWING
! SINCE ALL THE INFORMATION ON THE PRECEDING WORD IS AVAILABLE
!
            IF (NBMOT.GT.1 .AND. (.NOT.(VUMOT)) ) THEN
              IF (INDX.GT.NMAXR(NTYP)) NMAXR(NTYP)=INDX
              CALL CLASSE(DIMENS,TAILLE,MOTCLE,UTINDX,NMAX,
     &                    OFFSET,ADRESS,INDIC,LUIGN,
     &                    MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT ,
     &                    DEFCAR,DEFINT,DEFLOG,DEFREA,DEFATT )
            ENDIF
!
! SIGNALS THAT THIS NEW KEYWORD WAS ALREADY ENCOUNTERED IN ANOTHER LANGUAGE
            IF (.NOT.(VUMOT)) VUMOT=.TRUE.
!
!           NAME OF THE KEYWORD
            IF (LANGUE) THEN
              PARAM2= CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,TAILLE,MOTIGN,
     &                      LONIGN,NMAXR,NFICDA,LEN(PARAM))
              LONGU = LCAR
              PARAM=PARAM2(1:MIN(72,LONGU))
            ELSE
! READS THE NAME OF A NON-REQUESTED LANGUAGE (NOT USED)
              NULCAR = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,TAILLE,
     &                       MOTIGN,LONIGN,NMAXR,NFICDA,LEN(NULCAR))
            ENDIF
!
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    TYPE
!
          ELSE IF(NUMERO.EQ.2) THEN
            VUMOT = .FALSE.
            IF (ORDRE.NE.1) GOTO 1500
            ORDRE=2
            TYPE2= CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,TAILLE,MOTIGN,
     &                   LONIGN,NMAXR,NFICDA,LEN(TYPE))
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
!    *      OR.TYPE(1:4).EQ.'FILE'
!    *      OR.TYPE(1:7).EQ.'FICHIER') THEN
              NTYP = 4
            ELSE
!           ERROR: UNKNOWN TYPE
              WRITE (LU,1003) LIGNE
1003          FORMAT(1X,A72,/,1X,'UNKNOWN TYPE ON THIS LINE')
              CALL PLANTE(1)
              STOP
            ENDIF
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
              MOTIGN(NIGN)=PARAM(1:LONGU)
              LONIGN(NIGN)=LONGU
              TYPIGN(NIGN)=NTYP
            ENDIF
!
!    SIZE
!
          ELSE IF(NUMERO.EQ.4) THEN
            IF (ORDRE.NE.3) GOTO 1500
            ORDRE=4
            ITAI = INTLU(ICOL,LIGNE)
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
            IF (LANGUE) THEN
              DEFLU = 1
              IF (NTYP.NE.4) TROUVE(NTYP,INDX) = 1
!
200           CONTINUE
!
              IF (NTYP .EQ. 1) THEN
                DEFINT(DEFLU) = INTLU(ICOL,LIGNE)
              ELSE IF (NTYP .EQ. 2) THEN
                DEFREA(DEFLU) = REALU(ICOL,LIGNE)
              ELSE IF (NTYP .EQ. 3) THEN
                DEFLOG(DEFLU) = LOGLU(ICOL,LIGNE)
              ELSE IF (NTYP .EQ. 4) THEN
                DEFCAR(DEFLU) = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     &                               TAILLE,MOTIGN,LONIGN,NMAXR,NFICDA,
     &                               LEN(DEFCAR(DEFLU)))
                L1 = LONGLU(DEFCAR(DEFLU))
                IF (ITAI.LE.1.AND.INDIC(NTYP,INDX).GE.2) THEN
                  IF (TROUVE(NTYP,INDX).LE.3) THEN
                    IF (L1.GT.0) TROUVE(NTYP,INDX)=1
                  ELSEIF(TROUVE(NTYP,INDX).EQ.6) THEN
                    TROUVE(NTYP,INDX)=7
                  ENDIF
                ELSE
                  TROUVE(NTYP,INDX)=1
                ENDIF
              ENDIF
!
              ICOL = NEXT(ICOL+1,LIGNE)
!
              IF(ICOL.GT.LONGLI) THEN
                ICOL = LONGLI
              ELSE
                IF(LIGNE(ICOL:ICOL).EQ.PTVIRG) THEN
                  DEFLU = DEFLU + 1
                  GO TO 200
                ELSE
                  ICOL=ICOL-1
                ENDIF
              ENDIF

!
!
!
            ELSE
!
! READS THE DEFAULT OF A NON-REQUESTED LANGUAGE (NOT USED)
!
 210          CONTINUE
!
              IF (NTYP .EQ. 1) THEN
                NULINT = INTLU(ICOL,LIGNE)
              ELSE IF (NTYP .EQ. 2) THEN
                NULREA = REALU(ICOL,LIGNE)
              ELSE IF (NTYP .EQ. 3) THEN
                NULLOG = LOGLU(ICOL,LIGNE)
              ELSE IF (NTYP .EQ. 4) THEN
                NULCAR = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,TAILLE,
     &                        MOTIGN,LONIGN,NMAXR,NFICDA,LEN(NULCAR))
              ENDIF
!
              ICOL = NEXT(ICOL+1,LIGNE)
!
              IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) THEN
                GO TO 210
              ELSE
                ICOL=ICOL-1
              ENDIF
            ENDIF
!
            ICOL = NEXT(ICOL+1,LIGNE)
!
!    HELP
!
          ELSE IF(NUMERO.EQ.6) THEN
!
            IF(AIDLNG.AND.DOC) THEN
              WRITE(LU,511)
511           FORMAT(1X,72('-'))
              WRITE(LU,*) PARAM(1:LONGU)
              WRITE(LU,511)
            ENDIF
            CALL AIDELU(ICOL,LIGNE,DOC.AND.AIDLNG)
            AIDLNG = .FALSE.
!
!
!    'CHOIX' 'RUBRIQUE' 'NIVEAU' 'MNEMO' 'COMPOSE' 'COMPORT' 'CONTROLE' 'APPARENCE'
!    NUMBER 7 TO 14 INCLUDED
!
          ELSE IF((NUMERO .GE. 7) .AND. (NUMERO .LE. 14)) THEN
            CALL AIDELU(ICOL,LIGNE,.FALSE.)
!
!    DEFINES A SUBMIT TYPE
          ELSE IF (NUMERO .EQ. 15) THEN
            IF (ORDRE.NE.3.AND.ORDRE.NE.4) GOTO 1500
            ORDRE=5
            IF (.NOT.(LUIGN)) INDIC(NTYP,INDX)=INDIC(NTYP,INDX)+2
            ICOL = NEXT(ICOL+1,LIGNE) -1
            CALL INFLU(ICOL,LIGNE,DEFATT,TROUVE,LUIGN,MOTCLE,TAILLE,
     &                 MOTIGN,LONIGN,NMAXR,NFICDA,GESTD)
            DO I=1,DEFLU
              DEFINT(I)    = 0
              DEFREA(I)    = 0.
              DEFLOG(I)    = .FALSE.
              DEFCAR(I)    = ' '
            ENDDO ! I
            IF (ERREUR) GO TO 900
            ICOL = NEXT(ICOL,LIGNE)
          ENDIF
!
        ENDIF
!
      ENDIF
!
      GO TO 100
900   CONTINUE
      IF(ERREUR) THEN
        WRITE(LU,*)' '
        IF(NFIC.EQ.NFICMO) THEN
          WRITE(LU,*)'-------------------------------'
          WRITE(LU,*)'- ERROR IN THE DICTIONARY     -'
          WRITE(LU,*)'-------------------------------'
          CALL PLANTE(1)
          STOP
        ELSE
          WRITE(LU,*)'-----------------------------------------'
          WRITE(LU,*)'- ERROR IN THE STEERING FILE            -'
          WRITE(LU,*)'-----------------------------------------'
          RETRY=RETRY+1
        ENDIF
        IF(RETRY.LE.1) THEN
          RETURN
        ELSE
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(NFIC.EQ.NFICMO) THEN
        IF (INDX.GT.NMAXR(NTYP)) NMAXR(NTYP)=INDX
        CALL CLASSE(DIMENS,TAILLE,MOTCLE,UTINDX,NMAX,
     &              OFFSET,ADRESS,INDIC,LUIGN,
     &              MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT ,
     &              DEFCAR,DEFINT,DEFLOG,DEFREA,DEFATT )
      ENDIF
      IF(NFICMO.EQ.NFICDA.OR.NFIC.EQ.NFICDA) THEN
!       TRUE END: 2 FILES READ OR 2 FILES IN 1 READ
        GO TO 1000
      ELSE
!       FALSE END: REMAINS A FILE
        NFIC = NFICDA
        RETOUR = .FALSE.
        GO TO 99
      ENDIF
!
1515  CONTINUE
      WRITE(LU,*)'*********************************************'
      WRITE(LU,*)'ADRESS GREATER THAN NMAX = ',NMAX
      WRITE(LU,*)'TOO MANY VALUES OF TYPE : ',NTYP,' DECLARED.'
      WRITE(LU,*)'STOP OF DAMOCLES AT KEY-WORD NUMBER: ',INDX
      WRITE(LU,*)'*********************************************'
      CALL PLANTE(1)
      STOP
!
1000  CONTINUE
!
! COMPACTS WHITE CHARS - REDISTRIBUTES - TESTS THE RESULTS
!
      DO K=1,NMAXR(4)
        IF (UTINDX(4,K).AND.INDIC(4,K).GE.2.AND.
     &      TROUVE(4,K).LT.3.AND.TROUVE(4,K).GT.0) THEN
          ADD = ADRESS(4,K)
          PARAM = MOTCLE(4,K)
          LONGU = TAILLE(4,K)
          NVAL = DIMENS(4,K)
          I=0
 1180     CONTINUE
          I=I+1
 1185     CONTINUE
! IF IT IS A WHITE CHAR (LENGTH=0):
          IF (LONGLU(MOTCAR(ADD+I-1)).EQ.0) THEN
            DO J=I,NVAL-1
              MOTCAR(ADD+J-1)=MOTCAR(ADD+J)
!
! SUBMITS DO NOT FOLLOW IF THIS LINE IS COMMENTED OUT
! OTHERWISE PB EXPERIENCED WITH STBTEL
!            MOTATT(4,ADD+J-1)=MOTATT(4,ADD+J)
!
            ENDDO ! J
            NVAL = NVAL-1
            IF (I.LE.NVAL) GO TO 1185
          ENDIF
          IF (I.LT.NVAL) GO TO 1180
!
! CASE OF EMPTY ALLOCATIONS FOR NON ARRAYS
!
          IF (NVAL.EQ.0.AND.INDIC(4,K).EQ.2) THEN
            WRITE(LU,*) 'EMPTY ALLOCATION NOT ALLOWED FOR ',
     &                  'THE KEY WORD : ', PARAM(1:LONGU)
            WRITE(LU,*)
            ARRET = .TRUE.
            GO TO 1300
          ENDIF
!
! HAS COMPACTED ARRAYS TO DIMENSION NVAL (CAN BE = 0)
          IF (NVAL.LT.DIMENS(4,K)) THEN
            DIMENS(4,K) = NVAL
            TROUVE(4,K) = 5
          ENDIF
        ENDIF
!
! CASE OF SUBMIT ARRAYS NEVER AFFECTED -> DIMENSION = 0
        IF (UTINDX(4,K).AND.INDIC(4,K).EQ.3.AND.TROUVE(4,K).EQ.0.
     &      AND.DIMENS(4,K).GT.1) THEN
          DIMENS(4,K) = 0
          TROUVE(4,K) = 3
        ENDIF
!
      ENDDO ! K
!
! CARRIES OUT THE COMMANDS RECORDED BEFORE THE END
      EXECMD = .TRUE.
!     TO AVOID TESTS ON LINE IN CMD
      LIGNE = 'NUL'
      DO K = 1,5
        VUCMD0(K) = VUCMD(K)
        VUCMD(K)  = .FALSE.
      ENDDO
!
      DO K=1,5
        VUCMD(K)=VUCMD0(K)
        IF (VUCMD(K).AND.(.NOT.(ERREUR))) THEN
          CALL CMD (ICOL,LIGNE,ADRESS,DIMENS,TROUVE,MOTCLE,NMOT,
     &          MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT,INDIC,TAILLE,
     &          UTINDX,DYNAM,VUCMD,EXECMD,NFICDA,NMAXR)
          VUCMD(K) = .FALSE.
        ENDIF
      ENDDO
!
!  LOOKS FOR REQUIRED KEYWORDS THAT HAVE NOT BEEN READ:
!
      WRITE(LU,*) ' '
!
      DO K = 1,4
      DO INDX = 1 , NMAXR(K)
        IF (UTINDX(K,INDX)) THEN
          IF (TROUVE(K,INDX).EQ.0) THEN
!
! IF NO DEFAULT VALUE AND NOTHING IN STEERING FILE, DIMENS = 0 FOR ARRAYS
            IF (DIMENS(K,INDX).NE.1) THEN
              IF (DYNAM) DIMENS(K,INDX) = 0
            ELSE
              WRITE(LU,*)'----------------------------------------'
              ARRET= .TRUE.
              WRITE(LU,1102) MOTCLE(K,INDX)(1:TAILLE(K,INDX))
 1102         FORMAT(1X,'BEWARE, THE KEY-WORD:',1X,A,/,1X,
     &        'HAS BEEN GIVEN NO VALUE')
            ENDIF
          ENDIF
!
!       WILL WRITE THE UNUSED INDICES IN EVERY CATEGORY
!
!       ELSE
!         WRITE(LU,*) 'KEYWORD ',INDX,' OF TYPE ',K,' NOT USED'
        ENDIF
      ENDDO ! INDX
      ENDDO ! K
!
1300  CONTINUE
      IF(ARRET) THEN
        WRITE(LU,*)  ' '
        WRITE(LU,*) 'DAMOC IS STOPPED'
        CALL PLANTE(1)
        STOP
      ENDIF
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
      GOTO 900
!
!-----------------------------------------------------------------------
!
      END


