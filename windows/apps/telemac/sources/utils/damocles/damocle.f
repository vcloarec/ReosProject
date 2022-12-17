!                   ******************
                    SUBROUTINE DAMOCLE
!                   ******************
!
     &( ADRESS , DIMENS , NMAX   , DOC    , LLNG , LLU ,
     &  MOTINT , MOTREA , MOTLOG , MOTCAR ,
     &  MOTCLE , TROUVE , NFICMO , NFICDA , GESTD , MOTATT )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MAIN ROUTINE OF THE DAMOCLES LIBRARY
!+             CALLED BY THE DAMOCLES EXECUTABLE (DAMOCLE.F)
!+             CALLED BY THE LNH COMPUTATIONAL CODES.
!
!history  O. QUIQUEMPOIX (LNH)
!+        14/12/1993
!+
!+
!
!history  A. DESITTER (NAG)
!+        01/05/1998
!+
!+
!
!history  J-M HERVOUET (LNH); A. YESSAYAN; L. LEGUE
!+        04/10/2005
!+        V5P6
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADRESS         |<--| TABLEAU DES ADRESSES DES MOTS CLES
!| DIMENS         |<--| TABLEAU DES DIMENSIONS DES MOTS CLES
!| DOC            |-->| LOGIQUE DE DOCUMENTATION DE LA SORTIE
!|                |   | = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
!|                |   | = FAUX : N'IMPRIME PAS L'AIDE
!| GESTD          |-->| LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
!| LLNG           |-->| NUMERO DE LA LANGUE DE DECODAGE
!| LLU            |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
!| MOTATT         |<--| TABLEAU DES SUBMITS
!| MOTCAR         |<--| TABLEAU DES VALEURS CARACTERES
!| MOTCLE         |<--| TABLEAU DES MOTS CLES ACTIFS
!| MOTINT         |<--| TABLEAU DES VALEURS ENTIERES
!| MOTLOG         |<--| TABLEAU DES VALEURS LOGIQUES
!| MOTREA         |<--| TABLEAU DES VALEURS REELLES
!| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
!| NFICMO         |-->| NUMERO DE CANAL DU FICHIER DES MOTS-CLES
!| NMAX           |-->| TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER            ,INTENT(IN)  :: NMAX,LLU,NFICMO,NFICDA
      INTEGER            ,INTENT(INOUT)  :: LLNG
      INTEGER            ,INTENT(OUT) :: MOTINT(*),ADRESS(4,*)
      INTEGER            ,INTENT(OUT) :: DIMENS(4,*),TROUVE(4,*)
      LOGICAL            ,INTENT(OUT) :: MOTLOG(*)
      LOGICAL            ,INTENT(IN)  :: DOC, GESTD
      CHARACTER(LEN=72)  ,INTENT(OUT) :: MOTCLE(4,*)
      CHARACTER(LEN=PATH_LEN) ,INTENT(OUT) :: MOTCAR(*)
      DOUBLE PRECISION   ,INTENT(INOUT) :: MOTREA(*)
!
!     AUTOMATIC ARRAYS
!
      INTEGER            :: DEFINT(NMAX),USRINT(NMAX)
      INTEGER            :: SIZE(4,NMAX)
      INTEGER            :: INDIC(4,NMAX)
      LOGICAL            :: DEFLOG(NMAX),USRLOG(NMAX),UTINDX(4,NMAX)
      CHARACTER(LEN=PATH_LEN) :: MOTATT(4,NMAX),DEFATT(NMAX),
     & USRATT(NMAX)
      CHARACTER(LEN=PATH_LEN) :: DEFCAR(NMAX),USRCAR(NMAX)
      DOUBLE PRECISION   :: DEFREA(NMAX),USRREA(NMAX)
!
      INTEGER,PARAMETER :: NBLANG = 2
      INTEGER :: RETRY,I
      RETRY = 0
!
!     CALLS DAMOC
!
      CALL DAMOC( ADRESS , DIMENS , NMAX   , DOC    , LLNG   , LLU  ,
     &            MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     &            DEFINT , DEFREA , DEFLOG , DEFCAR , DEFATT ,
     &            USRINT , USRREA , USRLOG , USRCAR , USRATT ,
     &            MOTCLE , SIZE   , TROUVE , UTINDX , NFICMO , NFICDA ,
     &            INDIC  , GESTD  , NBLANG , RETRY )
!
      IF(RETRY.EQ.1) THEN
        REWIND(NFICMO)
        REWIND(NFICDA)
        DO I=1,10
          WRITE(LLU,*)
          WRITE(LLU,*) 'DAMOCLE: TRYING ANOTHER LANGUAGE'
        ENDDO
        LLNG=3-LLNG
        CALL DAMOC( ADRESS, DIMENS, NMAX   , DOC    , LLNG   , LLU,
     &              MOTINT, MOTREA, MOTLOG , MOTCAR , MOTATT ,
     &              DEFINT, DEFREA, DEFLOG , DEFCAR , DEFATT ,
     &              USRINT, USRREA, USRLOG , USRCAR , USRATT ,
     &              MOTCLE, SIZE  , TROUVE , UTINDX , NFICMO , NFICDA,
     &              INDIC , GESTD , NBLANG , RETRY )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
