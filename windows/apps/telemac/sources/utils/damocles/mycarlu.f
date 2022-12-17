!                   ***********************************
                    CHARACTER(LEN=PATH_LEN) FUNCTION MYCARLU
!                   ***********************************
!
     &( LCAR   , ICOL  , LIGNE  , EXTREM , LGVAR  )
!
!***********************************************************************
! DAMOCLES   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    DECODES A CHARACTER STRING, FROM COLUMN ICOL+1 OF THE
!+             CURRENT LINE (MAXIMUM OF LGA CHARACTERS).
!+             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!+             IF NEED BE.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER
!+             OR TO ICOL=0 IF THE NEXT LINE WAS READ WITH NO REASON.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!warning  FOLLOWS THE FORTRAN CONVENTION : '' IS READ AS
!+            ' WHEN WITHIN A CHARACTER STRING
!+
!warning  STRINGS WITHOUT ' OR " CANNOT CONTAIN SEPARATOR
!+            CHARACTERS
!
!history  O. QUIQUEMPOIX (LNH)
!+        14/12/1993
!+
!+
!
!history  J.M. HERVOUET (LNH); A. YESSAYAN
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
!history  J.M. HERVOUET (EDF R&D, LNHE)
!+        31/12/2013
!+        V7P0
!+   Prints of LIGNED limited to maximum size LGVAR.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EXTREM         |-->| SEPARATEUR DE CHAINE = ' OU "
!| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
!| LCAR           |<--| LONGUEUR DE LA CHAINE DE CARACTERES
!| LGVAR          |-->| LONGUEUR MAXIMUM DE LA CHAINE A LIRE
!| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_DAMOCLES
      IMPLICIT NONE
!
      INTEGER       LCAR,ICOL,LGVAR
      CHARACTER(LEN=*) LIGNE
      CHARACTER(LEN=1) EXTREM
!
      INTEGER  NEXT,PRECAR,LONGLU
      EXTERNAL NEXT,PRECAR,LONGLU
!
!
!-----------------------------------------------------------------------
!
      INTEGER       I,IDEB,IFIN,NCAR,ICOL2,NLIGN2,LGLU
      INTEGER       QCAS
      LOGICAL       COTE,LISUIV,LUFIC,LUCOTE
      CHARACTER(LEN=1)   QUOTE,TABUL
      CHARACTER(LEN=72)  LIGNE2
      CHARACTER(LEN=PATH_LEN) LIGNED
!
!-----------------------------------------------------------------------
!
!
      INTRINSIC CHAR
!
!-----------------------------------------------------------------------
!
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      COTE   = .FALSE.
      LISUIV = .FALSE.
      LUFIC  = .FALSE.
      LUCOTE = .FALSE.
      LCAR   = 1
      MYCARLU  = ' '
      QUOTE  = ''''
      TABUL  = CHAR(9)
      NLIGN2 = NLIGN
      ICOL2  = ICOL
      LIGNE2 = LIGNE(1:MIN(72,LEN(LIGNE)))
      LIGNED = ' '
      LGLU   = 0
      QCAS   = 0
!
      ICOL   = NEXT( ICOL+1 , LIGNE )
!
!        //// FINDS THE ENDS OF THE STRING ////
!
!    NOTE: THE STRING CAN BE BETWEEN QUOTES OR WITHOUT QUOTES
!          IT CANNOT CONTAIN WHITE CHARACTERS IF THERE ARE
!          NO QUOTES
!
      IF ( LIGNE(ICOL:ICOL).NE.EXTREM ) THEN
        IDEB = ICOL
!              PRECAR : SAME ROLE AS PREVAL, EXCEPT IT DOES NOT
!                       SKIP COMMENTED LINES
        ICOL = PRECAR ( ICOL+1 , LIGNE , ' ' , ';' , TABUL) - 1
        IFIN = ICOL
        LIGNED = LIGNE(IDEB:IFIN)
        LGLU = IFIN-IDEB+1
!
! STEERING FILE : GOES TO THE NEXT, WHEN GETS TO THE END OF A LINE
!
290     IF (IFIN.GE.LONGLI) THEN
          LISUIV = .TRUE.
          LUFIC = .TRUE.
          READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
          ICOL2 = 0
          IF (LIGNE2(1:1).EQ.'&'.OR.
     &        LIGNE2(1:1).EQ.'='.OR.LIGNE2(1:1).EQ.':'.OR.
     &        LIGNE2(1:1).EQ.';'.OR.LIGNE2(1:1).EQ.'/' ) THEN
            LISUIV = .FALSE.
            GO TO 96
          ENDIF
!
! CHECKS IF IT'S A KNOWN KEYWORD FOR THE STEERING FILE
!
!
! GETS TO THIS POINT IF/WHEN HAS TO READ THE NEXT LINE
!
          ICOL2 =PRECAR (1 , LIGNE2 , ' ' , TABUL ,' ') - 1
!
          LGLU = LGLU + ICOL2
!
          IF(LGLU.GT.LGVAR) THEN
            ERREUR = .TRUE.
            IF (LONGLU(LIGNED).GT.0) THEN
              LIGNED = LIGNED(1:LONGLU(LIGNED))//LIGNE2(1:ICOL2)
            ELSE
              LIGNED = LIGNE2(1:ICOL2)
            ENDIF
            IF(LGLU.GT.0) WRITE(LU,'(1X,A)')
     &                LIGNED(1:MIN(LGLU,PATH_LEN))
            WRITE(LU,*) ' '
            WRITE(LU,'(1X,A5,I4,1X,A23)') 'LINE: ',NLIGN,
     &              'ERROR : STRING TOO LONG'
            ICOL = ICOL -1
            GO TO 1000
          ELSE
!           NEEDS TO READ ANOTHER LINE - SIMULATES A SHIFT OF LINE
            LISUIV = .FALSE.
            LIGNE = LIGNE2
            IF (LONGLU(LIGNED).GT.0) THEN
              LIGNED = LIGNED(1:LONGLU(LIGNED))//LIGNE2(1:LONGLI)
            ELSE
              LIGNED = LIGNE2(1:LONGLI)
            ENDIF
            NLIGN = NLIGN2
            ICOL = ICOL2
            IFIN = LONGLI+1
            GO TO 290
          ENDIF
  96      IF(LISUIV) THEN
            IF(LONGLU(LIGNED).GT.0) THEN
              LIGNED = LIGNED(1:LONGLU(LIGNED))//LIGNE2(1:ICOL2)
            ELSE
              LIGNED = LIGNE2(1:LONGLI)
            ENDIF
            IFIN = LGLU+ICOL2
            IDEB = 1
          ENDIF
        ENDIF
!
        GO TO 901
 900    CONTINUE
        RETOUR = .TRUE.
 901    CONTINUE
        DO I = 1 , LGLU
          IF (LIGNED(I:I).EQ.QUOTE.OR.LIGNED(I:I).EQ.'&'.OR.
     &       LIGNED(I:I).EQ.'='.OR.LIGNED(I:I).EQ.':'.OR.
     &       LIGNED(I:I).EQ.'/') THEN
            IF (NLIGN2.NE.NLIGN.AND.(.NOT.(LUFIC)))
     &             WRITE(LU,'(1X,A)') LIGNE2(1:LONGLI)
            IF (LGLU.GT.0) WRITE(LU,'(1X,A)') LIGNED(1:LGLU)
              WRITE(LU,'(1X,A5,I4,A)') 'LINE: ',NLIGN,
     &       ' ERROR: UNEXPECTED CHARACTER IN A STRING WITHOUT QUOTES'
            ERREUR = .TRUE.
            GO TO 1000
          ENDIF
        ENDDO ! I
!
      ELSE
!
! CASE WHERE THERE ARE QUOTES
!
        IDEB = ICOL + 1
!
! THE 1ST QUOTE IS IN LAST POSITION (QCAS=4 OR QCAS=5)
        IF (ICOL.EQ.LONGLI) QCAS=45
!
 100    ICOL   = PRECAR ( ICOL+1 , LIGNE , EXTREM , EXTREM , EXTREM )
        IF (ICOL.EQ.LONGLI) ICOL = LONGLI+1
!
! CASE WHERE DOUBLE QUOTES CAN BE FOUND IN THE 1ST LINE EXCEPT IN COLUMN 72
!
        IF(ICOL.LT.LONGLI) THEN
          IF(LIGNE(ICOL+1:ICOL+1).EQ.EXTREM.AND.EXTREM.EQ.QUOTE) THEN
            ICOL = ICOL + 1
! THE QUOTE IN 72 IS THE 2ND QUOTE OF A DOUBLE QUOTE (QCAS=3)
            IF (ICOL.EQ.LONGLI) QCAS=3
            COTE = .TRUE.
            GO TO 100
          ENDIF
        ENDIF
!
        LGLU = MAX(0,ICOL-IDEB)
        IF (LGLU.GT.0) LIGNED = LIGNE(IDEB:ICOL-1)
!
! HAS NOT FOUND THE END, OR A QUOTE WAS FOUND IN COLUMN 72
!
        IF (ICOL.GT.LONGLI) THEN
390       LISUIV = .TRUE.
          LUFIC = .TRUE.
          READ(NFIC,END=905,ERR=998,FMT='(A)') LIGNE2
!
! CASE WHERE THE PRECEDING LINE ENDS WITH A QUOTE
!
          IF (LIGNE(LONGLI:LONGLI).EQ.QUOTE) THEN
! THE QUOTE IN COLUMN 72 STARTS A STRING, OR IS THE 2ND OF A DOUBLE QUOTE
            IF (QCAS.EQ.45.OR.QCAS.EQ.3) THEN
              QCAS=0
            ELSEIF (LIGNE2(1:1).EQ.QUOTE) THEN
              COTE = .TRUE.
              LUCOTE = .TRUE.
              QCAS=0
            ELSE
              LGLU=LGLU-1
              IF (LGLU.GT.0) LIGNED = LIGNED(1:LGLU)
              LISUIV = .FALSE.
              QCAS=0
              GO TO 920
            ENDIF
          ENDIF
!
          ICOL2 = 0
          IF(LIGNE2(1:1).EQ.QUOTE.AND.LUCOTE) THEN
            LUCOTE = .FALSE.
            ICOL2=1
          ENDIF
 110      ICOL2 =PRECAR (ICOL2+1,LIGNE2,EXTREM,EXTREM,EXTREM)
          IF(ICOL2.LT.LONGLI) THEN
          IF(LIGNE2(ICOL2+1:ICOL2+1).EQ.
     &      EXTREM.AND.EXTREM.EQ.QUOTE) THEN
!           ICOL2 = PRECAR(ICOL2+1,LIGNE2,EXTREM,EXTREM,EXTREM)
            ICOL2=ICOL2+1
            COTE=.TRUE.
            IF (ICOL2.EQ.LONGLI) QCAS=3
            GO TO 110
          ENDIF
          ENDIF
          IF(ICOL2.EQ.LONGLI) ICOL2=ICOL2+1
          IF(LGLU.GT.0) THEN
            LIGNED = LIGNED(1:LGLU)//LIGNE2(1:ICOL2-1)
          ELSE
            LIGNED = LIGNE2(1:ICOL2-1)
          ENDIF
          LGLU = LGLU + ICOL2-1
!
          IF(LGLU.GT.LGVAR) GO TO 910
!
! GOES TO NEXT LINE IF NOT COMPLETE, OR IF HAS FOUND A QUOTE IN 72
!
          IF(ICOL2.GE.LONGLI) THEN
            LISUIV = .FALSE.
            LIGNE = LIGNE2
            NLIGN = NLIGN2
            ICOL = ICOL2
            IFIN = ICOL2
            GO TO 390
          ENDIF
! HERE IT'S OK
          GO TO 920
!
 905      CONTINUE
          RETOUR = .TRUE.
!
 910      CONTINUE
          WRITE(LU,'(1X,A)') LIGNED(1:MAX(1,MIN(LGLU,LGVAR)))
          WRITE(LU,*)
          WRITE(LU,'(1X,A5,I4,A)') 'LINE: ',NLIGN,
     &    ' ERROR: QUOTE MISSING AT THE END OF THE STRING'
          WRITE(LU,*)'OR STRING TOO LONG ... '
          ERREUR = .TRUE.
          ICOL = LONGLI
          GO TO 1000
!
        ENDIF
        IFIN   = ICOL - 1
      ENDIF
!
 920  CONTINUE
      IF(LGLU.NE.0) THEN
        LCAR = MIN(LGLU,LGVAR)
        MYCARLU = LIGNED(1:LGLU)
      ENDIF
!
!  CHANGES DOUBLE QUOTES WITH SIMPLE QUOTES
!
      IF(COTE) THEN
        NCAR = LCAR
        I = 1
 200    CONTINUE
        IF(I.GT.NCAR) THEN
          LCAR = NCAR
          GO TO 1000
        ENDIF
        IF(MYCARLU(I:I).EQ.QUOTE.AND.MYCARLU(I+1:I+1).EQ.QUOTE) THEN
          MYCARLU(I+1:LCAR) = MYCARLU(I+2:LCAR)//' '
          NCAR = NCAR - 1
        ENDIF
        I = I + 1
        GO TO 200
      ENDIF
!
1000  CONTINUE
!
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (LISUIV) THEN
          ICOL = ICOL2
        ELSE
          ICOL = 0
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
998   CONTINUE
      WRITE(LU,1999) NFIC,NLIGN+1
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
!
!-----------------------------------------------------------------------
!
      END
