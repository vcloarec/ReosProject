!                   **********************
                    LOGICAL FUNCTION LOGLU
!                   **********************
!
     &( ICOL , LIGNE )
!
!***********************************************************************
! DAMOCLES   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    DECODES A LOGICAL VALUE, FROM COLUMN ICOL+1 OF THE LINE.
!+             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!+             IF NEED BE.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!+             OR TO ICOL=0 IF THE NEXT LINE WAS READ.
!
!warning  ACCEPTED VALUES ARE (UPPER OR LOWER CASE):
!+            VRAI OUI TRUE  YES .TRUE.  1
!+            FAUX NON FALSE NO  .FALSE. 0
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
!+        11/03/2013
!+        V6P3
!+   Slight modification for avoiding to read beyong the actual size
!+   of LIGNE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
!| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_DAMOCLES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,       INTENT(INOUT) :: ICOL
      CHARACTER(LEN=*), INTENT(INOUT) :: LIGNE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  NEXT,PRECAR
      EXTERNAL NEXT,PRECAR
!
!-----------------------------------------------------------------------
!
      INTEGER       I1,I2
      CHARACTER(LEN=1)   TABUL
      CHARACTER(LEN=7)   L
      CHARACTER(LEN=72)  LIGNE2
      LOGICAL       LUFIC,LISUIV
!
!-----------------------------------------------------------------------
!
      INTRINSIC CHAR
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      LUFIC  = .FALSE.
      LISUIV = .FALSE.
      LIGNE2 = ' '
      TABUL  = CHAR(9)
!
      I1 = NEXT( ICOL+1 , LIGNE )
      I2 = PRECAR(I1,LIGNE,' ',';',TABUL)
!
!     CASE WHERE WE MIGHT HAVE TO READ THE FOLLOWING LINE
!
      IF(I2.GT.LONGLI.AND.I1+6.GT.LONGLI) THEN
        LUFIC=.TRUE.
        READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
        IF(I1.LE.LONGLI) THEN
          L(1:7)=LIGNE(I1:LONGLI)//LIGNE2(1:(7-(LONGLI-I1+1)))
        ELSE
          L(1:7)=LIGNE2(1:7)
        ENDIF
        I2 = 0
        I2 = PRECAR(I2+1,LIGNE2,' ',';',TABUL)
      ELSEIF(I1+6.GT.LONGLI) THEN
        L(2:7) = '      '
        L(1:LONGLI-I1+1)= LIGNE(I1:LONGLI)
      ELSE
        L(1:7) = LIGNE(I1:I1+6)
      ENDIF
      CALL MAJUS(L)
      GO TO 910
!
 900  CONTINUE
      RETOUR = .TRUE.
!
 910  CONTINUE
!
! ORDERED IN THE MOST PROBABLE ORDER: NON OUI NO YES 0 1 ...
!
      IF(L(1:3).EQ.'NON') THEN
        LOGLU = .FALSE.
        ICOL = I1 + 2
      ELSEIF(L(1:2).EQ.'NO') THEN
        LOGLU = .FALSE.
        ICOL = I1 + 1
      ELSEIF(L(1:3).EQ.'OUI' ) THEN
        LOGLU = .TRUE.
        ICOL = I1 + 2
      ELSEIF(L(1:3).EQ.'YES' ) THEN
        LOGLU = .TRUE.
        ICOL = I1 + 2
      ELSEIF(L(1:1).EQ.'0') THEN
        LOGLU = .FALSE.
        ICOL = I1
      ELSEIF(L(1:1).EQ.'1') THEN
        LOGLU = .TRUE.
        ICOL = I1
      ELSEIF(L(1:7).EQ.'.FALSE.' ) THEN
        LOGLU = .FALSE.
        ICOL = I1 + 6
      ELSEIF(L(1:5).EQ.'FALSE' ) THEN
        LOGLU = .FALSE.
        ICOL = I1 + 4
      ELSEIF(L(1:4).EQ.'FAUX') THEN
        LOGLU = .FALSE.
        ICOL = I1 + 3
      ELSEIF(L(1:6).EQ.'.TRUE.' ) THEN
        LOGLU = .TRUE.
        ICOL = I1 + 5
      ELSEIF(L(1:4).EQ.'TRUE' ) THEN
        LOGLU = .TRUE.
        ICOL = I1 + 3
      ELSEIF(L(1:4).EQ.'VRAI' ) THEN
        LOGLU = .TRUE.
        ICOL = I1 + 3
      ELSE
!
!       ERROR: NOT A LOGICAL VALUE
!
        ERREUR = .TRUE.
        WRITE(LU,'(1X,A)') LIGNE(1:LONGLI)
        IF(LUFIC) WRITE(LU,'(1X,A)') LIGNE2(1:LONGLI)
        WRITE(LU,*) ' '
        WRITE(LU,'(1X,A6,I4,A)') 'LOGLU (UTILE) : LINE: ',NLIGN,
     &                           ' WRONG LOGICAL VALUE'
        LOGLU = .FALSE.
        GO TO 1000
!
      ENDIF
!
!       //// UPDATES THE POINTER ////
!
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF(ICOL.GT.LONGLI) LISUIV = .TRUE.
        IF(LISUIV) THEN
          ICOL = I2-1
        ELSE
          ICOL = 0
        ENDIF
      ELSE
        ICOL = I2 - 1
      ENDIF
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
!
998   CONTINUE
      WRITE(LU,1999) NFIC,NLIGN+1
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
!
!-----------------------------------------------------------------------
!
      RETURN
      END
