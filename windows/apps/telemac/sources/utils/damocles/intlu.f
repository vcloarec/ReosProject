!                   **********************
                    INTEGER FUNCTION INTLU
!                   **********************
!
     &( ICOL , LIGNE )
!
!***********************************************************************
! DAMOCLES   V7P1
!***********************************************************************
!
!brief    DECODES AN INTEGER, FROM COLUMN ICOL+1 OF THE LINE.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!+             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!+             IF NEED BE.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!+             OR TO ICOL=0 IF THE NEXT LINE WAS READ.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!warning  IF THE VALUE READ IS NOT AN INTEGER, COULD YIELD A
!+            NON-CONTROLLED ERROR BY THE PROGRAM
!
!history  J.M. HERVOUET (LNH); A. YESSAYAN
!+        30/09/1993
!+        V5P1
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
!history  J.M. HERVOUET (EDF LAB, LNHE)
!+        23/06/2015
!+        V7P1
!+   Mixture of French and English corrected in an error message.
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
      INTEGER, INTENT(INOUT)       :: ICOL
      CHARACTER(LEN=*), INTENT(INOUT) :: LIGNE
!
      INTEGER          NEXT,PREVAL
      EXTERNAL NEXT,PREVAL
!
!-----------------------------------------------------------------------
!
      INTRINSIC LOG10,DBLE,INT,CHAR
!
      INTEGER           I1,I2,ILONG,ISIGNE,IVAL,JD1,I3
      LOGICAL           LUFIC,LISUIV
      CHARACTER(LEN=1)  CDEB,TABUL
      CHARACTER(LEN=3)  LLONG
      CHARACTER(LEN=72) LIGNE2,FORMA
!
!***********************************************************************
!
      LUFIC = .FALSE.
      LISUIV = .FALSE.
      TABUL = CHAR(9)
!
      I1     = NEXT( ICOL+1 , LIGNE )
!
!     //// DECODES THE SIGN IF NEED BE ////
!
      IF ( LIGNE(I1:I1).EQ.'-' ) THEN
        ISIGNE = -1
        I1     =   NEXT ( I1+1      , LIGNE )
      ELSE IF ( LIGNE(I1:I1).EQ.'+' ) THEN
        ISIGNE = +1
        I1     =   NEXT ( I1+1      , LIGNE )
      ELSE
        ISIGNE = +1
      ENDIF
!
!     //// SEEKS THE FIRST WHITE CHARACTER FOLLOWING THE NUMBER ////
!                       OR A SEPARATOR ';'
!
      I2 = PREVAL (  I1  , LIGNE ,  ' ' , ';' , TABUL)
!
!     CASE WHERE THE INTEGER DOES NOT FINISH ON THE LINE                                                                                                                                                                                                                                                                                                                                                                                                                                              LINE
!
      IF (I2.GT.LONGLI) THEN
        LUFIC=.TRUE.
        READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
        CDEB = LIGNE2(1:1)
        IF (CDEB.EQ.'0'.OR.CDEB.EQ.'1'.OR.CDEB.EQ.'2'.OR.
     &      CDEB.EQ.'3'.OR.CDEB.EQ.'4'.OR.CDEB.EQ.'5'.OR.
     &      CDEB.EQ.'6'.OR.CDEB.EQ.'7'.OR.CDEB.EQ.'8'.OR.
     &      CDEB.EQ.'9'.OR.CDEB.EQ.'.') THEN
          LISUIV = .TRUE.
          I3=1
          I3=PREVAL(I3,LIGNE2 , ' ' , ';', TABUL)
          IF (I1.LE.LONGLI) THEN
            LIGNE = LIGNE(I1:LONGLI)//LIGNE2(1:I3)
          ELSE
            LIGNE =LIGNE2(1:I3)
          ENDIF
          I2 = LONGLI-I1+1+I3
          I1 = 1
        ENDIF
      ENDIF
      GOTO 910
!
 900  CONTINUE
      RETOUR = .TRUE.
 910  CONTINUE
!     ACCEPTS THE CASE WHERE A USER WRITES AN INTEGER IN
!     REAL FORM WITH A POINT AT THE END
      IF(LIGNE(I2-1:I2-1).EQ.'.') THEN
        LIGNE(I2-1:I2-1)=' '
        I2 = I2 - 1
      ENDIF
!
!     ILONG: LENGTH OF THE INTEGER
      ILONG  = I2 - I1
!
!     //// DECODING FORMAT ////
!
      JD1 = 3 - INT(LOG10(DBLE(ILONG)))
      WRITE ( LLONG , '(I3)' ) ILONG
!
      IF(I1.EQ.1) THEN
        WRITE (FORMA , 1101 )  LLONG(JD1:3)
      ELSE
        WRITE (FORMA , 1100 )  I1-1 , LLONG(JD1:3)
      ENDIF
!
!     ////  DECODES ////
!
      READ  ( LIGNE , FORMA , ERR=995 ) IVAL
      INTLU = ISIGNE * IVAL
!
!     //// UPDATES THE POINTER ////
!
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (LISUIV) THEN
          ICOL = I3-1
        ELSE
          ICOL = 0
        ENDIF
      ELSE
        ICOL = I2 - 1
      ENDIF
!
1100  FORMAT('(',I3,'X,I',A,')')
1101  FORMAT('(I',A,')')
!
!-----------------------------------------------------------------------
!
      RETURN
!
! TREATS THE ERRORS DUE TO THE INTERNAL READ FOR CONVERSION
!
995   CONTINUE
      WRITE(LU,1996) NLIGN
      WRITE(LU,*) LIGNE
1996  FORMAT(1X,'ERROR LINE ',1I6,', INTEGER EXPECTED : ',/)
      ERREUR=.TRUE.
      RETURN
!
! TREATS THE ERRORS DUE TO FILE MISREADING
!
998   CONTINUE
      WRITE(LU,1999) NFIC,NLIGN+1
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
!
      END
