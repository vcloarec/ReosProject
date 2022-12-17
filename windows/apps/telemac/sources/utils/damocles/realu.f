!                   *******************************
                    DOUBLE PRECISION FUNCTION REALU
!                   *******************************
!
     &( ICOL , LIGNE )
!
!***********************************************************************
! DAMOCLES   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    DECODES A REAL, FROM COLUMN ICOL+1 OF THE LINE.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.
!+             ACCEPTS F FORMAT OR E FORMAT.
!+             ACCEPTS REALS WITH DECIMAL POINTS; ACCEPTS ',' FOR '.'.
!+             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!+             IF NEED BE.
!+             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER
!+             OR TO ICOL=0 IF THE NEXT LINE WAS READ.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!warning  IF THE VALUE READ IS NOT A REAL, COULD YIELD A
!+            NON-CONTROLLED ERROR BY THE PROGRAM
!
!history  J.M. HERVOUET (LNH); A. YESSAYAN
!+        30/09/1993
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
!history  J-M HERVOUET (LNHE)
!+        30/05/2012
!+   Test  CDEB.EQ.'E'.OR.CDEB.EQ.'E'.OR.CDEB.EQ.'D'.OR.CDEB.EQ.'D'
!+   Replaced by  CDEB.EQ.'E'.OR.CDEB.EQ.'D', other stupid tests alike.
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
      INTEGER          ICOL
      CHARACTER(LEN=*)    LIGNE
!
      INTEGER          NEXT,PREVAL
      EXTERNAL         NEXT,PREVAL
!
!-----------------------------------------------------------------------
!
      INTRINSIC LOG10,DBLE,INT,CHAR
!
      INTEGER          I,I1,I2,ILONG,IPOINT,IFDECI,ILDECI,JD1,JD2,I3
      LOGICAL          FORMAE,LUFIC,LISUIV,VUPOIN
      CHARACTER(LEN=1)      CODE,CDEB,CDEB2,TABUL
      CHARACTER(LEN=3)      LLONG,LLDECI
      CHARACTER(LEN=72)     FORMA,LIGNE2
      DOUBLE PRECISION RSIGNE , RVAL
!
!***********************************************************************
!
      LUFIC = .FALSE.
      LISUIV = .FALSE.
      VUPOIN = .FALSE.
      TABUL = CHAR(9)
!
      I1     = NEXT( ICOL+1 , LIGNE )
!
!     //// DETERMINES THE FORMAT: F OR E ////
!
      FORMAE = .FALSE.
!
!     //// DECODES THE SIGN IF NEED BE ////
!
      RSIGNE = +1.D0
      IF ( LIGNE(I1:I1).EQ.'-' ) THEN
        RSIGNE = -1.D0
        I1     =   NEXT ( I1+1      , LIGNE )
      ELSE IF ( LIGNE(I1:I1).EQ.'+' ) THEN
        RSIGNE = +1.D0
        I1     =   NEXT ( I1+1      , LIGNE )
      ENDIF
!
!     //// SEEKS THE FIRST WHITE CHARACTER FOLLOWING THE NUMBER ////
!                       OR A SEPARATOR ';'
!
      I2     = PREVAL (  I1  , LIGNE ,  ' ' , ';' ,TABUL)
!
!     CASE WHERE THE REAL DOES NOT FINISH ON THE LINE                                                                                                                                                                                                                                                                                                                                                                                                                                              LINE
!
      IF (I2.GT.LONGLI) THEN
        LUFIC=.TRUE.
        READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
        CDEB = LIGNE2(1:1)
        CDEB2 = LIGNE2(2:2)
!
        IF ((CDEB.EQ.'0'.OR.CDEB.EQ.'1'.OR.CDEB.EQ.'2'.OR.
     &       CDEB.EQ.'3'.OR.CDEB.EQ.'4'.OR.CDEB.EQ.'5'.OR.
     &       CDEB.EQ.'6'.OR.CDEB.EQ.'7'.OR.CDEB.EQ.'8'.OR.
     &       CDEB.EQ.'9'.OR.CDEB.EQ.'.'.OR.CDEB.EQ.'+'.OR.
     &       CDEB.EQ.'-'.OR.CDEB.EQ.',')
!
     &     .OR.
!
! CASE WHERE IT DEPENDS ON THE SECOND CHARACTER OF THE FOLLOWING LINE
!
     &     ( (CDEB.EQ.'E'.OR.CDEB.EQ.'D')
     &     .AND.
     &     ( CDEB2.EQ.'0'.OR.CDEB2.EQ.'1'.OR.CDEB2.EQ.'2'.OR.
     &       CDEB2.EQ.'3'.OR.CDEB2.EQ.'4'.OR.CDEB2.EQ.'5'.OR.
     &       CDEB2.EQ.'6'.OR.CDEB2.EQ.'7'.OR.CDEB2.EQ.'8'.OR.
     &       CDEB2.EQ.'9'.OR.CDEB2.EQ.'+'.OR.CDEB2.EQ.'-'    )))
!
     &     THEN
!
          LISUIV = .TRUE.
          I3=1
          I3=PREVAL(I3,LIGNE2 , ' ' , ';' ,TABUL)
          IF (I1.LE.LONGLI) THEN
            LIGNE = LIGNE(I1:LONGLI)//LIGNE2(1:I3)
          ELSE
            LIGNE = LIGNE2(1:I3)
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
!
!     ILONG: LENGTH OF THE REAL
      ILONG  = I2 - I1
      IPOINT = I2 - 1
      IFDECI = I2 - 1
      DO I = I1 , I2-1
!       ACCEPTS '.' AND ','
        IF ( LIGNE(I:I).EQ.'.' ) THEN
          IPOINT = I
          VUPOIN=.TRUE.
        ELSEIF ( LIGNE(I:I).EQ.',' ) THEN
          LIGNE(I:I)='.'
          IPOINT = I
          VUPOIN=.TRUE.
        ELSEIF (LIGNE(I:I).EQ.'E') THEN
!       ACCEPTS BOTH FORMATS E AND D
          FORMAE = .TRUE.
          IFDECI = I-1
        ELSEIF (LIGNE(I:I).EQ.'D') THEN
          LIGNE(I:I)='E'
          FORMAE = .TRUE.
          IFDECI = I-1
        ENDIF
      ENDDO ! I
!
!     //// NUMBER OF DECIMAL POINTS ///
!
      IF (VUPOIN) THEN
        ILDECI = IFDECI - IPOINT
      ELSE
        ILDECI = 0
      ENDIF
!
!     //// DECODING FORMAT ////
!
      CODE = 'F'
      IF ( FORMAE ) CODE = 'E'
      JD1 = 3 - INT(LOG10(DBLE(ILONG)))
      WRITE (LLONG,'(I3)') ILONG
      JD2 = 3
      IF ( ILDECI.GT.0 ) JD2 = 3-INT(LOG10(DBLE(ILDECI)))
      WRITE (LLDECI,'(I3)') ILDECI
      IF ( I1.GT.1 ) THEN
        WRITE ( FORMA , 1010 )  I1-1,CODE,LLONG(JD1:3),LLDECI(JD2:3)
      ELSE
        WRITE ( FORMA , 1020 )  CODE,LLONG(JD1:3),LLDECI(JD2:3)
      ENDIF
!
1010  FORMAT('(',I3,'X,',A1,A,'.',A,')' )
1020  FORMAT('(',A1,A,'.',A,')' )
!
!     ////  DECODES ////
!
      READ  ( LIGNE , FORMA , ERR=995 ) RVAL
      REALU = RSIGNE * RVAL
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
!-----------------------------------------------------------------------
!
      RETURN
!
! TREATS THE ERRORS DUE TO THE INTERNAL READ FOR CONVERSION
!
995   CONTINUE
      WRITE(LU,1996) NLIGN
      WRITE(LU,*) LIGNE
1996  FORMAT(1X,'ERREUR LINE ',1I6,', REAL EXPECTED : ',/)
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
