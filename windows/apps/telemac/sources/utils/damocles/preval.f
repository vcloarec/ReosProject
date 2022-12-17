!                   ***********************
                    INTEGER FUNCTION PREVAL
!                   ***********************
!
     &( ICOL , LIGNE , CAR1 , CAR2 , CAR3 )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    RETURNS THE COLUMN INDEX OF THE 1ST VALID CHARACTER CAR
!+             IN THE LINE (I.E. NON-WHITE, NON-TABULATION AND NON-COMMENTED
!+             STRING)
!+             IF CANNOT FIND IT, RETURNS LONGLI + 1
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!history  O. QUIQUEMPOIX (LNH)
!+        15/12/1993
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CAR1,CAR2,CAR3 |-->| CARACTERES RECHERCHES DANS LA LIGNE
!| ICOL           |-->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
!| LIGNE          |-->| LIGNE EN COURS DE DECODAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_DAMOCLES
      IMPLICIT NONE
!
      INTEGER       ICOL
      CHARACTER(LEN=1)   CAR1,CAR2,CAR3
      CHARACTER(LEN=*) LIGNE
!
!-----------------------------------------------------------------------
!
      INTEGER       I,J
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      PREVAL = LONGLI + 1
      I      = ICOL -1
!
100   CONTINUE
      I = I + 1
      IF (LIGNE(I:I).NE.CAR1.AND.LIGNE(I:I).NE.CAR2.AND.
     &    LIGNE(I:I).NE.CAR3) THEN
!-----------------------------------------------------------------------
!       DOES NOT CONSIDER THE COMMENTED LINES:
!
        IF ( I.GE.LONGLI ) GO TO 1000
        IF ( LIGNE(I:I).EQ.'/' ) THEN
          DO J = I+1 , LONGLI
            IF ( LIGNE(J:J).EQ.'/' ) THEN
              I = J
              GO TO 100
            ENDIF
          ENDDO ! J
          GO TO 1000
!--------------------------------------------------------------------
        ELSE
          GO TO 100
        ENDIF
      ELSE
        PREVAL = I
      ENDIF
!
1000  CONTINUE
!
      RETURN
      END
