!                   ***********************
                    INTEGER FUNCTION PRECAR
!                   ***********************
!
     &( ICOL , LIGNE , CAR1 , CAR2 , CAR3 )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    RETURNS THE COLUMN INDEX OF THE 1ST CHARACTER CAR
!+             IN THE LINE (EVEN IF IT FOLLOWS '/')
!+             RETURNS THE MAXIMUM LENGTH OF THE LINE IF THIS CHARACTER
!+             IS NOT FOUND.
!+
!+             THIS FUNCTION IS USED TO FIND THE END OF A STRING OF
!+             CHARACTERS. THIS STRING CAN CONTAIN THE CHARACTER '/',
!+             WHICH IS WHY PREVAL IS NOT USED IN THIS CASE (PREVAL
!+             SKIPS COMMENTED LINES).
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!note      OPTIMISED: SENDS CAR1, CAR2, CAR3 IN THE MOST PROBABLE ORDER
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
      INTEGER       K
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      PRECAR = LONGLI
!
      DO K = ICOL,LONGLI
      IF (LIGNE(K:K).EQ.CAR1.OR.LIGNE(K:K).EQ.CAR2.OR.
     &    LIGNE(K:K).EQ.CAR3) THEN
        PRECAR = K
        GO TO 1000
      ENDIF
      ENDDO ! K
!
      PRECAR=LONGLI+1
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
