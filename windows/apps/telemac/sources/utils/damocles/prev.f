!                   *********************
                    INTEGER FUNCTION PREV
!                   *********************
!
     &( ICOL , LIGNE )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    RETURNS THE INDEX OF THE 1ST NON-WHITE, NON-TABULATION
!+             AND NON-COMMENT CHARACTER OF THE LINE, BEFORE COLUMN ICOL
!+             COLUMN ICOL IS EXCLUDED
!+             IF CANNOT FIND ANY, PREV = ICOL
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
!| ICOL           |-->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
!| LIGNE          |-->| LIGNE EN COURS DE DECODAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER       ICOL
      CHARACTER(LEN=*) LIGNE
!
!-----------------------------------------------------------------------
!
      INTEGER       I,J
      CHARACTER(LEN=1)   TABUL
      INTRINSIC CHAR
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      TABUL = CHAR(9)
      PREV   = ICOL
      I      = ICOL
!
  100 CONTINUE
      I = I - 1
      IF ( I.LT.1 ) GO TO 1000
!
      IF (LIGNE(I:I).EQ.' '.OR.LIGNE(I:I).EQ.TABUL) GOTO 100
!
!-----------------------------------------------------------------------
!       DOES NOT CONSIDER THE COMMENTED LINES:
!
        IF ( LIGNE(I:I).NE.'/' ) THEN
          PREV = I
          GO TO 1000
        ELSE
          IF ( I.LE.1 ) GO TO 1000
          DO J = I-1 , 1 , -1
            IF ( LIGNE(J:J).EQ.'/' ) THEN
              I = J
              GO TO 100
            ENDIF
          ENDDO ! J
        ENDIF
!-----------------------------------------------------------------------
!
 1000 CONTINUE
!
      RETURN
      END
