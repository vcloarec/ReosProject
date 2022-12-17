!                   ***********************
                    INTEGER FUNCTION LONGLU
!                   ***********************
!
     &( LIGNE )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    RETURNS THE POSITION OF THE LAST NON-WHITE AND NON-
!+             TABULATION CHARACTER OF THE LINE IN ARGUMENT.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!history  O. QUIQUEMPOIX (LNH)
!+        15/12/1993
!+
!+
!
!history  J.M. HERVOUET (LNH); A. YESSAYAN; L. LEGUE
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
!| LIGNE          |-->| ARGUMENT A ANALYSER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      CHARACTER(LEN=*) LIGNE
!
!-----------------------------------------------------------------------
!
      INTEGER       I,LONG
      CHARACTER(LEN=1)   TABUL
      INTRINSIC CHAR
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      TABUL = CHAR(9)
      LONG = LEN(LIGNE)
      IF (LONG .EQ. 0) THEN
        I = 0
        GO TO 110
      ENDIF
      DO I = LONG , 1 , -1
        IF (LIGNE(I:I).NE.' '.AND.LIGNE(I:I).NE.TABUL) EXIT
      ENDDO ! I
110   CONTINUE
      LONGLU = I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
