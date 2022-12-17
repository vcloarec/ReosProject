!                   ****************
                    SUBROUTINE MINUS
!                   ****************
!
     &(CHAINE)
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CONVERTS A CHARACTER STRING FROM UPPER TO LOWER CASE.
!
!history  J-M HERVOUET (LNH)
!+        30/01/1992
!+
!+
!
!history  A. DESITTER (BRISTOL)
!+        06/10/1997
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
!| CHAINE         |<->| CHAINE DE CARACTERES A MODIFIER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      CHARACTER(LEN=26) :: STMAJ,STMIN
      CHARACTER(LEN=*) CHAINE
!
      INTEGER I,IPOS
!
      INTRINSIC LEN,INDEX
!
      PARAMETER ( STMAJ = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
      PARAMETER ( STMIN = 'abcdefghijklmnopqrstuvwxyz' )
!
!----------------------------------------------------------------------
!
      DO I=1,LEN(CHAINE)
!
      IPOS=INDEX(STMAJ,CHAINE(I:I))
      IF(IPOS.NE.0) CHAINE(I:I)=STMIN(IPOS:IPOS)
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
