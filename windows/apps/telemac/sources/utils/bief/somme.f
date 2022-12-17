!                   *******************************
                    DOUBLE PRECISION FUNCTION SOMME
!                   *******************************
!
     &( X , NPX )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SUMS THE COMPONENTS OF A VECTOR
!+               (ALSO SEE SOMME2 AND SUM).
!
!history  J-M HERVOUET (LNH)
!+        08/12/94
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
!| NPX            |-->| SIZE OF X
!| X              |-->| ARRAY TO SUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NPX,I
!
      DOUBLE PRECISION X(*)
!
!-----------------------------------------------------------------------
!
        SOMME = 0.D0
        DO I = 1 , NPX
          SOMME = SOMME + X(I)
        ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
