!                   ********************************
                    DOUBLE PRECISION FUNCTION SOMME2
!                   ********************************
!
     &( X , NPX )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SUMS THE COMPONENTS OF A VECTOR WHILE MINIMISING
!+                THE TRUNCATION ERRORS.+
!
!reference  GOLDBERG, DAVID  1991. WHAT EVERY COMPUTER SCIENTIST
!+ SHOULD KNOW   ABOUT FLOATING-POINT ARITHMETIC, ACM  COMPUTING
!+ SURVEYS, 23(1), PP5-48 (CORRIGENDUM, COMPUTING SURVEYS, 1991, 23(3))
!+(ARTICLE REPRODUCED IN THE NUMERICAL COMPUTATION GUIDE OF SUN
!+ MICROSYSTEM, HTTP://DOCS.SUN.COM )
!reference    KNUTH, D.E. 1981. THE ART OF PROGRAMMING.
!+ ADDISON-WESLEY, READING, MASS., VOL. II, 2ND ED.
!+ PROOF P 572
!
!history  A. DESITTER (NAG)
!+        08/12/98
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPX
!
      DOUBLE PRECISION, INTENT(IN) :: X(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION C,Y,T
!
!-----------------------------------------------------------------------
! KAHAN SUMMATION FORMULA
!
      SOMME2 = X(1)
      C = 0.D0
      DO I = 2 , NPX
        Y = X(I) - C
        T = SOMME2 + Y
        C = (T - SOMME2) - Y
        SOMME2 = T
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
