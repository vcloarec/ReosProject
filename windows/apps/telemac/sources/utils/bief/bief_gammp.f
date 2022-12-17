!                 ************************************
                  DOUBLE PRECISION FUNCTION BIEF_GAMMP
!                 ************************************
!
     &(A,X)
!
!***********************************************************************
! BIEF   V6P3                                   07/03/2013
!***********************************************************************
!
!brief    Incomplete Gamma function (until all compilers have it),
!+        P(a,x) from 'Numerical Recipes' Chapter 6.2, and Tideway model
!+        This function is only valid for X.GE.0 or A.GT.0. Negative
!+        X or A.LE.0 cannot occur when called from ERF or ERFC.
!
!history  D J Evans-Roberts (HRW)
!+        23/11/1993
!+        V6P3
!+   First version sent by Michiel Knaapen (HRW) on 07/03/2013.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| PARAMETER
!| X              |-->| OPERAND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: X,A
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION GAMMCF,GLN
!
!-----------------------------------------------------------------------
!
      IF(X.LT.A+1.D0) THEN
!       Use the series representation
        CALL BIEF_GSER(BIEF_GAMMP,A,X,GLN)
      ELSE
!       Use the continued fraction representation and take its complement
        CALL BIEF_GCF(GAMMCF,A,X,GLN)
        BIEF_GAMMP = 1.D0 - GAMMCF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
