!                   **********************************
                    DOUBLE PRECISION FUNCTION BIEF_ERF
!                   **********************************
!
     &(X)
!
!***********************************************************************
! BIEF   V6P3                                   07/03/2013
!***********************************************************************
!
!brief    Function erf (until all compilers have it).
!+        From 'Numerical Recipes' Chapter 6, and Tideway model.
!
!history  D J Evans-Roberts (HRW)
!+        23/11/1993
!+        V6P3
!+   First version sent by Michiel Knaapen (HRW) on 07/03/2013.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      DOUBLE PRECISION, INTENT(IN) :: X
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION BIEF_GAMMP
      EXTERNAL         BIEF_GAMMP
!
!-----------------------------------------------------------------------
!
      IF(X.LT.0.D0) THEN
        BIEF_ERF = -BIEF_GAMMP(0.5D0,X**2)
      ELSE
        BIEF_ERF =  BIEF_GAMMP(0.5D0,X**2)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
