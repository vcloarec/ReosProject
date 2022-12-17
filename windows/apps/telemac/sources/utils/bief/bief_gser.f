!                       ********************
                        SUBROUTINE BIEF_GSER
!                       ********************
!
     &(GAMSER,A,X,GLN)
!
!***********************************************************************
! BIEF   V6P3                                   07/03/2013
!***********************************************************************
!
!brief  Returns the incomplete gamma function P(a,x) evaluated by its
!+      series representation as GAMSER.  Also returns ln  gamma(a) as
!+      GLN. From 'Numerical Recipes' Chapter 6.2 This routine is only
!+      valid for X.GE.0 Negative X cannot occur when calling from ERF
!+      or ERFC via GAMMP or GAMMQ
!
!history  D J Evans-Roberts (HRW)
!+        23/11/1993
!+        V6P3
!+   First version sent by Michiel Knaapen (HRW) on 07/03/2013.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| PARAMETER
!| GAMSER         |<->| Incomplete gamma function P(a,x)
!| GLN            |<->| gamma(a)
!| X              |-->| OPERAND
!| IFAIL          |<->| ERROR TAG: 1 IS OK, 0 IS ERROR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: X,A
      DOUBLE PRECISION, INTENT(INOUT) :: GAMSER,GLN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: N
      DOUBLE PRECISION :: AP,SOMME,DEL
!
      INTEGER, PARAMETER :: ITMAX = 100
      DOUBLE PRECISION   :: EPS = 3.D-7
!
      DOUBLE PRECISION BIEF_GAMMLN
      EXTERNAL         BIEF_GAMMLN
!
      INTRINSIC ABS,EXP,LOG
!
!-----------------------------------------------------------------------
!
      GLN = BIEF_GAMMLN(A)
!
      IF(X.LT.0.D0) THEN
!       Cannot occur when used by ERF or ERFC
        WRITE(LU,*) 'BIEF_GSER: UNEXPECTED NEGATIVE X'
        CALL PLANTE(1)
        STOP
      ELSEIF(X.EQ.0.D0) THEN
        GAMSER = 0.D0
      ELSE
        AP  = A
        SOMME = 1.D0/A
        DEL = SOMME
        DO N = 1,ITMAX
          AP  = AP + 1.D0
          DEL = DEL*X/AP
          SOMME = SOMME + DEL
          IF(ABS(DEL).LT.ABS(SOMME)*EPS) GO TO 110
        ENDDO
        WRITE(LU,*) 'BIEF_GSER: MAXIMUM OF ITERATIONS REACHED:',ITMAX
        CALL PLANTE(1)
        STOP
110     CONTINUE
        GAMSER = SOMME*EXP(-X+A*LOG(X)-GLN)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
