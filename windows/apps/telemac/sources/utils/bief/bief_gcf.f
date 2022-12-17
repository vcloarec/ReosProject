!                       *******************
                        SUBROUTINE BIEF_GCF
!                       *******************
!
     &(GAMMCF,A,X,GLN)
!
!***********************************************************************
! BIEF   V6P3                                   07/03/2013
!***********************************************************************
!
!brief  Returns the incomplete gamma function Q(a,x) evaluated by its
!+      continued fraction representation as GAMMCF.  Also returns
!+      gamma (a) as GLN. From 'Numerical Recipes' Chapter 6.2
!
!history  D J Evans-Roberts (HRW)
!+        23/11/1993
!+        V6P3
!+   First version sent by Michiel Knaapen (HRW) on 07/03/2013.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| PARAMETER
!| GAMMCF         |<->| Incomplete gamma function Q(a,x)
!| GLN            |<->| gamma(a)
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
      DOUBLE PRECISION, INTENT(IN)    :: A,X
      DOUBLE PRECISION, INTENT(INOUT) :: GAMMCF,GLN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: N
      DOUBLE PRECISION GOLD,A0,A1,B0,B1,FAC,AN,ANA,ANF,G
!
      INTEGER, PARAMETER :: ITMAX = 100
      DOUBLE PRECISION   :: EPS = 3.E-7
!
      DOUBLE PRECISION BIEF_GAMMLN
      EXTERNAL         BIEF_GAMMLN
!
      INTRINSIC EXP,LOG,FLOAT,ABS
!
!-----------------------------------------------------------------------
!
      GLN = BIEF_GAMMLN(A)
!     This is the previous value, tested against for convergence.
      GOLD = 0.D0
      A0   = 1.D0
!     We are here setting up the A's and B's of equation (5.2.4) for
!     evaluating the continued fraction.
      A1     = X
      B0     = 0.D0
      B1     = 1.D0
!     FAC is the renormalization factor for preventing overflow of the
!     partial numerators and denominators.
      FAC    = 1.D0
      DO N = 1,ITMAX
        AN     = FLOAT(N)
        ANA    = AN - A
!       One step of the recurrence (5.2.5).
        A0     = (A1+A0*ANA)*FAC
        B0     = (B1+B0*ANA)*FAC
        ANF    = AN*FAC
!       The next step of the recurrence (5.2.5).
        A1     = X*A0 + ANF*A1
        B1     = X*B0 + ANF*B1
!       Shall we renormalize?
        IF(A1.NE.0.D0) THEN
!         Yes.  Set FAC so it happens.
          FAC    = 1.D0/A1
!         New value of answer.
          G      = B1*FAC
!         Converged? If so, exit.
          IF (ABS((G-GOLD)/G).LT.EPS) GO TO 110
!         If not, save value.
          GOLD   = G
        ENDIF
      ENDDO
      WRITE(LU,*) 'BIEF_GCF: FATAL ERROR'
      CALL PLANTE(1)
      STOP
110   CONTINUE
!
      GAMMCF = EXP(-X+A*LOG(X)-GLN)*G
!
!-----------------------------------------------------------------------
!
      RETURN
      END
