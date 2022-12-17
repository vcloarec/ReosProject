!                   ***********************
                    SUBROUTINE CUBEEQUATION
!                   ***********************
!
     & (ACOF, BCOF, CCOF, DCOF, REALS, X)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief
!
!code
!+   RESOLUTION OF THE EQUATION OF AX**3+BX**2+CX+D=0 (E1)
!+
!+   1/ CHANGE THE VARIABLE X BY X = X -B/(3A)
!+                              P = (C/A) - B*B/(3A*A)
!+                              Q = 2B**3/(27A**3)+D/A-BC/(3A*A)
!+
!+      =>  (E1)  X**3+PX+Q = 0 (E2)
!+
!+   2/ COMPUTE DELTA = (Q*Q)/4 + (P**3)/27
!+
!+   3/ DELTA  > 0 : SOLUTION OF CARDAN      (1 REAL ROOT)
!+
!+   4/ DELTA
!
!history  F. HUVELIN
!+        20/04/2004
!+        V5P5
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
!history  U.H.Merkel
!+        20/07/2012
!+        V6P2
!+    sign   to signum due to errors with NAG
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACOF           |-->| CONSTANT FOR X**3
!| BCOF           |-->| CONSTANT FOR X**2
!| CCOF           |-->| CONSTANT FOR X
!| DCOF           |-->| CONSTANT OF THE EQUATION
!| REALS          |<--| NUMBER OF REAL SOLUTIONS
!| X              |<--| THE SOLUTIONS (1 OR 3)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: ACOF, BCOF, CCOF, DCOF
      INTEGER,          INTENT(OUT) :: REALS
      DOUBLE PRECISION, INTENT(OUT) :: X(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: PI
      DOUBLE PRECISION :: BA, CA, P, Q, Q2P3, U, V
      DOUBLE PRECISION :: EXPO, SIGNUM, TMP, PHI
!
      PI = 4.D0 * ATAN( 1.D0 )
!
!=======================================================================
!=======================================================================
!
      BA = BCOF / ACOF / 3.D0
      CA = CCOF / ACOF
!
      P  = CA/3.D0 - BA**2
      Q  = BA**3 - BA*CA/2.D0 + DCOF/ACOF/2.D0
!
      Q2P3 = Q**2 + P**3
!
      IF (Q2P3 > 0.D0) THEN
!
        REALS = 1
        EXPO  = 1.D0/3.D0
        TMP   = -Q + SQRT(Q2P3)
        SIGNUM  = TMP / ABS(TMP)
        U     = SIGNUM * ABS(TMP)**(EXPO)
        TMP   = -Q - SQRT(Q2P3)
        SIGNUM  = TMP / ABS(TMP)
        V     = SIGNUM * ABS(TMP)**EXPO
        X(1)  = (U + V) - BA
!
      ELSE
!
        REALS = 3
        TMP = -Q / (-P)**(1.5D0)
!
        IF (TMP >= 1.D0) THEN
          PHI = 0.D0
        ELSE IF (TMP <= -1.D0) THEN
          PHI = PI
        ELSE
          PHI = ACOS (TMP)
        ENDIF
!
        X(1) = 2.D0* SQRT(-P)* COS(PHI/3.D0)           -  BA
        X(2) = 2.D0* SQRT(-P)* COS((PHI+2.D0*PI)/3.D0) -  BA
        X(3) = 2.D0* SQRT(-P)* COS((PHI+4.D0*PI)/3.D0) -  BA
!
      ENDIF
!
!=======================================================================
!=======================================================================
!
      RETURN
      END
