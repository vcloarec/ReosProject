!                   ******************
                    SUBROUTINE SD_NNSC
!                   ******************
!
     &(N,R,C,IL,JL,IJL,L,D,IU,JU,IJU,U,Z,B,TMP)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief NUMERICAL SOLUTION OF SPARSE NONSYMMETRIC SYSTEM OF LINEAR
!+      EQUATIONS GIVEN LDU-FACTORIZATION (COMPRESSED POINTER STORAGE)
!+
!+
!+       INPUT VARIABLES..  N, R, C, IL, JL, IJL, L, D, IU, JU, IJU, U, B
!+       OUTPUT VARIABLES.. Z
!+
!+       PARAMETERS USED INTERNALLY..
!+ FIA   \ TMP   - TEMPORARY VECTOR WHICH GETS RESULT OF SOLVING  LY = B.
!+       \           SIZE = N.
!+
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!
!         DON'T HESITATE TO CHANGE IN/OUTPUT VARIABLES COMMENTS
!         FOR CLARITY
!
!history  C. PEYRARD (LNHE)
!+        30/06/13
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |-->| RIGHT-HAND SIDE B ;
!| C              |-->| ORDERING OF THE COLUMNS OF MATRIX
!| D              |-->| DIAGONAL FACTORIZED OF MATRIX
!| IL, JL         |-->| STRUCTURE OF LOWER FACTORISED TRIANGULAR MATRIX
!| IU, JU         |-->| STRUCTURE OF UPPER FACTORISED TRIANGULAR MATRIX
!| IJU,IJL        |-->| USED TO COMPRESS STORAGE OF JU and JL
!| L              |-->| LOWER FACTORIZED TRIANGULAR MATRIX
!| N              |-->| RANK OF MATRIX
!| R              |-->| ORDERING OF THE ROWS OF MATRIX
!| TMP            |-->| REAL ONE-DIMENSIONAL WORK ARRAY
!| U              |-->| UPPER FACTORIZED TRIANGULAR MATRIX
!| Z              |<--| SOLUTION X
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER R(*), C(*), IL(*), JL(*), IJL(*), IU(*), JU(*), IJU(*),N
      DOUBLE PRECISION  L(*), D(*), U(*), B(*), Z(*), TMP(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,JMIN,JMAX,ML,J,MU,I
      DOUBLE PRECISION TMPK,SOMME
!
!-----------------------------------------------------------------------
!
!     SET TMP TO REORDERED B
!
      DO K=1,N
        TMP(K) = B(R(K))
      ENDDO
!
!     SOLVE  LY = B  BY FORWARD SUBSTITUTION
!
      DO K=1,N
        JMIN = IL(K)
        JMAX = IL(K+1) - 1
        TMPK = -D(K) * TMP(K)
        TMP(K) = -TMPK
        IF (JMIN .GT. JMAX) CYCLE
        ML = IJL(K) - JMIN
        DO J=JMIN,JMAX
          TMP(JL(ML+J)) = TMP(JL(ML+J)) + TMPK * L(J)
        ENDDO
      ENDDO ! K
!
!     SOLVE  UX = Y  BY BACK SUBSTITUTION
!
      K = N
      DO I=1,N
        SOMME = -TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN .GT. JMAX) GO TO 5
        MU = IJU(K) - JMIN
        DO J=JMIN,JMAX
          SOMME = SOMME + U(J) * TMP(JU(MU+J))
        ENDDO
   5    TMP(K) = -SOMME
        Z(C(K)) = -SOMME
        K = K - 1
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
