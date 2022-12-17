!                   ******************
                    SUBROUTINE SD_NNTC
!                   ******************
!
     &(N,R,C,IL,JL,IJL,L,D,IU,JU,IJU,U,Z,B,TMP)
!
!***********************************************************************
! BIEF   V6P3                                   30/06/2013
!***********************************************************************
!
!brief NUMERIC SOLUTION OF THE TRANSPOSE OF A SPARSE NONSYMMETRIC SYSTEM
!+      OF LINEAR EQUATIONS GIVEN LU-FACTORIZATION (COMPRESSED POINTER
!+      STORAGE)
!+
!code
!+       INPUT VARIABLES..  N, R, C, IL, JL, IJL, L, D, IU, JU, IJU, U, B
!+       OUTPUT VARIABLES.. Z
!+
!+       PARAMETERS USED INTERNALLY..
!+ FIA    \ TMP   - TEMPORARY VECTOR WHICH GETS RESULT OF SOLVING UT Y = B
!+        \           SIZE = N.
!
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
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
      DOUBLE PRECISION L(*), D(*), U(*), B(*), Z(*), TMP(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,JMIN,JMAX,MU,J,I,ML
      DOUBLE PRECISION TMPK,SOMME
!
!  ******  SET TMP TO REORDERED B  ************************************
      DO  K=1,N
        TMP(K) = B(C(K))
      ENDDO
!  ******  SOLVE  UT Y = B  BY FORWARD SUBSTITUTION  *******************
      DO K=1,N
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        TMPK = -TMP(K)
        IF (JMIN .GT. JMAX) CYCLE
        MU = IJU(K) - JMIN
        DO J=JMIN,JMAX
          TMP(JU(MU+J)) = TMP(JU(MU+J)) + TMPK * U(J)
        ENDDO
      ENDDO
!  ******  SOLVE  LT X = Y  BY BACK SUBSTITUTION  **********************
      K = N
      DO I=1,N
        SOMME = -TMP(K)
        JMIN = IL(K)
        JMAX = IL(K+1) - 1
        IF (JMIN .LE. JMAX) THEN
          ML = IJL(K) - JMIN
          DO J=JMIN,JMAX
            SOMME = SOMME + L(J) * TMP(JL(ML+J))
          ENDDO ! J
        ENDIF
        TMP(K) = -SOMME * D(K)
        Z(R(K)) = TMP(K)
        K = K - 1
      ENDDO ! I
      RETURN
      END
