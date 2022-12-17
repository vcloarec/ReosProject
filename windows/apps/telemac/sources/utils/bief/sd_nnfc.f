!                   ******************
                    SUBROUTINE SD_NNFC
!                   ******************
!
     &(N,R,C,IC,IA,JA,A,Z,B,LMAX,IL,JL,IJL,L,D,UMAX,IU,JU,IJU,U,
     & ROW,TMP,IRL,JRL,FLAG)
!
!***********************************************************************
! BIEF   V6P3                                   30/06/2013
!***********************************************************************
!
!brief NUMERICAL LDU-FACTORIZATION OF SPARSE NONSYMMETRIC MATRIX AND
!+      SOLUTION OF SYSTEM OF LINEAR EQUATIONS (COMPRESSED POINTER
!+      STORAGE)
!+
!+
!+       INPUT VARIABLES..  N, R, C, IC, IA, JA, A, B,
!+                          IL, JL, IJL, LMAX, IU, JU, IJU, UMAX
!+       OUTPUT VARIABLES.. Z, L, D, U, FLAG
!+
!+ FIA   \ ROW   - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.
!+       \           SIZE = N.
!+ FIA   \ TMP   - HOLDS NEW RIGHT-HAND SIDE  B*  FOR SOLUTION OF THE
!+       \           EQUATION UX = B*.
!+       \           SIZE = N.
!+
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
!| A              |-->| NONZERO ENTRIES OF THE COEFFICIENT MATRIX M,
!|                |   | STORED BY ROWS
!| B              |-->| RIGHT-HAND SIDE B ;
!| C              |-->| ORDERING OF THE COLUMNS OF MATRIX
!| D              |<--| DIAGONAL FACTORIZED OF MATRIX
!| FLAG           |<--| INDICATOR ERROR :
!|                |   |= 4*N + 1:INSUFFICIENT STORAGE FOR L
!|                |   |= 7*N + 1:INSUFFICIENT STORAGE FOR U
!| IA, JA         |-->| STRUCTURE OF A NONSYMMETRICAL MATRIX
!| IA, JA         |-->| STRUCTURE OF A NONSYMMETRICAL MATRIX
!| IC             |-->| INVERSE OF THE ORDERING OF THE COLUMNS OF MATRIX
!| IL, JL         |-->| STRUCTURE OF LOWER FACTORISED TRIANGULAR MATRIX
!| IU, JU         |-->| STRUCTURE OF UPPER FACTORISED TRIANGULAR MATRIX
!| IJU,IJL        |-->| USED TO COMPRESS STORAGE OF JU and JL
!| IRL,JRL        |-->| VECTORS USED TO FINDTHE ROWS OF L, AT THE kth STEP
!| L              |<--| LOWER FACTORIZED TRIANGULAR MATRIX
!| LMAX           |-->| PREVISIONAL MAXIMUM DIMENSION OF JL
!| N              |-->| RANK OF MATRIX
!| R              |-->| ORDERING OF THE ROWS OF MATRIX
!| ROW            |---| REAL ONE-DIMENSIONAL WORK ARRAY
!| TMP            |---| REAL ONE-DIMENSIONAL WORK ARRAY
!| U              |<--| UPPER FACTORIZED TRIANGULAR MATRIX
!| UMAX           |-->| PREVISIONAL MAXIMUM DIMENSION OF JU
!| Z              |<--| SOLUTION X
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER RK,UMAX
      INTEGER  R(*), C(*), IC(*), IA(*), JA(*), IL(*), JL(*), IJL(*)
      INTEGER  IU(*), JU(*), IJU(*), IRL(*), JRL(*), FLAG
      INTEGER  N,K,I1,I,I2,JMIN,JMAX,J,MU,IJLB,LMAX
      DOUBLE PRECISION  A(*), L(*), D(*), U(*), Z(*), B(*), ROW(*)
      DOUBLE PRECISION  TMP(*), LKI, SOMME, DK
!
!  ******  INITIALIZE POINTERS AND TEST STORAGE  ***********************
!
      IF(IL(N+1)-1 .GT. LMAX) GO TO 104
      IF(IU(N+1)-1 .GT. UMAX) GO TO 107
      DO 1 K=1,N
        IRL(K) = IL(K)
        JRL(K) = 0
   1    CONTINUE
!
!  ******  FOR EACH ROW  ***********************************************
      DO 19 K=1,N
!  ******  REVERSE JRL AND ZERO ROW WHERE KTH ROW OF L WILL FILL IN  ***
        ROW(K) = 0
        I1 = 0
        IF (JRL(K) .EQ. 0) GO TO 3
        I = JRL(K)
   2    I2 = JRL(I)
        JRL(I) = I1
        I1 = I
        ROW(I) = 0
        I = I2
        IF (I .NE. 0) GO TO 2
!  ******  SET ROW TO ZERO WHERE U WILL FILL IN  ***********************
   3    JMIN = IJU(K)
        JMAX = JMIN + IU(K+1) - IU(K) - 1
        IF (JMIN .GT. JMAX) GO TO 5
        DO 4 J=JMIN,JMAX
   4      ROW(JU(J)) = 0
!  ******  PLACE KTH ROW OF A IN ROW  **********************************
   5    RK = R(K)
        JMIN = IA(RK)
        JMAX = IA(RK+1) - 1
        DO 6 J=JMIN,JMAX
          ROW(IC(JA(J))) = A(J)
   6      CONTINUE
!  ******  INITIALIZE SOMME, AND LINK THROUGH JRL  *********************
        SOMME = B(RK)
        I = I1
        IF (I .EQ. 0) GO TO 10
!  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW, SOMME  **************
   7      LKI = -ROW(I)
!  ******  IF L IS NOT REQUIRED, THEN COMMENT OUT THE FOLLOWING LINE  **
          L(IRL(I)) = -LKI
          SOMME = SOMME + LKI * TMP(I)
          JMIN = IU(I)
          JMAX = IU(I+1) - 1
          IF (JMIN .GT. JMAX) GO TO 9
          MU = IJU(I) - JMIN
          DO 8 J=JMIN,JMAX
   8        ROW(JU(MU+J)) = ROW(JU(MU+J)) + LKI * U(J)
   9      I = JRL(I)
          IF (I .NE. 0) GO TO 7
!
!  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  *************
  10    IF (ROW(K) .EQ. 0.0D0) GO TO 108
        DK = 1.0D0 / ROW(K)
        D(K) = DK
        TMP(K) = SOMME * DK
        IF (K .EQ. N) GO TO 19
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 12
        MU = IJU(K) - JMIN
        DO 11 J=JMIN,JMAX
  11      U(J) = ROW(JU(MU+J)) * DK
  12    CONTINUE
!
!  ******  UPDATE IRL AND JRL, KEEPING JRL IN DECREASING ORDER  ********
        I = I1
        IF (I .EQ. 0) GO TO 18
  14    IRL(I) = IRL(I) + 1
        I1 = JRL(I)
        IF (IRL(I) .GE. IL(I+1)) GO TO 17
        IJLB = IRL(I) - IL(I) + IJL(I)
        J = JL(IJLB)
  15    IF (I .GT. JRL(J)) GO TO 16
          J = JRL(J)
          GO TO 15
  16    JRL(I) = JRL(J)
        JRL(J) = I
  17    I = I1
        IF (I .NE. 0) GO TO 14
  18    IF (IRL(K) .GE. IL(K+1)) GO TO 19
        J = JL(IJL(K))
        JRL(K) = JRL(J)
        JRL(J) = K
  19    CONTINUE
!
!  ******  SOLVE  UX = TMP  BY BACK SUBSTITUTION  **********************
      K = N
      DO 22 I=1,N
        SOMME =  TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 21
        MU = IJU(K) - JMIN
        DO 20 J=JMIN,JMAX
  20      SOMME = SOMME - U(J) * TMP(JU(MU+J))
  21    TMP(K) =  SOMME
        Z(C(K)) =  SOMME
  22    K = K-1
      FLAG = 0
      RETURN
!
! ** ERROR.. INSUFFICIENT STORAGE FOR L
 104  FLAG = 4*N + 1
      RETURN
! ** ERROR.. INSUFFICIENT STORAGE FOR U
 107  FLAG = 7*N + 1
      RETURN
! ** ERROR.. ZERO PIVOT
 108  FLAG = 8*N + K
      RETURN
      END
