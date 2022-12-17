!                 ******************
                  SUBROUTINE SD_NSFC
!                 ******************
!
     &(N,R,IC,IA,JA,JLMAX,IL,JL,IJL,JUMAX,IU,JU,IJU,Q,IRA,JRA,IRAC,
     & IRL,JRL,IRU,JRU,FLAG)
!
!***********************************************************************
! BIEF   V6P3                                   30/06/2013
!***********************************************************************
!
!brief     SYMBOLIC LDU-FACTORIZATION OF NONSYMMETRIC SPARSE MATRIX
!+            (COMPRESSED POINTER STORAGE)
!code
!+       INPUT VARIABLES.. N, R, IC, IA, JA, JLMAX, JUMAX.
!+       OUTPUT VARIABLES.. IL, JL, IJL, IU, JU, IJU, FLAG.
!+
!+       PARAMETERS USED INTERNALLY..
!+ NIA   \ Q    - SUPPOSE  M*  IS THE RESULT OF REORDERING  M.  IF
!+       \          PROCESSING OF THE ITH ROW OF  M*  (HENCE THE ITH
!+       \          ROW OF  U) IS BEING DONE,  Q(J)  IS INITIALLY
!+       \          NONZERO IF  M*(I,J) IS NONZERO (J.GE.I).  SINCE
!+       \          VALUES NEED NOT BE STORED, EACH ENTRY POINTS TO THE
!+       \          NEXT NONZERO AND  Q(N+1)  POINTS TO THE FIRST.  N+1
!+       \          INDICATES THE END OF THE LIST.  FOR EXAMPLE, IF N=9
!+       \          AND THE 5TH ROW OF  M*  IS
!+       \             0 X X 0 X 0 0 X 0
!+       \          THEN  Q  WILL INITIALLY BE
!+       \             A A A A 8 A A 10 5    (A - ARBITRARY).
!+       \          AS THE ALGORITHM PROCEEDS, OTHER ELEMENTS OF  Q
!+       \          ARE INSERTED IN THE LIST BECAUSE OF FILLIN.
!+       \           Q  IS USED IN AN ANALOGOUS MANNER TO COMPUTE THE
!+       \           ITH COLUMN OF  L.
!+       \           SIZE = N+1.
!+ NIA   \ IRA,  - VECTORS USED TO FIND THE COLUMNS OF  M.  AT THE KTH
!+ NIA   \ JRA,      STEP OF THE FACTORIZATION,  IRAC(K)  POINTS TO THE
!+ NIA   \ IRAC      HEAD OF A LINKED LIST IN  JRA  OF ROW INDICES I
!+       \           SUCH THAT I .GE. K AND  M(I,K)  IS NONZERO.  ZERO
!+       \           INDICATES THE END OF THE LIST.  IRA(I)  (I.GE.K)
!+       \           POINTS TO THE SMALLEST J SUCH THAT J .GE. K AND
!+       \           M(I,J)  IS NONZERO.
!+       \           SIZE OF EACH = N.
!+ NIA   \ IRL,  - VECTORS USED TO FIND THE ROWS OF  L.  AT THE KTH STEP
!+ NIA   \ JRL      OF THE FACTORIZATION,  JRL(K)  POINTS TO THE HEAD
!+       \           OF A LINKED LIST IN  JRL  OF COLUMN INDICES J
!+       \           SUCH J .LT. K AND  L(K,J)  IS NONZERO.  ZERO
!+       \           INDICATES THE END OF THE LIST.  IRL(J)  (J.LT.K)
!+       \           POINTS TO THE SMALLEST I SUCH THAT I .GE. K AND
!+       \           L(I,J)  IS NONZERO.
!+       \           SIZE OF EACH = N.
!+ NIA   \ IRU,  - VECTORS USED IN A MANNER ANALOGOUS TO  IRL AND JRL
!+ NIA   \ JRU      TO FIND THE COLUMNS OF  U.
!+       \           SIZE OF EACH = N.
!+
!+  INTERNAL VARIABLES..
!+    JLPTR - POINTS TO THE LAST POSITION USED IN  JL.
!+    JUPTR - POINTS TO THE LAST POSITION USED IN  JU.
!+    JMIN,JMAX - ARE THE INDICES IN  A OR U  OF THE FIRST AND LAST
!+                ELEMENTS TO BE EXAMINED IN A GIVEN ROW.
!+                FOR EXAMPLE,  JMIN=IA(K), JMAX=IA(K+1)-1.
!+
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!         DON'T HESITATE TO CHANGE IN/OUTPUT VARIABLES COMMENTS
!         FOR CLARITY
!
!history  C. PEYRARD (LNHE)
!+        30/01/13
!+        V6P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLAG           |<--| INDICATOR ERROR :
!|                |   |= N + R(K):  NULL ROW IN A
!|                |   |= 2*N + R(K):DUPLICATE ENTRY IN A
!|                |   |= 3*N + K:INSUFFICIENT STORAGE FOR JL
!|                |   |= 6*N + K:INSUFFICIENT STORAGE FOR JU
!|                |   |= 5*N + K: ZERO PIVOT
!| IA, JA         |-->| STRUCTURE OF A NONSYMMETRICAL MATRIX
!| IC             |-->| INVERSE OF THE ORDERING OF THE COLUMNS OF MATRIX
!| IL, JL         |<--| STRUCTURE OF LOWER FACTORISED TRIANGULAR MATRIX
!| IU, JU         |<--| STRUCTURE OF UPPER FACTORISED TRIANGULAR MATRIX
!| IJU,IJL        |-->| USED TO COMPRESS STORAGE OF JU and JL
!| IM             |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;SIZE = N
!| JLMAX          |-->| PREVISIONAL MAXIMUM DIMENSION OF JL
!| JUMAX          |-->| PREVISIONAL MAXIMUM DIMENSION OF JU
!| N              |-->| RANK OF MATRIX
!| Q              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;SIZE = N+1
!| R              |-->| ORDERING OF THE ROWS OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IA(*), JA(*), IRA(*), JRA(*), IL(*), JL(*), IJL(*)
      INTEGER IU(*), JU(*), IJU(*), IRL(*), JRL(*), IRU(*), JRU(*)
      INTEGER R(*), IC(*), Q(*), IRAC(*),FLAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER CEND, QM, REND, RK, VJ
      INTEGER NP1,N,JLMIN,JLPTR,JUMIN,JUPTR,K,IAK,JAIAK,LUK,M
      INTEGER LASTID,LASTI,I,JMIN,JMAX,LONG,JTMP,J,I1,IRLL,JLMAX
      INTEGER JUMAX,IRAI,JAIRAI,IRUL
!
!  ******  INITIALIZE POINTERS  ****************************************
      NP1 = N + 1
      JLMIN = 1
      JLPTR = 0
      IL(1) = 1
      JUMIN = 1
      JUPTR = 0
      IU(1) = 1
      DO 1 K=1,N
        IRAC(K) = 0
        JRA(K) = 0
        JRL(K) = 0
   1    JRU(K) = 0
!  ******  INITIALIZE COLUMN POINTERS FOR A  ***************************
      DO 2 K=1,N
        RK = R(K)
        IAK = IA(RK)
        IF (IAK .GE. IA(RK+1))  GO TO 101
        JAIAK = IC(JA(IAK))
        IF (JAIAK .GT. K)  GO TO 105
        JRA(K) = IRAC(JAIAK)
        IRAC(JAIAK) = K
   2    IRA(K) = IAK
!
!  ******  FOR EACH COLUMN OF L AND ROW OF U  **************************
      DO 41 K=1,N
!
!  ******  INITIALIZE Q FOR COMPUTING KTH COLUMN OF L  *****************
        Q(NP1) = NP1
        LUK = -1
!  ******  BY FILLING IN KTH COLUMN OF A  ******************************
        VJ = IRAC(K)
        IF (VJ .EQ. 0)  GO TO 5
   3      QM = NP1
   4      M = QM
          QM =  Q(M)
          IF (QM .LT. VJ)  GO TO 4
          IF (QM .EQ. VJ)  GO TO 102
            LUK = LUK + 1
            Q(M) = VJ
            Q(VJ) = QM
            VJ = JRA(VJ)
            IF (VJ .NE. 0)  GO TO 3
!  ******  LINK THROUGH JRU  *******************************************
   5    LASTID = 0
        LASTI = 0
        IJL(K) = JLPTR
        I = K
   6      I = JRU(I)
          IF (I .EQ. 0)  GO TO 10
          QM = NP1
          JMIN = IRL(I)
          JMAX = IJL(I) + IL(I+1) - IL(I) - 1
          LONG = JMAX - JMIN
          IF (LONG .LT. 0)  GO TO 6
          JTMP = JL(JMIN)
          IF (JTMP .NE. K)  LONG = LONG + 1
          IF (JTMP .EQ. K)  R(I) = -R(I)
          IF (LASTID .GE. LONG)  GO TO 7
            LASTI = I
            LASTID = LONG
!  ******  AND MERGE THE CORRESPONDING COLUMNS INTO THE KTH COLUMN  ****
   7      DO 9 J=JMIN,JMAX
            VJ = JL(J)
   8        M = QM
            QM = Q(M)
            IF (QM .LT. VJ)  GO TO 8
            IF (QM .EQ. VJ)  GO TO 9
              LUK = LUK + 1
              Q(M) = VJ
              Q(VJ) = QM
              QM = VJ
   9        CONTINUE
            GO TO 6
!  ******  LASTI IS THE LONGEST COLUMN MERGED INTO THE KTH  ************
!  ******  SEE IF IT EQUALS THE ENTIRE KTH COLUMN  *********************
  10    QM = Q(NP1)
        IF (QM .NE. K)  GO TO 105
        IF (LUK .EQ. 0)  GO TO 17
        IF (LASTID .NE. LUK)  GO TO 11
!  ******  IF SO, JL CAN BE COMPRESSED  ********************************
        IRLL = IRL(LASTI)
        IJL(K) = IRLL + 1
        IF (JL(IRLL) .NE. K)  IJL(K) = IJL(K) - 1
        GO TO 17
!  ******  IF NOT, SEE IF KTH COLUMN CAN OVERLAP THE PREVIOUS ONE  *****
  11    IF (JLMIN .GT. JLPTR)  GO TO 15
        QM = Q(QM)
        DO 12 J=JLMIN,JLPTR
          IF (JL(J) < QM ) GOTO 12
          IF (JL(J) == QM ) GOTO 13
          IF (JL(J) > QM ) GOTO 15
  12      CONTINUE
        GO TO 15
  13    IJL(K) = J
        DO 14 I=J,JLPTR
          IF (JL(I) .NE. QM)  GO TO 15
          QM = Q(QM)
          IF (QM .GT. N)  GO TO 17
  14      CONTINUE
        JLPTR = J - 1
!  ******  MOVE COLUMN INDICES FROM Q TO JL, UPDATE VECTORS  ***********
  15    JLMIN = JLPTR + 1
        IJL(K) = JLMIN
        IF (LUK .EQ. 0)  GO TO 17
        JLPTR = JLPTR + LUK
        IF (JLPTR .GT. JLMAX)  GO TO 103
          QM = Q(NP1)
          DO 16 J=JLMIN,JLPTR
            QM = Q(QM)
  16        JL(J) = QM
  17    IRL(K) = IJL(K)
        IL(K+1) = IL(K) + LUK
!
!  ******  INITIALIZE Q FOR COMPUTING KTH ROW OF U  ********************
        Q(NP1) = NP1
        LUK = -1
!  ******  BY FILLING IN KTH ROW OF REORDERED A  ***********************
        RK = R(K)
        JMIN = IRA(K)
        JMAX = IA(RK+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 20
        DO 19 J=JMIN,JMAX
          VJ = IC(JA(J))
          QM = NP1
  18      M = QM
          QM = Q(M)
          IF (QM .LT. VJ)  GO TO 18
          IF (QM .EQ. VJ)  GO TO 102
            LUK = LUK + 1
            Q(M) = VJ
            Q(VJ) = QM
  19      CONTINUE
!  ******  LINK THROUGH JRL,  ******************************************
  20    LASTID = 0
        LASTI = 0
        IJU(K) = JUPTR
        I = K
        I1 = JRL(K)
  21      I = I1
          IF (I .EQ. 0)  GO TO 26
          I1 = JRL(I)
          QM = NP1
          JMIN = IRU(I)
          JMAX = IJU(I) + IU(I+1) - IU(I) - 1
          LONG = JMAX - JMIN
          IF (LONG .LT. 0)  GO TO 21
          JTMP = JU(JMIN)
          IF (JTMP .EQ. K)  GO TO 22
!  ******  UPDATE IRL AND JRL, *****************************************
            LONG = LONG + 1
            CEND = IJL(I) + IL(I+1) - IL(I)
            IRL(I) = IRL(I) + 1
            IF (IRL(I) .GE. CEND)  GO TO 22
              J = JL(IRL(I))
              JRL(I) = JRL(J)
              JRL(J) = I
  22      IF (LASTID .GE. LONG)  GO TO 23
            LASTI = I
            LASTID = LONG
!  ******  AND MERGE THE CORRESPONDING ROWS INTO THE KTH ROW  **********
  23      DO 25 J=JMIN,JMAX
            VJ = JU(J)
  24        M = QM
            QM = Q(M)
            IF (QM .LT. VJ)  GO TO 24
            IF (QM .EQ. VJ)  GO TO 25
              LUK = LUK + 1
              Q(M) = VJ
              Q(VJ) = QM
              QM = VJ
  25        CONTINUE
          GO TO 21
!  ******  UPDATE JRL(K) AND IRL(K)  ***********************************
  26    IF (IL(K+1) .LE. IL(K))  GO TO 27
          J = JL(IRL(K))
          JRL(K) = JRL(J)
          JRL(J) = K
!  ******  LASTI IS THE LONGEST ROW MERGED INTO THE KTH  ***************
!  ******  SEE IF IT EQUALS THE ENTIRE KTH ROW  ************************
  27    QM = Q(NP1)
        IF (QM .NE. K)  GO TO 105
        IF (LUK .EQ. 0)  GO TO 34
        IF (LASTID .NE. LUK)  GO TO 28
!  ******  IF SO, JU CAN BE COMPRESSED  ********************************
        IRUL = IRU(LASTI)
        IJU(K) = IRUL + 1
        IF (JU(IRUL) .NE. K)  IJU(K) = IJU(K) - 1
        GO TO 34
!  ******  IF NOT, SEE IF KTH ROW CAN OVERLAP THE PREVIOUS ONE  ********
  28    IF (JUMIN .GT. JUPTR)  GO TO 32
        QM = Q(QM)
        DO 29 J=JUMIN,JUPTR
          IF (JU(J) < QM) GOTO 29
          IF (JU(J) == QM) GOTO 30
          IF (JU(J) > QM) GOTO 32
  29      CONTINUE
        GO TO 32
  30    IJU(K) = J
        DO 31 I=J,JUPTR
          IF (JU(I) .NE. QM)  GO TO 32
          QM = Q(QM)
          IF (QM .GT. N)  GO TO 34
  31      CONTINUE
        JUPTR = J - 1
!  ******  MOVE ROW INDICES FROM Q TO JU, UPDATE VECTORS  **************
  32    JUMIN = JUPTR + 1
        IJU(K) = JUMIN
        IF (LUK .EQ. 0)  GO TO 34
        JUPTR = JUPTR + LUK
        IF (JUPTR .GT. JUMAX)  GO TO 106
          QM = Q(NP1)
          DO 33 J=JUMIN,JUPTR
            QM = Q(QM)
  33        JU(J) = QM
  34    IRU(K) = IJU(K)
        IU(K+1) = IU(K) + LUK
!
!  ******  UPDATE IRU, JRU  ********************************************
        I = K
  35      I1 = JRU(I)
          IF (R(I) .LT. 0)  GO TO 36
          REND = IJU(I) + IU(I+1) - IU(I)
          IF (IRU(I) .GE. REND)  GO TO 37
            J = JU(IRU(I))
            JRU(I) = JRU(J)
            JRU(J) = I
            GO TO 37
  36      R(I) = -R(I)
  37      I = I1
          IF (I .EQ. 0)  GO TO 38
          IRU(I) = IRU(I) + 1
          GO TO 35
!
!  ******  UPDATE IRA, JRA, IRAC  **************************************
  38    I = IRAC(K)
        IF (I .EQ. 0)  GO TO 41
  39      I1 = JRA(I)
          IRA(I) = IRA(I) + 1
          IF (IRA(I) .GE. IA(R(I)+1))  GO TO 40
          IRAI = IRA(I)
          JAIRAI = IC(JA(IRAI))
          IF (JAIRAI .GT. I)  GO TO 40
          JRA(I) = IRAC(JAIRAI)
          IRAC(JAIRAI) = I
  40      I = I1
          IF (I .NE. 0)  GO TO 39
  41    CONTINUE
!
      IJL(N) = JLPTR
      IJU(N) = JUPTR
      FLAG = 0
      RETURN
!
! ** ERROR.. NULL ROW IN A
 101  FLAG = N + RK
      RETURN
! ** ERROR.. DUPLICATE ENTRY IN A
 102  FLAG = 2*N + RK
      RETURN
! ** ERROR.. INSUFFICIENT STORAGE FOR JL
 103  FLAG = 3*N + K
      RETURN
! ** ERROR.. NULL PIVOT
 105  FLAG = 5*N + K
      RETURN
! ** ERROR.. INSUFFICIENT STORAGE FOR JU
 106  FLAG = 6*N + K
      RETURN
      END
