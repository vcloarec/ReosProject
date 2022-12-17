!                   *****************
                    SUBROUTINE SD_SSF
!                   *****************
!
     &(N,P,IP,IA,JA,IJU,JU,IU,JUMAX,Q,MARK,JL,FLAG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SYMBOLIC UT-D-U FACTORISATION OF SPARSE SYMMETRIC MATRIX.
!+                COMPRESSED STORAGE OF SPARSE MATRICES.
!code
!+  IMPORTANT NOTE: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!+
!+
!+    THE STRICT UPPER TRIANGULAR PORTION OF THE MATRIX U IS STORED IN
!+    (IA,JA,A) FORMAT USING THE ARRAYS IU, JU, AND U, EXCEPT THAT AN
!+    ADDITIONAL ARRAY IJU IS USED TO REDUCE THE STORAGE REQUIRED FOR JU
!+    BY ALLOWING SOME SEQUENCES OF COLUMN INDICES TO CORRESPOND TO MORE
!+    THAN ONE ROW.  FOR I
!+    ENTRY FOR THE I-TH ROW;  IJU(N) IS THE NUMBER OF ENTRIES IN JU.
!+    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IU(I+1) - IU(I),
!+    THE NONZERO ENTRIES OF THE I-TH ROW ARE STORED CONSECUTIVELY IN
!+
!+        U(IU(I)),   U(IU(I)+1),   ..., U(IU(I+1)-1),
!+
!+    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN
!+
!+        JU(IJU(I)), JU(IJU(I)+1), ..., JU(IJU(I)+IU(I+1)-IU(I)-1).
!+
!+    COMPRESSION IN JU OCCURS IN TWO WAYS.  FIRST, IF A ROW I WAS MERGED
!+    INTO ROW K, AND THE NUMBER OF ELEMENTS MERGED IN FROM (THE TAIL
!+    PORTION OF) ROW I IS THE SAME AS THE FINAL LENGTH OF ROW K, THEN
!+    THE KTH ROW AND THE TAIL OF ROW I ARE IDENTICAL AND IJU(K) POINTS
!+    TO THE START OF THE TAIL.  SECOND, IF SOME TAIL PORTION OF THE
!+    (K-1)ST ROW IS IDENTICAL TO THE HEAD OF THE KTH ROW, THEN IJU(K)
!+    POINTS TO THE START OF THAT TAIL PORTION.  FOR EXAMPLE, THE NONZERO
!+    STRUCTURE OF THE STRICT UPPER TRIANGULAR PART OF THE MATRIX
!+
!+             ( D 0 0 0 X X X )
!+             ( 0 D 0 X X 0 0 )
!+             ( 0 0 D 0 X X 0 )
!+         U = ( 0 0 0 D X X 0 )
!+             ( 0 0 0 0 D X X )
!+             ( 0 0 0 0 0 D X )
!+             ( 0 0 0 0 0 0 D )
!+
!+    WOULD BE STORED AS
!+
!+             \ 1  2  3  4  5  6  7  8
!+         ----+------------------------
!+          IU \ 1  4  6  8 10 12 13 13
!+          JU \ 5  6  7  4  5  6
!+         IJU \ 1  4  5  5  2  3  6           .
!+
!+    THE DIAGONAL ENTRIES OF U ARE EQUAL TO ONE AND ARE NOT STORED.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/06
!+        V5P7
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
!| FLAG           |<--| LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN
!|                |   | STORE NONZERO DIAGONAL ELEMENTS AT THE
!|                |   | BEGINNING OF THE ROW
!| IA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!|                |   | POINTERS TO DELIMIT ROWS IN JA AND A;
!|                |   | DIMENSION = N+1
!| IJU            |---| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!|                |   | POINTERS TO THE START OF EACH ROW IN JU;  DIMENSION = N
!| IP             |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!|                |   | THE INVERSE OF THE PERMUTATION RETURNED IN P;
!|                |   | DIMENSION = N
!| IU             |---| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!|                |   | POINTERS TO DELIMIT ROWS IN U;  DIMENSION = N+1
!| JA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!|                |   | OF A;  DIMENSION = NUMBER OF NONZERO ENTRIES
!|                |   | IN (THE UPPER TRIANGLE OF) M
!| JL             |-->| INTEGER ONE-DIMENSIONAL WORK ARRAY; DIMENSION N
!|                |   | DIMENSION = NUMBER OF NONZERO ENTRIES IN THE
!|                |   | UPPER TRIANGLE OF M. JL CONTAINS LISTS OF ROWS
!|                |   | TO BE MERGED INTO UNELIMINATED ROWS --
!|                |   | I GE K => JL(I) IS THE FIRST ROW TO BE
!|                |   | MERGED INTO ROW I
!|                |   | I LT K => JL(I) IS THE ROW FOLLOWING ROW I IN
!|                |   | SOME LIST OF ROWS
!|                |   | IN EITHER CASE, JL(I) = 0 INDICATES THE
!|                |   | END OF A LIST
!| JU             |---| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!|                |   | OF U;  DIMENSION = JUMAX
!| JUMAX          |---| DECLARED DIMENSION OF THE ONE-DIMENSIONAL
!|                |   | ARRAY JU;  JUMAX MUST BE AT LEAST THE SIZE
!|                |   | OF U MINUS COMPRESSION (IJU(N) AFTER THE
!|                |   | CALL TO SSF)
!| MARK           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY; DIMENSION N
!|                |   | THE LAST ROW STORED IN JU FOR WHICH U(MARK(I),I)
!|                |   | NE 0
!| N              |-->| ORDER OF THE MATRIX
!| P              |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!|                |   | THE PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| Q              |-->| INTEGER ONE-DIMENSIONAL WORK ARRAY, DIMENSION N
!|                |   | Q CONTAINS AN ORDERED LINKED LIST
!|                |   | REPRESENTATION OF THE NONZERO
!|                |   | STRUCTURE OF THE K-TH ROW OF U --
!|                |   | Q(K) IS THE FIRST COLUMN WITH A NONZERO ENTRY
!|                |   | Q(I) IS THE NEXT COLUMN WITH A NONZERO ENTRY
!|                |   | AFTER COLUMN I
!|                |   | IN EITHER CASE, Q(I) = N+1 INDICATES THE
!|                |   | END OF THE LIST
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_SSF => SD_SSF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: N,JUMAX
      INTEGER, INTENT(INOUT) :: P(N),IP(N),IA(N+1),JA(*),IJU(N),FLAG
      INTEGER, INTENT(INOUT) :: JU(JUMAX),IU(N+1),Q(N),MARK(N),JL(N)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,M,TAG,VJ,QM,JUMIN,JUPTR,K,LUK,LUI,JMIN,JMAX,LMAX
      LOGICAL CLIQUE
!
!-----------------------------------------------------------------------
!
!----INITIALISES
!
!    JUMIN AND JUPTR ARE THE INDICES IN JU OF THE FIRST AND LAST
!    ELEMENTS IN THE LAST ROW SAVED IN JU
!
!    LUK IS THE NUMBER OF NONZERO ENTRIES IN THE K-TH ROW
!
      JUMIN = 1
      JUPTR = 0
      IU(1) = 1
      DO K=1,N
        MARK(K) = 0
        JL(K) = 0
      ENDDO
!
!----FOR EACH ROW K
!
      DO 19 K=1,N
        LUK = 0
        Q(K) = N+1
!
        TAG = MARK(K)
        CLIQUE = .FALSE.
        IF(JL(K).NE.0)  CLIQUE = JL(JL(K)).EQ.0
!
!------INITIALISES NONZERO STRUCTURE OF K-TH ROW TO ROW P(K) OF M
!
        JMIN = IA(P(K))
        JMAX = IA(P(K)+1) - 1
        IF (JMIN.GT.JMAX)  GO TO 4
        DO 3 J=JMIN,JMAX
          VJ = IP(JA(J))
          IF(VJ.LE.K)  GO TO 3
!
          QM = K
2         M = QM
          QM = Q(M)
          IF (QM.LT.VJ)  GO TO 2
          IF (QM.EQ.VJ)  GO TO 102
          LUK = LUK+1
          Q(M) = VJ
          Q(VJ) = QM
          IF (MARK(VJ).NE.TAG)  CLIQUE = .FALSE.
!
3       CONTINUE
!
!------IF EXACTLY ONE ROW IS TO BE MERGED INTO THE K-TH ROW AND THERE IS
!------A NONZERO ENTRY IN EVERY COLUMN IN THAT ROW IN WHICH THERE IS A
!------NONZERO ENTRY IN ROW P(K) OF M, THEN DOES NOT COMPUTE FILL-IN, JUST
!------USES THE COLUMN INDICES FOR THE ROW WHICH WAS TO HAVE BEEN MERGED
!
4       IF(.NOT.CLIQUE)  GO TO 5
        IJU(K) = IJU(JL(K)) + 1
        LUK = IU(JL(K)+1) - (IU(JL(K))+1)
        GO TO 17
!
!------MODIFIES NONZERO STRUCTURE OF K-TH ROW BY COMPUTING FILL-IN
!------FOR EACH ROW I TO BE MERGED IN
!
5       LMAX = 0
        IJU(K) = JUPTR
!
        I = K
6       I = JL(I)
        IF (I.EQ.0)  GO TO 10
!
!--------MERGES ROW I INTO K-TH ROW
!
        LUI = IU(I+1) - (IU(I)+1)
        JMIN = IJU(I) +  1
        JMAX = IJU(I) + LUI
        QM = K
!
        DO 8 J=JMIN,JMAX
          VJ = JU(J)
7         M = QM
          QM = Q(M)
          IF (QM.LT.VJ)  GO TO 7
          IF (QM.EQ.VJ)  GO TO 8
          LUK = LUK+1
          Q(M) = VJ
          Q(VJ) = QM
          QM = VJ
8       CONTINUE
!
!--------REMEMBERS LENGTH AND POSITION IN JU OF LONGEST ROW MERGED
!
        IF(LUI.LE.LMAX)  GO TO 6
        LMAX = LUI
        IJU(K) = JMIN
!
        GO TO 6
!
!------IF THE K-TH ROW IS THE SAME LENGTH AS THE LONGEST ROW MERGED,
!------THEN USES THE COLUMN INDICES FOR THAT ROW
!
10      IF (LUK.EQ.LMAX)  GO TO 17
!
!------IF THE TAIL OF THE LAST ROW SAVED IN JU IS THE SAME AS THE HEAD
!------OF THE K-TH ROW, THEN OVERLAPS THE TWO SETS OF COLUMN INDICES --
!--------SEARCHES LAST ROW SAVED FOR FIRST NONZERO ENTRY IN K-TH ROW ...
!
        I = Q(K)
        IF (JUMIN.GT.JUPTR)  GO TO 12
        DO 11 JMIN=JUMIN,JUPTR
          IF (JU(JMIN) < I) GOTO 11
          IF (JU(JMIN) == I) GOTO 13
          IF (JU(JMIN) > I) GOTO 12
11      CONTINUE
12      GO TO 15
!
!--------... AND THEN TESTS WHETHER TAIL MATCHES HEAD OF K-TH ROW
!
13      IJU(K) = JMIN
        DO J=JMIN,JUPTR
          IF (JU(J).NE.I)  GO TO 15
          I = Q(I)
          IF (I.GT.N)  GO TO 17
        ENDDO
        JUPTR = JMIN - 1
!
!------SAVES NONZERO STRUCTURE OF K-TH ROW IN JU
!
15      I = K
        JUMIN = JUPTR +  1
        JUPTR = JUPTR + LUK
        IF (JUPTR.GT.JUMAX)  GO TO 106
        DO J=JUMIN,JUPTR
          I = Q(I)
          JU(J) = I
          MARK(I) = K
        ENDDO
        IJU(K) = JUMIN
!
!------ADDS K TO ROW LIST FOR FIRST NONZERO ELEMENT IN K-TH ROW
!
17      IF(LUK.LE.1)  GO TO 18
        I = JU(IJU(K))
        JL(K) = JL(I)
        JL(I) = K
!
18    IU(K+1) = IU(K) + LUK
!
19    CONTINUE
!
      FLAG = 0
      RETURN
!
! ** ERROR -- DUPLICATE ENTRY IN A
!
102   FLAG = 2*N + P(K)
      RETURN
!
! ** ERROR -- INSUFFICIENT STORAGE FOR JU
!
106   FLAG = 6*N + K
      RETURN
      END
