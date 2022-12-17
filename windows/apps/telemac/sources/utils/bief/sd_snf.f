!                   *****************
                    SUBROUTINE SD_SNF
!                   *****************
!
     &(N,P,IP,IA,JA,A,D,IJU,JU,IU,U,UMAX,IL,JL,FLAG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    NUMERICAL UT-D-U FACTORISATION OF SPARSE SYMMETRICAL
!+                POSITIVE DEFINITE MATRIX.
!code
!+  COMPRESSED STORAGE OF SPARSE MATRICES
!+
!+    THE STRICT UPPER TRIANGULAR PORTION OF THE MATRIX U IS STORED IN
!+    (IA,JA,A) FORMAT USING THE ARRAYS IU, JU, AND U, EXCEPT THAT AN
!+    ADDITIONAL ARRAY IJU IS USED TO REDUCE THE STORAGE REQUIRED FOR JU
!+    BY ALLOWING SOME SEQUENCES OF COLUMN INDICES TO CORRESPOND TO MORE
!+    THAN ONE ROW.  FOR I < N, IJU(I) IS THE INDEX IN JU OF THE FIRST
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
!note     IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
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
!| A              |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M,
!|                |   | STORED BY ROWS;  DIMENSION =NUMBER OF NONZERO
!|                |   | ENTRIES IN (THE UPPER TRIANGLE OF) M
!| D              |<--| (D(I),I=K,N) CONTAINS THE K-TH ROW OF U (EXPANDED)
!| FLAG           |<--| LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN
!|                |   | STORE NONZERO DIAGONAL ELEMENTS AT THE
!|                |   | BEGINNING OF THE ROW
!| IA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!|                |   | POINTERS TO DELIMIT ROWS IN JA AND A;
!|                |   | DIMENSION = N+1
!| IJU            |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!|                |   | POINTERS TO THE START OF EACH ROW IN JU;  DIMENSION = N
!| IL             |<--| IL(I) POINTS TO THE FIRST NONZERO ELEMENT IN
!|                |   | COLUMNS K,...,N OF ROW I OF U
!| IP             |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!|                |   | THE INVERSE OF THE PERMUTATION RETURNED IN P;
!|                |   | DIMENSION = N
!| IU             |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
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
!| JU             |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!|                |   | OF U;  DIMENSION = JUMAX
!| N              |-->| ORDER OF THE MATRIX
!| P              |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!|                |   | THE PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| U              |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
!|                |   | OF U, STORED BY ROWS; DIMENSION = UMAX
!| UMAX           |-->| DECLARED DIMENSION OF THE ONE-DIMENSIONAL
!|                |   | ARRAY U;  UMAX MUST BE AT LEAST THE NUMBER
!|                |   | OF NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
!|                |   | OF M PLUS FILLIN (IU(N+1)-1 AFTER THE CALL TO SSF)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_SNF => SD_SNF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: N,UMAX
      INTEGER, INTENT(IN) :: P(N),IP(N),IA(N+1),JA(*),IJU(N),JU(*)
      INTEGER, INTENT(IN) :: IU(N+1)
      INTEGER, INTENT(INOUT) :: IL(*),JL(*),FLAG
      DOUBLE PRECISION, INTENT(IN)    :: A(*)
      DOUBLE PRECISION, INTENT(INOUT) :: D(N),U(UMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,JMIN,JMAX,VJ,NEXTI,ILI,MU,I,J,JUMUJ
      DOUBLE PRECISION DK,UKIDI
!
!-----------------------------------------------------------------------
!
!
!----CHECKS FOR SUFFICIENT STORAGE FOR U
!
      IF(IU(N+1)-1.GT.UMAX)  GO TO 107
!
!----INITIALISES
!
      DO K=1,N
        D(K)  = 0
        JL(K) = 0
      ENDDO
!
!----FOR EACH ROW K
!
      DO K=1,N
!
!------INITIALISES K-TH ROW WITH ELEMENTS NONZERO IN ROW P(K) OF M
!
        JMIN = IA(P(K))
        JMAX = IA(P(K)+1) - 1
        IF(JMIN.GT.JMAX) GO TO 5
        DO J=JMIN,JMAX
          VJ = IP(JA(J))
          IF (K.LE.VJ)  D(VJ) = A(J)
        ENDDO ! J
!
!------MODIFIES K-TH ROW BY ADDING IN THOSE ROWS I WITH U(I,K) NE 0
!------FOR EACH ROW I TO BE ADDED IN
!
5       DK = D(K)
        I = JL(K)
6       IF(I.EQ.0) GO TO 9
        NEXTI = JL(I)
!
!--------COMPUTES MULTIPLIER AND UPDATES DIAGONAL ELEMENT
!
        ILI = IL(I)
        UKIDI = - U(ILI) * D(I)
        DK = DK + UKIDI * U(ILI)
        U(ILI) = UKIDI
!
!--------ADDS MULTIPLE OF ROW I TO K-TH ROW ...
!
        JMIN = ILI     + 1
        JMAX = IU(I+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 8
        MU = IJU(I) - IU(I)
        DO J=JMIN,JMAX
          D(JU(MU+J)) = D(JU(MU+J)) + UKIDI * U(J)
        ENDDO
!
!--------... AND ADDS I TO ROW LIST FOR NEXT NONZERO ENTRY
!
        IL(I) = JMIN
        J = JU(MU+JMIN)
        JL(I) = JL(J)
        JL(J) = I
!
8       I = NEXTI
        GO TO 6
!
!------CHECKS FOR 0 PIVOT AND SAVES DIAGONAL ELEMENT
!
9       IF(DK.EQ.0)  GO TO 108
        D(K) = 1 / DK
!
!------SAVES NONZERO ENTRIES IN K-TH ROW OF U ...
!
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMIN.GT.JMAX) CYCLE
        MU = IJU(K) - JMIN
        DO J=JMIN,JMAX
          JUMUJ = JU(MU+J)
          U(J) = D(JUMUJ)
          D(JUMUJ) = 0
        ENDDO
!
!------... AND ADDS K TO ROW LIST FOR FIRST NONZERO ENTRY IN K-TH ROW
!
        IL(K) = JMIN
        I = JU(MU+JMIN)
        JL(K) = JL(I)
        JL(I) = K
      ENDDO ! K
!
      FLAG = 0
      RETURN
!
! ** ERROR -- INSUFFICIENT STORAGE FOR U
!
107   FLAG = 7*N + 1
      RETURN
!
! ** ERROR -- 0 PIVOT
!
108   FLAG = 8*N + K
!
!-----------------------------------------------------------------------
!
      RETURN
      END
