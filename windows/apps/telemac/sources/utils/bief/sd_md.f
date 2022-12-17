!                 ****************
                  SUBROUTINE SD_MD
!                 ****************
!
     &(N,IA,JA,MAXU,V,L,HEAD,LAST,NEXT,MARK,FLAG)
!
!***********************************************************************
! BIEF   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    MINIMUM DEGREE ALGORITHM (BASED ON ELEMENT MODEL).
!+
!+            MD FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND
!+                COLUMNS OF A SYMMETRICAL MATRIX M STORED IN (IA,JA,A) FORMAT.
!code
!+  PARAMETERS
!+
!+    MAX  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS V AND L;
!+           MAX MUST BE AT LEAST  N+2K,  WHERE K IS THE NUMBER OF
!+           NONZEROES IN THE STRICT UPPER TRIANGLE OF M
!+
!+    V    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX
!+
!+    L    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX
!+
!+    HEAD - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N
!+
!+    LAST - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE PERMUTATION
!+           OF THE ROWS AND COLUMNS OF M CORRESPONDING TO THE MINIMUM
!+           DEGREE ORDERING;  DIMENSION = N
!+
!+    NEXT - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE INVERSE OF
!+           THE PERMUTATION RETURNED IN LAST;  DIMENSION = N
!+
!+    MARK - INTEGER ONE-DIMENSIONAL WORK ARRAY (MAY BE THE SAME AS V);
!+           DIMENSION = N
!+
!+    FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -
!+             0      NO ERRORS DETECTED
!+             11N+1  INSUFFICIENT STORAGE IN MD
!+
!+
!+  DEFINITIONS OF INTERNAL PARAMETERS
!+
!+    ---------+---------------------------------------------------------
!+    V(S)     \ VALUE FIELD OF LIST ENTRY
!+    ---------+---------------------------------------------------------
!+    L(S)     \ LINK FIELD OF LIST ENTRY  (0 => END OF LIST)
!+    ---------+---------------------------------------------------------
!+    L(VI)    \ POINTER TO ELEMENT LIST OF UNELIMINATED VERTEX VI
!+    ---------+---------------------------------------------------------
!+    L(EJ)    \ POINTER TO BOUNDARY LIST OF ACTIVE ELEMENT EJ
!+    ---------+---------------------------------------------------------
!+    HEAD(D)  \ VJ => VJ HEAD OF D-LIST D
!+             \  0 => NO VERTEX IN D-LIST D
!+             \          VI UNELIMINATED VERTEX
!+             \          VI IN EK           \       VI NOT IN EK
!+    ---------+-----------------------------+---------------------------
!+    NEXT(VI) \ UNDEFINED BUT NONNEGATIVE   \ VJ => VJ NEXT IN D-LIST
!+             \                             \  0 => VI TAIL OF D-LIST
!+    ---------+-----------------------------+---------------------------
!+    LAST(VI) \ (NOT SET UNTIL MDP)         \ -D => VI HEAD OF D-LIST D
!+             \-VK => COMPUTE DEGREE        \ VJ => VJ LAST IN D-LIST
!+             \ EJ => VI PROTOTYPE OF EJ    \  0 => VI NOT IN ANY D-LIST
!+             \  0 => DO NOT COMPUTE DEGREE \
!+    ---------+-----------------------------+---------------------------
!+    MARK(VI) \ MARK(VK)                    \ NONNEGATIVE TAG < MARK(VK)
!+
!+
!+             \                   VI ELIMINATED VERTEX
!+             \      EI ACTIVE ELEMENT      \           OTHERWISE
!+    ---------+-----------------------------+---------------------------
!+    NEXT(VI) \ -J => VI WAS J-TH VERTEX    \ -J => VI WAS J-TH VERTEX
!+             \       TO BE ELIMINATED      \       TO BE ELIMINATED
!+    ---------+-----------------------------+---------------------------
!+    LAST(VI) \  M => SIZE OF EI = M        \ UNDEFINED
!+    ---------+-----------------------------+---------------------------
!+    MARK(VI) \ -M => OVERLAP COUNT OF EI   \ UNDEFINED
!+             \       WITH EK = M           \
!+             \ OTHERWISE NONNEGATIVE TAG   \
!+             \       < MARK(VK)            \
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
!history  U.H.MErkel
!+        2012
!+        V6P2
!+   Changed MAX to MAXU for NAG Compiler
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLAG           |<--| FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR
!|                |   | MEANINGS ARE : 0      NO ERRORS DETECTED
!|                |   |               11N+1  INSUFFICIENT STORAGE IN MD
!| HEAD           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| IA, JA         |-->| SYMETRICAL COMPACT STORAGE OF MATRIX
!| L              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!| LAST           |---| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE
!|                |   | PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| MARK           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| MAXU           |-->| DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS
!|                |   | V AND L; MAX MUST BE AT LEAST  N+2K,  WHERE K IS
!|                |   | THE NUMBER OF NONZEROES IN THE STRICT UPPER
!|                |   | TRIANGLE OF M
!| N              |-->| DIMENSION OF SYSTEM
!| NEXT           |<--| INVERSE OF THE PERMUTATION RETURNED IN LAST
!|                |   | DIMENSION = N
!| V              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_MD => SD_MD
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: N,MAXU
      INTEGER, INTENT(INOUT) :: IA(*),JA(*),V(MAXU),L(MAXU),HEAD(N)
      INTEGER, INTENT(INOUT) :: LAST(N),NEXT(N),MARK(N),FLAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER TAG,DMIN,VK,EK,TAIL,K
      EQUIVALENCE  (VK,EK)
!
!-----------------------------------------------------------------------
!
!----INITIALISES
!
      TAG = 0
      CALL SD_MDI(N,IA,JA,MAXU,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG)
      IF(FLAG.NE.0)  RETURN
!
      K = 0
      DMIN = 1
!
!----WHILE  K
!
1     IF(K.GE.N) GO TO 4
!
!------SEARCHES FOR VERTEX OF MINIMUM DEGREE
!
2     IF(HEAD(DMIN).GT.0)  GO TO 3
      DMIN = DMIN + 1
      GO TO 2
!
!------REMOVES VERTEX VK OF MINIMUM DEGREE FROM DEGREE LIST
!
3     VK = HEAD(DMIN)
      HEAD(DMIN) = NEXT(VK)
      IF (HEAD(DMIN).GT.0) LAST(HEAD(DMIN)) = -DMIN
!
!------NUMBERS VERTEX VK, ADJUSTS TAG, AND TAGS VK
!
      K = K+1
      NEXT(VK) = -K
      LAST(EK) = DMIN - 1
      TAG = TAG + LAST(EK)
      MARK(VK) = TAG
!
!------FORMS ELEMENT EK FROM UNELIMINATED NEIGHBOURS OF VK
!
      CALL SD_MDM(VK,TAIL,V,L,LAST,NEXT,MARK)
!
!------PURGES INACTIVE ELEMENTS AND DOES MASS ELIMINATION
!
      CALL SD_MDP(K,EK,TAIL,V,L,HEAD,LAST,NEXT,MARK)
!
!------UPDATES DEGREES OF UNELIMINATED VERTICES IN EK
!
      CALL SD_MDU(EK,DMIN,V,L,HEAD,LAST,NEXT,MARK)
!
      GO TO 1
!
!----GENERATES INVERSE PERMUTATION FROM PERMUTATION
!
4     DO K=1,N
        NEXT(K) = -NEXT(K)
        LAST(NEXT(K)) = K
      ENDDO ! K
!
!-----------------------------------------------------------------------
!
      RETURN
      END
