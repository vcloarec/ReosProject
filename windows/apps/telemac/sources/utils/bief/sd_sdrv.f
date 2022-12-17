!                   ******************
                    SUBROUTINE SD_SDRV
!                   ******************
!
     &(N,P,IP,IA,JA,A,B,Z,NSP,ISP,RSP,ESP,PATH,FLAG)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    DRIVER FOR SPARSE MATRIX REORDERING ROUTINE.
!code
!+    SDRV SOLVES SPARSE SYMMETRIC POSITIVE DEFINITE SYSTEMS OF LINEAR
!+    EQUATIONS.  THE SOLUTION PROCESS IS DIVIDED INTO THREE STAGES --
!+
!+      SSF - THE COEFFICIENT MATRIX M IS FACTORED SYMBOLICALLY TO
!+            DETERMINE WHERE FILLIN WILL OCCUR DURING THE NUMERIC
!+            FACTORIZATION.
!+
!+      SNF - M IS FACTORED NUMERICALLY INTO THE PRODUCT UT-D-U, WHERE
!+            D IS DIAGONAL AND U IS UNIT UPPER TRIANGULAR.
!+
!+      SNS - THE LINEAR SYSTEM  MX = B  IS SOLVED USING THE UT-D-U
!+            FACTORIZATION FROM SNF.
!+
!+    FOR SEVERAL SYSTEMS WITH THE SAME COEFFICIENT MATRIX, SSF AND SNF
!+    NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNS IS DONE
!+    ONCE FOR EACH ADDITIONAL RIGHT-HAND SIDE.  FOR SEVERAL SYSTEMS
!+    WHOSE COEFFICIENT MATRICES HAVE THE SAME NONZERO STRUCTURE, SSF
!+    NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNF AND SNS
!+    ARE DONE ONCE FOR EACH ADDITIONAL SYSTEM.
!+
!+
!+  STORAGE OF SPARSE MATRICES
!+
!+    THE NONZERO ENTRIES OF THE MATRIX M ARE STORED ROW-BY-ROW IN THE
!+    ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO ENTRIES IN EACH ROW,
!+    WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY LIES.  THESE COLUMN
!+    INDICES ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN
!+    JA(K) = J.  TO IDENTIFY THE INDIVIDUAL ROWS, WE NEED TO KNOW WHERE
!+    EACH ROW STARTS.  THESE ROW POINTERS ARE STORED IN THE ARRAY IA;
!+    I.E., IF M(I,J) IS THE FIRST NONZERO ENTRY (STORED) IN THE I-TH ROW
!+    AND  A(K) = M(I,J),  THEN  IA(I) = K.  MOREOVER, IA(N+1) POINTS TO
!+    THE FIRST LOCATION FOLLOWING THE LAST ELEMENT IN THE LAST ROW.
!+    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IA(I+1) - IA(I),
!+    THE NONZERO ENTRIES IN THE I-TH ROW ARE STORED CONSECUTIVELY IN
!+
!+            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),
!+
!+    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN
!+
!+            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).
!+
!+    SINCE THE COEFFICIENT MATRIX IS SYMMETRIC, ONLY THE NONZERO ENTRIES
!+    IN THE UPPER TRIANGLE NEED BE STORED, FOR EXAMPLE, THE MATRIX
!+
!+             ( 1  0  2  3  0 )
!+             ( 0  4  0  0  0 )
!+         M = ( 2  0  5  6  0 )
!+             ( 3  0  6  7  8 )
!+             ( 0  0  0  8  9 )
!+
!+    COULD BE STORED AS
!+
!+            \ 1  2  3  4  5  6  7  8  9 10 11 12 13
!+         ---+--------------------------------------
!+         IA \ 1  4  5  8 12 14
!+         JA \ 1  3  4  2  1  3  4  1  3  4  5  4  5
!+          A \ 1  2  3  4  2  5  6  3  6  7  8  8  9
!+
!+    OR (SYMMETRICALLY) AS
!+
!+            \ 1  2  3  4  5  6  7  8  9
!+         ---+--------------------------
!+         IA \ 1  4  5  7  9 10
!+         JA \ 1  3  4  2  3  4  4  5  5
!+          A \ 1  2  3  4  5  6  7  8  9          .
!+
!+
!+  REORDERING THE ROWS AND COLUMNS OF M
!+
!+    A SYMMETRIC PERMUTATION OF THE ROWS AND COLUMNS OF THE COEFFICIENT
!+    MATRIX M (E.G., WHICH REDUCES FILLIN OR ENHANCES NUMERICAL
!+    STABILITY) MUST BE SPECIFIED.  THE SOLUTION Z IS RETURNED IN THE
!+    ORIGINAL ORDER.
!+
!+    TO SPECIFY THE TRIVIAL ORDERING (I.E., THE IDENTITY PERMUTATION),
!+    SET  P(I) = IP(I) = I,  I=1,...,N.  IN THIS CASE, P AND IP CAN BE
!+    THE SAME ARRAY.
!+
!+    IF A NONTRIVIAL ORDERING (I.E., NOT THE IDENTITY PERMUTATION) IS
!+    SPECIFIED AND M IS STORED SYMMETRICALLY (I.E., NOT BOTH M(I,J) AND
!+    M(J,I) ARE STORED FOR I NE J), THEN ODRV SHOULD BE CALLED (WITH
!+    PATH = 3 OR 5) TO SYMMETRICALLY REORDER (IA,JA,A) BEFORE CALLING
!+    SDRV.  THIS IS TO ENSURE THAT IF M(I,J) WILL BE IN THE UPPER
!+    TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN M(I,J) IS
!+    STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J)
!+    WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN
!+    ROW J (AND THUS M(I,J) IS NOT STORED).
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
!history  J-M HERVOUET (LNHE)
!+        10/08/2012
!+        V6P2
!+   RATIO set to 1 and removed.
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M,
!|                |   | STORED BY ROWS;  DIMENSION =NUMBER OF NONZERO
!|                |   | ENTRIES IN (THE UPPER TRIANGLE OF) M
!| B              |-->| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | RIGHT-HAND SIDE B; B AND Z CAN BE THE SAME ARRAY;
!|                |   | DIMENSION = N
!| ESP            |<--| INTEGER VARIABLE;  IF SUFFICIENT STORAGE WAS
!|                |   | AVAILABLE TO PERFORM THE SYMBOLIC
!|                |   | FACTORIZATION (SSF), THEN ESP IS SET TO
!|                |   | THE AMOUNT OF EXCESS STORAGE PROVIDED
!|                |   | (NEGATIVE IF INSUFFICIENT STORAGE WAS
!|                |   | AVAILABLE TO PERFORM THE NUMERIC
!|                |   | FACTORIZATION (SNF))
!| FLAG           |<--| INTEGER ERROR FLAG; VALUES AND THEIR MEANINGS:
!|                |   | 0     NO ERRORS DETECTED
!|                |   | 2N+K   DUPLICATE ENTRY IN A  --  ROW = K
!|                |   | 6N+K   INSUFFICIENT STORAGE IN SSF -- ROW = K
!|                |   | 7N+1   INSUFFICIENT STORAGE IN SNF
!|                |   | 8N+K   ZERO PIVOT  --  ROW = K
!|                |   | 10N+1  INSUFFICIENT STORAGE IN SDRV
!|                |   | 11N+1  ILLEGAL PATH SPECIFICATION
!| IA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!|                |   | POINTERS TO DELIMIT ROWS IN JA AND A;
!|                |   | DIMENSION = N+1
!| IP             |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!|                |   | THE INVERSE OF THE PERMUTATION RETURNED IN P;
!|                |   | DIMENSION = N
!| ISP            |<--| INTEGER ONE-DIMENSIONAL ARRAY USED FOR
!|                |   | WORKING STORAGE; DIMENSION = NSP
!| JA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!|                |   | OF A;  DIMENSION = NUMBER OF NONZERO ENTRIES
!|                |   | IN (THE UPPER TRIANGLE OF) M
!| N              |-->| ORDER OF THE MATRIX
!| NSP            |-->| DECLARED DIMENSION OF THE ONE-DIMENSIONAL
!|                |   | ARRAY ISP;  NSP MUST BE AT LEAST  3N+4K,
!|                |   | WHERE K IS THE NUMBER OF NONZEROES
!|                |   | IN THE STRICT UPPER TRIANGLE OF M
!| P              |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!|                |   | THE PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| PATH           |-->| INTEGER PATH SPECIFICATION;
!|                |   | VALUES AND THEIR MEANINGS ARE -
!|                |   | 1  PERFORM SSF, SNF, AND SNS
!|                |   | 2  PERFORM SNF AND SNS (ISP/RSP IS ASSUMED
!|                |   | TO HAVE BEEN SET UP IN AN EARLIER CALL
!|                |   | TO SDRV (FOR SSF))
!|                |   | 3  PERFORM SNS ONLY (ISP/RSP IS ASSUMED
!|                |   | TO HAVE BEEN SET UP IN AN EARLIER CALL
!|                |   | TO SDRV (FOR SSF AND SNF))
!|                |   | 4  PERFORM SSF
!|                |   | 5  PERFORM SSF AND SNF
!|                |   | 6  PERFORM SNF ONLY (ISP/RSP IS ASSUMED TO
!|                |   | HAVE BEEN SET UP IN AN EARLIER CALL TO
!|                |   | SDRV (FOR SSF))
!| RSP            |<--| REAL ONE-DIMENSIONAL ARRAY USED FOR WORKING
!|                |   | STORAGE;  RSP AND ISP SHOULD BE EQUIVALENCED;
!|                |   | DIMENSION = NSP
!| Z              |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | SOLUTION X;  Z AND B CAN BE THE SAME ARRAY;
!|                |   | DIMENSION = N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_SDRV => SD_SDRV
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: N,NSP,PATH
      INTEGER, INTENT(INOUT) :: FLAG,P(N),IP(*),IA(*),JA(*),ISP(*),ESP
      DOUBLE PRECISION, INTENT(IN)    :: B(N)
      DOUBLE PRECISION, INTENT(INOUT) :: A(*),Z(N),RSP(NSP)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER Q,MARK,D,U,UMAX,IJU,IU,IL,JL,JU,JUMAX
!
!-----------------------------------------------------------------------
!
!----VALIDATES PATH SPECIFICATION
!
      IF(PATH.LT.1.OR.6.LT.PATH) GO TO 111
!
!----ALLOCATES STORAGE AND FACTORS M SYMBOLICALLY TO DETERMINE FILL-IN
!
      IJU   = 1
      IU    = IJU     +  N
      JL    = IU      +  N+1
      JU    = JL      +  N
      Q     = NSP+1   -  N
      MARK  = Q       -  N
      JUMAX = MARK    - JU
!
      IF((PATH-1) * (PATH-4) * (PATH-5) .NE. 0)  GO TO 1
      IF(JUMAX.LE.0) GO TO 110
      CALL SD_SSF(N,P,IP,IA,JA,ISP(IJU),ISP(JU),ISP(IU),JUMAX,
     &            ISP(Q),ISP(MARK),ISP(JL),FLAG)
      IF (FLAG.NE.0) GO TO 100
!
!----ALLOCATES STORAGE AND FACTORS M NUMERICALLY
!
1     CONTINUE
      IL   = JU      + ISP(IJU+N-1)
      D    = IL      + N
      U    = D       + N
      UMAX = (NSP+1) - U
      ESP  = UMAX    - (ISP(IU+N)-1)
!
      IF(PATH.NE.1.AND.PATH.NE.2.AND.
     &   PATH.NE.5.AND.PATH.NE.6) GO TO 2
      IF (UMAX.LE.0)  GO TO 110
      CALL SD_SNF(N,P,IP,IA,JA,A,
     &            RSP(D),ISP(IJU),ISP(JU),ISP(IU),RSP(U),UMAX,
     &            ISP(IL),ISP(JL),FLAG)
      IF(FLAG.NE.0)  GO TO 100
!
!----SOLVES SYSTEM OF LINEAR EQUATIONS  MX = B
!
2     IF((PATH-1) * (PATH-2) * (PATH-3) .NE. 0)  GO TO 3
      IF (UMAX.LE.0)  GO TO 110
      CALL SD_SNS(N,P,RSP(D),ISP(IJU),ISP(JU),
     &            ISP(IU),RSP(U),Z,B,RSP(IL))
!
3     RETURN
!
! ** ERROR -- ERROR DETECTED IN SSF, SNF, OR SNS
!
100   RETURN
!
! ** ERROR -- INSUFFICIENT STORAGE
!
110   FLAG = 10*N + 1
      RETURN
!
! ** ERROR -- ILLEGAL PATH SPECIFICATION
!
111   FLAG = 11*N + 1
!
!-----------------------------------------------------------------------
!
      RETURN
      END
