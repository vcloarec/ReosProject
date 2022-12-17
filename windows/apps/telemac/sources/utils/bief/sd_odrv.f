!                   ******************
                    SUBROUTINE SD_ODRV
!                   ******************
!
     &(N,IA,JA,A,P,IP,NSP,ISP,PATH,FLAG)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    DRIVER FOR SPARSE MATRIX REORDERING ROUTINE.
!code
!+    ODRV FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND COLUMNS OF A
!+    SYMMETRIC MATRIX M STORED IN (IA,JA,A) FORMAT (SEE BELOW).  FOR THE
!+    REORDERED MATRIX, THE WORK AND STORAGE REQUIRED TO PERFORM GAUSSIAN
!+    ELIMINATION IS (USUALLY) SIGNIFICANTLY LESS.
!+
!+    IF ONLY THE NONZERO ENTRIES IN THE UPPER TRIANGLE OF M ARE BEING
!+    STORED, THEN ODRV SYMMETRICALLY REORDERS (IA,JA,A), (OPTIONALLY)
!+    WITH THE DIAGONAL ENTRIES PLACED FIRST IN EACH ROW.  THIS IS TO
!+    ENSURE THAT IF M(I,J) WILL BE IN THE UPPER TRIANGLE OF M WITH
!+    RESPECT TO THE NEW ORDERING, THEN M(I,J) IS STORED IN ROW I (AND
!+    THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J) WILL BE IN THE
!+    STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN ROW J (AND
!+    THUS M(I,J) IS NOT STORED).
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
!+    IN THE UPPER TRIANGLE NEED BE STORED.  FOR EXAMPLE, THE MATRIX
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
!+          A \ 1  2  3  4  5  6  7  8  9
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
!history  U.H.Merkel
!+        2012
!+        V6P2
!+   Changed MAX to MAXU for NAG Compiler
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M,
!|                |   | STORED BY ROWS;  DIMENSION =NUMBER OF NONZERO
!|                |   | ENTRIES IN (THE UPPER TRIANGLE OF) M
!| FLAG           |<--| INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -
!|                |   | 0    NO ERRORS DETECTED
!|                |   | 9N+K  INSUFFICIENT STORAGE IN MD
!|                |   | 10N+1  INSUFFICIENT STORAGE IN ODRV
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
!|                |   | 1  FIND MINIMUM DEGREE ORDERING ONLY
!|                |   | 2  FIND MINIMUM DEGREE ORDERING AND
!|                |   | REORDER SYMMETRICALLY
!|                |   | STORED MATRIX (USED WHEN ONLY THE NONZERO
!|                |   | ENTRIES IN THE UPPER TRIANGLE OF M ARE
!|                |   | BEING STORED)
!|                |   | 3  REORDER SYMMETRICALLY STORED MATRIX AS
!|                |   | SPECIFIED BY INPUT PERMUTATION (USED WHEN
!|                |   | AN ORDERING HAS ALREADY BEEN DETERMINED
!|                |   | AND ONLY THE NONZERO ENTRIES IN THE
!|                |   | UPPER TRIANGLE OF M ARE BEING STORED)
!|                |   | 4  SAME AS 2 BUT PUT DIAGONAL ENTRIES AT
!|                |   | START OF EACH ROW
!|                |   | 5  SAME AS 3 BUT PUT DIAGONAL ENTRIES AT
!|                |   | START OF EACH ROW
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_ODRV => SD_ODRV
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: N,NSP,PATH
      INTEGER, INTENT(INOUT)          :: FLAG
      INTEGER, INTENT(INOUT)          :: IA(N+1),JA(*),P(N)
      INTEGER, INTENT(INOUT)          :: IP(N),ISP(NSP)
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER V,L,HEAD,TMP,Q,MAXU
      LOGICAL DFLAG
!
!-----------------------------------------------------------------------
!
!----INITIALISES ERROR FLAG AND VALIDATES PATH SPECIFICATION
!
      FLAG = 0
      IF(PATH.LT.1.OR.5.LT.PATH) GO TO 111
!
!----ALLOCATES STORAGE AND FINDS MINIMUM DEGREE ORDERING
!
      IF((PATH-1)*(PATH-2)*(PATH-4).NE.0) GO TO 1
      MAXU = (NSP-N)/2
      V    = 1
      L    = V     +  MAXU
      HEAD = L     +  MAXU
      IF(MAXU.LT.N) GO TO 110
!
      CALL SD_MD(N,IA,JA,MAXU,ISP(V),ISP(L),ISP(HEAD),P,IP,ISP(V),FLAG)
!
      IF(FLAG.NE.0) GO TO 100
!
!----ALLOCATES STORAGE AND SYMMETRICALLY REORDERS MATRIX
!
1     IF ((PATH-2) * (PATH-3) * (PATH-4) * (PATH-5) .NE. 0)  GO TO 2
!
      TMP = (NSP+1) -      N
      Q   = TMP     - (IA(N+1)-1)
      IF (Q.LT.1)  GO TO 110
!
      DFLAG = PATH.EQ.4 .OR. PATH.EQ.5
      CALL SD_SRO(N,IP,IA,JA,A,ISP(TMP),ISP(Q),DFLAG)
!
2     RETURN
!
! ** ERROR -- ERROR DETECTED IN MD
!
100   RETURN
!
! ** ERROR -- INSUFFICIENT STORAGE
!
110   FLAG = 10*N + 1
      RETURN
!
! ** ERROR -- ILLEGAL PATH SPECIFIED
!
111   FLAG = 11*N + 1
!
!-----------------------------------------------------------------------
!
      RETURN
      END
