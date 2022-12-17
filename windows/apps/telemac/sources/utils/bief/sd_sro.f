!                   *****************
                    SUBROUTINE SD_SRO
!                   *****************
!
     &(N,IP,IA,JA,A,Q,R,DFLAG)
!
!***********************************************************************
! BIEF   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    SYMMETRIC REORDERING OF SPARSE SYMMETRIC MATRIX.
!code
!+    THE NONZERO ENTRIES OF THE MATRIX M ARE ASSUMED TO BE STORED
!+    SYMMETRICALLY IN (IA,JA,A) FORMAT (I.E., NOT BOTH M(I,J) AND M(J,I)
!+    ARE STORED IF I NE J).
!+
!+    SRO DOES NOT REARRANGE THE ORDER OF THE ROWS, BUT DOES MOVE
!+    NONZEROES FROM ONE ROW TO ANOTHER TO ENSURE THAT IF M(I,J) WILL BE
!+    IN THE UPPER TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN
!+    M(I,J) IS STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS
!+    IF M(I,J) WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS
!+    STORED IN ROW J (AND THUS M(I,J) IS NOT STORED).
!
!note     IMPORTANT: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
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
!| A              |---| NONZERO ELEMENT OF MATRIX
!| DFLAG          |-->| LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN
!|                |   | STORE NONZERO DIAGONAL ELEMENTS AT THE
!|                |   | BEGINNING OF THE ROW
!| IA             |---| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!|                |   | POINTERS TO DELIMIT ROWS IN JA AND A;
!|                |   | DIMENSION = N+1
!! IP             |-->| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!|                |   | THE INVERSE OF THE PERMUTATION RETURNED IN IP;
!|                |   | DIMENSION = N
!| JA             |---| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!|                |   | OF A;
!| N              |-->| DIMENSION = NUMBER OF
!|                |   | NONZERO ENTRIES IN THE UPPER TRIANGLE OF M
!| Q              |<--| INTEGER ONE-DIMENSIONAL WORK ARRAY
!| R              |<--| INTEGER ONE-DIMENSIONAL WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_SRO => SD_SRO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: N
      INTEGER, INTENT(IN)             :: IP(*)
      INTEGER, INTENT(INOUT)          :: JA(*),R(*),Q(N),IA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
      LOGICAL, INTENT(IN)             :: DFLAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,JMIN,JMAX,ILAST,JDUMMY,JAK
      DOUBLE PRECISION AK
!
!-----------------------------------------------------------------------
!
!--PHASE 1 -- FINDS ROW IN WHICH TO STORE EACH NONZERO
!----INITIALISES COUNT OF NONZEROES TO BE STORED IN EACH ROW
!
      DO I=1,N
        Q(I) = 0
      ENDDO
!
!----FOR EACH NONZERO ELEMENT A(J)
!
      DO 3 I=1,N
        JMIN = IA(I)
        JMAX = IA(I+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 3
        DO 2 J=JMIN,JMAX
!
!--------FINDS ROW (=R(J)) AND COLUMN (=JA(J)) IN WHICH TO STORE A(J)
!
          K = JA(J)
          IF (IP(K).LT.IP(I))  JA(J) = I
          IF (IP(K).GE.IP(I))  K = I
          R(J) = K
!
!--------AND INCREMENTS COUNT OF NONZEROES (=Q(R(J)) IN THAT ROW
!
          Q(K) = Q(K) + 1
2       CONTINUE
3     CONTINUE
!
!--PHASE 2 -- FINDS NEW IA AND PERMUTATION TO APPLY TO (JA,A)
!----DETERMINES POINTERS TO DELIMIT ROWS IN PERMUTED (JA,A)
!
      DO 4 I=1,N
        IA(I+1) = IA(I) + Q(I)
        Q(I) = IA(I+1)
4     CONTINUE
!
!----DETERMINES WHERE EACH (JA(J),A(J)) IS STORED IN PERMUTED (JA,A)
!----FOR EACH NONZERO ELEMENT (IN REVERSE ORDER)
!
      ILAST = 0
      JMIN = IA(1)
      JMAX = IA(N+1) - 1
      J = JMAX
      DO 6 JDUMMY=JMIN,JMAX
        I = R(J)
        IF(.NOT.DFLAG .OR. JA(J).NE.I .OR. I.EQ.ILAST)  GO TO 5
!
!------IF DFLAG, THEN PUTS DIAGONAL NONZERO AT BEGINNING OF ROW
!
        R(J) = IA(I)
        ILAST = I
        GO TO 6
!
!------PUTS (OFF-DIAGONAL) NONZERO IN LAST UNUSED LOCATION IN ROW
!
5       Q(I) = Q(I) - 1
        R(J) = Q(I)
!
        J = J-1
6     CONTINUE
!
!--PHASE 3 -- PERMUTES (JA,A) TO UPPER TRIANGULAR FORM (WRT NEW ORDERING)
!
      DO 8 J=JMIN,JMAX
7       IF (R(J).EQ.J)  GO TO 8
        K = R(J)
        R(J) = R(K)
        R(K) = K
        JAK = JA(K)
        JA(K) = JA(J)
        JA(J) = JAK
        AK = A(K)
        A(K) = A(J)
        A(J) = AK
        GO TO 7
8     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
