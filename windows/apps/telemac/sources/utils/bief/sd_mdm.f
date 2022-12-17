!                   *****************
                    SUBROUTINE SD_MDM
!                   *****************
!
     &(VK,TAIL,V,L,LAST,NEXT,MARK)
!
!***********************************************************************
! BIEF   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    FORMS ELEMENT FROM UNELIMINATED NEIGHBOURS OF VK.
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
!| L              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!| LAST           |-->| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE
!|                |   | PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| MARK           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| NEXT           |-->| INVERSE OF THE PERMUTATION RETURNED IN LAST
!|                |   | DIMENSION = N
!| TAIL           |<--| UNELIMINATED NEIGHBOURS OF VERTEX VK
!| V              |-->| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!| VK             |-->| VERTEX NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_MDM => SD_MDM
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: VK,LAST(*),NEXT(*),V(*)
      INTEGER, INTENT(INOUT) :: TAIL,L(*),MARK(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER TAG,S,LS,VS,ES,B,LB,VB,BLP,BLPMAX
      EQUIVALENCE (VS,ES)
!
!----INITIALISES TAG AND LIST OF UNELIMINATED NEIGHBOURS
!
      TAG = MARK(VK)
      TAIL = VK
!
!----FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VK
!
      LS = L(VK)
1     S = LS
      IF(S.EQ.0)  GO TO 5
      LS = L(S)
      VS = V(S)
      IF(NEXT(VS).LT.0)  GO TO 2
!
!------IF VS IS UNELIMINATED VERTEX, THEN TAGS AND APPENDS TO LIST OF
!------UNELIMINATED NEIGHBOURS
!
      MARK(VS) = TAG
      L(TAIL) = S
      TAIL = S
      GO TO 4
!
!------IF ES IS ACTIVE ELEMENT, THEN ...
!--------FOR EACH VERTEX VB IN BOUNDARY LIST OF ELEMENT ES
!
2     LB = L(ES)
      BLPMAX = LAST(ES)
      DO BLP=1,BLPMAX
        B = LB
        LB = L(B)
        VB = V(B)
!
!----------IF VB IS UNTAGGED VERTEX, THEN TAGS AND APPENDS TO LIST OF
!----------UNELIMINATED NEIGHBOURS
!
        IF(MARK(VB).GE.TAG) CYCLE
        MARK(VB) = TAG
        L(TAIL) = B
        TAIL = B
      ENDDO ! BLP
!
!--------MARKS ES INACTIVE
!
      MARK(ES) = TAG
!
4     GO TO 1
!
!----TERMINATES LIST OF UNELIMINATED NEIGHBOURS
!
5     L(TAIL) = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
