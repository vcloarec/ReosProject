!                   *****************
                    SUBROUTINE SD_MDU
!                   *****************
!
     &(EK,DMIN,V,L,HEAD,LAST,NEXT,MARK)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    UPDATES DEGREES OF UNELIMINATED VERTICES IN EK.
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
!| DMIN           |---|
!| EK             |-->|
!| HEAD           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| L              |<--| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!| LAST           |---| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE
!|                |   | PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| MARK           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| NEXT           |---| INVERSE OF THE PERMUTATION RETURNED IN LAST
!|                |   | DIMENSION = N
!| V              |-->| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_MDU => SD_MDU
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    ::  EK,V(*),L(*)
      INTEGER, INTENT(INOUT) ::  DMIN,HEAD(*),LAST(*),NEXT(*),MARK(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER TAG,VI,EVI,DVI,S,VS,ES,B,VB,ILP,ILPMAX,BLP,BLPMAX,I
      EQUIVALENCE (VS,ES)
!
!----INITIALISES TAG
!
      TAG = MARK(EK) - LAST(EK)
!
!----FOR EACH VERTEX VI IN EK
!
      I = EK
      ILPMAX = LAST(EK)
      IF(ILPMAX.LE.0)  GO TO 11
      DO 10 ILP=1,ILPMAX
        I = L(I)
        VI = V(I)
        IF (LAST(VI) < 0) GOTO 1
        IF (LAST(VI) == 0) GOTO 10
        IF (LAST(VI) > 0) GOTO 8
!
!------IF VI NEITHER PROTOTYPE NOR DUPLICATE VERTEX, THEN MERGES ELEMENTS
!------TO COMPUTE DEGREE
!
1       TAG = TAG + 1
        DVI = LAST(EK)
!
!--------FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VI
!
        S = L(VI)
2       S = L(S)
        IF(S.EQ.0) GO TO 9
        VS = V(S)
        IF(NEXT(VS).LT.0)  GO TO 3
!
!----------IF VS IS UNELIMINATED VERTEX, THEN TAGS AND ADJUSTS DEGREE
!
        MARK(VS) = TAG
        DVI = DVI + 1
        GO TO 5
!
!----------IF ES IS ACTIVE ELEMENT, THEN EXPANDS
!------------CHECK FOR OUTMATCHED VERTEX
!
3       IF(MARK(ES).LT.0)  GO TO 6
!
!------------FOR EACH VERTEX VB IN ES
!
        B = ES
        BLPMAX = LAST(ES)
        DO 4 BLP=1,BLPMAX
          B = L(B)
          VB = V(B)
!
!--------------IF VB IS UNTAGGED, THEN TAGS AND ADJUSTS DEGREE
!
          IF(MARK(VB).GE.TAG)  GO TO 4
          MARK(VB) = TAG
          DVI = DVI + 1
4       CONTINUE
!
5       GO TO 2
!
!------ELSE IF VI IS OUTMATCHED VERTEX, THEN ADJUSTS OVERLAPS BUT DOES NOT
!------COMPUTE DEGREE
!
6       LAST(VI) = 0
        MARK(ES) = MARK(ES) - 1
7       S = L(S)
        IF(S.EQ.0)  GO TO 10
        ES = V(S)
        IF(MARK(ES).LT.0)  MARK(ES) = MARK(ES) - 1
        GO TO 7
!
!------ELSE IF VI IS PROTOTYPE VERTEX, THEN CALCULATES DEGREE BY
!------INCLUSION/EXCLUSION AND RESETS OVERLAP COUNT
!
8       EVI = LAST(VI)
        DVI = LAST(EK) + LAST(EVI) + MARK(EVI)
        MARK(EVI) = 0
!
!------INSERTS VI IN APPROPRIATE DEGREE LIST
!
9       NEXT(VI)  = HEAD(DVI)
        HEAD(DVI) = VI
        LAST(VI)  = -DVI
        IF(NEXT(VI).GT.0)  LAST(NEXT(VI)) = VI
        IF(DVI.LT.DMIN)  DMIN = DVI
!
10    CONTINUE
!
11    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
