!                   *****************
                    SUBROUTINE SD_MDP
!                   *****************
!
     &(K,EK,TAIL,V,L,HEAD,LAST,NEXT,MARK)
!
!***********************************************************************
! BIEF   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    PURGES INACTIVE ELEMENTS AND DOES MASS ELIMINATION.
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
!| EK             |-->|
!| HEAD           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| K              |-->|
!| L              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!| LAST           |---| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE
!|                |   | PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| MARK           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| NEXT           |---| INVERSE OF THE PERMUTATION RETURNED IN LAST
!|                |   | DIMENSION = N
!| TAIL           |---|
!| V              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_MDP => SD_MDP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: EK
      INTEGER, INTENT(INOUT) :: K,TAIL,V(*),L(*),HEAD(*)
      INTEGER, INTENT(INOUT) :: LAST(*),NEXT(*),MARK(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER TAG,FREE,ILI,VI,LVI,EVI,S,LS,ES,ILP,ILPMAX,I
!
!----INITIALISES TAG
!
      TAG = MARK(EK)
!
!----FOR EACH VERTEX VI IN EK
!
      ILI = EK
      ILPMAX = LAST(EK)
      IF(ILPMAX.LE.0)  GO TO 12
      DO 11 ILP=1,ILPMAX
        I = ILI
        ILI = L(I)
        VI = V(ILI)
!
!------REMOVES VI FROM DEGREE LIST
!
        IF(LAST(VI).EQ.0) GO TO 3
        IF(LAST(VI).GT.0) GO TO 1
        HEAD(-LAST(VI)) = NEXT(VI)
        GO TO 2
1       NEXT(LAST(VI)) = NEXT(VI)
2       IF(NEXT(VI).GT.0)  LAST(NEXT(VI)) = LAST(VI)
!
!------REMOVES INACTIVE ITEMS FROM ELEMENT LIST OF VI
!
3       LS = VI
4       S = LS
        LS = L(S)
        IF(LS.EQ.0)  GO TO 6
        ES = V(LS)
        IF (MARK(ES).LT.TAG)  GO TO 5
        FREE = LS
        L(S) = L(LS)
        LS = S
5       GO TO 4
!
!------IF VI IS INTERIOR VERTEX, THEN REMOVES FROM LIST AND ELIMINATES
!
6       LVI = L(VI)
        IF(LVI.NE.0)  GO TO 7
        L(I) = L(ILI)
        ILI = I
!
        K = K+1
        NEXT(VI) = -K
        LAST(EK) = LAST(EK) - 1
        GO TO 11
!
!------ELSE ...
!--------CLASSIFIES VERTEX VI
!
7       IF (L(LVI).NE.0)  GO TO 9
        EVI = V(LVI)
        IF(NEXT(EVI).GE.0)  GO TO 9
        IF(MARK(EVI).LT.0)  GO TO 8
!
!----------IF VI IS PROTOTYPE VERTEX, THEN MARKS AS SUCH, INITIALISES
!----------OVERLAP COUNT FOR CORRESPONDING ELEMENT, AND MOVES VI TO END
!----------OF BOUNDARY LIST
!
        LAST(VI) = EVI
        MARK(EVI) = -1
        L(TAIL) = ILI
        TAIL = ILI
        L(I) = L(ILI)
        ILI = I
        GO TO 10
!
!----------ELSE IF VI IS DUPLICATE VERTEX, THEN MARKS AS SUCH AND ADJUSTS
!----------OVERLAP COUNT FOR CORRESPONDING ELEMENT
!
8       LAST(VI) = 0
        MARK(EVI) = MARK(EVI) - 1
        GO TO 10
!
!----------ELSE MARKS VI TO COMPUTE DEGREE
!
9       LAST(VI) = -EK
!
!--------INSERTS EK IN ELEMENT LIST OF VI
!
10      V(FREE) = EK
        L(FREE) = L(VI)
        L(VI) = FREE
11    CONTINUE
!
!----TERMINATES BOUNDARY LIST
!
12    L(TAIL) = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
