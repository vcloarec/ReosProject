!                   *****************
                    SUBROUTINE SD_MDI
!                   *****************
!
     &(N,IA,JA,MAXIMUM,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES.
!
!note     IMPORTANT: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!
!history  E. RAZAFINDRAKOTO (LNHE)
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
!+   Changed MAX to MAXIMUM for NAG Compiler
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLAG           |<--| FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR
!|                |   | MEANINGS ARE : 0      NO ERRORS DETECTED
!|                |   |         9*N + VI  INSUFFICIENT STORAGE IN MDI
!| HEAD           |<--| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| IA, JA         |-->| COMPACT STORAGE STRUCTURE OF MATRIX
!| L              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!| LAST           |---| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE
!|                |   | PERMUTATION OF THE ROWS AND COLUMNS OF M
!|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!|                |   | DIMENSION = N
!| MARK           |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=N
!| MAXIMUM        |-->| DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS
!|                |   | V AND L;
!| N              |-->| RANK OF MATRIX
!| NEXT           |<--| INVERSE OF THE PERMUTATION RETURNED IN LAST
!|                |   | DIMENSION = N
!| TAG            |-->| SEE DEFINITION IN INTERNAL PARAMATERS OF SD_MD.f
!| V              |---| INTEGER ONE-DIMENSIONAL WORK ARRAY;DIMENSION=MAX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_MDI => SD_MDI
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: N,MAXIMUM,IA(*),JA(*)
      INTEGER, INTENT(INOUT) :: V(*),L(*),HEAD(*),LAST(*)
      INTEGER, INTENT(INOUT) :: NEXT(*),MARK(*),TAG,FLAG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER SFS,VI,DVI,VJ,JMIN,JMAX,J
!
!----INITIALISES DEGREES, ELEMENT LISTS, AND DEGREE LISTS
!
      DO VI=1,N
        MARK(VI) = 1
        L(VI) = 0
        HEAD(VI) = 0
      ENDDO ! VI
      SFS = N+1
!
!----CREATES NONZERO STRUCTURE
!----FOR EACH NONZERO ENTRY A(VI,VJ) IN STRICT UPPER TRIANGLE
!
      DO VI=1,N
        JMIN = IA(VI)
        JMAX = IA(VI+1) - 1
        IF(JMIN.GT.JMAX)  CYCLE
        DO J=JMIN,JMAX
          VJ = JA(J)
          IF(VI.GE.VJ) CYCLE
          IF(SFS.GE.MAXIMUM) GO TO 101
!
!------ENTERS VJ IN ELEMENT LIST FOR VI
!
          MARK(VI) = MARK(VI) + 1
          V(SFS) = VJ
          L(SFS) = L(VI)
          L(VI) = SFS
          SFS = SFS+1
!
!------ENTERS VI IN ELEMENT LIST FOR VJ
!
          MARK(VJ) = MARK(VJ) + 1
          V(SFS) = VI
          L(SFS) = L(VJ)
          L(VJ) = SFS
          SFS = SFS+1
        ENDDO ! J
      ENDDO ! VI
!
!----CREATES DEGREE LISTS AND INITIALISES MARK VECTOR
!
      DO VI=1,N
        DVI = MARK(VI)
        NEXT(VI) = HEAD(DVI)
        HEAD(DVI) = VI
        LAST(VI) = -DVI
        IF(NEXT(VI).GT.0)  LAST(NEXT(VI)) = VI
        MARK(VI) = TAG
      ENDDO ! VI
!
      RETURN
!
! ** ERROR -- INSUFFICIENT STORAGE
!
101   FLAG = 9*N + VI
!
!-----------------------------------------------------------------------
!
      RETURN
      END
