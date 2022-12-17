!                   *****************
                    SUBROUTINE ASMVEC
!                   *****************
!
     &(X, IKLE,NPOIN,NELEM,NELMAX,NDP,W,INIT,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MULTIPLICATIVE ASSEMBLY FOR A VECTOR.
!
!warning  THIS VECTOR IS INITIALISED TO 1 IF INIT = .TRUE.
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        17/08/94
!+        V5P1
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
!| IKLE           |-->| CONNECTIVITY TABLE
!| INIT           |-->| LOGICAL : IF TRUE X IS INITIALISED TO 0.
!| LV             |-->| VECTOR LENGTH OF THE COMPUTER
!| NDP            |-->| SECOND DIMENSION OF IKLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NPOIN          |-->| NUMBER OF POINTS IN X
!| W              |-->| WORK ARRAY WITH A NON ASSEMBLED FORM OF THE
!|                |   | RESULT
!|                |   | W HAS DIMENSION NELMAX * NDP(IELM)
!|                |   | NDP IS THE NUMBER OF POINTS IN THE ELEMENT
!| X              |<->| ASSEMBLED VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ASMVEC => ASMVEC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NPOIN,NELEM,NDP,LV
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX,NDP)
      LOGICAL         , INTENT(IN)    :: INIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDP
!
!-----------------------------------------------------------------------
!   INITIALISES VECTOR X TO 1 IF(INIT)
!-----------------------------------------------------------------------
!
      IF(INIT) CALL OV('X=C     ', X=X, C=1.D0, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!   ASSEMBLES, CONTRIBUTION OF LOCAL POINTS 1,... TO NDP
!-----------------------------------------------------------------------
!
      DO IDP = 1 , NDP
!
        CALL ASMVE1(X, IKLE(1,IDP),W(1,IDP),NPOIN,NELEM,NELMAX,LV)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
