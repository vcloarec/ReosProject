!                   *****************
                    SUBROUTINE FILTER
!                   *****************
!
     &(VEC,BLDMAT,T1,T2,
     & A,FORMUL,
     & XMUL,F,G,H,U,V,W,
     & MESH,MSK,MASKEL,N)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FILTERS A VECTOR USING A MATRIX.
!+
!+            FOR EXAMPLE, THE USE OF A MASS MATRIX YIELDS
!+                SMOOTHING.
!
!note     IF BLDMAT=.FALSE. MATRIX A IS GIVEN, IT IS NOT RE-BUILT.
!
!history  J-M HERVOUET (LNH)
!+        24/04/97
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
!| A              |<->| MATRIX (GIVEN OR BUILT DEPENDING ON BLDMAT)
!| BLDMAT         |-->| IF YES : MATRIX BUILT ACCORDING TO FORMUL
!| F,G,H,U,V,W    |-->| FUNCTIONS THAT MAY BE USED FOR BUILDING A
!| FORMUL         |-->| FORMULA DESCRIBING THE MATRIX (AS IN MATRIX)
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| N              |-->| FILTERING WILL BE DONE N TIMES
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY. MATRIX A LUMPED AT THE END
!| VEC            |<->| VECTOR TO BE FILTERED
!| XMUL           |-->| MULTIPLICATION FACTOR
!|                |   | NO INFLUENCE EXCEPT ON T2.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FILTER => FILTER
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: N
      DOUBLE PRECISION, INTENT(IN)  :: XMUL
      LOGICAL, INTENT(IN)           :: BLDMAT,MSK
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VEC,A,T1,T2
      TYPE(BIEF_OBJ), INTENT(IN)    :: F,G,H,U,V,W,MASKEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION C
!
!-----------------------------------------------------------------------
!
      DO I=1,N
!
!  COMPUTES THE MATRIX ACCORDING TO THE GIVEN FORMULATION (OPTIONAL)
!
      IF(BLDMAT.AND.I.EQ.1) THEN
!
          CALL MATRIX(A,'M=N     ',FORMUL,VEC%ELM,VEC%ELM,
     &                XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE PRODUCT A * VEC (WITH ASSEMBLY)
!
      CALL MATVEC( 'X=AY    ',T1,A,VEC,C,MESH)
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
!
!-----------------------------------------------------------------------
!
!  COMPRESSES A ON ITS DIAGONAL
!
      IF(I.EQ.1) THEN
        CALL LUMP(T2,A,MESH,XMUL)
        IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
        CALL OS('X=1/Y   ',X=T2,Y=T2,IOPT=2,INFINI=0.D0,ZERO=1.D-20)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPUTES F = A * F / (ASSEMBLED A)
!
!  CHECKS DIVISIONS BY 0 CAUSED BY EXTERNAL POINTS IN
!  THE LEONARD FORMAT, WHICH CAN HAVE 0 VALUES
!
      CALL OS('X=YZ    ',X=VEC,Y=T1,Z=T2)
!
!-----------------------------------------------------------------------
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
