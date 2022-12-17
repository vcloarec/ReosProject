!                   *****************
                    SUBROUTINE ASMVE1
!                   *****************
!
     &(X, IKLE,W, NPOIN,NELEM,NELMAX,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MULTIPLICATIVE ASSEMBLY LOOP FOR A VECTOR.
!+
!+            THIS FORM OF ASSEMBLY IS USED WITH PRECONDITIONINGS
!+                OF THE TYPE ELEMENTARY 'CROUT'.
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
!| LV             |-->| VECTOR LENGTH OF THE COMPUTER
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NPOIN          |-->| NUMBER OF POINTS IN X
!| W              |-->| WORK ARRAY WITH A NON ASSEMBLED FORM OF THE
!|                |   | RESULT
!| X              |<->| ASSEMBLED VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,NPOIN,LV
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IB
!
      INTRINSIC MIN
!
!-----------------------------------------------------------------------
! LOOP IN SCALAR MODE (LV=1) OR WITH FORCED VECTORISATION
!-----------------------------------------------------------------------
!
      IF(LV.EQ.1) THEN
!
!  SCALAR MODE
!
      DO IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) * W(IELEM)
      ENDDO ! IELEM
!
      ELSE
!
!  VECTOR MODE
!
      DO IB = 1,(NELEM+LV-1)/LV
!VOCL LOOP,NOVREC
!DIR$ IVDEP
      DO IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        X(IKLE(IELEM)) = X(IKLE(IELEM)) * W(IELEM)
      ENDDO ! IELEM
      ENDDO ! IB
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
