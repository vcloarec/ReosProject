!                   *****************
                    SUBROUTINE ASSVE1
!                   *****************
!
     &(X, IKLE,W, NELEM,NELMAX,LV,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLY LOOP FOR A VECTOR.
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        18/08/94
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
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
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
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,LV
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX),MASKEL(*)
      LOGICAL         , INTENT(IN)    :: MSK
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
!  WITH MASKING
!
      IF(MSK) THEN
!
      IF(LV.EQ.1) THEN
!
!  SCALAR MODE
!
      DO IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM) * MASKEL(IELEM)
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
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM) * MASKEL(IELEM)
      ENDDO ! IELEM
      ENDDO ! IB
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  WITHOUT MASKING
!
      ELSE
!
      IF(LV.EQ.1) THEN
!
!  SCALAR MODE
!
      DO IELEM = 1 , NELEM
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM)
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
        X(IKLE(IELEM)) = X(IKLE(IELEM)) + W(IELEM)
      ENDDO ! IELEM
      ENDDO ! IB
!
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
