!                   ****************
                    SUBROUTINE DES11
!                   ****************
!
     &(X, XA1,XA2,XA3,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,NPOIN,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE SYSTEM L X = B (ELEMENT: P1 TRIANGLE).
!+
!+            CASE.
!code
!+            B IS THE SAME AS X TO START WITH
!+
!+            L IS THE LOWER PART OF THE MATRIX, BUILT IN
!+            SUBROUTINE DECLDU.
!+
!+            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:
!+
!+            LE X DE X UE
!+
!+            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!+
!+                                                T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
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
!| IKLE1          |-->| GLOBAL NUMBER OF THE FIRST POINT OF TRIANGLES
!| IKLE2          |-->| GLOBAL NUMBER OF THE SECOND POINT OF TRIANGLES
!| IKLE3          |-->| GLOBAL NUMBER OF THE THIRD POINT OF TRIANGLES
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| X              |<->| AT THE BEGINNING : B
!|                |   | AT THE END       : THE RESULT
!| XA1            |<--| OFF-DIAGONAL TERMS OF THE LOWER PART OF MATRIX
!| XA2            |<--| OFF-DIAGONAL TERMS OF THE LOWER PART OF MATRIX
!| XA3            |<--| OFF-DIAGONAL TERMS OF THE LOWER PART OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: NPOIN,NELEM,NELMAX,LV
      DOUBLE PRECISION , INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION , INTENT(IN)    :: XA1(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA2(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA3(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE1(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE2(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE3(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IB
!
      INTRINSIC MIN
!
!-----------------------------------------------------------------------
!
! RESUMES INVERSIONS OF THE LOWER TRIANGULAR MATRICES
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
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA1(IELEM)*X(IKLE1(IELEM))
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA2(IELEM)*X(IKLE1(IELEM))
     &                                 -XA3(IELEM)*X(IKLE2(IELEM))
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
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA1(IELEM)*X(IKLE1(IELEM))
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA2(IELEM)*X(IKLE1(IELEM))
     &                                 -XA3(IELEM)*X(IKLE2(IELEM))
      ENDDO ! IELEM
      ENDDO ! IB
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
