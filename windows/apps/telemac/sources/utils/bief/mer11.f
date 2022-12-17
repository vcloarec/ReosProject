!                   ****************
                    SUBROUTINE MER11
!                   ****************
!
     &(X, XA1,XA2,XA3,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,NPOIN,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRODUCT X = U B (BEWARE: ELEMENT BY ELEMENT).
!+
!+            HERE: P1 ELEMENT OR ELEMENT WITH 3 POINTS.
!+
!+            REVERSE OPERATION FROM THAT IN SUBROUTINE REMONT,
!+                HENCE THE NAME.
!code
!+        THE MATRIX U IS HERE THE RESULT OF A DECOMPOSITION
!+        DONE IN SUBROUTINE DECLDU.
!+
!+        EACH ELEMENTARY MATRIX HAS BEEN FACTORISED IN THE FORM :
!+
!+        LE X DE X UE
!+
!+        LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+        DE : DIAGONAL
!+        UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!+
!+                                            T
!+        IF THE MATRIX IS SYMMETRICAL : LE =  UE
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
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
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| X              |<--| RESULTING VECTOR
!| XA1            |-->| OFF-DIAGONAL TERM
!| XA2            |-->| OFF-DIAGONAL TERM
!| XA3            |-->| OFF-DIAGONAL TERM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NELEM,NELMAX,LV
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XA1(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XA2(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XA3(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IB
!
      INTRINSIC MIN
!
!-----------------------------------------------------------------------
!
! RESUMES INVERSION OF THE LOWER TRIANGULAR MATRICES
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
          X(IKLE1(IELEM))=X(IKLE1(IELEM))+XA1(IELEM)*X(IKLE2(IELEM))
     &                                   +XA2(IELEM)*X(IKLE3(IELEM))
          X(IKLE2(IELEM))=X(IKLE2(IELEM))+XA3(IELEM)*X(IKLE3(IELEM))
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
          X(IKLE1(IELEM))=X(IKLE1(IELEM))+XA1(IELEM)*X(IKLE2(IELEM))
     &                                   +XA2(IELEM)*X(IKLE3(IELEM))
          X(IKLE2(IELEM))=X(IKLE2(IELEM))+XA3(IELEM)*X(IKLE3(IELEM))
      ENDDO ! IELEM
      ENDDO ! IB
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
