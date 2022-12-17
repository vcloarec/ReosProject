!                   ****************
                    SUBROUTINE PUOG1
!                   ****************
!
     &(X, A,B ,DITR,MESH,COPY)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE VECTOR X = U B     (ELEMENT BY ELEMENT).
!code
!+            MATRIX L IS HERE THE RESULT OF A DECOMPOSITION
!+            PERFORMED IN SUBROUTINE DECLDU.
!+
!+            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM:
!+
!+            LE X DE X UE
!+
!+            LE : LOWER TRIANGULAR WITH 1 ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1 ON THE DIAGONAL
!+
!+                                                T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!
!code
!+-----------------------------------------------------------------------
!+  MEANING OF IELM :
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : P1 TRIANGLE            3                       YES
!+  12 : QUASI-BUBBLE TRIANGLE  4                       YES
!+  21 : Q1 QUADRILATERAL       4                       YES
!+  41 : TELEMAC-3D PRISMS      6                       YES
!+
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (LNH)
!+        23/12/94
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
!| A              |<--| MATRIX IN LDU FORM
!| B              |<--| RIGHT-HAND SIDE OF THE LINEAR SYSTEM TO BE SOLVED
!| COPY           |-->| IF .TRUE. B IS COPIED INTO X TO START WITH
!| DITR           |-->| CHARACTER, IF  'D' : DIRECT MATRIX A CONSIDERED
!|                |   |                'T' : TRANSPOSED MATRIX A CONSIDERED
!| MESH           |-->| MESH STRUCTURE
!| X              |<--| SOLUTION OF THE SYSTEM AX = B
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PUOG1 => PUOG1
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=1), INTENT(IN) :: DITR
!
      LOGICAL, INTENT(IN) :: COPY
!
!-----------------------------------------------------------------------
!
!  VECTORS STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURE
!
      TYPE(BIEF_OBJ), INTENT(IN) :: A
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM,NPOIN,NELEM,NELMAX
      CHARACTER(LEN=1) :: TYPX
!
!-----------------------------------------------------------------------
!
      TYPX  = A%TYPEXT
      NPOIN = A%D%DIM1
      IELM  = A%ELMLIN
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
      CALL CPSTVC(B,X)
!
!-----------------------------------------------------------------------
!
! 1) DESCENT WITH RECOPY OF B IN X
!
      CALL TNOMER(X%R,A%X%R,TYPX,
     &     B%R,MESH%IKLE%I,NELEM,NELMAX,NPOIN,IELM,DITR,COPY,MESH%LV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
