!                   *****************
                    SUBROUTINE DWNUP1
!                   *****************
!
     &(X, A,B ,DITR,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE SYSTEM A X = B.
!+
!+            THE MATRIX A IS HERE THE RESULT OF A DECOMPOSITION
!+                DONE IN SUBROUTINE DECLDU.
!code
!+            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM :
!+
!+            LE X DE X UE
!+
!+            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!+
!+                                                T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!+
!+            "DE" MATRICES ARE CONSIDERED LIKE DIAGONALS OF SIZE
!+            NPOIN X NPOIN, WHICH ARE FILLED WITH 1S FOR THE POINTS
!+            WHICH DO NOT BELONG TO THE CONSIDERED ELEMENT
!+
!+            THEN PERFORMS THE PRODUCT OF ALL THESE DIAGONALS
!+            YIELDING DIAGONAL DB
!+
!+
!+ !!!!!!!!!  FINALLY: DB HAS BEEN INVERTED BECAUSE THAT'S HOW
!+                     IT IS USED IN THIS SUBROUTINE
!+
!+            MATRIX A IS HERE :
!+
!+            THE PRODUCT FROM 1 TO NELEM OF ALL THE MATRICES: LE
!+
!+            MULTIPLIED BY :
!+
!+            THE DIAGONAL: DB
!+
!+            MULTIPLIED BY :
!+
!+            THE PRODUCT FROM NELEM TO 1 OF ALL THE MATRICES: UE
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
!+        26/02/04
!+        V5P5
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
!| A              |-->| MATRIX A IN LDU FORM
!| B              |<--| RIGHT-HAND SIDE OF THE SYSTEM
!| DITR           |-->| OPTION  'D' : MATRIX A IS TAKEN
!|                |   |         'T' : MATRIX TRANSPOSED(A)
!| MESH           |-->| MESH STRUCTURE
!| X              |<--| SOLUTION OF SYSTEM AX = B
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DWNUP1 => DWNUP1
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: B
      TYPE(BIEF_OBJ), INTENT(IN)    :: A
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      CHARACTER(LEN=1), INTENT(IN)  :: DITR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM,NPOIN,NELEM,NELMAX
!
      CHARACTER(LEN=1) TYPD,TYPX
!
!-----------------------------------------------------------------------
!
      TYPD  = A%TYPDIA
      TYPX  = A%TYPEXT
      NPOIN = A%D%DIM1
      IELM  = A%ELMLIN
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
      CALL CPSTVC(B,X)
!
!-----------------------------------------------------------------------
!
! 1) DESCENT WITH COPY OF B IN X
!
      IF(A%STO.EQ.1) THEN
        CALL DESCEN(X%R, A%X%R,TYPX,B%R,
     &       MESH%IKLE%I,NELEM,NELMAX,NPOIN,IELM,DITR,.TRUE.,MESH%LV)
      ELSEIF(A%STO.EQ.3) THEN
        CALL DESSEG(X%R, A%X%R,TYPX,B%R,
     &              MESH%GLOSEG%I,MESH%NSEG,NPOIN,DITR,.TRUE.)
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2) RESUMES INVERSIONS OF DIAGONAL MATRICES
!
      IF(TYPD(1:1).NE.'I') THEN
        CALL OV('X=XY    ', X=X%R, Y=A%D%R, DIM1=NPOIN)
      ENDIF
!
!-----------------------------------------------------------------------
!
! 3) TRACES BACK WITHOUT PRELIMINARY COPY OF B IN X
!
      IF(A%STO.EQ.1) THEN
        CALL REMONT(X%R, A%X%R,TYPX,B%R,
     &       MESH%IKLE%I,NELEM,NELMAX,NPOIN,IELM,DITR,.FALSE.,MESH%LV)
      ELSE
        CALL REMSEG(X%R, A%X%R,TYPX,B%R,
     &              MESH%GLOSEG%I,MESH%NSEG,NPOIN,DITR,.FALSE.)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
