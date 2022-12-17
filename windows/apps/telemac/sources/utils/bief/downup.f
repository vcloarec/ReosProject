!                   *****************
                    SUBROUTINE DOWNUP
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
!| A              |-->| MATRIX A
!| B              |<--| RIGHT-HAND SIDE OF THE SYSTEM
!| DITR           |-->| OPTION  'D' : MATRIX A IS TAKEN
!|                |   |         'T' : MATRIX TRANSPOSED(A)
!| MESH           |-->| MESH STRUCTURE
!| X              |<--| SOLUTION OF SYSTEM AX = B
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DOXNUP => DOWNUP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: A,B
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      CHARACTER(LEN=1), INTENT(IN)  :: DITR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER S,SA,I
!
!-----------------------------------------------------------------------
!
      IF(X%TYPE.EQ.4) THEN
        S = X%N
      ELSE
        S = 0
      ENDIF
!
!     COVERS THE CASE WHERE THE SYSTEM IS A BLOCK BUT WHERE ONLY ONE OF
!     PRECONDITIONING MATRICES IS USED
!
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        WRITE(LU,400) A%TYPE
400     FORMAT(1X,'DOWNUP (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(S.EQ.0.AND.SA.EQ.0) THEN
!
!     CASE WHERE A IS A SIMPLE MATRIX AND X A SIMPLE VECTOR
!
        CALL DWNUP1(X, A,B ,DITR,MESH)
!
      ELSEIF(S.GT.0.AND.S.EQ.SA) THEN
!
!     CASE WHERE BLOCK A ONLY CONTAINS THE DIAGONALS
!
        DO I=1,S
          CALL DWNUP1(X%ADR(I)%P,
     &                A%ADR(I)%P,
     &                B%ADR(I)%P,
     &                DITR,MESH)
        ENDDO ! I
!
      ELSEIF(S.GT.0.AND.S**2.EQ.SA) THEN
!
!     CASE WHERE BLOCK A CONTAINS AS MANY MATRICES AS THERE ARE IN
!     THE COMPLETE SYSTEM: ONLY CONSIDERS THE DIAGONALS
!
        DO I=1,S
          CALL DWNUP1(X%ADR(I)%P,
     &                A%ADR(1+(S+1)*(I-1))%P,
     &                B%ADR(I)%P,
     &                DITR,MESH)
        ENDDO ! I
!
!     CASE WHERE A IS A SINGLE MATRIX AND X IS A BLOCK
!
      ELSEIF(S.GT.0.AND.SA.EQ.0) THEN
!
        DO I=1,S
          CALL DWNUP1(X%ADR(I)%P,
     &                A,
     &                B%ADR(I)%P,
     &                DITR,MESH)
        ENDDO ! I
!
      ELSE
        WRITE(LU,401)
401     FORMAT(1X,'DOWNUP (BIEF) : UNEXPECTED CASE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
