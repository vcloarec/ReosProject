!                   ***************
                    SUBROUTINE PUOG
!                   ***************
!
     &(X, A,B ,DITR,MESH,COPY)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE VECTOR X = U B     (ELEMENT BY ELEMENT).
!+
!+            REVERSE OF WHAT GOUP DOES, HENCE THE NAME.
!code
!+            MATRIX U IS HERE THE RESULT OF A DECOMPOSITION
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
      USE BIEF, EX_PUOG => PUOG
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: B
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
!     CASE WHERE THE SYSTEM IS A BLOCK BUT ONLY ONE PRECONDITIONING
!     MATRIX IS USED
!
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        WRITE(LU,400) A%TYPE
400     FORMAT(1X,'PUOG (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(S.EQ.0.AND.SA.EQ.0) THEN
!
!     CASE WHERE A IS A SIMPLE MATRIX AND X A SIMPLE VECTOR
!
        CALL PUOG1(X, A,B ,DITR,MESH,COPY)
!
      ELSEIF(S.GT.0.AND.S.EQ.SA) THEN
!
!     CASE WHERE THE BLOCK A ONLY CONTAINS DIAGONALS
!
        DO I=1,S
          CALL PUOG1( X%ADR(I)%P,
     &                A%ADR(I)%P,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
        ENDDO ! I
!
      ELSEIF(S.GT.0.AND.S**2.EQ.SA) THEN
!
!     CASE WHERE THE BLOCK A CONTAINS AS MANY MATRICES AS THERE ARE
!     IN THE WHOLE SYSTEM: ONLY TAKES THE DIAGONALS
!
        DO I=1,S
          CALL PUOG1(X%ADR(I)%P,
     &                A%ADR(1+(S+1)*(I-1))%P,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
        ENDDO ! I
!
!     CASE WHERE A IS A SINGLE MATRIX AND X A BLOCK
!
      ELSEIF(S.GT.0.AND.SA.EQ.0) THEN
!
        DO I=1,S
          CALL PUOG1( X%ADR(I)%P,
     &                A,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
        ENDDO ! I
!
      ELSE
        WRITE(LU,401)
401     FORMAT(1X,'PUOG (BIEF) : UNEXPECTED CASE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
