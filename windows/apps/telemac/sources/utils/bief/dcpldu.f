!                   *****************
                    SUBROUTINE DCPLDU
!                   *****************
!
     &(B,A,MESH,COPY,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    L D U FACTORISATION OF THE ELEMENTARY MATRICES
!+                IN MATRIX A.
!+
!+           (A CAN ALSO BE A BLOCK OF MATRICES, IN WHICH CASE
!+                THE DIAGONAL MATRICES OF THE BLOCK ARE TREATED).
!+
!+            REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY.
!code
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
!+
!+            "DE" MATRICES ARE CONSIDERED LIKE DIAGONALS OF SIZE
!+            NPOIN X NPOIN, WHICH ARE FILLED WITH 1S FOR THE POINTS
!+            WHICH DO NOT BELONG TO THE CONSIDERED ELEMENT
!+
!+            THEN PERFORMS THE PRODUCT OF ALL THESE DIAGONALS
!+            YIELDING DIAGONAL DB
!+
!+   ||||||   FINALLY: DB IS INVERTED BECAUSE THAT'S HOW IT WILL BE
!+                     USED IN DOWNUP.
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
!| A              |<--| MATRIX A.
!| B              |<--| MATRICE B, THE RESULT.
!| COPY           |-->| IF .TRUE. A IS COPIED INTO B.
!|                |   | IF .FALSE. B IS CONSIDERED ALREADY INITIALISED
!| LV             |-->| VECTOR LENGTH OF THE COMPUTER
!| MESH           |-->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DCPLDU => DCPLDU
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: B
      TYPE(BIEF_OBJ) , INTENT(IN)    :: A
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL        , INTENT(IN)    :: COPY
      INTEGER        , INTENT(IN)    :: LV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER SA,SB,I
!
!-----------------------------------------------------------------------
!
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        WRITE(LU,400) A%TYPE
400     FORMAT(1X,'DCPLDU (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(B%TYPE.EQ.3) THEN
        SB = 0
      ELSEIF(B%TYPE.EQ.4) THEN
        SB = B%N
      ELSE
        WRITE(LU,401) B%TYPE
401     FORMAT(1X,'DCPLDU (BIEF) :',1I6,' UNEXPECTED TYPE FOR B.')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(SA.EQ.0.AND.SB.EQ.0) THEN
!
        CALL DECLDU(B,A,MESH,COPY,LV)
!
      ELSEIF(SB.GT.0.AND.SA.GT.0) THEN
!
!       TAKES THE DIAGONALS OF BLOCK A
!
        DO I=1,SB
          CALL DECLDU(B%ADR(I)%P,A%ADR(1+(SB+1)*(I-1))%P,
     &                MESH,COPY,LV)
        ENDDO
!
      ELSEIF(SA.NE.0.AND.SB.EQ.0) THEN
!
!       TAKES THE FIRST DIAGONAL OF BLOCK A
!
        CALL DECLDU(B,A%ADR(1)%P,MESH,COPY,LV)
!
      ELSE
!
        WRITE(LU,402)
402     FORMAT(1X,'DCPLDU (BIEF) : UNEXPECTED CASE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
