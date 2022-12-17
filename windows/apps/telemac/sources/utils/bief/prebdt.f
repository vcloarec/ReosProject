!                   *****************
                    SUBROUTINE PREBDT
!                   *****************
!
     &(X,A,B,D,MESH,PREXSM,DIADON,S)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BLOCK-DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!+               (CAN BE MADE OF 4-MATRIX OR 9-MATRIX BLOCKS).
!code
!+    EXAMPLE FOR A BLOCK OF 4 :
!+
!+         (   A11   A12  ) ( X1 ) = ( B1 )
!+         (              ) (    )   (    )
!+         (              ) (    )   (    )
!+         (   A21   A22  ) ( X2 ) = ( B2 )
!
!history  J.M. HERVOUET (LNH)
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
!| A              |-->| BLOCK OF MATRICES
!| B              |-->| BLOCK OF RIGHT-HAND SIZES
!| D              |<--| BLOCK OF DIAGONALS
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X1,X2 AND B1,B2
!| S              |-->| 0 : A NORMAL SYSTEM       (FORBIDDEN HERE)
!|                |   | 1 : BLOCK OF ONE MATRIX   (FORBIDDEN HERE)
!|                |   | 2 : BLOCK OF 4 MATRICES
!|                |   | 3 : BLOCK OF 9 MATRICES
!| X              |<->| BLOCK OF UNKNOWN VECTORS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PREBDT => PREBDT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: S
!
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
!
!-----------------------------------------------------------------------
!
!  VECTOR OR VECTOR BLOCK STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B,D
!
!-----------------------------------------------------------------------
!
!  MATRIX OR MATRIX BLOCK STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  4-MATRIX BLOCK:
!
      IF(S.EQ.2) THEN
!
!  4-MATRIX BLOCK:
!
        CALL PREBD4(X%ADR(1)%P,X%ADR(2)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,A%ADR(4)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,D%ADR(3)%P,D%ADR(4)%P,
     &              MESH,PREXSM,DIADON)
!
      ELSEIF(S.EQ.3) THEN
!
!  9-MATRIX BLOCK:
!
        CALL PREBD9(X%ADR(1)%P,X%ADR(2)%P,X%ADR(3)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,
     &              A%ADR(4)%P,A%ADR(5)%P,A%ADR(6)%P,
     &              A%ADR(7)%P,A%ADR(8)%P,A%ADR(9)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,B%ADR(3)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,D%ADR(3)%P,
     &              D%ADR(4)%P,D%ADR(5)%P,D%ADR(6)%P,
     &              D%ADR(7)%P,D%ADR(8)%P,D%ADR(9)%P,
     &              MESH,PREXSM,DIADON)
!
      ELSE
!
!-----------------------------------------------------------------------
!
!  ERROR
!
        WRITE(LU,200) S
200     FORMAT(1X,'PREBDT (BIEF) : UNEXPECTED S :',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!                                                            -1
      RETURN
      END
