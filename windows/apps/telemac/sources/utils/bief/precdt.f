!                   *****************
                    SUBROUTINE PRECDT
!                   *****************
!
     &(X,A,B,D,MESH,PRECON,PREXSM,DIADON,S)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!+
!+           (CAN BE MADE OF A SIMPLE MATRIX, 4-MATRIX OR
!+                9-MATRIX BLOCKS).
!code
!+   IF DIADON=.TRUE. THE PRECONDITIONING DIAGONALS ARE GIVEN BY THE USER
!+
!+
!+   IF DIADON=.FALSE. :
!+
!+   IF PRECON IS A MULTIPLE OF 2 : DIAGONAL PRECONDITIONING USING
!+                                  THE DIAGONAL FROM A
!+   IF PRECON IS A MULTIPLE OF 3 : BLOCK-DIAGONAL PRECONDITIONING
!+                                  THIS PRECONDITIONING STARTS IN
!+                                  SUBROUTINE PREBDT, BUT ENDS WITH
!+                                  A STANDARD DIAGONAL PRECONDITIONING,
!+                                  PERFORMED HERE
!+   IF PRECON IS A MULTIPLE OF 5 : DIAGONAL PRECONDITIONING USING
!+                                  THE ABSOLUTE VALUE OF THE DIAGONAL
!+                                  FROM A
!
!warning  SHOULD NOT FORGET TO REVERT THE CHANGE OF VARIABLE
!+            AFTER RESOLUTION
!code
!+-----------------------------------------------------------------------
!+
!+   EXAMPLE FOR A 4-MATRIX BLOCK:
!+
!+        (  A11    A12  ) ( X1 ) = ( B1 )
!+        (              ) (    )   (    )
!+        (              ) (    )   (    )
!+        (  A21    A22  ) ( X2 ) = ( B2 )
!+
!+   THE DIAGONAL PRECONDITIONING MATRICES ARE D1 AND D2
!+
!+   THESE DIAGONALS ARE GIVEN BY THE USER IF DIADON=.TRUE. OR
!+   ARE THE DIAGONALS FROM A11, A22 IF PRECON IS A MULTIPLE OF 2
!+
!+                                   -1
!+   CHANGE OF VARIABLE X1PRIME =  D1    X1
!+                                   -1
!+                      X2PRIME =  D2    X2
!+
!+   THE WHOLE SYSTEM IS THEN MULTIPLIED BY D ON THE LEFT
!+
!+   PRODUCT:
!+
!+  ( D1   0  )       (  A11   A12  )     ( D1   0  )
!+  (         )       (             )     (         )
!+  (         )   X   (             )     (         )
!+  ( 0   D2  )       (  A21   A22  )  X  ( 0   D2  )
!+
!+
!+   WHICH GIVES :
!+
!+           (  D1  A11 D1       D1  A12 D2  )
!+           (                               )
!+           (                               )
!+           (  D2  A21 D1       D2  A22 D2  )
!+
!+
!+   BEWARE: ALL AIJ MATRICES ARE REMPLACED BY THEIR VALUE
!+           AFTER PRECONDITIONING
!+
!+   TREATS THE SECOND MEMBERS:
!+
!+   ( D1   0 )       ( B1 )
!+   (        )  X    (    )
!+   ( 0   D2 )       ( B2 )
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
!| A              |-->| BLOCK OF MATRICES
!| B              |-->| BLOCK OF RIGHT-HAND SIZES
!| D              |<--| BLOCK OF DIAGONALS
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PRECON         |-->| CHOICE OF PRECONDITIONING
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X1,X2 AND B1,B2
!| S              |-->| 0 : A NORMAL SYSTEM       (FORBIDDEN HERE)
!|                |   | 1 : BLOCK OF ONE MATRIX   (FORBIDDEN HERE)
!|                |   | 2 : BLOCK OF 4 MATRICES
!|                |   | 3 : BLOCK OF 9 MATRICES
!| X              |<->| BLOCK OF UNKNOWN VECTORS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PRECDT => PRECDT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: PRECON,S
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
!  STANDARD CASE
!
      IF(S.EQ.0) THEN
!
        CALL PRECD1(X,A,B,D%ADR(1)%P,MESH,PRECON,PREXSM,DIADON)
!
!-----------------------------------------------------------------------
!
!  CASE WHERE A X, B AND D ARE BLOCKS OF 1 OBJECT
!
      ELSEIF(S.EQ.1) THEN
!
        CALL PRECD1(X%ADR(1)%P,A%ADR(1)%P,B%ADR(1)%P,D%ADR(1)%P,MESH,
     &              PRECON,PREXSM,DIADON)
!
      ELSEIF(S.EQ.2) THEN
!
        CALL PRECD4(X%ADR(1)%P,X%ADR(2)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,A%ADR(4)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,
     &              MESH,PRECON,PREXSM,DIADON)
!
      ELSEIF(S.EQ.3) THEN
!
        CALL PRECD9(X%ADR(1)%P,X%ADR(2)%P,X%ADR(3)%P,
     &              A%ADR(1)%P,A%ADR(2)%P,A%ADR(3)%P,A%ADR(4)%P,
     &              A%ADR(5)%P,A%ADR(6)%P,A%ADR(7)%P,A%ADR(8)%P,
     &              A%ADR(9)%P,
     &              B%ADR(1)%P,B%ADR(2)%P,B%ADR(3)%P,
     &              D%ADR(1)%P,D%ADR(2)%P,D%ADR(3)%P,
     &              MESH,PRECON,PREXSM,DIADON)
!
      ELSE
!
!-----------------------------------------------------------------------
!
!  ERROR
!
        WRITE(LU,200) S
200     FORMAT(1X,'PRECDT (BIEF) : UNEXPECTED S :',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
