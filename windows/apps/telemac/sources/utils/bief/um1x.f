!                   ***************
                    SUBROUTINE UM1X
!                   ***************
!
     &(X,D,S)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    END OF THE OPERATIONS FOR DIAGONAL PRECONDITIONING
!+                WITH THE CONDENSED MATRIX  (OR ANY SIMILAR OPERATION).
!code
!+                   -1   PRIME
!+    OPERATION X = U    X
!+
!+    EXAMPLE OF A BLOCK OF 4:
!+
!+              (   I     D12  )
!+              (              )
!+         U =  (              )
!+              (              )
!+              (   0      I   )
!+                                   PRIME          PRIME         PRIME
!+              (   I    -D12  ) ( X1      )    ( X1    -   D12 X2 )
!+  -1  PRIME   (              ) (         )    (                  )
!+ U   X     =  (              ) (         )  = (                  )
!+              (              ) (   PRIME )    (                  )
!+              (   0      I   ) ( X2      )    ( X2               )
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
!| D              |<--| BLOCK OF DIAGONAL MATRICES
!| S              |-->| 2 : BLOCK WITH 4 MATRICES
!|                |   | 3 : BLOCK WITH 9 MATRICES
!| X              |<->| X AND X' (IN SITU TRANSFORMATION)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_UM1X => UM1X
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     STRUCTURES OF VECTORS OR BLOCKS OF VECTORS
!
      INTEGER, INTENT(IN)           :: S
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     BLOCKS OF 4 MATRICES:
!
      IF(S.EQ.2) THEN
!
!     BLOCKS OF 4 MATRICES:
!
        CALL UM1X04(X%ADR(1)%P,X%ADR(2)%P,D%ADR(3)%P)
!
      ELSEIF(S.EQ.3) THEN
!
!     BLOCKS OF 9 MATRICES:
!
        CALL UM1X09(X%ADR(1)%P,X%ADR(2)%P,X%ADR(3)%P,
     &              D%ADR(4)%P,D%ADR(5)%P,D%ADR(7)%P)
!
      ELSE
!
!-----------------------------------------------------------------------
!
!  ERROR
!
        WRITE(LU,200) S
200     FORMAT(1X,'UM1X (BIEF) : UNEXPECTED S :',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!                                                            -1
      RETURN
      END
