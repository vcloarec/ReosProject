!                   *****************
                    SUBROUTINE UM1X09
!                   *****************
!
     &(X1,X2,X3,D12,D13,D23)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SEE UM1X.
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
!| D12            |-->| DIAGONAL
!| D13            |-->| DIAGONAL
!| D23            |-->| DIAGONAL
!| X1             |<->| FIRST UNKNOWN
!| X2             |<->| SECOND UNKNOWN
!| X3             |-->| THIRD UNKNOWN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_UM1X09 => UM1X09
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2
      TYPE(BIEF_OBJ), INTENT(IN)    :: X3,D12,D13,D23
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL OS( 'X=X-YZ  ' , X=X2 , Y=X3 , Z=D23 )
      CALL OS( 'X=X-YZ  ' , X=X1 , Y=X2 , Z=D12 )
      CALL OS( 'X=X-YZ  ' , X=X1 , Y=X3 , Z=D13 )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
