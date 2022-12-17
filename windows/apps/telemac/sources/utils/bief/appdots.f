!                   ******************
                    SUBROUTINE APPDOTS
!                   ******************
!
     &( X , MESH )
!
!***********************************************************************
! BIEF   V6P2                                   11/05/2012
!***********************************************************************
!
!brief    Assemble and Prints P_DOTS of a vector which is not assembled.
!         This is for debugging parallelism and applies e.g. to
!         diagonals and right-hand sides of linear systems
!         P_DOTS is the dot product of a vector in parallel.
!         Only the master processor (IPID=0) prints the result
!
!history  J-M HERVOUET (LNHE)
!+        11/05/2012
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| X              |-->| THE VECTOR
!| MESH           |-->| THE MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN) :: X
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION RESULT
!
!-----------------------------------------------------------------------
!
      CALL OS('X=Y     ',X=MESH%T,Y=X)
!
      IF(NCSIZE.GT.1) CALL PARCOM(MESH%T,2,MESH)
!
      RESULT=P_DOTS(MESH%T,MESH%T,MESH)
!
      IF(IPID.EQ.0) THEN
        WRITE(LU,*) X%NAME,' DOTS=',RESULT
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
