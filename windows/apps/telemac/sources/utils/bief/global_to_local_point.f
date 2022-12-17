!                   **************************************
                    INTEGER FUNCTION GLOBAL_TO_LOCAL_POINT
!                   **************************************
!
     &(IPOIN,MESH)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    In parallel, returns the local value of a point given in
!+    global value. 0 is returned if point not in subdomain. In scalar
!+    mode returns IPOIN. This function replaces array MESH%KNOGL%I
!
!warning   Naive programming with a mere loop, will be optimised
!+    in further versions. Only linear points are considered.
!
!history  J-M HERVOUET (LNHE)
!+        30/07/2012
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IPOIN          |-->| THE POINT NUMBER IN ORIGINAL SCALAR MESH
!| MESH           |-->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GLOBAL_TO_LOCAL_POINT => GLOBAL_TO_LOCAL_POINT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: IPOIN
      TYPE(BIEF_MESH) , INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        GLOBAL_TO_LOCAL_POINT=0
        DO I=1,MESH%NPOIN
          IF(MESH%KNOLG%I(I).EQ.IPOIN) THEN
            GLOBAL_TO_LOCAL_POINT=I
          ENDIF
        ENDDO
      ELSE
        GLOBAL_TO_LOCAL_POINT=IPOIN
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
