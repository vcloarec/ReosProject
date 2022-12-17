!                   ************************
                    SUBROUTINE BIEF_DEALLVEC
!                   ************************
!
     &(VEC)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A VECTOR STRUCTURE.
!
!history  Y AUDOUIN (LNHE)
!+        23/05/2013
!+        V7P1
!+    First version
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        21/01/2016
!+        V7P2
!+    Adding VEC%NAT=3, combining double precision and integers.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| VEC            |<--| VECTOR TO BE DEALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VEC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(VEC%NAT.EQ.1) THEN
        IF(ASSOCIATED(VEC%R)) DEALLOCATE(VEC%R)
      ELSEIF(VEC%NAT.EQ.2) THEN
        IF(ASSOCIATED(VEC%I)) DEALLOCATE(VEC%I)
      ELSEIF(VEC%NAT.EQ.3) THEN
        IF(ASSOCIATED(VEC%R)) DEALLOCATE(VEC%R)
        IF(ASSOCIATED(VEC%I)) DEALLOCATE(VEC%I)
      ELSE
        WRITE(LU,*) 'UNKNOWN NAT IN BIEF_DEALLVEC FOR :', VEC%NAME
        CALL PLANTE(1)
        STOP
      ENDIF
      NULLIFY(VEC%R)
      NULLIFY(VEC%I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BIEF_DEALLVEC
