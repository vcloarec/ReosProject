!                   *****************
                    SUBROUTINE DEALLBLO
!                   *****************
!
     &( BLO )
!
!***********************************************************************
! BIEF   V6P3
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A BLOCK STRUCTURE.
!
!history  Y AUDOUIN (LNHE)
!+        25/05/2015
!+        V7P0
!+
!
!history  S.E.BOURBAN (HRW)
!+        20/01/2017
!+        V7P4
!+        DEALLOCATION of a pointer switched to a NULLIFY statement
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BLO            |-->| THE BLOCK TO BE DEALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
      ! If the block contains nothing ... do nothing
      IF(BLO%N.GT.0) THEN
        DO I=1,BLO%N
          IF(ASSOCIATED(BLO%ADR(I)%P)) THEN
            IF(BLO%ADR(I)%P%FATHER.EQ.BLO%NAME) THEN
              IF(BLO%ADR(I)%P%TYPE.EQ.4) THEN
                ! Not deallocating element in the block as they should
                ! point on block from a higher level that will be
                ! deallocatted later
                DO J=1,BLO%ADR(I)%P%N
                  NULLIFY(BLO%ADR(I)%P%ADR(J)%P)
                ENDDO
                DEALLOCATE(BLO%ADR(I)%P%ADR)
                NULLIFY(BLO%ADR(I)%P%ADR)
              ELSE IF(BLO%ADR(I)%P%TYPE.EQ.2) THEN
                CALL BIEF_DEALLVEC(BLO%ADR(I)%P)
              ELSE IF(BLO%ADR(I)%P%TYPE.EQ.3) THEN
                CALL BIEF_DEALLMAT(BLO%ADR(I)%P)
              ELSE
                WRITE(LU,*) 'UNKNOWN BIEF_OBJ TYPE'
                WRITE(LU,*) 'FOR OBJECT NAMED: ',BLO%ADR(I)%P%NAME
                WRITE(LU,*) 'OF TYPE: ',BLO%ADR(I)%P%TYPE
                CALL PLANTE(1)
                STOP
              ENDIF
              DEALLOCATE(BLO%ADR(I)%P)
              NULLIFY(BLO%ADR(I)%P)
            ENDIF
          NULLIFY(BLO%ADR(I)%P)
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE(BLO%ADR)
      NULLIFY(BLO%ADR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
