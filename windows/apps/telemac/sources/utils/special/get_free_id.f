!                   **********************
                    SUBROUTINE GET_FREE_ID
!                   **********************
!
     &(ID)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    Returns an unused id to open a file
!
!history  Y AUDOUIN (LNHE)
!+        09/09/2016
!+        V7P2
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ID             |-->| ID
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL, ONLY : MAX_ID, LU
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

      INTEGER, INTENT(INOUT) :: ID
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I
      LOGICAL :: IS_USED
!
!
!-----------------------------------------------------------------------
!
      I=MAX_ID
      DO
        IF(I.EQ.5.OR.I.EQ.6) THEN
          I = I + 1
          CONTINUE
        ENDIF
        INQUIRE(I,OPENED=IS_USED)
        IF(.NOT.IS_USED) THEN
          ID = I
          MAX_ID = I
          RETURN
        ENDIF
        I = I+1
      ENDDO
      WRITE(LU,*) "Ay Caramba !! Should not happen"
      CALL PLANTE(1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
