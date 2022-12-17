!                   ************************
                    SUBROUTINE BIEF_DEALLOBJ
!                   ************************
!
     &(OBJ)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    DEALLOCATES MEMORY FOR A BIEF_OBJ STRUCTURE.
!
!history  Y AUDOUIN (LNHE)
!+        25/05/2013
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| OBJ            |<->| THE OBJECT TO BE DEALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_DEALLOBJ => BIEF_DEALLOBJ
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ! VECTOR
      IF(OBJ%TYPE.EQ.2) THEN
        CALL BIEF_DEALLVEC(OBJ)
      ! MATRICE
      ELSEIF(OBJ%TYPE.EQ.3) THEN
        CALL BIEF_DEALLMAT(OBJ)
      ! BLOCK
      ELSEIF(OBJ%TYPE.EQ.4) THEN
        CALL DEALLBLO(OBJ)
      ELSE
        WRITE(LU,*) 'UNKNOWN BIEF_OBJ TYPE'
        WRITE(LU,*) 'FOR OBJECT NAMED: ',OBJ%NAME
        WRITE(LU,*) 'OF TYPE: ',OBJ%TYPE
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

