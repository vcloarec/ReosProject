!                   *****************
                    SUBROUTINE OUTBIEF
!                   *****************
     &(MESH)
!
!***********************************************************************
! BIEF   V7P0
!***********************************************************************
!
!brief    CLEAN UP THE DATA FROM MESH
!
!history Y AUDOUIN (LNHE)
!+       21/05/2015
!+       V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| THE MESH TO BE DEALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(MODASS.EQ.2) THEN
        DEALLOCATE(MESH%WI8)
        DEALLOCATE(MESH%TI8)
      ENDIF
!
!     THESE STUCTURES ARE ALLOCATED IN PARINI
      CALL BIEF_DEALLOBJ(MESH%NB_NEIGHB_PT    )
      CALL BIEF_DEALLOBJ(MESH%LIST_SEND       )
      CALL BIEF_DEALLOBJ(MESH%NH_COM          )
      CALL BIEF_DEALLOBJ(MESH%NB_NEIGHB_PT_SEG)
      CALL BIEF_DEALLOBJ(MESH%LIST_SEND_SEG   )
      CALL BIEF_DEALLOBJ(MESH%NH_COM_SEG      )
      CALL BIEF_DEALLOBJ(MESH%BUF_SEND        )
      CALL BIEF_DEALLOBJ(MESH%BUF_RECV        )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
