!                   ************************
                    SUBROUTINE P_WAIT_PARACO
!                   ************************
!
     &(IBUF,NB)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    WAITS AT THE END OF PARACO.
!
!history  PASCAL VEZOLLE (IBM)
!+        23/06/2008
!+        V5P9
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
!| IBUF           |--->| ARRAY OF REQUESTS
!| NB             |--->| LISTS LENGTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(INOUT) :: IBUF(*)
      INTEGER,INTENT(IN)    :: NB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
      IF(NB.GT.100) THEN
        WRITE(LU,*) 'WAIT_PARACO:'
        WRITE(LU,*) 'DIMENSION OF WAIT_REQ TOO SMALL'
        CALL PLANTE(1)
        STOP
      ENDIF
!
#  if defined COMPAD
      CALL AMPI_WAITALL(NB,IBUF,WAIT_REQ_PARACO,IER)
#  else
      CALL MPI_WAITALL(NB,IBUF,WAIT_REQ_PARACO,IER)
#  endif
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'WAIT_PARACO:'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      WRITE(LU,*) 'CALL OF P_WAIT_PARACO IN VOID VERSION'
#endif
!
!----------------------------------------------------------------------
!
      RETURN
      END
