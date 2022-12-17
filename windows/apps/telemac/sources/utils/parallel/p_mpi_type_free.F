!                   **************************
                    SUBROUTINE P_MPI_TYPE_FREE
!                   **************************
!
     &(DATATYPE,IERR)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_TYPE_FREE.
!
!history  C. DENIS (SINETICS)
!+        27/10/2009
!+        V6P0
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
!| DATATYPE         |-->| DATATYPE
!| IERR             |<--| ERROR VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)  :: DATATYPE
      INTEGER, INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IERR = 0
#if defined HAVE_MPI
      CALL MPI_TYPE_FREE(DATATYPE,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_MPI_TYPE_FREE:'
        WRITE(LU,*) 'MPI ERROR ',IERR
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      WRITE(LU,*) 'CALL OF P_MPI_TYPE_FREE VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
