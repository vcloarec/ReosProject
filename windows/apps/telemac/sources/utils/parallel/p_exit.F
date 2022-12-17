!                   *****************
                    SUBROUTINE P_EXIT
!                   *****************
!
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    END OF MPI.
!
!history  RAINER JOHANNI (SGI MUNICH)
!+        **/10/1999
!+
!+   ADAPTED FOR MPI
!
!history  J.A. JANKOWSKI (BAW KARLSRUHE)
!+        28/12/1999
!+
!+   RELEASE 5.0 MODIFIED
!
!history  J-M HERVOUET (LNHE)
!+        16/06/2009
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) ' '
      WRITE(LU,*) 'EXITING MPI'
      WRITE(LU,*) ' '
!
!     AVOIDS EXITING BEFORE EVERYTHING IS DONE IN OTHER PROCESSORS
!
      CALL MPI_BARRIER(COMM,IER)
!
!     EXITS
!
#  ifdef COMPAD
      CALL AMPI_FINALIZE(IER)
#  else
      CALL MPI_FINALIZE(IER)
#  endif
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_EXIT: ERROR IN MPI_FINALIZE'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      WRITE(LU,*) 'CALL OF P_EXIT IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
