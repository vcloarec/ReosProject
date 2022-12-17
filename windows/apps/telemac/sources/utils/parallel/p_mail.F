!                   *****************
                    SUBROUTINE P_MAIL
!                   *****************
!
     &(CHAINE,NCAR)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PASSES A CHARACTER STRING (OF LENGTH NCAR)
!+                FROM THE MASTER TO THE SLAVES.
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
!history  J.-M. HERVOUET (LNHE)
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
!| CHAINE         |---| CHARACTER STRING
!| NCAR           |---| SIZE OF THE CHARACTER STRING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=PATH_LEN), INTENT(INOUT) :: CHAINE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
      CALL MPI_BCAST(CHAINE,NCAR,MPI_CHARACTER,0,COMM,IER)
!
      IF (IER.NE.0) THEN
        WRITE(LU,*) 'P_MAIL: PROBLEM IN MPI_BCAST'
        WRITE (LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
#else
      WRITE(LU,*) 'CALL OF P_MAIL IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
