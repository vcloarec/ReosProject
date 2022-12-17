!                   ***********************
                    INTEGER FUNCTION P_IMAX
!                   ***********************
!
     &(MYPART)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MAXIMUM VALUE FROM ALL THE PROCESSORS.
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
!+        10/06/2005
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
!| MYPART         |-->| SEND BUFFER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MYPART
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!
!
      CALL MPI_ALLREDUCE(MYPART,P_IMAX,1,MPI_INTEGER,MPI_MAX,
     &                   COMM,IER)
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_IMAX: ERROR IN MPI_ALLREDUCE'
        WRITE(LU,*) 'MPI ERROR ', IER
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
      WRITE(LU,*) 'CALL OF P_IMAX IN ITS VOID VERSION'
!
!----------------------------------------------------------------------
!
      P_IMAX=MYPART
!
#endif
!
!-----------------------------------------------------------------------
!
      END
