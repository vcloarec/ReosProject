!                   *************************
                    SUBROUTINE P_ALLGATHERV_I
!                   *************************
!
     &(SEND_BUFFER, SEND_COUNT, RECV_BUFFER,
     & RECV_COUNT, DISPLS, IERR)
!
!***********************************************************************
! PARALLEL   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    Gathers data from all tasks and deliver the combined data to all tasks
!
!warning  THIS ROUTINE MUST BE CALLED BY ALL THE NODES/PROCESSORS,
!+            OTHERWISE THE PROGRAM WILL "HANG"
!
!history  AUDOUIN YOANN
!+        25/04/2013
!+
!+   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: SEND_BUFFER(*)
      INTEGER, INTENT(IN)    :: SEND_COUNT
      INTEGER, INTENT(INOUT)   :: RECV_BUFFER(*)
      INTEGER, INTENT(IN)    :: RECV_COUNT(*)
      INTEGER, INTENT(IN)    :: DISPLS(*)
      INTEGER, INTENT(OUT)   :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IERR = 0
      RECV_BUFFER(1) = 0
#if defined HAVE_MPI
      CALL MPI_ALLGATHERV(SEND_BUFFER, SEND_COUNT, MPI_INTEGER,
     &                   RECV_BUFFER, RECV_COUNT,
     &                   DISPLS, MPI_INTEGER, COMM, IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_ALLGATHERV_I: ERROR IN MPI_BARRIER'
        WRITE(LU,*) 'MPI ERROR ',IERR
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
      WRITE(LU,*) 'CALL OF P_ALLGATHERV_I '//
     &                         'IN ITS VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      END

