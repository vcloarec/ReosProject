!                   ***********************
                    SUBROUTINE P_IMAX_ARRAY
!                   ***********************
!
     &(SEND_BUFFER, RECV_BUFFER, NCOUNT, IERR)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MAX VALUES FROM ALL PROCESSES AND DISTRIBUTES THE RESULT BACK TO ALL PROCESSES
!
!history  AUDOUIN YOANN
!+        25/04/2013
!+
!+   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MYPART         |-->| SEND BUFFER.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NCOUNT
      INTEGER, INTENT(IN)  :: SEND_BUFFER(NCOUNT)
      INTEGER, INTENT(OUT) :: RECV_BUFFER(NCOUNT)
      INTEGER, INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IERR = 0
      RECV_BUFFER = 0
#if defined HAVE_MPI
      CALL MPI_ALLREDUCE(SEND_BUFFER,RECV_BUFFER,NCOUNT,MPI_INTEGER,
     &                   MPI_MAX,COMM,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_IMAX_ARRAY: ERROR IN MPI_ALLREDUCE'
        WRITE(LU,*) 'MPI ERROR ',IERR
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
      WRITE(LU,*)'CALL OF P_IMAX_ARRAY IN ITS VOID VERSION'
!
!-----------------------------------------------------------------------
!
#endif
!
!-----------------------------------------------------------------------
!
      END


