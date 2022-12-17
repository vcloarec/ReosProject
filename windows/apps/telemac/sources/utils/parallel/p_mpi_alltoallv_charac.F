!                   *********************************
                    SUBROUTINE P_MPI_ALLTOALLV_CHARAC
!                   *********************************
!
     &(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,RECV_BUFFER,NRECV,
     & RECV_DISPL,RECV_DATYP,IERR)
!
!***********************************************************************
! PARALLEL   V6P2                                  21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ALLTOALLV.
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
!| SEND_BUFFER    |-->| SEND BUFFER
!| NSEND          |-->| SPECIFIES THE NUMBER OF ELEMENTS TO SEND TO EACH
!|                |   | PROCESSOR
!| SEND_DISPL     |-->| DISPLACEMENT ARRAY FOR THE SEND BUFFER
!| SEND_DATYP     |-->| DATA TYPE OF SEND BUFFER ELEMENTS
!| RECV_BUFFER    |<--| RECEIVE BUFFER
!| NRECV          |-->| SPECIFIES THE MAXIMUM NUMBER OF ELEMENTS THAT
!|                |   | CAN BE RECEIVED FROM EACH PROCESSOR
!| RECV_DISPL     |-->| DISPLACEMENT ARRAY FOR THE RECEIVE BUFFER
!| RECV_DATYP     |-->| DATA TYPE OF RECEIVE BUFFER ELEMENTS
!| COMM           |-->| COMMUNICATOR
!| IERR           |-->| ERROR VALUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)  :: NSEND(*),SEND_DISPL(*),SEND_DATYP,NRECV(*)
      INTEGER, INTENT(IN)  :: RECV_DISPL(*),RECV_DATYP
      INTEGER, INTENT(OUT) :: IERR
      TYPE(CHARAC_TYPE), INTENT(IN)  :: SEND_BUFFER(*)
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      TYPE(CHARAC_TYPE), INTENT(INOUT) :: RECV_BUFFER(*)
!      TYPE(CHARAC_TYPE), INTENT(OUT) :: RECV_BUFFER(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IERR = 0
      RECV_BUFFER(1)%MYPID = 0
!
#if defined HAVE_MPI
!     TERMINATE ON MPI PARTS NOT HANDLED BY COMPAD
#  if defined COMPAD
      WRITE(LU,*) 'P_MPI_ALLTOALLV: COMPAD CRITICAL MPI OPERATION'
      WRITE(LU,*) '  PLEASE CONTACT JR @ ADJOINTWARE'
      CALL PLANTE(1)
      STOP
#  endif
      CALL MPI_ALLTOALLV(SEND_BUFFER,NSEND,SEND_DISPL,SEND_DATYP,
     &                   RECV_BUFFER,NRECV,RECV_DISPL,RECV_DATYP,
     &                   COMM,IERR)
!
      IF(IERR.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ALLTOALLV:'
        WRITE(LU,*) 'MPI ERROR ',IERR
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
      RECV_BUFFER(1)%MYPID=0
      WRITE(LU,*) 'CALL OF P_MPI_ALLTOALLV_CHARAC VOID VERSION'
#endif
!
!-----------------------------------------------------------------------
!
      END
