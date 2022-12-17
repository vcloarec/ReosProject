!                   ***********************************
                    DOUBLE PRECISION FUNCTION P_DSUMERR
!                   ***********************************
!
     &(PARTIAL)
!
!***********************************************************************
! PARALLEL   V7P3                                  24/02/2016
!***********************************************************************
!
!brief    SUM AND ERROR ROUNDING OF VALUES FROM ALL THE PROCESSORS.
!
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| PARTIAL         |-->| SEND BUFFER (SUM_VALUE, ERROR VALUE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: PARTIAL(2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined (HAVE_MPI)
      INTEGER IER,NUM_PROCS
      DOUBLE PRECISION,ALLOCATABLE, DIMENSION(:)::ALL_PARTIAL_SUM
!
!-----------------------------------------------------------------------
!
#  ifdef COMPAD
        WRITE(LU,*) 'P_DSUMERR: NOT YET IMPLEMENTED'
        CALL PLANTE(1)
        STOP
#  else
      CALL MPI_COMM_SIZE (MPI_COMM_WORLD, NUM_PROCS, IER)
      ALLOCATE(ALL_PARTIAL_SUM(1:2*NUM_PROCS))
      ALL_PARTIAL_SUM=0.D0
      CALL MPI_ALLGATHER (PARTIAL, 2, MPI_DOUBLE_PRECISION,
     & ALL_PARTIAL_SUM, 2, MPI_DOUBLE_PRECISION, MPI_COMM_WORLD, IER)
#  endif
!
      CALL SUMTWO(2*NUM_PROCS, ALL_PARTIAL_SUM, P_DSUMERR)
      DEALLOCATE(ALL_PARTIAL_SUM)
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_DSUMERR: ERROR IN MPI_ALLGATHER'
        WRITE(LU,*) 'MPI ERROR: ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
      WRITE(LU,*) 'CALL OF P_DSUM IN ITS VOID VERSION'
!
      P_DSUMERR=PARTIAL(1)
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
