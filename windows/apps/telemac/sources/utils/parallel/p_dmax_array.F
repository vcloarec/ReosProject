!                   ***********************
                    SUBROUTINE P_DMAX_ARRAY
!                   ***********************
!
     &(MYPART, N, VALUES)
!
!***********************************************************************
! PARALLEL   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    MAXIMUM VALUE FROM ALL THE PROCESSORS.
!
!history  Y AUDOUIN (LNHE)
!+        09/03/2018
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MYPART         |-->| SEND BUFFER
!| N              |-->| SIZE OF BUFFER
!| VALUES         |-->| RECEIVED BUFFER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARALLEL
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(IN) :: MYPART(N)
      DOUBLE PRECISION, INTENT(INOUT) :: VALUES(N)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MPI
      INTEGER IER
!
!-----------------------------------------------------------------------
!

#  ifdef COMPAD
      VALUES=0.D0
      CALL AMPI_ALLREDUCE(MYPART,VALUES,N,AMPI_TYPE,AMPI_OP_MAX,
     &                    COMM,IER)
#  else
      CALL MPI_ALLREDUCE(MYPART,VALUES,N,MPI_DOUBLE_PRECISION,MPI_MAX,
     &                   COMM,IER)
#  endif
!
      IF(IER.NE.0) THEN
        WRITE(LU,*) 'P_DMAX_ARRAY: ERROR IN MPI_ALLREDUCE'
        WRITE(LU,*) 'MPI ERROR ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
      RETURN
#else
!
      VALUES=MYPART
!
#endif
!
!-----------------------------------------------------------------------
!
      END
