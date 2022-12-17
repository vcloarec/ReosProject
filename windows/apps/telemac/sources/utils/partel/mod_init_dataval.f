!                   ***********************
                    MODULE MOD_INIT_DATAVAL
!                   ***********************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!BRIEF    Initialisation of data values
!
              IMPLICIT NONE
!
              CONTAINS
!***********************************************************************
      SUBROUTINE INIT_DATAVAL
!***********************************************************************
     & (DATAVAL, NPOIN, NVAR, NTIMESTEP, FFORMAT, NINP, VARIABLE)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DATAVAL        |-->| DATA ARRAY
!| NPOIN          |<--| Number of points
!| NVAR           |<--| Number of variables
!| NTIMESTEP      |<--| Number of time steps
!| FFORMAT        |<--| File format
!| NINP           |<--| Index of the file
!| VARIABLE       |<--| Name of the variables
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        DOUBLE PRECISION, ALLOCATABLE, INTENT(OUT) :: DATAVAL(:,:,:)
        INTEGER, INTENT(IN) :: NPOIN, NVAR, NTIMESTEP, NINP
        CHARACTER(LEN=32), ALLOCATABLE, INTENT(IN) :: VARIABLE(:)
        CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER :: IERR, ITIME, IVAR
!
        ALLOCATE(DATAVAL(NPOIN,NVAR,NTIMESTEP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'PARTEL:DATAVAL')
!
        DO ITIME=1,NTIMESTEP
          DO IVAR=1,NVAR
            ! READING VARIABLE VALUE FOR TIME ITIME-1 ON INPUT FILE
            CALL GET_DATA_VALUE(FFORMAT,NINP,ITIME-1,
     &        VARIABLE(IVAR)(1:16),DATAVAL(:,IVAR,ITIME),NPOIN,IERR)
            CALL CHECK_CALL(IERR,'PARTEL:GET_DATA_VALUE:NINP')
          END DO
        END DO
      END SUBROUTINE
!
      END MODULE MOD_INIT_DATAVAL
