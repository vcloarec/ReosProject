!                   *************************
                    MODULE M_COUPLING_ESTEL3D
!                   *************************
!
!
!***********************************************************************
! TELEMAC2D  V6P1
!***********************************************************************
!
!code
!+-----------------------------------------------------------------------
!+ THIS FILE IS PART OF TELEMAC-2D V5P7
!+-----------------------------------------------------------------------
!+ WRITTEN BY JP RENAUD
!+ SET OF STRUCTURES AND SUBROUTINES THAT LETS TELEMAC-2D AND ESTEL-3D
!+ INTERACT BY READING/WRITING TWO ARRAYS WHICH LIVE IN THIS MODULE:
!+
!+       - ESTEL-3D WRITES FLUX VALUES
!+       - TELEMAC-2D READS THE FLUX VALUES
!+       - TELEMAC-2D WRITES THE DEPTH VALUES
!+       - ESTEL-3D READS THE DEPTH VALUES
!+       - ETC...
!+
!+ AS THE ARRAYS ARE PRIVATE, THEY ARE ACCESSED BY PUBLIC METHODS:
!+ DEPTH_FILL SAVES THE DEPTHS FROM TELEMAC-2D AND DEPTH_GET ALLOWS
!+ ESTEL-3D TO RECOVER THE INFORMATION.
!+-----------------------------------------------------------------------
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: INFILTRATION_INIT
      PUBLIC :: INFILTRATION_FINISH
      PUBLIC :: INFILTRATION_FILL
      PUBLIC :: INFILTRATION_GET
      PUBLIC :: DEPTH_FILL
      PUBLIC :: DEPTH_GET
      INTEGER :: NPOIN2D
      LOGICAL :: DOINFILTRATION
      DOUBLE PRECISION, ALLOCATABLE :: FLUX_FROM_ESTEL3D(:)
      DOUBLE PRECISION, ALLOCATABLE :: DEPTH_FROM_T2D(:)
      LOGICAL :: DEJA_ESTEL3D = .FALSE.
!-----------------------------------------------------------------------
      CONTAINS
!-----------------------------------------------------------------------
      SUBROUTINE INFILTRATION_INIT(NPOIN,ACTIVATE)
!-----------------------------------------------------------------------
! ALLOCATES THE COUPLING ARRAYS IF REQUIRED AND FILLS THEM UP WITH ZEROS
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER, INTENT(IN) :: NPOIN
      LOGICAL, INTENT(IN) :: ACTIVATE
!-----------------------------------------------------------------------
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      NPOIN2D = 1
      IF(ACTIVATE) THEN
        NPOIN2D        = NPOIN
        DOINFILTRATION = .TRUE.
      ENDIF
      IF(.NOT.DEJA_ESTEL3D) THEN
        ALLOCATE( FLUX_FROM_ESTEL3D( NPOIN2D ) )
        ALLOCATE( DEPTH_FROM_T2D( NPOIN2D ) )
        DEJA_ESTEL3D=.TRUE.
      ENDIF
      DO I=1,NPOIN2D
        FLUX_FROM_ESTEL3D(I) = 0.D0
        DEPTH_FROM_T2D(I)    = 0.D0
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_INIT
!
      SUBROUTINE INFILTRATION_FINISH()
!-----------------------------------------------------------------------
! DE-ALLOCATES THE COUPLING ARRAYS
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
      DEALLOCATE( FLUX_FROM_ESTEL3D )
      DEALLOCATE( DEPTH_FROM_T2D )
      DEJA_ESTEL3D = .FALSE.
!
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_FINISH
!
      SUBROUTINE INFILTRATION_FILL(ARRAY1,ARRAY2,COEFF)
!-----------------------------------------------------------------------
! FILLS THE ARRAY FLUX_FROM_ESTEL3D WITH THE VALUES FROM THE ARGUMENTS
!
! THIS SUBROUTINE IS CALLED FROM ESTEL-3D
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(IN) :: ARRAY1(NPOIN2D)
      DOUBLE PRECISION, INTENT(IN) :: ARRAY2(NPOIN2D)
      DOUBLE PRECISION, INTENT(IN) :: COEFF
!-----------------------------------------------------------------------
              FLUX_FROM_ESTEL3D(:) = COEFF       * ARRAY1(:)
     &                             + (1 - COEFF) * ARRAY2(:)
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_FILL
!
      SUBROUTINE DEPTH_FILL(ARRAY_FROM_T2D)
!-----------------------------------------------------------------------
! FILLS THE ARRAY DEPTH_FROM_T2D WITH THE VALUES FROM THE ARGUMENT
!
! THIS SUBROUTINE IS CALLED FROM TELEMAC-2D
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(INOUT) :: ARRAY_FROM_T2D(NPOIN2D)
!-----------------------------------------------------------------------
      DEPTH_FROM_T2D(:) = ARRAY_FROM_T2D(:)
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE DEPTH_FILL
!
      SUBROUTINE DEPTH_GET(ARRAY_FROM_ESTEL3D)
!-----------------------------------------------------------------------
! BASICALLY READS THE ARRAY DEPTH_FROM_T2D SO THAT ESTEL-3D CAN
!  USE IT FOR ITS BOUNDARY CONDITIONS
!
! THIS SUBROUTINE IS CALLED FROM ESTEL-3D
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(INOUT) :: ARRAY_FROM_ESTEL3D(NPOIN2D)
!-----------------------------------------------------------------------
      ARRAY_FROM_ESTEL3D(:) = DEPTH_FROM_T2D(:)
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE DEPTH_GET
!
      SUBROUTINE INFILTRATION_GET(SMH,UNSV2D,YASMH)
!-----------------------------------------------------------------------
! ADDS THE INFILTRATION TERM TO THE SOURCE TERM SMH AND SWITCHES YASMH TO
! TRUE. NOTE THAT A MASS VECTOR IS REQUIRED AS ARGUMENT BECAUSE THE FLUX
! CALCULATED WITHIN ESTEL-3D IS MULTIPLIED BY A MASS VECTOR AND THE
! DIVISION IS EASIER TO DO FROM WITHIN TELEMAC-2D FOR MESH REASONS.
!
! THIS SUBROUTINE IS CALLED FROM TELEMAC-2D
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(INOUT) :: SMH(NPOIN2D)
      DOUBLE PRECISION, INTENT(IN)    :: UNSV2D(NPOIN2D)
      LOGICAL, INTENT(INOUT)          :: YASMH
!-----------------------------------------------------------------------
      IF(DOINFILTRATION) THEN
        YASMH  = .TRUE.
        SMH(:) = SMH(:) + FLUX_FROM_ESTEL3D(:)*UNSV2D(:)
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_GET
!
!-----------------------------------------------------------------------
      END MODULE M_COUPLING_ESTEL3D
