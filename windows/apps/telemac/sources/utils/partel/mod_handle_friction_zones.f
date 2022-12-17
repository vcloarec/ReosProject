!     ********************************
      MODULE MOD_HANDLE_FRICTION_ZONES
!     ********************************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!BRIEF    Treatment of sections
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        IMPLICIT NONE
        CONTAINS
!     ********************************
      SUBROUTINE HANDLE_FRICTION_ZONES
!     ********************************
!
     &        (NAMEZFI, NPARTS, NPOIN, NPOIN_P, MAX_NPOIN_P, KNOLG)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMEZFI        |<--| Name of friction zones file
!| NPARTS         |<--| Number of partitions
!| NPOIN          |<--| Number of points
!| NPOIN_P        |<--| Number of points per partition
!| MAX_NPOIN_P    |<--| Maxw Number of points on a partition
!| KNOLG          |<--| Local to global numbering
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_PARTEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NAMEZFI
      INTEGER, INTENT(IN) :: NPARTS
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: MAX_NPOIN_P
      INTEGER, INTENT(IN) :: NPOIN_P(NPARTS)
      INTEGER, INTENT(IN) :: KNOLG(MAX_NPOIN_P, NPARTS)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTEGER, ALLOCATABLE :: FRICTION(:)
      INTEGER :: NZFI,I_GLOB,VAL_ZFI
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS
      INTEGER I, J, N, IERR
      CHARACTER(LEN=PATH_LEN)  :: NAMEOUT
!
!     READING THE FRICTION INFORMATIONS
      CALL GET_FREE_ID(NZFI)
      OPEN(NZFI,FILE=TRIM(NAMEZFI),FORM='FORMATTED',STATUS='OLD')
      ALLOCATE (FRICTION(NPOIN), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'FRICTION')
      FRICTION(:) = 0
      DO J=1,NPOIN
        READ(NZFI,*,END=304,ERR=303) I, VAL_ZFI
        FRICTION(I) = VAL_ZFI
      ENDDO
      CLOSE(NZFI)
!
      DO N=1,NPARTS
        NAMEOUT=TRIM(NAMEZFI)//EXTENS(NPARTS-1,N-1)

        WRITE(LU,*) 'WRITING: ', TRIM(NAMEOUT)

        OPEN (NZFI,FILE=TRIM(NAMEOUT),FORM='FORMATTED',STATUS='NEW')
        DO I=1,NPOIN_P(N)
          I_GLOB = KNOLG(I,N)
          WRITE(NZFI,*) I, FRICTION(I_GLOB)
        END DO
        CLOSE(NZFI)
      END DO
      DEALLOCATE(FRICTION)
      RETURN
!
 303  WRITE(LU,*) 'ERROR WITH ZONE FILE FORMAT'
      GO TO 999
 304  WRITE(LU,*) 'ABNORMAL END OF FILE'
      GO TO 999
 999  CALL PLANTE(1)
      STOP
      END SUBROUTINE
      END MODULE
