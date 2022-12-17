!     ************************************
      MODULE MOD_NUMBERING_OPEN_BOUNDARIES
!     ************************************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!BRIEF    Numbering of open boundaries for partel
!
              IMPLICIT NONE
              CONTAINS
!***********************************************************************
      SUBROUTINE NUMBERING_OPEN_BOUNDARIES
!***********************************************************************
     & (NAMEINP, IKLE, IKLES,
     &  KP1BOR, NUMLIQ, DIM_MESH, NPOIN2, NPTFR, NPOIN, NELEM2,
     &  NELBOR, LIUBOR, LIHBOR, NBOR, IFABOR, F, LISTIN)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMEINP        |<--| Name of the geometry file
!| IKLE           |-->| Connectivity array
!| IKLES          |<--| Connectivity array 1D form
!| KP1BOR         |-->| Neigbouring boundary nodes array
!| NUMLIQ         |-->| Array for liquid boundaries
!| DIM_MESH       |<--| Dimension of the mesh
!| NPOIN2         |<--| Number of 2D points
!| NPTFR          |<->| Number of boundary points
!| NPOIN          |<--| Number of points
!| NELEM2         |<->| Number of 2D elements
!| NELBOR         |-->| Number of boundary elements
!| LIUBOR         |<--| Boundary value for velocity
!| LIHBOR         |<--| Boundary value for height
!| NBOR           |<->| Boundary numbering array
!| IFABOR         |-->| Array for boundaries
!| F              |<--| Coordinates
!| LISTIN         |<--| If True display front2 listin
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE DECLARATIONS_PARTEL
        USE BIEF, ONLY : NPTIR, NBMAXNSHARE
        USE DECLARATIONS_SPECIAL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER, INTENT(IN) :: DIM_MESH, NPOIN2, NPOIN
        INTEGER, INTENT(INOUT)::NPTFR,NELEM2
        INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
!
        INTEGER, ALLOCATABLE, INTENT(OUT) :: NELBOR(:), IKLE(:,:),
     &    KP1BOR(:,:),IFABOR(:,:),NUMLIQ(:)
!
        DOUBLE PRECISION, INTENT(IN) :: F(NPOIN,2)
        INTEGER, INTENT(IN) :: IKLES(NELEM2*3)
        INTEGER, INTENT(IN) :: LIUBOR(NPTFR), LIHBOR(NPTFR)
        CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NAMEINP
        LOGICAL, INTENT(IN) :: LISTIN
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER, ALLOCATABLE :: IFANUM(:,:),IKLBOR(:,:),NULONE(:,:),
     &    NACHB(:,:)
        INTEGER, ALLOCATABLE ::IT1(:),IT2(:),IT3(:),DEJAVU(:),ISEGF(:)
        INTEGER :: NFRLIQ, I, J, K, IERR, ID
        INTEGER, PARAMETER :: MAXFRO = 30
        INTEGER :: NELEBD
!
        !THESE ARRAYS WILL LIVE OUTSIDE THIS SUBROUTINE
        ALLOCATE (KP1BOR(NPTFR,2),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'KP1BOR')
        ALLOCATE (IFABOR(NELEM2,3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IFABOR')
        ALLOCATE (NELBOR(NPTFR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'NELBOR')
        ALLOCATE (IKLE(NELEM2,3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IKLE')
        ALLOCATE (NUMLIQ(NPTFR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'NUMLIQ')
!
        !LOCAL ARRAYS
        ALLOCATE (DEJAVU(NPTFR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'DEJAVU')
!       CHANGED NELEM TO NELEM2, NDP TO 3 HUH!
!       CAUSING ERRORS WHEN 3D RESTART/REFERENCE FILES ARE PARTITIONED
!       AND BC FILE IS WRITTEN AGAIN (WHAT FOR, ACTUALLY???)
!       CAUSE: CALLING VOISIN WITH NELEM2 BUT IFABOR(NELEM=NELEM3,NDP=6)
        ALLOCATE (IFANUM(NELEM2,3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IFANUM')
        ALLOCATE (IKLBOR(NPTFR,2),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IKLBOR')
        ALLOCATE (NULONE(NPTFR,2),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'NULONE')
        ALLOCATE (ISEGF(NPTFR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'ISEGF')
        ALLOCATE (IT1(NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IT1')
        ALLOCATE (IT2(NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IT2')
        ALLOCATE (IT3(NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'IT3')
        NPTIR = 1
        ALLOCATE (NACHB(NBMAXNSHARE,NPTIR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'NACHB')
!
! TRANSFORM IKLES--> IKLE FOR 2D ROUTINES  (AN OLD TELEMAC DISEASE)
!
        DO I = 1,3
          DO J  = 1,NELEM2
            IKLE(J,I) = IKLES((J-1)*3+I)
          ENDDO
        ENDDO
!
        CALL VOISIN(IFABOR, NELEM2, NELEM2, 11, IKLE, NELEM2,
     &                     NPOIN2, NACHB, NBOR, NPTFR, IT1, IT2)
        DEALLOCATE(NACHB)
!
        CALL ELEBD (NELBOR, NULONE, KP1BOR, IFABOR, NBOR, IKLE,
     &                     NELEM2, IKLBOR, NELEM2, NELEM2,
     &                     NPOIN2, NPTFR, 11, LIHBOR, 2,
     &                     ISEGF, IT1, IT2, IT3 ,
     &                     NPTFR ,NELEBD)
!
        IF (NAMEINP(1:3)== 'ART') THEN
          CALL GET_FREE_ID(ID)
          OPEN(UNIT=ID,FILE='FRONT_GLOB.DAT')
          WRITE(ID,*) NPOIN
          WRITE(ID,*) NPTFR
          DO K=1,NPTFR
            WRITE(ID,*) NBOR(K)
          END DO
          DO K=1,NPTFR
            WRITE(ID,*) KP1BOR(K,1)
          END DO
          DO K=1,NPTFR
            WRITE(ID,*) KP1BOR(K,2)
          END DO
          CLOSE(ID)
        END IF
        NFRLIQ = 0
        IF(DIM_MESH.NE.3) THEN
          CALL FRONT2
     &           (NFRLIQ,
     &           LIHBOR,LIUBOR,F(:,1),F(:,2),
     &           NBOR,KP1BOR(1:NPTFR,1),DEJAVU,NPOIN2,NPTFR,
     &           2,LISTIN,NUMLIQ,MAXFRO)
        ENDIF
!
        DEALLOCATE (DEJAVU)
!JAJ // IFABOR APPLIED LATER FOR FINDING HALO CELL NEIGHBOURHOODS
!       DEALLOCATE (IFABOR)
        DEALLOCATE (IFANUM)
        DEALLOCATE (IKLBOR)
        DEALLOCATE (NULONE)
        DEALLOCATE (ISEGF)
        DEALLOCATE (IT1)
        DEALLOCATE (IT2)
        DEALLOCATE (IT3)
      END SUBROUTINE
      END MODULE MOD_NUMBERING_OPEN_BOUNDARIES
