!     *****************************************
      MODULE MOD_COMPUTE_BOUNDARY_AND_INTERFACE
!     *****************************************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!BRIEF    Computing boundary and interface for partel
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        IMPLICIT NONE
        CONTAINS
!     *****************************************
      SUBROUTINE COMPUTE_BOUNDARY_AND_INTERFACE
!     *****************************************
     &        (NPARTS, NDP_2D, NPOIN_P,
     &         NPTFR_P, ELELG, NELEM_P, IKLES, KNOGL,
     &         CUT_P, EF_I, EF_II, NPTIR_P, NBRE_EF_I, KNOLG, IRAND,
     &         PART_P, NBRE_EF)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPARTS         |<--| Number of partitions
!| NDP_2D         |<--| Number of points per element
!| NPOIN_P        |<->| Number of point in partition
!| NPTFR_P        |<->| Number of boundary points in partition
!| ELELG          |<--| Local to global numbering for elements
!| NELEM_P        |<--| Number of elements in partition
!| IKLES          |<->| Connectivity array
!| KNOGL          |<--| Global to local numbering array
!| CUT_P          |<->| Global to local numbering for interface points
!| EF_I           |<->| EF_I(E) is the global label of the interface
!|                |   | finite element number e
!| EF_II          |<->| EF_II(E) is the local label of the interface
!|                |   | finite element number e
!| NPTIR_P        |<->| Number of interface points in the partition
!| NBRE_EF_I      |<->| Number of interface element on the subdomain
!| KNOLG          |<->| Local to global numbering array
!| IRAND          |<--| Ipobo array for the partition
!| PART_P         |<->| List of subdomain each node belongs to
!| NBRE_EF        |<->| Number of finite element containing I
!|                |   | I is a global label
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        USE DECLARATIONS_SPECIAL
        USE BIEF, ONLY : NBMAXNSHARE
        USE MOD_HASH_TABLE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        INTEGER, ALLOCATABLE, INTENT(INOUT) :: NPOIN_P(:),NPTFR_P(:),
     &    IKLES(:), PART_P(:,:), EF_I(:,:), EF_II(:,:),
     &    NPTIR_P(:), NBRE_EF_I(:), KNOLG(:,:), NBRE_EF(:)
!
        TYPE(HASH_TABLE), INTENT(INOUT) :: CUT_P
!
        INTEGER, ALLOCATABLE, INTENT(IN) :: ELELG(:,:), NELEM_P(:)
        INTEGER, ALLOCATABLE, INTENT(IN) :: IRAND(:)
        INTEGER, INTENT(IN) :: NPARTS, NDP_2D
        TYPE(HASH_TABLE), INTENT(IN) :: KNOGL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
        INTEGER :: I, J, K, POSI, TEMP, EF, NOEUD
        INTEGER :: MAX_NPOIN_P, IERR
        LOGICAL :: INTERFCE
        INTEGER, ALLOCATABLE :: NBRE_EF_LOC(:)

        MAX_NPOIN_P=MAXVAL(NPOIN_P)
!
!     NBRE_EF_LOC(I) : NUMBER OF FINITE ELEMENTS CONTAINING THE POINT I
!                      ON SUBMESH I
!     I IS THE LOCAL LABEL ON SUBMESH I
        ALLOCATE (NBRE_EF_LOC(MAX_NPOIN_P),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'NBRE_EF_LOC')

        DO I=1,NPARTS
          DO J=1,NPOIN_P(I)
            NBRE_EF_LOC(J)=0
          ENDDO
!
          NPOIN_P(I)=0
          NPTFR_P(I)=0
!
          DO J=1,NELEM_P(I)
            EF=ELELG(J,I)
            DO K=1,NDP_2D
              NOEUD=IKLES((EF-1)*3+K)
              TEMP=HASH_TABLE_GET(KNOGL, NOEUD, I)
              NBRE_EF_LOC(TEMP)=NBRE_EF_LOC(TEMP)+1
              IF(NBRE_EF_LOC(TEMP) .EQ. 1) THEN
!               THE POINT NOEUD IS ENCOUNTERED FOR THE FIRST TIME
                NPOIN_P(I)=NPOIN_P(I)+1
!               IS NOEUD A BOUNDARY POINT ?
                IF(IRAND(NOEUD) .NE. 0) THEN
                  NPTFR_P(I)= NPTFR_P(I)+1
                ENDIF
!               MODIFICATION OF KNOGL
                KNOLG(NPOIN_P(I),I)=NOEUD
              ENDIF
            ENDDO
          ENDDO
!
          NPTIR_P(I)=0
!
!         NOMBRE DE NOEUD INTERFACE DU SDI
!
          NBRE_EF_I(I)=0            ! NOMBRE D'ELEMENTS FINIS INTERFACES DU SDI
          DO J=1,NELEM_P(I)      ! ON PARCOURS A NOUVEAU LES ELEMENTS FINIS DU SDI
            INTERFCE=.FALSE.
            EF=ELELG(J,I)
            DO K=1,NDP_2D
              NOEUD=IKLES((EF-1)*3+K)
              TEMP=HASH_TABLE_GET(KNOGL, NOEUD, I)
              IF(ABS(NBRE_EF_LOC(TEMP)).NE.NBRE_EF(NOEUD)) THEN
                INTERFCE=.TRUE.
              ENDIF
              IF(NBRE_EF_LOC(TEMP).NE. NBRE_EF(NOEUD).AND.
     &           NBRE_EF_LOC(TEMP).GT.0) THEN
!               NOEUD EST INTERFACE CAR IL RESTE DES ELEMENTS FINIS
!               HORS DE SDI QUI LE CONTIENT
                INTERFCE=.TRUE.
                NPTIR_P(I)=NPTIR_P(I)+1
                CALL HASH_TABLE_INSERT(CUT_P, NPTIR_P(I), I, NOEUD)
                PART_P(NOEUD,0)=PART_P(NOEUD,0)+1
                POSI=PART_P(NOEUD,0)
                IF(POSI.GT.NBMAXNSHARE-1) THEN
                  WRITE(LU,*)  'ERROR : AN INTERFACE NODE BELONGS TO
     &                  MORE THAN NBMAXNSHARE-1 SUBDOMAINS'
                  CALL PLANTE(1)
                  STOP
                ENDIF
                PART_P(NOEUD,POSI)=I
                NBRE_EF_LOC(TEMP)=-NBRE_EF_LOC(TEMP)
              ENDIF
            ENDDO
            IF(INTERFCE) THEN
              NBRE_EF_I(I)=NBRE_EF_I(I)+1 ! L'ELEMENT FINI EST DONC AUSSI INTERFACE
              EF_I(I, NBRE_EF_I(I))=EF
              EF_II(I, NBRE_EF_I(I))=J
            ENDIF
          ENDDO
        ENDDO
      END SUBROUTINE
      END MODULE MOD_COMPUTE_BOUNDARY_AND_INTERFACE
