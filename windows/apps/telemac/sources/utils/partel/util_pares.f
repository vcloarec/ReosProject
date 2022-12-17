      MODULE UTIL_PARES

      IMPLICIT NONE

      CONTAINS

      FUNCTION I2CHAR2 (INT_IN)
      !-----------------------------------------------------------------------
      ! DESCRIPTION:
      !
      ! CONVERTS AN INTEGER INTO A STRING. MAXIMUM OF 12 DIGITS.
      ! THE INTEGER IS FORMATTED PROPERLY SO THAT THE RETURNED STRING CAN
      ! THEN BE TRIMMED ( TRIM(I2CHAR() )
      !-----------------------------------------------------------------------
      IMPLICIT NONE
      !-----------------------------------------------------------------------
      ! ARGUMENTS
      INTEGER, INTENT(IN) :: INT_IN ! THE INTEGER TO CONVERT
      CHARACTER (LEN=12) :: I2CHAR2
      !-----------------------------------------------------------------------
      ! LOCAL VARIABLES
      CHARACTER(LEN=12) :: STRING    ! TEMPORARY STRING
      CHARACTER(LEN=5)  :: THEFORMAT ! FORMAT TO USE FOR THE INTEGER
      INTEGER           :: N         ! NUMBER OF DECIMALS IN THE INTEGER
      !-----------------------------------------------------------------------
      ! WE LOOK FOR N SUCH THAT 10^{N-1} < INT_IN < 10^{N}
      ! THIS IS DONE TO MAKE SURE THAT WE DO NOT CREATE A FORMAT "OVERFLOW"
      N = 1
      DO WHILE (INT_IN.GE.10**N)
        N = N + 1
      ENDDO

      ! CHECK ON THE "LENGTH" OF THE INTEGER
      IF (N .LE. 9) THEN

        ! WRITE THE INTEGER IN A STRING WITH THE RIGHT FORMAT
        WRITE(UNIT=THEFORMAT,FMT='(''(I'',I1,'')'')') N
        WRITE(UNIT=STRING,FMT=THEFORMAT) INT_IN

      ELSE IF ( (N .GE. 10) .AND. (N .LE. 12) ) THEN

        ! WRITE THE INTEGER IN A STRING WITH THE RIGHT FORMAT
        WRITE(UNIT=THEFORMAT,FMT='(''I'',I2)') N
        WRITE(UNIT=STRING,FMT=THEFORMAT) INT_IN

      ELSE
        ! IT IS NOT POSSIBLE TO OUTPUT THIS INTEGER WITHT HE DEFAULT FORMAT
        WRITE(*,*)'FORMAT ERROR IN I2CHAR2.'
      ENDIF

      ! TRIM THE STRING AND RETURN
      I2CHAR2 = TRIM(STRING)
      !-----------------------------------------------------------------------
      END FUNCTION I2CHAR2

!     ---------------------------------------------------

      SUBROUTINE CALC_NELEB(IKLE, NELEM, NPOIN,NELEB)

      !-----------------------------------------------------------------------

      IMPLICIT NONE

      !-----------------------------------------------------------------------
      ! GLOBAL VARIABLES
      !VOIR LA SUBROUTINE VOISIN31.F DONT CETTE PROCEDURE EST DIRECTEMENT INSPIRE
      !
      INTEGER,          INTENT(IN)  :: NELEM
      INTEGER,          INTENT(IN)  :: NPOIN
      INTEGER,          INTENT(IN)  :: IKLE(NELEM,4)
      INTEGER,          INTENT(OUT) :: NELEB
      !-----------------------------------------------------------------------
      ! VARIABLES LOCALES

      INTEGER :: NVOIS(NPOIN),IADR(NPOIN)
      INTEGER :: I,INOEUD,IELEM,IPOIN,ADR,IMAX,NBTRI,NV,NMXVOISIN,
     &           IVOIS,IFACE,M1,M2,M3,I1,I2,I3,NFACE,ITRI,IELEM2,IFACE2
      INTEGER, DIMENSION(:), ALLOCATABLE :: NEIGH
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLE_TRI,VOIS_TRI,IFABOR
      LOGICAL :: FOUND
      !   ~~~~~~~~~~~~~~~~~~~~~~~
      !     DEFINITION DES QUATRE TRIANGLES DU TETRAEDRE : LA PREMIERE
      !     DIMENSION DU TABLEAU EST LE NUMERO DU TRIANGLE, LA DEUXIEME DONNE
      !     LES NUMEROS DES NOEUDS DE TETRAEDRES QUI LE DEFINISSENT.
      INTEGER :: SOMFAC(3,4)
      PARAMETER ( SOMFAC = RESHAPE((/ 1,2,3 , 4,1,2 ,
     &                                2,3,4 , 3,4,1 /),(/3,4/) ))

!-----------------------------------------------------------------------
      NFACE=4
!-----------------------------------------------------------------------
! ETAPE 1 : Comptage du nombre d'elements voisins d'un noeud.
!-----------------------------------------------------------------------
      DO I = 1, NPOIN
        NVOIS(I) = 0
      END DO
      DO INOEUD = 1, 4
        DO IELEM = 1,NELEM
          IPOIN        = IKLE( IELEM , INOEUD )
          NVOIS(IPOIN) = NVOIS(IPOIN) + 1
        END DO
      END DO

!-----------------------------------------------------------------------
! ETAPE 2 : Determination de la taille du tableau NEIGH() et de la
! table auxiliaire pour indexer NEIGH. allocation de NEIGH
!-----------------------------------------------------------------------
      ADR       = 1
      IADR(1)   = ADR
      NV        = NVOIS(1)
      NMXVOISIN = NV

      DO IPOIN = 2,NPOIN
          ADR         = ADR + NV
          IADR(IPOIN) = ADR
          NV          = NVOIS(IPOIN)
          NMXVOISIN   = MAX(NMXVOISIN,NV)
      END DO

      IMAX = IADR(NPOIN) + NVOIS(NPOIN)

      ALLOCATE(NEIGH(IMAX))
!-----------------------------------------------------------------------
! ETAPE 3 : initialisation de NEIGH
!-----------------------------------------------------------------------
      NVOIS(:) = 0

      DO INOEUD = 1, 4
        DO IELEM=1,NELEM
          IPOIN     = IKLE( IELEM , INOEUD )
          NV           = NVOIS(IPOIN) + 1
          NVOIS(IPOIN) = NV
          NEIGH(IADR(IPOIN)+NV) = IELEM
        END DO
      END DO
!-----------------------------------------------------------------------
! ETAPE 4 : Reperer les faces communes des tetraedres et remplir le
! tableau IFABOR.
!-----------------------------------------------------------------------
      NBTRI = NMXVOISIN * 3
!
      ALLOCATE(IKLE_TRI(NBTRI,3))
      ALLOCATE(VOIS_TRI(NBTRI,2))
      ALLOCATE(IFABOR(NELEM,4))
      IFABOR(:,:) = 0
      DO IPOIN = 1, NPOIN
        IKLE_TRI(:,:) = 0
        VOIS_TRI(:,:) = 0
        NBTRI         = 0
        NV            = NVOIS(IPOIN)
        ADR           = IADR(IPOIN)
        DO IVOIS = 1, NV
          IELEM = NEIGH(ADR+IVOIS)
          DO IFACE = 1 , NFACE
            IF ( IFABOR(IELEM,IFACE) .EQ. 0 ) THEN
              I1 = IKLE(IELEM,SOMFAC(1,IFACE))
              I2 = IKLE(IELEM,SOMFAC(2,IFACE))
              I3 = IKLE(IELEM,SOMFAC(3,IFACE))
              M1 = MAX(I1,(MAX(I2,I3)))
              M3 = MIN(I1,(MIN(I2,I3)))
              M2 = I1+I2+I3-M1-M3
              FOUND = .FALSE.
              DO ITRI = 1, NBTRI
                IF ( IKLE_TRI(ITRI,1) .EQ. M1 ) THEN
                  IF ( IKLE_TRI(ITRI,2) .EQ. M2 .AND.
     &                 IKLE_TRI(ITRI,3) .EQ. M3 ) THEN
                    IELEM2 = VOIS_TRI(ITRI,1)
                    IFACE2 = VOIS_TRI(ITRI,2)
                    IFABOR(IELEM ,IFACE ) = IELEM2
                    IFABOR(IELEM2,IFACE2) = IELEM
                    FOUND = .TRUE.
                  END IF
                END IF
              END DO
              IF ( .NOT. FOUND) THEN
                NBTRI             = NBTRI + 1
                IKLE_TRI(NBTRI,1) = M1
                IKLE_TRI(NBTRI,2) = M2
                IKLE_TRI(NBTRI,3) = M3
                VOIS_TRI(NBTRI,1) = IELEM
                VOIS_TRI(NBTRI,2) = IFACE
              END IF
            END IF
          END DO
!
        END DO
      END DO
      !
      NELEB = COUNT(IFABOR==0)
      DEALLOCATE(NEIGH)
      DEALLOCATE(IKLE_TRI)
      DEALLOCATE(VOIS_TRI)
      DEALLOCATE(IFABOR)
      !-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE CALC_NELEB

      END MODULE UTIL_PARES
