!                   *******************
                    SUBROUTINE VOISIN31
!                   *******************
!
     &(IFABOR,NELEM,NELMAX,IELM,IKLE,SIZIKL,NPOIN,NBOR,NPTFR,
     & LIHBOR,KLOG,INDPU,IKLESTR,NELEB2)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE ARRAY IFABOR, WHERE IFABOR(IELEM, IFACE) IS
!+                THE GLOBAL NUMBER OF THE NEIGHBOUR OF SIDE IFACE OF
!+                ELEMENT IELEM (IF THIS NEIGHBOUR EXISTS) AND 0 IF THE
!+                SIDE IS ON THE DOMAIN BOUNDARY.
!
!history  REGINA NEBAUER (LNHE)
!+        22/01/08
!+        V5P8
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
!history  J-M HERVOUET (LNHE)
!+        10/09/2012
!+        V6P3
!+   Treatment of prisms cut into tetrahedra added (IELM=51)
!+   and simplifications.
!
!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM           |-->| 31: TETRAHEDRA
!|                |   | 51: PRISMS CUT INTO TETRAHEDRA
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| INDPU          |-->| IF NOT 0, INTERFACE POINT IN PARALLEL
!| IKLESTR        |-->| CONNECTIVITY TABLE OF BOUNDARY TRIANGLES
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELEB2         |-->| NUMBER OF BOUNDARY TRIANGLES (oversized)
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| SIZIKL         |-->| FIRST DIMENSION OF IKLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VOISIN31 => VOISIN31
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)   :: IELM,NPTFR,NELEM,NELMAX,NPOIN,SIZIKL,KLOG
      INTEGER, INTENT(IN)   :: NBOR(NPTFR)
      INTEGER, INTENT(INOUT):: IFABOR(NELMAX,4)
      INTEGER, INTENT(IN)   :: IKLE(SIZIKL,4),LIHBOR(*)
      INTEGER, INTENT(IN)   :: INDPU(*)
      INTEGER, INTENT(IN)   :: NELEB2
      INTEGER, INTENT(IN)   :: IKLESTR(NELEB2,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! LOCAL VARIABLES
!
      ! ARRAY WHICH IS THE REVERSE OF NBOR
      ! (GIVES FOR EACH NODE IN THE DOMAIN THE BOUNDARY NODE NUMBER E,
      ! OR 0 IF IT IS AN INTERIOR NODE)
      INTEGER, DIMENSION(:  ), ALLOCATABLE :: NBOR_INV
      ! ARRAY DEFINING THE NUMBER OF ELEMENT (TETRAHEDRONS) NEIGHBOURING
      ! A NODE
      INTEGER, DIMENSION(:), ALLOCATABLE :: NVOIS
      ! ARRAY DEFINING THE IDENTIFIERS OF THE ELEMENTS NEIGHBOURING
      ! EACH NODE
      INTEGER, DIMENSION(:  ), ALLOCATABLE :: NEIGH
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLE_TRI
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: VOIS_TRI
      ! ARRAY DEFINING THE ADDRESSES OF THE VARIOUS ENTRIES IN
      ! ARRAY NEIGH
      INTEGER, DIMENSION(NPOIN)            :: IADR
      ! THE VALUE OF AN ENTRY IN THIS ARRAY
      INTEGER                              :: ADR
!
      ! THE MAXIMUM NUMBER OF ELEMENTS NEIGHBOURING A NODE
      INTEGER :: NMXVOISIN
      INTEGER :: IMAX       ! SIZE OF ARRAY IADR
!
      INTEGER :: NFACE      ! NUMBER OF SIDES TO ELEMENT (TETRA: 4)
      INTEGER :: NBTRI      ! NUMBER OF DEFINED TRIANGLES
!
      INTEGER :: IELEM      ! ELEMENTS COUNTER
      INTEGER :: IELEM2     ! ELEMENTS COUNTER
      INTEGER :: IPOIN      ! DOMAIN NODES COUNTER
      INTEGER :: INOEUD     ! TETRAHEDRONS/TRIANGLES NODES COUNTER
      INTEGER :: IFACE      ! SIDE COUNTER
      INTEGER :: IFACE2     ! SIDE COUNTER
      INTEGER :: ITRI       ! TRIANGLES COUNTER
      INTEGER :: IVOIS      ! NEIGHBOURS COUNTER
      INTEGER :: NV         ! NUMBER OF NEIGHBOURS
!
      INTEGER :: ERR        ! ERROR CODE FOR MEMORY ALLOCATION
!
      LOGICAL :: FOUND      ! FOUND OR NOT ...
!
      INTEGER :: I1, I2, I3 ! THE THREE NODES OF A TRIANGLE
      INTEGER :: M1, M2, M3 ! SAME THING, ORDERED
!
      INTEGER :: I,J,K      ! USEFUL ...
!
      INTEGER :: IR4,IR5,IR6,COMPT
      LOGICAL :: BORD
!
!     DEFINES THE FOUR TRIANGLES OF THE TETRAHEDRON: THE FIRST
!     DIMENSION IS THE NUMBER OF THE TRIANGLE, THE SECOND GIVES
!     THE NODE NUMBERS OF THE NODES OF TETRAHEDRONS WHICH DEFINE IT.
      INTEGER :: SOMFAC(3,4)
      PARAMETER ( SOMFAC = RESHAPE( (/
     &    1,2,3 , 4,1,2 , 2,3,4 , 3,4,1 /), SHAPE=(/ 3,4 /) ) )
!
!-----------------------------------------------------------------------
! START
!-----------------------------------------------------------------------
!
! CHECKS FIRST THAT WE ARE INDEED DEALING WITH TETRAHEDRONS. IF NOT,
! GOODBYE.
!
      IF(IELM.EQ.31.OR.IELM.EQ.51) THEN
        NFACE = 4
      ELSE
        WRITE(LU,99) IELM
99    FORMAT(1X,'VOISIN31: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
! ALLOCATES TEMPORARY ARRAYS
!
      ALLOCATE(NBOR_INV(NPOIN),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'VOISIN31:NBOR_INV')
!
! ALLOCATES TEMPORARY ARRAYS
!
      ALLOCATE(NVOIS(NPOIN),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'VOISIN31:NVOIS')
!
!-----------------------------------------------------------------------
! STEP 1: COUNTS THE NUMBER OF ELEMENTS NEIGHBOURING A NODE
!-----------------------------------------------------------------------
! COMPUTES THE NUMBER OF ELEMENTS NEIGHBOURING EACH NODE OF THE MESH.
! RESULT: NVOIS(INOEUD) GIVES THE NUMBER OF ELEMENTS NEIGHBOURING
! NODE INOEUD
!
      ! INITIALISES THE NEIGHBOURING ELEMENT COUNTER TO 0
      !
      DO I = 1,NPOIN
        NVOIS(I) = 0
      ENDDO
      ! COUNTS THE NEIGHBOURING ELEMENTS
      ! USING THE CONNECTIVITY TABLE, THE COUNTER IS INCREMENTED
      ! EACH TIME THAT AN ELEMENT REFERENCES NODE IPOIN
      ! LOOP ON THE 4 NODES OF THE ELEMENT
      DO INOEUD = 1, 4
        ! LOOP ON THE ELEMENTS
        DO IELEM = 1,NELEM
          ! THE ID OF NODE I OF ELEMENT IELEM
          IPOIN        = IKLE( IELEM , INOEUD )
          ! INCREMENT THE COUNTER
          NVOIS(IPOIN) = NVOIS(IPOIN) + 1
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
! STEP 2: DETERMINES THE SIZE OF ARRAY NEIGH() AND OF AUXILIARY
! ARRAY TO INDEX NEIGH. ALLOCATES NEIGH
!-----------------------------------------------------------------------
! CREATES AN ARRAY WHICH WILL CONTAIN THE IDENTIFIERS OF THE ELEMENTS
! NEIGHBOURING EACH NODE. SINCE THE NUMBER OF NEIGHBOURS IS A PRIORI
! DIFFERENT FOR EACH NODE, AND IN AN EFFORT NOT TO CREATE A (TOO) BIG
! ARRAY FOR NO REASON, AN AUXILIARY ARRAY IS REQUIRED THAT GIVES THE
! ADDRESS OF THE ENTRIES FOR A GIVEN NODE. THIS ARRAY HAS AS MANY
! ENTRIES AS THERE ARE NODES.
! WILL ALSO COMPUTE THE MAXIMUM NUMBER OF NEIGHBOURS, SOME TIME.
!
      ! THE FIRST ENTRY IN THE ID OF THE NEIGHBOURS ARRAY IS 1
      ADR       = 1
      IADR(1)   = ADR
      ! THE MAX NUMBER OF NEIGHBOURING ELEMENTS
      NV        = NVOIS(1)
      NMXVOISIN = NV
!
      DO IPOIN = 2,NPOIN
        ! ADDRESS FOR THE OTHER ENTRIES:
        ADR         = ADR + NV
        IADR(IPOIN) = ADR
        NV          = NVOIS(IPOIN)
        ! IDENTIFIES THE MAX. NUMBER OF NEIGHBOURS
        NMXVOISIN   = MAX(NMXVOISIN,NV)
      ENDDO
!
      ! THE TOTAL NUMBER OF NEIGHBOURING ELEMENTS FOR ALL THE NODES
      ! GIVES THE SIZE OF THE NEIGHBOURS ARRAY:
      IMAX = IADR(NPOIN) + NVOIS(NPOIN)
      ! ALLOCATES THE ARRAY CONTAINING THE IDENTIFIERS OF THE ELEMENTS
      ! NEIGHBOURING EACH NODE
      ALLOCATE(NEIGH(IMAX),STAT=ERR)
      IF(ERR.NE.0) GOTO 999
!
!-----------------------------------------------------------------------
! STEP 3: INITIALISES NEIGH
!-----------------------------------------------------------------------
! NEEDS TO FILL NEIGH NOW THAT IT'S BEEN ALLOCATED: I.E.
! STARTS AGAIN THE LOOP ON THE 4 NODES OF EACH ELEMENT AND THIS
! TIME, ALSO STORES THE IDENTIFIER IN ARRAY NEIGH.
!
!
      ! RE-INITIALISES THE COUNTER OF THE NEIGHBOURING ELEMENTS TO 0,
      ! TO KNOW WHERE WE ARE AT
      DO I = 1,NPOIN
        NVOIS(I) = 0
      ENDDO
!     NVOIS(:) = 0
      ! FOR EACH NODE OF THE ELEMENTS, STORES THE IDENTIFIER
      DO INOEUD = 1, 4  ! LOOP ON THE ELEMENT NODES
        DO IELEM=1,NELEM ! LOOP ON THE ELEMENTS
          IPOIN     = IKLE( IELEM , INOEUD )
          ! ONE MORE NEIGHBOUR
          NV           = NVOIS(IPOIN) + 1
          NVOIS(IPOIN) = NV
          ! STORES THE IDENTIFIER OF THE NEIGHBOURING ELEMENT IN THE ARRAY
          NEIGH(IADR(IPOIN)+NV) = IELEM
        ENDDO ! END OF LOOP ELEMENTS
      ENDDO  ! END OF LOOP NODES
!
!-----------------------------------------------------------------------
! STEP 4: IDENTIFIES COMMON FACES OF THE TETRAHEDRONS AND FILLS IN
! ARRAY IFABOR
!-----------------------------------------------------------------------
! TO IDENTIFY FACES COMMON TO THE ELEMENTS :
! FROM THE ELEMENTS THAT SHARE A NODE, AT LEAST 2 SHARE A FACE
! (IF THE NODE IS NOT A BOUNDARY NODE).
! THE ALGORITHM PRINCIPLE:
! BUILDS THE TRIANGLES OF THE TETRAHEDRON FACES, ONCE HAVE IDENTIFIED
! THOSE THAT SHARE NODE IPOIN.
! IF 2 TRIANGLES SHARE THE SAME NODES, IT MEANS THAT THE TETRAHEDRONS
! DEFINING THEM ARE NEIGHBOURS.
! IF NO NEIGHBOUR CAN BE FOUND, IT MEANS THAT THE TRIANGLE IS A
! BOUNDARY FACE.
! BASED ON THE ASSUMPTION THAT A TRIANGLE CANNOT BE DEFINED BY MORE
! THAN 2 TETRAHEDRONS.
! IF THAT WAS NOT THE CASE, IT WOULD MEAN THAT THERE WAS A PROBLEM WITH
! THE MESH; AND THIS IS NOT CATERED FOR ...
!
! ADVANTAGES:
! SAVES QUITE A BIT OF MEMORY, BY STORING THE TRIANGLES AROUND A NODE.
! DISADVANTAGES:
! COULD BE DOING TOO MANY (USELESS) COMPUTATIONS (TO GET TO THE STAGE
! WHERE THE CONNECTIVITY TABLE FOR THE TRIANGLES IS DEFINED)
! COULD MAYBE SKIP THIS STEP BY CHECKING IF IFABOR ALREADY CONTAINS
! SOMETHING OR NOT ...
!
! BUILDS THE CONNECTIVITY TABLE FOR THE TRIANGLES
! THIS CONNECTIVITY TABLE IS NOT SUPPOSED TO MAKE A LIST OF ALL
! THE TRIANGLES, BUT MERELY THOSE AROUND A NODE.
! THE MAXIMUM NUMBER OF (TETRAHEDRONS) NEIGHBOURS IS KNOWN FOR A
! NODE. IN THE WORST CASE, THE NODE IS A BOUNDARY NODE.
! WILL MAXIMISE (A LOT) BY ASSUMING THAT THE MAXIMUM NUMBER OF
! TRIANGLES AROUND A NODE CAN BE THE NUMBER OF NEIGHBOURING
! TETRAHEDRONS.
! ALSO BUILDS THE ARRAY VOIS_TRI CONTAINING THE ID OF THE TETRAHEDRON
! ELEMENT THAT DEFINED IT FIRST (AND THAT WILL BE NEIGHBOUR TO THE
! TETRAHEDRON THAT WILL FIND THAT ANOTHER ONE ALREADY DEFINES IT)
! THIS ARRAY HAS 2 ENTRIES : THE ID OF THE ELEMENT AND THE ID OF THE SIDE.
!
      NBTRI = NMXVOISIN * 3
!
      ALLOCATE(IKLE_TRI(NBTRI,3),STAT=ERR)
      IF(ERR.NE.0) GOTO 999
      ALLOCATE(VOIS_TRI(NBTRI,2),STAT=ERR)
      IF(ERR.NE.0) GOTO 999
!
!     ALL FACES PRESUMED SOLID TO START WITH...
!
      ! ASSUMING SOLID BOUNDARIES EVERYWHERE
      IFABOR(:,:) = -1
!
      ! LOOP ON ALL THE NODES IN THE MESH
      DO IPOIN = 1, NPOIN
        ! FOR EACH NODE, CHECKS THE NEIGHBOURING TETRAHEDRON ELEMENTS
        ! (MORE PRECISELY: THE TRIANGULAR FACES THAT MAKE IT)
        ! RE-INITIALISES THE CONNECTIVITY TABLE FOR THE TETRAHEDRON
        ! TRIANGLES TO 0, AND THE NUMBER OF TRIANGLES WHICH HAVE BEEN
        ! FOUND:
        !
        IKLE_TRI(:,:) = 0
        ! SAME THING FOR THE ARRAY THAT IDENTIFIES WHICH ELEMENT HAS
        ! ALREADY DEFINED THE TRIANGLE :
        VOIS_TRI(:,:) = 0
        ! STARTS COUNTING THE TRIANGLES AGAIN:
        NBTRI         = 0
        NV            = NVOIS(IPOIN)
        ADR           = IADR(IPOIN)
        DO IVOIS = 1, NV
          ! THE IDENTIFIER OF THE NEIGHBOURING ELEMENT IVOIS TO
          ! THE NODE IPOIN:
          IELEM = NEIGH(ADR+IVOIS)
          ! LOOP ON THE 4 SIDES OF THIS ELEMENT
          DO IFACE = 1 , NFACE
            ! IF THIS SIDE ALREADY HAS A NEIGHBOUR, THAT'S
            ! ENOUGH AND GOES TO NEXT.
            ! OTHERWISE, LOOKS FOR IT...
            IF ( IFABOR(IELEM,IFACE) .LE. 0 ) THEN
            ! EACH FACE DEFINES A TRIANGLE. THE TRIANGLE IS
            ! GIVEN BY 3 NODES.
            I1 = IKLE(IELEM,SOMFAC(1,IFACE))
            I2 = IKLE(IELEM,SOMFAC(2,IFACE))
            I3 = IKLE(IELEM,SOMFAC(3,IFACE))
            ! THESE 3 NODES ARE ORDERED, M1 IS THE NODE WITH
            ! THE SMALLEST IDENTIFIER, M3 THAT WITH THE
            ! LARGEST IDENTIFIER AND M2 IS IN THE MIDDLE:
            M1 = MAX(I1,(MAX(I2,I3)))
            M3 = MIN(I1,(MIN(I2,I3)))
            M2 = I1+I2+I3-M1-M3
            ! GOES THROUGH THE ARRAY WITH TRIANGLES ALREADY DEFINED
            ! TO SEE IF ONE OF THEM BEGINS WITH M1.
            ! IF THAT'S THE CASE, CHECKS THAT IT ALSO HAS NODES
            ! M2 AND M3. IF THAT'S THE CASE, HAS FOUND A NEIGHBOUR.
            ! OTHERWISE, A NEW TRIANGLE ENTRY IS CREATED.
            !
            FOUND = .FALSE.
            DO ITRI = 1, NBTRI
              IF ( IKLE_TRI(ITRI,1) .EQ. M1 ) THEN
                IF ( IKLE_TRI(ITRI,2) .EQ. M2 .AND.
     &            IKLE_TRI(ITRI,3) .EQ. M3 ) THEN
                  ! FOUND IT! ALL IS WELL.
                  ! STORES THE INFORMATION IN VOIS_TRI.
                  ! (I.E. THE ELEMENT THAT HAS ALREADY
                  ! DEFINED THE TRIANGLE AND THE FACE)
                  IELEM2 = VOIS_TRI(ITRI,1)
                  IFACE2 = VOIS_TRI(ITRI,2)
                  IF ( IELEM2 .EQ. IELEM ) THEN
                    WRITE(LU,909) IELM
909                 FORMAT(1X,'VOISIN: IELM=',1I6,',
     &              NEIGHBOUR PROBLEM')
                    CALL PLANTE(1)
                    STOP
                  END IF
                  ! TO BE SURE :
                  IF ( IELEM2 .EQ. 0 .OR.
     &                 IFACE2 .EQ. 0 ) THEN
                    WRITE(LU,919) IELEM2,IFACE2
919                 FORMAT(1X,'VOISIN31:UNDEFINED TRIANGLE,
     &              IELEM=',1I6,'IFACE=',1I6)
                    CALL PLANTE(1)
                    STOP
                  END IF
                  ! THE ELEMENT AND ITS NEIGHBOUR : STORES
                  ! THE CONNECTION IN IFABOR.
                  IFABOR(IELEM ,IFACE ) = IELEM2
                  IFABOR(IELEM2,IFACE2) = IELEM
                  FOUND = .TRUE.
                END IF
              END IF
            END DO
            ! NO, THIS TRIANGLE WAS NOT ALREADY THERE; THEREFORE
            ! CREATES A NEW ENTRY.
            IF ( .NOT. FOUND) THEN
                NBTRI             = NBTRI + 1
                IKLE_TRI(NBTRI,1) = M1
                IKLE_TRI(NBTRI,2) = M2
                IKLE_TRI(NBTRI,3) = M3
                VOIS_TRI(NBTRI,1) = IELEM
                VOIS_TRI(NBTRI,2) = IFACE
            END IF
          END IF ! IFABOR 0
          END DO ! END OF LOOP ON FACES OF THE NEIGHBOURING ELEMENTS
!
        END DO ! END OF LOOP ON ELEMENTS NEIGHBOURING THE NODE
      END DO ! END OF LOOP ON NODES
!
      DEALLOCATE(NEIGH)
      DEALLOCATE(IKLE_TRI)
      DEALLOCATE(VOIS_TRI)
!
!-----------------------------------------------------------------------
!     STEP 5: FACES BETWEEN DIFFERENT COMPUTATION DOMAIN
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        !
        ! WHY A DIFFERENT ALGORITHM ?
        ! a) Partitionning method : 51 submeshes are obtained from a 2D mesh
        ! b) No iklestr is available
        ! c) Some pathological cases with 31
        IF (IELM.EQ.51) THEN
          !
          DO IFACE=1,NFACE
            DO IELEM=1,NELEM
!     IF A FACE HAS 3 POINTS WHICH ARE INTERFACES BETWEEN SUB-DOMAINS
!     IT IS ASSIGNED A VALUE OF -2
!
              I1=IKLE(IELEM,SOMFAC(1,IFACE))
              I2=IKLE(IELEM,SOMFAC(2,IFACE))
              I3=IKLE(IELEM,SOMFAC(3,IFACE))
!
              IF( INDPU(I1).NE.0.AND.
     &            INDPU(I2).NE.0.AND.
     &            INDPU(I3).NE.0     ) IFABOR(IELEM,IFACE)=-2
!
            ENDDO
          ENDDO
!
        ELSE IF (IELM.EQ.31) THEN
!
          DO IFACE=1,NFACE
            DO IELEM=1,NELEM
!     IF A FACE HAS 3 POINTS WHICH ARE INTERFACES BETWEEN SUB-DOMAINS
!     IT IS ASSIGNED A VALUE OF -2

!     PATHOLOGIC CASES (THANKS TO OLIVIER BOITEAU, EDF R&D/SINETICS)
!     a) 3 nodes may have INDPU /= 0, this doesn't mean automatically
!     that the triangle is an interface one (case of true unstructured tetra mesh)
!     b) a border triangle may have INDPU /= 0 on its 3 nodes
!     => priority to border triangles
!     c) 3 nodes of a triangle may be on the boundary and have INDPU /= 0
!     this doesn't mean automatically that the triangle is on the boundary.

              IF (IFABOR(IELEM,IFACE).EQ.-1) THEN
!
                I1=IKLE(IELEM,SOMFAC(1,IFACE))
                I2=IKLE(IELEM,SOMFAC(2,IFACE))
                I3=IKLE(IELEM,SOMFAC(3,IFACE))

                IF ( INDPU(I1).NE.0.AND.
     &               INDPU(I2).NE.0.AND.
     &               INDPU(I3).NE.0     ) THEN

!     THESE ARE INTERFACE NODES; DO THEY CORRESPOND TO A
!     (VIRTUAL) INTERFACE TRIANGLE OR TO A BOUNDARY TRIANGLE ?

!     THE FOLLOWING TEST IS NOT SUFFICIENT
!     IF (.NOT. (IR5.EQ.1.AND.IR4.EQ.1.AND.IR6.EQ.1)) IFABOR(IELEM,IFACE)=-2
!
                  BORD=.FALSE.
                  IR4=0
                  IR5=0
                  IR6=0
                  DO J=1,NPTFR
                    IF (I1.EQ.NBOR(J)) IR5=1
                    IF (I2.EQ.NBOR(J)) IR4=1
                    IF (I3.EQ.NBOR(J)) IR6=1
                  ENDDO ! J

!     THEY ARE ALSO BOUNDARY NODES
                  IF (IR5.EQ.1.AND.IR4.EQ.1.AND.IR6.EQ.1) THEN
!
                    DO J=1,NELEB2
!     IT IS A BOUNDARY TRIANGLE
                      COMPT=0
                      DO I=1,3
                        IF (IKLESTR(J,I)==I1) COMPT=COMPT+1
                        IF (IKLESTR(J,I)==I2) COMPT=COMPT+10
                        IF (IKLESTR(J,I)==I3) COMPT=COMPT+100
                      ENDDO
!     THESE 3 NODES INDEED BELONG TO THE SAME BOUNDARY TRIANGLE
                      IF (COMPT==111) THEN
                        BORD=.TRUE.
!     WRITE(LU,*)'VERTICES OF A BOUNDARY TRIANGLE'
                        EXIT
                      ENDIF
                    ENDDO
                  ENDIF
                  IF (.NOT.BORD) THEN
!     THESE 3 NODES BELONG TO AN INTERFACE MESH
!     WRITE(LU,*) 'INTERFACE NODES'
                    IFABOR(IELEM,IFACE)=-2
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!
!-----------------------------------------------------------------------
!
!     WITH ELEMENT 51 THE DIFFERENCE BETWEEN SOLID AND LIQUID
!     BOUNDARIES IS DONE LATER
!     AND I DOUBT THAT THE CODE BELOW WITH LIHBOR CAN WORK WITH
!     REAL TETRAHEDRA
!
      IF((IELM.EQ.51).OR.(IELM.EQ.31)) GO TO 1000
!
!-----------------------------------------------------------------------
!
!  IFABOR DISTINGUISHES BETWEEN THE BOUNDARY FACES AND THE LIQUID FACES
!
!  INITIALISES NBOR_INV TO 0
!
      DO IPOIN=1,NPOIN
        NBOR_INV(IPOIN) = 0
      ENDDO
!
!  CONNECTS GLOBAL NUMBERING TO BOUNDARY NUMBERING
!
      DO K = 1, NPTFR
        NBOR_INV(NBOR(K)) = K
      ENDDO
!
!  LOOP ON ALL THE SIDES OF ALL THE ELEMENTS:
!
      DO IFACE = 1 , NFACE
      DO IELEM = 1 , NELEM
!
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
!
!       IT IS A TRUE BOUNDARY SIDE (IN PARALLEL MODE THE INTERNAL SIDES
!                                   ARE INDICATED WITH -2).
!       GLOBAL NUMBERS OF THE NODES OF THE SIDE :
!
        I1 = IKLE( IELEM , SOMFAC(1,IFACE) )
        I2 = IKLE( IELEM , SOMFAC(2,IFACE) )
        I3 = IKLE( IELEM , SOMFAC(3,IFACE) )
!
!       A LIQUID SIDE IS IDENTIFIED WITH THE BOUNDARY CONDITION ON H
!
        IF(NPTFR.GT.0) THEN
        IF(LIHBOR(NBOR_INV(I1)).NE.KLOG.AND.LIHBOR(NBOR_INV(I2)).NE.KLOG
     &      .AND.LIHBOR(NBOR_INV(I3)).NE.KLOG  ) THEN
!         LIQUID SIDE : IFABOR=0  SOLID SIDE : IFABOR=-1
          IFABOR(IELEM,IFACE)=0
        ENDIF
        ENDIF
!
      ENDIF
!
      ENDDO ! IELEM
      ENDDO ! IFACE
!
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
999   WRITE(LU,2000) ERR
2000  FORMAT(1X,'VOISIN31: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      END
