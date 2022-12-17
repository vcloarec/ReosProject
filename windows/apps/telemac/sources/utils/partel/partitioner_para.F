!                   **************
                    SUBROUTINE PARTITIONER_PARA
!                   **************
     &    (PMETHOD, NELEM, NPOIN, NDP, NPARTS, IKLES, EPART, NPART)
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    call to the partionning software
!
!history   R. KOPMANN (BAW)
!+
!+
!+         created
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| PMETHOD        |<--| 1: FOR METIS 3: FOR PARMETIS 4: FOR PTSCOTCH
!| NELEM          |<--| THE NUMBER OF ELEMENTS
!| NDP            |<--| THE NUMBE OF POINT PER ELEMENT
!| NPARTS         |<--| NUMBER OF PARTITIONS
!| IKLES          |<--| CONNECTIVITY TABLE
!| EPART          |-->| PARTITION NUMBER OF AN ELEMENT
!| MYPART         |-->| PARTITION NUMBER OF A POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
#if defined HAVE_PTSCOTCH
      USE ISO_C_BINDING
#endif
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE DECLARATIONS_PARALLEL
      USE INTERFACE_PARALLEL
      IMPLICIT NONE
!
!     PTSCOTCH'S HEADER (FORTRAN)
#if defined HAVE_PTSCOTCH
      INCLUDE "ptscotchf.h"
#endif
!
      INTEGER, INTENT(IN)  :: PMETHOD
      INTEGER, INTENT(IN)  :: NELEM
      INTEGER, INTENT(IN)  :: NPOIN
      INTEGER, INTENT(IN)  :: NDP
      INTEGER, INTENT(IN)  :: IKLES(NELEM*NDP)
      INTEGER, INTENT(IN)  :: NPARTS
      INTEGER, INTENT(OUT) :: EPART(NELEM)
!TODO: Remove ??
      INTEGER, INTENT(OUT) :: NPART(NELEM)
!
!-----------------------------------------------------------------------
!
!
!
      INTEGER :: ERR, I, J, K
!
!     FOR PARMETIS
!
      INTEGER :: NCOMMONNODES
      INTEGER :: EDGECUT
      INTEGER, ALLOCATABLE :: EPTR(:), EIND(:)
      INTEGER, ALLOCATABLE :: NULLTABLE(:)
#if defined HAVE_PTSCOTCH || HAVE_PARMETIS
      INTEGER, ALLOCATABLE :: EPART_LOC(:)
      INTEGER :: NUMFLAG
      INTEGER :: NCON
      INTEGER :: NELEM_LOC
      INTEGER :: OPTIONS(0:2)
      REAL*4, ALLOCATABLE :: TPWGTS(:), UBVEC(:)
      INTEGER :: WGTFLAG
      INTEGER, ALLOCATABLE :: VWGT(:)
      INTEGER, ALLOCATABLE :: RECVCOUNT(:)
      INTEGER, ALLOCATABLE :: RECVBUF(:)
      INTEGER,ALLOCATABLE :: ELMDIST(:)
      INTEGER,ALLOCATABLE :: DISPLS(:)
#endif

#if defined HAVE_PTSCOTCH
!
      REAL*8, DIMENSION(SCOTCH_DGRAPHDIM) :: PTSCOTCHGRAPH
      REAL*8, DIMENSION(SCOTCH_STRATDIM) :: PTSCOTCHSTRAT
      TYPE(C_PTR), POINTER :: PTXADJ(:), PTADJNCY(:)
      INTEGER, POINTER :: XADJ2(:), ADJNCY2(:)
      INTEGER :: NEDGE
!
#endif
        IF(PMETHOD.EQ.3) THEN
#if defined HAVE_PARMETIS
          WRITE(LU,*) 'BEGIN PARTITIONING WITH PARMETIS'
          ! Calling parmetis metis parallel partitionner
          ALLOCATE (ELMDIST(NPARTS+1),STAT=ERR)
          CALL CHECK_ALLOCATE (ERR, 'ELMDIST')
!
!         THE NUMBER OF COMMON POINT NEEDED BETWEEN 2 ELEMENTS TO MAKE AN
!         EDGE
          IF (NDP==3.OR.NDP==6) THEN
            NCOMMONNODES = 2 ! FOR TRIANGLE OR RECTANGLE
          ELSE
            WRITE(LU,*) 'PARMETIS: IMPLEMENTED FOR ',
     &                  'TRIANGLES OR PRISMS ONLY'
            CALL PLANTE(1)
            STOP
          ENDIF
!
!         ELMDIST: THIS ARRAY DESCRIBES HOW THE ELEMENTS OF THE MESH ARE DISTRIBUTED AMONG THE PROCESSORS.
!              IT IS ANALOGOUS TO THE VTXDIST ARRAY. ITS CONTENTS ARE IDENTICAL FOR EVERY PROCESSOR.
          ELMDIST(1)=1
          DO I=1,NPARTS-1
            ELMDIST(I+1) = ELMDIST(I) + NELEM/NPARTS
          ENDDO
          ELMDIST(NPARTS+1) = NELEM + 1
!
          NELEM_LOC = ELMDIST(IPID+2) - ELMDIST(IPID+1)
!
          ALLOCATE (EPTR(NELEM_LOC+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPTR')
          ALLOCATE (EIND(NELEM_LOC*NDP),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EIND')
          ALLOCATE (EPART_LOC(NELEM_LOC),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPART_LOC')
!
!         EPTR, EIND: THESE ARRAYS SPECIFIES THE ELEMENTS THAT ARE STORED LOCALLY AT EACH PROCESSOR.
!
          DO I=1,NELEM_LOC+1
            EPTR(I) = (I-1)*NDP + 1
          ENDDO
!
          K=1
          DO I=ELMDIST(IPID+1),ELMDIST(IPID+2)-1
            DO J=1,NDP
              EIND(K) = IKLES((I-1)*NDP+J)
              K = K + 1
            ENDDO
          ENDDO
!
!         REPRESENT THE NUMBER OF PARAMETER FOR BALANCING THE PARTIRION
          NCON = 1
!
          ALLOCATE (TPWGTS(NCON*NPARTS),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'TPWGTS')
          ALLOCATE (UBVEC(NCON),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'UBVEC')
          ALLOCATE(VWGT(NELEM_LOC),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'VWGT')
!
          TPWGTS(:) = 1.D0/FLOAT(NPARTS)
          OPTIONS(:)=0
          UBVEC(:) = 1.D05
          VWGT(:) = 1
          WGTFLAG = 0
          NUMFLAG = 1
!
          CALL PARMETIS_V3_PARTMESHKWAY(ELMDIST, EPTR, EIND, VWGT,
     &                              WGTFLAG, NUMFLAG,
     &                              NCON, NCOMMONNODES, NPARTS, TPWGTS,
     &                              UBVEC, OPTIONS, EDGECUT, EPART_LOC,
     &                              COMM)
          DEALLOCATE(TPWGTS)
          DEALLOCATE(UBVEC)
          DEALLOCATE(VWGT)
          DEALLOCATE(EIND)
          DEALLOCATE(EPTR)
!
          ALLOCATE(RECVCOUNT(NPARTS),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR,'RECVCOUNT')
          ALLOCATE(DISPLS(NPARTS),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR,'RECVCOUNT')
          DO I=1,NPARTS
            RECVCOUNT(I) = ELMDIST(I+1) - ELMDIST(I)
          ENDDO
          DISPLS(1) = 0
          DO I=2,NPARTS
            DISPLS(I) = DISPLS(I-1) + RECVCOUNT(I-1)
          ENDDO
!
          CALL P_ALLGATHERV_I(EPART_LOC,NELEM_LOC,EPART,
     &                      RECVCOUNT,DISPLS, ERR)
!
          DEALLOCATE(ELMDIST)
          DEALLOCATE(DISPLS)
          DEALLOCATE(EPART_LOC)
          DEALLOCATE(RECVCOUNT)
#else
        WRITE(LU,*) "TRYING TO USE PARMETIS TO PARTIONNE WHEN PARMETIS",
     &              " IS NOT INSTALLED"
        CALL PLANTE(1)
#endif
        ELSE IF(PMETHOD.EQ.4) THEN
#if defined HAVE_PTSCOTCH
          WRITE(LU,*) 'BEGIN PARTITIONING WITH PTSCOTCH'
!
          ALLOCATE (ELMDIST(NPARTS+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'ELMDIST')
!
!!        The number of common point needed between 2 elements to make an
!         edge
          IF (NDP==3.OR.NDP==6) THEN
            NCOMMONNODES = 2 ! FOR TRIANGLE OR RECTANGLE
          ELSE
            WRITE(LU,*) 'PTSCOTCH: IMPLEMENTED FOR ',
     &                  'TRIANGLES OR PRISMS ONLY'
            CALL PLANTE(1)
            STOP
          ENDIF
!
!    ELM  DIST: THIS ARRAY DESCRIBES HOW THE ELEMENTS OF THE MESH ARE DISTRIBUTED AMONG THE PROCESSORS.
!!             IT IS ANALOGOUS TO THE VTXDIST ARRAY. ITS CONTENTS ARE IDENTICAL FOR EVERY PROCESSOR.
!              SAME PRINCIPAL AS FOR XADJ AND ADJCNY
          ELMDIST(1)=1
          DO I=1,NPARTS-1
            ELMDIST(I+1) = ELMDIST(I) + NELEM/NPARTS
          ENDDO
          ELMDIST(NPARTS+1) = NELEM + 1
!
          NELEM_LOC = ELMDIST(IPID+2) - ELMDIST(IPID+1)
!
          ALLOCATE (EPTR(NELEM_LOC+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPTR')
          ALLOCATE (EIND(NELEM_LOC*NDP),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EIND')
          ALLOCATE (EPART_LOC(NELEM_LOC),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPART_LOC')
!
!         EPTR, EIND: THESE ARRAYS SPECIFIES THE ELEMENTS THAT ARE STORED LOCALLY AT EACH PROCESSOR.
!                  (SEE DISCUSSION IN SECTION 4.3).
!
          DO I=1,NELEM_LOC+1
            EPTR(I) = (I-1)*NDP + 1
          ENDDO
!
          K=1
          DO I=ELMDIST(IPID+1),ELMDIST(IPID+2)-1
            DO J=1,NDP
              EIND(K) = IKLES((I-1)*NDP+J)
              K = K + 1
            ENDDO
          ENDDO
!
!!!! REP! RESENT THE NUMBER OF PARAMETER FOR BALANCING THE PARTIRION
          NCON = 1
          NUMFLAG = 1
!
          ALLOCATE(PTXADJ(1))
          ALLOCATE(PTADJNCY(1))
          CALL PARMETIS_V3_MESH2DUAL(ELMDIST, EPTR, EIND,
     &                             NUMFLAG, NCOMMONNODES,
     &                             PTXADJ,PTADJNCY,
     &                             COMM)
!
!          DEALLOCATE(EPTR,EIND)
          ALLOCATE(RECVBUF(1))
          RECVBUF(1) = NELEM_LOC+1
          CALL C_F_POINTER(PTXADJ(1),XADJ2,RECVBUF)
!
          NEDGE = XADJ2(NELEM_LOC+1)-1
          RECVBUF(1) = NEDGE
          CALL C_F_POINTER(PTADJNCY(1),ADJNCY2,RECVBUF)
          DEALLOCATE(RECVBUF)
!
!         BEGINNING PT-SCOTCH PARTIONNING
!
          CALL SCOTCHFSTRATINIT(PTSCOTCHSTRAT,ERR)
          IF (ERR.NE.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT INITIALIZE STRAT',ERR
            CALL PLANTE(1)
          ENDIF
!
          CALL SCOTCHFDGRAPHINIT(PTSCOTCHGRAPH,COMM,ERR)
          IF (ERR.NE.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT INITIALIZE GRAPH'
            CALL PLANTE(1)
          ENDIF
!
          CALL SCOTCHFDGRAPHBUILD ( PTSCOTCHGRAPH,     ! GRAFDAT
     &                              NUMFLAG,     ! BASEVAL
     &                              NELEM_LOC,  ! VERTLOCNBR
     &                              NELEM_LOC,  ! VERTLOCMAX=VERLOCNBR (NO HOLES IN GLOBAL
                                    ! NUMBERING)
     &                              XADJ2(1:NELEM_LOC),  ! VERTLOCTAB
     &                              XADJ2(2:NELEM_LOC+1),  ! VENDLOCTAB = NULL
     &                              XADJ2,  ! VELOLOCTAB = NULL
     &                              XADJ2,  ! VLBLLOCTAB = NULL
     &                              XADJ2(NELEM_LOC+1)-1,  ! EDGELOCNBR
     &                              XADJ2(NELEM_LOC+1)-1,  ! EDGELOCSIZ
     &                              ADJNCY2,  ! EDGELOCATAB
     &                              ADJNCY2,  ! EDGEGSTTAB = NULL
     &                              ADJNCY2,  ! EDLOLOCTAB = NULL
     &                              ERR)
          IF (ERR.NE.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT BUILD GRAPH'
            CALL PLANTE(1)
          ENDIF
!
          CALL SCOTCHFDGRAPHCHECK(PTSCOTCHGRAPH,ERR)
          IF (ERR.NE.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: GRAPH NOT CONSISTANT'
            CALL PLANTE(1)
          ENDIF
!
          CALL SCOTCHFDGRAPHPART ( PTSCOTCHGRAPH,
     &                            NPARTS,
     &                            PTSCOTCHSTRAT,
     &                            EPART_LOC,
     &                            ERR )
!
          IF (ERR.NE.0) THEN
            WRITE(LU,*) 'PTSCOTCH ERROR: CANNOT PARTITION GRAPH'
            CALL PLANTE(1)
          END IF
!!!! CHANGING EPART NUMBERING TO 1-NPART
          DO I=1,NELEM_LOC
            EPART_LOC(I) = EPART_LOC(I) + 1
          ENDDO
!
          CALL SCOTCHFGRAPHEXIT(PTSCOTCHGRAPH)
!
          CALL SCOTCHFSTRATEXIT(PTSCOTCHSTRAT)
!
!         Computing epart
          ALLOCATE(RECVCOUNT(NPARTS),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR,'RECVCOUNT')
          ALLOCATE(DISPLS(NPARTS),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR,'RECVCOUNT')
          DO I=1,NPARTS
            RECVCOUNT(I) = ELMDIST(I+1) - ELMDIST(I)
          ENDDO
          DISPLS(1) = 0
          DO I=2,NPARTS
            DISPLS(I) = DISPLS(I-1) + RECVCOUNT(I-1)
          ENDDO
!
          CALL P_ALLGATHERV_I(EPART_LOC,NELEM_LOC,EPART,
     &                      RECVCOUNT,DISPLS,ERR)
!          DEALLOCATE(XADJ2)
!          DEALLOCATE(ADJNCY2)
          DEALLOCATE(PTXADJ)
          DEALLOCATE(PTADJNCY)
          DEALLOCATE(ELMDIST)
          DEALLOCATE(EPART_LOC)
#else
          WRITE(LU,*) "TRYING TO PARTITIONNE WITH PTSCOTCH WHEN",
     &                "PTSCOTCH IS NOT INSTALLED"
          CALL PLANTE(1)
#endif
        ELSEIF(PMETHOD.EQ.1) THEN
          WRITE(LU,*) 'BEGIN PARTITIONING WITH METIS'
          ALLOCATE (EPTR(NELEM+1),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EPTR')
          ALLOCATE (EIND(NELEM*NDP),STAT=ERR)
          CALL CHECK_ALLOCATE(ERR, 'EIND')
!
          DO I=1,NELEM+1
            EPTR(I) = (I-1)*NDP + 1
          ENDDO
!
          K=1
          DO I=1,NELEM
            DO J=1,NDP
              EIND(K) = IKLES((I-1)*NDP+J)
              K = K + 1
            ENDDO
          ENDDO
!
!         SWITCH TO C NUMBERING
          EIND = EIND -1
          EPTR = EPTR -1
!
!         METIS REQUIRES THE NUMBER OF COMMON POINT NEEDED BETWEEN 2 ELEMENTS TO MAKE AN EDGE
!         NCOMMONNODES = 2   FOR TRIANGLE OR RECTANGLE
!         NCOMMONNODES = 3   FOR TETRAHEDRE
!         NCOMMONNODES = 4   FOR HEXAHEDRE

!
          IF (NDP==3.OR.NDP==6) THEN
            NCOMMONNODES = 2 ! FOR TRIANGLE OR RECTANGLE
          ELSE
          WRITE(LU,*) 'METIS: IMPLEMENTED FOR TRIANGLES OR PRISMS ONLY'
            CALL PLANTE(1)
            STOP
          ENDIF

!         WE ONLY USE METIS_PARTMESHDUAL AS ONLY THE FINITE ELEMENTS PARTITION
!         IS RELEVANT HERE.
!
#if defined HAVE_MPI
            CALL METIS_PARTMESHDUAL
     &          (NELEM, NPOIN, EPTR, EIND, NULLTABLE,
     &           NULLTABLE, NCOMMONNODES, NPARTS, NULLTABLE,
     &           NULLTABLE, EDGECUT, EPART, NPART)
#else
        WRITE(LU,*) 'ERROR: TRY TO RUN PARTEL_PARA WITH A '//
     &              'SERIAL CONFIGURATION'
        CALL PLANTE(1)
#endif
!
!
!         DEALLOCATING TEMPORARY ARRAYS FOR METIS
!         SWITCHING EPART TO FORTRAN NUMBERING (1...N)
          EPART = EPART+1
          DEALLOCATE(EPTR)
          DEALLOCATE(EIND)
!
        ENDIF
      END SUBROUTINE PARTITIONER_PARA

