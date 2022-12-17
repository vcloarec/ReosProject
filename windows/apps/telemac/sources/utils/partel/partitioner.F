!                   **************
                    SUBROUTINE PARTITIONER
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
!| PMETHOD        |<--| 1: FOR METIS 2: FOR SCOTCH
!| NELEM          |<--| THE NUMBER OF ELEMENTS
!| NDP            |<--| THE NUMBE OF POINT PER ELEMENT
!| NPARTS         |<--| NUMBER OF PARTITIONS
!| IKLES          |<--| CONNECTIVITY TABLE
!| EPART          |-->| PARTITION NUMBER OF AN ELEMENT
!| MYPART         |-->| PARTITION NUMBER OF A POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE ISO_C_BINDING
!
      USE DECLARATIONS_SPECIAL
      USE C_BINDING
      IMPLICIT NONE
#if defined HAVE_SCOTCH
      INCLUDE 'scotchf.h'
#endif
!
      INTEGER, INTENT(IN)  :: PMETHOD
      INTEGER, INTENT(IN)  :: NELEM
      INTEGER, INTENT(IN)  :: NPOIN
      INTEGER, INTENT(IN)  :: NDP
      INTEGER, INTENT(IN)  :: IKLES(NELEM*NDP)
      INTEGER, INTENT(IN)  :: NPARTS
      INTEGER, INTENT(OUT) :: EPART(NELEM)
      INTEGER, INTENT(OUT) :: NPART(NPOIN)
!
!-----------------------------------------------------------------------
!
!
!
      INTEGER I,J,K,ERR,NUMFLAG,EDGECUT,NCOMMONNODES
      INTEGER, ALLOCATABLE :: EPTR(:), EIND(:)
!
! SCOTCH
!
#if defined HAVE_SCOTCH
      REAL*8, DIMENSION(SCOTCH_GRAPHDIM) :: SCOTCHGRAPH
      REAL*8, DIMENSION(SCOTCH_STRATDIM) :: SCOTCHSTRAT
      TYPE(C_PTR), POINTER :: PTXADJ(:), PTADJNCY(:)
      INTEGER, ALLOCATABLE :: RECVBUF(:)
      INTEGER, POINTER :: XADJ2(:), ADJNCY2(:)
#endif

      IF(PMETHOD.EQ.1) THEN
        WRITE(LU,*) 'BEGIN PARTITIONING WITH METIS'
!       NEW METIS INTERFACE (>= VERSION 5) :
!
!       EPTR, EIND: THESE ARRAYS SPECIFIES THE ELEMENTS
!       THAT ARE STORED LOCALLY AT EACH PROCESSOR.
!       CF. DISCUSSION IN SECTION 4.3 (?)
!
        ALLOCATE(EPTR(NELEM+1),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'EPTR')
        ALLOCATE(EIND(NELEM*NDP),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'EIND')
!
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
!       END OF CORRECTION JMH 04/07/2012
!
!       SWITCH TO C NUMBERING
        EIND = EIND -1
        EPTR = EPTR -1
!
!       METIS REQUIRES THE NUMBER OF COMMON POINT NEEDED BETWEEN 2 ELEMENTS TO MAKE AN EDGE
!       NCOMMONNODES = 2   FOR TRIANGLE OR RECTANGLE
!       NCOMMONNODES = 3   FOR TETRAHEDRE
!       NCOMMONNODES = 4   FOR HEXAHEDRE

!
        IF (NDP==3.OR.NDP==6) THEN
          NCOMMONNODES = 2 ! FOR TRIANGLE OR PRISM
        ELSE IF(NDP==4) THEN
          NCOMMONNODES = 3 ! FORT TETRAHEDRON
        ELSE
          WRITE(LU,*) 'METIS: IMPLEMENTED FOR TRIANGLES OR PRISMS ONLY'
          CALL PLANTE(1)
        ENDIF
!
!       WE ONLY USE METIS_PARTMESHDUAL AS ONLY THE FINITE ELEMENTS PARTITION
!       IS RELEVANT HERE.
!
!       IMPORTANT: WE USE FORTRAN-LIKE FIELD ELEMENTS NUMBERING 1...N
!       IN C VERSION, 0...N-1 NUMBERING IS APPLIED!!!
!
        NUMFLAG = 1
!
#if defined HAVE_MPI
        CALL MYMETIS_PARTMESHDUAL
     &        (NELEM, NPOIN, EPTR, EIND,
     &          NCOMMONNODES, NPARTS,
     &          EDGECUT, EPART, NPART)
#else
        WRITE(LU,*) 'ERROR: TRY TO RUN PARTEL WITH A '//
     &              'SERIAL CONFIGURATION'
        CALL PLANTE(1)
#endif
!
!       DEALLOCATING TEMPORARY ARRAYS FOR METIS
!
!       EPART IS AN ARRAY
        EPART = EPART+1
        DEALLOCATE(EPTR)
        DEALLOCATE(EIND)
      ELSE IF(PMETHOD.EQ.2) THEN
#if defined HAVE_SCOTCH
!
!       SCOTCH PARTITIONER
!
        WRITE(LU,*) 'BEGIN PARTITIONING WITH SCOTCH'
        ALLOCATE (EPTR(NELEM+1),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,"EPTR")
        ALLOCATE (EIND(NELEM*NDP),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,"EIND")
!
!       bUILDING EPTR AND EIND SAME AS BEFORE.
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
!       THE NUMBER OF COMMON POINT NEEDED BETWEEN 2 ELEMENTS TO MAKE AN
!       EDGE
!       NCOMMONNODES = 2 FOR TRIANGLE OR RECTANGLE
!       NCOMMONNODES = 3 FOR TETRAHEDRE
!       NCOMMONNODES = 4 FOR HEXAHEDRE
        IF (NDP==3.OR.NDP==6) THEN
          NCOMMONNODES = 2 ! FOR TRIANGLE OR RECTANGLE
        ELSE
          WRITE(LU,*)'SCOTCH: IMPLEMENTED FOR TRIANGLES OR PRISMS ONLY'
          CALL PLANTE(1)
        ENDIF

!
!       mETIS_MESHTODUAL USES POINTER OF POINTER (PTXADJ, PTADJNCY) SO IN ORDER TO USE THAT
!       IN FORTRAN WE USE THE ISO_BINDING MODULE
!
        ALLOCATE(PTXADJ(1))
        ALLOCATE(PTADJNCY(1))
!
!       BUILD THE DUAL GRAPH BY CALLING THE METIS SUBROUTINE
!
        NUMFLAG = 1
        CALL METIS_MESHTODUAL
     &       (NELEM, NPOIN, EPTR, EIND, NCOMMONNODES, NUMFLAG,
     &        PTXADJ, PTADJNCY)

!
!       TRANSFORM THE C POINTER OF POINTER BACK TO A FORTRAN ARRAY
!
        ALLOCATE(RECVBUF(1))
        RECVBUF(1) = NELEM+1
        CALL C_F_POINTER(PTXADJ(1),XADJ2,RECVBUF)
!
        RECVBUF(1) = XADJ2(NELEM+1)-1
        CALL C_F_POINTER(PTADJNCY(1),ADJNCY2,RECVBUF)
        DEALLOCATE(RECVBUF)
!
!       INITIALIZE THE GRAPH AND THE STRATEGY USED BY SCOTCH FOR THE
!       PARTITIONING WE USE THE DEFAULT STRATEGY
!
        CALL SCOTCHFSTRATINIT(SCOTCHSTRAT, ERR)
        IF (ERR.NE.0) THEN
          WRITE(LU,*) 'SCOTCH ERROR: CANNOT INITIALIZE STRAT'
          CALL PLANTE(1)
        ENDIF
!
        CALL SCOTCHFGRAPHINIT(SCOTCHGRAPH, ERR)
        IF (ERR.NE.0) THEN
          WRITE(LU,*) 'SCOTCH ERROR: CANNOT INITIALIZE GRAPH'
          CALL PLANTE(1)
        ENDIF
!
        CALL SCOTCHFGRAPHBUILD ( SCOTCHGRAPH,         ! GRAFDAT
     &                           NUMFLAG,             ! BASEVAL
     &                           NELEM,              ! VERTNBR
     &                           XADJ2(1:NELEM),     ! VERTTAB
     &                           XADJ2(2:NELEM+1),   ! VENDTAB
     &                           XADJ2(1:NELEM),     ! VELOTAB, VERTEX WEIGHTS (nULL)
     &                           XADJ2(1:NELEM),     ! VLBLTAB, VERTEX LABELS (nULL)
     &                           XADJ2(NELEM+1)-1,   ! EDGENBR
     &                           ADJNCY2,             ! EDGETAB
     &                           ADJNCY2,             ! EDLOTAB (nULL)
     &                           ERR )
!
        IF (ERR.NE.0) THEN
          WRITE(LU,*) 'SCOTCH ERROR: CANNOT BUILD GRAPH'
          CALL PLANTE(1)
        ENDIF
!
        CALL SCOTCHFGRAPHCHECK(SCOTCHGRAPH,ERR)
        IF (ERR.NE.0) THEN
          WRITE(LU,*) 'SCOTCH ERROR: GRAPH NOT CONSISTANT'
          CALL PLANTE(1)
        ENDIF
!
        ! RUN PARTITIONING
        CALL SCOTCHFGRAPHPART ( SCOTCHGRAPH,
     &                          NPARTS,
     &                          SCOTCHSTRAT,
     &                          EPART,
     &                          ERR )
!
        IF (ERR.NE.0) THEN
          WRITE(LU,*) 'SCOTCH ERROR: CANNOT PARTITION GRAPH'
          CALL PLANTE(1)
        END IF
!!!! CHANGING EPART NUMBERING TO 1-NPART
        EPART = EPART + 1

        CALL SCOTCHFGRAPHEXIT (SCOTCHGRAPH)
        CALL SCOTCHFSTRATEXIT (SCOTCHSTRAT)

!
!        THOSE DEALLOCATION CAN GENERATE ERROR WITH SOME COMPILER
!        DEALLOCATE(XADJ2)
!        DEALLOCATE(ADJNCY2)
        DEALLOCATE(PTXADJ)
        DEALLOCATE(PTADJNCY)
        DEALLOCATE(EIND)
        DEALLOCATE(EPTR)
#else
        WRITE(LU,*) "TRYING TO USE SCOTCH TO PARTIONNE WHEN SCOTCH",
     &              " IS NOT INSTALLED"
        CALL PLANTE(1)
#endif
        ENDIF
      END SUBROUTINE PARTITIONER

