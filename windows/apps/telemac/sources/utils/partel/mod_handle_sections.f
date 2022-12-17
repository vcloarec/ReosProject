!     **************************
      MODULE MOD_HANDLE_SECTIONS
!     **************************
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
!     **************************
      SUBROUTINE HANDLE_SECTIONS
!     **************************
!
     &        (NAMESEC, NPARTS, NELEM, NDP, IKLE, NPOIN, F, KNOGL)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMESEC        |<--| Name of sections file
!| NPARTS         |<--| Number of partitions
!| NELEM          |<--| Number of elements
!| NDP            |<--| Number of points per element
!| IKLE           |<->| Connectiviy array
!| NPOIN          |<--| Number of points
!| F              |<->| Coordinates
!| KNOGL          |<--| Global to local numbering
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_PARTEL
      USE MOD_HASH_TABLE
      USE BIEF, ONLY : CHAIN_TYPE,NBMAXNSHARE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NAMESEC
      INTEGER, INTENT(IN) :: NELEM, NDP
      INTEGER, INTENT(IN) :: IKLE(NELEM,NDP)
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: NPARTS
      DOUBLE PRECISION, INTENT(IN) :: F(NPOIN,2)
      TYPE(HASH_TABLE), INTENT(IN) :: KNOGL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTEGER NSCT
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
      INTEGER, PARAMETER :: NSEMAX=500 ! MAX NUMBER OF SEGMENTS IN A SECTION
      INTEGER, ALLOCATABLE :: LISTE(:,:), ANPBEG(:),ANPEND(:)
      INTEGER :: NSEC, IHOWSEC, ISEC, IELEM, IM(1), IN(1), NPBEG, NPEND
      INTEGER :: PT, I1,I2,I3, ARR,DEP, ILPREC,ILBEST,ELBEST,IGBEST
      DOUBLE PRECISION :: XA, YA, DISTB, DISTE, DMINB, DMINE
      DOUBLE PRECISION :: DIST1, DIST2, DIST3, DIST
      LOGICAL FOUND
      INTEGER :: I, IERR, ISEG, J, K, M, N
      INTEGER :: TEMP
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS

      CHARACTER(LEN=PATH_LEN) :: NAMEOUT
!
      CALL GET_FREE_ID(NSCT)
      OPEN (NSCT,FILE=TRIM(NAMESEC),FORM='FORMATTED',STATUS='OLD')
      READ (NSCT,*) ! COMMENT LINE
      READ (NSCT,*) NSEC, IHOWSEC
      IF (.NOT.ALLOCATED(CHAIN)) ALLOCATE (CHAIN(NSEC))
      IF (IHOWSEC<0) THEN
        DO ISEC=1,NSEC
          READ (NSCT,*) CHAIN(ISEC)%DESCR
          READ (NSCT,*) CHAIN(ISEC)%NPAIR(:)
          CHAIN(ISEC)%XYBEG(1)=F(CHAIN(ISEC)%NPAIR(1),1)
          CHAIN(ISEC)%XYBEG(2)=F(CHAIN(ISEC)%NPAIR(1),2)
          CHAIN(ISEC)%XYEND(1)=F(CHAIN(ISEC)%NPAIR(2),1)
          CHAIN(ISEC)%XYEND(2)=F(CHAIN(ISEC)%NPAIR(2),2)
          WRITE(LU,*) 'SECTION ',CHAIN(ISEC)%DESCR
          WRITE(LU,*) 'BEGINS AT X=',CHAIN(ISEC)%XYBEG(1),
     &                         ' Y=',CHAIN(ISEC)%XYBEG(2)
          WRITE(LU,*) 'ENDS   AT X=',CHAIN(ISEC)%XYEND(1),
     &                         ' Y=',CHAIN(ISEC)%XYEND(2)
        ENDDO
      ELSE
        DO ISEC=1,NSEC
          READ (NSCT,*) CHAIN(ISEC)%DESCR
          READ (NSCT,*)
     &    ( CHAIN(ISEC)%XYBEG(I), I = 1, SIZE(CHAIN(ISEC)%XYBEG(:),1) ),
     &    ( CHAIN(ISEC)%XYEND(I), I = 1, SIZE(CHAIN(ISEC)%XYEND(:),1) )
          CHAIN(ISEC)%NPAIR(:)=0
        ENDDO
      ENDIF
      CLOSE(NSCT)
!
!     IF TERMINAL POINTS GIVEN BY COORDINATES, FIND NEAREST NODES FIRST
!
      WRITE(LU,*) 'NPOIN:',NPOIN
      IF(IHOWSEC.GE.0) THEN
        DO ISEC=1,NSEC
          XA=F(1,1)
          YA=F(1,2)
          DMINB = (CHAIN(ISEC)%XYBEG(1)-XA)**2
     &          + (CHAIN(ISEC)%XYBEG(2)-YA)**2
          DMINE = (CHAIN(ISEC)%XYEND(1)-XA)**2
     &          + (CHAIN(ISEC)%XYEND(2)-YA)**2
          CHAIN(ISEC)%NPAIR(1)=1
          CHAIN(ISEC)%NPAIR(2)=1
          DO I=2,NPOIN ! COMPUTATIONALLY INTENSIVE
            XA=F(I,1)
            YA=F(I,2)
            DISTB = (CHAIN(ISEC)%XYBEG(1)-XA)**2
     &            + (CHAIN(ISEC)%XYBEG(2)-YA)**2
            DISTE = (CHAIN(ISEC)%XYEND(1)-XA)**2
     &            + (CHAIN(ISEC)%XYEND(2)-YA)**2
            IF ( DISTB < DMINB ) THEN
              CHAIN(ISEC)%NPAIR(1)=I
              DMINB=DISTB
            ENDIF
            IF ( DISTE < DMINE ) THEN
              CHAIN(ISEC)%NPAIR(2)=I
              DMINE=DISTE
            ENDIF
          ENDDO
          WRITE(LU,'(A,3(1X,I9))')
     &          ' -> SECTION, TERMINAL NODES: ',
     &          ISEC, CHAIN(ISEC)%NPAIR(:)
        ENDDO
      ELSE
        DO ISEC=1,NSEC
          WRITE(LU,'(A,1X,I9,4(1X,1PG13.6))')
     &          ' -> SECTION, TERMINAL COORDINATES: ', ISEC,
     &          CHAIN(ISEC)%XYBEG, CHAIN(ISEC)%XYEND
        ENDDO
      ENDIF
!
      WRITE(LU,*) 'NSEC,IHOWSEC: ',NSEC,IHOWSEC
      WRITE(LU,*) 'ANTICIPATED SECTIONS SUMMARY:'
      DO ISEC=1,NSEC
        WRITE(LU,*) CHAIN(ISEC)%DESCR
        WRITE(LU,*) CHAIN(ISEC)%XYBEG(:), CHAIN(ISEC)%XYEND(:)
        WRITE(LU,*) CHAIN(ISEC)%NPAIR(:)
      ENDDO
!
!     NOW FOLLOW THE FLUSEC SUBROUTINE IN BIEF TO FIND SECTIONS
!     IN THE GLOBAL MESH -> FILL THE FIELD LISTE
!
      ALLOCATE(LISTE(NSEMAX,2),STAT=IERR) ! WORKHORSE
      CALL CHECK_ALLOCATE(IERR, 'LISTE')
!
      DO ISEC =1,NSEC
!
        DEP = CHAIN(ISEC)%NPAIR(1)
        ARR = CHAIN(ISEC)%NPAIR(2)
!
        PT = DEP
        ISEG = 0
        DIST=(F(DEP,1)-F(ARR,1))**2+(F(DEP,2)-F(ARR,2))**2
!
 1010   CONTINUE ! A JUMP POINT
!
        DO IELEM =1,NELEM
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          IF (PT.EQ.I1.OR.PT.EQ.I2.OR.PT.EQ.I3) THEN
            DIST1 = (F(I1,1)-F(ARR,1))**2 + (F(I1,2)-F(ARR,2))**2
            DIST2 = (F(I2,1)-F(ARR,1))**2 + (F(I2,2)-F(ARR,2))**2
            DIST3 = (F(I3,1)-F(ARR,1))**2 + (F(I3,2)-F(ARR,2))**2
            IF (DIST1.LT.DIST) THEN
              DIST = DIST1
              ELBEST = IELEM
              IGBEST = I1
              ILBEST = 1
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF (DIST2.LT.DIST) THEN
              DIST = DIST2
              ELBEST = IELEM
              IGBEST = I2
              ILBEST = 2
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF(DIST3.LT.DIST) THEN
              DIST = DIST3
              ELBEST = IELEM
              IGBEST = I3
              ILBEST = 3
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
          ENDIF
!
        END DO ! OVER ELEMENTS
!
        IF (IGBEST.EQ.PT) THEN
          WRITE(LU,*)'FLUSEC : ALGORITHM FAILED'
          CALL PLANTE(1)
          STOP
        ELSE
          PT = IGBEST
          ISEG = ISEG + 1
          IF (ISEG.GT.NSEMAX) THEN
            WRITE(LU,*) 'TOO MANY SEGMENTS IN A   '
            WRITE(LU,*) 'SECTION. INCREASE  NSEMAX'
            CALL PLANTE(1)
            STOP
          ENDIF
          LISTE(ISEG,1) = IKLE(ELBEST,ILPREC)
          LISTE(ISEG,2) = IKLE(ELBEST,ILBEST)
          IF (IGBEST.NE.ARR) GOTO 1010
        ENDIF
        CHAIN(ISEC)%NSEG = ISEG
        ALLOCATE (CHAIN(ISEC)%LISTE(CHAIN(ISEC)%NSEG,3), STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'CHAIN(ISEC)%LISTE')
        DO ISEG=1,CHAIN(ISEC)%NSEG
          CHAIN(ISEC)%LISTE(ISEG,1)=LISTE(ISEG,1)
          CHAIN(ISEC)%LISTE(ISEG,2)=LISTE(ISEG,2)
          CHAIN(ISEC)%LISTE(ISEG,3)=-1 ! INITIALISE... FOR DEVEL
        END DO
      ENDDO ! OVER SECTIONS
      DEALLOCATE (LISTE)
!
      ALLOCATE (ANPBEG(NBMAXNSHARE), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'ANPBEG')
      ALLOCATE (ANPEND(NBMAXNSHARE), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'ANPEND')
!
      DO ISEC=1,NSEC
        DO ISEG=1,CHAIN(ISEC)%NSEG
!
          NPBEG=0
          DO M=1,NPARTS
            IF(HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,1),M)>0)THEN
              NPBEG=NPBEG+1
            END IF
          END DO

          NPEND=0
          DO M=1,NPARTS
            IF(HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,2),M)>0)THEN
                    NPEND=NPEND+1
            END IF
          END DO
!
          IF (NPBEG>NBMAXNSHARE .OR. NPEND>NBMAXNSHARE) THEN
            WRITE(LU,*) 'NPBEG OR NPEND: ',NPBEG,NPEND
            WRITE(LU,*) 'ARE LARGER THAN NBMAXNSHARE: ',NBMAXNSHARE
            CALL PLANTE(1)
            STOP
          ENDIF
!
          ! THE NICE AND USUAL CASE WHEN BOTH SEGMENT ENDS
          ! BELONG TO ONE SUBDOMAIN - ONLY ONE POSITION IN KNOGL
          IF ( NPBEG==1 .AND. NPEND==1) THEN
            M=0
            DO K=1,NPARTS
              TEMP=HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,1),K)
              IF(M<TEMP)THEN
                IM(:)=K
                M=TEMP
              END IF
            END DO

            !IM(:) = MAXLOC (
     &      !  HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,1),:) )
            !IN(:) = MAXLOC (
     &       ! HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,2),:) )
            M=0
            DO K=1,NPARTS
              TEMP=HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,2),K)
              IF(M<TEMP)THEN
                IN(:)=K
                M=TEMP
              END IF
            END DO

            IF (IM(1)==IN(1)) THEN
              CHAIN(ISEC)%LISTE(ISEG,3)=IM(1)
            ELSE ! THEY BELONG TO DIFFERENT SUBDOMAINS? HOW COME?
              WRITE(LU,*) 'IMPOSSIBLE CASE (1) BY SECTIONS'
              CALL PLANTE(1)
              STOP
            ENDIF
            ! AT LEAST ONE OF THE TERMINAL NODES IS ON THE INTERFACE
            ! TAKE THE LARGEST COMMON PARTITION NUMBER THEY BOTH BELONG TO
          ELSE
            IF (NPBEG==1 .AND. NPEND>1) THEN ! THE SEGMENT'S END TOUCHES THE INTERFACE
              M=0
              DO K=1,NPARTS
              TEMP=HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,1),K)
                IF(M<TEMP)THEN
                  IM(:)=K
                  M=TEMP
                END IF
              END DO
              !IM(:) = MAXLOC (
     &        !  HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,1),:) )
              IF (HASH_TABLE_GET(KNOGL,
     &            CHAIN(ISEC)%LISTE(ISEG,2),IM(1))>0 ) THEN
                CHAIN(ISEC)%LISTE(ISEG,3) = IM(1)
              ELSE
                WRITE(LU,*) 'IMPOSSIBLE CASE (2) BY SECTIONS'
                CALL PLANTE(1)
                STOP
              ENDIF
            ELSE IF (NPBEG>1 .AND. NPEND==1) THEN ! THE SEGMENT'S BEG. TOUCHES THE INTERFACE
              M=0
              DO K=1,NPARTS
              TEMP=HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,2),K)
                IF(M<TEMP)THEN
                  IN(:)=K
                  M=TEMP
                END IF
              END DO
              !IN(:) = MAXLOC (
     &        !  HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,2),:) )
              IF ( HASH_TABLE_GET(KNOGL,
     &            CHAIN(ISEC)%LISTE(ISEG,1),IN(1))>0 ) THEN
                CHAIN(ISEC)%LISTE(ISEG,3) = IN(1)
              ELSE
                WRITE(LU,*) 'IMPOSSIBLE CASE (3) BY SECTIONS'
                CALL PLANTE(1)
                STOP
              ENDIF
            ELSE ! I.E. (NPBEG>1 .AND. NPEND>1) - LIES JUST ON THE INTERFACE OR "A SHORTCUT"
              ANPBEG=0
              ANPEND=0
              I=0
              DO N=1,NPARTS
                IF ( HASH_TABLE_GET(KNOGL,
     &              CHAIN(ISEC)%LISTE(ISEG,1),N)>0 ) THEN
                  I=I+1
                  ANPBEG(I)=N
                ENDIF
              END DO
              IF (I/=NPBEG) WRITE(LU,*) 'OH! I/=NPBEG'
              I=0
              DO N=1,NPARTS
                IF ( HASH_TABLE_GET(KNOGL,
     &              CHAIN(ISEC)%LISTE(ISEG,2),N)>0 ) THEN
                  I=I+1
                  ANPEND(I)=N
                ENDIF
              END DO
              IF (I/=NPEND) WRITE(LU,*) 'OH! I/=NPEND'
!
              WRITE(LU,*) 'ANPBEG: ',ANPBEG
              WRITE(LU,*) 'ANPEND: ',ANPEND
!
              FOUND=.FALSE.
              DO I=NPBEG,1,-1
                DO J=NPEND,1,-1
                  IF (ANPBEG(I)==ANPEND(J)) THEN
                    CHAIN(ISEC)%LISTE(ISEG,3) = ANPBEG(I)
                    FOUND=.TRUE.
                    EXIT
                  ENDIF
                END DO
                IF (FOUND) EXIT
              END DO
              IF (.NOT.FOUND) THEN
                WRITE(LU,*) 'BY SECTION WITH NODES: ',
     &            CHAIN(ISEC)%LISTE(ISEG,1),CHAIN(ISEC)%LISTE(ISEG,2)
                WRITE(LU,*) 'IMPOSSIBLE CASE (4) BY SECTIONS'
                CALL PLANTE(1)
                STOP
              ENDIF

            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
      DEALLOCATE (ANPBEG,ANPEND)
!
! WRITE FILES
!
      DO N=1,NPARTS
        NAMEOUT=TRIM(NAMESEC)//EXTENS(NPARTS-1,N-1)

        WRITE(LU,*) 'WRITING: ', TRIM(NAMEOUT)

        OPEN(NSCT,FILE=TRIM(NAMEOUT),FORM='FORMATTED',STATUS='UNKNOWN')
        REWIND(NSCT)
        WRITE(NSCT,*) '# SECTIONS PARTITIONED FOR ',
     &   EXTENS(NPARTS-1,N-1)
        WRITE(NSCT,*) NSEC, 1
        DO ISEC=1,NSEC
          WRITE(NSCT,*) TRIM(CHAIN(ISEC)%DESCR)
          I=COUNT(CHAIN(ISEC)%LISTE(:,3)==N)
          WRITE(NSCT,*) I
          DO ISEG=1,CHAIN(ISEC)%NSEG
            IF (CHAIN(ISEC)%LISTE(ISEG,3)==N) THEN
              WRITE(NSCT,*)
     &          HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,1),N),
     &          HASH_TABLE_GET(KNOGL,CHAIN(ISEC)%LISTE(ISEG,2),N)
            ENDIF
          END DO
        END DO
        CLOSE(NSCT)
      END DO
!
      END SUBROUTINE
      END MODULE
