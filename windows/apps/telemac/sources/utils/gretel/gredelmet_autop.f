!                   ***********************
                    PROGRAM GREDELMET_AUTOP
!                   ***********************
!
!
!***********************************************************************
! PARALLEL   V7P0                                   27/03/2014
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION (COUPLING
!+                WITH DELWAQ) TO WRITE A SINGLE FILE IN DELWAQ FORMAT.
!
!history  JAJ
!+        2001/2
!+
!+   SLIGHTLY CHANGED TO DEAL WITH:
!
!history  HW, BAW-HAMBURG
!+        20/02/2003
!+
!+   IMPROVED READING OF DATASETS
!
!history  JAJ
!+        14/03/2003
!+
!+   ADDED EXIT CODES
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/03/2014
!+        V7P0
!+   Calls of stoseg and elebd modified.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY : NCSIZE
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      CHARACTER(LEN=30) GEO
!
      INTEGER ERR
      INTEGER NELEM,ECKEN,NDUM,I,J,K,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2
      INTEGER NPROC
      INTEGER I_S, I_SP, I_LEN
      INTEGER IDUM, NPTFR
      INTEGER IELM,NELEM2,NELMAX2,NPTFR2,NSEG2,KLOG
      INTEGER ISEG
      INTEGER IELEM,ND1,ND2,ND3,MBND,IFROM,ITO,IFRM1,ITOP1,KNOLG(1)
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,IPOBO,NOQ,NSEG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESA
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NACHB,IFANUM
      INTEGER, DIMENSION(:),   ALLOCATABLE :: ISEGF
!
!
      REAL   , DIMENSION(:)  , ALLOCATABLE :: XORIG,YORIG
      DOUBLE PRECISION, DIMENSION(:)  , ALLOCATABLE :: AREA
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: LENGTH
!
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLE       ! IKLE(SIZIKL,*) OU IKLE(NELMAX,*)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IFABOR     ! IFABOR(NELMAX,*) OU IFABOR(NELMAX2,*)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NVOIS,IADR ! NVOIS(NPOIN),IADR(NPOIN)
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NELBOR,LIHBOR      ! NELBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NULONE             ! NULONE(NPTFR,2) OU NULONE(NPTFR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KP1BOR             ! KP1BOR(NPTFR,2) OU KP1BOR(NPTFR)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR               ! NBOR(*)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLBOR             ! IKLBOR(NPTFR,2)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: T3                 ! T3(NPOIN)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR0,LIHBOR0      ! NBOR0(NPTFR),LIHBOR0(NPTFR)
!
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: GLOSEG         ! GLOSEG(MAXSEG,2)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ELTSEG,ORISEG  ! ELTSEG(NELMAX,*),ORISEG(NELMAX,3)
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NODENRS
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: IFROM1,ITOPL1
!
      REAL RDUM
      DOUBLE PRECISION X2,X3,Y2,Y3,SURFACC,DX,DY
!
      LOGICAL IS
!
      CHARACTER(LEN=30) RES
      CHARACTER(LEN=50) RESPAR
      CHARACTER(LEN=11) EXTENS
      CHARACTER(LEN=30) CONLIM
      CHARACTER(LEN=7)  FILETYPE
      EXTERNAL    EXTENS
      INTRINSIC MAXVAL
!
      LI=5
      LU=6
      LNG=2
!HW
!JAJ INTRODUCE YOURSELF WITH THE RELEASE DATE
!
      WRITE(LU,*) 'I AM GREDELELMET, COUSIN OF GRETEL FROM BAW HAMBURG'
      WRITE(LU,*)
!
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
!      REWIND(LI)
      READ(LI,*) GEO
      WRITE(LU,*) GEO
!
! READS FILENAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
!
      WRITE (LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
      WRITE(LU,*) RES
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE(LU,*) NPROC
      INQUIRE (FILE=GEO,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', GEO
        CALL PLANTE(1)
        STOP
      END IF
!
      I_S  = LEN (RES)
      I_SP = I_S + 1
      DO I=1,I_S
        IF(RES(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
!
!     GEOMETRY FILE, READ UNTIL 10 PARAMETERS:
!
      OPEN(2,FILE=GEO,FORM='UNFORMATTED',STATUS='OLD',ERR=990)
      READ(2,ERR=990)
      READ(2,ERR=990) NBV1,NBV2
      DO I=1,NBV1+NBV2
        READ(2,ERR=990)
      ENDDO ! I
      GO TO 992
990   WRITE(LU,*) 'ERROR WHEN OPENING OR READING FILE: ',GEO
      CALL PLANTE(1)
      STOP
992   CONTINUE
!     READS THE 10 PARAMETERS AND THE DATE
      READ(2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(2) (PARAM(I),I=1,6)
!
!     RESULTS FILE:
!
      OPEN(3,FILE=RES,FORM='UNFORMATTED',ERR=991)
      GO TO 993
991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RES
      CALL PLANTE(1)
      STOP
993   CONTINUE
!
!     1) READS THE BEGINNING OF THE FIRST RESULTS FILE
!
!CC      RESPAR=RES // EXTENS(2**IDIMS-1,0)
!
      RESPAR=RES(1:I_LEN) // EXTENS(NPROC-1,0)
!
      INQUIRE (FILE=RESPAR,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RESPAR
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL PLANTE(1)
        STOP
      END IF
!
      OPEN(4,FILE=RESPAR,FORM='UNFORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(1)
      STOP
995   CONTINUE
!
      READ(4) FILETYPE
      READ(4) NPLAN
      CLOSE(4)
!
!  5 : 4 PARAMETERS
!
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',  NELEM
      WRITE(LU,*) 'NPOIN2=', NPOIN2
      WRITE(LU,*) 'ECKEN=',  ECKEN
      WRITE(LU,*) 'NDUM=',   NDUM
!
!  DYNAMICALLY ALLOCATES THE ARRAYS
!
      ALLOCATE(NPOIN(NPROC),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NPOIN')
      ALLOCATE(NOQ(NPROC),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NOQ')
      ALLOCATE(NSEG(NPROC),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NSEG')
      ALLOCATE(IKLESA(3,NELEM),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLESA')
      ALLOCATE(IPOBO(NPOIN2)      ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IPOBO')
!  X AND Y
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'YORIG')
!
      ALLOCATE(IFABOR(NELEM,3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IFABOR')
      ALLOCATE(IKLE(NELEM,3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLE')
      ALLOCATE(IADR(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IADR')
      ALLOCATE(NVOIS(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NVOIS')
      ALLOCATE(T3(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'T3')
      ALLOCATE(AREA(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'AREA')
      ALLOCATE(NODENRS(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NODENRS')
!
!  END OF ALLOCATION ...
!
!  6 : IKLE
!
      READ(2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
!
!  7 : IPOBO
!
      READ(2)  (IPOBO(I),I=1,NPOIN2)
!
!  8 : X AND Y, WILL BE CHECKED LATER ...
!
      READ(2)  (XORIG(I),I=1,NPOIN2)
      READ(2)  (YORIG(I),I=1,NPOIN2)
!
!----------------------------------------------------------------------
!
!
      IF(NPLAN.LE.1) THEN
        CONLIM = "T2DCLI"
      ELSE
        CONLIM = "T3DCLI"
      ENDIF
!
      OPEN(4,FILE=CONLIM,FORM='FORMATTED',ERR=996)
      GO TO 997
 996  WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',CONLIM
      CALL PLANTE(1)
      STOP
 997  CONTINUE
!
      ALLOCATE(LIHBOR0(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LIHBOR')
      ALLOCATE(NBOR0(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NBOR')
      DO I=1,NPOIN2
        READ(4,*,END=989) LIHBOR0(I),IDUM,IDUM,RDUM,RDUM,RDUM,RDUM,
     &                    IDUM,RDUM,RDUM,RDUM,NBOR0(I),IDUM
      ENDDO
!
      CLOSE(4)
 989  NPTFR=I-1
!
      ALLOCATE(LIHBOR(NPTFR),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LIHBOR')
      ALLOCATE(NBOR(NPTFR),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NBOR')
      ALLOCATE(NELBOR(NPTFR),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NELBOR')
      ALLOCATE(NULONE(NPTFR,2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NULONE')
      ALLOCATE(KP1BOR(NPTFR,2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'KP1BOR')
      ALLOCATE(IKLBOR(NPTFR,2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLBOR')
      ALLOCATE(ELTSEG(NELEM,3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ELTSEG')
      ALLOCATE(ORISEG(NELEM,3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ORISEG')
!
      MBND=0
!
      DO I=1,NPOIN2
        NODENRS(I) = I
      ENDDO
!
      DO I=1,NPTFR
        NBOR(I)   = NBOR0(I)
        LIHBOR(I) = LIHBOR0(I)
        IF (LIHBOR(I).NE.2) THEN
          MBND = MBND + 1
          NODENRS(NBOR(I)) = -MBND
        ENDIF
      ENDDO
!
!------------------------------------------------------------------------------
!
! LOCAL CONSTRUCTION OF GLOSEG
!
!------------------------------------------------------------------------------
!
!     WITH PRISMS, DIFFERENT FROM 2D VALUES, OTHERWISE
!
      IELM = 11 ! WARNING: IS HARD-CODED !!!
      NELEM2  =NELEM
      NELMAX2 =NELEM
      NPTFR2  =NPTFR
!
!     NEIGHBOURS OF THE BOUNDARY SIDES FOR TRIANGULAR MESH
!
      DO J=1,NELEM
        DO I=1,3
          IKLE(J,I)=IKLESA(I,J)
        ENDDO
      ENDDO
      NCSIZE = 1
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
        ! DUMMY ARRAY
        ALLOCATE(NACHB(1,1),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'NACHB')
!
        CALL VOISIN(IFABOR,NELEM2,NELEM,IELM,IKLE,
     &              NELEM,
     &              NPOIN2,NACHB,NBOR,NPTFR,IADR,NVOIS)
!
        DEALLOCATE(NACHB)
!
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
      KLOG = 2 ! SOLID BOUNDARY CONDITION: IS HARD-CODED !!!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
        ! Dummy arrays
        ALLOCATE(IFANUM(1,1),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'IFANUM')
        ALLOCATE(ISEGF(NPTFR),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'ISEG')
!
        CALL ELEBD(NELBOR,NULONE,KP1BOR,
     &             IFABOR,NBOR,IKLE,NELEM,
     &             IKLBOR,NELEM2,NELMAX2,
     &             NPOIN2,NPTFR2,IELM,
     &             LIHBOR,KLOG,
     &             ISEGF,
     &             IADR,NVOIS,T3,NPTFR2,IDUM)
!                                NELEBX,NELEB (HERE EQUAL TO NPTFR2)
!                                       NELEB IS INOUT => DUMMY
        DEALLOCATE(IFANUM)
        DEALLOCATE(ISEGF)
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DATA STRUCTURE FOR EDGE-BASED STORAGE (FROM 5.9 ON ALWAYS DONE IN 2D)
!  SEE CALL TO COMP_SEG BELOW FOR COMPLETING THE STRUCTURE
!
      IF(IELM.EQ.11) THEN
!
        NSEG2 = (3*NELEM+NPTFR)/2
        ALLOCATE(LENGTH(2,NSEG2+MBND),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'LENGTH')
        ALLOCATE(GLOSEG(NSEG2,2),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'GLOSEG')
        ALLOCATE(IFROM1(NSEG2),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'IFROM1')
        ALLOCATE(ITOPL1(NSEG2),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'ITOPL1')
!
        CALL STOSEG(IFABOR,NELEM,NELMAX2,NELMAX2,IELM,IKLE,
     &              NBOR,NPTFR,
     &              GLOSEG,NSEG2,    ! GLOSEG%MAXDIM1,
     &              ELTSEG,ORISEG,NSEG2,
     &              NELBOR,NULONE,KNOLG,IKLBOR,NPTFR ,NPTFR)
!                                              NELENX,NELEB (HERE EQUAL TO NPTFR)
      ENDIF
!
      IF(FILETYPE(1:6).EQ.'AREA2D') THEN
        DO I=1,NPOIN2
          AREA(I)=0.D0
        ENDDO
        DO IELEM=1,NELEM2
          ND1 = IKLE(IELEM,1)
          ND2 = IKLE(IELEM,2)
          ND3 = IKLE(IELEM,3)
          X2 = DBLE(XORIG(ND2))-DBLE(XORIG(ND1))
          X3 = DBLE(XORIG(ND3))-DBLE(XORIG(ND1))
          Y2 = DBLE(YORIG(ND2))-DBLE(YORIG(ND1))
          Y3 = DBLE(YORIG(ND3))-DBLE(YORIG(ND1))
          SURFACC = 0.5D0*(X2*Y3-X3*Y2)
          AREA(ND1) = AREA(ND1)+SURFACC/3.D0
          AREA(ND2) = AREA(ND2)+SURFACC/3.D0
          AREA(ND3) = AREA(ND3)+SURFACC/3.D0
        ENDDO
      ELSEIF(FILETYPE(1:6).EQ.'LENGTH') THEN
        DO ISEG=1,NSEG2
          DX = DBLE(XORIG(GLOSEG(ISEG,1))) - DBLE(XORIG(GLOSEG(ISEG,2)))
          DY = DBLE(YORIG(GLOSEG(ISEG,1))) - DBLE(YORIG(GLOSEG(ISEG,2)))
          LENGTH(1,ISEG) = SQRT(DX**2+DY**2)*0.5D0
          LENGTH(2,ISEG) = LENGTH(1,ISEG)
        ENDDO
        DO I = 1, NPTFR2                    ! LP 05/04/2009
          IF (LIHBOR(I).NE.2 ) THEN         ! OPEN BOUNDARY
            IFROM = NODENRS(NBOR(I))        ! EXCHANGES ADDED
            LENGTH(1,NSEG2-IFROM) = 10.0D0  ! DUMMY LENGTH
            LENGTH(2,NSEG2-IFROM) = 10.0D0
          ENDIF
        ENDDO
      ENDIF
!
      IF(FILETYPE(1:6).EQ.'AREA2D') THEN
        WRITE(3) NPOIN2,0,NPOIN2,NPOIN2,NPOIN2,0
        WRITE(3) (REAL(AREA(I)),I=1,NPOIN2)
      ELSEIF(FILETYPE(1:6).EQ.'LENGTH') THEN
!        WRITE(3) 0
!        DO K=1,NPLAN
!          WRITE(3) ((REAL(LENGTH(I,J)),I=1,2),J=1,NSEG2+MBND)
!        ENDDO
!        DO K=1,NPLAN-1
!          WRITE(3) (1.0, I=1,NPOIN2*2)
!        ENDDO
        WRITE(3) 0,(((REAL(LENGTH(I,J)),I=1,2),J=1,NSEG2+MBND),     ! LP 27/02/2011
     &                K=1,NPLAN), ((1.0,1.0), K=1,(NPLAN-1)*NPOIN2) ! BECAUSE OF
!                                                                   ! UNFORMATTED FILES
!                                                                   ! ALL NOW IN 1 RECORD
      ELSEIF(FILETYPE(1:6).EQ.'IFRMTO') THEN
        DO K=1,NPLAN
          DO ISEG=1,NSEG2
            IFROM = GLOSEG(ISEG,1)
            ITO   = GLOSEG(ISEG,2)
            IF ( K.EQ.1 ) THEN
              CALL GREDEL_FDNRST(IFROM,ITO,XORIG,YORIG,NODENRS,
     &         NPOIN2,IFROM1(ISEG),ITOPL1(ISEG))
              IF ( IFROM1(ISEG) .LT. 0 .AND.              !  *START*  LP 24/04/2009
     &             IFROM1(ISEG) .NE. NODENRS(IFROM) ) THEN
                DO I = 1,NPOIN2
                  IF ( NODENRS(I) .EQ. IFROM1(ISEG) ) THEN
                    IFROM1(ISEG) = I
                    EXIT
                  ENDIF
                ENDDO
              ENDIF
              IF ( ITOPL1(ISEG) .LT. 0 .AND.
     &             ITOPL1(ISEG) .NE. NODENRS(ITO  ) ) THEN
                DO I = 1,NPOIN2
                  IF ( NODENRS(I) .EQ. ITOPL1(ISEG) ) THEN
                    ITOPL1(ISEG) = I
                    EXIT
                  ENDIF
                ENDDO
              ENDIF                                       !  **END**  LP 24/04/2009
            ENDIF
            IFRM1 = IFROM1(ISEG)
            ITOP1 = ITOPL1(ISEG)
            IFROM = IFROM + (K-1)*NPOIN2
            IF ( IFRM1 .GT. 0 ) THEN
              IFRM1 = IFRM1 + (K-1)*NPOIN2
            ELSE
              IFRM1 = IFRM1 - (K-1)*MBND                      ! LP 24/04/2009
            ENDIF
            ITO   = ITO   + (K-1)*NPOIN2
            IF ( ITOP1 .GT. 0 ) THEN
              ITOP1 = ITOP1 + (K-1)*NPOIN2
            ELSE
              ITOP1 = ITOP1 - (K-1)*MBND                      ! LP 24/04/2009
            ENDIF
            WRITE(3) IFROM,ITO,IFRM1,ITOP1
          ENDDO
          DO I=1,NPTFR2                                      ! LP 05/04/2009
            IF ( LIHBOR(I) .NE. 2 ) THEN                       ! OPEN BOUNDARY
              IFROM = NODENRS(NBOR(I))                        ! EXCHANGES ADDED
              ITO   = NBOR(I)
              IFRM1 = IFROM
              ITOP1 = ITO
              IFROM = IFROM - (K-1)*MBND
              IFRM1 = IFRM1 - (K-1)*MBND
              ITO   = ITO   + (K-1)*NPOIN2
              ITOP1 = ITOP1 + (K-1)*NPOIN2
              WRITE(3)IFROM,ITO,IFRM1,ITOP1
            ENDIF
          ENDDO
!        THE WRITING OF EXCHANGE POINTERS IS CHANGED       **END**     LP 05/04/2009
        ENDDO
!
!       DERIVE THE FROM-TO EXCHANGE TABLE FOR COMPUTATIONAL ELEMENTS
!       VERTICALLY FOR ALL LAYERS. THE LAYERS DIFFER NPOIN2 IN
!       COMPUTATIONAL ELEMENT NUMBER. BOUNDARY NODES HAVE NO VERTICAL FLOW
!       WRITE 1.0 FOR THE VERTICAL 'FROM' AND 'TO' HALFDISTANCES
!       THEY ARE UPDATED BY WAQ TO BECOME VOLUME/AREA/2.0 DURING
!       SIMULATION TIME, SINCE VERTICAL DISTANCES CHANGE WITH VOLUME.
!
        DO K=1,NPLAN-1
          DO I=1,NPOIN2
!       THE WRITING OF EXCHANGE POINTERS IS CHANGED       *START*     LP 05/04/2009
            IFROM = I
            IFRM1 = IFROM +  MAX(K-2,   0   )*NPOIN2
            ITOP1 = IFROM +  MIN(K+1,NPLAN-1)*NPOIN2
            IFROM = IFROM + (    K-1        )*NPOIN2
            ITO   = IFROM +                      NPOIN2
            WRITE (3) IFROM,ITO,IFRM1,ITOP1
!       THE WRITING OF EXCHANGE POINTERS IS CHANGED       **END**     LP 05/04/2009
          ENDDO
        ENDDO                  ! WAQ COMPUTES THEM ON THE FLY FROM VOLUMES
      ENDIF
!
      WRITE(LU,*) 'END OF PROGRAM '
!
      CLOSE(2)
      CLOSE(3)
!
      STOP 0
      END PROGRAM GREDELMET_AUTOP
