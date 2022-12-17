!     ***********************
      MODULE MOD_HANDLE_WEIRS
!     ***********************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!BRIEF    Treatment of weirs
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        IMPLICIT NONE
        CONTAINS
!     ***********************
      SUBROUTINE HANDLE_WEIRS
!     ***********************
!
     &        (NAMESEU, NPARTS, KNOGL, NPOIN2)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMESEU        |<--| Name of weirs file
!| NPARTS         |<--| Number of partitions
!| KNOGL          |<->| Global to local numbering
!| NPOIN2         |<--| Number of 2d points
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_PARTEL
      USE MOD_HASH_TABLE
      USE BIEF, ONLY : NBMAXNSHARE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CHARACTER(LEN=PATH_LEN), INTENT(IN) :: NAMESEU
      INTEGER, INTENT(IN) :: NPARTS
      TYPE(HASH_TABLE), INTENT(INOUT) ::KNOGL
      INTEGER, INTENT(IN) :: NPOIN2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CHARACTER(LEN=PATH_LEN) :: NAMEOUT
      INTEGER :: NOUT
      INTEGER I,J,K,L,M,N,IERR,POSI
      INTEGER :: NSEU, NBSEU, OPSEU, JJ, PROC
      INTEGER :: DEB, FIN
      INTEGER, ALLOCATABLE :: NUM_SEU1(:,:), NUM_SEU2(:,:)
      INTEGER, ALLOCATABLE :: NUM_SEU1_P(:,:,:), NUM_SEU2_P(:,:,:)
      INTEGER, ALLOCATABLE :: SEU_P(:,:), CSEU_P(:,:), P_CSEU(:,:)
      INTEGER, ALLOCATABLE :: LRECV_P(:,:), NRECV_P(:)
      INTEGER, ALLOCATABLE :: LSEND_PP(:,:,:), LSEND_P(:,:), NSEND_P(:)
      INTEGER, ALLOCATABLE :: LIST_TMP(:,:)
      INTEGER, ALLOCATABLE :: IP1(:), IP2(:)
      INTEGER, ALLOCATABLE :: DEBSEU(:), FINSEU(:)
      DOUBLE PRECISION :: RELAX
      DOUBLE PRECISION, ALLOCATABLE :: XSEU1(:), YSEU1(:), ZSEU1(:)
      DOUBLE PRECISION, ALLOCATABLE :: XSEU2(:), YSEU2(:), ZSEU2(:)
      DOUBLE PRECISION, ALLOCATABLE :: RP1(:), RP2(:)
      INTEGER, ALLOCATABLE :: PART_P(:,:)
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS
      CHARACTER(LEN=10) FMT1

!     READING THE WEIRS INFORMATIONS
      CALL GET_FREE_ID(NSEU)
      OPEN(NSEU,FILE=TRIM(NAMESEU),FORM='FORMATTED',STATUS='OLD')
!     first read to count the total number of elements
      NBSEU = 0
      READ(NSEU,*,END=304,ERR=305)
      READ(NSEU,*,END=304,ERR=305) N, K, RELAX
      DO I=1, N
        READ(NSEU,*,END=304,ERR=305)
        READ(NSEU,*,END=304,ERR=305) K
        NBSEU = NBSEU + K
        DO J=1, K+3
          READ(NSEU,*,END=304,ERR=305)
        ENDDO
      ENDDO
      REWIND(NSEU)
!     ALLOCATION
      ALLOCATE (XSEU1(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'XSEU1')
      XSEU1(:) = 0
      ALLOCATE (YSEU1(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'YSEU1')
      YSEU1(:) = 0
      ALLOCATE (ZSEU1(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'ZSEU1')
      ZSEU1(:) = 0
      ALLOCATE (XSEU2(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'XSEU2')
      XSEU2(:) = 0
      ALLOCATE (YSEU2(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'YSEU2')
      YSEU2(:) = 0
      ALLOCATE (ZSEU2(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'ZSEU2')
      ZSEU2(:) = 0
      ALLOCATE (RP1(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'RP1')
      RP1(:) = 0.D0
      ALLOCATE (RP2(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'RP2')
      RP2(:) = 0.D0
      ALLOCATE (IP1(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'IP1')
      IP1(:) = 0
      ALLOCATE (IP2(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'IP2')
      IP2(:) = 0
      ALLOCATE (DEBSEU(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'DEBSEU')
      DEBSEU(:) = 0
      ALLOCATE (FINSEU(NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'FINSEU')
      FINSEU(:) = 0
      ALLOCATE (NUM_SEU1(4,NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'NUM_SEU1')
      NUM_SEU1(:,:) = 0
      ALLOCATE (NUM_SEU2(4,NBSEU), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'NUM_SEU2')
      NUM_SEU2(:,:) = 0
!     Full read of the file
      READ(NSEU,*,END=304,ERR=305)
      READ(NSEU,*,END=304,ERR=305) N, OPSEU, RELAX
      J = 0
      DO M=1, N
        READ(NSEU,*,END=304,ERR=305)
        READ(NSEU,*,END=304,ERR=305) JJ
        READ(NSEU,*,END=304,ERR=305)
        READ(NSEU,*,END=304,ERR=305)
        READ(NSEU,*,END=304,ERR=305)
        DEB = J+1
        FIN = J+JJ
        DO L=1, JJ
          J = J + 1
          READ(NSEU,*,END=304,ERR=305) I,
     &         XSEU1(J), YSEU1(J), ZSEU1(J),
     &         (NUM_SEU1(K,J), K = 1,4),
     &         XSEU2(J), YSEU2(J), ZSEU2(J),
     &         (NUM_SEU2(K,J), K = 1,4),
     &         IP1(J), IP2(J), RP1(J), RP2(J)
          DEBSEU(J) = DEB
          FINSEU(J) = FIN
        END DO
      END DO
      CLOSE(NSEU)
!
      ALLOCATE (PART_P(NPOIN2,0:NBMAXNSHARE),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'PART_P')
      PART_P(:,:)=0
!     PARTITION OF THE GLOBAL DOMAIN.
!     FOR EACH GLOBAL NODE, PART_P GIVES THE NUMBER AND THE LIST OF SD WHICH CONTAINS THE NODE
      DO N=1,NPARTS
        DO I=1, NPOIN2
          IF (HASH_TABLE_GET(KNOGL,I,N).GT.0) THEN
            PART_P(I,0)=PART_P(I,0)+1
            POSI=PART_P(I,0)
            PART_P(I,POSI)=N
          ENDIF
        END DO
      END DO
!
      ALLOCATE (SEU_P(NBSEU,NPARTS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'SEU_P')
      SEU_P(:,:)=0
      ALLOCATE (CSEU_P(NBSEU,0:NBMAXNSHARE),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'CSEU_P')
      CSEU_P(:,:)=0
      ALLOCATE (P_CSEU(NPARTS,0:NBSEU))
      CALL CHECK_ALLOCATE(IERR, 'P_CSEU')
      P_CSEU(:,:)=0
!     PARTITION OF THE ELEMENT OF WEIRS.
!     SEU_P INDICATE IF THE ELEMENT OF WEIR IS COMPUTED OR NOT BY SD
      DO N=1,NPARTS
        DO J=1,NBSEU
          DO K=1,4
            IF (HASH_TABLE_GET(KNOGL,NUM_SEU1(K,J),N).GT.0) THEN
              SEU_P(J,N)=1
            END IF
            IF (HASH_TABLE_GET(KNOGL,NUM_SEU2(K,J),N).GT.0) THEN
              SEU_P(J,N)=1
            END IF
          END DO
        END DO
      END DO
!     PARTITION OF THE ELEMENT OF WEIRS.
!     FOR EACH ELEMENT, CSEU_P GIVES THE NUMBER AND THE LIST OF SD WHICH COMPUTE THE DISCHARGE
      DO N=1,NPARTS
        DO J=1,NBSEU
          IF (SEU_P(J,N)>0) THEN
            CSEU_P(J,0)=CSEU_P(J,0)+1
            POSI=CSEU_P(J,0)
            CSEU_P(J,POSI)=N
          END IF
        END DO
      END DO
!     PARTITION OF THE ELEMENT OF WEIRS.
!     FOR EACH SD, P_CSEU GIVES THE NUMBER AND THE LIST ELEMENT OF WEIR COMPUTED
      DO J=1,NBSEU
        DO L=1, CSEU_P(J,0)
          PROC = CSEU_P(J,L)
          P_CSEU(PROC,0) = P_CSEU(PROC,0)+1
          POSI = P_CSEU(PROC,0)
          P_CSEU(PROC,POSI) = J
        END DO
      END DO
!
      ALLOCATE (NRECV_P(NPARTS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'NRECV_P')
      NRECV_P(:)=0
      ALLOCATE (LRECV_P(8*NBSEU,NPARTS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'LRECV_P')
      LRECV_P(:,:)=0
!     INFORMATIONS ON THE NODES INVOLVED IN THE ELEMENT OF WEIRS COMPUTATION
!     NRECV_P GIVES THE NUMBERS OF NODES USED IN THE COMPUTATION FOR EACH SD
!     LRECV_P GIVES THE LIST OF NODES USED IN THE COMPUTATION FOR EACH SD
      DO N=1,NPARTS
        DO J=1,NBSEU
          IF (SEU_P(J,N)>0) THEN
            DO K=1,4
              DO L=1, NRECV_P(N)
                IF (NUM_SEU1(K,J) == LRECV_P(L,N)) GOTO 602
                IF (NUM_SEU1(K,J) < LRECV_P(L,N)) GOTO 601
              END DO
 601          CONTINUE
              NRECV_P(N)=NRECV_P(N)+1
              DO I= NRECV_P(N),L+1,-1
                LRECV_P(I,N) = LRECV_P(I-1,N)
              END DO
              LRECV_P(L,N)=NUM_SEU1(K,J)
 602          CONTINUE
!
              DO L=1, NRECV_P(N)
                IF (NUM_SEU2(K,J) == LRECV_P(L,N)) GOTO 604
                IF (NUM_SEU2(K,J) < LRECV_P(L,N)) GOTO 603
              END DO
 603          CONTINUE
              NRECV_P(N)=NRECV_P(N)+1
              DO I= NRECV_P(N),L+1,-1
                LRECV_P(I,N) = LRECV_P(I-1,N)
              END DO
              LRECV_P(L,N)=NUM_SEU2(K,J)
 604          CONTINUE
            END DO
          END IF
        END DO
      END DO
!
      ALLOCATE (NSEND_P(NPARTS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'NSEND_P')
      NSEND_P(:)=0
      ALLOCATE (LSEND_P(8*NBSEU,NPARTS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'LSEND_P')
      LSEND_P(:,:)=0
      ALLOCATE (LSEND_PP(NBMAXNSHARE,8*NBSEU,NPARTS),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'LSEND_PP')
      LSEND_PP(:,:,:)=0
!     INFORMATIONS ON THE NODES INVOLVED IN THE ELEMENT OF WEIRS COMPUTATION
!     NSEND_P GIVES FOR EACH SD THE NUMBERS OF NODES USED IN THE COMPUTATION OF OTHER SD
!     LSEND_P GIVES FOR EACH SD THE LIST OF NODES USED IN THE COMPUTATION OF OTHER SD
!     LSEND_PP GIVES FOR EACH SD AND FOR EACH NODES USED IN THE COMPUTATION OF OTHER SD THE LIST OF SD
      DO J=1,NBSEU
        DO M=1, CSEU_P(J,0)
          N = CSEU_P(J,M)
          DO K=1,4
            IF(HASH_TABLE_GET(KNOGL,NUM_SEU1(K,J),N)>0) THEN
              DO L=1, NSEND_P(N)
                IF (NUM_SEU1(K,J) == LSEND_P(L,N)) GOTO 702
                IF (NUM_SEU1(K,J) < LSEND_P(L,N)) GOTO 701
              END DO
 701          CONTINUE
              NSEND_P(N)=NSEND_P(N)+1
              DO I= NSEND_P(N),L+1,-1
                LSEND_P(I,N) = LSEND_P(I-1,N)
                LSEND_PP(:,I,N) = LSEND_PP(:,I-1,N)
              END DO
              LSEND_P(L,N)=NUM_SEU1(K,J)
              LSEND_PP(:,L,N)=0
 702          CONTINUE
              DO I=1, CSEU_P(J,0)
                DO JJ = 1, NBMAXNSHARE
                  IF (CSEU_P(J,I) == LSEND_PP(JJ,L,N)) GOTO 703
                  IF (LSEND_PP(JJ,L,N) == 0) THEN
                    LSEND_PP(JJ,L,N) = CSEU_P(J,I)
                    GOTO 703
                  END IF
                END DO
 703            CONTINUE
              END DO
            END IF
!
            IF(HASH_TABLE_GET(KNOGL,NUM_SEU2(K,J),N)>0) THEN
              DO L=1, NSEND_P(N)
                IF (NUM_SEU2(K,J) == LSEND_P(L,N)) GOTO 705
                IF (NUM_SEU2(K,J) < LSEND_P(L,N)) GOTO 704
              END DO
 704          CONTINUE
              NSEND_P(N)=NSEND_P(N)+1
              DO I= NSEND_P(N),L+1,-1
                LSEND_P(I,N) = LSEND_P(I-1,N)
                LSEND_PP(:,I,N) = LSEND_PP(:,I-1,N)
              END DO
              LSEND_P(L,N)=NUM_SEU2(K,J)
              LSEND_PP(:,L,N)=0
 705          CONTINUE
              DO I=1, CSEU_P(J,0)
                DO JJ = 1, NBMAXNSHARE
                  IF (CSEU_P(J,I) == LSEND_PP(JJ,L,N)) GOTO 706
                  IF (LSEND_PP(JJ,L,N) == 0) THEN
                    LSEND_PP(JJ,L,N) = CSEU_P(J,I)
                    GOTO 706
                  END IF
                END DO
 706            CONTINUE
              END DO
            END IF
          END DO
        END DO
      END DO
!
      ALLOCATE (NUM_SEU1_P(4,NBSEU,NPARTS), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'NUM_SEU1_P')
      NUM_SEU1_P(:,:,:)=0
      ALLOCATE (NUM_SEU2_P(4,NBSEU,NPARTS), STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'NUM_SEU2_P')
      NUM_SEU2_P(:,:,:)=0
!     LOCAL NUMBERING OF NODES INVOLVED IN ELEMENT OF WEIRS DESCRIPTION
      DO N=1,NPARTS
        DO J=1,NBSEU
          IF (SEU_P(J,N)>0) THEN
            DO K=1,4
              DO L=1, NRECV_P(N)
                IF (NUM_SEU1(K,J)==LRECV_P(L,N)) THEN
                  NUM_SEU1_P(K,J,N) = L
                  GOTO 605
                END IF
              ENDDO
              WRITE(LU,*) 'HOUSTON ON A UN PROBLEME (1)', J, K, L
 605          CONTINUE
            END DO
!
            DO K=1,4
              DO L=1, NRECV_P(N)
                IF (NUM_SEU2(K,J)==LRECV_P(L,N)) THEN
                  NUM_SEU2_P(K,J,N) = L
                  GOTO 606
                END IF
              ENDDO
              WRITE(LU,*) 'HOUSTON ON A UN PROBLEME (2)', J, K, L
 606          CONTINUE
            END DO
          END IF
        END DO
      END DO
!
! WRITE THE WEIR FILE FOR EACH SUBDOMAIN
      CALL GET_FREE_ID(NOUT)
      DO N=1,NPARTS
        NAMEOUT=TRIM(NAMESEU)//EXTENS(NPARTS-1,N-1)
        WRITE(LU,*) 'WRITING: ', TRIM(NAMEOUT)
        OPEN (NOUT,FILE=TRIM(NAMEOUT),FORM='FORMATTED',STATUS='NEW')
        WRITE(NOUT,*) ' NB DE SINGULARITES OPTION UTAN'
        WRITE(NOUT,*) '1  ', OPSEU, RELAX
        WRITE(NOUT,*) '#Comment line'
        WRITE(NOUT,*) SUM(SEU_P(:,N))
        WRITE(NOUT,*) '#Comment line'
        WRITE(NOUT,*) '#Comment line'
        WRITE(NOUT,*) '#Comment line'
! WRITE THE DESCRIPTION OF ELEMENTS OF WEIRS
        DO J=1,NBSEU
          IF (SEU_P(J,N)>0) THEN
1000        FORMAT(I0,1X,3(F27.15, 1X), 4(I0, 1X),
     &             3(F27.15, 1X), 4(I0, 1X),
     &             2(I0, 1X), 2(F27.15, 1X),
     &             2(I0, 1X) )
            WRITE(NOUT,1000) J,
     &         XSEU1(J), YSEU1(J), ZSEU1(J),
     &         (NUM_SEU1_P(K,J,N), K = 1,4),
     &         XSEU2(J), YSEU2(J), ZSEU2(J),
     &         (NUM_SEU2_P(K,J,N), K = 1,4),
     &         IP1(J), IP2(J), RP1(J), RP2(J),
     &         DEBSEU(J), FINSEU(J)
          END IF
        END DO
! WRITE THE PARALLEL INFORMATION WHICH DESCRIBE WHICH SUBDOMAIN NEED TO COMPUTE EACH ELEMENT
        WRITE(NOUT,'(A)')
     &    ' /1/ : FOR EACH ELEMENT, NUMBER AND LIST OF SD'//
     &    ' WHICH WILL ALSO COMPUTE DISCHARGE ON IT'
!
        ALLOCATE (LIST_TMP(1,NBMAXNSHARE),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'LIST_TMP')
        DO J=1,NBSEU
          IF (SEU_P(J,N)>0) THEN
            LIST_TMP(:,:)=0
            I = 0
            DO K=1, NBMAXNSHARE
              IF((CSEU_P(J,K)>0).AND.(CSEU_P(J,K)/=N)) THEN
                I = I + 1
                LIST_TMP(1,I) = CSEU_P(J,K)
              END IF
            END DO
            WRITE(NOUT,*) J, I
            IF (I>0) THEN
              WRITE(NOUT,*) (LIST_TMP(1,K)-1, K=1,I)
            END IF
          END IF
        END DO
        DEALLOCATE (LIST_TMP)
!
        WRITE(NOUT,'(A)')
     &    ' /2/ : FOR EACH SD, NUMBER AND LIST OF ELEMENT'//
     &    ' WHICH WILL BE COMPUTED'
!       FIRST PASSAGE TO COUNT THE NUMBER OF SD AND WRITE IT IN THE FILE
        PROC = 0
        DO L=1, NPARTS
          IF (L/=N) THEN
            DO J=1,P_CSEU(L,0)
              IF (SEU_P(P_CSEU(L,J),N)>0) THEN
                PROC = PROC + 1
                GOTO 501
              END IF
            END DO
          END IF
 501      CONTINUE
        END DO
        WRITE(NOUT,*) PROC
!
        ALLOCATE (LIST_TMP(1,NBSEU),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'LIST_TMP')
        LIST_TMP(:,:)=0
        DO L=1, NPARTS
          IF (L/=N) THEN
            I = 0
            DO J=1,P_CSEU(L,0)
              IF (SEU_P(P_CSEU(L,J),N)>0) THEN
                I = I + 1
                LIST_TMP(1,I) = P_CSEU(L,J)
              END IF
            END DO
            IF (I>0) THEN
              WRITE(NOUT,'(I0,1X,I0)') L-1, I
              FMT1 = ' '
              WRITE(FMT1, '(I0)') I
              WRITE(NOUT,'('//ADJUSTL(fmt1)//'(I0,1X))')
     &          (LIST_TMP(1,K), K=1,I)
            END IF
          END IF
        END DO
        DEALLOCATE (LIST_TMP)
! WRITE THE PARALLEL INFORMATION ABOUT THE NODES INVOLVED IN WEIR DESCRIPTION
! THE VALUES OF THESE NODES SHOULD BE RECEIVE FROM OTHER SD
        WRITE(NOUT,'(A)')
     &    ' /3/ : FOR EACH NODE REQUIRED BY THE SD TO'//
     &    ' COMPUTE THE DISCHARGE, NUMBER, LIST AND LOCAL'//
     &    ' NUMBERING OF NODES FROM ALL INVOLVED SD'
        ALLOCATE (LIST_TMP(2,NBMAXNSHARE),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'LIST_TMP')
        WRITE(NOUT,*) NRECV_P(N)
        IF (NRECV_P(N)>0) THEN
          DO I=1, NRECV_P(N)
            LIST_TMP(:,:)=0
            J = 0
            DO K=1, PART_P(LRECV_P(I,N),0)
              J = J + 1
              LIST_TMP(1,K) = PART_P(LRECV_P(I,N),K)
              LIST_TMP(2,K) = HASH_TABLE_GET(KNOGL,
     &          LRECV_P(I,N),PART_P(LRECV_P(I,N),K))
            END DO
            WRITE(NOUT,'(I0,1X,I0)') LRECV_P(I,N), J
            FMT1 = ' '
            WRITE(FMT1, '(I0)') J
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X,I0,1X))')
     &        (LIST_TMP(1,K)-1,LIST_TMP(2,K), K=1,J)
          END DO
        END IF
        DEALLOCATE (LIST_TMP)
!
        WRITE(NOUT,'(A)')
     &    ' /4/ : FOR EACH OTHER SD WHICH SEND VALUES,'//
     &    ' NUMBER AND LIST OF NODE (WITH LOCAL NUMBER)'
        PROC = 0
        IF (NRECV_P(N)>0) THEN
          ALLOCATE (LIST_TMP(NPARTS,3*NRECV_P(N)),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'LIST_TMP')
          LIST_TMP(:,:)=0
          DO I=1, NRECV_P(N)
            DO K=1, PART_P(LRECV_P(I,N),0)
              DO J=1, PROC
                IF (LIST_TMP(J,1) == PART_P(LRECV_P(I,N),K)) GOTO 522
                IF (LIST_TMP(J,1) >  PART_P(LRECV_P(I,N),K)) GOTO 521
              END DO
 521          CONTINUE
              PROC = PROC + 1
              DO M = PROC, J+1, -1
                LIST_TMP(M,1) = LIST_TMP(M-1,1)
              END DO
              LIST_TMP(J,1) = PART_P(LRECV_P(I,N),K)
 522          CONTINUE
            END DO
          END DO
        END IF
        WRITE(NOUT,'(I0)') PROC
!
        DO M=1, PROC
          POSI = 2
          DO I=1, NRECV_P(N)
            JJ = LRECV_P(I,N)
            DO K=1, PART_P(JJ,0)
              IF (PART_P(JJ,K)==LIST_TMP(M,1)) THEN
                LIST_TMP(M,POSI  ) = I
                LIST_TMP(M,POSI+1) = JJ
                LIST_TMP(M,POSI+2) =
     &            HASH_TABLE_GET(KNOGL,JJ,PART_P(JJ,K))
                POSI = POSI + 3
              END IF
            END DO
          END DO
          WRITE(NOUT,'(I0,1X,I0)') LIST_TMP(M,1)-1, (POSI-1)/3
          FMT1 = ' '
          WRITE(FMT1, '(I0)') (POSI-1)/3
          IF (POSI > 2) THEN
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X))')
     &        (LIST_TMP(M,3*K-1), K=1,(POSI-1)/3)
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X))')
     &        (LIST_TMP(M,3*K  ), K=1,(POSI-1)/3)
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X))')
     &        (LIST_TMP(M,3*K+1), K=1,(POSI-1)/3)
          END IF
        END DO
        IF (ALLOCATED(LIST_TMP)) DEALLOCATE(LIST_TMP)
!
! WRITE THE PARALLEL INFORMATION ABOUT THE NODES INVOLVED IN WEIR DESCRIPTION OF OTHER SD
! THE VALUES OF THESE NODES SHOULD BE SEND TO OTHER SD
        WRITE(NOUT,'(A)')
     &    ' /5/ : FOR EACH NODE INSIDE THE SD, NUMBER AND'//
     &    ' LIST OF OTHERS SD WHICH ALSO NEED THE VALUE'
        ALLOCATE (LIST_TMP(2,NBMAXNSHARE),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'LIST_TMP')
        WRITE(NOUT,'(I0)') NSEND_P(N)
        IF (NSEND_P(N)>0) THEN
          DO I=1, NSEND_P(N)
            LIST_TMP(:,:)=0
            J = 0
            DO K=1, NBMAXNSHARE
              IF (LSEND_PP(K,I,N)>0) THEN
                J = J + 1
                LIST_TMP(1,J) = LSEND_PP(K,I,N)
              END IF
            END DO
            WRITE(NOUT,'(I0,1X,I0)') LSEND_P(I,N), J
            FMT1 = ' '
            WRITE(FMT1, '(I0)') J
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X))')
     &        (LIST_TMP(1,K)-1, K=1,J)
          END DO
        END IF
        DEALLOCATE (LIST_TMP)
!
        WRITE(NOUT,'(A)')
     &    ' /6/ : FOR EACH OTHER SD WHICH NEEDS NODE '//
     &    'VALUES, NUMBER AND LIST OF NODES'
        PROC = 0
        IF (NSEND_P(N)>0) THEN
          ALLOCATE (LIST_TMP(NPARTS,3*NSEND_P(N)+1),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'LIST_TMP')
          LIST_TMP(:,:)=0
          DO I=1, NSEND_P(N)
            DO K=1, NBMAXNSHARE
              IF (LSEND_PP(K,I,N)>0) THEN
                DO J=1, PROC
                  IF (LIST_TMP(J,1) == LSEND_PP(K,I,N)) GOTO 512
                  IF (LIST_TMP(J,1) >  LSEND_PP(K,I,N)) GOTO 511
                END DO
 511            CONTINUE
                PROC = PROC + 1
                DO M = PROC, J+1, -1
                  LIST_TMP(M,1) = LIST_TMP(M-1,1)
                END DO
                LIST_TMP(J,1) = LSEND_PP(K,I,N)
 512            CONTINUE
              END IF
            END DO
          END DO
        END IF
!
        WRITE(NOUT,*) PROC
        IF (NSEND_P(N)>0) THEN
          DO M=1, PROC
            POSI = 2
            DO I=1, NSEND_P(N)
            JJ = LSEND_P(I,N)
              DO K=1, NBMAXNSHARE
                IF (LSEND_PP(K,I,N)==LIST_TMP(M,1)) THEN
                  J = 1
                  DO WHILE (LRECV_P(J,N) /= JJ)
                    J = J + 1
                  END DO
                  LIST_TMP(M,POSI  ) = J
                  LIST_TMP(M,POSI+1) = JJ
                  J = 1
                  DO WHILE (PART_P(JJ,J) /= N)
                    J = J + 1
                  END DO
                  LIST_TMP(M,POSI+2) =
     &              HASH_TABLE_GET(KNOGL,JJ,PART_P(JJ,J))
                  POSI = POSI + 3
                END IF
              END DO
            END DO
            J = (POSI-1)/3
            FMT1 = ' '
            WRITE(FMT1, '(I0)') J
            WRITE(NOUT,'(I0,1X,I0)') LIST_TMP(M,1)-1, J
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X))')
     &        (LIST_TMP(M,3*K-1), K=1,J)
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X))')
     &        (LIST_TMP(M,3*K  ), K=1,J)
            WRITE(NOUT,'('//ADJUSTL(FMT1)//'(I0,1X))')
     &        (LIST_TMP(M,3*K+1), K=1,J)
          END DO
          IF (ALLOCATED(LIST_TMP)) DEALLOCATE (LIST_TMP)
        END IF
!
        CLOSE(NOUT)
      END DO
!
      DEALLOCATE(XSEU1)
      DEALLOCATE(YSEU1)
      DEALLOCATE(ZSEU1)
      DEALLOCATE(XSEU2)
      DEALLOCATE(YSEU2)
      DEALLOCATE(ZSEU2)
      DEALLOCATE(RP1)
      DEALLOCATE(RP2)
      DEALLOCATE(IP1)
      DEALLOCATE(IP2)
      DEALLOCATE(DEBSEU)
      DEALLOCATE(FINSEU)
      DEALLOCATE(NUM_SEU1)
      DEALLOCATE(NUM_SEU2)
      DEALLOCATE(NUM_SEU1_P)
      DEALLOCATE(NUM_SEU2_P)
      DEALLOCATE(PART_P)
      DEALLOCATE(SEU_P)
      DEALLOCATE(CSEU_P)
      DEALLOCATE(LSEND_P)
      DEALLOCATE(LSEND_PP)
      DEALLOCATE(NRECV_P)
      DEALLOCATE(LRECV_P)

      RETURN

 304  WRITE(LU,*) 'ABNORMAL END OF FILE'
      GO TO 999
 305  WRITE(LU,*) 'ERROR WITH WEIR FILE FORMAT'
      GO TO 999
!
 999  CALL PLANTE(1)
      STOP
      END SUBROUTINE
      END MODULE
