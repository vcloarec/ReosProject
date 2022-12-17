!                     **************************
                      SUBROUTINE SOLVE_MUMPS_PAR
!                     **************************
!
     &(NPOIN,NSEGB,GLOSEG,MAXSEG,DA,XA,XINC,RHS,TYPEXT,KNOLG,
     & NPOIN_TOT)
!
!***********************************************************************
!  BIEF VERSION 7.1
!***********************************************************************
!
!brief    PARALLEL DIRECT SYSTEM SOLUTION
!+
!
!history  C. DENIS (SINETICS)
!+        26/02/2010
!+        V6P0
!+     First version
!
!history  J-M HERVOUET (LNHE)
!+        23/11/2015
!+        V7P1
!+   Correction in the case of non symmetric matrix.
!+   Decommenting the deallocation of allocatable arrays.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINTS IN THE PROCESSOR.
!| ...            |-->| ...
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     PARALLEL SOLVING USING MUMPS
      USE DECLARATIONS_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!     structures MPI et MUMPS
#if defined HAVE_MUMPS
      INCLUDE 'dmumps_struc.h'
#endif
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NSEGB,MAXSEG,NPOIN_TOT
      INTEGER, INTENT(IN)             :: GLOSEG(MAXSEG,2)
      DOUBLE PRECISION, INTENT(INOUT) :: XA(*),RHS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XINC(NPOIN),DA(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
      INTEGER, INTENT(IN)             :: KNOLG(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined HAVE_MUMPS
!
      TYPE (DMUMPS_STRUC) MUMPS_PAR
!     INDIRECT ARRAYS
      DOUBLE PRECISION ,ALLOCATABLE :: TEMP1(:),TEMP2(:)
      INTEGER, ALLOCATABLE :: TEMP3(:)
      INTEGER I,J,K,ERR,NBELEM,IER, IAUX, KTRI, NPOIN2, N2
! variables chantier logiciel ob
      DOUBLE PRECISION :: RMIN, RMAX, RAUX, EPSBLR
      INTEGER :: INIV, INPREC, NAUX
      CHARACTER(LEN=80) :: KVERS, KNOM, KRENUM, KPRE, KMEM,
     &                     KPOST, KACCE
      LOGICAL :: LTRI, LPIVOTSTAT, LDEFAULT

! constantes
      RMIN=1.D-100
      RMAX=1.D+100
! temporary parameters
      INIV=1 ! verbose mode=2, standard mode=1
      INPREC=8 ! 8, 9, 10... or <0
      EPSBLR=1.D-9 ! between d-6 and d-15
!
      KNOM='<solve_mumps_par>'
      KNOM=TRIM(ADJUSTL(KNOM))
!
      KACCE='FR' ! 'FR', 'FR+', 'LR+'
      KPRE='AUTO' ! 'AUTO' ou 'SANS'
      KRENUM='AUTO' ! 'AUTO'/'AMD'/'AMF'/'SCOTCH'/'PTSCOTCH'/'PORD'&
     &             ! 'METIS'/'PARMETIS'/'QAMD'
      KMEM = 'IC' ! 'IC'/'OOC'
      KPOST = 'AUTO' ! 'AUTO' ou 'MINI' ou 'SANS' ou 'FORCE'
!
      LPIVOTSTAT  =.FALSE. ! .true. less robust but quicker
      LTRI=.TRUE.      ! .true. filtering of tiny or huge matrix terms
!
! utils/bief PARAMETRIZATION
      LDEFAULT=.TRUE.
      IF (LDEFAULT) THEN
        INPREC=8
        LPIVOTSTAT=.FALSE.
        LTRI=.FALSE.
        KPRE='AUTO'
        KRENUM='AUTO'
        KMEM='IC'
        KPOST='FORCE'
        KACCE='FR'
      ENDIF
!
!MUMPS INITIALISATION
      MUMPS_PAR%COMM = COMM
      MUMPS_PAR%JOB = -1
      IF(TYPEXT.EQ.'S') THEN
        MUMPS_PAR%SYM = 2
      ELSE
        MUMPS_PAR%SYM = 0
      ENDIF
      MUMPS_PAR%PAR = 1
      CALL DMUMPS(MUMPS_PAR)

!CHECK MUMPS VERSION NUMBER
      KVERS=MUMPS_PAR%VERSION_NUMBER
      KVERS=TRIM(ADJUSTL(KVERS))
      SELECT CASE (KVERS)
      CASE('4.10.0','5.2.1','5.2.1consortium')
      CASE DEFAULT
        WRITE(LU,*)'*************************************************'
        WRITE(LU,*)KNOM//' * Warning *, MUMPS version not validated'
        WRITE(LU,*)'version=',KVERS
        WRITE(LU,*)'*************************************************'
      END SELECT

!MUMPS PARAMETRIZATION
!-- VERBOSE MODE
      IF (INIV.EQ.1) THEN
        MUMPS_PAR%ICNTL(1)=LU
        MUMPS_PAR%ICNTL(2)=0
        MUMPS_PAR%ICNTL(3)=0
        MUMPS_PAR%ICNTL(4)=1
      ELSE
        MUMPS_PAR%ICNTL(1)=LU
        MUMPS_PAR%ICNTL(2)=LU
        MUMPS_PAR%ICNTL(3)=LU
        MUMPS_PAR%ICNTL(4)=2
      ENDIF
!-- FRAMEWORK OF MUMPS COMPUTATION
      MUMPS_PAR%ICNTL(5)=0  ! Input matrix format
      MUMPS_PAR%ICNTL(9)=1  ! AX=b
      MUMPS_PAR%ICNTL(18)=3 ! Distributed MPI mode
!-- PRETREATEMENTS
      IF (KPRE(1:4).EQ.'AUTO') THEN
        MUMPS_PAR%ICNTL(6)=7
        MUMPS_PAR%ICNTL(8)=77
        MUMPS_PAR%ICNTL(12)=0
      ELSE IF (KPRE(1:4).EQ.'SANS') THEN
        MUMPS_PAR%ICNTL(6)=0
        MUMPS_PAR%ICNTL(8)=0
        MUMPS_PAR%ICNTL(12)=1
      ELSE
        WRITE(LU,*)'***************************************************'
        WRITE(LU,*)KNOM//' *** Syntax Error ****, Mauvaise option kpre'
        WRITE(LU,*)'***************************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
! -- BLR COMPRESSIONS
      SELECT CASE(KACCE)
      CASE('FR')
        MUMPS_PAR%ICNTL(35)=0
      CASE('FR+')
        MUMPS_PAR%KEEP(370)=1
        MUMPS_PAR%KEEP(371)=1
        MUMPS_PAR%ICNTL(35)=0
      CASE('LR')
        MUMPS_PAR%ICNTL(35)=2
        MUMPS_PAR%ICNTL(36)=0
        MUMPS_PAR%ICNTL(37)=0
        MUMPS_PAR%CNTL(7)=EPSBLR
      CASE('LR+')
        MUMPS_PAR%KEEP(370)=1
        MUMPS_PAR%KEEP(371)=1
        MUMPS_PAR%ICNTL(35)=2
        MUMPS_PAR%ICNTL(36)=0
        MUMPS_PAR%ICNTL(37)=0
        MUMPS_PAR%CNTL(7)=EPSBLR
      CASE DEFAULT
        WRITE(LU,*)'***************************************************'
        WRITE(LU,*)KNOM//' ** Syntax Error ***, Mauvaise option kacce'
        WRITE(LU,*)'***************************************************'
        CALL PLANTE(1)
        STOP
      END SELECT
! -- ORDERING
      SELECT CASE(KRENUM)
      CASE('AUTO')
        MUMPS_PAR%ICNTL(7)=7
        MUMPS_PAR%ICNTL(28)=1
        MUMPS_PAR%ICNTL(29)=0
      CASE('AMD')
        MUMPS_PAR%ICNTL(7)=0
        MUMPS_PAR%ICNTL(28)=1
        MUMPS_PAR%ICNTL(29)=0
      CASE('AMF')
        MUMPS_PAR%ICNTL(7)=2
        MUMPS_PAR%ICNTL(28)=1
        MUMPS_PAR%ICNTL(29)=0
      CASE('SCOTCH')
        MUMPS_PAR%ICNTL(7)=3
        MUMPS_PAR%ICNTL(28)=1
        MUMPS_PAR%ICNTL(29)=0
      CASE('PTSCOTCH')
        MUMPS_PAR%ICNTL(7)=3
        MUMPS_PAR%ICNTL(28)=2
        MUMPS_PAR%ICNTL(29)=1
      CASE('PORD')
        MUMPS_PAR%ICNTL(7)=4
        MUMPS_PAR%ICNTL(28)=1
        MUMPS_PAR%ICNTL(29)=0
      CASE('METIS')
        MUMPS_PAR%ICNTL(7)=5
        MUMPS_PAR%ICNTL(28)=1
        MUMPS_PAR%ICNTL(29)=0
      CASE('PARMETIS')
        MUMPS_PAR%ICNTL(7)=5
        MUMPS_PAR%ICNTL(28)=2
        MUMPS_PAR%ICNTL(29)=2
      CASE('QAMD')
        MUMPS_PAR%ICNTL(7)=6
        MUMPS_PAR%ICNTL(28)=1
        MUMPS_PAR%ICNTL(29)=0
      CASE DEFAULT
        WRITE(LU,*)'***************************************************'
        WRITE(LU,*)KNOM//' ** Syntax Error ***, Mauvaise option krenum'
        WRITE(LU,*)'***************************************************'
        CALL PLANTE(1)
        STOP
      END SELECT
! -- NULL PIVOT DETECTION
      MUMPS_PAR%ICNTL(25)=0
      IF (INPREC .GE. 0) THEN
        MUMPS_PAR%ICNTL(13)=1
        MUMPS_PAR%ICNTL(24)=1
        MUMPS_PAR%CNTL(3)=-10.D0**(-INPREC)
        MUMPS_PAR%CNTL(5)=1.D+6
      ELSE
        MUMPS_PAR%ICNTL(13)=0
        MUMPS_PAR%ICNTL(24)=0
        MUMPS_PAR%CNTL(3)=0.D0
        MUMPS_PAR%CNTL(5)=0.D0
      ENDIF
! -- PIVOT MANAGEMENT
      IF (LPIVOTSTAT) THEN
        MUMPS_PAR%CNTL(1)=-1.D0
      ENDIF
! -- MEMORY MANAGEMENT
      MUMPS_PAR%ICNTL(14)=50
      IF (KMEM(1:2).EQ.'IC') THEN
        MUMPS_PAR%ICNTL(22)=0
        MUMPS_PAR%ICNTL(23)=0
      ELSE IF (KMEM(1:3).EQ.'OOC') THEN
        MUMPS_PAR%ICNTL(22)=1
        MUMPS_PAR%ICNTL(23)=0
        MUMPS_PAR%OOC_TMPDIR='.'
      ELSE
        WRITE(LU,*)'***************************************************'
        WRITE(LU,*)KNOM//' *** Syntax Error ***, Mauvaise option kmem'
        WRITE(LU,*)'***************************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
! -- POSTTREATMENT
      SELECT CASE(KPOST)
      CASE('AUTO')
        MUMPS_PAR%CNTL(2)=1.D-14
        MUMPS_PAR%ICNTL(10)=4
        MUMPS_PAR%ICNTL(11)=2
      CASE('SANS')
        MUMPS_PAR%CNTL(2)=0.D0
        MUMPS_PAR%ICNTL(10)=0
        MUMPS_PAR%ICNTL(11)=0
      CASE('MINI')
        MUMPS_PAR%CNTL(2)=0.D0
        MUMPS_PAR%ICNTL(10)=-2
        MUMPS_PAR%ICNTL(11)=0
      CASE('FORCE')
        MUMPS_PAR%CNTL(2)=1.D-50
        MUMPS_PAR%ICNTL(10)=10
        MUMPS_PAR%ICNTL(11)=1
      CASE DEFAULT
        WRITE(LU,*)'***************************************************'
        WRITE(LU,*)KNOM//' *** Syntax Error ****, Mauvaise option kpost'
        WRITE(LU,*)'***************************************************'
        CALL PLANTE(1)
        STOP
      END SELECT
!
!     SIZE OF THE MATRIX
      MUMPS_PAR%N = 2*NPOIN_TOT
      IF(TYPEXT.EQ.'S') THEN
        MUMPS_PAR%NZ_LOC = NPOIN+NSEGB
      ELSE
        MUMPS_PAR%NZ_LOC = NPOIN+2*NSEGB
      ENDIF
!      IF (.NOT. ASSOCIATED(MUMPS_PAR%IRN_LOC)) THEN
        ALLOCATE(TEMP1(MUMPS_PAR%N),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "TEMP1")
        ALLOCATE(TEMP2(MUMPS_PAR%N),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "TEMP2")
        ALLOCATE(TEMP3(NPOIN),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "TEMP3")
        ALLOCATE(MUMPS_PAR%IRN_LOC(MUMPS_PAR%NZ_LOC),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "MUMPS_PAR%IRN_LOC")
        ALLOCATE(MUMPS_PAR%JCN_LOC(MUMPS_PAR%NZ_LOC),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "MUMPS_PAR%JCN_LOC")
        ALLOCATE(MUMPS_PAR%A_LOC(MUMPS_PAR%NZ_LOC),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "MUMPS_PAR%A_LOC")
        ALLOCATE(MUMPS_PAR%RHS(MUMPS_PAR%N),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, "MUMPS_PAR%RHS")
!      END IF
      TEMP1(:)=0.D0
      TEMP2(:)=0.D0
      TEMP3(:)=0
      MUMPS_PAR%IRN_LOC(:)=0
      MUMPS_PAR%JCN_LOC(:)=0
      MUMPS_PAR%A_LOC(:)=0.0
      MUMPS_PAR%RHS(:)=0.0
!
      NPOIN2=NPOIN/2
      N2=MUMPS_PAR%N/2
      IF (LTRI) THEN
        KTRI=1
        DO K = 1,NPOIN
          RAUX = ABS(DA(K))
          IF (K.LE.NPOIN2) THEN
            IAUX = KNOLG(K)
          ELSE
            IAUX = KNOLG(K-NPOIN2) + N2
          ENDIF
          IF ((RAUX.GT.RMIN).AND.(RAUX.LT.RMAX).AND.(IAUX.GT.0)) THEN
            MUMPS_PAR%IRN_LOC(KTRI) = IAUX
            MUMPS_PAR%JCN_LOC(KTRI) = IAUX
            MUMPS_PAR%A_LOC(KTRI)   = DA(K)
            KTRI=KTRI+1
          END IF
          TEMP1(IAUX)=RHS(K)
          TEMP3(K)=IAUX
        ENDDO
      ELSE
        DO K = 1,NPOIN
          IF (K  .LE.  NPOIN/2) THEN
            MUMPS_PAR%IRN_LOC(K) = KNOLG(K)
            MUMPS_PAR%JCN_LOC(K) = KNOLG(K)
            TEMP1(KNOLG(K))=RHS(K)
            TEMP3(K)=KNOLG(K)
          ELSE
            MUMPS_PAR%IRN_LOC(K) = KNOLG(K-NPOIN/2) + MUMPS_PAR%N/2
            MUMPS_PAR%JCN_LOC(K) = KNOLG(K-NPOIN/2) + MUMPS_PAR%N/2
            TEMP1(KNOLG(K-NPOIN/2)+ MUMPS_PAR%N/2)=RHS(K)
            TEMP3(K)=KNOLG(K-NPOIN/2)+ MUMPS_PAR%N/2
          END IF
          MUMPS_PAR%A_LOC(K) = DA(K)
        ENDDO
      ENDIF
#if defined COMPAD
      WRITE(LU,*) '(AD) COMPAD :: SOLVE_MUMPS_PAR.F: DIRECT CALL OF ',
     &     'MPI_ALLREDUCE NOT AD-READY.'
      WRITE(LU,*) '  PLEASE CONTACT JR @ ADJOINTWARE'
      CALL PLANTE(1)
      STOP
#endif
!     GLOBAL REDUCTION OF RHS VECTOR
      CALL MPI_ALLREDUCE(TEMP1,TEMP2,MUMPS_PAR%N,MPI_DOUBLE_PRECISION,
     &     MPI_SUM,
     &     COMM,IER)
      DO I=1,MUMPS_PAR%N
        MUMPS_PAR%RHS(I)=TEMP2(I)
      END DO
!
      IF (LTRI) THEN
        NBELEM=KTRI
        RAUX=100.D0*KTRI/(NPOIN*1.0)
        WRITE(LU,*)'*************************************************'
        WRITE(LU,*)KNOM//'MATRIX n: ',MUMPS_PAR%N
        WRITE(LU,*)KNOM//'MATRIX filtering 1: ',RAUX,' %'
        WRITE(LU,*)'*************************************************'
      ELSE
        NBELEM = NPOIN
      ENDIF
      IF(TYPEXT.EQ.'S') THEN
        IF (LTRI) THEN
          NAUX=0
          DO K = 1,NSEGB
            RAUX = ABS(XA(K))
            I = TEMP3(GLOSEG(K,1))
            J = TEMP3(GLOSEG(K,2))
            IF ((RAUX.GT.RMIN).AND.(RAUX.LT.RMAX).AND.(I.GT.0).
     &           AND.(J.GT.0)) THEN
              NBELEM = NBELEM + 1
              IF(I.LT.J) THEN
                MUMPS_PAR%IRN_LOC(NBELEM) = I
                MUMPS_PAR%JCN_LOC(NBELEM) = J
              ELSE
                MUMPS_PAR%IRN_LOC(NBELEM) = J
                MUMPS_PAR%JCN_LOC(NBELEM) = I
              ENDIF
              MUMPS_PAR%A_LOC(NBELEM) = XA(K)
            ELSE
              NAUX=NAUX+1
            ENDIF
          ENDDO
          RAUX=100.D0*NAUX/(1.D0*NSEGB)
          WRITE(LU,*)'*************************************************'
          WRITE(LU,*)KNOM//'MATRIX nz_loc: ',MUMPS_PAR%NZ_LOC
          WRITE(LU,*)KNOM//'MATRIX filtering 2: ',raux,' %'
          WRITE(LU,*)'*************************************************'
        ELSE
          DO K = 1,NSEGB
            I = TEMP3(GLOSEG(K,1))
            J = TEMP3(GLOSEG(K,2))
            NBELEM = NBELEM + 1
            IF(I.LT.J) THEN
              MUMPS_PAR%IRN_LOC(NBELEM) = I
              MUMPS_PAR%JCN_LOC(NBELEM) = J
            ELSE
              MUMPS_PAR%IRN_LOC(NBELEM) = J
              MUMPS_PAR%JCN_LOC(NBELEM) = I
            ENDIF
            MUMPS_PAR%A_LOC(NBELEM) = XA(K)
          ENDDO
        ENDIF
      ELSE
        IF (LTRI) THEN
          NAUX=0
          DO K = 1,NSEGB
            RAUX = ABS(XA(K))
            I = TEMP3(GLOSEG(K,1))
            J = TEMP3(GLOSEG(K,2))
            IF ((RAUX.GT.RMIN).AND.(RAUX.LT.RMAX).AND.(I.GT.0).
     &           AND.(J.GT.0)) THEN
              NBELEM = NBELEM + 1
              MUMPS_PAR%A_LOC(NBELEM) = XA(K)
            ELSE
              NAUX=NAUX+1
            ENDIF
          ENDDO
          DO K = 1,NSEGB
            RAUX = ABS(XA(K+NSEGB))
            I = TEMP3(GLOSEG(K,2))
            J = TEMP3(GLOSEG(K,1))
            IF ((RAUX.GT.RMIN).AND.(RAUX.LT.RMAX).AND.(I.GT.0).
     &           AND.(J.GT.0)) THEN
              NBELEM = NBELEM + 1
              MUMPS_PAR%IRN_LOC(NBELEM) = I
              MUMPS_PAR%JCN_LOC(NBELEM) = J
              MUMPS_PAR%A_LOC(NBELEM) = XA(K+NSEGB)
            ELSE
              NAUX=NAUX+1
            ENDIF
          ENDDO
          RAUX=100.D0*NAUX/(2.D0*NSEGB)
          WRITE(LU,*)'*************************************************'
          WRITE(LU,*)KNOM//'MATRIX nz_loc: ',MUMPS_PAR%NZ_LOC
          WRITE(LU,*)KNOM//'MATRIX filtering 2: ',raux,' %'
          WRITE(LU,*)'*************************************************'
        ELSE
          DO K = 1,NSEGB
            I = TEMP3(GLOSEG(K,1))
            J = TEMP3(GLOSEG(K,2))
            NBELEM = NBELEM + 1
            MUMPS_PAR%IRN_LOC(NBELEM) = I
            MUMPS_PAR%JCN_LOC(NBELEM) = J
            MUMPS_PAR%A_LOC(NBELEM) = XA(K)
          ENDDO
          DO K = 1,NSEGB
            I = TEMP3(GLOSEG(K,2))
            J = TEMP3(GLOSEG(K,1))
            NBELEM = NBELEM + 1
            MUMPS_PAR%IRN_LOC(NBELEM) = I
            MUMPS_PAR%JCN_LOC(NBELEM) = J
            MUMPS_PAR%A_LOC(NBELEM) = XA(K+NSEGB)
          ENDDO
        ENDIF
      ENDIF
!
!     -----------
!     RESOLUTION
!     -----------
!
      MUMPS_PAR%JOB = 1
      CALL DMUMPS(MUMPS_PAR)
!
      MUMPS_PAR%JOB = 2
      CALL DMUMPS(MUMPS_PAR)
!
      MUMPS_PAR%JOB = 3
      CALL DMUMPS(MUMPS_PAR)
!
      TEMP1(:)=0.0
      IF(MUMPS_PAR%MYID.EQ. 0 ) THEN
!       An error occured...
        IF(MUMPS_PAR%INFO(1).LT.0) THEN
          WRITE(LU,2001) MUMPS_PAR%INFO(1)
          CALL PLANTE(1)
          STOP
        ENDIF
        DO K = 1,MUMPS_PAR%N
          TEMP1(K)=MUMPS_PAR%RHS(K)
        END DO
      END IF
      TEMP2(:)=0.0
!
!! COMPAD-DCO-MPICHECK  BEGIN  JR2016
#if defined COMPAD
      WRITE(LU,*) '(AD) COMPAD :: SOLVE_MUMPS_PAR.F: DIRECT CALL OF ',
     &     'MPI_ALLREDUCE NOT AD-READY.'
      WRITE(LU,*) '  PLEASE CONTACT JR @ ADJOINTWARE'
      CALL PLANTE(1)
      STOP
#endif
!! COMPAD-DCO-MPICHECK  END  JR2016
      CALL MPI_BCAST(TEMP1,MUMPS_PAR%N,MPI_DOUBLE_PRECISION,
     &               0,COMM,IER)
      CALL MPI_BARRIER(COMM,IER)
      DO  K = 1,NPOIN
        I=TEMP3(K)
        XINC(K)=TEMP1(I)
      ENDDO

!
!     DEALLOCATION
!
      DEALLOCATE(TEMP1)
      DEALLOCATE(TEMP2)
      DEALLOCATE(TEMP3)
      DEALLOCATE(MUMPS_PAR%IRN_LOC)
      DEALLOCATE(MUMPS_PAR%JCN_LOC)
      DEALLOCATE(MUMPS_PAR%A_LOC)
      DEALLOCATE(MUMPS_PAR%RHS)
      CALL MPI_BARRIER(COMM,IER)
!
!     Killing the current instance
!
      MUMPS_PAR%JOB = -2
      CALL DMUMPS(MUMPS_PAR)
      RETURN
2001  FORMAT(1X,'SOLVE_MUMPS: ERROR DURING SOLVE: '
     &     ,/,1X,'ERROR CODE INFO(1): ',1I6)
#else
      WRITE(LU,2019)
2019  FORMAT(1X,'MUMPS_PAR NOT INSTALLED IN THIS SYSTEM',/,1X,
     &     'CHOOSE OTHER METHOD ',///)
      CALL PLANTE(1)
      STOP
!
#endif
!
      END
