!                   *******************
                    SUBROUTINE WALLDIST
!                   *******************
     &(W_DIST,T11,T12,T13,T14,T15,FLBOR,TB,AM1,AM2,S,
     & LIUBOR,IELMNU,NPTFR,MESH)

!***********************************************************************
! TELEMAC2D   V7P3                                             11/2016
!***********************************************************************
!
!brief    COMPUTES THE DISTANCE TO THE CLOSEST WALL FOR
!              SPALART ALLMARAS MODEL-2D COMPUTATION.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AM1,AM2        |<--| WORKING MATRICES
!| LIUBOR         |-->| BOUNDARY CONDITION ON VELOCITIES
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| S              |<--| VOID STRUCTURE
!| T12..FLBOR     |<--| WORKING ARRAYS
!| WDIST          |<--| DISTANCE FROM THE CLOSEST WALL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!***********************************************************************
      USE BIEF, EX_WALLDIST => WALLDIST
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_PARALLEL
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC, ONLY: KADH,KLOG,KNEU,KDIR
!
      IMPLICIT NONE
      INTRINSIC SQRT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER        , INTENT(IN   ) :: IELMNU,NPTFR
      TYPE(BIEF_OBJ) , INTENT(IN   ) :: LIUBOR
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: W_DIST
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: T11,T12,T13,T14,T15,FLBOR,TB
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: AM1,AM2,S
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL :: SOLVER_INFO
      INTEGER :: IPTFR,IP,IELWD,IERR,DEBUG
      DOUBLE PRECISION  :: GRAD_D
      INTEGER, ALLOCATABLE :: LIMDIST(:)
      TYPE(SLVCFG) :: SLVDIST
!
!-----------------------------------------------------------------------
      ALLOCATE(LIMDIST(NPTFR))
      IERR = 0
      DEBUG= 0
!     SETUP LINEAR SOLVER CONFIGURATION STRUCTURE
      SLVDIST%SLV = 1
      SLVDIST%PRECON = 2
      SLVDIST%KRYLOV = 0
      SLVDIST%NITMAX = 1000
      SLVDIST%EPS = 1.D-8
      IF(DEBUG.GT.0) THEN
        SOLVER_INFO = .TRUE.
      ELSE
        SOLVER_INFO = .FALSE.
      ENDIF
!
      IELWD = IELMNU ! ELEMENTS USED FOR S-A
            WRITE(LU,*) 'TELEMAC2D: 31 '
!
      DO IP=1,W_DIST%DIM1
        T11%R(IP) = 1.D0 ! VISC
        T12%R(IP) = 1.D0 ! RHS SOURCE TERM
        T13%R(IP) = 0.D0 ! WALL DIST
        T14%R(IP) = 0.D0 ! WORK
        T15%R(IP) = 0.D0 ! BCS
      ENDDO
!
      DO IPTFR=1,NPTFR
!
        IF(LIUBOR%I(IPTFR).EQ.KLOG.OR.LIUBOR%I(IPTFR).EQ.KADH) THEN
          LIMDIST(IPTFR) = KDIR
          FLBOR%R(IPTFR)   = 0.D0
        ELSE
          LIMDIST(IPTFR) = KNEU
        ENDIF
!
      ENDDO
!
!     CREATES THE MASS MATRIX (AM1)
!
      CALL MATRIX(AM1,'M=N     ','MATMAS          ',IELWD,IELWD,
     &            1.D0,S,S,S,S,S,S,MESH,.FALSE.,S)
!
!     CREATES THE DIFFUSION MATRIX (AM2)
!
      CALL MATRIX(AM2,'M=N     ','MATDIF          ',IELWD,IELWD,
     &            1.D0,S,S,S,T11,T11,T11,MESH,.FALSE.,S)
!
!     PREPARES RHS
      CALL MATVEC('X=AY    ',T13,AM1,T12,1.D0,MESH)
!
!     IMPOSES DIRICHLET CONDITIONS
!
!     BE AWARE: T1 AND T2 ARE ERASED BY DIRICH VIA TB
      IF(NPTFR.GT.0) THEN
        CALL DIRICH(T14,AM2,T13,FLBOR,LIMDIST,
     &              TB,MESH,KDIR,.FALSE.,S)
      ENDIF
!
!     SOLVES THE LINEAR SYSTEM
!
!     BE AWARE: T1 -> T7 ARE ERASED BY SOLVE VIA TB
      IF (DEBUG.GT.0) WRITE(LU,*) 'SOLVING WALL DISTANCE'
      CALL SOLVE(T14,AM2,T13,TB,SLVDIST,SOLVER_INFO,MESH,AM2)
!
!     COMPUTES VERTEX GRADIENT
      CALL VECTOR(T11,'=','GRADF          X',IELWD,
     &            1.D0,T14,S,S,S,S,S,MESH,.FALSE.,S,ASSPAR=.TRUE.)
      CALL VECTOR(T12,'=','GRADF          Y',IELWD,
     &            1.D0,T14,S,S,S,S,S,MESH,.FALSE.,S,ASSPAR=.TRUE.)
      CALL VECTOR(T15 , '=' , 'MASBAS          ' , IELWD ,
     &            1.D0,S,S,S,S,S,S,
     &            MESH,.FALSE.,S,ASSPAR=.TRUE.)
!     NORMALIZES GRADIENT
      CALL OS('X=Y/Z   ', X=T11, Y=T11, Z=T15)
      CALL OS('X=Y/Z   ', X=T12, Y=T12, Z=T15)
!
!     CALCULATES THE WALL DISTANCE
!
      DO IP=1,W_DIST%DIM1
        GRAD_D = T11%R(IP)**2.D0 + T12%R(IP)**2.D0
        IF(GRAD_D + 2.D0*T14%R(IP).GT.0.D0.AND.GRAD_D.GT.0) THEN
          W_DIST%R(IP) = -SQRT(GRAD_D) + SQRT(GRAD_D + 2.D0*T14%R(IP))
        ELSE
          W_DIST%R(IP) = 0.D0
          IERR = IERR + 1
        ENDIF
      ENDDO
      IF(NCSIZE.GT.0)THEN
        IERR = P_SUM(IERR)
      ENDIF
!
      IF(IERR.NE.0)THEN
        WRITE(LU,*) 'WALLDIST (BIEF): WARNING WITH THE WALL DISTANCE '
        WRITE(LU,*) '           COMPUTATION. NEGATIVE DETERMINANT '
        WRITE(LU,*) '           ON ', IERR, 'NODES'
        WRITE(LU,*) '           (PROBABLY OVER CONSTRAINT NODES)'
      ENDIF
      DEALLOCATE(LIMDIST)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE
