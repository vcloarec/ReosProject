      MODULE ALGAE_TRANSP
        USE BIEF_DEF, ONLY : BIEF_OBJ

      USE INITIAL_DROGUES, ONLY : NDRG_CLSS,NODCLSS,PARCLSS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!***********************************************************************
! TELEMAC2D   V6P3                                   14/06/2013
! EDF R&D                                           antoine.joly@edf.fr
!***********************************************************************
!
!brief    MODULE USED FOR ALGAE TRANSPORT
!
!history  A. JOLY
!+        14/06/2013
!+        V6P3
!+   First version
!
! SUBROUTINES MADE AVAILABLE
      PUBLIC :: DEALLOC_ALGAE,ALLOC_ALGAE,INTERP_ALGAE,
     &          DISP_ALGAE
!
!     MAXIMUM NUMBER OF ALGAE CLASSES - FOR MEMORY ALLOCATION PURPOSES
      INTEGER :: NALG_CLSS
!     MAXIMUM NUMBER OF ALGAE CLASSES - FOR MEMORY ALLOCATION PURPOSES
      INTEGER, PARAMETER :: MAX_NALG_CLSS = 10
!
! MODEL VARIABLES
! MEAN FLUID VARIABLES AT THE POSITION OF EACH ALGAE PARTICLES
      TYPE(BIEF_OBJ):: U_X_AV_0
      TYPE(BIEF_OBJ):: U_Y_AV_0
      TYPE(BIEF_OBJ):: U_Z_AV_0
      TYPE(BIEF_OBJ):: U_X_AV
      TYPE(BIEF_OBJ):: U_Y_AV
      TYPE(BIEF_OBJ):: U_Z_AV
      TYPE(BIEF_OBJ):: K_AV_0
      TYPE(BIEF_OBJ):: EPS_AV_0
      TYPE(BIEF_OBJ):: K_AV
      TYPE(BIEF_OBJ):: EPS_AV
      TYPE(BIEF_OBJ):: H_FLU
! VARIABLES OF THE BODIES
      TYPE(BIEF_OBJ):: U_X_0
      TYPE(BIEF_OBJ):: U_Y_0
      TYPE(BIEF_OBJ):: U_Z_0
      TYPE(BIEF_OBJ):: U_X
      TYPE(BIEF_OBJ):: U_Y
      TYPE(BIEF_OBJ):: U_Z
      TYPE(BIEF_OBJ):: V_X_0
      TYPE(BIEF_OBJ):: V_Y_0
      TYPE(BIEF_OBJ):: V_Z_0
      TYPE(BIEF_OBJ):: V_X
      TYPE(BIEF_OBJ):: V_Y
      TYPE(BIEF_OBJ):: V_Z
      TYPE(BIEF_OBJ):: DX_A
      TYPE(BIEF_OBJ):: DY_A
      TYPE(BIEF_OBJ):: DZ_A
      TYPE(BIEF_OBJ):: TEFF
      TYPE(BIEF_OBJ):: I_A_GL
      TYPE(BIEF_OBJ):: DISLODGE
! VARIABLES USED TO CALCULATE THE BASSET HISTORY FORCE
      INTEGER:: NB
      INTEGER:: IB
      INTEGER:: NP
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: T_TIL_P
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE:: A_P
      INTEGER:: NWIN
      DOUBLE PRECISION:: TWIN
      DOUBLE PRECISION,DIMENSION(:,:,:),ALLOCATABLE:: PSI
      DOUBLE PRECISION:: PHI_PLUS
      DOUBLE PRECISION:: PHI_MOINS
      DOUBLE PRECISION:: CB
      DOUBLE PRECISION,DIMENSION(:,:,:),ALLOCATABLE:: FI_P
      DOUBLE PRECISION:: F_TAIL
      DOUBLE PRECISION:: CI_BAS
      ! algae_transp
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: U_I_0_ALGAE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: U_I_ALGAE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: V_I_0_ALGAE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: V_I_ALGAE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: X_I_0_ALGAE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: X_I_ALGAE
      DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: C_I_ALGAE
!
      SAVE
!
      CONTAINS
!
!                   **********************
                    SUBROUTINE ALLOC_ALGAE
!                   **********************
!
     &(NP_TOT,MESH,DT)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.3    MAI 2013                       ANTOINE JOLY
! EDF R&D                                           antoine.joly@edf.fr
!***********************************************************************
!
!brief    ALLOCATES THE VARIABLES ASSOCIATED TO THE ALGAE PARTICLES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |  NAME          |MODE|                  ROLE                        |
! |________________|____|______________________________________________|
! | DT             | -->| NUMERICAL TIME STEP OF THE SIMULATIONS       |
! | MESH           | -->| MESH STRUCTURE WITH ALL THE INFORMATIONS     |
! |                |    | OF THE MESH TREATED                          |
! | NP_TOT         | -->| TOTAL NUMBER OF ALGAE PARTICLES              |
! |________________|____|______________________________________________|
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!
!-----------------------------------------------------------------------
!
! CALLED BY : DERIVE
!
! SUBROUTINE CALLED : INIT_BASSET
!
!***********************************************************************
!
      USE BIEF
      USE STREAMLINE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: NP_TOT
      TYPE(BIEF_MESH) , INTENT(IN) :: MESH
      DOUBLE PRECISION, INTENT(IN) :: DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!=======================================================================
! ALLOCATE THE VARIABLES
!=======================================================================
! MEAN FLUID VARIABLES
      CALL BIEF_ALLVEC(1,U_X_AV_0,'UX_AV0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Y_AV_0,'UY_AV0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Z_AV_0,'UZ_AV0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_X_AV,'U_X_AV',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Y_AV,'U_Y_AV',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Z_AV,'U_Z_AV',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,K_AV_0,'K_AV_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,EPS_AV_0,'E_AV_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,K_AV,'K_AVER',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,EPS_AV,'E_AVER',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,H_FLU,'H_FL_A',NP_TOT,1,0,MESH)
! VARIABLES OF THE BODIES
      CALL BIEF_ALLVEC(1,U_X_0,'UF_X_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Y_0,'UF_Y_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Z_0,'UF_Z_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_X,'UFLU_X',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Y,'UFLU_Y',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,U_Z,'UFLU_Z',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,V_X_0,'VC_X_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,V_Y_0,'VC_Y_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,V_Z_0,'VC_Z_0',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,V_X,'VCOR_X',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,V_Y,'VCOR_Y',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,V_Z,'VCOR_Y',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,DX_A,'DX_ALG',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,DY_A,'DY_ALG',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,DZ_A,'DZ_ALG',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(1,TEFF,'TEFF__',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(2,I_A_GL,'I_A_GL',NP_TOT,1,0,MESH)
      CALL BIEF_ALLVEC(2,DISLODGE,'DISLOD',NP_TOT,1,0,MESH)
! VARIABLES USED TO CALCULATE THE BASSET HISTORY FORCE
      CALL INIT_BASSET(NP_TOT,MESH%DIM1,DT)
!
!=======================================================================
! INITIALISE THE VARIABLES
!=======================================================================
! MEAN FLUID VARIABLES
      CALL OS('X=C     ',X=U_X_AV,C=0.D0)
      CALL OS('X=C     ',X=U_Y_AV,C=0.D0)
      CALL OS('X=C     ',X=U_Z_AV,C=0.D0)
      CALL OS('X=C     ',X=K_AV,C=0.D0)
      CALL OS('X=C     ',X=EPS_AV,C=1.D0)
      CALL OS('X=C     ',X=H_FLU,C=1.D0)
! VARIABLES OF THE BODIES
      CALL OS('X=C     ',X=U_X,C=0.D0)
      CALL OS('X=C     ',X=U_Y,C=0.D0)
      CALL OS('X=C     ',X=U_Z,C=0.D0)
      CALL OS('X=C     ',X=V_X,C=0.D0)
      CALL OS('X=C     ',X=V_Y,C=0.D0)
      CALL OS('X=C     ',X=V_Z,C=0.D0)
      CALL OS('X=C     ',X=DX_A,C=0.D0)
      CALL OS('X=C     ',X=DY_A,C=0.D0)
      CALL OS('X=C     ',X=DZ_A,C=0.D0)
      CALL OS('X=C     ',X=TEFF,C=0.D0)
      DO I = 1,NP_TOT
        DISLODGE%I(I) = 0
      ENDDO
!=======================================================================
! ALLOCATE VARIABLES USED IN PARALLEL TRANSPORT
!=======================================================================
      IF(NCSIZE.GT.1) CALL ORGANISE_ALGS(NP_TOT,NWIN*MESH%DIM1)
!
      RETURN
      END SUBROUTINE ALLOC_ALGAE
!
!                   **********************
                    SUBROUTINE INIT_BASSET
!                   **********************
!
     &(N_A,NDIM,DT)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.3    MAI 2013                       ANTOINE JOLY
! EDF R&D                                           antoine.joly@edf.fr
!***********************************************************************
!
!brief    ALLOCATES THE VARIABLES USED TO CALCULATE THE BASSET
!+        HISTORY FORCE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |  NAME          |MODE|                  ROLE                        |
! |________________|____|______________________________________________|
! | NA             | -->| TOTAL NUMBER OF ALGAE PARTICLES              |
! | NDIM           | -->| NUMBER OF DIMENSIONS TREATED IN THE          |
! |                |    | SIMULATION                                   |
! | DT             | -->| NUMERICAL TIME STEP OF THE SIMULATIONS       |
! |________________|____|______________________________________________|
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!
!-----------------------------------------------------------------------
!
! CALLED BY : ALLOC_ALGAE
!
! SUBROUTINE CALLED : NONE
!
!***********************************************************************
!
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         ,INTENT(IN)    :: N_A
      INTEGER         ,INTENT(IN)    :: NDIM
      DOUBLE PRECISION,INTENT(IN)    :: DT
      INTEGER                        :: I_A
      INTEGER                        :: I_DIM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER:: IP
      INTEGER:: IWIN
!
      NP=10
!
      IF(ALLOCATED(T_TIL_P))DEALLOCATE(T_TIL_P)
      ALLOCATE(T_TIL_P(NP))
      T_TIL_P(1)=0.1D0
      T_TIL_P(2)=0.3D0
      T_TIL_P(3)=1.D0
      T_TIL_P(4)=3.D0
      T_TIL_P(5)=10.D0
      T_TIL_P(6)=40.D0
      T_TIL_P(7)=190.D0
      T_TIL_P(8)=1000.D0
      T_TIL_P(9)=6500.D0
      T_TIL_P(10)=50000.D0
!
      IF(ALLOCATED(A_P))DEALLOCATE(A_P)
      ALLOCATE(A_P(NP))
      A_P(1)=0.23477481312586D0
      A_P(2)=0.28549576238194D0
      A_P(3)=0.28479416718255D0
      A_P(4)=0.26149775537574D0
      A_P(5)=0.32056200511938D0
      A_P(6)=0.35354490689146D0
      A_P(7)=0.39635904496921D0
      A_P(8)=0.42253908596514D0
      A_P(9)=0.48317384225265D0
      A_P(10)=0.63661146557001D0
!
      IF(ALLOCATED(FI_P))DEALLOCATE(FI_P)
      ALLOCATE(FI_P(N_A,2,NP))
      DO I_A=1,N_A
        DO I_DIM=1,2
          DO IP=1,NP
            FI_P(I_A,I_DIM,IP)=0.D0
          END DO
        END DO
      END DO
!
      NWIN=100
      TWIN=REAL(NWIN)*DT
!
      IF(ALLOCATED(PSI))DEALLOCATE(PSI)
      ALLOCATE(PSI(N_A,NDIM,NWIN+1))
      DO I_A=1,N_A
        DO I_DIM=1,NDIM
          DO IWIN=1,NWIN+1
            PSI(I_A,I_DIM,IWIN)=0.D0
          END DO
        END DO
      END DO
!
      RETURN
      END SUBROUTINE INIT_BASSET
!
!                   ************************
                    SUBROUTINE INTERP_ALGAE
!                   ************************
!
     &(NP,NP_TOT,SHP_P,SHZ_P,ELT_P,U_X_AV,U_Y_AV,U_Z_AV,K_AV,EPS_AV,
     & H_FLU,NPOIN,IELM,NDP,NDP2,NPLAN,NELMAX,IKLE,W1,
     & IELMU,NPOINU,UCONV,VCONV,WCONV,
     & AK,EP,H)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.3    MAI 2013                       ANTOINE JOLY
! EDF R&D                                           antoine.joly@edf.fr
!***********************************************************************
!
!brief    INTERPOLATES THE MEAN FLUID VARIABLES AT THE POSITIONS
!+        OF EACH ALGAE PARTICLE
!
!note     THIS SUBROUTINE HAS BEEN WRITTEN TO WORK IN 2D ONLY
!+        AND IT DOES NOT WORK FOR QUADRATIC ELEMENTS (IELM=13)
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |  NAME          |MODE|                  ROLE                        |
! |________________|____|______________________________________________|
! | NP_TOT         | -->| TOTAL NUMBER OF ALGAE PARTICLES              |
! | NP             | -->| NUMBER OF ALGAE PARTICLES IN THE CURRENT     |
! |                |    | PROCESSOR                                    |
! | SHP_P          | -->| 2D BARYCENTRIC COORDINATES FOR EACH ALGAE    |
! |                |    | PARTICLE                                     |
! | ELT_P          | -->| NUMBER OF THE ELEMENT CONTAINING THE ALGAE   |
! |                |    | PARTICLE                                     |
! | U_X_AV,U_Y_AV  |<-- | MEAN FLUID VELOCITIES AT THE POSITION OF     |
! |                |    | EACH ALGAE PARTICLE                          |
! | K_AV,EPS_AV    |<-- | TURBULENT PROPERTIES OF THE FLOW AT THE      |
! |                |    | POSITION OF EACH ALGAE PARTICLE              |
! | H_FLU          |<-- | WATER DEPTH AT THE POSITION OF EACH ALGAE    |
! |                |    | PARTICLE                                     |
! | NPOIN          | -->| NUMBER OF POINTS IN THE MESH                 |
! | IELM           | -->| ELEMENT TYPE OF THE MESH (ASSUMED THE SAME   |
! |                |    | FOR K, EPS AND H)                            |
! | NDP            | -->| NUMBER OF NODES PER ELEMENTS FOR IELM        |
! | NDP            | -->| NUMBER OF NODES PER ELEMENTS FOR IELMU       |
! | NPLAN          | -->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS    |
! | NELMAX         | -->| MAXIMUM NUMBER OF ELEMENTS IN 2D             |
! | IKLE           | -->| CONNECTIVITY TABLE                           |
! | W1             | -->| TEMPORARY WORK ARRAY USED AS A BUFFER FOR    |
! |                |    | FREQUENCY                                    |
! | IELMU          | -->| ELEMENT TYPE FOR U, V AND W                  |
! | NPOINU         | -->| NUMBER OF POINTS IN THE MESH FOR VARIABLES   |
! |                |    | U, V AND W                                   |
! | UCONV,VCONV,   | -->| THE X, Y AND Z COMPONENTS OF THE FLUID       |
! |   WCONV        |    | VELOCITIES AT EACH NODE OF THE MESH          |
! | AK,EPS         | -->| TURBULENT PROPERTIES OF THE FLOW AT EACH     |
! |                |    | NODE OF THE MESH                             |
! | H              | -->| WATER DEPTH AT EACH NODE OF THE MESH         |
! |________________|____|______________________________________________|
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!
!-----------------------------------------------------------------------
!
! CALLED BY : DERIVE
!
! SUBROUTINE CALLED : INIT_BASSET
!
!***********************************************************************
!
      USE BIEF
      USE STREAMLINE, ONLY : BIEF_INTERP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! FLOW VARIABLES THAT ARE USED IN THE INTERPOLATION
      INTEGER         , INTENT(IN)     :: NDP,NDP2,NPLAN,NELMAX
      INTEGER         , INTENT(IN)     :: NPOIN,IELM,IELMU,NPOINU
      DOUBLE PRECISION, INTENT(IN)     :: UCONV(NPOINU),VCONV(NPOINU)
      DOUBLE PRECISION, INTENT(IN)     :: WCONV(NPOINU)
      DOUBLE PRECISION, INTENT(IN)     :: AK(NPOIN),EP(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: H(NPOIN)
! ALGAE VARIABLES THAT NEED TO BE INTERPOLATED
      INTEGER         , INTENT(IN)     :: NP,NP_TOT
      DOUBLE PRECISION, INTENT(IN)     :: SHP_P(NDP,NP_TOT)
      INTEGER         , INTENT(IN)     :: IKLE(NELMAX,NDP2)
      DOUBLE PRECISION, INTENT(IN)     :: SHZ_P(NP_TOT)
      INTEGER         , INTENT(IN)     :: ELT_P(NP_TOT)
      DOUBLE PRECISION, INTENT(OUT)    :: U_X_AV(NP_TOT)
      DOUBLE PRECISION, INTENT(OUT)    :: U_Y_AV(NP_TOT)
      DOUBLE PRECISION, INTENT(OUT)    :: U_Z_AV(NP_TOT)
      DOUBLE PRECISION, INTENT(OUT)    :: K_AV(NP_TOT)
      DOUBLE PRECISION, INTENT(OUT)    :: EPS_AV(NP_TOT)
      DOUBLE PRECISION, INTENT(OUT)    :: H_FLU(NP_TOT)
! BUFFERS (USED ON FREQUENCY)
      DOUBLE PRECISION, INTENT(IN)    :: W1(*)
      INTEGER FRE(1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!=======================================================================
! INITIALISE THE VARIABLES
!=======================================================================
! U,V,W
      CALL BIEF_INTERP(UCONV,U_X_AV,SHP_P,NDP,SHZ_P,ELT_P,W1,FRE,
     &           ELT_P,NP,NPOINU,NPLAN,IELMU,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
      CALL BIEF_INTERP(VCONV,U_Y_AV,SHP_P,NDP,SHZ_P,ELT_P,W1,FRE,
     &           ELT_P,NP,NPOINU,NPLAN,IELMU,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
      CALL BIEF_INTERP(WCONV,U_Z_AV,SHP_P,NDP,SHZ_P,ELT_P,W1,FRE,
     &           ELT_P,NP,NPOINU,NPLAN,IELMU,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
! K, EPS
      CALL BIEF_INTERP(AK,K_AV,SHP_P,NDP,SHZ_P,ELT_P,W1,FRE,
     &           ELT_P,NP,NPOIN,1,IELM,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
      CALL BIEF_INTERP(EP,EPS_AV,SHP_P,NDP,SHZ_P,ELT_P,W1,FRE,
     &           ELT_P,NP,NPOIN,1,IELM,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
! H
      CALL BIEF_INTERP(H,H_FLU,SHP_P,NDP,SHZ_P,ELT_P,W1,FRE,
     &           ELT_P,NP,NPOIN,1,IELM,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
!
      RETURN
      END SUBROUTINE INTERP_ALGAE
!
!                   ********************
                    SUBROUTINE DISP_ALGAE
!                   ********************
!
     & (NA_TOT,NA,NDIM,DT,AT,U_X_AV_0,U_Y_AV_0,U_Z_AV_0,K_AV_0,
     &  EPS_AV_0,H_FLU,U_X_AV,U_Y_AV,U_Z_AV,U_X_0,U_Y_0,U_Z_0,V_X_0,
     &  V_Y_0,V_Z_0,DX_A,DY_A,DZ_A,ELEM_ALG,U_X,U_Y,U_Z,V_X,V_Y,V_Z,
     &  X_A,Y_A,Z_A,LT,DALGAE,RALGAE,EALGAE,TALGAE,YALGAE,
     &  REL_ALGAE)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.3    MAI 2013                       ANTOINE JOLY
! EDF R&D                                           antoine.joly@edf.fr
!***********************************************************************
!
!brief    CALCULATES THE TRANSPORT OF ALGAE PARTICLES AND OUTPUTS THE
!+        ASSOCIATED VALUES
!
!note     THIS SUBROUTINE HAS BEEN WRITTEN TO WORK IN 2D ONLY
!
!history  A. JOLY
!+        14/06/2013
!+        V6P3
!+   First version
!
!history  M.S.TURNBULL (HRW)
!+        26/11/2019
!+        V8P2
!+   Algae Dislodgement
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |  NAME          |MODE|                  ROLE                        |
! |________________|____|______________________________________________|
! | AT             | -->| TIME IN SECONDS                              |
! | NA_TOT         | -->| TOTAL NUMBER OF ALGAE PARTICLES              |
! | NA             | -->| NUMBER OF ALGAE PARTICLES IN THE CURRENT     |
! |                |    | PROCESSOR                                    |
! | NDIM           | -->| NUMBER OF DIMENSIONS TREATED                 |
! | DT             | -->| TIME STEP OF THE SIMULATION                  |
! |                |    | RELEASED                                     |
! | U_X_AV_0,      | -->| MEAN FLUID VELOCITIES AT THE POSITION OF     |
! |   U_Y_AV_0,    |    | EACH ALGAE PARTICLE ONE TIME STEP BEFORE     |
! |     U_Z_AV_0   |    | THE CALCULATIONS                             |
! | U_X_AV,U_Y_AV, | -->| MEAN FLUID VELOCITIES AT THE POSITION OF     |
! |   U_Y_AV       |    | EACH ALGAE PARTICLE                          |
! | K_AV_0,        | -->| TURBULENT PROPERTIES OF THE FLOW AT THE      |
! |   EPS_AV_0     |    | POSITION OF EACH ALGAE PARTICLE ONE TIME     |
! |                |    | STEP BEFORE THE CALCULATIONS                 |
! | H_FLU          | -->| WATER DEPTH AT THE POSITION OF EACH ALGAE    |
! |                |    | PARTICLE                                     |
! | U_X_0,U_Y_0,   | -->| TURBULENT FLUID VELOCITIES AT THE POSITION   |
! |   U_Z_0        |    | OF EACH ALGAE PARTICLE ONE TIME STEP BEFORE  |
! |                |    | THE CALCULATIONS                             |
! | U_X,U_Y,U_Z    |<-- | TURBULENT FLUID VELOCITIES AT THE POSITION   |
! |                |    | OF EACH ALGAE PARTICLE                       |
! | V_X_0,V_Y_0,   | -->| BODY VELOCITIES AT THE POSITION OF EACH      |
! |   V_Z_0        |    | ALGAE PARTICLE ONE TIME STEP BEFORE          |
! |                |    | THE CALCULATIONS                             |
! | V_X,V_Y,V_Z    |<-- | BODY VELOCITIES AT THE POSITION OF EACH      |
! |                |    | ALGAE PARTICLE                               |
! | X_A,Y_A,Z_A    |<-->| POSITION OF THE ALGAE PARTICLES BEFORE AND   |
! |                |    | AFTER DISPLACEMENT                           |
! | DX_A,DY_A,DZ_A |<-- | DISPLACEMENT OF EACH ALGAE PARTICLE          |
! | ELEM_ALG       |<-->| NUMBER OF THE ELEMENT CONTAINING THE ALGAE   |
! |                |    | PARTICLE                                     |
! | LT             | -->| TIME ITERATION OF THE SIMULATION             |
! | DALGAE         | -->| DIAMETER OF THE ALGAE PARTICLES              |
! | RALGAE         | -->| DENSITY OF THE ALGAE PARTICLES               |
! | EALGAE         | -->| THICKNESS OF THE ALGAE PARTICLES             |
! | TALGAE         | -->| TIME AT WHICH ALGAE PARTICLES ARE RELEASED   |
! | YALGAE         | -->| ALGAE TYPE OF THE PARTICLES                  |
! | REL_ALGAE      | -->| TYPE OF ALGAE RELEASE                        |
! |________________|____|______________________________________________|
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!
!-----------------------------------------------------------------------
!
! CALLED BY : DERIVE
!
! SUBROUTINE CALLED : NONE
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! PARAMETRES
      DOUBLE PRECISION :: PI
      DOUBLE PRECISION :: E
      DOUBLE PRECISION,PARAMETER     :: C0=2.1D0
      DOUBLE PRECISION,PARAMETER     :: RHO_F=1000.D0
      DOUBLE PRECISION,PARAMETER     :: NU=0.000001D0
! VARIABLES DEPENDENT ON THE ALGAE SIMULATIONS
      INTEGER         ,INTENT(IN)    :: NA_TOT
      INTEGER         ,INTENT(IN)    :: NA
      DOUBLE PRECISION,INTENT(IN)    :: DT,AT
      INTEGER                        :: I_A
      INTEGER         ,INTENT(IN)    :: LT
      DOUBLE PRECISION,INTENT(IN)    :: DALGAE(*)
      DOUBLE PRECISION,INTENT(IN)    :: RALGAE(*)
      DOUBLE PRECISION,INTENT(IN)    :: EALGAE(*)
      DOUBLE PRECISION,INTENT(IN)    :: TALGAE(*)
      INTEGER         ,INTENT(IN)    :: YALGAE(*)
      INTEGER         ,INTENT(IN)    :: REL_ALGAE(*)
! CONSTANTS OF THE BODIES
      DOUBLE PRECISION               :: S
      DOUBLE PRECISION               :: OMEGA
      DOUBLE PRECISION               :: M
      DOUBLE PRECISION               :: MASS
! TEMPORARY VARIABLES TO CALCULATE VARIABLES FOR EACH DIRECTION
      INTEGER         ,INTENT(IN)    :: NDIM
      INTEGER                        :: I_DIM
! PROPERTIES OF THE BODIES
      DOUBLE PRECISION               :: F_A
      DOUBLE PRECISION               :: F_B
      DOUBLE PRECISION               :: TAU_PART
      DOUBLE PRECISION               :: FI_C
      DOUBLE PRECISION               :: D_ALG,R_ALG,E_ALG,T_ALG
      INTEGER                        :: Y_ALG,REL_ALG
! PROPERTIES OF THE FLOW
      DOUBLE PRECISION               :: T_I
      DOUBLE PRECISION               :: B_I
      DOUBLE PRECISION               :: NORME_U0_V0
! MEAN FLUID VARIABLES
      DOUBLE PRECISION,INTENT(IN)    :: U_X_AV_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: U_Y_AV_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: U_Z_AV_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: U_X_AV(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: U_Y_AV(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: U_Z_AV(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: K_AV_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: EPS_AV_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: H_FLU(NA_TOT)
! VARIABLES OF THE BODIES
      DOUBLE PRECISION,INTENT(IN)    :: U_X_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: U_Y_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: U_Z_0(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: U_X(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: U_Y(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: U_Z(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: V_X_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: V_Y_0(NA_TOT)
      DOUBLE PRECISION,INTENT(IN)    :: V_Z_0(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: V_X(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: V_Y(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: V_Z(NA_TOT)
      DOUBLE PRECISION,INTENT(INOUT) :: X_A(NA_TOT)
      DOUBLE PRECISION,INTENT(INOUT) :: Y_A(NA_TOT)
      DOUBLE PRECISION,INTENT(INOUT) :: Z_A(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: DX_A(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: DY_A(NA_TOT)
      DOUBLE PRECISION,INTENT(OUT)   :: DZ_A(NA_TOT)
      INTEGER         ,INTENT(IN)    :: ELEM_ALG(NA_TOT)
! RANDOM NUMBERS
      DOUBLE PRECISION               :: RAND1
      DOUBLE PRECISION               :: XI_G_I
      DOUBLE PRECISION               :: RAND2
      DOUBLE PRECISION               :: XI_CAPG_I
      DOUBLE PRECISION               :: RAND3
      DOUBLE PRECISION               :: XI_CAPP_I
! VARIABLES USED TO FIND THE DRAG COEFFICIENT
      DOUBLE PRECISION               :: RE
      DOUBLE PRECISION               :: CD
      DOUBLE PRECISION               :: PHI_1
      DOUBLE PRECISION               :: PHI_2
      DOUBLE PRECISION               :: PHI_3
      DOUBLE PRECISION               :: PHI_4
! VARIABLES USED TO CALCULATE THE STOCHASTIC INTEGRALS
      DOUBLE PRECISION               :: GAMMA_I
      DOUBLE PRECISION               :: CAPGAMMA_I
      DOUBLE PRECISION               :: CAPPHI_I
      DOUBLE PRECISION               :: COV_G_I2
      DOUBLE PRECISION               :: COV_CAPG_I2
      DOUBLE PRECISION               :: COV_CAPP_I2
      DOUBLE PRECISION               :: COV_G_ICAPG_I
      DOUBLE PRECISION               :: COV_G_ICAPP_I
      DOUBLE PRECISION               :: COV_CAPG_ICAPP_I
      DOUBLE PRECISION               :: L11
      DOUBLE PRECISION               :: L21
      DOUBLE PRECISION               :: L22
      DOUBLE PRECISION               :: L31
      DOUBLE PRECISION               :: L32
      DOUBLE PRECISION               :: L33
! VARIABLES USED FOR THE EXACT INTEGRATOR
      DOUBLE PRECISION               :: ALPHA
      DOUBLE PRECISION               :: BETA
      DOUBLE PRECISION               :: C_CHECK
      DOUBLE PRECISION               :: K_CHECK
      DOUBLE PRECISION               :: Q_CHECK
      DOUBLE PRECISION               :: G_CHECK
      INTEGER:: IP
      INTEGER:: IWIN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
      E = EXP( 1.D0 )
!
!-----------------------------------------------------------------------
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! C
! C FICHIER TXT AVEC LE JOURNAL DES ERREURS
! !       WRITE(LOG_NAME,'(A,I1,A)')'../erreur_log',RANG,'.txt'
! !       OPEN(9999,FILE=LOG_NAME)
! !       OPEN(9999,FILE='../erreur_log.txt')
! !
! !             WRITE(9999,*)LT
! !             WRITE(9999,*)PSI(1,1,:)

!=======================================================================
! PREAMBLE OF THE CALCULATIONS
!=======================================================================
! ALLOCATE VARIABLES
      IF(.NOT.ALLOCATED(U_I_0_ALGAE))ALLOCATE(U_I_0_ALGAE(NDIM))
      IF(.NOT.ALLOCATED(U_I_ALGAE))ALLOCATE(U_I_ALGAE(NDIM))
      IF(.NOT.ALLOCATED(V_I_0_ALGAE))ALLOCATE(V_I_0_ALGAE(NDIM))
      IF(.NOT.ALLOCATED(V_I_ALGAE))ALLOCATE(V_I_ALGAE(NDIM))
      IF(.NOT.ALLOCATED(X_I_0_ALGAE))ALLOCATE(X_I_0_ALGAE(NDIM))
      IF(.NOT.ALLOCATED(X_I_ALGAE))ALLOCATE(X_I_ALGAE(NDIM))
      IF(.NOT.ALLOCATED(C_I_ALGAE))ALLOCATE(C_I_ALGAE(NDIM))
!
!=======================================================================
! START THE CALCULATIONS FOR EACH PARTICLE
!=======================================================================
      DO I_A=1,NA
!
!       SET PROPERTY FOR THAT CLASS
        D_ALG = DALGAE(PARCLSS%I(I_A))
        R_ALG = RALGAE(PARCLSS%I(I_A))
        E_ALG = EALGAE(PARCLSS%I(I_A))
        T_ALG = TALGAE(PARCLSS%I(I_A))
        Y_ALG = YALGAE(PARCLSS%I(I_A))
        REL_ALG = REL_ALGAE(PARCLSS%I(I_A))
! CONSTANTS
        IF( Y_ALG.EQ.1 )THEN !SPHERE
          S = PI*D_ALG**2/4.D0
          OMEGA = PI*D_ALG**3/6.D0
          M = 0.5D0*RHO_F*OMEGA
        ELSEIF( Y_ALG.EQ.2 )THEN !IRIDAEA FLACCIDA (CLOSE TO ULVA)
          S = PI*D_ALG**2/4.D0
          OMEGA = S*E_ALG
          M = 3.57D0*RHO_F*OMEGA
        ELSEIF( Y_ALG.EQ.3 )THEN !PELVETIOPSIS LIMITATA
          S = PI*D_ALG**2/4.D0
          OMEGA = S*E_ALG
          M = 4.64D0*RHO_F*OMEGA
        ELSEIF( Y_ALG.EQ.4 )THEN !GIGARTINA LEPTORHYNCHOS
          S = PI*D_ALG**2/4.D0
          OMEGA = S*E_ALG
          M = 3.26D0*RHO_F*OMEGA
        END IF
        MASS = R_ALG*OMEGA
        CB = 6.D0*D_ALG**2*RHO_F*SQRT(PI*NU)
!
!=======================================================================
! CHECK TO SEE IF THE TRANSPORT NEEDS TO BE CALCULATED
!=======================================================================
        IF( ELEM_ALG(I_A).LE.0 ) GOTO 12
        IF( H_FLU(I_A).LT.D_ALG ) GOTO 12
        IF(REL_ALG.EQ.2) THEN
          IF( DISLODGE%I(I_A).EQ.0 ) GOTO 12
        ELSE
          IF( AT.LT.T_ALG ) GOTO 12
        ENDIF
        IB = LT - INT( T_ALG/DT ) + 1
!
!=======================================================================
! REDEFINE THE PREVIOUS VARIABLES
!=======================================================================
        U_I_0_ALGAE(1)=U_X_0(I_A)
        U_I_0_ALGAE(2)=U_Y_0(I_A)
        V_I_0_ALGAE(1)=V_X_0(I_A)
        V_I_0_ALGAE(2)=V_Y_0(I_A)
        X_I_0_ALGAE(1)=X_A(I_A)
        X_I_0_ALGAE(2)=Y_A(I_A)
        IF(NDIM.EQ.3)THEN
          U_I_0_ALGAE(3)=U_Z_0(I_A)
          V_I_0_ALGAE(3)=V_Z_0(I_A)
          X_I_0_ALGAE(3)=Z_A(I_A)
        END IF
!=======================================================================
! REDEFINE THE FLUID PROPERTIES
!=======================================================================
        T_I=1.D0/(0.5D0+0.75D0*C0)*K_AV_0(I_A)/EPS_AV_0(I_A)
! IN C_I_ALGAE IT IS ASSUMED THAT
! 1/RHO_F*dP/dX_i = (U_X_AV(I_A)-U_X_AV_0(I_A))/DT
!    => maybe use variation in surface elevation
!               ALONG X (I.E. HYDROSTATIC PRESSURE)
        C_I_ALGAE(1)=(U_X_AV(I_A)-U_X_AV_0(I_A))/DT
     &               +1.D0/T_I*U_X_AV_0(I_A)
        C_I_ALGAE(2)=(U_Y_AV(I_A)-U_Y_AV_0(I_A))/DT
     &               +1.D0/T_I*U_Y_AV_0(I_A)
        B_I=(C0*EPS_AV_0(I_A))**0.5
        NORME_U0_V0=SQRT((U_X_0(I_A)-V_X_0(I_A))**2+(U_Y_0(I_A)
     &   -V_Y_0(I_A))**2)
        IF(NDIM.EQ.3)THEN
          C_I_ALGAE(2)=(U_Z_AV(I_A)-U_Z_AV_0(I_A))/DT
     &                 +1.D0/T_I*U_Z_AV_0(I_A)
          NORME_U0_V0=SQRT((U_X_0(I_A)-V_X_0(I_A))**2+(U_Y_0(I_A)
     &     -V_Y_0(I_A))**2+(U_Z_0(I_A)-V_Z_0(I_A))**2)
        END IF
!=======================================================================
        DO I_DIM=1,NDIM
!=======================================================================
!=======================================================================
! REDEFINE THE BODY PROPERTIES
!=======================================================================
          CI_BAS=0.D0
          F_TAIL=0.D0
!
          IF(IB.LE.NWIN)THEN
            NB=IB
!
            CI_BAS=CI_BAS+2.D0/3.D0*CB*PSI(I_A,I_DIM,NB)*SQRT(DT)
     &       *(3.D0*SQRT(REAL(NB))+2.D0*(REAL(NB)-1.D0)**(1.5D0)
     &       -2.D0*REAL(NB)**(1.5D0))
            DO IWIN=1,NB-1
              CI_BAS=CI_BAS+4.D0/3.D0*CB*SQRT(DT)*PSI(I_A,I_DIM,IWIN)
     &         *((REAL(IWIN)-1.D0)**(1.5D0)-2.D0*REAL(IWIN)**(1.5D0)
     &         +(REAL(IWIN)+1.D0)**(1.5D0))
            END DO
!
            CI_BAS=CI_BAS+F_TAIL
!
            F_A=(RHO_F*OMEGA+M+4.D0/3.D0*CB*SQRT(DT))/(MASS+M+4.D0/3.D0
     &        *CB*SQRT(DT))
!
            RE=NORME_U0_V0*D_ALG/NU
            IF(Y_ALG.EQ.1)THEN !SPHERE
              IF(RE.EQ.0.D0)THEN
                CD=0.D0
                F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT))*D_ALG)
              ELSEIF(RE.LT.0.4)THEN
                CD=24.D0/RE
                F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT))*D_ALG)
              ELSEIF(RE.GT.1000000)THEN
                CD=0.2
                F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT)))
              ELSE
                PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &           +(4.D0*RE**(-0.33))**10+0.4D0**10
                PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &           +0.2**(-10))
                CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)**0.1
                F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT)))
              END IF
            ELSEIF(Y_ALG.EQ.2)THEN !IRIDAEA FLACCIDA (CLOSE TO ULVA)
              IF(RE.GE.14073.D0)THEN
                  CD=EXP(6.822121D0-0.800627D0*LOG(RE))
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
              ELSE
                IF(RE.EQ.0.D0)THEN
                  CD=0.D0
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.LT.0.4)THEN
                  CD=24.D0/RE
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.GT.1000000)THEN
                  CD=0.2
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                ELSE
                  PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &             +(4.D0*RE**(-0.33))**10+0.4D0**10
                  PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                  PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                  PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &             +0.2**(-10))
                  CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)
     &             **0.1
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                END IF
              END IF
            ELSEIF(Y_ALG.EQ.3)THEN !PELVETIOPSIS LIMITATA
              IF(RE.GE.28611.D0)THEN
                  CD=EXP(8.214783D0-0.877036D0*LOG(RE))
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
              ELSE
                IF(RE.EQ.0.D0)THEN
                  CD=0.D0
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.LT.0.4)THEN
                  CD=24.D0/RE
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.GT.1000000)THEN
                  CD=0.2
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                ELSE
                  PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &             +(4.D0*RE**(-0.33))**10+0.4D0**10
                  PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                  PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                  PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &             +0.2**(-10))
                  CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)
     &             **0.1
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                END IF
              END IF
            ELSEIF(Y_ALG.EQ.4)THEN !GIGARTINA LEPTORHYNCHOS
              IF(RE.GE.17981.D0)THEN
                  CD=EXP(6.773712D0-0.774252D0*LOG(RE))
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
              ELSE
                IF(RE.EQ.0.D0)THEN
                  CD=0.D0
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.LT.0.4)THEN
                  CD=24.D0/RE
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.GT.1000000)THEN
                  CD=0.2
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                ELSE
                  PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &             +(4.D0*RE**(-0.33))**10+0.4D0**10
                  PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                  PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                  PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &             +0.2**(-10))
                  CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)
     &             **0.1
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                END IF
              END IF
            END IF
!
            TAU_PART=1.D0/F_B
!
            FI_C=CI_BAS/(MASS+M+4.D0/3.D0*CB*SQRT(DT))
          ELSE
            NB=NWIN
!
            DO IP=1,NP
              PHI_PLUS=(EXP(DT/(2.D0*T_TIL_P(IP)*TWIN))-1.D0)/(DT
     &         /(2.D0*T_TIL_P(IP)*TWIN))
              PHI_MOINS=(EXP(-DT/(2.D0*T_TIL_P(IP)*TWIN))-1.D0)/(-DT
     &         /(2.D0*T_TIL_P(IP)*TWIN))
!
              FI_P(I_A,I_DIM,IP)=2.D0*CB*SQRT(E*T_TIL_P(IP)*TWIN)
     &         *EXP(-1.D0/(2.D0*T_TIL_P(IP)))*(PSI(I_A,I_DIM,NB)*(1.D0
     &         -PHI_MOINS)+PSI(I_A,I_DIM,NB+1)*EXP(-DT/(2.D0*T_TIL_P(IP)
     &         *TWIN))*(PHI_PLUS-1))+EXP(-DT/(2.D0*T_TIL_P(IP)*TWIN))
     &         *FI_P(I_A,I_DIM,IP)
!
              F_TAIL=F_TAIL+A_P(IP)*FI_P(I_A,I_DIM,IP)
            END DO
!
            CI_BAS=CI_BAS+2.D0/3.D0*CB*PSI(I_A,I_DIM,NB)*SQRT(DT)
     &       *(3.D0*SQRT(REAL(NB))+2.D0*(REAL(NB)-1.D0)**(1.5D0)
     &       -2.D0*REAL(NB)**(1.5D0))
            DO IWIN=1,NB-1
              CI_BAS=CI_BAS+4.D0/3.D0*CB*SQRT(DT)*PSI(I_A,I_DIM,IWIN)
     &         *((REAL(IWIN)-1.D0)**(1.5D0)-2.D0*REAL(IWIN)**(1.5D0)
     &         +(REAL(IWIN)+1.D0)**(1.5D0))
            END DO
            CI_BAS=CI_BAS+F_TAIL
!
            F_A=(RHO_F*OMEGA+M+4.D0/3.D0*CB*SQRT(DT))/(MASS+M+4.D0/3.D0
     &        *CB*SQRT(DT))
!
            RE=NORME_U0_V0*D_ALG/NU
            IF(Y_ALG.EQ.1)THEN !SPHERE
              IF(RE.EQ.0.D0)THEN
                CD=0.D0
                F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT))*D_ALG)
              ELSEIF(RE.LT.0.4)THEN
                CD=24.D0/RE
                F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT))*D_ALG)
              ELSEIF(RE.GT.1000000)THEN
                CD=0.2
                F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT)))
              ELSE
                PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &           +(4.D0*RE**(-0.33))**10+0.4D0**10
                PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &           +0.2**(-10))
                CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)**0.1
                F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &           *SQRT(DT)))
              END IF
            ELSEIF(Y_ALG.EQ.2)THEN !IRIDAEA FLACCIDA (CLOSE TO ULVA)
              IF(RE.GE.14073.D0)THEN
                  CD=EXP(6.822121D0-0.800627D0*LOG(RE))
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
              ELSE
                IF(RE.EQ.0.D0)THEN
                  CD=0.D0
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.LT.0.4)THEN
                  CD=24.D0/RE
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.GT.1000000)THEN
                  CD=0.2
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                ELSE
                  PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &             +(4.D0*RE**(-0.33))**10+0.4D0**10
                  PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                  PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                  PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &             +0.2**(-10))
                  CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)
     &             **0.1
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                END IF
              END IF
            ELSEIF(Y_ALG.EQ.3)THEN !PELVETIOPSIS LIMITATA
              IF(RE.GE.28611.D0)THEN
                  CD=EXP(8.214783D0-0.877036D0*LOG(RE))
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
              ELSE
                IF(RE.EQ.0.D0)THEN
                  CD=0.D0
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.LT.0.4)THEN
                  CD=24.D0/RE
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.GT.1000000)THEN
                  CD=0.2
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                ELSE
                  PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &             +(4.D0*RE**(-0.33))**10+0.4D0**10
                  PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                  PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                  PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &             +0.2**(-10))
                  CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)
     &             **0.1
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                END IF
              END IF
            ELSEIF(Y_ALG.EQ.4)THEN !GIGARTINA LEPTORHYNCHOS
              IF(RE.GE.17981.D0)THEN
                  CD=EXP(6.773712D0-0.774252D0*LOG(RE))
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
              ELSE
                IF(RE.EQ.0.D0)THEN
                  CD=0.D0
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.LT.0.4)THEN
                  CD=24.D0/RE
                  F_B=(RHO_F*S*24.D0*NU)/(2.D0*(MASS+M+4.D0/3.D0*CB
     &             *SQRT(DT))*D_ALG)
                ELSEIF(RE.GT.1000000)THEN
                  CD=0.2
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                ELSE
                  PHI_1=(24.D0*RE**(-1))**10+(21.D0*RE**(-0.67))**10
     &             +(4.D0*RE**(-0.33))**10+0.4D0**10
                  PHI_2=1.D0/((0.148D0*RE**0.11)**(-10)+0.5D0**(-10))
                  PHI_3=(1.57D0*10.D0**8*RE**(-1.625))**10
                  PHI_4=1.D0/((6.D0*10.D0**(-17)*RE**2.63)**(-10)
     &             +0.2**(-10))
                  CD=(1.D0/((PHI_1+PHI_2)**(-1)+PHI_3**(-1))+PHI_4)
     &             **0.1
                  F_B=(RHO_F*S*CD*NORME_U0_V0)/(2.D0*(MASS+M+4.D0/3.D0
     &             *CB*SQRT(DT)))
                END IF
              END IF
            END IF
!
            TAU_PART=1.D0/F_B
!
            FI_C=CI_BAS/(MASS+M+4.D0/3.D0*CB*SQRT(DT))
!
          END IF
! T_I SHOULD NEVER EQUAL TAU_PART, PEFORM A CHECK JUST IN CASE
          IF(T_I-TAU_PART.EQ.0)THEN
            WRITE(LU,*)''
            WRITE(LU,*)'                           **************'
            WRITE(LU,*)'                           *T_I=TAU_PART*'
            WRITE(LU,*)'                           **************'
            WRITE(LU,*)''
            CALL PLANTE(1)
            STOP
          END IF
!=======================================================================
! RANDOM NUMBERS
!=======================================================================
          CALL RANDOM_NUMBER(RAND1)
          XI_G_I=(RAND1*2.D0-1.D0)*(12.D0**0.5)/2.D0
          CALL RANDOM_NUMBER(RAND2)
          XI_CAPG_I=(RAND2*2.D0-1.D0)*(12.D0**0.5)/2.D0
          CALL RANDOM_NUMBER(RAND3)
          XI_CAPP_I=(RAND3*2.D0-1.D0)*(12.D0**0.5)/2.D0
!=======================================================================
! FLUID VELOCITIES
!=======================================================================
! VARIABLES OF THE EXACT INTEGRATOR
          ALPHA=EXP(-DT/T_I)
!
          COV_G_I2=(1.D0-ALPHA**2)*B_I**2*T_I/2.D0
!
          L11=SQRT(COV_G_I2)
          GAMMA_I=L11*XI_G_I
!FLUID VELOCITY
          U_I_ALGAE(I_DIM)=ALPHA*U_I_0_ALGAE(I_DIM)
     &               +(1.D0-ALPHA)*C_I_ALGAE(I_DIM)*T_I
     &               +GAMMA_I
!
!=======================================================================
! BODY VELOCITIES
!=======================================================================
! VARIABLES OF THE EXACT INTEGRATOR
          BETA=EXP(-DT/TAU_PART)
          C_CHECK=(T_I-F_A*TAU_PART)/(T_I-TAU_PART)
          K_CHECK=F_A/C_CHECK-1.D0
          Q_CHECK=K_CHECK*T_I*TAU_PART/(T_I+TAU_PART)
!
          COV_CAPG_I2=(B_I*C_CHECK)**2*((1.D0-ALPHA**2)*T_I/2.D0+(1.D0
     &     -BETA**2)*K_CHECK**2*TAU_PART/2.D0+2.D0*(1.D0-ALPHA*BETA)
     &     *Q_CHECK)
          COV_G_ICAPG_I=B_I**2*C_CHECK*((1.D0-ALPHA**2)*T_I/2.D0
     &     +(1.D0-ALPHA*BETA)*Q_CHECK)
!
          L21=COV_G_ICAPG_I/L11
! TO STOP ROUND OFF ERRORS CREATING NaN
          IF(COV_CAPG_I2.GE.L21**2)THEN
            L22=SQRT(COV_CAPG_I2-L21**2)
          ELSE
            L22=SQRT(L21**2-COV_CAPG_I2)
          END IF
          IF(L22.EQ.0.D0)THEN
            L22=1.D-12
          END IF
!
          CAPGAMMA_I=L21*XI_G_I+L22*XI_CAPG_I
! BODY VELOCITY
          V_I_ALGAE(I_DIM)=BETA*V_I_0_ALGAE(I_DIM)
     &               +(1.D0-BETA)*(C_I_ALGAE(I_DIM)*T_I+FI_C)
     &               +(ALPHA-BETA)*C_CHECK*(U_I_0_ALGAE(I_DIM)
     &                                      -C_I_ALGAE(I_DIM)*T_I)
     &               +CAPGAMMA_I
!
!=======================================================================
! ALGAE POSITIONS
!=======================================================================
! VARIABLES OF THE EXACT INTEGRATOR
          G_CHECK=T_I+K_CHECK*TAU_PART
!
          COV_CAPP_I2=(B_I*C_CHECK)**2*(G_CHECK**2*DT+(1.D0-ALPHA**2)
     &     *T_I**3/2.D0+(1.D0-BETA**2)*K_CHECK**2*TAU_PART**3/2.D0
     &     -2.D0*G_CHECK*((1.D0-ALPHA)*T_I**2+(1.D0-BETA)*K_CHECK
     &     *TAU_PART**2)+2.D0*(1.D0-ALPHA*BETA)*Q_CHECK*T_I*TAU_PART)
!
          COV_G_ICAPP_I=B_I**2*C_CHECK*((1.D0-ALPHA)*G_CHECK*T_I-(1.D0
     &     -ALPHA**2)*T_I**2/2.D0-(1.D0-ALPHA*BETA)*Q_CHECK*TAU_PART)
!
          COV_CAPG_ICAPP_I=(B_I*C_CHECK)**2*(((1.D0-ALPHA)*T_I+(1.D0
     &     -BETA)*K_CHECK*TAU_PART)*G_CHECK-(1.D0-ALPHA**2)*T_I**2
     &     /2.D0-(1.D0-BETA**2)*K_CHECK**2*TAU_PART**2/2.D0-(1.D0
     &     -ALPHA*BETA)*K_CHECK*T_I*TAU_PART)
!
          L31=COV_G_ICAPP_I/L11
          L32=(COV_CAPG_ICAPP_I-L21*L31)/L22
! TO STOP ROUND OFF ERRORS CREATING NaN
          IF(COV_CAPP_I2.GE.L31**2+L32**2)THEN
            L33=SQRT(COV_CAPP_I2-L31**2-L32**2)
          ELSE
            L33=SQRT(ABS(COV_CAPP_I2-L31**2-L32**2))
          END IF
!
          CAPPHI_I=L31*XI_G_I+L32*XI_CAPG_I+L33*XI_CAPP_I
!ALGAE POSITION
          X_I_ALGAE(I_DIM)=X_I_0_ALGAE(I_DIM)
     &     +(1.D0-BETA)*TAU_PART*V_I_0_ALGAE(I_DIM)+(DT
     &     -(1.D0-BETA)*TAU_PART)*(C_I_ALGAE(I_DIM)*T_I+FI_C*TAU_PART)
     &     +C_CHECK*(U_I_0_ALGAE(I_DIM)-C_I_ALGAE(I_DIM)*T_I)
     &             *((1.D0-ALPHA)*T_I-(1.D0-BETA)*TAU_PART)
     &     +CAPPHI_I
!
!=======================================================================
! CHECK FOR POSSIBLE ERRORS
!=======================================================================
          IF(ABS(X_I_ALGAE(I_DIM)).GT.10.D0**10)THEN
            WRITE(LU,*)''
            WRITE(LU,*)'                           **************'
            WRITE(LU,*)'                           *POSITION INF*'
            WRITE(LU,*)'                           **************'
            WRITE(LU,*)''
            CALL PLANTE(1)
            STOP
          END IF
!
!=======================================================================
! REDEFINE PSI
!=======================================================================
          DO IWIN=1,NWIN
            PSI(I_A,I_DIM,NWIN+1-IWIN+1)=PSI(I_A,I_DIM,NWIN+1-IWIN)
          END DO
          PSI(I_A,I_DIM,1)=((U_I_ALGAE(I_DIM)-V_I_ALGAE(I_DIM))
     &                    -(U_I_0_ALGAE(I_DIM)-V_I_0_ALGAE(I_DIM)))/DT
!=======================================================================
        END DO
!=======================================================================
!=======================================================================
! REDEFINIR THE CURRENT VARIABLES VARIABLES ACTUELLES
!=======================================================================
        U_X(I_A)=U_I_ALGAE(1)
        U_Y(I_A)=U_I_ALGAE(2)
        V_X(I_A)=V_I_ALGAE(1)
        V_Y(I_A)=V_I_ALGAE(2)
        X_A(I_A)=X_I_ALGAE(1)
        Y_A(I_A)=X_I_ALGAE(2)
!
        DX_A(I_A)=X_I_ALGAE(1)-X_I_0_ALGAE(1)
        DY_A(I_A)=X_I_ALGAE(2)-X_I_0_ALGAE(2)
        IF(NDIM.EQ.3)THEN
          Z_A(I_A)=X_I_ALGAE(3)
          DZ_A(I_A)=X_I_ALGAE(3)-X_I_0_ALGAE(3)
        END IF

!
        GOTO 13
12      CONTINUE
! IF THE PARTICLES ARE NOT TRANSPORTED
        DX_A(I_A)=0.D0
        DY_A(I_A)=0.D0
        DZ_A(I_A)=0.D0
        V_X(I_A)=0.D0
        V_Y(I_A)=0.D0
        V_Z(I_A)=0.D0
        U_X(I_A)=0.D0
        U_Y(I_A)=0.D0
        U_Z(I_A)=0.D0
!
        DO I_DIM=1,NDIM
          DO IWIN=1,NWIN+1
            PSI(I_A,I_DIM,IWIN)=0.D0
          END DO
        END DO
!
13      CONTINUE
      END DO
!
      RETURN
      END SUBROUTINE DISP_ALGAE
!
!                   **********************
                    SUBROUTINE DEALLOC_ALGAE
!                   **********************
!
     &()
!
!***********************************************************************
! TELEMAC 2D VERSION 6.3    MAI 2013                       ANTOINE JOLY
! EDF R&D                                           antoine.joly@edf.fr
!***********************************************************************
!
!brief    DEALLOCATES THE VARIABLES ASSOCIATED TO THE ALGAE PARTICLES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |  NAME          |MODE|                  ROLE                        |
! |________________|____|______________________________________________|
! |________________|____|______________________________________________|
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!
!-----------------------------------------------------------------------
!
!***********************************************************************
!
      IF(ALLOCATED(T_TIL_P)) DEALLOCATE(T_TIL_P)
      IF(ALLOCATED(A_P)) DEALLOCATE(A_P)
      IF(ALLOCATED(PSI)) DEALLOCATE(PSI)
      IF(ALLOCATED(FI_P)) DEALLOCATE(FI_P)
      IF(ALLOCATED(U_I_0_ALGAE)) DEALLOCATE(U_I_0_ALGAE)
      IF(ALLOCATED(U_I_ALGAE)) DEALLOCATE(U_I_ALGAE)
      IF(ALLOCATED(V_I_0_ALGAE)) DEALLOCATE(V_I_0_ALGAE)
      IF(ALLOCATED(V_I_ALGAE)) DEALLOCATE(V_I_ALGAE)
      IF(ALLOCATED(X_I_0_ALGAE)) DEALLOCATE(X_I_0_ALGAE)
      IF(ALLOCATED(X_I_ALGAE)) DEALLOCATE(X_I_ALGAE)
      IF(ALLOCATED(C_I_ALGAE)) DEALLOCATE(C_I_ALGAE)
!
      IF (NDRG_CLSS.GT.0) THEN
        CALL BIEF_DEALLOBJ(NODCLSS)
        CALL BIEF_DEALLOBJ(PARCLSS)
        IF (NALG_CLSS.GT.0) THEN
          CALL BIEF_DEALLOBJ(U_X_AV_0)
          CALL BIEF_DEALLOBJ(U_Y_AV_0)
          CALL BIEF_DEALLOBJ(U_Z_AV_0)
          CALL BIEF_DEALLOBJ(U_X_AV)
          CALL BIEF_DEALLOBJ(U_Y_AV)
          CALL BIEF_DEALLOBJ(U_Z_AV)
          CALL BIEF_DEALLOBJ(K_AV_0)
          CALL BIEF_DEALLOBJ(EPS_AV_0)
          CALL BIEF_DEALLOBJ(K_AV)
          CALL BIEF_DEALLOBJ(EPS_AV)
          CALL BIEF_DEALLOBJ(H_FLU)
          CALL BIEF_DEALLOBJ(U_X_0)
          CALL BIEF_DEALLOBJ(U_Y_0)
          CALL BIEF_DEALLOBJ(U_Z_0)
          CALL BIEF_DEALLOBJ(U_X)
          CALL BIEF_DEALLOBJ(U_Y)
          CALL BIEF_DEALLOBJ(U_Z)
          CALL BIEF_DEALLOBJ(V_X_0)
          CALL BIEF_DEALLOBJ(V_Y_0)
          CALL BIEF_DEALLOBJ(V_Z_0)
          CALL BIEF_DEALLOBJ(V_X)
          CALL BIEF_DEALLOBJ(V_Y)
          CALL BIEF_DEALLOBJ(V_Z)
          CALL BIEF_DEALLOBJ(DX_A)
          CALL BIEF_DEALLOBJ(DY_A)
          CALL BIEF_DEALLOBJ(DZ_A)
          CALL BIEF_DEALLOBJ(TEFF)
          CALL BIEF_DEALLOBJ(I_A_GL)
          CALL BIEF_DEALLOBJ(DISLODGE)
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DEALLOC_ALGAE
!
      END MODULE ALGAE_TRANSP
