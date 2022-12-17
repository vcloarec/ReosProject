!                   *****************
                    SUBROUTINE CVTRVF
!                   *****************
!
     &(F,FN,FSCEXP,H,HN,HPROP,UCONV,VCONV,DM1,ZCONV,SOLSYS,
     & SM,SMH,YASMH,SMI,YASMI,FBOR,MASKTR,MESH,
     & AGGLOH,DT,ENTET,MSK,MASKEL,S,MASSOU,OPTSOU,
     & LIMTRA,KDIR,KDDL,NPTFR,FLBOR,YAFLBOR,VOLU2D,V2DPAR,UNSV2D,IOPT,
     & FLBORTRA,MASKPT,RAIN,PLUIE,TRAIN,OPTADV,TB,FREE,AM2,TB2,
     & NCO_DIST,NSP_DIST,YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE,
     & SLVTRA)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    DISTRIBUTIVE EXPLICIT OR IMPLICIT ADVECTOR.
!
!warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!+            WITH NO FRICTION
!
!history  CHI-TUAN PHAM  (LNHE)
!+        09/10/09
!+        V6P0
!+   First version.
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
!+        24/02/2012
!+        V6P2
!+   Rain and evaporation added (after initiative by O. Boutron, from
!+   Tour du Valat, and O. Bertrand, Artelia group)
!
!history  SARA PAVAN & J-M HERVOUET (LNHE)
!+        18/06/2013
!+        V6P3
!+   New call to CFLVF, for new monotonicity criterion.
!
!history  SARA PAVAN & J-M HERVOUET (EDF LAB, LNHE)
!+        05/05/2014
!+        V7P0
!+   New predictor-corrector PSI scheme (OPTADV=2). Security coefficient
!+   on maximum time step to avoid truncation errors that would give
!+   negative values.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/05/2014
!+        V7P0
!+   Boundary conditions LIMTRA redone (they may have been done with
!+   U.N in diffin.f, which is different from flbor here. Argument
!+   VOLU2D added.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        01/09/2015
!+        V7P1
!+   Now with the locally implicit predictor-corrector.
!+   Call of CFLVF changed, with option OPTCFL added in the arguments.
!+   For locally implicit schemes, fluxes may be reduced with array
!+   FLULIM. The solver configuration of tracers is copied but the
!+   choice of the solver, if not GMRES or direct, is set to Jacobi
!+   in TVF_IMP.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/06/2016
!+        V7P2
!+   Stability criterion for the LIPS scheme was too restrictive
!+   compared to theory. The real theoretical value is now taken.
!
!history  J-M HERVOUET (jubilado)
!+        27/09/2016
!+        V7P2
!+   Predictor and corrector of LIPS grouped in the same loop.
!+   Limitation of the predictor even for the first correction.
!+   Extrema FMIN and FMAX depend on the scheme, so their computation
!+   is slightly changed. Coefficients COEMIN and COESOU removed in the
!+   call to cflvf.
!
!history  J-M HERVOUET (jubilado)
!+        20/10/2016
!+        V7P2
!+   Reduction of time-step for predictor-corrector is no longer fixed
!+   to be 2, any value > 1 is possible, but here hardcoded at 2.D0.
!+   So no change except for further research (values less than 2 seem
!+   to give better results).
!
!history  J-M HERVOUET (jubilado)
!+        29/09/2017
!+        V7P3
!+   Theory of stability condition slightly changed for all predictor-
!+   corrector and LIPS schemes. Steady state flows unchanged.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AGGLOH         |-->| MASS-LUMPING IN CONTINUITY EQUATION
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS
!|                |   | CONSERVATION.
!| F              |<--| F AT TIME T(N+1)
!| FBOR           |-->| DIRICHLET CONDITIONS ON F.
!| FLBOR          |-->| FLUXES AT BOUNDARIES
!| FLBORTRA       |<->| TRACER FLUXES AT BOUNDARIES
!| FLULIM         |-->| A LIMITATION OF FLUXES IF YAFLULIM=.TRUE.
!| FN             |-->| F AT TIME T(N)
!| FSCEXP         |-->| EXPLICIT PART OF THE SOURCE TERM
!|                |   | EQUAL TO ZERO EVERYWHERE BUT ON SOURCES
!|                |   | WHERE THERE IS FSCE - (1-TETAT) FN
!|                |   | SEE DIFSOU
!| HPROP          |-->| PROPAGATION DEPTH (DONE IN CVDFTR).
!| IOPT           |-->| OPTIONS FOR COMPUTATION (NUMBER BETWEEN 0 AND 13)
!|                |   | THE TENS (IOPT2, I.E. 0 OR 1):
!|                |   | 0: UCONV OBEYS THE CONTINUITY EQUATION
!|                |   | 1: UCONV DOES NOT OBEY THE CONTINUITY EQUATION
!|                |   | THE UNITS (IOPT1, I.E. 0 TO 3): VARIANT FOR FLUXES
!|                |   | 0: CONSTANT PER ELEMENT = 0
!|                |   | 1: CHI-TUAN PHAM'S CONSTANT
!|                |   | 2: N SCHEME
!|                |   | 3: PSI SCHEME
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMTRA         |-->| BOUNDARY CONDITIONS ON BOOUNDARY POINTS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!| MASSOU         |-->| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NCO_DIST       |-->| NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES
!| NSP_DIST       |-->| NUMBER OF SUB-STEPS OF DISTRIBUTIVE SCHEMES
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPDADV         |-->| SCHEME OPTION FOR THE ADVECTION OF TRACERS
!|                |   | WITH N OR PSI SCHEME:
!|                |   |  1: EXPLICIT
!|                |   |  2: PREDICTOR-CORRECTOR 1ST ORDER IN TIME
!|                |   |  3: PREDICTOR-CORRECTOR 2ND ORDER IN TIME
!|                |   |  4: LOCALLY IMPLICIT PREDICTOR-CORRECTOR
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| S              |-->| VOID STRUCTURE
!| SLVTRA         |-->| SOLVER CONFIGURATION FOR TRACER DIFFUSION.
!| SM             |-->| SOURCE TERMS.
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SMI            |-->| IMPLICIT SOURCE TERM
!| SOLSYS         |-->| 1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
!| TB             |<->| BLOCK OF WORK STRUCTURES
!| TB2            |<->| SECOND BLOCK OF AT LEAST 7 WORK STRUCTURES
!| TRAIN          |-->| VALUE OF TRACER IN THE RAIN
!| UCONV,VCONV    |-->| ADVECTION VELOCITY FIELD
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS, NOT ASSEMBLED IN PARALLEL
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| YAFLBOR        |-->| IF YES FLBOR IS GIVEN
!| YAFLULIM       |-->| IF YES FLULIM IS GIVEN
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!| YASMI          |-->| IF YES, SMI MUST BE TAKEN INTO ACCOUNT
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CVTRVF => CVTRVF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: KDDL,IOPT,OPTADV,FREE
      INTEGER, INTENT(IN)             :: NCO_DIST,NSP_DIST
      INTEGER, INTENT(INOUT)          :: LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,AGGLOH,TRAIN
      DOUBLE PRECISION, INTENT(IN)    :: FLULIM(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      LOGICAL, INTENT(IN)             :: YASMH,YAFLBOR
      LOGICAL, INTENT(IN)             :: MSK,ENTET,YASMI,RAIN
      LOGICAL, INTENT(IN)             :: YAFLULIM,YAFLULIMEBE
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,DM1,ZCONV,MASKPT
      TYPE(BIEF_OBJ), INTENT(IN), TARGET :: H,HN
      TYPE(BIEF_OBJ), INTENT(IN)      :: VOLU2D,V2DPAR,UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F,SM,AM2
      TYPE(BIEF_OBJ), INTENT(IN)      :: UCONV,VCONV,FN,SMI,SMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FBOR,TB,TB2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLBORTRA
      TYPE(BIEF_OBJ), INTENT(IN)      :: FSCEXP,S,MASKTR
      TYPE(BIEF_OBJ), INTENT(IN)      :: PLUIE
      TYPE(BIEF_MESH)                 :: MESH
      DOUBLE PRECISION, INTENT(IN)    :: FLULIMEBE(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLBOR
      TYPE(SLVCFG), INTENT(IN)        :: SLVTRA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMF,I,IOPT1,IOPT2,N,IELEM,I1,I2,I3,ICOR,NIT,OPTCFL
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION DT_REMAIN,DDT,TDT,SECU,TETAFCOR
      DOUBLE PRECISION ADMASS,LOCALMIN,LOCALMAX,TWOTHIRDS
!
      CHARACTER(LEN=16) FORMUL
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT,FXMATPAR
!
      DOUBLE PRECISION K,KM1,KM1SURK
      LOGICAL MASS_BAL,PREDICOR,LIPS,WITHABS
!
      TYPE(SLVCFG)::SLVPSI
!
!     HARDCODED PARAMETER !!!!!!!!!!
!
!     MAXIMUM NUMBER OF ITERATIONS
      INTEGER, PARAMETER :: NITMAX = 200
!
!-----------------------------------------------------------------------
!
      TYPE(BIEF_OBJ), POINTER :: T1,T2,FLBOUND,T4,T6,FXBORPAR,T8,HNT,HT
      TYPE(BIEF_OBJ), POINTER :: HNP1MT,TETAF_VAR,FMIN,FMAX
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: DFDT
!
!-----------------------------------------------------------------------
!
!     HARDCODED PARAMETER FOR STABILITY OF PREDICTOR CORRECTOR
!     AND NUMBERS DEPENDING OF K. K MUST BE > 1.D0
!
      K=2.D0
!
      KM1=K-1.D0
      KM1SURK=KM1/K
      TWOTHIRDS=2.D0*KM1/(2.D0*K-1.D0)
!
!-----------------------------------------------------------------------
!
!     SOLVER OPTIONS FOR IMPLICIT SCHEMES : SLVTRA TAKEN BUT EITHER
!                                           GMRES OR DIRECT ASKED
!                                           AND HARDCODED HERE
!                                           KRYLOV DIMENSION SET TO 3
!
!     SEE ALSO TVF_IMP WITH A JACOBI METHOD
!
      SLVPSI%SLV   =SLVTRA%SLV
      SLVPSI%NITMAX=SLVTRA%NITMAX
      SLVPSI%PRECON=SLVTRA%PRECON
      SLVPSI%KRYLOV=SLVTRA%KRYLOV
      SLVPSI%EPS   =SLVTRA%EPS
      SLVPSI%ZERO  =SLVTRA%ZERO
!
!     CHANGING THIS WILL TRIGGER CHANGING THE SIZE OF TB2 IN CALLING
!     PROGRAMMES
      SLVPSI%KRYLOV=3
!
!-----------------------------------------------------------------------
!
!     BEWARE : OPTIMISATION OF MEMORY USE...
!
      HNP1MT    =>TB%ADR(FREE)%P
      T1        =>TB%ADR(FREE+1)%P
      T2        =>TB%ADR(FREE+2)%P
      FLBOUND   =>TB%ADR(FREE+3)%P
      T4        =>TB%ADR(FREE+4)%P
      T6        =>TB%ADR(FREE+5)%P
      FXBORPAR  =>TB%ADR(FREE+6)%P
      T8        =>TB%ADR(FREE+7)%P
      HNT       =>TB%ADR(FREE+8)%P
      HT        =>TB%ADR(FREE+9)%P
      TETAF_VAR =>TB%ADR(FREE+10)%P
!
!     BEWARE : FMIN AND FMAX MIXED IN MEMORY WITH T6%R AND T8%R
!              LOOK HOW DFDT AND T8 ARE ALWAYS RECOMPUTED AFTER FMIN AND FMAX
!              ARE USED  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     BEWARE : DFDT=T6%R !!!!!!!!!!!
      DFDT      => TB%ADR(FREE+5)%P%R
      FMIN      => T6
      FMAX      => T8
!
!-----------------------------------------------------------------------
!
      FXMAT=>MESH%MSEG%X%R(1:MESH%NSEG)
!     IN PARALLEL MODE, ASSEMBLED AND NON ASSEMBLED VERSIONS ARE DIFFERENT
      IF(NCSIZE.GT.1) THEN
        FXMATPAR=>MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG)
      ELSE
        FXMATPAR=>MESH%MSEG%X%R(1:MESH%NSEG)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CHECKING OPTIONS
!
      IF(OPTADV.LT.1.OR.OPTADV.GT.4) THEN
        WRITE(LU,*) 'CVTRVF: OPTION OF ADVECTION SCHEME'
        WRITE(LU,*) '        UNKNOWN: ',OPTADV
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(AM2%STO.NE.3.AND.OPTADV.EQ.4) THEN
        WRITE(LU,*) 'CVTRVF: EDGE-BASED STORAGE REQUESTED WITH'
        WRITE(LU,*) '        IMPLICIT N OR PSI SCHEME'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     EXTRACTS OTHER OPTIONS
!
      IOPT2=IOPT/10
      IOPT1=IOPT-10*IOPT2
!
      IF(IOPT1.LT.1.OR.IOPT1.GT.3) THEN
        WRITE(LU,*) 'CVTRVF : OPTION IOPT1=',IOPT1,' UNKNOWN'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(IOPT2.LT.0.OR.IOPT2.GT.1) THEN
        WRITE(LU,*) 'CVTRVF : OPTION IOPT2=',IOPT2,' UNKNOWN'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     SIMPLIFYING TESTS: OPTIONS WITH AN EXPLICIT PREDICTOR-CORRECTOR
!
      IF((IOPT1.EQ.2.OR.IOPT1.EQ.3).AND.
     &  (OPTADV.EQ.2.OR.OPTADV.EQ.3)) THEN
        PREDICOR=.TRUE.
      ELSE
        PREDICOR=.FALSE.
      ENDIF
!
!     SIMPLIFYING TESTS: OPTION WITH LOCALLY IMPLICIT PSI (OR N) SCHEME
!
      IF((IOPT1.EQ.2.OR.IOPT1.EQ.3).AND.OPTADV.EQ.4) THEN
        LIPS=.TRUE.
      ELSE
        LIPS=.FALSE.
      ENDIF
!
!     SELECTING THE IMPLICITATION COEFFICIENT
!
      IF(LIPS) THEN
!       ADAPTIVE IMPLICIT, N OR PSI
      ELSEIF((IOPT1.EQ.2.OR.IOPT1.EQ.3).AND.OPTADV.EQ.3) THEN
!       EXPLICIT PREDICTOR, PSEUDO-IMPLICIT CORRECTOR
!       FROM 0 TO 1
        TETAFCOR=0.5D0
      ELSE
!       EXPLICIT N OR PSI, EXPLICIT PREDICTOR-CORRECTOR PSI
        TETAFCOR=0.D0
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IELMF = F%ELM
!     FORCED TO LINEAR
      IELMF=11
!
!     TAKES MASS-LUMPING INTO ACCOUNT IN THE CONTINUITY EQUATION
!
      IF(ABS(1.D0-AGGLOH).GT.1.D-8) THEN
        CALL VECTOR(HT ,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,H ,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(HNT,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,HN,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(HT ,2,MESH)
          CALL PARCOM(HNT,2,MESH)
        ENDIF
        CALL OS('X=YZ    ',X=HT ,Y=HT ,Z=UNSV2D)
        CALL OS('X=YZ    ',X=HNT,Y=HNT,Z=UNSV2D)
        CALL OS('X=X+CY  ',X=HT ,Y=H  ,C=AGGLOH)
        CALL OS('X=X+CY  ',X=HNT,Y=HN ,C=AGGLOH)
      ELSE
!       CALL OS('X=Y     ',X=HT ,Y=H )
!       CALL OS('X=Y     ',X=HNT,Y=HN)
        HT=>H
        HNT=>HN
      ENDIF
!
!     IF NO FLBOR IS GIVEN, IT IS COMPUTED
!
      IF(YAFLBOR) THEN
        CALL OS('X=Y     ',X=FLBOUND,Y=FLBOR)
      ELSE
!       MASK=5 FOR NON NEUMANN BOUNDARIES IN DIFFIN
        CALL VECTOR(FLBOUND,'=','FLUBDF          ',1,1.D0,
     &              HPROP,HPROP,HPROP,
     &              UCONV,VCONV,VCONV,MESH,.TRUE.,MASKTR%ADR(5)%P)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CORRECTION OF THE BOUNDARY CONDITIONS
!
!-----------------------------------------------------------------------
!
!     A SIMILAR CORRECTION IS DONE IN DIFFIN, BUT IT MAY BE INCOMPATIBLE
!     AS U*N MAY NOT BE OF THE SAME SIGN AS FLBOR HERE (RARE BUT ALREADY
!     SEEN).
!
      CALL CPSTVC(HN,FXBORPAR)
!     JUST IN CASE INTERNAL POINTS HAVE NON INITIALISED VALUES
      CALL OS('X=0     ',X=FXBORPAR)
      DO I=1,MESH%NPTFR
        N=MESH%NBOR%I(I)
        FXBORPAR%R(N)=FLBOUND%R(I)
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(FXBORPAR,2,MESH)
      DO I=1,MESH%NPTFR
        N=MESH%NBOR%I(I)
        IF(LIMTRA(I).EQ.KDIR.AND.FXBORPAR%R(N).GT.0.D0) THEN
          LIMTRA(I)=KDDL
        ELSEIF(LIMTRA(I).EQ.KDDL.AND.FXBORPAR%R(N).LT.0.D0) THEN
          LIMTRA(I)=KDIR
!         WHEN VELOCITIES RE-ENTER THROUGH AN EXIT, WE ARBITRARILY CHOOSE
!         THAT THE PRESCRIBED VALUE IS THE LAST KNOWN
          FBOR%R(I)=FN%R(N)
        ENDIF
      ENDDO
!
!     MASKS FLBOR IF(MSK)
!
      IF(MSK) CALL OS('X=XY    ',X=FXBORPAR,Y=MASKPT)
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE ADDED MASS AND THE TRACER FLUX AT THE BOUNDARY
!
      ADMASS=0.D0
      DO I=1,MESH%NPTFR
        FLBORTRA%R(I)=0.D0
      ENDDO
!
!     COMPUTES THE FLUXES PHIIJ = FXMAT
!
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
      CALL VECTOR(T2,'=',FORMUL,IELMF,-1.D0,
     &            HPROP,DM1,ZCONV,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
!                 T2 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
      NIT=0
      DT_REMAIN=DT
      TDT=0.D0
      CALL CPSTVC(H,HNP1MT)
      CALL CPSTVC(H,T4)
      CALL CPSTVC(F,T8)
!
!     T4 WILL BE F PROGRESSIVELY UPDATED
!     HNP1MT WILL BE THE DEPTH AT THE END OF THE PREVIOUS SUB-TIMESTEP
!     HENCE THE DEPTH At THE BEGINNING OF THE NEXT SUB TIME-STEP
!     (INITIALISED HERE TO CALL CFLVF)
!
      DO I=1,HN%DIM1
        T4%R(I)=FN%R(I)
        HNP1MT%R(I)=HNT%R(I)
      ENDDO
!
!     T1 WILL BE THE DEPTH ACCORDING TO THE CONTINUITY EQUATION
!
      IF(IOPT2.EQ.1) THEN
        DO I=1,HN%DIM1
          T1%R(I)=HNT%R(I)
        ENDDO
      ENDIF
!
100   CONTINUE
      NIT=NIT+1
!
!---------------------------------------
! VARIOUS OPTIONS TO COMPUTE THE FLUXES
!---------------------------------------
!
      IF(NIT.EQ.1.OR.(IOPT1.EQ.3.AND..NOT.LIPS)) THEN
        CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%NELMAX,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
!                                          N SCHEME FORCED (HERE FOR CFL ONLY)
     &                  MESH%IKLE%I,.TRUE.,2,
!                       OPTIONAL LIMITATION OF FLUXES FOR LIPS
!                       (OTHER SCHEMES DO NOT WORK WITH TIDAL FLATS)
     &                  YAFLULIM=YAFLULIM.AND.LIPS,FLULIM=FLULIM,
     &                  YAFLULIMEBE=YAFLULIMEBE.AND.LIPS,
     &                  FLULIMEBE=FLULIMEBE)
!    &                  MESH%IKLE%I,.TRUE.,2    ,FN=T4)
!                                          IOPT1 HERE FORCED TO N SCHEME
!                                          NO NEED OF FN
!
!       CANCELS FLUXES TO AND FROM MASKED POINTS
!
        IF(MSK) THEN
          CALL FLUX_MASK(FXMAT,MESH%NSEG,
     &                   MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MASKPT%R)
        ENDIF
!       ASSEMBLES THE FLUXES AT INTERFACES IN PARALLEL MODE, THIS
!       IS FOR UPWINDING (STORED IN SECOND DIMENSION OF MESH%MSEG)
        IF(NCSIZE.GT.1) THEN
          CALL OV('X=Y     ', X=FXMATPAR, Y=FXMAT, DIM1=MESH%NSEG)
          CALL PARCOM2_SEG(FXMATPAR,FXMATPAR,FXMATPAR,
     &                     MESH%NSEG,1,2,1,MESH,1,11)
        ENDIF
      ENDIF
!
!--------------------------------------------
! DETERMINES THE LARGEST ADMISSIBLE TIMESTEP
!--------------------------------------------
!
!     COMPUTES THE MAXIMUM TIMESTEP ENSURING MONOTONICITY
!     ACCORDING TO THEORY OF N SCHEME, BUT POSSIBLY
!     DIVIDED BY 2 FOR PREDICTOR-CORRECTORS
!
      IF(PREDICOR.AND.NCO_DIST.GT.0) THEN
        SECU=1.D0
        WITHABS=.TRUE.
      ELSEIF(LIPS) THEN
        SECU=1.D0
        WITHABS=.TRUE.
      ELSE
        SECU=0.99D0
        WITHABS=.FALSE.
      ENDIF
!
!     HARDCODED OPTION FOR THE CFL (AND OPTION 2 ONLY FOR THE N SCHEME)
!
      OPTCFL=1
!     OPTCFL=2
!     IF(OPTADV.NE.1.OR.IOPT1.NE.2) OPTCFL=1
!
      CALL CFLVF(DDT,HNP1MT%R,FXMAT,FXMATPAR,
!                                   FLBOR%R(NPOIN)
     &           V2DPAR%R,DT_REMAIN,FXBORPAR%R   ,SMH%R,
     &           YASMH,T8,MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &           MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH,MSK,MASKPT,
     &           RAIN,PLUIE%R,T4%R,MESH%NELEM,MESH%IKLE%I,
     &           LIMTRA,KDIR,KDDL,FBOR%R,FSCEXP%R,TRAIN,MESH%NBOR%I,
     &           T2,T6,SECU,OPTCFL,WITHABS)
!
!     NOW RECOMPUTING THE PSI FLUXES (THE N FLUXES HAVE BEEN
!                                     USED FOR THE STABILITY CRITERION).
!
!     WITH THE SEMI-IMPLICIT SCHEME, THE PSI FLUXES CANNOT BE USED
!     AT THIS LEVEL AND WILL NOT BE COMPUTED.
!
      IF(IOPT1.EQ.3.AND..NOT.LIPS) THEN
        CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%NELMAX,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,IOPT1,T4)
!       CANCELS FLUXES TO AND FROM MASKED POINTS
        IF(MSK) THEN
          CALL FLUX_MASK(FXMAT,MESH%NSEG,
     &                   MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MASKPT%R)
        ENDIF
!       ASSEMBLES THE FLUXES AT INTERFACES IN PARALLEL MODE, THIS
!       IS FOR UPWINDING (STORED IN SECOND DIMENSION OF MESH%MSEG)
        IF(NCSIZE.GT.1) THEN
          CALL OV('X=Y     ', X=FXMATPAR, Y=FXMAT, DIM1=MESH%NSEG)
          CALL PARCOM2_SEG(FXMATPAR,FXMATPAR,FXMATPAR,
     &                     MESH%NSEG,1,2,1,MESH,1,11)
        ENDIF
      ENDIF
!
!
      IF(NCSIZE.GT.1) DDT=P_MIN(DDT)
      DDT=MIN(DDT,DT_REMAIN)
!
!     CASE OF ADAPTIVE IMPLICIT N OR PSI SCHEME
!
      IF(LIPS) THEN
        DO I=1,HN%DIM1
          TETAF_VAR%R(I)=MAX(0.D0,1.D0-NSP_DIST*T2%R(I)/DT)
        ENDDO
        DDT=DT/NSP_DIST/0.999999D0
      ENDIF
!
      DDT=MIN(DDT,DT_REMAIN)
!
!     T2 WILL TAKE THE SUCCESSIVE VALUES OF H
!     AT THE BEGINNING OF THE SUB-TIMESTEP
!     WARNING: T2 ALSO USED WITH IOPT2=1, BUT SO FAR PREDICTOR-CORRECTOR
!              NOT USED WITH THIS OPTION
!
      IF(PREDICOR) THEN
        DO I=1,HN%DIM1
          T2%R(I)=HNT%R(I)+TDT*(HT%R(I)-HNT%R(I))/DT
        ENDDO
      ENDIF
!
      TDT=TDT+DDT
!
!     HNP1MT WILL TAKE THE SUCCESSIVE VALUES OF H
!     AT THE END OF THE SUB-TIMESTEP (EXPLICIT) OR IN BETWEEN (IMPLICIT)
!
      IF(LIPS) THEN
        DO I=1,HN%DIM1
          HNP1MT%R(I)=HNT%R(I)+
     &               (TDT-TETAF_VAR%R(I)*DDT)*(HT%R(I)-HNT%R(I))/DT
        ENDDO
      ELSE
        DO I=1,HN%DIM1
          HNP1MT%R(I)=HNT%R(I)+TDT*(HT%R(I)-HNT%R(I))/DT
        ENDDO
      ENDIF
!
!     IN TVF FACTOR HT/HLIN MAY TRIGGER DIVERGENCE FOR DRY POINTS
!
      IF(MSK) THEN
        DO I=1,HN%DIM1
          IF(MASKPT%R(I).LT.0.5D0) HNP1MT%R(I)=HT%R(I)
        ENDDO
      ENDIF
!
!------------------------------------
!  FINAL RESOLUTION OR PREDICTOR STEP
!------------------------------------
!
      IF(.NOT.LIPS) THEN
!
        CALL TRACVF(F,FSCEXP,FXMAT,FXMATPAR,
     &              VOLU2D,UNSV2D,
     &              DDT,FLBOUND,FBOR,SMH,YASMH,T1,T2,T4,HNP1MT,
     &              FXBORPAR,T8,
     &              MESH,LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,
     &              DT,RAIN,PLUIE,TRAIN,ADMASS,
!                   CASES WHERE THE MASS BALANCE MUST BE PREPARED
     &              OPTADV.EQ.1.OR.NCO_DIST.EQ.0)
!
      ELSE
!
!       LOOP WITH PREDICTOR AND ALL CORRECTIONS
!
        DO ICOR=0,NCO_DIST
!
!         UNLIKE WITH EXPLICIT SCHEMES, THE PREDICTOR IS NOT
!         GUARANTEED WITHOUT STABILITY PROBLEM IF NOT LIMITED
          IF(ICOR.GT.0) THEN
!           LIMITING THE PREDICTOR
            DO I=1,HN%DIM1
              FMIN%R(I)=T4%R(I)
              FMAX%R(I)=T4%R(I)
            ENDDO
            DO I=1,NPTFR
              IF(LIMTRA(I).EQ.KDIR) THEN
                N=MESH%NBOR%I(I)
                FMIN%R(N)=MIN(FMIN%R(N),FBOR%R(I))
                FMAX%R(N)=MAX(FMAX%R(N),FBOR%R(I))
              ENDIF
            ENDDO
            DO IELEM=1,MESH%NELEM
              I1=MESH%IKLE%I(IELEM)
              I2=MESH%IKLE%I(IELEM+  MESH%NELMAX)
              I3=MESH%IKLE%I(IELEM+2*MESH%NELMAX)
              LOCALMIN=MIN( F%R(I1), F%R(I2), F%R(I3),
     &                     T4%R(I1),T4%R(I2),T4%R(I3))
              LOCALMAX=MAX( F%R(I1), F%R(I2), F%R(I3),
     &                     T4%R(I1),T4%R(I2),T4%R(I3))
              FMIN%R(I1)=MIN(FMIN%R(I1),LOCALMIN)
              FMAX%R(I1)=MAX(FMAX%R(I1),LOCALMAX)
              FMIN%R(I2)=MIN(FMIN%R(I2),LOCALMIN)
              FMAX%R(I2)=MAX(FMAX%R(I2),LOCALMAX)
              FMIN%R(I3)=MIN(FMIN%R(I3),LOCALMIN)
              FMAX%R(I3)=MAX(FMAX%R(I3),LOCALMAX)
            ENDDO
            IF(NCSIZE.GT.1) THEN
              CALL PARCOM(FMIN,4,MESH)
              CALL PARCOM(FMAX,3,MESH)
            ENDIF
            DO I=1,HN%DIM1
              F%R(I)=MIN(F%R(I),T4%R(I)+KM1SURK*(FMAX%R(I)-T4%R(I)))
              F%R(I)=MAX(F%R(I),T4%R(I)+KM1SURK*(FMIN%R(I)-T4%R(I)))
            ENDDO
          ENDIF
!
!         DERIVATIVE IN TIME
!
          IF(ICOR.EQ.0) THEN
!           FOR GETTING THE EXPLICIT PSI CONTRIBUTION
!           WITHOUT THE DERIVATIVE IN TIME
            DO I=1,HN%DIM1
              DFDT(I)=0.D0
            ENDDO
          ELSE
!           FOR GETTING THE EXPLICIT PSI CONTRIBUTION
!           WITH THE DERIVATIVE IN TIME
            DO I=1,HN%DIM1
!             HERE DFDT=H*DFDT WITH SEMI IMPLICIT H
              DFDT(I)=HNP1MT%R(I)*(F%R(I)-T4%R(I))/DDT
            ENDDO
          ENDIF
!
!         EXPLICIT CONTRIBUTION
!
          IF(ICOR.EQ.0) THEN
!           FN GIVEN FOR FSTAR
            CALL FLUX_EF_VF_3(MESH%W%R,MESH%NELEM,MESH%NELMAX,
     &                        MESH%ELTSEG%I,MESH%ORISEG%I,
     &                        FXMATPAR,MESH%NSEG,
!                                                          FN
     &                        MESH%IKLE%I,MESH%NPOIN,T4,
!    &                        FI_I               HDFDT
     &                        T8%R,MESH%SURFAC%R,DFDT,TETAF_VAR%R,
!                                             FN FOR FSTAR
     &                        YAFLULIM,FLULIM,YAFLULIMEBE,
     &                        FLULIMEBE)
          ELSE
!           FSTAR REALLY GIVEN
            CALL FLUX_EF_VF_3(MESH%W%R,MESH%NELEM,MESH%NELMAX,
     &                        MESH%ELTSEG%I,MESH%ORISEG%I,
     &                        FXMATPAR,MESH%NSEG,
!                                                          FN
     &                        MESH%IKLE%I,MESH%NPOIN,T4,
!    &                        FI_I               HDFDT
     &                        T8%R,MESH%SURFAC%R,DFDT,TETAF_VAR%R,
!                                             FSTAR
     &                        YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE)
          ENDIF
!         NO, WILL GO INTO SM THAT IS NOT ASSEMBLED
!         IF(NCSIZE.GT.1) CALL PARCOM(T8,2,MESH)
!
!         BUILDING AND SOLVING THE LINEAR SYSTEM (NORMAL OR PREDICTOR)
!
          CALL TVF_IMP(F%R,T4%R,FXMAT,
     &                 FXMATPAR,DDT,
     &                 FLBOUND%R,HNP1MT%R,
     &                 FBOR%R,SMH%R,YASMH,FSCEXP%R,
     &                 MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &                 MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &                 MESH%NBOR%I,LIMTRA,KDIR,KDDL,
     &                 OPTSOU,IOPT2,FLBORTRA%R,DDT/DT,MESH,F,
     &                 RAIN,PLUIE%R,TRAIN,TETAF_VAR%R,
     &                 ENTET,VOLU2D%R,
     &                 T6,T8%R,AM2,TB2,SLVPSI,
!                      PREDICTOR CORRECTOR
     &                 ICOR.EQ.0,ICOR.NE.0,ICOR,NCO_DIST,ADMASS)
!
        ENDDO
!
      ENDIF
!
!-------------------------------------
!  CORRECTOR STEP FOR N AND PSI SCHEME
!-------------------------------------
!
      IF(PREDICOR.AND.NCO_DIST.GT.0) THEN
!
        DO ICOR=1,NCO_DIST
!
!       CASES WITH A LIMITATION OF THE FIRST CORRECTOR
!
        IF(OPTADV.EQ.3.OR.ICOR.GT.1.OR.K.LT.2.D0) THEN
!         COMPUTING THE MINIMUM AND MAXIMUM
          DO I=1,HN%DIM1
            FMIN%R(I)=T4%R(I)
            FMAX%R(I)=T4%R(I)
          ENDDO
          DO I=1,NPTFR
            IF(LIMTRA(I).EQ.KDIR) THEN
              N=MESH%NBOR%I(I)
              FMIN%R(N)=MIN(FMIN%R(N),FBOR%R(I))
              FMAX%R(N)=MAX(FMAX%R(N),FBOR%R(I))
            ENDIF
          ENDDO
          IF(OPTADV.EQ.2) THEN
!           ALL FSTAR(J) NOT INCLUDED
            DO IELEM=1,MESH%NELEM
              I1=MESH%IKLE%I(IELEM)
              I2=MESH%IKLE%I(IELEM+  MESH%NELMAX)
              I3=MESH%IKLE%I(IELEM+2*MESH%NELMAX)
              LOCALMIN=MIN(T4%R(I1),T4%R(I2),T4%R(I3))
              LOCALMAX=MAX(T4%R(I1),T4%R(I2),T4%R(I3))
              FMIN%R(I1)=MIN(F%R(I1),FMIN%R(I1),LOCALMIN)
              FMAX%R(I1)=MAX(F%R(I1),FMAX%R(I1),LOCALMAX)
              FMIN%R(I2)=MIN(F%R(I2),FMIN%R(I2),LOCALMIN)
              FMAX%R(I2)=MAX(F%R(I2),FMAX%R(I2),LOCALMAX)
              FMIN%R(I3)=MIN(F%R(I3),FMIN%R(I3),LOCALMIN)
              FMAX%R(I3)=MAX(F%R(I3),FMAX%R(I3),LOCALMAX)
            ENDDO
          ELSEIF(OPTADV.EQ.3) THEN
!           ALL FSTAR(J) INCLUDED
            DO IELEM=1,MESH%NELEM
              I1=MESH%IKLE%I(IELEM)
              I2=MESH%IKLE%I(IELEM+  MESH%NELMAX)
              I3=MESH%IKLE%I(IELEM+2*MESH%NELMAX)
              LOCALMIN=MIN( F%R(I1), F%R(I2), F%R(I3),
     &                     T4%R(I1),T4%R(I2),T4%R(I3))
              LOCALMAX=MAX( F%R(I1), F%R(I2), F%R(I3),
     &                     T4%R(I1),T4%R(I2),T4%R(I3))
              FMIN%R(I1)=MIN(FMIN%R(I1),LOCALMIN)
              FMAX%R(I1)=MAX(FMAX%R(I1),LOCALMAX)
              FMIN%R(I2)=MIN(FMIN%R(I2),LOCALMIN)
              FMAX%R(I2)=MAX(FMAX%R(I2),LOCALMAX)
              FMIN%R(I3)=MIN(FMIN%R(I3),LOCALMIN)
              FMAX%R(I3)=MAX(FMAX%R(I3),LOCALMAX)
            ENDDO
          ENDIF
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(FMIN,4,MESH)
            CALL PARCOM(FMAX,3,MESH)
          ENDIF
        ENDIF
!
!       LIMITATIONS OF THE PREDICTOR
!
        IF(OPTADV.EQ.3) THEN
          DO I=1,HN%DIM1
            F%R(I)=MIN(F%R(I),T4%R(I)+KM1*(T4%R(I)-FMIN%R(I)))
            F%R(I)=MAX(F%R(I),T4%R(I)+KM1*(T4%R(I)-FMAX%R(I)))
          ENDDO
          IF(ICOR.GT.1.OR.K.LT.2.D0) THEN
            DO I=1,HN%DIM1
              F%R(I)=MIN(F%R(I),T4%R(I)+TWOTHIRDS*(FMAX%R(I)-T4%R(I)))
              F%R(I)=MAX(F%R(I),T4%R(I)+TWOTHIRDS*(FMIN%R(I)-T4%R(I)))
            ENDDO
          ENDIF
        ELSEIF(OPTADV.EQ.2.AND.(ICOR.GT.1.OR.K.LT.2.D0)) THEN
          DO I=1,HN%DIM1
            F%R(I)=MIN(F%R(I),T4%R(I)+KM1SURK*(FMAX%R(I)-T4%R(I)))
            F%R(I)=MAX(F%R(I),T4%R(I)+KM1SURK*(FMIN%R(I)-T4%R(I)))
          ENDDO
        ENDIF
!
        DO I=1,HN%DIM1
          DFDT(I)=(F%R(I)-T4%R(I))/DDT
        ENDDO
!
        CALL FLUX_EF_VF_2(MESH%W%R,MESH%NELEM,MESH%NELMAX,
!                                                      FN
     &                    MESH%IKLE%I,IOPT1,MESH%NPOIN,T4,
!    &                    FI_I,FSTAR, HN   H
     &                    T8%R,F%R   ,T2%R,HNP1MT%R,MESH%SURFAC%R,
     &                    TETAFCOR,DFDT)
        IF(NCSIZE.GT.1) CALL PARCOM(T8,2,MESH)
!
        IF(ICOR.EQ.NCO_DIST) THEN
          MASS_BAL=.TRUE.
        ELSE
          MASS_BAL=.FALSE.
        ENDIF
        DO I=1,HN%DIM1
!         WILL BE FSTAR IN TVF_2
          T6%R(I)=F%R(I)
        ENDDO
        CALL TVF_2(F%R,T6%R,T4%R,VOLU2D%R,UNSV2D%R,DDT,
     &             FLBOUND%R,FXBORPAR%R,FBOR%R,SMH%R,YASMH,FSCEXP%R,
     &             MESH%NPOIN,MESH%NPTFR,MESH%NBOR%I,LIMTRA,KDIR,KDDL,
     &             OPTSOU,HNP1MT%R,IOPT2,FLBORTRA%R,DDT/DT,RAIN,
     &             PLUIE%R,TRAIN,T8%R,TETAFCOR,MASS_BAL,ADMASS)
!                                FI_I
!
        ENDDO
!
      ENDIF  ! IF(PREDICOR.AND.NCO_DIST.GT.1)
!
!-----------------
! END CORRECTOR STEP
!-----------------
!
!     PREPARING FOR FURTHER ITERATIONS
!
      IF(.NOT.(LIPS.AND.NSP_DIST.EQ.1)) THEN
!
        DO I=1,HN%DIM1
!         T4 IS F(N+1)
          T4%R(I)=F%R(I)
        ENDDO
        IF(IOPT2.EQ.1) THEN
          DO I=1,HN%DIM1
            T1%R(I)=T2%R(I)
          ENDDO
        ENDIF
!
      ENDIF
!
      DT_REMAIN=DT_REMAIN-DDT
!
      IF(DT_REMAIN.NE.0.D0.AND.NIT.LT.NITMAX) GO TO 100
!
      IF(NIT.GE.NITMAX.AND.OPTADV.NE.4) THEN
        WRITE(LU,901) NIT
901     FORMAT(1X,'CVTRVF: ',1I6,' SUB-ITERATIONS REQUIRED FOR THE'
     &   ,/,1X,   '        DISTRIBUTIVE SCHEME. DECREASE THE TIME-STEP')
        CALL PLANTE(1)
        STOP
      ELSEIF(ENTET) THEN
        WRITE(LU,903) NIT
903     FORMAT(1X,'CVTRVF (BIEF): ',1I6,' SUB-ITERATIONS')
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERM
!
      DO I = 1,MESH%NPOIN
        F%R(I) = F%R(I)+DT*SM%R(I)
      ENDDO
!
!     IMPLICIT SOURCE TERM
!
      IF(YASMI) THEN
        DO I = 1,MESH%NPOIN
          F%R(I) = F%R(I)/(1.D0-DT*SMI%R(I)/MAX(H%R(I),1.D-15))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     UPDATING MASSOU
!
      IF(NCSIZE.GT.1) THEN
        MASSOU=MASSOU+P_SUM(ADMASS)
      ELSE
        MASSOU=MASSOU+ADMASS
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

