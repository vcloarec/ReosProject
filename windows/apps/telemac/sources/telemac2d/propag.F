!                   *****************
                    SUBROUTINE PROPAG
!                   *****************
!
     &(U,V,H,UCONV,VCONV,CONVV,H0,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T8,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,DT,
     & TETAH,TETAU,TETAD,
     & AGGLOH,AGGLOU,KDIR,INFOGR,KFROT,ICONVF,
     & ISOUSI,BILMAS,MASSES,MASS_RAIN,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,PRECCU,SOLSYS,OPDVIT,OPTSOU,
     & NFRLIQ,SLVPRO,EQUA,VERTIC,ADJO,ZFLATS,TETAZCOMP,UDEL,VDEL,DM1,
     & ZCONV,FLBOR,BM1S,BM2S,CV1S,VOLU2D,V2DPAR,UNSV2D,
     & NDGA1,NDGB1,NWEIRS,NPSING,HFROT,FLULIM,YAFLULIM,
     & FLULIMEBE,YAFLULIMEBE,RAIN,PLUIE,
     & MAXADV,OPTADV_VI,IKLE,NELMAX)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    PROPAGATION - DIFFUSION - SOURCE TERMS STEP TO SOLVE
!+                THE SAINT-VENANT EQUATIONS.
!+
!+
!+      BOUNDARY CONDITIONS:
!+
!+
!+      ==>   NEUMANN CONDITION
!+
!+
!+            * DIFFUSION   : NU DU/DN = AUBOR . U;
!+                            TREATS THE DIFFUSION MATRIX DIRECTLY
!+
!+
!+            * PROPAGATION : THE BOUNDARY TERMS ARE TREATED IN
!+                            THE SECOND MEMBERS (IMPLICIT)
!+
!+
!+      ==>   DIRICHLET CONDITION
!+
!+
!+            * DIFFUSION, PROPAGATION :
!+                            TREATED USING MODIFIED EQUATIONS IN " PROCLI "
!code
!+      IN MATRIX FORM:
!+
!+                   N+1          N+1          N+1
!+             AM1  H     +  BM1 U     +  BM2 V     =  CV1
!+
!+            T     N+1           N+1
!+          -  CM1 H      +  AM2 U                  =  CV2
!+
!+            T     N+1                        N+1
!+          -  CM2 H                   +  AM3 V     =  CV3
!
!note     BM* REPRESENT DIVERGENCE MATRICES;
!+            BM1: DERIVATION RELATIVE TO X;
!+            BM2: DERIVATION RELATIVE TO Y.
!note
!+THE TRANSPOSE OF MATRICES BM* IS EQUAL TO THE OPPOSITE
!+            OF GRADIENT. SOME SIGNS ARE THEREFORE OPPOSITE IN
!+            THE EQUATIONS OF SPEED.
!note
!+THE LAPLACIAN MATRIX (TM1) HAS BEEN INTEGRATED IN PART.
!+            THE SIGN IS THEREFORE OPPOSITE IN THE EQUATIONS OF
!+            SPEED.
!
!history  JMH
!+        07/05/2007
!+
!+   MODIFICATION ON THE SOURCES IN CASE OF MASS-LUMPING
!
!history
!+        10/06/2008
!+
!+   FINITE VOLUME ADVECTION FOR SPEEDS
!
!history
!+        02/10/2008
!+
!+   CALL TO CVTRVF (ONE MORE WORKING ARRAY)
!
!history
!+        20/07/2009
!+
!+   ICONVF (2) = 5 MANDATORY, ALL OTHER CASES ERASED
!
!history
!+        22/07/2009
!+
!+   EQUALITY OF FLUXES IMPOSED ON EITHER SIDE OF A WEIR
!
!history  J-M HERVOUET (LNHE)
!+        09/10/2009
!+        V6P0
!+   PARAMETERISED ADVECTION OPTIONS
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
!+        09/08/2011
!+        V6P2
!+   Adaptation to parallelism
!
!history  J-M HERVOUET (LNHE)
!+        249/02/2012
!+        V6P2
!+   Rain and evaporation added
!
!history  J-M HERVOUET (LNHE)
!+        09/04/2013
!+        V6P3
!+   DIMGLO=MESH%GLOSEG%DIM1 used in call to CVTRVF_POS_2. Strangely
!+   avoids an "array temporary created" with Intel compiler.
!
!history  J-M HERVOUET (LNHE)
!+        12/04/2013
!+        V6P3
!+   Value of NELBOR controlled for allowing bound checking.
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Adaptation to the dynamic allocation of weirs
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments. Arguments C0 and COTOND removed. Incident wave
!+   removed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        17/04/2014
!+        V7P0
!+   Correction for weirs. The function P_SUM for weirs must be called
!+   in all processors, so it must be called outside the test:
!+   IF(MESH%NPTFR.GT0) where it was in previous versions.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/05/2014
!+        V7P0
!+   A copy of LIMPRO is done to be sent to cvtrvf (that may change it).
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7
!+        Compensated assembly of the interface node
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        15/03/2016
!+        V7P2
!+   Enabling advection solver 15 (ERIA) for velocities with a double
!+   to cvtrvf_pos. Advection sschemes ADV_PSI_NC and ADV_NSC_NC removed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/04/2016
!+        V7P2
!+   A23%STOX and A32%STOX set to 1 to enable a call by MATVEC because
!+   these matrices are not built by MATRIX.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        07/09/2016
!+        V7P2
!+   Adaptation to splitting of cvtrvf_pos into cvtrvf_nerd and
!+   cvtrvf_eria.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A23            |<->| MATRIX
!| A32            |<->| MATRIX
!| ADJO           |-->| IF YES : ADJOINT MODE
!| AGGLOH         |-->| KEYWORD: 'MASS-LUMPING ON H'
!| AGGLOU         |-->| KEYWORD: 'MASS-LUMPING ON VELOCITY'
!| AM1            |<->| MATRIX APPLYING TO H
!| AM2            |<->| MATRIX APPLYING TO U
!| AM3            |<->| MATRIX APPLYING TO V
!| ATMOS          |-->| IF YES, ATMOSPHERIC PRESSURE IN PATMOS
!| AUBOR          |<--| LAW OF FRICTION ON BOUNDARIES
!|                |   | NUT*DU/DN=AUBOR*U+BUBOR
!| BILMAS         |-->| LOGICAL TRIGGERING A MASS BALANCE INFORMATION
!| BM1S           |<->| MATRIX
!| BM2            |<->| MATRIX
!| BM2S           |<->| MATRIX
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CM1            |<->| MATRIX
!| CM2            |<->| MATRIX
!| CONVV          |-->| ARRAY OF LOGICAL GIVING THE VARIABLES TO BE
!|                |   | ADVECTED
!|                |   | CONVV(1):U,V CONVV(2):H
!| CORCON         |-->| CONTINUITY CORRECTION ON POINTS WITH
!|                |   | IMPOSED DEPTH (COMPATIBLE FLUX IS COMPUTED)
!| CV1            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV2            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV3            |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| CV1S           |<->| RIGHT-HAND SIDE OF LINEAR SYSTEM
!| DH             |<--| H(N+1)-H(N)
!| DHN            |<--| H(N)-H(N-1)
!| DIFVIT         |-->| IF YES, DIFFUSION OF VELOCITY
!| DIRBOR         |<--| BLOCK WITH DIRICHLET BOUNDARY CONDITIONS
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| DU             |<--| U(N+1)-U(N)
!| DV             |<--| V(N+1)-V(N)
!| EQUA           |-->| KEYWORD: 'EQUATIONS'
!| FLBOR          |<--| FLUXES AT BOUNDARY POINTS
!| FLULIM         |-->| FLUX LIMITATION
!| FU             |<->| SOURCE TERMS ON VELOCITY U
!| FV             |<->| SOURCE TERMS ON VELOCITY V
!| GRAV           |-->| GRAVITY
!| H0             |-->| REFERENCE DEPTH
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| HN             |-->| DEPTH AT TIME T(N)
!| HFROT          |-->| KEYWORD: 'DEPTH IN FRICTION TERMS'
!| HPROP          |-->| PROPAGATION DEPTH
!| HTILD          |-->| DEPTH AFTER ADVECTION
!| ICONVF         |-->| TYPE OF ADVECTION: 4 INTEGERS
!|                |   | ICONVF(1) : U AND V
!|                |   | ICONVF(2) : H (MANDATORY VALUE = 5)
!|                |   | ICONVF(3) : TRACERS
!|                |   | ICONVF(4) : K AND EPSILON
!| IKLE           |-->| CONNECTIVITY TABLE
!| INFOGR         |-->| IF YES, INFORMATION ON GRADIENT
!| IORDRH         |-->| ORDER OF INITIAL GUESS OF H
!| IORDRU         |-->| ORDER OF INITIAL GUESS OF U
!| ISOUSI         |-->| NUMBER OF SUB-ITERATION IN THE TIME-STEP
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KFROT          |-->| KEYWORD: 'LAW OF BOTTOM FRICTION'
!| LIMPRO         |<->| BOUNDARY CONDITIONS FOR H, U V PER POINTS
!|                |   | AND SEGMENTS
!| LT             |-->| ITERATION NUMBER
!| MASK           |-->| BLOCK OF MASKS FOR SEGMENTS :
!|                |   | MASK(MSK1): 1. IF KDIR ON U 0. ELSE
!|                |   | MASK(MSK2): 1. IF KDIR ON V 0. ELSE
!|                |   | MASK(MSK3): 1. IF KDDL ON U 0. ELSE
!|                |   | MASK(MSK4): 1. IF KDDL ON V 0. ELSE
!|                |   | MASK(MSK6): 1. IF KNEU ON V 0. ELSE
!|                |   | MASK(MSK7): 1. IF KOND 0. ELSE
!|                |   | MASK(MSK9): 1. IF KDIR ON H (POINT)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MASSES         |-->| MASS OF WATER ADDED BY SOURCE TERM
!| MASS_RAIN      |-->| MASS ADDED BY RAIN OR EVAPORATION
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES
!| MAT            |<--| BLOCK OF MATRICES
!| MBOR           |<--| BOUNDARY MATRIX
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NDGA1          |-->| NDGA1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side1)
!| NDGB1          |-->| NDGB1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side2)
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| OPDVIT         |-->| OPTION FOR DIFFUSION OF VELOCITIES
!| OPTADV_VI      |-->| OPTION FOR THE ADVECTION SCHEME OF VELOCITIES
!| OPTBAN         |-->| KEYWORD: 'OPTION FOR THE TREATMENT OF TIDAL FLATS'
!| OPTSOU         |-->| KEYWORD: 'TYPE OF SOURCES'
!| OPTSUP         |-->| KEYWORD: 'SUPG OPTION'
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| PLUIE          |-->| RAIN OR EVAPORATION IN M/S IN A BIEF_OBJ
!| PRECCU         |-->| KEYWORD: 'C-U PRECONDITIONING'
!| RAIN           |-->| IF YES, RAIN OR EVAPORATION
!| RHS            |<->| BLOCK OF PRIVATE BIEF_OBJ STRUCTURES
!| RO             |-->| WATER DENSITY IF VARIABLE
!| ROEAU          |-->| WATER DENSITY
!| ROVAR          |-->| IF YES, VARIABLE WATER DENSITY.
!| S              |-->| VOID STRUCTURE
!| SLVPRO         |-->| SOLVER STRUCTURE FOR PROPAGATION
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SOLSYS         |-->| KEYWORD: 'TREATMENT OF THE LINEAR SYSTEM'
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| TB             |<->| BLOCK WITH T1,T2,...
!| TE1            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE2            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE3            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE4            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TE5            |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TETAD          |-->| IMPLICITATION ON DIFFUSION
!| TETAH          |-->| IMPLICITATION OF H IN U EQUATION
!| TETAU          |-->| IMPLICITATION OF U AND
!| TM1            |<->| MATRIX
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| UCONV          |-->| X-COMPONENT OF ADVECTION VELOCITY FIELD
!| VCONV          |-->| Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| UDEL           |<--| COMPATIBLE X-COMPONENT OF ADVECTION VELOCITY FIELD
!| UN             |<->| X-COMPONENT OF VELOCITY AT TIME T(N)
!| VN             |<->| Y-COMPONENT OF VELOCITY AT TIME T(N)
!| UNK            |<->| BLOCK OF UNKNOWNS
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| UTILD          |-->| VELOCITY U IF ADVECTED BY CHARACTERISTICS
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| VBOR           |-->| CONDITIONS AUX LIMITES SUR V.
!| VDEL           |<--| COMPATIBLE Y-COMPONENT OF ADVECTION VELOCITY FIELD
!| VERTIC         |-->| IF YES, THERE ARE VERTICAL STRUCTURES
!| VISC           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PERR POINT
!| VISC_S         |<->| WORK ARRAY FOR SAVING VISC
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS, NOT ASSEMBLED IN PARALLEL
!| VTILD          |-->| VELOCITY V IF ADVECTED BY CHARACTERISTICS
!| W1             |<->| WORK ARRAY
!| YAFLULIM       |-->| IF, YES, FLULIM TAKEN INTO ACCOUNT
!| YASMH          |-->| IF YES, SMH TAKEN INTO ACCOUNT
!| ZCONV          |<--| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFLATS         |<--| ELEVATION OF BOTTOM, MODIFIED FOR TIDAL FLATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : ADV_CAR,ADV_SUP,ADV_NSC,ADV_PSI,
     &                                 ADV_LPO,
     &                                 ADV_NSC_TF,ADV_PSI_TF,ADV_LPO_TF,
     &                                 KDDL
      USE DECLARATIONS_TELEMAC2D, ONLY : TYPSEUIL,IT1,IT2,TB2,NCO_DIST,
     &                                   NSP_DIST,DEBU,TROUVE
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE INTERFACE_TELEMAC2D, EX_PROPAG => PROPAG
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,OPTSUP(4),KDIR,KFROT,ICONVF(4),NWEIRS
      INTEGER, INTENT(IN) :: NELMAX
      INTEGER, INTENT(IN) :: IORDRH,IORDRU,ISOUSI,OPTBAN,OPTSOU,SOLSYS
      INTEGER, INTENT(IN) :: OPDVIT,NFRLIQ,HFROT,MAXADV,OPTADV_VI
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: TETAU,TETAD,TETAH,AGGLOH,AGGLOU
      DOUBLE PRECISION, INTENT(IN)    :: DT,GRAV,ROEAU
      DOUBLE PRECISION, INTENT(IN)    :: TETAZCOMP
      DOUBLE PRECISION, INTENT(INOUT) :: MASSES,MASS_RAIN
      LOGICAL, INTENT(IN) :: BILMAS,ATMOS,DIFVIT,INFOGR,CONVV(4),MSK
      LOGICAL, INTENT(IN) :: YASMH,ROVAR,PRECCU,VERTIC,ADJO,CORCON
      LOGICAL, INTENT(IN) :: YAFLULIM,RAIN,YAFLULIMEBE
      TYPE(SLVCFG), INTENT(INOUT)     :: SLVPRO
      CHARACTER(LEN=20),  INTENT(IN)  :: EQUA
!
!  STRUCTURES OF VECTORS
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: NPSING,NDGA1,NDGB1,FLULIMEBE
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,SMH,UN,VN,HN
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU2D,V2DPAR,UNSV2D,FLULIM
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RO,UDEL,VDEL,DM1,ZCONV,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: UTILD,VTILD,PATMOS,CF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: U,V,H,CV1,CV2,CV3,DH,DHN
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CV1S
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DU,DV,FU,FV,VISC,VISC_S,HTILD
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UBOR,VBOR,HBOR,AUBOR,LIMPRO
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,MASKPT,ZF,PLUIE
      TYPE(BIEF_OBJ), INTENT(IN)    :: HPROP,H0
!
!     TE : BY ELEMENT               TE4,TE5 ONLY IF OPTBAN=3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TE1,TE2,TE3,TE4,TE5,ZFLATS
!     T  : BY POINT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T8
!     DUMMY STRUCTURE
      TYPE(BIEF_OBJ), INTENT(IN)    :: S
!
!-----------------------------------------------------------------------
!
!  STRUCTURES OF MATRICES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A23,A32,MBOR,BM1S,BM2S
!
!-----------------------------------------------------------------------
!
!  STRUCTURES OF BLOCKS
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASK,MAT,RHS,UNK,TB,DIRBOR
!
!-----------------------------------------------------------------------
!
!  STRUCTURE OF MESH
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELMU,IELMH,UDIR,UDDL,UNEU,UNONNEU,IELEM,NELEM
      INTEGER I1,I2,I3,DIMLIM,DIMGLO,N,IOPT,DISCLIN
!
      DOUBLE PRECISION Z(1),SL1,SL1U,C,FL1,FL2
!
      LOGICAL MSKGRA
!
      CHARACTER(LEN=16) FORMUL
!
!-----------------------------------------------------------------------
!
      DIMLIM=LIMPRO%DIM1
      DIMGLO=MESH%GLOSEG%DIM1
!
      IELMH=H%ELM
      IELMU=U%ELM
!
!  ADDRESSES OF THE ARRAYS IN THE MASKING BLOCK: MASK
!
      UDIR = 1
!     VDIR = 2
      UDDL = 3
!     VDDL = 4
      UNEU = 5
!     VNEU = 6
      UNONNEU = 8
!
!  CONVENTION FOR LINEAR DISCRETISATION
!
      DISCLIN=11
!
!-----------------------------------------------------------------------
!
!  DIRICHLET BOUNDARY CONDITIONS FOR INCREASE IN H
!
!  HBOR = HBOR - HN (HBOR ON THE BOUNDARY, HN IN THE DOMAIN)
!
      HBOR%DIM1=MESH%NPTFR
      CALL OSBD( 'X=X-Y   ' , HBOR , HN , HN , C , MESH )
!
!=======================================================================
!
!    GRADIENT MATRICES FOR THE CONTINUITY EQUATION:
!
!    BM1 = - TETAU ( D(HN.F1)/DX * F2)
!    BM2 = - TETAU ( D(HN.F1)/DY * F2)
!
      IF(OPTBAN.EQ.3) THEN
!       TAKES POROSITY INTO ACCOUNT
        CALL MATRIX(BM1,'M=N     ','MATFGR         X',IELMH,IELMU,
     &              TETAU,HPROP,S,S,S,S,S,MESH,.TRUE.,TE5)
        CALL MATRIX(BM2,'M=N     ','MATFGR         Y',IELMH,IELMU,
     &              TETAU,HPROP,S,S,S,S,S,MESH,.TRUE.,TE5)
!
!       MATRICES RESULTING FROM SUPG APPLIED TO THE ADVECTION TERM
!
        IF(OPTSUP(2).EQ.1) THEN
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(BM1,'M=M+N   ','MATUGH         X',IELMH,IELMU,
     &                TETAU,HPROP,S,S,TE1,TE2,S,MESH,.TRUE.,TE5)
          CALL MATRIX(BM2,'M=M+N   ','MATUGH         Y',IELMH,IELMU,
     &                TETAU,HPROP,S,S,TE1,TE2,S,MESH,.TRUE.,TE5)
        ELSEIF(OPTSUP(2).EQ.2) THEN
          CALL MATRIX(BM1,'M=M+N   ','MATUGH         X',IELMH,IELMU,
     &                0.5*DT*TETAU,HPROP,S,S,UCONV,VCONV,S,
     &                MESH,.TRUE.,TE5)
          CALL MATRIX(BM2,'M=M+N   ','MATUGH         Y',IELMH,IELMU,
     &                0.5*DT*TETAU,HPROP,S,S,UCONV,VCONV,S,
     &                MESH,.TRUE.,TE5)
        ENDIF
!
      ELSE
!
        CALL MATRIX(BM1,'M=N     ','MATFGR         X',IELMH,IELMU,
     &              TETAU,HPROP,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL MATRIX(BM2,'M=N     ','MATFGR         Y',IELMH,IELMU,
     &              TETAU,HPROP,S,S,S,S,S,MESH,MSK,MASKEL)
!
!       MATRICES RESULTING FROM SUPG APPLIED TO THE ADVECTION TERM
!
        IF(OPTSUP(2).EQ.1) THEN
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(BM1,'M=M+N   ','MATUGH         X',IELMH,IELMU,
     &                TETAU,HPROP,S,S,TE1,TE2,S,MESH,MSK,MASKEL)
          CALL MATRIX(BM2,'M=M+N   ','MATUGH         Y',IELMH,IELMU,
     &                TETAU,HPROP,S,S,TE1,TE2,S,MESH,MSK,MASKEL)
        ELSEIF(OPTSUP(2).EQ.2) THEN
          CALL MATRIX(BM1,'M=M+N   ','MATUGH         X',IELMH,IELMU,
     &                0.5*DT*TETAU,HPROP,S,S,UCONV,VCONV,S,
     &                MESH,MSK,MASKEL)
          CALL MATRIX(BM2,'M=M+N   ','MATUGH         Y',IELMH,IELMU,
     &                0.5*DT*TETAU,HPROP,S,S,UCONV,VCONV,S,
     &                MESH,MSK,MASKEL)
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!   BUILDS THE DIFFUSION MATRIX :
!
!    TM1 =  VISC . (P03 * (DF1/DX * DF2/DX + DF1/DY * DF2/DY) )
!
      IF(DIFVIT) THEN
!
        IF(OPDVIT.EQ.2) THEN
!         SAVES DIFFUSION
          CALL OS('X=Y     ',X=VISC_S,Y=VISC)
!         MULTIPLIES DIFFUSION BY HPROP
          CALL OV_2('X=XY    ',VISC%R,1,HPROP%R,1,Z,1,C,
     &                         VISC%MAXDIM1,VISC%DIM1)
          IF(VISC%DIM2.EQ.3) THEN
            CALL OV_2('X=XY    ',VISC%R,2,HPROP%R,1,Z,1,C,
     &                           VISC%MAXDIM1,VISC%DIM1)
            CALL OV_2('X=XY    ',VISC%R,3,HPROP%R,1,Z,1,C,
     &                           VISC%MAXDIM1,VISC%DIM1)
          ENDIF
        ENDIF
!
        CALL MATRIX(TM1,'M=N     ','MATDIF          ',IELMU,IELMU,
     &              1.D0,S,S,S,VISC,S,S,MESH,MSK,MASKEL)
!
        IF(OPDVIT.EQ.2) THEN
!         MULTIPLIES THE MATRIX BY 1/HPROP
          CALL CPSTVC(HPROP,T4)
          DO I=1,HPROP%DIM1
!           BEWARE: HIDDEN PARAMETER 1.D-2, NO DIFFUSION BELOW 1 CM DEPTH
!                                           WITH THIS OPTION
            IF(HPROP%R(I).GT.1.D-2) THEN
              T4%R(I)=1.D0/HPROP%R(I)
            ELSE
              T4%R(I)=0.D0
            ENDIF
          ENDDO
          IF(T4%ELM.NE.IELMU) CALL CHGDIS(T4,T4%ELM,IELMU,MESH)
          CALL OM('M=X(M)  ', M=TM1, MESH=MESH)
          CALL OM('M=DM    ', M=TM1, D=T4, MESH=MESH)
!         RESTORES DIFFUSION
          CALL OS('X=Y     ',X=VISC,Y=VISC_S)
        ENDIF
!
        ! NELEB MAY BE NUL IN PARALLEL MODE
        IF(MESH%NELEB.GT.0) THEN
          CALL MATRIX(MBOR,'M=N     ','FMATMA          ',
     &                IELBOR(IELMU,1),IELBOR(IELMU,1),
     &               -1.D0,AUBOR,S,S,S,S,S,MESH,.TRUE.,MASK%ADR(UNEU)%P)
          CALL OM('M=M+N   ', M=TM1, N=MBOR, MESH=MESH)
        ENDIF
!
!       EXPLICIT PART DEALT WITH IN THE NEXT IF(DIFVIT...
!
      ENDIF
!
!=======================================================================
!
!  COMPUTES THE FREE SURFACE ELEVATION (IN T8)
!
      CALL OS('X=Y+Z   ', X=T8, Y=HN, Z=ZF)
!
!  OPTION 1 FOR THE TREATMENT OF TIDAL FLATS
!  A MASK MAY BE USED FOR TIDAL FLATS ALTHOUGH THIS
!  IS NOT THE MASKING OPTION.
!  THIS MASK IS IN TE3
!
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
        CALL DECVRT(TE3,T8,ZF,MESH)
      ENDIF
!
!     FREE SURFACE GRADIENT
!
      IF (OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
!                   SL CORRECTED BY ELEMENT
        CALL CORRSL(ZFLATS,T8,ZF,MESH)
        CALL VECTOR(CV2,'=','GRADF          X',IELMU,
     &              -GRAV,ZFLATS,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(CV3,'=','GRADF          Y',IELMU,
     &              -GRAV,ZFLATS,S,S,S,S,S,MESH,MSK,MASKEL)
!       CORRSL DECLARES A DISCONTINUOUS ELEMENT, RESTORES BACK
      ELSE
        CALL VECTOR(CV2,'=','GRADF          X',IELMU,
     &              -GRAV,T8,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(CV3,'=','GRADF          Y',IELMU,
     &              -GRAV,T8,S,S,S,S,S,MESH,MSK,MASKEL)
      ENDIF
!
!  ADDITIONAL GRADIENT TERMS : ATMOSPHERIC PRESSURE
!                              VARIABLE DENSITY
!
!  THESE DRIVING TERMS SHOULD NOT BE ADDED IN TIDAL FLATS
!
!
      IF(ROVAR.OR.ATMOS) THEN
!
!  CASE WHERE THESE GRADIENTS SHOULD BE MASKED: MASKING REQUIRED OR
!                                               OPTBAN=1
!
      IF(MSK.OR.OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
        MSKGRA = .TRUE.
!       IF OPTBAN=1 THE MASK USED HERE SHOULD HAVE VALUES
        IF(OPTBAN.NE.1.AND.OPTBAN.NE.3) THEN
          CALL OV('X=Y     ', X=TE3%R, Y=MASKEL%R, DIM1=TE3%DIM1)
        ENDIF
      ELSE
        MSKGRA = .FALSE.
      ENDIF
!
!     ATMOSPHERIC PRESSURE GRADIENT
!
      IF(ATMOS) THEN
        CALL VECTOR(CV2,'+','GRADF          X',IELMU,
     &              -1.D0/ROEAU,PATMOS,S,S,S,S,S,MESH,MSKGRA,TE3)
        CALL VECTOR(CV3,'+','GRADF          Y',IELMU,
     &              -1.D0/ROEAU,PATMOS,S,S,S,S,S,MESH,MSKGRA,TE3)
      ENDIF
!
!     ADDITIONAL TERMS IF THE DENSITY IS VARIABLE
!
      IF(ROVAR) THEN
!
        CALL OS( 'X=X+C   ' , X=RO , C=-ROEAU )
!
!       PRESSURE BAROCLINE
!
        CALL VECTOR(CV2,'+','GGRADF         X',IELMU,
     &              -0.5D0*GRAV/ROEAU,RO,HN,S,S,S,S,MESH,MSKGRA,TE3)
        CALL VECTOR(CV3,'+','GGRADF         Y',IELMU,
     &              -0.5D0*GRAV/ROEAU,RO,HN,S,S,S,S,MESH,MSKGRA,TE3)
        CALL OS( 'X=X+C   ' , X=RO , C=+ROEAU )
!
      ENDIF
!
      ENDIF
!
!=======================================================================
!
!    MASS MATRIX / DT
!    AM1 WILL BE MODIFIED AT A LATER DATE IF SUPG IS USED ON H
!
!    AM1 = ( 1 / DT )  * (F1 * F2)
!
      SL1 = 1.D0 / DT
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
!       LOCALLY LUMPED MASS MATRIX (TIDAL FLATS)
        FORMUL='MSLUMP          '
      ELSE
!       NORMAL MASS MATRIX
        FORMUL='MATMAS          '
      ENDIF
      IF(OPTBAN.NE.3) THEN
        CALL MATRIX(AM1,'M=N     ',FORMUL,IELMH,IELMH,
     &              SL1,TE3,S,S,S,S,S,MESH,MSK,MASKEL)
      ELSE
        CALL MATRIX(AM1,'M=N     ',FORMUL,IELMH,IELMH,
     &              SL1,TE3,S,S,S,S,S,MESH,.TRUE.,TE5)
      ENDIF
!
!   MASS MATRIX FOR THE MOMENTUM EQUATION
!
      IF(SOLSYS.EQ.1) THEN
!
!                           OPTBAN.NE.3 TO AVOID POROSITY IN AM2
      IF(IELMU.EQ.IELMH.AND.OPTBAN.NE.3) THEN
        CALL OM('M=N     ', M=AM2, N=AM1, MESH=MESH)
      ELSE
        CALL MATRIX(AM2,'M=N     ',FORMUL,IELMU,IELMU,
     &              SL1,TE3,S,S,S,S,S,MESH,MSK,MASKEL)
      ENDIF
!     MASS-LUMPING OF AM2 :
      IF(AGGLOU.GT.0.001D0) THEN
        CALL LUMP(T2,AM2,MESH,AGGLOU)
        CALL OM('M=CM    ', M=AM2, C=1.D0-AGGLOU, MESH=MESH)
        CALL OM('M=M+D   ', M=AM2, D=T2, MESH=MESH)
      ENDIF
!
      ELSEIF(SOLSYS.EQ.2) THEN
!
        IF(IELMU.NE.IELMH) THEN
          CALL VECTOR(AM2%D,'=','MASBAS          ',IELMU,
     &                SL1,S,S,S,S,S,S,MESH,MSK,MASKEL)
        ELSE
          CALL OS('X=CY    ',X=AM2%D,Y=VOLU2D,C=SL1)
        ENDIF
        AM2%TYPDIA='Q'
        AM2%TYPEXT='0'
!
      ENDIF
!
! MASS-LUMPING OF AM1 :
!
      IF(AGGLOH.GT.0.001D0) THEN
        CALL LUMP(T1,AM1,MESH,AGGLOH)
        CALL OM('M=CM    ', M=AM1, C=1.D0-AGGLOH, MESH=MESH)
        CALL OM('M=M+D   ', M=AM1, D=T1, MESH=MESH)
      ENDIF
!
! END OF MASS-LUMPING
!
! TEMPORARILY STORES THE LUMPED MASS MATRIX: AM2 IN AM3
! FOR THE COMPUTATION OF THE TERMS FROM TIME DERIVATIVES
!
      IF(SOLSYS.EQ.1) THEN
        CALL OM('M=N     ', M=AM3, N=AM2, MESH=MESH)
      ELSEIF(SOLSYS.EQ.2) THEN
        CALL OS('X=Y     ',X=AM3%D,Y=AM2%D)
        AM3%TYPDIA=AM2%TYPDIA
        AM3%TYPEXT=AM2%TYPEXT
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE VECTORS IN THE SECOND MEMBERS
!
!   BEWARE : PAY A LOT OF ATTENTION TO PARAMETER LEGO (FALSE OR TRUE)
!            IN CALLS TO MATVEC, VGRADF. IF LEGO = .FALSE. THE
!            COMPUTATION RESULT GOES IN W1. IN THIS CASE, SHOULD ALWAYS
!            USE OPERATIONS 'X=X+...' NOT TO ERASE WHAT WAS IN W1,
!            AND END BY LEGO=.TRUE. FOR THE FINAL ASSEMBLY.
!
!
!-----------------------------------------------------------------------
!
!     CV1 = CV1 + SL1U * ( BM1 * UN + BM2 * VN )
!
      SL1U   = (TETAU-1.D0)/TETAU
!                           TETAU : BM1 WAS BUILT WITH THIS COEFFICIENT
      IF(ABS(SL1U).GT.0.0001D0) THEN
        CALL MATVEC('X=CAY   ',CV1,BM1,UN,SL1U,MESH)
        CALL MATVEC('X=X+CAY ',CV1,BM2,VN,SL1U,MESH)
      ELSE
        CALL CPSTVC(H,CV1)
        CALL OS('X=0     ',X=CV1)
      ENDIF
!
!     SOURCE TERMS IN THE CONTINUITY EQUATION :
!
      MASSES    = 0.D0
      MASS_RAIN = 0.D0
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
!         STANDARD VERSION
!         TAKING MASS-LUMPING INTO ACCOUNT
!         COEFFICIENT DT TO COUNTERACT THE FACT THAT
!         AM1 IS DONE WITH A COEFFICIENT 1/DT
          CALL MATVEC( 'X=CAY   ',T1,AM1,SMH,DT,MESH)
!         WITHOUT TAKING MASS-LUMPING INTO ACCOUNT
!         CALL VECTOR(T1,'=','MASVEC          ',IELMH,
!    *                1.D0,SMH,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL OS( 'X=X+Y   ' , X=CV1 , Y=T1 )
          IF(BILMAS) MASSES = DT * BIEF_SUM(T1)
        ELSE
!         DIRAC VERSION
          CALL OS( 'X=X+Y   ' ,X=CV1,Y=SMH)
          IF(BILMAS) MASSES = DT * BIEF_SUM(SMH)
        ENDIF
!       THE FOLLOWING LINE GOES IN BILAN
!       IF (NCSIZE.GT.1) MASSES=P_SUM (MASSES)
      ENDIF
!
!     SAME THING WITH RAIN-EVAPORATION (LIKE OPTSOU=1)
!
      IF(RAIN) THEN
        CALL MATVEC( 'X=CAY   ',T1,AM1,PLUIE,DT,MESH)
        CALL OS( 'X=X+Y   ' , X=CV1 , Y=T1 )
        IF(BILMAS) THEN
          MASS_RAIN = DT * BIEF_SUM(T1)
          MASSES    = MASSES + MASS_RAIN
        ENDIF
      ENDIF
!
!  DEBUT DES CONVECTIONS DE U
!
!-----------------------------------------------------------------------
!
!     ADVECTION OF U AND V
!
      IF(ICONVF(1).NE.ADV_LPO.AND.ICONVF(1).NE.ADV_LPO_TF.AND.
     &   ICONVF(1).NE.ADV_NSC.AND.ICONVF(1).NE.ADV_NSC_TF.AND.
     &   ICONVF(1).NE.ADV_PSI.AND.ICONVF(1).NE.ADV_PSI_TF     ) THEN
        CALL OS( 'X=CY    ' , X=T1 , Y=FU , C=DT )
        CALL OS( 'X=CY    ' , X=T2 , Y=FV , C=DT )
      ENDIF
!
      IF(ICONVF(1).EQ.ADV_CAR.OR.(.NOT.CONVV(1))) THEN
!
!       ADVECTION WITH THE METHOD OF CHARACTERISTICS
!
        CALL OS( 'X=X+Y   ' , X=T1 , Y=UTILD )
        CALL OS( 'X=X+Y   ' , X=T2 , Y=VTILD )
!
!------ SCHEMA SEMI-IMPLICITE CENTRE + S.U.P.G -----------------------
!
      ELSEIF(ICONVF(1).EQ.ADV_SUP) THEN
!
!       CENTRED SEMI-IMPLICIT ADVECTION TERM: MATRIX
!
        CALL MATRIX(CM2,'M=N     ','MATVGR          ',IELMU,IELMU,
     &              1.D0,S,S,S,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
!
!       SUPG CONTRIBUTION
!
        IF(OPTSUP(1).EQ.1) THEN
!         CLASSICAL SUPG
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(CM2,'M=M+N   ','MASUPG          ',IELMU,IELMU,
     &                1.D0,TE1,TE2,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ELSEIF(OPTSUP(1).EQ.2) THEN
!         MODIFIED SUPG
          CALL MATRIX(CM2,'M=M+N   ','MAUGUG          ',IELMU,IELMU,
     &                0.5D0*DT,S,S,S,UCONV,VCONV,VCONV,
     &                MESH,MSK,MASKEL)
        ENDIF
!
!       END OF SUPG CONTRIBUTION
!
!       EXPLICIT SECOND MEMBER
!
        IF(ABS(SL1U).GT.0.0001D0) THEN
          CALL MATVEC( 'X=X+CAY ',CV2,CM2,UN,TETAU-1.D0,MESH)
          CALL MATVEC( 'X=X+CAY ',CV3,CM2,VN,TETAU-1.D0,MESH)
        ENDIF
!
!       MATRIX : AM2 HAS A NON-SYMMETRICAL STRUCTURE
!
        IF(AM2%TYPEXT.NE.'Q') THEN
          CALL OM('M=X(M)  ', M=AM2, MESH=MESH)
        ENDIF
        CALL OM('M=M+CN  ', M=AM2, N=CM2, C=TETAU, MESH=MESH)
!
        CALL OS( 'X=X+Y   ' , X=T1 , Y=UN )
        CALL OS( 'X=X+Y   ' , X=T2 , Y=VN )
!
!------ FINITE VOLUMES SCHEME --------------------------------------
!
!     NOTE: HERE THE CONTINUITY EQUATION IS NOT SOLVED
!           BY H, HN AND UCONV,VCONV (UCONV, VCONV HAVE BEEN
!           UPDATED AND H, HN ARE STILL UNCHANGED, SO THE FINAL H
!           COMPUTED IN CVTRVF WILL NOT BE THE FINAL DEPTH OF THE
!           TIMESTEP).
!
      ELSEIF(ICONVF(1).EQ.ADV_LPO.OR.
     &       ICONVF(1).EQ.ADV_NSC.OR.
     &       ICONVF(1).EQ.ADV_PSI     ) THEN
!
        IF(ICONVF(1).EQ.ADV_LPO.OR.ICONVF(1).EQ.ADV_NSC) IOPT=2
        IF(ICONVF(1).EQ.ADV_PSI) IOPT=3
!       HERE YASMH=.FALSE. (SOURCES ACCOUNTED FOR IN FU)
        IF(TB%N.LT.22) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL FOR CVTRVF IN PROPAG'
          CALL PLANTE(1)
          STOP
        ENDIF
!       USING A COPY OF LIMPRO (IT MAY BE CHANGED BY CVTRVF)
        DO I=1,MESH%NPTFR
          IT1%I(I)=LIMPRO%I(DIMLIM+I)
          IT2%I(I)=LIMPRO%I(2*DIMLIM+I)
        ENDDO
        CALL CVTRVF(T1,UN,S,H,HN,
     &              HPROP,UCONV,VCONV,S,S,
     &              1,FU,S,.FALSE.,S,.FALSE.,UBOR,MASK,MESH,
     &              AGGLOH,DT,INFOGR,
     &              MSK,MASKEL,S,C,1,IT1%I,
     &              KDIR,KDDL,MESH%NPTFR,FLBOR,.FALSE.,
     &              VOLU2D,V2DPAR,UNSV2D,IOPT,TB%ADR(12)%P,MASKPT,
     &              RAIN,PLUIE,0.D0,OPTADV_VI,TB,13,BM1S,TB2,NCO_DIST,
     &              NSP_DIST,
     &              YAFLULIM,FLULIM%R,YAFLULIMEBE,
     &              FLULIMEBE%R,SLVPRO)
        CALL CVTRVF(T2,VN,S,H,HN,
     &              HPROP,UCONV,VCONV,S,S,
     &              1,FV,S,.FALSE.,S,.FALSE.,VBOR,MASK,MESH,
     &              AGGLOH,DT,INFOGR,
     &              MSK,MASKEL,S,C,1,IT2%I,
     &              KDIR,KDDL,MESH%NPTFR,FLBOR,.FALSE.,
     &              VOLU2D,V2DPAR,UNSV2D,IOPT,TB%ADR(12)%P,MASKPT,
     &              RAIN,PLUIE,0.D0,OPTADV_VI,TB,13,BM1S,TB2,NCO_DIST,
     &              NSP_DIST,
     &              YAFLULIM,FLULIM%R,YAFLULIMEBE,
     &              FLULIMEBE%R,SLVPRO)
        IF(IELMU.NE.11) THEN
          CALL CHGDIS(T1,DISCLIN,IELMU,MESH)
          CALL CHGDIS(T2,DISCLIN,IELMU,MESH)
        ENDIF
!
!------ SCHEMA VOLUMES FINIS AVEC BANCS DECOUVRANTS -------------------
!
!     NOTE: HERE THE CONTINUITY EQUATION IS NOT SOLVED
!           BY H, HN AND UCONV,VCONV (UCONV, VCONV HAVE BEEN
!           UPDATED AND H, HN ARE STILL UNCHANGED, SO THE FINAL H
!           COMPUTED IN CVTRVF WILL NOT BE THE FINAL DEPTH OF THE
!           TIMESTEP).
!
      ELSEIF(ICONVF(1).EQ.ADV_LPO_TF.OR.
     &       ICONVF(1).EQ.ADV_NSC_TF.OR.
     &       ICONVF(1).EQ.ADV_PSI_TF     ) THEN
!
        IF(ICONVF(1).EQ.ADV_LPO_TF) IOPT=2
        IF(ICONVF(1).EQ.ADV_NSC_TF) IOPT=2
        IF(ICONVF(1).EQ.ADV_PSI_TF) IOPT=3
!       HERE YASMH=.FALSE. (SOURCES ACCOUNTED FOR IN FU)
        IF(TB%N.LT.22) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL IN PROPAG'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(ICONVF(1).EQ.ADV_LPO_TF.OR.ICONVF(1).EQ.ADV_NSC_TF) THEN
!         THIS IS EQUIVALENT TO TWO SUCCESSIVE CALLS TO CVTRVF_NERD
!         FOR U AND V
          CALL CVTRVF_NERD_2(T1,UN,S,T2,VN,S,H,HN,
     &        HPROP,UCONV,VCONV,S,S,
     &        1,FU,FV,S,.FALSE.,S,S,.FALSE.,UBOR,VBOR,MASK,MESH,
     &        TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &        TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &        TB%ADR(19)%P,TB%ADR(20)%P,
     &        TB%ADR(22)%P,
     &        DT,INFOGR,MSK,MASKEL,1,
     &        LIMPRO%I(1+DIMLIM:2*DIMLIM),
     &        LIMPRO%I(1+2*DIMLIM:3*DIMLIM),
     &        KDIR,KDDL,MESH%NPTFR,FLBOR,.FALSE.,
     &        UNSV2D,IOPT,TB%ADR(11)%P,TB%ADR(12)%P,
     &        MESH%GLOSEG%I(       1:  DIMGLO),
     &        MESH%GLOSEG%I(DIMGLO+1:2*DIMGLO),
     &        MESH%NBOR%I,RAIN,PLUIE,0.D0,0.D0,
     &        MAXADV)
!                       2: HARDCODED OPTION
        ELSE
!         SCHEME 15 HAS NOT BEEN DONE FOR 2 VARIABLES, SO 2 CALLS...
!                                FSCEXP (IF YASMH, HERE GIVEN FALSE)
          CALL CVTRVF_ERIA(T1,UN,S,H,HN,HPROP,UCONV,VCONV,
!                          DM1,ZCONV
     &                     S  ,S,
!                          SOLSYS (FORCED TO 1 BECAUSE DM1... NOT YET DONE)
     &                     1,
!                             SMH,YASMH,SMI,YASMI,
     &                     FU,S, .FALSE.,S,.FALSE.,
     &                     UBOR,MASK,MESH,
     &                     TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &                     TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &                     TB%ADR(19)%P,TB%ADR(20)%P,
     &                     DT,INFOGR,MSK,MASKEL,1,
     &                     LIMPRO%I(1+DIMLIM:2*DIMLIM),
!                                                     YAFLBOR
     &                     KDIR,KDDL,MESH%NPTFR,FLBOR,.FALSE.,
     &                     UNSV2D,IOPT,TB%ADR(11)%P,
     &                     MESH%NBOR%I,RAIN,PLUIE,0.D0,
     &                     MAXADV,NCO_DIST,OPTADV_VI)
!                                FSCEXP (IF YASMH, HERE GIVEN FALSE)
          CALL CVTRVF_ERIA(T2,VN,S,H,HN,HPROP,UCONV,VCONV,
!                              SOLSYS (FORCED TO 1 BECAUSE DM1... NOT YET DONE)
     &                     S,S,1,FV,S,.FALSE.,S,.FALSE.,
     &                     VBOR,MASK,MESH,
     &                     TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &                     TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &                     TB%ADR(19)%P,TB%ADR(20)%P,
     &                     DT,INFOGR,MSK,MASKEL,1,
     &                     LIMPRO%I(1+2*DIMLIM:3*DIMLIM),
!                                                     YAFLBOR
     &                     KDIR,KDDL,MESH%NPTFR,FLBOR,.FALSE.,
     &                     UNSV2D,IOPT,TB%ADR(11)%P,
     &                     MESH%NBOR%I,RAIN,PLUIE,0.D0,
     &                     MAXADV,NCO_DIST,OPTADV_VI)
!
        ENDIF
!
        IF(IELMU.NE.11) THEN
          CALL CHGDIS(T1,DISCLIN,IELMU,MESH)
          CALL CHGDIS(T2,DISCLIN,IELMU,MESH)
        ENDIF
!
!-----------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,2003) ICONVF(1)
2003    FORMAT(1X,'PROPAG : UNKNOWN TYPE OF ADVECTION FOR U: ',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! ADDS THE TERM AM3 * T1
! HERE AM3 MUST BE MASS/DT POSSIBLY WITH MASS-LUMPING ON U
!
      IF(SOLSYS.EQ.1) THEN
        CALL MATVEC( 'X=X+AY  ',CV2,AM3,T1,C,MESH)
        CALL MATVEC( 'X=X+AY  ',CV3,AM3,T2,C,MESH)
      ELSEIF(SOLSYS.EQ.2) THEN
        CALL OS('X=X+YZ  ',X=CV2,Y=AM3%D,Z=T1)
        CALL OS('X=X+YZ  ',X=CV3,Y=AM3%D,Z=T2)
      ENDIF
!
!  END OF ADVECTION OF U AND V
!
!=======================================================================
!
!
!     COMPUTES THE DIAGONAL MATRICES: - FU* (MASS OF THE BASES)
!                                AND: - FV* (MASS OF THE BASES)
!
      IF(KFROT.NE.0.OR.VERTIC) THEN
!
!       T3,T4 : BOTTOM FRICTION      T5,T6 : VERTICAL STRUCTURES
!
        CALL FRICTI(T3,T4,T5,T6,UN,VN,HN,CF,MESH,T1,T2,VERTIC,
     &              UNSV2D,MSK,MASKEL,HFROT)
!
!       COMPUTES THE DIAGONAL MATRICES: - FU* (MASS OF THE BASES)
!                                  AND: - FV* (MASS OF THE BASES)
!
        CALL SLOPES(TE3,ZF,MESH)
        CALL VECTOR(T2,'=','MASBAS          ',IELMU,
     &              -1.D0,S,S,S,S,S,S,MESH,.TRUE.,TE3)
!
        CALL OS('X=XY    ', X=T3 , Y=T2 )
        CALL OS('X=XY    ', X=T4 , Y=T2 )
!
        IF(VERTIC) THEN
          CALL VECTOR(T2,'=','MASBAS          ',IELMU,
     &              -1.D0,S,S,S,S,S,S,MESH,.FALSE.,TE3)
          CALL OS('X=XY    ', X=T5, Y=T2)
          CALL OS('X=XY    ', X=T6, Y=T2)
          CALL OS('X=X+Y   ', X=T3, Y=T5)
          CALL OS('X=X+Y   ', X=T4, Y=T6)
        ENDIF
!
      ELSE
!
        CALL CPSTVC(U,T3)
        CALL CPSTVC(V,T4)
        CALL OS( 'X=0     ' , X=T3 )
        CALL OS( 'X=0     ' , X=T4 )
!
        IF(OPTBAN.EQ.1) THEN
!         SLOWING DOWN VELOCITIES ON TIDAL FLATS
!         HAPPENS WHEN CALLED BY TELEMAC-3D WITH OPTT2D=1
          IF(IELMU.NE.IELMH) THEN
            CALL VECTOR(T2,'=','MASBAS          ',IELMU,
     &                  1.D0,S,S,S,S,S,S,MESH,.FALSE.,TE3)
!           T3 AND T4 ARE ADDED TO AM3, WHICH IS ASSEMBLED LATER
!           SO NO PARCOM FOR T2 IN PARALLEL
!           IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
            CALL OS('X=Y     ',X=T5,Y=HPROP)
            IF(T5%ELM.NE.IELMU) CALL CHGDIS(T5,T5%ELM,IELMU,MESH)
            DO I=1,BIEF_NBPTS(IELMU,MESH)
!             HIDDEN PARAMETER
              IF(T5%R(I).LT.1.D-3) THEN
                T3%R(I)=10.D0*T2%R(I)/DT
                T4%R(I)=T3%R(I)
              ENDIF
            ENDDO
          ELSE
            DO I=1,BIEF_NBPTS(IELMU,MESH)
!             HIDDEN PARAMETER
              IF(HPROP%R(I).LT.1.D-3) THEN
!               T3 AND T4 ARE ADDED TO AM3, WHICH IS ASSEMBLED LATER
!               SO USE OF VOLU2D RATHER THAN OLD V2DPAR IN PARALLEL
                T3%R(I)=10.D0*VOLU2D%R(I)/DT
                T4%R(I)=T3%R(I)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
!
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE MATRICES
!
!    AM1: ALREADY COMPUTED
!
!    AM2 = AM2 + TM1
!
      IF(DIFVIT) THEN
!
!    TEST: IMPLICITATION OF TM1 DIAGONAL OF TM1 IN WAVE EQUATION
!
!
        IF(SOLSYS.EQ.2) THEN
!
          CALL OS('X=X+CY  ',X=AM2%D,Y=TM1%D,C=TETAD)
          CALL OS('X=CX    ',X=TM1%D,C=1.D0-TETAD)
!
          CALL MATVEC( 'X=X+CAY ',CV2,TM1,UN,-1.D0,MESH)
          CALL MATVEC( 'X=X+CAY ',CV3,TM1,VN,-1.D0,MESH)
!
        ELSEIF(SOLSYS.EQ.1) THEN
!
          IF(TETAD.LT.0.9999D0) THEN
            IF(TETAD.GT.0.0001D0) THEN
              IF(AM2%TYPEXT.EQ.'S'.AND.TM1%TYPEXT.EQ.'Q') THEN
                CALL OM('M=X(M)  ', M=AM2, MESH=MESH)
              ENDIF
              CALL OM('M=M+CN  ', M=AM2, N=TM1, C=TETAD, MESH=MESH)
            ENDIF
!           EXPLICIT PART :
            CALL MATVEC( 'X=X+CAY ',CV2,TM1,UN,TETAD-1.D0,MESH)
            CALL MATVEC( 'X=X+CAY ',CV3,TM1,VN,TETAD-1.D0,MESH)
          ELSE
!           ENTIRELY IMPLICIT
            IF(AM2%TYPEXT.EQ.'S'.AND.TM1%TYPEXT.EQ.'Q') THEN
              CALL OM('M=X(M)  ', M=AM2, MESH=MESH)
            ENDIF
            CALL OM('M=M+N   ', M=AM2, N=TM1, MESH=MESH)
          ENDIF
!
        ELSE
!
          WRITE(LU,*) 'PROPAG : WRONG CHOICE FOR SOLSYS'
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDIF
!
!    AM3 = AM2 (DIFFUSION HAS BEEN ADDED TO AM2, NOT TO AM3)
!
      IF(SOLSYS.EQ.1) THEN
        CALL OM('M=N     ', M=AM3, N=AM2, MESH=MESH)
      ELSEIF(SOLSYS.EQ.2) THEN
        CALL OS('X=Y     ',X=AM3%D,Y=AM2%D)
      ENDIF
!
!=======================================================================
!
!   DEFINA METHOD CORRECTED : RIGHT HAND SIDE MODIFIED
!
!     TM1 IS DONE AS AM1, BUT WITH TE4 INSTEAD OF TE5
!
      IF(OPTBAN.EQ.3) THEN
        SL1 = 1.D0 / DT
!       LOCALLY LUMPED MASS MATRIX (TIDAL FLATS)
        FORMUL='MSLUMP          '
        CALL MATRIX(TM1,'M=N     ',FORMUL,IELMH,IELMH,
     &              SL1,TE3,S,S,S,S,S,MESH,.TRUE.,TE4)
!       MASS-LUMPING :
        IF(AGGLOH.GT.0.001D0) THEN
          CALL LUMP(T1,TM1,MESH,AGGLOH)
          CALL OM('M=CM    ', M=TM1, C=1.D0-AGGLOH, MESH=MESH)
          CALL OM('M=M+D   ', M=TM1, D=T1, MESH=MESH)
        ENDIF
        CALL MATVEC('X=X+AY  ',CV1,TM1,HN,C,MESH)
      ENDIF
!
!=======================================================================
!
!  TAKES THE IMPLICIT SOURCE TERMS INTO ACCOUNT:
!
      CALL OM('M=M+D   ', M=AM2, D=T3, MESH=MESH)
      CALL OM('M=M+D   ', M=AM3, D=T4, MESH=MESH)
!
!=======================================================================
!
!
!  BOUNDARY TERMS
!
!     PARALLEL MODE : SUBDOMAINS MAY HAVE NO BOUNDARY POINT AT ALL
!
      IF(MESH%NPTFR.GT.0) THEN
!
!  TAKES INTO ACCOUNT THE TERMS SUM(PSI H(N) U(N). N) AT THE BOUNDARY
!  THESE TERMS SHOULD NOT BE TAKEN INTO ACCOUNT ON SOLID BOUNDARIES,
!  HENCE THE USE OF MASK(*, 8) WHICH IS 0 FOR SEGMENTS OF TYPE KLOG.
!
!
      CALL VECTOR(FLBOR,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            1.D0-TETAU,HPROP,S,S,UN,VN,S,
     &            MESH,.TRUE.,MASK%ADR(UNONNEU)%P)
!
!-----------------------------------------------------------------------
!
!  TAKES INTO ACCOUNT THE TERMS  SUM(PSI HN U(N+1) . N ) AT THE BOUNDARY
!
!  DIRICHLET CONDITIONS : U(N+1) = UBOR ; V(N+1) = VBOR
!
!  UDIR : 1 FOR A DIRICHLET SEGMENT FOR U, 0 OTHERWISE
!
      CALL VECTOR(FLBOR,'+','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU,HPROP,S,S,UBOR,VBOR,S,
     &            MESH,.TRUE.,MASK%ADR(UDIR)%P)
!
!  TAKES INTO ACCOUNT THE TERMS  SUM(PSI HN U(N+1) . N ) AT THE BOUNDARY
!
!  FREE EXIT CONDITIONS
!
!  UDDL : 1 FOR A FREE EXIT SEGMENT FOR U, 0 OTHERWISE
!
      CALL VECTOR(FLBOR,'+','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU,HPROP,S,S,UN,VN,S,
     &            MESH,.TRUE.,MASK%ADR(UDDL)%P)
!
!     WITH POROSITY
!
      IF(OPTBAN.EQ.3) THEN
        DO I=1,MESH%NPTFR
          N=MESH%NELBOR%I(I)
!         N MAY BE 0 IN PARALLELISM
          IF(N.GT.0) THEN
            FLBOR%R(I)=FLBOR%R(I)*TE5%R(N)
          ENDIF
        ENDDO
      ENDIF
!
!     END OF: IF(MESH%NPTFR.GT.0) THEN
      ENDIF
!
!     IMPOSING EQUALITY OF FLUXES ON EITHER SIDE OF A WEIR
!     HERE ALL PROCESSORS DO ALL WEIRS...
!
      IF(NWEIRS.GT.0.AND.TYPSEUIL.EQ.1) THEN
        DO N=1,NWEIRS
          DO I=1,NPSING%I(N)
            I1=NDGA1%ADR(N)%P%I(I)
            I2=NDGB1%ADR(N)%P%I(I)
            IF(I1.GT.0) THEN
              FL1=FLBOR%R(I1)
            ELSE
              FL1=0.D0
            ENDIF
            IF(I2.GT.0) THEN
              FL2=FLBOR%R(I2)
            ELSE
              FL2=0.D0
            ENDIF
!           IN PARALLEL ASSEMBLED FLUXES
            IF(NCSIZE.GT.1) THEN
              FL1=P_SUM(FL1)
              FL2=P_SUM(FL2)
            ENDIF
!           MULTIPLICATION FACTOR SO THAT BOTH FLUXES ARE EQUAL
            IF(ABS(FL1).GT.1.D-4.AND.ABS(FL2).GT.1.D-4) THEN
              IF(I1.GT.0) FLBOR%R(I1)= FLBOR%R(I1)*(FL1-FL2)*0.5D0/FL1
              IF(I2.GT.0) FLBOR%R(I2)=-FLBOR%R(I2)*(FL1-FL2)*0.5D0/FL2
            ELSE
              IF(I1.GT.0) FLBOR%R(I1)=0.D0
              IF(I2.GT.0) FLBOR%R(I2)=0.D0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!     BOUNDARY TERMS IN THE RIGHT HAND SIDE
!
      IF(MESH%NPTFR.GT.0) THEN
        CALL OSDB( 'X=X-Y   ' , CV1 , FLBOR , FLBOR , C , MESH )
      ENDIF
!
! END OF BOUNDARY TERMS
!
!-----------------------------------------------------------------------
!
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
!
!       TAKES BOUSSINESQ TERMS INTO ACCOUNT
!
        CALL ROTNE0(MESH,CM1,
     &              AM2,A23,A32,AM3,CV2,CV3,UN,VN,H0,MSK,MASKEL,S,DT)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  GRADIENT MATRICES
!
!     NOT USED WITH WAVE EQUATION
      IF(SOLSYS.EQ.1) THEN
!
      CALL MATRIX(CM1,'M=N     ','MATGRA         X',IELMU,IELMH,
     &            TETAH*GRAV,S,S,S,S,S,S,MESH,MSK,MASKEL)
      CALL MATRIX(CM2,'M=N     ','MATGRA         Y',IELMU,IELMH,
     &            TETAH*GRAV,S,S,S,S,S,S,MESH,MSK,MASKEL)
!
      ENDIF
!
!=======================================================================
!
! INITIAL GUESS
!
! FOR NOW U AND V ARE NOT MODIFIED, WHICH MEANS THAT U AND V STILL
! HOLD THE RESULT OF THE LAST SOLVED SYSTEM
!
      IF(IORDRH.EQ.0) THEN
!
        CALL OS( 'X=0     ' , X=DH )
!
      ELSEIF(IORDRH.EQ.1) THEN
!
        IF(LT.EQ.1.AND.ISOUSI.EQ.1
     &            .AND.(DEBU.OR.TROUVE(36).NE.1)) THEN
          CALL OS( 'X=0     ' , X=DH )
        ENDIF
!
      ELSEIF(IORDRH.EQ.2) THEN
!
        IF(LT.EQ.1.AND.ISOUSI.EQ.1
     &            .AND.(DEBU.OR.TROUVE(36).NE.1)) THEN
          CALL OS( 'X=0     ' , X=DH )
        ENDIF
        IF(LT.EQ.1.AND.ISOUSI.EQ.1
     &            .AND.(DEBU.OR.TROUVE(39).NE.1)) THEN
          CALL OS( 'X=0     ' , X=DHN)
        ENDIF
        IF(LT.LE.2.AND..NOT.DEBU.AND.TROUVE(36).EQ.1) THEN
          CALL OS('X=CX    ' , X=DH , C=2.D0 )
        ENDIF
        IF (LT.GT.2) CALL OS('X=CX    ' , X=DH , C=2.D0 )
        CALL OS( 'X=X-Y   ' , X=DH , Y=DHN)
!       STORES DH(N) IN DH(N-1)
        CALL OS( 'X=X+Y   ' , X=DHN , Y=DH)
        CALL OS( 'X=CX    ' , X=DHN , C=0.5D0 )
!
      ELSE
!
        WRITE(LU,31) IORDRH
31      FORMAT(1X,'PROPAG : IORDRH=',1I6,' VALUE OUT OF RANGE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
      IF(IORDRU.EQ.0) THEN
!
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
!
      ELSEIF(IORDRU.EQ.1) THEN
!
!       U = UN AND V = VN ALREADY DONE
!
      ELSEIF(IORDRU.EQ.2) THEN
!
        IF(LT.EQ.1.AND.ISOUSI.EQ.1) THEN
          IF(DEBU.OR.TROUVE(37).NE.1) THEN
            CALL OS( 'X=0     ' , X=DU )
          ENDIF
          IF(DEBU.OR.TROUVE(38).NE.1) THEN
            CALL OS( 'X=0     ' , X=DV )
          ENDIF
        ENDIF
        IF(ISOUSI.EQ.1) THEN
          CALL OS( 'X=Y+Z   ' , X=U , Y=UN , Z=DU )
          CALL OS( 'X=Y+Z   ' , X=V , Y=VN , Z=DV )
        ENDIF
!
      ELSE
!
        WRITE(LU,33) IORDRU
33      FORMAT(1X,'PROPAG : IORDRU=',1I6,' VALUE OUT OF RANGE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!=======================================================================
!
      IF(SOLSYS.EQ.2) THEN
!       COMPENSATION OF THE MATRIX DIAGONALS
        IF(NCSIZE.GT.1) THEN
          IF (MODASS .EQ. 1) THEN
            CALL PARCOM(AM2%D,2,MESH)
            CALL PARCOM(AM3%D,2,MESH)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL PARCOM_COMP(AM3%D,AM3%D%E,2,MESH)
            CALL PARCOM_COMP(AM2%D,AM2%D%E,2,MESH)
          ENDIF
        ENDIF
        IF (MODASS .EQ.3)THEN
          AM2%D%R=AM2%D%R+AM2%D%E
          AM2%D%E=0.D0
          AM3%D%R=AM3%D%R+AM3%D%E
          AM3%D%E=0.D0
        ENDIF
!       INVERSION OF AM2%D AND AM3%D (WILL BE USED AGAIN AT A LATER STAGE)
        CALL OS('X=1/Y   ', X=AM2%D, Y=AM2%D,
     &                      IOPT=2,INFINI=0.D0,ZERO=1.D-6)
        CALL OS('X=1/Y   ', X=AM3%D, Y=AM3%D,
     &                      IOPT=2,INFINI=0.D0,ZERO=1.D-6)
!
!       ADDS THE "DIFFUSION" MATRIX TO AM1
!
!       HERE AM2%D HAS ALREADY BEEN INVERSED
        IF(IELMH.EQ.IELMU) THEN
!         WANT TO DIVIDE BY (1/DT + FROT) WHICH IS IN AM2%D EXCEPT
!         THAT IT IS PROJECTED ON THE BASES (IN AM2%D); THEREFORE HAS
!         TO MULTIPLY BY THE MASS OF THE BASES
          CALL OS('X=CYZ   ',X=DM1,Y=V2DPAR,Z=AM2%D,C=-TETAU*TETAH*GRAV)
        ELSE
!         TAKE HERE THE MASS OF THE BASES FOR U
          CALL VECTOR(T4,'=','MASBAS          ',IELMU,
     &                1.D0,S,S,S,S,S,S,MESH,.FALSE.,TE3)
          IF(NCSIZE.GT.1) CALL PARCOM(T4,2,MESH)
          CALL OS('X=CYZ   ',X=DM1,Y=T4,Z=AM2%D,C=-TETAU*TETAH*GRAV)
          CALL CHGDIS(DM1,IELMU,IELMH,MESH)
        ENDIF
!
        CALL MATRIX(AM1,'M=M+N   ','MATDIFUV        ',IELMH,IELMH,
     &              -1.D0,S,S,S,DM1,HPROP,S,MESH,MSK,MASKEL)
!
!       NEW SECOND MEMBER CV1
!
        IF(ABS(TETAZCOMP-1.D0).LT.1.D-6) THEN
          CALL OS( 'X=YZ    ' , X=T2 , Y=CV2 , Z=AM2%D )
          CALL OS( 'X=YZ    ' , X=T3 , Y=CV3 , Z=AM3%D )
        ELSE
          CALL OS( 'X=Y     ' , X=T4 , Y=CV2 )
          CALL OS( 'X=Y     ' , X=T5 , Y=CV3 )
!         FREE SURFACE GRADIENT
          IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
            CALL VECTOR(T4,'+','GRADF          X',IELMU,
     &      (1.D0-TETAZCOMP)*GRAV,ZFLATS,S,S,S,S,S,MESH,MSK,MASKEL)
            CALL VECTOR(T5,'+','GRADF          Y',IELMU,
     &      (1.D0-TETAZCOMP)*GRAV,ZFLATS,S,S,S,S,S,MESH,MSK,MASKEL)
          ELSE
            CALL VECTOR(T4,'+','GRADF          X',IELMU,
     &      (1.D0-TETAZCOMP)*GRAV,T8,S,S,S,S,S,MESH,MSK,MASKEL)
            CALL VECTOR(T5,'+','GRADF          Y',IELMU,
     &      (1.D0-TETAZCOMP)*GRAV,T8,S,S,S,S,S,MESH,MSK,MASKEL)
          ENDIF
          CALL OS( 'X=YZ    ' , X=T2 , Y=T4 , Z=AM2%D )
          CALL OS( 'X=YZ    ' , X=T3 , Y=T5 , Z=AM3%D )
        ENDIF
!       COMPENSATION OF VECTORS
        IF(NCSIZE.GT.1) THEN
          IF (MODASS .EQ. 1) THEN
            CALL PARCOM(T2,2,MESH)
            CALL PARCOM(T3,2,MESH)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL PARCOM_COMP(T2,T2%E,2,MESH)
            CALL PARCOM_COMP(T3,T3%E,2,MESH)
          ENDIF
        ENDIF
        IF (MODASS .EQ.3)THEN
          T2%R=T2%R+T2%E
          T2%E=0.D0
          T3%R=T3%R+T3%E
          T3%E=0.D0
        ENDIF
!
!       TAKES THE BOUNDARY CONDITIONS INTO ACCOUNT
!       ERROR IN GRAD(DH)
        DO I=1,MESH%NPTFR
          IF(LIMPRO%I(I+DIMLIM).EQ.KDIR) THEN
            T2%R(MESH%NBOR%I(I)) = UBOR%R(I)
          ENDIF
          IF(LIMPRO%I(I+2*DIMLIM).EQ.KDIR) THEN
            T3%R(MESH%NBOR%I(I)) = VBOR%R(I)
          ENDIF
        ENDDO
!
!       REMEMBER THAT COEFFICIENT TETAU IS IN BM1 AND BM2
!       AND THAT SUPG UPWINDING IS ALSO IN BM1 AND BM2
!       OTHERWISE COULD BE INCLUDED IN HUGRADP BELOW
        CALL MATVEC('X=X-AY  ',CV1,BM1,T2,C,MESH)
        CALL MATVEC('X=X-AY  ',CV1,BM2,T3,C,MESH)
!
        IF(ABS(TETAZCOMP-1.D0).GT.1.D-6) THEN
          FORMUL='HUGRADP3        '
!                        3: U AND V, HERE T2 AND T3 NOT TAKEN
          CALL OS('X=CY    ',X=T1,Y=DM1,
     &                       C=(1.D0-TETAZCOMP)/TETAH/TETAU)
          IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
            CALL VECTOR(CV1,'+',FORMUL,IELMH,TETAU,
     &                  HPROP,T1,ZFLATS,T2,T3,T3,MESH,MSK,MASKEL)
          ELSE
            CALL VECTOR(CV1,'+',FORMUL,IELMH,TETAU,
     &                  HPROP,T1,T8    ,T2,T3,T3,MESH,MSK,MASKEL)
          ENDIF
        ENDIF
!
        CALL OS('X=CY    ',X=UDEL,Y=T2,C=TETAU)
        CALL OS('X=CY    ',X=VDEL,Y=T3,C=TETAU)
        CALL OS('X=X+CY  ',X=UDEL,Y=UN,C=1.D0-TETAU)
        CALL OS('X=X+CY  ',X=VDEL,Y=VN,C=1.D0-TETAU)
!
      ENDIF
!
!=======================================================================
!
!     AT THIS STAGE, A23 AND A32 EQUAL 0
!     THE MATRICES HAVE A VALUE ONLY AFTER A DIAGONAL-BLOCK
!     PRECONDITIONING OU WITH BOUSSINESQ
!
      IF(EQUA(1:10).NE.'BOUSSINESQ') THEN
        A23%TYPDIA='0'
        A32%TYPDIA='0'
        A23%TYPEXT='0'
        A32%TYPEXT='0'
        A23%STOX=1
        A32%STOX=1
      ENDIF
!
!=======================================================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     IN ADJOINT MODE : THE SYSTEM IS NOT SOLVED, RETURN HERE
!
!
      IF(ADJO) RETURN
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!=======================================================================
!
!   DIRICHLET BOUNDARY CONDITIONS:
!
      IF(SOLSYS.EQ.1) THEN
!
        IF(CORCON.AND.NFRLIQ.GT.0) THEN
!
!         SAVES THE CONTINUITY EQUATION
          CALL OM('M=N     ', M=TM1 , N=AM1, MESH=MESH)
          CALL OM('M=N     ', M=BM1S, N=BM1, MESH=MESH)
          CALL OM('M=N     ', M=BM2S, N=BM2, MESH=MESH)
          CALL OS( 'X=Y     ' ,X=CV1S,Y=CV1)
!
        ENDIF
!
        CALL DIRICH(UNK,MAT,RHS,DIRBOR,LIMPRO%I,TB,MESH,KDIR,MSK,MASKPT)
!
      ENDIF
!
!  SOLVES THE OBTAINED SYSTEM:
!
!+++++++++++++++++++++++++++++++++++
!  SPECIAL PRECONDITIONING H-U     +
!+++++++++++++++++++++++++++++++++++
!
      IF(PRECCU) THEN
!
!     PREPARES THE DIAGONALS FOR PRECONDITIONING:
!
!     HTILD: D1 (MUST BE SIMPLIFIED BY GRAV THERE? )
      CALL OS('X=+(Y,C)' , X=HTILD , Y=HN , C=0.D0 )
      CALL OS('X=CX    ' , X=HTILD , C=4.D0/GRAV )
      CALL OS('X=SQR(Y)' , X=HTILD , Y=HTILD )
      CALL OS('X=+(Y,C)' , X=HTILD , Y=HTILD , C=2.D0/GRAV )
!     T1: D2 (NOT KEPT)
      CALL OS('X=1/Y   ' , X=T1 , Y=HTILD )
      CALL OS('X=CX    ' , X=T1 , C=4.D0*TETAH/TETAU )
!
!     MODIFIES THE SECOND MEMBER
!
      CALL OS('X=XY    ' , X=CV1 , Y=T1 )
!
!     MODIFIES THE VARIABLE DH
!
      DH%DIM1=MESH%NPOIN
      HTILD%DIM1=MESH%NPOIN
      CALL OS('X=Y/Z   ' , X=DH , Y=DH , Z=HTILD )
!
!     PRECONDITIONING FOR AM1
!
      IF(AM1%TYPEXT.EQ.'S') CALL OM('M=X(M)  ', M=AM1, MESH=MESH)
      CALL OM('M=DM    ', M=AM1, D=T1   , MESH=MESH)
      CALL OM('M=MD    ', M=AM1, D=HTILD, MESH=MESH)
!
!     PRECONDITIONING FOR BM1 AND BM2 :
!
      CALL OM('M=DM    ', M=BM1, D=T1, MESH=MESH)
      CALL OM('M=DM    ', M=BM2, D=T1, MESH=MESH)
!
!     PRECONDITIONING FOR CM1 AND CM2 :
!
      CALL OM('M=MD    ', M=CM1, D=HTILD, MESH=MESH)
      CALL OM('M=MD    ', M=CM2, D=HTILD, MESH=MESH)
!
      ENDIF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  END OF SPECIAL PRECONDITIONING H-U                               +
!  EXCEPT FOR RECOVERY OF THE DH VARIABLE (SEE AFTER CALL TO SOLV09)+
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
      IF(SOLSYS.EQ.1) THEN
!
!       CLASSICAL METHOD
!
!       CASE OF THE BLOCK-DIAGONAL PRECONDITIONING
!       A23 AND A32 WILL BE USED, THEY ARE INITIALISED.
!       ALREADY DONE WITH COUPLED BOUSSINESQ
        IF(EQUA(1:10).NE.'BOUSSINESQ') THEN
          IF(3*(SLVPRO%PRECON/3).EQ.SLVPRO%PRECON) THEN
            A23%TYPDIA='Q'
            A32%TYPDIA='Q'
            A23%TYPEXT='Q'
            A32%TYPEXT='Q'
            CALL OM('M=0     ', M=A23, MESH=MESH)
            CALL OM('M=0     ', M=A32, MESH=MESH)
            IF (AM2%TYPEXT.EQ.'S') THEN
              CALL OM('M=X(M)  ', M=AM2, MESH=MESH)
            ENDIF
            IF (AM3%TYPEXT.EQ.'S') THEN
              CALL OM('M=X(M)  ', M=AM3, MESH=MESH)
            ENDIF
          ENDIF
        ENDIF
!
!       FOR CHECKING PROBLEMS IN PARALLEL
!       NON ASSEMBLED VECTORS MUST BE ASSEMBLED FIRST
!       AND THEIR DOT PRODUCT (THAT SHOULD BE EQUAL IN SCALAR AND PARALLEL)
!       COMPUTED WITH P_DOTS. ALL THIS IS DONE BY APPDOTS
!
!       CALL APPDOTS(AM1%D,MESH)
!       CALL APPDOTS(AM2%D,MESH)
!       CALL APPDOTS(AM3%D,MESH)
!       CALL APPDOTS(BM1%D,MESH)
!       CALL APPDOTS(BM2%D,MESH)
!       CALL APPDOTS(CM1%D,MESH)
!       CALL APPDOTS(CM2%D,MESH)
!       CALL APPDOTS(CV1,MESH)
!       CALL APPDOTS(CV2,MESH)
!       CALL APPDOTS(CV3,MESH)
!
#if defined COMPAD
        CALL AD_SOLVE(UNK,MAT,RHS,TB,SLVPRO,INFOGR,MESH,TM1)
#else
        CALL SOLVE(UNK,MAT,RHS,TB,SLVPRO,INFOGR,MESH,TM1)
#endif
!
      ELSEIF(SOLSYS.EQ.2) THEN
!
!       GENERALISED WAVE EQUATION
!
!       SYSTEM IN H
!
!       STORES AM1 IN BM1 AND CV1 IN BM2%D
!
        IF(CORCON.AND.NFRLIQ.GT.0) THEN
          CALL OM('M=N     ', M=BM1, N=AM1, MESH=MESH)
          CALL OS('X=Y     ', X=BM2%D, Y=CV1)
        ENDIF
!
        CALL DIRICH(DH,AM1,CV1,HBOR,LIMPRO%I,TB,MESH,KDIR,MSK,MASKPT)
#if defined COMPAD
        CALL AD_SOLVE(DH,AM1,CV1,TB,SLVPRO,INFOGR,MESH,TM1)
#else
        CALL SOLVE(DH,AM1,CV1,TB,SLVPRO,INFOGR,MESH,TM1)
#endif
!
        NELEM=MESH%NELEM
        DO IELEM=1,NELEM
          ZCONV%R(IELEM        )=DH%R(IKLE(IELEM,1))
          ZCONV%R(IELEM+  NELEM)=DH%R(IKLE(IELEM,2))
          ZCONV%R(IELEM+2*NELEM)=DH%R(IKLE(IELEM,3))
        ENDDO
        IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
          C=(1.D0-TETAZCOMP)/TETAH
          IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
!           FREE SURFACE PIECE-WISE LINEAR IN ZFLATS
            CALL OS('X=X+CY  ',X=ZCONV,Y=ZFLATS,C=C)
          ELSE
!           FREE SURFACE LINEAR
            DO IELEM=1,NELEM
              I1=IKLE(IELEM,1)
              I2=IKLE(IELEM,2)
              I3=IKLE(IELEM,3)
              ZCONV%R(IELEM        )=ZCONV%R(IELEM        )+
     &        C*(T8%R(I1))
              ZCONV%R(IELEM+  NELEM)=ZCONV%R(IELEM+  NELEM)+
     &        C*(T8%R(I2))
              ZCONV%R(IELEM+2*NELEM)=ZCONV%R(IELEM+2*NELEM)+
     &        C*(T8%R(I3))
            ENDDO
          ENDIF
        ENDIF
!
!       SYSTEMS IN U AND V
!
        CALL VECTOR(CV2,'+','GRADF          X',IELMU,
     &              -GRAV*TETAH,DH,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(CV3,'+','GRADF          Y',IELMU,
     &              -GRAV*TETAH,DH,S,S,S,S,S,MESH,MSK,MASKEL)
!       COMPENSATION OF VECTORS
        IF(NCSIZE.GT.1) THEN
          IF (MODASS .EQ. 1) THEN
            CALL PARCOM(CV2,2,MESH)
            CALL PARCOM(CV3,2,MESH)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL PARCOM_COMP(CV2,CV2%E,2,MESH)
            CALL PARCOM_COMP(CV3,CV3%E,2,MESH)
          ENDIF
        ENDIF
        IF (MODASS .EQ.3)THEN
          CV2%R=CV2%R+CV2%E
          CV2%E=0.D0
          CV3%R=CV3%R+CV3%E
          CV3%E=0.D0
        ENDIF
!                                      AM2%D AND AM3%D ALREADY INVERSED
        CALL OS('X=YZ    ',X=U,Y=CV2,Z=AM2%D)
        CALL OS('X=YZ    ',X=V,Y=CV3,Z=AM3%D)
!
        DO I=1,MESH%NPTFR
          IF(LIMPRO%I(I+DIMLIM).EQ.KDIR) THEN
            U%R(MESH%NBOR%I(I)) = UBOR%R(I)
          ENDIF
          IF(LIMPRO%I(I+2*DIMLIM).EQ.KDIR) THEN
            V%R(MESH%NBOR%I(I)) = VBOR%R(I)
          ENDIF
        ENDDO
!
!       FINAL CORRECTION OF BOUNDARY FLUXES FOR ELEVATION IMPOSED
!       BOUNDARIES (TO SOLVE THE CONTINUITY EQUATION)
!
        IF(CORCON.AND.NFRLIQ.GT.0) THEN
          CALL MATVEC('X=X+CAY ',BM2%D,BM1,DH,-1.D0,MESH)
          DO I=1,MESH%NPTFR
            IF(LIMPRO%I(I).EQ.KDIR) THEN
              FLBOR%R(I)=FLBOR%R(I)+BM2%D%R(MESH%NBOR%I(I))
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! SPECIAL PRECONDITIONING H-U : RECOVERY OF THE DH VARIABLE
!
      IF(PRECCU) CALL OS('X=XY    ' , X=DH , Y=HTILD )
!
      IF(CORCON.AND.SOLSYS.EQ.1.AND.NFRLIQ.GT.0) THEN
!
!       FINAL CORRECTION OF BOUNDARY FLUXES FOR ELEVATION IMPOSED
!       BOUNDARIES (TO SOLVE THE CONTINUITY EQUATION)
!
        CALL MATVEC('X=X+CAY ',CV1S,TM1,DH,-1.D0,MESH)
        CALL MATVEC('X=X+CAY ',CV1S,BM1S,U,-1.D0,MESH)
        CALL MATVEC('X=X+CAY ',CV1S,BM2S,V,-1.D0,MESH)
        DO I=1,MESH%NPTFR
          IF(LIMPRO%I(I).EQ.KDIR) THEN
            FLBOR%R(I)=FLBOR%R(I)+CV1S%R(MESH%NBOR%I(I))
          ENDIF
        ENDDO
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  FROM DH TO H
!
      CALL OS( 'X=Y+Z   ' , X=H , Y=DH , Z=HN )
!
!  HBOR = HBOR + HN
!  HBOR IS USED AGAIN IN SUB-ITERATIONS
!
      CALL OSBD( 'X=X+Y   ' , HBOR , HN , S , C , MESH )
!
!  STORES THE RELATIVE CHANGE IN SPEEDS
!
      IF(IORDRU.EQ.2) THEN
        CALL OS( 'X=Y-Z   ' , X=DU , Y=U , Z=UN )
        CALL OS( 'X=Y-Z   ' , X=DV , Y=V , Z=VN )
      ENDIF
!
!  COMPATIBLE VELOCITY FIELD IN CONTINUITY EQUATION
!
      IF(SOLSYS.EQ.1) THEN
        CALL OS ('X=CY    ',X=UDEL,Y=U ,C=     TETAU)
        CALL OS ('X=CY    ',X=VDEL,Y=V ,C=     TETAU)
        CALL OS ('X=X+CY  ',X=UDEL,Y=UN,C=1.D0-TETAU)
        CALL OS ('X=X+CY  ',X=VDEL,Y=VN,C=1.D0-TETAU)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

