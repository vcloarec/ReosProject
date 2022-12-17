!                   *****************
                    SUBROUTINE CVDFTR
!                   *****************
!
     &(F,FTILD,FN,FSCEXP,DIFT,ICONVF,CONV,
     & H,HN,HPROP,UCONV,VCONV,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,AM1,AM2,
     & ZF,FBOR,AFBOR,BFBOR,LIMTRA,MASKTR,MESH,TB,
     & T1,T2,T4,T10,TE1,TE2,TE3,KDIR,KDDL,DT,ENTET,
     & TETAT,AGGLOT,INFOGT,BILAN,OPTADV,
     & ISOUSI,OPDTRA,OPTBAN,MSK,MASKEL,MASKPT,MBOR,
     & S,MASSOU,OPTSOU,SLVTRA,FLBOR,VOLU2D,V2DPAR,UNSV2D,OPTVF,FLBORTRA,
     & FLULIM,YAFLULIM,FLULIMEBE,YAFLULIMEBE,DIRFLU,RAIN,PLUIE,TRAIN,
     & GIVEN_FLUX,FLUX_GIVEN,MAXADV,TB2,NCO_DIST,NSP_DIST)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    DIFFUSION, ADVECTION AND SOURCE TERMS FOR A TRACER.
!code
!+  THE EQUATION SOLVED IS :
!+
!+
!+          N+1                                            TILD
!+         F           1                                  F   + DT*SM
!+      ---------  -  ---  DIV ( H VISC * GRAD ( F   )) = ____________
!+         DT          H                                      DT
!+
!+                                                      N+1
!+                                  + SOURCES  + SMI * F
!+                                                     ___
!+                                                     H
!+
!+     WITH :    N+1  TILD   N
!+              F   ,F     ,F  =    DIFFUSED FUNCTION
!+              VISC           =    TURBULENT VISCOSITY
!+              SM             =    SECOND MEMBER (SOURCE TERMS)
!+              TETAT          =    IMPLICITATION COEFFICIENT
!+              DT             =    TIME STEP
!+                                         N+1              N
!+              F              =    TETAT F  + (1-TETAT) * F
!+              SMI            =    IMPLICIT SOURCE TERM
!+
!+
!+                    TILD       N
!+     DISTINGUISHES F     FROM F   IN CASE A FRACTIONAL STEPS METHOD
!+
!+     HAD BEEN PREVIOUSLY PERFORMED (ADVECTION FOR EXAMPLE) GIVING
!+
!+      TILD       N
!+     F     FROM F
!+
!+-----------------------------------------------------------------------
!+
!+      BOUNDARY CONDITIONS :
!+
!+      ==>   NEUMANN CONDITION
!+
!+      VISC DF/DN = AFBOR . F  +  BFBOR
!+
!+
!+      ==>   DIRICHLET CONDITION
!+
!+            TREATED BY MODIFICATION OF THE EQUATIONS IN THE
!+            SUBROUTINE DIRICH
!
!note     JMH : W IS NOT USED.
!
!warning  MATDIF DOES NOT GIVE THE DIFFUSION MATRIX. IT MISSES THE
!+            BOUNDARY TERMS AND THERE IS A MINUS SIGN WHICH IS TAKEN
!+            INTO ACCOUNT HERE
!warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!+            WITH NO FRICTION
!warning  BEWARE DISCRETISATION OF VISC
!
!history  JMH
!+        27/02/2009
!+
!+   CALLS CVTFVF_POS, OPTION 14
!
!history  J-M HERVOUET (LNHE)     ; C MOULIN (LNH)
!+        29/12/2009
!+        V6P0
!+
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
!+
!history  J-M HERVOUET (LNHE)
!+        17/07/2012
!+        V6P2
!+   Mass balance programmed in case of implicit source term.
!+   Arguments GIVEN_FLUX and FLUX_GIVEN added.
!
!history  J-M HERVOUET (LNHE)
!+        09/04/2013
!+        V6P3
!+   DIMGLO=MESH%GLOSEG%DIM1 used in call to CVTRVF_POS. Strangely
!+   avoids an "array temporary created" with Intel compiler.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        01/04/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!+   OPTSUP changed into OPTADV for a more general use.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        16/05/2014
!+        V7P0
!+   Argument VOLU2D added for calling CVTRVF.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/09/2014
!+        V7P0
!+   Checking mass in case of implicit source terms SMI depends on the
!+   advection scheme, a section was added for distributive schemes.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        20/08/2015
!+        V7P1
!+   Adaptation to new subroutine cvtrvf with locally implicit
!+   predictor-corrector distributive schemes.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        20/11/2015
!+        V7P1
!+   Implicitation on diffusion forced to 1. with parameter TETATD. It
!+   should be a keyword but there is no reason so far to do an explicit
!+   diffusion.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        03/06/2016
!+        V7P2
!+   Adapting to new scheme ERIA, ADV_PSI_TF = 15. Computation of HPROP
!+   removed, HPROP must now be given and is not rebuilt with TETA,
!+   which was a mistake. Adding OPTADV in cvtrvf_pos.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        06/09/2016
!+        V7P2
!+   Splitting cvtrvf_pos into cvtrvf_nerd and cvtrvf_eria + cleaning.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AFBOR,BFBOR    |-->| COEFFICIENTS OF NEUMANN CONDITION
!|                |   | VISC*DF/DN = AFBOR*F + BFBOR
!|                |   | GIVEN FOR EVERY BOUNDARY POINT
!| AGGLOT         |-->| MASS-LUMPING COEFFICIENT FOR T.
!| AM1            |<->| MATRIX.
!| AM2            |<->| MATRIX.
!| BILAN          |-->| LOGICAL, IF YES A BALANCE OF MASS EXCHANGES HAS
!|                |   | TO BE DONE.
!| CONV           |-->| IF YES ADVECTION OF F
!| DIFT           |-->| IF YES, DIFFUSION IS DONE
!| DIRFLU         |-->| 1: PRIORITY TO DIRICHLET VALUES (CALL DIRICH DONE)
!|                |   | 2: PRIORITY TO FLUXES (CALL DIRICH NOT DONE)
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS
!|                |   | CONSERVATION.
!| F              |<--| F AT TIME T(N+1)
!| FBOR           |-->| DIRICHLET CONDITIONS ON F.
!| FLBOR          |-->| FLUXES AT BOUNDARIES
!| FLBORTRA       |<->| TRACER FLUXES AT BOUNDARIES
!| FLUX_GIVEN     |-->| IF GIVEN_FLUX=YES, THE FLUX IS GIVEN IN
!|                |   | GIVEN_FLUX
!| FN             |-->| F AT TIME T(N)
!| FSCEXP         |-->| EXPLICIT PART OF THE SOURCE TERM
!|                |   | EQUAL TO ZERO EVERYWHERE BUT ON SOURCES
!|                |   | WHERE THERE IS FSCE - (1-TETAT) FN
!|                |   | SEE DIFSOU
!| FTILD          |-->| F AFTER ADVECTION
!| GIVEN_FLUX     |-->| IF GIVEN_FLUX=YES, THE FLUX IS GIVEN IN
!|                |   | GIVEN_FLUX AND WILL NOT BE COMPUTED HERE
!| HPROP          |-->| WORK ARRAY
!| ICONVF         |-->| OPTION FOR ADVECTION TERMS
!|                |   | ICONVF = 1 : CHARACTERISTICS.
!|                |   | ICONVF = 2 : S.U.P.G.
!|                |   | ICONVF = 3 : CONSERVATIVE FINITE VOLUMES
!|                |   | ICONVF = 4 : IDEM
!|                |   | ICONVF = 6 : NON CONSERVATIVE PSI SCHEME.
!|                |   | ICONVF = 7 : NON CONSERVATIVE N SCHEME.
!|                |   | ICONVF =13 : EDGE BY EDGE FORM OF 3
!|                |   | ICONVF =14 : IDEM
!| INFOGT         |-->| LOGICAL, IF YES INFORMATION ON SOLVER WILL BE
!|                |   | PRINTED.
!| ISOUSI         |-->| SUB-ITERATION NUMBER
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR INFLOW POINT
!| LIMTRA         |-->| BOUNDARY CONDITIONS ON BOUNDARY POINTS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!| MASSOU         |-->| MASS OF TRACER ADDED BY SOURCE TERM
!|                |   | SEE DIFSOU
!| MAXADV         |-->| MAXIMUM NUMBER OF ITERATIONS OF ADVECTION SCHEMES
!| MBOR           |-->| BOUNDARY MATRIX
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NCO_DIST       |-->| NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES
!| NSP_DIST       |-->| NUMBER OF SUB-STEPS OF DISTRIBUTIVE SCHEMES
!| OPDTRA         |-->| OPTION FOR THE DIFFUSION OF TRACERS
!| OPTADV         |-->| ADVECTION SCHEME OPTION, THE MEANING DEPENDS ON
!|                |   | THE SCHEME. IF SCHEME IS SUPG:
!|                |   | 0: NO SUPG UPWIND
!|                |   | 1: CLASSIC SUPG
!|                |   | 2: MODIFIED SUPG
!|                |   | IF SCHEME IS PSI:
!|                |   | 1: EXPLICIT
!|                |   | 2: PREDICTOR-CORRECTOR
!|                |   | 3: SECOND ORDER PREDICTOR-CORRECTOR
!|                |   | 4: LOCALLY IMPLICIT
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!|                |   | 1:NORMAL   2:WITH MASKING
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| OPTVF          |-->| OPTIONS FOR FINITE VOLUMES (SEE CVTRVF)
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| S              |-->| VOID STRUCTURE
!| SLVTRA         |-->| SOLVER CONFIGURATION (SLVCFG) STRUCTURE
!|                |   | CONTAINING DATA FOR CALLING SOLVE
!| SM             |-->| SOURCE TERMS.
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SMI            |-->| IMPLICIT SOURCE TERM
!| SOLSYS         |-->| 1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| TB             |<->| BLOCK OF WORK BIEF_OBJ STRUCTURES (CONTAINS T1,...)
!| TB2            |<->| SECOND BLOCK OF WORK BIEF_OBJ STRUCTURES
!|                |   | FOR IMPLICIT SCHEMES ONLY
!| TE1,TE2,TE3    |<->| WORK BIEF_OBJ STRUCTURE FOR ELEMENTS
!| TETAT          |-->| IMPLICITATION COEFFICIENT OF ADVECTION
!| TRAIN          |-->| VALUE OF TRACER IN THE RAIN
!| UCONV,VCONV    |-->| ADVECTION VELOCITY FIELD
!| UNSV2D         |-->| =1/V2DPAR
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS (NOT ASSEMBLED IN PARALLEL)
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS (ASSEMBLED IN PARALLEL)
!| VISC           |-->| VISCOSITY COEFFICIENTS ALONG X,Y AND Z .
!|                |   | IF P0 : PER ELEMENT
!|                |   | IF P1 : PER POINT
!| VISC_S         |<->| WORK ARRAY FOR SAVING VISC
!| YASMH          |-->| IF YES SMH TAKEN INTO ACCOUNT
!| YASMI          |-->| IF YES SMI TAKEN INTO ACCOUNT
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| ZF             |-->| BOTTOM ELEVATION.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CVDFTR => CVDFTR
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC, ONLY : ADV_CAR,ADV_SUP,ADV_NSC,ADV_PSI,
     &    ADV_LPO,ADV_NSC_TF,ADV_PSI_TF,ADV_LPO_TF,ADV_VOID
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: ICONVF,ISOUSI,OPTADV,OPDTRA
      INTEGER, INTENT(IN)           :: OPTBAN,OPTSOU,KDIR,SOLSYS
      INTEGER, INTENT(IN)           :: KDDL,OPTVF,DIRFLU,MAXADV
      INTEGER, INTENT(IN)           :: NCO_DIST,NSP_DIST
      DOUBLE PRECISION, INTENT(IN)  :: TETAT,AGGLOT,DT,TRAIN
      DOUBLE PRECISION, INTENT(INOUT)  :: MASSOU
      LOGICAL, INTENT(IN)           :: INFOGT,BILAN,CONV,YASMH,RAIN
      LOGICAL, INTENT(IN)           :: DIFT,MSK,ENTET,YASMI,YAFLULIM
      LOGICAL, INTENT(IN)           :: FLUX_GIVEN,YAFLULIMEBE
      TYPE(SLVCFG), INTENT(INOUT)   :: SLVTRA
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,MASKPT,H,HN,AFBOR,BFBOR
      TYPE(BIEF_OBJ), INTENT(IN), TARGET :: HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F,SM,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,ZF
      TYPE(BIEF_OBJ), INTENT(IN)    :: FTILD,FN,SMI,FLULIM,PLUIE
      TYPE(BIEF_OBJ), INTENT(IN)    :: FLULIMEBE
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SMH,FBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TE1,TE2,TE3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T4
      TYPE(BIEF_OBJ), INTENT(INOUT), TARGET :: T10
      TYPE(BIEF_OBJ), INTENT(IN)    :: FSCEXP,DM1,ZCONV
      TYPE(BIEF_OBJ), INTENT(IN)    :: S,V2DPAR,UNSV2D,VOLU2D
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLBOR,LIMTRA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VISC_S,VISC
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AM1,AM2,MBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TB,TB2
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKTR,GIVEN_FLUX
      TYPE(BIEF_MESH)               :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C,TETATD
!
      INTEGER IELMF,IELMH,IELMS,MSKNEU,I,N,IOPT,DIMGLO
!
      LOGICAL MSQ,FV_SCHEME
!
      CHARACTER(LEN=16) FORMUL
!
      TYPE(BIEF_OBJ), POINTER :: HPR
!
!-----------------------------------------------------------------------
!
!     HARDCODED IMPLICITATION COEFFICIENT FOR DIFFUSION
!     WAITING FOR A KEYWORD... BUT NO REAL INTEREST TO HAVE AN
!     EXPLICIT FORM, EXCEPT TO AVOID A LINEAR SYSTEM...
!
      TETATD=1.D0
!
!-----------------------------------------------------------------------
!
      IELMF = F%ELM
      IELMH = H%ELM
      IELMS = SM%ELM
      DIMGLO=MESH%GLOSEG%DIM1
!
!-----------------------------------------------------------------------
!
!     IS IT A FINITE VOLUME SCHEME FOR ADVECTION ?
!
      FV_SCHEME=.FALSE.
      IF(  ICONVF.EQ.ADV_LPO.OR.ICONVF.EQ.ADV_LPO_TF.OR.
     &     ICONVF.EQ.ADV_NSC.OR.ICONVF.EQ.ADV_NSC_TF.OR.
     &     ICONVF.EQ.ADV_PSI.OR.ICONVF.EQ.ADV_PSI_TF     ) THEN
        FV_SCHEME=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CASE WHERE H AND T DON'T HAVE THE SAME DISCRETISATION
!
      IF(IELMF.NE.IELMS) THEN
        CALL CHGDIS(SM ,IELMS,IELMF,MESH)
      ENDIF
      IF(IELMF.NE.IELMH.AND.YASMH) THEN
        CALL CHGDIS(SMH ,IELMH,IELMF,MESH)
      ENDIF
!
!
!-----------------------------------------------------------------------
!
!     HPROP IS THE DEPTH IN THE DIVERGENCE TERM IN THE CONTINUITY
!     EQUATION. IT IS REPLACED HERE BY POINTER HPR, IN CASE IT HAS
!     TO BE EXTENDED.
!
      IF(IELMF.NE.IELMH) THEN
        CALL OS( 'X=Y     ' , X=T10   , Y=HPROP )
        CALL CHGDIS(T10,IELMH,IELMF,MESH)
        HPR=>T10
      ELSE
        HPR=>HPROP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE VARIABLES
!
!     SOLUTION INITIALISED TO F AT TIME N
      IF(ISOUSI.EQ.1) CALL OS( 'X=Y     ' , X=F , Y=FN )
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  IF SUPG, BUILDS THE SEMI-IMPLICIT MATRIX + SUPG IN AM2
!
      IF(ICONVF.EQ.ADV_SUP.AND.CONV) THEN
!
!       TERM IN U.GRAD(T) CENTERED:
!
        CALL MATRIX(AM2,'M=N     ','MATVGR          ',IELMF,IELMF,
     &              1.D0,S,S,S,UCONV,VCONV,S,
     &              MESH,MSK,MASKEL)
!
!       ADDS SUPG CONTRIBUTION TO AM2
!
        IF(OPTADV.EQ.1) THEN
!         CLASSICAL SUPG
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(AM2,'M=M+N   ','MASUPG          ',IELMF,IELMF,
     &                1.D0,TE1,TE2,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
!
        ELSEIF(OPTADV.EQ.2) THEN
!         MODIFIED SUPG
          CALL MATRIX(AM2,'M=M+N   ','MAUGUG          ',IELMF,IELMF,
     &                0.5D0*DT,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
        ELSEIF(OPTADV.NE.0) THEN
          WRITE(LU,*) 'CVDFTR: UNKNOWN OPTION FOR SUPG =',OPTADV
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   COMPUTES AM1: MASS MATRIX MULTIPLIED BY 1/DT
!
      IF(DIFT.OR..NOT.FV_SCHEME.OR..NOT.CONV.OR.BILAN) THEN
!
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
        CALL OS('X=Y+Z   ',X=T2,Y=ZF,Z=HN)
        CALL DECVRT(TE3,T2,ZF,MESH)
!       MASS MATRIX LOCALLY LUMPED ON THE TIDAL FLATS
        FORMUL='MSLUMP          '
!       WILL USE MSQ TO MASK THE DIFFUSION
        MSQ=.TRUE.
        IF(MSK) CALL OS('X=XY    ',X=TE3,Y=MASKEL)
      ELSE
!       NORMAL MASS MATRIX
        FORMUL='MATMAS          '
!       MASK FOR THE DIFFUSION = MASKEL
        IF(MSK) CALL OS('X=Y     ',X=TE3,Y=MASKEL)
        MSQ=MSK
      ENDIF
      CALL MATRIX(AM1,'M=N     ',FORMUL,IELMF,IELMF,
     &            1.D0/DT,TE3,S,S,S,S,S,MESH,MSK,MASKEL)
!
!   POSSIBLE MASS-LUMPING
!
      IF(AGGLOT.GT.0.001D0) THEN
        CALL LUMP(T1,AM1,MESH,AGGLOT)
        CALL OM('M=CM    ', M=AM1, C=1.D0-AGGLOT, MESH=MESH)
        CALL OM('M=M+D   ', M=AM1, D=T1, MESH=MESH)
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
!   COMPUTES THE SECOND MEMBERS
!
!     COMPUTES DT * SM IN T2
!
!     CVTRVF AND CVTRVF_POS WILL TREAT SM IN A DIFFERENT WAY
!
      IF(.NOT.FV_SCHEME) THEN
        CALL OS( 'X=CY    ' , X=T2 , Y=SM , C=DT )
      ENDIF
!
!=======================================================================
! TREATS THE VARIOUS TYPES OF ADVECTION:
!-----------------------------------------------------------------------
!
      IF(ICONVF.EQ.ADV_CAR.OR..NOT.CONV) THEN
!
        CALL OS( 'X=X+Y   ' , X=T2 , Y=FTILD )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
!
!-----------------------------------------------------------------------
!
      ELSEIF(ICONVF.EQ.ADV_SUP.AND.CONV) THEN
!
!       AM1 MADE NONSYMMETRICAL IF IT WAS NOT ALREADY
!
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM('M=X(M)  ', M=AM1, MESH=MESH)
        ENDIF
!
!       SUPG CONTRIBUTION TO THE MASS MATRIX
!
        IF(OPTADV.EQ.1) THEN
!         CLASSICAL SUPG
!         TE1 AND TE2 ALREADY COMPUTED
          CALL MATRIX(AM1,'M=M+TN    ','MATVGR          ',IELMF,IELMF,
     &                1.D0/DT,S,S,S,TE1,TE2,S,MESH,MSK,MASKEL)
!
        ELSEIF(OPTADV.EQ.2) THEN
!         MODIFIED SUPG
          CALL MATRIX(AM1,'M=M+TN    ','MATVGR          ',IELMF,IELMF,
     &                0.5D0,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
        ELSEIF(OPTADV.NE.0) THEN
          WRITE(LU,*) 'CVDFTR: UNKNOWN OPTION FOR SUPG =',OPTADV
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       END OF THE SUPG CONTRIBUTION TO THE MASS MATRIX
!
        CALL OS( 'X=X+Y   ' , X=T2 , Y=FN )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
!
! EXPLICIT ADVECTION TERM:
!
        CALL MATVEC( 'X=X+CAY ',SM,AM2,FN,TETAT-1.D0,MESH)
!
! ADDS THE IMPLICIT ADVECTION PART IN AM2 TO AM1
!
        CALL OM('M=M+CN  ', M=AM1, N=AM2, C=TETAT, MESH=MESH)
!
!-----------------------------------------------------------------------
!
      ELSEIF( (ICONVF.EQ.ADV_LPO.OR.
     &         ICONVF.EQ.ADV_NSC.OR.
     &         ICONVF.EQ.ADV_PSI    ).AND.CONV ) THEN
!
! CONSERVATIVE EQUATION, DISTRIBUTIVE SCHEMES (LEO POSTMA, N AND PSI)
!                        LEO POSTMA AND N-SCHEME ARE THE SAME IN 2D
!
!       TO BE REMOVED WHEN ALL CALLS TO CVDFTR ARE CHECKED
!       OPTVF SHOULD BE 0 (VELOCITY FIELD OBEYS THE CONTINUITY EQUATION)
!       OR 10 (VELOCITY FIELD DOES NOT OBEY THE CONTINUITY EQUATION)
        IOPT=10*(OPTVF/10)
!       OPTION TO DISTRIBUTE THE FLUXES (HERE 2 OR 3)
        IF(ICONVF.EQ.ADV_LPO) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_NSC) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_PSI) IOPT=IOPT+3
!
        IF(TB%N.LT.22) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL IN CVDFTR'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL CVTRVF(F,FN,FSCEXP,H,HN,HPROP,UCONV,VCONV,
     &              DM1,ZCONV,SOLSYS,SM,SMH,YASMH,SMI,YASMI,
     &              FBOR,MASKTR,MESH,AGGLOT,DT,ENTET,
     &              MSK,MASKEL,S,MASSOU,OPTSOU,
!                                                       YAFLBOR
     &              LIMTRA%I,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              VOLU2D,V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT,
     &              RAIN,PLUIE,TRAIN,OPTADV,TB,12,AM2,TB2,
     &              NCO_DIST,NSP_DIST,YAFLULIM,FLULIM%R,
     &              YAFLULIMEBE,FLULIMEBE%R,SLVTRA)
!       IF EXITS AT THIS POINT, THE DIRICHLET ARE NOT DONE, ALSO WORKS
!       CAN THEN CHECK THE MASS CONSERVATION EXACTLY
        IF(.NOT.DIFT) RETURN
        CALL MATVEC( 'X=AY    ',SM,AM1,F,C,MESH)
!
!-----------------------------------------------------------------------
!
      ELSEIF( (ICONVF.EQ.ADV_LPO_TF.OR.
     &         ICONVF.EQ.ADV_NSC_TF.OR.
     &         ICONVF.EQ.ADV_PSI_TF    ).AND.CONV ) THEN
!
!       SCHEMES NERD OR ERIA, FOR TIDAL FLATS
!       LEO POSTMA AND N-SCHEME ARE THE SAME IN 2D
!
!       OPTION TO DISTRIBUTE THE FLUXES (HERE 2 OR 3 AND 12 OR 13)
        IOPT=10*(OPTVF/10)
        IF(TB%N.LT.20) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL IN CVDFTR'
          WRITE(LU,*) 'FOR CALLING CVTRVF_NERD AND CVTRVF_ERIA'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(ICONVF.EQ.ADV_LPO_TF.OR.
     &     ICONVF.EQ.ADV_NSC_TF) THEN
!
!         NERD
          IOPT=IOPT+2
          CALL CVTRVF_NERD(F,FN,FSCEXP,H,HN,HPROP,UCONV,VCONV,
     &              DM1,ZCONV,SOLSYS,SM,SMH,YASMH,SMI,YASMI,
     &              FBOR,MASKTR,MESH,
     &              TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &              TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &              TB%ADR(19)%P,TB%ADR(20)%P,
     &              DT,ENTET,MSK,MASKEL,OPTSOU,
!                                                       YAFLBOR
     &              LIMTRA%I,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              UNSV2D,IOPT,FLBORTRA,
     &              MESH%GLOSEG%I(       1:  DIMGLO),
     &              MESH%GLOSEG%I(DIMGLO+1:2*DIMGLO),
     &              MESH%NBOR%I,FLULIM%R,YAFLULIM,RAIN,PLUIE,
     &              TRAIN,GIVEN_FLUX,FLUX_GIVEN,MAXADV)
!
        ELSEIF(ICONVF.EQ.ADV_PSI_TF) THEN
!
!         ERIA
          IOPT=IOPT+3
          CALL CVTRVF_ERIA(F,FN,FSCEXP,H,HN,HPROP,UCONV,VCONV,
     &              DM1,ZCONV,SOLSYS,SM,SMH,YASMH,SMI,YASMI,
     &              FBOR,MASKTR,MESH,
     &              TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &              TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &              TB%ADR(19)%P,TB%ADR(20)%P,
     &              DT,ENTET,MSK,MASKEL,OPTSOU,
!                                                       YAFLBOR
     &              LIMTRA%I,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              UNSV2D,IOPT,FLBORTRA,
     &              MESH%NBOR%I,RAIN,PLUIE,
     &              TRAIN,MAXADV,NCO_DIST,OPTADV)
!
        ENDIF
!

!       IF EXITS AT THIS POINT, THE DIRICHLET ARE NOT DONE, ALSO WORKS
!       CAN THEN CHECK THE MASS CONSERVATION EXACTLY
        IF(.NOT.DIFT) RETURN
        CALL MATVEC( 'X=AY    ',SM,AM1,F,C,MESH)
!
!-----------------------------------------------------------------------
!
      ELSEIF( ICONVF.EQ.ADV_VOID.AND.CONV ) THEN
!
        CALL OS( 'X=Y     ' , X=F, Y=FN)
        CALL OS( 'X=X+Y   ' , X=T2 , Y=FN )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'CVDFTR: UNKNOWN ADVECTION OPTION : ',ICONVF
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
! END OF TREATMENT OF THE VARIOUS TYPES OF ADVECTION:
!=======================================================================
!
!                   COMPUTES THE MATRICES
!
!-----------------------------------------------------------------------
!   COMPUTES AM2 : - DIFFUSION MATRIX, AND BOUNDARY TERMS
!
      IF(DIFT) THEN
!
        IF(OPDTRA.EQ.2) THEN
!         SAVES THE DIFFUSION
          CALL OS('X=Y     ',X=VISC_S,Y=VISC)
!         MULTIPLIES THE DIFFUSION BY HPROP
          CALL OV_2('X=XY    ',VISC%R,1,HPR%R,1,HPR%R,1,C,
     &                         VISC%MAXDIM1,VISC%DIM1)
          IF(VISC%DIM2.EQ.3) THEN
          CALL OV_2('X=XY    ',VISC%R,2,HPR%R,1,HPR%R,1,C,
     &                         VISC%MAXDIM1,VISC%DIM1)
          CALL OV_2('X=XY    ',VISC%R,3,HPR%R,1,HPR%R,1,C,
     &                         VISC%MAXDIM1,VISC%DIM1)
          ENDIF
        ENDIF
!
!       COMPUTES THE DIFFUSION MATRIX (OPTION WITH MONOTONICITY)
!
        CALL MATRIX(AM2,'M=N     ','MATDIF       MON',IELMF,IELMF,
     &              1.D0,S,S,S,VISC,S,S,MESH,MSQ,TE3)
!
        IF(OPDTRA.EQ.2) THEN
!         MULTIPLIES THE MATRIX BY 1/HPROP
          CALL OS( 'X=1/Y   ',X=T4,Y=HPR,
     &             IOPT=2,INFINI=0.D0,ZERO=1.D-2)
          CALL OM('M=X(M)  ', M=AM2, MESH=MESH )
          CALL OM('M=DM    ', M=AM2, D=T4, MESH=MESH)
!         RETURNS THE DIFFUSION
          CALL OS('X=Y     ',X=VISC,Y=VISC_S)
        ENDIF
!
!   TAKES THE BOUNDARY TERMS INTO ACCOUNT IN THE DIFFUSION MATRIX
!
        MSKNEU=3
!
        IF(MESH%NELEB.GT.0) THEN
          CALL MATRIX(MBOR,'M=N     ','FMATMA          ',
     &                IELBOR(IELMF,1),IELBOR(IELMF,1),
     &                -1.D0,AFBOR,S,S,S,S,S,
     &                MESH,.TRUE.,MASKTR%ADR(MSKNEU)%P)
          CALL OM('M=M+N   ', M=AM2, N=MBOR, MESH=MESH)
        ENDIF
!
!       DIFFUSION TERMS
!
        IF(AM1%TYPEXT.NE.'Q'.AND.AM2%TYPEXT.EQ.'Q') THEN
          CALL OM('M=X(M)  ', M=AM1, MESH=MESH)
        ENDIF
!
        IF(TETATD.NE.1.D0) THEN
!         EXPLICIT DIFFUSION TERM
          CALL MATVEC( 'X=AY    ',T1,AM2,FN,C,MESH)
          CALL OS( 'X=X+CY  ' , X=SM , Y=T1 , C=TETATD-1.D0 )
!         IMPLICIT DIFFUSION TERM ( AM1 + TETAT * AM2 )
          CALL OM('M=M+CN  ', M=AM1, N=AM2, C=TETATD, MESH=MESH)
        ELSE
!         IMPLICIT DIFFUSION TERM ( AM1 + AM2 )
          CALL OM('M=M+N   ', M=AM1, N=AM2, MESH=MESH)
        ENDIF
!
!       BOUNDARY STRESS TERMS
!
        CALL VECTOR(T2,'=','MASVEC          ',IELBOR(IELMF,1),
     &              1.D0,BFBOR,S,S,S,S,S,MESH,
     &              .TRUE.,MASKTR%ADR(MSKNEU)%P)
        CALL OSDB( 'X=X+Y   ' , SM , T2 , T2 , C , MESH )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IMPLICIT AND EXPLICIT TERM DUE TO THE SOURCES:
!
      IF(YASMH.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
!
        IF(OPTSOU.EQ.1) THEN
          CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &                1.D0,SMH,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL OS('X=Y/Z   ',X=T1,Y=T2,Z=HPR,
     &              IOPT=2,INFINI=0.D0,ZERO=1.D-3)
!         IMPLICIT PART OF THE POINT SOURCE TERM
!         - TETAT T 1/HPROP SUM ( SCE PSI D(OMEGA)
          CALL OS('X=CX    ', X=T1 , C=TETAT )
          CALL OM('M=M+D   ', M=AM1, D=T1, MESH=MESH)
!         PREPARES THE EXPLICIT PART
          CALL OS('X=YZ    ', X=T1 , Y=SMH , Z=FSCEXP)
          CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &                1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL OS('X=Y/Z   ',X=T1,Y=T2,Z=HPR,
     &             IOPT=2,INFINI=0.D0,ZERO=1.D-3)
          CALL OS('X=X+Y   ', X=SM , Y=T1)
        ELSEIF(OPTSOU.EQ.2) THEN
          CALL OS('X=Y/Z   ',X=T1,Y=SMH,Z=HPR,
     &              IOPT=2,INFINI=0.D0,ZERO=1.D-3)
!         EXPLICIT PART OF THE POINT SOURCE TERM
!         1/HPROP (FSCE-(1-TETAT)FN) SMH
          CALL OS('X=X+YZ  ', X=SM , Y=T1 , Z=FSCEXP)
!         IMPLICIT PART OF THE POINT SOURCE TERM
!         - TETAT T 1/HPROP SUM ( SCE PSI D(OMEGA)
          CALL OS('X=CX    ', X=T1, C=TETAT)
          CALL OM('M=M+D   ', M=AM1, D=T1, MESH=MESH)
        ENDIF
!
      ENDIF
!
!     IMPLICIT AND EXPLICIT TERM DUE TO RAIN AND EVAPORATION
!
      IF(RAIN.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
!
        CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &              1.D0,PLUIE,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL OS('X=Y/Z   ' ,X=T1,Y=T2,Z=HPR,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-3)
!       IMPLICIT PART OF THE POINT SOURCE TERM
!       - TETAT T 1/HPROP SUM ( SCE PSI D(OMEGA)
        CALL OS('X=CX    ',X=T1,C=TETAT)
        CALL OM('M=M+D   ', M=AM1, D=T1, MESH=MESH)
!       PREPARES THE EXPLICIT PART
        CALL OS('X=CYZ   ',X=T1,Y=PLUIE,Z=FN,C=TETAT-1.D0)
!       VALUE OF TRACER IN RAIN CONSIDERED (ONLY EXPLICIT HENCE LIKE TETAT=0)
        IF(ABS(TRAIN).GT.1.D-15) THEN
          DO I=1,FN%DIM1
!                           ONLY REAL RAIN CONSIDERED, NOT EVAPORATION
            T1%R(I)=T1%R(I)+MAX(PLUIE%R(I),0.D0)*TRAIN
          ENDDO
        ENDIF
        CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &              1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL OS('X=Y/Z   ',X=T1,Y=T2,Z=HPR,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-3)
        CALL OS('X=X+Y   ',X=SM,Y=T1)
!
      ENDIF
!
!   IMPLICIT TERM IF THERE IS ONE :
!
!   THE TREATMENT BELOW ENSURES THAT IF THE EXPLICIT SOURCE TERM
!   IS IN THE FORM  K*FN/H AND SMI EQUALS -K THEN THE TWO TERMS
!   WILL BE BALANCED (CASE OF EROSION AND DEPOSITION)
!
!                  FV_SCHEME : IMPLICIT SOURCE TERM HAS BEEN TREATED
!                  AND WILL NOT BE DONE TWICE
      IF(YASMI.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
        CALL MATRIX(AM2,'M=N     ','MATMAS          ',IELMF,IELMF,
     &              -1.D0,S,S,S,S,S,S,MESH,MSK,MASKEL)
!       POSSIBLE MASS-LUMPING
        IF(AGGLOT.GT.0.001D0) THEN
          CALL LUMP(T1,AM2,MESH,AGGLOT)
          CALL OM('M=CM    ', M=AM2, C=1.D0-AGGLOT, MESH=MESH)
          CALL OM('M=M+D   ', M=AM2, D=T1, MESH=MESH)
        ENDIF
!       COMPUTES SMI/H (DOES NOT CHECK IF H SIZE IS SUFFICIENT!!)
        IF(OPTBAN.GT.0) THEN
!         DIVIDES BY H WITH HARD-CODED CLIPPING AT 1.D-4
          CALL CPSTVC(SMI,T4)
          DO I=1,SMI%DIM1
            T4%R(I)=SMI%R(I)/MAX(H%R(I),1.D-4)
          ENDDO
        ELSE
!         DIVIDES WITHOUT CHECKING
          CALL OS( 'X=Y/Z   ',X=T4,Y=SMI,Z=H)
        ENDIF
        CALL OM('M=X(M)  ', M=AM2, MESH=MESH)
        CALL OM('M=MD    ', M=AM2, D=T4, MESH=MESH)
!       ADDS TO MATRIX AM1
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM('M=X(M)  ', M=AM1, MESH=MESH)
        ENDIF
        CALL OM('M=M+N   ', M=AM1, N=AM2, MESH=MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(ICONVF.EQ.ADV_CAR.AND..NOT.DIFT) THEN
        CALL OS('X=Y     ', X=F, Y=FTILD)
      ENDIF
!
!-----------------------------------------------------------------------
!
!   BOUNDARY CONDITIONS (POINTS OF THE TYPE DIRICHLET)
!
!     DIRICHLET BOUNDARY CONDITIONS ARE TREATED IN A WEAK FORM BY
!     FINITE VOLUME SCHEMES, TO ENSURE THE CORRECT FLUX
!     THEN CALL DIRICH HERE WOULD SPOIL MASS CONSERVATION. HOWEVER
!     FORCING THE CALL IS ALLOWED IN THIS CASE WITH PARAMETER DIRFLU
!
!
      IF(.NOT.FV_SCHEME.OR.DIRFLU.EQ.1) THEN
        CALL DIRICH(F, AM1, SM,FBOR,LIMTRA%I,TB,MESH,KDIR,MSK,MASKPT)
      ENDIF
!
!-----------------------------------------------------------------------
!
!   SOLVES THE LINEAR SYSTEM:
!
#if defined COMPAD
      CALL AD_SOLVE(F,AM1,SM,TB,SLVTRA,INFOGT,MESH,AM2)
!!                                               NOT USED HERE
#else
      CALL SOLVE(F,AM1,SM,TB,SLVTRA,INFOGT,MESH,AM2)
!!                                               NOT USED HERE
#endif
!
!-----------------------------------------------------------------------
!
      IF(YASMI.AND.BILAN) THEN
        IF(YASMI.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
!         WE ASSUME HERE THAT AM2 HAS NOT BEEN DESTROYED IN
!         SOLVE (THIS WOULD BE THE CASE WITH SOME PRECONDITIONING
!         BUT IN THIS CASE AM2 COULD NOT BE USED BEFORE)
          CALL MATVEC( 'X=AY    ',T2,AM2,F,C,MESH)
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(T2,2,MESH)
            MASSOU = MASSOU - DT*P_DOTS(T2,HPROP,MESH)
          ELSE
            MASSOU = MASSOU - DT*DOTS(T2,HPROP)
          ENDIF
        ELSEIF(YASMI.AND.(FV_SCHEME.AND.CONV)) THEN
!         THIS IS CONSISTENT WITH WHAT IS DONE
!         AT THE END OF CVTRVF_POS, FOR SCHEME 13 OR 14.
          C=0.D0
          DO I=1,SMI%DIM1
            C = C + DT*SMI%R(I)*F%R(I)*VOLU2D%R(I)
          ENDDO
          IF(NCSIZE.GT.1) C=P_SUM(C)
          MASSOU=MASSOU+C
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE TRACER FLUX AT THE BOUNDARY
!
      IF(.NOT.FV_SCHEME) THEN
        DO I=1,MESH%NPTFR
          N=MESH%NBOR%I(I)
          FLBORTRA%R(I)=FLBOR%R(I)*(TETAT*F%R(N)+(1.D0-TETAT)*FN%R(N))
        ENDDO
!     ELSE
!       FLBORTRA ALREADY COMPUTED
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IELMF.NE.IELMS) CALL CHGDIS(SM  ,IELMF,IELMS,MESH)
      IF(IELMF.NE.IELMH.AND.YASMH) CALL CHGDIS(SMH ,IELMF,IELMH,MESH)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
