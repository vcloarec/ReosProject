!                       ******************
                        SUBROUTINE VOLFIN2
!                       ******************
     & (W,FLUX,H,U,V,QU,QV,FLUSORT,FLUENT,SMH,MASSES,T,HT,MASSOU,
     &  FLUTENT,FLUTSOR,DJX,DJY,DX,DY,DSZ,FLBOR,FLUXT,FLUXT_OLD,
     &  FLUHTEMP,DXT,DYT,DJXT,DJYT,FLUX_OLD,MESH,MASS_RAIN,YASMO,DT,
     &  YASMH,UBOR,VBOR,IVIS,BETA,SMTR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Solves the Shallow Water Equations system using finite volume
!!        schemes.
!
!>@history  N.GOUTAL; INRIA
!!        22/03/1998
!!
!!   ROE SCHEME (NG); KINETIC SCHEMES (INRIA)
!
!>@history  J-M HERVOUET (LNHE)
!!        05/09/2007
!!
!!   MULTIPLE TRACERS
!
!>@history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!!        13/07/2010
!!        V6P0
!!   Translation of French comments within the FORTRAN sources into
!!   English comments
!
!>@history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!!        21/08/2010
!!        V6P0
!!   Creation of DOXYGEN tags for automated documentation and
!!   cross-referencing of the FORTRAN sources
!
!>@history  R. ATA (EDF-LNHE)
!!        03/15/2011
!!        V6P1
!!    CHANGE EXPLICIT EULER BY NEWMARK SCHEME
!!    ADD TCHAMEN AND ZOKAGOA FLUXES
!
!>@history  R. ATA (EDF-LNHE)
!!        07/15/2012
!!        V6P2
!!     ADD HLLC AND WAF FLUXES
!
!>@history  R. ATA (EDF-LNHE)
!!
!!        01/07/2013
!!        V6P3
!!      adaptation with the new data structure (common with FEM)
!!      remove unused variables
!!      parallel version
!
!>@history  R. ATA
!!        28/01/2014
!!        V7P0
!!    change diemensions of CMI
!!    from (2,NSEG) to (NSEG,2)
!
!>@history S.PAVAN
!!        02/05/2014
!!        V7P0
!!    Initialization of flux_old
!!    for kinetic schemes
!
!>@history R. ATA (EDF R&D-LNHE)
!!        20/06/2014
!!        V7P0
!!    change winf values which are directly
!!    obtained by bord
!!    add parcom_bord after cdl routines
!!    change cdl routines to exactly impose boundary conditions
!!    initiliaze QU,QV and Hn
!!
!>@history R. ATA (EDF R&D-LNHE)
!!        20/01/2015
!!        V7P0
!!    correction for parallelization
!!    parcom_bord removed and parcom placed
!!    after cdl of each scheme
!
!>@history  R. ATA
!!        25/12/2016
!!        V7P2
!!    include rain and evaporation
!
!>@history  J,RIEHME (ADJOINTWARE)
!!        November 2016
!!        V7P2
!!     Replaced EXTERNAL statements to parallel functions / subroutines
!!     by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  BETA       EXTRAPOLATION COEFFICIENT FOR ORDRE 2
!>@param  [in,out]  DJX        GRADIENT PER TRAINGLE ALONG X
!>@param  [in,out]  DJXT       GRADIENT PER TRAINGLE FOR TRACER ALONG X
!>@param  [in,out]  DJY        GRADIENT PER TRAINGLE ALONG Y
!>@param  [in,out]  DJYT       GRADIENT PER TRAINGLE FOR TRACER ALONG Y
!>@param  [in,out]  DSZ        VARIATION OF Z FOR ORDER 2
!>@param  [in,out]  DT         TIME STEP
!>@param  [in,out]  DX         WORKING TABLE
!>@param  [in,out]  DXT        GRADIENT AT NODE ALONG X
!>@param  [in,out]  DYT        GRADIENT AT NODE ALONG Y
!>@param  [in,out]  DY         WORKING TABLE
!>@param  [in,out]  FLBOR      BOUNDARY MASS FLUXES
!>@param  [in,out]  FLUHTEMP   FLUX FOR TRACER
!>@param  [in,out]  FLUENT     MASS FLUX INLET FROM TN TO TN+1
!>@param  [in,out]  FLUSORT    MASS FLUX OUTLET FROM TN TO TN+1
!>@param  [in,out]  FLUTENT    FLUX TRACER INLET
!>@param  [in,out]  FLUTSOR    FLUX TRACER OUTLET
!>@param  [in,out]  FLUX       FLUX AT TIME N
!>@param  [in,out]  FLUX_OLD   FLUX AT TIME N-1
!>@param  [in,out]  FLUXT      FLUX FOR TRACER AT TIME N
!>@param  [in,out]  FLUXT_OLD  FLUX FOR TRACER AT TIME N-1
!>@param  [in,out]  H          WATER DEPTH AT TIME N+1
!>@param  [in,out]  IVIS       LOCAL VARIABLE FOR DIFFUSION MODEL
!>@param  [in,out]  MASSES     ADDED MASS BY SOURCE TERMS
!>@param  [in,out]  MASSOU     ADDED TRACER MASS BY SOURCE TERM
!>@param  [in,out]  MASS_RAIN  MASS ADDED BY RAIN OR EVAPORATION
!>@param  [in,out]  QU         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  QV         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  SMH        SOURCE TERMS FOR CONTINUITY EQUATION
!>@param  [in,out]  SMTR       SOURCE TERMS FOR TRACEUR
!>@param  [in,out]  T          TRACER UPDATED
!>@param  [in,out]  HT         WATER DEPTH*TRACER UPDATED
!>@param  [in,out]  U          X-VELOCITY COMPONENTS AT TIME N+1
!>@param  [in,out]  V          Y-VELOCITY COMPONENTS AT TIME N+1
!>@param  [in,out]  W          WORKING TABLE
!>@param  [in,out]  UBOR       PRESCRIBED VALUES ON BOUNDARIES FOR U
!>@param  [in,out]  VBOR       PRESCRIBED VALUES ON BOUNDARIES FOR V
!>@param  [in]      YASMH      LOGICAL: TO TAKE INTO ACCOUNT SMH
!>@param  [in]      YASMO      IF TRUE FU AND FV ARE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_VOLFIN2 => VOLFIN2
      USE DECLARATIONS_TELEMAC2D, ONLY : NPOIN,NTRAC,LIMPRO,HC,NEISEG,
     &                                   IKLE,ENTET
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT) :: IVIS
!
      LOGICAL, INTENT(INOUT)    :: YASMO
      LOGICAL, INTENT(IN)       :: YASMH
      DOUBLE PRECISION, INTENT(INOUT) :: DT,MASS_RAIN,BETA
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSORT,FLUENT,MASSES
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DXT(NPOIN),DYT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJYT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,*)
!
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUXT,FLUXT_OLD
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUHTEMP,SMTR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: UBOR,VBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(NTRAC.GT.0) THEN
        CALL INIT_TRAC(HT,SMTR,FLUXT,FLUHTEMP,MASSOU,FLUTENT,
     &                 FLUTSOR,FLBOR,MESH)
      ENDIF
!
      CALL FLUX_FV(W,LIMPRO%I,MESH%NUBO%I,MESH%VNOIN%R,FLUX,FLUSORT,
     &             FLUENT,FLBOR,MESH%ELTSEG%I,MESH%IFABOR%I,
     &             FLUHTEMP,NEISEG%I,UBOR,VBOR)
!
      IF(NTRAC.GT.0) THEN
        CALL FLUX_TRAC(MESH%NUBO%I,IKLE%I,FLUTENT,FLUTSOR,
     &                 MESH%CMI%R,DJXT,DJYT,DXT,DYT,MESH%DPX%R,
     &                 MESH%DPY%R,BETA,DSZ,MESH%AIRST%R,HC%R,FLUXT,
     &                 MESH%ELTSEG%I,MESH%IFABOR%I,MESH%VNOIN%R,FLBOR)
      ENDIF
!
      CALL DIFFUSION_FV(FLUX,FLUXT,IVIS,FLBOR,
     &                  W,DJX,DJY,DX,DY,DJXT,DJYT,DXT,DYT)
!
      IF(ENTET) WRITE(LU,*) 'TIME-STEP: ',DT
!
      CALL MAJZZ(W,HT,FLUX,FLUX_OLD,QU,QV,LIMPRO%I)
!
      CALL SOURCE_MOMENT(W,YASMO)
!
      CALL FV_BALANCE(MASSES,MASS_RAIN,YASMH,SMH,H,QU,QV,FLUX,
     &                FLUX_OLD,W,HT,U,V,T,FLUXT_OLD,MASSOU)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
