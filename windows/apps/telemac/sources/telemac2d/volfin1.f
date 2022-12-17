!                       ******************
                        SUBROUTINE VOLFIN1
!                       ******************
!
     & (W,DTN,FLUX,QU,QV,DJX,DJY,DX,DY,DSZ,LOGFR,T1,T2,T3,T4,T5,
     &  FLUX_OLD,MESH,LEO,DT,CORR_I,CORR_J,CORR_ZL,CORR_ZR,CORR_HL,
     &  CORR_HR,CORR_UL,CORR_UR,CORR_VL,CORR_VR,IVIS,BETA,GPRDTIME)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Solves the Shallow Water Equations system using finite volume
!!        schemes. Initialization step
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
!>@param  [in,out]  CORR_I     SECOND ORDER CORRECTION FOR SLOPE ALONG X
!>@param  [in,out]  CORR_J     SECOND ORDER CORRECTION FOR SLOPE ALONG Y
!>@param  [in,out]  CORR_HL    SECOND ORDER CORRECTION FOR LEFT DEPTH
!>@param  [in,out]  CORR_HR    SECOND ORDER CORRECTION FOR RIGHT DEPTH
!>@param  [in,out]  CORR_UL    SECOND ORDER CORRECTION FOR LEFT X-VEL
!>@param  [in,out]  CORR_UR    SECOND ORDER CORRECTION FOR RIGHT X-VEL
!>@param  [in,out]  CORR_VL    SECOND ORDER CORRECTION FOR LEFT Y-VEL
!>@param  [in,out]  CORR_VR    SECOND ORDER CORRECTION FOR RIGHT Y-VEL
!>@param  [in,out]  CORR_ZL    SECOND ORDER CORRECTION FOR LEFT BOTTOM
!>@param  [in,out]  CORR_ZR    SECOND ORDER CORRECTION FOR RIGHT BOTTOM
!>@param  [in,out]  DJX        GRADIENT PER TRAINGLE ALONG X
!>@param  [in,out]  DJY        GRADIENT PER TRAINGLE ALONG Y
!>@param  [in,out]  DSZ        VARIATION OF Z FOR ORDER 2
!>@param  [in,out]  DT         TIME STEP
!>@param  [in,out]  DTN        TIME STEP AT PREVIOUS ITERATION
!>@param  [in,out]  DX         WORKING TABLE
!>@param  [in,out]  DY         WORKING TABLE
!>@param  [in,out]  FLUENT     MASS FLUX INLET FROM TN TO TN+1
!>@param  [in,out]  FLUSORT    MASS FLUX OUTLET FROM TN TO TN+1
!>@param  [in,out]  FLUX       FLUX AT TIME N
!>@param  [in,out]  FLUX_OLD   FLUX AT TIME N-1
!>@param  [in,out]  FLUXT      FLUX FOR TRACER AT TIME N
!>@param  [in]      GPRDTIME   TEST TIME STEP BIGGER THAN GRAPHIC OUTPUT
!>@param  [in,out]  IVIS       LOCAL VARIABLE FOR DIFFUSION MODEL
!>@param  [in,out]  LEO        LOGICAL FOR GRAPHICAL OUTPUT
!>@param  [in,out]  LOGFR      REFERENCE OF BOUNDARY NODES
!>@param  [in,out]  QU         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  QV         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  T1         WORKING TABLE
!>@param  [in,out]  T2         WORKING TABLE
!>@param  [in,out]  T3         WORKING TABLE
!>@param  [in,out]  T4         WORKING TABLE
!>@param  [in,out]  T5         WORKING TABLE
!>@param  [in,out]  W          WORKING TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_VOLFIN1 => VOLFIN1
      USE DECLARATIONS_TELEMAC2D, ONLY : NPOIN,LIMPRO,
     &                                   HC,NEISEG,ICIN,SORDER
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT) :: LOGFR(*),IVIS
!
      LOGICAL, INTENT(INOUT)    :: LEO
      DOUBLE PRECISION, INTENT(INOUT) :: T1(*),T2(*),T3(*),T4(*),T5(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DT,DTN,BETA,GPRDTIME
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,*)
!
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: CORR_I,CORR_J,CORR_ZL,CORR_ZR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: CORR_HL,CORR_HR,CORR_UL,CORR_UR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: CORR_VL,CORR_VR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL INIT_FV(GPRDTIME,ICIN,QU,QV,FLUX,FLUX_OLD,W,IVIS,NEISEG%I,
     &             CORR_I,CORR_J,CORR_HL,CORR_HR,CORR_UL,CORR_UR,
     &             CORR_VL,CORR_VR,CORR_ZL,CORR_ZR)
!
      CALL CALDT(DT,DTN,LEO)
!
      IF(SORDER.EQ.2) THEN
        CALL SECOND_ORDER(DSZ,BETA,T1,T2,T3,T4,T5,LOGFR,W,DJX,DJY,DX,
     &                    DY,DT,CORR_I,CORR_J,CORR_HL,CORR_HR,CORR_UL,
     &                    CORR_UR,CORR_VL,CORR_VR,CORR_ZL,CORR_ZR,
     &                    MESH%AIRST%R,MESH%VNOIN%R,MESH%ELTSEG%I,
     &                    MESH%IFABOR%I,MESH%NUBO%I,MESH%CMI%R,
     &                    LIMPRO%I,HC%R,GPRDTIME,LEO)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
