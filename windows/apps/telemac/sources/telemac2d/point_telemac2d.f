!                   **************************
                    SUBROUTINE POINT_TELEMAC2D
!                   **************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    Memory allocation of structures, aliases, blocks...
!
!history  J-M HERVOUET (LNHE)
!+        02/04/2009
!+
!+   T2D_FILES(T2DGEO)%LU REPLACES NGEO
!
!history
!+        26/11/2009
!+
!+   SPECIFIC ADVECTION IF EQUA='SAINT-VENANT VF', NO
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
!+        21/03/2011
!+        V6P1
!+   Allocation of KFRO_B as an integer instead of a real BIEF_OBJ
!+   An overlooked bug for a long time!
!
!history  C.COULET (ARTELIA)
!+        30/03/2012
!+        V6P2
!+   Modification for culvert management (no declared sources)
!+   Allocation for Tubes/Bridges
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        12/02/2013
!+        V6P3
!+   Treatment of drogues (floats) changed.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        14/03/2013
!+        V6P3
!+   Treatment of latitude-longitude coordinates (call to almesh).
!
!history  C. COULET (ARTELIA)
!+        23/04/2013
!+        V6P3
!+   Correction of a bug in BUSE.F
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        22/05/2013
!+        V6P3
!+   Size of arrays in TB modified in case of floats.
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Adaptation to the dynamic allocation of weirs.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        18/06/2013
!+        V6P3
!+   Size of IT1,2,3,4 modified in case of weak characteristics.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        31/03/2014
!+        V7P0
!+   SLVTRA is now an array.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/04/2014
!+        V7P0
!+   Mixing strong and weak characteristics now possible, two new blocks
!+   FTILD2 and FNCAR2 for this.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13. Here case of ZFLATS.
!
!history  D. WANG & P. TASSI (EDF LAB, LNHE)
!+        23/09/2014
!+        V7P0
!+   Adding variables for secondary currents.
!
!history R. ATA (EDF LAB, LNHE)
!+        10/11/2014
!+        V7P0
!+   Adding variables for waq, wind and rain
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        15/03/2016
!+        V7P2
!+   Advection scheme ADV_NSC_NC removed.
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        20/07/2016
!+        V7P2
!+   In VARSOR bief_obj T10 put instead of VISC when Elder model of
!+   turbulence is asked.
!
!history R. ATA (EDF LAB, LNHE)
!+        28/09/2016
!+        V7P2
!+   add SA turbulence model's tools
!
!history J-M HERVOUET (jubilado)
!+        07/09/2017
!+        V7P3
!+   Hack corrected in the allocation of W. If it is correctly allocated
!+   before, the previous implementation triggered an error because ERR
!+   was not initialised. Other modifications and simplifications to
!+   enable several successive calls allocating the same objects, for
!+   the refinement procedure. No more extra argument REF for
!+   BIEF_ALLVEC and BIEF_ALLMAT.
!
!history A. LEROY (LNHE) & J-M HERVOUET (jubilado)
!+        26/09/2017
!+        V7P3
!+   Allocation of FINEMESH must be done once, unlike MESH.
!+   FINEMESH at same level than the finest computation (was one level
!+   more before).
!
!history  B.GLANDER (BAW)
!+        06/12/2017
!+        V7P2
!+   add new variable ZRL (reference level for Nestor)
!
!history  S.E.BOURBAN (HRW)
!+        20/01/2018
!+        V7P4
!+   YASMI is now done in POINT_TELEMAC2D, ahead of logical YESIMP
!+   for the memory allocation of implicit arrays.
!+   This includes water quality processes and secondary currents.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE METEO_TELEMAC !, ONLY: WINDX,WINDY,PATMOS,POINT_METEO
      USE INITIAL_DROGUES, ONLY: NDRG_CLSS,NODCLSS,PARCLSS
      USE ALGAE_TRANSP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER MEMW1,NTR,NTRT,NTRKE,I,J,ITRAC,SIZ
      INTEGER IELMX,IELMC1,IELMC2,IELMUT,IELMHT,ILAST
      INTEGER IELBU,IELBH,IELBT,IELBK,IELBE,IELB1,IELBNU
      INTEGER IELBX,CFG(2),CFGBOR(2),ERR
!
      LOGICAL YESWEAK,YESIMP
      INTEGER REF
!
      CHARACTER(LEN=1) TYP
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
        WRITE(LU,21)
      ENDIF
21    FORMAT(1X,///,26X,'*****************************',/,
     &26X,              '*    MEMORY ORGANIZATION    *',/,
     &26X,              '*****************************',/)
!
!-----------------------------------------------------------------------
!
!     WILL THERE BE WEAK CHARACTERISTICS?
!
      YESWEAK=.FALSE.
      IF(OPTCHA.GT.1) THEN
        DO I=1,4
          IF(CONVV(I).AND.ICONVF(I).EQ.ADV_CAR) YESWEAK=.TRUE.
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     WILL THERE BE IMPLICIT DISTRIBUTIVE SCHEMES?
!
      YESIMP=.FALSE.
!
!     LOOKING AT VELOCITIES
      IF(ICONVF(1).GE.3.AND.ICONVF(1).LE.5.AND.OPTADV_VI.EQ.4) THEN
        YESIMP=.TRUE.
      ENDIF
!
!     LOOKING AT K-EPSILON
      IF(ICONVF(4).GE.3.AND.ICONVF(4).LE.5.AND.OPTADV_KE.EQ.4) THEN
        YESIMP=.TRUE.
      ENDIF
!
!     LOOKING AT TRACERS
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(ICONVFT(ITRAC).GE.3.AND.ICONVFT(ITRAC).LE.5
     &                          .AND.OPTADV_TR(ITRAC).EQ.4) THEN
            YESIMP=.TRUE.
          ENDIF
        ENDDO
      ENDIF
!
!     LOOKING AT WATER QUALITY EXCHANGES AND ICE PROCESSES
      IF(NTRAC.GT.0) THEN
        IF(.NOT.ALLOCATED(YASMI)) ALLOCATE(YASMI(NTRAC))
        DO ITRAC=1,NTRAC
          YASMI(ITRAC)  = .FALSE.
        ENDDO
        IF(INCLUS(COUPLING,'WAQTEL')) THEN
          CALL YASMI_WAQ(YASMI)
        ENDIF
        IF(INCLUS(COUPLING,'KHIONE')) THEN
          CALL YASMI_KHIONE(YASMI)
        ENDIF
        DO ITRAC=1,NTRAC
          YESIMP = YESIMP .OR. YASMI(ITRAC)
        ENDDO
      ENDIF
!
!     LOOKING AT SECONDARY CURRENTS
      IF( SECCURRENTS ) THEN
        YESIMP=.TRUE.
        IF(NTRAC.GT.0) THEN
          YASMI(NTRAC)=.TRUE.
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TYPES OF DISCRETISATIONS
!
      IELM0 = 10*(IELMH/10)
      IELM1 = IELM0 + 1
!
      IELB1 = IELBOR(IELM1,1)
      IELBU = IELBOR(IELMU,1)
      IELBH = IELBOR(IELMH,1)
      IELBT = IELBOR(IELMT,1)
      IELBK = IELBOR(IELMK,1)
      IELBE = IELBOR(IELME,1)
      IELBNU= IELBOR(IELMNU,1)
!
      IELMX=MAX(IELMU,IELMH,IELMT,IELMK,IELME,IELBNU)
!
! TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
!
      CFG(1) = OPTASS
      CFG(2) = PRODUC
!     CFG FOR THE BOUNDARY MATRICES
      CFGBOR(1) = 1
      CFGBOR(2) = 1
!
!=======================================================================
!
!     ALLOCATES THE MESH STRUCTURE
!
      REF = RLEVELS
      CALL ALMESH(MESH,'MESH  ',IELMX,SPHERI,CFG,
     &            T2D_FILES(T2DGEO)%FMT,T2D_FILES(T2DGEO)%LU,
     &            EQUA,RLEVELS,PROJECTION=PROTYP,LATI0=LAMBD0,
     &            LONGI0=PHI0,CONVERGENCE=CONVERGENCE,RLEVEL=RLEVEL)
!
!     FINEMESH ALLOCATED ONLY ONCE, WITH OVERDIMENSIONING ALLOWING
!     FURTHER REFINEMENT
!
      IF(CONVERGENCE.AND.RLEVEL.EQ.0) THEN
        WRITE(LU,*) 'ALLOCATE FINEMESH FOR CONVERGENCE STUDY'
        WRITE(LU,*) 'RLEVELS=',RLEVELS
        CALL ALMESH(FINEMESH,'FMESH ',IELMX,SPHERI,CFG,
     &              T2D_FILES(T2DGEO)%FMT,T2D_FILES(T2DGEO)%LU,
     &              EQUA,RLEVELS,PROJECTION=PROTYP,LATI0=LAMBD0,
     &              LONGI0=PHI0,CONVERGENCE=CONVERGENCE,RLEVEL=RLEVEL)
      ENDIF
!
!     ALIAS FOR CERTAIN COMPONENTS OF MESH
!
      IKLE  => MESH%IKLE
      X     => MESH%X%R
      Y     => MESH%Y%R
!
      NELEM => MESH%NELEM
      NELMAX=> MESH%NELMAX
      NPTFR => MESH%NPTFR
      NPTFRX=> MESH%NPTFRX
      TYPELM=> MESH%TYPELM
      NPOIN => MESH%NPOIN
      NPMAX => MESH%NPMAX
      MXPTVS=> MESH%MXPTVS
      MXELVS=> MESH%MXELVS
      LV    => MESH%LV
      NSEG  => MESH%NSEG
!
!=======================================================================
!
!                     **********************
!                     *   VARIOUS ARRAYS   *
!                     **********************
!
!-----------------------------------------------------------------------
!
      IF(.NOT.ALLOCATED(W)) THEN
        ALLOCATE(W(NPOIN),STAT=ERR)
      ELSEIF(SIZE(W).NE.NPOIN) THEN
        DEALLOCATE(W)
        ALLOCATE(W(NPOIN),STAT=ERR)
      ELSE
!       W ALREADY ALLOCATED WITH THE RIGHT SIZE
        ERR=0
      ENDIF
      CALL CHECK_ALLOCATE(ERR, "W")
!
!     FOR TRACERS
!
!     DOUBLE PRECISION
      IF(.NOT.ALLOCATED(MASTEN)) ALLOCATE(MASTEN(NTRAC))
      IF(.NOT.ALLOCATED(MASTOU)) ALLOCATE(MASTOU(NTRAC))
      IF(.NOT.ALLOCATED(MASTRAIN)) ALLOCATE(MASTRAIN(NTRAC))
      IF(.NOT.ALLOCATED(MASSOU)) ALLOCATE(MASSOU(NTRAC))
      IF(.NOT.ALLOCATED(MASTR0)) ALLOCATE(MASTR0(NTRAC))
      IF(.NOT.ALLOCATED(MASTR2)) ALLOCATE(MASTR2(NTRAC))
      IF(.NOT.ALLOCATED(FLUTSOR)) ALLOCATE(FLUTSOR(NTRAC))
      IF(.NOT.ALLOCATED(FLUTENT)) ALLOCATE(FLUTENT(NTRAC))

      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        IF(.NOT.ALLOCATED(VNX1)) ALLOCATE(VNX1(NPTFR))
        IF(.NOT.ALLOCATED(VNY1)) ALLOCATE(VNY1(NPTFR))
        IF(.NOT.ALLOCATED(YESNOFR)) ALLOCATE(YESNOFR(NPTFR))
      ENDIF
!
!-----------------------------------------------------------------------
!
!                       ******************
!                       *   STRUCTURES   *
!                       ******************
!
!-----------------------------------------------------------------------
!
!  ALLOCATES AN EMPTY STRUCTURE
!
      CALL BIEF_ALLVEC(1,S,'S     ',0,1,1,MESH)
!
!  ARRAYS CONTAINING THE VARIABLES WHICH WILL BE OUTPUT TO THE RESULT FILE:
!
      CALL BIEF_ALLVEC(1,U,'U     ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,V,'V     ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,H,'H     ',IELMH,1,1,MESH)
!
!  ARRAYS CONTAINING THE ADVECTED VARIABLES U, V, T, K AND EPSILON
!
      CALL BIEF_ALLVEC(1,UTILD,'UTILD ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,VTILD,'VTILD ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,HTILD,'HTILD ',IELMH,1,2,MESH)
!
!  ARRAYS CONTAINING THE VARIABLES U, V, H STORED AT TIME N
!
      CALL BIEF_ALLVEC(1,UN,'UN    ', IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VN,'VN    ', IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HN,'HN    ', IELMH,1,1,MESH)
!
!  ARRAYS STORING THE RELATIVE CHANGES
!
      CALL BIEF_ALLVEC(1,DH  ,'DH    ' , IELMH ,1,2 ,MESH)
      IF(IORDRU.EQ.2) THEN
        CALL BIEF_ALLVEC(1,DU  ,'DU    ' , IELMU , 1,2,MESH)
        CALL BIEF_ALLVEC(1,DV  ,'DV    ' , IELMU , 1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,DU  ,'DU    ' , 0     , 1,0,MESH)
        CALL BIEF_ALLVEC(1,DV  ,'DV    ' , 0     , 1,0,MESH)
      ENDIF
      IF(IORDRH.EQ.2) THEN
        CALL BIEF_ALLVEC(1,DHN ,'DHN   ' , IELMH , 1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,DHN ,'DHN   ' , 0     , 1,0,MESH)
      ENDIF
!
!  BLOCK OF THE UNKNOWNS IN PROPAG
!
      CALL ALLBLO(UNK,'UNK   ')
      CALL ADDBLO(UNK,DH)
      CALL ADDBLO(UNK, U)
      CALL ADDBLO(UNK, V)
!
!  BOUNDARY CONDITIONS ARRAYS (BOUNDARY ARRAYS)
!  FOR UBOR AND VBOR, SIZE 2 TO ALLOW VELOCITIES
!  OR FLOWRATES IMPOSED BY FUNCTION
!
      CALL BIEF_ALLVEC(1,UBOR    ,'UBOR  ',IELBU,2,1,MESH)
      CALL BIEF_ALLVEC(1,VBOR    ,'VBOR  ',IELBU,2,1,MESH)
      CALL BIEF_ALLVEC(1,HBOR    ,'HBOR  ',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(1,AUBOR   ,'AUBOR ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,CFBOR   ,'CFBOR ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,UETUTA  ,'UETUTA',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,FLBOR   ,'FLBOR ',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(1,FLBORTRA,'FLBTRA',IELBT,1,1,MESH)
!
      IF(TIDALTYPE.EQ.0) THEN
        CALL BIEF_ALLVEC(1,HBTIDE ,'HBTIDE',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,UBTIDE ,'UBTIDE',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,VBTIDE ,'VBTIDE',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMTIDE,'NUMTID',0,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,HBTIDE ,'HBTIDE',IELB1 ,1,1,MESH)
        CALL BIEF_ALLVEC(1,UBTIDE ,'UBTIDE',IELB1 ,1,1,MESH)
        CALL BIEF_ALLVEC(1,VBTIDE ,'VBTIDE',IELB1 ,1,1,MESH)
        CALL BIEF_ALLVEC(2,NUMTIDE,'NUMTID',IELB1 ,1,1,MESH)
      ENDIF
!
!  BLOCK OF DIRICHLET CONDITIONS TO PREPARE CALL TO DIRICH
!
      CALL ALLBLO(DIRBOR,'DIRBOR')
      CALL ADDBLO(DIRBOR,HBOR)
      CALL ADDBLO(DIRBOR,UBOR)
      CALL ADDBLO(DIRBOR,VBOR)
!
! BOTTOM ELEVATION ARRAY:
!
      CALL BIEF_ALLVEC(1,ZF,'ZF    ',IELMH,1,1,MESH)

!     REFERENCE LEVEL FOR NESTOR
!
      CALL BIEF_ALLVEC(1,ZRL,'ZRL   ',IELMH,1,1,MESH)

!
! BOTTOM ELEVATION ARRAY BY ELEMENT (TIDAL FLATS)
!
      IF(MSK) THEN
        CALL BIEF_ALLVEC(1,ZFE,'ZFE   ',IELM0,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,ZFE,'ZFE   ',    0,1,0,MESH)
      ENDIF
!
! VISCOSITY : FOR NOW IN P1
!             BUT SIZE 2 TO CATER FOR ELDER'S MODEL
!
      IF(ITURB.EQ.2) THEN
        CALL BIEF_ALLVEC(1,VISC ,'VISC  ',IELM1,3,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,VISC ,'VISC  ',IELM1,1,1,MESH)
      ENDIF
!     BACKUP ARRAY FOR VISCOSITY
      IF(OPDVIT.EQ.2.OR.(NTRAC.GT.0.AND.OPDTRA.EQ.2)) THEN
        IF(ITURB.EQ.2) THEN
          CALL BIEF_ALLVEC(1,VISC_S,'VISC_S',IELM1,3,1,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,VISC_S,'VISC_S',IELM1,1,1,MESH)
        ENDIF
      ENDIF
!
!  FRICTION COEFFICIENT
!
      CALL BIEF_ALLVEC(1,CHESTR ,'CHESTR',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,CHESTR0,'CHSTR0',IELMU,1,1,MESH)
!
      IF(ROVAR) THEN
        CALL BIEF_ALLVEC(1,RO,'RO    ',IELMH,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,RO,'RO    ',    0,1,0,MESH)
      ENDIF
!
!     WINDX, WINDY, PATMOS NOW ALLOCATED IN METEO_TELEMAC
!
!     WIND GIVEN IN P1
!
!     IF(VENT) THEN
!       CALL BIEF_ALLVEC(1,WINDX,'WINDX ',IELM1,1,1,MESH)
!       CALL BIEF_ALLVEC(1,WINDY,'WINDY ',IELM1,1,1,MESH)
!     ELSE IF(.NOT.INCLUS(COUPLING,'WAQTEL')) THEN
!       CALL BIEF_ALLVEC(1,WINDX,'WINDX ',    0,1,0,MESH)
!       CALL BIEF_ALLVEC(1,WINDY,'WINDY ',    0,1,0,MESH)
!     ENDIF
!
!     ATMOSPHERIC PRESSURE GIVEN IN P1
!
!     IF(ATMOS) THEN
!       CALL BIEF_ALLVEC(1,PATMOS,'PATMOS',IELM1,1,1,MESH)
!     ELSE IF(.NOT.INCLUS(COUPLING,'WAQTEL')) THEN
!       CALL BIEF_ALLVEC(1,PATMOS,'PATMOS',    0,1,0,MESH)
!     ENDIF
!
!  SOURCE TERM ARRAYS
!
      CALL BIEF_ALLVEC(1,FU,'FU    ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,FV,'FV    ',IELMU,1,2,MESH)
!
!  WAVE STRESSES
!
      IF(COUROU.OR.INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL BIEF_ALLVEC(1,FXWAVE,'FXWAVE',IELMU,1,2,MESH)
        CALL BIEF_ALLVEC(1,FYWAVE,'FYWAVE',IELMU,1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,FXWAVE,'FXWAVE',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,FYWAVE,'FYWAVE',0    ,1,0,MESH)
      ENDIF
!
!  POINTERS FOR THE MATRICES
!
      IELMHT = IELMH
!     AM1 USED FOR THE TRACERS
      IF(NTRAC.GT.0) IELMHT = MAX(IELMHT,IELMT)
      CALL BIEF_ALLMAT(AM1,'AM1   ',IELMHT,IELMHT,CFG,'Q','Q',MESH)
!
      TYP='Q'
      IF(ICONVF(1).NE.ADV_SUP    .AND.
     &   3*(SLVPRO%PRECON/3).NE.SLVPRO%PRECON) TYP = 'S'
!
      IF(OPDVIT.EQ.2) TYP='Q'
!
      IELMUT = IELMU
!     AM2 AND AM3 USED FOR THE TRACERS
      IF(NTRAC.GT.0) THEN
        IELMUT = MAX(IELMU,IELMT)
        TYP='Q'
      ENDIF
!     AM2 AND AM3 MODIFIED FOR BOUSSINESQ
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        TYP='Q'
      ENDIF
      CALL BIEF_ALLMAT(AM2,'AM2   ',IELMUT,IELMUT,CFG,'Q',TYP,MESH)
      CALL BIEF_ALLMAT(AM3,'AM3   ',IELMUT,IELMUT,CFG,'Q',TYP,MESH)
!
!  BM1 AND BM2:
!
      CALL BIEF_ALLMAT(BM1,'BM1   ',IELMH,IELMU,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(BM2,'BM2   ',IELMH,IELMU,CFG,'Q','Q',MESH)
!
!  STORES CV1, BM1 AND BM2 FOR CORRECTION FOR CONTINUITY
!  BM1S ALWAYS ALLOCATED FOR CALLING CVTRVF IN CASE OF
!  IMPLICIT ADVECTION SCHEME
!
      CALL BIEF_ALLMAT(BM1S,'BM1S  ',IELMH,IELMU,CFG,'Q','Q',MESH)
      IF(CORCON.AND.SOLSYS.EQ.1) THEN
        CALL BIEF_ALLMAT(BM2S,'BM2S  ',IELMH,IELMU,CFG,'Q','Q',MESH)
        CALL BIEF_ALLVEC(1,CV1S,'CV1S  ',IELMX,1,2,MESH)
      ELSE
        CALL BIEF_ALLMAT(BM2S,'BM2S  ',IELMH,IELMU,CFG,'0','0',MESH)
        CALL BIEF_ALLVEC(1,CV1S,'CV1S  ',0,1,0,MESH)
      ENDIF
!
!  CM1 AND CM2:
!
      IELMC1 = IELMH
      IELMC2 = IELMU
!     CM2 USED FOR U IN SOME CASES
      IF(ICONVF(1).EQ.ADV_SUP) THEN
        IELMC1 = MAX(IELMC1,IELMU)
      ENDIF
      IF(EQUA(1:10).EQ.'BOUSSINESQ') IELMC1 = MAX(IELMC1,IELMU)
!
      CALL BIEF_ALLMAT(CM1,'CM1   ',IELMC1,IELMC2,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(CM2,'CM2   ',IELMC1,IELMC2,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TM1,'TM1   ',IELMU ,IELMU ,CFG,'Q','Q',MESH)
!
!  BOUNDARY MATRIX
!
      IELBX = MAX(IELBU,IELBH,IELBT,IELBK,IELBE)
      CALL BIEF_ALLMAT(MBOR,'MBOR  ',IELBX,IELBX,CFGBOR,'Q','Q',MESH)
!
!  MATRICES A23 AND A32 USED FOR DIAGONAL-BLOCK PRECONDITIONING
!  OR FOR THE BOUSSINESQ EQUATIONS
!
      TYP = '0'
      IF(3*(SLVPRO%PRECON/3).EQ.SLVPRO%PRECON) TYP = 'Q'
      IF(EQUA(1:10).EQ.'BOUSSINESQ') TYP = 'Q'
      CALL BIEF_ALLMAT(A23,'A23   ',IELMU,IELMU,CFG,TYP,TYP,MESH)
      CALL BIEF_ALLMAT(A32,'A32   ',IELMU,IELMU,CFG,TYP,TYP,MESH)
!
! BLOCK OF THE MATRICES IN PROPAG
!
      CALL ALLBLO(MAT,'MAT   ')
      CALL ADDBLO(MAT,AM1)
      CALL ADDBLO(MAT,BM1)
      CALL ADDBLO(MAT,BM2)
      CALL ADDBLO(MAT,CM1)
      CALL ADDBLO(MAT,AM2)
      CALL ADDBLO(MAT,A23)
      CALL ADDBLO(MAT,CM2)
      CALL ADDBLO(MAT,A32)
      CALL ADDBLO(MAT,AM3)
!
! WORKING ARRAY W1 (SIZE TO BE CHECKED)
!
!     NECESSARY MEMORY FOR W1 IN VALIDA
      MEMW1 = 9*NPOIN
!     FINITE VOLUMES
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        MEMW1 = MAX(MEMW1,9*NPOIN+3*NPTFR,2*MXPTVS*NPOIN)
      ENDIF
!     THIS MEMORY SPACE IS RESERVED IN THE FORM OF ONE
!     ARRAY P0 OF SIZE 2
      MEMW1 = MAX(3,1+MEMW1/BIEF_NBMPTS(IELM0,MESH))
      CALL BIEF_ALLVEC(1,W1,'W1    ',IELM0,MEMW1,1,MESH)
      CALL BIEF_ALLVEC(1,W2,'W2    ',IELM0,MEMW1,1,MESH)
!
! WORKING ARRAY (DIFFERENT FROM W1 FOR FINITE VOLUMES, WITH DEBIMP)
!
      CALL BIEF_ALLVEC(1,W1DEB,'W1DEB ',IELM0,MEMW1,1,MESH)
!
!_______________________________________________________________________
!
!  POINTERS FOR THE SECOND MEMBERS OF THE PROPAGATION STEP
!
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,CV1,'CV1   ',IELMX,1,2,MESH)
      CALL BIEF_ALLVEC(1,CV2,'CV2   ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,CV3,'CV3   ',IELMU,1,2,MESH)
!
!  BLOCK OF THE SECOND MEMBERS IN PROPAG
!
      CALL ALLBLO(RHS,'RHS   ')
      CALL ADDBLO(RHS,CV1)
      CALL ADDBLO(RHS,CV2)
      CALL ADDBLO(RHS,CV3)
!_______________________________________________________________________
!
!  POINTERS FOR THE SOURCE TERMS OF THE PROPAGATION STEP
!
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,SMH,'SMH   ',IELMX,1,2,MESH)
!_______________________________________________________________________
!
!  POINTERS FOR ADVECTION AND PROPAGATION FIELDS
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,UCONV,'UCONV ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VCONV,'VCONV ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HPROP,'HPROP ',IELMH,1,1,MESH)
!_______________________________________________________________________
!
!  POINTERS FOR INTEGRAL OF THE BASES, IN PARALLEL, AND REVERSE
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,VOLU2D,'VOLU2D',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,V2DPAR,'V2DPAR',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,UNSV2D,'UNSV2D',IELMH,1,1,MESH)
!_______________________________________________________________________
!
!  POINTERS USED FOR LAGRANGIAN DRIFTS
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,XLAG  ,'XLAG  ',NPOIN*NLAG*7**REF,1,0,MESH)
      CALL BIEF_ALLVEC(1,YLAG  ,'YLAG  ',NPOIN*NLAG*7**REF,1,0,MESH)
      CALL BIEF_ALLVEC(1,SHPLAG,'SHPLAG',
     &            NPOIN*BIEF_NBPEL(IELM1,MESH)*NLAG*7**REF,1,0,MESH)
!
!-----------------------------------------------------------------------
!
!  POINTERS FOR WORKING ARRAYS:
!
!-----------------------------------------------------------------------
!
!  NUMBER OF ARRAYS TO BE ALLOCATED : NTR
!           21 : POUR CGSTAB =3 X 7, 23 POUR CVDFTR (APPEL DE CVTRVF)
      NTR = 23
      IF(SLVPRO%SLV.EQ.7) NTR = MAX(NTR,6+6*SLVPRO%KRYLOV)
!     6 ADDITIONAL DIAGONALS TO STORE IN BLOCK-DIAGONAL PRECONDITIONING
      IF(3*(SLVPRO%PRECON/3).EQ.SLVPRO%PRECON) NTR = NTR + 6
!
!  MAXIMUM USEFUL SIZE
!
      NTRT=0
      ILAST=ADR_TRAC
!
      IF(NTRAC.GT.0) THEN
!       NTRT = 7
!       BECAUSE OF THE POSITION OF TRACERS IN VARSOR (WILL BE
!       THE SAME IN TB, USED BY VALIDA)
        NTRT = ILAST+NTRAC
        DO ITRAC=1,NTRAC
          IF(SLVTRA(ITRAC)%SLV.EQ.7) THEN
            NTRT = MAX(2+2*SLVTRA(ITRAC)%KRYLOV,NTRT)
          ENDIF
        ENDDO
        NTR = MAX(NTR,NTRT)
      ENDIF
      NTRKE=0
      IF(ITURB.EQ.3) THEN
        NTRKE=7
        IF(SLVK%SLV.EQ.7) NTRKE = MAX(NTRKE,2+2*SLVK%KRYLOV)
        NTR  = MAX(NTR,NTRKE)
      ENDIF
!
!  ALLOCATES NTR WORKING ARRAYS (SIZE: THE MAXIMUM NUMBER OF
!                                      DEGREES OF FREEDOM)
!
!     TB WILL CONTAIN ARRAYS T1,T2,...
!
      CALL ALLBLO(TB ,'TB    ')
!
      IF(NFLOT_MAX.GT.BIEF_NBMPTS(IELMX,MESH)) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(TB,NTR,1,'TB    ',
     &                            NFLOT_MAX,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC_IN_BLOCK(TB,NTR,1,'TB    ',IELMX,1,2,MESH)
      ENDIF
!
      CALL ALLBLO(TB2,'TB2   ')
      IF(YESIMP) THEN
!       SEE CVTRVF, 9 IS THE VALUE CORRESPONDING TO SOLVER 7=GMRES
        CALL BIEF_ALLVEC_IN_BLOCK(TB2,9,1,'TB2   ',IELMX,1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC_IN_BLOCK(TB2,9,1,'TB2   ',    0,1,0,MESH)
      ENDIF
!
!     ALIAS FOR THE FIRST 22 WORKING ARRAYS OF THE BLOCK: TB
!
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
      T5 =>TB%ADR( 5)%P
      T6 =>TB%ADR( 6)%P
      T7 =>TB%ADR( 7)%P
      T8 =>TB%ADR( 8)%P
      T9 =>TB%ADR( 9)%P
      T10=>TB%ADR(10)%P
      T11=>TB%ADR(11)%P
      T12=>TB%ADR(12)%P
      T13=>TB%ADR(13)%P
      T14=>TB%ADR(14)%P
      T15=>TB%ADR(15)%P
      T16=>TB%ADR(16)%P
      T17=>TB%ADR(17)%P
      T18=>TB%ADR(18)%P
      T19=>TB%ADR(19)%P
      T20=>TB%ADR(20)%P
      T21=>TB%ADR(21)%P
      T22=>TB%ADR(22)%P
!
!  ALLOCATES WORKING ARRAYS (SIZE: THE MAXIMUM NUMBER OF ELEMENTS)
!
!
      CALL BIEF_ALLVEC(1,TE1,'TE1   ',IELM0,1,1,MESH)
      CALL BIEF_ALLVEC(1,TE2,'TE2   ',IELM0,1,1,MESH)
      CALL BIEF_ALLVEC(1,TE3,'TE3   ',IELM0,1,1,MESH)
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
!       PIECE-WISE LINEAR FREE SURFACE (ELEMENT NUMBER 15)
        CALL BIEF_ALLVEC(1,ZFLATS, 'ZFLATS',15,3,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,ZFLATS, 'ZFLATS', 0,1,1,MESH)
      ENDIF
      IF(OPTBAN.EQ.3) THEN
        CALL BIEF_ALLVEC(1,TE4,'TE4   ',IELM0,1,1,MESH)
        CALL BIEF_ALLVEC(1,TE5,'TE5   ',IELM0,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TE4,'TE4   ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,TE5,'TE5   ',    0,1,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     JAJ #### IF REQUIRED, WE READ HERE THE INPUT SECTIONS FILE
!     AND MODIFY NCP AND CTRLSC(1:NCP) ACCORDINGLY IN READ_SECTIONS
!
      IF (TRIM(T2D_FILES(T2DSEC)%NAME).NE.'') THEN
        WRITE(LU,*)
     &   'POINT_TELEMAC2D: SECTIONS DEFINED IN THE SECTIONS INPUT FILE'
        CALL READ_SECTIONS_TELEMAC2D
      ELSE ! THE EARLIER WAY OF DOING THINGS
        IF (NCP.NE.0) WRITE(LU,*)
     &   'POINT_TELEMAC2D: SECTIONS DEFINED IN THE PARAMETER FILE'
      ENDIF
!
!     BLOCK OF MASKS FOR THE COMPUTATION OF FLUXES ACCROSS SECTIONS
!     ONLY WITH COMPATIBLE FLUXES
!
      CALL ALLBLO(MSKSEC,'MSKSEC')
      IF(NCP.GT.1.AND.COMFLU) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(MSKSEC,NCP/2,1,'MSKS  ',
     &                            IELM0,1,1,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
! POINTERS OF THE MASKS
!
!     BLOCK OF THE MASKS FOR BOUNDARY CONDITIONS
!     (PROPAGATION)
!
      CALL ALLBLO(MASK,'MASK  ')
      CALL BIEF_ALLVEC_IN_BLOCK(MASK,11,1,'MASK  ',IELBH,1,2,MESH)
!
      IF(MSK) THEN
        CALL BIEF_ALLVEC(1,MASKEL,'MASKEL',IELM0,1,1,MESH)
        CALL BIEF_ALLVEC(1,MASKPT,'MASKPT',IELMX,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,MASKEL,'MASKEL',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,MASKPT,'MASKPT',    0,1,0,MESH)
      ENDIF
!
!  ADDITIONAL ARRAYS IF THERE ARE TRACERS
!
      CALL ALLBLO(T      ,'TRAC  ')
      CALL ALLBLO(TTILD  ,'TTILD ')
      CALL ALLBLO(TN     ,'TN    ')
      CALL ALLBLO(TEXP   ,'TEXP  ')
      CALL ALLBLO(TIMP   ,'TIMP  ')
      CALL ALLBLO(TSCEXP ,'TSCEXP')
      CALL ALLBLO(VISCT  ,'VISCT ')
      CALL ALLBLO(MASKTR ,'MASKTR')
      CALL ALLBLO(TBOR   ,'TBOR  ')
      CALL ALLBLO(ATBOR  ,'ATBOR ')
      CALL ALLBLO(BTBOR  ,'BTBOR ')
      CALL ALLBLO(LITBOR ,'LITBOR')
      IF(NTRAC.GT.0) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(T     ,NTRAC,1,'T     ',
     &                            IELMT,1,1,MESH)
        DO ITRAC=1,NTRAC
          CALL OV('X=0     ', X=T%ADR(ITRAC)%P%R, DIM1=MESH%NPOIN)
        ENDDO
        CALL BIEF_ALLVEC_IN_BLOCK(TTILD ,NTRAC,1,'TTILD ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TN    ,NTRAC,1,'TN    ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TEXP  ,NTRAC,1,'TEXP  ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TIMP  ,NTRAC,1,'TIMP  ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TSCEXP,NTRAC,1,'TSCEXP',
     &                            IELMT,1,1,MESH)
        IF(ITURB.EQ.2) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,NTRAC,1,'VISCT ',
     &                              IELMT,3,1,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,NTRAC,1,'VISCT ',
     &                              IELMT,1,1,MESH)
        ENDIF
        CALL BIEF_ALLVEC_IN_BLOCK(MASKTR,5,1,'MSKTR ',IELBH,1,2,MESH)
        IF(THOMFR) THEN
!         SECOND DIMENSION USED AS A WORKING ARRAY
!         IN THOMPS
          CALL BIEF_ALLVEC_IN_BLOCK(TBOR,NTRAC,1,'TBOR  ',
     &                              IELBT,2,1,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(TBOR,NTRAC,1,'TBOR  ',
     &                              IELBT,1,1,MESH)
        ENDIF
        CALL BIEF_ALLVEC_IN_BLOCK(ATBOR  ,NTRAC,1,'ATBOR ',
     &                            IELBT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(BTBOR  ,NTRAC,1,'BTBOR ',
     &                            IELBT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(LITBOR ,NTRAC,2,'LITBOR',
     &                            IELBT,1,1,MESH)
      ELSE
!       AT LEAST ONE ELEMENT IN BLOCKS, NOT NTRAC
        CALL BIEF_ALLVEC_IN_BLOCK(T     ,1,1,'T     ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TTILD ,1,1,'TTILD ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TN    ,1,1,'TN    ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TEXP  ,1,1,'TEXP  ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TIMP  ,1,1,'TIMP  ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TSCEXP,1,1,'TSCEXP',0,1,0,MESH)
        IF(ITURB.EQ.2) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,1,1,'VISCT ',0,3,0,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,1,1,'VISCT ',0,1,0,MESH)
        ENDIF
        CALL BIEF_ALLVEC_IN_BLOCK(MASKTR ,4,1,'MSKTR ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TBOR   ,1,1,'TBOR  ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(ATBOR  ,1,1,'ATBOR ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(BTBOR  ,1,1,'BTBOR ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(LITBOR ,1,2,'LITBOR',0,1,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FRICTION COEFFICIENT CF
!
      CALL BIEF_ALLVEC(1,CF,'CF    ',IELMU,1,1,MESH)
!
!  DATA FOR FRICTION SET PER ZONE
!
!  FRICTION LAW USED
!
      CALL BIEF_ALLVEC(2,NKFROT,'NKFROT',IELMU,1,1,MESH)
!
!  CHESTR ON THE BOUNDARY
!
      CALL BIEF_ALLVEC(1,CHBORD,'CHBORD',IELBU,1,1,MESH)
!
      CALL ALLBLO (VCOEFF,'VCOEFF')
!
      CALL BIEF_ALLVEC(2,VEGLAW,'VEGLAW',IELMU,1,1,MESH)
      IF(FRICTB) THEN
        ALLOCATE(FRTAB%ADR(NZONMX))
        DO I=1,NZONMX
          ALLOCATE(FRTAB%ADR(I)%P)
        ENDDO
        CALL BIEF_ALLVEC(2,KFROPT,'KFROPT',IELMU,1,1,MESH)
        CALL BIEF_ALLVEC(1,NDEFMA,'NDEFMA',IELMU,1,1,MESH)
        IF(VEGETATION) THEN
          CALL BIEF_ALLVEC_IN_BLOCK
     &      (VCOEFF ,15,1,'VCOEFF ',IELMU,1,1,MESH)
!
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK
     &      (VCOEFF ,1,1,'VCOEFF ',0,1,0,MESH)
!
        ENDIF
        CALL BIEF_ALLVEC(1,NDEF_B,'NDEF_B',IELBT,1,1,MESH)
        CALL BIEF_ALLVEC(2,KFRO_B,'KFRO_B',IELBT,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,KFROPT,'KFROPT',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,NDEFMA,'NDEFMA',0,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK
     &      (VCOEFF ,1,1,'VCOEFF ',0,1,0,MESH)
!
        CALL BIEF_ALLVEC(1,NDEF_B,'NDEF_B',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,KFRO_B,'KFRO_B',0,1,0,MESH)
      ENDIF
!
!  END OF DATA FOR FRICTION SET PER ZONE
!
!  ADDITIONAL ARRAY IF THE K-EPSILON MODEL IS USED
!
      IF(ITURB.EQ.3) THEN
        CALL BIEF_ALLVEC(1,AK     ,'AK    ',IELMK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EP     ,'EP    ',IELME,1,1,MESH)
        CALL BIEF_ALLVEC(1,AKN    ,'AKN   ',IELMK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EPN    ,'EPN   ',IELME,1,1,MESH)
        CALL BIEF_ALLVEC(1,AKTILD ,'AKTILD',IELMK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EPTILD ,'EPTILD',IELME,1,1,MESH)
        CALL BIEF_ALLVEC(1,KBOR   ,'KBOR  ',IELBK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EBOR   ,'EBOR  ',IELBE,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,AK     ,'AK    ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EP     ,'EP    ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,AKN    ,'AKN   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EPN    ,'EPN   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,AKTILD ,'AKTILD',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EPTILD ,'EPTILD',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,KBOR   ,'KBOR  ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EBOR   ,'EBOR  ',0,1,0,MESH)
      ENDIF
!
!  ARRAYS FOR SPALART ALLMARAS MODEL
!
      IF(ITURB.EQ.6) THEN
        CALL BIEF_ALLVEC(1,VISCSA ,'VISCSA',IELMNU,1,1,MESH)
        CALL BIEF_ALLVEC(1,NUN    ,'NUN   ',IELMNU,1,1,MESH)
        CALL BIEF_ALLVEC(1,NUTILD ,'NUTILD',IELMNU,1,1,MESH)
        CALL BIEF_ALLVEC(1,WDIST  ,'WDIST ',IELMNU,1,1,MESH)
        CALL BIEF_ALLVEC(1,NUBOR  ,'NUBOR ',IELBNU,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,VISCSA ,'VISCSA',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,NUN    ,'NUN   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,NUTILD ,'NUTILD',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,NUBOR  ,'NUBOR ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,WDIST  ,'WDIST ',0,1,0,MESH)
      ENDIF
!
      CALL BIEF_ALLVEC(1,UDEL   ,'UDEL  ',    IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VDEL   ,'VDEL  ',    IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,DM1    ,'DM1   ',    IELMU,1,2,MESH)
!                                         PIECE-WISE LINEAR DISCONTINUOUS
      CALL BIEF_ALLVEC(1,ZCONV  ,'ZCONV ',       15,1,1,MESH)
      CALL BIEF_ALLVEC(1,FLODEL ,'FLODEL',MESH%NSEG*4**REF,1,0,MESH)
!
      IF(OPT_HNEG.EQ.2) THEN
        CALL BIEF_ALLVEC(1,FLULIM ,'FLULIM',MESH%NSEG*4**REF,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,FLULIM ,'FLULIM',0,1,0,MESH)
      ENDIF
!
      IF(OPT_HNEG.EQ.3) THEN
        CALL BIEF_ALLVEC(1,FLULIMEBE,'LIMEBE',IELM0,3,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,FLULIMEBE,'LIMEBE',0,3,0,MESH)
      ENDIF
!
!     FOR RAIN-EVAPORATION
!
      IF(RAIN.OR.INCLUS(COUPLING,'WAQTEL')) THEN
        CALL BIEF_ALLVEC(1,PLUIE ,'PLUIE ',IELMH,1,1,MESH)
!     RUNOFF-RAINFALL
        CALL BIEF_ALLVEC(1,ACCROF,'ACCROF',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,ACCR  ,'ACCR  ',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,POTMAXRET,'POTMAX',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,IABST ,'IABST ',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,CN    ,'CN    ',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,ZFSLOP,'ZFSLOP',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,FC    ,'FC    ',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,F0    ,'F0    ',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,ACCINF,'ACCINF',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,KS    ,'KS    ',IELMH,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,PLUIE ,'PLUIE ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,ACCROF,'ACCROF',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,ACCR  ,'ACCR  ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,CN    ,'CN    ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZFSLOP,'ZFSLOP',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,POTMAXRET,'POTMAX',0 ,1,0,MESH)
        CALL BIEF_ALLVEC(1,IABST ,'IABST ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,FC    ,'FC    ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,F0    ,'F0    ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,ACCINF,'ACCINF',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,KS    ,'KS    ',0    ,1,0,MESH)
      ENDIF
!
!     VARIABLES TRANSMITTED FROM TOMAWAC TO SISYPHE
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL BIEF_ALLVEC(1,DIRMOY,'DIRMOY',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,COSDIR,'COSDIR',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,SINDIR,'SINDIR',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,HM0   ,'HM0   ',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,TPR5  ,'TPR5  ',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,ORBVEL,'ORBVEL',IELMH,1,1,MESH)
        CALL OS('X=0     ', X=ORBVEL)
      ELSE
        CALL BIEF_ALLVEC(1,DIRMOY,'DIRMOY',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,COSDIR,'COSDIR',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,SINDIR,'SINDIR',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,HM0   ,'HM0   ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,TPR5  ,'TPR5  ',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,ORBVEL,'ORBVEL',0    ,1,0,MESH)
! Cv added
        FRICOU = .FALSE.
      ENDIF
!
!     FOR FINITE VOLUME SECOND ORDER
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CALL BIEF_ALLVEC(1,CORR_I ,'CORR_I',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_J ,'CORR_I',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_ZL,'COR_ZL',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_ZR,'COR_ZR',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_HL,'COR_HL',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_HR,'COR_HR',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_UL,'COR_UL',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_UR,'COR_UR',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_VL,'COR_VL',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,CORR_VR,'COR_VR',NSEG,1,0,MESH)
        CALL BIEF_ALLVEC(1,ALRTPF ,'ALRTPF',4*NSEG,1,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ALLOCATES THE BLOCKS
!
!     FUNCTIONS GATHERED IN A BLOCK FOR TIME N AND A BLOCK FOR TIME N+1
!
      CALL ALLBLO(FN    , 'FN    ')
      CALL ALLBLO(F     , 'F     ')
!
      CALL ADDBLO(FN,UN)
      CALL ADDBLO(FN,VN)
      CALL ADDBLO(FN,HN)
      CALL ADDBLO(F ,U )
      CALL ADDBLO(F ,V )
      CALL ADDBLO(F ,H )
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL ADDBLO(FN   ,TN%ADR(ITRAC)%P   )
          CALL ADDBLO(F    ,T%ADR(ITRAC)%P    )
        ENDDO
      ENDIF
      IF(ITURB.EQ.3) THEN
        CALL ADDBLO(FN    ,AKN)
        CALL ADDBLO(FN    ,EPN)
        CALL ADDBLO(F     ,AK )
        CALL ADDBLO(F     ,EP )
      ENDIF
      IF(ITURB.EQ.6) THEN
        CALL ADDBLO(FN    ,NUN    )
        CALL ADDBLO(F     ,VISCSA )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FUNCTIONS TO ADVECT BY CHARACTERISTICS: STRONG OR WEAK (INDEX 2)
!
!-----------------------------------------------------------------------
!
      CALL ALLBLO(FTILD , 'FTILD ')
      CALL ALLBLO(FNCAR , 'FNCAR ')
      CALL ALLBLO(FTILD2, 'FTILD2')
      CALL ALLBLO(FNCAR2, 'FNCAR2')
!
!     WITH FINITE VOLUMES OR KINETIC SCHEMES ADVECTION IS DONE IN VOLFIN
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!       VELOCITIES
        IF(CONVV(1).AND.ICONVF(1).EQ.ADV_CAR) THEN
          IF(OPTADV_VI.EQ.1) THEN
            CALL ADDBLO(FTILD,UTILD)
            CALL ADDBLO(FTILD,VTILD)
            CALL ADDBLO(FNCAR,UN   )
            CALL ADDBLO(FNCAR,VN   )
          ELSEIF(OPTADV_VI.EQ.2) THEN
            CALL ADDBLO(FTILD2,UTILD)
            CALL ADDBLO(FTILD2,VTILD)
            CALL ADDBLO(FNCAR2,UN   )
            CALL ADDBLO(FNCAR2,VN   )
          ELSE
            WRITE(LU,*) 'POINT_TELEMAC2D : ',OPTADV_VI
            WRITE(LU,*) 'UNKNOWN OPTION OPTADV_VI'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(NTRAC.GT.0.AND.CONVV(3)) THEN
          DO ITRAC=1,NTRAC
            IF(ICONVFT(ITRAC).EQ.ADV_CAR) THEN
              IF(OPTADV_TR(ITRAC).EQ.1) THEN
                CALL ADDBLO(FTILD,TTILD%ADR(ITRAC)%P)
                CALL ADDBLO(FNCAR,TN%ADR(ITRAC)%P)
              ELSEIF(OPTADV_TR(ITRAC).EQ.2) THEN
                CALL ADDBLO(FTILD2,TTILD%ADR(ITRAC)%P)
                CALL ADDBLO(FNCAR2,TN%ADR(ITRAC)%P)
              ELSE
                WRITE(LU,*) 'POINT_TELEMAC2D: ',OPTADV_TR(ITRAC)
                WRITE(LU,*) 'UNKNOWN OPTION OPTADV_TR'
                WRITE(LU,*) 'FOR TRACER ',ITRAC
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!     ALL TURBULENCE MODELS HAVE THE SAME NUMERICAL OPTIONS
      IF(CONVV(4).AND.ITURB.EQ.3.AND.ICONVF(4).EQ.ADV_CAR) THEN
        IF(OPTADV_KE.EQ.1) THEN
          CALL ADDBLO(FTILD,AKTILD)
          CALL ADDBLO(FTILD,EPTILD)
          CALL ADDBLO(FNCAR,AKN   )
          CALL ADDBLO(FNCAR,EPN   )
        ELSEIF(OPTADV_KE.EQ.2) THEN
          CALL ADDBLO(FTILD2,AKTILD)
          CALL ADDBLO(FTILD2,EPTILD)
          CALL ADDBLO(FNCAR2,AKN   )
          CALL ADDBLO(FNCAR2,EPN   )
        ELSE
          WRITE(LU,*) 'POINT_TELEMAC2D : ',OPTADV_KE
          WRITE(LU,*) 'UNKNOWN OPTION OPTADV_KE'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     FOR SA TURBULENCE MODEL
!
      IF(CONVV(4).AND.ITURB.EQ.6.AND.ICONVF(4).EQ.ADV_CAR) THEN
        IF(OPTADV_SA.EQ.1) THEN
          CALL ADDBLO(FTILD, NUTILD)
          CALL ADDBLO(FNCAR, NUN   )
        ELSEIF(OPTADV_SA.EQ.2) THEN
          CALL ADDBLO(FTILD2, NUTILD)
          CALL ADDBLO(FNCAR2, NUN   )
        ELSE
          WRITE(LU,*) 'POINT_TELEMAC2D : ',OPTADV_SA
          WRITE(LU,*) 'UNKNOWN OPTION OPTADV_SA'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS USED TO FOLLOW THE DRIFTS
!
!_______________________________________________________________________
!
!      IF( NDRG_CLSS.GT.0 ) THEN
!
        CALL BIEF_ALLVEC(1,XFLOT ,'XFLOT ',NFLOT_MAX,1,0,MESH)
        CALL BIEF_ALLVEC(1,YFLOT ,'YFLOT ',NFLOT_MAX,1,0,MESH)
        CALL BIEF_ALLVEC(1,SHPFLO,'SHPFLO',
     &                        BIEF_NBPEL(IELM1,MESH)*NFLOT_MAX,1,
     &                        0,MESH)
      IF( NDRG_CLSS.GT.0 ) THEN
!
        CALL BIEF_ALLVEC(3,NODCLSS, 'CLSFLO',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(2,PARCLSS, 'TYPFLO',NFLOT_MAX,1,0,MESH)
        DO I = 1,NFLOT_MAX
          PARCLSS%I(I) = 1
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  ARRAYS USED FOR CULVERTS/SIPHONS
!
      IF(NBUSE.GT.0) THEN
        CALL BIEF_ALLVEC(2,ENTBUS ,'ENTBUS ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(2,SORBUS ,'SORBUS ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,ALTBUS ,'ALTBUS ',NBUSE,2,0,MESH)
        CALL BIEF_ALLVEC(1,CSBUS  ,'CSBUS  ',NBUSE,2,0,MESH)
        CALL BIEF_ALLVEC(1,CEBUS  ,'CEBUS  ',NBUSE,2,0,MESH)
        CALL BIEF_ALLVEC(1,ANGBUS ,'ANGBUS ',NBUSE,2,0,MESH)
        CALL BIEF_ALLVEC(1,LBUS   ,'LBUS   ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,UBUS   ,'UBUS   ',2,NBUSE,0,MESH)
        CALL BIEF_ALLVEC(1,VBUS   ,'VBUS   ',2,NBUSE,0,MESH)
        CALL BIEF_ALLVEC(1,DBUS   ,'DBUS   ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,LRGBUS ,'LRGBUS ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,HAUBUS ,'HAUBUS ',NBUSE,2,0,MESH)
        CALL BIEF_ALLVEC(1,SECBUS ,'SECBUS ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(2,CLPBUS ,'CLPBUS ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,CV     ,'CV     ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,C56    ,'C56    ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,CV5    ,'CV5    ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,C5     ,'C5     ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,CTRASH ,'CTRASH ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,FRICBUS,'FRICBUS',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,LONGBUS,'LONGBUS',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(2,CIRC   ,'CIRC   ',NBUSE,1,0,MESH)
        CALL BIEF_ALLVEC(1,DELBUS ,'DELBUS ',NBUSE,2,0,MESH)
        CALL BIEF_ALLVEC(2,AABUS  ,'AABUS  ',NBUSE,1,0,MESH)
        DO I=1, NBUSE
          DBUS%R(I)=0.D0
          SECBUS%R(I)=1.D0
        ENDDO
        CALL ALLBLO(TBUS ,'TBUS  ')
        IF(NTRAC.GT.0) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(TBUS,NTRAC,1,'TBUS  ',
     &                              NBUSE,2,0,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(TBUS,1    ,1,'TBUS  ',
     &                              NBUSE,2,0,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC(2,ENTBUS ,'ENTBUS ',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,SORBUS ,'SORBUS ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ALTBUS ,'ALTBUS ',0,2,0,MESH)
        CALL BIEF_ALLVEC(1,CSBUS  ,'CSBUS  ',0,2,0,MESH)
        CALL BIEF_ALLVEC(1,CEBUS  ,'CEBUS  ',0,2,0,MESH)
        CALL BIEF_ALLVEC(1,ANGBUS ,'ANGBUS ',0,2,0,MESH)
        CALL BIEF_ALLVEC(1,LBUS   ,'LBUS   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,UBUS   ,'UBUS   ',2,0,0,MESH)
        CALL BIEF_ALLVEC(1,VBUS   ,'VBUS   ',2,0,0,MESH)
        CALL BIEF_ALLVEC(1,DBUS   ,'DBUS   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,LRGBUS ,'LRGBUS ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,HAUBUS ,'HAUBUS ',0,2,0,MESH)
        CALL BIEF_ALLVEC(1,SECBUS ,'SECBUS ',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,CLPBUS ,'CLPBUS ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,CV     ,'CV     ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,C56    ,'C56    ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,CV5    ,'CV5    ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,C5     ,'C5     ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,CTRASH ,'CTRASH ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,FRICBUS,'FRICBUS',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,LONGBUS,'LONGBUS',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,CIRC   ,'CIRC   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,DELBUS ,'DELBUS ',0,2,0,MESH)
        CALL BIEF_ALLVEC(2,AABUS  ,'AABUS  ',0,1,0,MESH)
        CALL ALLBLO(TBUS ,'TBUS  ')
        IF(NTRAC.GT.0) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(TBUS,NTRAC,1,'TBUS  ',
     &                              0,2,0,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(TBUS,1    ,1,'TBUS  ',
     &                              0,2,0,MESH)
        ENDIF
      ENDIF
!
!=======================================================================
!     VARIABLES FOR SECONDARY CURRENTS
!-----------------------------------------------------------------------
!
      IF(SECCURRENTS) THEN
        CALL BIEF_ALLVEC(1,SEC_TAU,'SECTAU',IELMH,1,1,MESH)
        CALL BIEF_ALLVEC(1,SEC_R  ,'SEC_R ',IELMH,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SEC_TAU,'SECTAU',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,SEC_R  ,'SEC_R ',0    ,1,0,MESH)
      ENDIF
!
!=======================================================================
!
!-----------------------------------------------------------------------
!
!  ARRAYS AT THE USER'S DISPOSAL
!
      CALL ALLBLO(PRIVE ,'PRIVE ')
!
      IF(NPRIV.GT.0) THEN
!       THESE ARRAYS MUST EXIST BUT CAN BE EMPTY
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,NPRIV,1,'PRIV  ',IELMX,1,2,MESH)
      ENDIF
!     AT LEAST 4 ARRAYS ARE REQUIRED BUT THEY CAN BE EMPTY
      IF(NPRIV.LT.4) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,4-NPRIV,1,'PRIV  ',0,1,2,MESH)
      ENDIF
!
!     ALIAS FOR THE FIRST 4 'PRIVE' ARRAYS
!
      PRIVE1 => PRIVE%ADR(1)%P%R
      PRIVE2 => PRIVE%ADR(2)%P%R
      PRIVE3 => PRIVE%ADR(3)%P%R
      PRIVE4 => PRIVE%ADR(4)%P%R
!
!  BLOCK OF THE CLANDESTINE VARIABLES
!
      CALL ALLBLO(VARCL,'VARCL ')
      CALL BIEF_ALLVEC_IN_BLOCK(VARCL,NVARCL,1,'CL    ',IELMX,1,2,MESH)
!
!     INITIALISES AT 0
!
!
      IF(NVARCL.GT.0) THEN
        DO I=1,NVARCL
          CALL OS('X=C     ',X=VARCL%ADR(I)%P, C=0.D0)
        ENDDO
      ENDIF
!
!_______________________________________________________________________
!
!                         * INTEGER ARRAYS *
!_______________________________________________________________________
!
      IF(MSK) THEN
        CALL BIEF_ALLVEC(2,IFAMAS,'IFAMAS',
     &                   IELM0,BIEF_NBFEL(IELM0,MESH),1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,IFAMAS,'IFAMAS',0,1,0,MESH)
      ENDIF
      CALL BIEF_ALLVEC(2,LIUBOR,'LIUBOR',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIVBOR,'LIVBOR',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIHBOR,'LIHBOR',IELBH,1,1,MESH)
!     CLU, CLV AND CLH ARE WORKING ARRAYS IN PROPIN
      CALL BIEF_ALLVEC(2,CLU            ,'CLU   ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,CLV            ,'CLV   ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,CLH            ,'CLH   ',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(2,BOUNDARY_COLOUR,'BNDCOL',IELB1,1,1,MESH)
!
      CALL BIEF_ALLVEC(2,NUMLIQ,'NUMLIQ',IELB1,1,1,MESH)
      IF(ITURB.EQ.3) THEN
        CALL BIEF_ALLVEC(2,LIMKEP,'LIMKEP',IELB1,2,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,LIMKEP,'LIMKEP',    0,2,0,MESH)
      ENDIF
      IF(ITURB.EQ.6) THEN
        CALL BIEF_ALLVEC(2,LIMSA,'LIMSA ',IELB1,2,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,LIMSA,'LIMSA ',    0,2,0,MESH)
      ENDIF
      CALL BIEF_ALLVEC(2,LIMPRO,'LIMPRO',MAX(IELBH,IELBU),6,1,MESH)
      CALL BIEF_ALLVEC(2,LIMTRA,'LIMTRA',IELBT,1,1,MESH)
      CALL BIEF_ALLVEC(2,SECMOU,'SECMOU',IELM0,1,1,MESH)
!
!     INTEGER WORKING ARRAY (MINIMUM SIZE NELEM)
!
      SIZ=MAX(BIEF_NBMPTS(IELMX,MESH),BIEF_NBMPTS(10,MESH),NFLOT_MAX)
      IF(YESWEAK) SIZ=MAX(SIZ,BIEF_NBMPTS(10,MESH)*NGAUSS)
!
      CALL BIEF_ALLVEC(2,IT1,'IT1   ',SIZ,1,0,MESH)
      CALL BIEF_ALLVEC(2,IT2,'IT2   ',SIZ,1,0,MESH)
      CALL BIEF_ALLVEC(2,IT3,'IT3   ',SIZ,1,0,MESH)
      CALL BIEF_ALLVEC(2,IT4,'IT4   ',SIZ,1,0,MESH)
!
!_______________________________________________________________________
!
!  ARRAYS USED TO FOLLOW THE DRIFTS
!
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(2,DEBFLO,'DEBFLO',NFLOT_MAX,1,0,MESH)
      CALL BIEF_ALLVEC(2,FINFLO,'FINFLO',NFLOT_MAX,1,0,MESH)
      CALL BIEF_ALLVEC(2,ELTFLO,'ELTFLO',NFLOT_MAX,1,0,MESH)
      CALL BIEF_ALLVEC(2,TAGFLO,'TAGFLO',NFLOT_MAX,1,0,MESH)
      CALL BIEF_ALLVEC(2,CLSFLO,'CLSFLO',NFLOT_MAX,1,0,MESH)
!
!_______________________________________________________________________
!
!  ARRAYS USED FOR LAGRANGIAN DRIFTS
!
!-----------------------------------------------------------------------
!
      IF(NLAG.NE.0) THEN
        CALL BIEF_ALLVEC(2,DEBLAG,'DEBLAG',NLAG      ,1,0,MESH)
        CALL BIEF_ALLVEC(2,FINLAG,'FINLAG',NLAG      ,1,0,MESH)
        CALL BIEF_ALLVEC(2,ELTLAG,'ELTLAG',NLAG*NPOIN*7**REF,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,DEBLAG,'DEBLAG',0         ,1,0,MESH)
        CALL BIEF_ALLVEC(2,FINLAG,'FINLAG',0         ,1,0,MESH)
        CALL BIEF_ALLVEC(2,ELTLAG,'ELTLAG',0         ,1,0,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS USED FOR THE ZONE NUMBERS
!
!-----------------------------------------------------------------------
!
      IF(DEFZON) THEN
        CALL BIEF_ALLVEC(2,ZONE,'ZONE  ',IELM1,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,ZONE,'ZONE  ',0    ,1,0,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS FOR SOURCES REGIONS
!_______________________________________________________________________
!
      IF(ALLOCATED(XCOO)) THEN
        DEALLOCATE(XCOO)
        DEALLOCATE(YCOO)
        DEALLOCATE(PTS_REG)
        DEALLOCATE(AREA_P)
        DEALLOCATE(TNP)
        DEALLOCATE(PT_IN_POLY)
      ENDIF
      IF(T2D_FILES(T2DSDN)%NAME(1:1).NE.' ') THEN
        ALLOCATE(XCOO(MAXPTSCE,MAXSCE))
        ALLOCATE(YCOO(MAXPTSCE,MAXSCE))
        ALLOCATE(PTS_REG(MAXPTSCE))
        ALLOCATE(AREA_P(MAXSCE))
        ALLOCATE(TNP(MAXSCE))
        ALLOCATE(PT_IN_POLY(MAXSCE,NPOIN))
      ELSE
        ALLOCATE(XCOO(1,1))
        ALLOCATE(YCOO(1,1))
        ALLOCATE(PTS_REG(1))
        ALLOCATE(AREA_P(1))
        ALLOCATE(TNP(1))
        ALLOCATE(PT_IN_POLY(1,1))
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS NOT COMMON TO ALL TYPES OF SOLVED EQUATIONS
!_______________________________________________________________________
!
      CALL ALLBLO(SMTR     ,'SMTR  ')
      CALL ALLBLO(FLUXT    ,'FLUXT ')
      CALL ALLBLO(FLUXT_OLD,'FLUTOL')
      CALL ALLBLO(FLUHTEMP ,'FLUHTE')
      CALL ALLBLO(HT       ,'HT    ')
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CALL BIEF_ALLVEC(1,QU       ,'QU    ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,QV       ,'QV    ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,HSTOK    ,'HSTOK ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,HCSTOK   ,'HCSTOK',2    ,MESH%NSEG,0,MESH)
        CALL BIEF_ALLVEC(2,LOGFR    ,'LOGFR ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,HC       ,'HC    ',2    ,
     &                   MESH%NSEG*4**REF,0,MESH)
        CALL BIEF_ALLVEC(1,DSZ      ,'DSZ   ',2    ,MESH%NSEG*4**REF,
     &                   0,MESH)
        CALL BIEF_ALLVEC(1,FLUX_OLD ,'FLUOLD',IELM1,3        ,1,MESH)
        CALL BIEF_ALLVEC(2,NEISEG   ,'NEISEG',2    ,
     &                   MESH%NSEG*4**REF,0,MESH)
        CALL BIEF_ALLVEC(1,FLUX,'FLUX  ',6*NPOIN+1,1,0,MESH)
        IF(NTRAC.GT.0) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXT    ,NTRAC,1,'FLUXT ',
     &                              MESH%NSEG*4**REF,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXT_OLD,NTRAC,1,'FLUTOL',
     &                              MESH%NSEG*4**REF,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUHTEMP ,NTRAC,1,'FLUHTE',
     &                              MESH%NSEG*4**REF,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(HT       ,NTRAC,1,'HT    ',
     &                              IELM1    ,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(SMTR     ,NTRAC,1,'SMTR  ',
     &                              IELM1    ,1,1,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXT    ,1,1,'FLUXT ',0,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXT_OLD,1,1,'FLUTOL',0,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUHTEMP ,1,1,'FLUHTE',0,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(HT       ,1,1,'HT    ',0,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(SMTR     ,1,1,'SMTR  ',0,1,1,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC(1,QU       ,'QU    ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,QV       ,'QV    ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,HSTOK    ,'HSTOK ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,HCSTOK   ,'HCSTOK',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(2,LOGFR    ,'LOGFR ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,HC       ,'HC    ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,DSZ      ,'DSZ   ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(FLUXT    ,1,1,'FLUXT ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(FLUXT_OLD,1,1,'FLUTOL',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(FLUHTEMP ,1,1,'FLUHTE',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(HT       ,1,1,'HT    ',0,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(SMTR     ,1,1,'SMTR  ',0,1,1,MESH)
        CALL BIEF_ALLVEC(1,FLUX_OLD ,'FLUOLD',0 , 1,0,MESH)
      ENDIF
!
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        CALL BIEF_ALLVEC(1,H0  ,'H0    ',IELMH,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,H0  ,'H0    ',0    ,1,0 ,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!    FOR MAX FREE SURFACE ELEVATION, MAX SPEEDS
!    AND CORRESPONDING TIMES
!
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        CALL BIEF_ALLVEC(1,MAXZ,'MAXZ  ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,MAXZ,'MAXZ  ',0    ,1,0 ,MESH)
      ENDIF
      IF(SORLEO(28).OR.SORIMP(28)) THEN
        CALL BIEF_ALLVEC(1,TMAXZ,'TMAXZ ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TMAXZ,'TMAXZ ',0    ,1,0 ,MESH)
      ENDIF
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        CALL BIEF_ALLVEC(1,MAXV,'MAXV  ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,MAXV,'MAXV  ',0    ,1,0 ,MESH)
      ENDIF
      IF(SORLEO(30).OR.SORIMP(30)) THEN
        CALL BIEF_ALLVEC(1,TMAXV,'TMAXV ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TMAXV,'TMAXV ',0    ,1,0 ,MESH)
      ENDIF
!
!    FOR FOURIER ANALYSES
!
      CALL ALLBLO(AMPL,'AMPL  ')
      CALL ALLBLO(PHAS,'PHAS  ')
      IF(NPERIAF.GT.0) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(AMPL,NPERIAF,1,'AMPL  ',
     &                            IELM1,1,2,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(PHAS,NPERIAF,1,'PHAS  ',
     &                            IELM1,1,2,MESH)
      ENDIF


!-----------------------------------------------------------------------
!
! NEW TELEMAC TO TOMAWAC COUPLING
!                                                       WAC2

      NVARTOM2TEL = 0
      NVARTEL2TOM = 0
      IF(INCLUS(COUPLING,'TOMAWAC2')) THEN

        ! SENDING VARIABLES TO TOMAWAC
        CALL ALLBLO(TEL2TOM ,'TEL2TO')
        CALL ADDBLO(TEL2TOM,U)
        CALL ADDBLO(TEL2TOM,V)
        CALL ADDBLO(TEL2TOM,H)
        NVARTEL2TOM = 3
        ! RECEIVING VARIABLES FROM TOMAWAC

        CALL ALLBLO(TOM2TEL ,'TOM2TE')
        NVARTOM2TEL = 0
        IF (INCLUS(COUPLING,'SISYPHE').OR.INCLUS(COUPLING,'GAIA')) THEN
          CALL ADDBLO(TOM2TEL,COSDIR)
          CALL ADDBLO(TOM2TEL,SINDIR)
          CALL ADDBLO(TOM2TEL,HM0)
          CALL ADDBLO(TOM2TEL,TPR5)
          CALL ADDBLO(TOM2TEL,ORBVEL)
          NVARTOM2TEL = NVARTOM2TEL + 5
        ENDIF
        IF(COUROU) THEN
          CALL ADDBLO(TOM2TEL,FXWAVE)
          CALL ADDBLO(TOM2TEL,FYWAVE)
          NVARTOM2TEL = NVARTOM2TEL + 2
        ENDIF
        ! WIND CHECK IS THIS IS POSSIBLE
        IF (VENT) THEN
          CALL ADDBLO(TEL2TOM,WINDX)
          CALL ADDBLO(TEL2TOM,WINDY)
          NVARTEL2TOM = NVARTEL2TOM + 2
        ENDIF
      ENDIF


!
!-----------------------------------------------------------------------
!
! COMPUTES THE BLOCK WHICH CONNECTS A VARIABLE NAME
! TO ITS ARRAY
!
      CALL ALLBLO(VARSOR ,'VARSOR')
! 01
      CALL ADDBLO(VARSOR,U)
! 02
      CALL ADDBLO(VARSOR,V)
! 03
      CALL ADDBLO(VARSOR,FU)
! 04
      CALL ADDBLO(VARSOR,H)
! 05
      CALL ADDBLO(VARSOR,FV)
! 06
      CALL ADDBLO(VARSOR,ZF)
! 07
      CALL ADDBLO(VARSOR,T2)
! 08
      CALL ADDBLO(VARSOR,T3)
! 09  OLD TRACER
!     REPEATED HERE BUT NOT USED; MOVED ELSEWHERE
      CALL ADDBLO(VARSOR,T%ADR(1)%P)
! 10
      CALL ADDBLO(VARSOR,AK)
! 11
      CALL ADDBLO(VARSOR,EP)
! 12
      IF(ITURB.EQ.2) THEN
!       WITH ELDER MODEL THE LONGITUDINAL VISCOSITY WILL BE GIVEN
!       INSTEAD OF THE VISCOSITY, T10 BUILT IN PRERES_TELEMAC2D
        CALL ADDBLO(VARSOR,T10)
      ELSE
        CALL ADDBLO(VARSOR,VISC)
      ENDIF
! 13
      CALL ADDBLO(VARSOR,T4)
! 14
      CALL ADDBLO(VARSOR,T5)
! 15
      CALL ADDBLO(VARSOR,T6)
! 16
      CALL ADDBLO(VARSOR,WINDX)
! 17
      CALL ADDBLO(VARSOR,WINDY)
! 18
      CALL ADDBLO(VARSOR,PATMOS)
! 19
      CALL ADDBLO(VARSOR,CHESTR)
! 20
      CALL ADDBLO(VARSOR,T7)
! 21
      CALL ADDBLO(VARSOR,T8)
! 22
      CALL ADDBLO(VARSOR,T9)
! 23
      CALL ADDBLO(VARSOR,PRIVE%ADR(1)%P)
! 24
      CALL ADDBLO(VARSOR,PRIVE%ADR(2)%P)
! 25
      CALL ADDBLO(VARSOR,PRIVE%ADR(3)%P)
! 26
      CALL ADDBLO(VARSOR,PRIVE%ADR(4)%P)
! 27
      CALL ADDBLO(VARSOR,MAXZ)
! 28
      CALL ADDBLO(VARSOR,TMAXZ)
! 29
      CALL ADDBLO(VARSOR,MAXV)
! 30
      CALL ADDBLO(VARSOR,TMAXV)
! 31  FRICTION VELOCITY
      CALL ADDBLO(VARSOR,T7)
! 32  FOR SECONDARY CURRENTS
      CALL ADDBLO(VARSOR,SEC_TAU)
! 33  FOR SECONDARY CURRENTS
      CALL ADDBLO(VARSOR,SEC_R)
! 34  WALL DISTANCE
      CALL ADDBLO(VARSOR,WDIST)
! 35  REFERENCE LEVEL FOR NESTOR
      CALL ADDBLO(VARSOR,ZRL)
! 36  INCREMENT OF H
      CALL ADDBLO(VARSOR,DH)
! 37  INCREMENT OF U
      CALL ADDBLO(VARSOR,DU)
! 38  INCREMENT OF V
      CALL ADDBLO(VARSOR,DV)
! 39  INCREMENT OF HN
      CALL ADDBLO(VARSOR,DHN)
! 40  SPALART ALLMARAS VISCOTIY
      CALL ADDBLO(VARSOR,VISCSA)
!
!     THE LAST RANK IN VARSOR (SO FAR ADR_TRAC)
!
      IF(NVAR_T2D.NE.VARSOR%N) THEN
        WRITE(LU,*) 'MESSAGE TO DEVELOPPERS:'
        WRITE(LU,*) 'NVAR_T2D DIFFERENT THAN VARSOR SIZE ',VARSOR%N
        WRITE(LU,*) 'IN POINT_TELEMAC2D'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     TRACERS
!
      J=ILAST
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL ADDBLO(VARSOR,T%ADR(ITRAC)%P)
        ENDDO
!       SIZE COUNTER INCREMENTATION
        J = J+NTRAC
      ENDIF
!
!     FOURIER ANALYSIS
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
!         OUTPUT VARIABLES (TO BE CHECKED)
          SORLEO(J+2*(I-1)+1)=.TRUE.
          SORLEO(J+2*(I-1)+2)=.TRUE.
!         END OF OUTPUT VARIABLES (TO BE CHECKED)
          CALL ADDBLO(VARSOR,AMPL%ADR(I)%P)
          CALL ADDBLO(VARSOR,PHAS%ADR(I)%P)
        ENDDO
!       SIZE COUNTER INCREMENTATION
        J = J+2*NPERIAF
      ENDIF
!
!     CLANDESTINE VARIABLES
!
!      J = ILAST+NTRAC+2*NPERIAF
      IF(VARCL%N.NE.0) THEN
        DO I=1,VARCL%N
          CALL ADDBLO(VARSOR,VARCL%ADR(I)%P)
          SORLEO(J+I)=.TRUE.
          TEXTE(J+I)=VARCLA(I)
        ENDDO
!       SIZE COUNTER INCREMENTATION
        J = J+VARCL%N
      ENDIF
!
!
!     BLOCK OF DIFFERENTIATED VARIABLES
!     ARRAYS AD1, AD2, ... MUST EXIST BUT WILL
!     ONLY BE INTIALISED BY THE AD USER SUBROUTINES AD_GET_TELEMAC2D
!
!     DIFFERENTIATED VARIABLES
!
      IF( NADVAR.GT.0 ) THEN
        CALL ALLBLO(ADVAR ,'ADVAR ')
        CALL BIEF_ALLVEC_IN_BLOCK(ADVAR,NADVAR,1,'AD    ',IELMX,
     &                            1,2,MESH)
!
        IF( J+NADVAR.GT.MAXVAR ) THEN
          WRITE(LU,*) 'POINT : TOO MANY DERIVATIVES TO PRINT OUT'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO I = 1,NADVAR
          ADVAR%ADR(I)%P%R = 0.D0
          CALL AD_GET_TELEMAC2D(I,ADVAR%ADR(I)%P)
          IF( SORLEO(J+I).OR.SORIMP(J+I) ) THEN
            CALL ADDBLO(VARSOR,ADVAR%ADR(I)%P)
          ENDIF
        ENDDO
!
!       SIZE COUNTER INCREMENTATION
        J = J+NADVAR
      ENDIF
!
!     METEO MODULE
!
      CALL POINT_METEO(T2D_FILES,T2ATMA,T2ATMB,MESH,IELMT,VENT,ATMOS,
     &                 INCLUS(COUPLING,'WAQTEL'),
     &                 INCLUS(COUPLING,'KHIONE'),FREE_ATMO)
!
!=======================================================================
!
! WRITES OUT TO LISTING :
!
      IF(LISTIN) THEN
        WRITE(LU,23)
      ENDIF
23    FORMAT(1X,///,21X,'*************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
     &21X,              '*************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
