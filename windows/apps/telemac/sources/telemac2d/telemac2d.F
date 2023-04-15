!                   ********************
                    SUBROUTINE TELEMAC2D
!                   ********************
!
     &(PASS,ATDEP,NITER,CODE,DTDEP,NEWTIME,DOPRINT,NITERORI,
     & CONVERGENCE_LEVEL)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    SOLVES THE SAINT-VENANT EQUATIONS FOR U,V,H.
!+
!+            ADJO = .TRUE.  : DIRECT MODE
!+
!+            ADJO = .FALSE. : ADJOINT MODE
!
!history
!+        06/06/2008
!+
!+   OPTIONAL ARGUMENT BOUNDARY_COLOUR ADDED TO LECLIM
!
!history
!+        16/06/2008
!+
!+   SECOND CALL TO PROPIN FOLLOWING CALL TO BORD
!
!history
!+        25/06/2008
!+
!+   DIFFIN2 RENAMED DIFFIN + ARGUMENT MESH
!
!history
!+        27/06/2008
!+
!+   ARGUMENTS OF PROPIN_TELEMAC2D : MESH ADDED TO THE END
!
!history
!+        29/07/2008
!+
!+   ADDED CALL TO FLUSEC BEFORE THE 1ST CALL PRERES
!
!history
!+        13/08/2008
!+
!+   CHANGED CALL AND CALL CONDITIONS TO CHARAC
!
!history
!+        20/08/2008
!+
!+   LIST_PTS MODIFIED IN PARALLEL
!
!history
!+        02/09/2008
!+
!+   CALL TO MODIFIED TEL4DEL (ADDED VELOCITY AND DIFFUSION)
!
!history
!+        25/09/2008
!+
!+   CALL TO MODIFIED TEL4DEL (FLUXES SENT THRU MESH%W%R)
!
!history
!+        21/10/2008
!+
!+   CALL TO MODIFIED MASKTO (PARALLEL VERSION OF MASKTO)
!
!history
!+        09/02/2009
!+
!+   IF H CLIPPED, USES HMIN INSTEAD OF 0.D0
!
!history
!+        16/02/2009
!+
!+   CALL TO POSITIVE_DEPTHS
!
!history
!+        19/02/2009
!+
!+   H CLIPPED IN CASE OF COMPUTATION CONTINUED
!
!history
!+        02/04/2009
!+
!+   NEW FILE STRUCTURE T2D_FILES AND MED FORMAT
!
!history
!+        09/07/2009
!+
!+   ARGUMENT NPTFR2 ADDED TO LECLIM
!
!history
!+        20/07/2009
!+
!+   1 OUT OF 3 CALLS TO TEL4DEL REMOVED (THANKS TO A
!
!history
!+        22/07/2009
!+
!+   3 NEW ARGUMENTS IN PROPAG
!
!history  J-M HERVOUET (LNHE)
!+        25/11/2009
!+        V6P0
!+   VERSION WITH MULTIPLE TRACERS
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
!+        19/04/2011
!+        V6P1
!+   SECOND CALL TO SISYPHE MOVED AT THE END OF THE TIME LOOP SO THAT
!+   A CORRECT CONTINUITY EQUATION CAN BE SENT EVEN AT THE FIRST TIME
!+   STEP (H, HN, USIS, VSIS, DM1, ZCONV COMPATIBLE)
!
!history  J-M HERVOUET (LNHE)
!+        19/05/2011
!+        V6P1
!+   NEW THOMPSON THEORY, THAT WORKS ALSO IN PARALLEL
!
!history  J-M HERVOUET (LNHE)
!+        09/08/2011
!+        V6P1
!+   Call to lecsng changed
!
!history  C.COULET (ARTELIA)
!+        23/05/2012
!+        V6P2
!+   Modification for adding "bridge" file and separation of weirs and
!+   culvert files.
!
!history  J-M HERVOUET (LNHE)
!+        16/07/2012
!+        V6P1
!+   Call to TEL4DEL modified.
!
!history  P. CHASSE (CETMEF) / C.COULET (ARTELIA)
!+        03/08/2012
!+        V6P2
!+   Modification for adding breaches management during simulation
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        12/02/2013
!+        V6P1
!+   Call to FLOT and DERIVE modified, call to SORFLO removed.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/03/2013
!+        V6P3
!+   Call to METEO modified.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        22/03/2013
!+        V6P3
!+   Call to WAC and SISYPHE modified.
!
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the file format in calls to FIND_IN_SEL.
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Modification for new treatment of weirs
!
!history  A. JOLY (EDF R&D, LNHE)
!+        15/07/2013
!+        V6P3
!+   Allocating algae variables and initialising them for the next
!+   time step
!
!history R.ATA (EDF R&D, LNHE)
!+        10/10/2013
!+        V6P3
!+   FORCING LISTING AND GRAPHIC OUTPUTS FOR LAST TIME STEP, FOR FV
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/12/2013
!+        V7P0
!+   Initialisation of YAFLODEL added (overlooked bug?).
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        02/01/2014
!+        V7P0
!+   Removing a use of KNOGL. KNOGL suppressed in call to
!+   flusec_telemac2d.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        31/03/2014
!+        V7P0
!+   1) Now written to enable different numbering of boundary points and
!+      boundary segments.
!+   2) Incident wave suppressed.
!+   3) Different advection schemes for different tracers allowed.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        30/04/2014
!+        V7P0
!+   Now 2 calls to charac
!+   one for strong and one for weak characteristics.
!+   Second call to DIFFIN: U and V replaced by UCONV and VCONV.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/06/2014
!+        V7P0
!+   LIMTRA replaced by a copy in the call to cvdftr (some schemes may
!+   change it and it may cause problems for the next tracers)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        05/08/2014
!+        V7P0
!+   Initialisation of PRIVE move before call to FONSTR (it could be
!+   used in FONSTR and wrongly cancelled by initialisation).
!
!history  C VILLARET (HRW+EDF) & J-M HERVOUET (EDF - LNHE)
!+        18/09/2014
!+        V7P0
!+   Calls to sisyphe and wac changed.
!
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction:
!+   add the calculation of \Omega
!
!history  R. ATA (EDF LAB, LNHE)
!+        05/11/2014
!+        V7P0
!+   add optional variables to meteo in a sake of harmonization
!+   with telemac-3d
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/05/2015
!+        V7P1
!+   ALIRE variable was wrong for tracers, they now begin at rank 34,
!+   so ALIRE(33+ITRAC)=1. After a remark by Noemie Durand.
!
!history Y. AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P1
!+   Modification to comply with the hermes module.
!
!history R. ATA (LNHE)
!+       27/05/2015
!+       V7P1
!+   UDEL and VDEL built for Delwaq in finite element options.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/05/2015
!+        V7P1
!+   Call to CVDFTR modified. 3 new arguments.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        18/09/2015
!+        V7P1
!+   Call to TEL4DEL modified. Printouts in Debug mode added. Argument
!+   NREJTR changed into NREJET in the call to difsou.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        14/03/2016
!+        V7P2
!+   Adding the argument HPROP in the call to SISYPHE. TETAHC removed
!+   and replaced everywhere by TETAC. HTILD replaced with HPROP in the
!+   call to CVDFTR.
!+   Call to COSAKE moved at the beginning.
!+   Call to FRICTION_CHOICE now without KARMAN.
!+   Call to KEPSIL modified.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/05/2016
!+        V7P2
!+   FLBOR initialised before the first call to bilan.f, for the sake of
!+   computations continued with stage-discharge curves.
!
!history  C.COULET (ARTELIA)
!+        01/09/2016
!+        V7P2
!+   Call lecsng splitted in 2 according to typseuil
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        243/09/2016
!+        V7P2
!+   MASS_RAIN set to 0.D0 before first call to BILANT. Local variables
!+   CHARR and SUSP removed.
!
!history R. ATA (LNHE)
!+       27/09/2016
!+       V7P2
!+   add new turbulence model of Spalart-Allmaras
!
!history S.E.BOURBAN (HRW))
!+       01/01/2017
!+       V7P2
!+   Adding differentiated variables
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        19/11/2016
!+        V7P3
!+  Coupling TELEMAC-2D with KHIONE (ice modelling component)
!+  including the management of separate result files.
!
!history B. GLANDER (BAW)
!+       28/11/2017
!+       V7P2
!+   add coupling with Nestor
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!param atdep     [in] starting time when called for coupling
!param code      [in] calling program (if coupling)
!param doprint   [in] for overwriting the keyword on listing
!param dtdep     [in] time step to use when coupling with estel-3d
!param newtime   [in] are we starting a new time step or just iterating?
!+                    this is for coupling with estel-3d
!param niter     [in] number of iterations when called for coupling
!param pass      [in] -1 : all steps
!+                     0 : only initialisation
!+                     1 : only time-steps steps
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE COUPLE_MOD
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_GAIA, ONLY:CSRATIO,HN_GAI,HOLD,UCONV_GAI,
     &  VCONV_GAI,SOLSYS_GAI,FLUER_ADV,FLUDPT_ADV,SAVE_UCONV,SAVE_VCONV,
     &  FLUDPT,FLUDP,FLULIM_GAI,YAFLULIM_GAI,NSUSP_TEL
      USE METEO_TELEMAC !, ONLY: WINDX,WINDY,PATMOS
      USE DECLARATIONS_WAQTEL, ONLY: WAQPROCESS,IND_SF,IND_CSF,IND_CSF2,
     &                               KIN_MICROPOL
      USE INTERFACE_TELEMAC2D, EX_TELEMAC2D => TELEMAC2D
      USE INTERFACE_SISYPHE, ONLY: SISYPHE
      USE INTERFACE_GAIA,    ONLY: GAIA_STEP
      USE INTERFACE_TOMAWAC, ONLY: WAC
      USE INTERFACE_KHIONE
      USE GRACESTOP
      USE FRICTION_DEF
!     MODULE SPECIFIC TO COUPLING WITH ESTEL-3D
      USE M_COUPLING_ESTEL3D
!     OIL SPILL MODEL
      USE OILSPILL
!     ALGAE MODEL
      USE ALGAE_TRANSP
!     DELWAQ
      USE TEL4DEL, ONLY: TEL4DELWAQ
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: PASS,NITER
      DOUBLE PRECISION, INTENT(IN) :: ATDEP
      CHARACTER(LEN=*), INTENT(IN) :: CODE
!     TIME STEP TO USE WHEN COUPLING WITH ESTEL-3D
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: DTDEP
!     ARE WE STARTING A NEW TIME STEP OR JUST ITERATING?
      LOGICAL,          INTENT(IN), OPTIONAL :: NEWTIME
!     DO WE WANT TELEMAC2D TO OUTPUT IN THE LISTING OR NOT?
      LOGICAL,          INTENT(IN), OPTIONAL :: DOPRINT
      INTEGER,          INTENT(IN), OPTIONAL :: NITERORI
      INTEGER,          INTENT(IN), OPTIONAL :: CONVERGENCE_LEVEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! Both init and time loop
!
! INTEGERS
!
      INTEGER I,ISOUSI,STOP2,DISCLIN,ISUSP
      INTEGER IELMX, IERR
!
! REAL SCALARS
!
      DOUBLE PRECISION CFLMAX,RELAX
      DOUBLE PRECISION MODVIT
      DOUBLE PRECISION, PARAMETER :: RAD2DEG = 90.0D0/ACOS(0.0D0)
!
! LOGICALS
!
      LOGICAL INFOGS,INFOGT,YASMH
      LOGICAL CHARR_TEL,SUSP1
      LOGICAL YASMO,YASIS,YAGAI
!
!
!-----------------------------------------------------------------------
!
      INTEGER IMAX,ITRAC,IVAR
!
!-----------------------------------------------------------------------
!
! ADDED FOR KINETIC SCHEMES
!
      DOUBLE PRECISION SVOID(1)
!
!-----------------------------------------------------------------------
!
!     FOR SISYPHE : GRAIN FEEDING AND CONSTANT FLOW DISCHARGE
      INTEGER :: ISIS_CFD
!
      INTEGER :: OLD_LEOPRD
!
!-----------------------------------------------------------------------
!
      INTRINSIC MAX

      IELMX = MAX(IELMH,IELMU,IELMT,IELMK,IELME)
!
!-----------------------------------------------------------------------
!
!  VARIABLES TO READ IN THE EVENT OF A CONTINUATION:
!  0 : DISCARD    1 : READ  (SEE SS-PG NOMVAR)
!
!                                 0: OLD PLACE FOR THE TRACER
!
!-----------------------------------------------------------------------
!
#if defined(COMPAD)
      CALL AD_TELEMAC2D_BEGIN
#endif
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(NITERORI)) THEN
        TOTAL_ITER = NITERORI
      ELSE
        TOTAL_ITER = NIT
      ENDIF
      CHARR_TEL=.FALSE.
      SIS_CPL%CHARR=.FALSE.
      SIS_CPL%SUSP=.FALSE.
      DISCLIN=11
!!      NFLOT=0
!
!-----------------------------------------------------------------------
!
!     SET REF_T2D VALUE FOR CONVERGENCE STUDIES
      REF_T2D = 0
      IF (PRESENT(CONVERGENCE_LEVEL)) THEN
        REF_T2D = CONVERGENCE_LEVEL
      ENDIF
!
!-----------------------------------------------------------------------
!     LOGICAL FOR LISTING AND GRAPHICAL OUTPUT
      IMP=.FALSE.
      LEO=.FALSE.
      YAGAI=.FALSE.
      YASIS=.FALSE.
      YASMH=.FALSE.
      IF(PASS.LE.0) COMPLEO=0
!
!-----------------------------------------------------------------------
!
!     INITIALISATION OF CONSTANTS FOR K-EPSILON, + KARMAN+ SA
!
      CALL COSAKE(KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,
     &            ESTAR,SCHMIT,KMIN,KMAX,EMIN,EMAX)
!
      IF(ITURB.EQ.3) THEN
!       WILL HAVE TO INITIALISE K AND EPSILON
        AKEP = .TRUE.
      ELSEIF(ITURB.EQ.6) THEN
!       WILL HAVE TO INTIALISE SPALART-ALLMARAS
        SA=.TRUE.
        CALL COSASA(SIGMANU,NUMIN,NUMAX)
      ELSE
!       SHOULD NOT INITIALISE K NOR EPSILON NEITHER SA
        AKEP = .FALSE.
        SA   = .FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR TAKING INTO ACCOUNT FLUX LIMITATION OF ARRAY FLULIM IN ADVECTION
!     SCHEMES (SO FAR ONLY FOR TRACERS IN CASE SOLSYS=2 AND OPT_HNEG=2).
!
      IF(LT.EQ.0) THEN
        YAFLULIM=.FALSE.
        YAFLULIMEBE=.FALSE.
        YAFLODEL=.FALSE.
      ENDIF
!
!     FOR READING GRADIENTS IN SELAFIN FILES
!
      IF(NADVAR.GT.0) THEN
        DO IVAR=1,NADVAR
!         SEE NOMVAR_TELEMAC2D
          ALIRE(ADR_TRAC+NTRAC+2*NPERIAF+IVAR) = 1
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     USE DOPRINT TO LIMIT TELEMAC-2D OUTPUTS IN THE LISTING
!
      IF(PRESENT(DOPRINT)) THEN
        LISTIN =  DOPRINT
        ENTET  =  DOPRINT
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(PASS.LE.0) THEN
        WRITE(LU,*) 'INITIALIZING TELEMAC2D FOR ',CODE
        CALL TELEMAC2D_INIT(CODE)
        WRITE(LU,*) 'TELEMAC2D INITIALIZED'
        IF(PASS.EQ.0) THEN
          RETURN
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_TIMESTEP_BEGIN
#endif
700   CONTINUE
!
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF LEO
      LEO=.FALSE.
      IMP=.FALSE.
!
!-----------------------------------------------------------------------
!
      IF(PASS.EQ.1) THEN
        IF(CODE(1:7).EQ.'ESTEL3D') THEN
          AT=ATDEP
          NIT=NITER
!         USE THE TIME STEP SPECIFIED BY ESTEL-3D
          IF(PRESENT(DTDEP)) THEN
            DT = DTDEP
            DTCAS = DTDEP
          ENDIF
        ENDIF
      ENDIF
      ! If pass == 2 (writing results only)
      IF(PASS.EQ.2) GOTO 800
!
666   LT = LT + 1
!
      IF(BRECHE) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BREACH'
        CALL BREACH
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BREACH'
      ENDIF
!
      IF(DTVARI.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!       COURANT NUMBER FOR PSI SCHEME IN P1
        CALL CFLPSI(T1,U,V,DT,IELM,MESH,MSK,MASKEL)
        CALL MAXI(CFLMAX,IMAX,T1%R,NPOIN)
!       LIMITS VARIATIONS IN THE RANGE (1/2, 2)
        DT = DT * MAX(MIN(CFLWTD/MAX(CFLMAX,1.D-6),2.D0),0.5D0)
!       LIMITS DT TO THAT OF THE STEERING FILE
        DT=MIN(DT,DTCAS)
!       for final time step
        IF(DUREE+AT0.GT.AT)DT= MIN(DT,DUREE+AT0-AT)
        IF(NCSIZE.GT.1) DT=P_MIN(DT)
        IF(ENTET) THEN
          WRITE(LU,79) CFLMAX,DT
79        FORMAT(1X,'    MAXIMUM COURANT NUMBER: ',G16.7,/,1X,
     &              '    TIME-STEP                 :',G16.7)
        ENDIF
      ENDIF
!
!=======================================================================
!
      IF(ADJO) THEN
        AT = AT - DT
      ELSE
!       DT IS NOT YET KNOWN IN FINITE VOLUMES
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') AT = AT + DT
      ENDIF

      IF(DTVARI) THEN
        IF(ABS(AT-(DUREE+AT0)).LT.1.E-14) THEN
!         LAST TIME STEP
          NIT   = LT
          ENTET = LISTIN
          TOTAL_ITER= NIT
        ELSE
!         DUMMY VALUE GREATER THAN LT
          NIT = LT + 10
          TOTAL_ITER = NIT
        ENDIF
      ENDIF
!
      IF((LISPRD*(LT/LISPRD).EQ.LT.AND.LT.GE.PTINIL).OR.
     &   LT.EQ.TOTAL_ITER) THEN
        ENTET=LISTIN
      ELSE
        ENTET=.FALSE.
      ENDIF
!
!     CONSTRAINS TELEMAC-2D OUTPUT IN THE LISTING
!
      IF (PRESENT(DOPRINT)) ENTET = ENTET .AND. DOPRINT
!
      IF(ENTET) CALL ENTETE(2,AT,LT)
!
!  WEATHER CONDITIONS.
!
      IF(VENT.OR.ATMOS.OR.INCLUS(COUPLING,'WAQTEL')) THEN
        CALL METEO(PATMOS%R,WINDX%R,WINDY%R,
     &             CST_WINDX,CST_WINDY,AT,LT,NPOIN,VENT,ATMOS,
     &             T2ATMA,T2ATMB,T2D_FILES,LISTIN,CST_PATMOS,
     &             INCLUS(COUPLING,'WAQTEL'),PLUIE,OPTWIND,WIND_SPD)
      ENDIF
!!
!=======================================================================
!
!     COUPLING WITH TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC').AND.
     &   PERCOU_WAC*((LT-1)/PERCOU_WAC).EQ.LT-1) THEN
!
        CALL CONFIG_CODE(3)
!                                                       WAC2
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING TOMAWAC'

        IF(INCLUS(COUPLING,'TOMAWAC2')) THEN
          CALL SEND_COUPLE(1,NPOIN,NVARTEL2TOM,TEL2TOM)
        ENDIF
        CALL T2D_WAC_CPL_UPDATE(PART=1)
        CALL WAC(PART=1)
        IF(INCLUS(COUPLING,'TOMAWAC2')) THEN
          CALL GET_MESH_NPTIR(T2D_FILES(T2DGEO)%FMT,
     &       T2D_FILES(T2DGEO)%LU,NPTIR,IERR)
          CALL RECEIVE_COUPLE(2,NPOIN,NVARTOM2TEL,TOM2TEL)
          ! CONVERT DIRECTIONS BACK
          DO I=1,NPOIN
            IF((ABS(SINDIR%R(I)).GE.1.D-14).OR.
     &          (ABS(COSDIR%R(I)).GE.1.D-14)) THEN
              DIRMOY%R(I) = RAD2DEG * ATAN2(SINDIR%R(I),COSDIR%R(I))
            ELSE
!     POINT HAS NO INFORMATION
              DIRMOY%R(I) = 0.D0
            ENDIF
          ENDDO
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM TOMAWAC'
        CALL CONFIG_CODE(1)
!
      ENDIF

!=======================================================================
!
! BACKUP OF UN, VN, HN, TN, AKN AND EPN (THEY ARE IN FN)
!
! THIS IS NOT DONE WHEN ITERATING FOR THE COUPLING WITH ESTEL-3D
      IF(CODE(1:7).EQ.'ESTEL3D'.AND.PRESENT(NEWTIME)) THEN
        IF(NEWTIME) CALL OS('X=Y     ',X=FN,Y=F)
      ELSE
        CALL OS('X=Y     ',X=FN,Y=F)
      ENDIF
!
!=======================================================================
!
! NEW COUPLING WITH SISYPHE FOR CONSTANT FLOW DISCHARGE
!
      IF(SIS_CPL%SISYPHE_CFD.AND.SIS_CPL%CONSTFLOW) GOTO 999
!
      DO ISIS_CFD=1,SIS_CPL%NSIS_CFD
!
!=======================================================================
!
!  MASKING OF THE WETTING/DRYING ELEMENTS
!
      IF(MSK) CALL OS('X=C     ', X=MASKEL, C=1.D0)
      IF (OPTBAN.EQ.2) THEN
        CALL MASKBD(MASKEL%R,ZFE%R,ZF%R,H%R,
     &              HMIN,MESH%IKLE%I,MESH%IFABOR%I,IT1%I,NELEM,NPOIN)
      ENDIF
!
!  MASKING SPECIFIED BY USER
!
      IF(MSKUSE) THEN
        CALL MASKOB(MASKEL%R,MESH%X%R,MESH%Y%R,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,AT,LT)
      ENDIF
!
! CREATES THE MASK OF THE POINTS FROM THE MASK OF THE ELEMENTS
! AND CHANGES OF IFAMAS (IFABOR WITH MASKING)
!
      IF(MSK) THEN
        CALL MASKTO(MASKEL%R,MASKPT,IFAMAS%I,MESH%IKLE%I,
     &              MESH%IFABOR%I,MESH%ELTSEG%I,MESH%NSEG,
     &              NELEM,IELMT,MESH)
        IF(IELMX.NE.IELM1) CALL CHGDIS(MASKPT,IELM1,IELMX,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!  COMPUTATION OF THE INTEGRAL OF THE BASES
!-----------------------------------------------------------------------
!
!     IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING MASBAS2D'
!     IF(MSK) THEN
!       CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,IELM1,MESH,MSK,MASKEL,T2,T2)
!     ENDIF
!     IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM MASBAS2D'
!
!-----------------------------------------------------------------------
!
! UPDATES POROSITY : NEW VALUE IN TE5
!                    OLD - NEW IN TE4
!
!
      IF(OPTBAN.EQ.3) THEN
!
        CALL OS('X=Y     ',X=TE4,Y=TE5)
        CALL POROS(TE5,ZF,HN,MESH)
        IF(MSK) CALL OS('X=XY    ',X=TE5,Y=MASKEL)
!       TEST OF UNDER-RELAXATION
        RELAX = 0.05D0
        CALL OS('X=CX    ',X=TE5,C=RELAX)
        CALL OS('X=X+CY  ',X=TE5,Y=TE4,C=1.D0-RELAX)
!       TE4 = OLD POROS - NEW POROS
        CALL OS('X=X-Y   ',X=TE4,Y=TE5)
!
      ENDIF
!
!=======================================================================
!
! NEW ADVECTION AND PROPAGATION FIELDS
! NOTE THAT U = UN, V = VN AND H = HN AT THIS STAGE
!
      IF(CONV) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHPCONV 1'
        CALL CHPCONV(UCONV,VCONV,U,V,UN,VN,TETAU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHPCONV 1'
      ELSE
        CALL OS('X=Y     ',X=UCONV,Y=UN)
        CALL OS('X=Y     ',X=VCONV,Y=VN)
      ENDIF
!
!     COMPUTATION OF THE NEW PROPAGATION TERM
!
      IF(PROPA) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING HPROPA'
        CALL HPROPA(HPROP ,HN,H,PROLIN,HAULIN,TETAC,NSOUSI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM HPROPA'
      ENDIF
!
!=======================================================================
!
! PREPARES BOUNDARY CONDITIONS FOR WEIRS.
!
      IF(NWEIRS.GT.0) THEN
!
        IF(TYPSEUIL.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CLSING'
          CALL CLSING(NWEIRS,NPSING,NDGA1,NDGB1,
     &                MESH%X%R,MESH%Y%R,ZF%R,CHESTR%R,NKFROT%I,
     &                KARMAN,ZDIG,PHIDIG,MESH%NBOR%I,
     &                H%R,T,NTRAC,IOPTAN_T2D,T1%R,UBOR%R,VBOR%R,TBOR,
     &                LIHBOR%I,LIUBOR%I,LIVBOR%I,LITBOR,GRAV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CLSING'
        ELSEIF(TYPSEUIL.EQ.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CALCUL_Q_WEIR'
          CALL CALCUL_Q_WEIR
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CALCUL_Q_WEIR'
        ELSE
          WRITE(LU,*)'TELEMAC-2D : TYPE OF WEIRS NOT IMPLEMENTED'
        ENDIF
!
      ENDIF
!
! IT IS ASSUMED THAT THE TYPES OF BOUNDARY CONDITIONS DO NOT CHANGE
! DURING THE SUB-ITERATIONS. IF NOT IT IS NECESSARY TO MOVE THE CALLS
! TO KEPSIN, DIFFIN, PROPIN
!
! TYPES OF CONDITIONS FOR THE K-EPSILON MODEL
!
      IF(ITURB.EQ.3) CALL KEPSIN(LIMKEP%I,LIUBOR%I,NPTFR,
     &                           KENT,KENTU,KSORT,KADH,KLOG,
     &                           KINC,KNEU,KDIR)
!
! TYPES OF CONDITIONS FOR THE SPALART-ALLMARAS MODEL
!
      IF(ITURB.EQ.6) CALL SPALALLIN(LIMSA%I, LIUBOR%I, NPTFR,
     &                              KENT, KENTU, KSORT, KADH, KLOG,
     &                              KINC, KNEU, KDIR)
!
! TYPES OF CONDITIONS FOR THE DIFFUSION OF THE TRACER:
!
      IF(NTRAC.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DIFFIN'
        DO ITRAC=1,NTRAC
        CALL DIFFIN(MASKTR,LIMTRA%I,LITBOR%ADR(ITRAC)%P%I,
     &              IT1%I,UCONV%R,VCONV%R,
     &              MESH%XNEBOR%R,MESH%YNEBOR%R,
     &              MESH%NBOR%I,NPTFR,
     &              KENT,KSORT,KLOG,KNEU,KDIR,KDDL,
     &              ICONVFT(ITRAC),
     &              MESH%NELBOR%I,NPOIN,MSK,MASKEL%R,
     &              NFRLIQ,THOMFR,FRTYPE,
     &              TN%ADR(ITRAC)%P,TBOR%ADR(ITRAC)%P,NUMLIQ%I,
     &              MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DIFFIN'
      ENDIF
!
! TYPES OF CONDITIONS FOR THE PROPAGATION:
! REQUIRED FOR THOMFR ?? (OTHERWISE DONE AFTER BORD !)
!
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,
     &             KNEU,KDIR,KDDL,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,NPOIN,
     &             MESH%NELBOR%I,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,NUMLIQ%I,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,MESH%IKLBOR%I,.FALSE.,
     &             MESH%NELEBX,MESH%NELEB)
!    *             MESH%XNEBOR%R,MESH%YNEBOR%R,MESH%IKLBOR%I,ENTET )
!       WARNINGS WILL BE GIVEN AT THE SECOND CALL AFTER BORD
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPIN'
!
!=======================================================================
!                 COMPUTES THE FRICTION COEFFICIENTS
!                         VARIABLE IN TIME
!=======================================================================
! CORSTR DOES NOT DO ANYTHING UNLESS MODIFIED BY THE USER.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CORSTR'
      CALL CORSTR
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CORSTR'
!
      IF(IELMU.EQ.12.OR.IELMU.EQ.13) THEN
        CALL CHGDIS(CHESTR,DISCLIN,IELMU,MESH)
      ENDIF
!
      CALL FRICTION_CHOICE(1)
!
!=======================================================================
!                 COMPUTES VISCOSITY COEFFICIENTS
!=======================================================================
!
!  COMPUTES DYNAMIC VISCOSITY VISC
!
      IF(ITURB.EQ.1) THEN
!
        CALL OS('X=C     ', X=VISC, C=PROPNU)
!
      ELSEIF(ITURB.EQ.2) THEN
!
        CALL DISPER( VISC , U%R , V%R , H%R , CF%R , ELDER , PROPNU )
!
      ELSEIF(ITURB.EQ.3) THEN
!
        CALL VISTUR(VISC,AK,EP,NPOIN,CMU,PROPNU)
!
      ELSEIF(ITURB.EQ.4) THEN
!
        CALL SMAGOR(VISC,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
!
      ELSEIF(ITURB.EQ.5) THEN
!
        CALL MIXLENGTH(VISC,MESH,T1,T2,T3,T4)
!
      ELSEIF(ITURB.EQ.6) THEN
!
        CALL VISTURSA(VISC,VISCSA,NPOIN,PROPNU)
!
      ELSE
!
        IF(LISTIN) THEN
          WRITE(LU,16) ITURB
16        FORMAT(1X,'ITURB=',1I6,'UNKNOWN TURBULENCE MODEL')
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!  COEFFICIENT FOR THERMAL DIFFUSION (PRANDTL = 1 FOR NOW)
!  AND THE SAME FOR ALL THE TRACERS
!
      IF(NTRAC.GT.0.AND.DIFT) THEN
        DO ITRAC=1,NTRAC
          CALL OS('X=Y+C   ',X=VISCT%ADR(ITRAC)%P,Y=VISC,
     &                       C=DIFNU(ITRAC)-PROPNU)
        ENDDO
!     IF MICROPOL MODULE IS ACTIVATED, TRACERS BOTTOM SEDIMENT (IND_SF)
!     AND MICROPOLLUTANT ADSORBED BY BOTTOM SEDIMENT (IND_CSF)
!     ARE NEITHER ADVECTED NOR DIFFUSED (NO ADVECTION DONE IN LECDON)
        IF(INCLUS(COUPLING,'WAQTEL').AND.
     &     7*INT(WAQPROCESS/7).EQ.WAQPROCESS) THEN
          CALL OS('X=0     ', X=VISCT%ADR(IND_SF)%P)
          CALL OS('X=0     ', X=VISCT%ADR(IND_CSF)%P)
          IF(KIN_MICROPOL.EQ.2) THEN
            CALL OS('X=0     ', X=VISCT%ADR(IND_CSF2)%P)
          ENDIF
        ENDIF
      ENDIF
!
!  IT IS POSSIBLE TO CORRECT THE VISCOSITY COEFFICIENTS.
!
      CALL CORVIS
!
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING VOLFIN 1'
        CALL VOLFIN1(W2%R,DTN,FLUX%R,QU%R,QV%R,CM1%X%R,CM2%X%R,BM1%X%R,
     &               BM2%X%R,DSZ%R,LOGFR%I,TB%ADR(6)%P%R,TB%ADR(7)%P%R,
     &               TB%ADR(8)%P%R,TB%ADR(9)%P%R,TB%ADR(10)%P%R,
     &               FLUX_OLD%R,MESH,LEO,DT,CORR_I,CORR_J,CORR_ZL,
     &               CORR_ZR,CORR_HL,CORR_HR,CORR_UL,CORR_UR,CORR_VL,
     &               CORR_VR,IVIS,BETA,GPRDTIME)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM VOLFIN 1'
      ENDIF
!
!=======================================================================
!  SOURCES : COMPUTATION OF INPUTS WHEN VARYING IN TIME
!            IF NO VARIATION IN TIME DSCE2=DSCE AND TSCE2=TSCE
!=======================================================================
!
      IF(NREJET.GT.0.AND.OKREJET) THEN
        DO I=1,NREJET
          DSCE2(I)=DEBSCE(AT,I,DSCE)
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO I=1,NREJET
            DO ITRAC=1,NTRAC
              TSCE2(I,ITRAC)=TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
        ENDIF
      ELSEIF(.NOT.OKREJET) THEN
!     WHEN USER GIVE SOURCE REGIONS AND VELOCITIES (BUT NOT FLOWRATES)
        DO I=1, NREG
          MODVIT=SQRT(VUSCE(AT,I)**2+VVSCE(AT,I)**2)
          DSCE2(I)=MODVIT*AREA_P(I)
        ENDDO
!       TO NOT BREAK ALL THE CYCLES WHICH ARE BUILT ON NREJET
        NREJET=NREJEU
        IF(NTRAC.GT.0) THEN
          DO I=1,NREG
            DO ITRAC=1,NTRAC
              TSCE2(I,ITRAC)=TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! BOUNDARY CONDITIONS
!=======================================================================
!
      IF(THOMFR) THEN
!
      CALL CPSTVC(H,T9)
      CALL PREBOR(HBOR%R,UBOR%R,VBOR%R,TBOR,U%R,V%R,H%R,
     &            T9%R,T,MESH%NBOR%I,
     &            NPOIN,NPTFR,NTRAC,NFRLIQ,FRTYPE,NUMLIQ%I)
!
      ENDIF
!
! CALLS THE USER-SUBROUTINE DETERMINING THE BOUNDARY CONDITIONS.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BORD'
      CALL BORD(HBOR%R,UBOR%R,VBOR%R,TBOR,
     &          U,V,H,ZF%R,MESH%NBOR%I,W1DEB,T8,
     &          LIHBOR%I,LIUBOR%I,LITBOR,
     &          MESH%XNEBOR%R,MESH%YNEBOR%R,NPOIN,NPTFR,
     &          NPTFR2,AT,
     &          NDEBIT,NCOTE,NVITES,NTRAC,NTRACE,NFRLIQ,NUMLIQ%I,
     &          KENT,KENTU,PROVEL,MASK,MESH,EQUA,T2D_FILES(T2DIMP)%NAME)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BORD'
!
! COMPUTES ICE RELATED PROCESSES THROUGH BOUNDARIES (CLOGGING, ETC.)
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BORICE'
        CALL BORICE( H,U,V,TN,AT,LT,DT, W1DEB,T8,
     &    LIUBOR%I,NPTFR,NUMLIQ%I,KLOG,
     &    MASK,MESH,S )
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BORICE'
      ENDIF
!
! COMPUTES LIMPRO, CLU,CLV, CLH AND MASK
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,
     &             KNEU,KDIR,KDDL,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,NPOIN,
     &             MESH%NELBOR%I,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,NUMLIQ%I,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,MESH%IKLBOR%I,ENTET,
     &             MESH%NELEBX,MESH%NELEB)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPIN'
!
! PREPARING THE FRICTION ON THE LATERAL BOUNDARIES
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING WALL_FRICTION'
      CALL WALL_FRICTION(UETUTA%R,AUBOR%R,CFBOR%R,
     &                   MESH%DISBOR%R,UN%R,VN%R,LIMPRO%I,
     &                   MESH%NBOR%I,NPTFR,KARMAN,PROPNU,
     &                   LISRUG,KNEU,
     &                   IELMU,MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM WALL_FRICTION'
!
! K-EPSILON BOUNDARY CONDITIONS: KBOR,EBOR AND AUBOR
!
      IF(ITURB.EQ.3) THEN
        CALL KEPSCL(KBOR%R,EBOR%R,CF%R,
     &              MESH%DISBOR%R,
     &              UN%R,VN%R,HN%R,LIMKEP%I,LIUBOR%I,
     &              MESH%NBOR%I,NPTFR,KARMAN,CMU,C2,ESTAR,
     &              SCHMIT,EMIN,KDIR,
     &              KENT,KENTU,KADH,KLOG,UETUTA%R)
      ENDIF
!
! SA BOUNDARY CONDITIONS : NUBOR
!
      IF(ITURB.EQ.6) THEN
        CALL SPALALLCL(NUBOR%R, LIMSA%I, LIUBOR%I,NPTFR,NUMIN,
     &                 KDIR, KENT, KENTU, KADH, KLOG)
      ENDIF
!
! CALLS THE SYSTEM OF RESOLUTION FOR BOUNDARIES BY THE CHARACTERISTICS
! METHOD (THOMPSON)
!
      IF(THOMFR) THEN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING THOMPS'
      CALL THOMPS(HBOR%R,UBOR%R,VBOR%R,TBOR,U,V,T9,
     &            T,ZF,MESH%X%R,MESH%Y%R,MESH%NBOR%I,
     &            FRTYPE,T2,T3,T4,T10,T11,
     &            LIHBOR%I,LIUBOR%I,LIVBOR%I,IT1%I,
     &            CV2%R,CV3%R,TE1%R,HTILD,UTILD,VTILD,
     &            TTILD,T15,MESH%SURDET%R,MESH%IKLE%I,
     &            MESH%IFABOR%I,NELEM,MESH,
     &            MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            NPOIN,NPTFR,DT,GRAV,NTRAC,
     &            NFRLIQ,KENT,KENTU,MSK,MASKEL,
     &            NELMAX,IELM,T5%R,NUMLIQ%I,BM1%X%R,
     &            T12%R,T13%R,T14%R,IT3,IT4,
     &            T17,T18,T19,T20,T21,T22,W1)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM THOMPS'
!
      ENDIF
!
!     CHECKS HBOR BECAUSE THE USER CAN MODIFY BORD AND MAKE A MISTAKE
      CALL CLIP(HBOR,0.D0,.TRUE.,1.D6,.FALSE.,0)
!
!=======================================================================
!
! LOOP OVER THE SUB-ITERATIONS WHERE ADVECTION AND PROPAGATION ARE UPDATED
!
!=======================================================================
!
      DO ISOUSI = 1 , NSOUSI
      IF(DEBUG.GT.0) WRITE(LU,*) 'BOUCLE 701 ISOUSI=',ISOUSI
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_SUBITERATION_BEGIN
#endif
!
!=======================================================================
!
! : 4                     ADVECTION
!
!=======================================================================
!
      IF(CONV.AND.(FTILD%N.GT.0.OR.FTILD2%N.GT.0)) THEN
!
        IF(ENTET) CALL ENTETE(3,AT,LT)
!
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',X=UCONV,Y=UCONV,Z=MESH%COSLAT)
          CALL OS('X=Y/Z   ',X=VCONV,Y=VCONV,Z=MESH%COSLAT)
        ENDIF
!
!       COMPUTATION OF STRONG CHARACTERISTICS AND INTERPOLATION
!
        IF(FTILD%N.GT.0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHARAC OPTION STRONG'
          CALL CHARAC(FNCAR , FTILD  , FTILD%N  , UCONV , VCONV,S,S,S,S,
     &                DT    , IFAMAS , IELM     , NPOIN , 1,1,1,
     &                MSK   , BM1%X  , BM1%D    , BM1%D , TB   ,
     &                IT1%I , IT2%I  , IT2%I    ,IT3%I  , IT4%I , IT2%I,
     &                MESH  , MESH%NELEM        ,MESH%NELMAX    ,
     &                MESH%IKLE,MESH%SURDET,
!                     FOR WEAK FORM OF ADVECTION                OPTCHA
     &                AM1,CV1,SLVPRO,AGGLOW,ENTET,NGAUSS,UNSV2D,1)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHARAC OPTION STRONG'
        ENDIF
!
!       COMPUTATION OF WEAK CHARACTERISTICS AND INTERPOLATION
!
        IF(FTILD2%N.GT.0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHARAC OPTION WEAK'
          CALL CHARAC(FNCAR2, FTILD2 , FTILD2%N , UCONV , VCONV,S,S,S,S,
     &                DT    , IFAMAS , IELM     , NPOIN , 1,1,1,
     &                MSK   , BM1%X  , BM1%D    , BM1%D , TB   ,
     &                IT1%I , IT2%I  , IT2%I    ,IT3%I  , IT4%I , IT2%I,
     &                MESH  , MESH%NELEM        ,MESH%NELMAX    ,
     &                MESH%IKLE,MESH%SURDET,
!                     FOR WEAK FORM OF ADVECTION                OPTCHA
     &                AM1,CV1,SLVPRO,AGGLOW,ENTET,NGAUSS,UNSV2D,2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHARAC OPTION WEAK'
        ENDIF
!
        IF(SPHERI) THEN
          CALL OS('X=XY    ',X=UCONV,Y=MESH%COSLAT)
          CALL OS('X=XY    ',X=VCONV,Y=MESH%COSLAT)
        ENDIF
!
      ENDIF
!
! MANAGEMENT OF THE ARRAYS.
!
      CALL GESTIO(UN   ,VN   ,HN   ,TN   ,AKN   ,EPN   ,NUN   ,
     &            UTILD,VTILD,HTILD,TTILD,AKTILD,EPTILD,NUTILD,
     &            NTRAC.GT.0,PROPA,CONVV,ITURB,3)
!
!=======================================================================
!                       END OF ADVECTION
!=======================================================================
!=======================================================================
!
! : 6                DIFFUSION - PROPAGATION
!
!=======================================================================
!
      IF(PROPA) THEN
      IF(ENTET) CALL ENTETE(6,AT,LT)
!     INFORMATION ON THE METHOD OF RESOLUTION IS GIVEN ONLY
!     IF LISTING IS REQUESTED
      INFOGS=.FALSE.
      IF(INFOGR.AND.ENTET) INFOGS=.TRUE.
!
!  COMPUTES THE DENSITY WHEN IT IS VARIABLE
!
      IF(ROVAR) THEN
!       BEWARE, SALINITY MUST BE HERE THE FIRST TRACER
        CALL VALRO(RO,T,ROEAU)
      ENDIF
!
!  SOURCE TERMS DUE TO TUBES, BRIDGES OR SIPHONS.
!
      IF(NBUSE.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BUSE'
        CALL BUSE(RELAXB,NBUSE,ENTBUS%I,SORBUS%I,GRAV,
     &            H%R,ZF%R,DBUS%R,LRGBUS%R,HAUBUS%R,CLPBUS%I,
     &            ALTBUS%R,CSBUS%R,CEBUS%R,ANGBUS%R,LBUS%R,
     &            NTRAC,T,TBUS,UBUS%R,VBUS%R,U%R,V%R,ENTET,
     &            CV%R,C56%R,CV5%R,C5%R,CTRASH%R,FRICBUS%R,
     &            LONGBUS%R,CIRC%I,DELBUS%R,OPTBUSE,
     &            V2DPAR,DT,SECBUS%R,MAXSCE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BUSE'
      ENDIF
!
!  SOURCE TERMS FOR PROPAGATION.
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROSOU'
      CALL PROSOU(FU,FV,SMH,UN,VN,HN,GRAV,
     &            FAIR,WINDX,WINDY,VENT,HWIND,
     &            CORIOL,FCOR,SPHERI,YASMH,YASMO,
     &            MESH%COSLAT,MESH%SINLAT,AT,LT,DT,
     &            NREJET,NREJEU,DSCE2,ISCE,T1,MESH,MSK,MASKEL,
     &            MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,
     &            VARCL,NVARCL,VARCLA,UNSV2D,FXWAVE,FYWAVE,
     &            RAIN,CST_RAINFALL,PLUIE,T2D_FILES,T2DBI1,
     &            BANDEC,
     &            NBUSE,ENTBUS%I,SORBUS%I,DBUS%R,UBUS%R,VBUS%R,
     &            TYPSEUIL,NWEIRS,N_NGHB_W_NODES,
     &            MAXSCE,NREG,PT_IN_POLY,
     &            TNP,AREA_P,EQUA,CF)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROSOU'
!
!  PROPAGATION.
!
      HN%DIM1=NPOIN
      ZF%DIM1=NPOIN
      IF(EQUA(1:15).EQ.'SAINT-VENANT EF'.OR.
     &   EQUA(1:10).EQ.'BOUSSINESQ') THEN
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPAG'
      CALL PROPAG
     &(U,V,H,UCONV,VCONV,CONVV,H0,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,
     & FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T8,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,DT,
     & TETAC,TETAU,TETAD,
     & AGGLOC,AGGLOU,KDIR,INFOGS,KFROT,ICONVF,
     & ISOUSI,BILMAS,MASSES,MASS_RAIN,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,PRECCU,SOLSYS,OPDVIT,
     & OPTSOU,NFRLIQ,SLVPRO,EQUA,VERTIC,ADJO,ZFLATS,TETAZCOMP,
     & UDEL,VDEL,DM1,ZCONV,FLBOR,BM1S,BM2S,CV1S,
     & VOLU2D,V2DPAR,UNSV2D,NDGA1,NDGB1,NWEIRS,NPSING,HFROT,
     & FLULIM,YAFLULIM,FLULIMEBE,YAFLULIMEBE,
     & RAIN,PLUIE,MAXADV,OPTADV_VI,MESH%IKLE%I,MESH%NELMAX)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPAG'
!
      IF(ADJO) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING PROPAG_ADJ'
        CALL PROPAG_ADJ
     & (UN,VN,HN,
     &  MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,
     &  CV1,CV2,CV3,
     &  TE3,T1,T2,T3,T4,T5,T6,T7,T10,T11,
     &  LIMPRO,MASK,GRAV,CF,LT,AT,DT,
     &  TETAC,TETAU,
     &  KDIR,INFOGS,KFROT,
     &  MSK,MASKEL,MASKPT,
     &  RHS,TB,SOLSYS,
     &  SLVPRO,VERTIC,
     &  U,V,H,UU,VV,HH,UIT1,VIT1,HIT1,PP,QQ,RR,
     &  TAM1,TAM2,TAM3,TBM1,TBM2,TCM1,TCM2,MATADJ,UNKADJ,
     &  ADJDIR,ESTIME,OPTCOST,NIT,
     &  VARSOR,
     &  ALIRE,TROUVE,MAXVAR,TEXTE,
     &  CHESTR,KARMAN,NDEF,LISRUG,
     &  CHBORD,CFBOR,HFROT,UNSV2D)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM PROPAG_ADJ'
!
      ENDIF
!
      ELSEIF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
!
      IF(INCLUS(COUPLING,'GAIA')) THEN
!
        CALL CONFIG_CODE(6)
!
        SUSP1=GAI_CPL%SUSP
!
!=======================================================================
!       LEO IS COMMON FOR THE OUTPUT OF SISYPHE AND T2D
        CALL PRERES_TELEMAC2D()
!       KEEP THE SAME GRAPHIC COUNTER FOR T2D ANS SIS
        IF(LEO)COMPLEO=COMPLEO-1
!
!       MANAGE COUPLING PERIOD CALLS
!
        YAGAI = .TRUE.
!
        IF(SUSP1) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING GAIA, SUSPENSION'
!
          CALL GAIA_STEP(
     &         LT,LEOPRD_CHARR,LISPRD,TOTAL_ITER,U,V,H,ZF,
     &         CF,CF,CF,CHESTR,CODE1,
     &         U,V,AT,VISC,DT,GAI_CPL%CHARR,SUSP1,
     &         ROEAU,GRAV,
     &         DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM GAIA, SUSPENSION'
!
        ELSEIF(GAI_CPL%CHARR.AND.YAGAI) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING GAIA, BEDLOAD'
          CALL GAIA_STEP(
     &         LT,LEOPRD_CHARR,LISPRD,TOTAL_ITER,U,V,H,ZF,
     &         CF,CF,CF,CHESTR,CODE1,
     &         U,V,AT+DT,VISC,DT,GAI_CPL%CHARR,SUSP1,
     &         ROEAU,GRAV,
     &         DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM GAIA, BEDLOAD'
        ENDIF
!
!
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!       VOLFIN MAY CHANGE DT
!
!       CM1%D%R : HT
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING VOLFIN 2'
        CALL VOLFIN2(W2%R,FLUX%R,H%R,U%R,V%R,QU%R,QV%R,FLUSOR,FLUENT,
     &              SMH%R,MASSES,T,HT,MASSOU,FLUTENT,FLUTSOR,CM1%X%R,
     &              CM2%X%R,BM1%X%R,BM2%X%R,DSZ%R,FLBOR,FLUXT,FLUXT_OLD,
     &              FLUHTEMP,TB%ADR(3)%P%R,TB%ADR(4)%P%R,TE1%R,TE2%R,
     &              FLUX_OLD%R,MESH,MASS_RAIN,YASMO,DT,YASMH,UBOR,
     &              VBOR,IVIS,BETA,SMTR)

        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM VOLFIN 2'
!
!       COMPATIBLE VELOCITY FIELD IN CONTINUITY EQUATION
!       USED BY SISYPHE AND DELWAQ
        CALL OV ('X=Y     ', X=UDEL%R, Y=U%R, DIM1=NPOIN)
        CALL OV ('X=Y     ', X=VDEL%R, Y=V%R, DIM1=NPOIN)
!
        AT = AT + DT
        IF (LT.GE.NIT.OR.AT.GT.TMAX.OR.
     &      ((TMAX-AT)/TMAX).LT.1E-15) THEN !LAST TIME STEP
          ! Set lt as the last timestep
          NIT = LT
          ENTET = .TRUE.
          CALL ENTETE(1,AT,LT)
          TOTAL_ITER=LT
        ENDIF
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN EQUATIONS: ',EQUA
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! IF NO PROPAGATION :
!
      ELSE
!
! MANAGEMENT OF THE ARRAYS .
!
        CALL GESTIO(U    ,V    ,H    ,T,AK  ,EP , NUN   ,
     &              UTILD,VTILD,HTILD,T,AK  ,EP , NUTILD,
     &              NTRAC.GT.0,PROPA,CONVV,ITURB ,6)
!
!       SMH USED BY THE TRACER
!       TO SIMULATE SUBIEF TAKING OFF PROPAGATION
!       AND ADVECTION, PROSOU IS NOT CALLED AND DISCRETE
!       SOURCES ARE NOT TAKEN INTO ACCOUNT.
!       STRICTLY 'CALL PROSOU' SHOULD BE HERE.
        IF(NTRAC.GT.0) CALL OS('X=0     ',X=SMH)
!
      ENDIF
!
!     TREATMENT OF NEGATIVE DEPTHS
!
      CALL CORRECTION_DEPTH_2D(MESH%GLOSEG%I,MESH%GLOSEG%DIM1,YASMH)
!
!=======================================================================
!                          END OF PROPAGATION
!=======================================================================
!
!  COMPUTES THE NEW ADVECTION FIELDS IF THERE REMAIN
!  SUB-ITERATIONS.
!
!  THE TEST ON ISOUSI IS MADE ONLY FOR HPROP AND NOT FOR UCONV
!  FOR REASONS OF TRACER MASS CONSERVATION (IT IS NECESSARY TO KEEP
!  THE SAME HPROP FOR THE TRACER AS THAT FOR H AND U)
!
      IF(ISOUSI.NE.NSOUSI) THEN
!       COMPUTES THE NEW PROPAGATION FIELD IF PROPAGATION
        IF(PROPA) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING HPROPA'
          CALL HPROPA(HPROP ,HN,H,PROLIN,HAULIN,TETAC,NSOUSI)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM HPROPA'
        ENDIF
      ENDIF
!
!     COMPUTES THE NEW ADVECTION FIELD (IF ADVECTION)
      IF(CONV) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CHPCONV 2'
        CALL CHPCONV(UCONV,VCONV,U,V,UN,VN,TETAU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CHPCONV 2'
      ENDIF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_SUBITERATION_END
#endif
!
!=======================================================================
! END OF THE LOOP OF THE SUB-ITERATIONS
!
      ENDDO ! ISOUSI
!
!=======================================================================
!     COUPLING WITH GAIA
!=======================================================================
!

      IF(INCLUS(COUPLING,'GAIA').AND.
     &   EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!
        CALL CONFIG_CODE(6)
!
        SUSP1=GAI_CPL%SUSP
!
!=======================================================================
!       LEO IS COMMON FOR THE OUTPUT OF SISYPHE AND T2D
        CALL PRERES_TELEMAC2D()
!       KEEP THE SAME GRAPHIC COUNTER FOR T2D ANS SIS
        IF(LEO)COMPLEO=COMPLEO-1
!
!       MANAGE COUPLING PERIOD CALLS
!
        YAGAI = .TRUE.
!
        IF(SUSP1) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING GAIA, SUSPENSION'
!
          CALL GAIA_STEP(
     &         LT,LEOPRD_CHARR,LISPRD,TOTAL_ITER,U,V,H,ZF,
     &         CF,CF,CF,CHESTR,CODE1,
     &         U,V,AT,VISC,DT,GAI_CPL%CHARR,SUSP1,
     &         ROEAU,GRAV,
     &         DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM GAIA, SUSPENSION'
!
        ELSEIF(GAI_CPL%CHARR.AND.YAGAI) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING GAIA, BEDLOAD'
          CALL GAIA_STEP(
     &         LT,LEOPRD_CHARR,LISPRD,TOTAL_ITER,U,V,H,ZF,
     &         CF,CF,CF,CHESTR,CODE1,
     &         U,V,AT,VISC,DT,GAI_CPL%CHARR,SUSP1,
     &         ROEAU,GRAV,
     &         DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM GAIA, BEDLOAD'
        ENDIF
!
!
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!=======================================================================
!
! : 5                 DIFFUSION OF THE TRACER
!
!=======================================================================
!
      IF(NTRAC.GT.0.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!
      IF(ENTET) CALL ENTETE(5,AT,LT)
!
      DO ITRAC=1,NTRAC
!
!       BOUNDARY CONDITIONS FOR THE DIFFUSION OF THE TRACER.
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DIFFCL POUR ITRAC=',ITRAC
        CALL DIFFCL(LITBOR%ADR(ITRAC)%P%I,
     &            TTILD%ADR(ITRAC)%P%R,TBOR%ADR(ITRAC)%P%R,
     &            MESH%NBOR%I,ICONVFT(ITRAC),NPOIN,NPTFR)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DIFFCL'
!
      ENDDO
!
!  SOURCE TERMS FOR DIFFUSION - SOURCE TERMS OF THE TRACER
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DIFSOU'
      CALL DIFSOU(TEXP,TIMP,TSCEXP,HPROP,TN,TETAT,NREJET,
     &            ISCE,DSCE2,TSCE2,MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,
     &            MESH%IFAC%I,NBUSE,ENTBUS%I,SORBUS%I,DBUS%R,TBUS,
     &            NWEIRS,TYPSEUIL,N_NGHB_W_NODES,
     &            NREG,PT_IN_POLY,TNP,MESH%NPOIN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DIFSOU'
!
!=======================================================================
!     OIL SPILL MODEL (UNDER DEVELOPMENT IN MYGRHYCAR PROJECT)
!=======================================================================
!
      IF(SPILL_MODEL) THEN
!
        CALL OIL_SPILL_2D
!
      ENDIF
!
!=======================================================================
!     ADVECTION-DIFFUSION OF TRACERS
!=======================================================================
!
      DO ITRAC=1,NTRAC
!
!  CALLS THE STANDARD DIFFUSER. (CV1 IS THE SECOND MEMBER)
!
      INFOGT=INFOGR.AND.ENTET
!     HTILD: WORKING ARRAY WHERE HPROP IS RE-COMPUTED
!             (SAME ARRAY STRUCTURE)
!
!     LIMTRA REPLACED BY A COPY (IT MAY BE CHANGED BY THE ADVECTION SCHEME)
!
      DO I=1,NPTFR
        IT1%I(I)=LIMTRA%I(I)
      ENDDO
!
      TTILD%ADR(ITRAC)%P%DIM1=NPOIN
!
      IF(INCLUS(COUPLING,'GAIA').AND.
     &   (ITRAC.GE.IND_SED.AND.ITRAC.LT.IND_SED+NSUSP_TEL)) THEN
!
!     CASE OF COUPLING WITH GAIA
!
        ISUSP=ITRAC-IND_SED+1
        IF(SOLSYS.EQ.1) THEN
          CALL PREP_ADVECTION_GAIA(UCONV,VCONV,ICONVFT(ITRAC),
     &                             SOLSYS,ISUSP,LITBOR%ADR(ITRAC)%P%I,
     &                            TBOR%ADR(ITRAC)%P%R,TN%ADR(ITRAC)%P%R,
     &                             KENT,FLBOR,HN,MASSOU(ITRAC))
        ELSEIF(SOLSYS.EQ.2) THEN
          CALL PREP_ADVECTION_GAIA(UDEL,VDEL,ICONVFT(ITRAC),
     &                             SOLSYS,ISUSP,LITBOR%ADR(ITRAC)%P%I,
     &                            TBOR%ADR(ITRAC)%P%R,TN%ADR(ITRAC)%P%R,
     &                             KENT,FLBOR,HN,MASSOU(ITRAC))
        ENDIF

!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CVDFTR SOLSYS=',SOLSYS_GAI
        CALL CVDFTR(T%ADR(ITRAC)%P,TTILD%ADR(ITRAC)%P,
     &              TN%ADR(ITRAC)%P,TSCEXP%ADR(ITRAC)%P,
     &              DIFT,ICONVFT(ITRAC),.TRUE.,HN_GAI,HOLD,HPROP,
     &              UCONV_GAI,VCONV_GAI,DM1,ZCONV,SOLSYS_GAI,
     &              VISCT%ADR(ITRAC)%P,VISC_S,FLUER_ADV,
     &              SMH,YASMH,
!                              YASMI
     &              FLUDPT_ADV,.TRUE.,AM1,AM2,ZF,
     &              TBOR%ADR(ITRAC)%P,ATBOR%ADR(ITRAC)%P,
     &              BTBOR%ADR(ITRAC)%P,IT1,MASKTR,MESH,TB,
     &              T1,T2,T4,T10,TE1,TE2,TE3,
     &              KDIR,KDDL,
!                                              BILAN
     &              DT,ENTET,TETAT,1.D0,INFOGT,.TRUE.,
     &              OPTADV_TR(ITRAC),1,OPDTRA,OPTBAN,
     &              MSK,MASKEL,MASKPT,MBOR,S,MASSOU(ITRAC),
     &              1,SLVTRA(ITRAC),FLBOR,VOLU2D,V2DPAR,UNSV2D,
     &              2,FLBORTRA,FLULIM_GAI,YAFLULIM_GAI,
     &              FLULIM_GAI,.FALSE.,
     &              DIRFLU,RAIN,PLUIE,TRAIN(ITRAC),
     &              FLULIM_GAI,.FALSE.,MAXADV,TB2,NCO_DIST,NSP_DIST)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CVDFTR'
!
        CALL OS('X=YZ    ',X=FLUDP%ADR(ISUSP)%P,
     &          Y=FLUDPT%ADR(ISUSP)%P,Z=T%ADR(ITRAC)%P)
        CALL OS('X=+(Y,C)',X=FLUDP%ADR(ISUSP)%P,
     &          Y=FLUDP%ADR(ISUSP)%P,C=0.D0)
!
      ELSE
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING CVDFTR SOLSYS=',SOLSYS
        IF(SOLSYS.EQ.1) THEN
        CALL CVDFTR(T%ADR(ITRAC)%P,TTILD%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,
     &              TSCEXP%ADR(ITRAC)%P,
     &              DIFT,ICONVFT(ITRAC),CONVV(3),H,HN,HPROP,
     &              UCONV,VCONV,DM1,ZCONV,SOLSYS,
     &              VISCT%ADR(ITRAC)%P,VISC_S,TEXP%ADR(ITRAC)%P,SMH,
     &              YASMH,TIMP%ADR(ITRAC)%P,YASMI(ITRAC),AM1,AM2,ZF,
     &              TBOR%ADR(ITRAC)%P,ATBOR%ADR(ITRAC)%P,
     &              BTBOR%ADR(ITRAC)%P,IT1,MASKTR,MESH,TB,
     &              T1,T2,T4,T10,TE1,TE2,TE3,
     &              KDIR,KDDL,
     &              DT,ENTET,TETAT,AGGLOT,INFOGT,BILMAS,
     &              OPTADV_TR(ITRAC),ISOUSI,OPDTRA,OPTBAN,
     &              MSK,MASKEL,MASKPT,MBOR,S,MASSOU(ITRAC),
     &              OPTSOU,SLVTRA(ITRAC),FLBOR,VOLU2D,V2DPAR,UNSV2D,
     &              2,FLBORTRA,FLULIM,YAFLULIM,FLULIMEBE,YAFLULIMEBE,
     &              DIRFLU,RAIN,PLUIE,TRAIN(ITRAC),
     &              FLODEL,.FALSE.,MAXADV,TB2,NCO_DIST,NSP_DIST)
!
        ELSE
        CALL CVDFTR(T%ADR(ITRAC)%P,TTILD%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,
     &              TSCEXP%ADR(ITRAC)%P,
     &              DIFT,ICONVFT(ITRAC),CONVV(3),H,HN,HPROP,
     &              UDEL,VDEL,DM1,ZCONV,SOLSYS,
     &              VISCT%ADR(ITRAC)%P,VISC_S,TEXP%ADR(ITRAC)%P,SMH,
     &              YASMH,TIMP%ADR(ITRAC)%P,YASMI(ITRAC),AM1,AM2,ZF,
     &              TBOR%ADR(ITRAC)%P,ATBOR%ADR(ITRAC)%P,
     &              BTBOR%ADR(ITRAC)%P,IT1,MASKTR,MESH,TB,
     &              T1,T2,T4,T10,TE1,TE2,TE3,
     &              KDIR,KDDL,
     &              DT,ENTET,TETAT,AGGLOT,INFOGT,BILMAS,
     &              OPTADV_TR(ITRAC),ISOUSI,OPDTRA,OPTBAN,
     &              MSK,MASKEL,MASKPT,MBOR,S,MASSOU(ITRAC),
     &              OPTSOU,SLVTRA(ITRAC),FLBOR,VOLU2D,V2DPAR,UNSV2D,
     &              2,FLBORTRA,FLULIM,YAFLULIM,FLULIMEBE,YAFLULIMEBE,
     &              DIRFLU,RAIN,PLUIE,TRAIN(ITRAC),
     &              FLODEL,YAFLODEL,MAXADV,TB2,NCO_DIST,NSP_DIST)
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM CVDFTR'
      ENDIF
!
      IF(BILMAS) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BILANT'
      CALL BILANT(H,T2,T3,DT,LT,TOTAL_ITER,ENTET,
     &            T%ADR(ITRAC)%P,AGGLOT,MASSOU(ITRAC),MASTR0(ITRAC),
     &            MASTR2(ITRAC),MASTEN(ITRAC),
     &            MASTOU(ITRAC),MSK,MASKEL,MESH,NUMLIQ%I,
     &            NFRLIQ,NPTFR,NAMETRAC(ITRAC),FLBORTRA,MASS_RAIN,
     &            TRAIN(ITRAC),MASTRAIN(ITRAC))
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BILANT'
!
      ENDDO
!
      ENDIF
!
!=======================================================================
!                    END OF DIFFUSION OF THE TRACER
!=======================================================================
!
!
!
      IF(INCLUS(COUPLING,'GAIA').AND.SUSP1) THEN
!
        CALL CONFIG_CODE(6)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING GAIA, STEP 2'
!
        CALL GAIA_STEP(
     &       LT,LEOPRD_CHARR,LISPRD,TOTAL_ITER,U,V,H,ZF,
     &       CF,CF,CF,CHESTR,CODE1,
     &       U,V,AT,VISC,DT,GAI_CPL%CHARR,SUSP1,
     &       ROEAU,GRAV,
     &       DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM GAIA, STEP 2'
!
        UCONV_GAI%R=>SAVE_UCONV
        VCONV_GAI%R=>SAVE_VCONV
!
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!
!=======================================================================
!           DIFFUSION AND SOURCE TERMS FOR K-EPSILON MODEL OR SA MODELS
!=======================================================================
!
      IF(ITURB.EQ.3.AND..NOT.ADJO) THEN
!
        IF (ENTET) CALL ENTETE(4,AT,LT)
!
! BEWARE THE MATRIX STRUCTURE (SYMMETRICAL OR NOT)
! WHEN CONSIDERING THE COUPLED SYSTEM K-E
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING KEPSIL'
        CALL KEPSIL(AK,EP,AKTILD,EPTILD,AKN,EPN,VISC,CF,U,V,H,
     &              UCONV,VCONV,KBOR,EBOR,LIMKEP%I,IELMK,IELME,
     &              CV1,CV2,TM1,BM1,BM2,CM2,TE1,TE2,NPTFR,DT,MESH,
     &              T1,T2,T3,TB,CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,
     &              KMIN,KMAX,EMIN,EMAX,INFOKE.AND.ENTET,MSK,MASKEL,
     &              MASKPT,S,SLVK,SLVEP,ICONVF(4),YASMH,
     &              YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM KEPSIL'
!
      ELSEIF(ITURB.EQ.6.AND..NOT.ADJO) THEN
!
        IF (ENTET) CALL ENTETE(14,AT,LT)
!
        IF (DEBUG.GT.0) WRITE(LU,*) 'CALLING SPALART_ALLMARAS'
        CALL SPALART_ALLMARAS(U, V, VISCSA, DT, NUN, NUTILD, PROPNU,
     &                        IELMNU,SLVNU,
     &                        INFONU.AND.ENTET,
     &                        MSK,MASKEL,MASKPT,NPTFR,LIMSA%I,
     &                        NUBOR,S,UCONV,VCONV,ICONVF(4),
     &                        BM1,TM1,CV1,CM2,T3,T1,T2,MESH,TB,
     &                        T4,WDIST,NUMIN,NUMAX,
     &                        YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE,
     &                        TE1,TE2,YASMH)
        IF(DEBUG.GT.0) WRITE(LU, *) 'BACK FROM SPALART_ALLMARAS'
      ENDIF
!
!=======================================================================
!  1)                 CHECKS MASS BALANCE
!=======================================================================
!
! CONTROL SECTIONS
!
      IF(NCP.NE.0.AND.(ENTET.OR.CUMFLO)) THEN
        CALL FLUSEC_TELEMAC2D(U,V,H,MESH%IKLE%I,
     &                        MESH%NELMAX,MESH%NELEM,
     &                        MESH%X%R,MESH%Y%R,DT,NCP,
     &                        CTRLSC,ENTET,AT,
     &                        MSKSEC,BM1,BM2,HPROP,MESH,S,CV1,
     &                        MESH%IFABOR%I,COMFLU,CUMFLO)
      ENDIF
!
! MASS BALANCE
!
      IF(BILMAS) THEN
!
        CALL BILAN(MESH,H,T1,AT,DT,LT,TOTAL_ITER,ENTET,
     &             MASSES,MSK,MASKEL,EQUA,TE5,OPTBAN,
     &             MESH%NPTFR,FLBOR,
     &             FLUX_BOUNDARIES,NUMLIQ%I,NFRLIQ,GAMMA)
!
        IF(NTRAC.GT.0) THEN
          IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
            DO ITRAC=1,NTRAC
              CALL BILANT1(H,U,V,H,T3,T4,T5,T6,
     &                     LT,TOTAL_ITER,ENTET,MASKTR,
     &                     T%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,TETAT,
     &                     MASSOU(ITRAC),MSK,MASKEL,MESH,FLUTSOR(ITRAC),
     &                     FLUTENT(ITRAC),EQUA,ITRAC)
            ENDDO
          ENDIF
        ENDIF
!
      ENDIF
!
!=======================================================================
!                           DROGUE(S)
!=======================================================================
!
      IF(NFLOT_MAX.NE.0.AND..NOT.SPILL_MODEL) THEN
!
        IF(ENTET) CALL ENTETE(12,AT,LT)
!
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',X=UCONV,Y=UCONV,Z=MESH%COSLAT)
          CALL OS('X=Y/Z   ',X=VCONV,Y=VCONV,Z=MESH%COSLAT)
        ENDIF
!
!       ADDING AND REMOVING DROGUES
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING FLOT'
        CALL FLOT(XFLOT%R,YFLOT%R,NFLOT,NFLOT_MAX,MESH%X%R,MESH%Y%R,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,TAGFLO%I,CLSFLO%I,
     &            SHPFLO%R,ELTFLO%I,MESH,LT,NIT,AT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM FLOT'
!
!       MOVING THEM
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DERIVE'
        CALL DERIVE(UCONV%R,VCONV%R,VCONV%R,DT,AT,
     &              MESH%X%R,MESH%Y%R,MESH%Y%R,
     &              MESH%IKLE%I,MESH%IFABOR%I,LT,IELM,UCONV%ELM,
     &              MESH%NDS(IELM,3),MESH%NDS(UCONV%ELM,3),
     &              NPOIN,NPOIN,
     &              NELEM,NELMAX,MESH%SURDET%R,XFLOT%R,YFLOT%R,YFLOT%R,
     &              SHPFLO%R,SHPFLO%R,TAGFLO%I,CLSFLO%I,ELTFLO%I,
     &              ELTFLO%I,NFLOT,NFLOT_MAX,MESH,
     &              IT1%I,T1%R,T2%R,T2%R,IT2%I,W1%R,W1%R,NPOIN,STOCHA,
     &              VISC,1,SVOID,0,
     &              AALGAE=ALGAE,DALGAE=DALGAE,RALGAE=RALGAE,
     &              EALGAE=EALGAE,TALGAE=TALGAE,
     &              YALGAE=YALGAE,
     &              REL_ALGAE=REL_ALGAE,TW1_ALGAE=TW1_ALGAE,
     &              TW2_ALGAE=TW2_ALGAE,A_ALGAE=A_ALGAE,
     &              ORBVEL=ORBVEL%R,
     &              AK=AK%R,EP=EP%R,H=H%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DERIVE'
!
!       PRINTING THEM
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING UTIMP_DROGUES'
        CALL UTIMP_DROGUES( LT,AT,NPOIN,NPOIN,
     &    XFLOT%R,YFLOT%R,YFLOT%R,TAGFLO%I,CLSFLO%I,
     &    NFLOT,NFLOT_MAX,FLOPRD,DEJA_DROGUES_UTIMP,
     &    T2D_FILES(T2DFLO),T2D_FILES(T2DBLO), MARDAT,MARTIM )
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM UTIMP_DROGUES'
!
        IF(SPHERI) THEN
          CALL OS('X=XY    ',X=UCONV,Y=MESH%COSLAT)
          CALL OS('X=XY    ',X=VCONV,Y=MESH%COSLAT)
        ENDIF
!
      ENDIF
!
!=======================================================================
!                        LAGRANGIAN DRIFT(S)
!=======================================================================
!
      IF(NLAG.NE.0) THEN
!
        IF (ENTET) CALL ENTETE(13,AT,LT)
!
          CALL DERLAG(UCONV%R,VCONV%R,DT,MESH%X%R,MESH%Y%R,
     &                LT,IELM,UCONV%ELM,3,NPOIN,NELEM,NELMAX,
     &                XLAG%R,YLAG%R,T1%R,T2%R,IT1%I,SHPLAG%R,
     &                DEBLAG%I,FINLAG%I,ELTLAG%I,NLAG,
     &                T7%R,T8%R,IT2%I,MESH)
!
      ENDIF
!
!=======================================================================
!                        ICEDYNAMICS
!=======================================================================
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BORDER_ICOVER'
        CALL BORDER_ICOVER(U,V, MESH )
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BORDER_ICOVER'
!        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING DYNAMICE'
!        THIS IS LAGARNGIAN PART- TO BE INTRODUCED SOON
!        CALL DYNAMICE(UCONV%R,VCONV%R,H%R,AT,MESH%X%R,MESH%Y%R,
!     &                LT,IELM,UCONV%ELM,NPOIN,NELEM,NELMAX,
!     &                MESH,ZF%R,CHESTR%R,WINDX%R,WINDY%R,T,
!     &                IND_T,MARDAT,MARTIM,
!     &                PLUIE%R)
!        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM DYNAMICE'
      ENDIF
!
!=======================================================================
!                     CREDIBILITY CHECKS
!                   LOOKS FOR A STEADY STATE
!=======================================================================
!
      ARRET1=.FALSE.
      IF(VERLIM) THEN
        CALL ISITOK(H%R,H%DIM1,U%R,U%DIM1,V%R,V%DIM1,NTRAC,
     &              T,T%ADR(1)%P%DIM1,
     &              MESH%X%R,MESH%Y%R,BORNES,ARRET1)
!       CORRECTION SUGGESTED BY NOEMIE DURAND (CHC-NRC) 04/01/2006
        IF(NCSIZE.GT.1) THEN
          STOP2=0
          IF(ARRET1) STOP2=1
          STOP2=P_MAX(STOP2)
          IF(STOP2.EQ.1) ARRET1=.TRUE.
        ENDIF
      ENDIF
      ARRET2=.FALSE.
      IF(STOPER) THEN
        CALL STEADY(H%R,HN%R,H%DIM1,U%R,UN%R,U%DIM1,V%R,VN%R,
     &              V%DIM1,NTRAC,T,TN,T%ADR(1)%P%DIM1,
     &              CRIPER,ARRET2)
!       CORRECTION BY NOEMIE DURAND (CHC-NRC) 04/01/2006
        IF(NCSIZE.GT.1) THEN
          STOP2=0
          IF(ARRET2) STOP2=1
          STOP2=P_MIN(STOP2)
          ARRET2=.NOT.(STOP2.EQ.0)
        ENDIF
      ENDIF
      IF(ARRET1.OR.ARRET2) THEN
        LEOPRD=1
        LISPRD=1
      ENDIF
!
      ARRET3=.FALSE.
      CALL TRAPSIG()
      IF(BREAKER) ARRET3=.TRUE.
!
      IF(ARRET1.OR.ARRET2.OR.ARRET3) THEN
        LEOPRD=1
        LISPRD=1
      ENDIF
!
! FH-BMD
!=============================================
!     FOR NEW COUPLING
      ENDDO ! ISIS_CFD
      IF (SIS_CPL%SISYPHE_CFD) SIS_CPL%CONSTFLOW = .TRUE.
999   CONTINUE
!
!=======================================================================
!     BGL @ BAW     COUPLING WITH NESTOR
!
      IF(NESTOR) CALL NESTOR_T2D_INTERFACE(2)
!
!=======================================================================
!
!     COUPLING WITH SISYPHE
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        CALL CONFIG_CODE(2)
!
        SUSP1=SIS_CPL%SUSP.AND.PERCOU.EQ.1
!
!=======================================================================
!       LEO IS COMMON FOR THE OUTPUT OF SISYPHE AND T2D
        CALL PRERES_TELEMAC2D()
!       KEEP THE SAME GRAPHIC COUNTER FOR T2D ANS SIS
        IF(LEO)COMPLEO=COMPLEO-1
!       MANAGE COUPLING PERIOD CALLS
!
        YASIS = .FALSE.
        YASIS = PERCOU*(LT/PERCOU).EQ.LT
!
        IF(SUSP1.OR.(SIS_CPL%CHARR.AND.YASIS))THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING SISYPHE, CHARRIAGE'
          CALL SISYPHE(1,LT,LEOPRD_CHARR,LISPRD,NIT,U,V,H,HN,ZF,
     &                 CF,CF,CHESTR,SIS_CPL%CONSTFLOW,SIS_CPL%NSIS_CFD,
     &                 SIS_CPL%SISYPHE_CFD,CODE1,
     &                 PERCOU,U,V,AT,VISC,DT*PERCOU,SIS_CPL%CHARR,SUSP1,
     &                 FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV,
     &                 DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN APPEL SISYPHE, CHARRIAGE'
!
        ENDIF
!
        IF(SIS_CPL%SUSP.AND.PERCOU.NE.1) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING SISYPHE, SUSPENSION'
          CALL SISYPHE(1,LT,LEOPRD,LISPRD,NIT,U,V,H,HN,ZF,
     &                 CF,CF,CHESTR,SIS_CPL%CONSTFLOW,SIS_CPL%NSIS_CFD,
     &                 SIS_CPL%SISYPHE_CFD,
     &                 CODE1,1,U,V,AT,VISC,DT,CHARR_TEL,SIS_CPL%SUSP,
     &                 FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV,
     &                 DIRMOY,HM0,TPR5,ORBVEL,LEO,GRCOMP=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN APPEL DE SISYPHE, SUSPENSION'
!
        ENDIF
!
        CALL CONFIG_CODE(1)
!
      ENDIF


800   IF(PASS.EQ.1) RETURN
!
!=======================================================================
!                      WRITES OUT THE RESTART FILE
!=======================================================================
!
      IF(.NOT.ADJO.AND.CODE(1:7).NE.'ESTEL3D') THEN
        CALL PRERES_TELEMAC2D()
!
!       OUTPUT FOR RESTART
!
        IF(RESTART_MODE.AND.T2D_FILES(T2DRST)%NAME(1:1).NE.' ') THEN
          IF(RSTPRD.NE.0.AND.MOD(LT,RSTPRD).EQ.0) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_DESIMP RESTART'
            CALL BIEF_DESIMP(T2D_FILES(T2DRST)%FMT,VARSOR,NPOIN,
     &                       T2D_FILES(T2DRST)%LU,AT,LT,1,RSTPRD,
     &                       SOREST,SORIS2,MAXVAR,TEXTE,0,0)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_DESIMP RESTART'
          ELSEIF(RESTART_RECORD.EQ.-1.AND.LT.EQ.TOTAL_ITER
     &       .OR.RESTART_RECORD.NE.-1.AND.LT.EQ.RESTART_RECORD) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_DESIMP RESTART'
            CALL BIEF_DESIMP(T2D_FILES(T2DRST)%FMT,VARSOR,NPOIN,
     &                       T2D_FILES(T2DRST)%LU,AT,LT,1,LT,
     &                       SOREST,SORIS2,MAXVAR,TEXTE,0,0)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_DESIMP RESTART'
          ENDIF
        ENDIF
      ENDIF
!
!=======================================================================
!                      WRITES OUT THE RESULTS
!=======================================================================
!
      IF(ADJO) THEN
!
        IF(T2D_FILES(T2DRBI)%NAME.NE.' '.AND.
     &     INCLU2(ESTIME,'DEBUG')) THEN
          CALL BIEF_DESIMP(T2D_FILES(T2DRBI)%FMT,VARSORA,
     &                     NPOIN,T2D_FILES(T2DRBI)%LU,
     &                     -AT,LT,LISPRD,1,
     &                     SORLEOA,SORIMPA,MAXVAR,TEXTE,PTINIG,PTINIL)
        ENDIF
!
      ELSE
!
        IF(CODE(1:7).EQ.'ESTEL3D') THEN
!
!         SAVES THE DEPTH FOR ESTEL3D
          CALL DEPTH_FILL(H%R)
!
! (NOTE THAT OUTPUTS ARE DONE WITHIN ESTEL3D IN COUPLED MODE)
!
        ELSE
          ! Keeping in memory the value of leoprd as it will be
          ! set to 1 by preres_telemac2d on the last time step
          OLD_LEOPRD = LEOPRD
!
!          CALL PRERES_TELEMAC2D()
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_DESIMP',LEO, LT, AT
          CALL BIEF_DESIMP(T2D_FILES(T2DRES)%FMT,VARSOR,
     &            NPOIN,T2D_FILES(T2DRES)%LU,AT,LT,
     &            LISPRD,OLD_LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL,
     &            IIMP=IMP,ILEO=LEO,COMPGRAPH=COMPLEO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_DESIMP',LEO
!
          IF(INCLUS(COUPLING,'KHIONE')) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING OUTPUT_KHIONE'
            CALL OUTPUT_KHIONE(
     &        TITCAS,        ! TITLE
     &        NPOIN,         ! LOCAL NUMBER OF POINTS
     &        AT,LT,         ! TIME AND TIME NUMBER
     &        MESH,
     &        VARSOR,        ! POINTERS TO TELEMAC VARIABLES
     &        MARDAT,        ! START DATE
     &        MARTIM,        ! START TIME
     &        .FALSE.,       ! WHETHER TO WRITE THE FILE HEADER
     &        .FALSE.,       ! WHETHER TO WRITE THE FILE MESH
     &        .TRUE.,        ! WHETEHR TO WRITE THE VARIABLES AT AT
     &        T)
            IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM OUTPUT_KHIONE'
          ENDIF
        ENDIF
!
!
        IF(INCLUS(COUPLING,'DELWAQ')) THEN
!
!         T3 : MODIFIED DEPTH TO TAKE INTO ACCOUNT MASS-LUMPING
!              IN THE CONTINUITY EQUATION
          IF(ABS(1.D0-AGGLOC).GT.1.D-8) THEN
            CALL VECTOR(T3,'=','MASVEC          ',IELMH,
     &                  1.D0-AGGLOC,H ,S,S,S,S,S,MESH,MSK,MASKEL)
            IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
            CALL OS('X=XY    ',X=T3 ,Y=UNSV2D)
            CALL OS('X=X+CY  ',X=T3 ,Y=H ,C=AGGLOC)
          ELSE
            CALL OS('X=Y     ',X=T3 ,Y=H )
          ENDIF
!
!         NOTE: FLODEL IS DONE IN CORRECTION_DEPTH_2D
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING TEL4DELWAQ'
          CALL TEL4DELWAQ(MESH%NPOIN,MESH%NPOIN,MESH%NSEG,
     &                 MESH%IKLE%I,MESH%ELTSEG%I,MESH%GLOSEG%I,
     &                 MESH%GLOSEG%DIM1,
     &                 MESH%X%R,MESH%Y%R,MESH%NPTFR,LIHBOR%I,
     &                 MESH%NBOR%I,1,AT,DT,LT,NIT,T3%R,HPROP%R,T3%R,
     &                 UDEL%R,VDEL%R,T%ADR(MAX(IND_S,1))%P%R,
     &                 T%ADR(MAX(IND_T,1))%P%R,
     &                 VISC%R,TITCAS,T2D_FILES(T2DGEO)%NAME,
     &                 T2D_FILES(T2DCLI)%NAME,WAQPRD,
     &                 T2DDL1,T2DDL2,T2DDL3,T2DDL5,T2DDL6,T2DDL7,
     &                 T2DL11,T2DDL4,T2DDL8,T2DDL9,T2DL10,
     &                 ENTET,NELEM,SALI_DEL,
     &                 TEMP_DEL,VELO_DEL,DIFF_DEL,
     &                 MARDAT,MARTIM,FLODEL%R,
     &                 V2DPAR%R,MESH%KNOLG%I,T2D_FILES)
          IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM TEL4DELWAQ'
!
        ENDIF
!
      ENDIF  !(ADJO)
!
!     OPTIONAL USER OUTPUT (COURTESY JACEK JANKOWSKI, BAW)
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING UTIMP_TELEMAC2D'
      CALL UTIMP_TELEMAC2D(LT,AT,PTINIG,LEOPRD,PTINIL,LISPRD)
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM UTIMP_TELEMAC2D'
!
!=======================================================================
!              COMPARISON AGAINST A REFERENCE FILE
!=======================================================================
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
      IF(VALID) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING BIEF_VALIDA'
        CALL BIEF_VALIDA(TB,TEXTPR,
     &                   T2D_FILES(T2DREF)%LU,T2D_FILES(T2DREF)%FMT,
     &                   VARSOR,TEXTE,
     &                   T2D_FILES(T2DRES)%LU,T2D_FILES(T2DRES)%FMT,
     &                   MAXVAR,NPOIN,LT,TOTAL_ITER,ALIRE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM BIEF_VALIDA'
      ENDIF


!=======================================================================
!              REDEFINING THE ALGAE VARIABLES FOR THE NEXT
!                            TIME STEP
!=======================================================================
!
! UPDATE THE ALGAE VARIABLES AT T_0 FOR THE CALCULATIONS OF THE NEXT TIME STEP
      IF(ALGAE) THEN
        CALL OS('X=Y     ',X=U_X_AV_0,Y=U_X_AV)
        CALL OS('X=Y     ',X=U_Y_AV_0,Y=U_Y_AV)
        CALL OS('X=Y     ',X=U_Z_AV_0,Y=U_Z_AV)
        CALL OS('X=Y     ',X=K_AV_0,Y=K_AV)
        CALL OS('X=Y     ',X=EPS_AV_0,Y=EPS_AV)
        CALL OS('X=Y     ',X=U_X_0,Y=U_X)
        CALL OS('X=Y     ',X=U_Y_0,Y=U_Y)
        CALL OS('X=Y     ',X=U_Z_0,Y=U_Z)
        CALL OS('X=Y     ',X=V_X_0,Y=V_X)
        CALL OS('X=Y     ',X=V_Y_0,Y=V_Y)
        CALL OS('X=Y     ',X=V_Z_0,Y=V_Z)
      ENDIF
!
!=======================================================================
!
!  NEAT (PROGRAMMED) STOP OF THE MODEL:
!
      IF(ARRET1) THEN
        WRITE(LU,*)
        WRITE(LU,*) 'LIMIT VALUES TRESPASSED, TELEMAC-2D IS STOPPED'
        WRITE(LU,*)
        RETURN
      ENDIF
      IF(ARRET2) THEN
        WRITE(LU,*)
        WRITE(LU,*) 'STEADY STATE REACHED, TELEMAC-2D IS STOPPED'
        WRITE(LU,*)
        RETURN
      ENDIF
      IF(ARRET3) THEN
        CALL ENTETE(1,AT,LT)
        WRITE(LU,*)
        WRITE(LU,*) 'TELEMAC-2D CHECKPOINTED BY THE USER'
        WRITE(LU,*) 'USING SIGNAL ',SIGUSR1
        WRITE(LU,*)
        RETURN
      ENDIF
!
!     NOW ADVECTION SCHEME WILL BE CHANGED AND FLULIM
!     WILL NOT CORRESPOND TO IT.
!
      YAFLULIM=.FALSE.
      YAFLULIMEBE=.FALSE.
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_TIMESTEP_END
#endif
!
! 700: TIME LOOP
!
      ! Work around for finite volume with api
      IF((EQUA(1:15).EQ.'SAINT-VENANT VF')
     &   .AND.PASS.EQ.2.AND.LT.LT.NIT) THEN
        GOTO 666
      ENDIF
      IF(LT.LT.NIT) GO TO 700
!
!=======================================================================
!
! :                 /* END OF THE LOOP IN TIME */
!
!=======================================================================
!
      IF(PASS.NE.2) THEN
        IF(LISTIN) WRITE(LU,19)
19      FORMAT(/,1X,'END OF TIME LOOP',////)
      ENDIF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_END
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
