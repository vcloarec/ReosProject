!                   ************************
                    PROGRAM HOMERE_TELEMAC2D
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    1) READS ALL NECESSARY DATA.
!+
!+            2) CALLS TELEMAC2D AND SISYPHE IN CASE OF COUPLING.
!
!note     IN CASE OF PARAMETER ESTIMATION, HOMERE_ADJ_T2D IS
!+            CALLED INSTEAD OF HOMERE_TELEMAC2D.
!
!history  R. ATA
!+        10/11/2014
!+        V7P0
!+        add waq variables for lecdon_telemac2d
!
!history  F. HUANG (CLARKSON U.) AND S.E. BOURBAN (HRW)
!+        19/11/2016
!+        V7P3
!+        Coupling TELEMAC-2D with KHIONE (ice modelling component)
!
!history  A. LEROY (LNHE) & J-M HERVOUET (jubilado)
!+        26/09/2017
!+        V7P3
!+        Convergence procedure updated.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC, ONLY : COUPLING,DEJA_PDEPT_NERD,
     &                                 DEJA_PDEPT_ERIA,INDIC_PDEPT_NERD,
     &                                 INDIC_PDEPT_ERIA,
     &                                 DEJA_CPOS,INDIC_CPOS,
     &                                 DEJA_CPOS2,INDIC_CPOS2
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SISYPHE, ONLY : SIS_FILES,MAXLU_SIS
!                                                       WAC2
      USE DECLARATIONS_TOMAWAC, ONLY : WAC_FILES,MAXLU_WAC,
     &    WACGEO, MESH_TOM=>MESH, COUROU_TEL, VENT_TEL, COPSIS_TEL
      USE DECLARATIONS_WAQTEL,  ONLY : WAQ_FILES,MAXLU_WAQ
      USE DECLARATIONS_KHIONE,  ONLY : ICE_FILES,MAXLU_ICE
      USE DECLARATIONS_GAIA,    ONLY : GAI_FILES,MAXLU_GAI
      USE INTERFACE_TELEMAC2D
      USE INTERFACE_WAQTEL
      USE INTERFACE_STBTEL
      USE INTERFACE_KHIONE
      USE INTERFACE_TOMAWAC
      USE DECLARATIONS_SPECIAL
      USE COUPLE_MOD
!
      IMPLICIT NONE
!
      INTEGER TDEB(8),TFIN(8),NCAR
!
      CHARACTER(LEN=24), PARAMETER :: CODE2='SISYPHE                 '
      CHARACTER(LEN=24), PARAMETER :: CODE3='TOMAWAC                 '
      CHARACTER(LEN=24), PARAMETER :: CODE4='WAQTEL                  '
      CHARACTER(LEN=24), PARAMETER :: CODE5='KHIONE                  '
      CHARACTER(LEN=24), PARAMETER :: CODE6='GAIA                    '
!
      CHARACTER(LEN=MAXLENTMPDIR) PATH
      CHARACTER(LEN=PATH_LEN) MOTCAR(MAXKEYWORD),FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=PATH_LEN) DUMMY
!
      INTEGER ITRAC
      INTEGER, DIMENSION(:,:), TARGET, ALLOCATABLE :: CORRESP
!
!======================================================================
!
#if defined COMPAD
      CALL AD_TELEMAC2D_MAIN_INIT
#endif
!
!-----------------------------------------------------------------------
!
!     INITIALISES FILES (NAMES OF FILES=' ' AND LOGICAL UNITS =0)
!     GETTING NCSIZE BY CALLING P_INIT
!
      CALL BIEF_INIT(PATH,NCAR,.TRUE.)
!
!     INITIAL TIME FOR COMPUTATION DURATION
!
      CALL DATE_AND_TIME(VALUES=TDEB)
!
!     PRINTS BANNER TO LISTING
!
      CALL PRINT_HEADER(CODE1,'                        ')
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
!
      DUMMY = ' '
!
      CALL LECDON_TELEMAC2D(MOTCAR,FILE_DESC,
     &                      PATH,NCAR,DUMMY,DUMMY)
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_MAIN_AFTER_LECDON_TELEMAC2D
#endif
!
!-----------------------------------------------------------------------
!
!     OPENS THE FILES FOR TELEMAC2D
!
      CALL BIEF_OPEN_FILES(CODE1,T2D_FILES,MAXLU_T2D,PATH,NCAR,
     &                     1,.FALSE.)
!
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY (AND LEVEL 0 OF REFINEMENT IF CONVERGENCE STUDY)
!
      RLEVEL=0
      CALL POINT_TELEMAC2D
!
!-----------------------------------------------------------------------
!
!     INITIALISES SISYPHE
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
!
        CALL PRINT_HEADER(CODE2,CODE1)
!
      CALL LECDON_SISYPHE(MOTCAR,FILE_DESC,PATH,NCAR,
     &                    CODE1,DUMMY,DUMMY)
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_MAIN_AFTER_LECDON_SISYPHE
#endif
!
!-----------------------------------------------------------------------
!
      CALL BIEF_OPEN_FILES(CODE2,SIS_FILES,MAXLU_SIS,PATH,NCAR,
     &                     2,.FALSE.)
!
!     RESETS TELEMAC2D CONFIGURATION
!
      CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
      CALL POINT_SISYPHE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES GAIA
!
      IF(INCLUS(COUPLING,'GAIA')) THEN
!
        CALL PRINT_HEADER(CODE6,CODE1)
!
        CALL LECDON_GAIA(MOTCAR,FILE_DESC,PATH,NCAR,
     &                   CODE1,DUMMY,DUMMY)
!
!-----------------------------------------------------------------------
!
!     ALGORITHMIC DIFFERENTIATION
#if defined COMPAD
      CALL AD_TELEMAC2D_MAIN_AFTER_LECDON_GAIA
#endif
!
!-----------------------------------------------------------------------
!
      CALL BIEF_OPEN_FILES(CODE6,GAI_FILES,MAXLU_GAI,PATH,NCAR,
     &                     2,.FALSE.)
!
!     RESETS TELEMAC2D CONFIGURATION
!
      CALL CONFIG_CODE(1)
!
!     MEMORY ORGANISATION
!
      CALL POINT_GAIA
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES TOMAWAC
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
!
!                                                       WAC2
        IF(INCLUS(COUPLING,'TOMAWAC2')) CALL INIT_COUPLE
        CALL PRINT_HEADER(CODE3,CODE1)
!
        CALL T2D_WAC_CPL_UPDATE(PART=0)
        CALL LECDON_TOMAWAC(FILE_DESC,PATH,NCAR,DUMMY,DUMMY,PART=0)
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
        CALL AD_TELEMAC2D_MAIN_AFTER_LECDON_TOMAWAC
#endif
!
!-----------------------------------------------------------------------
!
        CALL BIEF_OPEN_FILES(CODE3,WAC_FILES,MAXLU_WAC,PATH,NCAR,
     &                     3,.FALSE.)
!
!     RESETS TELEMAC2D CONFIGURATION
!
        CALL CONFIG_CODE(1)
!     SET TELEMAC PARAMETERS IN TOMAWAC
        COUROU_TEL = COUROU
        VENT_TEL   = VENT
        COPSIS_TEL = INCLUS(COUPLING,'SISYPHE').OR.
     &               INCLUS(COUPLING,'GAIA')

!
!     MEMORY ORGANISATION
!
        CALL POINT_TOMAWAC

!     COUPLING WITH MESHES THAT ARE NOT EQUAL
!     SENDING DATA FROM TELEMAC TO TOMAWAC
        IF(INCLUS(COUPLING,'TOMAWAC2')) THEN
          CALL ADD_SENDER(MESH,1)
          CALL ADD_RECEIVER(MESH_TOM,WAC_FILES(WACGEO),
     &    'TEL2TOM         ',1,NVARTEL2TOM)

!     SENDING DATA FROM TOMAWAC TO TELEMAC
          CALL ADD_SENDER(MESH_TOM,2)
          CALL ADD_RECEIVER(MESH,T2D_FILES(T2DGEO),
     &         'TOM2TEL         ',2,NVARTOM2TEL)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES WAQTEL
!
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
!
!
        CALL PRINT_HEADER(CODE4,CODE1)
!
        CALL LECDON_WAQTEL(FILE_DESC,PATH,NCAR,DUMMY,DUMMY)
!
        CALL BIEF_OPEN_FILES(CODE4,WAQ_FILES,MAXLU_WAQ,PATH,NCAR,
     &                       4,.FALSE.)
!
!       RESETS TELEMAC2D CONFIGURATION
!
        CALL CONFIG_CODE(1)
!
!       MEMORY ORGANISATION
!
        CALL POINT_WAQTEL(MESH,IELMT)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES KHIONE
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
!
        CALL PRINT_HEADER(CODE5,CODE1)
!
        CALL LECDON_KHIONE(FILE_DESC,PATH,NCAR)
        CALL BIEF_OPEN_FILES(CODE5,ICE_FILES,MAXLU_ICE,PATH,NCAR,
     &                       5,.FALSE.)
!
!       MEMORY ORGANISATION
!
        CALL POINT_KHIONE(MESH,IELMT)
!
!       RESETS TELEMAC2D CONFIGURATION
!
        CALL CONFIG_CODE(1)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!=======================================================================
!
      IF(ESTIME.EQ.' '.AND..NOT.CONVERGENCE) THEN
!
!-----------------------------------------------------------------------
!
!       STANDARD MODE: ONE TELEMAC2D CALL
!
        CALL TELEMAC2D(PASS=-1,ATDEP=0.D0,NITER=0,CODE='       ')
!
!-----------------------------------------------------------------------
!
      ELSE IF (ESTIME.EQ.' '.AND.CONVERGENCE) THEN
!
!       CONVERGENCE MODE
!       MESH REFINEMENT WITH RLEVELS DIVISIONS
!
        WRITE(LU,*) 'TELEMAC2D LAUNCHED IN CONVERGENCE MODE'
        WRITE(LU,*) '--------------------------------------'
        WRITE(LU,*) 'GENERATE THE FINEST MESH'
        WRITE(LU,*) 'WITH ', FINEMESH%NELMAX, 'ELEMENTS MAX'
        WRITE(LU,*) 'WITH ', FINEMESH%NELEM, 'INITIAL ELEMENTS'
        WRITE(LU,*) 'WITH ', FINEMESH%NPMAX, 'POINTS MAX'
        WRITE(LU,*) 'WITH ', FINEMESH%NPOIN, 'INITIAL POINTS'
        WRITE(LU,*) 'WITH ', FINEMESH%NPTFRX, 'BOUNDARY POINTS MAX'
        WRITE(LU,*) 'WITH ', FINEMESH%NPTFR, 'INITIAL BOUNDARY POINTS'
        ALLOCATE(CORRESP(FINEMESH%NELMAX,RLEVELS))
        CALL REFINE_MESH(RLEVELS,FINEMESH,FINEMESH%NELMAX,
     &                   FINEMESH%NPTFRX,NTRAC,.FALSE.,CORRESP=CORRESP)
        WRITE(LU,*) 'END OF THE FINE MESH GENERATION'
!
!       RLEVELS+1 TELEMAC2D CALLS WITH SUCCESSIVE REFINEMENTS
!
        WRITE(LU,*) 'ENTERING THE LOOP OF TELEMAC2D CALLS'
        DO RLEVEL = 1,RLEVELS+1
!
          WRITE(LU,*) 'CALL NUMBER ',RLEVEL,' TO TELEMAC2D'
          CALL TELEMAC2D(PASS=-1,ATDEP=0.D0,NITER=0,CODE='       ',
     &                   CONVERGENCE_LEVEL=RLEVEL)
!
          IF(NTRAC.GT.0) THEN
            DO ITRAC=1,NTRAC
              WRITE(LU,*) 'ERROR ON THE TRACER',ITRAC
              CALL ERROR_COMPUTATION(T%ADR(ITRAC)%P%R,MESH,FINEMESH,
     &                               FINEMESH%NELMAX,MESH%NPOIN,
     &                               CORRESP,RLEVELS,RLEVEL,MESH%IKLE%I,
     &                               FINEMESH%IKLE%I)
            ENDDO
          ENDIF
!
          IF(RLEVEL.EQ.RLEVELS+1) EXIT
          CALL REFINE_MESH(1,MESH,MESH%NELMAX,MESH%NPTFRX,
     &                     NTRAC,.TRUE.,LIHBOR=LIHBOR%I,LIUBOR=LIUBOR%I,
     &                     LIVBOR=LIVBOR%I,LITBOR=LITBOR,HBOR=HBOR%R,
     &                     UBOR=UBOR%R,VBOR=VBOR%R,CHBORD=CHBORD%R,
     &                     TBOR=TBOR,ATBOR=ATBOR,BTBOR=BTBOR,ZF=ZF)
!         NEW SIZES OF ARRAYS
          !CALL DEALL_TELEMAC2D(DEALL_LECDON=.FALSE.)
          CALL POINT_TELEMAC2D
          IF(DEJA_PDEPT_NERD) THEN
            DEALLOCATE(INDIC_PDEPT_NERD)
            DEJA_PDEPT_NERD=.FALSE.
          ENDIF
          IF(DEJA_PDEPT_ERIA) THEN
            DEALLOCATE(INDIC_PDEPT_ERIA)
            DEJA_PDEPT_ERIA=.FALSE.
          ENDIF
          IF(DEJA_CPOS) THEN
            DEALLOCATE(INDIC_CPOS)
            DEJA_CPOS=.FALSE.
          ENDIF
          IF(DEJA_CPOS2) THEN
            DEALLOCATE(INDIC_CPOS2)
            DEJA_CPOS2=.FALSE.
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
!       PARAMETER ESTIMATION MODE : CALLS HOMERE_ADJ_T2D
!
        CALL HOMERE_ADJ_T2D
!
      ENDIF
!
!=======================================================================
!
!     CLOSES FILES
!
      CALL BIEF_CLOSE_FILES(T2D_FILES,MAXLU_T2D,.TRUE.)
!
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
        CALL CONFIG_CODE(2)
        CALL BIEF_CLOSE_FILES(SIS_FILES,MAXLU_SIS,.FALSE.)
        CALL DEALL_SISYPHE
      ENDIF
!
      IF(INCLUS(COUPLING,'GAIA')) THEN
        CALL CONFIG_CODE(6)
        CALL BIEF_CLOSE_FILES(GAI_FILES,MAXLU_GAI,.FALSE.)
        CALL DEALL_GAIA
      ENDIF
!
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
        CALL CONFIG_CODE(3)
        CALL BIEF_CLOSE_FILES(WAC_FILES,MAXLU_WAC,.FALSE.)
        CALL DEALL_TOMAWAC()
      ENDIF
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
        CALL CONFIG_CODE(4)
        CALL BIEF_CLOSE_FILES(WAQ_FILES,MAXLU_WAQ,.FALSE.)
      ENDIF
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        CALL CONFIG_CODE(5)
        CALL BIEF_CLOSE_FILES(ICE_FILES,MAXLU_ICE,.FALSE.)
      ENDIF

      CALL DEALL_TELEMAC2D(.TRUE.)
      CALL DEALL_BIEF
!
!-----------------------------------------------------------------------
!
#if defined COMPAD
      CALL AD_TELEMAC2D_MAIN_FINALIZE
#endif
!
!-----------------------------------------------------------------------
!
      WRITE(LU,11)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
!
!     TIME OF END OF COMPUTATION
!
      CALL DATE_AND_TIME(VALUES=TFIN)
      CALL ELAPSE(TDEB,TFIN)
!
!-----------------------------------------------------------------------
!
      STOP 0
      END

