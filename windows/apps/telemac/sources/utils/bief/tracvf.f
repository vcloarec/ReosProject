!                   *****************
                    SUBROUTINE TRACVF
!                   *****************
!
     &(F,FSCEXP,FXMAT,FXMATPAR,
     & VOLU2D,UNSV2D,DDT,FXBOR,FBOR,SMH,YASMH,
     & T1,T2,T4,T5,T7,T8,
     & MESH,LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,DT,RAIN,PLUIE,
     & TRAIN,MASSOU,MASS_BALANCE)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    COMPUTES THE TRACER FOR FINITE VOLUME SCHEME.
!+                TO COMPLETE.
!
!history  C-T PHAM (LNHE)
!+        06/02/09
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
!
!history  J-M HERVOUET (LNHE)
!+        08/06/2015
!+        V7P1
!+   Now mass balance done on request.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DDT            |-->| SUB TIME-STEP
!| DT             |-->| TIME-STEP
!| F              |<--| VALUES OF F AT TIME N+1.
!| FBOR           |-->| VALUES OF F AT THE PRESCRIBED BOUNDARIES
!| FLBORTRA       |<->| FLUX OF TRACER AT THE BOUNDARIES
!| FSCEXP         |-->| EXPLICIT SOURCE TERM FOR F
!| FXBOR          |-->| FLUXES ON BOUNDARIES
!| FXMAT          |-->| FLUXES (NON ASSEMBLED IN PARALLEL)
!| FXMATPAR       |-->| FLUXES (ASSEMBLED IN PARALLEL)
!| H              |-->| WATER DEPTH AT TIME N+1
!| HN             |-->| WATER DEPTH AT TIME N
!| IOPT2          |-->| 0 : UCONV OBEYS CONTINUITY EQUATION
!|                |   | 1 : UCONV DOES NOT OBEY CONTINUITY EQUATION
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMTRA         |-->| TECHNICAL BOUNDARY CONDITIONS FOR TRACERS
!| MASS_BALANCE   |-->| IF YES, ALL TERMS FOR MASS BALANCE
!|                |   | WILL BE COMPUTED
!| MASSOU         |-->| MASS ADDED BY SOURCE TERM
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, MASKING OF DRY ELEMENTS
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PLUIE          |-->| RAIN OR EVAPORATION IN M/S, IN BIEF_OBJ STRUCTURE
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| T1             |<->| BIEF_OBJ STRUCTURE USED AS WORK ARRAY
!| T2             |<->| BIEF_OBJ STRUCTURE USED AS WORK ARRAY
!| T4             |<->| BIEF_OBJ STRUCTURE USED AS WORK ARRAY
!| T5             |<->| BIEF_OBJ STRUCTURE USED AS WORK ARRAY
!| T6             |<->| BIEF_OBJ STRUCTURE USED AS WORK ARRAY
!| T7             |<->| BIEF_OBJ STRUCTURE USED AS WORK ARRAY
!| T8             |<->| BIEF_OBJ STRUCTURE USED AS WORK ARRAY
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TRACVF => TRACVF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: KDIR,KDDL,OPTSOU,LIMTRA(*)
      INTEGER, INTENT(IN)             :: IOPT2
      DOUBLE PRECISION, INTENT(IN)    :: DDT,DT,TRAIN
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F,T1,T2,T4,T5,T7,T8,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(IN)      :: SMH,FBOR,FSCEXP
      TYPE(BIEF_OBJ), INTENT(IN)      :: FXBOR,UNSV2D,VOLU2D,PLUIE
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(*),FXMATPAR(*)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      LOGICAL, INTENT(IN)             :: YASMH,MSK,RAIN,MASS_BALANCE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      IF(IOPT2.EQ.0) THEN
!
!-----------------------------------------------------------------------
!
!     CASE WHERE THE ADVECTION FIELD SATISFIES THE CONTINUITY EQUATION
!     (THE DEPTH COULD BE COMPUTED BY INTERPOLATION IN TIME)
!
!     T4 WILL TAKE THE SUCCESSIVE VALUES OF F (INITIALISED IN CVTRVF)
!
      CALL TVF(F%R,T4%R,T5%R,FXMAT,FXMATPAR,VOLU2D%R,UNSV2D%R,DDT,
     &         FXBOR%R,T7%R,T8,FBOR%R,SMH%R,YASMH,FSCEXP%R,
     &         MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &         MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &         MESH%NBOR%I,LIMTRA,KDIR,KDDL,
     &         OPTSOU,T5%R,IOPT2,FLBORTRA%R,DDT/DT,MESH,RAIN,PLUIE%R,
     &         TRAIN,MASSOU,MASS_BALANCE)
!
!-----------------------------------------------------------------------
!
!     CASE WHERE THE ADVECTION FIELD DOES NOT SATISFY THE CONTINUITY EQUATION
!
      ELSEIF(IOPT2.EQ.1) THEN
!
!     T1 WILL TAKE THE SUCCESSIVE VALUES OF HN COMPUTED WITH CONTINUITY
!     T2 WILL TAKE THE SUCCESSIVE VALUES OF H COMPUTED WITH CONTINUITY
!     T4 WILL TAKE THE SUCCESSIVE VALUES OF F
!     T5 WILL TAKE THE SUCCESSIVE VALUES OF TRUE DEPTH
!
!     H2 DEPTH BY CONTINUITY EQUATION
!
      CALL HVF(T2%R,T1%R,FXMAT,UNSV2D%R,DDT,T7%R,SMH%R,
     &         YASMH,MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &         MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH%NBOR%I,OPTSOU,
     &         T8,MESH,MSK,RAIN,PLUIE%R)
!
      CALL TVF(F%R,T4%R,T2%R,FXMAT,FXMATPAR,VOLU2D%R,UNSV2D%R,DDT,
     &         FXBOR%R,T7%R,T8,FBOR%R,SMH%R,YASMH,FSCEXP%R,
     &         MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &         MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &         MESH%NBOR%I,LIMTRA,KDIR,KDDL,
     &         OPTSOU,T5%R,IOPT2,FLBORTRA%R,DDT/DT,MESH,RAIN,PLUIE%R,
     &         TRAIN,MASSOU,MASS_BALANCE)
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'TRACVF: UNKNOWN OPTION'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

