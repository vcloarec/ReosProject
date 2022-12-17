!                   **********************
                    SUBROUTINE CVTRVF_ERIA
!                   **********************
!
     &(F,FN,FSCEXP,H,HN,HPROP,UDEL,VDEL,DM1,ZCONV,SOLSYS,
     & SM,SMH,YASMH,SMI,YASMI,FBOR,MASKTR,MESH,
     & T1,T2,T3,T4,T5,T6,T7,HT,DT,ENTET,
     & MSK,MASKEL,OPTSOU,LIMTRA,KDIR,KDDL,NPTFR,FLBOR,
     & YAFLBOR,UNSV2D,IOPT,FLBORTRA,NBOR,
     & RAIN,PLUIE,TRAIN,NITMAX,NCO_DIST,OPTADV)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    Distributive advection scheme ERIA
!
!warning  SEE BELOW FOR DEFINITION OF IOPT1 AND IOPT2, RETRIEVED FROM IOPT
!+        IOPT2=1 NOT TREATED HERE, MASS-CONSERVATION WILL BE DOWNGRADED
!+        IN THIS CASE (A CORRECT TREATMENT MAY RESULT IN INFINITE F)
!+        THE PROGRAM WILL NOT STOP IF IOPT2=1
!
!history  J-M HERVOUET   (EDF LAB, LNHE)
!+        09/09/2016
!+        V7P2
!+   First version. Comes from the former cvtrvf_pos with option 1.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/11/2016
!+        V7P3
!+   Adding an option for predictor for sharing volumes of points
!+   between elements (see variable OPTPRE, OPTPRE=1 old strategy,
!+   OPTPRE=2, new and simpler strategy). Tests do not show much
!+   difference, kept here to be tested in other cases.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        02/01/2017
!+        V7P3
!+   Second order in time added.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        09/09/2017
!+        V7P3
!+   Treating cases where NELMAX is not equal to NELEM.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| DT             |-->| TIME STEP
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS
!|                |   | CONSERVATION.
!| F              |<--| F AT TIME T(N+1)
!| FBOR           |-->| DIRICHLET CONDITIONS ON F.
!| FLBOR          |-->| FLUXES AT BOUNDARIES
!| FLBORTRA       |<->| TRACER FLUXES AT BOUNDARIES
!| FN             |-->| F AT TIME T(N)
!| FSCEXP         |-->| EXPLICIT PART OF THE SOURCE TERM
!|                |   | EQUAL TO ZERO EVERYWHERE BUT ON SOURCES
!|                |   | WHERE THERE IS FSCE - (1-TETAT) FN
!|                |   | SEE DIFSOU
!| HT             |<--| WORK ARRAY (DEPTH GOING FROM HN TO H)
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
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NCO_DIST       |-->| NUMBER OF CORRECTIONS IN DISTRIBUTIVE SCHEMES
!| NITMAX         |-->| MAXIMUM NUMBER OF ITERATIONS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPTADV         |-->| FOR ERIA SCHEME 1: FIRST ORDER
!|                |   |                 2: SECOND ORDER
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SM             |-->| SOURCE TERMS.
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SMI            |-->| IMPLICIT SOURCE TERM
!| SOLSYS         |-->| 1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| TRAIN          |-->| VALUE OF TRACER IN THE RAIN
!| UDEL           |-->| X-COMPONENT OF ADVECTION VELOCITY
!| UNSV2D         |-->| INVERSE OF V2DPAR
!| VDEL           |-->| X-COMPONENT OF ADVECTION VELOCITY
!| YAFLBOR        |-->| IF YES FLBOR IS GIVEN
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!| YASMI          |-->| IF YES, SMI MUST BE TAKEN INTO ACCOUNT
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CVTRVF_ERIA => CVTRVF_ERIA
      USE DECLARATIONS_TELEMAC, ONLY : DEJA_CPOS, INDIC_CPOS
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: KDDL,IOPT,NITMAX,OPTADV
      INTEGER, INTENT(IN)             :: NCO_DIST
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(INOUT)          :: LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,TRAIN
      LOGICAL, INTENT(IN)             :: YASMH,YAFLBOR,RAIN
      LOGICAL, INTENT(IN)             :: MSK,ENTET,YASMI
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,H,HN,DM1,ZCONV
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(IN)      :: UDEL,VDEL,FN,SMI,SMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLBORTRA,FBOR,F,SM,HT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: FSCEXP,MASKTR,PLUIE
      TYPE(BIEF_MESH)                 :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IOPT2,NPOIN,IPTFR,I1,I2,I3,NITER,NEWREMAIN,NELMAX
      INTEGER IR,N,NELEM,IELEM,REMAIN,NSEG
      INTEGER OPTPRE
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION C,CPREV,CINIT,HFL1,TET,LOCALMIN,LOCALMAX
      DOUBLE PRECISION F1,F2,F3,COEF,FI1,FI2,FI3,VOL1,VOL2,VOL3
      DOUBLE PRECISION DT1,DT2,DT3,FMIN,FMAX,VOLD1,VOLD2,VOLD3
      DOUBLE PRECISION SURDT,FITOT,BETA1,BETA2,BETA3,A1,A2,A3
      DOUBLE PRECISION FP1,FP2,FP3,FIP1,FIP2,FIP3,FS1,FS2,FS3
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT
      DOUBLE PRECISION, PARAMETER :: TIERS=1.D0/3.D0
      LOGICAL, PARAMETER :: TESTING = .FALSE.
      DOUBLE PRECISION, PARAMETER :: EPS_FLUX = 1.D-15
      DOUBLE PRECISION, POINTER :: FLOP1(:),FLOP2(:),FLOP3(:)
      DOUBLE PRECISION, POINTER :: DTLIM1(:),DTLIM2(:),DTLIM3(:)
      DOUBLE PRECISION, POINTER :: SVOL1(:),SVOL2(:),SVOL3(:)
!
!-----------------------------------------------------------------------
!
!     INDIC_CPOS WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
!
      NELEM=MESH%NELEM
      NELMAX=MESH%NELMAX
      NSEG=MESH%NSEG
      NPOIN=H%DIM1
      SURDT=1.D0/DT
!
      IF(.NOT.DEJA_CPOS) THEN
        ALLOCATE(INDIC_CPOS(NELEM))
        DEJA_CPOS=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     THERE IS PLENTY OF MEMORY AVAILABLE IN A BIEF_MESH STRUCTURE...
!
!     3*NELEM+NPTFR=2*NSEG IN 2D, SO 2*NSEG IS ENOUGH TO STORE 3*NELEM
      FLOP1=>MESH%MSEG%X%R(        1:  NELEM)
      FLOP2=>MESH%MSEG%X%R(  NELEM+1:2*NELEM)
      FLOP3=>MESH%MSEG%X%R(2*NELEM+1:3*NELEM)
      SVOL1=>     MESH%W%R(        1:  NELEM)
      SVOL2=>     MESH%W%R(  NELEM+1:2*NELEM)
      SVOL3=>     MESH%W%R(2*NELEM+1:3*NELEM)
      DTLIM1=>  MESH%M%X%R(        1:  NELEM)
      DTLIM2=>  MESH%M%X%R(  NELEM+1:2*NELEM)
      DTLIM3=>  MESH%M%X%R(2*NELEM+1:3*NELEM)
!     ASSUMING THAT NSEG<3*NELEM
!     WILL BE DESTROYED WHEN DTLIM1 IS BUILT (BEWARE !!!!)
      FXMAT=>MESH%M%X%R(1:NSEG)
!
!-----------------------------------------------------------------------
!
!     VARIANTS (1 OR 2, DIFFERENT IMPLEMENTATION FOR SHARING VOLUMES
!               AT PREDICTOR LEVEL).
!
!     OPTPRE MUST BE EQUAL TO ITS VALUE IN POSITIVE_DEPTHS_ERIA !!!!!!!!
      OPTPRE=1
!
!-----------------------------------------------------------------------
!
!     EXTRACTING OPTIONS, AND CONTROL
!
      IOPT2=IOPT/10
      IF(IOPT2.NE.0.AND.IOPT2.NE.1) THEN
        WRITE(LU,*) 'CVTRVF_ERIA: OPTION IOPT2 UNKNOWN: ',IOPT2
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     STARTING AGAIN FROM NON CORRECTED DEPTH
!
      IF(TESTING) THEN
        C=1.D99
        CINIT=1.D99
        DO I=1,NPOIN
          C    =MIN(C    ,H%R(I))
          CINIT=MIN(CINIT,HN%R(I))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          C=P_MIN(C)
          CINIT=P_MIN(CINIT)
        ENDIF
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, H MIN=',C
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, HN MIN=',CINIT
      ENDIF
!
      CALL CPSTVC(H,T1)
      CALL CPSTVC(H,T2)
      CALL CPSTVC(H,T3)
      CALL CPSTVC(H,T4)
      CALL CPSTVC(H,T5)
      CALL CPSTVC(H,T6)
      CALL CPSTVC(H,T7)
!
!     T2 WILL BE THE ASSEMBLED FLBOR, INITIALISATION HERE
!     IS USELESS EXCEPT THAT PARCOM MAY ADD UNDEFINED
!     NUMBERS (THAT WILL NOT BE USED BUT THAT WILL STOP
!     A COMPILER... TOO BAD!)
      IF(NCSIZE.GT.1) CALL OS('X=0     ',X=T2)
!     OPTIONAL COMPUTATION OF BOUNDARY FLUXES
      IF(.NOT.YAFLBOR) THEN
!       MASK=8 FOR LIQUID BOUNDARIES
        CALL VECTOR(FLBOR,'=','FLUBDF          ',1,1.D0,
     &              HPROP,HPROP,HPROP,
     &              UDEL , VDEL, VDEL,MESH,.TRUE.,MASKTR%ADR(8)%P)
      ENDIF
!     BOUNDARY FLUXES : ADDING THE ENTERING (NEGATIVE) FLUXES
!     FIRST PUTTING FLBOR (BOUNDARY) IN T2 (DOMAIN)
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
!     ASSEMBLING T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
!     POSSIBLE CORRECTION OF LIMTRA AND FBOR (LIMTRA HAS BEEN DONE BY
!     DIFFIN WITH U.N)
      DO I=1,MESH%NPTFR
        N=MESH%NBOR%I(I)
        IF(LIMTRA(I).EQ.KDIR.AND.T2%R(N).GT.0.D0) THEN
          LIMTRA(I)=KDDL
        ELSEIF(LIMTRA(I).EQ.KDDL.AND.T2%R(N).LT.0.D0) THEN
          LIMTRA(I)=KDIR
!         HERE FBOR MAY NOT HAVE BEEN GIVEN, FN TAKEN
          FBOR%R(I)=FN%R(N)
        ELSE
!         INITIALISING FLBORTRA FOR POINTS NOT KDIR AND KDDL
          FLBORTRA%R(I)=0.D0
        ENDIF
      ENDDO
!
!     INITIALIZING F AND INITIAL H
!
      CALL OS('X=Y     ',X=F,Y=FN)
      CALL OS('X=Y     ',X=HT,Y=HN)
!
!     FLUXES
!
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
!     COMPUTING THE FLUXES LEAVING NODES
      CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
     &            HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
!                 T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
!     COPYING EBE FLUXES INTO FLOPOINT
      DO I=1,NELEM
        FLOP1(I)=MESH%W%R(I)
        FLOP2(I)=MESH%W%R(I+NELMAX)
        FLOP3(I)=MESH%W%R(I+2*NELMAX)
      ENDDO
!     ASSEMBLING EBE FLUXES BY SEGMENT (TE1 SUIVI DE FALSE NON UTILISE)
!     HARDCODED OPTION 2 (MUST BE THE SAME AS IN POSITIVE_DEPTHS)
!     FXMAT IS NOT ASSEMBLED IN //
      CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,NELEM,NELMAX,
     &                MESH%ELTSEG%I,MESH%ORISEG%I,
     &                MESH%IKLE%I,.TRUE.,2)
!     ASSEMBLING FXMAT IN PARALLEL
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FXMAT,FXMAT,FXMAT,MESH%NSEG,1,2,1,MESH,1,11)
      ENDIF
!     CHANGING FLUXES FROM POINTS INTO N FLUXES BETWEEN POINTS
      DO IELEM = 1,NELEM
        A1 = ABS(FLOP1(IELEM))
        A2 = ABS(FLOP2(IELEM))
        A3 = ABS(FLOP3(IELEM))
        IF(A1.GE.A2.AND.A1.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 1
          FLOP1(IELEM)=-FLOP2(IELEM)
          FLOP2(IELEM)=0.D0
!         FLOP3(IELEM)= UNCHANGED!
        ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 2
!         FLOP1(IELEM)= UNCHANGED!
          FLOP2(IELEM)=-FLOP3(IELEM)
          FLOP3(IELEM)=0.D0
        ELSE
!         ALL FLOW TO AND FROM NODE 3
          FLOP3(IELEM)=-FLOP1(IELEM)
          FLOP1(IELEM)=0.D0
!         FLOP2(IELEM)= UNCHANGED!
        ENDIF
      ENDDO
!
!     ADDING THE POSITIVE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            IF(SMH%R(I).GT.0.D0) THEN
              HT%R(I)=HT%R(I)+DT*SMH%R(I)
              F%R(I)=F%R(I)+DT/MAX(HT%R(I),1.D-4)*SMH%R(I)*FSCEXP%R(I)
            ENDIF
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            IF(SMH%R(I).GT.0.D0) THEN
              HT%R(I)=HT%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
              F%R(I)=F%R(I)+DT/MAX(HT%R(I),1.D-4)*
     &                        UNSV2D%R(I)*SMH%R(I)*FSCEXP%R(I)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN-EVAPORATION: RAIN FIRST, EVAPORATION IN THE END
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          C=MAX(PLUIE%R(I),0.D0)
!         HT WILL BE AT LEAST C*DT
          HT%R(I)=HT%R(I)+C*DT
          IF(C.NE.0.D0) THEN
!                                           VALUE IN RAIN
            F%R(I)=F%R(I)+((C*DT)/HT%R(I))*(TRAIN-F%R(I))
          ENDIF
        ENDDO
      ENDIF
!
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        HT%R(I)=HT%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0)
!       ENTERING FLUXES OF TRACERS
!       THE FINAL DEPTH IS TAKEN
        IF(LIMTRA(IPTFR).EQ.KDIR) THEN
          F%R(I)=F%R(I)-DT/MAX(HT%R(I),1.D-4)*
     &       UNSV2D%R(I)*T2%R(I)*(FBOR%R(IPTFR)-F%R(I))
!       ELSEIF(LIMTRA(IPTFR).EQ.KDDL) THEN
!         NOTHING TO DO
        ENDIF
      ENDDO
!
!     FOR OPTIMIZING THE LOOP ON SEGMENTS, ONLY SEGMENTS
!     WITH NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST
!     WILL BE UPDATED. TO START WITH, ALL FLUXES ASSUMED NON ZERO
!
      REMAIN=MESH%NELEM
!
      DO I=1,REMAIN
        INDIC_CPOS(I)=I
      ENDDO
!
!     MAXIMUM INITIAL FLUX
!
      CPREV=0.D0
      DO IR=1,NELEM
        CPREV=CPREV+ABS(FLOP1(IR))+ABS(FLOP2(IR))+ABS(FLOP3(IR))
      ENDDO
      IF(NCSIZE.GT.1) CPREV=P_SUM(CPREV)
      IF(TESTING) WRITE(LU,*) 'INITIAL SUM OF FLUXES=',CPREV
      CINIT=CPREV
!
      NITER = 0
777   CONTINUE
      NITER = NITER + 1
!
!     COMPUTING DEMAND (T7) AND OFFER (T1)
!
      CALL OS('X=0     ',X=T7)
      CALL OS('X=0     ',X=T1)
      IF(OPTPRE.EQ.1) THEN
      DO IR=1,REMAIN
        I=INDIC_CPOS(IR)
        I1=MESH%IKLE%I(I        )
        I2=MESH%IKLE%I(I  +NELMAX)
        I3=MESH%IKLE%I(I+2*NELMAX)
!       A PRIORI AVAILABLE VOLUMES
        VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS
        VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS
        VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
!       FLUXES LEAVING POINTS AGAIN
        F1= FLOP1(I)-FLOP3(I)
        F2=-FLOP1(I)+FLOP2(I)
        F3=-FLOP2(I)+FLOP3(I)
!       DEMAND OR OFFER
        IF(F1*DT.GT.VOL1) THEN
!         POINT IN NEED OF EXTRA WATER
          T7%R(I1)=T7%R(I1)+F1*DT-VOL1
        ELSE
!         POINT THAT CAN OFFER EXTRA WATER
!         BUT NOT MORE THAN ITS OWN VOLUME (AS WOULD BE THE CASE IF F1<0...)
          T1%R(I1)=T1%R(I1)+MIN(VOL1,VOL1-F1*DT)
        ENDIF
        IF(F2*DT.GT.VOL2) THEN
          T7%R(I2)=T7%R(I2)+F2*DT-VOL2
        ELSE
          T1%R(I2)=T1%R(I2)+MIN(VOL2,VOL2-F2*DT)
        ENDIF
        IF(F3*DT.GT.VOL3) THEN
          T7%R(I3)=T7%R(I3)+F3*DT-VOL3
        ELSE
          T1%R(I3)=T1%R(I3)+MIN(VOL3,VOL3-F3*DT)
        ENDIF
      ENDDO
      ELSEIF(OPTPRE.EQ.2) THEN
      DO IR=1,REMAIN
        I=INDIC_CPOS(IR)
        I1=MESH%IKLE%I(I        )
        I2=MESH%IKLE%I(I  +NELMAX)
        I3=MESH%IKLE%I(I+2*NELMAX)
!       A PRIORI AVAILABLE VOLUMES
        VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS
        VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS
        VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
!       FLUXES LEAVING POINTS AGAIN
        F1= FLOP1(I)-FLOP3(I)
        F2=-FLOP1(I)+FLOP2(I)
        F3=-FLOP2(I)+FLOP3(I)
!       DEMAND AND OFFER
        IF(F1.GT.0.D0) T7%R(I1)=T7%R(I1)+F1
        IF(F2.GT.0.D0) T7%R(I2)=T7%R(I2)+F2
        IF(F3.GT.0.D0) T7%R(I3)=T7%R(I3)+F3
        T1%R(I1)=T1%R(I1)+VOL1
        T1%R(I2)=T1%R(I2)+VOL2
        T1%R(I3)=T1%R(I3)+VOL3
      ENDDO
      ENDIF
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(T7,2,MESH)
        CALL PARCOM(T1,2,MESH)
      ENDIF
!
!     PREPARING VOLUMES AND LIMITATION FOR PREDICTOR
!
      DO IR=1,REMAIN
!
        I=INDIC_CPOS(IR)
!
        I1=MESH%IKLE%I(I        )
        I2=MESH%IKLE%I(I  +NELMAX)
        I3=MESH%IKLE%I(I+2*NELMAX)
!
!       A PRIORI AVAILABLE VOLUMES FOR THIS ELEMENT
!
        F1= FLOP1(I)-FLOP3(I)
        F2=-FLOP1(I)+FLOP2(I)
        F3=-FLOP2(I)+FLOP3(I)
!
!       PRELIMINARY DISTRIBUTION OF VOLUMES BETWEEN ELEMENTS,
!       ACCORDING TO DEMAND AND OFFER
!
        IF(OPTPRE.EQ.1) THEN
        VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS
        VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS
        VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
        IF(F1*DT.GT.VOL1) THEN
!         VOLUME TOO SMALL, TRYING TO INCREASE IT WITH GIFTS FROM OTHER
!         ELEMENTS
          IF(T7%R(I1).GT.T1%R(I1)) THEN
!           MORE DEMAND THAN OFFER, NEED TO SHARE
!                                  Commas very important to have T1/T7 <1
!                                  before multiplication
!                                  (                 )
            VOL1=VOL1+(F1*DT-VOL1)*(T1%R(I1)/T7%R(I1))
          ELSE
!           MORE OFFER THAN DEMAND, THE POINT CAN HAVE THE REQUESTED VOLUME
            VOL1=F1*DT
          ENDIF
        ELSE
!         VOLUME LARGE ENOUGH, GIVING WATER TO OTHER ELEMENTS ?
          IF(T1%R(I1).GT.T7%R(I1)) THEN
!           MORE OFFER THAN DEMAND, GIVING ENOUGH, BUT NOT ALL
            VOL1=VOL1-MIN(VOL1,VOL1-F1*DT)*(T7%R(I1)/T1%R(I1))
          ELSE
!           MORE DEMAND THAN OFFER, KEEPING ONLY WHAT IS NECESSARY
            VOL1=MAX(F1,0.D0)*DT
          ENDIF
        ENDIF
        IF(F2*DT.GT.VOL2) THEN
          IF(T7%R(I2).GT.T1%R(I2)) THEN
            VOL2=VOL2+(F2*DT-VOL2)*(T1%R(I2)/T7%R(I2))
          ELSE
            VOL2=F2*DT
          ENDIF
        ELSE
          IF(T1%R(I2).GT.T7%R(I2)) THEN
            VOL2=VOL2-MIN(VOL2,VOL2-F2*DT)*(T7%R(I2)/T1%R(I2))
          ELSE
            VOL2=MAX(F2,0.D0)*DT
          ENDIF
        ENDIF
        IF(F3*DT.GT.VOL3) THEN
          IF(T7%R(I3).GT.T1%R(I3)) THEN
            VOL3=VOL3+(F3*DT-VOL3)*(T1%R(I3)/T7%R(I3))
          ELSE
            VOL3=F3*DT
          ENDIF
        ELSE
          IF(T1%R(I3).GT.T7%R(I3)) THEN
            VOL3=VOL3-MIN(VOL3,VOL3-F3*DT)*(T7%R(I3)/T1%R(I3))
          ELSE
            VOL3=MAX(F3,0.D0)*DT
          ENDIF
        ENDIF
        ELSEIF(OPTPRE.EQ.2) THEN
        IF(T7%R(I1).GT.1.D-30) THEN
!                       ( THIS IS IMPORTANT
          VOL1=T1%R(I1)*(MAX(F1,0.D0)/T7%R(I1))
        ELSE
          VOL1=MESH%SURFAC%R(I)*H%R(I1)*TIERS
        ENDIF
        IF(T7%R(I2).GT.1.D-30) THEN
          VOL2=T1%R(I2)*(MAX(F2,0.D0)/T7%R(I2))
        ELSE
          VOL2=MESH%SURFAC%R(I)*H%R(I2)*TIERS
        ENDIF
        IF(T7%R(I3).GT.1.D-30) THEN
          VOL3=T1%R(I3)*(MAX(F3,0.D0)/T7%R(I3))
        ELSE
          VOL3=MESH%SURFAC%R(I)*H%R(I3)*TIERS
        ENDIF
        ENDIF
!
!       SAVING VOLUMES FOR CORRECTOR (ACTUALLY NOT USED BY PREDICTOR, THOUGH
!                                     DONE FOR IT, BUT IT GIVES THE LIMITATIONS...)
!
        SVOL1(I)=VOL1
        SVOL2(I)=VOL2
        SVOL3(I)=VOL3
!
!       LIMITATION OF FLUXES, FIRST IN TERMS OF LIMITED TIME-STEP
!
          IF(F1*DT.GT.VOL1) THEN
            DT1=DT*(VOL1/(F1*DT))
          ELSE
            DT1=DT
          ENDIF
          IF(F2*DT.GT.VOL2) THEN
            DT2=DT*(VOL2/(F2*DT))
          ELSE
            DT2=DT
          ENDIF
          IF(F3*DT.GT.VOL3) THEN
            DT3=DT*(VOL3/(F3*DT))
          ELSE
            DT3=DT
          ENDIF
!
!       NOW LIMITED VOLUMES TRANSITING BETWEEN POINTS (1/DT MISSING)
!       A SEGMENT GETS THE STRONGEST LIMITATION OF ITS TWO POINTS
!       THIS WILL CREATE NO NEGATIVE VOLUME, AS IT COULD BE FEARED,
!       BECAUSE WE HAVE ONLY THE 1-TARGET CASE AND THE 2-TARGET CASE
!       THUS ALL FLUXES LEAVING OR ARRIVING TO A POINT HAVE THE SAME
!       SIGN OR ARE ZERO !
!
        DTLIM1(I)=MIN(DT1,DT2)
        DTLIM2(I)=MIN(DT2,DT3)
        DTLIM3(I)=MIN(DT3,DT1)
!
      ENDDO
!
!
!     NOW SEQUENCE PREDICTOR-CORRECTOR
!
!
!     INITIALISING T3, FUTURE VALUE OF F BECAUSE POINTS THAT DO NOT RECEIVE
!                      WATER WILL NOT BE TREATED
!     T4 IS THE EVOLUTION OF VOLUME OF WATER, HERE INITIALISED TO 0
!     T5 IS THE EVOLUTION OF MASSES OF TRACER, HERE INITIALISED TO 0
!     T6 IS THE INITIAL TRACER*DEPTH
      CALL OS('X=0     ',X=T4)
      CALL OS('X=0     ',X=T5)
      DO I=1,F%DIM1
        T6%R(I)=F%R(I)*HT%R(I)
      ENDDO
      DO I=1,F%DIM1
        T3%R(I)=F%R(I)
      ENDDO
!
!     COMPUTING MIN AND MAX FOR FINAL CLIPPING, THIS WILL ENSURE A GLOBAL
!     MONOTONICITY, THAT COULD OTHERWISE BE VIOLATED BY TRUNCATION ERRORS
!     THIS COULD BE DONE LOCALLY, BUT IT WOULD BE MORE EXPENSIVE
!     FOR LOCAL MIN AND MAX SEE CVTRVF.F
!
      FMIN=F%R(1)
      FMAX=F%R(1)
      DO I=2,F%DIM1
        FMIN=MIN(FMIN,F%R(I))
        FMAX=MAX(FMAX,F%R(I))
      ENDDO
      IF(NCSIZE.GT.1) THEN
        FMIN=P_MIN(FMIN)
        FMAX=P_MAX(FMAX)
      ENDIF
!
!     PREDICTOR
!
      DO IR=1,REMAIN
!
        I=INDIC_CPOS(IR)
!
        I1=MESH%IKLE%I(I        )
        I2=MESH%IKLE%I(I  +NELMAX)
        I3=MESH%IKLE%I(I+2*NELMAX)
!
        FP1=FLOP1(I)*DTLIM1(I)
        FP2=FLOP2(I)*DTLIM2(I)
        FP3=FLOP3(I)*DTLIM3(I)
!
!       CORRESPONDING VARIATIONS OF VOLUMES OF POINTS (DT MISSING SO OK)
!
        T4%R(I1)=T4%R(I1)-( FP1-FP3)
        T4%R(I2)=T4%R(I2)-(-FP1+FP2)
        T4%R(I3)=T4%R(I3)-(-FP2+FP3)
!
!       VARIATIONS OF MASSES ADDED AND REMOVED IN REDUCED FORM
!
        T5%R(I1)=T5%R(I1)-( FP1-FP3)*F%R(I1)
        T5%R(I2)=T5%R(I2)-(-FP1+FP2)*F%R(I2)
        T5%R(I3)=T5%R(I3)-(-FP2+FP3)*F%R(I3)
!
        FI1=-MIN(FP1,0.D0)*(F%R(I2)-F%R(I1))
     &      +MAX(FP3,0.D0)*(F%R(I3)-F%R(I1))
        FI2=+MAX(FP1,0.D0)*(F%R(I1)-F%R(I2))
     &      -MIN(FP2,0.D0)*(F%R(I3)-F%R(I2))
        FI3=+MAX(FP2,0.D0)*(F%R(I2)-F%R(I3))
     &      -MIN(FP3,0.D0)*(F%R(I1)-F%R(I3))
!
!       PSI LIMITATION
!
        FITOT=FI1+FI2+FI3
!
        IF(FITOT.GT.EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MAX(FI1,0.D0)
          BETA2=MAX(FI2,0.D0)
          BETA3=MAX(FI3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FI1=BETA1*COEF
          FI2=BETA2*COEF
          FI3=BETA3*COEF
        ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!         PSI REDUCTION
          BETA1=MIN(FI1,0.D0)
          BETA2=MIN(FI2,0.D0)
          BETA3=MIN(FI3,0.D0)
          COEF=FITOT/(BETA1+BETA2+BETA3)
          FI1=BETA1*COEF
          FI2=BETA2*COEF
          FI3=BETA3*COEF
        ELSE
!         NO REDUCTION
        ENDIF
!
        T5%R(I1)=T5%R(I1)+FI1
        T5%R(I2)=T5%R(I2)+FI2
        T5%R(I3)=T5%R(I3)+FI3
!
      ENDDO
!
!     ADDING THE EVOLUTIONS TO THE DEPTHS AND TRACERS
!     AFTER ASSEMBLY AT INTERFACES AND AFTER
!     CHANGING VOLUMES INTO DEPTHS.
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(T4,2,MESH)
        CALL PARCOM(T5,2,MESH)
      ENDIF
      CALL OS('X=XY    ',X=T4,Y=UNSV2D)
      CALL OS('X=XY    ',X=T5,Y=UNSV2D)
!     UPDATING DEPOTH AND TRACER
      DO I=1,F%DIM1
!       ADDING INITIAL QUANTITY T6 AND VARIATION T5
        T6%R(I)=T6%R(I)+T5%R(I)
!       NOW T5 USED TO STORE OLD DEPTH
        T5%R(I)=HT%R(I)
!       NEW DEPTH
        HT%R(I)=HT%R(I)+T4%R(I)
        IF(HT%R(I).GT.1.D-15) THEN
!         NEW VALUE OF TRACER
          T3%R(I)=T6%R(I)/HT%R(I)
        ENDIF
!       TO COPE WITH TRUNCATION ERRORS AND AVOID NEGATIVE VALUES
!       IF HT NEGATIVE FOR OTHER REASONS IT WILL MAKE MASS ERRORS
        HT%R(I)=MAX(0.D0,HT%R(I))
!       ENSURING A GLOBAL MONOTONICITY OF T3
        T3%R(I)=MIN(FMAX,MAX(FMIN,T3%R(I)))
      ENDDO
!
!     CORRECTOR
!
      IF(NCO_DIST.GT.0) THEN
!
        DO I=1,F%DIM1
          T7%R(I)=0.D0
          T1%R(I)=0.D0
        ENDDO
!
!       EVALUATING OFFER (T1) AND DEMAND (T7)
!
        DO IR=1,REMAIN
          I=INDIC_CPOS(IR)
!
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELMAX)
          I3=MESH%IKLE%I(I+2*NELMAX)
!         LIMITED VOLUMES TRAVELLING BETWEEN POINTS
          FP1=FLOP1(I)*DTLIM1(I)
          FP2=FLOP2(I)*DTLIM2(I)
          FP3=FLOP3(I)*DTLIM3(I)
!         VOLUMES OF BASIC SOLUTION AT T(N+1)
          VOL1=SVOL1(I)-( FP1-FP3)
          VOL2=SVOL2(I)-(-FP1+FP2)
          VOL3=SVOL3(I)-(-FP2+FP3)
          IF(VOL1.LT.MESH%SURFAC%R(I)*HT%R(I1)*TIERS) THEN
            T7%R(I1)=T7%R(I1)+MESH%SURFAC%R(I)*HT%R(I1)*TIERS-VOL1
          ELSE
            T1%R(I1)=T1%R(I1)+VOL1+MIN( FP1,0.D0)+MIN(-FP3,0.D0)
          ENDIF
          IF(VOL2.LT.MESH%SURFAC%R(I)*HT%R(I2)*TIERS) THEN
            T7%R(I2)=T7%R(I2)+MESH%SURFAC%R(I)*HT%R(I2)*TIERS-VOL2
          ELSE
            T1%R(I2)=T1%R(I2)+VOL2+MIN(-FP1,0.D0)+MIN( FP2,0.D0)
          ENDIF
          IF(VOL3.LT.MESH%SURFAC%R(I)*HT%R(I3)*TIERS) THEN
            T7%R(I3)=T7%R(I3)+MESH%SURFAC%R(I)*HT%R(I3)*TIERS-VOL3
          ELSE
            T1%R(I3)=T1%R(I3)+VOL3+MIN(-FP2,0.D0)+MIN( FP3,0.D0)
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
!         GLOBAL OFFER (T1) AND DEMAND (T7)
          CALL PARCOM(T1,2,MESH)
          CALL PARCOM(T7,2,MESH)
        ENDIF
!
!       VOLUMES THAT WILL BE USED FOR THE DERIVATIVE IN TIME
!
        DO IR=1,REMAIN
!
          I=INDIC_CPOS(IR)
!
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELMAX)
          I3=MESH%IKLE%I(I+2*NELMAX)
!
!         LIMITED VOLUMES BETWEEN POINTS
!
          FP1=FLOP1(I)*DTLIM1(I)
          FP2=FLOP2(I)*DTLIM2(I)
          FP3=FLOP3(I)*DTLIM3(I)
!         LOCAL VOLUMES AFTER PREDICTOR
          VOL1=SVOL1(I)-( FP1-FP3)
          VOL2=SVOL2(I)-(-FP1+FP2)
          VOL3=SVOL3(I)-(-FP2+FP3)
!         VOLUME1
          IF(VOL1.LT.MESH%SURFAC%R(I)*HT%R(I1)*TIERS) THEN
            IF(T7%R(I1).GT.T1%R(I1)) THEN
!             MORE DEMAND THAN OFFER, VOL1 GETS ONLY A SHARE
              VOL1=VOL1+(MESH%SURFAC%R(I)*HT%R(I1)*TIERS-VOL1)
     &                 *(T1%R(I1)/T7%R(I1))
            ELSE
!             VOL1 GETS ALL
              VOL1=MESH%SURFAC%R(I)*HT%R(I1)*TIERS
            ENDIF
          ELSE
            IF(T1%R(I1).GT.T7%R(I1)) THEN
!             MORE OFFER THAN DEMAND, VOL1 GIVES A SHARE ONLY
              VOL1=VOL1-(VOL1+(MIN( FP1,0.D0)+MIN(-FP3,0.D0)))
     &                 *(T7%R(I1)/T1%R(I1))
            ELSE
!             MORE DEMAND THAN OFFER, VOL1 GIVES ALL IT CAN
              VOL1=-(MIN( FP1,0.D0)+MIN(-FP3,0.D0))
            ENDIF
          ENDIF
!         VOLUME 2
          IF(VOL2.LT.MESH%SURFAC%R(I)*HT%R(I2)*TIERS) THEN
            IF(T7%R(I2).GT.T1%R(I2)) THEN
!             MORE DEMAND THAN OFFER, VOL2 GETS ONLY A SHARE
              VOL2=VOL2+(MESH%SURFAC%R(I)*HT%R(I2)*TIERS-VOL2)
     &                 *(T1%R(I2)/T7%R(I2))
            ELSE
!             VOL2 GETS ALL
              VOL2=MESH%SURFAC%R(I)*HT%R(I2)*TIERS
            ENDIF
          ELSE
            IF(T1%R(I2).GT.T7%R(I2)) THEN
!             MORE OFFER THAN DEMAND, VOL2 GIVES A SHARE ONLY
              VOL2=VOL2-(VOL2+(MIN(-FP1,0.D0)+MIN( FP2,0.D0)))
     &                 *(T7%R(I2)/T1%R(I2))
            ELSE
!             MORE DEMAND THAN OFFER, VOL1 GIVES ALL IT CAN
              VOL2=-(MIN(-FP1,0.D0)+MIN( FP2,0.D0))
            ENDIF
          ENDIF
!         VOLUME 3
          IF(VOL3.LT.MESH%SURFAC%R(I)*HT%R(I3)*TIERS) THEN
            IF(T7%R(I3).GT.T1%R(I3)) THEN
!             MORE DEMAND THAN OFFER, VOL1 GETS ONLY A SHARE
              VOL3=VOL3+(MESH%SURFAC%R(I)*HT%R(I3)*TIERS-VOL3)
     &                 *(T1%R(I3)/T7%R(I3))
            ELSE
!             VOL3 GETS ALL
              VOL3=MESH%SURFAC%R(I)*HT%R(I3)*TIERS
            ENDIF
          ELSE
            IF(T1%R(I3).GT.T7%R(I3)) THEN
!             MORE OFFER THAN DEMAND, VOL3 GIVES A SHARE ONLY
              VOL3=VOL3-(VOL3+(MIN(-FP2,0.D0)+MIN( FP3,0.D0)))
     &                 *(T7%R(I3)/T1%R(I3))
            ELSE
!             MORE DEMAND THAN OFFER, VOL3 GIVES ALL IT CAN
              VOL3=-(MIN(-FP2,0.D0)+MIN( FP3,0.D0))
            ENDIF
          ENDIF
          SVOL1(I)=VOL1
          SVOL2(I)=VOL2
          SVOL3(I)=VOL3
        ENDDO
!
        DO N=1,NCO_DIST
!
          DO I=1,F%DIM1
!           INITIAL QUANTITY
            T6%R(I)=F%R(I)*HT%R(I)
          ENDDO
!
          IF(OPTADV.EQ.2) THEN
!           FOR SECOND ORDER, COMPUTING POSSIBLE MIN AND MAX
!           MIN IN T1 AND MAX IN T7
            DO I=1,F%DIM1
              T1%R(I)=F%R(I)
              T7%R(I)=F%R(I)
            ENDDO
            DO IR=1,REMAIN
              I=INDIC_CPOS(IR)
              I1=MESH%IKLE%I(I        )
              I2=MESH%IKLE%I(I  +NELMAX)
              I3=MESH%IKLE%I(I+2*NELMAX)
              LOCALMIN=MIN( F%R(I1), F%R(I2), F%R(I3),
     &                     T3%R(I1),T3%R(I2),T3%R(I3))
              LOCALMAX=MAX( F%R(I1), F%R(I2), F%R(I3),
     &                     T3%R(I1),T3%R(I2),T3%R(I3))
              T1%R(I1)=MIN(T1%R(I1),LOCALMIN)
              T1%R(I2)=MIN(T1%R(I2),LOCALMIN)
              T1%R(I3)=MIN(T1%R(I3),LOCALMIN)
              T7%R(I1)=MAX(T7%R(I1),LOCALMAX)
              T7%R(I2)=MAX(T7%R(I2),LOCALMAX)
              T7%R(I3)=MAX(T7%R(I3),LOCALMAX)
            ENDDO
            IF(NCSIZE.GT.1) THEN
!                            4:MIN
              CALL PARCOM(T1,4,MESH)
!                            3:MAX
              CALL PARCOM(T7,3,MESH)
            ENDIF
          ENDIF
!
          CALL OS('X=0     ',X=T5)
!
!         NOW THE REAL CORRECTOR
!
          IF(OPTADV.EQ.1) THEN
!
!           FIRST ORDER IN TIME
!
            DO IR=1,REMAIN
!
              I=INDIC_CPOS(IR)
              I1=MESH%IKLE%I(I        )
              I2=MESH%IKLE%I(I  +NELMAX)
              I3=MESH%IKLE%I(I+2*NELMAX)
!
!             LIMITED VOLUMES BETWEEN POINTS
!
              FP1=FLOP1(I)*DTLIM1(I)
              FP2=FLOP2(I)*DTLIM2(I)
              FP3=FLOP3(I)*DTLIM3(I)
!
              FI1=(T3%R(I1)-F%R(I1))*SVOL1(I)
              FI2=(T3%R(I2)-F%R(I2))*SVOL2(I)
              FI3=(T3%R(I3)-F%R(I3))*SVOL3(I)
!
!             ADDING THE DERIVATIVE THAT WILL BE REMOVED IN UPWIND FORM
!             THIS IS DONE ONLY FOR REMAINING ELEMENTS.
!
              T5%R(I1)=T5%R(I1)+FI1
              T5%R(I2)=T5%R(I2)+FI2
              T5%R(I3)=T5%R(I3)+FI3
!
!             THEN FLUXES
!
              FIP1=-MIN(FP1,0.D0)*(F%R(I2)-F%R(I1))
     &             +MAX(FP3,0.D0)*(F%R(I3)-F%R(I1))
              FIP2=+MAX(FP1,0.D0)*(F%R(I1)-F%R(I2))
     &             -MIN(FP2,0.D0)*(F%R(I3)-F%R(I2))
              FIP3=+MAX(FP2,0.D0)*(F%R(I2)-F%R(I3))
     &             -MIN(FP3,0.D0)*(F%R(I1)-F%R(I3))
!
!             THIS EXTRA PSI REDUCTION IS NOT NECESSARY
!             BUT GIVES SLIGHTLY BETTER RESULTS
              FITOT=FIP1+FIP2+FIP3
              IF(FITOT.GT.EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MAX(FIP1,0.D0)
                BETA2=MAX(FIP2,0.D0)
                BETA3=MAX(FIP3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FIP1=BETA1*COEF
                FIP2=BETA2*COEF
                FIP3=BETA3*COEF
              ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MIN(FIP1,0.D0)
                BETA2=MIN(FIP2,0.D0)
                BETA3=MIN(FIP3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FIP1=BETA1*COEF
                FIP2=BETA2*COEF
                FIP3=BETA3*COEF
              ELSE
!               NO REDUCTION
              ENDIF
!
!             ADDING TO FINAL CONTRIBUTIONS THAT WILL BE REDUCED AGAIN
!
              FI1=-FI1+FIP1
              FI2=-FI2+FIP2
              FI3=-FI3+FIP3
!
!             PSI LIMITATION
!
              FITOT=FI1+FI2+FI3
!
              IF(FITOT.GT.EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MAX(FI1,0.D0)
                BETA2=MAX(FI2,0.D0)
                BETA3=MAX(FI3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FI1=BETA1*COEF
                FI2=BETA2*COEF
                FI3=BETA3*COEF
              ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MIN(FI1,0.D0)
                BETA2=MIN(FI2,0.D0)
                BETA3=MIN(FI3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FI1=BETA1*COEF
                FI2=BETA2*COEF
                FI3=BETA3*COEF
              ELSE
!               NO REDUCTION
              ENDIF
!
              T5%R(I1)=T5%R(I1)+FI1
              T5%R(I2)=T5%R(I2)+FI2
              T5%R(I3)=T5%R(I3)+FI3
!
            ENDDO
!
          ELSEIF(OPTADV.EQ.2) THEN
!
!           SECOND ORDER IN TIME
!
            DO IR=1,REMAIN
!
              I=INDIC_CPOS(IR)
              I1=MESH%IKLE%I(I        )
              I2=MESH%IKLE%I(I  +NELMAX)
              I3=MESH%IKLE%I(I+2*NELMAX)
!
!             LIMITED VOLUMES BETWEEN POINTS
!
              FP1=FLOP1(I)*DTLIM1(I)
              FP2=FLOP2(I)*DTLIM2(I)
              FP3=FLOP3(I)*DTLIM3(I)
!
              FI1=(F%R(I1)-T3%R(I1))*SVOL1(I)
              FI2=(F%R(I2)-T3%R(I2))*SVOL2(I)
              FI3=(F%R(I3)-T3%R(I3))*SVOL3(I)
!
!             ADDING THE DERIVATIVE THAT WILL BE REMOVED IN UPWIND FORM
!
              T5%R(I1)=T5%R(I1)-FI1
              T5%R(I2)=T5%R(I2)-FI2
              T5%R(I3)=T5%R(I3)-FI3
!
!             THEN FLUXES
!
              VOLD1=MAX(0.D0,SVOL1(I)+MIN(FP1,0.D0)-MAX(FP3,0.D0))
              VOLD2=MAX(0.D0,SVOL2(I)-MAX(FP1,0.D0)+MIN(FP2,0.D0))
              VOLD3=MAX(0.D0,SVOL3(I)-MAX(FP2,0.D0)+MIN(FP3,0.D0))
              A1=2.D0*VOLD1/MAX(1.D-30, MAX(FP1,0.D0)-MIN(FP3,0.D0))
              A2=2.D0*VOLD2/MAX(1.D-30,-MIN(FP1,0.D0)+MAX(FP2,0.D0))
              A3=2.D0*VOLD3/MAX(1.D-30,-MIN(FP2,0.D0)+MAX(FP3,0.D0))
              FS1=MIN(T3%R(I1),F%R(I1)+A1*(F%R(I1)-T1%R(I1)))
              FS1=MAX(FS1     ,F%R(I1)+A1*(F%R(I1)-T7%R(I1)))
              FS2=MIN(T3%R(I2),F%R(I2)+A2*(F%R(I2)-T1%R(I2)))
              FS2=MAX(FS2     ,F%R(I2)+A2*(F%R(I2)-T7%R(I2)))
              FS3=MIN(T3%R(I3),F%R(I3)+A3*(F%R(I3)-T1%R(I3)))
              FS3=MAX(FS3     ,F%R(I3)+A3*(F%R(I3)-T7%R(I3)))
!
              FIP1=0.5D0*(-MIN(FP1,0.D0)*(FS2-F%R(I1)+F%R(I2)-F%R(I1))
     &                    +MAX(FP3,0.D0)*(FS3-F%R(I1)+F%R(I3)-F%R(I1))
     &                    -MAX(FP1,0.D0)*(FS1-F%R(I1))
     &                    +MIN(FP3,0.D0)*(FS1-F%R(I1)))
              FIP2=0.5D0*(+MAX(FP1,0.D0)*(FS1-F%R(I2)+F%R(I1)-F%R(I2))
     &                    -MIN(FP2,0.D0)*(FS3-F%R(I2)+F%R(I3)-F%R(I2))
     &                    +MIN(FP1,0.D0)*(FS2-F%R(I2))
     &                    -MAX(FP2,0.D0)*(FS2-F%R(I2)))
              FIP3=0.5D0*(+MAX(FP2,0.D0)*(FS2-F%R(I3)+F%R(I2)-F%R(I3))
     &                    -MIN(FP3,0.D0)*(FS1-F%R(I3)+F%R(I1)-F%R(I3))
     &                    +MIN(FP2,0.D0)*(FS3-F%R(I3))
     &                    -MAX(FP3,0.D0)*(FS3-F%R(I3)))
!
!             GO TO 2000
!             THIS EXTRA PSI REDUCTION IS NOT NECESSARY
!             BUT GIVES SLIGHTLY BETTER RESULTS
              FITOT=FIP1+FIP2+FIP3
              IF(FITOT.GT.EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MAX(FIP1,0.D0)
                BETA2=MAX(FIP2,0.D0)
                BETA3=MAX(FIP3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FIP1=BETA1*COEF
                FIP2=BETA2*COEF
                FIP3=BETA3*COEF
              ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MIN(FIP1,0.D0)
                BETA2=MIN(FIP2,0.D0)
                BETA3=MIN(FIP3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FIP1=BETA1*COEF
                FIP2=BETA2*COEF
                FIP3=BETA3*COEF
              ELSE
!               NO REDUCTION
              ENDIF
!2000          CONTINUE
!
!             ADDING TO FINAL CONTRIBUTIONS THAT WILL BE REDUCED AGAIN
!
              FI1=FI1+FIP1
              FI2=FI2+FIP2
              FI3=FI3+FIP3
!
!             PSI LIMITATION
!
              FITOT=FI1+FI2+FI3
!
              IF(FITOT.GT.EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MAX(FI1,0.D0)
                BETA2=MAX(FI2,0.D0)
                BETA3=MAX(FI3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FI1=BETA1*COEF
                FI2=BETA2*COEF
                FI3=BETA3*COEF
              ELSEIF(FITOT.LT.-EPS_FLUX) THEN
!               PSI REDUCTION
                BETA1=MIN(FI1,0.D0)
                BETA2=MIN(FI2,0.D0)
                BETA3=MIN(FI3,0.D0)
                COEF=FITOT/(BETA1+BETA2+BETA3)
                FI1=BETA1*COEF
                FI2=BETA2*COEF
                FI3=BETA3*COEF
              ELSE
!               NO REDUCTION
              ENDIF
!
              T5%R(I1)=T5%R(I1)+FI1
              T5%R(I2)=T5%R(I2)+FI2
              T5%R(I3)=T5%R(I3)+FI3
!
            ENDDO
!
          ELSE
            WRITE(LU,*) 'UNKNOWN OPTION IN CVTRVF_ERIA: ',OPTADV
            CALL PLANTE(1)
            STOP
          ENDIF
!
!         ADDING THE EVOLUTIONS TO THE TRACERS
!         AFTER ASSEMBLY AT INTERFACES AND AFTER
!         CHANGING VOLUMES INTO DEPTHS.
!
          IF(NCSIZE.GT.1) CALL PARCOM(T5,2,MESH)
          CALL OS('X=XY    ',X=T5,Y=UNSV2D)
!         NEW AVERAGE VALUE OF TRACER
          DO I=1,F%DIM1
            IF(HT%R(I).GT.1.D-15) THEN
              T3%R(I)=(T6%R(I)+T5%R(I))/HT%R(I)
!             ENSURING A GLOBAL MONOTONICITY
              T3%R(I)=MIN(FMAX,MAX(FMIN,T3%R(I)))
            ENDIF
          ENDDO
!
!         END OF THE LOOP OF CORRECTIONS
!
        ENDDO
      ENDIF
!
!     SETTING THE FINAL VALUE OF THE TRACER FOR THIS SUB-ITERATION
      DO I=1,F%DIM1
        F%R(I)=T3%R(I)
      ENDDO
!
!     IF REMAINING FLUXES, THE ELEMENT IS KEPT IN THE LIST
      NEWREMAIN=0
      C=0.D0
!
      DO IR=1,REMAIN
        I=INDIC_CPOS(IR)
        IF(DTLIM1(I).EQ.DT.AND.DTLIM2(I).EQ.DT.AND.
     &     DTLIM3(I).EQ.DT) THEN
          FLOP1(I)=0.D0
          FLOP2(I)=0.D0
          FLOP3(I)=0.D0
        ELSE
          NEWREMAIN=NEWREMAIN+1
!         BEFORE NEWREMAIN: FOR NEXT ITERATION
!         AFTER  NEWREMAIN: STILL VALID FOR NEXT ITERATION
          INDIC_CPOS(NEWREMAIN)=I
          FLOP1(I)=FLOP1(I)*(1.D0-DTLIM1(I)*SURDT)
          FLOP2(I)=FLOP2(I)*(1.D0-DTLIM2(I)*SURDT)
          FLOP3(I)=FLOP3(I)*(1.D0-DTLIM3(I)*SURDT)
          C=C+ABS(FLOP1(I))+ABS(FLOP2(I))+ABS(FLOP3(I))
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) C=P_SUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
!
      REMAIN=NEWREMAIN
!
      IF(C.NE.CPREV.AND.ABS(C-CPREV).GT.CINIT*1.D-9
     &             .AND.C.NE.0.D0) THEN
        CPREV=C
        IF(NITER.LT.NITMAX) GO TO 777
      ENDIF
!
!     ADDING THE NEGATIVE SOURCES
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            IF(SMH%R(I).LT.0.D0) THEN
              HT%R(I)=HT%R(I)+DT*SMH%R(I)
              F%R(I)=F%R(I)+DT/MAX(HT%R(I),1.D-4)*SMH%R(I)*FSCEXP%R(I)
            ENDIF
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            IF(SMH%R(I).LT.0.D0) THEN
              HT%R(I)=HT%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
              F%R(I)=F%R(I)+DT/MAX(HT%R(I),1.D-4)*
     &                        UNSV2D%R(I)*SMH%R(I)*FSCEXP%R(I)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN-EVAPORATION: RAIN DONE ABOVE, NOW EVAPORATION
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          C=MIN(PLUIE%R(I),0.D0)
!         POSITIVITY NOT TESTED HERE, WOULD REQUIRE C=MAX(C,-HT%R(I)/DT)
!         BUT THEN MASS-BALANCE WOULD NOT BE CORRECT,
          HT%R(I)=HT%R(I)+C*DT
!         MONOTONICITY NON OBEYED HERE OF COURSE
!                        VALUE IN VAPOR
!         F%R(I)=F%R(I)+(0.D0-F%R(I))*((C*DT)/MAX(HT%R(I),1.D-4))
          F%R(I)=F%R(I)*(1.D0-((C*DT)/MAX(HT%R(I),1.D-4)))
        ENDDO
      ENDIF
!
!     BOUNDARY FLUXES : ADDING THE EXITING (POSITIVE) FLUXES
!                       WITH A POSSIBLE LIMITATION
!
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
!                               T2 = // ASSEMBLED FLBOR
        HFL1=DT*UNSV2D%R(I)*MAX(T2%R(I),0.D0)
        TET=1.D0
        IF(HFL1.GT.HT%R(I)) TET=HT%R(I)/HFL1
!       MAX IS ONLY TO PREVENT TRUNCATION ERROR
        HT%R(I)=MAX(HT%R(I)-HFL1*TET,0.D0)
!       LIMITATION OF FLBOR (MUST HAVE BEEN DONE ALREADY
!                            IN POSITIVE_DEPTHS)
!       FLBOR%R(IPTFR)=FLBOR%R(IPTFR)*TET
        IF(LIMTRA(IPTFR).EQ.KDIR) THEN
          F%R(I)=F%R(I)-HFL1*TET/MAX(HT%R(I),1.D-4)*
     &           (FBOR%R(IPTFR)-F%R(I))
          FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*FBOR%R(IPTFR)
        ELSEIF(LIMTRA(IPTFR).EQ.KDDL) THEN
          FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*F%R(I)
        ENDIF
      ENDDO
!
      IF(TESTING) THEN
        C=0.D0
        DO I=1,NPOIN
          C=C+(HT%R(I)-H%R(I))**2
        ENDDO
!                       FAUX MAIS PAS GRAVE SI 0.
        IF(NCSIZE.GT.1) C=P_SUM(C)
        WRITE(LU,*) 'DIFFERENCE ENTRE H ET HT =',C
!
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,F%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_MIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR MIN=',C
        C=-1.D99
        DO I=1,NPOIN
          C=MAX(C,F%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_MAX(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR MAX=',C
      ENDIF
!
!-----------------------------------------------------------------------
!
!     SOURCE TERMS
!
      IF(YASMI) THEN
!
!       IMPLICIT AND EXPLICIT SOURCE TERM
!
        IF(IOPT2.EQ.0) THEN
          DO I = 1,MESH%NPOIN
            F%R(I)=(F%R(I)+DT*SM%R(I))/
     &           (1.D0-DT*SMI%R(I)/MAX(H%R(I),1.D-15))
!           COULD BE DONE LIKE THIS...
!           F%R(I)=H%R(I)*(F%R(I)+DT*SM%R(I))/(H%R(I)-DT*SMI%R(I))
          ENDDO
        ELSEIF(IOPT2.EQ.1) THEN
!         HERE WE ASSUME THAT SMI WILL PREVENT A DIVISION BY ZERO
!         THIS IS THE CASE WITH SETTLING VELOCITY IN SISYPHE
          DO I = 1,MESH%NPOIN
          F%R(I)=(F%R(I)*HT%R(I)+DT*SM%R(I)*H%R(I))/
     &           (H%R(I)-DT*SMI%R(I))
          ENDDO
        ENDIF
!
      ELSE
!
!       EXPLICIT SOURCE TERM ONLY (AND IOPT2=1 NOT TREATED !!!)
!
        DO I = 1,MESH%NPOIN
          F%R(I) = F%R(I)+DT*SM%R(I)
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(ENTET) THEN
        WRITE(LU,102) NITER
      ENDIF
!
      IF(NITER.EQ.NITMAX) THEN
        WRITE(LU,103) NITER
      ENDIF
!
102   FORMAT(' CVTRVF_ERIA (SCHEME ERIA, 15): ',1I3,' ITERATIONS')
103   FORMAT(' CVTRVF_ERIA (SCHEME ERIA, 15): ',1I3,' ITERATIONS',
     &  ' = MAXIMUM',
     &  /,'INCREASE MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
