!                   *******************************
                    SUBROUTINE POSITIVE_DEPTHS_ERIA
!                   *******************************
!
     &(T1,T2,T3,T4,H,HN,MESH,FLODEL,COMPUTE_FLODEL,FLBOR,DT,
     & UNSV2D,NPOIN,GLOSEG1,GLOSEG2,NBOR,NPTFR,
     & SMH,YASMH,PLUIE,RAIN,OPTSOU,LIMPRO,HBOR,KDIR,INFO,
     & FLOPOINT,NAMECODE,NITMAX,MAKEFLULIMEBE,FLULIMEBE)
!
!***********************************************************************
! BIEF   V8P3
!***********************************************************************
!
!brief    Suppresses negative depths by a limitation of fluxes, with the
!+        ERIA technique.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/11/2016
!+        V7P3
!+   First version, taken out of previous positive_depths
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/11/2016
!+        V7P3
!+   Adding an option (hardcoded so far) for sharing volumes of points
!+   between elements (see variable OPTPRE, OPTPRE=1 old strategy,
!+   OPTPRE=2, new and simpler strategy). Tests do not show much
!+   difference, kept here to be tested in other cases.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COMPUTE_FLODEL |-->| IF YES, COMPUTE FLODEL HERE
!| MAKEFLULIMEBE  |-->| OPTIONAL, IF YES DOES ARRAY FLULIMEBE
!| DT             |-->| TIME STEP
!| FLBOR          |<->| BOUNDARY FLUXES
!| FLODEL         |<->| FLUXES GIVEN BY SEGMENT
!|                |   | MAY BE COMPUTED HERE (SEE COMPUTE-FLODEL)
!|                |   | OR SIMPLY GIVEN. AT THE EXIT, THE REAL FLUX
!|                |   | TRANSMITTED IS GIVEN BACK.
!| FLOPOINT       |-->| FLUXES GIVEN BY POINTS (ELEMENT BY ELEMENT)
!| FLULIMEBE      |<--| AS FLULIM BUT GIVEN PER ELEMENT
!|                |   | HENCE FLULIMEBE(NELMAX,3)
!| GLOSEG1        |-->| FIRST POINT OF SEGMENTS
!| GLOSEG2        |-->| SECOND POINT OF SEGMENTS
!| H              |<->| NEW DEPTH
!| HBOR           |-->| PRESCRIBED DEPTHS AT BOUNDARIES
!| HN             |-->| OLD DEPTH
!| INFO           |-->| IF YES, PRINTING INFORMATION ON LISTING
!| KDIR           |-->| CONVENTION FOR DIRICHLET BOUNDARY CONDITION
!| LIMPRO         |-->| TYPE OF BOUNDARY CONDITIONS
!|                |   | IF EQUAL TO KDIR: PRESCRIBED DEPTH.
!| MESH           |<->| MESH STRUCTURE
!| NAMECODE       |-->| NAME OF CALLING CODE (SISYPHE, TELEMEC2D, ETC.)
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NITMAX         |-->| MAXIMUM NUMBER OF ITERATIONS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| OPTSOU         |-->| OPTION FOR SOURCES 1: NORMAL 2: DIRAC
!| PLUIE          |-->| RAIN IN A BIEF_OBJ, IN M/S.
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| SMH            |-->| SOURCE TERMS
!| T1             |-->| WORK ARRAY
!| T2             |-->| WORK ARRAY
!| T3             |-->| WORK ARRAY
!| T4             |-->| WORK ARRAY
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!| YASMH          |-->| IF(YES) SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_POSITIVE_DEPTHS_ERIA => POSITIVE_DEPTHS_ERIA
      USE DECLARATIONS_TELEMAC, ONLY : DEJA_PDEPT_ERIA,INDIC_PDEPT_ERIA
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,OPTSOU,KDIR
      INTEGER, INTENT(IN)             :: NITMAX
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,HBOR(NPTFR)
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: FLOPOINT(MESH%NELMAX,3)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,FLODEL,H,FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PLUIE
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D,HN,SMH
      LOGICAL, INTENT(IN)             :: YASMH,INFO,RAIN
      LOGICAL, INTENT(IN)             :: COMPUTE_FLODEL
      CHARACTER(LEN=24)               :: NAMECODE
      LOGICAL, INTENT(IN)             :: MAKEFLULIMEBE
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIMEBE(MESH%NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,I2,I3,IPTFR,REMAIN,NEWREMAIN,IR,NITER
      INTEGER NELEM,NELMAX,NSEG,IELEM,OPTPRE
      DOUBLE PRECISION C,CPREV,CINIT,VOL1,VOL2,VOL3,HFL1,F1,F2,F3
      DOUBLE PRECISION DTLIM1,DTLIM2,DTLIM3,FP1,FP2,FP3,DT1,DT2,DT3
      DOUBLE PRECISION SURDT,A1,A2,A3
      DOUBLE PRECISION, PARAMETER :: TIERS=1.D0/3.D0
!
      DOUBLE PRECISION, PARAMETER :: EPS_FLUX = 1.D-15
      LOGICAL, PARAMETER :: TESTING = .FALSE.
!
!-----------------------------------------------------------------------
!
!     VARIANTS
!
!     OPTPRE MUST BE EQUAL TO ITS VALUE IN CVTRVF_ERIA !!!!!!!!!!
      OPTPRE=1
!
!-----------------------------------------------------------------------
!
!     INDIC_PDEPT_ERIA WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
!     HSEG IS THE DEPTH SHARED BETWEEN SEGMENTS
!
!-----------------------------------------------------------------------
!
      SURDT=1.D0/DT
      NELEM=MESH%NELEM
      NELMAX=MESH%NELMAX
      NSEG=MESH%NSEG
!
      IF(.NOT.DEJA_PDEPT_ERIA) THEN
        ALLOCATE(INDIC_PDEPT_ERIA(NELEM))
        DEJA_PDEPT_ERIA=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
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
        C=0.D0
        CINIT=0.D0
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN
            C    =C    +H%R(I) *MESH%IFAC%I(I)/UNSV2D%R(I)
            CINIT=CINIT+HN%R(I)*MESH%IFAC%I(I)/UNSV2D%R(I)
          ENDDO
          C    =P_SUM(C    )
          CINIT=P_SUM(CINIT)
        ELSE
          DO I=1,NPOIN
            C    =C    +H%R(I) /UNSV2D%R(I)
            CINIT=CINIT+HN%R(I)/UNSV2D%R(I)
          ENDDO
        ENDIF
        WRITE(LU,*) 'AVANT TRAITEMENT MASSE INITIALE=',CINIT
        WRITE(LU,*) 'AVANT TRAITEMENT MASSE FINALE  =',C
      ENDIF
!
!     CALCUL DES FLUX PAR SEGMENT (T1 SUIVI DE FALSE NON UTILISE)
!     WHEN RECEIVED FLODEL IS NOT ASSEMBLED IN //
!
      IF(COMPUTE_FLODEL) THEN
!       SO FAR HARDCODED OPTION 2
        CALL FLUX_EF_VF(FLODEL%R,FLOPOINT,NSEG,NELEM,NELMAX,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,2)
      ENDIF
!
!     ASSEMBLING FLODEL IN PARALLEL
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FLODEL%R,FLODEL%R,FLODEL%R,
     &                   NSEG,1,2,1,MESH,1,11)
      ENDIF
!
!     CHANGING FLUXES FROM POINTS INTO N FLUXES BETWEEN POINTS
      DO IELEM = 1,NELEM
        A1 = ABS(FLOPOINT(IELEM,1))
        A2 = ABS(FLOPOINT(IELEM,2))
        A3 = ABS(FLOPOINT(IELEM,3))
        IF(A1.GE.A2.AND.A1.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 1
          FLOPOINT(IELEM,1)=-FLOPOINT(IELEM,2)
          FLOPOINT(IELEM,2)=0.D0
!         FLOPOINT(IELEM,3)= UNCHANGED!
        ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 2
!         FLOPOINT(IELEM,1)= UNCHANGED!
          FLOPOINT(IELEM,2)=-FLOPOINT(IELEM,3)
          FLOPOINT(IELEM,3)=0.D0
        ELSE
!         ALL FLOW TO AND FROM NODE 3
          FLOPOINT(IELEM,3)=-FLOPOINT(IELEM,1)
          FLOPOINT(IELEM,1)=0.D0
!         FLOPOINT(IELEM,2)= UNCHANGED!
        ENDIF
      ENDDO
!
!     SAVING THE ORIGINAL FLOPOINT
!
      IF(MAKEFLULIMEBE) THEN
        DO IELEM=1,NELEM
          FLULIMEBE(IELEM,1)=FLOPOINT(IELEM,1)
          FLULIMEBE(IELEM,2)=FLOPOINT(IELEM,2)
          FLULIMEBE(IELEM,3)=FLOPOINT(IELEM,3)
        ENDDO
      ENDIF
!
      CALL CPSTVC(H,T2)
      CALL CPSTVC(H,T4)
      CALL CPSTVC(H,T1)
!
!     ADDING THE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
!     FIRST THE POSITIVE SOURCES
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*MAX(SMH%R(I),0.D0)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*MAX(SMH%R(I),0.D0)*UNSV2D%R(I)
          ENDDO
        ENDIF
      ELSE
        DO I=1,NPOIN
          H%R(I)=HN%R(I)
        ENDDO
      ENDIF
!
!     RAIN (POSITIVE PLUIE)
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          H%R(I)=H%R(I)+DT*MAX(PLUIE%R(I),0.D0)
        ENDDO
      ENDIF
!
!     BOUNDARY FLUXES : ADDING THE ENTERING (NEGATIVE) FLUXES
!     FIRST PUTTING FLBOR (BOUNDARY) IN T2 (DOMAIN)
!
!     T2 WILL BE THE ASSEMBLED FLBOR, INITIALISATION HERE
!     IS USELESS EXCEPT THAT PARCOM MAY ADD UNDEFINED
!     NUMBERS (THAT WILL NOT BE USED BUT THAT WILL STOP
!     A COMPILER... TOO BAD!)
      IF(NCSIZE.GT.1) CALL OS('X=0     ',X=T2)
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
!     ASSEMBLING T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        H%R(I)=H%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0)
      ENDDO
!
!     FOR OPTIMIZING THE LOOP ON ELEMENTS, ONLY ELEMENTS
!     WITH NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST
!     WILL BE UPDATED. TO START WITH, ALL ELEMENTS IN THE LIST
!
      REMAIN=NELEM
!
      DO I=1,REMAIN
        INDIC_PDEPT_ERIA(I)=I
      ENDDO
!
      CPREV=0.D0
!     MAXIMUM INITIAL FLUX
      DO IR=1,NELEM
        CPREV=CPREV+ABS(FLOPOINT(IR,1))
     &             +ABS(FLOPOINT(IR,2))
     &             +ABS(FLOPOINT(IR,3))
      ENDDO
      IF(NCSIZE.GT.1) CPREV=P_SUM(CPREV)
      IF(TESTING) WRITE(LU,*) 'INITIAL SUM OF FLUXES=',CPREV
      CINIT=CPREV
!
!     LOOP OVER THE LOOP OVER THE ELEMENTS
!
      NITER = 0
777   CONTINUE
      NITER = NITER + 1
!
!       T4 IS THE EVOLUTION OF VOLUME, HERE INITIALISED TO 0
        CALL OS('X=0     ',X=T4)
!
!       LOOP OVER THE ELEMENTS
!
        NEWREMAIN=0
        C=0.D0
!
!       COMPUTING DEMAND (T1) AND OFFER (T3)
!
        CALL OS('X=0     ',X=T1)
        CALL OS('X=0     ',X=T3)
        IF(OPTPRE.EQ.1) THEN
        DO IR=1,REMAIN
          I=INDIC_PDEPT_ERIA(IR)
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELMAX)
          I3=MESH%IKLE%I(I+2*NELMAX)
!         A PRIORI AVAILABLE VOLUMES
          VOL1=MESH%SURFAC%R(I)*H%R(I1)*TIERS
          VOL2=MESH%SURFAC%R(I)*H%R(I2)*TIERS
          VOL3=MESH%SURFAC%R(I)*H%R(I3)*TIERS
!         FLUXES FROM POINTS
          F1= FLOPOINT(I,1)-FLOPOINT(I,3)
          F2=-FLOPOINT(I,1)+FLOPOINT(I,2)
          F3=-FLOPOINT(I,2)+FLOPOINT(I,3)
!         DEMAND OR OFFER, COMPARED TO A PRIORI AVAILABLE VOLUMES
          IF(F1*DT.GT.VOL1) THEN
            T1%R(I1)=T1%R(I1)+F1*DT-VOL1
          ELSE
            T3%R(I1)=T3%R(I1)+MIN(VOL1,VOL1-F1*DT)
          ENDIF
          IF(F2*DT.GT.VOL2) THEN
            T1%R(I2)=T1%R(I2)+F2*DT-VOL2
          ELSE
            T3%R(I2)=T3%R(I2)+MIN(VOL2,VOL2-F2*DT)
          ENDIF
          IF(F3*DT.GT.VOL3) THEN
            T1%R(I3)=T1%R(I3)+F3*DT-VOL3
          ELSE
            T3%R(I3)=T3%R(I3)+MIN(VOL3,VOL3-F3*DT)
          ENDIF
        ENDDO
        ELSEIF(OPTPRE.EQ.2) THEN
        DO IR=1,REMAIN
          I=INDIC_PDEPT_ERIA(IR)
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELMAX)
          I3=MESH%IKLE%I(I+2*NELMAX)
!         A PRIORI AVAILABLE VOLUMES
          VOL1=MESH%SURFAC%R(I)*H%R(I1)*TIERS
          VOL2=MESH%SURFAC%R(I)*H%R(I2)*TIERS
          VOL3=MESH%SURFAC%R(I)*H%R(I3)*TIERS
!         FLUXES FROM POINTS
          F1= FLOPOINT(I,1)-FLOPOINT(I,3)
          F2=-FLOPOINT(I,1)+FLOPOINT(I,2)
          F3=-FLOPOINT(I,2)+FLOPOINT(I,3)
!         DEMAND AND OFFER
          IF(F1.GT.0.D0) T1%R(I1)=T1%R(I1)+F1
          IF(F2.GT.0.D0) T1%R(I2)=T1%R(I2)+F2
          IF(F3.GT.0.D0) T1%R(I3)=T1%R(I3)+F3
          T3%R(I1)=T3%R(I1)+VOL1
          T3%R(I2)=T3%R(I2)+VOL2
          T3%R(I3)=T3%R(I3)+VOL3
        ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T1,2,MESH)
          CALL PARCOM(T3,2,MESH)
        ENDIF
!
        DO IR=1,REMAIN
!
          I=INDIC_PDEPT_ERIA(IR)
!
          I1=MESH%IKLE%I(I        )
          I2=MESH%IKLE%I(I  +NELMAX)
          I3=MESH%IKLE%I(I+2*NELMAX)
!
!         A PRIORI AVAILABLE VOLUMES
!
          VOL1=MESH%SURFAC%R(I)*H%R(I1)*TIERS
          VOL2=MESH%SURFAC%R(I)*H%R(I2)*TIERS
          VOL3=MESH%SURFAC%R(I)*H%R(I3)*TIERS
!
!         FLUXES FROM POINTS
!
          F1= FLOPOINT(I,1)-FLOPOINT(I,3)
          F2=-FLOPOINT(I,1)+FLOPOINT(I,2)
          F3=-FLOPOINT(I,2)+FLOPOINT(I,3)
!
!         DISTRIBUTION OF VOLUMES ACCORDING TO DEMAND AND OFFER
!
          IF(OPTPRE.EQ.1) THEN
          IF(F1*DT.GT.VOL1) THEN
            IF(T1%R(I1).GT.T3%R(I1)) THEN
              VOL1=VOL1+(F1*DT-VOL1)*(T3%R(I1)/T1%R(I1))
            ELSE
              VOL1=F1*DT
            ENDIF
          ELSE
            IF(T3%R(I1).GT.T1%R(I1)) THEN
              VOL1=VOL1-MIN(VOL1,VOL1-F1*DT)*(T1%R(I1)/T3%R(I1))
            ELSE
              VOL1=MAX(F1,0.D0)*DT
            ENDIF
          ENDIF
          IF(F2*DT.GT.VOL2) THEN
            IF(T1%R(I2).GT.T3%R(I2)) THEN
              VOL2=VOL2+(F2*DT-VOL2)*(T3%R(I2)/T1%R(I2))
            ELSE
              VOL2=F2*DT
            ENDIF
          ELSE
            IF(T3%R(I2).GT.T1%R(I2)) THEN
              VOL2=VOL2-MIN(VOL2,VOL2-F2*DT)*(T1%R(I2)/T3%R(I2))
            ELSE
              VOL2=MAX(F2,0.D0)*DT
            ENDIF
          ENDIF
          IF(F3*DT.GT.VOL3) THEN
            IF(T1%R(I3).GT.T3%R(I3)) THEN
              VOL3=VOL3+(F3*DT-VOL3)*(T3%R(I3)/T1%R(I3))
            ELSE
              VOL3=F3*DT
            ENDIF
          ELSE
            IF(T3%R(I3).GT.T1%R(I3)) THEN
              VOL3=VOL3-MIN(VOL3,VOL3-F3*DT)*(T1%R(I3)/T3%R(I3))
            ELSE
              VOL3=MAX(F3,0.D0)*DT
            ENDIF
          ENDIF
          ELSEIF(OPTPRE.EQ.2) THEN
          IF(T1%R(I1).GT.1.D-30) THEN
!                         ( THIS IS IMPORTANT
            VOL1=T3%R(I1)*(MAX(F1,0.D0)/T1%R(I1))
          ELSE
            VOL1=MESH%SURFAC%R(I)*H%R(I1)*TIERS
          ENDIF
          IF(T1%R(I2).GT.1.D-30) THEN
            VOL2=T3%R(I2)*(MAX(F2,0.D0)/T1%R(I2))
          ELSE
            VOL2=MESH%SURFAC%R(I)*H%R(I2)*TIERS
          ENDIF
          IF(T1%R(I3).GT.1.D-30) THEN
            VOL3=T3%R(I3)*(MAX(F3,0.D0)/T1%R(I3))
          ELSE
            VOL3=MESH%SURFAC%R(I)*H%R(I3)*TIERS
          ENDIF
          ENDIF
!
!         NOW LIMITATION OF FLUXES
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
!         LIMITED VOLUMES TRANSITING BETWEEN POINTS (1/DT MISSING)
!
          DTLIM1=MIN(DT1,DT2)
          DTLIM2=MIN(DT2,DT3)
          DTLIM3=MIN(DT3,DT1)
!
          FP1=FLOPOINT(I,1)*DTLIM1
          FP2=FLOPOINT(I,2)*DTLIM2
          FP3=FLOPOINT(I,3)*DTLIM3
!
!         CORRESPONDING VARIATIONS OF VOLUMES OF POINTS (DT MISSING SO OK)
!
          T4%R(I1)=T4%R(I1)-( FP1-FP3)
          T4%R(I2)=T4%R(I2)-(-FP1+FP2)
          T4%R(I3)=T4%R(I3)-(-FP2+FP3)
!
!         IF REMAINING FLUXES, THE ELEMENT IS KEPT IN THE LIST
!
          IF(DTLIM1.EQ.DT.AND.DTLIM2.EQ.DT.AND.DTLIM3.EQ.DT) THEN
            FLOPOINT(I,1)=0.D0
            FLOPOINT(I,2)=0.D0
            FLOPOINT(I,3)=0.D0
          ELSE
            NEWREMAIN=NEWREMAIN+1
!           BEFORE NEWREMAIN: FOR NEXT ITERATION
!           AFTER  NEWREMAIN: STILL VALID FOR NEXT ITERATION
            INDIC_PDEPT_ERIA(NEWREMAIN)=I
            FLOPOINT(I,1)=FLOPOINT(I,1)*(1.D0-DTLIM1*SURDT)
            FLOPOINT(I,2)=FLOPOINT(I,2)*(1.D0-DTLIM2*SURDT)
            FLOPOINT(I,3)=FLOPOINT(I,3)*(1.D0-DTLIM3*SURDT)
            C=C+ABS(FLOPOINT(I,1))+ABS(FLOPOINT(I,2))
     &         +ABS(FLOPOINT(I,3))
          ENDIF
!
        ENDDO
!
      REMAIN=NEWREMAIN
!
!     ADDING THE EVOLUTIONS TO THE DEPTHS
!     AFTER ASSEMBLY AT INTERFACES AND AFTER
!     CHANGING VOLUMES INTO DEPTHS.
!
      IF(NCSIZE.GT.1) CALL PARCOM(T4,2,MESH)
      CALL OS('X=XY    ',X=T4,Y=UNSV2D)
      CALL OS('X=X+Y   ',X=H,Y=T4)
      DO I=1,H%DIM1
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
      IF(NCSIZE.GT.1) C=P_SUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
!
!     STOP CRITERION
!
      IF(C.NE.CPREV.AND.ABS(C-CPREV).GT.CINIT*1.D-9
     &             .AND.C.NE.0.D0) THEN
        CPREV=C
        IF(NITER.LT.NITMAX) GO TO 777
      ENDIF
!
!     ADDING THE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
!     NOW THE NEGATIVE SOURCES
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            H%R(I)=H%R(I)+DT*MIN(SMH%R(I),0.D0)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            H%R(I)=H%R(I)+DT*MIN(SMH%R(I),0.D0)*UNSV2D%R(I)
          ENDDO
        ENDIF
      ENDIF
!
!     EVAPORATION (NEGATIVE PLUIE), WITH A POSSIBLE LIMITATION
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          IF(-DT*PLUIE%R(I).LT.H%R(I)) THEN
            H%R(I)=H%R(I)+DT*MIN(PLUIE%R(I),0.D0)
          ELSE
            PLUIE%R(I)=-H%R(I)/DT
            H%R(I)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!     BOUNDARY FLUXES : ADDING THE EXITING (POSITIVE) FLUXES
!                       WITH A POSSIBLE LIMITATION
!     IMPORTANT: MUST BE DONE AFTER ALL OTHER SOURCES
!
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
!                               T2 = // ASSEMBLED FLBOR
        HFL1=DT*UNSV2D%R(I)*MAX(T2%R(I),0.D0)
        IF(HFL1.GT.H%R(I)) THEN
!         FLBOR ACTUALLY TAKEN INTO ACCOUNT
          FLBOR%R(IPTFR)=FLBOR%R(IPTFR)*H%R(I)/HFL1
          H%R(I)=0.D0
        ELSE
          H%R(I)=H%R(I)-HFL1
        ENDIF
        IF(LIMPRO(IPTFR).EQ.KDIR) THEN
          IF(HBOR(IPTFR).LT.0.D0) THEN
            WRITE(LU,*) 'NEGATIVE DEPTH PRESCRIBED ON BOUNDARY'
            WRITE(LU,*) 'CHECK YOUR SPECIFIC SUBROUTINE:'
            IF(NAMECODE(1:7).EQ.'SISYPHE') THEN
              WRITE(LU,*) 'BEDLOAD_SOLVS_FE'
            ELSEIF(NAMECODE(1:4).EQ.'GAIA') THEN
              WRITE(LU,*) 'BEDLOAD_SOLVS_FE'
            ELSEIF(NAMECODE(1:9).EQ.'TELEMAC2D') THEN
              WRITE(LU,*) 'BORD'
            ELSEIF(NAMECODE(1:9).EQ.'TELEMAC3D') THEN
              WRITE(LU,*) 'BORD3D'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!         HERE V2DPAR WOULD BE MORE CONVENIENT THAN UNSV2D...
          FLBOR%R(IPTFR)=FLBOR%R(IPTFR)
     &                  +(H%R(I)-HBOR(IPTFR))/(DT*UNSV2D%R(I))
          H%R(I)= HBOR(IPTFR)
        ENDIF
      ENDDO
!
      IF(TESTING) THEN
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,H%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_MIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT HAUTEURS NEGATIVES, HMIN=',C
        C=0.D0
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN
            C=C+H%R(I)*MESH%IFAC%I(I)/UNSV2D%R(I)
          ENDDO
          C=P_SUM(C)
        ELSE
          DO I=1,NPOIN
            C=C+H%R(I)/UNSV2D%R(I)
          ENDDO
        ENDIF
        WRITE(LU,*) 'APRES TRAITEMENT MASSE FINALE =',C
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NOW WE WANT:
!     FLULIM TO BE THE PERCENTAGE OF FLUX THAT HAS BEEN TRANSMITTED
!     FLODEL TO BE THE ACTUAL FLUX THAT HAS BEEN TRANSMITTED
!
!     WE START WITH FLULIM=THE ORIGINAL FLODEL
!
!     WORKING ON ASSEMBLED VALUES TO FIND AN AVERAGE FLULIM
!     THAT WILL BE THE SAME AT INTERFACES
!
!     ASSEMBLING THE REMAINING FLUXES IN FLODEL
!
      DO I=1,NELEM
        IF(ABS(FLULIMEBE(I,1)).GT.1.D-30) THEN
          FLULIMEBE(I,1)=(FLULIMEBE(I,1)-FLOPOINT(I,1))
     &                  /FLULIMEBE(I,1)
        ELSE
          FLULIMEBE(I,1)=0.D0
        ENDIF
        IF(ABS(FLULIMEBE(I,2)).GT.1.D-30) THEN
          FLULIMEBE(I,2)=(FLULIMEBE(I,2)-FLOPOINT(I,2))
     &                  /FLULIMEBE(I,2)
        ELSE
          FLULIMEBE(I,2)=0.D0
        ENDIF
        IF(ABS(FLULIMEBE(I,3)).GT.1.D-30) THEN
          FLULIMEBE(I,3)=(FLULIMEBE(I,3)-FLOPOINT(I,3))
     &                  /FLULIMEBE(I,3)
        ELSE
          FLULIMEBE(I,3)=0.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECKING: COMPARING H(N+1) WITH H RECONSTRUCTED WITH THE FLUXES
!               SOURCES LACKING...
!
      IF(TESTING) THEN
        DO I=1,NPOIN
          T1%R(I)=0.D0
        ENDDO
        DO I=1,NSEG
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          T1%R(I1)=T1%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
          T1%R(I2)=T1%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
        ENDDO
        DO IPTFR=1,NPTFR
          I=NBOR(IPTFR)
          T1%R(I)=T1%R(I)-DT*UNSV2D%R(I)*FLBOR%R(IPTFR)
        ENDDO
        IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
        DO I=1,NPOIN
          T1%R(I)=T1%R(I)+HN%R(I)-H%R(I)
        ENDDO
        WRITE(LU,*) 'ERREUR POSITIVE_DEPTHS_ERIA=',P_DOTS(T1,T1,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(INFO) THEN
        IF(NAMECODE(1:7).EQ.'SISYPHE'.OR.NAMECODE(1:4).EQ.'GAIA') THEN
          WRITE(LU,102) NITER
        ELSE
          WRITE(LU,202) NITER
        ENDIF
      ENDIF
!
      IF(NITER.EQ.NITMAX) THEN
        IF(NAMECODE(1:4).EQ.'GAIA') THEN
          WRITE(LU,103) NITER
        ELSEIF(NAMECODE(1:7).EQ.'SISYPHE') THEN
          WRITE(LU,303) NITER
        ELSE
          WRITE(LU,203) NITER
        ENDIF
      ENDIF
!
102   FORMAT(' BEDLOAD EQUATION SOLVED IN ',1I5,' ITERATIONS')
202   FORMAT(' POSITIVE DEPTHS OBTAINED IN ',1I5,' ITERATIONS')
103   FORMAT(' BEDLOAD EQUATION SOLVED IN ',1I5,' ITERATIONS = MAXIMUM',
     & /,'INCREASE MAXIMUM NUMBER OF ITERATIONS FOR POSITIVE THICKNESS')
203   FORMAT(' POSITIVE DEPTHS SOLVED IN ',1I5,' ITERATIONS = MAXIMUM',
     &  /,'INCREASE MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES')
303   FORMAT(' BEDLOAD EQUATION SOLVED IN ',1I5,' ITERATIONS = MAXIMUM',
     &  /,'INCREASE MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
