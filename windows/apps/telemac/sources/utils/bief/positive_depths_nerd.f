!                   *******************************
                    SUBROUTINE POSITIVE_DEPTHS_NERD
!                   *******************************
!
     &(T1,T2,T4,H,HN,MESH,FLODEL,COMPUTE_FLODEL,FLBOR,DT,
     & UNSV2D,NPOIN,GLOSEG1,GLOSEG2,NBOR,NPTFR,
     & SMH,YASMH,PLUIE,RAIN,OPTSOU,FLULIM,LIMPRO,HBOR,KDIR,INFO,
     & FLOPOINT,NAMECODE,NITMAX,MAKEFLULIM)
!
!***********************************************************************
! BIEF   V8P3
!***********************************************************************
!
!brief    Suppresses negative depths by a limitation of fluxes, with the
!+        NERD technique.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/11/2016
!+        V7P3
!+   First version, taken out of previous positive_depths.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COMPUTE_FLODEL |-->| IF YES, COMPUTE FLODEL HERE
!| DOFLULIM       |-->| OPTIONAL, IF YES DOES ARRAY FLULIM
!| DT             |-->| TIME STEP
!| FLBOR          |<->| BOUNDARY FLUXES
!| FLODEL         |<->| FLUXES GIVEN BY SEGMENT
!|                |   | MAY BE COMPUTED HERE (SEE COMPUTE-FLODEL)
!|                |   | OR SIMPLY GIVEN. AT THE EXIT, THE REAL FLUX
!|                |   | TRANSMITTED IS GIVEN BACK.
!| FLOPOINT       |-->| FLUXES GIVEN BY POINTS (ELEMENT BY ELEMENT)
!| FLULIM         |<--| PER SEGMENT: PERCENTAGE OF FLUX THAT HAS NOT
!|                |   | BEEN TRANSMITTED AT THE END OF THE ALGORITHM
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
!| T4             |-->| WORK ARRAY
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!| YASMH          |-->| IF(YES) SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_POSITIVE_DEPTHS_NERD => POSITIVE_DEPTHS_NERD
      USE DECLARATIONS_TELEMAC, ONLY : DEJA_PDEPT_NERD,INDIC_PDEPT_NERD
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
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(*)
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: FLOPOINT(MESH%NELEM,3)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T4,FLODEL,H,FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PLUIE
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D,HN,SMH
      LOGICAL, INTENT(IN)             :: YASMH,INFO,RAIN
      LOGICAL, INTENT(IN)             :: COMPUTE_FLODEL
      CHARACTER(LEN=24)               :: NAMECODE
      LOGICAL, INTENT(IN)             :: MAKEFLULIM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,I2,IPTFR,REMAIN,NEWREMAIN,IR,NITER
      INTEGER NELEM,NELMAX,NSEG
      DOUBLE PRECISION C,CPREV,CINIT,HFL1
      DOUBLE PRECISION HSEG1,HSEG2,TET,HFL2
      DOUBLE PRECISION, PARAMETER :: TIERS=1.D0/3.D0
!
      DOUBLE PRECISION, PARAMETER :: EPS_FLUX = 1.D-15
      LOGICAL, PARAMETER :: TESTING = .FALSE.
!
!-----------------------------------------------------------------------
!
!     INDIC_PDEPT_NERD WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
!     HSEG IS THE DEPTH SHARED BETWEEN SEGMENTS
!
!-----------------------------------------------------------------------
!
      NELEM=MESH%NELEM
      NELMAX=MESH%NELMAX
      NSEG=MESH%NSEG
!
      IF(.NOT.DEJA_PDEPT_NERD) THEN
        ALLOCATE(INDIC_PDEPT_NERD(NSEG))
        DEJA_PDEPT_NERD=.TRUE.
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
!     ON INTERFACE SEGMENTS, ONLY ONE OF THE TWO TWIN SEGMENTS
!     WILL RECEIVE THE TOTAL FLUX, THE OTHER WILL GET 0.
!
      IF(NCSIZE.GT.1) THEN
        CALL MULT_INTERFACE_SEG(FLODEL%R,MESH%NH_COM_SEG%I,
     &                          MESH%NH_COM_SEG%DIM1,
     &                          MESH%NB_NEIGHB_SEG,
     &                          MESH%NB_NEIGHB_PT_SEG%I,
     &                          MESH%LIST_SEND_SEG%I,NSEG)
      ENDIF
!
      CALL CPSTVC(H,T2)
      CALL CPSTVC(H,T4)
      CALL CPSTVC(H,T1)
!
      IF(MAKEFLULIM) THEN
        DO I=1,NSEG
!         SAVING INITIAL FLODEL INTO FLULIM
          FLULIM(I)=FLODEL%R(I)
        ENDDO
      ENDIF
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
      REMAIN=NSEG
!
      DO I=1,REMAIN
        INDIC_PDEPT_NERD(I)=I
      ENDDO
!
      CPREV=0.D0

!     INITIAL SUM OF FLUXES
      DO I=1,NSEG
        CPREV=CPREV+ABS(FLODEL%R(I))
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
        IF(NITER.EQ.1) THEN
          DO I=1,NPOIN
            T1%R(I)=0.D0
            T4%R(I)=H%R(I)
          ENDDO
        ELSE
!         NOT ALL THE POINTS NEED TO BE INITIALISED NOW
          DO IR=1,REMAIN
            I=INDIC_PDEPT_NERD(IR)
            I1=GLOSEG1(I)
            I2=GLOSEG2(I)
            T1%R(I1)=0.D0
            T1%R(I2)=0.D0
!           SAVING THE DEPTH
            T4%R(I1)=H%R(I1)
            T4%R(I2)=H%R(I2)
          ENDDO
!         CANCELLING INTERFACE POINTS (SOME MAY BE ISOLATED IN A SUBDOMAIN
!         AT THE TIP OF AN ACTIVE SEGMENT WHICH IS ELSEWHERE)
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              T1%R(I)=0.D0
!             SAVING THE DEPTH
              T4%R(I)=H%R(I)
            ENDDO
          ENDIF
        ENDIF
!
!       COMPUTING THE DEMAND FOR EVERY POINT
!       CANCELLING DEPTHS THAT WILL BE DISTRIBUTED TO ACTIVE SEGMENTS
!       I.E. AS SOON AS THERE IS A DEMAND
!       ANYWAY THEY ARE STORED IN T4 THAT WILL BE USED INSTEAD
!
        DO IR=1,REMAIN
          I=INDIC_PDEPT_NERD(IR)
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          IF(FLODEL%R(I).GT.EPS_FLUX) THEN
            T1%R(I1)=T1%R(I1)+FLODEL%R(I)
            H%R(I1)=0.D0
          ELSEIF(FLODEL%R(I).LT.-EPS_FLUX) THEN
            T1%R(I2)=T1%R(I2)-FLODEL%R(I)
            H%R(I2)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
!         DEMAND ASSEMBLED IN PARALLEL
          CALL PARCOM(T1,2,MESH)
!         FOR ISOLATED POINTS CONNECTED TO AN ACTIVE
!         SEGMENT THAT IS IN ANOTHER SUBDOMAIN
!         H MUST BE CANCELLED, IF NOT, IT IS SHARED
!         SO THAT IT IS FOUND AGAIN ONCE ASSEMBLED
          DO IPTFR=1,NPTIR
            I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
!           AT THIS LEVEL H IS THE SAME AT INTERFACE POINTS
!           NOW H IS SHARED BETWEEN PROCESSORS TO ANTICIPATE
!           THE FINAL PARALLEL ASSEMBLY
            IF(T1%R(I).GT.EPS_FLUX) THEN
!             POINT THAT WILL GIVE
              H%R(I)=0.D0
            ELSE
!             POINT THAT WILL ONLY RECEIVE
!             IN THIS CASE THEIR DEPTH WILL NOT BE DISTRIBUTED
!             IN THE LOOP ON SEGMENTS, IT IS LEFT UNCHANGED
!             H IS SHARED TO ANTICIPATE THE FURTHER PARCOM
              H%R(I)=H%R(I)*MESH%IFAC%I(I)
            ENDIF
          ENDDO
        ENDIF
!
        C=0.D0
        NEWREMAIN=0
!
!       TRANSFER OF FLUXES
!
        DO IR=1,REMAIN
          I=INDIC_PDEPT_NERD(IR)
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          IF(FLODEL%R(I).GT.EPS_FLUX) THEN
!           SHARING ON DEMAND
            HSEG1=T4%R(I1)*FLODEL%R(I)/T1%R(I1)
!           END OF SHARING ON DEMAND
            HFL1= DT*UNSV2D%R(I1)*FLODEL%R(I)
            IF(HFL1.GT.HSEG1) THEN
              TET=HSEG1/HFL1
              H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)*TET
              FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
              C=C+FLODEL%R(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC_PDEPT_NERD(NEWREMAIN)=I
            ELSE
              H%R(I1)=H%R(I1)+HSEG1-HFL1
              H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
              FLODEL%R(I)=0.D0
            ENDIF
          ELSEIF(FLODEL%R(I).LT.-EPS_FLUX) THEN
!           SHARING ON DEMAND
            HSEG2=-T4%R(I2)*FLODEL%R(I)/T1%R(I2)
!           END OF SHARING ON DEMAND
            HFL2=-DT*UNSV2D%R(I2)*FLODEL%R(I)
            IF(HFL2.GT.HSEG2) THEN
              TET=HSEG2/HFL2
!             GATHERING DEPTHS
              H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)*TET
              FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
              C=C-FLODEL%R(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC_PDEPT_NERD(NEWREMAIN)=I
            ELSE
!             GATHERING DEPTHS
              H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
              H%R(I2)=H%R(I2)+HSEG2-HFL2
              FLODEL%R(I)=0.D0
            ENDIF
          ENDIF
!
        ENDDO
!
      REMAIN=NEWREMAIN
!     SUMMING THE NEW POSITIVE PARTIAL DEPTHS OF INTERFACE POINTS
      IF(NCSIZE.GT.1) CALL PARCOM(H,2,MESH)
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
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FLODEL%R,FLODEL%R,FLODEL%R,
     &                   NSEG,1,2,1,MESH,1,11)
        CALL PARCOM2_SEG(FLULIM,FLULIM,FLULIM,
     &                   NSEG,1,2,1,MESH,1,11)
      ENDIF
!
      DO I=1,NSEG
!       ACTUAL FLUX TRANSMITTED (=ORIGINAL-REMAINING)
        FLODEL%R(I)=FLULIM(I)-FLODEL%R(I)
!       PERCENTAGE OF ACTUAL FLUX WITH RESPECT TO ORIGINAL FLUX
        IF(ABS(FLULIM(I)).GT.EPS_FLUX) THEN
          FLULIM(I)=FLODEL%R(I)/FLULIM(I)
        ELSE
          FLULIM(I)=0.D0
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
!       SHARING AGAIN FLODEL FOR FURTHER USES
!       ON INTERFACE SEGMENTS, ONLY ONE OF THE TWO TWIN SEGMENTS
!       WILL RECEIVE THE TOTAL FLUX, THE OTHER WILL GET 0.
        CALL MULT_INTERFACE_SEG(FLODEL%R,MESH%NH_COM_SEG%I,
     &                          MESH%NH_COM_SEG%DIM1,
     &                          MESH%NB_NEIGHB_SEG,
     &                          MESH%NB_NEIGHB_PT_SEG%I,
     &                          MESH%LIST_SEND_SEG%I,NSEG)
      ENDIF
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
        WRITE(LU,*) 'ERREUR POSITIVE_DEPTHS_NERD=',P_DOTS(T1,T1,MESH)
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
