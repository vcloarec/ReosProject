!                   ********************
                    SUBROUTINE FLUX_TRAC
!                   ********************
!
     &(NUBO,IKLE,FLUTENT,FLUTSOR,CMI,DJXT,DJYT,DXT,DYT,DPX,DPY,BETA,
     & DSZ,AIRST,HC,FLUXT,ELTSEG,IFABOR,VNOCL,FLBOR)
!
!***********************************************************************
! TELEMAC2D   V8P1
!***********************************************************************
!
!brief    COMPUTES TRACER FLUXES.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      AIRST       AREA OF SUB-TRIANGLES (SECOND ORDER)
!>@param  [in,out]  BETA        COEFFICIENT OF EXTRAPOLATION FOR ORDRE2
!>@param  [in]      CMI         COORDINATES OF MIDDLE PONTS OF EDGES
!>@param  [in]      DIFT        LOGICAL TELLING IF THERE IS DIFFUSION
!+                              FOR TRACER OR NOT
!>@param  [in]      DJXT        GRADIENTS PER TRIANGLES
!>@param  [in]      DJYT        GRADIENTS PER TRIANGLES
!>@param  [in]      DPX         GRADIENTS OF P1 BASE FUNCTIONS
!>@param  [in]      DPY         GRADIENTS OF P1 BASE FUNCTIONS
!>@param  [in]      DSZ         VARIATION OF Z FOR ORDRE 2
!>@param  [in]      DXT         GRADIENTS AT THE NODES
!>@param  [in]      DYT         GRADIENTS AT THE NODES
!>@param  [in]      ELTSEG      SEGMENT CONPOSING AN ELEMENT
!>@param  [in,out]  FLUXT       FLUX FOR TRACER AT TIME N
!>@param  [in,out]  FLUTENT     TRACER FLUX AT THE INLET
!>@param  [in,out]  FLUTSOR     TRACER FLUX AT THE OUTLET
!>@param  [in]      HC          STOCKED H RECONSTRUCTED FOR ORDRE 2
!>@param  [in]      IFABOR      ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!>@param  [in]      IKLE        NUMBERING OF NODES IN THE TRIANGLES
!>@param  [in]      NUBO        GLOBAL INDICES OF EDGE EXTREMITIES
!>@param  [in]      VNOCL       NORMAL VECTOR TO THE INTERFACE
!>@param  [in]      FLBOR       BOUNDARY MASS FLUXES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FLUX_TRAC => FLUX_TRAC
      USE BIEF_DEF
      USE DECLARATIONS_TELEMAC2D,ONLY: DST_MT,DSP_MT,NSEG,NPTFR,
     &                                 DSM_MT,CORRT_MT,GRADI_MT,SORDER,
     &                                 GRADJ_MT,GRADJI_MT,GRADIJ_MT,
     &                                 DEJA_MT,NPOIN,NELMAX,NTRAC,ILIMT,
     &                                 LITBOR,X,Y,V2DPAR,MESH,ZF,
     &                                 H,TN,TBOR,FLUHTEMP
      USE DECLARATIONS_TELEMAC, ONLY: KENT
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),IKLE(NELMAX,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(IN)             :: IFABOR(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: DSZ(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELMAX),DPY(3,NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: CMI(2,*),AIRST(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(*),DJYT(*),DXT(*),DYT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA
      DOUBLE PRECISION, INTENT(IN)    :: VNOCL(3,NSEG),HC(2,*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUXT
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NSG,NUBO1,NUBO2,J,ERR,I,IEL,ITRAC
!
      DOUBLE PRECISION ZF1,ZF2,FLUT,HI0,HJ0,AIX,AIY,AJX,AJY,AMDS
      DOUBLE PRECISION FLU11,FLU41,UAS41,UAS42,DSZ1,DSZ2
      DOUBLE PRECISION DEMI,PROD_SCAL,DSH
!
!     DYNAMIC ARRAY ALLOCATION !!!!!!!!
!
      LOGICAL, ALLOCATABLE ::   YESNO(:)
!
!-----------------------------------------------------------------------
!     LOOP ON TRACERS
      DO ITRAC=1,NTRAC
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA_MT) THEN
        ALLOCATE(DST_MT(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSP_MT(NPOIN),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSM_MT(NPOIN)    ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(CORRT_MT(NPOIN)   ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(GRADI_MT(NSEG),GRADJ_MT(NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(GRADIJ_MT(NSEG),GRADJI_MT(NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        GO TO 1002
1001    CONTINUE
        WRITE(LU,2000) ERR
2000    FORMAT(1X,'MAFTRAC: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
1002    CONTINUE
        DEJA_MT=.TRUE.
      ENDIF
      DEMI = 0.5D0
!
!-----------------------------------------------------------------------
!
!     INITIALISES
      ALLOCATE(YESNO(NSEG),STAT=ERR)
      IF(ERR.GT.0)THEN
        WRITE(LU,2000) ERR
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
!     COMPUTES THE TRACER GRADIENTS BY TRIANGLE AND BY NODE
!     COMPUTES THE DIFFUSION TERM
      IF(SORDER.EQ.2) CALL GRADNODT(NPOIN,NELMAX,IKLE,
     &MESH%SURFAC%R,V2DPAR%R,TN%ADR(ITRAC)%P%R,DPX,DPY,DJXT,DJYT,
     &DXT,DYT,MESH)
!
!     *****************************
!     REBUILDS 2ND ORDER FOR TRACER
!     *****************************
      IF(SORDER.EQ.2) THEN
!
!     INITIALIZATION
      DSP_MT(:)  =(/(0.D0,IS=1,NPOIN)/)
      DSM_MT(:)  =(/(0.D0,IS=1,NPOIN)/)
      DST_MT(1,:)=(/(0.D0,IS=1,NSEG)/)
      DST_MT(2,:)=(/(0.D0,IS=1,NSEG)/)
!
!     INITIALIZATION  OF GRADIENTS
      GRADI_MT(:) =(/(0.D0,IS=1,NSEG)/)
      GRADJ_MT(:) =(/(0.D0,IS=1,NSEG)/)
      GRADIJ_MT(:)=(/(0.D0,IS=1,NSEG)/)
      GRADJI_MT(:)=(/(0.D0,IS=1,NSEG)/)
!
!     LOOP ON ELEMENTS
      DO IEL=1, NELMAX
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!           RECUPERATE JMI
!           THIS THE TRIANGLE IN WHICH IS LOCATED CMI
            J   = MESH%JMI%I(NSG)
!
!           THAT MEANS CMI IS NOT LOCATED IN TRIANGLE J
            IF(NCSIZE.GT.1.AND.J.EQ.0) CYCLE
!
!           RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!           WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
!
            ZF1 = ZF%R(NUBO1)
            ZF2 = ZF%R(NUBO2)
!
            DSZ1 = DSZ(1,NSG)
            DSZ2 = DSZ(2,NSG)
!
            HI0 = H%R(NUBO1)
            HJ0 = H%R(NUBO2)
!
!           STICKS TO 1ST ORDER FOR A COVERED EDGE
!
            IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &         .OR. 2.*ABS(DSZ1).GE.HI0
     &         .OR. 2.*ABS(DSZ1).GE.HJ0
     &         .OR. 2.*ABS(DSZ2).GE.HI0
     &         .OR. 2.*ABS(DSZ2).GE.HJ0)  THEN
!             DST_MT(1,NSG) =0.D0
!             DST_MT(2,NSG) =0.D0
              YESNO(NSG) = .TRUE.
              CYCLE
            ELSE
!
              AIX         = CMI(1,NSG)-X(NUBO1)
              AIY         = CMI(2,NSG)-Y(NUBO1)
              AJX         = CMI(1,NSG)-X(NUBO2)
              AJY         = CMI(2,NSG)-Y(NUBO2)
!
              GRADI_MT(NSG)  = AIX*DXT(NUBO1) + AIY*DYT(NUBO1)
              GRADJ_MT(NSG)  = AJX*DXT(NUBO2) + AJY*DYT(NUBO2)
              GRADIJ_MT(NSG) = AIX*DJXT(J) + AIY*DJYT(J)
              GRADJI_MT(NSG) = AJX*DJXT(J) + AJY*DJYT(J)
            ENDIF
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1)THEN      ! NPON,NPLAN,ICOM,IAN , HERE ICOM=1 VALUE WITH MAX | |
        CALL PARCOM2_SEG(GRADI_MT,GRADJ_MT,GRADI_MT,
     &              NSEG,1,1,2,MESH,1,11)
        CALL PARCOM2_SEG(GRADIJ_MT,GRADJI_MT,GRADJI_MT,
     &              NSEG,1,1,2,MESH,1,11)
      ENDIF
!
!     EXTRAPOLATES THE GRADIENTS AND SLOPE LIMITOR
!
      BETA=1.D0/3.D0
!
      DO NSG=1,NSEG
        DST_MT(1,NSG)  = EXLIM (ILIMT,BETA,GRADI_MT(NSG),GRADIJ_MT(NSG))
        DST_MT(2,NSG)  = EXLIM (ILIMT,BETA,GRADJ_MT(NSG),GRADJI_MT(NSG))
      ENDDO
!
!     INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
      DO IEL=1, NELMAX
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!           RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!           WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
!
!           FOR PARALLELILSM
            IF(NCSIZE.GT.1.AND.IFABOR(IEL,I).EQ.-2)THEN ! THIS IS AN INTERFACE EDGE
              IF(DST_MT(1,NSG).GE.0.D0) THEN
                DSP_MT(NUBO1) = DSP_MT(NUBO1) +
     &                   DEMI*AIRST(1,NSG)*HC(1,NSG)*DST_MT(1,NSG) ! WE CONSIDER ONLY
              ELSE                                                      ! 0.5 AIRST
                DSM_MT(NUBO1) = DSM_MT(NUBO1) -
     &                   DEMI*AIRST(1,NSG)*HC(1,NSG)*DST_MT(1,NSG) ! PARCOM2 WILL ADD
              ENDIF                                                     ! CONTRIBUTIONS
              IF(DST_MT(2,NSG).GE.0.D0) THEN
                DSP_MT(NUBO2) = DSP_MT(NUBO2) +
     &                   DEMI*AIRST(2,NSG)*HC(2,NSG)*DST_MT(2,NSG)
              ELSE
                DSM_MT(NUBO2) = DSM_MT(NUBO2) -
     &                   DEMI*AIRST(2,NSG)*HC(2,NSG)*DST_MT(2,NSG)
              ENDIF
              IF(DST_MT(2,NSG).GE.0.D0) THEN
                DSP_MT(NUBO2) = DSP_MT(NUBO2) +
     &                   DEMI*AIRST(2,NSG)*HC(2,NSG)*DST_MT(2,NSG)
              ELSE
                DSM_MT(NUBO2) = DSM_MT(NUBO2) -
     &                   DEMI*AIRST(2,NSG)*HC(2,NSG)*DST_MT(2,NSG)
              ENDIF
            ELSE ! NO PARALLELILSM OR NO INTERFACE EDGE
              IF(DST_MT(1,NSG).GE.0.D0) THEN
                DSP_MT(NUBO1) = DSP_MT(NUBO1) +
     &          AIRST(1,NSG)* HC(1,NSG)*DST_MT(1,NSG)
              ELSE
                DSM_MT(NUBO1) = DSM_MT(NUBO1) -
     &          AIRST(1,NSG)* HC(1,NSG)*DST_MT(1,NSG)
              ENDIF
              IF(DST_MT(2,NSG).GE.0.) THEN
                DSP_MT(NUBO2) = DSP_MT(NUBO2) +
     &          AIRST(2,NSG)* HC(2,NSG)*DST_MT(2,NSG)
              ELSE
                DSM_MT(NUBO2) = DSM_MT(NUBO2) -
     &          AIRST(2,NSG)* HC(2,NSG)*DST_MT(2,NSG)
              ENDIF
            ENDIF
!
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
      !  FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(DSP_MT,DSM_MT,DSM_MT,NPOIN,1,2,2,MESH)
      ENDIF
!
!     COMPUTES THE CORRECTIONS TO ENSURE CONSERVATION OF HT
!                  ***********           ******************
!
      DO IS=1,NPOIN
        CORRT_MT(IS) =  DSM_MT(IS) - DSP_MT(IS)
        AMDS =MAX(DSP_MT(IS),DSM_MT(IS))
        IF(AMDS.GT.0.D0) THEN
          CORRT_MT(IS) = CORRT_MT(IS)/AMDS
        ENDIF
      ENDDO
!
      ENDIF
!     ENDIF OF SORDER.EQ.2
!
!     *******************************************
!     COMPUTES FLUXES FOR THE INTERNAL INTERFACES
!     *******************************************
!
!     REINITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
!     LOOP ON GLOBAL LIST OF EDGES
      DO IEL=1, NELMAX
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!
!           RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!           WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)

            IF(SORDER.EQ.2) THEN
              DST_MT(1,NSG) = DST_MT(1,NSG) +
     &            MIN(0.D0,CORRT_MT(NUBO1))*MAX(0.D0,DST_MT(1,NSG))+
     &            MAX(0.D0,CORRT_MT(NUBO1))*MAX(0.D0,-DST_MT(1,NSG))

              DST_MT(2,NSG) = DST_MT(2,NSG) +
     &            MIN(0.D0,CORRT_MT(NUBO2))*MAX(0.D0,DST_MT(2,NSG))+
     &            MAX(0.D0,CORRT_MT(NUBO2))*MAX(0.D0,-DST_MT(2,NSG))

              PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                    (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
              IF(PROD_SCAL.LT.0.D0)THEN
                NUBO1 = NUBO(2,NSG)
                NUBO2 = NUBO(1,NSG)
                DSH = DST_MT(1,NSG)
                DST_MT(1,NSG) = DST_MT(2,NSG)
                DST_MT(2,NSG) = DSH
              ENDIF
            ELSE
!
              PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                    (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
              IF(PROD_SCAL.LT.0.D0)THEN
                NUBO1 = NUBO(2,NSG)
                NUBO2 = NUBO(1,NSG)
              ENDIF
            ENDIF
!
            UAS41 = TN%ADR(ITRAC)%P%R(NUBO1)
            UAS42 = TN%ADR(ITRAC)%P%R(NUBO2)
!
            FLU11 = FLUHTEMP%ADR(ITRAC)%P%R(NSG)
!
            IF (FLU11.GE.0.) THEN
              IF(SORDER.EQ.2) THEN
                UAS41 = UAS41  + DST_MT(1,NSG)
              ENDIF
              FLU41 =  UAS41 * FLU11
            ELSE
              IF(SORDER.EQ.2) THEN
                UAS42 = UAS42 + DST_MT(2,NSG)
              ENDIF
              FLU41 =  UAS42 * FLU11
            ENDIF
!
            FLUXT%ADR(ITRAC)%P%R(NUBO1) = FLUXT%ADR(ITRAC)%P%R(NUBO1)
     &                                  - FLU41
            FLUXT%ADR(ITRAC)%P%R(NUBO2) = FLUXT%ADR(ITRAC)%P%R(NUBO2)
     &                                  + FLU41
!
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
!     FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(FLUXT%ADR(ITRAC)%P%R,
     &               FLUXT%ADR(ITRAC)%P%R,
     &               FLUXT%ADR(ITRAC)%P%R,NPOIN,1,2,1,MESH)
      ENDIF
!
!     *********************************
!     COMPUTES BOUNDARY FLUXES
!     *********************************
!
      IF(NPTFR.GT.0)THEN  ! USEFUL FOR PARALLEL CASE
        DO K=1,NPTFR
          IS=MESH%NBOR%I(K)
!
!         OUTLET
          IF(FLBOR%R(K).GT.0.D0) THEN
            FLUT = TN%ADR(ITRAC)%P%R(IS)*FLBOR%R(K)
            FLUTSOR(ITRAC) = FLUTSOR(ITRAC) + FLUT

!         INTLET
          ELSE
!           IMPOSED TRACER VALUE
            IF (LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT) THEN
              FLUT = TBOR%ADR(ITRAC)%P%R(K)*FLBOR%R(K)
!           FREE TRACER VALUE, TRACER IMPOSED AT THE LAST VALUE
            ELSE
              FLUT = TN%ADR(ITRAC)%P%R(IS)*FLBOR%R(K)
            ENDIF
            FLUTENT(ITRAC) = FLUTENT(ITRAC) + FLUT
          ENDIF
!
!         FINAL UPDATE OF BOUNDARY FLUX
          FLUXT%ADR(ITRAC)%P%R(IS) = FLUXT%ADR(ITRAC)%P%R(IS) - FLUT
!
        ENDDO
      ENDIF
!
      DEALLOCATE(YESNO)
!
!-----------------------------------------------------------------------
!     END OF LOOP ON TRACERS
      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END
