!                       *****************
                        SUBROUTINE CDL_FV
!                       *****************
!
     &(LIMPRO,W,CE,FLUENT,FLUSORT,FLBOR,UBOR,VBOR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES
!!        FOR FINITE VOLUME SCHEMES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CE         FLUX
!>@param  [in,out]  FLBOR      IN AND OUT WATER MASS FLUX
!>@param  [in,out]  FLUENT     ENTERING MASS FLUX
!>@param  [in,out]  FLUSORT    EXITING MASS FLUX
!>@param  [in]      LIMPRO     TYPES OF BOUNDARY CONDITION
!>@param  [in]      W          WORKING TABLE CONTAINING H,HU,HV
!>@param  [in,out]  UBOR       PRESCRIBED VALUES ON BOUNDARIES FOR U
!>@param  [in,out]  VBOR       PRESCRIBED VALUES ON BOUNDARIES FOR V
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,ONLY: KDIR,KENT,KDDL,KNEU,KENTU
      USE DECLARATIONS_TELEMAC2D,ONLY:NUMLIQ,LIUBOR,ENTET,ICIN,
     &                           NPOIN,MESH,HBOR,ZF,GRAV,
     &                           NPTFR,EPS_FV,VNX1,VNY1,YESNOFR,NFRLIQ,
     &                           NDEBIT,T2D_FILES,T2DIMP
      USE INTERFACE_TELEMAC2D, EX_CDL_FV => CDL_FV
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NPOIN,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: UBOR,VBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,IDRY,IFRLIQ
!
      DOUBLE PRECISION :: VNX,VNY,XNN,YNN,VNL(NPTFR)
      DOUBLE PRECISION :: UNN,VNN,LAMBDA1,LAMBDA2
      DOUBLE PRECISION :: FLX(NPTFR,3),FLXG(3),FLXG_DW(3),FLXD(3)
      DOUBLE PRECISION :: H1,U10,U1,V1,UGN,C1
      DOUBLE PRECISION :: HG,UG,VG,CG,UGTEMP
      DOUBLE PRECISION :: HG_DW,UG_DW,VG_DW,U1_DW,V1_DW,XNN1,YNN1
      DOUBLE PRECISION :: VNL1(NPTFR)
      DOUBLE PRECISION :: OUTFLOW,REGIME,DX,Q2(NFRLIQ)
      LOGICAL          :: YESNODW(NPTFR)
      LOGICAL          :: DEJA
!
      DEJA =.FALSE.
!
!     CORRECTION OF U/VBOR IF NECESSARY
!
      DO IFRLIQ=1,NFRLIQ
        Q2(IFRLIQ) = 0.D0
      ENDDO
      DO K=1,NPTFR
        IS = MESH%NBOR%I(K)
        IFRLIQ = NUMLIQ%I(K)
        IF(IFRLIQ.GT.0) THEN
          VNX=MESH%XNEBOR%R(K+NPTFR)
          VNY=MESH%YNEBOR%R(K+NPTFR)
          Q2(IFRLIQ) = Q2(IFRLIQ)
     &                 -W(1,IS)*(UBOR%R(K)*VNX+VBOR%R(K)*VNY)
        ENDIF
      ENDDO
      DO IFRLIQ = 1,NFRLIQ
        IF(NCSIZE.GT.1) Q2(IFRLIQ) = P_SUM(Q2(IFRLIQ))
      ENDDO
      DO K=1,NPTFR
        IFRLIQ = NUMLIQ%I(K)
        IF(IFRLIQ.GT.0) THEN
          IF(Q2(IFRLIQ).GT.0.D0.AND.LIUBOR%I(K).EQ.KENT.AND.
     &       (NDEBIT.GT.0.OR.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ')) THEN
            UBOR%R(K) = UBOR%R(K)*Q(IFRLIQ)/Q2(IFRLIQ)
            VBOR%R(K) = VBOR%R(K)*Q(IFRLIQ)/Q2(IFRLIQ)
          ENDIF
        ENDIF
      ENDDO
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     LOOP OVER BOUNDARY NODES
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF(NPTFR.GT.0)THEN    ! FOR PARALLEL CASES
      DO K=1,NPTFR
        IS=MESH%NBOR%I(K)
!
!       INITIALIZATION
        FLBOR%R(K) = 0.D0
        FLUENT     = 0.D0
        FLUSORT    = 0.D0
        OUTFLOW    = 0.D0
        FLX(K,1)   = 0.D0
        FLX(K,2)   = 0.D0
        FLX(K,3)   = 0.D0
        FLXG(1) = 0.D0
        FLXG(2) = 0.D0
        FLXG(3) = 0.D0
        FLXD(1) = 0.D0
        FLXD(2) = 0.D0
        FLXD(3) = 0.D0
        FLXG_DW(1) = 0.D0
        FLXG_DW(2) = 0.D0
        FLXG_DW(3) = 0.D0
        YESNODW(K) = .FALSE.
!
!       INDICATOR FOR DRY CELLS
        IDRY=0
!
!       NORMALIZED NORMAL
        XNN=MESH%XNEBOR%R(K)
        YNN=MESH%YNEBOR%R(K)
!
!       NON NORMALIZED NORMAL
        VNX=MESH%XNEBOR%R(K+NPTFR)
        VNY=MESH%YNEBOR%R(K+NPTFR)
        VNL(K)=SQRT(VNX**2+VNY**2)
!
!       NORMALS TO SOLID HALF EDGE
!       (LIQUID NEXT TO SOLID BND)
        IF(YESNOFR(K)) THEN
          VNL1(K)=SQRT(VNX1(K)**2+VNY1(K)**2)
          XNN1=VNX1(K)/VNL1(K)
          YNN1=VNY1(K)/VNL1(K)
        ELSE
          VNL1(K)=0.D0
          XNN1=0.D0
          YNN1=0.D0
        ENDIF
!
        H1 = W(1,IS)
        IF(H1.GT.EPS_FV)THEN
          U1 = W(2,IS)/H1
          V1 = W(3,IS)/H1
        ELSE
          U1 = 0.0D0
          V1 = 0.0D0
          IDRY=IDRY+1
        ENDIF
!
!       MEAN DISTANCE, FOR CFL WITH WAF
        DX = MESH%DTHAUT%R(IS)
!
!       **************************************************
!       WALL BOUNDARY
!       SLIPPING CONDITION
!       **************************************************
        IF(LIMPRO(K,1).EQ.KNEU) THEN

!         FIRST METHOD: STRONG IMPOSITION
!         ********************************
!         CE.n = 0  is done in cdlproj
!         DEFINITION OF THE GHOST STATE Ue
          HG = H1
!
!         ROTATION
          U10 = U1
          U1  = XNN*U10+YNN*V1
          V1  =-YNN*U10+XNN*V1
!
!         PUT NORMAL COMPONENT = 0
          U1 =  0.D0
          UG =  U1
          VG =  V1
!
!         INVERSE ROTATION
          U10 = U1
          U1  = -YNN*V1
          V1  =  XNN*V1
          UG  = -YNN*VG
          VG  =  XNN*VG
!
!         SECOND METHOD: WEAK IMPOSITION
!         ********************************
!         DEFINITION OF THE GHOST STATE Ue
!         HG = H1
!         INNER PRODUCT 2V.n
!         U10 = 2.D0*(U1*XNN + V1*YNN)
!         WEAK IMPOSITION: PUT VG = V1-2(V1.n)n
!         UG = U1 - U10*XNN
!         VG = V1 - U10*YNN
!
!       **************************************************
!       LIQUID BOUNDARIES
!       **************************************************
        ELSEIF(LIMPRO(K,1).EQ.KDIR.OR.LIMPRO(K,1).EQ.KDDL)THEN
!
!         ROTATION
          IF(H1.LT.EPS_FV)THEN
            UNN = 0.D0
            VNN = 0.D0
          ELSE
            UNN =  XNN*U1 + YNN*V1
            VNN = -YNN*U1 + XNN*V1
          ENDIF
!
!         ===============================
!         IF H GIVEN
!         ===============================
          IF(LIMPRO(K,1).EQ.KDIR) THEN
!
!           GHOST STATE FOR H DEFINED BY USER
            HG = HBOR%R(K)
!
!           REGIME ASSESSMENT: WE USE REAL H (H1)
            CG = SQRT(GRAV*HG)
            C1 = SQRT(GRAV*H1)
            LAMBDA1 = UNN + C1
            LAMBDA2 = UNN - C1
            REGIME  = LAMBDA1*LAMBDA2
!
!           SUBCRITICAL REGIME OR INFLOW
!           ----------------------------
            IF(REGIME.LT.0.D0.OR.UNN.LE.0.D0) THEN
!
              IF(HG.LT.EPS_FV)THEN
                UG = 0.D0
                VG = 0.D0
                IDRY = IDRY + 1
              ELSE
                IF(REGIME.LT.0.D0) THEN
!
!                 SUBCRITICAL
!                 -----------
                  UG = UNN + 2.D0*(C1-CG)
                  VG = VNN
!                 INVERSE ROTATION
                  UGTEMP = UG
                  UG = XNN*UGTEMP - YNN*VG
                  VG = YNN*UGTEMP + XNN*VG
!
                ELSE
!
!                 SUPERCRITICAL INFLOW
!                 --------------------
                  IF(LIUBOR%I(K).EQ.KENTU.OR.LIUBOR%I(K).EQ.KENT) THEN
!                   IMPOSED INFLOW
                    UG = UBOR%R(K)
                    VG = VBOR%R(K)
                  ELSE
!                   DATA MISSING
!                   WE SUPPOSE "THE LAKE AT REST"
                    UG = 0.D0
                    VG = 0.D0
                    IF(.NOT.DEJA.AND.ENTET)THEN
                      WRITE(LU,61) NUMLIQ%I(K)
                      DEJA=.TRUE.
                    ENDIF
                  ENDIF
!
                ENDIF
!
              ENDIF
!
!           SUPERCRITICAL OUTFLOW
!           ---------------------
            ELSE
              !HG = 0.D0
              HG = H1
              UG = 0.D0
              VG = 0.D0
              IF(.NOT.DEJA.AND.ENTET)THEN
                WRITE(LU,91) NUMLIQ%I(K),ABS(UNN)/MAX(EPS_FV,C1)
                DEJA=.TRUE.
!               NO CONTRIBUTION
              ENDIF
            ENDIF
!
!         ==================================
!         IF GIVEN VELOCITY OR DISCHARGE
!         ==================================
          ELSEIF(LIMPRO(K,2).EQ.KDIR)THEN     !   (LIUBOR%I(K).EQ.KENTU)THEN
!
!           GHOST STATE FOR U DEFINED BY USER
            UG = UBOR%R(K)
            VG = VBOR%R(K)
            UGN =  XNN*UG + YNN*VG ! TO RETRIEVE NORMAL COMPONENT OF UG
!           VGN = -YNN*UG + XNN*VG ! AND SO ON
!
!           IN CASE OF ROE: IMPOSSIBLE TO IMPOSE NEGATIVE DISCHARGE
            IF ((ICIN.EQ.0).AND.(UGN.GE.0.D0)) THEN
              WRITE(LU,21) NUMLIQ%I(K)
              CALL PLANTE(1)
              STOP
            ENDIF
!
!           REGIME ASSESSMENT: WE USE REAL H (H1)
            C1 = SQRT(GRAV*H1)
            LAMBDA1 = UNN + C1
            LAMBDA2 = UNN - C1
            REGIME  = LAMBDA1*LAMBDA2

!           SUBCRITICAL REGIME OR INFLOW
!           ----------------------------
            IF(REGIME.LT.0.D0.OR.UNN.LE.0.D0) THEN
!
              IF(REGIME.LT.0.D0) THEN
!
!               SUBCRITICAL
!               -----------
!               GHOST STATE DEFINED WITH THE CONSERVATION
!               OF SECOND RIEMANN INVARIANT
                HG = (UNN + 2.D0*SQRT(GRAV*H1)-UGN)**2/(4.D0*GRAV)
!
              ELSE
!
!               SUPERCRITICAL INFLOW
!               --------------------
                IF(LIUBOR%I(K).EQ.KENTU.OR.LIUBOR%I(K).EQ.KENT) THEN
!                 IMPOSED INFLOW
                  HG = HBOR%R(K)
                ELSE
                  ! DATA MISSING
                  HG = H1
                  IF(.NOT.DEJA.AND.ENTET)THEN
                    WRITE(LU,31) NUMLIQ%I(K)
                    DEJA=.TRUE.
                  ENDIF
                ENDIF

                ! DRY CELL
                IF(HG.LT.EPS_FV)THEN
                  IDRY = IDRY + 1
                ENDIF
!
              ENDIF
!
!           SUPERCRITICAL OUTFLOW
!           ---------------------
            ELSE
              !HG = 0.D0
              HG = H1
              UG = 0.D0
              VG = 0.D0
              IF(.NOT.DEJA.AND.ENTET)THEN
                WRITE(LU,91) NUMLIQ%I(K),ABS(UNN)/MAX(EPS_FV,C1)
                DEJA=.TRUE.
              ENDIF
            ENDIF
!
!         ===============================
!         CRITICAL OUTFLOW
!         ===============================
          ELSE
!
!           CRITICAL OUTFLOW FOR WAF/TCHAMEN/ZOKAGOA
            IF((ICIN.EQ.2).OR.(ICIN.EQ.3).OR.(ICIN.EQ.5)) THEN
              HG = H1
              IF(HG.GT.EPS_FV)THEN
                UG = U1
                VG = V1
              ELSE
                UG = 0.0D0
                VG = 0.0D0
                IDRY = IDRY + 1
              ENDIF
!
!           CRITICAL OUTFLOW
            ELSE
              !HG = 0.D0
              HG = H1
              UG = 0.D0
              VG = 0.D0
              IDRY = IDRY + 1
            ENDIF
!
          ENDIF
!
!         ============================================
!         WALL BOUNDARY CONDITION ON HALF EDGES
!         (LIQUID NODES CONNECTED TO SOLID BOUNDARIES)
!         ============================================
          IF(YESNOFR(K)) THEN
            YESNODW(K) = .TRUE.
            HG_DW = H1
            U1_DW = U1
            V1_DW = V1

!           ROTATION
            U10 = U1_DW
            U1_DW  = XNN1*U10+YNN1*V1_DW
            V1_DW  =-YNN1*U10+XNN1*V1_DW

!           PUT NORMAL COMPONENT = 0
            U1_DW = 0.D0
            UG_DW = U1_DW
            VG_DW = V1_DW

!           INVERSE ROTATION
            U10 = U1_DW
            U1_DW = -YNN1*V1_DW
            V1_DW =  XNN1*V1_DW
            UG_DW = -YNN1*VG_DW
            VG_DW =  XNN1*VG_DW
            ENDIF
!
        ENDIF
!
!       **************************************************
!       COMPUTE THE FLUX
!       **************************************************
!       AT LEAST ONE WET CELL
        IF(IDRY.LT.2)THEN
          CALL FLUX_CHOICE(H1,HG,H1,HG,U1,UG,V1,VG,ZF%R(IS),ZF%R(IS),
     &                     XNN,YNN,FLXG,FLXD,H1,HG,V1,VG,DX)

          IF(YESNODW(K).EQV..TRUE.) THEN
            CALL FLUX_CHOICE(H1,HG_DW,H1,HG_DW,U1_DW,UG_DW,V1_DW,
     &                       VG_DW,ZF%R(IS),ZF%R(IS),XNN1,YNN1,
     &                       FLXG_DW,FLXD,H1,HG_DW,V1_DW,VG_DW,DX)
          ENDIF
        ENDIF
        FLX(K,1) = VNL(K)*FLXG(1) + VNL1(K)*FLXG_DW(1)
        FLX(K,2) = VNL(K)*FLXG(2) + VNL1(K)*FLXG_DW(2)
        FLX(K,3) = VNL(K)*FLXG(3) + VNL1(K)*FLXG_DW(3)
!
      ENDDO
      ENDIF
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     FINAL BALANCE
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     FOR PARALLELISM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM_BORD(FLX(:,1),1,MESH)
        CALL PARCOM_BORD(FLX(:,2),1,MESH)
        CALL PARCOM_BORD(FLX(:,3),1,MESH)
      ENDIF
!
      IF(NPTFR.GT.0)THEN
      DO K=1,NPTFR
        IS=MESH%NBOR%I(K)
        IF(NCSIZE.GT.1)THEN
          OUTFLOW  = FLX(K,1)*MESH%IFAC%I(IS)
        ELSE
          OUTFLOW  = FLX(K,1)
        ENDIF
        IF(FLX(K,1).LE.0.D0)THEN ! INLET
          FLUENT = FLUENT + OUTFLOW
        ELSE                     ! OUTLET
          FLUSORT = FLUSORT + OUTFLOW
        ENDIF
        FLBOR%R(K) = OUTFLOW
!
        CE(IS,1)  = CE(IS,1) - FLX(K,1)
        CE(IS,2)  = CE(IS,2) - FLX(K,2)
        CE(IS,3)  = CE(IS,3) - FLX(K,3)
!
      ENDDO
!
      ENDIF ! PARALLEL CASES
!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     WARNINGS
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
21    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUBRITICAL OUTLET WITH IMPOSED  ',/,1X,
     & '          DISCHARGE INCOMPATIBLE WITH ROE SCHEME')
!
31    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL INLET        ',/,1X,
     & '          AND NO WATER DEPTH PROVIDED',/,1X)
!
61    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL INLET        ',/,1X,
     & '          AND NO DISCHARGE PROVIDED',/,1X)
!
91    FORMAT(1X,'CDL_FV: WARNING, LIQUID BOUNDARY ',1I6,/,1X,
     & '          SUPERCRITICAL OUTLET        ',/,1X,
     & '          DESIRED BOUNDARY CONDITION MAY BE UNSATISFIED',/,1X,
     & '          FROUDE AT BOUNDARY IS: ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
