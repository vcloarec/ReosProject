!                       ******************
                        SUBROUTINE CDL_CIN
!                       ******************
!
     &(LIMPRO,UA,CE,FLUENT,FLUSORT,FLBOR,UBOR,VBOR)
!
!***********************************************************************
! TELEMAC 2D
!***********************************************************************
!
!>@brief  COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES
!!    UA(1,IS) = H,  UA(2,IS)=HU  ,UA(3,IS)=HV
!
!>@history  INRIA
!!
!!        V5P8
!!
!
!>@history  R. ATA (EDF-LNHE) BALANCE OF WATER
!!        15/03/2010
!!        V6P1
!!   Translation of French comments within the FORTRAN sources into
!!   English comments
!
!>@history  R. ATA (EDF-LNHE)
!!        30/01/2015
!!        V7p0
!!   parallelization
!!
!
!>@history  J,RIEHME (ADJOINTWARE)
!!        November 2016
!!        V7P2
!!   Replaced EXTERNAL statements to parallel functions / subroutines
!!   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CE         FLUX
!>@param  [in,out]  FLBOR      IN AND OUT WATER MASS FLUX
!>@param  [in,out]  FLUENT     ENTERING MASS FLUX
!>@param  [in,out]  FLUSORT    EXITING MASS FLUX
!>@param  [in]      LIMPRO     TYPES OF BOUNDARY CONDITION
!>@param  [in]      UA         UA(1,IS) = H,  UA(2,IS)=HU  ,UA(3,IS)=HV
!>@param  [in,out]  UBOR       PRESCRIBED VALUES ON BOUNDARIES FOR U
!>@param  [in,out]  VBOR       PRESCRIBED VALUES ON BOUNDARIES FOR V
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_CDL_CIN => CDL_CIN
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: NPOIN,NPTFR,HBOR,GRAV,
     &                            MESH,EPS_FV,NUMLIQ,
     &                            NFRLIQ,NDEBIT,T2D_FILES,T2DIMP,
     &                            LIUBOR
      USE DECLARATIONS_TELEMAC, ONLY: KDIR,KNEU,KENT
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: UA(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NPOIN,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: UBOR,VBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NIT,IFRLIQ
!
      DOUBLE PRECISION RA3,RA32,RA33, ALP,ALP2,ALP3,SG,SQ2
      DOUBLE PRECISION VNX,VNY,VNX1,VNY1,VNL,H,U,V,RUN
      DOUBLE PRECISION FLUH(NPTFR),FLUU(NPTFR),FLUV(NPTFR)
      DOUBLE PRECISION AUX,FLUTMP,RH,HRH,UNN,VNN
      DOUBLE PRECISION FHPLUS,FUPLUS,FHMOINS,FUMOINS
      DOUBLE PRECISION A,A1,A2,A3,ALPHA0,ALPHA1,ALPHA2,C,VP1,VP2 ,VP3
      DOUBLE PRECISION HG ,RHG,HRHG,UG,VG,DEST,RVG,CA1,AM
      DOUBLE PRECISION UIN,VIN,HUIN,HVIN
      DOUBLE PRECISION OUTFLOW,Q2(NFRLIQ)
!
      SQ2   = SQRT(2.D0)
      SG    = SQRT(GRAV)
      RA3   = SQRT(1.5D0*GRAV)
      RA32  = RA3**2
      RA33  = RA3*RA32
      ALP   = 0.5D0/RA3
      ALP2  = 0.5D0 *ALP
      ALP3  = ALP/3.D0
!
      FLUENT=0.D0
      FLUSORT=0.D0
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
     &                 -UA(1,IS)*(UBOR%R(K)*VNX+VBOR%R(K)*VNY)
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
      IF(NPTFR.GT.0)THEN ! USEFUL FOR PARALLEL
        DO K=1,NPTFR
          IS=MESH%NBOR%I(K)
!
!         INITIALIZATION
          VNX1=MESH%XNEBOR%R(K)
          VNY1=MESH%YNEBOR%R(K)
          VNX=MESH%XNEBOR%R(K+NPTFR)
          VNY=MESH%YNEBOR%R(K+NPTFR)
          VNL=SQRT(VNX**2+VNY**2)
!
          H   = UA(1,IS)
          IF(H.GT.EPS_FV) THEN
            U   = UA(2,IS)/H
            V   = UA(3,IS)/H
            RH  = SQRT(H)
          ELSE
            U = 0.D0
            V = 0.D0
            RH = 0.D0
          ENDIF
!
!         **************************************************
!         WALL BOUNDARY
!         SLIPPING CONDITION
!         **************************************************
          IF(LIMPRO(K,1).EQ.KNEU) THEN
            AUX=0.5D0*GRAV*H**2
            FLUH(K) = 0.D0
            FLUU(K) = AUX*VNX
            FLUV(K) = AUX*VNY
!
!         **************************************************
!         LIQUID BOUNDARIES
!         **************************************************
          ELSE
!
!           CALCULATION OF F+(H,U,V)
            HRH = RH * H
            IF(H.LT.EPS_FV) THEN
              U=0.D0
              V=0.D0
              UNN=0.D0
              VNN=0.D0
              FHPLUS = 0.D0
              FUPLUS = 0.D0
            ELSE
              UNN= +VNX1*U+VNY1*V
              VNN= -VNY1*U+VNX1*V
              A=MIN(RA3,MAX(-RA3,-UNN/RH))
              A2 =A * A
              A3 =A2 * A
              ALPHA0=ALP*(RA3-A)
              ALPHA1=ALP2*(RA32-A2)
              ALPHA2=ALP3*(RA33-A3)
              FHPLUS = H*UNN*ALPHA0 + HRH*ALPHA1
              FUPLUS = UNN*(FHPLUS+HRH*ALPHA1) + H*H*ALPHA2
            ENDIF
!
!           CALCULATION OF FICTIVE STATE (HG,UG,VG)
!
!           ===============================
!           IF H GIVEN
!           ===============================
            IF(LIMPRO(K,1).EQ.KDIR) THEN
!
              C   = SG*RH
              VP1 = UNN
              VP2 = VP1  + C
              VP3 = VP1  - C
!
              HG     =HBOR%R(K)
              RHG    =SQRT (HG)
              HRHG   =RHG*HG
!
!             SUBCRITICAL REGIME OR INFLOW
!             ----------------------------
              IF (VP2*VP3.LE.0.D0.OR. VP1.LE.0.D0) THEN
!
                IF(HG.EQ.0.D0) THEN
                  UG=0.D0
                  VG=0.D0
                  FHMOINS = 0.D0
                  FUMOINS = 0.D0
                ELSE
                  IF (VP2*VP3.LE.0.D0) THEN
!
!                   SUBCRITICAL
!                   -----------
                    UG = UNN + 2.D0*SG*(RH-RHG)
                    VG = VNN
!
                  ELSE
!
!                   SUPERCRITICAL INFLOW
!                   --------------------
                    IF(LIMPRO(K,2).EQ.KDIR) THEN
!                     IMPOSED INFLOW
                      UIN = UBOR%R(K)
                      VIN = VBOR%R(K)
                      HUIN = H*UIN
                      HVIN = H*VIN
!
                      DEST = HUIN*VNX1+HVIN*VNY1
                      RVG  =-HUIN*VNY1+HVIN*VNX1
!
                      A1 = DEST-FHPLUS
                      CA1= SQ2*A1/(SG*HG*RHG)
                      CALL ZEROPHI(-1.D0,AM,NIT,CA1)
!
                      UG= AM*SG*RHG
                      VG=RVG/HG
!
                    ELSE
!                     ONE DATUM IS MISSING
!                     WE SUPPOSE "THE LAKE AT REST"
                      UG= 0.D0
                      VG= 0.D0
!
                    ENDIF
!
                  ENDIF
!
                  GOTO 220
                ENDIF
                GOTO 200
!
!             SUPERCRITICAL OUTFLOW
!             ---------------------
!             THE OUTFLOW IS TORRENTIAL SO WE HAVE NO NEED FOR THE GIVEN H
              ELSE
                GOTO 100
              ENDIF
!
!           ==================================
!           IF GIVEN VELOCITY OR DISCHARGE
!           ==================================
            ELSE IF(LIMPRO(K,2).EQ.KDIR) THEN
!
              UIN = UBOR%R(K)
              VIN = VBOR%R(K)
              HUIN = H*UIN
              HVIN = H*VIN
!
              DEST=HUIN*VNX1+HVIN*VNY1
              RVG =-HUIN*VNY1+HVIN*VNX1
!             WARNING: SIGN CHANGE / INRIA REPORT
              A1 = -DEST+FHPLUS
              A2 = -UNN - 2.D0*SG*RH
!
              IF (A1.LE.0.D0) THEN
!
!               FH- =-A1 CANNOT BE SATISFIED
!
                FHMOINS = 0.D0
                FUMOINS = 0.D0
                VG=0.D0
              ELSE
                CA1= 1.D0/(GRAV*SQ2*A1)**(1.D0/3.D0)
                CALL ZEROPSI(-0.5D0,AM,NIT,CA1,A2)
!
                RHG =A2/(SG*(AM-2.D0))
                HG= RHG * RHG
                HRHG= RHG * HG
!
                IF (HG.EQ.0.D0) THEN
                  UG=0.D0
                  VG=0.D0
                  FHMOINS = 0.D0
                  FUMOINS = 0.D0
                ELSE
                  UG=-AM*A2/(AM-2.D0)
                  VG=RVG/HG
                  GOTO 220
                ENDIF
              ENDIF
              GOTO 200
!
!           ===============================
!           CRITICAL OUTFLOW
!           ===============================
            ELSE
!
              GOTO 100
!
            ENDIF
            GOTO 1000
!
!
!           CALCULATION OF F-(HG,UG,VG)
!
 220        CONTINUE
!
            A=MIN(RA3,MAX(-RA3,-UG/RHG))
            A2 =A * A
            A3 =A2 * A
            ALPHA0=ALP*(A+RA3)
            ALPHA1=ALP2*(A2-RA32)
            ALPHA2=ALP3*(A3+RA33)
!
            FHMOINS = HG*UG*ALPHA0 + HRHG*ALPHA1
            FUMOINS = UG*(FHMOINS + HRHG*ALPHA1)
     &      + HG*HG*ALPHA2
!
!           CALCUL DES FLUX ET ROTATION INVERSE
!
 200        CONTINUE
            FLUH(K)=(FHPLUS +FHMOINS)*VNL
            FLUU(K)=(FUPLUS +FUMOINS)*VNL
!
            IF (FLUH(K).GE.0.D0) THEN
              FLUV(K)= VNN*FLUH(K)
            ELSE
              FLUV(K)= VG*FLUH(K)
            ENDIF
!
            FLUTMP=FLUU(K)
            FLUU(K) = +VNX1*FLUTMP-VNY1*FLUV(K)
            FLUV(K) = +VNY1*FLUTMP+VNX1*FLUV(K)
!
!           CORRECTION OF THE TIME STEP
!
            GOTO 1000
100         CONTINUE
            RUN     = H*UNN
!
            FLUH(K) =  RUN* VNL
            FLUU(K) =  (U *RUN + 0.5D0*GRAV*H**2* VNX)*VNL
            FLUV(K) =  (V *RUN + 0.5D0*GRAV*H**2* VNY)*VNL
!
1000        CONTINUE
          ENDIF
        ENDDO
      ENDIF

!
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM_BORD(FLUH,1,MESH)
        CALL PARCOM_BORD(FLUU,1,MESH)
        CALL PARCOM_BORD(FLUV,1,MESH)
      ENDIF
!
      IF(NPTFR.GT.0)THEN
        DO K=1,NPTFR
          IS=MESH%NBOR%I(K)
!
          IF(NCSIZE.GT.1)THEN
            OUTFLOW  = FLUH(K)*MESH%IFAC%I(IS)
          ELSE
            OUTFLOW  = FLUH(K)
          ENDIF
          IF(LIMPRO(K,1).EQ.KDIR)  FLUSORT = FLUSORT + OUTFLOW
          IF(LIMPRO(K,2).EQ.KDIR)  FLUENT  = FLUENT  + OUTFLOW
!RA
          FLBOR%R(K)=OUTFLOW
!
          CE(IS,1)  = CE(IS,1) - FLUH(K)
          CE(IS,2)  = CE(IS,2) - FLUU(K)
          CE(IS,3)  = CE(IS,3) - FLUV(K)
!
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
