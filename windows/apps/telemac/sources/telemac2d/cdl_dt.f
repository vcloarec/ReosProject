!                       *****************
                        SUBROUTINE CDL_DT
!                       *****************
     &(DT,LIMPRO)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Limitation of time step due to imposed boundary values
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  DT         TIME STEP
!>@param  [in]      LIMPRO     TYPES OF BOUNDARY CONDITION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_CDL_DT => CDL_DT
      USE DECLARATIONS_TELEMAC2D, ONLY: ICIN,BNDCIN,GRAV,NPTFR,MESH,
     &                            EPS_FV,HN,QU,QV,HBOR,UBOR,VBOR,CFLWTD
      USE DECLARATIONS_TELEMAC, ONLY: KDIR,KNEU,KENT
      USE INTERFACE_PARALLEL, ONLY:P_MIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(INOUT) :: DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NIT
      DOUBLE PRECISION SQ2,RA3,RA32,ALP,ALP2,SG,A,A1,A2,ALPHA0,ALPHA1
      DOUBLE PRECISION AM,C,CA1,DEST,DTL,FHPLUS,HG,HUIN,HVIN,RH,RHG,RVG
      DOUBLE PRECISION SIGMAX,U,UG,UIN,UNN,UNORM,V,VG,VIN,VNN,VNX1,VNY1
      DOUBLE PRECISION VP1,VP2,VP3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      AM = 0.D0
!
      IF((ICIN.EQ.1).AND.(BNDCIN.EQ.1)) THEN
        SIGMAX= 1.D-2
        SQ2   = SQRT(2.D0)
        RA3   = SQRT(1.5D0*GRAV)
        RA32  = RA3*RA3
        ALP   = 0.5D0/RA3
        ALP2  = 0.5D0 *ALP
        SG    = SQRT(GRAV)
        IF(NPTFR.GT.0)THEN ! USEFUL FOR PARALLEL
          DO K=1,NPTFR
            IS=MESH%NBOR%I(K)
            VNX1=MESH%XNEBOR%R(K)
            VNY1=MESH%YNEBOR%R(K)
            IF(HN%R(IS).GT.EPS_FV) THEN
              RH = SQRT(HN%R(IS))
              U = QU%R(IS)/HN%R(IS)
              V = QV%R(IS)/HN%R(IS)
            ELSE
              RH = 0.D0
              U = 0.D0
              V = 0.D0
            ENDIF
            IF(LIMPRO(K,1).NE.KNEU) THEN
              IF(HN%R(IS).LT.EPS_FV) THEN
                UNN = 0.D0
                VNN=0.D0
                FHPLUS = 0.D0
              ELSE
                UNN = +VNX1*U+VNY1*V
                VNN= -VNY1*U+VNX1*V
                A=MIN(RA3,MAX(-RA3,-UNN/RH))
                A2 =A * A
                ALPHA0=ALP*(RA3-A)
                ALPHA1=ALP2*(RA32-A2)
                FHPLUS = HN%R(IS)*UNN*ALPHA0 + HN%R(IS)*RH*ALPHA1
              ENDIF
              IF(LIMPRO(K,1).EQ.KDIR) THEN
                HG  = HBOR%R(K)
                C   = SQRT(GRAV)*RH
                VP1 = UNN
                VP2 = VP1  + C
                VP3 = VP1  - C
                RHG = SQRT(HG)
                IF (VP2*VP3.LE.0.D0.OR. VP1.LE.0.D0) THEN
                  IF(HG.EQ.0.D0) THEN
                    SIGMAX = 1.D-2
                  ELSE
                    IF (VP2*VP3.LE.0.D0) THEN
                      UG = UNN + 2.D0*SG*(RH-RHG)
                      VG = VNN
                    ELSE
                      IF(LIMPRO(K,2).EQ.KDIR) THEN
!                       IMPOSED INFLOW
                        UIN = UBOR%R(K)
                        VIN = VBOR%R(K)
                        HUIN = HN%R(IS)*UIN
                        HVIN = HN%R(IS)*VIN
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
                      ELSE
                        UG = 0.D0
                        VG = 0.D0
                      ENDIF
                    ENDIF
                    UNORM=SQRT(UG *UG + VG*VG)
                    SIGMAX=MAX( 1.D-2, RA3 *RHG +UNORM )
                  ENDIF
                ENDIF
              ELSE IF(LIMPRO(K,2).EQ.KDIR) THEN
                HG  = HBOR%R(K)
                UIN = UBOR%R(K)
                VIN = VBOR%R(K)
                HUIN = HN%R(IS)*UIN
                HVIN = HN%R(IS)*VIN
                DEST=HUIN*VNX1+HVIN*VNY1
                RVG =-HUIN*VNY1+HVIN*VNX1
                A1 = -DEST+FHPLUS
                IF (A1.LE.0.D0) THEN
                  SIGMAX = 1.D-2
                ELSE
                  RHG = A2/(SG*(AM-2.D0))
                  IF(HG.EQ.0.D0) THEN
                    SIGMAX = 1.D-2
                  ELSE
                    CA1= 1.D0/(GRAV*SQ2*A1)**(1.D0/3.D0)
                    CALL ZEROPSI(-0.5D0,AM,NIT,CA1,A2)
                    RHG = A2/(SG*(AM-2.D0))
                    UG = -AM*A2/(AM-2.D0)
                    VG = RVG/HG
                    UNORM = SQRT(UG *UG + VG*VG)
                    SIGMAX = MAX( 1.D-2, RA3 *RHG +UNORM )
                  ENDIF
                ENDIF
              ENDIF
              DTL = CFLWTD*MESH%DTHAUT%R(IS)/SIGMAX
              DT  = MIN(DT, DTL)
            ENDIF
          ENDDO
        ENDIF
!
        IF(NCSIZE.GT.1)DT=P_MIN(DT)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
