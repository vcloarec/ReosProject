!                   ***********************
                    SUBROUTINE SECOND_ORDER
!                   ***********************
!
     &(DSZ0,BETA,T1,T2,T3,T4,T5,LOGFR,UA,DJX,DJY,DX,DY,DT,CORR_I,
     & CORR_J,CORR_HL,CORR_HR,CORR_UL,CORR_UR,CORR_VL,CORR_VR,CORR_ZL,
     & CORR_ZR,AIRST,VNOIN,ELTSEG,IFABOR,NUBO,CMI,LIMPRO,HC,GPRDTIME,
     & LEO)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Computes the corrections of water depth, velocity, bottom and
!!        slope source term for spatial second order computation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  BETA       EXTRAPOLATION COEFFICIENT FOR ORDRE 2
!>@param  [in,out]  CORR_HL    2ND ORDER CORRECTION FOR WATER DEPTH
!>@param  [in,out]  CORR_HR    2ND ORDER CORRECTION FOR WATER DEPTH
!>@param  [in,out]  CORR_I     2ND ORDER CORRECTION FOR SLOPE TERM
!>@param  [in,out]  CORR_J     2ND ORDER CORRECTION FOR SLOPE TERM
!>@param  [in,out]  CORR_UL    2ND ORDER CORRECTION FOR VELOCITY
!>@param  [in,out]  CORR_UR    2ND ORDER CORRECTION FOR VELOCITY
!>@param  [in,out]  CORR_VL    2ND ORDER CORRECTION FOR VELOCITY
!>@param  [in,out]  CORR_VR    2ND ORDER CORRECTION FOR VELOCITY
!>@param  [in,out]  CORR_ZL    2ND ORDER CORRECTION FOR BOTTOM
!>@param  [in,out]  CORR_ZR    2ND ORDER CORRECTION FOR BOTTOM
!>@param  [in,out]  DJX        GRADIENT PER TRIANGLE
!>@param  [in,out]  DJY        GRADIENT PER TRIANGLE
!>@param  [in,out]  DX         GRADIENTS AT NODES
!>@param  [in,out]  DY         GRADIENTS AT NODES
!>@param  [in,out]  DSZ0       VARIATION OF Z FOR ORDER 2
!>@param  [in]      GPRDTIME   TEST TIME STEP BIGGER THAN GRAPHIC OUTPUT
!>@param  [in,out]  LEO        LOGICAL FOR GRAPHICAL OUTPUT
!>@param  [in,out]  LOGFR      REFERENCE OF BOUNDARY NODES
!>@param  [in,out]  T1         WORKING TABLE
!>@param  [in,out]  T2         WORKING TABLE
!>@param  [in,out]  T3         WORKING TABLE
!>@param  [in,out]  T4         WORKING TABLE
!>@param  [in,out]  T5         WORKING TABLE
!>@param  [in,out]  UA         STATES VARIABLE 1:H 2:HU 3:HV
!>@param  [in,out]  HC         RECONSTRUCTED H FOR TRACER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: DSH_FC,DSU_FC,DSV_FC,LT,NPOIN,
     &                            DSP_FC,DSM_FC,DSZ_FC,CORR_FC,
     &                            DTLL_FC,GRADI_FC,GRADJ_FC,NELEM,NSEG,
     &                            GRADIJ_FC,GRADJI_FC,DEJA_FC,IKLE,
     &                            NPTFR,ILIMHZ,ILIMUV,X,Y,V2DPAR,ZF,
     &                            GRAV,CFLWTD,MESH,EPS_FV,AT,TMAX,PTINIG
      USE DECLARATIONS_TELEMAC, ONLY: KDIR,KNEU
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MIN
      USE INTERFACE_TELEMAC2D, EX_SECOND_ORDER => SECOND_ORDER
      USE BIEF_DEF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IMPLICIT NONE
!
      LOGICAL, INTENT(INOUT) :: LEO
!
      INTEGER, INTENT(IN) :: ELTSEG(NELEM,3),IFABOR(NELEM,3)
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),LIMPRO(NPTFR,6)
      INTEGER, INTENT(INOUT) :: LOGFR(*)
!
      DOUBLE PRECISION, INTENT(IN) :: AIRST(2,NSEG),VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN) :: CMI(2,NSEG),GPRDTIME
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ0(2,NSEG),BETA,UA(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: T1(*),T2(*),T3(*),T4(*),T5(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DT,DX(3,*),DY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: HC(2,*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_HL,CORR_HR,CORR_I
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_UL,CORR_UR,CORR_J
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_VL,CORR_VR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_ZL,CORR_ZR
!
      LOGICAL THEEND
      INTEGER ERR,I,IEL,IS,IVAR,J,K,NSG,NUBO1,NUBO2
      DOUBLE PRECISION AIX,AJX,AIY,AJY,DEMI,DSZ1,DSZ2,DTL,GRADI2
      DOUBLE PRECISION GRADIJ2,GRADJ2,GRADJI2,HI0,HJ0,PROD_SCAL
      DOUBLE PRECISION RA3,RNN,SIGMAX,UAS11,UAS12,UAS21,UAS22
      DOUBLE PRECISION UAS31,UAS32,UAS210,UAS220,UNORM,VNL,VNX
      DOUBLE PRECISION VNY,XNN,YNN,ZF1,ZF2,AMDS,AUX,AUX1,U,V
      DOUBLE PRECISION CORR_TMP,DSH,RESTE
      LOGICAL, ALLOCATABLE ::   YESNO(:)
      DOUBLE PRECISION, ALLOCATABLE :: TMP1(:),TMP2(:)
!
!      DOUBLE PRECISION EXLIM
!      EXTERNAL         EXLIM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DEMI = 0.5D0
!
      IF(LT.EQ.1) THEN
        CALL GRADZ(IKLE%I,NUBO,CMI,MESH%DPX%R,MESH%DPY%R,DSZ0,BETA,
     &             AIRST,T1,T2,T3,T4,T5,ELTSEG,IFABOR,VNOIN)
!        DEFINITION OF A REFERENCE TO DISTINGUISH INTERIOR
!        AND BOUNDARY NODES FOR TRACER ORDRE 2
!
        DO IS=1,NPOIN
          LOGFR(IS)=0
        ENDDO
!
        IF(NPTFR.GT.0)THEN !FOR PARALLLEL CASES
          DO K=1,NPTFR
            IS=MESH%NBOR%I(K)
            IF(LIMPRO(K,2).EQ.KDIR) LOGFR(IS)=1
            IF(LIMPRO(K,1).EQ.KDIR) LOGFR(IS)=3
            IF(LIMPRO(K,1).EQ.KNEU) LOGFR(IS)=2
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL GRADNOD(IKLE%I,UA,MESH%DPX%R,MESH%DPY%R,DJX,DJY,DX,DY)
!
      IF(.NOT.DEJA_FC) THEN
        ALLOCATE(DSH_FC(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSU_FC(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSV_FC(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSP_FC(NPOIN)    ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSM_FC(NPOIN)    ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSZ_FC(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(CORR_FC(NPOIN)   ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DTLL_FC(NPOIN)   ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(GRADI_FC(3,NSEG),GRADJ_FC(3,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(GRADIJ_FC(3,NSEG),GRADJI_FC(3,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        GO TO 1002
1001    CONTINUE
        WRITE(LU,2000) ERR
2000    FORMAT(1X,'SECONDORDER ERROR DURING ALLOCATION OF MEMORY:',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
1002    CONTINUE
        DEJA_FC=.TRUE.
      ENDIF
!
!
      ALLOCATE(YESNO(NSEG),STAT=ERR)
      RA3 = SQRT(1.5D0*GRAV)
      IF(ERR.NE.0)THEN
        WRITE(LU,2000) ERR
        CALL PLANTE(1)
        STOP
      ENDIF
!
! INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
!  2ND ORDER RECONSTRUCTION
!  ************************
!
!    INITIALIZATION
      DTLL_FC(:)=(/(1.D10,I=1,NPOIN)/)
      DSP_FC (:)=(/(0.D0,I=1,NPOIN)/)
      DSM_FC (:)=(/(0.D0,I=1,NPOIN)/)
      DSH_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      DSH_FC(2,:)=(/(0.D0,I=1,NSEG)/)
      DSU_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      DSU_FC(2,:)=(/(0.D0,I=1,NSEG)/)
      DSV_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      DSV_FC(2,:)=(/(0.D0,I=1,NSEG)/)
      DSZ_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      DSZ_FC(2,:)=(/(0.D0,I=1,NSEG)/)
!GIVES ERROR WITH INTEL COMPILER IF WRITTEN LIKE THIS
!       CALL OV( 'X=C     ' ,X=DSH_FC(1,1:NSEG), C=0.D0, DIM1=NSEG)
!
!    INITIALIZATION  OF GRADIENTS
      GRADI_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADI_FC(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADI_FC(3,:)=(/(0.D0,I=1,NSEG)/)
!
      GRADJ_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADJ_FC(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADJ_FC(3,:)=(/(0.D0,I=1,NSEG)/)
!
      GRADIJ_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADIJ_FC(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADIJ_FC(3,:)=(/(0.D0,I=1,NSEG)/)
!
      GRADJI_FC(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADJI_FC(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADJI_FC(3,:)=(/(0.D0,I=1,NSEG)/)
!
      DO IEL=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!     RECUPERATE JMI
            J   = MESH%JMI%I(NSG) ! THIS THE TRIANGLE IN WHICH IS LOCATED CMI
            IF(NCSIZE.GT.1.AND.J.EQ.0)  THEN
! THAT MEANS CMI IS NOT LOCATED IN TRIANGLE J
              YESNO(NSG) = .TRUE.
              CYCLE
            ENDIF
!
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
!
            DSZ_FC(1,NSG) = DSZ0(1,NSG)
            DSZ_FC(2,NSG) = DSZ0(2,NSG)
!
            ZF1        = ZF%R(NUBO1)
            ZF2        = ZF%R(NUBO2)
!
            HI0=UA(1,NUBO1)
            HJ0=UA(1,NUBO2)
!
!   FOR AN EDGE BEING RECOVERED, ONE WILL REMAIN 1ST ORDER
!
            IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &     .OR. 2.*ABS(DSZ_FC(1,NSG)).GE.HI0
     &     .OR. 2.*ABS(DSZ_FC(1,NSG)).GE.HJ0
     &     .OR. 2.*ABS(DSZ_FC(2,NSG)).GE.HI0
     &     .OR. 2.*ABS(DSZ_FC(2,NSG)).GE.HJ0)  THEN
!ra02/05/2013 FOR OPTIMIZATION
              YESNO(NSG)=.TRUE.
              CYCLE

            ELSE
!
!     NORMALIZED UNIT NORMAL (VNOIN), RNN LENGTH OF LIJ
!
              XNN = VNOIN(1,NSG)
              YNN = VNOIN(2,NSG)
              RNN = VNOIN(3,NSG)
!
              AIX = CMI(1,NSG)-X(NUBO1) ! THESE ARE COORDINATES OF
              AIY = CMI(2,NSG)-Y(NUBO1) !  VECTOR PM (EQ 5.1)
              AJX = CMI(1,NSG)-X(NUBO2) ! P: NUBO1 OR NUBO2
              AJY = CMI(2,NSG)-Y(NUBO2) ! M: CMI(NSG)
!
              DO IVAR = 1,3
                GRADI_FC(IVAR,NSG) =
     &                 AIX*DX(IVAR,NUBO1)+AIY*DY(IVAR,NUBO1)!NODE GRADIENT (PM.GRADZ)
                GRADJ_FC(IVAR,NSG) =
     &                 AJX*DX(IVAR,NUBO2)+AJY*DY(IVAR,NUBO2)!eq 5.1 of audusse paper)
!
                GRADIJ_FC(IVAR,NSG) = AIX*DJX(IVAR,J) + AIY*DJY(IVAR,J)
                GRADJI_FC(IVAR,NSG) = AJX*DJX(IVAR,J) + AJY*DJY(IVAR,J)
              ENDDO
! ROTATION OF THE GRADIENTS
!
              GRADI2       = GRADI_FC(2,NSG)
              GRADI_FC(2,NSG) = XNN*GRADI2+YNN*GRADI_FC(3,NSG)
              GRADI_FC(3,NSG) =-YNN*GRADI2+XNN*GRADI_FC(3,NSG)
!
              GRADIJ2      = GRADIJ_FC(2,NSG)
              GRADIJ_FC(2,NSG)= XNN*GRADIJ2+YNN*GRADIJ_FC(3,NSG)
              GRADIJ_FC(3,NSG)=-YNN*GRADIJ2+XNN*GRADIJ_FC(3,NSG)
!
              GRADJ2       = GRADJ_FC(2,NSG)
              GRADJ_FC(2,NSG) = XNN*GRADJ2+YNN*GRADJ_FC(3,NSG)
              GRADJ_FC(3,NSG) =-YNN*GRADJ2+XNN*GRADJ_FC(3,NSG)
!
              GRADJI2      = GRADJI_FC(2,NSG)
              GRADJI_FC(2,NSG)= XNN*GRADJI2+YNN*GRADJI_FC(3,NSG)
              GRADJI_FC(3,NSG)=-YNN*GRADJI2+XNN*GRADJI_FC(3,NSG)
            ENDIF
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1)THEN      ! PARCOM USEFUL WHEN JMI NOT AFFECTED
        DO IVAR = 1,3
          ALLOCATE(TMP1(NSEG))
          ALLOCATE(TMP2(NSEG))
!
          TMP1 = GRADI_FC(IVAR,1:NSEG)
          TMP2 = GRADJ_FC(IVAR,1:NSEG)
          CALL PARCOM2_SEG(TMP1,TMP2,TMP2,
     &                NSEG,1,1,2,MESH,1,11)
          GRADI_FC(IVAR,1:NSEG) = TMP1
          GRADJ_FC(IVAR,1:NSEG) = TMP2
!
          TMP1 = GRADIJ_FC(IVAR,1:NSEG)
          TMP2 = GRADJI_FC(IVAR,1:NSEG)
          CALL PARCOM2_SEG(TMP1,TMP2,TMP2,
     &                NSEG,1,1,2,MESH,1,11)
          GRADIJ_FC(IVAR,1:NSEG) = TMP1
          GRADJI_FC(IVAR,1:NSEG) = TMP2
!
          DEALLOCATE(TMP1)
          DEALLOCATE(TMP2)
        ENDDO
      ENDIF
!
!
! INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
      DO IEL=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            DSZ_FC(1,NSG) = DSZ0(1,NSG)
            DSZ_FC(2,NSG) = DSZ0(2,NSG)

            ZF1        = ZF%R(NUBO1)
            ZF2        = ZF%R(NUBO2)
!
            HI0=UA(1,NUBO1)
            HJ0=UA(1,NUBO2)
!
!   FOR AN EDGE BEING RECOVERED, ONE WILL REMAIN 1ST ORDER
!
            IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &     .OR. 2.*ABS(DSZ_FC(1,NSG)).GE.HI0
     &     .OR. 2.*ABS(DSZ_FC(1,NSG)).GE.HJ0
     &     .OR. 2.*ABS(DSZ_FC(2,NSG)).GE.HI0
     &     .OR. 2.*ABS(DSZ_FC(2,NSG)).GE.HJ0)  THEN
!ra02/05/2013 FOR APTIMIZATION
              YESNO(NSG)=.TRUE.
              CYCLE
            ELSE

              BETA=1.D0
!
              DSH_FC(1,NSG) =
     &          EXLIM(ILIMHZ,BETA,GRADI_FC(1,NSG),GRADIJ_FC(1,NSG))
              DSH_FC(2,NSG) =
     &          EXLIM(ILIMHZ,BETA,GRADJ_FC(1,NSG),GRADJI_FC(1,NSG))
!
              !   FOR PARALLELILSM
              IF(NCSIZE.GT.1.AND.IFABOR(IEL,I).EQ.-2)THEN ! THIS IS AN INTERFACE EDGE
                IF(DSH_FC(1,NSG).GE.0.D0) THEN
                  DSP_FC(NUBO1) =
     &            DSP_FC(NUBO1)+DEMI*AIRST(1,NSG)*DSH_FC(1,NSG) ! WE CONSIDER ONLY
                ELSE                                                  ! 0.5 AIRST
                  DSM_FC(NUBO1) = DSM_FC(NUBO1)
     &                          -DEMI*AIRST(1,NSG)*DSH_FC(1,NSG) ! PARCOM2 WILL ADD
                ENDIF                                                 ! CONTRIBUTIONS
                IF(DSH_FC(2,NSG).GE.0.D0) THEN
                  DSP_FC(NUBO2) =
     &              DSP_FC(NUBO2)+DEMI*AIRST(2,NSG)*DSH_FC(2,NSG)
                ELSE
                  DSM_FC(NUBO2) =
     &              DSM_FC(NUBO2)-DEMI*AIRST(2,NSG)*DSH_FC(2,NSG)
                ENDIF
              ELSE ! NO PARALLELILSM OR NO INTERFACE EDGE
                IF(DSH_FC(1,NSG).GE.0.D0) THEN
                  DSP_FC(NUBO1) = DSP_FC(NUBO1)
     &                + AIRST(1,NSG)*DSH_FC(1,NSG)
                ELSE
                  DSM_FC(NUBO1) = DSM_FC(NUBO1)
     &                - AIRST(1,NSG)*DSH_FC(1,NSG)
                ENDIF
                IF(DSH_FC(2,NSG).GE.0.D0) THEN
                  DSP_FC(NUBO2) = DSP_FC(NUBO2)
     &                + AIRST(2,NSG)*DSH_FC(2,NSG)
                ELSE
                  DSM_FC(NUBO2) = DSM_FC(NUBO2)
     &                - AIRST(2,NSG)*DSH_FC(2,NSG)
                ENDIF
              ENDIF
!
              BETA=0.3333D0 ! THESE ARE CHOICES OF INRIA 1/3 FOR
                            ! VELOCITIES AND 1 FOR H
!
              DSU_FC(1,NSG) =
     &          EXLIM(ILIMUV,BETA,GRADI_FC(2,NSG),GRADIJ_FC(2,NSG))
              DSU_FC(2,NSG) =
     &          EXLIM(ILIMUV,BETA,GRADJ_FC(2,NSG),GRADJI_FC(2,NSG))
!
              DSV_FC(1,NSG) =
     &          EXLIM(ILIMUV,BETA,GRADI_FC(3,NSG),GRADIJ_FC(3,NSG))
              DSV_FC(2,NSG) =
     &          EXLIM(ILIMUV,BETA,GRADJ_FC(3,NSG),GRADJI_FC(3,NSG))
!
            ENDIF
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
!
      !  FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(DSP_FC,DSM_FC,DSM_FC,NPOIN,1,2,2,MESH)
      ENDIF
!
!  ONE CALCULATES THE CORRECTIONS TO ENSURE THE CONSERVATION OF H
!
      DO IS=1,NPOIN
        CORR_FC(IS) =  DSM_FC(IS) - DSP_FC(IS)
        AMDS =MAX(DSP_FC(IS),DSM_FC(IS))
        IF(AMDS.GT.EPS_FV) THEN
          CORR_FC(IS) = CORR_FC(IS)/AMDS
        ENDIF
      ENDDO
! IF ORDER 2 REINITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO

      DO IEL=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
!
            DSH_FC(1,NSG) = DSH_FC(1,NSG) +
     &        MIN(0.D0,CORR_FC(NUBO1))*MAX(0.D0,DSH_FC(1,NSG))+
     &        MAX(0.D0,CORR_FC(NUBO1))*MAX(0.D0,-DSH_FC(1,NSG))
!
            DSH_FC(2,NSG) = DSH_FC(2,NSG) +
     &        MIN(0.D0,CORR_FC(NUBO2))*MAX(0.D0,DSH_FC(2,NSG))+
     &        MAX(0.D0,CORR_FC(NUBO2))*MAX(0.D0,-DSH_FC(2,NSG))
!
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOIN(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOIN(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
            IF(PROD_SCAL.LT.0.D0)THEN
              DSZ_FC(1,NSG) = DSZ0(2,NSG)
              DSZ_FC(2,NSG) = DSZ0(1,NSG)
              DSH = DSH_FC(1,NSG)
              DSH_FC(1,NSG) = DSH_FC(2,NSG)
              DSH_FC(2,NSG) = DSH
              DSH = DSU_FC(1,NSG)
              DSU_FC(1,NSG) = DSU_FC(2,NSG)
              DSU_FC(2,NSG) = DSH
              DSH = DSV_FC(1,NSG)
              DSV_FC(1,NSG) = DSV_FC(2,NSG)
              DSV_FC(2,NSG) = DSH
            ELSE
              DSZ_FC(1,NSG) = DSZ0(1,NSG)
              DSZ_FC(2,NSG) = DSZ0(2,NSG)
            ENDIF
!
            ZF1  = ZF%R(NUBO1)
            ZF2  = ZF%R(NUBO2)
            DSZ1 = 0.D0
            DSZ2 = 0.D0
!
            XNN  = VNOIN(1,NSG)
            YNN  = VNOIN(2,NSG)
            RNN  = VNOIN(3,NSG)
!
            UAS11 = UA(1,NUBO1)
            UAS12 = UA(1,NUBO2)
            IF (UAS11.GE.EPS_FV) THEN
              UAS21 = UA(2,NUBO1)/UAS11
              UAS31 = UA(3,NUBO1)/UAS11
            ELSE
              UAS21 = 0.D0
              UAS31 = 0.D0
            ENDIF
            IF (UAS12.GE.EPS_FV) THEN
              UAS22 = UA(2,NUBO2)/UAS12
              UAS32 = UA(3,NUBO2)/UAS12
            ELSE
              UAS22 = 0.D0
              UAS32 = 0.D0
            ENDIF
!
            HI0 = UAS11
            HJ0 = UAS12
!
            HC(1,NSG) = UAS11
            HC(2,NSG) = UAS12
!
! ROTATION
!
            UAS210 = UAS21
            UAS21  = XNN*UAS210+YNN*UAS31
            UAS31  =-YNN*UAS210+XNN*UAS31
!
            UAS220 = UAS22
            UAS22  = XNN*UAS220+YNN*UAS32
            UAS32  =-YNN*UAS220+XNN*UAS32
!
!    REBUILDING FOR 2ND ORDER
!    ***************************
!
!   FOR AN EDGE BEING RECOVERED, ONE REMAINS 1ST ORDER
!
            IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &     .OR. 2.*ABS(DSZ_FC(1,NSG)).GE.HI0
     &     .OR. 2.*ABS(DSZ_FC(1,NSG)).GE.HJ0
     &     .OR. 2.*ABS(DSZ_FC(2,NSG)).GE.HI0
     &     .OR. 2.*ABS(DSZ_FC(2,NSG)).GE.HJ0)  GOTO 1234
!
!
            CORR_HL%R(NSG) = DSH_FC(1,NSG) - DSZ_FC(1,NSG)
            UAS11 = UAS11 + CORR_HL%R(NSG)
!
            CORR_UL%R(NSG) = DSU_FC(1,NSG)
            UAS21 = UAS21 + CORR_UL%R(NSG)
            CORR_VL%R(NSG) = DSV_FC(1,NSG)
            UAS31 = UAS31 + CORR_VL%R(NSG)
            DSZ1   = DSZ_FC(1,NSG)
            CORR_ZL%R(NSG) = DSZ1
!
            CORR_HR%R(NSG) = DSH_FC(2,NSG) - DSZ_FC(2,NSG)
            UAS12 = UAS12 + CORR_HR%R(NSG)
!
            CORR_UR%R(NSG) = DSU_FC(2,NSG)
            UAS22 = UAS22 + CORR_UR%R(NSG)
            CORR_VR%R(NSG) = DSV_FC(2,NSG)
            UAS32 = UAS32 + CORR_VR%R(NSG)
            DSZ2   = DSZ_FC(2,NSG)
            CORR_ZR%R(NSG) = DSZ2
!
!    INVERSE ROTATION
!
            CORR_TMP = CORR_UL%R(NSG)
            CORR_UL%R(NSG) = XNN*CORR_TMP - YNN*CORR_VL%R(NSG)
            CORR_VL%R(NSG) = YNN*CORR_TMP + XNN*CORR_VL%R(NSG)
!
            CORR_TMP = CORR_UR%R(NSG)
            CORR_UR%R(NSG) = XNN*CORR_TMP - YNN*CORR_VR%R(NSG)
            CORR_VR%R(NSG) = YNN*CORR_TMP + XNN*CORR_VR%R(NSG)
!
!
            IF(UAS11.LE.0.D0) THEN
              UAS11 = 0.D0
              UAS21 = 0.D0
              UAS31 = 0.D0
            ENDIF
            IF(UAS12.LE.0.D0)  THEN
              UAS12 = 0.D0
              UAS22 = 0.D0
              UAS32 = 0.D0
            ENDIF
!
!
!    LIMITATION OF THE TIME STEP
!    ***************************
!
!
            SIGMAX=MAX( 1.D-2, RA3* SQRT(UAS11) + ABS(UAS21) )
            DTL    = CFLWTD*AIRST(1,NSG)/(RNN*SIGMAX)
            DTLL_FC(NUBO1) = MIN (DTL,DTLL_FC(NUBO1))
            DT          = MIN(DT, DTL)
!
            SIGMAX=MAX( 1.D-2, RA3* SQRT(UAS12) + ABS(UAS22) )
            DTL    = CFLWTD*AIRST(2,NSG)/(RNN*SIGMAX)
            DTLL_FC(NUBO2) = MIN (DTL,DTLL_FC(NUBO2))
            DT          = MIN(DT, DTL)
! PARALLEL: TAKE MIN DT OF ALL SUBDOMAINS
!          IF(NCSIZE.GT.1)DT = P_MIN(DT) !WILL BE PLACED AT THE END (SEE BELOW)
!
!
1234        CONTINUE
!
            CORR_I%R(NSG)= - DEMI*RNN*(HI0+UAS11)*DSZ1
            CORR_J%R(NSG)= - DEMI*RNN*(HJ0+UAS12)*DSZ2
            YESNO(NSG)=.TRUE.
!
          ENDIF
        ENDDO
      ENDDO

      IF(NPTFR.GT.0)THEN  ! USEFUL FOR PARALLEL CASE
        DO K=1,NPTFR
          IS = MESH%NBOR%I(K)
          VNX= MESH%XNEBOR%R(K+NPTFR)
          VNY= MESH%YNEBOR%R(K+NPTFR)
          VNL= SQRT(VNX**2+VNY**2)
          IF(UA(1,IS).GE.EPS_FV) THEN
            U = UA(2,IS)/UA(1,IS)
            V = UA(3,IS)/UA(1,IS)
          ELSE
            U = 0.D0
            V = 0.D0
          ENDIF
          SIGMAX= SQRT(UA(1,IS))
          UNORM=SQRT(U*U + V*V)
          SIGMAX=MAX( 1.D-2, RA3*SIGMAX +UNORM )
          DTL   = CFLWTD*V2DPAR%R(IS)/(VNL*SIGMAX)
          AUX   = DTL/DTLL_FC(IS)
          AUX1  =AUX/(1.D0+AUX)
          DT    =MIN(DT, AUX1*DTLL_FC(IS))
        ENDDO
      ENDIF

!     FOR PARALLELISME
      IF(NCSIZE.GT.1) DT=P_MIN(DT)
!
      DEALLOCATE(YESNO)
!
      THEEND =.FALSE.
      LEO    =.FALSE.
      IF((TMAX-AT).LE.DT)THEEND=.TRUE. !LAST TIME STEP
!     ADAPT DT TO TAKE INTO ACCOUNT GRAPHIC OUTPUT
      IS=CEILING(AT/GPRDTIME)
      RESTE=IS*GPRDTIME-AT
!
      IF(THEEND.OR.
     &  (RESTE.LE.DT.AND.
     &   RESTE.GT.EPSILON(RESTE).AND.LT.GT.PTINIG))THEN
!       HERE THERE IS GRAPHICAL OUTPUT
        LEO = .TRUE.
        DT=MIN(RESTE,DT)
      ENDIF
!
      DT = MIN(DT,TMAX-AT)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
