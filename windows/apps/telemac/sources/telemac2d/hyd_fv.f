!                       *****************
                        SUBROUTINE HYD_FV
!                       *****************

     &(NUBO,W,VNOCL,ELTSEG,CE,IFABOR,FLUHTEMP,NEISEG)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    Loop on segment to computes interfaces fluxes
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CE         FLUXES AT THE NODE
!>@param  [in]      ELTSEG     SEGMENT NUMBERS PER ELEMENT
!>@param  [in,out]  FLUHTEMP   FLUX FOR TRACER
!>@param  [in]      IFABOR     ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!>@param  [in]      NEISEG     NEIGHBORS OF SEGMENT (FOR LIMITER)
!>@param  [in]      NUBO       NUMBER OF EDGES
!>@param  [in]      VNOCL      NORMAL TO THE INTERFACE
!>@param  [in]      W          WORKING TABLE CONTAINING H,HU,HV
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE INTERFACE_TELEMAC2D, EX_HYD_FV => HYD_FV
      USE DECLARATIONS_TELEMAC2D, ONLY: NPOIN,NSEG,NELEM,HROPT,NTRAC,
     &                            ICIN,ZF,X,Y,GRAV,MESH,EPS_FV,
     &                            CORR_I,CORR_J,CORR_ZL,CORR_ZR,CORR_UL,
     &                            CORR_UR,CORR_VL,CORR_VR,CORR_HL,
     &                            CORR_HR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG),NEISEG(2,NSEG)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)    :: VNOCL(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NPOIN,3)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUHTEMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSG,NUBO1,NUBO2,IDRY,I,IEL,SEG1,SEG2,ITRAC
      INTEGER NUBOL,NUBOR
!
      DOUBLE PRECISION ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION V21,V22,V31,V32,SL1,SL2
      DOUBLE PRECISION HIJ,PROD_SCAL,DSINT
      DOUBLE PRECISION HJI,DZIJ,DZJI,ZINT,DZINT
      DOUBLE PRECISION HGZI,HGZJ,HDXZ1,HDYZ1,HDXZ2,HDYZ2
!
      DOUBLE PRECISION H1,H2,FLXG(3),FLXD(3),DEMI
      DOUBLE PRECISION DX,HGZ1,HGZ2,HL_UP,HR_UP,VL_UP,VR_UP
      LOGICAL   YESNO(NSEG)
!
!-----------------------------------------------------------------------
!
      DEMI=0.5D0
!
!-----------------------------------------------------------------------
!
!     INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP OVER GLOBAL LIST OF EDGES
!
      DO IEL=1,NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!           INDICATOR FOR DRY CELLS
            IDRY=0
!
!           INITIALIZATION
            FLXG(1) = 0.D0
            FLXG(2) = 0.D0
            FLXG(3) = 0.D0
            FLXD(1) = 0.D0
            FLXD(2) = 0.D0
            FLXD(3) = 0.D0
!
!           RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!           WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
!
!           THEIR BATHYMETRIES
            ZF1 = ZF%R(NUBO1)
            ZF2 = ZF%R(NUBO2)
!
!           NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
            XNN = VNOCL(1,NSG)
            YNN = VNOCL(2,NSG)
            RNN = VNOCL(3,NSG)
!
!           WATER DEPTH
            H1=W(1,NUBO1)
            H2=W(1,NUBO2)
!
!           VELOCITY COMPONENTS
            IF(H1.GT.EPS_FV)THEN
              V21 = W(2,NUBO1)/H1
              V31 = W(3,NUBO1)/H1
            ELSE
              V21=0.D0
              V31=0.D0
              IDRY=IDRY+1
            ENDIF
!
            IF(H2.GT.EPS_FV)THEN
              V22 = W(2,NUBO2)/H2
              V32 = W(3,NUBO2)/H2
            ELSE
              V22=0.0D0
              V32=0.0D0
              IDRY=IDRY+1
            ENDIF
!
!           SECOND ORDER RECONSTRUCTION
            H1 = H1 + CORR_HL%R(NSG)
            H2 = H2 + CORR_HR%R(NSG)
            V21 = V21 + CORR_UL%R(NSG)
            V22 = V22 + CORR_UR%R(NSG)
            V31 = V31 + CORR_VL%R(NSG)
            V32 = V32 + CORR_VR%R(NSG)
            ZF1 = ZF1 + CORR_ZL%R(NSG)
            ZF2 = ZF2 + CORR_ZR%R(NSG)
!
            IF(H1.LE.0.D0) THEN
              H1 = 0.D0
              V21 = 0.D0
              V31 = 0.D0
            ENDIF
            IF(H2.LE.0.D0) THEN
              H2 = 0.D0
              V22 = 0.D0
              V32 = 0.D0
            ENDIF
!
            IF(ICIN.EQ.5) THEN
!
              DX    = 0.5D0*(MESH%DTHAUT%R(NUBO1)+MESH%DTHAUT%R(NUBO2))
!
!             SEGMENT NEIGHBORS (FOR LIMITER)
              SEG1 = NEISEG(1,NSG)
              SEG2 = NEISEG(2,NSG)
!
!             VERIFY THAT WE HAVE THE GOOD NEIGHBORS
              IF((SEG1.LE.0.OR.SEG2.LE.0).AND.NCSIZE.LE.1)THEN
                WRITE(LU,*)'PROBLEM TO FIND SEGMENT NEIGHBORS'
                WRITE(LU,*)'WE ARE IN HYD_FV.F'
                WRITE(LU,*)'SEGMENT OF INTEREST  :',NSG
                WRITE(LU,*)'NEIGHBORS ARE  :',SEG1,SEG2
                CALL PLANTE(1)
                STOP
              ENDIF
!
              NUBOL=0
              NUBOR=0
!
              IF(NUBO(1,SEG1).EQ.NUBO1) THEN
                NUBOL = NUBO(2,SEG1)
              ELSE
                NUBOL = NUBO(1,SEG1)
              ENDIF
              IF(NUBO(1,SEG2).EQ.NUBO2) THEN
                NUBOR = NUBO(2,SEG2)
              ELSE
                NUBOR = NUBO(1,SEG2)
              ENDIF
!
!             VERIFY THAT WE HAVE THE GOOD NEIGHBORS
              IF(NUBOL.LE.0.OR.NUBOR.LE.0) THEN
                WRITE(LU,*)'PROBLEM TO FIND NEIGHBOR'
                WRITE(LU,*)'WE ARE IN HYD_FV.F'
                WRITE(LU,*)'NODES ARE  :',NUBO1,NUBO2
                WRITE(LU,*)'NEIGHBORS ARE  :', NUBOR,NUBOL
                CALL PLANTE(1)
                STOP
              ENDIF
!
!             WATER DEPTH, VELOCITY, TRACER OF NEIGHBORS
              HL_UP   = W(1,NUBOL)
              HR_UP   = W(1,NUBOR)
              IF(HL_UP.GT.EPS_FV)THEN
                VL_UP =  W(3,NUBOL)/HL_UP
              ELSE
                VL_UP = 0.0D0
              ENDIF
              IF(HR_UP.GT.EPS_FV)THEN
                VR_UP = W(3,NUBOR)/HR_UP
              ELSE
                VR_UP = 0.0D0
              ENDIF
!
            ELSE
!
              HL_UP = 0.D0
              HR_UP = 0.D0
              VL_UP = 0.D0
              VR_UP = 0.D0
!
            ENDIF
!
!           **********************
!           LOCAL FLUX COMPUTATION
!           AT LEAST ONE WET CELL
!           **********************
            IF(IDRY.LT.2)THEN
!
!             HYDROSTATIC RECONSTRUCTION OF AUDUSSE ET AL.
              IF(HROPT.EQ.1)THEN
                DZIJ = MAX(0.D0,ZF2-ZF1 )
                HIJ  = MAX(0.D0,H1 -DZIJ)
!
                DZJI = MAX(0.D0,ZF1-ZF2 )
                HJI  = MAX(0.D0,H2 -DZJI)
              ELSEIF(HROPT.EQ.2)THEN
!             HYDROSTATIC RECONSTRUCTION OF CHEN AND NOELLE
                SL1   = H1+ZF1
                SL2   = H2+ZF2
                DZINT = MAX(ZF2,ZF1)
                DSINT = MIN(SL2,SL1)
                ZINT  = MIN(DZINT,DSINT)
!
                HIJ   = MIN(SL1-ZINT,H1)
                HJI   = MIN(SL2-ZINT,H2)
              ELSE
                WRITE(LU,*) 'HYD_FV: OPTION OF HYDROSTATIC RECONS-'
                WRITE(LU,*) '          TRUCTION NOT IMPLEMENTED YET'
                CALL PLANTE(1)
                STOP
              ENDIF
!
              CALL FLUX_CHOICE(H1,H2,HIJ,HJI,V21,V22,V31,V32,
     &                         ZF1,ZF2,XNN,YNN,FLXG,FLXD,HL_UP,
     &                         HR_UP,VL_UP,VR_UP,DX)
!
!             *************************************************
!             GEOMETRIC SOURCE TERMS:HYDROSTATIC RECONSTRUCTION
!             *************************************************
!
              IF(ICIN.NE.2.AND.ICIN.NE.3.AND.ICIN.NE.0) THEN
!
!               HYDROSTATIC RECONSTRUCTION OF AUDUSSE ET AL.
                IF(HROPT.EQ.1)THEN
                  HGZ1 = 0.D0
                  HGZ2 = 0.D0
                  HGZI = 0.5D0*RNN*(HIJ+H1)*(HIJ-H1)+CORR_I%R(NSG)
                  HGZJ = 0.5D0*RNN*(HJI+H2)*(HJI-H2)+CORR_J%R(NSG)
                  HDXZ1 = GRAV*XNN*HGZI
                  HDYZ1 = GRAV*YNN*HGZI
                  HDXZ2 = GRAV*XNN*HGZJ
                  HDYZ2 = GRAV*YNN*HGZJ
!               HYDROSTATIC RECONSTRUCTION OF CHEN AND NOELLE
                ELSEIF(HROPT.EQ.2)THEN
                  HGZ1 = 0.D0
                  HGZ2 = 0.D0
                  HGZI = -0.5D0*RNN*(HIJ+H1)*(ZINT-ZF1)+CORR_I%R(NSG)
                  HGZJ =  0.5D0*RNN*(HJI+H2)*(ZF2-ZINT)+CORR_J%R(NSG)
                  HDXZ1 = GRAV*XNN*HGZI
                  HDYZ1 = GRAV*YNN*HGZI
                  HDXZ2 = GRAV*XNN*HGZJ
                  HDYZ2 = GRAV*YNN*HGZJ
                ENDIF
              ELSEIF(ICIN.EQ.0) THEN
                CALL FLUSRC(NUBO1,NUBO2,NSG,VNOCL,W,HGZ1,HGZ2,
     &                         HDXZ1,HDYZ1,HDXZ2,HDYZ2,EPS_FV)
                HGZ1 = -HGZ1
                HGZ2 = HGZ2
                HDXZ1 =-HDXZ1
                HDYZ1 =-HDYZ1
                HDXZ2 =HDXZ2
                HDYZ2 =HDYZ2
              ELSE
                HGZ1 = 0.D0
                HGZ2 = 0.D0
                HDXZ1 = 0.D0
                HDYZ1 = 0.D0
                HDXZ2 = 0.D0
                HDYZ2 = 0.D0
              ENDIF
!
!
!             FOR PARALLELISM
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
!               DEMI=DEMI*SIGN(1.0D0,PROD_SCAL)
                  FLXG(1)= DEMI*FLXG(1)
                  FLXG(2)= DEMI*FLXG(2)
                  FLXG(3)= DEMI*FLXG(3)
                  FLXD(1)= DEMI*FLXD(1)
                  FLXD(2)= DEMI*FLXD(2)
                  FLXD(3)= DEMI*FLXD(3)
!                 FLX(4)= DEMI*FLX(4)
                  HGZ1 = DEMI*HGZ1
                  HGZ2 = DEMI*HGZ2
                  HDXZ1 = DEMI*HDXZ1
                  HDYZ1 = DEMI*HDYZ1
                  HDXZ2 = DEMI*HDXZ2
                  HDYZ2 = DEMI*HDYZ2
                ENDIF
              ENDIF
!
!             ************************
!             TRACERS FLUX COMPUTATION
!             ************************
              IF(NTRAC.GT.0) THEN
                DO ITRAC=1,NTRAC
                  FLUHTEMP%ADR(ITRAC)%P%R(NSG)=RNN*FLXG(1)
                ENDDO
              ENDIF
!
!             **************
!             FLUX INCREMENT
!             **************
              CE(NUBO1,1) = CE(NUBO1,1) - RNN*FLXG(1) + HGZ1
              CE(NUBO1,2) = CE(NUBO1,2) - RNN*FLXG(2) + HDXZ1
              CE(NUBO1,3) = CE(NUBO1,3) - RNN*FLXG(3) + HDYZ1
!
              CE(NUBO2,1) = CE(NUBO2,1) + RNN*FLXD(1) - HGZ2
              CE(NUBO2,2) = CE(NUBO2,2) + RNN*FLXD(2) - HDXZ2
              CE(NUBO2,3) = CE(NUBO2,3) + RNN*FLXD(3) - HDYZ2
            ENDIF
!
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
