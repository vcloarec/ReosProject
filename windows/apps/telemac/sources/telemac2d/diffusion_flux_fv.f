!                   ****************************
                    SUBROUTINE DIFFUSION_FLUX_FV
!                   ****************************
!
     &(IKLE,VAR,DPX,DPY,CE,VNOCL,ELTSEG,NUBO,IFABOR,
     & MVIS,VISC,COORD_R,ALPHA_R,LIVAR,VBORD,DX,DY)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    COMPUTES THE DIFFUSION TERMS.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      IKLE       NUMBERING OF NODES IN THE TRIANGLE
!>@param  [in]      VAR        PRIMITIVE VARIABLE FOR DIFFUSION
!>@param  [in]      DPX        GRADIENTS OF AT NODES
!>@param  [in]      DPY        GRADIENTS OF AT NODES
!>@param  [in,out]  CE         FLUX AT TIME N
!>@param  [in]      VNOCL      NORMAL VECTOR TO THE INTERFACE
!>@param  [in]      ELTSEG     SEGMENTS COMPOSING AN ELEMENT
!>@param  [in]      NUBO       GLOBAL INDICES OF EDGE EXTREMITIES
!>@param  [in]      IFABOR     ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!>@param  [in]      MVIS       DIFFUSION SCHEME
!>@param  [in]      VISC       DIFFUSIVITY
!>@param  [in]      COORD_R    COORDINATES OF INTERPOLATION POINTS
!>@param  [in]      ALPHA_R    LINEAR INTERPOLATION WEIGHTS
!>@param  [in]      LIVAR      TYPE OF BOUNDARY CONDITIONS
!>@param  [in]      VBORD      IMPOSED BOUNDARY VALUE FOR DIRICHLET BC
!>@param  [in]      DX         GRADIENTS AT NODES
!>@param  [in]      DY         GRADIENTS AT NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE BIEF_DEF, ONLY: NCSIZE
      USE DECLARATIONS_TELEMAC2D, ONLY:NPOIN,NELEM,MESH,V2DPAR,NSEG,
     &                                 NPTFR,X,Y,
     &                                 H,EPS_FV,BNDTYP,OPTRTPF
      USE INTERFACE_PARALLEL, ONLY : P_MIN
      USE INTERFACE_TELEMAC2D, EX_DIFFUSION_FLUX_FV => DIFFUSION_FLUX_FV
      USE DECLARATIONS_TELEMAC, ONLY: KDIR
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: MVIS
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3)
      INTEGER, INTENT(IN)             :: LIVAR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: COORD_R(NSEG,4),ALPHA_R(NSEG,4)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELEM),DPY(3,NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: VNOCL(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: VISC(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: VBORD(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DX(NPOIN),DY(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL :: YESNO(NSEG)
      INTEGER :: IS,JT,NUBO1,NUBO2,NUBO3,IEL,I,NSG,K
      DOUBLE PRECISION :: AIRJ,HTT,AUX,VISCTT
      DOUBLE PRECISION :: RNN,DIST,DEMI
      DOUBLE PRECISION :: SAUX(NPOIN),DJX,DJY
      DOUBLE PRECISION :: XIP,XJP,YIP,YJP,VAR_IP,VAR_JP,GRDREC
      DOUBLE PRECISION :: VARIPP,VARJPP
      DOUBLE PRECISION :: OPP_VAR1(NSEG),OPP_VAR2(NSEG)
      DOUBLE PRECISION :: OPP_DJX1(NSEG),OPP_DJX2(NSEG)
      DOUBLE PRECISION :: OPP_DJY1(NSEG),OPP_DJY2(NSEG)
      DOUBLE PRECISION :: VNX,VNY,VNL(NPTFR)
      DOUBLE PRECISION :: VARD(NPOIN),TMPF(NPOIN)
      DOUBLE PRECISION, PARAMETER :: EPS_DTD = 1.D-8
!
!-----------------------------------------------------------------------
!     ADVANCED NUMERICAL PARAMETERS:
!
!     RELAXATION PARAMETER FOR WEAK IMPOSITION OF DIRICHLET BC:
      DOUBLE PRECISION, PARAMETER :: RELAX = 0.25D0
!     NEUMANN BND CONDITION AT DRY/WET INTERFACES:
      LOGICAL, PARAMETER :: DRYNEU = .TRUE.
!     OPTION FOR FIELD RECONSTRUCTION IN RTPF2 SCHEME
!     1: RECONSTRUCTION USING CELL GRADIENTS
!     2: RECONSTRUCTION USING FACE GRADIENT (DEFAULT)
      INTEGER, PARAMETER :: OPTGRD = 2
!     FORCE FIRST ORDER GRADIENT REC. IN RTPF2 SCHEME
!     (WITH THIS OPTION RTPF2 <=> RTPF1)
      LOGICAL, PARAMETER :: RTPF2ORD1 = .FALSE.
!
!-----------------------------------------------------------------------
!
      DEMI = 0.5D0
!
!     INITIALISATIONS
      CALL OV('X=0     ',X=CE, DIM1=NPOIN)
      CALL OV('X=0     ',X=SAUX, DIM1=NPOIN)
      VARD(:) = VAR(:)
      TMPF(:) = 1.D0
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS (STRONG)
      IF(BNDTYP.EQ.2)THEN
        IF(NPTFR.GT.0)THEN ! FOR PARALLEL CASES
          DO K=1,NPTFR
            IS = MESH%NBOR%I(K)
!           IF DIRICHLET
            IF(LIVAR(K).EQ.KDIR) THEN
              VARD(IS) = VBORD(K)
              TMPF(IS) = 0.D0
!           IF NEUMANN
            ELSE
              VARD(IS) = VAR(IS)
              TMPF(IS) = 1.D0
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXPLICIT P1 FINITE ELEMENT WITH MASS LUMPING
!     ********************************************
!
      IF(MVIS.EQ.1) THEN
!
!     LOOP ON GLOBAL LIST OF TRIANGLES
!
        DO JT=1,NELEM
!
          NUBO1 = IKLE(JT,1)
          NUBO2 = IKLE(JT,2)
          NUBO3 = IKLE(JT,3)
!
!         DIFFUSION ONLY ON WET CELLS
          IF (((H%R(NUBO1).GT.EPS_FV).AND.
     &         (H%R(NUBO2).GT.EPS_FV).AND.
     &         (H%R(NUBO3).GT.EPS_FV)).OR.
     &         (.NOT.DRYNEU)) THEN

            AIRJ = MESH%SURFAC%R(JT)
!
!           COMPUTES THE GRADIENTS
            DJX = VARD(NUBO1)*DPX(1,JT)+VARD(NUBO2)*DPX(2,JT)+
     &                     VARD(NUBO3)*DPX(3,JT)! GRAD_X(U)|_Tk
            DJY = VARD(NUBO1)*DPY(1,JT)+VARD(NUBO2)*DPY(2,JT)+
     &                     VARD(NUBO3)*DPY(3,JT)! GRAD_Y(U)|_Tk
!
!           COMPUTES THE DIFFUSION FLUX
            AUX = ((H%R(NUBO1) +H%R(NUBO2) )*
     &             (VISC(NUBO1)+VISC(NUBO2))+
     &             (H%R(NUBO2) +H%R(NUBO3) )*
     &             (VISC(NUBO2)+VISC(NUBO3))+
     &             (H%R(NUBO3) +H%R(NUBO1) )*
     &             (VISC(NUBO3)+VISC(NUBO1)))*AIRJ/12.D0

!            HTT = (H%R(NUBO1)+H%R(NUBO2)+H%R(NUBO3))/3.D0
!            AUX = VISCTT*HTT*AIRJ
!
            CE(NUBO1) = CE(NUBO1) - TMPF(NUBO1)*AUX*
     &                     (DJX*DPX(1,JT)+DJY*DPY(1,JT))
            CE(NUBO2) = CE(NUBO2) - TMPF(NUBO2)*AUX*
     &                     (DJX*DPX(2,JT)+DJY*DPY(2,JT))
            CE(NUBO3) = CE(NUBO3) - TMPF(NUBO3)*AUX*
     &                     (DJX*DPX(3,JT)+DJY*DPY(3,JT))
!
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
!     TWO POINT FLUX SCHEME
!     *********************
!
      ELSEIF(MVIS.EQ.2) THEN
        DO NSG=1,NSEG
          YESNO(NSG)=.FALSE.
        ENDDO
        DO IEL=1,NELEM
          DO I = 1,3
            IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
              NSG = ELTSEG(IEL,I)
              YESNO(NSG) = .TRUE.
!
!             RETRIEVE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!             WITH RESPECT TO THE NORMAL
              NUBO1 = NUBO(1,NSG)
              NUBO2 = NUBO(2,NSG)
              RNN = VNOCL(3,NSG)
!
!             DIFFUSION ONLY ON WET CELLS
              IF (((H%R(NUBO1).GT.EPS_FV).AND.
     &             (H%R(NUBO2).GT.EPS_FV)).OR.
     &             (.NOT.DRYNEU)) THEN
!
!             COMPUTES THE DIFFUSION FLUX
              DIST = SQRT((X(NUBO2)-X(NUBO1))**2 +
     &                    (Y(NUBO2)-Y(NUBO1))**2)
              VISCTT = (VISC(NUBO2)+VISC(NUBO1))/2.D0
              AUX = VISCTT*RNN/DIST
              HTT = (H%R(NUBO2)+H%R(NUBO1))/2.D0
!
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
                  AUX = DEMI*AUX
                ENDIF
              ENDIF
!
              CE(NUBO1) = CE(NUBO1) + TMPF(NUBO1)*AUX*HTT*
     &                               (VARD(NUBO2)-VARD(NUBO1))
              CE(NUBO2) = CE(NUBO2) + TMPF(NUBO2)*AUX*HTT*
     &                               (VARD(NUBO1)-VARD(NUBO2))
!
              ENDIF ! WET CELL
            ENDIF
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
!     RECONSTRUCTED TWO POINT FLUX SCHEME  (OPTION 1)
!     ***********************************************
!
      ELSEIF(MVIS.EQ.3) THEN ! RTPF SCHEME
!
      IF(OPTRTPF.EQ.1) THEN ! OPT FOR RTPF SCHEME
!
        CALL OPP_VAR(VARD,OPP_VAR1,OPP_VAR2,IKLE,ELTSEG,
     &               MESH%ORISEG%I,NUBO)
!
        DO NSG=1,NSEG
          YESNO(NSG)=.FALSE.
        ENDDO
        DO IEL=1,NELEM
          DO I = 1,3
            IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
              NSG = ELTSEG(IEL,I)
              YESNO(NSG) = .TRUE.
!
!             RETRIEVE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!             WITH RESPECT TO THE NORMAL
              NUBO1 = NUBO(1,NSG)
              NUBO2 = NUBO(2,NSG)
              RNN = VNOCL(3,NSG)
!
!             DIFFUSION ONLY ON WET CELLS
              IF (((H%R(NUBO1).GT.EPS_FV).AND.
     &             (H%R(NUBO2).GT.EPS_FV)).OR.
     &             (.NOT.DRYNEU)) THEN
!
              XIP = COORD_R(NSG,1)
              YIP = COORD_R(NSG,2)
              XJP = COORD_R(NSG,3)
              YJP = COORD_R(NSG,4)
!
              IF(IFABOR(IEL,I).EQ.-1.OR. IFABOR(IEL,I).EQ. 0) THEN
!               BOUNDARY EDGE, STANDARD TWO POINT FLUX
!               --------------------------------------
                VAR_IP = VARD(NUBO1)
                VAR_JP = VARD(NUBO2)
!
              ELSE
!               RECONSTRUCTED TWO POINT FLUX
!               ----------------------------
                VARIPP = ALPHA_R(NSG,2) * OPP_VAR2(NSG)
     &            +(1.D0-ALPHA_R(NSG,2))* OPP_VAR1(NSG)
                VARJPP = ALPHA_R(NSG,4) * OPP_VAR2(NSG)
     &            +(1.D0-ALPHA_R(NSG,4))* OPP_VAR1(NSG)
!
                VAR_IP = ALPHA_R(NSG,1) * VARIPP
     &            +(1.D0-ALPHA_R(NSG,1))* VARD(NUBO1)
                VAR_JP = ALPHA_R(NSG,3) * VARJPP
     &            +(1.D0-ALPHA_R(NSG,3))* VARD(NUBO2)
!
              ENDIF
!
!             COMPUTES THE DIFFUSION FLUX
              DIST = SQRT((XJP-XIP)**2 + (YJP-YIP)**2)
              VISCTT = (VISC(NUBO2)+VISC(NUBO1))/2.D0
              AUX = VISCTT*RNN/DIST
              HTT = (H%R(NUBO2)+H%R(NUBO1))/2.D0
!
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
                  AUX = DEMI*AUX
                ENDIF
              ENDIF
!
              CE(NUBO1) = CE(NUBO1) + TMPF(NUBO1)*AUX*HTT*
     &                                (VAR_JP-VAR_IP)
              CE(NUBO2) = CE(NUBO2) + TMPF(NUBO2)*AUX*HTT*
     &                                (VAR_IP-VAR_JP)
!
              ENDIF ! WET CELL
            ENDIF
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
!     RECONSTRUCTED TWO POINT FLUX SCHEME (OPTION 2)
!     **********************************************
!
      ELSEIF(OPTRTPF.EQ.2) THEN ! OPT FOR RTPF SCHEME
!
!       FIRST ORDER GRADIENTS COMPUTATION
        IF(RTPF2ORD1) THEN
          CALL OPP_GRD(VARD,OPP_DJX1,OPP_DJY1,OPP_DJX2,OPP_DJY2,
     &                 IKLE,ELTSEG,MESH%ORISEG%I,NUBO,DPX,DPY)
        ENDIF
!
        DO NSG=1,NSEG
          YESNO(NSG)=.FALSE.
        ENDDO
        DO IEL=1,NELEM
          DO I = 1,3
            IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
              NSG = ELTSEG(IEL,I)
              YESNO(NSG) = .TRUE.
!
!             RETRIEVE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!             WITH RESPECT TO THE NORMAL
              NUBO1 = NUBO(1,NSG)
              NUBO2 = NUBO(2,NSG)
              RNN = VNOCL(3,NSG)
!
!             DIFFUSION ONLY ON WET CELLS
              IF (((H%R(NUBO1).GT.EPS_FV).AND.
     &             (H%R(NUBO2).GT.EPS_FV)).OR.
     &             (.NOT.DRYNEU)) THEN
!
              XIP = COORD_R(NSG,1)
              YIP = COORD_R(NSG,2)
              XJP = COORD_R(NSG,3)
              YJP = COORD_R(NSG,4)
!
              IF(IFABOR(IEL,I).EQ.-1.OR. IFABOR(IEL,I).EQ. 0) THEN
!               BOUNDARY EDGE, STANDARD TWO POINT FLUX
!               --------------------------------------
                GRDREC = 0.D0
!
              ELSE
!               RECONSTRUCTED TWO POINT FLUX
!               ----------------------------
                IF (OPTGRD.EQ.1) THEN
                  IF(RTPF2ORD1) THEN
                    GRDREC = OPP_DJX2(NSG)*(XJP-X(NUBO2))
     &                     + OPP_DJY2(NSG)*(YJP-Y(NUBO2))
     &                     - OPP_DJX1(NSG)*(XIP-X(NUBO1))
     &                     - OPP_DJY1(NSG)*(YIP-Y(NUBO1))
                  ELSE
                    GRDREC = DX(NUBO2)*(XJP-X(NUBO2))
     &                     + DY(NUBO2)*(YJP-Y(NUBO2))
     &                     - DX(NUBO1)*(XIP-X(NUBO1))
     &                     - DY(NUBO2)*(YIP-Y(NUBO1))
                  ENDIF
!
                ELSE IF (OPTGRD.EQ.2) THEN
                  IF(RTPF2ORD1) THEN
                    GRDREC = DEMI*(OPP_DJX2(NSG)+OPP_DJX1(NSG))
     &                           *(XJP-X(NUBO2)-(XIP-X(NUBO1)))
     &                     + DEMI*(OPP_DJY2(NSG)+OPP_DJY1(NSG))
     &                           *(YJP-Y(NUBO2)-(YIP-Y(NUBO1)))
                  ELSE
                    GRDREC = DEMI*(DX(NUBO2)+DX(NUBO1))
     &                           *(XJP-X(NUBO2)-(XIP-X(NUBO1)))
     &                     + DEMI*(DY(NUBO2)+DY(NUBO1))
     &                           *(YJP-Y(NUBO2)-(YIP-Y(NUBO1)))
                  ENDIF
                ENDIF
              ENDIF
!
!             COMPUTES THE DIFFUSION FLUX
              DIST = SQRT((XJP-XIP)**2 + (YJP-YIP)**2)
              VISCTT = (VISC(NUBO2)+VISC(NUBO1))/2.D0
              AUX = VISCTT*RNN/DIST
              HTT = (H%R(NUBO2)+H%R(NUBO1))/2.D0
!
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
                  AUX = DEMI*AUX
                ENDIF
              ENDIF
!
              CE(NUBO1) = CE(NUBO1) + TMPF(NUBO1)*AUX*HTT*
     &                    (VARD(NUBO2)-VARD(NUBO1)+GRDREC)
              CE(NUBO2) = CE(NUBO2) + TMPF(NUBO2)*AUX*HTT*
     &                    (VARD(NUBO1)-VARD(NUBO2)-GRDREC)
!
              ENDIF ! WET CELL
            ENDIF
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDIF ! OPT FOR RTPF SCHEME
      ELSE
        WRITE(LU,*)'MODEL FOR DIFFUSION',MVIS
        WRITE(LU,*)'NOT IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS (WEAK)
      IF(BNDTYP.EQ.1)THEN
        IF(NPTFR.GT.0)THEN ! FOR PARALLEL CASES
          DO K=1,NPTFR
            IS=MESH%NBOR%I(K)
!
!           IF DIRICHLET
            IF(LIVAR(K).EQ.KDIR) THEN
              VNX=MESH%XNEBOR%R(K+NPTFR)
              VNY=MESH%YNEBOR%R(K+NPTFR)
              VNL(K)=SQRT(VNX**2+VNY**2)
!
              AUX = VISC(K)*VNL(K)/(SQRT(V2DPAR%R(IS))*RELAX)
              CE(IS) = CE(IS) + AUX*H%R(IS)*(VBORD(K)-VARD(IS))
!             FOR LIMITATION OF THE TIME STEP
            ENDIF
!           IF NEUMANN -> NULL FLUX, NO UPDATE NEEDED
!
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN                 ! NPON,NPLAN,ICOM,IAN
        CALL PARCOM2(CE,CE,CE,NPOIN,1,2,1,MESH )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
