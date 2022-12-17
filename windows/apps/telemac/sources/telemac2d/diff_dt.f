!                   ******************
                    SUBROUTINE DIFF_DT
!                   ******************
!
     &(DT,IKLE,NUBO,ELTSEG,IFABOR,VNOCL,COORD_R,MVIS,VIS,DIFT)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    Limitation of the time step due to diffusion terms.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      COORD_R    COORDINATES OF INTERPOLATION POINTS
!>@param  [in]      DIFT       DIFFUSIVITY FOR TRACER
!>@param  [in,out]  DT         TIME STEP
!>@param  [in]      ELTSEG     SEGMENTS COMPOSING AN ELEMENT
!>@param  [in]      IFABOR     ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!>@param  [in]      IKLE       NUMBERING OF NODES IN THE TRIANGLE
!>@param  [in]      MVIS       DIFFUSION SCHEME
!>@param  [in]      NUBO       GLOBAL INDICES OF EDGE EXTREMITIES
!>@param  [in]      VIS        DIFFUSIVITY FOR VELOCITY
!>@param  [in]      VNOCL      NORMAL VECTOR TO THE INTERFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: NPOIN,NELEM,H,MESH,
     &                                  V2DPAR,EPS_FV,BNDTYP,
     &                                  NPTFR,NSEG,
     &                                  FWTD,LIUBOR,FLBOR
      USE DECLARATIONS_TELEMAC, ONLY: KDIR,KENT,KNEU,KENTU
      USE INTERFACE_PARALLEL, ONLY : P_MIN
      IMPLICIT NONE
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3),MVIS
      DOUBLE PRECISION, INTENT(IN)    :: VNOCL(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: VIS(NPOIN)
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: DIFT
      DOUBLE PRECISION, INTENT(IN)    :: COORD_R(NSEG,4)
      DOUBLE PRECISION, INTENT(INOUT) :: DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL YESNO(NSEG)
      INTEGER IEL,JT,K,IS,NSG,NUBO1,NUBO2,NUBO3,I,LIVAR(NPTFR)
      DOUBLE PRECISION SAUX(NPOIN),AUX,DEMI,DIST,DTD,HTT,RNN
      DOUBLE PRECISION VISCTT,VNX,VNY,XIP,XJP,YIP,YJP,VNL(NPTFR)
      DOUBLE PRECISION VISC(NPOIN)
!     NEUMANN BND CONDITION AT DRY/WET INTERFACES:
      LOGICAL, PARAMETER :: DRYNEU = .TRUE.
!     RELAXATION PARAMETER FOR WEAK IMPOSITION OF DIRICHLET BC:
      DOUBLE PRECISION, PARAMETER :: RELAX = 0.25D0
      DOUBLE PRECISION, PARAMETER :: EPS_DTD = 1.D-8
!
      DEMI = 0.5D0
      CALL OV('X=0     ',X=SAUX, DIM1=NPOIN)
      IF (PRESENT(DIFT)) THEN
        CALL OV('X=C     ',X=VISC, C=DIFT, DIM1=NPOIN)
      ELSE
        CALL OV('X=Y     ',X=VISC, Y=VIS, DIM1=NPOIN)
      ENDIF
      DO K=1,NPTFR
        LIVAR(K) = KNEU
!
!       IMPOSED VELOCITY AND INLET
        IF((LIUBOR%I(K).EQ.KENT).OR.(LIUBOR%I(K).EQ.KENTU)) THEN
          IF(FLBOR%R(K).LT.0.D0) THEN
            LIVAR(K) = KDIR
          ENDIF
        ENDIF
      ENDDO
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
!
            VISCTT = (VISC(NUBO1)+VISC(NUBO2)+VISC(NUBO3))/3.D0
            SAUX(NUBO1) = SAUX(NUBO1) + VISCTT
            SAUX(NUBO2) = SAUX(NUBO2) + VISCTT
            SAUX(NUBO3) = SAUX(NUBO3) + VISCTT
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
              NUBO1 = NUBO(1,NSG)
              NUBO2 = NUBO(2,NSG)
              RNN = VNOCL(3,NSG)
!
              DIST = SQRT((MESH%X%R(NUBO2)-MESH%X%R(NUBO1))**2 +
     &                    (MESH%Y%R(NUBO2)-MESH%Y%R(NUBO1))**2)
              VISCTT = (VISC(NUBO2)+VISC(NUBO1))*0.5D0
              AUX = VISCTT*RNN/DIST
              HTT = (H%R(NUBO2)+H%R(NUBO1))*0.5D0
!
              IF(NCSIZE.GT.1)THEN
                IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
                  AUX = DEMI*AUX
                ENDIF
              ENDIF

!             FOR LIMITATION OF THE TIME STEP
              IF ((H%R(NUBO1).GT.EPS_FV).AND.
     &            (H%R(NUBO2).GT.EPS_FV)) THEN
                SAUX(NUBO1) = SAUX(NUBO1) + AUX*HTT/H%R(NUBO1)
                SAUX(NUBO2) = SAUX(NUBO2) + AUX*HTT/H%R(NUBO2)
              ELSE
                SAUX(NUBO1) = SAUX(NUBO1) + AUX
                SAUX(NUBO2) = SAUX(NUBO2) + AUX
              ENDIF
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
              XIP = COORD_R(NSG,1)
              YIP = COORD_R(NSG,2)
              XJP = COORD_R(NSG,3)
              YJP = COORD_R(NSG,4)
!
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
!             FOR LIMITATION OF THE TIME STEP
              IF ((H%R(NUBO1).GT.EPS_FV).AND.
     &            (H%R(NUBO2).GT.EPS_FV)) THEN
                SAUX(NUBO1) = SAUX(NUBO1) + AUX*HTT/H%R(NUBO1)
                SAUX(NUBO2) = SAUX(NUBO2) + AUX*HTT/H%R(NUBO2)
              ELSE
                SAUX(NUBO1) = SAUX(NUBO1) + AUX
                SAUX(NUBO2) = SAUX(NUBO2) + AUX
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF ! DIFF SCHEME
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
!             FOR LIMITATION OF THE TIME STEP
              SAUX(IS) = SAUX(IS) + AUX
            ENDIF
!           IF NEUMANN -> NULL FLUX, NO UPDATE NEEDED
!
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     LIMITATION OF THE TIME STEP
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(SAUX,SAUX,SAUX,NPOIN,1,2,1,MESH)
      ENDIF
      DO I=1,NPOIN
        DTD = FWTD*V2DPAR%R(I)/MAX(SAUX(I), EPS_DTD)
        DT = MIN(DT, DTD)
      ENDDO
!     FOR PARALLELISM
      IF(NCSIZE.GT.1) DT=P_MIN(DT)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
