!                       ******************
                        SUBROUTINE INIT_FV
!                       ******************
!
     & (GPRDTIME,ICIN,QU,QV,FLUX,FLUX_OLD,W,IVIS,NEISEG,CORR_I,CORR_J,
     &  CORR_HL,CORR_HR,CORR_UL,CORR_UR,CORR_VL,CORR_VR,CORR_ZL,CORR_ZR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    Initializes variables for finite volume computation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  CORR_I    SECOND ORDER CORRECTION FOR SLOPE ALONG X
!>@param  [in,out]  CORR_J    SECOND ORDER CORRECTION FOR SLOPE ALONG Y
!>@param  [in,out]  CORR_HL   SECOND ORDER CORRECTION FOR LEFT DEPTH
!>@param  [in,out]  CORR_HR   SECOND ORDER CORRECTION FOR RIGHT DEPTH
!>@param  [in,out]  CORR_UL   SECOND ORDER CORRECTION FOR LEFT X-VEL
!>@param  [in,out]  CORR_UR   SECOND ORDER CORRECTION FOR RIGHT X-VEL
!>@param  [in,out]  CORR_VL   SECOND ORDER CORRECTION FOR LEFT Y-VEL
!>@param  [in,out]  CORR_VR   SECOND ORDER CORRECTION FOR RIGHT Y-VEL
!>@param  [in,out]  CORR_ZL   SECOND ORDER CORRECTION FOR LEFT BOTTOM
!>@param  [in,out]  CORR_ZR   SECOND ORDER CORRECTION FOR RIGHT BOTTOM
!>@param  [in,out]  FLUX      FLUX INCREMENT AT INTERFACES
!>@param  [in,out]  FLUX_OLD  FLUX AT TN
!>@param  [in,out]  GPRDTIME  TEST TIME STEP BIGGER THAN GRAPHIC OUTPUT
!>@param  [in,out]  ICIN      WHICH SCHEME
!>@param  [in,out]  IVIS      OPTION FOR THE DIFFUSION OF VELOCITIES
!>@param  [in,out]  NEISEG    NEIGHBORS OF SEGMENT (FOR LIMITER)
!>@param  [in,out]  QU        FLOW COMPONENTS AT TIME N
!>@param  [in,out]  QV        FLOW COMPONENTS AT TIME N
!>@param  [in,out]  W         WORKING TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_INIT_FV => INIT_FV
      USE DECLARATIONS_TELEMAC2D, ONLY: DIFVIT,LEOPRD,NPOIN,ITURB,
     &                            NELMAX,IKLE,NELEM,DTINI,U,V,HN,X,Y,
     &                            AT,LT,SORDER,MXPTVS,NSEG,VERTIC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: ICIN,IVIS,NEISEG(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: GPRDTIME
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX_OLD(NPOIN,3),W(3,NPOIN)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_I
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_J
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_HL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_HR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_UL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_UR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_VL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_VR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_ZL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CORR_ZR
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      GPRDTIME=LEOPRD*DTINI
!
!     ==================================================================
!     INITIALIZE QU AND QV AND FLUX_OLD
!     ==================================================================
!
      IF(LT.EQ.1) THEN
        CALL OV('X=YZ    ', X=QU, Y=HN%R, Z=U%R, DIM1=NPOIN)
        CALL OV('X=YZ    ', X=QV, Y=HN%R, Z=V%R, DIM1=NPOIN)
        DO I=1,NPOIN
          FLUX_OLD(I,1) = 0.D0
          FLUX_OLD(I,2) = 0.D0
          FLUX_OLD(I,3) = 0.D0
        ENDDO
      ENDIF
!
!     ==================================================================
!     INITIALIZE FLUX
!     ==================================================================
!
      DO I=1,NPOIN
        FLUX(I,1) = 0.D0
        FLUX(I,2) = 0.D0
        FLUX(I,3) = 0.D0
      ENDDO
!
!     ==================================================================
!     LISTING FOR 1ST TIME STEP
!     ==================================================================
!
      IF(LT.EQ.1) CALL ENTETE(1,AT,LT,ICIN,SORDER)
!
!     ==================================================================
!     CORRECTION FOR SECOND ORDER
!     ==================================================================
!
      DO I=1,NSEG
        CORR_I%R(I) = 0.D0
        CORR_J%R(I) = 0.D0
        CORR_HL%R(I) = 0.D0
        CORR_HR%R(I) = 0.D0
        CORR_UL%R(I) = 0.D0
        CORR_UR%R(I) = 0.D0
        CORR_VL%R(I) = 0.D0
        CORR_VR%R(I) = 0.D0
        CORR_ZL%R(I) = 0.D0
        CORR_ZR%R(I) = 0.D0
      ENDDO
!
!     ==================================================================
!     SAVE STATE VARIABLES INTO W
!     ==================================================================
!
      DO I=1,NPOIN
        W(1,I)= HN%R(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
      ENDDO ! I
!
!     ==================================================================
!     INITIALIZATION FOR DIFFUSION
!     ==================================================================
!
      IVIS=0
      IF(DIFVIT.AND.ITURB.EQ.1) IVIS=1
      IF (ITURB.GE.2) THEN
        WRITE(LU,*) 'WITH FINITE VOLUME'
        WRITE(LU,*) 'TURBULENCE MODEL NOT TAKEN INTO ACCOUNT'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     ==================================================================
!     WAF SCHEME - INIT FOR NEISEG
!     ==================================================================
!
      IF(ICIN.EQ.5) THEN
        IF(LT.EQ.1) THEN
!
          DO I=1,NSEG
            NEISEG(1,I) = 0
            NEISEG(2,I) = 0
          ENDDO
          CALL SEG_NEIGHBORS
     &        (X,Y,IKLE%I,NPOIN,MXPTVS,NELEM,NELMAX,NSEG,NEISEG)
        ENDIF
      ENDIF
!
      IF(VERTIC) THEN
        WRITE(LU,*) 'VERTICAL STRUCTURES NOT COMPATIBLE'
        WRITE(LU,*) 'WITH FINITE VOLUME SCHEME'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!
      RETURN
      END
!
