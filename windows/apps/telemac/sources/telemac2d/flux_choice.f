!                       **********************
                        SUBROUTINE FLUX_CHOICE
!                       **********************
     &(HG,HD,HRG,HRD,UG,UD,VG,VD,ZG,ZD,XNN,YNN,FLXG,FLXD,HG_UP,HD_UP,
     & VG_UP,VD_UP,DX)
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Computes the inteface flux depending on the selected scheme
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      DX         SPACE STEP
!>@param  [in,out]  FLXD       RIGHT FLUX AT THE INTERFACE
!>@param  [in,out]  FLXG       LEFT FLUX AT THE INTERFACE
!>@param  [in]      G          GRAVITY CONSTANT
!>@param  [in]      HG         WATER DEPTH ON THE LEFT CELL
!>@param  [in]      HG_UP      LEFT WATER DEPTH ON THE NEIGBOR CELL
!>@param  [in]      HD         WATER DEPTH ON THE RIGHT CELL
!>@param  [in]      HD_UP      RIGHT WATER DEPTH ON THE NEIGBOR CELL
!>@param  [in]      HRG        RECONSTRUCTED WATER DEPTH ON LEFT CELL
!>@param  [in]      HRD        RECONSTRUCTED WATER DEPTH ON RIGHT CELL
!>@param  [in]      UD         VELOCITY U ON THE RIGHT CELL
!>@param  [in]      UG         VELOCITY U ON THE LEFT CELL
!>@param  [in]      VD         VELOCITY V ON THE RIGHT CELL
!>@param  [in]      VD_UP      RIGHT VELOCITY ON THE NEIGBOR CELL
!>@param  [in]      VG         VELOCITY V ON THE LEFT CELL
!>@param  [in]      VG_UP      LEFT VELOCITY ON THE NEIGBOR CELL
!>@param  [in]      XNN        X COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      YNN        Y COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      ZD         BOTTOM ELEVATION ON THE RIGHT CELL
!>@param  [in]      ZG         BOTTOM ELEVATION ON THE LEFT CELL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FLUX_CHOICE => FLUX_CHOICE
      USE DECLARATIONS_TELEMAC2D, ONLY: ICIN
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: HG,HD,UG,UD,VG,VD,ZG,ZD,HRG,HRD
      DOUBLE PRECISION, INTENT(IN) :: XNN,YNN,DX,HG_UP,HD_UP,VG_UP,VD_UP
      DOUBLE PRECISION, INTENT(INOUT) :: FLXG(3),FLXD(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (ICIN)
      CASE(0)
        CALL FLUX_ROE(HG,HD,UG,UD,VG,VD,XNN,YNN,FLXG)
        FLXD(1) = FLXG(1)
        FLXD(2) = FLXG(2)
        FLXD(3) = FLXG(3)
      CASE(1)
        CALL FLUX_KIN(HRG,HRD,UG,UD,VG,VD,XNN,YNN,FLXG)
        FLXD(1) = FLXG(1)
        FLXD(2) = FLXG(2)
        FLXD(3) = FLXG(3)
      CASE(2)
        CALL FLUX_ZOKAGOA(HG,HD,ZG,ZD,UG,UD,VG,VD,XNN,YNN,FLXG,FLXD)
      CASE(3)
        CALL FLUX_TCHAMEN(HG,HD,ZG,ZD,UG,UD,VG,VD,XNN,YNN,FLXG,FLXD)
      CASE(4)
        CALL FLUX_HLLC(HRG,HRD,UG,UD,VG,VD,XNN,YNN,FLXG,.TRUE.)
        FLXD(1) = FLXG(1)
        FLXD(2) = FLXG(2)
        FLXD(3) = FLXG(3)
      CASE(5)
        CALL FLUX_WAF(HRG,HRD,UG,UD,VG,VD,HG_UP,HD_UP,VG_UP,VD_UP,
     &                XNN,YNN,DX,FLXG)
        FLXD(1) = FLXG(1)
        FLXD(2) = FLXG(2)
        FLXD(3) = FLXG(3)
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
