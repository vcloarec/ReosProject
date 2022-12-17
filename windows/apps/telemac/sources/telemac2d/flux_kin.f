!                       *******************
                        SUBROUTINE FLUX_KIN
!                       *******************
     &(HIJ,HJI,U1,U2,V1,V2,XNN,YNN,FLX)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Computes kinetic fluxes at the ij interface
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  FLX        FLUX AT THE INTERFACE
!>@param  [in]      HIJ        RECONSTRUCTED WATER DEPTH ON LEFT CELL
!>@param  [in]      HJI        RECONSTRUCTED WATER DEPTH ON RIGHT CELL
!>@param  [in]      U1         VELOCITY U ON THE LEFT CELL
!>@param  [in]      U2         VELOCITY U ON THE RIGHT CELL
!>@param  [in]      V1         VELOCITY V ON THE LEFT CELL
!>@param  [in]      V2         VELOCITY V ON THE RIGHT CELL
!>@param  [in]      XNN        X COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      YNN        Y COMPONENT OF THE INTERFACE NORMAL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FLUX_KIN => FLUX_KIN
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: HIJ,HJI,U1,U2,V1,V2
      DOUBLE PRECISION, INTENT(IN) :: XNN,YNN
      DOUBLE PRECISION, INTENT(INOUT) :: FLX(3)
!
      DOUBLE PRECISION RA3,ALP,EXT1,CIJ,A01,A11,A21,EXT2
      DOUBLE PRECISION A02,A12,A22,FLU11,FLU12,FLU21,FLU22
      DOUBLE PRECISION FLU31,CJI,UAS21,UAS22,UAS31,UAS32
!
!-----------------------------------------------------------------------
!
      UAS21  = XNN*U1+YNN*V1
      UAS31  =-YNN*U1+XNN*V1
!
      UAS22 = XNN*U2+YNN*V2
      UAS32 =-YNN*U2+XNN*V2
!
      RA3 = SQRT(1.5D0*GRAV)
      ALP = 0.5D0/RA3
!
      IF(HIJ.LE.0.D0) THEN
        CIJ  = 0.D0
        FLU11= 0.D0
        FLU21= 0.D0
      ELSE
        CIJ  = SQRT(HIJ)
        EXT1 = MIN(RA3,MAX(-RA3,-UAS21/CIJ))
!
        A01  = ALP*(RA3-EXT1)
        A11  = ALP*(RA3**2-EXT1**2)/2.D0
        A21  = ALP*(RA3**3-EXT1**3)/3.D0
!
        FLU11= HIJ*(UAS21*A01+CIJ*A11)
        FLU21= UAS21*(FLU11+CIJ*HIJ*A11) +A21*HIJ*HIJ
      ENDIF
!
      IF(HJI.LE.0.D0) THEN
        CJI   = 0.D0
        FLU12 = 0.D0
        FLU22 = 0.D0
      ELSE
        CJI  = SQRT(HJI)
        EXT2 = MIN(RA3,MAX(-RA3,-UAS22/CJI))
!
        A02  = ALP*(RA3+EXT2)
        A12  = ALP*(EXT2**2-RA3**2)/2.D0
        A22  = ALP*(RA3**3+EXT2**3)/3.D0
!
        FLU12= HJI*(UAS22*A02+CJI*A12)
        FLU22= UAS22*(FLU12+CJI*HJI*A12) +A22*HJI*HJI
      ENDIF
!
      FLU11=FLU11+FLU12
      FLU21=FLU21+FLU22
!
      IF(FLU11.GE.0.D0) THEN
        FLU31 =  UAS31 * FLU11
      ELSE
        FLU31 =  UAS32 * FLU11
      ENDIF
!
! OPPOSITE ROTATION
!
      FLX(1)  = FLU11
      FLX(2)  = XNN*FLU21-YNN*FLU31
      FLX(3)  = YNN*FLU21+XNN*FLU31
!
      RETURN
      END
