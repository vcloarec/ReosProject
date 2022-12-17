!                       ***********************
                        SUBROUTINE FLUX_ZOKAGOA
!                       ***********************
     &(HI,HJ,ZFI,ZFJ,U1,U2,V1,V2,XNN,YNN,FLXI,FLXJ)
!
!***********************************************************************
! TELEMAC 2D
!***********************************************************************
!
!>@brief  COMPUTES ZOKAGOA FLUX AT THE INERNAL INTERFACES
!!   REF.:"MODELING OF WETTING-DRYING TRANSITIONS IN FREE SURFACE FLOWS
!!             OVER COMPLEX TOPOGRAPHIES" CMAME 199(2010) PP 2281-2304
!
!>@history  R. ATA (EDF-LNHE)
!!        06/01/2012
!!        V6P1
!
!>@history  R. ATA (EDF-LNHE)
!!        06/02/2013
!!        V6P3
!! remove unused variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  FLXI       LEFT FLUX AT THE INTERFACE
!>@param  [in,out]  FLXJ       RIGHT FLUX AT THE INTERFACE
!>@param  [in]      H1         WATER DEPTH ON LEFT CELL
!>@param  [in]      H2         WATER DEPTH ON RIGHT CELL
!>@param  [in]      U1         VELOCITY U ON THE LEFT CELL
!>@param  [in]      U2         VELOCITY U ON THE RIGHT CELL
!>@param  [in]      V1         VELOCITY V ON THE LEFT CELL
!>@param  [in]      V2         VELOCITY V ON THE RIGHT CELL
!>@param  [in]      XNN        X COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      YNN        Y COMPONENT OF THE INTERFACE NORMAL
!>@param  [in]      ZF1        LEFT BOTTOM ELEVATION
!>@param  [in]      ZF2        RIGHT BOTTOM ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_FLUX_ZOKAGOA => FLUX_ZOKAGOA
      USE DECLARATIONS_TELEMAC2D, ONLY: GRAV
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: HI,HJ,U1,U2
      DOUBLE PRECISION, INTENT(IN)    :: V1,V2,XNN,YNN,ZFI,ZFJ
      DOUBLE PRECISION, INTENT(INOUT) :: FLXI(3),FLXJ(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER CHOICE_D
!
      DOUBLE PRECISION UI,UJ,VI,VJ
      DOUBLE PRECISION ETAI,ETAJ
      DOUBLE PRECISION GSUR2,ALPHA
      DOUBLE PRECISION U_IJ,D_IJ,C_IJ,C_I,C_J,DIJS2
      DOUBLE PRECISION FLUIJ_1I,FLUIJ_1J,FLUIJ_1
      DOUBLE PRECISION FLUIJ_2I,FLUIJ_2J,FLUIJ_20
      DOUBLE PRECISION FLUIJ_3I,FLUIJ_3J,FLUIJ_3
!
!-----------------------------------------------------------------------
!
      ALPHA=0.5D0
      GSUR2=GRAV/2.D0
      CHOICE_D=1
      ETAI=HI+ZFI
      ETAJ=HJ+ZFJ
!
!-----------------------------------------------------------------------
!
!     ROTATION
!
      UI  = XNN*U1+YNN*V1
      VI  =-YNN*U1+XNN*V1
!
      UJ  = XNN*U2+YNN*V2
      VJ  =-YNN*U2+XNN*V2
!
!     D_IJ COMPUTATION
!
      IF(CHOICE_D.EQ.1) THEN
!       ZOKAGOA'S CHOICE (MASS CONSERVATIVE)
        U_IJ=0.5D0*(UI+UJ)
        C_IJ=SQRT(GSUR2*(HI+HJ))
        D_IJ=ALPHA*MAX(ABS(U_IJ-C_IJ),MAX(ABS(U_IJ),ABS(U_IJ+C_IJ)))
!
      ELSEIF(CHOICE_D.EQ.2) THEN
!       TORO'S CHOICE (VIOLATE MASS CONSERVATION)
        C_I=SQRT(GRAV*HI)
        C_J=SQRT(GRAV*HJ)
        D_IJ=MAX(ABS(UI)+C_I,ABS(UJ)+C_J)
!
      ELSE
        WRITE(LU,4020) CHOICE_D
4020    FORMAT(1X,'FLU_TCH: ERROR IN THE UPWIND CHOICE: ',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     CENTERED FLUX COMPUTATION
!
!     ZOKAGOA FLUX
!
      FLUIJ_1 = 0.5D0*(HI*UI+HJ*UJ)
      FLUIJ_2I= 0.5D0*(HI*(UI*UI)+HJ*(UJ*UJ) +
     &          GSUR2*((ETAI*ETAI)+(ETAJ*ETAJ))-
     &          GRAV*ZFI*(ETAI+ETAJ) )
      FLUIJ_2J= 0.5D0*(HI*(UI*UI)+HJ*(UJ*UJ) +
     &          GSUR2*((ETAI*ETAI)+(ETAJ*ETAJ))-
     &          GRAV*ZFJ*(ETAI+ETAJ) )
      FLUIJ_3 = 0.5D0*(HI*UI*VI+HJ*UJ*VJ)
!
!     UPWINDING
!
      DIJS2=0.5D0*D_IJ
!
      FLUIJ_1I = FLUIJ_1  - DIJS2*(ETAJ-ETAI)
      FLUIJ_1J = FLUIJ_1  - DIJS2*(ETAJ-ETAI)
      FLUIJ_2I = FLUIJ_2I - DIJS2*(HJ*UJ-HI*UI)
      FLUIJ_2J = FLUIJ_2J - DIJS2*(HJ*UJ-HI*UI)
      FLUIJ_3I = FLUIJ_3  - DIJS2*(HJ*VJ-HI*VI)
      FLUIJ_3J = FLUIJ_3  - DIJS2*(HJ*VJ-HI*VI)
!
!     INVERSE ROTATION
!
      FLUIJ_20 = FLUIJ_2I
      FLUIJ_2I = XNN*FLUIJ_20-YNN*FLUIJ_3I
      FLUIJ_3I = YNN*FLUIJ_20+XNN*FLUIJ_3I
!
      FLUIJ_20 = FLUIJ_2J
      FLUIJ_2J = XNN*FLUIJ_20-YNN*FLUIJ_3J
      FLUIJ_3J = YNN*FLUIJ_20+XNN*FLUIJ_3J
!
!     FINAL FLUX
!
      FLXI(1) = FLUIJ_1I
      FLXI(2) = FLUIJ_2I
      FLXI(3) = FLUIJ_3I
!
      FLXJ(1) = FLUIJ_1J
      FLXJ(2) = FLUIJ_2J
      FLXJ(3) = FLUIJ_3J
!
!-----------------------------------------------------------------------
!
      RETURN
      END
