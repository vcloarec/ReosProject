!                   ***********************
                    SUBROUTINE FLUX_EF_VF_2
!                   ***********************
!
     &(PHIEL,NELEM,NELMAX,IKLE,IOPT,NPOIN,FN,FI_I,FSTAR,HN,H,SU,TETA,
     & DFDT)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    Equivalent of FLUX_EF_VF, the result is given in terms of
!+        contribution per point, and it takes a derivative in time
!+        into account, as well of a TETA between FN and FSTAR.
!
!history  J-M HERVOUET & SARA PAVAN (EDF LAB, LNHE)
!+        29/04/2014
!+        V7P0
!+   First version, after Mario Ricchiuto's (INRIA Bordeaux) theory.
!
!history  J-M HERVOUET & SARA PAVAN (EDF LAB, LNHE)
!+        20/10/2016
!+        V7P2
!+   Simplification, N fluxes taken instead of PSI fluxes to be
!+   added to the derivative in time when option PSI is asked. IOPT
!+   is thus no longer used here.
!
!history  J-M HERVOUET (jubilado)
!+        09/09/2017
!+        V7P3
!+   Argument NELMAX added, for dimensioning arrays IKLE and PHIEL.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FI_I           |<--| ASSEMBLED CONTRIBUTIONS
!| FN             |-->| F AT TIME N
!| FSTAR          |-->| ESTIMATION OF F AT TIME N+1
!| H              |-->| DEPTH AT TIME N+1
!| HN             |-->| DEPTH AT TIME N
!| IKLE           |-->| CONNECTIVITY TABLE
!| IOPT           |-->| OPTION (2:N 3:PSI BUT NOT USED)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| PHIEL          |-->| PER ELEMENT, FLUXES LEAVING POINTS
!| SU             |-->| PER ELEMENT, AREA OF ELEMENT
!| TETA           |-->| CRANK-NICHOLSON COEFFICIENT
!| DFDT           |-->| ESTIMATION OF DERIVATIVE IN TIME OF F
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_EF_VF_2 => FLUX_EF_VF_2
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IOPT,NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: PHIEL(NELMAX,3)
      TYPE(BIEF_OBJ), INTENT(IN)      :: FN
      DOUBLE PRECISION, INTENT(INOUT) :: FI_I(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FSTAR(NPOIN),H(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SU(NELEM),DFDT(NPOIN),TETA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I
      DOUBLE PRECISION K1,K2,K3,TIERS,FN1,FN2,FN3,FS1,FS2,FS3
      DOUBLE PRECISION BETA1,BETA2,BETA3,BETA1PSI,BETA2PSI,BETA3PSI
      DOUBLE PRECISION FINCORR1,FINCORR2,FINCORR3
      DOUBLE PRECISION PHITCOR,SUMAX,H1,H2,H3,COEF
!     N FLUXES
      DOUBLE PRECISION FP21,FP32,FP13,FP12,FP23,FP31
!
      INTRINSIC MIN,MAX,ABS
!
      DOUBLE PRECISION, PARAMETER :: EPSPHI = 1.D-12
!
!-----------------------------------------------------------------------
!
      TIERS=1.D0/3.D0
!
!-----------------------------------------------------------------------
!
      IF(IOPT.EQ.2.OR.IOPT.EQ.3) THEN
!
!       N-SCHEME, CORRECTOR
!
        DO I=1,NPOIN
          FI_I(I)=0.D0
        ENDDO
!
        DO IELEM = 1,NELEM
!
          K1 = -PHIEL(IELEM,1)
          K2 = -PHIEL(IELEM,2)
          K3 = -PHIEL(IELEM,3)
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK
!
          FP12=MAX(MIN(K1,-K2),0.D0)
          FP23=MAX(MIN(K2,-K3),0.D0)
          FP31=MAX(MIN(K3,-K1),0.D0)
          FP21=MAX(MIN(K2,-K1),0.D0)
          FP32=MAX(MIN(K3,-K2),0.D0)
          FP13=MAX(MIN(K1,-K3),0.D0)
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
          FS1=FSTAR(I1)
          FS2=FSTAR(I2)
          FS3=FSTAR(I3)
!
!         COMPUTE THE NEW RESIDUAL AND NEW DISTRIBUTION
!
          COEF=TIERS*SU(IELEM)
          H1=((1.D0-TETA)*H(I1)+TETA*HN(I1))*COEF
          H2=((1.D0-TETA)*H(I2)+TETA*HN(I2))*COEF
          H3=((1.D0-TETA)*H(I3)+TETA*HN(I3))*COEF
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!
          FINCORR1=H1*DFDT(I1)
     &            +(1.D0-TETA)*(FP12*(FN1-FN2)+FP13*(FN1-FN3))
     &            +      TETA *(FP12*(FS1-FS2)+FP13*(FS1-FS3))
          FINCORR2=H2*DFDT(I2)
     &            +(1.D0-TETA)*(FP21*(FN2-FN1)+FP23*(FN2-FN3))
     &            +      TETA *(FP21*(FS2-FS1)+FP23*(FS2-FS3))
          FINCORR3=H3*DFDT(I3)
     &            +(1.D0-TETA)*(FP31*(FN3-FN1)+FP32*(FN3-FN2))
     &            +      TETA *(FP31*(FS3-FS1)+FP32*(FS3-FS2))
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(ABS(PHITCOR).GT.EPSPHI) THEN
!           BETA OF N-SCHEME
            BETA1=FINCORR1/PHITCOR
            BETA2=FINCORR2/PHITCOR
            BETA3=FINCORR3/PHITCOR
!           NOW THE PSI REDUCTION
            SUMAX=MAX(0.D0,BETA1)+MAX(0.D0,BETA2)+MAX(0.D0,BETA3)
            IF(SUMAX.GT.1.D-20) THEN
              BETA1PSI=MAX(0.D0,BETA1)/SUMAX
              BETA2PSI=MAX(0.D0,BETA2)/SUMAX
              BETA3PSI=MAX(0.D0,BETA3)/SUMAX
              FI_I(I1)=FI_I(I1)+BETA1PSI*PHITCOR
              FI_I(I2)=FI_I(I2)+BETA2PSI*PHITCOR
              FI_I(I3)=FI_I(I3)+BETA3PSI*PHITCOR
            ENDIF
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,*) 'FLUX_EF_VF_2:'
        WRITE(LU,*) 'UNKNOWN OPTION: ',IOPT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

