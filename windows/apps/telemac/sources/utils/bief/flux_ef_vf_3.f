!                   ***********************
                    SUBROUTINE FLUX_EF_VF_3
!                   ***********************
!
     &(PHIEL,NELEM,NELMAX,ELTSEG,ORISEG,FXMATPAR,NSEG,IKLE,NPOIN,
     & FN,FI_I,SU,HDFDT,TETA,YAFLULIM,FLULIM,YAFLULIMEBE,FLULIMEBE)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    Equivalent of FLUX_EF_VF, but only for PSI scheme, and the
!+        result is given in terms of contribution per point, not fluxes
!+        between points, and takes a derivative in time into account.
!
!warning  When both YAFLULIM and YAFLULIMEBE are true only the latter is
!+        taken into account !
!
!history  J-M HERVOUET & SARA PAVAN (EDF LAB, LNHE)
!+        04/08/2015
!+        V7P1
!+   First version. The N and PSI versions are not different here.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        05/11/2016
!+        V7P3
!+   Now two options of flux limitation offered.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        09/09/2017
!+        V7P3
!+   Argument NELMAX added for correctly dimensioning arrays.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| GIVES THE SEGMENTS IN A TRIANGLE
!| FI_I           |<--| CONTRIBUTIONS TO POINTS
!| FLULIM         |-->| LIMITATION OF FLUXES BY SEGMENT
!| FLULIMEBE      |-->| LIMITATION OF FLUXES BY SEGMENT BUT EBE
!| FN             |-->| TRACER AT TIME T(N)
!| FXMARPAR       |-->| FLUXES ASSEMBLED IN //
!| HDFDT          |-->| DEPTH*DH/DT
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELEM          |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| ORISEG         |-->| GIVES THE ORIENTATION OF SEGMENTS IN A TRIANGLE
!| PHIEL          |-->| PER ELEMENT, FLUXES LEAVING POINTS
!| SU             |-->| SURFACE OF TRIANGLES
!| TETA           |-->| LOCAL IMPLICITATION
!| YAFLULIM       |-->| IF YES, A LIMITATION OF FLUXES IS GIVEN BY SEGMENT
!| YAFLULIMEBE    |-->| IF YES, AN EBE LIMITATION OF FLUXES IS GIVEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF, EX_FLUX_EF_VF_3 => FLUX_EF_VF_3
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX,NPOIN,NSEG
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(IN)             :: ORISEG(NELMAX,3)
      LOGICAL, INTENT(IN)             :: YAFLULIM,YAFLULIMEBE
      DOUBLE PRECISION, INTENT(IN)    :: PHIEL(NELMAX,3),TETA(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLULIMEBE(*)
      DOUBLE PRECISION, INTENT(IN)    :: FXMATPAR(NSEG),FLULIM(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FI_I(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SU(NELEM),HDFDT(NPOIN)
      TYPE(BIEF_OBJ), INTENT(IN)      :: FN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I,ISEG1,ISEG2,ISEG3
      DOUBLE PRECISION K1,K2,K3,FN1,FN2,FN3,BETA1,BETA2,BETA3
      DOUBLE PRECISION FINCORR1,FINCORR2,FINCORR3,PHITCOR,COEF
!     PSI FLUXES WITH FN
      DOUBLE PRECISION FP21,FP32,FP13,FP12,FP23,FP31
      DOUBLE PRECISION MIN12,MIN23,MIN13,MT1,MT2,MT3
!
      INTRINSIC MIN,MAX
!
      DOUBLE PRECISION, PARAMETER :: EPSPHI=1.D-12
      DOUBLE PRECISION, PARAMETER :: TIERS =1.D0/3.D0
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        FI_I(I)=0.D0
      ENDDO
!
!     DEPENDING ON THE FLUX LIMITATION...
!
      IF(YAFLULIMEBE) THEN
!
!       AS WHEN YAFLULIM=.TRUE., BUT WITH FLULIM ASSUMED TO BE 1
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
          ISEG1=ELTSEG(IELEM,1)
          ISEG2=ELTSEG(IELEM,2)
          ISEG3=ELTSEG(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK
!         I.E. FIJ>0 MEANS FLUX FROM J TO I !!!!!
!
          FP12=MAX(MIN(K1,-K2),0.D0)*FLULIMEBE((1-1)*NELMAX+IELEM)
          FP23=MAX(MIN(K2,-K3),0.D0)*FLULIMEBE((2-1)*NELMAX+IELEM)
          FP31=MAX(MIN(K3,-K1),0.D0)*FLULIMEBE((3-1)*NELMAX+IELEM)
          FP21=MAX(MIN(K2,-K1),0.D0)*FLULIMEBE((1-1)*NELMAX+IELEM)
          FP32=MAX(MIN(K3,-K2),0.D0)*FLULIMEBE((2-1)*NELMAX+IELEM)
          FP13=MAX(MIN(K1,-K3),0.D0)*FLULIMEBE((3-1)*NELMAX+IELEM)
!
!         CORRECTING THE FLUXES WHEN THEIR SIGN IS NOT THE SAME
!         AS THE ASSEMBLED VALUE, KNOWING THAT ALL THE FPIJ ARE
!         POSITIVE BY CONSTRUCTION
!
!         SEGMENT 1
!
          IF(ORISEG(IELEM,1).EQ.1) THEN
            IF(FXMATPAR(ISEG1).GT.0.D0) THEN
              FP12=0.D0
              IF(FP21.GT. FXMATPAR(ISEG1)) FP21=FXMATPAR(ISEG1)
            ELSE
              FP21=0.D0
              IF(FP12.GT.-FXMATPAR(ISEG1)) FP12=-FXMATPAR(ISEG1)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG1).GT.0.D0) THEN
              FP21=0.D0
              IF(FP12.GT. FXMATPAR(ISEG1)) FP12=FXMATPAR(ISEG1)
            ELSE
              FP12=0.D0
              IF(FP21.GT.-FXMATPAR(ISEG1)) FP21=-FXMATPAR(ISEG1)
            ENDIF
          ENDIF
!
!         SEGMENT 2
!
          IF(ORISEG(IELEM,2).EQ.1) THEN
            IF(FXMATPAR(ISEG2).GT.0.D0) THEN
              FP23=0.D0
              IF(FP32.GT. FXMATPAR(ISEG2)) FP32=FXMATPAR(ISEG2)
            ELSE
              FP32=0.D0
              IF(FP23.GT.-FXMATPAR(ISEG2)) FP23=-FXMATPAR(ISEG2)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG2).GT.0.D0) THEN
              FP32=0.D0
              IF(FP23.GT. FXMATPAR(ISEG2)) FP23=FXMATPAR(ISEG2)
            ELSE
              FP23=0.D0
              IF(FP32.GT.-FXMATPAR(ISEG2)) FP32=-FXMATPAR(ISEG2)
            ENDIF
          ENDIF
!
!         SEGMENT 3
!
          IF(ORISEG(IELEM,3).EQ.1) THEN
            IF(FXMATPAR(ISEG3).GT.0.D0) THEN
              FP31=0.D0
              IF(FP13.GT. FXMATPAR(ISEG3)) FP13=FXMATPAR(ISEG3)
            ELSE
              FP13=0.D0
              IF(FP31.GT.-FXMATPAR(ISEG3)) FP31=-FXMATPAR(ISEG3)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG3).GT.0.D0) THEN
              FP13=0.D0
              IF(FP31.GT.FXMATPAR(ISEG3)) FP31=FXMATPAR(ISEG3)
            ELSE
              FP31=0.D0
              IF(FP13.GT.-FXMATPAR(ISEG3)) FP13=-FXMATPAR(ISEG3)
            ENDIF
          ENDIF
!
!         END OF CORRECTION OF FLUXES
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
!         MINIMUM OF THE 1-TETA
!
          MT1=1.D0-TETA(I1)
          MT2=1.D0-TETA(I2)
          MT3=1.D0-TETA(I3)
          MIN12=MIN(MT1,MT2)
          MIN13=MIN(MT1,MT3)
          MIN23=MIN(MT2,MT3)
!
!         PART OF CONTRIBUTIONS THAT WILL NOT BE LIMITED, IMMEDIATELY ASSEMBLED
!
          FI_I(I1)=FI_I(I1)+FP12*(FN1*(MT1-MIN12)-FN2*(MT2-MIN12))
     &                     +FP13*(FN1*(MT1-MIN13)-FN3*(MT3-MIN13))
          FI_I(I2)=FI_I(I2)+FP21*(FN2*(MT2-MIN12)-FN1*(MT1-MIN12))
     &                     +FP23*(FN2*(MT2-MIN23)-FN3*(MT3-MIN23))
          FI_I(I3)=FI_I(I3)+FP31*(FN3*(MT3-MIN13)-FN1*(MT1-MIN13))
     &                     +FP32*(FN3*(MT3-MIN23)-FN2*(MT2-MIN23))
!
!         NOW PART OF CONTRIBUTIONS THAT WILL BE LIMITED
!
          COEF=SU(IELEM)*TIERS
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!         HDFDT MUST BE ((1.D0-TETA)*H+TETA*HN)*(FSTAR-F)/DDT
!
          FINCORR1=
     &    HDFDT(I1)*COEF+FP12*(FN1-FN2)*MIN12+FP13*(FN1-FN3)*MIN13
          FINCORR2=
     &    HDFDT(I2)*COEF+FP21*(FN2-FN1)*MIN12+FP23*(FN2-FN3)*MIN23
          FINCORR3=
     &    HDFDT(I3)*COEF+FP31*(FN3-FN1)*MIN13+FP32*(FN3-FN2)*MIN23
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(PHITCOR.GT.EPSPHI) THEN
!           PSI REDUCTION
            BETA1=MAX(FINCORR1,0.D0)
            BETA2=MAX(FINCORR2,0.D0)
            BETA3=MAX(FINCORR3,0.D0)
            COEF=PHITCOR/(BETA1+BETA2+BETA3)
            FI_I(I1)=FI_I(I1)+BETA1*COEF
            FI_I(I2)=FI_I(I2)+BETA2*COEF
            FI_I(I3)=FI_I(I3)+BETA3*COEF
          ELSEIF(PHITCOR.LT.-EPSPHI) THEN
!           PSI REDUCTION
            BETA1=MIN(FINCORR1,0.D0)
            BETA2=MIN(FINCORR2,0.D0)
            BETA3=MIN(FINCORR3,0.D0)
            COEF=PHITCOR/(BETA1+BETA2+BETA3)
            FI_I(I1)=FI_I(I1)+BETA1*COEF
            FI_I(I2)=FI_I(I2)+BETA2*COEF
            FI_I(I3)=FI_I(I3)+BETA3*COEF
          ELSE
!           NO REDUCTION
            FI_I(I1)=FI_I(I1)+FINCORR1
            FI_I(I2)=FI_I(I2)+FINCORR2
            FI_I(I3)=FI_I(I3)+FINCORR3
          ENDIF
!
        ENDDO
!
      ELSEIF(YAFLULIM) THEN
!
!       THE SAME BUT WITH FLUX LIMITATION GIVEN
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
          ISEG1=ELTSEG(IELEM,1)
          ISEG2=ELTSEG(IELEM,2)
          ISEG3=ELTSEG(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK
!         I.E. FIJ>0 MEANS FLUX FROM J TO I !!!!!
!
          FP12=MAX(MIN(K1,-K2),0.D0)*FLULIM(ISEG1)
          FP23=MAX(MIN(K2,-K3),0.D0)*FLULIM(ISEG2)
          FP31=MAX(MIN(K3,-K1),0.D0)*FLULIM(ISEG3)
          FP21=MAX(MIN(K2,-K1),0.D0)*FLULIM(ISEG1)
          FP32=MAX(MIN(K3,-K2),0.D0)*FLULIM(ISEG2)
          FP13=MAX(MIN(K1,-K3),0.D0)*FLULIM(ISEG3)
!
!         CORRECTING THE FLUXES WHEN THEIR SIGN IS NOT THE SAME
!         AS THE ASSEMBLED VALUE
!
!         SEGMENT 1
!
          IF(ORISEG(IELEM,1).EQ.1) THEN
            IF(FXMATPAR(ISEG1).GT.0.D0) THEN
              FP12=0.D0
              IF(FP21.GT. FXMATPAR(ISEG1)) FP21=FXMATPAR(ISEG1)
            ELSE
              FP21=0.D0
              IF(FP12.GT.-FXMATPAR(ISEG1)) FP12=-FXMATPAR(ISEG1)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG1).GT.0.D0) THEN
              FP21=0.D0
              IF(FP12.GT. FXMATPAR(ISEG1)) FP12=FXMATPAR(ISEG1)
            ELSE
              FP12=0.D0
              IF(FP21.GT.-FXMATPAR(ISEG1)) FP21=-FXMATPAR(ISEG1)
            ENDIF
          ENDIF
!
!         SEGMENT 2
!
          IF(ORISEG(IELEM,2).EQ.1) THEN
            IF(FXMATPAR(ISEG2).GT.0.D0) THEN
              FP23=0.D0
              IF(FP32.GT. FXMATPAR(ISEG2)) FP32=FXMATPAR(ISEG2)
            ELSE
              FP32=0.D0
              IF(FP23.GT.-FXMATPAR(ISEG2)) FP23=-FXMATPAR(ISEG2)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG2).GT.0.D0) THEN
              FP32=0.D0
              IF(FP23.GT. FXMATPAR(ISEG2)) FP23=FXMATPAR(ISEG2)
            ELSE
              FP23=0.D0
              IF(FP32.GT.-FXMATPAR(ISEG2)) FP32=-FXMATPAR(ISEG2)
            ENDIF
          ENDIF
!
!         SEGMENT 3
!
          IF(ORISEG(IELEM,3).EQ.1) THEN
            IF(FXMATPAR(ISEG3).GT.0.D0) THEN
              FP31=0.D0
              IF(FP13.GT. FXMATPAR(ISEG3)) FP13=FXMATPAR(ISEG3)
            ELSE
              FP13=0.D0
              IF(FP31.GT.-FXMATPAR(ISEG3)) FP31=-FXMATPAR(ISEG3)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG3).GT.0.D0) THEN
              FP13=0.D0
              IF(FP31.GT.FXMATPAR(ISEG3)) FP31=FXMATPAR(ISEG3)
            ELSE
              FP31=0.D0
              IF(FP13.GT.-FXMATPAR(ISEG3)) FP13=-FXMATPAR(ISEG3)
            ENDIF
          ENDIF
!
!         END OF CORRECTION OF FLUXES
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
!         MINIMUM OF THE 1-TETA
!
          MT1=1.D0-TETA(I1)
          MT2=1.D0-TETA(I2)
          MT3=1.D0-TETA(I3)
          MIN12=MIN(MT1,MT2)
          MIN13=MIN(MT1,MT3)
          MIN23=MIN(MT2,MT3)
!
!         PART OF CONTRIBUTIONS THAT WILL NOT BE LIMITED, IMMEDIATELY ASSEMBLED
!
          FI_I(I1)=FI_I(I1)+FP12*(FN1*(MT1-MIN12)-FN2*(MT2-MIN12))
     &                     +FP13*(FN1*(MT1-MIN13)-FN3*(MT3-MIN13))
          FI_I(I2)=FI_I(I2)+FP21*(FN2*(MT2-MIN12)-FN1*(MT1-MIN12))
     &                     +FP23*(FN2*(MT2-MIN23)-FN3*(MT3-MIN23))
          FI_I(I3)=FI_I(I3)+FP31*(FN3*(MT3-MIN13)-FN1*(MT1-MIN13))
     &                     +FP32*(FN3*(MT3-MIN23)-FN2*(MT2-MIN23))
!
!         NOW PART OF CONTRIBUTIONS THAT WILL BE LIMITED
!
          COEF=SU(IELEM)*TIERS
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!         HDFDT MUST BE ((1.D0-TETA)*H+TETA*HN)*(FSTAR-F)/DDT
!
          FINCORR1=HDFDT(I1)*COEF
     &            +FP12*(FN1-FN2)*MIN12+FP13*(FN1-FN3)*MIN13
          FINCORR2=HDFDT(I2)*COEF
     &            +FP21*(FN2-FN1)*MIN12+FP23*(FN2-FN3)*MIN23
          FINCORR3=HDFDT(I3)*COEF
     &            +FP31*(FN3-FN1)*MIN13+FP32*(FN3-FN2)*MIN23
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(PHITCOR.GT.EPSPHI) THEN
!           PSI REDUCTION
            BETA1=MAX(FINCORR1,0.D0)
            BETA2=MAX(FINCORR2,0.D0)
            BETA3=MAX(FINCORR3,0.D0)
            COEF=PHITCOR/(BETA1+BETA2+BETA3)
            FI_I(I1)=FI_I(I1)+BETA1*COEF
            FI_I(I2)=FI_I(I2)+BETA2*COEF
            FI_I(I3)=FI_I(I3)+BETA3*COEF
          ELSEIF(PHITCOR.LT.-EPSPHI) THEN
!           PSI REDUCTION
            BETA1=MIN(FINCORR1,0.D0)
            BETA2=MIN(FINCORR2,0.D0)
            BETA3=MIN(FINCORR3,0.D0)
            COEF=PHITCOR/(BETA1+BETA2+BETA3)
            FI_I(I1)=FI_I(I1)+BETA1*COEF
            FI_I(I2)=FI_I(I2)+BETA2*COEF
            FI_I(I3)=FI_I(I3)+BETA3*COEF
          ELSE
!           NO REDUCTION
            FI_I(I1)=FI_I(I1)+FINCORR1
            FI_I(I2)=FI_I(I2)+FINCORR2
            FI_I(I3)=FI_I(I3)+FINCORR3
          ENDIF
!
        ENDDO
!
      ELSE
!
!       HERE NO FLUX LIMITATION
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
          ISEG1=ELTSEG(IELEM,1)
          ISEG2=ELTSEG(IELEM,2)
          ISEG3=ELTSEG(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK
!         I.E. FIJ>0 MEANS FLUX FROM J TO I !!!!!
!
          FP12=MAX(MIN(K1,-K2),0.D0)
          FP23=MAX(MIN(K2,-K3),0.D0)
          FP31=MAX(MIN(K3,-K1),0.D0)
          FP21=MAX(MIN(K2,-K1),0.D0)
          FP32=MAX(MIN(K3,-K2),0.D0)
          FP13=MAX(MIN(K1,-K3),0.D0)
!
!         CORRECTING THE FLUXES WHEN THEIR SIGN IS NOT THE SAME
!         AS THE ASSEMBLED VALUE, KNOWING THAT ALL THE FPIJ ARE
!         POSITIVE BY CONSTRUCTION
!
!         SEGMENT 1
!
          IF(ORISEG(IELEM,1).EQ.1) THEN
            IF(FXMATPAR(ISEG1).GT.0.D0) THEN
              FP12=0.D0
              IF(FP21.GT. FXMATPAR(ISEG1)) FP21=FXMATPAR(ISEG1)
            ELSE
              FP21=0.D0
              IF(FP12.GT.-FXMATPAR(ISEG1)) FP12=-FXMATPAR(ISEG1)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG1).GT.0.D0) THEN
              FP21=0.D0
              IF(FP12.GT. FXMATPAR(ISEG1)) FP12=FXMATPAR(ISEG1)
            ELSE
              FP12=0.D0
              IF(FP21.GT.-FXMATPAR(ISEG1)) FP21=-FXMATPAR(ISEG1)
            ENDIF
          ENDIF
!
!         SEGMENT 2
!
          IF(ORISEG(IELEM,2).EQ.1) THEN
            IF(FXMATPAR(ISEG2).GT.0.D0) THEN
              FP23=0.D0
              IF(FP32.GT. FXMATPAR(ISEG2)) FP32=FXMATPAR(ISEG2)
            ELSE
              FP32=0.D0
              IF(FP23.GT.-FXMATPAR(ISEG2)) FP23=-FXMATPAR(ISEG2)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG2).GT.0.D0) THEN
              FP32=0.D0
              IF(FP23.GT. FXMATPAR(ISEG2)) FP23=FXMATPAR(ISEG2)
            ELSE
              FP23=0.D0
              IF(FP32.GT.-FXMATPAR(ISEG2)) FP32=-FXMATPAR(ISEG2)
            ENDIF
          ENDIF
!
!         SEGMENT 3
!
          IF(ORISEG(IELEM,3).EQ.1) THEN
            IF(FXMATPAR(ISEG3).GT.0.D0) THEN
              FP31=0.D0
              IF(FP13.GT. FXMATPAR(ISEG3)) FP13=FXMATPAR(ISEG3)
            ELSE
              FP13=0.D0
              IF(FP31.GT.-FXMATPAR(ISEG3)) FP31=-FXMATPAR(ISEG3)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG3).GT.0.D0) THEN
              FP13=0.D0
              IF(FP31.GT.FXMATPAR(ISEG3)) FP31=FXMATPAR(ISEG3)
            ELSE
              FP31=0.D0
              IF(FP13.GT.-FXMATPAR(ISEG3)) FP13=-FXMATPAR(ISEG3)
            ENDIF
          ENDIF
!
!         END OF CORRECTION OF FLUXES
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
!         MINIMUM OF THE 1-TETA
!
          MT1=1.D0-TETA(I1)
          MT2=1.D0-TETA(I2)
          MT3=1.D0-TETA(I3)
          MIN12=MIN(MT1,MT2)
          MIN13=MIN(MT1,MT3)
          MIN23=MIN(MT2,MT3)
!
!         PART OF CONTRIBUTIONS THAT WILL NOT BE LIMITED, IMMEDIATELY ASSEMBLED
!
          FI_I(I1)=FI_I(I1)+FP12*(FN1*(MT1-MIN12)-FN2*(MT2-MIN12))
     &                     +FP13*(FN1*(MT1-MIN13)-FN3*(MT3-MIN13))
          FI_I(I2)=FI_I(I2)+FP21*(FN2*(MT2-MIN12)-FN1*(MT1-MIN12))
     &                     +FP23*(FN2*(MT2-MIN23)-FN3*(MT3-MIN23))
          FI_I(I3)=FI_I(I3)+FP31*(FN3*(MT3-MIN13)-FN1*(MT1-MIN13))
     &                     +FP32*(FN3*(MT3-MIN23)-FN2*(MT2-MIN23))
!
!         NOW PART OF CONTRIBUTIONS THAT WILL BE LIMITED
!
          COEF=SU(IELEM)*TIERS
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!         HDFDT MUST BE ((1.D0-TETA)*H+TETA*HN)*(FSTAR-F)/DDT
!
          FINCORR1=
     &    HDFDT(I1)*COEF+FP12*(FN1-FN2)*MIN12+FP13*(FN1-FN3)*MIN13
          FINCORR2=
     &    HDFDT(I2)*COEF+FP21*(FN2-FN1)*MIN12+FP23*(FN2-FN3)*MIN23
          FINCORR3=
     &    HDFDT(I3)*COEF+FP31*(FN3-FN1)*MIN13+FP32*(FN3-FN2)*MIN23
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(PHITCOR.GT.EPSPHI) THEN
!           PSI REDUCTION
            BETA1=MAX(FINCORR1,0.D0)
            BETA2=MAX(FINCORR2,0.D0)
            BETA3=MAX(FINCORR3,0.D0)
            COEF=PHITCOR/(BETA1+BETA2+BETA3)
            FI_I(I1)=FI_I(I1)+BETA1*COEF
            FI_I(I2)=FI_I(I2)+BETA2*COEF
            FI_I(I3)=FI_I(I3)+BETA3*COEF
          ELSEIF(PHITCOR.LT.-EPSPHI) THEN
!           PSI REDUCTION
            BETA1=MIN(FINCORR1,0.D0)
            BETA2=MIN(FINCORR2,0.D0)
            BETA3=MIN(FINCORR3,0.D0)
            COEF=PHITCOR/(BETA1+BETA2+BETA3)
            FI_I(I1)=FI_I(I1)+BETA1*COEF
            FI_I(I2)=FI_I(I2)+BETA2*COEF
            FI_I(I3)=FI_I(I3)+BETA3*COEF
          ELSE
!           NO REDUCTION
            FI_I(I1)=FI_I(I1)+FINCORR1
            FI_I(I2)=FI_I(I2)+FINCORR2
            FI_I(I3)=FI_I(I3)+FINCORR3
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

