!                   *********************
                    SUBROUTINE NONNEWT_FV
!                   *********************
!
     &(IS,UA,AKAP)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    COMPUTE FRICTION SLOPE SOURCE TERM FOR NON-NEWTONIAN FLUID.
!         SEMI-IMPLICIT FV TREATMENT.
!
!note     NON-NEWTONIAN MODELS : (1) BINGHAM WITH 3 DIFFERENT TREATMENT
!                                    OPTIONS
!                                (2) HERSCHEL-BULKLEY
!
!history  P-L. LIGIER (SWECO)
!+        22/06/2020
!+        First version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      IS     NODE NUMBER
!>@param  [in]      UA     (H,HU,HV) AT TN+1
!>@param  [in,out]  AKAP   SOURCE TERM COEFFICIENT USED IN SOURCE_MOMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: ROEAU,DT,NPOIN,HN,QU,QV,TN,
     &                                  NONNEWTMODEL,OPTBGHM,NN_VISC,
     &                                  NN_YIELD,NN_K,NN_N,NN_BIPHASIC,
     &                                  NN_RHO,IND_NN
      USE INTERFACE_TELEMAC2D, EX_NONNEW => NONNEWT_FV
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IS
      DOUBLE PRECISION, INTENT(IN)    :: UA(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: AKAP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER REALS,I
      DOUBLE PRECISION VMIN,AA,MUZERO,KB,SRXY,MUEFF
      DOUBLE PRECISION VNORM,ACOF,BCOF,CCOF,DCOF,TAU0,UNKWN(3),M
      DOUBLE PRECISION NN_CV,NN_A,NN_B,NN_C,NN_D,RHO
      DOUBLE PRECISION UNKWN_DIFF(3),THEOVAL
!
!-----------------------------------------------------------------------
!
!     NEWTONIAN BEHAVIOUR ASSUMED WHERE FLOW IS INFINITELY SHALLOW
!
      IF((HN%R(IS).LE.1.D-12).OR.
     &   (UA(1,IS).LE.1.D-12)) THEN ! WE DO NOTHING
      ELSE
!
!     NON-NEWTONIAN BEHAVIOUR
!
      VNORM = SQRT( QU%R(IS)**2 + QV%R(IS)**2 ) / HN%R(IS)
      VMIN = 1.D-12
!
!     PSEUDO-BIPHASIC MODEL
!     ---------------------
!     COEFFS FOR EMPIRICAL RELATIONSHIPS FOR YIELD STRESS & VISCOSITY
!       NN_YIELD = NN_A * 10**(NN_B*NN_CV)
!       NN_VISC  = NN_C * 10**(NN_D*NN_CV)
!
!     TO BE DEFINED BY USER:
      NN_A = 0.025D0
      NN_B = 8.D0
      NN_C = 0.001D0
      NN_D = 8.D0
!
      IF(NN_BIPHASIC) THEN
!       VOLUMETRIC CONCENTRATION CV
        NN_CV = TN%ADR(IND_NN)%P%R(IS)
        RHO = NN_CV * (NN_RHO-ROEAU) + ROEAU
!       UPDATE YIELD STRESS AND VISCOSITY BASED ON LOCAL CV
        NN_YIELD = NN_A*10.D0**(NN_B*NN_CV)
        NN_VISC  = NN_C*10.D0**(NN_D*NN_CV)
      ELSE
        RHO = NN_RHO
      ENDIF
!
!     BINGHAM MODEL (NON-NEWTONIAN MODEL = 1)
!     -------------
!
!     SHEAR STRESS = NN_YIELD + NN_VISC * SRXY
!
      IF(NONNEWTMODEL.EQ.1) THEN
!
!       OPTION 1: PAPANASTASIOU'S EXPONENTIAL REGULARIZATION (1987)
        IF(OPTBGHM.EQ.1) THEN
          M = 1.D3
!
!         COMPUTES STRAIN RATE DU/DZ = K/8*U/H
!         DEFAULT K=24, PARABOLIC PROFILE FOR LAMINAR FLOW
          SRXY = MAX(NN_K,24.D0) * VNORM / (8.D0 * HN%R(IS))
!
          AKAP = AKAP + DT*(NN_VISC*SRXY+NN_YIELD*(1.D0-EXP(-M*SRXY)))/
     &                      (RHO*HN%R(IS)*MAX(VNORM,VMIN))
!
!
!       OPTION 2: EFFECTIVE VISCOSITY WITH CROSS FORMULATION
!                 (SHAO & LO, 2003)
        ELSEIF(OPTBGHM.EQ.2) THEN
          AA = 1.D3
          MUZERO = AA*NN_VISC
          KB = MUZERO / MAX(NN_YIELD,1.D-6)
!
!         COMPUTES STRAIN RATE DU/DZ = K/8*U/H
!         DEFAULT K=24, PARABOLIC PROFILE FOR LAMINAR FLOW
          SRXY = MAX(NN_K,24.D0) * VNORM / (8.D0 * HN%R(IS))
!
!         COMPUTES EFFECTIVE VISCSITY WITH CROSS FORMULATION
          MUEFF = (MUZERO + NN_VISC*KB*SRXY) / (1.D0 + KB*SRXY)
!
          AKAP = AKAP + DT * SRXY * MUEFF /
     &                       (RHO * HN%R(IS)*MAX(VNORM,VMIN))
!
!
!       OPTION 3: CUBIC EQUATION FOR SHEAR STRESS (RICKENMANN, 1990)
        ELSEIF(OPTBGHM.EQ.3) THEN
          ACOF = 2.D0
          BCOF =-3.D0*(NN_YIELD+2.D0*NN_VISC*MAX(VNORM,VMIN)/HN%R(IS))
          CCOF = 0.D0
          DCOF = NN_YIELD**3
          CALL CUBEEQUATION(ACOF,BCOF,CCOF,DCOF,REALS,UNKWN)
          IF(REALS.EQ.1) TAU0 = UNKWN(1)
          IF(REALS.EQ.3) THEN
!           SELECTING THE POSITIVE ROOT CLOSEST FROM THEORETICAL TAU0
            UNKWN_DIFF = (/9.9D99,9.99D99,9.99D99/)
            THEOVAL = NN_YIELD+3.D0*MAX(VNORM,VMIN)/HN%R(IS)*NN_VISC
            DO I=1,3
              IF(UNKWN(I).GT.0.D0) UNKWN_DIFF(I)=ABS(UNKWN(I)-THEOVAL)
            ENDDO
            TAU0 = UNKWN(MINLOC(UNKWN_DIFF,1))
          ENDIF
!         RAISE ERROR IF NO POSITIVE ROOT
          IF(TAU0.LT.0.D0) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) 'NONNEWT_FV: NO POSITIVE ROOT FOR TAU0'
            WRITE(LU,*) '            IN CUBIC EQUATION AT NODE',IS
            WRITE(LU,*) ' '
            CALL PLANTE(1)
            STOP
          ENDIF
!
          AKAP = AKAP + DT * TAU0 / (RHO*HN%R(IS)*MAX(VNORM,VMIN))
!
        ELSE
          WRITE(LU,*) ' '
          WRITE(LU,*) 'NONNEWT_FV: BINGHAM OPTION NOT IMPLEMENTED'
          WRITE(LU,*) '            AVAILABLE OPTIONS: 1, 2 OR 3'
          WRITE(LU,*) ' '
          CALL PLANTE(1)
          STOP
        ENDIF
!
!     HERSCHEL-BULKLEY MODEL:  (NON-NEWTONIAN MODEL = 2)
!     ----------------------
!
!     SHEAR STRESS = NN_YIELD + K * SRXY**NN_N
!     NOTE: CONSISTENCY INDEX K IS TAKEN FROM NN_VISC
!
      ELSEIF(NONNEWTMODEL.EQ.2) THEN
!       USES PAPANASTASIOU'S EXPONENTIAL REGULARIZATION (1987)
        M = 1.D3
!
!       COMPUTES STRAIN RATE DU/DZ = K/8*U/H
!       DEFAULT K=24, PARABOLIC PROFILE FOR LAMINAR FLOW
        SRXY = MAX(NN_K,24.D0) * VNORM / (8.D0 * HN%R(IS))
!
        AKAP = AKAP+(NN_VISC*SRXY**NN_N+NN_YIELD*(1.D0-EXP(-M*SRXY)))*
     &               DT / (RHO*HN%R(IS)*MAX(VNORM,VMIN))
!
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*) 'NONNEWT_FV: NON-NEWTONIAN MODEL NOT IMPLEMENTED'
        WRITE(LU,*) '            AVAILABLE OPTIONS:'
        WRITE(LU,*) '            1: BINGHAM, 2: HERSCHEL-BULKLEY'
        WRITE(LU,*) ' '
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
