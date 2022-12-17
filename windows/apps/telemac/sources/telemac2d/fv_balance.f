!                       *********************
                        SUBROUTINE FV_BALANCE
!                       *********************
!
     & (MASSES,MASS_RAIN,YASMH,SMH,H,QU,QV,FLUX,FLUX_OLD,
     &  W,HT,U,V,T,FLUXT_OLD,MASSOU)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief  Computes mass balance and write state variables
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  FLUX       FLUX AT TIME N
!>@param  [in,out]  FLUX_OLD   FLUX AT TIME N-1
!>@param  [in,out]  FLUXT_OLD  FLUX FOR TRACER AT TIME N-1
!>@param  [in,out]  H          WATER DEPTH AT TIME N+1
!>@param  [in,out]  MASS_RAIN  MASS ADDED BY RAIN OR EVAPORATION
!>@param  [in,out]  MASSES     ADDED MASS BY SOURCE TERMS
!>@param  [in,out]  MASSOU     ADDED TRACER MASS BY SOURCE TERM
!>@param  [in,out]  T          TRACER
!>@param  [in,out]  HT         H*TRACER
!>@param  [in,out]  QU         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  QV         FLOW COMPONENTS AT TN THEN AT TN+1
!>@param  [in,out]  SMH        SOURCE TERMS FOR CONTINUITY EQUATION
!>@param  [in,out]  U          X-VELOCITY COMPONENTS AT TIME N+1
!>@param  [in,out]  V          Y-VELOCITY COMPONENTS AT TIME N+1
!>@param  [in,out]  W          WORKING TABLE (H,HU,HV)
!>@param  [in]      YASMH      LOGICAL: TO TAKE INTO ACCOUNT SMH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY:MSK,MASKEL,T6,IND_SED,BILMAS,
     &                            RAIN,NPOIN,NTRAC,DT,PLUIE,SMTR,FLUXT,
     &                            MESH,EPS_FV
      USE DECLARATIONS_TELEMAC, ONLY:COUPLING
      USE DECLARATIONS_GAIA, ONLY:FLUDPT,FLUDP,NSUSP_TEL
      USE INTERFACE_TELEMAC2D, EX_FV_BALANCE => FV_BALANCE
      USE INTERFACE_PARALLEL, ONLY : P_SUM
!
      IMPLICIT NONE
      LOGICAL, INTENT(IN) :: YASMH
      DOUBLE PRECISION, INTENT(INOUT) :: MASSES,MASS_RAIN,SMH(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN),U(NPOIN),V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HT,FLUXT_OLD
!
      INTEGER :: I, ITRAC, ISUSP

!
!-----------------------------------------------------------------------
!  COMPUTES VOLUME ADDED BY SOURCES
!-----------------------------------------------------------------------
!
      IF(BILMAS)THEN
        MASSES   = 0.D0
        MASS_RAIN= 0.D0
!       IF SOURCE TERMS (EXCEPT RAIN AND EVAPORATION)
        IF(YASMH) THEN
          DO  I=1,NPOIN
            MASSES = MASSES + SMH(I)
          ENDDO
        ENDIF
!       RAIN AND EVAPORATION
        IF(RAIN)THEN
          CALL VECTOR(T6,'=','MASVEC          ',PLUIE%ELM,
     &                1.D0,PLUIE,PLUIE,PLUIE,PLUIE,PLUIE,PLUIE,MESH,
     &                MSK,MASKEL)
          MASS_RAIN =BIEF_SUM(T6)
        ENDIF
        MASSES = DT*(MASSES + MASS_RAIN)
      ENDIF
!
!---------------------------------------------------------------------
!     REPARE HYDRO VARIABLES FOR OUTPUT AND FOR NEXT TIME STEP
!---------------------------------------------------------------------
!
      DO I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
!
!       SAVE FLUXES FOR NEXT TIME STEP
        FLUX_OLD(I,1) = FLUX(I,1)
        FLUX_OLD(I,2) = FLUX(I,2)
        FLUX_OLD(I,3) = FLUX(I,3)
!
!       COMPUTE U AND V
        IF (H(I).GT.EPS_FV) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
      ENDDO
!
!---------------------------------------------------------------------
!     PREPARE TRACERS FOR OUTPUT AND FOR NEXT TIME STEP
!---------------------------------------------------------------------
!
      IF (NTRAC.GE.0) THEN
        DO ITRAC=1,NTRAC
          DO I=1,NPOIN
!           SAVE TRACER FLUXES FOR NEXT TIME STEP
            FLUXT_OLD%ADR(ITRAC)%P%R(I) = FLUXT%ADR(ITRAC)%P%R(I)
!
!           COMPUTE T
            IF (H(I).GT.EPS_FV) THEN
              T%ADR(ITRAC)%P%R(I) = HT%ADR(ITRAC)%P%R(I) / H(I)
            ELSE
              T%ADR(ITRAC)%P%R(I) = 0.D0
            ENDIF
!
!           FINAL BALANCE OF TRACER
            MASSOU(ITRAC) = MASSOU(ITRAC) + DT*SMTR%ADR(ITRAC)%P%R(I)
          ENDDO
!
          IF(INCLUS(COUPLING,'GAIA').AND.
     &       (ITRAC.GE.IND_SED.AND.ITRAC.LT.IND_SED+NSUSP_TEL)) THEN
            ISUSP=ITRAC-IND_SED+1
            CALL OS('X=YZ    ',X=FLUDP%ADR(ISUSP)%P,
     &              Y=FLUDPT%ADR(ISUSP)%P,Z=T%ADR(ITRAC)%P)
            CALL OS('X=+(Y,C)',X=FLUDP%ADR(ISUSP)%P,
     &              Y=FLUDP%ADR(ISUSP)%P,C=0.D0)
          ENDIF
!
          IF(NCSIZE.GT.1)MASSOU(ITRAC)=P_SUM(MASSOU(ITRAC))
!
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
