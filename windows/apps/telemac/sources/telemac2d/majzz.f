!                       ****************
                        SUBROUTINE MAJZZ
!                       ****************
!
     &  (W,HT,FLUX,FLUX_OLD,QU,QV,LIMPRO)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief      TIME INTEGRATION WITH NEWMARK SCHEME:
!!            U_(N+1)=U_N + DT*( (1-GAMMA)ACC_N +GAMMA*ACC_(N+1))
!!            ACC: IS THE ACCELERATION (FLUX BALANCE FOR FV)
!!            FOR GAMMA=0.5 THE SCHEME IS SECOND ORDER ACCURATE
!!            FOR GAMMA=1.0 THE SCHEME IS EULER EXPLICIT (FIRST ORDER)
!
!
!>@history  R. ATA (EDF-LNHE)
!!        03/15/2011
!!        V6P1
!!    CHANGE EXPLICIT EULER BY NEWMARK SCHEME
!!    GAMMA FIXES THE SCHEME ACCURACY (SEE BELOW)
!
!>@history  R. ATA (EDF-LNHE)
!!        01/07/2013
!!        V6P3
!!    cleaning
!
!>@history  R. ATA (EDF-LNHE)
!!        01/25/2013
!!        V7p0
!!    add normal projection of flux for liquid
!!    boundaries
!!    warning: its impact on mass balance is not taken into account
!!             to be considered for next release
!
!>@history  R. ATA
!!        25/12/2016
!!        V7P2
!!    include rain and evaporation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]      FLUX      FLUX AT TIME N
!>@param  [in]      FLUX_OLD  FLUX AT TIME N-1
!>@param  [in]      LIMPRO    BC TYPE
!>@param  [in,out]  QU        HU AT TIME TN
!>@param  [in,out]  QV        HV AT TIME TN
!>@param  [in,out]  W         (H,HU,HV)
!>@param  [in,out]  HT        HT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_MAJZZ => MAJZZ
      USE DECLARATIONS_TELEMAC2D, ONLY: NPOIN,LT,NPTFR,NTRAC,ICIN,
     &                            TORDER,MESH,RAIN,DT,GAMMA,V2DPAR,HN,
     &                            GRAV,PLUIE,H,FLUXT_OLD,SMTR,FLUXT,
     &                            SMH,EPS_FV
      USE DECLARATIONS_TELEMAC, ONLY: KNEU
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN),FLUX(NPOIN,3)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX_OLD(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: QU(NPOIN),QV(NPOIN)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: HT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,ITRAC
      DOUBLE PRECISION FACT,UNMGAMMA,UGN,W1,XNC
!
!=======================
!---- TEST FOR DT
!=======================
!
      IF(DT.LE.0.D0) THEN
        WRITE(LU,*)'*********************************************'
        WRITE(LU,*)'          WARNING: TIME STEP =0'
        WRITE(LU,*)'          IN MAJZZ SUBROUTINE...'
        WRITE(LU,*)'*********************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!      PROJECTION ON THE NORMAL TO ELIMINATE THE TANGENT COMPONENT
!      ONLY FOR LIQUID BOUNDARY
!
      IF(ICIN.NE.0) THEN
        DO K=1,NPTFR
          I=MESH%NBOR%I(K)
          IF(LIMPRO(K,1).NE.KNEU) THEN
            !NORMALIZED NORMAL
            UGN =MESH%XNEBOR%R(K)*FLUX(I,2)+MESH%YNEBOR%R(K)*FLUX(I,3) ! TO RETRIEVE NORMAL COMPONENET OF UG
!           VGN =  0.D0  ! PUT TANGENTIAL COMPENENT =0
!           INVERSE ROTATION
            FLUX(I,2) = MESH%XNEBOR%R(K)*UGN
            FLUX(I,3) = MESH%YNEBOR%R(K)*UGN
          ENDIF
        ENDDO
      ENDIF
!
!++++++++++++++++++++++++++++++++++++
! TIME INTEGRATION
!++++++++++++++++++++++++++++++++++++
!
      IF(TORDER.EQ.1) THEN
!
!==========================
!---- EULER EXPLICIT SCHEME
!==========================
!
        DO I=1,NPOIN
          FACT=DT/V2DPAR%R(I)
!
!         HYDRO UPDATE
          W(1,I) = HN%R(I) + FACT*(FLUX(I,1)+SMH%R(I))
          IF(RAIN)W(1,I)=W(1,I)+DT*PLUIE%R(I)
          W(2,I) = QU(I) + FACT*FLUX(I,2)
          W(3,I) = QV(I) + FACT*FLUX(I,3)

!         TRACER UPDATE
          IF (NTRAC.GT.0) THEN
            DO ITRAC=1,NTRAC
              HT%ADR(ITRAC)%P%R(I) = HT%ADR(ITRAC)%P%R(I)
     &            + FACT*(FLUXT%ADR(ITRAC)%P%R(I)
     &                   +SMTR%ADR(ITRAC)%P%R(I))
            ENDDO
          ENDIF
!
        ENDDO
!
      ELSEIF(TORDER.EQ.2) THEN
!
!==========================
!---- NEWMARK SCHEME
!==========================
!
        IF (GAMMA.LE.0.D0.OR.GAMMA.GE.1.D0) THEN
          WRITE(LU,*) 'MAJZZ: ERROR:NEWMARK COEFFICIENT MUST...'
          WRITE(LU,*) '... BE BETWEEN 0 AND 1: ',GAMMA
          CALL PLANTE(1)
          STOP
        ENDIF
!
        UNMGAMMA = 1.D0-GAMMA
!
!       - FOR GAMMA=0.5, THIS CHOICE GIVES ORDER 2 ACCURACY AND
!         THE SCHEME IS UNCONDITIALLY STABLE
!       - FOR USER WHO PREFERS (EULER) EXPLICIT SCHEME,
!         YOU HAVE TO PUT GAMMA=1
        DO I=1,NPOIN
          FACT=DT/V2DPAR%R(I)
!
!         FIRST TIME STEP
!         ---------------
          IF(LT.EQ.1)THEN
!           HYDRO UPDATE
            W(1,I) = HN%R(I) + FACT*(FLUX(I,1)+SMH%R(I) )
            IF(RAIN)W(1,I)=W(1,I)+DT*PLUIE%R(I)
            W(2,I) = QU(I) + FACT* FLUX(I,2)
            W(3,I) = QV(I) + FACT* FLUX(I,3)
!
!           TRACER UPDATE
            IF (NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                HT%ADR(ITRAC)%P%R(I) = HT%ADR(ITRAC)%P%R(I)
     &              + FACT*(FLUXT%ADR(ITRAC)%P%R(I)
     &                     +SMTR%ADR(ITRAC)%P%R(I))
              ENDDO
            ENDIF
!
!         ----------------
          ELSE
!           HYDRO UPDATE
            W(1,I) = HN%R(I) + FACT*(UNMGAMMA*FLUX_OLD(I,1) +
     &                             GAMMA*FLUX(I,1)+SMH%R(I))
            IF(RAIN)W(1,I)=W(1,I)+DT*PLUIE%R(I)
            W(2,I) = QU(I) + FACT*(UNMGAMMA*FLUX_OLD(I,2) +
     &                       GAMMA*FLUX(I,2))
            W(3,I) = QV(I) + FACT*(UNMGAMMA*FLUX_OLD(I,3) +
     &                       GAMMA*FLUX(I,3))
!
!           TRACER UPDATE
            IF (NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                HT%ADR(ITRAC)%P%R(I) = HT%ADR(ITRAC)%P%R(I)
     &              + FACT*(UNMGAMMA*FLUXT_OLD%ADR(ITRAC)%P%R(I)
     &                     +GAMMA*FLUXT%ADR(ITRAC)%P%R(I)
     &                     +SMTR%ADR(ITRAC)%P%R(I))
              ENDDO
            ENDIF
!
          ENDIF
        ENDDO
      ENDIF
!
!     PROJECTION ON THE SLIPPING BOUNDARY CONDITIONS
!     **********************************************
!
      IF(ICIN.NE.0) THEN
!
        CALL CDLPROJ(NPOIN,NPTFR,MESH%NBOR%I,LIMPRO,MESH%XNEBOR%R,
     &               MESH%YNEBOR%R,KNEU,W)
!
        DO I =1,NPOIN
          IF(W(1,I).LE.1.D-12) W(1,I)=0.D0
          IF(ABS(W(2,I)).LE.1.D-12) W(2,I)=0.D0
          IF(ABS(W(3,I)).LE.1.D-12) W(3,I)=0.D0
        ENDDO
!
      ELSE
!
        XNC = 0.D0
        DO I=1,NPOIN
          IF(H%R(I).GT.EPS_FV) THEN
            W1=SQRT((QU(I)/H%R(I))**2+(QV(I)/H%R(I))**2)+
     &         SQRT(GRAV*H%R(I))
            IF(W1.GE.XNC) XNC = W1
            IF(W1.GE.50.D0) THEN
              QU(I) = 0.D0
              QV(I) = 0.D0
            ENDIF
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
