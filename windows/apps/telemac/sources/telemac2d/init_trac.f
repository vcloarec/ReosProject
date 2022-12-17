!                   ********************
                    SUBROUTINE INIT_TRAC
!                   ********************
!
     &(HT,SMTR,FLUXT,FLUHTEMP,MASSOU,FLUTENT,FLUTSOR,
     & FLBOR,MESH)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    INITIALISES TRACERS.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in,out]  FLBOR         BOUNDARY MASS FLUXES
!>@param  [in,out]  FLUXT         FLUX FOR TRACER AT TIME N
!>@param  [in,out]  FLUHTEMP      FLUX FOR TRACER
!>@param  [in,out]  FLUTENT       FLUX TRACER INLET
!>@param  [in,out]  FLUTSOR       FLUX TRACER OUTLET
!>@param  [in,out]  HT            TRACER*DEPTH
!>@param  [in,out]  MASSOU        ADDED TRACER MASS BY SOURCE TERM
!>@param  [in,out]  SMTR          SOURCE TERMS FOR TRACEUR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_GAIA, ONLY: FLUER_ADV,FLUDPT_ADV,NSUSP_TEL
      USE DECLARATIONS_TELEMAC2D, ONLY: IND_SED,LITBOR,HN,U,V,TEXP,T1,S,
     &                                  MARTIM,MARDAT,LAMBD0,TIMP,CF,T1,
     &                                  T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,
     &                                  T12,PHI0,NPOIN,NSEG,LT,
     &                                  NTRAC,NREJET,ISCE,NREG,
     &                                  TNP,PT_IN_POLY,TSCE2,SMH,H,DT,
     &                                  AT,V2DPAR,T,TN,TBOR,NBUSE,
     &                                  ENTBUS,SORBUS,AK,EP,ITURB,DEBUG

      USE DECLARATIONS_TELEMAC, ONLY: COUPLING,KENT
      USE METEO_TELEMAC, ONLY: PATMOS
      USE DECLARATIONS_KHIONE, ONLY: IND_T,IND_FRA,IND_DCI,NC_FRA,IND_S
      USE INTERFACE_PARALLEL, ONLY : P_SUM
      USE INTERFACE_TELEMAC2D, EX_INIT_TRAC => INIT_TRAC
      USE INTERFACE_KHIONE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: HT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SMTR,FLUXT,FLUHTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I,II,TTL,IREG,NSG,ITRAC,ISUSP,IR
      DOUBLE PRECISION SMTRAC,EMPT
!
      DO ITRAC=1,NTRAC
!       INIT GLOBAL BALANCE
        MASSOU(ITRAC) = 0.D0
        FLUTENT(ITRAC)= 0.D0
        FLUTSOR(ITRAC)= 0.D0
        CALL OS('X=0     ',X=TEXP%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
      ENDDO
!
!     COUPLING WITH KHIONE
!     ********************
      IF(INCLUS(COUPLING,'KHIONE')) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_THERMAL'
          CALL SOURCE_THERMAL
     &      ( NPOIN,TEXP%ADR(IND_T)%P%R,TN,HN%R,U,V,T1,S,MESH,
     &        DT,AT,MARDAT,MARTIM,LAMBD0 )
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_THERMAL'
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_FRAZIL'
          CALL SOURCE_FRAZIL
     &      ( NPOIN,TEXP,S,TN,HN,U,V,
     &        DT,CF,AK,EP,ITURB,LT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_FRAZIL'
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_ICOVER'
          CALL SOURCE_ICOVER
     &      (NPOIN,TEXP,TN,HN,DT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_ICOVER'
      ENDIF
!
!     LOOP ON TRACERS
      DO ITRAC=1,NTRAC
!
        DO IS=1,NPOIN
!         INIT CONSERVATIVE VAR FOR TRACER
          HT%ADR(ITRAC)%P%R(IS)=HN%R(IS)*TN%ADR(ITRAC)%P%R(IS)
!         INIT SOURCE TERM FOR TRACER
          SMTR%ADR(ITRAC)%P%R(IS)=0.D0
        ENDDO
!
!       INIT SOURCE TERM FOR TRACER
!       ***************************
        IF(NREJET.NE.0.AND.NREG.EQ.0) THEN
          DO I=1,NREJET
            IS =ISCE(I)
            IF(IS.GT.0) THEN
              SMTR%ADR(ITRAC)%P%R(IS)=SMH%R(IS)*TSCE2(I,ITRAC)
            ENDIF
          ENDDO
        ELSEIF(NREJET.NE.0.AND.NREG.NE.0) THEN
          DO IREG=1,NREG
            TTL=TNP(IREG)
!           TEST USEFUL FOR PARALLEL MODE
            IF(TTL.NE.0) THEN
              DO I=1,TTL
                II=PT_IN_POLY(IREG,I)
                SMTR%ADR(ITRAC)%P%R(II)=SMH%R(II)*TSCE2(IREG,ITRAC)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
!
!       INIT SOURCE TERM FOR CULVERTS
!       *****************************
        IF(NBUSE.GT.0) THEN
          DO I=1,NBUSE
            SMTRAC = 0.D0
            IR = ENTBUS%I(I)
            IF(IR.GT.0) THEN
              SMTRAC = SMH%R(IR)*TN%ADR(ITRAC)%P%R(IR)
              SMTR%ADR(ITRAC)%P%R(IR)=SMTRAC
            ENDIF
            SMTRAC = P_SUM(SMTRAC)
            IR = SORBUS%I(I)
            IF(IR.GT.0) THEN
              SMTR%ADR(ITRAC)%P%R(IR)=-SMTRAC
            ENDIF
          ENDDO
        ENDIF
!
!       COUPLING WITH GAIA
!       ******************
        IF(INCLUS(COUPLING,'GAIA').AND.
     &    (ITRAC.GE.IND_SED.AND.ITRAC.LT.IND_SED+NSUSP_TEL)) THEN
          ISUSP=ITRAC-IND_SED+1
          CALL PREP_ADVECTION_GAIA(U,V,0,1,
     &         ISUSP,LITBOR%ADR(ITRAC)%P%I,TBOR%ADR(ITRAC)%P%R,
     &         TN%ADR(ITRAC)%P%R,KENT,FLBOR,HN,EMPT)

          DO IS=1,NPOIN
            SMTR%ADR(ITRAC)%P%R(IS)=
     &      (FLUER_ADV%R(IS)+FLUDPT_ADV%R(IS)*TN%ADR(ITRAC)%P%R(IS))*
     &                              H%R(IS)
          ENDDO
        ENDIF
!
!       COUPLING WITH WAQTEL
!       ********************
        IF(INCLUS(COUPLING,'WAQTEL')) THEN
          CALL SOURCE_WAQ
     &   (NPOIN,NPOIN,TEXP,TIMP,TN,HN,U,V,CF,
     &    T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, T1,T2,T3,
     &    PATMOS,2,1,
     &    LAMBD0,PHI0,AT,MARDAT,MARTIM,MESH%X)
        ENDIF
!
!       COUPLING WITH KHIONE
!       ********************
        IF(INCLUS(COUPLING,'KHIONE')) THEN
!
          IF(ITRAC.EQ.IND_T.OR.
!       &     ITRAC.EQ.IND_FRA) THEN
     &      (ITRAC.GE.IND_FRA.AND.ITRAC.LE.IND_FRA+NC_FRA).OR.
     &      ITRAC.EQ.IND_DCI.OR.ITRAC.EQ.IND_S) THEN
            DO IS=1,NPOIN
              SMTR%ADR(ITRAC)%P%R(IS)=SMTR%ADR(ITRAC)%P%R(IS)+
     &                    TEXP%ADR(ITRAC)%P%R(IS)*H%R(IS)*V2DPAR%R(IS)
            ENDDO
          ENDIF
        ENDIF
!
!       INIT FLUXES
!       ***********
        DO NSG=1,NSEG
          FLUXT%ADR(ITRAC)%P%R(NSG)=0.D0
          FLUHTEMP%ADR(ITRAC)%P%R(NSG)=0.D0
        ENDDO
!
        CALL OS('X=Y     ',X=T%ADR(ITRAC)%P,Y=TN%ADR(ITRAC)%P)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
