!                   ***************
                    SUBROUTINE BORD
!                   ***************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    MODIFIES THE BOUNDARY CONDITIONS ARRAYS
!+                WHEN THEY VARY IN TIME.
!+
!
!note     THIS SUBROUTINE CAN BE COMPLETED BY THE USER DIRECTLY,
!+         OR THROUGH THE FUNCTIONS: Q , SL , TR , VIT
!
!history  J-M HERVOUET (LNHE)
!+        27/03/2008
!+        V5P9
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (LNHE)
!+        06/04/2012
!+        V6P2
!+   Original point numbers sent to functions SL, VIT and TR
!
!history  J-M HERVOUET (LNHE)
!+        25/04/2013
!+        V6P3
!+   A new test to see if NUMLIQ(K)>0 (it may happen with weirs that
!+   LIHBOR(K)=KENT and NUMLIQ(K)=0.
!
!history  J-M HERVOUET (LNHE)
!+        24/12/2013
!+        V7P0
!+   Stage-discharge curves Q(Z) now programmed.
!
!history  J-M HERVOUET (LNHE)
!+        18/07/2014
!+        V7P0
!+   When velocities imposed, values not initialised in the case of
!+   Thompson. Correction proposed by HRW (the values of U and V
!+   are in this case taken as the values at the previous time step).
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EQUA           |-->| STRING DESCRIBING THE EQUATIONS SOLVED
!| H              |-->| DEPTH AT TIME N
!| HBOR           |<->| PRESCRIBED DEPTH
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MASK           |-->| BLOCK OF MASKS FOR DIFFERENT BOUNDARY CONDITIONS
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NOMIMP         |-->| NAME OF LIQUID BOUNDARIES FILE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NTRACE         |-->| NUMBER OF BOUNDARIES WITH TRACER PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NVITES         |-->| NUMBER OF BOUNDARIES WITH VELOCITY PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| PROVEL         |-->| OPTION FOR VELOCITY PROFILES
!| TBOR           |<--| BLOCK WITH PRESCRIBED VALUES OF TRACERS
!| TEMPS          |-->| TIME IN SECONDS
!| TRA05          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| TRA06          |-->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY AT TIME N
!| V              |-->| Y-COMPONENT OF VELOCITY AT TIME N
!| UBOR           |<->| X-COMPONENT OF PRESCRIBED VELOCITY
!| VBOR           |<->| Y-COMPONENT OF PRESCRIBED VELOCITY
!| XNEBOR         |-->| X-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| YNEBOR         |-->| Y-COMPONENT OF NORMAL VECTOR AT BOUNDARY NODES
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY: STA_DIS_CURVES,PTS_CURVES,QZ,
     &                                  FLUX_BOUNDARIES,MAXFRO,FRTYPE,
     &                                  TIDALTYPE,RELAX_STA_DIS,
     &                                  SECCURRENTS
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LIUBOR(NPTFR2)
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      CHARACTER(LEN=PATH_LEN), INTENT(IN)  :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASK,LITBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,MSK8,IFRLIQ,YADEB(MAXFRO),IERR,ITRAC,IFR,N,IELEB,KP1
      INTEGER NTRACB
!
      DOUBLE PRECISION Z,QIMP,ZMIN(MAXFRO)
!
      LOGICAL YAZMIN
!
      INTRINSIC MAX
!
!     PROVISOIRE
!
      DOUBLE PRECISION DIS_STA_CUR
      EXTERNAL         DIS_STA_CUR
!
!-----------------------------------------------------------------------
!
!     IF VELOCITY PROFILE OPTION 5 OR STAGE-DISCHARGE CURVE Q(Z):
!     THE MINIMUM ELEVATION OF EVERY BOUNDARY IS NEEDED
!
      YAZMIN=.FALSE.
      DO IFR=1,NFRLIQ
        ZMIN(IFR)=1.D99
        IF(PROVEL(IFR).EQ.5.OR.STA_DIS_CURVES(IFR).EQ.2) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR
          IFR=NUMLIQ(K)
          IF(IFR.GT.0) ZMIN(IFR)=MIN(ZMIN(IFR),ZF(NBOR(K))+H%R(NBOR(K)))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFR=1,NFRLIQ
            ZMIN(IFR)=P_MIN(ZMIN(IFR))
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      MSK8 = 8
!
!  INITIALISATION OF YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO K=1,NPTFR
!
!  LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
      IF(LIHBOR(K).EQ.KENT) THEN
!
        IFRLIQ=NUMLIQ(K)
!
!          IFRLIQ.EQ.0 MAY HAPPEN WITH WEIRS
        IF(IFRLIQ.GT.0) THEN
          IF(STA_DIS_CURVES(IFRLIQ).EQ.1) THEN
            Z = STA_DIS_CUR(IFRLIQ,FLUX_BOUNDARIES(IFRLIQ),
     &                      PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                      ZF(NBOR(K))+H%R(NBOR(K)),RELAX_STA_DIS)
            HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
            H%R(NBOR(K))=HBOR(K)
          ELSEIF(NCOTE.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
            N=NBOR(K)
            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
            Z = SL(IFRLIQ,N)
            HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
            H%R(NBOR(K))=HBOR(K)
!         ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
          ENDIF
        ENDIF
!
      ENDIF
!
!  DISCHARGE IMPOSED: VARIOUS OPTIONS ACCORDING TO PROVEL
!                 ONE USES THE VALUES PROVIDED BY THE USER
!                 AS VELOCITY PROFILE.
!                 UBOR(K,2) AND VBOR(K,2) ARE THE VALUES OF
!                 THE CONLIM FILE, AND ARE CONSERVED.
!
      IF(LIUBOR(K).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        IFR=NUMLIQ(K)
        IF(PROVEL(IFR).EQ.1) THEN
!         CONSTANT NORMAL PROFILE
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(IFR).EQ.2) THEN
!         PROFILE PROVIDED BY THE USER
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.3) THEN
!         NORMAL VELOCITY PROVIDED IN UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.4) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H
          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
        ELSEIF(PROVEL(IFR).EQ.5) THEN
!         NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
!         DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
          UBOR(K,1)=-XNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
          VBOR(K,1)=-YNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
        ELSE
          WRITE(LU,*) 'BOUNDARY ',IFR
          WRITE(LU,*) 'VELOCITY PROFILE ',PROVEL(IFR),
     &                ' NOT IMPLEMENTED YET'
          WRITE(LU,*) 'PLEASE GIVE A VALUE BETWEEN 1 AND 5'
          CALL PLANTE(1)
          STOP
        ENDIF
!       ONE DOES NOT SET VELOCITY IF THERE IS NO WATER.
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
!       U AND V INITIALISED WITH THE IMPOSED VALUES
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
      ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
      IF(LIUBOR(K).EQ.KENTU.AND.
     &  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
          IFR=NUMLIQ(K)
          IF(PROVEL(IFR).EQ.1) THEN
            N=NBOR(K)
            IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
            UBOR(K,1) = - XNEBOR(K) * VIT(NUMLIQ(K),N)
            VBOR(K,1) = - YNEBOR(K) * VIT(NUMLIQ(K),N)
          ELSEIF(PROVEL(IFR).EQ.2) THEN
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(IFR).EQ.3) THEN
            UBOR(K,1) = - XNEBOR(K) * UBOR(K,2)
            VBOR(K,1) = - YNEBOR(K) * UBOR(K,2)
          ELSE
            WRITE(LU,*) 'BOUNDARY ',IFR
            WRITE(LU,*) 'PROFILE ',PROVEL(IFR),
     &                  ' ASKED'
            WRITE(LU,*) 'IMPOSSIBLE COMBINATION'
            CALL PLANTE(1)
            STOP
          ENDIF
!         U AND V INITIALISED WITH THE IMPOSED VALUES
!         IF NOT IN THOMPSON MODE
          IF(FRTYPE(IFR).NE.2) THEN
            U%R(NBOR(K)) = UBOR(K,1)
            V%R(NBOR(K)) = VBOR(K,1)
          ENDIF
        ENDIF
      ENDIF
!
!  IMPOSED TRACER
!
      IF(NTRAC.GT.0) THEN
        IF(SECCURRENTS) THEN
          NTRACB = NTRAC-1
        ELSE
          NTRACB = NTRAC
        ENDIF
        IF(NTRACB.GT.0) THEN
          DO ITRAC=1,NTRACB
            IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.
     &        (NTRACE.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
!             THE CASE NUMLIQ(K)=0 CORRESPONDS TO A SINGULARITY
!             INITIALLY DECLARED AS A SOLID BOUNDARY AND FOR WHICH
!             TBOR IS FILLED IN CLHUVT
              IF(NUMLIQ(K).GT.0) THEN
                N=NBOR(K)
                IF(NCSIZE.GT.1) N=MESH%KNOLG%I(N)
                Z = TR(NUMLIQ(K),ITRAC,N,IERR)
                IF(IERR.EQ.0) TBOR%ADR(ITRAC)%P%R(K) = Z
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
      ENDDO ! K
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALTYPE.GE.1) CALL TIDAL_MODEL_T2D()
!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
      IF(U%ELM .EQ.13)THEN
        DO IELEB=1,MESH%NELEB
          K  =MESH%IKLBOR%I(IELEB)
          KP1=MESH%IKLBOR%I(IELEB+MESH%NELEBX)
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     &      (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
            U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(KP1,1))*0.5D0
            V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(KP1,1))*0.5D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CASE OF DISCHARGE IMPOSED:
!
!  LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
!
        DO IFRLIQ = 1 , NFRLIQ
!
          IF(NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
!
!           ONE TAKES THE MASK OF LIQUID BOUNDARIES MSK8, WHICH IS
!           EQUAL TO THE MASK OF THE DISCHARGE IMPOSED ON A DISCHARGE
!           IMPOSED BOUNDARY. THIS MAKES IT POSSIBLE TO CHANGE A FREE
!           VELOCITY BOUNDARY TO A DISCHARGE IMPOSED TO A LEVEL IMPOSED
!           BOUNDARY, IN SPITE OF THE FACT THAT THE MASKS ARE MADE IN
!           PROPIN BEFORE THE CALL TO BORD
!
            IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_MAX(YADEB(IFRLIQ))
            IF(YADEB(IFRLIQ).EQ.1) THEN
              IF(STA_DIS_CURVES(IFRLIQ).EQ.2) THEN
                QIMP=DIS_STA_CUR(IFRLIQ,PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                           ZMIN(IFRLIQ))
              ELSE
                QIMP=Q(IFRLIQ)
              ENDIF
              CALL DEBIMP(QIMP,UBOR,VBOR,U,V,H,NUMLIQ,
     &                    IFRLIQ,TRA05,TRA06,
     &                    NPTFR,MASK%ADR(MSK8)%P%R,MESH)
            ENDIF
!
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
      IF(U%ELM.EQ.13) THEN
        DO IELEB=1,MESH%NELEB
          K  =MESH%IKLBOR%I(IELEB)
          KP1=MESH%IKLBOR%I(IELEB+MESH%NELEBX)
          UBOR(K+NPTFR,1) =(UBOR(K,1)+UBOR(KP1,1))*0.5D0
          VBOR(K+NPTFR,1) =(VBOR(K,1)+VBOR(KP1,1))*0.5D0
        ENDDO
      ENDIF

      CALL USER_BORD
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
