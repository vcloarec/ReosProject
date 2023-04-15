!                   *****************
                    SUBROUTINE DIFSOU
!                   *****************
!
     &(TEXP,TIMP,TSCEXP,HPROP,TN,TETAT,NREJET,ISCE,DSCE,TSCE,
     & MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,FAC,NBUSE,ENTBUS,SORBUS,
     & DBUS,TBUS,NWEIRS,TYPSEUIL,N_NGHB_W_NODES,
     & NREG,PT_IN_POLY,TNP,NPOIN)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    PREPARES THE SOURCES TERMS IN THE DIFFUSION EQUATION
!+                FOR THE TRACER.
!
!warning  BEWARE OF NECESSARY COMPATIBILITIES FOR HPROP, WHICH
!+            SHOULD REMAIN UNCHANGED UNTIL THE COMPUTATION OF THE
!+            TRACER MASS IN CVDFTR
!
!history  J-M HERVOUET (LNHE); C MOULIN (LNH)
!+        23/02/2009
!+        V6P0
!+
!
!history  J-M HERVOUET (LNHE)
!+        01/10/2009
!+       V6P0
!+   MODIFIED TEST ON ICONVF(3)
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
!history  C.COULET (ARTELIA)
!+        23/05/2012
!+        V6P2
!+   Modification for culvert management
!+   Addition of Tubes management
!
!history  C.COULET (ARTELIA)
!+        14/06/2012
!+        V6P2
!+   Addition of tracer degradation law treatment
!
!history  J-M HERVOUET (LNHE)
!+        26/07/2012
!+        V6P2
!+   In parallel, P_SUM on MASSOU must be done once at the end
!
!history  C.COULET (ARTELIA)
!+        14/06/2013
!+        V6P2
!+   Modification for weirs (type 2) management
!
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction:
!+   first calculate the local radius r_sec,
!+   then set the production and dissipation terms of Omega,
!
!history  R. ATA (LNHE)
!+        10/11/2014
!+        V7P0
!+   add new water quality processes
!
!history  J-M HERVOUET (LNHE)
!+        08/06/2015
!+        V7P1
!+   Treatment of sources modified for distributive schemes.
!
!history  J-M HERVOUET (LNHE)
!+        18/09/2015
!+        V7P1
!+   FAC is now an integer. NREJET is now the number of sources, before
!+   NREJTR was sent by telemac2d.f.
!
!history  R. ATA (LNHE)
!+        02/11/2015
!+        V7P1
!+   Updates for water quality: new subroutine for weir reaeration
!
!history  C. COULET (ARTELIA)
!+        01/09/2016
!+        V7P2
!+   Tentative update for weirs type 2 (parallel treatment)
!
!history  S.E.BOURBAN (HRW)
!+        20/01/2018
!+        V7P4
!+   Removal of local YASMI setup, now done in POINT_TELEMAC2D, ahead
!+   of logical YESIMP for the memory allocation of implicit arrays.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DBUS           |-->| DISCHARGE OF TUBES.
!| DSCE           |-->| DISCHARGE OF POINT SOURCES
!| DT             |-->| TIME STEP
!| ENTBUS         |-->| INDICES OF ENTRY OF TUBES IN GLOBAL NUMBERING
!| FAC            |-->| IN PARALLEL :
!|                |   | 1/(NUMBER OF SUB-DOMAINS OF THE POINT)
!| HPROP          |-->| PROPAGATION DEPTH
!| ISCE           |-->| NEAREST POINTS OF DISCHARGES
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NBUSE          |-->| NUMBER OF CULVERTS
!| NREJET         |-->| NUMBER OF POINT SOURCES.
!| NTRAC          |-->| NUMBER OF TRACERS
!| NWEIRS         |-->| NUMBER OF WEIRS
!| SORBUS         |-->| INDICES OF TUBES EXITS IN GLOBAL NUMBERING
!| TBUS           |-->| VALUES OF TRACERS AT TUBES EXTREMITY
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| TWEIRA         |-->| VALUES OF TRACERS ON SIDE A OF WEIR
!| TWEIRB         |-->| VALUES OF TRACERS ON SIDE B OF WEIR
!| TYPSEUIL       |-->| TYPE OF WEIRS (IF = 2, WEIRS TREATED AS SOURCES POINTS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL, ONLY: WAQTR,RANKTR
      USE DECLARATIONS_TELEMAC2D, ONLY:
     &  U,V,UNSV2D,V2DPAR,VOLU2D,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     &  T11,T12,MESH,MSK,
     &  IELMU,S,CF,H,SECCURRENTS,SEC_AS,SEC_DS,SEC_R,
     &  ICONVFT,DEBUG,MASKEL,
     &  MARDAT,MARTIM,LAMBD0,PHI0,
     &  WNODES_PROC,WNODES,AK,EP,ITURB,LT
      USE METEO_TELEMAC, ONLY: PATMOS
      USE DECLARATIONS_KHIONE, ONLY: IND_T
      USE INTERFACE_WAQTEL
      USE INTERFACE_KHIONE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: ISCE(*),NREJET,NTRAC
      INTEGER          , INTENT(IN)    :: NBUSE,NWEIRS
      INTEGER          , INTENT(IN)    :: N_NGHB_W_NODES
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      INTEGER          , INTENT(IN)    :: MAXSCE,MAXTRA,TYPSEUIL
      INTEGER          , INTENT(IN)    :: FAC(*)
      INTEGER          , INTENT(IN)    :: NPOIN,NREG
      INTEGER          , INTENT(IN)    :: TNP(NREG)
      INTEGER          , INTENT(IN)    :: PT_IN_POLY(MAXSCE,*)
      DOUBLE PRECISION , INTENT(IN)    :: AT,DT,TETAT,DSCE(*)
      DOUBLE PRECISION , INTENT(IN)    :: DBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: TSCE(MAXSCE,MAXTRA)
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: HPROP,TBUS
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TSCEXP,TEXP,TIMP,TN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,IR,ITRAC,N,NTRA
      INTEGER TTL,IREG,II
      LOGICAL DISTRI
!
      DOUBLE PRECISION DEBIT,TRASCE
      DOUBLE PRECISION DENOM,NUMER,NORM2,SEC_RMAX,RMAX
!
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-6
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!     SECONDARY CURRENTS WILL BE TREATED APART
!
      NTRA=NTRAC
      IF(SECCURRENTS) NTRA=NTRA-1
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERMS
!
      DO ITRAC=1,NTRA
        CALL OS('X=0     ',X=TSCEXP%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TEXP%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
        MASSOU(ITRAC) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!  TAKES THE SOURCES OF TRACER INTO ACCOUNT
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NTRA
!
        IF(NREJET.GT.0.AND.NREG.EQ.0) THEN
!
          DO I = 1 , NREJET
!
            IR = ISCE(I)
!           TEST IR.GT.0 FOR THE PARALLELISM
            IF(IR.GT.0) THEN
              DEBIT=DSCE(I)
              IF(DEBIT.GT.0.D0) THEN
                TRASCE = TSCE(I,ITRAC)
              ELSE
!               THE VALUE AT THE SOURCE IS TN IF THE FLOW IS OUTGOING
!               IT WILL BE WRONG BUT NOT CONSIDERED FOR LOCALLY IMPLICIT
!               SCHEMES
                TRASCE = TN%ADR(ITRAC)%P%R(IR)
              ENDIF
!
!             SCHEME SENSITIVE, HERE NOT FOR LOCALLY IMPLICIT SCHEMES
!             BECAUSE THEY WILL DO THE JOB THEMSELVES
!
              DISTRI=.FALSE.
              IF(ICONVFT(ITRAC).EQ.ADV_LPO) DISTRI=.TRUE.
              IF(ICONVFT(ITRAC).EQ.ADV_NSC) DISTRI=.TRUE.
              IF(ICONVFT(ITRAC).EQ.ADV_PSI) DISTRI=.TRUE.
!
              IF(.NOT.DISTRI) THEN
!               SOURCE TERM ADDED TO THE MASS OF TRACER
                IF(NCSIZE.GT.1) THEN
!                 FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!                 (SEE CALL TO P_SUM BELOW)
                  MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE*FAC(IR)
                ELSE
                  MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE
                ENDIF
                TRASCE = TRASCE - (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR)+TRASCE
!
!             THE IMPLICIT PART OF THE TERM - T * SCE
!             IS DEALT WITH IN CVDFTR.
!
            ENDIF
!
          ENDDO
!
        ELSEIF(NREG.NE.0) THEN
!
          DO IREG = 1 , NREG
!
            TTL = TNP(IREG)
!           TEST USEFUL FOR PARALLEL MODE
            IF(TTL.NE.0) THEN
              DO I=1,TTL
                II=PT_IN_POLY(IREG,I)
                DEBIT=DSCE(IREG)
                IF(DEBIT.GT.0.D0) THEN
                  TRASCE = TSCE(IREG,ITRAC)
                ELSE
!                 THE VALUE AT THE SOURCE IS TN IF THE FLOW IS OUTGOING
!                 IT WILL BE WRONG BUT NOT CONSIDERED FOR LOCALLY IMPLICIT
!                 SCHEMES
                  TRASCE = TN%ADR(ITRAC)%P%R(II)
                ENDIF
!
!               SCHEME SENSITIVE, HERE NOT FOR LOCALLY IMPLICIT SCHEMES
!               BECAUSE THEY WILL DO THE JOB THEMSELVES
!
                DISTRI=.FALSE.
                IF(ICONVFT(ITRAC).EQ.ADV_LPO) DISTRI=.TRUE.
                IF(ICONVFT(ITRAC).EQ.ADV_NSC) DISTRI=.TRUE.
                IF(ICONVFT(ITRAC).EQ.ADV_PSI) DISTRI=.TRUE.
!
                IF(.NOT.DISTRI) THEN
!                 SOURCE TERM ADDED TO THE MASS OF TRACER
                  IF(NCSIZE.GT.1) THEN
!                   FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!                   (SEE CALL TO P_SUM BELOW)
                    MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE*FAC(II)
                  ELSE
                    MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE
                  ENDIF
                  TRASCE = TRASCE -(1.D0-TETAT) * TN%ADR(ITRAC)%P%R(II)
                ENDIF
                TSCEXP%ADR(ITRAC)%P%R(II) = TSCEXP%ADR(ITRAC)%P%R(II)
     &                                    + TRASCE
!
!               THE IMPLICIT PART OF THE TERM - T * SCE
!               IS DEALT WITH IN CVDFTR.
              ENDDO
!
            ENDIF
!
          ENDDO
!
        ENDIF !NREJET ET NREG
!
        IF(NBUSE.GT.0) THEN
          DO I = 1 , NBUSE
            IR = ENTBUS(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_SUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TBUS%ADR(ITRAC)%P%R(I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
            IR = SORBUS(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_SUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(NBUSE+I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DBUS(I)*
     &                        TBUS%ADR(ITRAC)%P%R(NBUSE+I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TBUS%ADR(ITRAC)%P%R(NBUSE+I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
          ENDDO
        ENDIF
!
        IF(TYPSEUIL.EQ.2) THEN
          IF(N_NGHB_W_NODES.GT.0) THEN
            DO N=1,N_NGHB_W_NODES
              IF(WNODES_PROC(N)%NUM_NEIGH.EQ.IPID) GOTO 50
            ENDDO
50          CONTINUE
            DO I=1, WNODES_PROC(N)%NB_NODES
              IR = WNODES_PROC(N)%NUM_LOC(I)
              K  = WNODES_PROC(N)%LIST_NODES(I)
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_SUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*WNODES(K)%QN*
     &                        WNODES(K)%TRAC(ITRAC)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*WNODES(K)%QN*
     &                        WNODES(K)%TRAC(ITRAC)
              ENDIF
              IF((WNODES(K)%QN.GT.0.D0).OR.(WNODES(K)%QN.LT.0.D0)) THEN
                TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &              WNODES(K)%TRAC(ITRAC) -
!             The computation of discharge and by consequence the tracer is done explicitly
!             thus TETAT should be equal to 0 in the following term (forced in lecdon)
     &              (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
              ENDIF
!
            ENDDO
          ENDIF
        ENDIF
!
        IF(NCSIZE.GT.1.AND.
     &     (NREJET.GT.0.OR.NBUSE.GT.0.OR.TYPSEUIL.EQ.2)) THEN
          MASSOU(ITRAC)=P_SUM(MASSOU(ITRAC))
        ENDIF
!
      ENDDO
!
!     WATER QUALITY CONTRIBUTION TO TRACER SOURCES
!
      IF(INCLUS(COUPLING,'WAQTEL')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_WAQ'
        CALL SOURCE_WAQ
     & (NPOIN,NPOIN,TEXP,TIMP,TN,HPROP,U,V,CF,
     &  T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, T1,T2,T3,
     &  PATMOS,2,1,
     &  LAMBD0,PHI0,AT,MARDAT,MARTIM,MESH%X,DT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_WAQ'
      ENDIF
!
!     ICE CONTRIBUTION TO TRACER SOURCES
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_THERMAL'
        CALL SOURCE_THERMAL
     &    ( NPOIN,TEXP%ADR(IND_T)%P%R,TN,HPROP%R,U,V, T1,S,MESH,
     &      DT,AT-DT,MARDAT,MARTIM, LAMBD0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_THERMAL'
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_FRAZIL'
        CALL SOURCE_FRAZIL
     &    ( NPOIN,TEXP,TIMP,TN,HPROP,U,V,
     &      DT, CF, AK, EP, ITURB, LT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_FRAZIL'
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_ICOVER'
        CALL SOURCE_ICOVER
     &    ( NPOIN,TEXP,TN,HPROP,DT)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_ICOVER'
      ENDIF
!
!     TOTAL CONTRIBUTION OF TEXP AND TIMP
!
      IF( INCLUS(COUPLING,'WAQTEL').OR.INCLUS(COUPLING,'KHIONE')) THEN
        DO J = 1,WAQTR
          ITRAC = RANKTR(J)
          DO I = 1,NPOIN
            MASSOU(ITRAC) = MASSOU(ITRAC)
     &                  + DT*H%R(I)*TEXP%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
          ENDDO
          IF(NCSIZE.GT.0) MASSOU(ITRAC) = P_SUM(MASSOU(ITRAC))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     SECONDARY CURRENTS (OMEGA IS THE TRACER OF RANK NTRAC)
!
      IF(SECCURRENTS) THEN
!
        CALL VECTOR(T1,'=','GRADF          Y',IELMU,
     &              1.D0,V,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T2,'=','GRADF          X',IELMU,
     &              1.D0,U,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T3,'=','GRADF          X',IELMU,
     &              1.D0,V,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T4,'=','GRADF          Y',IELMU,
     &              1.D0,U,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM (T1, 2, MESH)
          CALL PARCOM (T2, 2, MESH)
          CALL PARCOM (T3, 2, MESH)
          CALL PARCOM (T4, 2, MESH)
        ENDIF
!
!       INITIALISATIONS
!
        CALL OS('X=0     ',X=TSCEXP%ADR(NTRAC)%P)
!        YASMI(NTRAC)=.TRUE.
!
!       SOURCE TERMS
!
        DO K=1,NPOIN
          NORM2=U%R(K)**2+V%R(K)**2
          NUMER = (U%R(K)*V%R(K)*(T1%R(K)-T2%R(K))+U%R(K)**2*(T3%R(K))
     &           -V%R(K)**2*(T4%R(K)))*UNSV2D%R(K)
          SEC_R%R(K)=NUMER/MAX(SQRT(NORM2)**3,1.D-9)
!         GEOMETRY: R OBVIOUSLY LARGER THAN 0.5 LOCAL MESH SIZE
!         THEORY ALSO SAYS R > 2H
!         LOCAL MESH SIZE HERE ASSUMED TO BE SQRT(V2DPAR)
          RMAX=MAX(2.D0*H%R(K),0.5D0*SQRT(V2DPAR%R(K)))
!         RMAX=0.5D0*SQRT(V2DPAR%R(K))
          SEC_RMAX=1.D0/RMAX
          SEC_R%R(K)=MAX(-SEC_RMAX,MIN(SEC_RMAX,SEC_R%R(K)))
!         EXPLICIT SOURCE TERMS (CREATION OF OMEGA)
!         CLIPPING OF H AT 1.D-2
!         NOTE: IMPLICIT TERMS (DESTRUCTION) IN CVDFTR CLIPPED AT 1.D-4
          DENOM=MAX(H%R(K),1.D-2)*(9.D0*(H%R(K)*SEC_R%R(K))**2+1.D0)
          TEXP%ADR(NTRAC)%P%R(K)=
     &                 SEC_AS*SQRT(0.5D0*CF%R(K))*NORM2*SEC_R%R(K)/DENOM
!         IMPLICIT SOURCE TERMS (DEPENDING ON THE LAW CHOSEN)
          TIMP%ADR(NTRAC)%P%R(K)=-SEC_DS*SQRT(0.5D0*CF%R(K)*NORM2)
        ENDDO
!
!       MASS ADDED BY EXPLICIT TERMS
!       THE MASS ADDED BY IMPLICIT TERMS IS COMPUTED IN CVDFTR
!
        MASSOU(NTRAC) = 0.D0
        DO K=1,NPOIN
          MASSOU(NTRAC)=MASSOU(NTRAC)
     &                 +H%R(K)*TEXP%ADR(NTRAC)%P%R(K)*VOLU2D%R(K)
        ENDDO
        MASSOU(NTRAC)=MASSOU(NTRAC)*DT
        IF(NCSIZE.GT.1) MASSOU(NTRAC)=P_SUM(MASSOU(NTRAC))
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

