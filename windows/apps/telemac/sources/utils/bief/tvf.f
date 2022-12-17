!                   **************
                    SUBROUTINE TVF
!                   **************
!
     &(F,FC,H,FXMAT,FXMATPAR,
     & VOLU2D,UNSV2D,DT,FXBOR,FXBORPAR,T7,FBOR,SMH,YASMH,FSCEXP,
     & NSEG,NPOIN,NPTFR,GLOSEG,SIZGLO,NBOR,LIMTRA,KDIR,KDDL,OPTSOU,HLIN,
     & IOPT2,FLBORTRA,SURNIT,MESH,RAIN,PLUIE,TRAIN,MASSOU,
     & MASS_BALANCE)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    COMPUTES THE TRACER FOR FINITE VOLUME SCHEME.
!+                TO COMPLETE.
!
!history  C-T PHAM (LNHE)
!+        27/02/09
!+        V5P9
!+   JMH : DISTINGUISHES BETWEEN FXBOR AND FXBORTRA
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
!+        24/02/2012
!+        V6P2
!+   Rain and evaporation added (after initiative by O. Boutron, from
!+   Tour du Valat, and O. Bertrand, Artelia group)
!
!history  J-M HERVOUET (LNHE)
!+        08/06/2015
!+        V7P1
!+   Now mass balance done on request.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME-STEP
!| F              |<--| VALUES OF F AT TIME N+1 OF SUB-ITERATION
!| FBOR           |-->| VALUES OF F AT THE PRESCRIBED BOUNDARIES
!| FC             |-->| VALUES OF F AT TIME N OF SUB-ITERATION
!| FLBORTRA       |<->| FLUX OF TRACER AT THE BOUNDARIES
!| FSCEXP         |-->| EXPLICIT SOURCE TERM FOR F
!| FXBOR          |-->| FLUXES ON BOUNDARIES
!| FXBORPAR       |-->| FLUXES ON BOUNDARIES (DEFINED ON ALL DOMAIN
!|                |   | AND ASSEMBLED IN PARALLEL)
!| FXMAT          |-->| FLUXES (NON ASSEMBLED IN PARALLEL)
!| FXMATPAR       |-->| FLUXES (ASSEMBLED IN PARALLEL)
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT.
!| H              |-->| WATER DEPTH AT TIME N+1
!|                |   | (VALUE COMPATIBLE WITH CONTINUITY)
!| HLIN           |-->| WATER DEPTH AT TIME N+1
!|                |   | (WITH LINEAR INTERPOLATION IN TIME BETWEEN
!|                |   | HN AND H)
!| IOPT2          |-->| 0: CONSERVATIVE ADVECTION FIELD
!|                |   | 1: NON CONSERVATIVE ADVECTION FIELD
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| LIMTRA         |-->| TECHNICAL BOUNDARY CONDITIONS FOR TRACERS
!| MASS_BALANCE   |-->| IF YES, ALL TERMS FOR MASS BALANCE
!|                |   | WILL BE COMPUTED
!| MASSOU         |-->| MASS ADDED BY SOURCE TERM
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTSOU         |-->| TYPE OF SOURCES
!|                |   | 1: NORMAL
!|                |   | 2: DIRAC
!| PLUIE          |-->| RAIN OR EVAPORATION, IN M/S
!| RAIN           |-->| IF YES: RAIN OR EVAPORATION
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SURNIT         |-->| SURNIT=1/NIT
!| T7             |<->| BIEF_OBJ STRUCTURE FOR A WORK ARRAY
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TVF => TVF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,KDIR,KDDL
      INTEGER, INTENT(IN)             :: SIZGLO,OPTSOU,IOPT2
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,SURNIT,TRAIN
      DOUBLE PRECISION, INTENT(INOUT) :: FLBORTRA(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN),MASSOU
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN),H(NPOIN),HLIN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(*),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(*),VOLU2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSCEXP(*)
      DOUBLE PRECISION, INTENT(IN)    :: FBOR(NPTFR),FXBORPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      LOGICAL, INTENT(IN)             :: YASMH,RAIN,MASS_BALANCE
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T7
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N
!
!-----------------------------------------------------------------------
!
      IF(IOPT2.EQ.0) THEN
!       CONSERVATIVE ADVECTION FIELD
        DO I = 1,NPOIN
          F(I) = FC(I)
        ENDDO
      ELSEIF(IOPT2.EQ.1) THEN
!       NON CONSERVATIVE ADVECTION FIELD
        DO I = 1,NPOIN
          F(I) = FC(I)*MAX(H(I),1.D-8)/MAX(HLIN(I),1.D-8)
        ENDDO
      ELSE
        WRITE(LU,*) 'TVF: UNKNOWN OPTION'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     THE CONTRIBUTION OF FLUXES IS BUILT APART FOR
!     PRELIMINARY PARALLEL ASSEMBLING BEFORE ADDING ON F
!
      DO I = 1,NPOIN
        T7%R(I) = 0.D0
      ENDDO
      DO I = 1,NSEG
        IF(FXMATPAR(I).LT.0.D0) THEN
          T7%R(GLOSEG(I,1)) = T7%R(GLOSEG(I,1))
     &    - DT/HLIN(GLOSEG(I,1))*UNSV2D(GLOSEG(I,1))
     &    *FXMAT(I)*(FC(GLOSEG(I,2))-FC(GLOSEG(I,1)))
        ELSEIF(FXMATPAR(I).GT.0.D0) THEN
          T7%R(GLOSEG(I,2)) = T7%R(GLOSEG(I,2))
     &    + DT/HLIN(GLOSEG(I,2))*UNSV2D(GLOSEG(I,2))
     &    *FXMAT(I)*(FC(GLOSEG(I,1))-FC(GLOSEG(I,2)))
        ENDIF
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(T7,2,MESH)
      DO I = 1,NPOIN
        F(I) = F(I)+T7%R(I)
      ENDDO
!
!     SOURCE TERMS
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            IF(SMH(I).GT.0.D0) THEN
              F(I)=F(I)+DT/HLIN(I)*SMH(I)*(FSCEXP(I)-FC(I))
            ENDIF
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            IF(SMH(I).GT.0.D0) THEN
              F(I)=F(I)+DT/HLIN(I)*UNSV2D(I)*SMH(I)*(FSCEXP(I)-FC(I))
            ENDIF
          ENDDO
        ENDIF
        IF(MASS_BALANCE) THEN
          IF(OPTSOU.EQ.1) THEN
            DO I=1,NPOIN
              IF(SMH(I).GT.0.D0) THEN
                MASSOU=MASSOU+DT*VOLU2D(I)*SMH(I)*FSCEXP(I)
              ELSE
                MASSOU=MASSOU+DT*VOLU2D(I)*SMH(I)*FC(I)
              ENDIF
            ENDDO
          ELSEIF(OPTSOU.EQ.2) THEN
            DO I=1,NPOIN
              IF(SMH(I).GT.0.D0) THEN
                MASSOU=MASSOU+DT*SMH(I)*FSCEXP(I)
              ELSE
                MASSOU=MASSOU+DT*SMH(I)*FC(I)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!     RAIN-EVAPORATION
!
      IF(RAIN) THEN
        DO I=1,NPOIN
          IF(PLUIE(I).GT.0.D0) THEN
!           REAL RAIN, VALUE IN RAIN CONSIDERED...
            F(I)=F(I)+DT/HLIN(I)*PLUIE(I)*(TRAIN-FC(I))
          ELSE
!           EVAPORATION, VALUE IN RAIN NOT CONSIDERED...
            F(I)=F(I)+DT/HLIN(I)*PLUIE(I)*(     -FC(I))
          ENDIF
        ENDDO
      ENDIF
!
!     ON THE DIRICHLET BOUNDARIES, FLUX TERMS TAKEN INTO ACCOUNT
!     ON OTHERS, FBOR IS TAKEN AS FN, SO NO CONTRIBUTION
!
      DO I=1,NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
          N=NBOR(I)
          F(N)=F(N)-DT/HLIN(N)*UNSV2D(N)*FXBORPAR(N)*(FBOR(I)-FC(N))
        ENDIF
      ENDDO
      IF(MASS_BALANCE) THEN
        DO I=1,NPTFR
          IF(LIMTRA(I).EQ.KDIR) THEN
            FLBORTRA(I)=FLBORTRA(I)+FXBOR(I)*FBOR(I)*SURNIT
          ELSEIF(LIMTRA(I).EQ.KDDL) THEN
            N=NBOR(I)
            FLBORTRA(I)=FLBORTRA(I)+FXBOR(I)*FC(N)*SURNIT
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

