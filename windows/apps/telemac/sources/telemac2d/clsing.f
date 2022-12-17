!                   *****************
                    SUBROUTINE CLSING
!                   *****************
!
     &(NWEIRS,NPSING,NDGA1,NDGB1,X,Y,ZF,CHESTR,NKFROT,KARMAN,
     & ZDIG,PHIDIG,NBOR,H,T,NTRAC,IOPTAN,UNORM,
     & UBOR,VBOR,TBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR,GRAV)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE COMPUTATION OF DISCHARGES AND
!+                DETERMINES BOUNDARY CONDITIONS.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        23/11/2005
!+        V5P6
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
!history  J.-M. HERVOUET (LNHE)
!+        09/08/2011
!+        V6P2
!+   Adaptation to parallelism
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Adaptation to the dynamic allocation of weirs
!
!history  J.-M. HERVOUET (EDF LAB, LNHE)
!+        11/02/2014
!+        V7P0
!+   All formulas involving P_MAX and P_MIN simplified.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHESTR         |-->| FRICTION COEFFICIENT
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER DEPTH
!| IOPTAN         |-->| OPTION FOR TANGENTIAL VELOCITIES.
!| KARMAN         |-->| VON KARMAN CONSTANT.
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NKFROT         |-->| FRICTION LAW, PER POINT
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NTRAC          |-->| NUMBER OF TRACERS
!| NDGA1          |-->| NDGA1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side1)
!| NDGB1          |-->| NDGB1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side2)
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| PHIDIG         |-->| DISCHARGE COEFFICIENT OF THE WEIR
!| T              |-->| BLOCK OF TRACERS
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| UNORM          |-->| NORM OF VELOCITY
!| X              |-->| ABSCISSAE OF NODES
!| Y              |-->| ORDINATES OF NODES
!| ZDIG           |-->| ELEVATIONS OF POINTS OF WEIRS
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_CLSING => CLSING
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DMAX_ARRAY,P_DMIN_ARRAY
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NWEIRS,IOPTAN
      INTEGER, INTENT(IN)             :: NKFROT(*),NBOR(*)
      INTEGER, INTENT(INOUT)          :: LIUBOR(*),LIVBOR(*),LIHBOR(*)
      INTEGER, INTENT(IN)             :: NTRAC
      DOUBLE PRECISION, INTENT(IN)    :: H(*)
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),ZF(*),CHESTR(*)
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UNORM(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: NPSING,NDGA1,NDGB1
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: PHIDIG,ZDIG
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR,LITBOR
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N,IA,IB,NA,NB
      DOUBLE PRECISION :: VALUES(4), MAXVALUES(4), MINVALUES(4)
!
      DOUBLE PRECISION HMIN,PHI,QAB,YAA,YBB,YDEN,YS,HA,HB,ZFA,ZFB
!
!-----------------------------------------------------------------------
!
      HMIN=1.D-3
!
!     COMPUTES UNIT DISCHARGES
!
      DO N=1,NWEIRS
      DO I=1,NPSING%I(N)
!
        IA=NDGA1%ADR(N)%P%I(I)
        IF(IA.GT.0) THEN
          NA=NBOR(IA)
          HA=H(NA)
          ZFA=ZF(NA)
        ELSE
          HA=0.D0
          ZFA=0.D0
        ENDIF
!
        IB=NDGB1%ADR(N)%P%I(I)
        IF(IB.GT.0) THEN
          NB=NBOR(IB)
          HB=H(NB)
          ZFB=ZF(NB)
        ELSE
          HB=0.D0
          ZFB=0.D0
        ENDIF
!
        IF(NCSIZE.GT.1) THEN
          VALUES(1) = HB
          VALUES(2) = HA
          VALUES(3) = ZFA
          VALUES(4) = ZFB

          CALL P_DMAX_ARRAY(VALUES, 4, MAXVALUES)
          CALL P_DMIN_ARRAY(VALUES, 4, MINVALUES)

          HB =MAXVALUES(1)+MINVALUES(1)
          HA =MAXVALUES(2)+MINVALUES(2)
          ZFA=MAXVALUES(3)+MINVALUES(3)
          ZFB=MAXVALUES(4)+MINVALUES(4)
        ENDIF
!
        YAA=HA+ZFA
        YBB=HB+ZFB
!
        YS=ZDIG%ADR(N)%P%R(I)
        PHI=PHIDIG%ADR(N)%P%R(I)
!
        IF(YAA.GT.YBB) THEN
!         CASE WHERE A IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YAA/3.D0
          IF(YBB.LT.YDEN) THEN
            CALL LOIDEN(YAA,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YAA,YBB,YS,PHI,QAB,GRAV)
          ENDIF
        ELSE
!         CASE WHERE B IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YBB/3.D0
          IF(YAA.LT.YDEN) THEN
            CALL LOIDEN(YBB,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YBB,YAA,YS,PHI,QAB,GRAV)
          ENDIF
          QAB=-QAB
        ENDIF
!
!       COMPUTES THE NORMAL DISCHARGE
!       IN CLHUVT ONLY UNORM OF POINTS IN THE DOMAIN
!       WILL BE USED
!
        IF(IA.GT.0) THEN
          IF(HA.LE.HMIN) THEN
            UNORM(IA)=0.D0
          ELSE
            UNORM(IA)=-QAB/HA
          ENDIF
        ENDIF
!
        IF(IB.GT.0) THEN
          IF(HB.LE.HMIN) THEN
            UNORM(IB)=0.D0
          ELSE
            UNORM(IB)=-QAB/HB
          ENDIF
        ENDIF
!
      ENDDO ! I
      ENDDO ! N
!
!     DETERMINES THE NUMERICAL VALUE
!     OF THE BOUNDARY CONDITIONS:
!
      CALL CLHUVT(NWEIRS,NPSING,NDGA1,NDGB1,ZDIG,X,Y,ZF,
     &            IOPTAN,UNORM,CHESTR,NKFROT,KARMAN,T,NTRAC,H,
     &            UBOR,VBOR,TBOR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
