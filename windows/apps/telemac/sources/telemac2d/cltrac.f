!                   *****************
                    SUBROUTINE CLTRAC
!                   *****************
!
     &(NWEIRS,NPSING,NDGA1,NDGB1,ZF,ZDIG,H,T,NBOR,LITBOR,TBOR,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE BOUNDARY CONDITIONS FOR TRACER.
!+                FOR WEIRS.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        03/10/1996
!+        V5P2
!+   MODIFIED
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
!+   Adaptation for parallelism
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        12/06/2013
!+        V6P3
!+   Adaptation to the dynamic allocation of weirs
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| H              |-->| WATER DEPTH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NDGA1          |-->| NDGA1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side1)
!| NDGB1          |-->| NDGB1%ADR(I)%I(NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF WEIR I (side2)
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| T              |-->| BLOCK OF TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ZDIG           |-->| ELEVATIONS OF POINTS OF WEIRS
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NWEIRS,NTRAC
      INTEGER, INTENT(IN)           :: NBOR(*)
      DOUBLE PRECISION, INTENT(IN)  :: ZF(*),H(*)
!
      TYPE(BIEF_OBJ)  , INTENT(IN)  :: NPSING,NDGA1,NDGB1
      TYPE(BIEF_OBJ)  , INTENT(IN)  :: ZDIG
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LITBOR,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,I2,N,N1,N2,ITRAC
!
      DOUBLE PRECISION Z1,Z2,T1,T2
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NTRAC
!
        DO N=1,NWEIRS
        DO I=1,NPSING%I(N)
!
          I1 = NDGA1%ADR(N)%P%I(I)
          I2 = NDGB1%ADR(N)%P%I(I)
          IF(I1.GT.0) THEN
            N1=NBOR(I1)
            Z1=H(N1)+ZF(N1)
            T1=T%ADR(ITRAC)%P%R(N1)
          ELSE
            Z1=0.D0
            T1=0.D0
          ENDIF
          IF(I2.GT.0) THEN
            N2=NBOR(I2)
            Z2=H(N2)+ZF(N2)
            T2=T%ADR(ITRAC)%P%R(N2)
          ELSE
            Z2=0.D0
            T2=0.D0
          ENDIF
!
          IF(NCSIZE.GT.1) THEN
            Z1=P_MAX(MAX(Z1,0.D0))-P_MIN(MAX(-Z1,0.D0))
            Z2=P_MAX(MAX(Z2,0.D0))-P_MIN(MAX(-Z2,0.D0))
            T1=P_MAX(MAX(T1,0.D0))-P_MIN(MAX(-T1,0.D0))
            T2=P_MAX(MAX(T2,0.D0))-P_MIN(MAX(-T2,0.D0))
          ENDIF
!
!         POINT 1
!
          IF(I1.GT.0) THEN
            IF(Z1.GT.Z2.AND.Z1.GT.ZDIG%ADR(N)%P%R(I)) THEN
              TBOR%ADR(ITRAC)%P%R(I1)=T1
              LITBOR%ADR(ITRAC)%P%I(I1)=4
            ELSEIF(Z2.GE.Z1.AND.Z2.GT.ZDIG%ADR(N)%P%R(I)) THEN
              TBOR%ADR(ITRAC)%P%R(I1)=T2
              LITBOR%ADR(ITRAC)%P%I(I1)=5
            ELSE
              LITBOR%ADR(ITRAC)%P%I(I1)=2
            ENDIF
          ENDIF
!
!         POINT 2
!
          IF(I2.GT.0) THEN
            IF(Z1.GT.Z2.AND.Z1.GT.ZDIG%ADR(N)%P%R(I)) THEN
              TBOR%ADR(ITRAC)%P%R(I2)=T1
              LITBOR%ADR(ITRAC)%P%I(I2)=5
            ELSEIF(Z2.GE.Z1.AND.Z2.GT.ZDIG%ADR(N)%P%R(I)) THEN
              TBOR%ADR(ITRAC)%P%R(I2)=T2
              LITBOR%ADR(ITRAC)%P%I(I2)=4
            ELSE
              LITBOR%ADR(ITRAC)%P%I(I2)=2
            ENDIF
          ENDIF
!
        ENDDO ! I
        ENDDO ! N
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
