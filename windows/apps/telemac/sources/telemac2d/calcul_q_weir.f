!                       ************************
                        SUBROUTINE CALCUL_Q_WEIR
!                       ************************
!
!***********************************************************************
! TELEMAC2D   V7P2                                   22/03/2013
!***********************************************************************
!
!brief    COMPUTE THE DISCHARGE ON THE WEIRS WHEN THE TYPE IS EQUAL 2
!+
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        22/03/2013
!+        V6P3
!+   Creation
!
!
!history  C.COULET(ARTELIA)
!+        01/09/2016
!+        V7P2
!+   New management of parallelism
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  C.COULET (ARTELIA)
!+        09/10/2020
!+        V8P2
!+   Trace the computed discharge on a dedicated file + corrections
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          N, ITRAC
      INTEGER          I_1A_1, I_1B_1, I_1A_2, I_1B_2
      INTEGER          I_2A_1, I_2B_1, I_2A_2, I_2B_2
!
      DOUBLE PRECISION PHI
      DOUBLE PRECISION YS1, YS2
      DOUBLE PRECISION SL_1A_1, SL_1B_1, SL_1A_2, SL_1B_2
      DOUBLE PRECISION SL_2A_1, SL_2B_1, SL_2A_2, SL_2B_2
      DOUBLE PRECISION SL_1A, SL_1B
      DOUBLE PRECISION SL_2A, SL_2B
      DOUBLE PRECISION ZF_1A_1, ZF_1B_1, ZF_1A_2, ZF_1B_2
      DOUBLE PRECISION ZF_2A_1, ZF_2B_1, ZF_2A_2, ZF_2B_2
      DOUBLE PRECISION H_1A_1, H_1B_1, H_1A_2, H_1B_2
      DOUBLE PRECISION H_2A_1, H_2B_1, H_2A_2, H_2B_2
      DOUBLE PRECISION SLA, SLB
      DOUBLE PRECISION ZFA, ZFB
      DOUBLE PRECISION H_A, H_B, HMINI
      DOUBLE PRECISION QNSUR4, QELEM
      DOUBLE PRECISION C1A, C1B, C2A, C2B
!
      DOUBLE PRECISION TRAC_1A_1(NTRAC),TRAC_1B_1(NTRAC)
      DOUBLE PRECISION TRAC_1A_2(NTRAC),TRAC_1B_2(NTRAC)
      DOUBLE PRECISION TRAC_2A_1(NTRAC),TRAC_2B_1(NTRAC)
      DOUBLE PRECISION TRAC_2A_2(NTRAC),TRAC_2B_2(NTRAC)
      DOUBLE PRECISION TRAC
      CHARACTER(LEN=10) FMT1
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!
      HMINI = 1.D-2
      PHI = 0.4
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING COLLECT_VALUES'
      CALL COLLECT_VALUES
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM COLLECT_VALUES'
!
!     MANAGEMENT OF THE TRACER
!
      IF(NTRAC.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING COLLECT_VALUES_TRAC'
        CALL COLLECT_VALUES_TRAC
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM COLLECT_VALUES_TRAC'
      ENDIF
!
!     INITIALISATION
!
      DO N = 1, NWEIRS_NODES
        WNODES(N)%QN = 0.D0
        DO ITRAC=1, NTRAC
          WNODES(N)%TRAC(ITRAC) = 0.D0
        ENDDO
      ENDDO
!
!     COMPUTATION
!
      DO N = 1, NWEIRS   !LOOP ON ELEMENTARY PEACE OF WEIRS
        I_1A_1  = WEIRS(N)%N_1A_1
        I_1A_2  = WEIRS(N)%N_1A_2
        I_2A_1  = WEIRS(N)%N_2A_1
        I_2A_2  = WEIRS(N)%N_2A_2
        I_1B_1  = WEIRS(N)%N_1B_1
        I_1B_2  = WEIRS(N)%N_1B_2
        I_2B_1  = WEIRS(N)%N_2B_1
        I_2B_2  = WEIRS(N)%N_2B_2
!
        ZF_1A_1 = WNODES(I_1A_1)%ZFN
        H_1A_1  = WNODES(I_1A_1)%HN
        ZF_1A_2 = WNODES(I_1A_2)%ZFN
        H_1A_2  = WNODES(I_1A_2)%HN
        ZF_2A_1 = WNODES(I_2A_1)%ZFN
        H_2A_1  = WNODES(I_2A_1)%HN
        ZF_2A_2 = WNODES(I_2A_2)%ZFN
        H_2A_2  = WNODES(I_2A_2)%HN
        ZF_1B_1 = WNODES(I_1B_1)%ZFN
        H_1B_1  = WNODES(I_1B_1)%HN
        ZF_1B_2 = WNODES(I_1B_2)%ZFN
        H_1B_2  = WNODES(I_1B_2)%HN
        ZF_2B_1 = WNODES(I_2B_1)%ZFN
        H_2B_1  = WNODES(I_2B_1)%HN
        ZF_2B_2 = WNODES(I_2B_2)%ZFN
        H_2B_2  = WNODES(I_2B_2)%HN
!
!
        ZFA = 0.25D0 * (ZF_1A_1 + ZF_1A_2 + ZF_2A_1 + ZF_2A_2 )
        ZFB = 0.25D0 * (ZF_1B_1 + ZF_1B_2 + ZF_2B_1 + ZF_2B_2 )
!
        SL_1A_1 = ZF_1A_1
        SL_1A_2 = ZF_1A_2
        IF (H_1A_1.GT.HMINI) SL_1A_1 = SL_1A_1 + H_1A_1
        IF (H_1A_2.GT.HMINI) SL_1A_2 = SL_1A_2 + H_1A_2
!
        IF (H_1A_1.GT.HMINI .AND. H_1A_2.GT.HMINI) THEN
          C1A = H_1A_1 / (H_1A_1 + H_1A_2)
        ELSEIF (H_1A_1.GT.HMINI) THEN
          C1A = 1.D0
        ELSEIF (H_1A_2.GT.HMINI) THEN
          C1A = 0.D0
        ELSE
          C1A = 0.D0
        ENDIF
! local correction for some particular case
        IF (SL_1A_1.GT.ZF_1A_2 .AND. SL_1A_2.GT.ZF_1A_1) THEN
          SL_1A = 0.5D0 * (SL_1A_1 + SL_1A_2)
        ELSEIF (SL_1A_1.GT.ZF_1A_2) THEN
          SL_1A = SL_1A_2
          C1A = 0.D0
        ELSEIF (SL_1A_2.GT.ZF_1A_1) THEN
          SL_1A = SL_1A_1
          C1A = 1.D0
        ELSE
          SL_1A = ZFA
          C1A = 0.D0
        ENDIF
!
        SL_2A_1 = ZF_2A_1
        SL_2A_2 = ZF_2A_2
        IF (H_2A_1.GT.HMINI) SL_2A_1 = SL_2A_1 + H_2A_1
        IF (H_2A_2.GT.HMINI) SL_2A_2 = SL_2A_2 + H_2A_2
!
        IF (H_2A_1.GT.HMINI .AND. H_2A_2.GT.HMINI) THEN
          C2A = H_2A_1 / (H_2A_1 + H_2A_2)
        ELSEIF (H_2A_1.GT.HMINI) THEN
          C2A = 1.D0
        ELSEIF (H_2A_2.GT.HMINI) THEN
          C2A = 0.D0
        ELSE
          C2A = 0.D0
        ENDIF
! local correction for some particular case
        IF (SL_2A_1.GT.ZF_2A_2 .AND. SL_2A_2.GT.ZF_2A_1) THEN
          SL_2A = 0.5D0 * (SL_2A_1 + SL_2A_2)
        ELSEIF (SL_2A_1.GT.ZF_2A_2) THEN
          SL_2A = SL_2A_2
          C2A = 0.D0
        ELSEIF (SL_2A_2.GT.ZF_2A_1) THEN
          SL_2A = SL_2A_1
          C2A = 1.D0
        ELSE
          SL_2A = ZFA
          C2A = 0.D0
        ENDIF
!
        SL_1B_1 = ZF_1B_1
        SL_1B_2 = ZF_1B_2
        IF (H_1B_1.GT.HMINI) SL_1B_1 = SL_1B_1 + H_1B_1
        IF (H_1B_2.GT.HMINI) SL_1B_2 = SL_1B_2 + H_1B_2
!
        IF (H_1B_1.GT.HMINI .AND. H_1B_2.GT.HMINI) THEN
          C1B = H_1B_1 / (H_1B_1 + H_1B_2)
        ELSEIF (H_1B_1.GT.HMINI) THEN
          C1B = 1.D0
        ELSEIF (H_1B_2.GT.HMINI) THEN
          C1B = 0.D0
        ELSE
          C1B = 0.D0
        ENDIF
! local correction for some particular case
        IF (SL_1B_1.GT.ZF_1B_2 .AND. SL_1B_2.GT.ZF_1B_1) THEN
          SL_1B = 0.5D0 * (SL_1B_1 + SL_1B_2)
        ELSEIF (SL_1B_1.GT.ZF_1B_2) THEN
          SL_1B = SL_1B_2
          C1B = 0.D0
        ELSEIF (SL_1B_2.GT.ZF_1B_1) THEN
          SL_1B = SL_1B_1
          C1B = 1.D0
        ELSE
          SL_1B = ZFB
          C1B = 0.D0
        ENDIF
!
        SL_2B_1 = ZF_2B_1
        SL_2B_2 = ZF_2B_2
        IF (H_2B_1.GT.HMINI) SL_2B_1 = SL_2B_1 + H_2B_1
        IF (H_2B_2.GT.HMINI) SL_2B_2 = SL_2B_2 + H_2B_2
!
        IF (H_2B_1.GT.HMINI .AND. H_2B_2.GT.HMINI) THEN
          C2B = H_2B_1 / (H_2B_1 + H_2B_2)
        ELSEIF (H_2B_1.GT.HMINI) THEN
          C2B = 1.D0
        ELSEIF (H_2B_2.GT.HMINI) THEN
          C2B = 0.D0
        ELSE
          C2B = 0.D0
        ENDIF
! local correction for some particular case
        IF (SL_2B_1.GT.ZF_2B_2 .AND. SL_2B_2.GT.ZF_2B_1) THEN
          SL_2B = 0.5D0 * (SL_2B_1 + SL_2B_2)
        ELSEIF (SL_2B_1.GT.ZF_2B_2) THEN
          SL_2B = SL_2B_2
          C2B = 0.D0
        ELSEIF (SL_2B_2.GT.ZF_2B_1) THEN
          SL_2B = SL_2B_1
          C2B = 1.D0
        ELSE
          SL_2B = ZFB
          C2B = 0.D0
        ENDIF
!
        SLA = 0.5D0 * (SL_1A + SL_2A)
        SLB = 0.5D0 * (SL_1B + SL_2B)
        H_A = SLA - ZFA
        H_B = SLB - ZFB
!
!       COMPUTATION OF THE DISCHARGE
!
!       ADDING A SECURITY ON THE LEVEL OF THE WEIR
        YS1 = MAX(WEIRS(N)%Z1,ZFA+0.01D0,ZFB+0.01D0)
        YS2 = MAX(WEIRS(N)%Z2,ZFA+0.01D0,ZFB+0.01D0)
!
        QELEM = 0.D0
!       UPSTREAM IS ON SIDE A
        IF (SLA.GT.SLB) THEN
          IF (H_A.GT.HMINI) THEN
            CALL LOI_W_INC(SLA,SLB,YS1,YS2,WEIRS(N)%WIDTH,
     &                     PHI,QELEM,GRAV)
          ENDIF
!       UPSTREAM IS ON SIDE B
        ELSE
          IF (H_B.GT.HMINI) THEN
            CALL LOI_W_INC(SLB,SLA,YS1,YS2,WEIRS(N)%WIDTH,
     &                     PHI,QELEM,GRAV)
            QELEM = -QELEM
          ENDIF
        ENDIF
!
        WEIRS(N)%Q  = QELEM * (1D0-RELAXS) + WEIRS(N)%Q0 * RELAXS
        WEIRS(N)%Q0 = WEIRS(N)%Q
!
! TODO: SHARE DISCHARGES COMPUTED ON IDENTICAL ELEMENTS
!
!       NOW WE DISTRIBUTE THE COMPUTED DISCHARGE OF EACH ELEMENTS OF WEIRS ON NODES
!       QELEM > 0 means the flow is from side A to side B
!
!
        QELEM = 0.5D0 * WEIRS(N)%Q
        QNSUR4 = 0.5D0 * QELEM
        IF (QELEM.GT.0) THEN
          WNODES(I_1A_1)%QN = WNODES(I_1A_1)%QN - C1A * QELEM
          WNODES(I_1A_2)%QN = WNODES(I_1A_2)%QN - (1.D0 - C1A) * QELEM
          WNODES(I_2A_1)%QN = WNODES(I_2A_1)%QN - C2A * QELEM
          WNODES(I_2A_2)%QN = WNODES(I_2A_2)%QN - (1.D0 - C2A) * QELEM
          WNODES(I_1B_1)%QN = WNODES(I_1B_1)%QN + QNSUR4
          WNODES(I_1B_2)%QN = WNODES(I_1B_2)%QN + QNSUR4
          WNODES(I_2B_1)%QN = WNODES(I_2B_1)%QN + QNSUR4
          WNODES(I_2B_2)%QN = WNODES(I_2B_2)%QN + QNSUR4
          DO ITRAC = 1, NTRAC
            TRAC_1A_1(ITRAC) = WNODES(I_1A_1)%TRAC0(ITRAC) *
     &                         C1A * QELEM
            TRAC_1A_2(ITRAC) = WNODES(I_1A_2)%TRAC0(ITRAC) *
     &                         (1.D0 - C1A) * QELEM
            TRAC_2A_1(ITRAC) = WNODES(I_2A_1)%TRAC0(ITRAC) *
     &                         C2A * QELEM
            TRAC_2A_2(ITRAC) = WNODES(I_2A_2)%TRAC0(ITRAC) *
     &                         (1.D0 - C1A) * QELEM
            TRAC = 0.25D0 * (TRAC_1A_1(ITRAC) + TRAC_1A_2(ITRAC) +
     &             TRAC_2A_1(ITRAC) + TRAC_2A_2(ITRAC))
            WNODES(I_1A_1)%TRAC(ITRAC) = WNODES(I_1A_1)%TRAC(ITRAC) -
     &                                    TRAC_1A_1(ITRAC)
            WNODES(I_1A_2)%TRAC(ITRAC) = WNODES(I_1A_2)%TRAC(ITRAC) -
     &                                    TRAC_1A_2(ITRAC)
            WNODES(I_2A_1)%TRAC(ITRAC) = WNODES(I_2A_1)%TRAC(ITRAC) -
     &                                    TRAC_2A_1(ITRAC)
            WNODES(I_2A_2)%TRAC(ITRAC) = WNODES(I_2A_2)%TRAC(ITRAC) -
     &                                    TRAC_2A_2(ITRAC)
            WNODES(I_1B_1)%TRAC(ITRAC) = WNODES(I_1B_1)%TRAC(ITRAC) +
     &                                    TRAC
            WNODES(I_1B_2)%TRAC(ITRAC) = WNODES(I_1B_2)%TRAC(ITRAC) +
     &                                    TRAC
            WNODES(I_2B_1)%TRAC(ITRAC) = WNODES(I_2B_1)%TRAC(ITRAC) +
     &                                    TRAC
            WNODES(I_2B_2)%TRAC(ITRAC) = WNODES(I_2B_2)%TRAC(ITRAC) +
     &                                    TRAC
          ENDDO
        ELSE
          WNODES(I_1A_1)%QN = WNODES(I_1A_1)%QN - QNSUR4
          WNODES(I_1A_2)%QN = WNODES(I_1A_2)%QN - QNSUR4
          WNODES(I_2A_1)%QN = WNODES(I_2A_1)%QN - QNSUR4
          WNODES(I_2A_2)%QN = WNODES(I_2A_2)%QN - QNSUR4
          WNODES(I_1B_1)%QN = WNODES(I_1B_1)%QN + C1B * QELEM
          WNODES(I_1B_2)%QN = WNODES(I_1B_2)%QN + (1.D0 - C1B) * QELEM
          WNODES(I_2B_1)%QN = WNODES(I_2B_1)%QN + C2B * QELEM
          WNODES(I_2B_2)%QN = WNODES(I_2B_2)%QN + (1.D0 - C2B) * QELEM
          DO ITRAC = 1, NTRAC
            TRAC_1B_1(ITRAC) = WNODES(I_1B_1)%TRAC0(ITRAC) *
     &                         C1B * QELEM
            TRAC_1B_2(ITRAC) = WNODES(I_1B_2)%TRAC0(ITRAC) *
     &                         (1.D0 - C1B) * QELEM
            TRAC_2B_1(ITRAC) = WNODES(I_2B_1)%TRAC0(ITRAC) *
     &                         C2B * QELEM
            TRAC_2B_2(ITRAC) = WNODES(I_2B_2)%TRAC0(ITRAC) *
     &                         (1.D0 - C1B) * QELEM
            TRAC = 0.25D0 * (TRAC_1B_1(ITRAC) + TRAC_1B_2(ITRAC) +
     &             TRAC_2B_1(ITRAC) + TRAC_2B_2(ITRAC))
            WNODES(I_1A_1)%TRAC(ITRAC) = WNODES(I_1A_1)%TRAC(ITRAC) -
     &                                    TRAC
            WNODES(I_1A_2)%TRAC(ITRAC) = WNODES(I_1A_2)%TRAC(ITRAC) -
     &                                    TRAC
            WNODES(I_2A_1)%TRAC(ITRAC) = WNODES(I_2A_1)%TRAC(ITRAC) -
     &                                    TRAC
            WNODES(I_2A_2)%TRAC(ITRAC) = WNODES(I_2A_2)%TRAC(ITRAC) -
     &                                    TRAC
            WNODES(I_1B_1)%TRAC(ITRAC) = WNODES(I_1B_1)%TRAC(ITRAC) +
     &                                    TRAC_1B_1(ITRAC)
            WNODES(I_1B_2)%TRAC(ITRAC) = WNODES(I_1B_2)%TRAC(ITRAC) +
     &                                    TRAC_1B_2(ITRAC)
            WNODES(I_2B_1)%TRAC(ITRAC) = WNODES(I_2B_1)%TRAC(ITRAC) +
     &                                    TRAC_2B_1(ITRAC)
            WNODES(I_2B_2)%TRAC(ITRAC) = WNODES(I_2B_2)%TRAC(ITRAC) +
     &                                    TRAC_2B_2(ITRAC)
          ENDDO
        ENDIF
      ENDDO
!
!     FINAL STEP FOR TRACER
!
      IF(NTRAC.GT.0) THEN
        DO N = 1, NWEIRS_NODES
          IF((WNODES(N)%QN.GT.0).OR.(WNODES(N)%QN.LT.0)) THEN
            DO ITRAC=1, NTRAC
              WNODES(N)%TRAC(ITRAC) = WNODES(N)%TRAC(ITRAC) /
     &                                WNODES(N)%QN
            ENDDO
          ELSE
            DO ITRAC=1, NTRAC
              WNODES(N)%TRAC(ITRAC) = 0.D0
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!     MANAGEMENT OF THE OUTPUTFILE FOR DISCHARGE IF REQUIRED
!
      IF(T2D_FILES(T2DWOP)%NAME(1:1).NE.' ') THEN
        WRITE(FMT1,'(I0)') NWEIRS
        WRITE(T2D_FILES(T2DWOP)%LU,
     &        '(F27.15,1X,'//ADJUSTL(FMT1)//'(A1,1X,F27.15))')
     &    AT,
     &    (',',WEIRS(N)%Q/(WEIRS(N)%NB_NEIGH+1), N=1, NWEIRS)
      ENDIF
!
      RETURN
      END
