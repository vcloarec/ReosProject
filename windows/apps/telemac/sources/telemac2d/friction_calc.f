!                   ************************
                    SUBROUTINE FRICTION_CALC
!                   ************************
!
     &(N_START, N_END, KFROT, NDEF, VK, GRAV,
     & KARMAN, CHESTR, DW_MESH, HC, VRES, CF)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    SETS THE FRICTION COEFFICIENT.
!code
!+     FRICTION LAWS PROGRAMMED :
!+
!+     KFROT = 0 :  NO FRICTION
!+     KFROT = 1 :  LAW OF HAALAND
!+     KFROT = 2 :  LAW OF CHEZY
!+     KFROT = 3 :  LAW OF STRICKLER
!+     KFROT = 4 :  LAW OF MANNING
!+     KFROT = 5 :  LAW OF NIKURADSE
!+     KFROT = 6 :  LOG LAW OF WALL
!+     KFROT = 7 :  LAW OF COLEBROOK-WHITE
!
!note     LAWS CODED UP : NO FRICTION; HAALAND; CHEZY;
!+                STRICKLER; MANNING; NIKURADSE; WALL; COLEBROOK-WHITE
!
!history  F. HUVELIN
!+        20/04/2004
!+
!
!history  J-M HERVOUET (LNHE)
!+
!+        V5P5
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CHESTR         |-->| FRICTION PARAMETER
!| DW_MESH        |-->| DISTANCE TO THE BOUNDARY
!| GRAV           |-->| GRAVITY ACCELERATION
!| HC             |-->| WATER DEPTH : MAX(H,HMIN)
!| KARMAN         |-->| VON KARMAN'S CONSTANT
!| KFROT          |-->| LAW USED FOR THE CALCULATION
!| NDEF           |-->| DEFAULT'S MANNING
!| N_START,N_END  |-->| STARTING AND ENDING POINT
!| VK             |-->| KINEMATIC VISCOSITY
!| VRES           |-->| RESULTANT VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: N_START, N_END, KFROT
      DOUBLE PRECISION, INTENT(IN)    :: NDEF, VK, GRAV, KARMAN
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CHESTR,DW_MESH,HC,VRES
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                       :: I, ITER
      DOUBLE PRECISION              :: TIERS
      DOUBLE PRECISION              :: UNORM, INLOG, AUX
      DOUBLE PRECISION              :: OLDUST, OLDCF
      DOUBLE PRECISION              :: RE, UST, DW, DWPLUS
      DOUBLE PRECISION              :: TERM1,TERM2
!
!-----------------------------------------------------------------------
!
      TIERS = 1.D0/3.D0
      SELECT CASE (KFROT)
!
! NO FRICTION
! -----------
!
      CASE(0)
!
        DO I = N_START, N_END
          CF%R(I) = 0.D0
        ENDDO
!
! LAW OF HAALAND
! --------------
!
      CASE(1)
!
        DO I = N_START, N_END
          UNORM = MAX(VRES%R(I),1.D-6)
!                       1.D-6: LAMINAR VISCOSITY OF WATER
          INLOG = (6.9D0*1.D-6/4.D0  /HC%R(I)/UNORM)**3
     &          + (CHESTR%R(I)/14.8D0/HC%R(I))**3.33
          INLOG = MIN(1.D0-1.D-6,INLOG)
          AUX   = -0.6D0*LOG(INLOG)/LOG(10.D0)
          CF%R(I) = 0.25D0 / AUX**2
        ENDDO
!
! LAW OF CHEZY
! ------------
!
      CASE(2)
!
        DO I = N_START, N_END
          CF%R(I) = 2.D0*GRAV/(CHESTR%R(I)**2)
        ENDDO
!
! LAW OF STRICKLER
! ----------------
!
      CASE(3)
!
        DO I = N_START, N_END
          CF%R(I) = 2.D0*GRAV/CHESTR%R(I)**2/HC%R(I)**TIERS
        ENDDO
!
! LAW OF MANNING
! --------------
!
      CASE(4)
!
        DO I = N_START, N_END
          CF%R(I) = 2.D0*GRAV*(CHESTR%R(I)**2)/HC%R(I)**TIERS
        ENDDO
!
! LAW OF NIKURADSE
! ----------------
!
      CASE(5)
!
!       NOTE: 11.036 IS 30.D0/EXP(1.D0)
        DO I = N_START, N_END
          AUX=MAX(1.001D0,HC%R(I)*11.036D0/CHESTR%R(I))
          CF%R(I) = 2.D0 / (LOG(AUX)/KARMAN)**2
        ENDDO
!
! LOG LAW OF WALL FOR VISCOUS FRICTION
! ---------------
!
      CASE(6)
!
        DO I = N_START, N_END
!
          IF(VRES%R(I) < 1.0D-9) THEN
            CF%R(I) = 20.D0 ! RISMO2D = 10.D0 AND TELEMAC2D = 2*10.D0
          ELSE
!
            DW = 0.33D0*DW_MESH%R(I)
!
            IF (CHESTR%R(I) < 1.0D-9) THEN
!
! ITERATIVE COMPUTATION OF FRICTION VELOCITY UST
! ----------------------------------------------
!
              UST    = 100.0*VK/DW
              OLDUST = 0.D0
!
              DO ITER = 1, 50
!
                IF (ABS((UST-OLDUST)/UST)<=1.0D-6) EXIT
!
                DWPLUS = DW*UST/VK
!
                IF (DWPLUS < 11.D0) DWPLUS = 11.D0
!
                OLDUST = UST
                UST    = KARMAN*VRES%R(I) / LOG(9.D0*DWPLUS)
!
              ENDDO
!
            ELSE
              UST = KARMAN*VRES%R(I) / (LOG(DW/CHESTR%R(I))+8.5D0)
              RE  = CHESTR%R(I)*UST  / VK
!
              IF (RE < 70.D0) THEN
!
! ITERATIVE COMPUTATION OF FRICTION VELOCITY UST
! ----------------------------------------------
!
                OLDUST = 0.D0
!
                DO ITER = 1, 50
!
                  IF (ABS((UST-OLDUST)/UST)<=1.0D-6) EXIT
!
                  DWPLUS = DW*UST/VK
!
                  IF (DWPLUS < 11.D0) DWPLUS = 11.D0
!
                  RE     = CHESTR%R(I)*UST/VK
                  OLDUST = UST
!
                  IF (RE < 3.32D0) THEN
                    UST = KARMAN*VRES%R(I) / LOG(9.D0*DWPLUS)
                  ELSE
                    UST = KARMAN*VRES%R(I)
     &                  / (  LOG(DW/CHESTR%R(I))
     &                     + 3.32D0*LOG(RE)/RE
     &                     + KARMAN*(8.5D0-9.96D0/RE))
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
!
            DWPLUS = DW*UST/VK
!
            IF (DWPLUS < 11.D0 ) THEN
              UST = 11.0*VK / DW
              UST = SQRT(VRES%R(I)*VK/DW)
            ENDIF
!
          CF%R(I) = 2.D0*UST**2 / VRES%R(I)**2
          ENDIF
        ENDDO
!
! LAW OF COLEBROOK-WHITE
! ----------------------
!
      CASE(7)
!
        DO I = N_START, N_END
!
          RE = 4.D0*VRES%R(I)*HC%R(I)/VK
!
! THE ORIGINAL CONDITION FOR LAMINAR/TURBULENT FLOW
! COULD NOT BE HOLD (PROBLEMS DURING NR ITERATION):
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
          IF(RE.GT.500.D0) THEN
!
! TEST CONDITION: ROUGHNESS LESS THAN FLOW DEPTH
! ----------------------------------------------
!
            IF(CHESTR%R(I) < HC%R(I)) THEN
!             NDEF : DEFAULT MANNING'S N
              CF%R(I) = 2.D0*(NDEF**2)*GRAV/HC%R(I)**TIERS
            ELSE
              TERM1   = 4.4D0 / RE
              TERM2   = CHESTR%R(I) / 14.84D0 / HC%R(I)
              CF%R(I) = 2.5D0 ! INITIALIZE CF=1/SQRT(CF) FOR ITERATION
              OLDCF   = 0.D0
!
              DO ITER = 1, 50
                IF (ABS((CF%R(I)-OLDCF)/CF%R(I))<=1.0D-6) EXIT
                OLDCF = CF%R(I)
                CF%R(I) = -2.03D0*LOG10(OLDCF*TERM1 + TERM2)
              ENDDO
!
              IF (ITER.GE.50) THEN
                CF%R(I) = -2.03D0*LOG10(TERM2)
              ENDIF
!
              CF%R(I) = 2.D0 / (CF%R(I)**2) / 8.D0
            ENDIF
!
          ELSEIF (RE.GT.100.D0 ) THEN
            CF%R(I) = 16.D0 / RE
          ELSE
            CF%R(I) = 0.16D0
          ENDIF
        ENDDO
!
! OTHER CASES
! -----------
!
      CASE DEFAULT
!
        WRITE(LU,2) KFROT
2       FORMAT(I5,' : UNKNOWN FRICTION LAW')
        CALL PLANTE(1)
        STOP
!
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
