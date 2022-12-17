!                   *************************
                    SUBROUTINE FRICTION_ZONES
!                   *************************
!
     &(MESH, H, U, V, CHESTR, CHBORD, NKFROT, NDEFMA,
     & KFRO_B, NDEF_B, LISRUG, VEGETATION, VK,
     & KARMAN, GRAV, T1, T2, CF, CFBOR)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    COMPUTES FRICTION FOR EACH NODE AND ZONE.
!
!history  F. HUVELIN
!+        20/04/2004
!+
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
!history  C.VILLARET (HRW)
!+        22/09/2014
!+        V7P0
!+ Enhanced friction due to waves
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/05/2015
!+        V7P1
!+   For boundaries, depth renumbered before being sent to friction_calc.
!
!history R.KOPMANN (BAW)
!+        31/10/2019
!+        V8P0
!+   Lateral boundary roughness coefficient is not read from table
!+   but will set in the steering file or from the boundary file
!+   15 possible coefficients, read of vegetation law
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CFBOR          |<--| ADIMENSIONAL FRICTION COEFFICIENT ON BOUNDARIES
!| CHBORD         |-->| FRICTION COEFFICIENTS ON BOUNDARIES
!| CHESTR         |-->| FRICTION COEFFICIENTS
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KFRO_B         |-->| LAW OF BOTTOM FRICTION FOR BOUNDARIES
!| LISRUG         |-->| TURBULENCE REGIME (1: SMOOTH 2: ROUGH)
!| MESH           |-->| MESH STRUCTURE
!| NDEFMA         |-->| DEFAULT MANNING COEFFICIENT
!| NDEF_B         |-->| DEFAULT MANNING COEFFICIENT OF BOUNDARIES
!| NKFROT         |-->| LAW OF BOTTOM FRICTION FOR EVERY POINT
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| VEGETATION     |-->| IF YES, THERE IS VEGETATION FRICTION
!| VK             |-->| KINEMATIC VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FRICTION_ZONES => FRICTION_ZONES
!
      USE DECLARATIONS_TELEMAC2D, ONLY : FRICOU,NPOIN,ORBVEL,VEGLAW,
     &    VCOEFF,ROEAU
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),    INTENT(IN)    :: MESH
      TYPE(BIEF_OBJ),     INTENT(IN)    :: H, U, V
      TYPE(BIEF_OBJ),     INTENT(IN)    :: CHESTR
      TYPE(BIEF_OBJ),     INTENT(IN)    :: CHBORD
      TYPE(BIEF_OBJ),     INTENT(IN)    :: NKFROT
      TYPE(BIEF_OBJ),     INTENT(IN)    :: NDEFMA
      TYPE(BIEF_OBJ),     INTENT(IN)    :: KFRO_B, NDEF_B
      INTEGER,            INTENT(IN)    :: LISRUG
      LOGICAL,            INTENT(IN)    :: VEGETATION
      DOUBLE PRECISION,   INTENT(IN)    :: VK, KARMAN, GRAV
      TYPE(BIEF_OBJ),     INTENT(INOUT) :: CF, CFBOR
      TYPE(BIEF_OBJ),     INTENT(INOUT) :: T1, T2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I, J
      INTEGER          :: IELMC, IELMH
      DOUBLE PRECISION :: CP
      DOUBLE PRECISION, PARAMETER :: MINH=1.D-4
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      ! ======================================= !
      ! INITIALIZATION AND DISCRETIZATION CHECK !
      ! ======================================= !
!
      ! ELEMENT TYPE
      ! ------------
      IELMC = CF%ELM
      IELMH = H%ELM
!
      ! MAXIMUM BETWEEN WATER DEPTH AND MINH
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL CPSTVC(H,T1)
      CALL OS('X=Y     ', X=T1, Y=H)
      IF(IELMC.NE.IELMH) CALL CHGDIS(T1, IELMH, IELMC, MESH)
      CALL OS('X=+(Y,C)', X=T1, Y=T1, C=MINH)
!
      ! RESULTANT VELOCITY IN T2
      ! ------------------------
      CALL CPSTVC(CF,T2)
      CALL OS('X=N(Y,Z)', X=T2, Y=U, Z=V)
      CALL OS('X=+(Y,C)', X=T2, Y=T2, C=1.D-6)
!
!
      ! =============== !
      ! BOTTOM FRICTION !
      ! =============== !
!
      ! BOTTOM FRICTION CALCULATION
      ! ---------------------------
      DO I = 1, CF%DIM1
!
        ! FRICTION COEFFICIENT FOR THE BOTTOM
        ! -----------------------------------
        CALL FRICTION_CALC
     &       (I, I, NKFROT%I(I), NDEFMA%R(I), VK, GRAV,
     &        KARMAN, CHESTR, T1, T1, T2, CF)
!
! FRICTION COEFFICIENT FOR VEGETATION
! -------------------------------------------------
        IF(VEGETATION) THEN
        SELECT CASE (VEGLAW%I(I))
        CASE (0)
!          WRITE(LU,*)'No vegetation law.'
        CASE (1)
          CALL FRICTION_LINDNER
     &       (T2%R(I), T1%R(I), CF%R(I), VK, GRAV,
     &       VCOEFF%ADR(1)%P%R(I),VCOEFF%ADR(2)%P%R(I),CP)
          IF(CP.LT.-0.9D0) THEN
            CP = 1.5D0*T1%R(I)*
     &           VCOEFF%ADR(1)%P%R(I)/ 
     &           VCOEFF%ADR(2)%P%R(I)/VCOEFF%ADR(2)%P%R(I)
          ENDIF
          CF%R(I) =CF%R(I)+CP
        CASE (2)
          CALL FRICTION_JAERVELAE
     &       (T2%R(I), T1%R(I),
     &       VCOEFF%ADR(1)%P%R(I),VCOEFF%ADR(2)%P%R(I),
     &       VCOEFF%ADR(3)%P%R(I),
     &       VCOEFF%ADR(4)%P%R(I),VCOEFF%ADR(5)%P%R(I),CP)
          CF%R(I) =CF%R(I)+CP
        CASE (3)
          CALL FRICTION_WHITTAKER
     &       (T2%R(I),T1%R(I),VCOEFF%ADR(1)%P%R(I),VCOEFF%ADR(2)%P%R(I),
     &       VCOEFF%ADR(3)%P%R(I),VCOEFF%ADR(4)%P%R(I),
     &       VCOEFF%ADR(5)%P%R(I),VCOEFF%ADR(6)%P%R(I),ROEAU,CP)
          CF%R(I) =CF%R(I)+CP
        CASE (4)
            CALL FRICTION_BAPTIST
     &       (T1%R(I),  VCOEFF%ADR(1)%P%R(I), 
     &       VCOEFF%ADR(2)%P%R(I), VCOEFF%ADR(3)%P%R(I),
     &       KARMAN ,CP)
          CF%R(I) =CF%R(I)+CP
        CASE (5)
          CALL FRICTION_HUTHOFF
     &       (T1%R(I), VCOEFF%ADR(1)%P%R(I),
     &       VCOEFF%ADR(2)%P%R(I), VCOEFF%ADR(3)%P%R(I),
     &       VCOEFF%ADR(4)%P%R(I),CP)
          CF%R(I) = CF%R(I)+ CP
        CASE (6)
          CALL FRICTION_VANVELZEN
     &       (T1%R(I), VCOEFF%ADR(1)%P%R(I),
     &       VCOEFF%ADR(2)%P%R(I), VCOEFF%ADR(3)%P%R(I),
     &       KARMAN, CP)
          CF%R(I) =CF%R(I)+ CP
        CASE (7)
          CALL FRICTION_LUHARNEPF
     &       (T1%R(I), VCOEFF%ADR(1)%P%R(I),
     &       VCOEFF%ADR(2)%P%R(I), VCOEFF%ADR(3)%P%R(I),
     &       VCOEFF%ADR(4)%P%R(I), CP)
          CF%R(I) =CF%R(I)+CP
        CASE (8)
          CALL FRICTION_VASTILA
     &       (T2%R(I), T1%R(I),VCOEFF%ADR(1)%P%R(I),
     &       VCOEFF%ADR(2)%P%R(I), VCOEFF%ADR(3)%P%R(I),
     &       VCOEFF%ADR(4)%P%R(I),VCOEFF%ADR(5)%P%R(I),
     &       VCOEFF%ADR(6)%P%R(I),VCOEFF%ADR(7)%P%R(I),
     &       VCOEFF%ADR(8)%P%R(I),VCOEFF%ADR(9)%P%R(I),CP)
          CF%R(I) =CF%R(I)+CP
        CASE (9)
          CALL FRICTION_HYBRID
     &       (T2%R(I), T1%R(I), VCOEFF%ADR(1)%P%R(I),
     &       VCOEFF%ADR(2)%P%R(I), VCOEFF%ADR(3)%P%R(I),
     &       VCOEFF%ADR(4)%P%R(I), VCOEFF%ADR(5)%P%R(I),
     &       KARMAN,CP)
          CF%R(I) =CF%R(I)+CP
        CASE DEFAULT
!
          WRITE(LU,2) VEGLAW%I(I)
 2             FORMAT(I5,' : UNKNOWN FRICTION LAW')
        CALL PLANTE(1)
        STOP
        END SELECT



        ENDIF !VEGETATION
!
      ENDDO
!
!     CV: WAVE INDUCED FRICTION ENHANCEMENT OCONNOR AND YOO (1988)
!
      IF(FRICOU)THEN
        CALL CPSTVC(CF,T2)
        CALL OS('X=N(Y,Z)',X=T2,Y=U,Z=V)
        CALL OS('X=+(Y,C)',X=T2,Y=T2,C=1.D-6)
        DO I=1,NPOIN
          CF%R(I)= CF%R(I)*(1.D0 + 0.72D0*ORBVEL%R(I)/T2%R(I))
        ENDDO
      ENDIF
!
      ! ============= !
      ! WALL FRICTION !
      ! ============= !
!
      IF(LISRUG.EQ.2) THEN
!
        DO J = 1, MESH%NPTFR
          I = MESH%NBOR%I(J)
!         DEPTH WITH BOUNDARY NUMBERS
          T1%R(J)=MAX(MINH,H%R(I))
!         BOTTOM FRICTION CALCULATION
          CALL FRICTION_CALC
     &         (J,J,KFRO_B%I(J),NDEF_B%R(J),VK,GRAV,KARMAN,
     &          CHBORD,MESH%DISBOR,T1,T2,CFBOR)
        ENDDO
!
      ENDIF
!
!=======================================================================!
!
      RETURN
      END

