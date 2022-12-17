!                   **************************
                    SUBROUTINE FRICTION_CHOICE
!                   **************************
!
     &(FRICTION_PASS)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    MAIN SUBROUTINE FOR FRICTION COMPUTATION.
!
!history  F. HUVELIN
!+        20/04/2004
!+
!+   First version
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/03/2016
!+        V7P2
!+   Removing dummy variable KARMAN
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/10/2016
!+        V7P2
!+   When KFROTL=0, allowing that CHBORD be not initialised and setting
!+   it to SB.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FRICTION_PASS  |-->| IF 0, INITIALISATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_FRICTION_CHOICE => FRICTION_CHOICE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: FRICTION_PASS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION, PARAMETER :: VK = 1.D-6
!
!-----------------------------------------------------------------------
!
! INITIALIZATION
! --------------
!
      IF(DEBUG.GT.0) WRITE(LU,*)'IN FRICTION_CHOICE-INIT STEP'
      IF(FRICTION_PASS.EQ.0) THEN
!
! ZONES INITIALIZATION
! --------------------
!
        IF(FRICTB) THEN
          CALL FRICTION_INIT
          CALL STRCHE
! FH : FOR QUASI-BUBBLE
! FH : 2004/03/01
!JAJ FOR QUADRATIC ELEMENTS
! =>
          IF(CF%ELM.NE.H%ELM) THEN
            IF(CF%ELM.EQ.12 .AND. H%ELM.EQ.11) THEN
              CALL FRICTION_BUBBLE
     &            (IKLE, NPOIN, NELEM, NELMAX, VEGETATION, NKFROT,
     &             CHESTR, NDEFMA, VCOEFF, VEGLAW)
            ELSEIF(CF%ELM.EQ.13 .AND. H%ELM.EQ.11) THEN
              CALL FRICTION_QUAD
     &            (IKLE%I, NELEM, NELMAX, VEGETATION, NKFROT,
     &             CHESTR, NDEFMA, VCOEFF, VEGLAW)
!              WRITE(LU,*)
!     &         'FRICTION_CHOICE::QUADRATIC ELEMENTS NOT IMPLEMENTED.'
!              CALL PLANTE(1)
            ELSE
              WRITE(LU,*)
     &         'FRICTION_CHOICE::DISCRETISATION NOT IMPLEMENTED.'
              WRITE(LU,*) 'CF%ELM, H%ELM: ',CF%ELM, H%ELM
              CALL PLANTE(1)
            ENDIF
          ENDIF
! <=
!JAJ FOR QUADRATIC ELEMENTS
! FH : 2004/03/01
! FH : FOR QUASI-BUBBLE
!
!        UNIFORM CASE
!        ------------
!
        ELSE
          ! CHESTR FOR BOUNDARY CONDITIONS INITIALIZATION
          ! -----------------------------------------------
          IF(LISRUG.EQ.2) THEN
            IF(KFROTL.EQ.1) THEN
              DO I = 1, NPTFR
                CHBORD%R(I) = CHESTR%R(MESH%NBOR%I(I))
              ENDDO
            ELSEIF(KFROTL.NE.0) THEN
!             BOUNDARY CONDITIONS FILE DATA IF ANY SUPERSEDE
!             THE KEY-WORD ROUGHNESS COEFFICIENT OF BOUNDARIES
              IF(P_DOTS(CHBORD,CHBORD,MESH).EQ.0.D0) THEN
                CALL OS('X=C     ', X=CHBORD, C=SB)
              ENDIF
            ELSE
!             IF KFROTL=0 CHBORD INITIALISED AT SB (BUT NOT USED ?)
              CALL OS('X=C     ', X=CHBORD, C=SB)
            ENDIF
          ENDIF
          ! TYPE OF FRICTION LAW FOR EACH NODE
          ! ----------------------------------
          DO I=1, CF%DIM1
            NKFROT%I(I) = KFROT
          ENDDO
!
        ENDIF
!
!     COMPUTATION
!     -----------
!
      ELSE
!
        ! FRICTION BY ZONES
        ! -----------------
        IF(FRICTB) THEN
        IF(DEBUG.GT.0) WRITE(LU,*)'FRICTION_CHOICE-START FRICTION ZONES'
! FH : FOR QUASI-BUBBLE
! FH : 2004/03/01
!JAJ FOR QUADRATIC ELEMENTS
! =>
          IF(CF%ELM.NE.H%ELM) THEN
            IF(CF%ELM.EQ.12 .AND. H%ELM.EQ.11) THEN
              CALL FRICTION_BUBBLE
     &            (IKLE, NPOIN, NELEM, NELMAX, VEGETATION, NKFROT,
     &             CHESTR, NDEFMA, VCOEFF, VEGLAW)
            ELSEIF(CF%ELM.EQ.13 .AND. H%ELM.EQ.11) THEN
              CALL FRICTION_QUAD
     &            (IKLE%I, NELEM, NELMAX, VEGETATION, NKFROT,
     &             CHESTR, NDEFMA, VCOEFF, VEGLAW)
            ELSE
              WRITE(LU,*)
     &         'FRICTION_CHOICE::DISCRETISATION NOT IMPLEMENTED.'
              WRITE(LU,*) 'CF%ELM, H%ELM: ',CF%ELM, H%ELM
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
!
          CALL FRICTION_ZONES
     &         (MESH, H, U, V, CHESTR, CHBORD, NKFROT, NDEFMA,
     &          KFRO_B, NDEF_B, LISRUG,
     &          VEGETATION, VK, KARMAN, GRAV, T1, T2, CF, CFBOR)
! <=
!JAJ FOR QUADRATIC ELEMENTS
! FH : 2004/03/01
! FH : FOR QUASI-BUBBLE
        IF(DEBUG.GT.0)WRITE(LU,*)'IN FRICTION_CHOICE-END FRICTION ZONES'
!
        ! UNIFORM FRICTION
        ! ----------------
        ELSE
          IF(DEBUG.GT.0) WRITE(LU,*)'IN FRICTION_CHOICE-CALLING UNIF'
          CALL FRICTION_UNIF
     &         (MESH,H,U,V,CHESTR,KFROT,KFROTL, LISRUG,
     &          VEGETATION, NDEF, VK, KARMAN, GRAV, T1,
     &          T2, CHBORD, CF, CFBOR, FRICOU, NPOIN, ORBVEL)
!
          IF(DEBUG.GT.0) WRITE(LU,*)'IN FRICTION_CHOICE-BACK FROM UNIF'
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
