!                   *****************
                    SUBROUTINE RESCUE
!                   *****************
!
     &(U,V,H,S,ZF,T,TRAC0,NTRAC,ITURB,NPOIN,AKEP,SA,TROUVE,ADR_TRAC)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES MISSING DATA/VARIABLES (WHEN RESUMING SIMULATION).
!
!history  J-M HERVOUET (LNH)
!+        31/08/2007
!+        V5P8
!+   First version.
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
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Tracers ranks shifted due to secondary currents variables.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/07/2015
!+        V5P8
!+   English message corrected.
!
!history  R. ATA (EDF LAB, LNHE)
!+        18/05/2018
!+        V7P3
!+   remove shifting of tracer since secondary current is
!+   already considered in the index 34
!
!history  R. ATA (EDF LAB, LNHE)
!+        18/06/2018
!+        V8P0
!+   Add a clipping of water depth
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADR_TRAC       |-->| ADDRESS OF TRACERS IN BLOCK VARSOR
!| AKEP           |-->| IF YES, K AND EPSILON TO BE INITIALISED
!| H              |<--| WATER DEPTH
!| ITURB          |-->| TURBULENCE MODEL
!| NPOIN          |-->| NUMBER OF POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| S              |<--| FREE SURFACE
!| SA             |-->| IF YES, SPALART ALLMARAS VARIABLES TO BE INITIALISED
!| T              |<--| BLOCK OF TRACERS
!| TRAC0          |-->| INITIAL VALUES OF TRACERS
!| TROUVE         |-->| INTEGER ARRAY SAYING IF VARIABLES HAVE BEEN FOUND
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: TROUVE(*),ITURB,NPOIN,NTRAC,ADR_TRAC
      LOGICAL, INTENT(INOUT)          :: AKEP,SA
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TRAC0(NTRAC)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!  VELOCITY U-COMPONENT
!
      IF(TROUVE(1).NE.1 )  THEN
        WRITE(LU,191)
191     FORMAT(1X,'RESCUE : PREVIOUS COMPUTATION RESULTS FILE',
     &         /,1X,'         WITHOUT VELOCITY U, WE FIX IT TO ZERO')
        CALL OV('X=C     ', X=U, C=0.D0, DIM1=NPOIN)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  VELOCITY V-COMPONENT
!
      IF(TROUVE(2).NE.1 )  THEN
        WRITE(LU,201)
201     FORMAT(1X,'RESCUE : PREVIOUS COMPUTATION RESULTS FILE',
     &         /,1X,'         WITHOUT VELOCITY V, WE FIX IT TO ZERO')
        CALL OV('X=C     ', X=V, C=0.D0, DIM1=NPOIN)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  WATER DEPTH
!
      IF(TROUVE(4).NE.1) THEN
        IF(TROUVE(5).EQ.1) THEN
          WRITE(LU,401)
401       FORMAT(1X,'RESCUE : WATER DEPTH COMPUTED WITH BATHYMETRY',
     &         /,1X,'         AND SURFACE ELEVATION')
          CALL OV('X=Y-Z   ', X=H, Y=S, Z=ZF, DIM1=NPOIN)
!
!         CLIP WATER DEPTH IN CASE OF NEGATIVE VALUES
!
          CALL OV('X=+(Y,C)', X=H, Y=H, C=0.D0,DIM1=NPOIN)
!
        ELSE
          WRITE(LU,421)
421       FORMAT(1X,'RESCUE : WATER DEPTH CANNOT BE COMPUTED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  TRACER
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(TROUVE(ADR_TRAC+ITRAC).EQ.0) THEN
            WRITE(LU,901)
901         FORMAT(1X,'RESCUE : PREVIOUS CALCULATION WITHOUT TRACER',
     &           /,1X,'         WE FIX IT TO TRAC0')
            CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC))
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  K AND EPSILON
!
      IF(ITURB.EQ.3.AND.TROUVE(10).EQ.1.AND.TROUVE(11).EQ.1) THEN
        AKEP=.FALSE.
      ENDIF
      IF(ITURB.EQ.3.AND.(TROUVE(10).EQ.0.OR.TROUVE(11).EQ.0)) THEN
        WRITE(LU,951)
951     FORMAT(1X,'RESCUE : K AND EPSILON WILL BE SET AGAIN')
      ENDIF
!
!-----------------------------------------------------------------------
!
!  SPALART ALLMARAS
!
      IF(ITURB.EQ.6.AND.TROUVE(40).EQ.1) THEN
        SA=.FALSE.
      ENDIF
      IF(ITURB.EQ.6.AND.TROUVE(40).EQ.0) THEN
        WRITE(LU,971)
971     FORMAT(1X,'RESCUE : SPALART ALLMARAS VAR WILL BE SET AGAIN')
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
