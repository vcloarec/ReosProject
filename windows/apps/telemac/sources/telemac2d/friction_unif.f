!                   ************************
                    SUBROUTINE FRICTION_UNIF
!                   ************************
!
     &(MESH,H,U,V,CHESTR,KFROT,KFROTL,LISRUG,VEGETATION,
     & NDEF,VK,KARMAN,GRAV,T1,T2,CHBORD,CF,CFBOR,FRICOU,NPOIN,ORBVEL)
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    COMPUTES FRICTION FOR EACH NODE WHEN THERE IS ONLY
!+                ONE FRICTION LAW IN THE DOMAIN.
!
!history  F. HUVELIN
!+        20/04/2004
!+
!+   WRITTEN FROM COEFRO.F
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
!history  C. VILLARET (HRW)
!+        22/09/2014
!+        V7P0
!+   Enhanced friction due to waves, depending on logical FRICOU
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/05/2015
!+        V7P1
!+   For boundaries, depth renumbered before being sent to
!+   friction_calc.
!
!history  R. KOPMANN (BAW)
!+        27/01/2016
!+        V7P1
!+   See "RESULTANT VELOCITY IN T2", more cases of computation are added
!+   with KFROTL.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CFBORD         |<--| ADIMENSIONAL FRICTION COEFFICIENT ON BOUNDARIES
!| CHBORD         |-->| DEFAULT'S MANNING ON BOUNDARY
!| CHESTR         |-->| FRICTION COEFFICIENTS
!| FRICOU         |-->| IF YES, WAVE FRICTION ENHANCEMENT IS ACCOUNTED
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KFROT          |-->| LAW OF BOTTOM FRICTION
!| LISRUG         |-->| TURBULENCE REGIME (1: SMOOTH 2: ROUGH)
!| MESH           |-->| MESH STRUCTURE
!| NDEF           |-->| DEFAULT'S MANNING
!| NPOIN          |-->| NUMBER OF NODES
!| ORBVEL         |-->| WAVE ORBITAL VELOCITY
!| T1             |<->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| T2             |<->| WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| VEGETATION     |-->| IF YES, THERE IS VEGETATION FRICTION
!| VK             |-->| KINEMATIC VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_FRICTION_UNIF => FRICTION_UNIF
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(IN)      :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)      :: H,U,V,CHESTR,CHBORD
      INTEGER,          INTENT(IN)      :: KFROT,KFROTL,LISRUG,NPOIN
      LOGICAL,          INTENT(IN)      :: FRICOU,VEGETATION
      DOUBLE PRECISION, INTENT(IN)      :: NDEF
      DOUBLE PRECISION, INTENT(IN)      :: VK,KARMAN,GRAV
!
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: T1,T2
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: CF,CFBOR,ORBVEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMC,IELMH,I,J
      DOUBLE PRECISION, PARAMETER :: MINH=1.D-4
!
!-----------------------------------------------------------------------
!
! =======================================
! INITIALIZATION AND DISCRETIZATION CHECK
! =======================================
!
! ELEMENT TYPE
!
      IELMC = CF%ELM
      IELMH = H%ELM
!
! SAME DISCRETIZATION FOR WATER DEPTH AND FRICTION COEFFICIENT IF NEEDED
!
      IF(KFROT.NE.0.AND.KFROT.NE.2) THEN
!
! MAXIMUM BETWEEN WATER DEPTH AND MINH
!
        CALL CPSTVC(H,T1)
        CALL OS('X=Y     ', X=T1, Y=H)
        IF(IELMC.NE.IELMH) CALL CHGDIS( T1 , IELMH , IELMC , MESH )
!       NIKURADSE LAW WILL DO ITS OWN CLIPPING
        IF(KFROT.NE.5) CALL OS('X=+(Y,C)',X=T1,Y=T1,C=MINH)
      ENDIF
!
! RESULTANT VELOCITY IN T2
!
      IF(KFROT .EQ.1.OR.KFROT .EQ.6.OR.KFROT .EQ.7.OR.
     &   KFROTL.EQ.1.OR.KFROTL.EQ.6.OR.KFROTL.EQ.7) THEN
        CALL CPSTVC(CF,T2)
        CALL OS('X=N(Y,Z)', X=T2, Y=U, Z=V)
        CALL OS('X=+(Y,C)', X=T2, Y=T2, C=1.D-6)
      ENDIF
!
! ===============
! BOTTOM FRICTION
! ===============
!
!     FRICTION COEFFICIENT FOR THE BOTTOM
!
      CALL FRICTION_CALC(1, CF%DIM1, KFROT, NDEF, VK, GRAV,
     &                   KARMAN, CHESTR, T1, T1, T2, CF)
!
!     FRICTION COEFFICIENT FOR VEGETATION
!
      IF(VEGETATION) THEN
        WRITE(LU,*) 'FOR VEGETATION FRICTION YOU NEED:'
        WRITE(LU,*) 'FRICTION DATA=YES'
        WRITE(LU,*) 'ZONES FILE OR FRIC_ID IN GEOMETRY FILE'
        WRITE(LU,*) 'FRICTION DATA FILE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     WAVE INDUCED FRICTION ENHANCMENT (OCONNOR AND YOO, 1988)
!
      IF(FRICOU) THEN
        CALL CPSTVC(CF,T2)
        CALL OS('X=N(Y,Z)', X=T2,  Y=U, Z=V)
        CALL OS('X=+(Y,C)', X=T2, Y=T2, C=1.D-6)
        DO I=1,NPOIN
          CF%R(I) = CF%R(I)*(1.D0 + 0.72D0*ORBVEL%R(I)/T2%R(I))
        ENDDO
      ENDIF
!
! =============
! WALL FRICTION
! =============
!
! WALL FRICTION COMPUTATION
!
      IF(LISRUG.EQ.2) THEN
        DO J = 1, MESH%NPTFR
          I = MESH%NBOR%I(J)
!         DEPTH WITH BOUNDARY NUMBERS
          T1%R(J)=MAX(MINH,H%R(I))
        ENDDO
        CALL FRICTION_CALC(1,MESH%NPTFR,KFROTL,NDEF,VK,GRAV,
     &                     KARMAN,CHBORD,MESH%DISBOR,T1,
     &                     T2,CFBOR)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
