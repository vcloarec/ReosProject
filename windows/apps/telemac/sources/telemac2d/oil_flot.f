!                   *******************
                    SUBROUTINE OIL_FLOT
!                   *******************
!
     &(PARTICULES,NFLOT,NFLOT_MAX,MESH,LT,VOLDEV,RHO_OIL,NB_COMPO,
     &NB_HAP,FMCOMPO,TBCOMPO,FMHAP,TBHAP,SOLU,ETAL,AREA,NPLAN,GRAV)
!
!***********************************************************************
! TELEMAC2D & TELEMAC3D  V7P0                               21/08/2010
!***********************************************************************
!
!brief    THE USER MUST GIVE :
!+
!+
!+   1) THE TIMESTEP WHEN THE FLOATING BODY IS RELEASED.
!+
!+
!+   2) THE TIME WHEN THE COMPUTATION IS STOPPED FOR THIS FLOATING BODY.
!+
!+
!+   3) THE INITIAL POSITION OF THE FLOATING BODY AT THE TIME OF RELEASE.
!
!history  CEDRIC GOEURY (LHSV)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!history  CEDRIC GOEURY (LHSV)
!+        23/05/2014
!+        V7P0
!+   Second version
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| PARTICLES      |<->| STRUCTURE OF THE OIL PARTICLE
!| NFLOT          |<->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| MESH           |<->| MESH STRUCTURE
!| LT             |-->| CURRENT TIME STEP
!| VOLDEV         |-->| VOLUME OF THE OIL SPILL
!| RHO_OIL        |-->| OIL DENSITY
!| NB_COMPO       |-->| NUMBER OF UNSOLUBLE COMPOUND IN OIL
!| NB_HAP         |-->| NUMBER OF SOLUBLE COMPOUND IN OIL
!| FMCOMPO        |-->| MASS FRACTION OF UNSOLUBLE COMPOUND IN OIL
!| TBCOMPO        |-->| BOILING POINT OF UNSOLUBLE COMPOUND IN OIL
!| FMHAP          |-->| MASS FRACTION OF SOLUBLE COMPOUND IN OIL
!| TBHAP          |-->| BOILING POINT OF SOLUBLE COMPOUND IN OIL
!| SOLU           |-->| SOLUBILITY OF SOLUBLE COMPOUND IN OIL
!| ETAL           |-->| NUMBER OF THE SPREADING MODEL
!| AREA           |-->| AREA OF OIL PARTICLE IF ETAL = 3
!| NPLAN          |-->| NUMBER OF PLANES
!| GRAV           |-->| GRAVITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE STREAMLINE, ONLY : ADD_PARTICLE
      USE INTERFACE_TELEMAC2D, EX_OIL_FLOT => OIL_FLOT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)          :: NFLOT
      INTEGER, INTENT(IN)             :: NFLOT_MAX,LT,NPLAN
      INTEGER, INTENT(IN)             :: NB_COMPO,NB_HAP
      INTEGER, INTENT(IN)             :: ETAL
      DOUBLE PRECISION, INTENT(IN)    :: GRAV
      DOUBLE PRECISION, INTENT(IN)    :: VOLDEV,RHO_OIL,AREA
      DOUBLE PRECISION, INTENT(IN)    :: FMCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: TBCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: FMHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: TBHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: SOLU(NB_HAP)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(OIL_PART), INTENT(INOUT)   :: PARTICULES(NFLOT_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,J,NUM_GLO,NUM_LOC,NUM_MAX,I,NFLOT_OIL
      INTEGER TAGFLO(1),CLSFLO(1),ELTFLO(1),ETAFLO(1)
      DOUBLE PRECISION RHO_EAU,PI,COEF1,COEF2,DELTA,NU,NU2,COORD_X
      DOUBLE PRECISION COORD_Y,XFLOT(1), YFLOT(1),ZFLOT(1),SHPFLO(3,1)
      DOUBLE PRECISION SHZFLO(1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS IS AN EXAMPLE !!!!!!!!!!!!!!!!!!!!
!
      RHO_EAU=1000.D0
      PI = 4.D0 * ATAN( 1.D0 )
!     HARDCODED WATER MOLECULAR VISCOSITY
      NU=1.D-6
      NU2=NU**2
!
      COEF1=1.21D0**4
      COEF2=COEF1/1.53D0**2
      DELTA=(RHO_EAU-RHO_OIL)/(RHO_EAU)
!
      IF(LT.EQ.10000) THEN
        NUM_GLO=0
        NUM_MAX=0
        NUM_LOC=0
        COORD_X=0.D0
        COORD_Y=0.D0
        NUM_MAX=INT(SQRT(REAL(NFLOT_MAX)))
        DO K=0,NUM_MAX-1
          DO J=0,NUM_MAX-1
            COORD_X=336000.D0+REAL(J)
            COORD_Y=371000.D0+REAL(K)
            NUM_GLO=NUM_GLO+1
            NFLOT_OIL = 0
            IF(MESH%DIM1.EQ.3)THEN
              CALL ADD_PARTICLE(COORD_X,COORD_Y,COORD_Y,NUM_GLO,1,
     &                    NFLOT_OIL,1,XFLOT,YFLOT,ZFLOT,TAGFLO,CLSFLO,
     &                    SHPFLO,SHZFLO,ELTFLO,ETAFLO,MESH,NPLAN,
     &                    0.D0,0.D0,0.D0,0.D0,0,0)
            ELSEIF(MESH%DIM1.EQ.2)THEN
              CALL ADD_PARTICLE(COORD_X,COORD_Y,0.D0,NUM_GLO,1,
     &                    NFLOT_OIL,1,XFLOT,YFLOT,YFLOT,TAGFLO,CLSFLO,
     &                    SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
     &                    0.D0,0.D0,0.D0,0.D0,0,0)
            END IF
            IF(NFLOT_OIL.EQ.1)THEN
              NUM_LOC = NUM_LOC+1
!=========================================================================
!----INITIALIZATION PARAMETERS FOR THE CALCULATION OF PARTICULE MOTION----
!=========================================================================
              PARTICULES(NUM_LOC)%XOIL = XFLOT(1)
              PARTICULES(NUM_LOC)%YOIL = YFLOT(1)
              PARTICULES(NUM_LOC)%ID = TAGFLO(1)
              PARTICULES(NUM_LOC)%CLS = CLSFLO(1)
              PARTICULES(NUM_LOC)%SHPOIL(1) = SHPFLO(1,1)
              PARTICULES(NUM_LOC)%SHPOIL(2) = SHPFLO(2,1)
              PARTICULES(NUM_LOC)%SHPOIL(3) = SHPFLO(3,1)
              PARTICULES(NUM_LOC)%ELTOIL = ELTFLO(1)
              IF(MESH%DIM1.EQ.3)THEN
                PARTICULES(NUM_LOC)%ZOIL = ZFLOT(1)
                PARTICULES(NUM_LOC)%ETAOIL = ETAFLO(1)
                PARTICULES(NUM_LOC)%SHZOIL = SHZFLO(1)
              END IF
!=========================================================================
!-----------INITIALIZATION PARAMETERS FOR THE CALCULATION OF OIL----------
!---------------------------WEATHERING PROCESSES--------------------------
!=========================================================================
              PARTICULES(NUM_LOC)%STATE=1
              PARTICULES(NUM_LOC)%TPSECH=0
              IF(ETAL.EQ.1)THEN
                PARTICULES(NUM_LOC)%SURFACE=PI*COEF2*
     &                           (DELTA*GRAV/(VOLDEV*NU2))**(1.D0/6.D0)*
     &                           VOLDEV/DBLE(NFLOT_MAX)
              ELSEIF(ETAL.EQ.2) THEN
                PARTICULES(NUM_LOC)%SURFACE = 0.D0
              ELSEIF(ETAL.EQ.3)THEN
                PARTICULES(NUM_LOC)%SURFACE = AREA
              ELSE
                WRITE(LU,*) 'ETAL=',ETAL,' UNKNOWN IN OIL_FLOT'
                CALL PLANTE(1)
                STOP
              END IF
              PARTICULES(NUM_LOC)%MASS0=(VOLDEV*RHO_OIL)/DBLE(NFLOT_MAX)
              PARTICULES(NUM_LOC)%MASS_EVAP=0.D0
              PARTICULES(NUM_LOC)%MASS_DISS=0.D0
              DO I=1,NB_COMPO
                PARTICULES(NUM_LOC)%COMPO(I)%MASS=
     &                PARTICULES(NUM_LOC)%MASS0*FMCOMPO(I)
                PARTICULES(NUM_LOC)%COMPO(I)%TB=TBCOMPO(I)
                PARTICULES(NUM_LOC)%COMPO(I)%SOL=0.D0
                PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &               PARTICULES(NUM_LOC)%COMPO(I)%MASS
              END DO
              DO I=1,NB_HAP
                PARTICULES(NUM_LOC)%HAP(I)%MASS=
     &                PARTICULES(NUM_LOC)%MASS0*FMHAP(I)
                PARTICULES(NUM_LOC)%HAP(I)%TB=TBHAP(I)
                PARTICULES(NUM_LOC)%HAP(I)%SOL=SOLU(I)
                PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &               PARTICULES(NUM_LOC)%HAP(I)%MASS
              END DO
              NFLOT = NUM_LOC
            END IF
          END DO
        END DO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

