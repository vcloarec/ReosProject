!                   *****************
                    SUBROUTINE DERIVE
!                   *****************
!
     &(U,V,W,DT,AT,X,Y,Z,IKLE,IFABOR,LT,IELM,IELMU,NDP,NDP2,
     & NPOIN,NPOIN2,
     & NELEM,NELMAX,SURDET,XFLOT,YFLOT,ZFLOT,
     & SHPFLO,SHZFLO,TAGFLO,CLSFLO,ELTFLO,ETAFLO,
     & NFLOT,NFLOT_MAX,MESH,
     & ISUB,DX,DY,DZ,ELTBUF,SHPBUF,SHZBUF,SIZEBUF,STOCHA,VISC,
     & NPLAN,ZSTAR,TRANSF,
     & AALGAE,DALGAE,RALGAE,EALGAE,TALGAE,YALGAE,
     & REL_ALGAE,TW1_ALGAE,TW2_ALGAE,A_ALGAE,
     & ORBVEL,AK,EP,H)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    - COMPUTES THE BARYCENTRIC COORDINATES OF A FLOAT
!+                  IN THE MESH AT THE TIME OF RELEASE.
!+
!+            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!+                  WHICH IS CARRIED WITHOUT FRICTION BY THE CURRENT
!+                 (SUBSEQUENT TIMESTEPS).
!
!history  J-M JANIN (LNH)
!+        18/08/94
!+        V5P1
!+   Original version.
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
!+        19/06/2012
!+        V6P2
!+   Adapted for calling SCARACT instead of CHAR11. However parallelism
!+   will require further modifications.
!
!history  J-M HERVOUET (LNHE)
!+        12/03/2013
!+        V6P3
!+   New file format for Tecplot. Works in parallel. Works in 3D.
!
!history  J-M HERVOUET (LNHE)
!+        12/03/2013
!+        V6P3
!+   Arguments STOCHA and VISC added
!
!history  A Joly
!+        20/06/2013
!+        V6P3
!+   New conditions added to treat algae transport
!+     - Only tested in 2D
!+     - Does not work for quadratic elements
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        14/08/2015
!+        V7P1
!+   Hardcoded argument NRK added for SCARACT.
!
!history  R. ATA (EDF LAB, LNHE)
!+        14/01/2017
!+        V7P2
!+  bug fix and  better preparation of the call of SCARACT for 3D.
!+    see lines between !RA
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!history  M.S.TURNBULL (HRW)
!+        26/11/2019
!+        V8P2
!+   Algae Dislodgement
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A_ALGAE        |-->| RATE OF DEGRADATION FOR ALGAE
!| CLSFLO         |-->| CLASS OF FLOATS
!| DT             |-->| TIME STEP (I.E. TIME INTERVAL).
!| DX             |<->| WORK ARRAY (DISPLACEMENTS ALONG X)
!| DY             |<->| WORK ARRAY (DISPLACEMENTS ALONG Y)
!| DZ             |<->| WORK ARRAY (DISPLACEMENTS ALONG Z)
!| ELTBUF         |<->| WORK ARRAY
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| ETAFLO         |<->| LEVELS WHERE ARE THE FLOATS
!|                |   | FOR FLOATS POSITIONS.
!| IELM           |-->| TYPE OF ELEMENT.
!| IELMU          |-->| TYPE OF ELEMENT FOR VELOCITIES.
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF ANOTHER ELEMENT
!|                |   | IF IFABOR NEGATIVE OR 0, THE EDGE IS A
!|                |   | LIQUID OR PERIODIC BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| ISUB           |<->| ARRIVAL SUB-DOMAIN OF PARTICLES.
!| LT             |-->| TIME STEP NUMBER.
!| MESH           |<->| MESH STRUCTURE.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT FOR IELM
!| NDP2           |-->| NUMBER OF POINTS PER ELEMENT FOR IELMU
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NFLOT          |<->| NUMBER OF FLOATS.
!| NFLOT_MAX      |<->| MAXIMUM NUMBER OF FLOATS.
!| NPLAN          |-->| NUMBER OF PLANES FOR 3D (1 FOR 2D)
!| NPOIN          |-->| NUMBER OF POINTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NSTAR          |-->| VERTICAL DISTRIBUTION OF PLANES
!| ORBVEL         |-->| WAVE ORBITAL VELOCITY
!| REL_ALGAE      |-->| ALGAE RELEASE TYPE
!| SHPBUF         |<->| WORK ARRAY
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR
!|                |   | ELEMENTS.
!| SHZBUF         |<->| WORK ARRAY
!| SHZFLO         |<->| BARYCENTRIC COORDINATE ON VERTICAL
!| SIZEBUF        |-->| DILMENSION OF SOME WORK ARRAYS
!| SURDET         |-->| 1/DETERMINANT, USED IN ISOPARAMETRIC
!|                |   | TRANSFORMATION.
!| TAGFLO         |-->| TAGS OF FLOATS
!| TALGAE         |-->| DURATION BEFORE ALGAE RELEASE
!| TRANSF         |-->| 3D MESH TRANSFORMATION
!| TW1_ALGAE      |-->| WAVE ORBITAL VELOCITY THRESHOLD FOR ALGAE 1
!| TW2_ALGAE      |-->| WAVE ORBITAL VELOCITY THRESHOLD FOR ALGAE 2
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| W              |-->| Z-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YALGAE         |-->| ALGAE TYPE
!| YFLOT          |<->| ORDINATES OF FLOATS
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!| ZFLOT          |<->| ELEVATIONS OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DERIVE => DERIVE
      USE DECLARATIONS_TELEMAC, ONLY : DEJA_DERIVE, SVOID_DERIVE,
     &                                 INIT_ALG, SIZEBUF2_D,BUFF_1D_D,
     &                                 BUFF_2D_D, ALG_DISLODGE,
     &                                 BUFF_ELT_I
      USE STREAMLINE
      USE ALGAE_TRANSP
!
      USE DECLARATIONS_SPECIAL
      USE INITIAL_DROGUES, ONLY : PARCLSS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,IELMU,NDP,NELEM
      INTEGER         , INTENT(IN)    :: NELMAX,SIZEBUF,NPOIN2
      INTEGER         , INTENT(IN)    :: NFLOT_MAX,STOCHA,NPLAN,TRANSF
      INTEGER         , INTENT(INOUT) :: NFLOT
      INTEGER         , INTENT(IN)    :: NDP2
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT
      DOUBLE PRECISION, INTENT(IN)    :: U(*),V(*),W(*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),Z(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP2)
      INTEGER         , INTENT(IN)    :: IFABOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM),ZSTAR(NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX),DX(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX),DY(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NFLOT_MAX),DZ(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: TAGFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: CLSFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ELTFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ETAFLO(NFLOT_MAX)
      INTEGER         , INTENT(INOUT) :: ELTBUF(SIZEBUF)
      INTEGER         , INTENT(INOUT) :: ISUB(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(NDP,NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZFLO(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPBUF(NDP,SIZEBUF)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZBUF(SIZEBUF)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: VISC
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      LOGICAL         , OPTIONAL, INTENT(IN) :: AALGAE
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: AK(*),EP(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: ORBVEL(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: H(NPOIN)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: DALGAE(*),RALGAE(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: EALGAE(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: TALGAE(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: TW1_ALGAE(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: TW2_ALGAE(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: A_ALGAE(*)
      INTEGER         , OPTIONAL, INTENT(IN) :: YALGAE(*)
      INTEGER         , OPTIONAL, INTENT(IN) :: REL_ALGAE(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFLOT,FRE(1),FREBUF(1),ELT,ETF
      INTEGER N1,N2,N3,N4,N5,N6,NOMB,SENS,NRK
!
      INTEGER REL_ALG
      DOUBLE PRECISION TW0, TW1, TW2, A_ALG
      DOUBLE PRECISION, ALLOCATABLE :: TW(:)
!
      LOGICAL SSIGMA
!
      LOGICAL ALGAE
!
      SAVE TW
!
!-----------------------------------------------------------------------
!
!     HARDCODED NUMBER OF SUB-STEPS FOR COMPUTING THE PATH-LINES
!
      NRK=3
!
!-----------------------------------------------------------------------
!
!     CHECKING ARGUMENTS FOR ALGAE
!
      IF(PRESENT(AALGAE)) THEN
        ALGAE=AALGAE
      ELSE
        ALGAE=.FALSE.
      ENDIF
      IF(ALGAE) THEN
        IF(.NOT.PRESENT(AK).OR.
     &     .NOT.PRESENT(EP).OR.
     &     .NOT.PRESENT(H).OR.
     &     .NOT.PRESENT(DALGAE).OR.
     &     .NOT.PRESENT(RALGAE).OR.
     &     .NOT.PRESENT(EALGAE).OR.
     &     .NOT.PRESENT(TALGAE).OR.
     &     .NOT.PRESENT(YALGAE)) THEN
          WRITE(LU,*) 'DERIVE: MISSING ARGUMENTS FOR ALGAE'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PARAMETERISING THE CALL TO SCARACT
!
!     NUMBER OF PLANES, NOW GOT BY ARGUMENT
!      NPLAN=NPOIN/NPOIN2
!     NO VARIABLE TO INTERPOLATE AT THE FOOT OF CHARACTERISTICS
      NOMB=0
!     FORWARD TRACKING
      SENS=1
!     CASE OF 3D CALLS, DETECT IF SIGMA TRANSFORM
      SSIGMA=IELM.EQ.41.AND.TRANSF.NE.0.AND.TRANSF.NE.5
!
      IF(IELM.NE.11.AND.IELM.NE.41) THEN
        WRITE(LU,124) IELM
124     FORMAT(1X,'DERIVE : UNEXPECTED TYPE OF ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISING SVOID_DERIVE AND HEADER OF A TECPLOT FILE
!
      IF(.NOT.DEJA_DERIVE) THEN
!
!       THOUGH NOMB = 0, THESE COMPONENTS WILL BE USED IN SCARACT
!
        SVOID_DERIVE%TYPE=2
        SVOID_DERIVE%NAT = 1
        SVOID_DERIVE%DIM1=1
        ALLOCATE(SVOID_DERIVE%R(1))
!
        IF(ALGAE.AND.ALG_DISLODGE)THEN
          ALLOCATE(TW(NFLOT_MAX))
        END IF
!
        DEJA_DERIVE=.TRUE.
!
      ENDIF
!
      SVOID_DERIVE%ELM=IELM
!
!-----------------------------------------------------------------------
!
!     TRAJECTORIES COMPUTED FOR ALL POINTS
!
!     ALLOCATE THE ALGAE VARIABLES IF NEEDED
!
      IF(ALGAE.AND.INIT_ALG) THEN
        INIT_ALG=.FALSE.
!       VERIFY THAT THE BUFFER SIZE IS BIG ENOUGH FOR PARTICLE TRANSPORT
        IF(NFLOT_MAX.GT.SIZEBUF)THEN
          SIZEBUF2_D=NFLOT_MAX
          ALLOCATE(BUFF_1D_D(SIZEBUF2_D))
          ALLOCATE(BUFF_2D_D(NDP,SIZEBUF2_D))
          ALLOCATE(BUFF_ELT_I(SIZEBUF2_D))
        ELSE
!         SET IT TO TRUE SO DEALL_BIEF DOES NOT TRY TO DEALLOCATE IT
          INIT_ALG=.TRUE.
        ENDIF
      ENDIF
!
      IF(ALGAE) THEN
        IF(LT.EQ.1) THEN
          IF(IELMU.EQ.11) THEN
            CALL INTERP_ALGAE(NFLOT,NFLOT_MAX,SHPFLO,SHZFLO,ELTFLO,
     &          U_X_AV_0%R,U_Y_AV_0%R,U_Z_AV_0%R,K_AV_0%R,EPS_AV_0%R,
     &          H_FLU%R,NPOIN,IELM,NDP,NDP2,NPLAN,NELMAX,IKLE,SHZBUF,
     &          IELMU,NPOIN,U,V,W,AK,EP,H)
            CALL INTERP_ALGAE(NFLOT,NFLOT_MAX,SHPFLO,SHZFLO,ELTFLO,
     &          U_X_AV%R,U_Y_AV%R,U_Z_AV%R,K_AV%R,EPS_AV%R,
     &          H_FLU%R,NPOIN,IELM,NDP,NDP2,NPLAN,NELMAX,IKLE,SHZBUF,
     &          IELMU,NPOIN,U,V,W,AK,EP,H)
          ELSEIF(IELMU.EQ.12) THEN
            CALL INTERP_ALGAE(NFLOT,NFLOT_MAX,SHPFLO,SHZFLO,ELTFLO,
     &          U_X_AV_0%R,U_Y_AV_0%R,U_Z_AV_0%R,K_AV_0%R,EPS_AV_0%R,
     &          H_FLU%R,NPOIN,IELM,NDP,NDP2,NPLAN,NELMAX,IKLE,SHZBUF,
     &          IELMU,NPOIN+NELMAX,U,V,W,AK,EP,H)
            CALL INTERP_ALGAE(NFLOT,NFLOT_MAX,SHPFLO,SHZFLO,ELTFLO,
     &          U_X_AV%R,U_Y_AV%R,U_Z_AV%R,K_AV%R,EPS_AV%R,
     &          H_FLU%R,NPOIN,IELM,NDP,NDP2,NPLAN,NELMAX,IKLE,SHZBUF,
     &          IELMU,NPOIN+NELMAX,U,V,W,AK,EP,H)
          ENDIF
        ENDIF
      ENDIF
!     BACK CONVERSION WHEN SIGMA TRANSFORM
      IF(SSIGMA)THEN
        DO IFLOT=1,NFLOT
          ETF = ETAFLO(IFLOT)
          ZFLOT(IFLOT)=ZSTAR(ETF)+ SHZFLO(IFLOT)*
     &                (ZSTAR(ETF+1)-ZSTAR(ETF))
        ENDDO
      ENDIF
!
! -----------------
! IF ALGAE IS .TRUE., THEN USE ALGAE TRANSPORT
! OTHERWISE THIS IS A NORMAL DROGUE TRANSPORT
! -----------------
!
      IF(ALGAE)THEN
!
! FILL I_A_GL, WHICH WILL BE USED TO VERIFY THAT THE ALGAE INFO IS SENT IN
! THE SAME FASHION AS THE PARTICLE POSITIONS
!
        DO IFLOT=1,NFLOT
          I_A_GL%I(IFLOT)=TAGFLO(IFLOT)
        END DO
!
        IF(IELMU.EQ.11)THEN
          CALL INTERP_ALGAE(NFLOT,NFLOT_MAX,SHPFLO,SHZFLO,ELTFLO,
     &          U_X_AV%R,U_Y_AV%R,U_Z_AV%R,K_AV%R,EPS_AV%R,
     &          H_FLU%R,NPOIN,IELM,NDP,NDP2,NPLAN,NELMAX,IKLE,SHZBUF,
     &          IELMU,NPOIN,U,V,W,AK,EP,H)
          IF(ALG_DISLODGE)THEN
            CALL BIEF_INTERP(ORBVEL,TW,SHPFLO,NDP,SHZFLO,
     &           ELTFLO,SHZBUF,FRE,
     &           ELTFLO,NFLOT,NPOIN,NPLAN,IELMU,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
          END IF
        ELSEIF(IELMU.EQ.12)THEN
          CALL INTERP_ALGAE(NFLOT,NFLOT_MAX,SHPFLO,SHZFLO,ELTFLO,
     &          U_X_AV%R,U_Y_AV%R,U_Z_AV%R,K_AV%R,EPS_AV%R,
     &          H_FLU%R,NPOIN,IELM,NDP,NDP2,NPLAN,NELMAX,IKLE,SHZBUF,
     &          IELMU,NPOIN+NELMAX,U,V,W,AK,EP,H)
          IF(ALG_DISLODGE)THEN
            CALL BIEF_INTERP(ORBVEL,TW,SHPFLO,NDP,SHZFLO,
     &           ELTFLO,SHZBUF,FRE,
     &           ELTFLO,NFLOT,NPOIN+NELMAX,NPLAN,IELMU,IKLE,NELMAX,
     &           .FALSE.,.FALSE.)
          END IF
        END IF
!
        DO IFLOT=1,NFLOT_MAX
          PARCLSS%I(IFLOT) = CLSFLO(IFLOT)
        END DO
!
        IF(ALG_DISLODGE)THEN
          DO IFLOT=1,NFLOT
            REL_ALG = REL_ALGAE(CLSFLO(IFLOT))
            IF (REL_ALG.EQ.2) THEN
              TW1 = TW1_ALGAE(CLSFLO(IFLOT))
              TW2 = TW2_ALGAE(CLSFLO(IFLOT))
              A_ALG = A_ALGAE(CLSFLO(IFLOT))
              TEFF%R(IFLOT) = TEFF%R(IFLOT) + DT * TW(IFLOT)
              TW0 = TW2 + (TW1 - TW2) * EXP(-A_ALG * TEFF%R(IFLOT))
              IF (TW(IFLOT).GT.TW0) THEN
                DISLODGE%I(IFLOT) = 1
              ENDIF
            ENDIF
          END DO
        END IF
!
        CALL DISP_ALGAE(NFLOT_MAX,NFLOT,MESH%DIM1,DT,AT,
     &                 U_X_AV_0%R,U_Y_AV_0%R,U_Z_AV_0%R,K_AV_0%R,
     &                 EPS_AV_0%R,H_FLU%R,U_X_AV%R,U_Y_AV%R,U_Z_AV%R,
     &                 U_X_0%R,U_Y_0%R,U_Z_0%R,V_X_0%R,V_Y_0%R,V_Z_0%R,
     &                 DX,DY,DZ,ELTFLO,U_X%R,U_Y%R,U_Z%R,V_X%R,V_Y%R,
     &                 V_Z%R,XFLOT,YFLOT,ZFLOT,LT,DALGAE,RALGAE,EALGAE,
     &                 TALGAE,YALGAE,REL_ALGAE)
!
! FIND THE ELEMENT AND SUBDOMAIN AFTER THE TRANSPORT (WITH A VERIFICATION
! IF SIZEBUF.LT.NFLOT_MAX)
!
        IF(NFLOT_MAX.GT.SIZEBUF)THEN
          CALL SCARACT(SVOID_DERIVE,SVOID_DERIVE,U,V,W,W,X,Y,ZSTAR,
     &                 ZSTAR,
     &                 XFLOT,YFLOT,ZFLOT,ZFLOT,DX,DY,DZ,DZ,Z,
     &                 SHPFLO,SHZFLO,SHZFLO,
     &                 SURDET,DT,IKLE,IFABOR,ELTFLO,ETAFLO,
     &                 FRE,BUFF_ELT_I,ISUB,IELM,IELMU,
     &                 NELEM,NELMAX,NOMB,NPOIN2,NDP,NRK,
     &                 NPLAN,1,MESH,NFLOT,NPOIN2,SENS,
     &                 BUFF_2D_D,BUFF_1D_D,BUFF_1D_D,FREBUF,SIZEBUF2_D,
     &                 APOST=.TRUE.,ASIGMA=SSIGMA,AALG=ALGAE)
        ELSE
          CALL SCARACT(SVOID_DERIVE,SVOID_DERIVE,U,V,W,W,X,Y,ZSTAR,
     &                 ZSTAR,
     &                 XFLOT,YFLOT,ZFLOT,ZFLOT,DX,DY,DZ,DZ,Z,
     &                 SHPFLO,SHZFLO,SHZFLO,
     &                 SURDET,DT,IKLE,IFABOR,ELTFLO,ETAFLO,
     &                 FRE,ELTBUF,ISUB,IELM,IELMU,
     &                 NELEM,NELMAX,NOMB,NPOIN2,NDP,NRK,
     &                 NPLAN,1,MESH,NFLOT,NPOIN2,SENS,
     &                 SHPBUF,SHZBUF,SHZBUF,FREBUF,SIZEBUF,
     &                 APOST=.TRUE.,ASIGMA=SSIGMA,AALG=ALGAE)
        ENDIF
      ELSE
        CALL SCARACT(SVOID_DERIVE,SVOID_DERIVE,U,V,W,W,X,Y,ZSTAR,ZSTAR,
     &               XFLOT,YFLOT,ZFLOT,ZFLOT,
     &               DX,DY,DZ,DZ,Z,SHPFLO,SHZFLO,SHZFLO,SURDET,DT,
     &               IKLE,IFABOR,ELTFLO,ETAFLO,
     &               FRE,ELTBUF,ISUB,IELM,IELMU,NELEM,NELMAX,
     &               NOMB,NPOIN2,NDP,NRK,
     &               NPLAN,1,MESH,NFLOT,NPOIN2,SENS,
     &               SHPBUF,SHZBUF,SHZBUF,FREBUF,SIZEBUF,
     &               APOST=.TRUE.,ASIGMA=SSIGMA,ASTOCHA=STOCHA,
!                    APOST=.TRUE. OTHERWISE ISUB IS NOT FILLED
     &               AVISC=VISC)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1.AND.NFLOT.GT.0) THEN
!
!       IN // XFLOT AND YFLOT MAY HAVE BEEN DESTROYED BY SCARACT
!       BECAUSE RE-USED FOR GENERATIONS OF LOST PARTICLES
!       THEY ARE REDONE HERE FOR PARTICLES WHICH ARE STILL IN THE
!       SUB-DOMAIN
!
        IF(IELM.EQ.11) THEN
          DO IFLOT=1,NFLOT
            IF(ISUB(IFLOT).EQ.IPID) THEN
              ELT=ELTFLO(IFLOT)
              IF(ELT.GT.0) THEN
                N1=IKLE(ELT,1)
                N2=IKLE(ELT,2)
                N3=IKLE(ELT,3)
                XFLOT(IFLOT)=SHPFLO(1,IFLOT)*X(N1)
     &                      +SHPFLO(2,IFLOT)*X(N2)
     &                      +SHPFLO(3,IFLOT)*X(N3)
                YFLOT(IFLOT)=SHPFLO(1,IFLOT)*Y(N1)
     &                      +SHPFLO(2,IFLOT)*Y(N2)
     &                      +SHPFLO(3,IFLOT)*Y(N3)
              ENDIF
            ENDIF
          ENDDO
        ELSEIF(IELM.EQ.41) THEN
          DO IFLOT=1,NFLOT
            IF(ISUB(IFLOT).EQ.IPID) THEN
              ELT=ELTFLO(IFLOT)
              IF(ELT.GT.0) THEN
                N1=IKLE(ELT,1)+NPOIN2*(ETAFLO(IFLOT)-1)
                N2=IKLE(ELT,2)+NPOIN2*(ETAFLO(IFLOT)-1)
                N3=IKLE(ELT,3)+NPOIN2*(ETAFLO(IFLOT)-1)
                N4=IKLE(ELT,1)+NPOIN2* ETAFLO(IFLOT)
                N5=IKLE(ELT,2)+NPOIN2* ETAFLO(IFLOT)
                N6=IKLE(ELT,3)+NPOIN2* ETAFLO(IFLOT)
                XFLOT(IFLOT)=SHPFLO(1,IFLOT)*X(N1)
     &                      +SHPFLO(2,IFLOT)*X(N2)
     &                      +SHPFLO(3,IFLOT)*X(N3)
                YFLOT(IFLOT)=SHPFLO(1,IFLOT)*Y(N1)
     &                      +SHPFLO(2,IFLOT)*Y(N2)
     &                      +SHPFLO(3,IFLOT)*Y(N3)
                ZFLOT(IFLOT)=(Z(N1)*SHPFLO(1,IFLOT)
     &                      +Z(N2)*SHPFLO(2,IFLOT)
     &                      +Z(N3)*SHPFLO(3,IFLOT))*(1.D0-SHZFLO(IFLOT))
     &                      +(Z(N4)*SHPFLO(1,IFLOT)
     &                      +Z(N5)*SHPFLO(2,IFLOT)
     &                      +Z(N6)*SHPFLO(3,IFLOT))*SHZFLO(IFLOT)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF
!
!     SEND THE ALGAE INFORMATION IF IT IS NECESSARY
!
      IF(NCSIZE.GT.1.AND.ALGAE) THEN
        IF(NFLOT_MAX.GT.SIZEBUF) THEN
          CALL SEND_INFO_ALG(ISUB,I_A_GL%I,CLSFLO,TEFF%R,DISLODGE%I,
     &                       BUFF_ELT_I,NFLOT,NFLOT_MAX,
     &                       U_X_AV%R,U_Y_AV%R,U_Z_AV%R,K_AV%R,EPS_AV%R,
     &                       H_FLU%R,U_X%R,U_Y%R,U_Z%R,V_X%R,V_Y%R,
     &                       V_Z%R,NWIN,MESH%DIM1,PSI)
        ELSE
          CALL SEND_INFO_ALG(ISUB,I_A_GL%I,CLSFLO,TEFF%R,DISLODGE%I,
     &                       ELTBUF,NFLOT,NFLOT_MAX,
     &                       U_X_AV%R,U_Y_AV%R,U_Z_AV%R,K_AV%R,EPS_AV%R,
     &                       H_FLU%R,U_X%R,U_Y%R,U_Z%R,V_X%R,V_Y%R,
     &                       V_Z%R,NWIN,MESH%DIM1,PSI)
        ENDIF
      ENDIF
!
!     SENDING THE PARTICLES THAT MIGRATED TO ANOTHER SUB-DOMAIN
!
      IF(NCSIZE.GT.1) THEN
        IF(ALGAE) THEN
          CALL SEND_PARTICLES(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &                 ETAFLO,ISUB,TAGFLO,CLSFLO,NDP,NFLOT,NFLOT_MAX,
     &                        MESH,NPLAN,DX=DX,DY=DY,DZ=DZ)
        ELSE
          CALL SEND_PARTICLES(XFLOT,YFLOT,ZFLOT,SHPFLO,SHZFLO,ELTFLO,
     &                 ETAFLO,ISUB,TAGFLO,CLSFLO,NDP,NFLOT,NFLOT_MAX,
     &                        MESH,NPLAN)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CASE OF LOST FLOATS (EXITED OR NOW REMOVED AFTER BEING SENT TO
!                          ANOTHER SUB-DOMAIN)
!
      IFLOT=1
      IF(NCSIZE.GT.1) THEN
!
!       IN // MODE
!
11      CONTINUE
!       LOST OR MIGRATED FLOATS
        IF(NFLOT.GT.0.AND.NCSIZE.GT.1) THEN
          IF(ELTFLO(IFLOT).LE.0.OR.ISUB(IFLOT).NE.IPID) THEN
!
!           REMOVE ALGAE INFORMATION FROM A SUB DOMAIN IF IT IS NECESSARY
!
            IF(ALGAE) THEN
              IF(NFLOT_MAX.GT.SIZEBUF) THEN
                CALL DEL_INFO_ALG(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                            MESH%TYPELM,I_A_GL%I,CLSFLO,
     &                            BUFF_ELT_I,V_X%R,V_Y%R,V_Z%R,
     &                            U_X%R,U_Y%R,U_Z%R,
     &                            U_X_AV%R,U_Y_AV%R,U_Z_AV%R,
     &                            K_AV%R,EPS_AV%R,H_FLU%R,NWIN,
     &                            MESH%DIM1,PSI)
              ELSE
                CALL DEL_INFO_ALG(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                            MESH%TYPELM,I_A_GL%I,CLSFLO,
     &                            ELTBUF,V_X%R,V_Y%R,V_Z%R,
     &                            U_X%R,U_Y%R,U_Z%R,
     &                            U_X_AV%R,U_Y_AV%R,U_Z_AV%R,
     &                            K_AV%R,EPS_AV%R,H_FLU%R,NWIN,
     &                            MESH%DIM1,PSI)
              ENDIF
              CALL DEL_PARTICLE(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,XFLOT,
     &                          YFLOT,ZFLOT,TAGFLO,CLSFLO,SHPFLO,
     &                          SHZFLO,ELTFLO,ETAFLO,MESH%TYPELM,
     &                          DX=DX,DY=DY,DZ=DZ,ISUB=ISUB,
     &                          TEFF=TEFF%R,DISLODGE=DISLODGE%I)
            ELSE
              CALL DEL_PARTICLE(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,XFLOT,
     &                          YFLOT,ZFLOT,TAGFLO,CLSFLO,SHPFLO,
     &                          SHZFLO,ELTFLO,ETAFLO,MESH%TYPELM,
     &                          ISUB=ISUB)
            ENDIF
!
!           THE SAME IFLOT IS NOW A NEW PARTICLE AND MUST BE CHECKED AGAIN!
            IF(IFLOT.LE.NFLOT) GO TO 11
          ENDIF
!
          IF(ALGAE)THEN
!           UPDATE DX_A,DY_A,DZ_A
            DX_A%R(IFLOT)=DX(IFLOT)
            DY_A%R(IFLOT)=DY(IFLOT)
            DZ_A%R(IFLOT)=DZ(IFLOT)
          ENDIF
!
          IFLOT=IFLOT+1
          IF(IFLOT.LE.NFLOT) GO TO 11
        ENDIF
!
      ELSE
!
!       IN SCALAR MODE
!
10      CONTINUE
!       LOST FLOATS ONLY
        IF(NFLOT.GT.0) THEN
          IF(ELTFLO(IFLOT).LE.0) THEN
!
!           REMOVE INFORMATION FROM A SUB DOMAIN IF NECESSARY
!
            IF(ALGAE) THEN
              IF(NFLOT_MAX.GT.SIZEBUF) THEN
                CALL DEL_INFO_ALG(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                            MESH%TYPELM,I_A_GL%I,CLSFLO,
     &                            BUFF_ELT_I,V_X%R,V_Y%R,V_Z%R,
     &                            U_X%R,U_Y%R,U_Z%R,
     &                            U_X_AV%R,U_Y_AV%R,U_Z_AV%R,
     &                            K_AV%R,EPS_AV%R,H_FLU%R,NWIN,
     &                            MESH%DIM1,PSI)
              ELSE
                CALL DEL_INFO_ALG(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                            MESH%TYPELM,I_A_GL%I,CLSFLO,
     &                            ELTBUF,V_X%R,V_Y%R,V_Z%R,
     &                            U_X%R,U_Y%R,U_Z%R,
     &                            U_X_AV%R,U_Y_AV%R,U_Z_AV%R,
     &                            K_AV%R,EPS_AV%R,H_FLU%R,NWIN,
     &                            MESH%DIM1,PSI)
              ENDIF
              CALL DEL_PARTICLE(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                          XFLOT,YFLOT,ZFLOT,TAGFLO,CLSFLO,SHPFLO,
     &                          SHZFLO,ELTFLO,ETAFLO,MESH%TYPELM,
     &                          DX=DX,DY=DY,DZ=DZ,TEFF=TEFF%R,
     &                          DISLODGE=DISLODGE%I)
            ELSE
              CALL DEL_PARTICLE(TAGFLO(IFLOT),NFLOT,NFLOT_MAX,
     &                          XFLOT,YFLOT,ZFLOT,TAGFLO,CLSFLO,SHPFLO,
     &                          SHZFLO,ELTFLO,ETAFLO,MESH%TYPELM)
            ENDIF
!
!           THE SAME IFLOT IS NOW A NEW PARTICLE AND MUST BE CHECKED AGAIN!
            IF(IFLOT.LE.NFLOT) GO TO 10
          ENDIF
!
          IF(ALGAE)THEN
!           UPDATE DX_A,DY_A,DZ_A
            DX_A%R(IFLOT)=DX(IFLOT)
            DY_A%R(IFLOT)=DY(IFLOT)
            DZ_A%R(IFLOT)=DZ(IFLOT)
          ENDIF
!
          IFLOT=IFLOT+1
          IF(IFLOT.LE.NFLOT) GO TO 10
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DERIVE
