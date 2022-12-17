!                   *****************
                    SUBROUTINE COEFRO
!                   *****************
!
     &(CF,H,U,V,KARMAN,KFROT,CHESTR,GRAV,MESH,T1,YAFV)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FRICTION COEFFICIENT CF.
!
!history  J-M HERVOUET (LNHE)
!+        27/07/2009
!+        V6P0
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
!history  C. VILLARET (LNHE)
!+        21/02/2012
!+        V6P2
!+   Nikuradse formula slightly changed (30/e instead of 11).
!+   Optimised by JMH
!+
!history  RIADH ATA (LNHE)
!+        21/09/2018
!+        V8P0
!+  Generalization for finite volumes
!+
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<--| ADIMENSIONAL FRICTION COEFFICIENT
!| CHESTR         |-->| FRICTION COEFFICIENTS
!| YAFV           |-->| IF YES FINITE VOLUMES
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KFROT          |-->| FRICTION LAW ON BOTTOM
!| MESH           |-->| MESH STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D,ONLY:CHESTR0
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(INOUT):: KFROT
      DOUBLE PRECISION,  INTENT(IN)   :: GRAV,KARMAN
      TYPE(BIEF_OBJ),    INTENT(INOUT):: CF,T1,CHESTR
      TYPE(BIEF_OBJ),    INTENT(IN)   :: H,U,V
      TYPE(BIEF_MESH),   INTENT(INOUT):: MESH
      LOGICAL,           INTENT(IN)   :: YAFV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN,N,IELMC,IELMH
!
      DOUBLE PRECISION TIERS,HC,UNORM,AUX,INLOG,TRENTESURE,CC,UNSURSIX
      DOUBLE PRECISION, POINTER :: HH(:)
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      IELMC = CF%ELM
      IELMH = H%ELM
!
!  DEPTH WITH THE SAME DISCRETISATION AS CF
!  IN CASES WHERE IT IS NEEDED.
!
      IF(KFROT.NE.0) THEN
!
        IF(IELMC.EQ.IELMH) THEN
          HH=>H%R
        ELSE
          CALL OS( 'X=Y     ' , X=T1 , Y=H )
          CALL CHGDIS( T1 , IELMH , IELMC , MESH )
          HH=T1%R
        ENDIF
!
      ENDIF
!
      NPOIN = CF%DIM1
!
!-----------------------------------------------------------------------
!
!     FOR FINITE ELEMENTS AND BOUSSINESQ
      TIERS  = 1.D0/3.D0
!
      IF(YAFV)GOTO 300 ! FINITE VOLUMES
!
!
!     FRICTION COEFFICIENT
!
!     LAWS OF FRICTION:
!
!     KFROT = 0:  NO FRICTION
!     KFROT = 1:  HAALAND
!     KFROT = 2:  CHEZY
!     KFROT = 3:  STRICKLER
!     KFROT = 4:  MANNING
!     KFROT = 5:  NIKURADSE
!
!     *******************
      IF(KFROT.EQ.0) THEN
!     *******************
!
        DO N=1,NPOIN
          CF%R(N) = 0.D0
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.1) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          UNORM = MAX(SQRT(U%R(N)**2+V%R(N)**2),1.D-6)
!                       1.D-6: LAMINAR VISCOSITY OF THE WATER
          INLOG =(6.9D0*1.D-6/4.D0/HC/UNORM)**3+
     &                                  (CHESTR%R(N)/14.8D0/HC)**3.33
          INLOG = MIN(1.D0-1.D-6,INLOG)
          AUX   = -0.6D0*LOG(INLOG)/LOG(10.D0)
          CF%R(N) = 0.25D0 / AUX**2
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.2) THEN
!     ***********************
!
        DO N=1,NPOIN
          CF%R(N) = 2 * GRAV / CHESTR%R(N)**2
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.3) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CF%R(N) = 2 * GRAV / CHESTR%R(N)**2 / HC**TIERS
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.4) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CF%R(N) = 2 * CHESTR%R(N)**2 * GRAV / HC**TIERS
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.5) THEN
!     ***********************
!
        TRENTESURE=30.D0/EXP(1.D0)
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CF%R(N) = 2.D0 / (LOG( TRENTESURE*HC/CHESTR%R(N))/KARMAN )**2
        ENDDO
!
!     ****
      ELSE
!     ****
!
        WRITE(LU,101) KFROT
101     FORMAT(1X,'COEFRO: UNKNOWN LAW OF BOTTOM FRICTION: ',1I6)
        CALL PLANTE(1)
        STOP
!
!     *****
      ENDIF
!     *****
      IF(.NOT.YAFV)RETURN
!
!
300   CONTINUE
!
!     FOR FINITE VOLUMES: NOT THE SAME IDEA AS FINITE ELEMENTS
!     SINCE WE CONVERT ALL THE LAWS TO STRICKLER
!
!     FRICTION COEFFICIENT
!
!     LAWS OF FRICTION:
!
!     KFROT = 0:  NO FRICTION
!     KFROT = 1:  HAALAND- NOT IMPLEMENTED YET
!     KFROT = 2:  CHEZY
!     KFROT = 3:  STRICKLER
!     KFROT = 4:  MANNING
!     KFROT = 5:  NIKURADSE
      UNSURSIX = 1.D0/6.D0
!     *******************
      IF(KFROT.EQ.0) THEN
!     *******************
!
        DO N=1,NPOIN
          CF%R(N)    = 0.D0
          CHESTR%R(N)= 0.D0
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.2) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CHESTR%R(N)=CHESTR0%R(N)/HC**UNSURSIX
!         TO RETRIEVE THE SAME CF THEN FINITE ELEMENTS
          CF%R(N) = 2 * GRAV / CHESTR0%R(N)**2
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.3) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
!         WE DO NOTHING FOR CHESTR
          CF%R(N) = 2 * GRAV / CHESTR0%R(N)**2 / HC**TIERS
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.4) THEN
!     ***********************
!
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
          CHESTR%R(N)=1.D0/MAX(CHESTR0%R(N),1.D-4)
          CF%R(N) = 2 * CHESTR0%R(N)**2 * GRAV / HC**TIERS
        ENDDO
!
!     ***********************
      ELSEIF(KFROT.EQ.5) THEN
!     ***********************
!
!       based on Ramette law ==> not consistant with finite elements
        TRENTESURE=30.D0/EXP(1.D0)
        CC=8.2D0*SQRT(GRAV)
        DO N=1,NPOIN
          HC = MAX(HH(N),1.D-4)
!         THIS IS RAMETTE LAW
          CHESTR%R(N)= CC/MAX(CHESTR0%R(N),1.D-10)**UNSURSIX
!
          CF%R(N) = 2.D0 / (LOG( TRENTESURE*HC/CHESTR0%R(N))/KARMAN )**2
        ENDDO
!
!     ****
      ELSE
!     ****
!
        WRITE(LU,601) KFROT
601     FORMAT(1X,'COEFRO: UNKNOWN LAW OF BOTTOM FRICTION: ',1I6)
        CALL PLANTE(1)
        STOP
!
!     *****
      ENDIF
!     *****
!     RETURN TO STRICKLER LAW
      IF(KFROT.NE.0)KFROT=3
!
!-----------------------------------------------------------------------
!
      RETURN
      END
