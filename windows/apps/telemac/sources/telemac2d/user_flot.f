!                   ********************
                    SUBROUTINE USER_FLOT
!                   ********************
!
     &(XFLOT,YFLOT,NFLOT,NFLOT_MAX,X,Y,IKLE,NELEM,NELMAX,NPOIN,
     & TAGFLO,CLSFLO,SHPFLO,ELTFLO,MESH,LT,NIT,AT)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    releasing and removing particles in the mesh.
!
!history  J-M JANIN (LNH)
!+        17/08/1994
!+        V5P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| CLSFLO         |<->| CLASS FOR EACH FLOAT
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| IKLE           |-->| CONNECTIVITY TABLE
!| LT             |-->| CURRENT TIME STEP
!| MESH           |<->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENT
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NIT            |-->| NUMBER OF TIME STEPS
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR
!|                |   | ELEMENTS.
!| TAGFLO         |<->| TAG FOR EACH FLOAT
!| X,Y            |-->| COORDINATES OF POINTS IN THE MESH
!| XFLOT,YFLOT    |<->| POSITIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE STREAMLINE, ONLY : ADD_PARTICLE,DEL_PARTICLE
      USE ALGAE_TRANSP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NIT,NFLOT_MAX,LT
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT)          :: NFLOT
      INTEGER, INTENT(INOUT)          :: TAGFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: CLSFLO(NFLOT_MAX)
      INTEGER, INTENT(INOUT)          :: ELTFLO(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),AT
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NFLOT_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(3,NFLOT_MAX)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : AT ITERATION 1 AND EVERY 10 ITERATIONS AFTER 600
!               A PARTICLE IS RELEASED WITH COORDINATES
!               X=-220.
!               Y=400.D0+LT/3.D0
!               AND TAG NUMBER LT (LT IS THE TIME STEP NUMBER)
!
!     IF(LT.LE.600.AND.(10*(LT/10).EQ.LT.OR.LT.EQ.1)) THEN
!       CALL ADD_PARTICLE(-220.D0,400.D0+LT/3.D0,0.D0,LT,1,NFLOT,
!    &                    NFLOT_MAX,XFLOT,YFLOT,YFLOT,TAGFLO,CLSFLO,
!    &                    SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
!    &                    0.D0,0.D0,0.D0,0.D0,0,0)
!     ENDIF
!
!     EXAMPLE : PARTICLE WITH TAG 20 REMOVED AT ITERATION 600
!
!     IF(LT.EQ.600) THEN
!        CALL DEL_PARTICLE(20,NFLOT,NFLOT_MAX,
!    &                     XFLOT,YFLOT,YFLOT,TAGFLO,CLSFLO,SHPFLO,SHPFLO,
!    &                     ELTFLO,ELTFLO,MESH%TYPELM)
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : FOR ALGAE PARTICLE TRANSPORT
!       ( RELEASE START FOR ALGAE IS NOW DEFINED IN THE CAS FILE )
!
!       DO I=1,NFLOT_MAX
!         CALL ADD_PARTICLE(0.175D0,0.45D0,0.D0,I,1,NFLOT,
!      &                  NFLOT_MAX,XFLOT,YFLOT,YFLOT,TAGFLO,CLSFLO,
!      &                  SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
!      &                  0.D0,0.D0,0.D0,0.D0,0,0)
!       END DO
!
!     EXAMPLE : AT ITERATION 1 AND EVERY 10 ITERATIONS AFTER 600
!               A PARTICLE IS RELEASED WITH COORDINATES
!               X=-220.
!               Y=400.D0+LT/3.D0
!               AND TAG NUMBER LT (LT IS THE TIME STEP NUMBER)
!
!     IF(LT.LE.600.AND.(10*(LT/10).EQ.LT.OR.LT.EQ.1)) THEN
!       CALL ADD_PARTICLE(-220.D0,400.D0+LT/3.D0,0.D0,1,LT,NFLOT,
!    &                    NFLOT_MAX,XFLOT,YFLOT,YFLOT,TAGFLO,CLSFLO,
!    &                    SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
!    &                    0.D0,0.D0,0.D0,0.D0,0,0)
!     ENDIF
!
!     EXAMPLE : PARTICLE WITH TAG 20 REMOVED AT ITERATION 600
!
!     IF(LT.EQ.600) THEN
!        CALL DEL_PARTICLE(20,NFLOT,NFLOT_MAX,
!    &                     XFLOT,YFLOT,YFLOT,TAGFLO,CLSFLO,SHPFLO,SHPFLO,
!    &                     ELTFLO,ELTFLO,MESH%TYPELM)
!     ENDIF
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : FOR ALGAE PARTICLE TRANSPORT
!       ( RELEASE START FOR ALGAE IS NOW DEFINED IN THE CAS FILE )
!
!       DO I=1,NFLOT_MAX
!         CALL ADD_PARTICLE(0.175D0,0.45D0,0.D0,I,1,NFLOT,
!      &                  NFLOT_MAX,XFLOT,YFLOT,YFLOT,TAGFLO,CLSFLO,
!      &                  SHPFLO,SHPFLO,ELTFLO,ELTFLO,MESH,1,
!      &                  0.D0,0.D0,0.D0,0.D0,0,0)
!       END DO
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
