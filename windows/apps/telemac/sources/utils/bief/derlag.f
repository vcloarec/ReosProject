!                   *****************
                    SUBROUTINE DERLAG
!                   *****************
!
     &(U,V,DT,X,Y,LT,IELM,IELMU,NDP,NPOIN,NELEM,NELMAX,XLAG,YLAG,DX,DY,
     & NSP,SHPLAG,DEBLAG,FINLAG,ELTLAG,NLAG,RESUX,RESUY,ISPDONE,MESH)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    - SETS THE BARYCENTRIC COORDINATES IN THE MESH,
!+                  AT THE START OF COMPUTATION FOR EACH DRIFTING FLOAT.
!+                  HERE WE COMPUTE THE LAGRANGIAN DRIFT.
!+
!+            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!+                 (SUBSEQUENT TIMESTEPS).
!
!warning  Will not work in parallel (this would require calling scaract
!+        instead of char11, and adaptation of scaract)
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
!+   Adapted to call SCARACT instead of CHAR11. However further
!+   modifications are required for parallelism.
!
!history  J-M HERVOUET (LNHE)
!+        28/10/2014
!+        V7P0
!+   Stopping drifts that exit the domain has been added
!+   + error message : parallelism does not work here.
!+   (possible but a long implementation copied on particles)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        14/08/2015
!+        V7P1
!+   Hardcoded argument NRK added for SCARACT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBLAG         |-->| TIME STEP FOR STARTING THE COMPUTATION
!| DT             |-->| TIME STEP
!| DX             |<->| WORK ARRAY
!| DY             |<->| WORK ARRAY
!| ELTLAG         |<->| ELEMENT NUMBERS OF FLOATS
!| FINLAG         |-->| TIME STEP FOR ENDING THE COMPUTATION
!| IELM           |-->| TYPE OF ELEMENT IN THE MESH
!| IELMU          |-->| TYPE OF ELEMENT FOR THE VELOCITIES
!| LT             |-->| TIME STEP NUMBER.
!| MASKEL         |-->| MASKING OF ELEMENTS.
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!| MESH           |-->| MESH STRUCTURE
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS.
!| NLAG           |-->| NOMBER OF FLOATS.
!| NPOIN          |-->| NUMBER OF POINTS
!| NSP            |-->| NUMBER OF SUB-STEPS IN THE RUNGE-KUTTA METHOD
!| RESUX          |<--| ARRAY WITH SUCCESSIVE ABSCISSAE OF FLOATS
!| RESUY          |<--| ARRAY WITH SUCCESSIVE ORDINATES OF FLOATS
!| SHPLAG         |<->| BARYCENTRIC COORDINATES OF FLOATS
!|                |   | IN THEIR ELEMENTS.
!| T8             |-->| BLOCK OF WORK BIEF_OBJ STRUCTURES.
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XLAG           |<->| INSTANTANEOUS X POSITIONS OF FLOATS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YLAG           |<->| INSTANTANEOUS Y POSITIONS OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DERLAG => DERLAG
      USE STREAMLINE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,NDP,NELEM,NLAG
      INTEGER         , INTENT(IN)    :: NELMAX,IELMU
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),DT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XLAG(NPOIN,NLAG)
      DOUBLE PRECISION, INTENT(INOUT) :: YLAG(NPOIN,NLAG)
      INTEGER         , INTENT(INOUT) :: DEBLAG(NLAG),FINLAG(NLAG)
      INTEGER         , INTENT(INOUT) :: ELTLAG(NPOIN,NLAG)
      INTEGER         , INTENT(INOUT) :: ISPDONE(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPOIN),DY(NPOIN)
      INTEGER         , INTENT(INOUT) :: NSP(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: RESUX(NPOIN),RESUY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPLAG(NDP,NPOIN,NLAG)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ILAG,JLAG,LTT,IPOIN,ETA(1),SENS,NPLAN,FRE(1),FREBUF(1),NRK
      TYPE(BIEF_OBJ) :: SVOID
!
      DOUBLE PRECISION ZSTAR(1),ZCONV(1),SHZ(1),Z(1),SHF(1)
      DOUBLE PRECISION SHPBUF(3,1),SHZBUF(1)
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) THEN
        WRITE(LU,*) 'LAGRANGIAN DRIFTS NOT PROGRAMMED IN PARALLEL'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     HARDCODED NUMBER OF SUB-STEPS FOR COMPUTING THE PATH-LINES
!
      NRK=3
!
!     2D HERE
!
      NPLAN=1
!
!     FORWARD CHARACTERISTICS
!
      SENS=1
!
      DO ILAG=1,NLAG
!
        IF(LT.EQ.DEBLAG(ILAG)) THEN
!
!-----------------------------------------------------------------------
!
!   - SETS THE BARYCENTRIC COORDINATES IN THE MESH , AT THE START
!     OF COMPUTATION FOR EACH FLOAT
!
!-----------------------------------------------------------------------
!
          IF(IELM.EQ.11) THEN
!
!  P1 TRIANGLES
!  ============
!
!      FILLS THE SHP AND ELT (OPTIMISED)
!
            CALL GTSH11(SHPLAG(1,1,ILAG),ELTLAG(1,ILAG),MESH%IKLE%I,
     &                  MESH%ELTCAR%I,
     &                  NPOIN,NELEM,NELMAX,1,.FALSE.,.FALSE.)
!                                          1=NSEG, WRONG VALUE, NOT USED
!
          ELSE
!
            WRITE(LU,*) IELM,': ELEMENT NOT IMPLEMENTED IN DERLAG'
            CALL PLANTE(1)
            STOP
!
          ENDIF
!
          CALL OV('X=Y     ', X=XLAG(1,ILAG), Y=X, DIM1=NPOIN)
          CALL OV('X=Y     ', X=YLAG(1,ILAG), Y=Y, DIM1=NPOIN)
!
        ELSEIF(LT.GT.DEBLAG(ILAG).AND.LT.LE.FINLAG(ILAG)) THEN
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!     (SUBSEQUENT TIMESTEPS)
!
!-----------------------------------------------------------------------
!
          CALL SCARACT(SVOID,SVOID,U,V,V,V,X,Y,
     &                 ZSTAR,ZSTAR,XLAG(1,ILAG),YLAG(1,ILAG),
     &                 ZCONV,ZCONV,
     &                 DX,DY,DY,DY,Z,SHPLAG(1,1,ILAG),SHZ,SHF,
     &                 MESH%SURDET%R,
     &                 DT,MESH%IKLE%I,MESH%IFABOR%I,ELTLAG(1,ILAG),
     &                 ETA,FRE,NSP,ISPDONE,IELM,IELMU,NELEM,NELMAX,
     &                 0,NPOIN,NDP,NRK,NPLAN,1,
     &                 MESH,NPOIN,BIEF_NBPTS(IELMU,MESH),SENS,
!                      PROVISIONAL, THIS WILL NOT WORK IN PARALLEL
     &                 SHPBUF,SHZBUF,SHZBUF,FREBUF,1,
     &                 .TRUE.)
!                       POST : DATA KEPT FOR A POSTERIORI INTERPOLATION
!                              HERE FOR NEXT STEP
!
!-----------------------------------------------------------------------
!
!       STOPPING THE FLOATS THAT LEFT THE DOMAIN
!
!-----------------------------------------------------------------------
!
          DO IPOIN=1,NPOIN
            IF(ELTLAG(IPOIN,ILAG).LT.0) THEN
!             THIS POINT WILL NO LONGER BE TREATED
              ELTLAG(IPOIN,ILAG) = 0
!             NEXT TWO LINES IN PREVIOUS VERSIONS
!             BUT WHY NOT KEEPING THE LAST POSITION AT THE EXIT?
!             XLAG(IPOIN,ILAG) = X(IPOIN)
!             YLAG(IPOIN,ILAG) = Y(IPOIN)
            ENDIF
          ENDDO
!
!-----------------------------------------------------------------------
!
        ENDIF
!
      ENDDO ! ILAG
!
!-----------------------------------------------------------------------
!
!   - STORAGE FOR RESULTS OUTPUT OF THE LAST COMPUTED FLOAT
!
!-----------------------------------------------------------------------
!
      CALL OV('X=C     ', X=RESUX, C=0.D0, DIM1=NPOIN)
      CALL OV('X=C     ', X=RESUY, C=0.D0, DIM1=NPOIN)
      LTT=0
      JLAG=1
      DO ILAG=1,NLAG
        IF(FINLAG(ILAG).GT.LTT.AND.FINLAG(ILAG).LE.LT) THEN
          LTT=FINLAG(ILAG)
          JLAG=ILAG
        ENDIF
      ENDDO
      IF(LTT.NE.0) THEN
        CALL OV('X=Y-Z   ', X=RESUX, Y=XLAG(1,JLAG), Z=X, DIM1=NPOIN)
        CALL OV('X=Y-Z   ', X=RESUY, Y=YLAG(1,JLAG), Z=Y, DIM1=NPOIN)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

