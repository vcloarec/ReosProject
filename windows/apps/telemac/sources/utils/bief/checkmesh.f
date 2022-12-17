!                   ********************
                    SUBROUTINE CHECKMESH
!                   ********************
!
     &(MESH,NPOIN)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    Checks the mesh.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/10/2015
!+        V7P1
!+   First version. Checking the coordinates and the number of
!+   neighbours of points.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| MESH STRUCTURE.
!| NPOIN          |-->| NUMBER OF POINTS (NPOIN2 IN 3D)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CHECKMESH => CHECKMESH
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: NPOIN
      TYPE(BIEF_MESH)  , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IDISTMIN,JDISTMIN,IELEM,I1,I2,I3,OVER
      DOUBLE PRECISION, POINTER :: X(:),Y(:)
      DOUBLE PRECISION DIST2,DIST2MIN
      INTRINSIC SQRT,NINT
      LOGICAL STOP_ERROR
!
!-----------------------------------------------------------------------
!
      X=>MESH%X%R
      Y=>MESH%Y%R
      STOP_ERROR=.FALSE.
!
!-----------------------------------------------------------------------
!     CHECKING THE COORDINATES
!-----------------------------------------------------------------------
!
      DIST2MIN=1.D20
      DO I=1,NPOIN-1
        DO J=I+1,NPOIN
          DIST2=(X(I)-X(J))**2+(Y(I)-Y(J))**2
          IF(DIST2.LT.DIST2MIN) THEN
            DIST2MIN=DIST2
            IDISTMIN=I
            JDISTMIN=J
          ENDIF
        ENDDO
      ENDDO
      IF(NCSIZE.GT.1) THEN
        IF(P_MIN(DIST2MIN).EQ.DIST2MIN) THEN
          IDISTMIN=MESH%KNOLG%I(IDISTMIN)
          JDISTMIN=MESH%KNOLG%I(JDISTMIN)
        ELSE
          IDISTMIN=0
          JDISTMIN=0
          DIST2MIN=0.D0
        ENDIF
        DIST2MIN=P_MAX(DIST2MIN)
        IDISTMIN=P_MAX(IDISTMIN)
        JDISTMIN=P_MAX(JDISTMIN)
      ENDIF
      WRITE(LU,*)
      WRITE(LU,*) 'CHECKING THE MESH'
      WRITE(LU,*)
      WRITE(LU,*) 'SMALLEST DISTANCE BETWEEN TWO POINTS:',
     &            SQRT(DIST2MIN)
      WRITE(LU,*) 'BETWEEN POINTS: ',IDISTMIN,' AND ',JDISTMIN
      IF(DIST2MIN.LT.1.D-3) THEN
        WRITE(LU,*) 'VALUE TOO SMALL'
        STOP_ERROR=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!     CHECKING THE NUMBER OF NEIGHBOURS
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        MESH%T%R(I)=0.D0
      ENDDO
!
      DO IELEM=1,MESH%NELEM
        I1=MESH%IKLE%I(IELEM)
        MESH%T%R(I1)=MESH%T%R(I1)+2.D0
        I2=MESH%IKLE%I(IELEM+MESH%NELMAX)
        MESH%T%R(I2)=MESH%T%R(I2)+2.D0
        I3=MESH%IKLE%I(IELEM+2*MESH%NELMAX)
        MESH%T%R(I3)=MESH%T%R(I3)+2.D0
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(MESH%T,2,MESH)
!
      OVER=0
!
      DO I=1,NPOIN
        IF(MESH%T%R(I).LT.3.D0) THEN
          IF(NCSIZE.GT.1) THEN
            J=MESH%KNOLG%I(I)
          ELSE
            J=I
          ENDIF
          WRITE(LU,*) 'POINT ',J,' HAS ONLY ',
     &                NINT(MESH%T%R(I)),' NEIGHBOURS'
          IF(MESH%T%R(I).LT.1.D0) THEN
            STOP_ERROR=.TRUE.
          ELSE
            OVER=OVER+1
          ENDIF
        ENDIF
      ENDDO
!
!     THIS SUM IS CORRECT BECAUSE A POINt WITH 2 NEIGHBOURS CANNOT
!     BELONG TO 2 SUB-DOMAINS
      IF(NCSIZE.GT.1) OVER=P_SUM(OVER)
!
      IF(OVER.GT.1) THEN
        WRITE(LU,*) OVER,' OVERCONSTRAINED TRIANGLES'
        WRITE(LU,*) 'POSSIBLE BUT NOT RECOMMENDED'
      ENDIF
!
!-----------------------------------------------------------------------
!     STOPPING IF FATAL ERROR
!-----------------------------------------------------------------------
!
      IF(STOP_ERROR) THEN
        WRITE(LU,*) 'ISSUE DETECTED WHILE CHECING THE MESH'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

