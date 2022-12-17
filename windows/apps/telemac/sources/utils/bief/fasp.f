!                   ***************
                    SUBROUTINE FASP
!                   ***************
!
     &(X,Y,ZF,NPOIN,XRELV,YRELV,ZRELV,NP,NBOR,KP1BOR,NPTFR,DM)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INTERPOLATES THE BOTTOM ELEVATIONS FROM A SET OF
!+                POINTS ON THE MESH NODES.
!
!history  J-M HERVOUET (LNHE)
!+        20/03/08
!+        V5P9
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DM             |-->| MINIMUM DISTANCE TO BOUNDARY TO ACCEPT A POINT
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NP             |-->| NUMBER OF BATHYMETRY POINTS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| X,Y            |-->| MESH COORDINATES
!| XRELV          |-->| ABCISSAE OF BATHYMETRY POINTS
!| YRELV          |-->| ORDINATES OF BATHYMETRY POINTS
!| ZF             |<--| INTERPOLATED BATHYMETRY
!| ZRELV          |-->| ELEVATIONS OF BATHYMETRY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FASP => FASP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NP,NPTFR
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN),Y(NPOIN),DM
      DOUBLE PRECISION, INTENT(IN)  :: XRELV(NP),YRELV(NP),ZRELV(NP)
      DOUBLE PRECISION, INTENT(OUT) :: ZF(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,INUM,I
!
      DOUBLE PRECISION DIST1,DIST2,DIST3,DIST4
      DOUBLE PRECISION ZCADR1,ZCADR2,ZCADR3,ZCADR4
      DOUBLE PRECISION DIFX,DIFY,DIST,X1,Y1,X2,Y2,X3,Y3,X4,Y4
      DOUBLE PRECISION ZNUM,ZDEN
!
      LOGICAL OK1,OK2,OK3,OK4
!
!-----------------------------------------------------------------------
!
!  LOOP ON THE MESH NODES:
!
      DO I = 1 , NPOIN
!
!     INTERPOLATES THE BOTTOM FROM 4 QUADRANTS
!
! ---->  INITIALISES:
!
      DIST1=1.D12
      DIST2=1.D12
      DIST3=1.D12
      DIST4=1.D12
!
      OK1 = .FALSE.
      OK2 = .FALSE.
      OK3 = .FALSE.
      OK4 = .FALSE.
!
      ZCADR1=0.D0
      ZCADR2=0.D0
      ZCADR3=0.D0
      ZCADR4=0.D0
!
! --------->  LOOP ON THE SET OF POINTS (THERE ARE NP):
      DO N=1,NP
        DIFX = XRELV(N)-X(I)
        DIFY = YRELV(N)-Y(I)
        DIST = DIFX*DIFX + DIFY*DIFY
!
        IF ( DIST.LT.1.D-6 ) DIST=1.D-6
! ->QUADRANT 1 :
          IF( DIFX.LE.0.D0.AND.DIFY.LE.0.D0) THEN
            IF(DIST.LE.DIST1)THEN
              X1=XRELV(N)
              Y1=YRELV(N)
              DIST1=DIST
              ZCADR1=ZRELV(N)
              OK1 = .TRUE.
            ENDIF
! ->QUADRANT 2 :
        ELSE IF( DIFX.GE.0.D0.AND.DIFY.LE.0.D0) THEN
          IF(DIST.LE.DIST2)THEN
            X2=XRELV(N)
            Y2=YRELV(N)
            DIST2=DIST
            ZCADR2=ZRELV(N)
            OK2 = .TRUE.
          ENDIF
! ->QUADRANT 3 :
        ELSE IF( DIFX.GE.0.D0.AND.DIFY.GE.0.D0) THEN
          IF(DIST.LE.DIST3)THEN
            X3=XRELV(N)
            Y3=YRELV(N)
            DIST3=DIST
            ZCADR3=ZRELV(N)
            OK3 = .TRUE.
          ENDIF
! ->QUADRANT 4 :
        ELSE IF( DIFX.LE.0.D0.AND.DIFY.GE.0.D0) THEN
          IF(DIST.LE.DIST4)THEN
            X4=XRELV(N)
            Y4=YRELV(N)
            DIST4=DIST
            ZCADR4=ZRELV(N)
            OK4 = .TRUE.
          ENDIF
        ENDIF
      ENDDO ! N
!
! --------->  END OF LOOP ON THE SET OF POINTS
!
      IF(OK1) CALL CROSFR(X(I),Y(I),X1,Y1,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK1)
      IF(OK2) CALL CROSFR(X(I),Y(I),X2,Y2,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK2)
      IF(OK3) CALL CROSFR(X(I),Y(I),X3,Y3,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK3)
      IF(OK4) CALL CROSFR(X(I),Y(I),X4,Y4,X,Y,NPOIN,NBOR,KP1BOR,
     &                    NPTFR,DM,OK4)
!
      ZNUM = 0.D0
      ZDEN = 0.D0
      INUM = 0
      IF(OK1) THEN
        ZNUM = ZNUM + ZCADR1/DIST1
        ZDEN = ZDEN + 1.D0/DIST1
        INUM = INUM + 1
      ENDIF
      IF(OK2) THEN
        ZNUM = ZNUM + ZCADR2/DIST2
        ZDEN = ZDEN + 1.D0/DIST2
        INUM = INUM + 1
      ENDIF
      IF(OK3) THEN
        ZNUM = ZNUM + ZCADR3/DIST3
        ZDEN = ZDEN + 1.D0/DIST3
        INUM = INUM + 1
      ENDIF
      IF(OK4) THEN
        ZNUM = ZNUM + ZCADR4/DIST4
        ZDEN = ZDEN + 1.D0/DIST4
        INUM = INUM + 1
      ENDIF
!
      IF(INUM.NE.0) THEN
!       ZF : WATER DEPTH AT THE POINT
        ZF(I)=ZNUM/ZDEN
      ELSE
        ZF(I) = -1.D6
      ENDIF
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
!
      RETURN
      END
