!                   *****************
                    SUBROUTINE PROXIM
!                   *****************
!
     &(IP,XP,YP,X,Y,NP,NPOIN,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    IDENTIFIES THE POINTS OF THE MESH CLOSEST TO A SET
!+                OF GIVEN POINTS.
!
!history  J-M HERVOUET (LNHE)
!+        03/07/2009
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
!history  J-M HERVOUET (LNHE)
!+        16/11/2012
!+        V6P3
!+   Write statements added.
!
!history  J-M HERVOUET (LNHE)
!+        24/12/2013
!+        V7P0
!+   In parallel the distance between given points and points in the
!+   mesh must be now less than 1.D-8 instead of 1.D-4 (to avoid
!+   problems with subroutine ecrspe in Tomawac)
!
!history  J-M HERVOUET (LNHE)
!+        14/11/2014
!+        V7P0
!+   Checking that a point belongs to at least one sub-domain in
!+   parallel mode.
!
!history  J-M HERVOUET (LNHE)
!+        16/06/2016
!+        V7P2
!+   Reverting to the original algorithm in sequential mode, for parallel
!+   also: choosing the nearest point in the mesh. In case of parallelism
!+   the nearest of all points in all sub-domains is chosen.
!
!history  J-M HERVOUET (LNHE)
!+        29/10/2017
!+        V7P3
!+   Once a processor has found a source point, it must be communicated
!+   to others in case this point is on an interface. This is done by
!+   simply comparing the coordinates.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |-->| CONNECTIVITY TABLE.
!| IP             |<--| ADDRESSES OF NEAREST POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NP             |-->| NUMBER OF POINTS IN THE SET
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XP             |-->| ABSCISSAE OF POINTS IN THE SET
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YP             |-->| ORDINATES OF POINTS IN THE SET
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_PARALLEL
      USE BIEF, EX_PROXIM => PROXIM
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NP,NPOIN,NELEM,NELMAX
      INTEGER, INTENT(INOUT) :: IP(NP)
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,3)
!
      DOUBLE PRECISION, INTENT(IN) :: XP(NP),YP(NP),X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IELEM
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,A31,A12,A23,DIST2,D2
      DOUBLE PRECISION XX,YY
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      IF(NP.GT.0) THEN
      DO K=1,NP
        IP(K)=0
        DIST2=1.D10
        XX=-1.D10
        YY=-1.D10
!
!       LOOP ON THE TRIANGLES TO FIND THE NEAREST POINT IN THE (SUB-)DOMAIN:
!
        DO IELEM=1,NELEM
          X1=X(IKLE(IELEM,1))
          X2=X(IKLE(IELEM,2))
          X3=X(IKLE(IELEM,3))
          Y1=Y(IKLE(IELEM,1))
          Y2=Y(IKLE(IELEM,2))
          Y3=Y(IKLE(IELEM,3))
          A31=XP(K)*Y3-YP(K)*X3+X3*Y1-X1*Y3+X1*YP(K)-XP(K)*Y1
          A12=XP(K)*Y1-YP(K)*X1+X1*Y2-X2*Y1+X2*YP(K)-XP(K)*Y2
          A23=XP(K)*Y2-YP(K)*X2+X2*Y3-X3*Y2+X3*YP(K)-XP(K)*Y3
          IF(A31.GT.-1.D-6.AND.A12.GT.-1.D-6.AND.A23.GT.-1.D-6) THEN
!           TAKES THE NEAREST NODE
            DO I=1,3
              D2=(XP(K)-X(IKLE(IELEM,I)))**2+(YP(K)-Y(IKLE(IELEM,I)))**2
              IF(D2.LT.DIST2) THEN
                IP(K)=IKLE(IELEM,I)
                XX=X(IKLE(IELEM,I))
                YY=Y(IKLE(IELEM,I))
                DIST2=D2
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
!       CHECKING THAT THE POINT IS IN THE GLOBAL DOMAIN
!       IF YES PRINTING THE COORDINATES OF THE NEAREST POINT FOUND
!
        I=IP(K)
        IF(NCSIZE.GT.1) I=P_MAX(I)
        IF(I.EQ.0) THEN
!         THE POINT IS NOT IN THE DOMAIN
          WRITE(LU,*)
          WRITE(LU,*) 'SPECTRUM OR SOURCE POINT ',K,' OUTSIDE DOMAIN'
          CALL PLANTE(1)
          STOP
        ELSE
!         THE POINT IS IN THE DOMAIN. IN PARALLEL SEVERAL NEAREST POINTS
!         MAY HAVE BEEN FOUND, FINDING THE REAL NEAREST ONE
          X1=XX
          Y1=YY
          IF(NCSIZE.GT.1) THEN
            IF(DIST2.NE.P_MIN(DIST2)) THEN
              X1=0.D0
              Y1=0.D0
            ENDIF
            X1=P_MIN(X1)+P_MAX(X1)
            Y1=P_MIN(Y1)+P_MAX(Y1)
!           ALL PROCESSORS HAVING THE POINT MUST KNOW IT
!           YET POINTS WITHIN A TRIANGLE IN ANOTHER PROCESSOR
!           AND FALLEN BACK ON AN INTERFACE HAVE BEEN OVERLOOKED
!           SO FAR BY OTHER PROCESSORS.
            DO I=1,NPOIN
              IF(X(I).EQ.X1.AND.Y(I).EQ.Y1) IP(K)=I
            ENDDO
          ENDIF
          WRITE(LU,*)
          WRITE(LU,*) 'SOURCE POINT ',K,'PUT ON POINT'
          WRITE(LU,*) X1,' AND ',Y1
          D2 = SQRT(P_MIN(DIST2))
          WRITE(LU,*) 'LOCATED AT ',D2,' METRES'
!         LINE FEED FOR THE LISTING
          IF(K.EQ.NP) WRITE(LU,*)
        ENDIF
      ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
