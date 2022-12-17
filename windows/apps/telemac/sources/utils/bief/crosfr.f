!                   *****************
                    SUBROUTINE CROSFR
!                   *****************
!
     &(X,Y,XR,YR,XMAIL,YMAIL,NPMAX,NBOR,KP1BOR,NPTFR,DM,OK)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WANT TO INTERPOLATE THE BOTTOM ELEVATION FOR A POINT
!+                WITH COORDINATES X AND Y. A POINT (XR,YR) IS USED
!+                IN THIS INTERPOLATION.
!+
!+            CHECKS HERE THAT THIS POINT IS NOT OUTSIDE OF THE
!+                DOMAIN, I.E. CHECKS THAT THE SEGMENT LINKING (X,Y)
!+                AND (XR,YR) DOES NOT INTERSECT WITH THE DOMAIN
!+                BOUNDARY.
!
!warning  JMH : DOES NOT WORK IN PARALLEL MODE
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
!| DM             |-->| MINIMUM DISTANCE TO THE BOUNDARY
!| KP1BOR         |-->| RANK OF THE NEXT POINT ALONG THE BOUNDARY
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY NODES
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY NODES
!| OK             |<--| IF YES, NO CROSSING OF BOUNDARY
!|                |   | IF NO, POINT OUTSIDE THE DOMAIN
!| X              |-->| ABSCISSA OF POINT WHERE TO INTERPOLATE
!| Y              |   | ORDINATE OF POINT WHERE TO INTERPOLATE
!| XMAIL          |-->| ABSCISSAE OF POINTS IN THE MESH
!| YMAIL          |-->| ORDINATES OF POINTS IN THE MESH
!| XR             |-->| ABSCISSA OF POINT TO BE USED FOR INTERPOLATION
!| XR,YR          |-->| ORDINATE OF POINT TO BE USED FOR INTERPOLATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: X,Y,XR,YR,DM
      INTEGER, INTENT(IN)          :: NPTFR,NPMAX
      DOUBLE PRECISION, INTENT(IN) :: XMAIL(NPMAX),YMAIL(NPMAX)
      INTEGER, INTENT(IN)          :: NBOR(NPTFR),KP1BOR(NPTFR)
      LOGICAL, INTENT(INOUT)       :: OK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER KA
!
      DOUBLE PRECISION DM2,XA,YA,XB,YB,DET,ALFA,BETA,EPS,DISTA2,DISTB2
!
!-----------------------------------------------------------------------
!
!     DOES NOT CONSIDER POINTS TOO CLOSE TO THE BOUNDARY
!     DM     : MINIMUM DISTANCE
      DM2 = DM**2
!
      DO KA=1,NPTFR
!
! INTERSECTION OF A BOUNDARY SEGMENT AND THE SEGMENT
! FORMED BY THE POINTS (X,Y) AND (XR,YR)
!
        XA = XMAIL(NBOR(KA))
        YA = YMAIL(NBOR(KA))
        XB = XMAIL(NBOR(KP1BOR(KA)))
        YB = YMAIL(NBOR(KP1BOR(KA)))
!
        DET = (XR-X)*(YA-YB) - (YR-Y)*(XA-XB)
!
        IF(ABS(DET).LT.1.D-6) CYCLE
!
        ALFA = ( (XA-X)*(YA-YB) - (YA-Y)*(XA-XB) ) / DET
        BETA = ( (XR-X)*(YA-Y ) - (YR-Y)*(XA-X ) ) / DET
!
        EPS=0.05D0
        IF(ALFA.GE.EPS.AND.ALFA.LE.1.D0-EPS.AND.
     &     BETA.GE.EPS.AND.BETA.LE.1.D0-EPS) THEN
          OK = .FALSE.
          EXIT
        ENDIF
!
! ALSO ELIMINATES THE POINTS TOO CLOSE TO THE BOUNDARY
!
        DISTA2 = (XR-XA)**2 + (YR-YA)**2
        DISTB2 = (XR-XB)**2 + (YR-YB)**2
        IF(DISTA2.LT.DM2.OR.DISTB2.LT.DM2) THEN
          OK = .FALSE.
          EXIT
        ENDIF
!
      ENDDO ! KA
!
!-----------------------------------------------------------------------
!
      RETURN
      END
