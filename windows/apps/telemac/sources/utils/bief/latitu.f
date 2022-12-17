!                   *****************
                    SUBROUTINE LATITU
!                   *****************
!
     &(YDIST,COSLAT,SINLAT,LAMBD0,Y,NPOIN)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    COMPUTES THE ARRAYS THAT DEPEND ON THE LATITUDE
!+                OF THE GIVEN POINT.
!
!history  J-M HERVOUET (LNH)
!+        10/01/95
!+        V5P1
!+   First version.
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
!history  C-T PHAM (R&D, LNHE)
!+        17/01/2013
!+        V6P3
!+   More precise value of Earth radius.
!
!history  M.S.TURNBULL (HRW)
!+        24/09/2015
!+        V7P1
!+   Correction to the computation of the Y in spherical coordinates.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COSLAT         |<--| COSINUS OF LAMBDA0
!| LAMBD0         |-->| LATITUDE OF ORIGIN POINT
!| NPOIN          |-->| NUMBER OF POINTS
!| SINLAT         |<--| SINUS OF LAMBDA0
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: YDIST(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: COSLAT(NPOIN),SINLAT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION LB2RAD,R,SURR,PISUR4,PISUR2,XLAMB
!
      INTRINSIC TAN,ATAN,SIN,COS,EXP
!
!-----------------------------------------------------------------------
!
! EARTH RADIUS
!
      R = 6370000.D0
      SURR = 1.D0 / R
!
!-----------------------------------------------------------------------
!
      PISUR4 = ATAN(1.D0)
      PISUR2 = PISUR4 + PISUR4
!
!     LAMBD0/2 IN RADIANS
!
      LB2RAD = LAMBD0 * PISUR4 / 90.D0
!
!     1/COS(LAMBDA),COS(LAMBDA),SIN(LAMBDA)
!
      DO I = 1 , NPOIN
!
        XLAMB = 2.D0* ATAN(EXP(Y(I)*SURR)*TAN(LB2RAD+PISUR4))-PISUR2
        YDIST(I) = XLAMB * R
        COSLAT(I) = COS(XLAMB)
        SINLAT(I) = SIN(XLAMB)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
