!                   ******************
                    SUBROUTINE LONGITU
!                   ******************
!
     &(XEL,COSLAT,IKLE,NELMAX,NELEM)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    CORRECT THE ARRAYS THAT DEPEND ON THE LONGITUDE
!+                OF THE GIVEN ELEMENT.
!
!history  M.S.TURNBULL (HRW) and S.E.BOURBAN (HRW)
!+        01/10/2015
!+        V7P1
!+   Correction to the computation of the XEL in spherical coordinates.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| XEL            |<->| VECTOR DEFINED PER ELEMENT
!| COSLAT         |-->| COSINUS OF LATITUDE OF ORIGIN POINT
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: COSLAT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: XEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
      DOUBLE PRECISION R,PIR,SCAEL
!
      INTRINSIC ATAN
!
!-----------------------------------------------------------------------
!
! EARTH RADIUS
!
      R = 6370000.D0
!
!-----------------------------------------------------------------------
!
!     FILTER XEL ON THE DATELINE, AND THEN ONLY SCALE
!
!     WARNING: XEL ON EITHER SIDE OF THE SAME SEGMENT MIGHT BE DIFFERENT
!     ANOTHER SOLUTION WHERE SCAEL is SCAEDGE MIGHT BE MORE APPROPRIATE
!
      PIR = 4.D0 * ATAN(1.D0) * R
!
      DO IELEM = 1,NELEM
!
        IF(XEL(IELEM,2).GT.PIR) THEN
          XEL(IELEM,2) = XEL(IELEM,2) - 2.D0 * PIR
        ELSEIF(XEL(IELEM,2).LT.-PIR) THEN
          XEL(IELEM,2) = XEL(IELEM,2) + 2.D0 * PIR
        ENDIF
!
        IF(XEL(IELEM,3).GT.PIR) THEN
          XEL(IELEM,3) = XEL(IELEM,3) - 2.D0 * PIR
        ELSEIF(XEL(IELEM,3).LT.-PIR) THEN
          XEL(IELEM,3) = XEL(IELEM,3) + 2.D0 * PIR
        ENDIF
!
        SCAEL = ( COSLAT(IKLE(IELEM,1))
     &          + COSLAT(IKLE(IELEM,2)) +
     &            COSLAT(IKLE(IELEM,3)) ) / 3.D0
        XEL(IELEM,2) = XEL(IELEM,2) * SCAEL
        XEL(IELEM,3) = XEL(IELEM,3) * SCAEL
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
