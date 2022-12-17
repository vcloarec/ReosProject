!                 *************************************
                  DOUBLE PRECISION FUNCTION DIS_STA_CUR
!                 *************************************
!
     &(IFRLIQ,PTS,QZ,NFRLIQ,ZN)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE DISCHARGE AS A FUNCTION OF THE FREE SURFACE
!+        ELEVATION BY INTERPOLATING FROM A STAGE-DISCHARGE CURVE.
!
!history  J-M HERVOUET (LNHE)
!+        28/11/2013
!+        V7P0
!+   First version inspired from sta_dis_cur.f
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFRLIQ         |-->| LIQUID BOUNDARY NUMBER
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| PTS            |-->| NUMBER OF POINTS IN THE STAGE-DISCHARGE CURVE
!| QZ             |-->| ARRAY WITH STAGE-DISCHARGE CURVES
!| ZN             |-->| ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IFRLIQ,NFRLIQ,PTS
      DOUBLE PRECISION, INTENT(IN) :: ZN,QZ(2,NFRLIQ,PTS)
!                                                    PTS AT LEAST
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION GOAL,TETA,Z1,Z2
!
!-----------------------------------------------------------------------
!
      Z1=QZ(2,IFRLIQ,1)
      Z2=QZ(2,IFRLIQ,PTS)
      IF(ZN.LE.Z1) THEN
!       OUTSIDE THE CURVE WITH LOWER ELEVATION
        GOAL=QZ(1,IFRLIQ,1)
      ELSEIF(ZN.GE.Z2) THEN
!       OUTSIDE THE CURVE WITH HIGHER ELEVATION
        GOAL=QZ(1,IFRLIQ,PTS)
      ELSE
!       IN BETWEEN: CASE WITH INTERPOLATION
        I=1
1       CONTINUE
        Z2=QZ(2,IFRLIQ,I+1)
        IF(ZN.GE.Z1.AND.ZN.LE.Z2) THEN
          TETA=(ZN-Z1)/MAX(Z2-Z1,1.D-8)
          GOAL=QZ(1,IFRLIQ,I)+TETA*(QZ(1,IFRLIQ,I+1)-QZ(1,IFRLIQ,I))
        ELSE
          I=I+1
          Z1=Z2
          GO TO 1
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      DIS_STA_CUR=GOAL
!
!-----------------------------------------------------------------------
!
      RETURN
      END
