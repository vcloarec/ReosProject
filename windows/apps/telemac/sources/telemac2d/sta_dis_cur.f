!                   *************************************
                    DOUBLE PRECISION FUNCTION STA_DIS_CUR
!                   *************************************
!
     &(IFRLIQ,FLUX,PTS,QZ,NFRLIQ,ZN,RELAX_STA_DIS)
!
!***********************************************************************
! TELEMAC2D   V8P3
!***********************************************************************
!
!brief    PRESCRIBES THE FREE SURFACE ELEVATION AS A FUNCTION OF
!+                THE DISCHARGE BY INTERPOLATING FROM A STAGE-DISCHARGE
!+                CURVE.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
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
!| FLUX           |-->| ACTUAL FLUX AT THIS BOUNDARY
!| IFRLIQ         |-->| LIQUID BOUNDARY NUMBER
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| PTS            |-->| NUMBER OF POINTS IN THE STAGE-DISCHARGE CURVE
!| QZ             |-->| ARRAY WITH STAGE-DISCHARGE CURVES
!| RELAX_STA_DIS  |-->| RELAXATION COEFFICIENT FOR STAGE-DISCHARGE CURVE
!| ZN             |-->| PREVIOUS ELEVATION (FOR RELAXATION)
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
      DOUBLE PRECISION, INTENT(IN) :: ZN,FLUX,QZ(2,NFRLIQ,PTS)
!                                                         PTS AT LEAST
      DOUBLE PRECISION, INTENT(IN) :: RELAX_STA_DIS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION GOAL,TETA,Q1,Q2
!
!-----------------------------------------------------------------------
!
      Q1=QZ(1,IFRLIQ,1)
      Q2=QZ(1,IFRLIQ,PTS)
      IF(FLUX.LE.Q1) THEN
!       OUTSIDE THE CURVE WITH LOWER DISCHARGE
        GOAL=QZ(2,IFRLIQ,1)
      ELSEIF(FLUX.GE.Q2) THEN
!       OUTSIDE THE CURVE WITH HIGHER DISCHARGE
        GOAL=QZ(2,IFRLIQ,PTS)
      ELSE
!       IN BETWEEN: CASE WITH INTERPOLATION
        I=1
1       CONTINUE
        Q2=QZ(1,IFRLIQ,I+1)
        IF(FLUX.GE.Q1.AND.FLUX.LE.Q2) THEN
          TETA=(FLUX-Q1)/MAX(Q2-Q1,1.D-8)
          GOAL=QZ(2,IFRLIQ,I)+TETA*(QZ(2,IFRLIQ,I+1)-QZ(2,IFRLIQ,I))
        ELSE
          I=I+1
          Q1=Q2
          GO TO 1
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     RELAXATION OF RESULT
!
!     OLD HARD CODED RELAXATION COEFFICIENT RELAX_STA_DIS = 0.02D0
      STA_DIS_CUR=ZN+RELAX_STA_DIS*(GOAL-ZN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
