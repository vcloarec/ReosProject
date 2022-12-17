!                   ******************
                    SUBROUTINE ZEROPHI
!                   ******************
!
     &(X0,X,NIT,CA1)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ZERO OF PHI-CA1 BY NEWTON'S METHOD.
!
!history  INRIA
!+
!+        V5P4
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
!| CA1            |-->| SECOND TERM
!| NIT            |<--| NUMBER OF ITERATIONS
!| X              |<--| FINAL X
!| X0             |-->| INITIAL GUESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)          :: NIT
      DOUBLE PRECISION, INTENT(IN)    :: X0,CA1
      DOUBLE PRECISION, INTENT(INOUT) :: X
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NITEPS
!
      DOUBLE PRECISION EPS,EPSX,SQ32,SQ3,AMPLUS,AMMOINS,DPHI,FDF
!
!-----------------------------------------------------------------------
!
      SQ3 =SQRT(3.D0)
      SQ32=SQRT(1.5D0)
      EPS=1.E-12
      EPSX=1.E-12
      NIT=0
      NITEPS=0
      X=X0
!
1     NIT=NIT+1
!
      IF(X.GE.SQ32) THEN
      X= SQ32 - EPSX
      NITEPS= NITEPS + 1
      ENDIF
      IF(NITEPS.EQ.3) THEN
      X = SQ32
      GOTO 10
      ENDIF
!
      AMPLUS =MIN(-X,+SQ32)
      AMMOINS=MIN(-X,-SQ32)
!
      DPHI= AMPLUS-AMMOINS
      FDF = (DPHI*(AMPLUS+AMMOINS+2.D0*X)-2.D0*SQ3*CA1)/(2.D0*DPHI)
!
      IF(ABS(FDF).LT.EPS) GOTO 10
      IF(NIT.EQ.20) GOTO 10
!
      X=X-FDF
      GOTO 1
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
