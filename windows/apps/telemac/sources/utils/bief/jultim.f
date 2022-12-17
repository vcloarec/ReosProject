!                   ********************************
                    DOUBLE PRECISION FUNCTION JULTIM
!                   ********************************
!
     &(YEAR,MONTH,DAY,HOUR,MINU,SEC,AT)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TIME ELAPSED SINCE 31/12/1899.
!+                EXPRESSES IT IN JULIAN CENTURIES.
!
!history  E. DAVID (LHF)
!+        12/07/1995
!+        V5P1
!+
!
!history  JMH (EDF-LNHE)
!+        03/09/2010
!+        V6P0
!+   For consistency, YEAR is now INTENT(IN) only
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
!| AT             |-->| TIME IN SECONDS
!| DAY            |-->| DAY
!| HOUR           |-->| HOUR IN UNIVERSAL TIME
!| MIN            |-->| MINUTE IN UNIVERSAL TIME
!| MONTH          |-->| MONTH
!| SEC            |-->| SECOND IN UNIVERSAL TIME
!| YEAR           |-->| YEAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: MONTH,DAY,HOUR,MINU,SEC,YEAR
      DOUBLE PRECISION, INTENT(IN) :: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER Y,M,YEAR2
      DOUBLE PRECISION J
!
      INTRINSIC INT
!
      INTEGER, PARAMETER :: GREG=15+31*(10+12*1582)
!
!-----------------------------------------------------------------------
!
      YEAR2=YEAR
      IF(YEAR2.EQ.0) THEN
        WRITE (LU,101)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(YEAR2.LT.0) YEAR2=YEAR2+1
!
      IF (MONTH.GT.2) THEN
        Y=YEAR2
        M=MONTH+1
      ELSE
        Y=YEAR2-1
        M=MONTH+13
      ENDIF
!
      J=INT(365.25D0*Y)+INT(30.6001D0*M)+DAY+1720995.D0
      IF(DAY+31*(MONTH+12*YEAR2).GE.GREG) THEN
        J=J+2-INT(0.01D0*Y)+INT(0.25D0*INT(0.01D0*Y))
      ENDIF
      J=J-2415020.5D0
      JULTIM=(J+(HOUR+(MINU+(SEC+AT)/60.D0)/60.D0)/24.D0)/36525.D0
!
!---------------------------------------------------------------
!
101   FORMAT (//,10X,'**********************************',
     &         /,10X,'       JULTIM FUNCTION',
     &         /,10X,' THE VALUE FOR THE YEAR IS ZERO',
     &         /,10X,' COMPUTATION NOT POSSIBLE ...'
     &         /,10X,'**********************************')
!
!---------------------------------------------------------------
!
      RETURN
      END
