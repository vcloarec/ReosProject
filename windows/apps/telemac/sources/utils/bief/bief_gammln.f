!                 *************************************
                  DOUBLE PRECISION FUNCTION BIEF_GAMMLN
!                 *************************************
!
     &(XX)
!
!***********************************************************************
! BIEF   V6P3                                   07/03/2013
!***********************************************************************
!
!brief    Returns the value ln(gamma(xx) for xx > 0) Full accuracy is
!+        obtained for xx > 1.
!
!history  D J Evans-Roberts (HRW)
!+        23/11/1993
!+        V6P3
!+   First version sent by Michiel Knaapen (HRW) on 07/03/2013.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| XX             |-->| OPERAND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: XX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER J
      DOUBLE PRECISION :: COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      PARAMETER ( COF = (/ 76.18009173D0,-86.50532033D0,24.01409822D0,
     &             -1.231739516D0,0.120858003D-2,-0.536382D-5 /) )
      PARAMETER ( STP = 2.506628727465D0 )
      PARAMETER ( HALF = 0.5D0, ONE = 1.D0, FPF = 5.5D0 )
!
      INTRINSIC LOG
!
!-----------------------------------------------------------------------
!
      X   = XX - ONE
      TMP = X + FPF
      TMP = (X+HALF)*LOG(TMP) - TMP
      SER = ONE
!
      DO J = 1,6
        X   = X + ONE
        SER = SER + COF(J)/X
      ENDDO
!
      BIEF_GAMMLN = TMP + LOG(STP*SER)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
