!                   ******************
                    SUBROUTINE TWOSUM
!                   ******************
!
     &(A,B,X,Y)
!
!***********************************************************************
! BIEF   V7P3                                  24/02/2016
!***********************************************************************
!
!brief    SCALAR SUM AND CALCULATE ROUNDING ERROR.
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A,B          |-->| DOUBLE PRECISION SCALAR
!| X            |<--| DOUBLE PRECISION SUM RESULT
!| Y            |<--| DOUBLE PRECISION ROUNDING ERROR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: A,B
      DOUBLE PRECISION, INTENT(OUT) :: X,Y
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION Z, XOUT
!
!-----------------------------------------------------------------------
!
        XOUT=A+B
        Z=XOUT-A
        Y=(A-(XOUT-Z))+(B-Z)
        X = XOUT
!
!
!-----------------------------------------------------------------------
!
      END
