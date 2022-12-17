!                   ******************
                    SUBROUTINE SUMTWO
!                   ******************
!
     &(N,P,RES)
!
!***********************************************************************
! PARALLEL   V7P3                                  24/02/2016
!***********************************************************************
!
!brief    CALCULATE A COMPENSATED SUM OF A VECTOR.
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| N            |-->| INTEGER VECTOR SIZE
!| P            |-->| DOUBLE PRECISION VECTOR
!| Y            |<--| DOUBLE PRECISION VECTOR RESULT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ,INTENT(IN) ::N
      DOUBLE PRECISION, INTENT(IN) :: P(N)
      DOUBLE PRECISION, INTENT(OUT) :: RES
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION R(N)
      DOUBLE PRECISION TMP, TMP2
      INTEGER I
!
!-----------------------------------------------------------------------
!
      RES=0.D0
      R=P
      DO I = 2 , N
        TMP = R(I)
        TMP2 = R(I-1)
        CALL TWOSUM1(TMP,R(I-1),R(I),TMP2)
      END DO
      DO I = 1 , N-1
        RES=RES+R(I)
      END DO
      RES=RES+R(N)
!
!
!-----------------------------------------------------------------------
!
      END
      SUBROUTINE TWOSUM1(A,B,X,Y)

      DOUBLE PRECISION, INTENT(IN)  :: A,B
      DOUBLE PRECISION, INTENT(OUT) :: X,Y
      DOUBLE PRECISION Z, XOUT
      XOUT=A+B
      Z=XOUT-A
      Y=(A-(XOUT-Z))+(B-Z)
      X = XOUT

      END
