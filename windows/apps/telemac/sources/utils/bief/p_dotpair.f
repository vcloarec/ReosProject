!                   *********************
                    SUBROUTINE P_DOTPAIR
!                   *********************
!
     &(NPOIN,X,Y,IFAC,PAIR)
!
!***********************************************************************
! BIEF   V7                                  24/02/2016
!***********************************************************************
!
!brief     SCALAR PRODUCT OF VECTORS X AND Y (SIZE NPOIN)
!+                TAKING PARALLELISM INTO ACCOUNT
!+                RETURN THE PAIR RESULT AND ERROR
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FAC            |-->| FAC=1/(NUMBER OF NEIGHBOURING SUB-DOMAINS)
!| NPOIN          |-->| SIZE OF X AND Y
!| X              |-->| VECTOR
!| Y              |-->| VECTOR
!| PAIR           |-->| DOT RESULT AND DOT ERROR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_P_DOTPAIR => P_DOTPAIR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
      INTEGER, INTENT(IN)          :: IFAC(NPOIN)
      DOUBLE PRECISION, INTENT(OUT) ::PAIR(2)

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      DOUBLE PRECISION :: P,S,H,R,Q
      DOUBLE PRECISION :: TMP
      INTEGER I
!
!-----------------------------------------------------------------------
!
      P=0.D0
      S=0.D0
      H=0.D0
      R=0.D0
      Q=0.D0
      PAIR=0.D0
      IF  (NPOIN .EQ. 0) THEN
        PAIR=0.D0
        RETURN
      END IF

      CALL TWOPROD(X(1),Y(1)*IFAC(1),P,S)

      DO I = 2 , NPOIN
        CALL TWOPROD(X(I),Y(I)*IFAC(I),H,R)
        TMP = P
        CALL TWOSUM(TMP,H,P,Q)
        S=S+(Q+R)
      END DO
      CALL TWOSUM(P,S,PAIR(1),PAIR(2))

!-----------------------------------------------------------------------
!
      RETURN
      END
