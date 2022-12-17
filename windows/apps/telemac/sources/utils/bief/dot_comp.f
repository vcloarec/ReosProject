!                   **********************************
                    DOUBLE PRECISION FUNCTION DOT_COMP
!                   **********************************
!
     &(NPOIN,X,Y)
!
!***********************************************************************
! BIEF   V7P3                                   24/02/2016
!***********************************************************************
!
!brief   COMPENSATED SCALAR PRODUCT OF VECTORS X AND Y OF SIZE NPOIN.
!
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| TAILLE DE X ET Y
!| X              |-->| FIRST DOUBLE PRECISION VECTOR
!| Y              |-->| SECOND DOUBLE PRECISION VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION :: P,S,H,R,Q
      DOUBLE PRECISION TMP
!
!-----------------------------------------------------------------------
!
      IF(NPOIN .EQ. 0) THEN
        DOT_COMP=0.D0
        RETURN
      ENDIF
!
      DOT_COMP = 0.D0
      P=0.D0
      S=0.D0
      H=0.D0
      R=0.D0
      Q=0.D0
!
      CALL TWOPROD(X(1),Y(1),P,S)
!
      DO I = 2 , NPOIN
!
        H=0.D0
        R=0.D0
        Q=0.D0
        CALL TWOPROD(X(I),Y(I),H,R)
        TMP = P
        CALL TWOSUM(TMP,H,P,Q)
        S=S+(Q+R)
!
      END DO
      DOT_COMP = P+S
!
!-----------------------------------------------------------------------
!
      RETURN
      END
