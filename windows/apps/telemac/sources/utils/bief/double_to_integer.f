!                   ****************************
                    SUBROUTINE DOUBLE_TO_INTEGER
!                   ****************************
!
     &(X,IX,N,QT,NSUM)
!
!***********************************************************************
! BIEF   V7P0                                   13/01/2014
!***********************************************************************
!
!brief    Coding a double precision array as an I8 integer.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        13/01/2014
!+        V7P0
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IX             |<--| INTEGER ARRAY CODING THE REAL ARRAY
!| N              |-->| NUMBER OF POINTS IN THE ARRAYS X AND IX
!| NSUM           |-->| MAXIMUM NUMBER OF SUMS THAT WILL BE POSSIBLE
!| QT             |<--| THE QUANTUM USED FOR CODING
!| X              |-->| DOUBLE PRECISION ARRAY TO BE CODED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_PARALLEL
      USE BIEF, EX_DOUBLE_TO_INTEGER => DOUBLE_TO_INTEGER
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: N,NSUM
      INTEGER(KIND=K8), INTENT(INOUT) :: IX(N)
      DOUBLE PRECISION, INTENT(IN)    :: X(N)
      DOUBLE PRECISION, INTENT(INOUT) :: QT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      INTEGER(KIND=K8) IMAX
      DOUBLE PRECISION XMIN,XMAX,SURQT
!
      INTRINSIC NINT,MAX,ABS
!
!-----------------------------------------------------------------------
!
      CALL MINI(XMIN,I,X,N)
      CALL MAXI(XMAX,I,X,N)
!
      IF(NCSIZE.GT.1) THEN
        XMIN=P_MIN(XMIN)
        XMAX=P_MAX(XMAX)
      ENDIF
!
!     WE WANT A RANGE CENTRED ON 0, THAT CONTAINS [XMIN,XMAX]
!
      XMAX=MAX(ABS(XMIN),ABS(XMAX))
      IMAX=HUGE(IMAX)/NSUM
!
      QT=XMAX/DBLE(IMAX)
!
      IF(QT.EQ.0.D0) THEN
        SURQT=0.D0
      ELSE
        SURQT=1.D0/QT
      ENDIF
!
!-----------------------------------------------------------------------
!     CODING
!-----------------------------------------------------------------------
!
      DO I=1,N
        IX(I)=NINT(X(I)*SURQT)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
