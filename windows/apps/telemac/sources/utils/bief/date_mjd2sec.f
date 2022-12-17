!               **************************************
                DOUBLE PRECISION FUNCTION DATE_MJD2SEC
!               **************************************
!
     &( DATE, TIME )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   16/01/2012
!***********************************************************************
!
!brief    CONVERTS DATE TO MJD (MODIFIED JULIAN DAYS)
!+        INPUT:  ID - DAY, MM - MONTH, IYYY - YEAR
!+                HH - HOUR, MN - MINUTES, SS - SECONDS
!+        OUTPUT: MJD > 0 - MODIFIED JULIAN DAYS
!+        DATE >= 11.17.1858 CORRESPONDS TO MJD = 0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  ID            |<->| DATE (YEAR, MONTH, DAY)
!|  TIME          |<->| TIME (HOUR, MINUTE, SECOND)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: DATE(3), TIME(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER DPM(12),DAYS,I,NLEAP,K
      PARAMETER ( DPM = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /) )
      INTEGER MM,ID,IYYY,HH,MN,SS
!
!-----------------------------------------------------------------------
!
      IYYY = DATE(1)
      MM = DATE(2)
      ID = DATE(3)
      HH = TIME(1)
      MN = TIME(2)
      SS = TIME(3)

      DATE_MJD2SEC = 0
!     NO EARLIER DATES THAN NOVEMBER 17TH 1858
      IF(     IYYY.LT.1858.OR.(IYYY.EQ.1858.AND.MM.LT.11)
     &   .OR.(IYYY.EQ.1858.AND.MM.EQ.11.AND.ID.LT.17) ) THEN
        WRITE(LU,*) 'NO EARLIER DATES ' //
     &               'THAN NOVEMBER 17TH 1858 ARE ALLOWED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DAYS = 0
      DO I = 1,MM-1
        DAYS = DAYS+DPM(I)
        IF( I.EQ.2.AND.INT(IYYY/4)*4.EQ.IYYY ) DAYS = DAYS+1
      ENDDO
!     321TH DAY CORRESPONDS TO NOVEMBER 17TH FOR A NON LEAP YEAR
      DAYS = DAYS+ID-321

!     LEAP DAY CORRECTION
      DO K = 1900,IYYY,100
        IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS-1
      ENDDO
      DO K = 2000,IYYY,400
        IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS+1
      ENDDO
!     EACH 4TH YEAR IS LEAP YEAR
      NLEAP = INT(REAL(IYYY-1-1860)*0.25)
      IF( IYYY.GT.1860 ) NLEAP = NLEAP+1
!     EXCEPT
      DO K = 1900,IYYY-1,100
        IF( K.LT.IYYY ) NLEAP = NLEAP-1
!     THE FOLLOWING LINE IS USELESS AS K.GE.IYYY-1
!       IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS-1
      ENDDO
!     BUT EACH IN THE ROW 2000:400:... IS LEAP YEAR AGAIN
      DO K = 2000,IYYY-1,400
        IF( K.LT.IYYY ) NLEAP = NLEAP+1
!     THE FOLLOWING LINE IS USELESS AS K.GE.IYYY-1
!       IF( K.EQ.IYYY.AND.MM.GT.2 ) DAYS = DAYS+1
      ENDDO
      DATE_MJD2SEC = (365.0*(IYYY-1858.0)+NLEAP+DAYS)*86400.0+
     &               HH*3600.0+MN*60.0+SS
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION DATE_MJD2SEC
