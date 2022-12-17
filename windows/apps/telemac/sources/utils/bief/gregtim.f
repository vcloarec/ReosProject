!                           ******************
                            SUBROUTINE GREGTIM
!                           ******************
!
     &(JULTIM,YEAR,MONTH,DAY,HOUR,MINU,SEC)
!
!
!***********************************************************************
! BIEF   V6P2                                   31/08/2011
!***********************************************************************
!
!brief    COMPUTES THE GREGORIAN CALENDAR DATE
!+        (YEAR,MONTH,DAY,HOUR,MIN,SEC)
!+        GIVEN THE JULIAN DATE (JD) IN CENTURY
!
!history  C.-T. PHAM (EDF-LNHE)
!+        31/08/2011
!+        V6P2
!+        FROM http://aa.usno.navy.mil/faq/docs/JD_Formula.php :
!+        GDATE ALGORITHM
!+        ORIGINAL ARTICLE: FLIEGEL AND VAN FLANDERN (1968),
!+        A MACHINE ALGORITHM FOR PROCESSING CALENDAR DATES
!+        FOR YEAR, MONTH AND DAY;
!+        AND FROM DELTARES, INITIALLY WL, SECTOR WATERBEHEER & MILIEU
!+        PROJET T0467 OR T1234.56, ANDRE HENDRIKS, V 1.01 (930429)
!+        FOR HOUR,MIN,SEC
!+
!
!history  U.H.Merkel
!+        19/07/2012
!+        V6P2
!+        Renamed MIN -> MINU because of Problems with NAG Compiler
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DAY            |<->| DAY    (1-28, 29, 30 OR 31)
!| HOUR           |<->| HOUR   (0-23) IN UNIVERSAL TIME
!| JULTIM         |-->| JULIAN DAY IN CENTURY
!| MIN            |<->| MINUTE (0-59) IN UNIVERSAL TIME
!| MONTH          |<->| MONTH  (1-12)
!| SEC            |<->| SECOND (0-59) IN UNIVERSAL TIME
!| YEAR           |<->| YEAR   (-4713-..)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(INOUT) :: YEAR,MONTH,DAY,HOUR,MINU,SEC
      DOUBLE PRECISION, INTENT(IN)    :: JULTIM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,L,N
      DOUBLE PRECISION JD,JDR
!
      INTRINSIC INT
!
!-----------------------------------------------------------------------
!
!  JULTIM UNIT: CENTURY
!  JD UNIT    : DAY
!
!  2415020 <=> 31/12/1899: DUE TO THE SHIFT IN JULTIM IN TELEMAC/BIEF
!
      JD=JULTIM*36525.D0+2415020.D0
!
      JDR=MOD(JD,1.D0)
!
      IF (JDR.LT.0.5D0) THEN
        JDR = JDR+0.5D0
      ELSE
        JDR = JDR-0.5D0
        JD  = JD+1.D0
      ENDIF
!
      L = INT(JD)+68569
      N = 4*L/146097
      L = L-(146097*N+3)/4
      I = 4000*(L+1)/1461001
      L = L-1461*I/4+31
      J = 80*L/2447
      K = L-2447*J/80
      L = J/11
      J = J+2-12*L
      I = 100*(N-49)+I+L
!
      YEAR  = I
      MONTH = J
      DAY   = K
!
      HOUR = INT(JDR*24.D0)
      MINU  = INT(JDR*1440.D0)-60*HOUR
      SEC  = NINT(JDR*86400.D0)-3600*HOUR-60*MINU
!
!  TO AVOID SEC = 60
!
      IF(SEC.EQ.60) THEN
        SEC = 0
        MINU = MINU + 1
      ENDIF
!
      IF(MINU.GE.60) THEN
        MINU  = MINU  - 60
        HOUR = HOUR + 1
      ENDIF
!
      IF(HOUR.GE.24) THEN
        HOUR = HOUR - 24
        DAY  = DAY  + 1
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
