!                       **********************
                        SUBROUTINE NODALF_PUGH
!                       **********************
!
     &(FFMN2,FFM4,NODALCORR,TEMPS,DEJA,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   23/03/2011
!***********************************************************************
!
!brief    COMPUTES NODAL FACTORS F FROM PUGH FORMULAE
!+
!
!history  C-T PHAM (LNHE)
!+        23/03/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFMN2          |<--| NODAL FACTOR FOR WAVES M2 AND N2
!| FFM4           |<--| NODAL FACTOR FOR WAVE  M4
!| MARDAT         |-->| DATE (YEAR_NP,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR_NP,MINUTE_NP,SECOND_NP)
!| NODALCORR      |-->| OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION
!| TEMPS          |-->| TIME
!| DEJA           |-->| LOGICAL FOR 1ST TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_NODALF_PUGH => NODALF_PUGH
      USE DECLARATIONS_TELEMAC2D, ONLY : YEAR_NP,HOUR_NP,MINUTE_NP,
     &                                   SECOND_NP,NDAY_NP
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NODALCORR,MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(OUT) :: FFMN2,FFM4
      DOUBLE PRECISION, INTENT(IN)  :: TEMPS
      LOGICAL, INTENT(IN)           :: DEJA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,DTR,NLUNPUGH,TJ,TJMIL,NMIL
!
      INTEGER MONTH,DAY,I
!
      INTRINSIC INT,MOD,DBLE,COS,ATAN
!
!-----------------------------------------------------------------------
!
      PI  = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
!
      IF(.NOT.DEJA) THEN
        YEAR_NP  = MARDAT(1)
        MONTH = MARDAT(2)
        DAY   = MARDAT(3)
!
        HOUR_NP   = MARTIM(1)
        MINUTE_NP = MARTIM(2)
        SECOND_NP = MARTIM(3)
!       NUMBER OF THE DAY IN YEAR_NP YEAR_NP
        NDAY_NP = DAY
!
        DO I=MONTH-1,1,-1
          IF((I.EQ.1).OR.(I.EQ.3).OR.(I.EQ.5).OR.(I.EQ.7).OR.(I.EQ.8)
     &    .OR.(I.EQ.10)) THEN
            NDAY_NP = NDAY_NP + 31
          ELSEIF((I.EQ.4).OR.(I.EQ.6).OR.(I.EQ.9).OR.(I.EQ.11)) THEN
            NDAY_NP = NDAY_NP + 30
          ELSEIF(I.EQ.2) THEN
            IF((MOD(YEAR_NP,4).NE.0)
     &      .OR.((MOD(YEAR_NP,100).EQ.0)
     &           .AND.(MOD(YEAR_NP,400).NE.0))) THEN
              NDAY_NP = NDAY_NP + 28
            ELSE
              NDAY_NP = NDAY_NP + 29
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
      TJ = DBLE(365*(YEAR_NP-1900)+(NDAY_NP-1)
     &         +DBLE(INT(DBLE(YEAR_NP-1901)/4.D0)))/36525.D0
     &   +(DBLE(HOUR_NP)+DBLE(MINUTE_NP)/60.D0
     &     +DBLE(SECOND_NP)/3600.D0)/876600.D0
     &   +TEMPS/3.15576D9
!
      NLUNPUGH = MOD(259.16D0-1934.14D0*TJ+0.0021D0*TJ**2,360.D0)
!
      IF(NODALCORR.EQ.2) THEN
        TJMIL = DBLE(365*(YEAR_NP-1900)+(183-1)
     &              +DBLE(INT(DBLE(YEAR_NP-1901)/4.D0)))/36525.D0
        NMIL  = MOD(259.16D0-1934.14D0*TJMIL+0.0021D0*TJMIL**2,360.D0)
        FFMN2 = 1.D0-0.037D0*COS(NMIL*DTR)
      ELSE
        FFMN2 = 1.D0-0.037D0*COS(NLUNPUGH*DTR)
      ENDIF
!
      FFM4  = FFMN2**2
!
!-----------------------------------------------------------------------
!
      RETURN
      END
