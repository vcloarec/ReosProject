!                       **********************************
                        DOUBLE PRECISION FUNCTION LIMITER
!                       **********************************
!
     &(ILIM,R,C)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.2                                           R.ATA
!***********************************************************************
!
!     FUNCTION THAT COMPUTES THE VALUE OF A GIVEN LIMITER FOR WAF SCHEME
!        BASED IN THE EXCELLENT PAPER OF FRIEDMANN: "A COMPARATIVE
!         STUDY OF TVD-LIMITERS- WELL KNOWN LIMITERS AND AN
!           INTRODUCTION OF NEW ONES". INT.J.NUMER.METH.FLUIDS(2010)
!
!history  R. ATA (EDF-LNHE) 04/15/2011
!+
!+        V6P1
!+
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |  ILIM          | -->|  OPTION POUR LIMITEUR                        |
! |                |    |     1 : MINMOD                               |
! |                |    |     2 : VAN ALBADA                           |
! |                |    |     3 : SUPERBEE                             |
! |                |    |     4 : MINBEE                               |
! |                |    |     5 : VAN LEER                             |
! |  Q             | -->|  CHOICE OF Q :                               |
! |                |    |     H : IF L(LEFT) OR R (RIGHT)              |
! |                |    |     V : FOR * STATE                          |
! |  R             | -->|  RATIO OF UPWIND CHANGE TO LOCAL CHANGE OF Q |
! |  C             | -->|  CELERITY                                    |
! |                |    |  FINITE VOLUME SCHEME                        |
! |                |    |     0      : ROE SCHEME                      |
! |                |    |     1      : KINETIC SCHEME                  |
! |                |    |     2      : ZOKAGOA SCHEME                  |
! |                |    |     3      : TCHAMEN SCHEME                  |
! |                |    |     4      : HLLC SCHEME                     |
! |                |    |     5      : WAF SCHEME                      |
! |________________|____|______________________________________________|
!
!  MODE: -->(UNCHANGEABLE INPUT),<--(OUTPUT),<-->(CHANGEABLE INPUT)
!-----------------------------------------------------------------------
!  CALLING SUBROUTINE FLUX_WAF
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: ILIM
      DOUBLE PRECISION, INTENT(IN) :: R,C
!
      DOUBLE PRECISION EPS
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
!     WAF SCHEME: WHAT WE COMPUTE HERE IS NOT THE LIMITER B(r)
!                 BUT THE FUNCTION A(r) = 1-(1-|C|)B(r)

      SELECT CASE (ILIM)
        CASE (1)
!
!       MINMOD
!
          LIMITER = 1.0D0 - (1.0D0-ABS(C))
     &             *0.5D0*(1.0D0 + SIGN(1.D0,R)) ! SIGN(A,B) = |A|*SIGN(B)
     &             *ABS(R)
!
!
!         ELSEIF (ILIM.EQ.2) THEN
        CASE (2)
!
!       VAN ALBADA
!
          IF(R.LE.0)THEN
            LIMITER = 1.0D0
          ELSE
            LIMITER = 1.0D0-(1.0D0-ABS(C))*R*(1.0D0+R)/(1.0D0+R**2)
          ENDIF
!
        CASE(3)
!
!       VAN LEER
!
          EPS = 1.D-12
          IF(R.LE.0)THEN
            LIMITER = 1.0D0
          ELSE
            LIMITER = 1.0D0-2.0D0*(1.0D0-ABS(C))*R/(1.0D0+R+EPS)
          ENDIF
!
        CASE(4)
!
!       MINBEE
!
          IF(R.LE.0)THEN
            LIMITER = 1.0D0
          ELSEIF(R.GT.0.0D0.AND.R.LE.1)THEN
            LIMITER = 1.0D0-(1.0D0-ABS(C))*R
          ELSE
            LIMITER = ABS(C)
          ENDIF
!
        CASE(5)
!
!       SUPERBEE
!
          IF(R.LE.0)THEN
            LIMITER = 1.0D0
          ELSEIF(R.GT.0.0D0.AND.R.LE.0.5D0)THEN
            LIMITER = 1.0D0-2.0D0*(1.0D0-ABS(C))*R
          ELSEIF(R.GT.0.5D0.AND.R.LE.1.0D0)THEN
            LIMITER = ABS(C)
          ELSEIF(R.GT.1.0D0.AND.R.LE.2.0D0)THEN
            LIMITER = 1.0D0-(1.0D0-ABS(C))*R
          ELSE
            LIMITER = 2.0D0*ABS(C)-1.0D0
          ENDIF
!
        CASE DEFAULT
!
          WRITE(LU,*)'INVALID LIMITER FOR WAF SCHEME'
          CALL PLANTE(1)
          STOP
!-----------------------------------------------------------------------
!
      END SELECT

      RETURN
      END FUNCTION LIMITER
