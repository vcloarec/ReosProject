!                   *****************
                    SUBROUTINE AKSAIN
!                   *****************
     &(VISCSA,NPOIN,NUMIN)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   31/08/2015
!***********************************************************************
!
!BRIEF    INITIALISES VISCSA.
!
!
!history  A BOURGOIN (LNHE)
!+        31/08/2015
!+        V7p0
!+
!history A BOURGOIN
!+       29/07/2017
!+        V7p2
!         improvements
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NUMIN          |-->| MINIMUM VISCSA IF CLIPPING
!| VISCSA         |-->| SPALART-ALLMARAS VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_AKSAIN => AKSAIN
      USE DECLARATIONS_TELEMAC2D, ONLY : CF,U,V,H
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         ,INTENT(IN   ) :: NPOIN
      DOUBLE PRECISION,INTENT(INOUT) :: VISCSA(NPOIN)
      DOUBLE PRECISION,INTENT(IN   ) :: NUMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           N
      DOUBLE PRECISION  USTAR,CNU
!
!-----------------------------------------------------------------------
!
      CNU=1.D0/12.96D0
      DO N=1,NPOIN
        USTAR = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2 ))
        VISCSA(N)=MAX(USTAR*H%R(N)*CNU,NUMIN)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
