!                   *******************
                    SUBROUTINE VISTURSA
!                   *******************
!
     &(VISC, VISCSA,NPOIN,PROPNU)
!
!***********************************************************************
! TELEMAC2D                                                  31/08/2015
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY ACCORDING TO SPALART ALLMARAS.
!
!history  A BOURGOIN (LNHE)
!+        31/08/2015
!+        V7P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| VISCSA         |-->| SPALART ALLAMRAS VISCOSITY
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PROPNU         |-->| LAMINAR VISCOSITY
!| VISC           |<--| TURBULENT DIFFUSION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TELEMAC2D, EX_VISTURSA => VISTURSA
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: PROPNU
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: VISCSA
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VISC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           I
      DOUBLE PRECISION  FV1,CHI3,CV13,PROPNU2
!
!-----------------------------------------------------------------------
!
!     REPERE CV1
      CV13    =7.1D0**3
      PROPNU2=1.D-6
!
      DO I=1,NPOIN
        CHI3 = (VISCSA%R(I)/PROPNU2)**3
        FV1  = CHI3/(CHI3+CV13)
        VISC%R(I) = PROPNU + VISCSA%R(I)*FV1
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
