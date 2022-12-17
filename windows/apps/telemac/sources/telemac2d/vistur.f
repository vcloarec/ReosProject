!                   *****************
                    SUBROUTINE VISTUR
!                   *****************
!
     &(VISC,AK,EP,NPOIN,CMU,PROPNU)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY ACCORDING TO K AND EPSILON.
!
!history  J-M HERVOUET (LNH)
!+        17/08/1994
!+        V5P2
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT KINETIC ENERGY
!| CMU            |-->| CONSTANT OF K-EPSILON MODEL
!| EP             |-->| DISSIPATION OF TURBULENT KINETIC ENERGY
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PROPNU         |-->| LAMINAR VISCOSITY
!| VISC           |<--| TURBULENT DIFFUSION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: CMU,PROPNU
      TYPE(BIEF_OBJ), INTENT(IN)    :: AK,EP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VISC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DO I = 1 , NPOIN
!
        VISC%R(I) = PROPNU + CMU * AK%R(I)**2 / EP%R(I)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
