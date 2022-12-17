!                   ****************
                    SUBROUTINE VALRO
!                   ****************
!
     &(RO,S,ROEAU)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE DENSITY ACCORDING TO SALINITY.
!
!history  J-M HERVOUET (LNH)
!+        01/09/1994
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
!| RO             |<--| WATER DENSITY
!| ROEAU          |-->| WATER DENSITY AT AVERAGE TEMPERATURE
!|                |   | WHEN SALINITY IS ZERO
!| S              |-->| BLOCK OF TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: S
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RO
      DOUBLE PRECISION, INTENT(IN)  :: ROEAU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     BEWARE: IT IS ASSUMED HERE THAT SALINITY IS THE FIRST TRACER
!
      CALL OS( 'X=CY    ' , X=RO , Y=S%ADR(1)%P , C=0.749979D0 )
      CALL OS( 'X=X+C   ' , X=RO , C=ROEAU      )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
