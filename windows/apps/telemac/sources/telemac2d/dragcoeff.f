!                   ********************
                    SUBROUTINE DRAGCOEFF
!                   ********************
!
     & (V, D, VK, CW)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE DRAG COEFFICIENT BEHIND A CYLINDER.
!
!history  F. HUVELIN
!+        20/04/2004
!+        V5P5
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
!| CW             |<--| DRAG COEFFICIENT BEHIND A CYLINDER
!| D              |-->| DIAMETER
!| V              |-->| VELOCITY UPSTREAM
!| VK             |-->| LAMINAR VISCOSITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)  :: V, D, VK
      DOUBLE PRECISION, INTENT(OUT) :: CW
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION              :: RE
!
!=======================================================================!
!=======================================================================!
!
      RE = V * D / VK
!
      IF (RE.LE.800.0D0) THEN
        CW = 3.07D0 / RE**(0.168D0)
      ELSEIF(RE.LE.6000.D0) THEN
        CW = 1.D0
      ELSEIF(RE.LE.11000.0D0) THEN
        CW = 1.0D0+0.2D0*(RE-6000.D0)/5000.D0
      ELSE
        CW = 1.2D0
      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
