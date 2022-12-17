!                   ***************
                    SUBROUTINE HREF
!                   ***************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE REFERENCE DEPTH FOR THE BOUSSINESQ
!+                EQUATIONS. BY DEFAULT THIS IS THE INITIAL DEPTH.
!
!note     THIS SUBROUTINE CAN BE USER-MODIFIED.
!+            FOR EXAMPLE IT CAN BE A LINEARISED DEPTH.
!+            TO GET BACK TO SAINT-VENANT, CAN HAVE H0 = 0.
!
!history  J-M HERVOUET (LNHE)
!+        01/03/1990
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      CALL OS('X=Y     ' , X=H0, Y=H)
      CALL USER_HREF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
