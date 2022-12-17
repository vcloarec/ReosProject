!                   *****************
                    SUBROUTINE COSAKE
!                   *****************
!
     &(KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,KMIN,KMAX,EMIN,EMAX)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SETS THE CONSTANTS FOR THE K-EPSILON MODEL.
!
!history  J-M HERVOUET (LNHE)
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
!| C1             |<--| CONSTANT IN K-EPSILON MODEL
!| C2             |<--| CONSTANT IN K-EPSILON MODEL
!| CMU            |<--| CONSTANT IN K-EPSILON MODEL
!| EMAX           |<--| MAXIMUM EPSILON IF CLIPPING
!| EMIN           |-->| MINIMUM EPSILON IF CLIPPING
!| ESTAR          |-->| CONSTANT IN K-EPSILON MODEL
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KMAX           |<--| MAXIMUM K IF CLIPPING
!| KMIN           |<--| MINIMUM K IF CLIPPING
!| SCHMIT         |-->| SCHMITT NUMBER
!| SIGMAE         |<--| K-EPSILON CONSTANT FOR DIFFUSION OF EPSILON
!| SIGMAK         |<--| K-EPSILON CONSTANT FOR DIFFUSION OF K
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(OUT) :: KMIN,KMAX,EMIN,EMAX
      DOUBLE PRECISION, INTENT(OUT) :: KARMAN,CMU,C1,C2
      DOUBLE PRECISION, INTENT(OUT) :: SIGMAK,SIGMAE,ESTAR,SCHMIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! KARMAN CONSTANT
!
!     UP TO VERSION 6.0 : 0.41, FROM VERSION 6.1 ON : 0.40
      KARMAN = 0.40D0
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
      SIGMAK = 1.00D0
      SIGMAE = 1.30D0
      ESTAR  = 0.15D0
!
! SCHMIDT NUMBER
!
      SCHMIT = 0.50D0
!
! RANGE OF VALUES USED TO CLIP K AND EPSILON
!
      KMIN = 1.D-8
      EMIN = 1.D-8
      KMAX = 1.D10
      EMAX = 1.D10
!
!-----------------------------------------------------------------------
!
      RETURN
      END
