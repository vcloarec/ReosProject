!                   ********************
                    SUBROUTINE FLUX_MASK
!                   ********************
!
     &(FXMAT,NSEG,GLOSEG,SIZGLO,MASKPT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MASKS THE FLUXES BY SEGMENT USING THE MASKS OF
!+                THE SEGMENT ENDS.
!
!history  J-M HERVOUET (LNHE)
!+        19/06/08
!+        V5P9
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
!| FXMAT          |<--| MATRIX FOR STORING THE FLUXES.
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| NSEG           |-->| NUMBER OF SEGMENTS
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_MASK => FLUX_MASK
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,SIZGLO
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      DOUBLE PRECISION, INTENT(INOUT) :: FXMAT(NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: MASKPT(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DO I = 1,NSEG
        FXMAT(I) = FXMAT(I) * MASKPT(GLOSEG(I,1)) * MASKPT(GLOSEG(I,2))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
