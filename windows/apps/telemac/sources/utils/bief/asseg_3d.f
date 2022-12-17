!                   *******************
                    SUBROUTINE ASSEG_3D
!                   *******************
!
     &(FLOW,F,NPOIN3,NPLAN,NSEG2D,GLOSEG,SIZGLO,INIFLO)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES HORIZONTAL EDGE BY EDGE FLUXES ON POINTS.
!
!history  J-M HERVOUET (LNHE)
!+        18/05/09
!+        V6P0
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
!| F              |<--| F WHERE THE FLUXES ARE ASSEMBLED
!| FLOW           |-->| FLUXES (SIZE OF FLOW MAY NOT EXCEED
!|                |   | NSEG2D*NPLAN, THOUGH THE TOTAL NUMBER OF
!|                |   | SEGMENTS IS LARGER)
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!| INIFLO         |-->| IF(YES) F WILL BE INITIALISED AT 0.
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN3         |-->| NUMBER OF POINTS
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)             :: NSEG2D,NPOIN3,SIZGLO,NPLAN
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: FLOW(*)
!                                        HERE * = NESG2D*NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN3)
      LOGICAL, INTENT(IN)             :: INIFLO
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER ISEG,I
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE FLOW TO 0.D0
!
      IF(INIFLO) THEN
        DO I = 1,NPOIN3
          F(I) = 0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ASSEMBLES THE FLUXES OF HORIZONTAL SEGMENTS
!
      DO ISEG = 1,NSEG2D*NPLAN
        F(GLOSEG(ISEG,1))=F(GLOSEG(ISEG,1))+FLOW(ISEG)
        F(GLOSEG(ISEG,2))=F(GLOSEG(ISEG,2))-FLOW(ISEG)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
