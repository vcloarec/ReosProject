!                   ********************
                    SUBROUTINE FLUX3DLIM
!                   ********************
!
     &(FLOW,FLULIM,NPLAN,NSEG2D,NPOIN2,OPT)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    LIMITS 3D HORIZONTAL EDGE BY EDGE FLUXES ON POINTS.
!
!history  J-M HERVOUET (LNHE)
!+        19/05/09
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
!history  J-M HERVOUET (LNHE)
!+        06/09/2012
!+        V6P2
!+   Option OPT added for crossed segments. Argument NPOIN2 added.
!+   Prisms and prisms cut into tetrahedra now treated.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLOW           |-->| FLUXES (SIZE OF FLOW MAY NOT EXCEED
!|                |   | NSEG2D*NPLAN, THOUGH THE TOTAL NUMBER OF
!|                |   | SEGMENTS IS LARGER)
!| FLULIM         |-->| LIMITING FACTOR OF 2D SEGMENTS
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!| OPT            |-->| 1: HORIZONTAL SEGMENTS
!|                |   | 2: HORIZONTAL AND CROSSED SEGMENTS FOR PRISMS
!|                |   | 3: HORIZONTAL AND CROSSED SEGMENTS FOR PRISMS
!|                |   |    CUT INTO TETRAHEDRA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)             :: NSEG2D,NPLAN,OPT,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: FLOW(*)
!                                        HERE * = NESG2D*NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: FLULIM(NSEG2D)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER ISEG,ISEG3D,IPLAN,OTHERS
!
!-----------------------------------------------------------------------
!
!     LIMITS 3D FLUXES BY COEFFICIENT OF 2D FLUXES
!
!-----------------------------------------------------------------------
!
!     HORIZONTAL SEGMENTS
!
      DO IPLAN=1,NPLAN
        DO ISEG=1,NSEG2D
          ISEG3D=ISEG+(IPLAN-1)*NSEG2D
          FLOW(ISEG3D)=FLOW(ISEG3D)*FLULIM(ISEG)
        ENDDO
      ENDDO
!
!     OPTIONALLY: CROSSED SEGMENTS
!
      IF(OPT.EQ.2) THEN
!
!       FOR PRISMS
!
        OTHERS=NPLAN*NSEG2D+NPOIN2*(NPLAN-1)
        DO IPLAN=1,NPLAN-1
          DO ISEG=1,NSEG2D
            ISEG3D=ISEG+(IPLAN-1)*2*NSEG2D+OTHERS
            FLOW(ISEG3D)=FLOW(ISEG3D)*FLULIM(ISEG)
            ISEG3D=ISEG3D+NSEG2D
            FLOW(ISEG3D)=FLOW(ISEG3D)*FLULIM(ISEG)
          ENDDO
        ENDDO
!
      ELSEIF(OPT.EQ.3) THEN
!
!       FOR PRISMS CUT INTO TETRAHEDRA
!
        OTHERS=NPLAN*NSEG2D+NPOIN2*(NPLAN-1)
        DO IPLAN=1,NPLAN-1
          DO ISEG=1,NSEG2D
            ISEG3D=ISEG+(IPLAN-1)*NSEG2D+OTHERS
            FLOW(ISEG3D)=FLOW(ISEG3D)*FLULIM(ISEG)
          ENDDO
        ENDDO
!
      ELSEIF(OPT.NE.1) THEN
!
        WRITE(LU,*) 'FLUX3DLIM : UNEXPECTED OPT:',OPT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
