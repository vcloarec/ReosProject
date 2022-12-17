!                   ********************
                    SUBROUTINE COMP_IKLE
!                   ********************
!
     &(IKLE,IKLBOR,ELTSEG,NBOR,NELBOR,NULONE,
     & IELM,NELEM,NELMAX,NPOIN,NPTFR,NELEB,NELEBX)
!
!***********************************************************************
! BIEF   V7P0                                    28/03/2014
!***********************************************************************
!
!brief    EXTENDS THE CONNECTIVITY TABLES AND ARRAY NBOR.
!
!history  J-M HERVOUET (LNHE)
!+        20/03/2008
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
!history  J-M HERVOUET (LNHE)
!+        16/05/2012
!+        V6P2
!+   New arguments passed to CPIK13.
!
!history  J-M HERVOUET (LNHE)
!+        28/03/2014
!+        V7P0
!+   Adaoted to new numbering of boundary segments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| SEGMENT NUMBERS OF AN ELEMENT
!| IELM           |-->| TYPE OF ELEMENT
!| IKLBOR         |<->| CONNECTIVITY TABLE FOR BOUNDARY POINTS
!| IKLE           |<->| CONNECTIVITY TABLE FOR ALL POINTS
!| NBOR           |<->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NELBOR         |-->| BOUNDARY ELEMENT THAT CONTAINS SEGMENT K
!| NELEB          |-->| NUMBER OF BOUNDARY SEGMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY SEGMENTS
!| NELEM          |-->| NOMBRE D'ELEMENTS
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE
!| NPTFR          |-->| NUMBER OF (LINEAR) BOUNDARY POINTS
!| NULONE         |-->| LOCAL NUMBER OF K IN ELEMENT NELBOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_COMP_IKLE => COMP_IKLE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,IELM,NPOIN,NPTFR
      INTEGER, INTENT(IN)    :: NELEB,NELEBX
      INTEGER, INTENT(IN)    :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(IN)    :: NELBOR(NELEBX),NULONE(NELEBX)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,*),IKLBOR(NELEBX,*),NBOR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(IELM.EQ.12) THEN
!
        CALL CPIK12(IKLE,NELEM,NELMAX,NPOIN)
!
      ELSEIF(IELM.EQ.13.OR.IELM.EQ.14) THEN
!
        CALL CPIK13(IKLE,IKLBOR,ELTSEG,NBOR,NELBOR,NULONE,
     &              NELEM,NELMAX,NPOIN,NPTFR,NELEB,NELEBX)
!
      ELSE
!
        WRITE(LU,11) IELM
11      FORMAT(1X,'COMP_IKLE: DISCRETIZATION NOT IMPLEMENTED:',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
