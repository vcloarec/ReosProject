!                   *****************
                    SUBROUTINE CPIK13
!                   *****************
!
     &(IKLE,IKLBOR,ELTSEG,NBOR,NELBOR,NULONE,NELEM,NELMAX,NPOIN,NPTFR,
     & NELEB,NELEBX)
!
!***********************************************************************
! BIEF   V7P0                                     28/03/2014
!***********************************************************************
!
!brief    EXTENDS THE CONNECTIVITY TABLE.
!+                CASE OF AN EXTENSION TO QUADRATIC ELEMENTS.
!
!history  J-M HERVOUET (LNH)
!+        20/03/08
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
!+   Bug corrected in parallel
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/03/2014
!+        V7P0
!+   Adapted to new numbering of boundary segments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| SEGMENTS NUMBERS IN EVERY ELEMENT
!| IKLBOR         |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IKLE           |<->| CONNECTIVITY TABLE
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NELBOR         |-->| BOUNDARY ELEMENT THAT CONTAINS SEGMENT K
!| NELEB          |-->| NUMBER OF BOUNDARY SEGMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY SEGMENTS
!| NULONE         |-->| LOCAL NUMBER OF K IN ELEMENT NELBOR
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY : NCSIZE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM,NELMAX,NPOIN,NPTFR,NELEB,NELEBX
      INTEGER, INTENT(IN)    :: ELTSEG(NELMAX,3)
      INTEGER, INTENT(IN)    :: NELBOR(NELEBX),NULONE(NELEBX)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,6),IKLBOR(NELEBX,3),NBOR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,K,IELEB
!
!-----------------------------------------------------------------------
!
!     CONNECTIVITY TABLE OF QUADRATIC GLOBAL POINTS
!
      DO IELEM = 1 , NELEM
!
!       NUMBER=NPOIN+NUMBER OF THE SEGMENT CONTAINING THE POINT
!
        IKLE(IELEM,4) = NPOIN + ELTSEG(IELEM,1)
        IKLE(IELEM,5) = NPOIN + ELTSEG(IELEM,2)
        IKLE(IELEM,6) = NPOIN + ELTSEG(IELEM,3)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CONNECTIVITY TABLE OF QUADRATIC BOUNDARY POINTS
!     GLOBAL NUMBERS OF BOUNDARY QUADRATIC POINTS
!
      DO IELEB=1,NELEB
        K=IKLBOR(IELEB,1)
!       WE DECIDE HERE THE BOUNDARY NUMBERING OF QUADRATIC POINTS
        IKLBOR(IELEB,3)=K+NPTFR
        IELEM=NELBOR(IELEB)
        NBOR(IKLBOR(IELEB,3))=IKLE(IELEM,NULONE(IELEB)+3)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     SECURITY CHECK IN SCALAR MODE
!
      IF(NCSIZE.LE.1) THEN
        DO IELEB=1,NELEB
          K=IKLBOR(IELEB,1)
          IF(NBOR(IKLBOR(IELEB,3)).NE.K+NPOIN) THEN
            WRITE(LU,*) 'CPIK13: PROBLEM OF NUMBERING FOR K=',K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
