!                   *****************
                    SUBROUTINE ELEB3D
!                   *****************
!
     &(IKLE3,NBOR,NELBOR,IKLBOR,NELEB,NELEBX,NULONE,
     & NELEM2,NPOIN2,NPLAN,NETAGE,NPTFR)
!
!***********************************************************************
! BIEF   V7P0                                   19/03/2014
!***********************************************************************
!
!brief    BUILDS THE 3D MESH.
!+
!+            INPUT: 3D MESH ARRAYS FILLED BY A PRELIMINARY CALL
!+                       TO ELEBD.
!+
!+            OUTPUT: ARRAYS COMPLETE IN 3D.
!
!history  J-M HERVOUET (LNHE)
!+        23/06/2008
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBOR         |-->| CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!| IKLE3          |<--| CONNECTIVITY TABLE IN 3D
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS IN 2D
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!|                |   | AT THE BEGINNING : IN 2D, AT THE EXIT, IN 3D
!|                |   | NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!|                |   | USED AS FIRST DIMENSION OF IKLBOR
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!|                |   | !!!!!  HERE IN 3D !!!!!!
!|                |   | WITH PRISMS, A CONDITION IS THAT THE FIRST
!|                |   | NPTFR VALUES OF MESH3D%NULONE ARE EQUAL
!|                |   | TO MESH2D%NULONE (SEE CALL TO STOSEG41 IN INBIEF)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ELEB3D => ELEB3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM2,NPOIN2,NPLAN,NETAGE,NPTFR
      INTEGER, INTENT(IN)    :: NELEBX
      INTEGER, INTENT(INOUT) :: NELEB
      INTEGER, INTENT(INOUT) :: IKLE3(NELEM2,NETAGE,6)
      INTEGER, INTENT(INOUT) :: IKLBOR(NELEBX,4),NULONE(NELEBX,4)
      INTEGER, INTENT(INOUT) :: NELBOR(NELEBX),NBOR(NPTFR*NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IPOIN,IELEB2,IELEB3,K1,K2,IETAGE,IPTFR,I1,I2,I3
      INTEGER IPLAN
!
!-----------------------------------------------------------------------
!
!     CONNECTIVITY TABLES FOR BOUNDARY FACES --> IKLBOR , NBOR ,
!     CORRESPONDENCE BETWEEN LOCAL BOUNDARY NUMBERS
!     AND 3D LOCAL NUMBERS --> NULONE
!
!     LATERAL BOUNDARIES: BUILDING NBOR
!
      DO IPLAN=2,NPLAN
        DO IPTFR=1,NPTFR
          NBOR(IPTFR+(IPLAN-1)*NPTFR)=NBOR(IPTFR)+(IPLAN-1)*NPOIN2
        ENDDO
      ENDDO
!
!     LATERAL BOUNDARIES
!
      DO IETAGE = 1,NETAGE
        DO IELEB2=1,NELEB
          IELEB3=IELEB2+(IETAGE-1)*NELEB
          K1=IKLBOR(IELEB2,1)
          K2=IKLBOR(IELEB2,2)
          IKLBOR(IELEB3,1) = K1 + (IETAGE-1)*NPTFR
          IKLBOR(IELEB3,2) = K2 + (IETAGE-1)*NPTFR
          IKLBOR(IELEB3,3) = IKLBOR(IELEB3,2) + NPTFR
          IKLBOR(IELEB3,4) = IKLBOR(IELEB3,1) + NPTFR
          IELEM = NELBOR(IELEB2)
          IPOIN = NBOR(K1)
          NELBOR(IELEB3)=IELEM+(IETAGE-1)*NELEM2
          I1=IKLE3(IELEM,1,1)
          I2=IKLE3(IELEM,1,2)
          I3=IKLE3(IELEM,1,3)
          IF(IPOIN.EQ.I1) THEN
            NULONE(IELEB3,1) = 1
            NULONE(IELEB3,2) = 2
            NULONE(IELEB3,3) = 5
            NULONE(IELEB3,4) = 4
          ELSEIF(IPOIN.EQ.I2) THEN
            NULONE(IELEB3,1) = 2
            NULONE(IELEB3,2) = 3
            NULONE(IELEB3,3) = 6
            NULONE(IELEB3,4) = 5
          ELSEIF(IPOIN.EQ.I3) THEN
            NULONE(IELEB3,1) = 3
            NULONE(IELEB3,2) = 1
            NULONE(IELEB3,3) = 4
            NULONE(IELEB3,4) = 6
          ELSE
            WRITE(LU,102) IPOIN,I1,I2,I3,K1,IELEM
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     NOW NELEB BECOMES THE 3D VALUE
!
      NELEB=NELEB*NETAGE
!
!-----------------------------------------------------------------------
!
102   FORMAT(' ELEB3D: PROBLEM WHEN BUILDING NULONE, IPOIN =',
     &  I6,'   I1,2,3=',I6,1X,I6,1X,I6,' K1=',I6,' IELEM=',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
