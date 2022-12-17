!                   *****************
                    SUBROUTINE STOSEG
!                   *****************
!
     &(IFABOR,NELEM,NELMAX,NELMAX2,IELM,IKLE,NBOR,NPTFR,
     & GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG,NELBOR,NULONE,KNOLG,
     & IKLBOR,NELEBX,NELEB)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE DATA STRUCTURE FOR EDGE-BASED STORAGE.
!
!history  J-M HERVOUET (LNH)
!+        02/10/08
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
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE.
!| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS.
!| IELM           |-->| 11: TRIANGLES.
!|                |   | 21: QUADRILATERALS.
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| KNOLG          |-->| GLOBAL NUMBER OF A LOCAL POINT IN PARALLEL
!| MAXSEG         |<--| MAXIMUM NUMBER OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS.
!| NELBOR         |-->| NUMBER OF ELEMENT CONTAINING SEGMENT K OF
!|                |   | THE BOUNDARY.
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS.
!| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH.
!| NULONE         |-->| LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
!|                |   | ELEMENT.
!| ORISEG         |<--| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_STOSEG => STOSEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELMAX,NELMAX2,NPTFR,NSEG,MAXSEG,IELM
      INTEGER, INTENT(IN)    :: NELEM,NELEBX,NELEB
      INTEGER, INTENT(IN)    :: NBOR(NPTFR)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX2,*),IKLE(NELMAX,*)
      INTEGER, INTENT(IN)    :: NELBOR(*),NULONE(*)
      INTEGER, INTENT(IN)    :: IKLBOR(NELEBX,2)
      INTEGER, INTENT(INOUT) :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(INOUT) :: ELTSEG(NELMAX,*),ORISEG(NELMAX,3)
      INTEGER, INTENT(IN)    :: KNOLG(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSE,IELEB
!
      INTEGER NEL,IFA,I1,I2,J1,J2,IFACE,JFACE,IG1,IG2
      INTEGER IELEM,IELEM1,IELEM2
!
      INTEGER :: NEXT(3)
      PARAMETER ( NEXT = (/ 2,3,1 /) )
!
!-----------------------------------------------------------------------
!
      IF(IELM.NE.11.AND.IELM.NE.12.AND.IELM.NE.13.AND.IELM.NE.14) THEN
        WRITE(LU,501) IELM
501     FORMAT(1X,'STOSEG (BIEF) : UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     INITIALISES ELTSEG
!
      DO IELEM = 1 , NELEM
        ELTSEG(IELEM,1) = 0
        ELTSEG(IELEM,2) = 0
        ELTSEG(IELEM,3) = 0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP ON BOUNDARY POINTS :
!
      DO IELEB = 1 , NELEB
!
!         NOTE: ON BOUNDARIES, SEGMENTS ARE NOT ORIENTED LOWER RANK
!               TO HIGHER RANK, AS IS DONE FOR INTERNAL SEGMENTS
          GLOSEG(IELEB,1) = NBOR(IKLBOR(IELEB,1))
          GLOSEG(IELEB,2) = NBOR(IKLBOR(IELEB,2))
          NEL = NELBOR(IELEB)
          IFA = NULONE(IELEB)
          ELTSEG(NEL,IFA) = IELEB
          ORISEG(NEL,IFA) = 1
!
      ENDDO
!
      NSE=NELEB
!
!-----------------------------------------------------------------------
!
!     LOOP ON ELEMENTS FOR NUMBERING INTERNAL SEGMENTS AND FILLING:
!     GLOSEG, ELTSEG, ORISEG
!
      DO IELEM1 = 1 , NELEM
        DO IFACE = 1 , 3
          IF(ELTSEG(IELEM1,IFACE).EQ.0) THEN
!           NEW SEGMENT (HENCE INTERNAL SO IFABOR<>0)
            NSE = NSE + 1
!           BOTH NEIGHBOURING ELEMENTS ARE TREATED FOR THIS SEGMENT
            I1 = IKLE(IELEM1,     IFACE)
            I2 = IKLE(IELEM1,NEXT(IFACE))
            IF(I1.EQ.I2) THEN
              WRITE(LU,*) 'STOSEG: EDGE MADE OF ONLY ONE POINT'
              WRITE(LU,*) '        ELEMENT ',IELEM1,' FACE ',IFACE
              CALL PLANTE(1)
              STOP
            ENDIF
            ELTSEG(IELEM1,IFACE) = NSE
            IF(NCSIZE.GT.1) THEN
              IG1=KNOLG(I1)
              IG2=KNOLG(I2)
            ELSE
              IG1=I1
              IG2=I2
            ENDIF
!           SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
            IF(IG1.LT.IG2) THEN
              GLOSEG(NSE,1) = I1
              GLOSEG(NSE,2) = I2
              ORISEG(IELEM1,IFACE) = 1
            ELSE
              GLOSEG(NSE,1) = I2
              GLOSEG(NSE,2) = I1
              ORISEG(IELEM1,IFACE) = 2
            ENDIF
!           OTHER ELEMENT NEIGHBOURING THIS SEGMENT
            IELEM2 = IFABOR(IELEM1,IFACE)
!           IELEM2 = 0 OR -1 MAY OCCUR IN PARALLEL MODE
            IF(IELEM2.GT.0) THEN
!             LOOKS FOR THE RIGHT SIDE OF ELEMENT IELEM2
              DO JFACE = 1,3
                J1 = IKLE(IELEM2,     JFACE)
                J2 = IKLE(IELEM2,NEXT(JFACE))
!               ALL ELEMENTS HAVE A COUNTER-CLOCKWISE NUMBERING
                IF(I1.EQ.J2.AND.I2.EQ.J1) THEN
                  ELTSEG(IELEM2,JFACE) = NSE
                  ORISEG(IELEM2,JFACE) = 3-ORISEG(IELEM1,IFACE)
!                 SIDE FOUND, NO NEED TO GO ON
                  GO TO 1000
                ELSEIF(I1.EQ.J1.AND.I2.EQ.J2) THEN
!                 SIDE BADLY ORIENTED
                  WRITE(LU,*) 'STOSEG: WRONG MESH'
                  WRITE(LU,*) '        FACE ',JFACE
                  WRITE(LU,*) '        OF ELEMENT ',IELEM2
                  WRITE(LU,*) '        IS NOT WELL ORIENTED'
                  WRITE(LU,*) '         (POINTS ',I1,' AND ',I2,')'
                  CALL PLANTE(1)
                  STOP
                ENDIF
              ENDDO
!             SIDE NOT FOUND, THIS IS AN ERROR
              WRITE(LU,*) 'STOSEG: WRONG MESH'
              WRITE(LU,*) '        ELEMENTS ',IELEM1,' AND ',IELEM2
              WRITE(LU,*) '        LINKED BY POINTS ',I1,' AND ',I2
              WRITE(LU,*) '        BUT THESE POINTS ARE NOT AN EDGE'
              WRITE(LU,*) '        OF ELEMENT ',IELEM2
              CALL PLANTE(1)
              STOP
            ENDIF
1000        CONTINUE
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     CHECKS
!
      IF(NSEG.NE.NSE) THEN
        WRITE(LU,503) NSE,NSEG
503     FORMAT(1X,'STOSEG (BIEF): WRONG NUMBER OF SEGMENTS : ',1I6,
     &            '               INSTEAD OF ',1I6,' EXPECTED')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
