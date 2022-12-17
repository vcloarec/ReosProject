!                   *******************
                    SUBROUTINE COMP_SEG
!                   *******************
!
     &(NELEM,NELMAX,IELM,IKLE,GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPLETES THE DATA STRUCTURE FOR EDGE-BASED STORAGE
!+                FOR HIGHER ORDER ELEMENTS.
!code
!+    NUMBERING OF QUADRATIC ELEMENTS SEGMENTS:
!+
!+    01 --> 1 - 2
!+    02 --> 2 - 3
!+    03 --> 3 - 1
!+    04 --> 1 - 4
!+    05 --> 2 - 5
!+    06 --> 3 - 6
!+    07 --> 2 - 4
!+    08 --> 3 - 5
!+    09 --> 1 - 6
!+    10 --> 1 - 5
!+    11 --> 2 - 6
!+    12 --> 3 - 4
!+    13 --> 4 - 5
!+    14 --> 5 - 6
!+    15 --> 6 - 4
!
!history  J-M HERVOUET (LNHE)
!+        05/02/2010
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
!| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE.
!| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS.
!| IELM           |-->| 11: TRIANGLES.
!|                |   | 21: QUADRILATERES.
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!| MAXSEG         |<--| 1st DIMENSION OF MAXSEG.
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!|                |   | (CAS DES MAILLAGES ADAPTATIFS)
!| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH.
!| ORISEG         |-->| ORIENTATION OF SEGMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_COMP_SEG => COMP_SEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELMAX,NSEG,MAXSEG,IELM,NELEM
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,*)
      INTEGER, INTENT(INOUT) :: GLOSEG(MAXSEG,2),ELTSEG(NELMAX,*)
      INTEGER, INTENT(INOUT) :: ORISEG(NELMAX,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IAD
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.12) THEN
!
!       3 ADDITIONAL SEGMENTS WITHIN QUASI-BUBBLE ELEMENTS
!       FOR THEM ORISEG IS IMPLICITLY 1 AND IS NEVER USED
!
        DO IELEM = 1 , NELEM
          ELTSEG(IELEM,4) = NSEG + 3*(IELEM-1) + 1
          ELTSEG(IELEM,5) = NSEG + 3*(IELEM-1) + 2
          ELTSEG(IELEM,6) = NSEG + 3*(IELEM-1) + 3
!         PRINCIPLE: FROM LINEAR POINT TO QUASI-BUBBLE POINT
          GLOSEG(ELTSEG(IELEM,4),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,4),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,5),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,5),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,6),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,6),2) = IKLE(IELEM,4)
        ENDDO
!
      ELSEIF(IELM.EQ.13) THEN
!
!       12 ADDITIONAL SEGMENTS IN QUADRATIC ELEMENTS
!       SEE GLOSEG BELOW
!
        DO IELEM = 1 , NELEM
!         6 SMALL LATERAL SEGMENTS (NUMBERED LIKE THEIR LARGER
!         SEGMENT WITH A SHIFT, AND SO THAT NUMBERS CORRESPOND
!         ON EITHER SIDE)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            ELTSEG(IELEM,04)=NSEG+ELTSEG(IELEM,01)
            ELTSEG(IELEM,07)=NSEG+ELTSEG(IELEM,01)+NSEG
          ELSE
            ELTSEG(IELEM,04)=NSEG+ELTSEG(IELEM,01)+NSEG
            ELTSEG(IELEM,07)=NSEG+ELTSEG(IELEM,01)
          ENDIF
          IF(ORISEG(IELEM,2).EQ.1) THEN
            ELTSEG(IELEM,05)=NSEG+ELTSEG(IELEM,02)
            ELTSEG(IELEM,08)=NSEG+ELTSEG(IELEM,02)+NSEG
          ELSE
            ELTSEG(IELEM,05)=NSEG+ELTSEG(IELEM,02)+NSEG
            ELTSEG(IELEM,08)=NSEG+ELTSEG(IELEM,02)
          ENDIF
          IF(ORISEG(IELEM,3).EQ.1) THEN
            ELTSEG(IELEM,06)=NSEG+ELTSEG(IELEM,03)
            ELTSEG(IELEM,09)=NSEG+ELTSEG(IELEM,03)+NSEG
          ELSE
            ELTSEG(IELEM,06)=NSEG+ELTSEG(IELEM,03)+NSEG
            ELTSEG(IELEM,09)=NSEG+ELTSEG(IELEM,03)
          ENDIF
        ENDDO
        IAD=3*NSEG
        DO IELEM = 1 , NELEM
!         THE 3 LARGE SEGMENTS INSIDE THE ELEMENT
          ELTSEG(IELEM,10) = IAD + 3*(IELEM-1) + 1
          ELTSEG(IELEM,11) = IAD + 3*(IELEM-1) + 2
          ELTSEG(IELEM,12) = IAD + 3*(IELEM-1) + 3
        ENDDO
        IAD=IAD+3*NELEM
        DO IELEM = 1 , NELEM
!         THE 3 SMALL SEGMENTS INSIDE THE ELEMENT
          ELTSEG(IELEM,13) = IAD + 3*(IELEM-1) + 1
          ELTSEG(IELEM,14) = IAD + 3*(IELEM-1) + 2
          ELTSEG(IELEM,15) = IAD + 3*(IELEM-1) + 3
        ENDDO
        IAD=IAD+3*NELEM
!
        IF(IAD.NE.MAXSEG) THEN
          WRITE(LU,*) 'COMP_SEG: ERROR ON MAXIMUM NUMBER OF SEGMENTS'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO IELEM = 1 , NELEM
!         FOR SEGMENTS 4 TO 12: FROM LINEAR POINT TO QUADRATIC POINT
!         THIS IS IMPORTANT FOR RECTANGULAR MATRICES AND MVSEG
          GLOSEG(ELTSEG(IELEM,04),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,04),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,05),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,05),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,06),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,06),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,07),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,07),2) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,08),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,08),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,09),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,09),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,10),1) = IKLE(IELEM,1)
          GLOSEG(ELTSEG(IELEM,10),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,11),1) = IKLE(IELEM,2)
          GLOSEG(ELTSEG(IELEM,11),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,12),1) = IKLE(IELEM,3)
          GLOSEG(ELTSEG(IELEM,12),2) = IKLE(IELEM,4)
!         FOR SEGMENTS 13 TO 15: NO SPECIFIC PRINCIPLE
          GLOSEG(ELTSEG(IELEM,13),1) = IKLE(IELEM,4)
          GLOSEG(ELTSEG(IELEM,13),2) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,14),1) = IKLE(IELEM,5)
          GLOSEG(ELTSEG(IELEM,14),2) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,15),1) = IKLE(IELEM,6)
          GLOSEG(ELTSEG(IELEM,15),2) = IKLE(IELEM,4)
!         SHOULD NOT BE USEFUL (MEMORY TO BE REMOVED ?)
!         ORIENTATION FROM LINEAR TO QUADRATIC NEEDS NO
!         EXTRA INFORMATION
          ORISEG(IELEM,04) = 1
          ORISEG(IELEM,05) = 1
          ORISEG(IELEM,06) = 1
          ORISEG(IELEM,07) = 1
          ORISEG(IELEM,08) = 1
          ORISEG(IELEM,09) = 1
          ORISEG(IELEM,10) = 1
          ORISEG(IELEM,11) = 1
          ORISEG(IELEM,12) = 1
          ORISEG(IELEM,13) = 1
          ORISEG(IELEM,14) = 1
          ORISEG(IELEM,15) = 1
        ENDDO
      ELSE
        WRITE(LU,501) IELM
501     FORMAT(1X,'COMP_SEG (BIEF): UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
