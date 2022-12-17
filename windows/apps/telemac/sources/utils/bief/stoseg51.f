!                   *******************
                    SUBROUTINE STOSEG51
!                   *******************
!
     &(IFABOR,NELMAX,IELM,IKLE,NBOR,
     & GLOSEG,MAXSEG,ELTSEG,ORISEG,NELBOR,NULONE,NELMAX2,
     & NELEM2,NPTFR2,NPOIN2,NPLAN,KNOLG,NSEG2D,IKLBOR,NELEB,NELEBX)
!
!***********************************************************************
! BIEF   V7P0                                   25/03/2014
!***********************************************************************
!
!brief    BUILDS THE DATA STRUCTURE FOR EDGE-BASED STORAGE
!+                OF PRISMS CUT INTO TETRAHEDRONS:
!+        GLOSEG, ELTSEG, ORISEG. GLOSEG must be already filled for
!+        horizontal segments of the first layer.
!code
!+       LOCAL NUMBERING OF SEGMENTS CHOSEN HERE IN THE ORIGINAL PRISM
!+
!+       HORIZONTAL
!+
!+       01 : POINT 1 TO 2 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       02 : POINT 2 TO 3 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       03 : POINT 3 TO 1 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       04 : POINT 4 TO 5 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       05 : POINT 5 TO 6 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       06 : POINT 6 TO 4 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+
!+       VERTICAL
!+
!+       07 : POINT 1 TO 4
!+       08 : POINT 2 TO 5
!+       09 : POINT 3 TO 6
!+
!+       CROSSED
!+
!+       10 : POINT 1 TO 5  OR  POINT 2 TO 4
!+       11 : POINT 2 TO 6  OR  POINT 3 TO 5
!+       12 : POINT 3 TO 4  OR  POINT 1 TO 6
!+
!+
!+       LOCAL NUMBERING OF SEGMENTS IN THE TETRAHEDRON (SEE ISEGT)
!+
!+       01 : POINT 1 TO 2 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       02 : POINT 2 TO 3 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       03 : POINT 3 TO 1 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       04 : POINT 1 TO 4 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       05 : POINT 2 TO 4 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+       06 : POINT 3 TO 4 (OR THE OPPOSITE DEPENDING OF ORISEG)
!+
!+
!
!history  J-M HERVOUET (LNHE)
!+        24/08/2011
!+        V6P2
!+   Copied from STOSEG41 and modified.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/03/2014
!+        V7P0
!+   Boundary segments have now their own numbering, independent of
!+   boundary points numbering.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE.
!| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS.
!| IELM           |-->| 11: TRIANGLES.
!|                |   | 21: QUADRILATERALS.
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLBOR         |-->| CONNECTIVITY OF BOUNDARY SEGMENTS IN 2D
!| IKLE           |-->| CONNECTIVITY TABLE.
!| KNOLG          |-->| GLOBAL NUMBER OF A LOCAL POINT IN PARALLEL
!| MAXSEG         |<--| MAXIMUM NUMBER OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS.
!| NELBOR         |-->| NUMBER OF ELEMENT CONTAINING SEGMENT K OF
!|                |   | THE BOUNDARY.
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS IN 2D.
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS IN 2D.
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR2         |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| NULONE         |-->| LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
!|                |   | ELEMENT.
!|                |   | HERE THE 3D NULONE IS PASSED THOUGH IT IS HERE
!|                |   | USED AS THE 2D ONE. HENCE BOTH MUST BEGIN BY
!|                |   | THE SAME BOUNDARY ELEMENTS OF THE LOWER PLANE.
!| ORISEG         |<--| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_STOSEG51 => STOSEG51
      USE DECLARATIONS_TELEMAC, ONLY : ISEGT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELMAX,NELMAX2,MAXSEG,IELM,NELEBX,NELEB
      INTEGER, INTENT(IN)    :: NELEM2,NPTFR2,NPOIN2,NPLAN,NSEG2D
      INTEGER, INTENT(IN)    :: NBOR(NPTFR2)
      INTEGER, INTENT(IN)    :: IKLBOR(NELEBX,2)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX2,*),IKLE(NELMAX,4)
      INTEGER, INTENT(IN)    :: NELBOR(NELEBX),NULONE(NELEBX)
      INTEGER, INTENT(INOUT) :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(INOUT) :: ELTSEG(NELMAX,6),ORISEG(NELMAX,6)
      INTEGER, INTENT(IN)    :: KNOLG(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,J1,J2,I,ISEG,II1,II2,II3
      INTEGER ISEG01,ISEG02,ISEG03,ISEG04,ISEG05,ISEG06
      INTEGER ISEG07,ISEG08,ISEG09,ISEG10,ISEG11,ISEG12
      INTEGER IPLAN,ISEG2D,ISEG3D,IELEM3D,NSEGH,NSEGV
!     THE SIX SEGMENTS IN A TETRAHEDRON
!     ISEGT(ISEG,1 OR 2) : FIRST OR SECOND POINT OF SEGMENT ISEG
!     INTEGER ISEGT(6,2)
!     DATA ISEGT/1,2,3,1,2,3,2,3,1,4,4,4/
!
!-----------------------------------------------------------------------
!
      IF(IELM.NE.51) THEN
        WRITE(LU,501) IELM
501     FORMAT(1X,'STOSEG51 (BIEF): UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BUILDS 2D SEGMENTS (THE FIRST IN THE NUMBERING)
!
      NSEGH=NSEG2D*NPLAN
      NSEGV=(NPLAN-1)*NPOIN2
!
!     BUILDING 2D VALUES (THAT WILL COINCIDE WITH FIRST LAYER OF THE 3D
!     MESH). NOTE THAT NELMAX IS TRANSMITTED, A 3D VALUE, AND IT WILL
!     DIMENSION ELTSEG AND ORISEG AS ELTSEG(NELMAX,3) AND ORISEG(NELMAX,3)
!     IN STOSEG, SO THAT AFTER THE 3D ORISEG WILL BE PARTLY BUT CORRECTLY
!     FILLED. THE SAME IS DONE WITH GLOSEG, WITH DIMENSION MAXSEG
!
!     IKLBOR, NELBOR AND NULONE ARE HERE THE 2D VALUES (SEE INBIEF)
!
      CALL STOSEG(IFABOR,NELEM2,NELMAX,NELMAX2,11,IKLE,NBOR,NPTFR2,
     &            GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG2D,
     &            NELBOR,NULONE,KNOLG,IKLBOR,NELEBX,NELEB)
!
!-----------------------------------------------------------------------
!
!     COMPLETES HORIZONTAL SEGMENTS (1,2,3,4,5,6 OF PRISMS)
!     SAME SEGMENTS AND SAME NUMBERING THAN WITH PRISMS
!
      DO IPLAN=2,NPLAN
      DO ISEG2D=1,NSEG2D
        ISEG3D=ISEG2D+(IPLAN-1)*NSEG2D
        GLOSEG(ISEG3D,1)=GLOSEG(ISEG2D,1)+NPOIN2*(IPLAN-1)
        GLOSEG(ISEG3D,2)=GLOSEG(ISEG2D,2)+NPOIN2*(IPLAN-1)
      ENDDO
      ENDDO
!
!     VERTICAL SEGMENTS (7,8,9 OF PRISMS)
!     SAME SEGMENTS AND SAME NUMBERING THAN WITH PRISMS
!
      DO IPLAN=1,NPLAN-1
      DO I=1,NPOIN2
        ISEG3D=NSEGH+NPOIN2*(IPLAN-1)+I
        GLOSEG(ISEG3D,1)=NPOIN2*(IPLAN-1)+I
        GLOSEG(ISEG3D,2)=NPOIN2*(IPLAN  )+I
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     NOW COMPLETING ELTSEG AND ORISEG (THEY ARE ALREADY CORRECT FOR
!     THE TETRAHEDRONS WITH A SIDE ON THE BOTTOM, WHICH ARE THE FIRST
!     NELEM2 ELEMENTS
!
!     ALSO COMPLETING GLOSEG FOR CROSSED SEGMENTS
!
!     PRINCIPLE : NUMBERS OF SEGMENTS IN THE PRISM ARE KNOWN (ISEG01,...)
!                 THEN WE LOOK AT TETRAHEDRA SEGMENTS AND LOOK FOR THE
!                 CORRESPONDING SEGMENT IN THE PRISM. NOT VERY CONCISE
!                 NOR CLEVER BUT EFFICIENT...
!
!     ARRAY ELTSEG GIVES GLOBAL NUMBERS OF SEGMENTS IN A PRISM
!     ARRAY ORISEG GIVES ORIENTATION OF SEGMENT
!
!     EVERY FORMER PRISM IS EXAMINED
!
      DO IPLAN=1,NPLAN-1
        DO IELEM=1,NELEM2
!
!         THE SIX GLOBAL NUMBERS OF POINTS IN THE PRISM
!         IKLE HAS BEEN BUILT SO THAT IT COINCIDES WITH IKLE2D ON THE
!         FIRST LAYER (SEE CPIKLE3).
!
          II1=IKLE(IELEM,1)
          II2=IKLE(IELEM,2)
          II3=IKLE(IELEM,3)
          I1=II1+(IPLAN-1)*NPOIN2
          I2=II2+(IPLAN-1)*NPOIN2
          I3=II3+(IPLAN-1)*NPOIN2
          I4=I1+NPOIN2
          I5=I2+NPOIN2
          I6=I3+NPOIN2
!
!         GLOBAL NUMBERS OF THE 12 SEGMENTS IN THE PRISM
!         I.E. THE 12 DIFFERENT SEGMENTS FORMED BY TETRAHEDRONS
!
!         HORIZONTAL
!         HERE ELTSEG COMES OUT FROM STOSEG
!         AND HAS A TRIANGLE NUMBERING
          ISEG01=ELTSEG(IELEM,1)+NSEG2D*(IPLAN-1)
          ISEG02=ELTSEG(IELEM,2)+NSEG2D*(IPLAN-1)
          ISEG03=ELTSEG(IELEM,3)+NSEG2D*(IPLAN-1)
          ISEG04=ISEG01+NSEG2D
          ISEG05=ISEG02+NSEG2D
          ISEG06=ISEG03+NSEG2D
!         VERTICAL
          ISEG07=NSEGH+I1
          ISEG08=NSEGH+I2
          ISEG09=NSEGH+I3
!         CROSSED (NUMBERED AS HORIZONTAL LOWER SEGMENTS IN PRISMS)
          ISEG10=NSEGH+NSEGV+ISEG01
          ISEG11=NSEGH+NSEGV+ISEG02
          ISEG12=NSEGH+NSEGV+ISEG03
!
!         EVERY TETRAHEDRON IN THE PRISM
!
          DO I=1,3
!
!           EVERY SEGMENT IN THE PRISM
!
            DO ISEG=1,6
!             SEE NUMBERING OF TETRAHEDRONS IN CPIKLE3
              IELEM3D=3*NELEM2*(IPLAN-1)+(I-1)*NELEM2+IELEM
              J1=IKLE(IELEM3D,ISEGT(ISEG,1))
              J2=IKLE(IELEM3D,ISEGT(ISEG,2))
!             LOWER HORIZONTAL SEGMENTS
              IF((J1.EQ.I1.AND.J2.EQ.I2).OR.
     &           (J1.EQ.I2.AND.J2.EQ.I1)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG01
                IF(J1.EQ.I1) THEN
!                 SAME SEGMENT SAME ORIENTATION THAN IN 2D
                  ORISEG(IELEM3D,ISEG)=ORISEG(IELEM,1)
                ELSE
                  ORISEG(IELEM3D,ISEG)=3-ORISEG(IELEM,1)
                ENDIF
              ELSEIF((J1.EQ.I2.AND.J2.EQ.I3).OR.
     &               (J1.EQ.I3.AND.J2.EQ.I2)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG02
                IF(J1.EQ.I2) THEN
                  ORISEG(IELEM3D,ISEG)=ORISEG(IELEM,2)
                ELSE
                  ORISEG(IELEM3D,ISEG)=3-ORISEG(IELEM,2)
                ENDIF
              ELSEIF((J1.EQ.I3.AND.J2.EQ.I1).OR.
     &               (J1.EQ.I1.AND.J2.EQ.I3)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG03
                IF(J1.EQ.I3) THEN
                  ORISEG(IELEM3D,ISEG)=ORISEG(IELEM,3)
                ELSE
                  ORISEG(IELEM3D,ISEG)=3-ORISEG(IELEM,3)
                ENDIF
!             UPPER HORIZONTAL SEGMENTS
              ELSEIF((J1.EQ.I4.AND.J2.EQ.I5).OR.
     &               (J1.EQ.I5.AND.J2.EQ.I4)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG04
                IF(J1.EQ.I4) THEN
                  ORISEG(IELEM3D,ISEG)=ORISEG(IELEM,1)
                ELSE
                  ORISEG(IELEM3D,ISEG)=3-ORISEG(IELEM,1)
                ENDIF
              ELSEIF((J1.EQ.I5.AND.J2.EQ.I6).OR.
     &               (J1.EQ.I6.AND.J2.EQ.I5)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG05
                IF(J1.EQ.I5) THEN
                  ORISEG(IELEM3D,ISEG)=ORISEG(IELEM,2)
                ELSE
                  ORISEG(IELEM3D,ISEG)=3-ORISEG(IELEM,2)
                ENDIF
              ELSEIF((J1.EQ.I6.AND.J2.EQ.I4).OR.
     &               (J1.EQ.I4.AND.J2.EQ.I6)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG06
                IF(J1.EQ.I6) THEN
                  ORISEG(IELEM3D,ISEG)=ORISEG(IELEM,3)
                ELSE
                  ORISEG(IELEM3D,ISEG)=3-ORISEG(IELEM,3)
                ENDIF
!             VERTICAL SEGMENTS
              ELSEIF((J1.EQ.I1.AND.J2.EQ.I4).OR.
     &               (J1.EQ.I4.AND.J2.EQ.I1)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG07
                IF(J1.EQ.I1) THEN
                  ORISEG(IELEM3D,ISEG)=1
                ELSE
                  ORISEG(IELEM3D,ISEG)=2
                ENDIF
              ELSEIF((J1.EQ.I2.AND.J2.EQ.I5).OR.
     &               (J1.EQ.I5.AND.J2.EQ.I2)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG08
                IF(J1.EQ.I2) THEN
                  ORISEG(IELEM3D,ISEG)=1
                ELSE
                  ORISEG(IELEM3D,ISEG)=2
                ENDIF
              ELSEIF((J1.EQ.I3.AND.J2.EQ.I6).OR.
     &               (J1.EQ.I6.AND.J2.EQ.I3)) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG09
                IF(J1.EQ.I3) THEN
                  ORISEG(IELEM3D,ISEG)=1
                ELSE
                  ORISEG(IELEM3D,ISEG)=2
                ENDIF
!             CROSSED SEGMENTS
              ELSEIF((J1.EQ.I1.AND.J2.EQ.I5).OR.
     &               (J1.EQ.I5.AND.J2.EQ.I1).OR.
     &               (J1.EQ.I2.AND.J2.EQ.I4).OR.
     &               (J1.EQ.I4.AND.J2.EQ.I2)    ) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG10
                IF(J1.EQ.I1.OR.J1.EQ.I2) THEN
                  ORISEG(IELEM3D,ISEG)=1
                  GLOSEG(ISEG10,1)=J1
                  GLOSEG(ISEG10,2)=J2
                ELSE
                  ORISEG(IELEM3D,ISEG)=2
                  GLOSEG(ISEG10,1)=J2
                  GLOSEG(ISEG10,2)=J1
                ENDIF
              ELSEIF((J1.EQ.I2.AND.J2.EQ.I6).OR.
     &               (J1.EQ.I6.AND.J2.EQ.I2).OR.
     &               (J1.EQ.I3.AND.J2.EQ.I5).OR.
     &               (J1.EQ.I5.AND.J2.EQ.I3)    ) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG11
                IF(J1.EQ.I2.OR.J1.EQ.I3) THEN
                  ORISEG(IELEM3D,ISEG)=1
                  GLOSEG(ISEG11,1)=J1
                  GLOSEG(ISEG11,2)=J2
                ELSE
                  ORISEG(IELEM3D,ISEG)=2
                  GLOSEG(ISEG11,1)=J2
                  GLOSEG(ISEG11,2)=J1
                ENDIF
              ELSEIF((J1.EQ.I3.AND.J2.EQ.I4).OR.
     &               (J1.EQ.I4.AND.J2.EQ.I3).OR.
     &               (J1.EQ.I1.AND.J2.EQ.I6).OR.
     &               (J1.EQ.I6.AND.J2.EQ.I1)    ) THEN
                ELTSEG(IELEM3D,ISEG)=ISEG12
                IF(J1.EQ.I3.OR.J1.EQ.I1) THEN
                  ORISEG(IELEM3D,ISEG)=1
                  GLOSEG(ISEG12,1)=J1
                  GLOSEG(ISEG12,2)=J2
                ELSE
                  ORISEG(IELEM3D,ISEG)=2
                  GLOSEG(ISEG12,1)=J2
                  GLOSEG(ISEG12,2)=J1
                ENDIF
              ELSE
                WRITE(LU,*) 'PROBLEM IN STOSEG51'
                WRITE(LU,*) 'FOR IPLAN=',IPLAN,' IELEM=',IELEM
                WRITE(LU,*) 'I=',I,' ISEG=',ISEG,' IELEM3D=',IELEM3D
                WRITE(LU,*) 'J1=',J1,' J2=',J2
                WRITE(LU,*) 'I1=',I1,' I2=',I2,' I3=',I3
                WRITE(LU,*) 'I4=',I4,' I5=',I5,' I6=',I6
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
!
          ENDDO
!
!         END OF LOOP ON THE 3 TETRAHEDRONS
!
        ENDDO
      ENDDO
!
!     CHECKING
!
!     LOOP ON TETRAHEDRA
      DO IELEM3D=1,3*NELEM2*(NPLAN-1)
!     LOOP ON LOCAL SEGMENTS
        DO I=1,6
!         GLOBAL POINTS SEEN BY TETRAHEDRON
          J1=IKLE(IELEM3D,ISEGT(I,1))
          J2=IKLE(IELEM3D,ISEGT(I,2))
!         GLOBAL POINTS SEEN BY GLOSEG
          ISEG=ELTSEG(IELEM3D,I)
          I1=GLOSEG(ISEG,1)
          I2=GLOSEG(ISEG,2)
          IF(ORISEG(IELEM3D,I).EQ.1) THEN
            IF(J1.NE.I1.OR.J2.NE.I2) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'ERROR IN STOSEG51'
              WRITE(LU,*) 'ELEMENT ',IELEM3D,' SEGMENT ',I
              WRITE(LU,*) 'POINTS ',J1,J2
              WRITE(LU,*) 'GLOBAL SEGMENT ',ISEG
              WRITE(LU,*) 'POINTS ',I1,I2
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSE
            IF(J1.NE.I2.OR.J2.NE.I1) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'ERROR IN STOSEG51'
              WRITE(LU,*) 'ELEMENT ',IELEM3D,' SEGMENT ',I
              WRITE(LU,*) 'POINTS ',J1,J2
              WRITE(LU,*) 'GLOBAL SEGMENT ',ISEG
              WRITE(LU,*) 'POINTS ',I1,I2
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
