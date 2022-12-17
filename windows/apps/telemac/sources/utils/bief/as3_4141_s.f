!                   *********************
                    SUBROUTINE AS3_4141_S
!                   *********************
!
     &(XM,NSEG1,XMT,DIM1XMT,DIM2XMT,STOXMT,NELMAX,NELEM,ELTSEG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF LINEAR-LINEAR PRISM
!+                AND SYMMETRICAL MATRIX.
!code
!+            LOCAL NUMBERING OF SEGMENTS CHOSEN HERE IN A PRISM
!+
!+            01 : POINT 1 TO 2
!+            02 : POINT 2 TO 3
!+            03 : POINT 3 TO 1
!+            04 : POINT 4 TO 5
!+            05 : POINT 5 TO 6
!+            06 : POINT 6 TO 4
!+            07 : POINT 1 TO 4
!+            08 : POINT 2 TO 5
!+            09 : POINT 3 TO 6
!+            10 : POINT 1 TO 5
!+            11 : POINT 2 TO 4
!+            12 : POINT 2 TO 6
!+            13 : POINT 3 TO 5
!+            14 : POINT 3 TO 4
!+            15 : POINT 1 TO 6
!+
!+            LOCAL NUMBERING OF ELEMENT BY ELEMENT EXTRA-DIAGONAL TERMS
!+
!+            01 : POINTS 1-2
!+            02 : POINTS 1-3
!+            03 : POINTS 1-4
!+            04 : POINTS 1-5
!+            05 : POINTS 1-6
!+            06 : POINTS 2-3
!+            07 : POINTS 2-4
!+            08 : POINTS 2-5
!+            09 : POINTS 2-6
!+            10 : POINTS 3-4
!+            11 : POINTS 3-5
!+            12 : POINTS 3-6
!+            13 : POINTS 4-5
!+            14 : POINTS 4-6
!+            15 : POINTS 5-6
!
!history  J-M HERVOUET (LNHE)
!+        11/08/09
!+
!+   CROSSED AND VERTICAL SEGMENTS SWAPPED (SEE STOSEG41)
!
!history  JMH
!+        14/10/09
!+        V6P0
!+   DIM1XMT,DIM2XMT,STOXMT ADDED, + CASE STOXMT=2
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
!| DIM1XMT        |-->| FIRST DIMENSION OF XMT
!| DIM2XMT        |-->| SECOND DIMENSION OF XMT
!| ELTSEG         |-->| SEGMENTS OF A TRIANGLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NSEG1          |-->| NUMBER OF SEGMENTS
!| ORISEG         |-->| ORIENTATION OF SEGMENTS
!| STOXMT         |-->| STORAGE OF XMT 1: (NELMAX,*)
!|                |   |                2: (*,NELMAX)
!| XM             |<--| ASSEMBLED OFF-DIAGONAL TERMS XA12,23,31
!| XMT            |-->| ELEMENT BY ELEMENT STORAGE OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_AS3_4141_S => AS3_4141_S
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG1
      INTEGER         , INTENT(IN)    :: DIM1XMT,DIM2XMT,STOXMT
      INTEGER         , INTENT(IN)    :: ELTSEG(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: XMT(DIM1XMT,DIM2XMT)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO ISEG = 1 , NSEG1
        XM(ISEG) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!  ASSEMBLES
!
!-----------------------------------------------------------------------
!
      IF(STOXMT.EQ.1) THEN
!
      DO IELEM = 1,NELEM
        XM(ELTSEG(IELEM,01)) = XM(ELTSEG(IELEM,01)) + XMT(IELEM,01)
        XM(ELTSEG(IELEM,02)) = XM(ELTSEG(IELEM,02)) + XMT(IELEM,06)
        XM(ELTSEG(IELEM,03)) = XM(ELTSEG(IELEM,03)) + XMT(IELEM,02)
        XM(ELTSEG(IELEM,04)) = XM(ELTSEG(IELEM,04)) + XMT(IELEM,13)
        XM(ELTSEG(IELEM,05)) = XM(ELTSEG(IELEM,05)) + XMT(IELEM,15)
        XM(ELTSEG(IELEM,06)) = XM(ELTSEG(IELEM,06)) + XMT(IELEM,14)
        XM(ELTSEG(IELEM,07)) = XM(ELTSEG(IELEM,07)) + XMT(IELEM,03)
        XM(ELTSEG(IELEM,08)) = XM(ELTSEG(IELEM,08)) + XMT(IELEM,08)
        XM(ELTSEG(IELEM,09)) = XM(ELTSEG(IELEM,09)) + XMT(IELEM,12)
        XM(ELTSEG(IELEM,10)) = XM(ELTSEG(IELEM,10)) + XMT(IELEM,04)
        XM(ELTSEG(IELEM,11)) = XM(ELTSEG(IELEM,11)) + XMT(IELEM,07)
        XM(ELTSEG(IELEM,12)) = XM(ELTSEG(IELEM,12)) + XMT(IELEM,09)
        XM(ELTSEG(IELEM,13)) = XM(ELTSEG(IELEM,13)) + XMT(IELEM,11)
        XM(ELTSEG(IELEM,14)) = XM(ELTSEG(IELEM,14)) + XMT(IELEM,10)
        XM(ELTSEG(IELEM,15)) = XM(ELTSEG(IELEM,15)) + XMT(IELEM,05)
      ENDDO
!
      ELSEIF(STOXMT.EQ.2) THEN
!
      DO IELEM = 1,NELEM
        XM(ELTSEG(IELEM,01)) = XM(ELTSEG(IELEM,01)) + XMT(01,IELEM)
        XM(ELTSEG(IELEM,02)) = XM(ELTSEG(IELEM,02)) + XMT(06,IELEM)
        XM(ELTSEG(IELEM,03)) = XM(ELTSEG(IELEM,03)) + XMT(02,IELEM)
        XM(ELTSEG(IELEM,04)) = XM(ELTSEG(IELEM,04)) + XMT(13,IELEM)
        XM(ELTSEG(IELEM,05)) = XM(ELTSEG(IELEM,05)) + XMT(15,IELEM)
        XM(ELTSEG(IELEM,06)) = XM(ELTSEG(IELEM,06)) + XMT(14,IELEM)
        XM(ELTSEG(IELEM,07)) = XM(ELTSEG(IELEM,07)) + XMT(03,IELEM)
        XM(ELTSEG(IELEM,08)) = XM(ELTSEG(IELEM,08)) + XMT(08,IELEM)
        XM(ELTSEG(IELEM,09)) = XM(ELTSEG(IELEM,09)) + XMT(12,IELEM)
        XM(ELTSEG(IELEM,10)) = XM(ELTSEG(IELEM,10)) + XMT(04,IELEM)
        XM(ELTSEG(IELEM,11)) = XM(ELTSEG(IELEM,11)) + XMT(07,IELEM)
        XM(ELTSEG(IELEM,12)) = XM(ELTSEG(IELEM,12)) + XMT(09,IELEM)
        XM(ELTSEG(IELEM,13)) = XM(ELTSEG(IELEM,13)) + XMT(11,IELEM)
        XM(ELTSEG(IELEM,14)) = XM(ELTSEG(IELEM,14)) + XMT(10,IELEM)
        XM(ELTSEG(IELEM,15)) = XM(ELTSEG(IELEM,15)) + XMT(05,IELEM)
      ENDDO
!
      ELSE
        WRITE(LU,*) 'AS3_4141_S: UNKNOWN STORAGE OF XMT : ',STOXMT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
