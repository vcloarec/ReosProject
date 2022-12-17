!                   *********************
                    SUBROUTINE AS3_1313_Q
!                   *********************
!
     &(XM,NSEG1,XMT,DIM1XMT,DIM2XMT,STOXMT,NELMAX,NELEM,ELTSEG,ORISEG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF QUADRATIC TRIANGLE AND NON SYMMETRICAL MATRIX.
!code
!+            LOCAL NUMBERING OF SEGMENTS IN A TRIANGLE
!+
!+            01 --> 1 - 2
!+            02 --> 2 - 3
!+            03 --> 3 - 1
!+            04 --> 1 - 4
!+            05 --> 2 - 5
!+            06 --> 3 - 6
!+            07 --> 2 - 4
!+            08 --> 3 - 5
!+            09 --> 1 - 6
!+            10 --> 1 - 5
!+            11 --> 2 - 6
!+            12 --> 3 - 4
!+            13 --> 4 - 5
!+            14 --> 5 - 6
!+            15 --> 6 - 4
!+
!+            LOCAL NUMBERING OF ELEMENT BY ELEMENT EXTRA-DIAGONAL TERMS
!+
!+            01 : POINTS 1-2  16 : POINTS 2-1
!+            02 : POINTS 1-3  17 : POINTS 3-1
!+            03 : POINTS 1-4  18 : POINTS 4-1
!+            04 : POINTS 1-5  19 : POINTS 5-1
!+            05 : POINTS 1-6  20 : POINTS 6-1
!+            06 : POINTS 2-3  21 : POINTS 3-2
!+            07 : POINTS 2-4  22 : POINTS 4-2
!+            08 : POINTS 2-5  23 : POINTS 5-2
!+            09 : POINTS 2-6  24 : POINTS 6-2
!+            10 : POINTS 3-4  25 : POINTS 4-3
!+            11 : POINTS 3-5  26 : POINTS 5-3
!+            12 : POINTS 3-6  27 : POINTS 6-3
!+            13 : POINTS 4-5  28 : POINTS 5-4
!+            14 : POINTS 4-6  29 : POINTS 6-4
!+            15 : POINTS 5-6  30 : POINTS 6-5
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG1
      INTEGER         , INTENT(IN)    :: DIM1XMT,DIM2XMT,STOXMT
      INTEGER         , INTENT(IN)    :: ELTSEG(NELMAX,15)
      INTEGER         , INTENT(IN)    :: ORISEG(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: XMT(DIM1XMT,DIM2XMT)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG1,2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!     INITIALISES
!
      DO ISEG = 1 , NSEG1
        XM(ISEG,1) = 0.D0
        XM(ISEG,2) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(STOXMT.EQ.1) THEN
!
!     ASSEMBLES
!
      DO IELEM = 1,NELEM
!
!         SEGMENT 01 (TERMS 1-2 AND 2-1)
          XM(ELTSEG(IELEM,01),ORISEG(IELEM,01))
     &  = XM(ELTSEG(IELEM,01),ORISEG(IELEM,01))   + XMT(IELEM,01)
          XM(ELTSEG(IELEM,01),3-ORISEG(IELEM,01))
     &  = XM(ELTSEG(IELEM,01),3-ORISEG(IELEM,01)) + XMT(IELEM,16)
!
!         SEGMENT 02 (TERMS 2-3 AND 3-2)
          XM(ELTSEG(IELEM,02),ORISEG(IELEM,02))
     &  = XM(ELTSEG(IELEM,02),ORISEG(IELEM,02))   + XMT(IELEM,06)
          XM(ELTSEG(IELEM,02),3-ORISEG(IELEM,02))
     &  = XM(ELTSEG(IELEM,02),3-ORISEG(IELEM,02)) + XMT(IELEM,21)
!
!         SEGMENT 03 (TERMS 3-1 AND 1-3)
          XM(ELTSEG(IELEM,03),ORISEG(IELEM,03))
     &  = XM(ELTSEG(IELEM,03),ORISEG(IELEM,03))   + XMT(IELEM,17)
          XM(ELTSEG(IELEM,03),3-ORISEG(IELEM,03))
     &  = XM(ELTSEG(IELEM,03),3-ORISEG(IELEM,03)) + XMT(IELEM,02)
!
!         SEGMENT 04 (TERMS 1-4 AND 4-1)
          XM(ELTSEG(IELEM,04),1)=XM(ELTSEG(IELEM,04),1)+XMT(IELEM,03)
          XM(ELTSEG(IELEM,04),2)=XM(ELTSEG(IELEM,04),2)+XMT(IELEM,18)
!
!         SEGMENT 05 (TERMS 2-5 AND 5-2)
          XM(ELTSEG(IELEM,05),1)=XM(ELTSEG(IELEM,05),1)+XMT(IELEM,08)
          XM(ELTSEG(IELEM,05),2)=XM(ELTSEG(IELEM,05),2)+XMT(IELEM,23)
!
!         SEGMENT 06 (TERMS 3-6 AND 6-3)
          XM(ELTSEG(IELEM,06),1)=XM(ELTSEG(IELEM,06),1)+XMT(IELEM,12)
          XM(ELTSEG(IELEM,06),2)=XM(ELTSEG(IELEM,06),2)+XMT(IELEM,27)
!
!         SEGMENT 7 (TERMS 2-4 AND 4-2)
          XM(ELTSEG(IELEM,07),1)=XM(ELTSEG(IELEM,07),1)+XMT(IELEM,07)
          XM(ELTSEG(IELEM,07),2)=XM(ELTSEG(IELEM,07),2)+XMT(IELEM,22)
!
!         SEGMENT 8 (TERMS 3-5 AND 5-3)
          XM(ELTSEG(IELEM,08),1)=XM(ELTSEG(IELEM,08),1)+XMT(IELEM,11)
          XM(ELTSEG(IELEM,08),2)=XM(ELTSEG(IELEM,08),2)+XMT(IELEM,26)
!
!         SEGMENT 9 (TERMS 1-6 AND 6-1)
          XM(ELTSEG(IELEM,09),1)=XM(ELTSEG(IELEM,09),1)+XMT(IELEM,05)
          XM(ELTSEG(IELEM,09),2)=XM(ELTSEG(IELEM,09),2)+XMT(IELEM,20)
!
!         SEGMENT 10 (TERMS 1-5 AND 5-1)
          XM(ELTSEG(IELEM,10),1)=XM(ELTSEG(IELEM,10),1)+XMT(IELEM,04)
          XM(ELTSEG(IELEM,10),2)=XM(ELTSEG(IELEM,10),2)+XMT(IELEM,19)
!
!         SEGMENT 11 (TERMS 2-6 AND 6-2)
          XM(ELTSEG(IELEM,11),1)=XM(ELTSEG(IELEM,11),1)+XMT(IELEM,09)
          XM(ELTSEG(IELEM,11),2)=XM(ELTSEG(IELEM,11),2)+XMT(IELEM,24)
!
!         SEGMENT 12 (TERMS 3-4 AND 4-3)
          XM(ELTSEG(IELEM,12),1)=XM(ELTSEG(IELEM,12),1)+XMT(IELEM,10)
          XM(ELTSEG(IELEM,12),2)=XM(ELTSEG(IELEM,12),2)+XMT(IELEM,25)
!
!         SEGMENT 13 (TERMS 4-5 AND 5-4)
          XM(ELTSEG(IELEM,13),1)=XM(ELTSEG(IELEM,13),1)+XMT(IELEM,13)
          XM(ELTSEG(IELEM,13),2)=XM(ELTSEG(IELEM,13),2)+XMT(IELEM,28)
!
!         SEGMENT 14 (TERMS 5-6 AND 6-5)
          XM(ELTSEG(IELEM,14),1)=XM(ELTSEG(IELEM,14),1)+XMT(IELEM,15)
          XM(ELTSEG(IELEM,14),2)=XM(ELTSEG(IELEM,14),2)+XMT(IELEM,30)
!
!         SEGMENT 15 (TERMS 6-4 AND 4-6)
          XM(ELTSEG(IELEM,15),1)=XM(ELTSEG(IELEM,15),1)+XMT(IELEM,29)
          XM(ELTSEG(IELEM,15),2)=XM(ELTSEG(IELEM,15),2)+XMT(IELEM,14)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(STOXMT.EQ.2) THEN
!
!     ASSEMBLES
!
      DO IELEM = 1,NELEM
!
!         SEGMENT 01 (TERMS 1-2 AND 2-1)
          XM(ELTSEG(IELEM,01),ORISEG(IELEM,01))
     &  = XM(ELTSEG(IELEM,01),ORISEG(IELEM,01))   + XMT(01,IELEM)
          XM(ELTSEG(IELEM,01),3-ORISEG(IELEM,01))
     &  = XM(ELTSEG(IELEM,01),3-ORISEG(IELEM,01)) + XMT(16,IELEM)
!
!         SEGMENT 02 (TERMS 2-3 AND 3-2)
          XM(ELTSEG(IELEM,02),ORISEG(IELEM,02))
     &  = XM(ELTSEG(IELEM,02),ORISEG(IELEM,02))   + XMT(06,IELEM)
          XM(ELTSEG(IELEM,02),3-ORISEG(IELEM,02))
     &  = XM(ELTSEG(IELEM,02),3-ORISEG(IELEM,02)) + XMT(21,IELEM)
!
!         SEGMENT 03 (TERMS 3-1 AND 1-3)
          XM(ELTSEG(IELEM,03),ORISEG(IELEM,03))
     &  = XM(ELTSEG(IELEM,03),ORISEG(IELEM,03))   + XMT(17,IELEM)
          XM(ELTSEG(IELEM,03),3-ORISEG(IELEM,03))
     &  = XM(ELTSEG(IELEM,03),3-ORISEG(IELEM,03)) + XMT(02,IELEM)
!
!         SEGMENT 04 (TERMS 1-4 AND 4-1)
          XM(ELTSEG(IELEM,04),1)=XM(ELTSEG(IELEM,04),1)+XMT(03,IELEM)
          XM(ELTSEG(IELEM,04),2)=XM(ELTSEG(IELEM,04),2)+XMT(18,IELEM)
!
!         SEGMENT 05 (TERMS 2-5 AND 5-2)
          XM(ELTSEG(IELEM,05),1)=XM(ELTSEG(IELEM,05),1)+XMT(08,IELEM)
          XM(ELTSEG(IELEM,05),2)=XM(ELTSEG(IELEM,05),2)+XMT(23,IELEM)
!
!         SEGMENT 06 (TERMS 3-6 AND 6-3)
          XM(ELTSEG(IELEM,06),1)=XM(ELTSEG(IELEM,06),1)+XMT(12,IELEM)
          XM(ELTSEG(IELEM,06),2)=XM(ELTSEG(IELEM,06),2)+XMT(27,IELEM)
!
!         SEGMENT 7 (TERMS 2-4 AND 4-2)
          XM(ELTSEG(IELEM,07),1)=XM(ELTSEG(IELEM,07),1)+XMT(07,IELEM)
          XM(ELTSEG(IELEM,07),2)=XM(ELTSEG(IELEM,07),2)+XMT(22,IELEM)
!
!         SEGMENT 8 (TERMS 3-5 AND 5-3)
          XM(ELTSEG(IELEM,08),1)=XM(ELTSEG(IELEM,08),1)+XMT(11,IELEM)
          XM(ELTSEG(IELEM,08),2)=XM(ELTSEG(IELEM,08),2)+XMT(26,IELEM)
!
!         SEGMENT 9 (TERMS 1-6 AND 6-1)
          XM(ELTSEG(IELEM,09),1)=XM(ELTSEG(IELEM,09),1)+XMT(05,IELEM)
          XM(ELTSEG(IELEM,09),2)=XM(ELTSEG(IELEM,09),2)+XMT(20,IELEM)
!
!         SEGMENT 10 (TERMS 1-5 AND 5-1)
          XM(ELTSEG(IELEM,10),1)=XM(ELTSEG(IELEM,10),1)+XMT(04,IELEM)
          XM(ELTSEG(IELEM,10),2)=XM(ELTSEG(IELEM,10),2)+XMT(19,IELEM)
!
!         SEGMENT 11 (TERMS 2-6 AND 6-2)
          XM(ELTSEG(IELEM,11),1)=XM(ELTSEG(IELEM,11),1)+XMT(09,IELEM)
          XM(ELTSEG(IELEM,11),2)=XM(ELTSEG(IELEM,11),2)+XMT(24,IELEM)
!
!         SEGMENT 12 (TERMS 3-4 AND 4-3)
          XM(ELTSEG(IELEM,12),1)=XM(ELTSEG(IELEM,12),1)+XMT(10,IELEM)
          XM(ELTSEG(IELEM,12),2)=XM(ELTSEG(IELEM,12),2)+XMT(25,IELEM)
!
!         SEGMENT 13 (TERMS 4-5 AND 5-4)
          XM(ELTSEG(IELEM,13),1)=XM(ELTSEG(IELEM,13),1)+XMT(13,IELEM)
          XM(ELTSEG(IELEM,13),2)=XM(ELTSEG(IELEM,13),2)+XMT(28,IELEM)
!
!         SEGMENT 14 (TERMS 5-6 AND 6-5)
          XM(ELTSEG(IELEM,14),1)=XM(ELTSEG(IELEM,14),1)+XMT(15,IELEM)
          XM(ELTSEG(IELEM,14),2)=XM(ELTSEG(IELEM,14),2)+XMT(30,IELEM)
!
!         SEGMENT 15 (TERMS 6-4 AND 4-6)
          XM(ELTSEG(IELEM,15),1)=XM(ELTSEG(IELEM,15),1)+XMT(29,IELEM)
          XM(ELTSEG(IELEM,15),2)=XM(ELTSEG(IELEM,15),2)+XMT(14,IELEM)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'AS3_1313_Q: UNKNOWN STORAGE OF XMT : ',STOXMT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
