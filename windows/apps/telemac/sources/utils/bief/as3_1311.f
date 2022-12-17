!                   *******************
                    SUBROUTINE AS3_1311
!                   *******************
!
     &(XM,NSEG11,NSEG13,XMT,NELMAX,NELEM,ELTSEG,ORISEG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES EXTRA-DIAGONAL TERMS OF MATRICES (XMT)
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF LINEAR - QUADRATIC ELEMENT.
!code
!+            LOCAL NUMBERING OF SEGMENTS IN A TRIANGLE (SEE COMP_SEG)
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
!+            TERMS IN XMT (STORAGE GIVEN BY ARRAY CAQ(6,3,2) IN MATRIY):
!+
!+            01  -->  1-2
!+            02  -->  1-3
!+            03  -->  2-1
!+            04  -->  2-3
!+            05  -->  3-1
!+            06  -->  3-2
!+            07  -->  4-1
!+            08  -->  4-2
!+            09  -->  4-3
!+            10  -->  5-1
!+            11  -->  5-2
!+            12  -->  5-3
!+            13  -->  6-1
!+            14  -->  6-2
!+            15  -->  6-3
!
!history  J-M HERVOUET (LNHE)
!+        02/02/2010
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
!| ELTSEG         |-->| SEGMENT OF A TRIANGLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NSEG11         |-->| NUMBER OF LINEAR SEGMENTS
!| NSEG13         |-->| NUMBER OF QUADRATIC SEGMENTS -
!|                |   | THE NUMBER OF PURELY QUADRATIC SEGMENTS
!|                |   | (THEY ARE NOT CONSIDERED IN RECTANGULAR
!|                |   | MATRICES)
!| ORISEG         |-->| ORIENTATION OF SEGMENTS
!| XM             |<--| ASSEMBLED OFF-DIAGONAL TERMS XA12,23,31
!| XMT            |-->| ELEMENT BY ELEMENT STORAGE OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG11,NSEG13
      INTEGER         , INTENT(IN)    :: ELTSEG(NELMAX,15)
      INTEGER         , INTENT(IN)    :: ORISEG(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: XMT(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG11+NSEG13-3*NELEM)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!     INITIALISES
!
!     -6*NELEM : SEGMENTS 10,11,12 NEED NO ASSEMBLY
!                SEGMENTS 13,14,15 ARE NOT CONSIDERED
      DO ISEG = 1 , NSEG11+NSEG13-6*NELEM
        XM(ISEG) = 0.D0
      ENDDO
!
!     ASSEMBLES LINEAR PART
!
      DO IELEM = 1,NELEM
!
!        SEGMENT 1 (TERMS 1-2 AND 2-1)
!
        XM(ELTSEG(IELEM,1)+NSEG11*(ORISEG(IELEM,1)-1))
     &  =XM(ELTSEG(IELEM,1)+NSEG11*(ORISEG(IELEM,1)-1))+XMT(IELEM,01)
        XM(ELTSEG(IELEM,1)+NSEG11*(2-ORISEG(IELEM,1)))
     &  =XM(ELTSEG(IELEM,1)+NSEG11*(2-ORISEG(IELEM,1)))+XMT(IELEM,03)
!
!        SEGMENT 2 (TERMS 2-3 AND 3-2)
!
        XM(ELTSEG(IELEM,2)+NSEG11*(ORISEG(IELEM,2)-1))
     &  =XM(ELTSEG(IELEM,2)+NSEG11*(ORISEG(IELEM,2)-1))+XMT(IELEM,04)
        XM(ELTSEG(IELEM,2)+NSEG11*(2-ORISEG(IELEM,2)))
     &  =XM(ELTSEG(IELEM,2)+NSEG11*(2-ORISEG(IELEM,2)))+XMT(IELEM,06)
!
!        SEGMENT 3 (TERMS 3-1 AND 1-3)
!
        XM(ELTSEG(IELEM,3)+NSEG11*(ORISEG(IELEM,3)-1))
     &  =XM(ELTSEG(IELEM,3)+NSEG11*(ORISEG(IELEM,3)-1))+XMT(IELEM,05)
        XM(ELTSEG(IELEM,3)+NSEG11*(2-ORISEG(IELEM,3)))
     &  =XM(ELTSEG(IELEM,3)+NSEG11*(2-ORISEG(IELEM,3)))+XMT(IELEM,02)
!
      ENDDO
!
!     ASSEMBLES, SEGMENTS BETWEEN LINEAR AND QUADRATIC POINTS
!     (I.E. THE REST BUT NOT 13, 14 AND 15)
!
!     ASSEMBLES THE QUADRATIC PART
!     BETWEEN XM(2*NSEG11+1) AND XM(NSEG11+NSEG13-3*NELEM)
!     SEE IN COMP_SEG HOW ELTSEG4,5,6,7,8,9,10,11,12 ARE BUILT,
!     THEIR NUMBERING STARTS AT NSEG11+1, HENCE HERE THE STORAGE IN
!     XM STARTS AT 2*NSEG11+1
!
      DO IELEM = 1,NELEM
!       TERM OF SEGMENT 1-4
        XM(ELTSEG(IELEM,04)+NSEG11) =
     &  XM(ELTSEG(IELEM,04)+NSEG11) + XMT(IELEM,07)
!       TERM OF SEGMENT 2-5
        XM(ELTSEG(IELEM,05)+NSEG11) =
     &  XM(ELTSEG(IELEM,05)+NSEG11) + XMT(IELEM,11)
!       TERM OF SEGMENT 3-6
        XM(ELTSEG(IELEM,06)+NSEG11) =
     &  XM(ELTSEG(IELEM,06)+NSEG11) + XMT(IELEM,15)
!       TERM OF SEGMENT 2-4
        XM(ELTSEG(IELEM,07)+NSEG11) =
     &  XM(ELTSEG(IELEM,07)+NSEG11) + XMT(IELEM,08)
!       TERM OF SEGMENT 3-5
        XM(ELTSEG(IELEM,08)+NSEG11) =
     &  XM(ELTSEG(IELEM,08)+NSEG11) + XMT(IELEM,12)
!       TERM OF SEGMENT 1-6
        XM(ELTSEG(IELEM,09)+NSEG11) =
     &  XM(ELTSEG(IELEM,09)+NSEG11) + XMT(IELEM,13)
      ENDDO
!
!     THESE 3 SEGMENTS ARE NOT SHARED, NO ASSEMBLY
!
      DO IELEM = 1,NELEM
!       TERM OF SEGMENT 1-5
        XM(ELTSEG(IELEM,10)+NSEG11) = XMT(IELEM,10)
!       TERM OF SEGMENT 2-6
        XM(ELTSEG(IELEM,11)+NSEG11) = XMT(IELEM,14)
!       TERM OF SEGMENT 3-4
        XM(ELTSEG(IELEM,12)+NSEG11) = XMT(IELEM,09)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
