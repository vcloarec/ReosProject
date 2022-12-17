!                   *******************
                    SUBROUTINE AS3_1112
!                   *******************
!
     &(XM,NSEG11,NSEG12,XMT,NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3,
     &                                   ELTSEG4,ELTSEG5,ELTSEG6,
     &                                   ORISEG1,ORISEG2,ORISEG3)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF LINEAR-QUASIBUBBLE ELEMENT.
!code
!+            THE EXTRA-DIAGONAL TERMS OF THIS RECTANGULAR MATRIX ARE :
!+
!+            ... 1-2 1-3 1-4
!+            2-1 ... 2-3 2-4
!+            3-1 3-2 ... 3-4
!+
!+            AND ARE STORED LIKE A SQUARE LINEAR MATRIX + 3 TERMS
!+            WHICH GIVES 2*NSEG11 + (NSEG12-NSEG11) TERMS
!+            OR NSEG11 + NSEG12, HENCE THE DIMENSION OF XM
!
!history  J-M HERVOUET (LNH)
!+        29/12/05
!+        V5P6
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
!| ELTSEG1        |-->| FIRST SEGMENT OF A TRIANGLE
!| ELTSEG2        |-->| SECOND SEGMENT OF A TRIANGLE
!| ELTSEG3        |-->| THIRD SEGMENT OF A TRIANGLE
!| ELTSEG4        |-->| FOURTH SEGMENT OF A QUASI-BUBBLE TRIANGLE
!| ELTSEG5        |-->| FIFTH SEGMENT OF A QUASI-BUBBLE TRIANGLE
!| ELTSEG6        |-->| SIXTH SEGMENT OF A QUASI-BUBBLE TRIANGLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NSEG11         |-->| NUMBER OF SEGMENTS (HERE JOINING LINEAR POINTS)
!| NSEG12         |-->| NUMBER OF SEGMENTS (HERE QUASI-BUBBLE)
!| ORISEG1        |-->| ORIENTATION OF SEGMENT 1 OF TRIANGLE
!| ORISEG2        |-->| ORIENTATION OF SEGMENT 2 OF TRIANGLE
!| ORISEG3        |-->| ORIENTATION OF SEGMENT 3 OF TRIANGLE
!| XM             |<--| ASSEMBLED OFF-DIAGONAL TERMS XA12,23,31
!| XMT            |-->| ELEMENT BY ELEMENT STORAGE OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG11,NSEG12
      INTEGER         , INTENT(IN)    :: ELTSEG1(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG3(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG4(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG5(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG6(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG1(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMT(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG11+NSEG12)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO ISEG = 1 , 2*NSEG11
        XM(ISEG) = 0.D0
      ENDDO
!
!  ASSEMBLES THE LINEAR PART (BETWEEN 1 AND 2*NSEG11)
!
!  THE SHIFT NSEG11*(...) PLACES THE TERM IN THE HIGHER OR LOWER
!  OFF-DIAGONAL TERMS, DEPENDING ON THE ORIENTATION OF THE SEGMENT
!
      DO IELEM = 1,NELEM
!         TERM 12
          XM(ELTSEG1(IELEM)+NSEG11*(ORISEG1(IELEM)-1))
     &  = XM(ELTSEG1(IELEM)+NSEG11*(ORISEG1(IELEM)-1)) + XMT(IELEM,01)
!         TERM 23
          XM(ELTSEG2(IELEM)+NSEG11*(ORISEG2(IELEM)-1))
     &  = XM(ELTSEG2(IELEM)+NSEG11*(ORISEG2(IELEM)-1)) + XMT(IELEM,05)
!         TERM 31
          XM(ELTSEG3(IELEM)+NSEG11*(ORISEG3(IELEM)-1))
     &  = XM(ELTSEG3(IELEM)+NSEG11*(ORISEG3(IELEM)-1)) + XMT(IELEM,07)
!         TERM 21
          XM(ELTSEG1(IELEM)+NSEG11*(2-ORISEG1(IELEM)))
     &  = XM(ELTSEG1(IELEM)+NSEG11*(2-ORISEG1(IELEM))) + XMT(IELEM,04)
!         TERM 32
          XM(ELTSEG2(IELEM)+NSEG11*(2-ORISEG2(IELEM)))
     &  = XM(ELTSEG2(IELEM)+NSEG11*(2-ORISEG2(IELEM))) + XMT(IELEM,08)
!         TERM 13
          XM(ELTSEG3(IELEM)+NSEG11*(2-ORISEG3(IELEM)))
     &  = XM(ELTSEG3(IELEM)+NSEG11*(2-ORISEG3(IELEM))) + XMT(IELEM,02)
      ENDDO
!
!  ASSEMBLES THE QUASI-BUBBLE PART
!  BETWEEN XM(2*NSEG11+1) AND XM(NSEG11+NSEG12)
!  SEE IN STOSEG HOW ELTSEG4,5,6 ARE BUILT
!
      DO IELEM = 1,NELEM
!       TERM 14
        XM(ELTSEG4(IELEM)+NSEG11) = XMT(IELEM,03)
!       TERM 24
        XM(ELTSEG5(IELEM)+NSEG11) = XMT(IELEM,06)
!       TERM 34
        XM(ELTSEG6(IELEM)+NSEG11) = XMT(IELEM,09)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
