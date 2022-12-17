!                   *********************
                    SUBROUTINE AS3_1212_Q
!                   *********************
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
!+            CASE OF QUASIBUBBLE-QUASIBUBBLE ELEMENT.
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
!| ELTSEG4        |-->| FOURTH SEGMENT OF A QUADRATIC TRIANGLE
!| ELTSEG5        |-->| FIFTH SEGMENT OF A QUADRATIC TRIANGLE
!| ELTSEG6        |-->| SIXTH SEGMENT OF A QUADRATIC TRIANGLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NSEG11         |-->| NUMBER OF SEGMENTS (HERE JOINING LINEAR POINTS)
!| NSEG12         |-->| NUMBER OF SEGMENTS (HERE QUADRATIC)
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
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG12*2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO ISEG = 1 , NSEG11
        XM(ISEG       ) = 0.D0
        XM(ISEG+NSEG12) = 0.D0
      ENDDO
!
!  ASSEMBLES PART P1
!
      DO IELEM = 1,NELEM
!         TERM 12
          XM(ELTSEG1(IELEM)+NSEG12*(ORISEG1(IELEM)-1))
     &  = XM(ELTSEG1(IELEM)+NSEG12*(ORISEG1(IELEM)-1)) + XMT(IELEM,01)
!         TERM 23
          XM(ELTSEG2(IELEM)+NSEG12*(ORISEG2(IELEM)-1))
     &  = XM(ELTSEG2(IELEM)+NSEG12*(ORISEG2(IELEM)-1)) + XMT(IELEM,04)
!         TERM 31
          XM(ELTSEG3(IELEM)+NSEG12*(ORISEG3(IELEM)-1))
     &  = XM(ELTSEG3(IELEM)+NSEG12*(ORISEG3(IELEM)-1)) + XMT(IELEM,08)
!         TERM 21
          XM(ELTSEG1(IELEM)+NSEG12*(2-ORISEG1(IELEM)))
     &  = XM(ELTSEG1(IELEM)+NSEG12*(2-ORISEG1(IELEM))) + XMT(IELEM,07)
!         TERM 32
          XM(ELTSEG2(IELEM)+NSEG12*(2-ORISEG2(IELEM)))
     &  = XM(ELTSEG2(IELEM)+NSEG12*(2-ORISEG2(IELEM))) + XMT(IELEM,10)
!         TERM 13
          XM(ELTSEG3(IELEM)+NSEG12*(2-ORISEG3(IELEM)))
     &  = XM(ELTSEG3(IELEM)+NSEG12*(2-ORISEG3(IELEM))) + XMT(IELEM,02)
      ENDDO
!
!  ASSEMBLES QUASIBUBBLE PART
!
      DO IELEM = 1,NELEM
!       TERM 14
        XM(ELTSEG4(IELEM)) = XMT(IELEM,03)
!       TERM 24
        XM(ELTSEG5(IELEM)) = XMT(IELEM,05)
!       TERM 34
        XM(ELTSEG6(IELEM)) = XMT(IELEM,06)
!       TERM 41
        XM(ELTSEG4(IELEM)+NSEG12) = XMT(IELEM,09)
!       TERM 42
        XM(ELTSEG5(IELEM)+NSEG12) = XMT(IELEM,11)
!       TERM 43
        XM(ELTSEG6(IELEM)+NSEG12) = XMT(IELEM,12)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
