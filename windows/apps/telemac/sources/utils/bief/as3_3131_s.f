!                   *********************
                    SUBROUTINE AS3_3131_S
!                   *********************
!
     &(XM,NSEG,XMT,DIM1XMT,DIM2XMT,STOXMT,NELMAX,NELEM,
     & ELTSEG1,ELTSEG2,ELTSEG3,ELTSEG4,ELTSEG5,ELTSEG6)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF TETRAHEDRON ELEMENT AND SYMMETRICAL MATRIX.
!
!history  J-M HERVOUET (LNHE)
!+        25/08/2011
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIM1XMT        |-->| FIRST DIMENSION OF XMT
!| DIM2XMT        |-->| SECOND DIMENSION OF XMT
!| ELTSEG1        |-->| FIRST SEGMENT OF A TRIANGLE
!| ELTSEG2        |-->| SECOND SEGMENT OF A TRIANGLE
!| ELTSEG3        |-->| THIRD SEGMENT OF A TRIANGLE
!| ELTSEG4        |-->| FOURTH SEGMENT OF A QUADRATIC TRIANGLE
!| ELTSEG5        |-->| FIFTH SEGMENT OF A QUADRATIC TRIANGLE
!| ELTSEG6        |-->| SIXTH SEGMENT OF A QUADRATIC TRIANGLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE AND W.
!| NSEG           |-->| NUMBER OF SEGMENTS
!| STOXMT         |-->| STORAGE MODE OF XMT
!| XM             |<--| ASSEMBLED OFF-DIAGONAL TERMS XA12,23,31
!| XMT            |-->| ELEMENT BY ELEMENT STORAGE OF MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG
      INTEGER         , INTENT(IN)    :: DIM1XMT,DIM2XMT,STOXMT
      INTEGER         , INTENT(IN)    :: ELTSEG1(NELMAX),ELTSEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG3(NELMAX),ELTSEG4(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG5(NELMAX),ELTSEG6(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: XMT(DIM1XMT,DIM2XMT)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO ISEG = 1 , NSEG
        XM(ISEG) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(STOXMT.EQ.1) THEN
!
!     ASSEMBLES
!
      DO IELEM = 1,NELEM
!       TERM 12
        XM(ELTSEG1(IELEM)) = XM(ELTSEG1(IELEM)) + XMT(IELEM,1)
!       TERM 23
        XM(ELTSEG2(IELEM)) = XM(ELTSEG2(IELEM)) + XMT(IELEM,4)
!       TERM 31
        XM(ELTSEG3(IELEM)) = XM(ELTSEG3(IELEM)) + XMT(IELEM,2)
!       TERM 14
        XM(ELTSEG4(IELEM)) = XM(ELTSEG4(IELEM)) + XMT(IELEM,3)
!       TERM 24
        XM(ELTSEG5(IELEM)) = XM(ELTSEG5(IELEM)) + XMT(IELEM,5)
!       TERM 34
        XM(ELTSEG6(IELEM)) = XM(ELTSEG6(IELEM)) + XMT(IELEM,6)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(STOXMT.EQ.2) THEN
!
!     ASSEMBLES
!
      DO IELEM = 1,NELEM
!       TERM 12
        XM(ELTSEG1(IELEM)) = XM(ELTSEG1(IELEM)) + XMT(1,IELEM)
!       TERM 23
        XM(ELTSEG2(IELEM)) = XM(ELTSEG2(IELEM)) + XMT(4,IELEM)
!       TERM 31
        XM(ELTSEG3(IELEM)) = XM(ELTSEG3(IELEM)) + XMT(2,IELEM)
!       TERM 14
        XM(ELTSEG4(IELEM)) = XM(ELTSEG4(IELEM)) + XMT(3,IELEM)
!       TERM 24
        XM(ELTSEG5(IELEM)) = XM(ELTSEG5(IELEM)) + XMT(5,IELEM)
!       TERM 34
        XM(ELTSEG6(IELEM)) = XM(ELTSEG6(IELEM)) + XMT(6,IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'AS3_3131_S: UNKNOWN STORAGE OF XMT : ',STOXMT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
