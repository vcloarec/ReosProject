!                   *********************
                    SUBROUTINE AS3_8181_S
!                   *********************
!
     &(XM,NSEG,XMT,DIM1XMT,DIM2XMT,STOXMT,NELMAX,NELEM,
     & ELTSEG1,ELTSEG2,ELTSEG3)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE.
!+
!+            CASE OF TETRAHEDRON ELEMENT AND SYMMETRICAL MATRIX.
!+            SEE EBE STORAGE IN XMT FROM AAS IN MATRIY.f
!
!history  F. DECUNG (LNHE)
!+        20/07/2012
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIM1XMT        |-->| FIRST DIMENSION OF XMT
!| DIM2XMT        |-->| SECOND DIMENSION OF XMT
!| ELTSEG1        |-->| FIRST SEGMENT OF A TRIANGLE
!| ELTSEG2        |-->| SECOND SEGMENT OF A TRIANGLE
!| ELTSEG3        |-->| THIRD SEGMENT OF A TRIANGLE
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
      INTEGER         , INTENT(IN)    :: ELTSEG3(NELMAX)
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
        XM(ELTSEG2(IELEM)) = XM(ELTSEG2(IELEM)) + XMT(IELEM,2)
!       TERM 31
        XM(ELTSEG3(IELEM)) = XM(ELTSEG3(IELEM)) + XMT(IELEM,3)
      ENDDO
!      pause
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
        XM(ELTSEG2(IELEM)) = XM(ELTSEG2(IELEM)) + XMT(2,IELEM)
!       TERM 31
        XM(ELTSEG3(IELEM)) = XM(ELTSEG3(IELEM)) + XMT(3,IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'AS3_8181_S: UNKNOWN STORAGE OF XMT : ',STOXMT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
