!                   *********************
                    SUBROUTINE AS3_8181_Q
!                   *********************
!
     &(XM,NSEG,XMT,DIM1XMT,DIM2XMT,STOXMT,
     & NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3,
     & ORISEG1,ORISEG2,ORISEG3)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!+                IN THE CASE OF EDGE-BASED STORAGE AND NON SYMMETRICAL
!+                MATRIX.
!+
!+            CASE OF TETRAHEDRON ELEMENT.
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
!| ORISEG1        |-->| ORIENTATION OF SEGMENT 1 OF TRIANGLE
!| ORISEG2        |-->| ORIENTATION OF SEGMENT 2 OF TRIANGLE
!| ORISEG3        |-->| ORIENTATION OF SEGMENT 3 OF TRIANGLE
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
      INTEGER         , INTENT(IN)    :: ORISEG1(NELMAX),ORISEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: XMT(DIM1XMT,DIM2XMT)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG*2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,IELEM
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO ISEG = 1 , 2*NSEG
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
!         TERM 12
          XM(ELTSEG1(IELEM)+NSEG*(ORISEG1(IELEM)-1))
     &  = XM(ELTSEG1(IELEM)+NSEG*(ORISEG1(IELEM)-1)) + XMT(IELEM,01)
!         TERM 23
          XM(ELTSEG2(IELEM)+NSEG*(ORISEG2(IELEM)-1))
     &  = XM(ELTSEG2(IELEM)+NSEG*(ORISEG2(IELEM)-1)) + XMT(IELEM,02)
!         TERM 31
          XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1))
     &  = XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1)) + XMT(IELEM,06)
!         TERM 21
          XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM)))
     &  = XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM))) + XMT(IELEM,04)
!         TERM 32
          XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM)))
     &  = XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM))) + XMT(IELEM,05)
!         TERM 13
          XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM)))
     &  = XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM))) + XMT(IELEM,03)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(STOXMT.EQ.2) THEN
!
      DO IELEM = 1,NELEM
!         TERM 12
          XM(ELTSEG1(IELEM)+NSEG*(ORISEG1(IELEM)-1))
     &  = XM(ELTSEG1(IELEM)+NSEG*(ORISEG1(IELEM)-1)) + XMT(01,IELEM)
!         TERM 23
          XM(ELTSEG2(IELEM)+NSEG*(ORISEG2(IELEM)-1))
     &  = XM(ELTSEG2(IELEM)+NSEG*(ORISEG2(IELEM)-1)) + XMT(02,IELEM)
!         TERM 31
          XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1))
     &  = XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1)) + XMT(06,IELEM)
!         TERM 21
          XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM)))
     &  = XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM))) + XMT(04,IELEM)
!         TERM 32
          XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM)))
     &  = XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM))) + XMT(05,IELEM)
!         TERM 13
          XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM)))
     &  = XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM))) + XMT(03,IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'AS3_8181_Q: UNKNOWN STORAGE OF XMT : ',STOXMT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
