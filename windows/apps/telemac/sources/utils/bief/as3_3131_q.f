!                   *********************
                    SUBROUTINE AS3_3131_Q
!                   *********************
!
     &(XM,NSEG,XMT,DIM1XMT,DIM2XMT,STOXMT,
     & NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3,
     & ELTSEG4,ELTSEG5,ELTSEG6,ORISEG1,ORISEG2,ORISEG3,
     & ORISEG4,ORISEG5,ORISEG6)
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
!| ORISEG1        |-->| ORIENTATION OF SEGMENT 1 OF TRIANGLE
!| ORISEG2        |-->| ORIENTATION OF SEGMENT 2 OF TRIANGLE
!| ORISEG3        |-->| ORIENTATION OF SEGMENT 3 OF TRIANGLE
!| ORISEG4        |-->| ORIENTATION OF SEGMENT 4 OF TRIANGLE
!| ORISEG5        |-->| ORIENTATION OF SEGMENT 5 OF TRIANGLE
!| ORISEG6        |-->| ORIENTATION OF SEGMENT 6 OF TRIANGLE
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
      INTEGER         , INTENT(IN)    :: ORISEG1(NELMAX),ORISEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG3(NELMAX),ORISEG4(NELMAX)
      INTEGER         , INTENT(IN)    :: ORISEG5(NELMAX),ORISEG6(NELMAX)
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
     &  = XM(ELTSEG2(IELEM)+NSEG*(ORISEG2(IELEM)-1)) + XMT(IELEM,04)
!         TERM 31
          XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1))
     &  = XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1)) + XMT(IELEM,08)
!         TERM 21
          XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM)))
     &  = XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM))) + XMT(IELEM,07)
!         TERM 32
          XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM)))
     &  = XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM))) + XMT(IELEM,10)
!         TERM 13
          XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM)))
     &  = XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM))) + XMT(IELEM,02)
!         TERM 14
          XM(ELTSEG4(IELEM)+NSEG*(ORISEG4(IELEM)-1))
     &  = XM(ELTSEG4(IELEM)+NSEG*(ORISEG4(IELEM)-1)) + XMT(IELEM,03)
!         TERM 24
          XM(ELTSEG5(IELEM)+NSEG*(ORISEG5(IELEM)-1))
     &  = XM(ELTSEG5(IELEM)+NSEG*(ORISEG5(IELEM)-1)) + XMT(IELEM,05)
!         TERM 34
          XM(ELTSEG6(IELEM)+NSEG*(ORISEG6(IELEM)-1))
     &  = XM(ELTSEG6(IELEM)+NSEG*(ORISEG6(IELEM)-1)) + XMT(IELEM,06)
!         TERM 41
          XM(ELTSEG4(IELEM)+NSEG*(2-ORISEG4(IELEM)))
     &  = XM(ELTSEG4(IELEM)+NSEG*(2-ORISEG4(IELEM))) + XMT(IELEM,09)
!         TERM 42
          XM(ELTSEG5(IELEM)+NSEG*(2-ORISEG5(IELEM)))
     &  = XM(ELTSEG5(IELEM)+NSEG*(2-ORISEG5(IELEM))) + XMT(IELEM,11)
!         TERM 43
          XM(ELTSEG6(IELEM)+NSEG*(2-ORISEG6(IELEM)))
     &  = XM(ELTSEG6(IELEM)+NSEG*(2-ORISEG6(IELEM))) + XMT(IELEM,12)
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
     &  = XM(ELTSEG2(IELEM)+NSEG*(ORISEG2(IELEM)-1)) + XMT(04,IELEM)
!         TERM 31
          XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1))
     &  = XM(ELTSEG3(IELEM)+NSEG*(ORISEG3(IELEM)-1)) + XMT(08,IELEM)
!         TERM 21
          XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM)))
     &  = XM(ELTSEG1(IELEM)+NSEG*(2-ORISEG1(IELEM))) + XMT(07,IELEM)
!         TERM 32
          XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM)))
     &  = XM(ELTSEG2(IELEM)+NSEG*(2-ORISEG2(IELEM))) + XMT(10,IELEM)
!         TERM 13
          XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM)))
     &  = XM(ELTSEG3(IELEM)+NSEG*(2-ORISEG3(IELEM))) + XMT(02,IELEM)
!         TERM 14
          XM(ELTSEG4(IELEM)+NSEG*(ORISEG4(IELEM)-1))
     &  = XM(ELTSEG4(IELEM)+NSEG*(ORISEG4(IELEM)-1)) + XMT(03,IELEM)
!         TERM 24
          XM(ELTSEG5(IELEM)+NSEG*(ORISEG5(IELEM)-1))
     &  = XM(ELTSEG5(IELEM)+NSEG*(ORISEG5(IELEM)-1)) + XMT(05,IELEM)
!         TERM 34
          XM(ELTSEG6(IELEM)+NSEG*(ORISEG6(IELEM)-1))
     &  = XM(ELTSEG6(IELEM)+NSEG*(ORISEG6(IELEM)-1)) + XMT(06,IELEM)
!         TERM 41
          XM(ELTSEG4(IELEM)+NSEG*(2-ORISEG4(IELEM)))
     &  = XM(ELTSEG4(IELEM)+NSEG*(2-ORISEG4(IELEM))) + XMT(09,IELEM)
!         TERM 42
          XM(ELTSEG5(IELEM)+NSEG*(2-ORISEG5(IELEM)))
     &  = XM(ELTSEG5(IELEM)+NSEG*(2-ORISEG5(IELEM))) + XMT(11,IELEM)
!         TERM 43
          XM(ELTSEG6(IELEM)+NSEG*(2-ORISEG6(IELEM)))
     &  = XM(ELTSEG6(IELEM)+NSEG*(2-ORISEG6(IELEM))) + XMT(12,IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,*) 'AS3_3131_Q: UNKNOWN STORAGE OF XMT : ',STOXMT
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
