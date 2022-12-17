!                   *****************
                    SUBROUTINE MT15PP
!                   *****************
!
     &(T,XM,XMUL,F,ZPT,SURFAC,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V7P0                                  03/06/2014
!***********************************************************************
!
!brief    Builds a matrix corresponding to the advection with a settling
!+        velocity. This vertical velocity WC is stored in F and
!+        is positive when going downwards.
!
!warning  Element linear prism : 41
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        03/06/2014
!+        V7P0
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE FORMULA
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!| ZPT            |-->| Z COORDINATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT15PP => MT15PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,30)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),ZPT(*)
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION XSUR3
!
!-----------------------------------------------------------------------
!
      XSUR3=XMUL/3.D0
!
!=======================================================================
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!
        T(IELEM,1)=0.D0
        T(IELEM,2)=0.D0
        T(IELEM,3)=0.D0
!
        XM(IELEM, 1)=0.D0
        XM(IELEM, 2)=0.D0
        XM(IELEM, 4)=0.D0
        XM(IELEM, 5)=0.D0
        XM(IELEM, 6)=0.D0
        XM(IELEM, 7)=0.D0
        XM(IELEM, 9)=0.D0
        XM(IELEM,10)=0.D0
        XM(IELEM,11)=0.D0
        XM(IELEM,13)=0.D0
        XM(IELEM,14)=0.D0
        XM(IELEM,15)=0.D0
!
        XM(IELEM,16)=0.D0
        XM(IELEM,17)=0.D0
        XM(IELEM,18)=0.D0
        XM(IELEM,19)=0.D0
        XM(IELEM,20)=0.D0
        XM(IELEM,21)=0.D0
        XM(IELEM,22)=0.D0
        XM(IELEM,23)=0.D0
        XM(IELEM,24)=0.D0
        XM(IELEM,25)=0.D0
        XM(IELEM,26)=0.D0
        XM(IELEM,27)=0.D0
        XM(IELEM,28)=0.D0
        XM(IELEM,29)=0.D0
        XM(IELEM,30)=0.D0
!
!-----------------------------------------------------------------------
!
!     EXTRA-DIAGONAL TERMS: BOTTOM POINTS RECEIVE FROM UPPER POINT
!                           A FLUX EQUAL TO WC*SURFAC/3 MULTIPLIED
!                           BY CONCENTRATION OF UPPER POINT
!     DIAGONAL TERMS : TOP POINTS GIVE TO LOWER POINTS
!                      A FLUX EQUAL TO WC*SURFAC/3 MULTIPLIED
!                      BY ITS CONCENTRATION.
!
!-----------------------------------------------------------------------
!
        IF(ZPT(I4)-ZPT(I1).GT.1.D-4) THEN
!         TERM 4-4
          T(IELEM,4)=   F(I4)*SURFAC(IELEM)*XSUR3
!         TERM 1-4
          XM(IELEM, 3)=-F(I4)*SURFAC(IELEM)*XSUR3
        ENDIF
        IF(ZPT(I5)-ZPT(I2).GT.1.D-4) THEN
!         TERM 5-5
          T(IELEM,5)=   F(I5)*SURFAC(IELEM)*XSUR3
!         TERM 2-5
          XM(IELEM, 8)=-F(I5)*SURFAC(IELEM)*XSUR3
        ENDIF
        IF(ZPT(I6)-ZPT(I3).GT.1.D-4) THEN
!         TERM 6-6
          T(IELEM,6)=   F(I6)*SURFAC(IELEM)*XSUR3
!         TERM 3-6
          XM(IELEM,12)=-F(I6)*SURFAC(IELEM)*XSUR3
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
