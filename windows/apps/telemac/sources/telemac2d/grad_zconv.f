!                   *********************
                    SUBROUTINE GRAD_ZCONV
!                   *********************
!
     &(GRAZCO,ZCONV,XEL,YEL,NELEM,NELMAX)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   07/09/2011
!***********************************************************************
!
!brief    COMPUTES THE GRADIENT OF A PIECE-WISE LINEAR FUNCTION IN 2D
!
!history  J-M HERVOUET (LNHE)
!+        07/09/2011
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GRAZCO         |<--| TWO COMPONENTS OF THE GRADIENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| XEL            |-->| ABSCISSAE OF POINTS, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS, PER ELEMENT
!| ZCONV          |-->| THE PIECE-WISE LINEAr FUNCTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(INOUT) :: GRAZCO(NELMAX,2)
      DOUBLE PRECISION, INTENT(IN)    :: ZCONV(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3,G2,G3,DET
!
!-----------------------------------------------------------------------
!
      DO IELEM=1,NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
        DET=X2*Y3-X3*Y2
        G2 = ZCONV(IELEM,2)-ZCONV(IELEM,1)
        G3 = ZCONV(IELEM,3)-ZCONV(IELEM,1)
        GRAZCO(IELEM,1)=(G2*Y3-G3*Y2)/DET
        GRAZCO(IELEM,2)=(X2*G3-X3*G2)/DET
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
