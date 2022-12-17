!                 *****************
                  SUBROUTINE KSPG11
!                 *****************
!
     &(KX,KY,XEL,YEL,U,V,IKLE,NELEM,NELMAX,XMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES A VECTOR, USED BY THE METHOD:
!+                STREAMLINE UPWIND PETROV GALERKIN (SUPG)
!+                WITH AN OFF-CENTERING OF 1.
!code
!+                    DX   U
!+             KX = -----------
!+                   2 NORM(U)
!+
!+                    DY   V
!+             KY = -----------
!+                   2 NORM(U)
!
!code
!+  MEANING OF IELM :
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : P1 TRIANGLE            3                       YES
!+  12 : P2 TRIANGLE            6
!+  21 : Q1 QUADRILATERAL       4                       YES
!+  41 : TELEMAC-3D PRISMS      6
!
!history  J-M HERVOUET (LNH)
!+        08/12/94
!+        V5P1
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
!| IKLE           |-->| CONNECTIVITY TABLE.
!| KX             |-->| FIRST COMPONENT OF RESULTING VECTOR
!| KY             |-->| SECOND COMPONENT OF RESULTING VECTOR
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| U              |-->| FIRST COMPONENT OF VELOCITY
!| V              |-->| SECOND COMPONENT OF VELOCITY
!| XMUL           |-->| MULTIPLICATION COEFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: KX(NELEM),KY(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: U(*),V(*),XMUL
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3
!
      DOUBLE PRECISION UMOY,VMOY,H,SUNORM,X2,X3,Y2,Y3
      DOUBLE PRECISION SURFAC,GP1X,GP1Y,GP2X,GP2Y,GP3X,GP3Y
      DOUBLE PRECISION A1,A2,A3,H1,H2,H3,C1,C2,C3,UNORM,VNORM
!
      INTRINSIC MAX,SQRT
!
!-----------------------------------------------------------------------
!
      DO IELEM = 1 , NELEM
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        GP1X = Y2-Y3
        GP1Y = X3-X2
        SUNORM = 1.D0 / SQRT(GP1X**2+GP1Y**2)
        GP1X = GP1X * SUNORM
        GP1Y = GP1Y * SUNORM
!
        GP2X = Y3
        GP2Y =   -X3
        SUNORM = 1.D0 / SQRT(GP2X**2+GP2Y**2)
        GP2X = GP2X * SUNORM
        GP2Y = GP2Y * SUNORM
!
        GP3X =   -Y2
        GP3Y = X2
        SUNORM = 1.D0 / SQRT(GP3X**2+GP3Y**2)
        GP3X = GP3X * SUNORM
        GP3Y = GP3Y * SUNORM
!
        C3 = SQRT(  X2**2     +  Y2**2 )
        C1 = SQRT( (X3-X2)**2 + (Y3-Y2)**2 )
        C2 = SQRT(  X3**2     +  Y3**2 )
!
        SURFAC = 0.5D0 * (X2*Y3 - X3*Y2)
!
        H1 = 2*SURFAC/C1
        H2 = 2*SURFAC/C2
        H3 = 2*SURFAC/C3
!
        H = MAX(H1,H2,H3)
!
        UMOY = U(I1) + U(I2) + U(I3)
        VMOY = V(I1) + V(I2) + V(I3)
!
        SUNORM = 1.D0 / MAX ( SQRT(UMOY**2+VMOY**2) , 1.D-10 )
!
        UNORM = UMOY * SUNORM
        VNORM = VMOY * SUNORM
!
        A1 = GP1X * UNORM + GP1Y * VNORM
        A2 = GP2X * UNORM + GP2Y * VNORM
        A3 = GP3X * UNORM + GP3Y * VNORM
!
        IF(A1*H.GT.H1) H = H1
        IF(A2*H.GT.H2) H = H2
        IF(A3*H.GT.H3) H = H3
!
        KX(IELEM) = 0.33333333D0 * XMUL * H * UNORM
        KY(IELEM) = 0.33333333D0 * XMUL * H * VNORM
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
