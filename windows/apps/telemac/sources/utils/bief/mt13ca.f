!                   *****************
                    SUBROUTINE MT13CA
!                   *****************
!
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   A41 , A42 , A43 ,
     &   A51 , A52 , A53 ,
     &   A61 , A62 , A63 ,
     &   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD = 1
!+
!+                  /           D
!+ A(I,J)= XMUL *  /  PSI1(I) * --( PSI2(J) ) D(OMEGA)
!+                /OMEGA        DX
!+
!+  ICOORD=2 WOULD GIVE A DERIVATIVE WRT Y
!+
!+  PSI1: BASES OF TYPE P2 TRIANGLE
!+  PSI2: BASES OF TYPE P1 TRIANGLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  ALGIANE FROEHLY
!+        09/07/2008
!+        V5P9
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
!| A11            |<--| ELEMENTS OF MATRIX
!| ...            |<--| ELEMENTS OF MATRIX
!| A63            |<--| ELEMENTS OF MATRIX
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT13CA => MT13CA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A51(*),A52(*),A53(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A61(*),A62(*),A63(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION XSUR6
!
!-----------------------------------------------------------------------
!
      XSUR6 = XMUL/6.D0
!
!================================
!  DERIVATIVE WRT X  =
!================================
!
        IF(ICOORD.EQ.1) THEN
!
!   LOOP ON THE ELEMENTS
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) = 0.D0
        A13(IELEM) = 0.D0
        A21(IELEM) = 0.D0
        A23(IELEM) = 0.D0
        A31(IELEM) = 0.D0
        A32(IELEM) = 0.D0
        A42(IELEM) =   Y3*XSUR6
        A43(IELEM) = - Y2*XSUR6
        A41(IELEM) = - A42(IELEM) - A43(IELEM)
        A51(IELEM) =   A41(IELEM)
        A52(IELEM) =   A42(IELEM)
        A53(IELEM) =   A43(IELEM)
        A61(IELEM) =   A41(IELEM)
        A62(IELEM) =   A42(IELEM)
        A63(IELEM) =   A43(IELEM)
!
!   DIAGONAL TERMS
!
        A11(IELEM) = 0.D0
        A22(IELEM) = 0.D0
        A33(IELEM) = 0.D0
!
      ENDDO ! IELEM
!
        ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) = 0.D0
        A13(IELEM) = 0.D0
        A21(IELEM) = 0.D0
        A23(IELEM) = 0.D0
        A31(IELEM) = 0.D0
        A32(IELEM) = 0.D0
        A42(IELEM) = - X3*XSUR6
        A43(IELEM) =   X2*XSUR6
        A41(IELEM) = - A42(IELEM) - A43(IELEM)
        A51(IELEM) =   A41(IELEM)
        A52(IELEM) =   A42(IELEM)
        A53(IELEM) =   A43(IELEM)
        A61(IELEM) =   A41(IELEM)
        A62(IELEM) =   A42(IELEM)
        A63(IELEM) =   A43(IELEM)
!
!   DIAGONAL TERMS
!
        A11(IELEM) = 0.D0
        A22(IELEM) = 0.D0
        A33(IELEM) = 0.D0
!
        ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
!
        ENDIF
!
201     FORMAT(1X,'MT13CA (BIEF) : IMPOSSIBLE COMPONENT ',
     &         1I6,' CHECK ICOORD')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
