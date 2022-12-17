!                   *****************
                    SUBROUTINE MT13CC
!                   *****************
!
     &(  A11 , A12 , A13 , A14 , A15 , A16 ,
     &   A21 , A22 , A23 , A24 , A25 , A26 ,
     &   A31 , A32 , A33 , A34 , A35 , A36 ,
     &   A41 , A42 , A43 , A44 , A45 , A46 ,
     &   A51 , A52 , A53 , A54 , A55 , A56 ,
     &   A61 , A62 , A63 , A64 , A65 , A66 ,
     &   XMUL,XEL,YEL,NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD = 1
!+
!+                  /           D
!+ A(I,J)= XMUL *  /  PSI2(I) * --( PSI1(J) ) D(OMEGA)
!+                /OMEGA        DX
!+
!+  ICOORD=2 WOULD GIVE A DERIVATIVE WRT Y
!+
!+  PSI1: BASES OF TYPE P2 TRIANGLE
!+  PSI2: BASES OF TYPE P2 TRIANGLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  ALGIANE FROEHLY (MATMECA)
!+        16/06/08
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
!| A66            |<--| ELEMENTS OF MATRIX
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT13CC => MT13CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A51(*),A52(*),A53(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A54(*),A55(*),A56(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A61(*),A62(*),A63(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A64(*),A65(*),A66(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION XSUR10,XSUR15,XSUR30
!
!-----------------------------------------------------------------------
!
      XSUR10 = XMUL/10.D0
      XSUR15 = XMUL/15.D0
      XSUR30 = XMUL/30.D0
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
!   DIAGONAL TERMS
!
        A22(IELEM) =   Y3 * XSUR15
        A33(IELEM) = - Y2 * XSUR15
        A44(IELEM) = - 4.D0 *   A33(IELEM)
        A55(IELEM) =   4.D0 * ( A22(IELEM) + A33(IELEM) )
        A66(IELEM) = - 4.D0 *   A22(IELEM)
        A11(IELEM) = - A22(IELEM) - A33(IELEM)
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) = - Y3 * XSUR30
        A13(IELEM) =   Y2 * XSUR30
        A42(IELEM) =   Y3 * XSUR10
        A53(IELEM) = - Y2 * XSUR10
        A14(IELEM) = - A13(IELEM) + A42(IELEM)
        A15(IELEM) =   A12(IELEM) + A13(IELEM)
        A16(IELEM) = - A12(IELEM) + A53(IELEM)
        A21(IELEM) = - A15(IELEM)
        A23(IELEM) =   A13(IELEM)
        A24(IELEM) = - A42(IELEM) - A33(IELEM)
        A25(IELEM) =   A12(IELEM) + A33(IELEM)
        A26(IELEM) = - A12(IELEM)
        A31(IELEM) = - A15(IELEM)
        A32(IELEM) =   A12(IELEM)
        A34(IELEM) = - A13(IELEM)
        A35(IELEM) =   A22(IELEM) + A13(IELEM)
        A36(IELEM) = - A53(IELEM) - A22(IELEM)
        A41(IELEM) = - A42(IELEM) - A53(IELEM)
        A43(IELEM) =   A13(IELEM)
        A45(IELEM) = 2.D0 *   A22(IELEM) - A44(IELEM)
        A46(IELEM) = 2.D0 * ( A33(IELEM) - A22(IELEM) )
        A51(IELEM) =   A21(IELEM)
        A52(IELEM) =   A42(IELEM)
        A54(IELEM) = - A45(IELEM)
        A56(IELEM) =   A66(IELEM) - 2.D0 * A33(IELEM)
        A61(IELEM) =   A41(IELEM)
        A62(IELEM) =   A12(IELEM)
        A63(IELEM) =   A53(IELEM)
        A64(IELEM) = - A46(IELEM)
        A65(IELEM) = - A56(IELEM)
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
!   DIAGONAL TERMS
!
        A22(IELEM) = - X3 * XSUR15
        A33(IELEM) =   X2 * XSUR15
        A44(IELEM) = - 4.D0 *   A33(IELEM)
        A55(IELEM) =   4.D0 * ( A22(IELEM) + A33(IELEM) )
        A66(IELEM) = - 4.D0 *   A22(IELEM)
        A11(IELEM) = - A22(IELEM) - A33(IELEM)
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) =   X3 * XSUR30
        A13(IELEM) = - X2 * XSUR30
        A42(IELEM) = - X3 * XSUR10
        A53(IELEM) =   X2 * XSUR10
        A14(IELEM) = - A13(IELEM) + A42(IELEM)
        A15(IELEM) =   A12(IELEM) + A13(IELEM)
        A16(IELEM) = - A12(IELEM) + A53(IELEM)
        A21(IELEM) = - A15(IELEM)
        A23(IELEM) =   A13(IELEM)
        A24(IELEM) = - A42(IELEM) - A33(IELEM)
        A25(IELEM) =   A12(IELEM) + A33(IELEM)
        A26(IELEM) = - A12(IELEM)
        A31(IELEM) = - A15(IELEM)
        A32(IELEM) =   A12(IELEM)
        A34(IELEM) = - A13(IELEM)
        A35(IELEM) =   A22(IELEM) + A13(IELEM)
        A36(IELEM) = - A53(IELEM) - A22(IELEM)
        A41(IELEM) = - A42(IELEM) - A53(IELEM)
        A43(IELEM) =   A13(IELEM)
        A45(IELEM) = 2.D0 *   A22(IELEM) - A44(IELEM)
        A46(IELEM) = 2.D0 * ( A33(IELEM) - A22(IELEM) )
        A51(IELEM) =   A21(IELEM)
        A52(IELEM) =   A42(IELEM)
        A54(IELEM) = - A45(IELEM)
        A56(IELEM) =   A66(IELEM) - 2.D0 * A33(IELEM)
        A61(IELEM) =   A41(IELEM)
        A62(IELEM) =   A12(IELEM)
        A63(IELEM) =   A53(IELEM)
        A64(IELEM) = - A46(IELEM)
        A65(IELEM) = - A56(IELEM)
!
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
201       FORMAT(1X,'MT13CC (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
