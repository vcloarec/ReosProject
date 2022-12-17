!                   *****************
                    SUBROUTINE MT08AC
!                   *****************
!
     &(  A11 , A12 , A13 , A14 , A15, A16,
     &   A21 , A22 , A23 , A24 , A25, A26,
     &   A31 , A32 , A33 , A34 , A35, A36,
     &   XMUL,SF,F,XEL,YEL,IKLE1,IKLE2,IKLE3,
     &                     IKLE4,IKLE5,IKLE6,
     &   NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD = 1
!+
!+                   /                     D
!+  A(I,J)= -XMUL   /  PSI2(J) *    F    * --( PSI1(I) ) D(OMEGA)
!+                 /OMEGA                  DX
!+
!+  BEWARE THE MINUS SIGN !!
!+
!+  PSI1: BASES OF TYPE P1 TRIANGLE
!+  PSI2: BASES OF TYPE P2 TRIANGLE
!+
!+  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  ALGIANE FROEHLY (MATMECA)
!+        19/06/08
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
!| A36            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| FOURTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE5          |-->| FIFTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE6          |-->| SIXTH POINTS OF TRIANGLES (QUADRATIC)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT08AC => MT08AC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
      INTEGER, INTENT(IN) :: IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION X2,X3,Y2,Y3,F1,F2,F3,F4,F5,F6
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!  CASE WHERE F IS OF P1 DISCRETISATION
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.11) THEN
!
!================================
!  CASE OF DERIVATIVE WRT X =
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
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) =  (Y3-Y2) * (-F1+2.D0*F2-F3) * (XMUL/120.D0)
        A13(IELEM) = -(Y3-Y2) * (F1+F2-2.D0*F3)  * (XMUL/120.D0)
        A14(IELEM) =  (Y3-Y2) * (2.D0*F1+F3+2.D0*F2) * (XMUL/30.D0)
        A15(IELEM) =  (Y3-Y2) * (F1+2.D0*F3+2.D0*F2) * (XMUL/30.D0)
        A16(IELEM) =  (Y3-Y2) * (F2+2.D0*F1+2.D0*F3) * (XMUL/30.D0)
        A21(IELEM) =  Y3      * (F2-2.D0*F1+F3)  * (XMUL/120.D0)
        A23(IELEM) = -Y3      * (-F1-F2+2.D0*F3) * (XMUL/120.D0)
        A24(IELEM) = -Y3      * (2.D0*F1+F3+2.D0*F2) * (XMUL/30.D0)
        A25(IELEM) = -Y3      * (F1+2.D0*F3+2.D0*F2) * (XMUL/30.D0)
        A26(IELEM) = -Y3      * (F2+2.D0*F1+2.D0*F3) * (XMUL/30.D0)
        A31(IELEM) = -Y2      * (F2-2.D0*F1+F3)      * (XMUL/120.D0)
        A32(IELEM) =  Y2      * (-F1+2.D0*F2-F3)     * (XMUL/120.D0)
        A34(IELEM) =  Y2      * (2.D0*F1+F3+2.D0*F2) * (XMUL/30.D0)
        A35(IELEM) =  Y2      * (F1+2.D0*F3+2.D0*F2) * (XMUL/30.D0)
        A36(IELEM) =  Y2      * (F2+2.D0*F1+2.D0*F3) * (XMUL/30.D0)
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
        ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  CASE OF DERIVATIVE WRT Y =
!================================
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) =  (X3-X2 ) * (F1-2.D0*F2+F3 ) * (XMUL/120.D0)
        A13(IELEM) =  (-X3+X2) * (-F1-F2+2.D0*F3) * (XMUL/120.D0)
        A14(IELEM) =  (-X3+X2) * (2.D0*F1+F3+2.D0*F2) * (XMUL/30.D0)
        A15(IELEM) =  (-X3+X2) * (F1+2.D0*F3+2.D0*F2) * (XMUL/30.D0)
        A16(IELEM) =  (-X3+X2) * (2.D0*F3+2.D0*F1+F2) * (XMUL/30.D0)
        A21(IELEM) = -X3       * (F3-2.D0*F1+F2 ) * (XMUL/120.D0)
        A23(IELEM) =  X3       * (-F1-F2+2.D0*F3) * (XMUL/120.D0)
        A24(IELEM) =  X3       * (2.D0*F1+F3+2.D0*F2) * (XMUL/30.D0)
        A25(IELEM) =  X3       * (F1+2.D0*F3+2.D0*F2) * (XMUL/30.D0)
        A26(IELEM) =  X3       * (2.D0*F3+2.D0*F1+F2) * (XMUL/30.D0)
        A31(IELEM) =  X2       * (F3-2.D0*F1+F2 ) * (XMUL/120.D0)
        A32(IELEM) =  X2       * (F1-2.D0*F2+F3 ) * (XMUL/120.D0)
        A34(IELEM) = -X2       * (2.D0*F1+F3+2.D0*F2) * (XMUL/30.D0)
        A35(IELEM) = -X2       * (F1+2.D0*F3+2.D0*F2) * (XMUL/30.D0)
        A36(IELEM) = -X2       * (2.D0*F3+2.D0*F1+F2) * (XMUL/30.D0)
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
        ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
!
!
!-----------------------------------------------------------------------
!  CASE WHERE F IS OF P2 DISCRETISATION
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.13) THEN
!
!================================
!  CASE OF DERIVATIVE WRT X =
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
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
        F4  =  F(IKLE4(IELEM))
        F5  =  F(IKLE5(IELEM))
        F6  =  F(IKLE6(IELEM))
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM) = (-Y3+Y2) *(F1-6.D0*F2+F3+4.D0*F6) * (XMUL/360.D0)
      A13(IELEM) = (-Y3+Y2) *(F1+F2-6.D0*F3+4.D0*F4) * (XMUL/360.D0)
      A14(IELEM) = (-Y3+Y2) *(F3-8.D0*F4-4.D0*F6-4.D0*F5) *(XMUL/90.D0)
      A15(IELEM) = (-Y3+Y2) *(F1-4.D0*F4-4.D0*F6-8.D0*F5) *(XMUL/90.D0)
      A16(IELEM) = (F2-4.D0*F4-8.D0*F6-4.D0*F5) *(-Y3+Y2) *(XMUL/90.D0)
      A21(IELEM) =-Y3       *(6.D0*F1-F2-F3-4.D0*F5) * (XMUL/360.D0)
      A23(IELEM) = Y3       *(F1+F2-6.D0*F3+4.D0*F4) * (XMUL/360.D0)
      A24(IELEM) = Y3       *(F3-8.D0*F4-4.D0*F6-4.D0*F5) *(XMUL/90.D0)
      A25(IELEM) = Y3       *(F1-4.D0*F4-4.D0*F6-8.D0*F5) *(XMUL/90.D0)
      A26(IELEM) = Y3       *(F2-4.D0*F4-8.D0*F6-4.D0*F5) *(XMUL/90.D0)
      A31(IELEM) = Y2       *(6.D0*F1-F2-F3-4.D0*F5) * (XMUL/360.D0)
      A32(IELEM) =-Y2       *(F1-6.D0*F2+F3+4.D0*F6) * (XMUL/360.D0)
      A34(IELEM) =-Y2       *(F3-8.D0*F4-4.D0*F6-4.D0*F5) *(XMUL/90.D0)
      A35(IELEM) =-Y2*(F1-4.D0*F4-4.D0*F6-8.D0*F5) *( XMUL/90.D0)
      A36(IELEM) =-Y2*(F2-4.D0*F4-8.D0*F6-4.D0*F5) * (XMUL/90.D0)
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
        ENDDO ! IELEM
!
        ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  CASE OF DERIVATIVE WRT Y =
!================================
!
        DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
        F4  =  F(IKLE4(IELEM))
        F5  =  F(IKLE5(IELEM))
        F6  =  F(IKLE6(IELEM))
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) = (X3-X2) *(F1-6.D0*F2+F3+4.D0*F6) * (XMUL/360.D0)
        A13(IELEM) = (X3-X2) *(F1+F2-6.D0*F3+4.D0*F4) * (XMUL/360.D0)
        A14(IELEM) = (X3-X2) *(F3-8.D0*F4-4.D0*F5-4.D0*F6) *(XMUL/90.D0)
        A15(IELEM) = (X3-X2) *(F1-4.D0*F4-8.D0*F5-4.D0*F6) *(XMUL/90.D0)
        A16(IELEM) = (X3-X2) *(F2-4.D0*F4-4.D0*F5-8.D0*F6) *(XMUL/90.D0)
        A21(IELEM) = X3      *(6.D0*F1-F2-F3-4.D0*F5) * (XMUL/360.D0)
        A23(IELEM) =-X3      *(F1+F2-6.D0*F3+4.D0*F4) * (XMUL/360.D0)
        A24(IELEM) =-X3 *(F3-8.D0*F4-4.D0*F5-4.D0*F6) * (XMUL/90.D0)
        A25(IELEM) =-X3 *(F1-4.D0*F4-8.D0*F5-4.D0*F6) * (XMUL/90.D0)
        A26(IELEM) =-X3 *(F2-4.D0*F4-4.D0*F5-8.D0*F6) * (XMUL/90.D0)
        A31(IELEM) =-X2 *(6.D0*F1-F2-F3-4.D0*F5) * (XMUL/360.D0)
        A32(IELEM) = X2 *(F1-6.D0*F2+F3+4.D0*F6) * (XMUL/360.D0)
        A34(IELEM) = X2 *(F3-8.D0*F4-4.D0*F5-4.D0*F6) * (XMUL/90.D0)
        A35(IELEM) = X2 *(F1-4.D0*F4-8.D0*F5-4.D0*F6) * (XMUL/90.D0)
        A36(IELEM) = X2 *(F2-4.D0*F4-4.D0*F5-8.D0*F6) * (XMUL/90.D0)
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM) - A31(IELEM)
        A22(IELEM) = - A12(IELEM) - A32(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
        ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,101) IELMF
101     FORMAT(1X,'MT08AC (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
201       FORMAT(1X,'MT08AC (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
