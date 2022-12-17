!                   *****************
                    SUBROUTINE MT11AC
!                   *****************
!
     &(  A11 , A12 , A13 , A14 , A15, A16,
     &   A21 , A22 , A23 , A24 , A25, A26,
     &   A31 , A32 , A33 , A34 , A35, A36,
     &   XMUL,SF,F,XEL,YEL,IKLE1,IKLE2,IKLE3,
     &   IKLE4,IKLE5,IKLE6,
     &   NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD=1
!+
!+             NPOIN
!+               _          /            D
!+  A(I,J)=-XMUL>_   F  *  /  PSI2(J) *  --( PSI1(K) * PSI1(I) ) D(OMEGA)
!+                    K   /OMEGA         DX
!+              K=1
!+
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
!history  A FROEHLY (MATMECA)
!+        01/07/08
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
      USE BIEF!, EX_MT11AC => MT11AC
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
      DOUBLE PRECISION XSUR30,XSUR120
      DOUBLE PRECISION XSUR90,XSUR360
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      XSUR30  = XMUL/30.D0
      XSUR120 = XMUL/120.D0
      XSUR90  = XMUL/90.D0
      XSUR360 = XMUL/360.D0
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!  CASE WHERE F IS OF TYPE P1
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.11) THEN
!
!================================
!  DERIVATIVE WRT X =
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
!   EXTRA-DIAGONAL TERMS
!
        A12(IELEM) = ( 2.D0*(F1-F2)*Y2 + (3.D0*F2-2.D0*F1-F3)*Y3)
     &             * XSUR120
        A13(IELEM) = ( 2.D0*(F3-F1)*Y3 + (2.D0*F1-3.D0*F3+F2)*Y2)
     &             * XSUR120
        A14(IELEM) = (( 4.D0*F1+F3)*Y3 + (F3-4.D0*F1-2.D0*F2)*Y2)
     &             * XSUR30
        A15(IELEM) = ((-2.D0*(F1+F2)-F3)*Y2  +
     &                ( 2.D0*(F1+F3)+F2)*Y3) * XSUR30
        A16(IELEM) = ((-4.D0*F1-F2)*Y2 + (4.D0*F1-F2+2.D0*F3)*Y3)
     &             * XSUR30
        A21(IELEM) = (      (F1-F3)*Y2 + (F3-3.D0*F1+2.D0*F2)*Y3)
     &             * XSUR120
        A23(IELEM) = (      2.D0*(F2-F3)*Y3 +        (F1-F3) *Y2)
     &             * XSUR120
        A24(IELEM) = ( 2.D0*(F3-F1)*Y2 - (        F3+4.D0*F2)*Y3)
     &             * XSUR30
        A25(IELEM) = ( 2.D0*(F3-F1)*Y2 + (F1-2.D0*F3-4.D0*F2)*Y3)
     &             * XSUR30
        A26(IELEM) = (      (F3-F1)*Y2 - (F1+2.D0*F2+2.D0*F3)*Y3)
     &             * XSUR30
        A31(IELEM) = (      (F2-F1)*Y3 + (3.D0*F1-F2-2.D0*F3)*Y2)
     &             * XSUR120
        A32(IELEM) = ( 2.D0*(F2-F3)*Y2 +             (-F1+F2)*Y3)
     &             * XSUR120
        A34(IELEM) = (      (F1-F2)*Y3 + (   F1+2.D0*(F2+F3))*Y2)
     &             * XSUR30
        A35(IELEM) = ( 2.D0*(F1-F2)*Y3 + (4.D0*F3-F1+2.D0*F2)*Y2)
     &             * XSUR30
        A36(IELEM) = (( 4.D0*F3+F2)*Y2 +         2.D0*(F1-F2)*Y3)
     &             * XSUR30
!
!   DIAGONAL TERMS
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
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
!
!   EXTRA-DIAGONAL TERMS
!
        A12(IELEM) = (  2.D0*(F2-F1)*X2 + (2.D0*F1-3.D0*F2+F3)*X3)
     &             * XSUR120
        A13(IELEM) = (  2.D0*(F1-F3)*X3 - (2.D0*F1-3.D0*F3+F2)*X2)
     &             * XSUR120
        A14(IELEM) = ((-4.D0*F1-F3 )*X3 + (4.D0*F1-F3+2.D0*F2)*X2)
     &             * XSUR30
        A15(IELEM) = (( 2.D0*(F1+F2)+F3)*X2  -
     &                ( 2.D0*(F1+F3)+F2)*X3) * XSUR30
        A16(IELEM) = ((4.D0*F1+F2  )*X2 + (F2-4.D0*F1-2.D0*F3)*X3)
     &             * XSUR30
        A21(IELEM) = (       (F3-F1)*X2 + (3.D0*F1-2.D0*F2-F3)*X3)
     &             * XSUR120
        A23(IELEM) = (       (F3-F1)*X2 +         2.D0*(F3-F2)*X3)
     &             * XSUR120
        A24(IELEM) = (  2.D0*(F1-F3)*X2 + (        F3+4.D0*F2)*X3)
     &             * XSUR30
        A25(IELEM) = (  2.D0*(F1-F3)*X2 + (2.D0*F3-F1+4.D0*F2)*X3)
     &             * XSUR30
        A26(IELEM) = (       (F1-F3)*X2 + (   F1+2.D0*(F2+F3))*X3)
     &             * XSUR30
        A31(IELEM) = (       (F1-F2)*X3 + (F2+2.D0*F3-3.D0*F1)*X2)
     &             * XSUR120
        A32(IELEM) = (  2.D0*(F3-F2)*X2 + (             F1-F2)*X3)
     &             * XSUR120
        A34(IELEM) = (       (F2-F1)*X3 - (F1+2.D0*F2+2.D0*F3)*X2)
     &             * XSUR30
        A35(IELEM) = (  2.D0*(F2-F1)*X3 - (4.D0*F3-F1+2.D0*F2)*X2)
     &             * XSUR30
        A36(IELEM) = ((-4.D0*F3-F2 )*X2 +         2.D0*(F2-F1)*X3)
     &             * XSUR30
!
!   DIAGONAL TERMS
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
!  CASE WHERE F IS OF TYPE P2
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.13) THEN
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
            F1  =  F(IKLE1(IELEM))
            F2  =  F(IKLE2(IELEM))
            F3  =  F(IKLE3(IELEM))
            F4  =  F(IKLE4(IELEM))
            F5  =  F(IKLE5(IELEM))
            F6  =  F(IKLE6(IELEM))
!
!   EXTRADIAGONAL TERMS
!
            A12(IELEM) = ((3.D0*F2-6.D0*F1-8.D0*(F6-F4)-F3+4.D0*F5)*Y3
     &                 +   6.D0*(F1-F2)*Y2 ) * XSUR360
            A13(IELEM) = ((8.D0*(F4-F6)-4.D0*F5+F2+6.D0*F1-3.D0*F3)*Y2
     &                 +   6.D0*(F3-F1)*Y3 ) * XSUR360
            A14(IELEM) = ((-16.D0*F4+4.D0*(F5+F6)-6.D0*F1-F3      )*Y2
     &                 +  (6.D0*F1-2.D0*F2+4.D0*F4+8.D0*F6-F3)*Y3)
     &                  * XSUR90
            A15(IELEM) = ((F3-8.D0*F4-4.D0*(F5+F6))*Y2
     &                 +  (4.D0*(F4+F5)+8.D0*F6-F2)*Y3) * XSUR90
            A16(IELEM) = ((F2+6.D0*F1+16.D0*F6-4.D0*(F4+F5)       )*Y3
     &                 -  (8.D0*F4-F2+6.D0*F1-2.D0*F3+4.D0*F6)*Y2)
     &                  * XSUR90
            A21(IELEM) = ((8.D0*(F4-F5)-3.D0*F1-F3+4.D0*F6        )*Y2
     &                 - (3.D0*F1-6.D0*F2+8.D0*(F4-F5)+4.D0*F6-F3 )*Y3)
     &                 * XSUR360
            A23(IELEM) = ((8.D0*(F4-F5)+F1+3.D0*F3-4.D0*F6        )*Y2
     &                 +  6.D0*(F2-F3)*Y3 ) * XSUR360
            A24(IELEM) = ((12.D0*(F5-F4)-2.D0*(F1+F3)+4.D0*F6     )*Y2
     &                 + (2.D0*F1-4.D0*F4+F3-6.D0*F2-8.D0*F5)*Y3 )
     &                 * XSUR90
            A25(IELEM) = ((12.D0*(F5-F4)+2.D0*(F1+F3)-4.D0*F6     )*Y2
     &                 +  (-F1+4.D0*(F4+F6)-6.D0*F2-16.D0*F5)*Y3 )
     &                 * XSUR90
            A26(IELEM) = ((F1-8.D0*F5-4.D0*(F6+F4)                )*Y3
     &                 - (4.D0*(F4-F5)+F1-F3)*Y2                 )
     &                 * XSUR90
            A31(IELEM) = ((4.D0*F4-8.D0*(F5-F6)+3.D0*F1-F2-6.D0*F3)*Y2
     &                 +  (3.D0*F1-8.D0*(F6-F5)-4.D0*F4+F2)*Y3 )
     &                 * XSUR360
            A32(IELEM) = ((4.D0*F4-F1-8.D0*(F6-F5)-3.D0*F2)*Y3
     &                 +  6.D0*(F2-F3)*Y2) * XSUR360
            A34(IELEM) = ((4.D0*(F4+F6)-F1+8.D0*F5)*Y2
     &                 +  (F1-4.D0*(F5-F6)-F2)*Y3 ) * XSUR90
            A35(IELEM) = ((16.D0*F5-4.D0*(F4+F6)+F1+6.D0*F3)*Y2
     &                 +  (-2.D0*(F1+F2)+4.D0*F4+12.D0*(F6-F5))*Y3)
     &                 * XSUR90
            A36(IELEM) = ((8.D0*F5-F2-2.D0*F1+6.D0*F3+4.D0*F6)*Y2
     &                 +  (2.D0*(F2+F1)+12.D0*(F6-F5)-4.D0*F4)*Y3)
     &                 * XSUR90
!
!   DIAGONAL TERMS
!
            A11(IELEM) = ((24.D0*(F6-F1)-5.D0*F3+4.D0*F5+F2)*Y2
     &                 +  (24.D0*(F1-F4)+5.D0*F2-4.D0*F5-F3)*Y3)*XSUR360
            A22(IELEM) = ((6.D0*(F1-F3)+24.D0*(F5-F4)      )*Y2
     &                 +  (24.D0*(F4-F2)+4.D0*F6+F3-5.D0*F1)*Y3)*XSUR360
            A33(IELEM) = ((24.D0*(F3-F6)+5.D0*F1-4.D0*F4-F2)*Y2
     &                 +  (6.D0*(F2-F1)+24.D0*(F6-F5)      )*Y3)*XSUR360
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
            F1  =  F(IKLE1(IELEM))
            F2  =  F(IKLE2(IELEM))
            F3  =  F(IKLE3(IELEM))
            F4  =  F(IKLE4(IELEM))
            F5  =  F(IKLE5(IELEM))
            F6  =  F(IKLE6(IELEM))
!
!   EXTRADIAGONAL TERMS
!
            A12(IELEM) = ((F3+8.D0*(F6-F4)+6.D0*F1-3.D0*F2-4.D0*F5)*X3
     &                 +   6.D0*(F2-F1)*X2 ) * XSUR360
            A13(IELEM) = ((8.D0*(F6-F4)-6.D0*F1+3.D0*F3+4.D0*F5-F2)*X2
     &                 -   6.D0*(F3-F1)*X3 ) * XSUR360
            A14(IELEM) = ((6.D0*F1+F3-4.D0*(F6+F5)+16.D0*F4  )*X2
     &                 +  (F3-8.D0*F6-6.D0*F1+2.D0*F2-4.D0*F4)*X3)
     &                 *  XSUR90
            A15(IELEM) = ((4.D0*(F6+F5)+8.D0*F4-F3)*X2
     &                 +  (F2-8.D0*F6-4.D0*(F4+F5))*X3 ) * XSUR90
            A16(IELEM) = ((6.D0*F1-2.D0*F3+4.D0*F6+8.D0*F4-F2)*X2
     &                 +  (-16.D0*F6-6.D0*F1-F2+4.D0*(F5+F4) )*X3)
     &                  * XSUR90
            A21(IELEM) = ((3.D0*F1+F3-4.D0*F6-8.D0*(F4-F5))*X2
     &                 +  (-F3+4.D0*F6+3.D0*F1-6.D0*F2+8.D0*(F4-F5))*X3)
     &                 *  XSUR360
            A23(IELEM) = ((4.D0*F6-F1-3.D0*F3-8.D0*(F4-F5))*X2
     &                 -   6.D0*(F2-F3)*X3 ) * XSUR360
            A24(IELEM) = ((2.D0*(F1+F3)-4.D0*F6+12.D0*(F4-F5) )*X2
     &                 +  (-F3-2.D0*F1+6.D0*F2+4.D0*F4+8.D0*F5)*X3)
     &                 * XSUR90
            A25(IELEM) = ((4.D0*F6-2.D0*(F1+F3)+12.D0*(F4-F5) )*X2
     &                 -  (4.D0*F6-F1-6.D0*F2+4.D0*F4-16.D0*F5)*X3)
     &                 * XSUR90
            A26(IELEM) = ((F1-F3+4.D0*(F4-F5)     )*X2
     &                 +  (4.D0*(F6+F4)-F1+8.D0*F5)*X3 )
     &                 * XSUR90
            A31(IELEM) = ((F2-3.D0*F1+6.D0*F3-8.D0*(F6-F5)-4.D0*F4)*X2
     &                 -  (-8.D0*F6+3.D0*F1+F2-4.D0*F4+8.D0*F5    )*X3)
     &                 * XSUR360
            A32(IELEM) = ((F1-8.D0*(F5-F6)+3.D0*F2-4.D0*F4)*X3
     &                 +   6.D0*(F3-F2)*X2 ) * XSUR360
            A34(IELEM) = ((F1-4.D0*(F6+F4)-8.D0*F5)*X2
     &                 +  (-4.D0*(F6-F5)-F1+F2    )*X3 ) * XSUR90
            A35(IELEM) = ((4.D0*(F6+F4)-F1-6.D0*F3-16.D0*F5  )*X2
     &                 -  (12.D0*(F6-F5)-2.D0*(F1+F2)+4.D0*F4)*X3)
     &                 * XSUR90
            A36(IELEM) = ((2.D0*F1-6.D0*F3-4.D0*F6-8.D0*F5+F2)*X2
     &                 +  (12.D0*(F5-F6)-2.D0*(F1+F2)+4.D0*F4)*X3)
     &                 * XSUR90
!
!   DIAGONAL TERMS
!
            A11(IELEM) = ((24.D0*(F1-F6)-4.D0*F5+5.D0*F3-F2)*X2
     &                 +  (24.D0*(F4-F1)-5.D0*F2+4.D0*F5+F3)*X3)*XSUR360
            A22(IELEM) = ((24.D0*(F4-F5)+6.D0*(F3-F1)      )*X2
     &                 +  (5.D0*F1+24.D0*(F2-F4)-4.D0*F6-F3)*X3)*XSUR360
            A33(IELEM) = ((4.D0*F4-5.D0*F1+24.D0*(F6-F3)+F2)*X2
     &                 +  (6.D0*(F1-F2)+24.D0*(F5-F6)      )*X3)*XSUR360
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
101     FORMAT(1X,'MT11AC (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
201   FORMAT(1X,'MT11AC (BIEF) : IMPOSSIBLE COMPONENT ',
     &          1I6,' CHECK ICOORD')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
