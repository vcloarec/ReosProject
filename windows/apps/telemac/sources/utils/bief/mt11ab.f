!                   *****************
                    SUBROUTINE MT11AB
!                   *****************
!
     &(  A11 , A12 , A13 , A14 ,
     &   A21 , A22 , A23 , A24 ,
     &   A31 , A32 , A33 , A34 ,
     &   XMUL,SF,F,XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,
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
!+  PSI2: BASES OF TYPE QUASI-BUBBLE TRIANGLE
!+
!+  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        09/12/94
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A14            |<--| ELEMENTS OF MATRIX
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT11AB => MT11AB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*),A34(*)
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
      DOUBLE PRECISION X2,X3,Y2,Y3,F1,F2,F3,F4
      DOUBLE PRECISION XSUR18,XSUR72,XSU216
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      XSUR18 = XMUL/18.D0
      XSUR72 = XMUL/72.D0
      XSU216 = XMUL/216.D0
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
        A12(IELEM) = (2*Y2*(-7*F2-5*F1)+Y3*(5*F3+9*F2+10*F1))*XSU216
        A13(IELEM) = (Y2*(-9*F3-5*F2-10*F1)+2*Y3*(7*F3+5*F1))*XSU216
        A14(IELEM) = (Y2*(-F2-2*F1)+Y3*(F3+2*F1))*XSUR18
        A21(IELEM) = (5*Y2*(F3-F1)+Y3*(-5*F3-10*F2-9*F1))*XSU216
        A23(IELEM) = (5*Y2*(F3-F1)+2*Y3*(-7*F3-5*F2))*XSU216
        A24(IELEM) = (Y2*(F3-F1)+Y3*(-F3-2*F2))*XSUR18
        A31(IELEM) = (Y2*(10*F3+5*F2+9*F1)+5*Y3*(-F2+F1))*XSU216
        A32(IELEM) = (2*Y2*(5*F3+7*F2)+5*Y3*(-F2+F1))*XSU216
        A34(IELEM) = (Y2*(2*F3+F2)+Y3*(-F2+F1))*XSUR18
!
!   DIAGONAL TERMS
!
        A11(IELEM) = (Y2*(9*F3-5*F2-28*F1)+Y3*(5*F3-9*F2+28*F1))*XSU216
        A22(IELEM) = (14*Y2*(F3-F1)+Y3*(-5*F3-28*F2+9*F1))*XSU216
        A33(IELEM) = (Y2*(28*F3+5*F2-9*F1)+14*Y3*(-F2+F1))*XSU216
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
        A12(IELEM) = (2*X2*(7*F2+5*F1)+X3*(-5*F3-9*F2-10*F1))*XSU216
        A13(IELEM) = (X2*(9*F3+5*F2+10*F1)+2*X3*(-7*F3-5*F1))*XSU216
        A14(IELEM) = (X2*(F2+2*F1)+X3*(-F3-2*F1))*XSUR18
        A21(IELEM) = (5*X2*(-F3+F1)+X3*(5*F3+10*F2+9*F1))*XSU216
        A23(IELEM) = (5*X2*(-F3+F1)+2*X3*(7*F3+5*F2))*XSU216
        A24(IELEM) = (X2*(-F3+F1)+X3*(F3+2*F2))*XSUR18
        A31(IELEM) = (X2*(-10*F3-5*F2-9*F1)+5*X3*(F2-F1))*XSU216
        A32(IELEM) = (2*X2*(-5*F3-7*F2)+5*X3*(F2-F1))*XSU216
        A34(IELEM) = (X2*(-2*F3-F2)+X3*(F2-F1))*XSUR18
!
!   DIAGONAL TERMS
!
        A11(IELEM) = (X2*(-9*F3+5*F2+28*F1)+X3*(-5*F3+9*F2-28*
     &                F1))*XSU216
        A22(IELEM) = (14*X2*(-F3+F1)+X3*(5*F3+28*F2-9*F1))*XSU216
        A33(IELEM) = (X2*(-28*F3-5*F2+9*F1)+14*X3*(F2-F1))*XSU216
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
!  CASE WHERE F IS OF QUASI-BUBBLE DISCRETISATION
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.12) THEN
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
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) = (Y2*(-F3+3*F4-15*F2-11*F1)+Y3*(2*F3+9*F4+
     &                6*F2+7*F1))*XSU216
        A13(IELEM) = (Y2*(-6*F3-9*F4-2*F2-7*F1)+Y3*(15*F3-3*F4
     &               +F2+11*F1))*XSU216
        A14(IELEM) = (Y2*(F3-3*F4-3*F2-7*F1)+Y3*(3*F3+3*F4-F2+
     &                                                     7*F1))*XSUR72
        A21(IELEM) = (Y2*(F3+12*F4-4*F2-9*F1)+Y3*(-2*F3-9*F4-7
     &               *F2-6*F1))*XSU216
        A23(IELEM) = (Y2*(9*F3-12*F4+4*F2-F1)+Y3*(-15*F3+3*F4-
     &                11*F2-F1))*XSU216
        A24(IELEM) = (4*Y2*(F3-F1)+Y3*(-3*F3-3*F4-7*F2+F1))*XSUR72
        A31(IELEM) = (Y2*(7*F3+9*F4+2*F2+6*F1)+Y3*(4*F3-12*F4-
     &                F2+9*F1))*XSU216
        A32(IELEM) = (Y2*(11*F3-3*F4+15*F2+F1)+Y3*(-4*F3+12*F4-
     &                9*F2+F1))*XSU216
        A34(IELEM) = (Y2*(7*F3+3*F4+3*F2-F1)+4*Y3*(-F2+F1))*XSUR72
!
!   DIAGONAL TERMS
!
        A11(IELEM) = (Y2*(4*F3+15*F4-10*F2-33*F1)+Y3*(10*F3-15
     &                                           *F4-4*F2+33*F1))*XSU216
        A22(IELEM) = (14*Y2*(F3-F1)+Y3*(-10*F3+15*F4-33*F2+4*F1))*XSU216
        A33(IELEM) = (Y2*(33*F3-15*F4+10*F2-4*F1)+14*Y3*(-F2+F1))*XSU216
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
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM) = (X2*(F3-3*F4+15*F2+11*F1)+X3*(-2*F3-9*F4-
     &                                                6*F2-7*F1))*XSU216
        A13(IELEM) = (X2*(6*F3+9*F4+2*F2+7*F1)+X3*(-15*F3+3*F4
     &                                                -F2-11*F1))*XSU216
        A14(IELEM) = (X2*(-F3+3*F4+3*F2+7*F1)+X3*(-3*F3-3*F4+F2
     &                                                    -7*F1))*XSUR72
        A21(IELEM) = (X2*(-F3-12*F4+4*F2+9*F1)+X3*(2*F3+9*F4+7
     &                                                 *F2+6*F1))*XSU216
        A23(IELEM) = (X2*(-9*F3+12*F4-4*F2+F1)+X3*(15*F3-3*F4+
     &                                                 11*F2+F1))*XSU216
        A24(IELEM) = (4*X2*(-F3+F1)+X3*(3*F3+3*F4+7*F2-F1))*XSUR72
        A31(IELEM) = (X2*(-7*F3-9*F4-2*F2-6*F1)+X3*(-4*F3+12*
     &                                               F4+F2-9*F1))*XSU216
        A32(IELEM) = (X2*(-11*F3+3*F4-15*F2-F1)+X3*(4*F3-12*F4+
     &                                                  9*F2-F1))*XSU216
        A34(IELEM) = (X2*(-7*F3-3*F4-3*F2+F1)+4*X3*(F2-F1))*XSUR72
!
!   DIAGONAL TERMS
!
        A11(IELEM) = (X2*(-4*F3-15*F4+10*F2+33*F1)+X3*(-10*F3+
     &                                         15*F4+4*F2-33*F1))*XSU216
        A22(IELEM) = (14*X2*(-F3+F1)+X3*(10*F3-15*F4+33*F2-4*F1))*XSU216
        A33(IELEM) = (X2*(-33*F3+15*F4-10*F2+4*F1)+14*X3*(F2-F1))*XSU216
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
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,101) IELMF
101     FORMAT(1X,'MT11AB (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
201   FORMAT(1X,'MT11AB (BIEF) : IMPOSSIBLE COMPONENT ',
     &          1I6,' CHECK ICOORD')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
