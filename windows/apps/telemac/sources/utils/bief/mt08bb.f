!                   *****************
                    SUBROUTINE MT08BB
!                   *****************
!
     &(  A11 , A12 , A13 , A14 ,
     &   A21 , A22 , A23 , A24 ,
     &   A31 , A32 , A33 , A34 ,
     &   A41 , A42 , A43 , A44 ,
     &   XMUL,SF,F,XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD = 1
!+
!+                  /                     D
!+  A(I,J)=-XMUL   /  PSI2(J) *    F    * --( PSI1(I) ) D(OMEGA)
!+                /OMEGA                  DX
!+
!+  BEWARE THE MINUS SIGN !!
!+
!+  PSI1: BASES OF TYPE P1 TRIANGLE
!+  PSI2: BASES OF TYPE IELM2
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
!| A41            |<--| ELEMENTS OF MATRIX
!| A42            |<--| ELEMENTS OF MATRIX
!| A43            |<--| ELEMENTS OF MATRIX
!| A44            |<--| ELEMENTS OF MATRIX
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
      USE BIEF, EX_MT08BB => MT08BB
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
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*),A44(*)
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
        A12(IELEM) = (2*Y2*(-F3-7*F2-4*F1)+Y3*(F3+7*F2+4*F1))*XMUL/216
        A13(IELEM) = (Y2*(-7*F3-F2-4*F1)+2*Y3*(7*F3+F2+4*F1))*XMUL/216
        A14(IELEM) = (Y2*(-3*F3-4*F2-5*F1)+Y3*(4*F3+3*F2+5*F1))*XMUL/72
        A21(IELEM) = (Y2*(-F3-4*F2-7*F1)+Y3*(-F3-4*F2-7*F1))*XMUL/216
        A23(IELEM) = (Y2*(7*F3+4*F2+F1)+2*Y3*(-7*F3-4*F2-F1))*XMUL/216
        A24(IELEM) = (Y2*(F3-F1)+Y3*(-4*F3-5*F2-3*F1))*XMUL/72
        A31(IELEM) = (Y2*(4*F3+F2+7*F1)+Y3*(4*F3+F2+7*F1))*XMUL/216
        A32(IELEM) = (2*Y2*(4*F3+7*F2+F1)+Y3*(-4*F3-7*F2-F1))*XMUL/216
        A34(IELEM) = (Y2*(5*F3+4*F2+3*F1)+Y3*(-F2+F1))*XMUL/72
        A41(IELEM) = (Y2*(F3+4*F2+7*F1)+Y3*(-4*F3-F2-7*F1))*XMUL/72
        A42(IELEM) = (3*Y2*(-F3+F1)+Y3*(4*F3+7*F2+F1))*XMUL/72
        A43(IELEM) = (Y2*(-7*F3-4*F2-F1)+3*Y3*(F2-F1))*XMUL/72
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM)  - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM)  - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM)  - A23(IELEM) - A43(IELEM)
        A44(IELEM) = - A14(IELEM)  - A24(IELEM) - A34(IELEM)
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
        A12(IELEM) = (2*X2*(F3+7*F2+4*F1)+X3*(-F3-7*F2-4*F1))*XMUL/216
        A13(IELEM) = (X2*(7*F3+F2+4*F1)+2*X3*(-7*F3-F2-4*F1))*XMUL/216
        A14(IELEM) = (X2*(3*F3+4*F2+5*F1)+X3*(-4*F3-3*F2-5*F1))*XMUL/72
        A21(IELEM) = (X2*(F3+4*F2+7*F1)+X3*(F3+4*F2+7*F1))*XMUL/216
        A23(IELEM) = (X2*(-7*F3-4*F2-F1)+2*X3*(7*F3+4*F2+F1))*XMUL/216
        A24(IELEM) = (X2*(-F3+F1)+X3*(4*F3+5*F2+3*F1))*XMUL/72
        A31(IELEM) = (X2*(-4*F3-F2-7*F1)+X3*(-4*F3-F2-7*F1))*XMUL/216
        A32(IELEM) = (2*X2*(-4*F3-7*F2-F1)+X3*(4*F3+7*F2+F1))*XMUL/216
        A34(IELEM) = (X2*(-5*F3-4*F2-3*F1)+X3*(F2-F1))*XMUL/72
        A41(IELEM) = (X2*(-F3-4*F2-7*F1)+X3*(4*F3+F2+7*F1))*XMUL/72
        A42(IELEM) = (3*X2*(F3-F1)+X3*(-4*F3-7*F2-F1))*XMUL/72
        A43(IELEM) = (X2*(7*F3+4*F2+F1)+3*X3*(-F2+F1))*XMUL/72
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM)  - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM)  - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM)  - A23(IELEM) - A43(IELEM)
        A44(IELEM) = - A14(IELEM)  - A24(IELEM) - A34(IELEM)
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
        A12(IELEM) = (2*Y2*(-F4-2*F2-F1)+Y3*(F4+2*F2+F1))*XMUL/72
        A13(IELEM) = (Y2*(-2*F3-F4-F1)+2*Y3*(2*F3+F4+F1))*XMUL/72
        A14(IELEM) = (Y2*(-F3-6*F4-2*F2-3*F1)+Y3*(2*F3+6*F4+F2+
     &                3*F1))*XMUL/72
        A21(IELEM) = (Y2*(-F4-F2-2*F1)+Y3*(-F4-F2-2*F1))*XMUL/72
        A23(IELEM) = (Y2*(2*F3+F4+F2)+2*Y3*(-2*F3-F4-F2))*XMUL/72
        A24(IELEM) = (Y2*(F3-F1)+Y3*(-2*F3-6*F4-3*F2-F1))*XMUL/72
        A31(IELEM) = (Y2*(F3+F4+2*F1)+Y3*(F3+F4+2*F1))*XMUL/72
        A32(IELEM) = (2*Y2*(F3+F4+2*F2)+Y3*(-F3-F4-2*F2))*XMUL/72
        A34(IELEM) = (Y2*(3*F3+6*F4+2*F2+F1)+Y3*(-F2+F1))*XMUL/72
        A41(IELEM) = (Y2*(F4+F2+2*F1)+Y3*(-F3-F4-2*F1))*XMUL/24
        A42(IELEM) = (Y2*(-F3+F1)+Y3*(F3+F4+2*F2))*XMUL/24
        A43(IELEM) = (Y2*(-2*F3-F4-F2)+Y3*(F2-F1))*XMUL/24
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM)  - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM)  - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM)  - A23(IELEM) - A43(IELEM)
        A44(IELEM) = - A14(IELEM)  - A24(IELEM) - A34(IELEM)
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
        A12(IELEM) = (2*X2*(F4+2*F2+F1)+X3*(-F4-2*F2-F1))*XMUL/72
        A13(IELEM) = (X2*(2*F3+F4+F1)+2*X3*(-2*F3-F4-F1))*XMUL/72
        A14(IELEM) = (X2*(F3+6*F4+2*F2+3*F1)+X3*(-2*F3-6*F4-F2-
     &                3*F1))*XMUL/72
        A21(IELEM) =  (X2*(F4+F2+2*F1)+X3*(F4+F2+2*F1))*XMUL/72
        A23(IELEM) =  (X2*(-2*F3-F4-F2)+2*X3*(2*F3+F4+F2))*XMUL/72
        A24(IELEM) =  (X2*(-F3+F1)+X3*(2*F3+6*F4+3*F2+F1))*XMUL/72
        A31(IELEM) =  (X2*(-F3-F4-2*F1)+X3*(-F3-F4-2*F1))*XMUL/72
        A32(IELEM) =  (2*X2*(-F3-F4-2*F2)+X3*(F3+F4+2*F2))*XMUL/72
        A34(IELEM) =  (X2*(-3*F3-6*F4-2*F2-F1)+X3*(F2-F1))*XMUL/72
        A41(IELEM) =  (X2*(-F4-F2-2*F1)+X3*(F3+F4+2*F1))*XMUL/24
        A42(IELEM) =  (X2*(F3-F1)+X3*(-F3-F4-2*F2))*XMUL/24
        A43(IELEM) =  (X2*(2*F3+F4+F2)+X3*(-F2+F1))*XMUL/24
!
!   DIAGONAL TERMS
!   (SUM OF EACH LINE IN THE MATRIX IS 0)
!
        A11(IELEM) = - A21(IELEM)  - A31(IELEM) - A41(IELEM)
        A22(IELEM) = - A12(IELEM)  - A32(IELEM) - A42(IELEM)
        A33(IELEM) = - A13(IELEM)  - A23(IELEM) - A43(IELEM)
        A44(IELEM) = - A14(IELEM)  - A24(IELEM) - A34(IELEM)
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
101     FORMAT(1X,'MT08BB (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F : ',1I6,' NOT AVAILABLE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
201   FORMAT(1X,'MT08BB (BIEF) : IMPOSSIBLE COMPONENT ',
     &       1I6,' CHECK ICOORD')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
