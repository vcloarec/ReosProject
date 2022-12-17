!                   *****************
                    SUBROUTINE MT99BB
!                   *****************
!
     &( A11 , A12 , A13 , A14 ,
     &  A21 , A22 , A23 , A24 ,
     &  A31 , A32 , A33 , A34 ,
     &  A41 , A42 , A43 , A44 ,
     &  XMUL,SF,F,XEL,YEL,
     &  SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,FORMUL,TDIA,TEXT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                       /     DF             D
!+          A    = XMUL /  F * -- * PSI2(J) * --( PSI1(I) ) D(O
!+           I J       /S      DX             DX
!+
!+     BY ELEMENT - THE ELEMENT IS THE QUASI-BUBBLE TRIANGLE
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        28/11/94
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
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| TDIA           |<--| TYPE OF DIAGONAL
!| TEXT           |<--| TYPE OF OFF-DIAGONAL TERMS
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT99BB => MT99BB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      CHARACTER(LEN=1), INTENT(INOUT) :: TDIA,TEXT
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
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
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM
      DOUBLE PRECISION F1,F2,F3,X2,X3,Y2,Y3,S
!
!=======================================================================
!
!     ONLY ONE CASE IMPLEMENTED FOR NOW: LINEAR F
!
      IF(SF%ELM.NE.11) THEN
!
        WRITE(LU,2001) SF%ELM
2001    FORMAT(1X,'MT99BB (BIEF) : TYPE OF F:',I6,' NOT IMPLEMENTED')
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(FORMUL(8:16).EQ.'     0XX0') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     & (7*Y2*F1+3*Y2*F2+2*Y2*F3
     & -7*Y3*F1-2*Y3*F2-3*Y3*F3)/S/144
!
      A12(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2+Y3)*
     & (7*F1+4*F2+F3)/S/432
!
      A13(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2+Y3)*
     & (4*F3+7*F1+F2)/S/432
!
      A14(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     & (7*Y2*F1+4*Y2*F2+Y2*F3-4*Y3*F3-7*Y3*F1-Y3*F2)/S/144
!
      A21(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(2*Y2-Y3)*
     &  (4*F1+7*F2+F3)/S/432
!
      A22(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (Y2*F1-Y2*F3+2*Y3*F1+7*Y3*F2+3*Y3*F3)/S/144
!
      A23(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(2*Y2-Y3)*
     &  (7*F2+4*F3+F1)/S/432
!
      A24(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (3*Y2*F1-3*Y2*F3+7*Y3*F2+4*Y3*F3+Y3*F1)/S/144
!
      A31(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2-2*Y3)*
     &  (7*F3+4*F1+F2)/S/432
!
      A32(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*(Y2-2*Y3)*
     &  (4*F2+F1+7*F3)/S/432
!
      A33(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (3*Y2*F2+2*Y2*F1+7*Y2*F3-Y3*F2+Y3*F1)/S/144
!
      A34(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (-3*Y3*F2+3*Y3*F1+4*Y2*F2+Y2*F1+7*Y2*F3)/S/144
!
      A41(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     & (5*Y2*F1+4*Y2*F2+3*Y2*F3-5*Y3*F1-3*Y3*F2-4*Y3*F3)
     & /S/144
!
      A42(IELEM) = (Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (Y2*F1-Y2*F3+3*Y3*F1+5*Y3*F2+4*Y3*F3)/S/144
!
      A43(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)*
     &  (4*Y2*F2+5*Y2*F3+3*Y2*F1-Y3*F2+Y3*F1)/S/144
!
      A44(IELEM) = -(Y2*F1-Y3*F1+Y3*F2-Y2*F3)**2/S/48
!
!
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     0YY0') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
!  ELEMENTS OUTSIDE OF THE DIAGONAL
!
      A11(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-7*X2*F1-
     & 3*X2*F2-2*X2*F3+7*X3*F1+2*X3*F2+3*X3*F3)/S/144
      A12(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(X2+X3)*
     & (7*F1+4*F2+F3)/S/432
      A13(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(X2+X3)*
     & (4*F3+7*F1+F2)/S/432
      A14(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-7*X2*F1-
     & 4*X2*F2-X2*F3+4*X3*F3+7*X3*F1+X3*F2)/S/144
      A21(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-2*X2+X3)*
     & (4*F1+7*F2+F3)/S/432
      A22(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(X2*F1-X2*F3+2*
     & X3*F1+7*X3*F2+3*X3*F3)/S/144
      A23(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-2*X2+X3)*
     & (7*F2+4*F3+F1)/S/432
      A24(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(3*X2*F1-3*X2*F3+
     & 7*X3*F2+4*X3*F3+X3*F1)/S/144
      A31(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-X2+2*X3)*
     & (7*F3+4*F1+F2)/S/432
      A32(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-X2+2*X3)*
     & (4*F2+F1+7*F3)/S/432
      A33(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-3*X2*F2-2*X2*F1-
     & 7*X2*F3+X3*F2-X3*F1)/S/144
      A34(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(3*X3*F2-3*X3*F1-
     & 4*X2*F2-X2*F1-7*X2*F3)/S/144
      A41(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)*(-5*X2*F1-4*X2*F2-
     & 3*X2*F3+5*X3*F1+3*X3*F2+4*X3*F3)/S/144
      A42(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(X2*F1-X2*F3+3*X3*
     & F1+5*X3*F2+4*X3*F3)/S/144
      A43(IELEM) = (X2*F1-X3*F1+X3*F2-X2*F3)*(-4*X2*F2-5*X2*F3-
     & 3*X2*F1+X3*F2-X3*F1)/S/144
      A44(IELEM) = -(X2*F1-X3*F1+X3*F2-X2*F3)**2/S/48
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     XX00') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
!
      A11(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/36
      A12(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A13(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A14(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A21(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A22(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/36
      A23(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A24(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A31(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A32(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/144
      A33(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/36
      A34(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A41(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A42(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A43(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/72
      A44(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2/S/24
!
!
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     0X0Y') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-7*X2*F1-3*X2*F2-2*X2*F3+7*X3*F1+2*X3*F2+3*X3*F3)
     & /S/144
!
      A12(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-2*X2+X3)*
     & (4*F1+7*F2+F3)/S/432
!
      A13(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2+2*X3)*
     & (7*F3+4*F1+F2)/S/432
!
      A14(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-5*X2*F1-4*X2*F2-
     & 3*X2*F3+5*X3*F1+3*X3*F2+4*X3*F3)/S/144
!
      A21(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2+X3)*
     & (7*F1+4*F2+F3)/S/432
!
      A22(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X2*F3+2*X3*F1+
     & 7*X3*F2+3*X3*F3)/S/144
!
      A23(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2+2*X3)*
     & (4*F2+F1+7*F3)/S/432
!
      A24(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X2*F3+3*X3*F1+
     & 5*X3*F2+4*X3*F3)/S/144
!
      A31(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2+X3)*
     & (4*F3+7*F1+F2)/S/432
!
      A32(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-2*X2+X3)*
     & (7*F2+4*F3+F1)/S/432
!
      A33(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(3*X2*F2+2*X2*F1+
     & 7*X2*F3-X3*F2+X3*F1)/S/144
!
      A34(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(4*X2*F2+5*X2*F3+
     & 3*X2*F1-X3*F2+X3*F1)/S/144
!
      A41(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-7*X2*F1-4*X2*F2-
     & X2*F3+4*X3*F3+7*X3*F1+X3*F2)/S/144
!
      A42(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(3*X2*F1-3*X2*F3+
     & 7*X3*F2+4*X3*F3+X3*F1)/S/144
!
      A43(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-3*X3*F2+3*X3*F1+
     & 4*X2*F2+X2*F1+7*X2*F3)/S/144
!
      A44(IELEM) = (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2*F1+X2*F3-X3*F2+
     & X3*F1)/S/48
!
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     XY00') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S =  SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/36
!
      A12(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
!
      A13(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
!
      A14(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
!
      A21(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
!
      A22(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/36
!
      A23(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
!
      A24(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
!
      A31(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
!
      A32(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/144
!
      A33(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/36
!
      A34(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
!
      A41(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
!
      A42(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
!
      A43(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/72
!
      A44(IELEM) = -(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*
     & (-X2*F1+X3*F1-X3*F2+X2*F3)/S/24
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     YY00') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/36
      A12(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A13(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A14(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A21(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A22(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/36
      A23(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A24(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A31(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A32(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/144
      A33(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/36
      A34(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A41(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A42(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A43(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/72
      A44(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)**2/S/24
!
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     0Y0X') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*(-7*Y2*F1-
     & 3*Y2*F2-2*Y2*F3+7*Y3*F1+2*Y3*F2+3*Y3*F3)/S/144
!
      A12(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-2*Y2+Y3)*(4*F1+7*F2+F3)/S/432
!
      A13(IELEM) = -(-Y2+2*Y3)*(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (7*F3+4*F1+F2)/S/432
!
      A14(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-5*Y2*F1-4*Y2*F2-3*Y2*F3+5*Y3*F1+3*Y3*F2+4*Y3*F3)/S/144
!
      A21(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(Y2+Y3)*
     & (7*F1+4*F2+F3)/S/432
!
      A22(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(Y2*F1-
     & Y2*F3+2*Y3*F1+7*Y3*F2+3*Y3*F3)/S/144
!
      A23(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-Y2+2*Y3)*(4*F2+F1+7*F3)/S/432
!
      A24(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (Y2*F1-Y2*F3+3*Y3*F1+5*Y3*F2+4*Y3*F3)/S/144
!
      A31(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*(Y2+Y3)*
     & (4*F3+7*F1+F2)/S/432
!
      A32(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(-2*Y2+Y3)*
     & (7*F2+4*F3+F1)/S/432
!
      A33(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (3*Y2*F2+2*Y2*F1+7*Y2*F3-Y3*F2+Y3*F1)/S/144
!
      A34(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (4*Y2*F2+5*Y2*F3+3*Y2*F1-Y3*F2+Y3*F1)/S/144
!
      A41(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(-7*Y2*F1-4*Y2*F2-
     & Y2*F3+4*Y3*F3+7*Y3*F1+Y3*F2)/S/144
!
      A42(IELEM) = -(-X2*F1+X3*F1-X3*F2+X2*F3)*(3*Y2*F1-3*Y2*F3+
     & 7*Y3*F2+4*Y3*F3+Y3*F1)/S/144
!
      A43(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*(4*Y2*F2+Y2*F1+
     & 7*Y2*F3-3*Y3*F2+3*Y3*F1)/S/144
!
      A44(IELEM) = (-X2*F1+X3*F1-X3*F2+X2*F3)*
     & (-Y2*F1+Y3*F1-Y3*F2+Y2*F3)/S/48
!
!
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'00XX+00YY') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM) = (-56*F2**2*X2*X3+25*F2*F3*X3**2-56*
     & F3**2*Y2*Y3-104*F1**2*X2*X3+37*F1*F2*Y3**2+37*F1*F2*X3**2+
     & 25*F2*F3*Y2**2+25*F2*F3*Y3**2-56*F2**2*Y2*Y3+73*F1*
     & F2*Y2**2+37*F1*F3*Y2**2-104*F1**2*Y2*Y3+73*F1*F3*Y3**2+
     & 37*F1*F3*X2**2+25*F2*F3*X2**2-56*F3**2*X2*X3+73*F1*F2*X2**2+
     & 73*F1*F3*X3**2+17*F3**2*Y2**2-40*F2*F3*Y2*Y3+65*F1**2*
     & X3**2+65*F1**2*X2**2-40*F2*F3*X2*X3+53*F3**2*Y3**2+
     & 17*F2**2*X3**2-88*F1*F3*Y2*Y3-88*F1*F3*X2*X3+17*F3**2*X2**2+
     & 53*F3**2*X3**2+53*F2**2*X2**2+53*F2**2*Y2**2+17*F2**2*Y3**2-
     & 88*F1*F2*Y2*Y3-88*F1*F2*X2*X3+65*F1**2*Y2**2+65*F1**2*Y3**2)
     & /S/648
!
      A12(IELEM) = -(13*F1**2+17*F1*F2+5*F1*F3+F3**2+
     & 13*F2**2+5*F2*F3)*
     & (-2*Y2**2-Y2*Y3+Y3**2-2*X2**2-X2*X3+X3**2)/S/648
!
      A13(IELEM) = (13*F1**2+5*F1*F2+17*F1*F3+13*F3**2+F2**2+5*F2*F3)*
     & (-Y2**2+Y2*Y3+2*Y3**2-X2**2+X2*X3+2*X3**2)/S/648
!
      A14(IELEM) = -(-7*F2**2*X2*X3+5*F2*F3*X3**2-7*F3**2*Y2*Y3-13*
     & F1**2*X2*X3+5*F1*F2*Y3**2+5*F1*F2*X3**2+5*F2*F3*Y2**2+5*F2*F3*
     & Y3**2-7*F2**2*Y2*Y3+17*F1*F2*Y2**2+5*F1*F3*Y2**2-
     & 13*F1**2*Y2*Y3+
     & 17*F1*F3*Y3**2+5*F1*F3*X2**2+5*F2*F3*X2**2-7*F3**2*X2*X3+17*F1*
     & F2*X2**2+17*F1*F3*X3**2+F3**2*Y2**2-5*F2*F3*Y2*Y3+
     & 13*F1**2*X3**2+
     & 13*F1**2*X2**2-5*F2*F3*X2*X3+13*F3**2*Y3**2+F2**2*X3**2-
     & 11*F1*F3*Y2*Y3-11*F1*F3*X2*X3+F3**2*X2**2+13*F3**2*X3**2+
     & 13*F2**2*X2**2+13*F2**2*Y2**2+F2**2*Y3**2-11*F1*F2*Y2*Y3-
     & 11*F1*F2*X2*X3+13*F1**2*Y2**2+13*F1**2*Y3**2)/S/108
!
      A22(IELEM) = (-26*F2**2*X2*X3+73*F2*F3*X3**2-50*F3**2*Y2*Y3+22*
     & F1**2*X2*X3+37*F1*F2*Y3**2+37*F1*F2*X3**2+22*F2*F3*Y2**2+
     & 73*F2*F3*Y3**2-26*F2**2*Y2*Y3+22*F1*F2*Y2**2+10*F1*F3*Y2**2+
     & 22*F1**2*Y2*Y3+25*F1*F3*Y3**2+10*F1*F3*X2**2+22*F2*F3*X2**2-
     & 50*F3**2*X2*X3+22*F1*F2*X2**2+25*F1*F3*X3**2+14*F3**2*Y2**2-
     & 58*F2*F3*Y2*Y3+17*F1**2*X3**2+14*F1**2*X2**2-58*F2*F3*X2*X3+
     & 53*F3**2*
     & Y3**2+65*F2**2*X3**2-10*F1*F3*Y2*Y3-10*F1*F3*X2*X3+
     & 14*F3**2*X2**2+
     & 53*F3**2*X3**2+26*F2**2*X2**2+26*F2**2*Y2**2+65*F2**2*Y3**2+
     & 14*F1*F2*Y2*Y3+14*F1*F2*X2*X3+14*F1**2*Y2**2+
     & 17*F1**2*Y3**2)/S/648
!
      A23(IELEM) = (13*F3**2+17*F2*F3+5*F1*F3+13*F2**2+
     & F1**2+5*F1*F2)*
     & (2*Y2**2-5*Y2*Y3+2*Y3**2+2*X2**2-5*X2*X3+2*X3**2)/S/648
!
      A24(IELEM) = -(-13*F2**2*X2*X3+17*F2*F3*X3**2-
     & 19*F3**2*Y2*Y3+5*F1**2*
     & X2*X3+5*F1*F2*Y3**2+5*F1*F2*X3**2+
     & 11*F2*F3*Y2**2+17*F2*F3*Y3**2-
     & 13*F2**2*Y2*Y3+11*F1*F2*Y2**2+5*F1*F3*Y2**2+5*F1**2*
     & Y2*Y3+5*F1*F3*
     & Y3**2+5*F1*F3*X2**2+11*F2*F3*X2**2-19*F3**2*X2*X3+
     & 11*F1*F2*X2**2+
     & 5*F1*F3*X3**2+7*F3**2*Y2**2-23*F2*F3*Y2*Y3+
     & F1**2*X3**2+7*F1**2*
     & X2**2-23*F2*F3*X2*X3+13*F3**2*Y3**2+13*F2**2*X3**2-
     & 5*F1*F3*Y2*Y3-
     & 5*F1*F3*X2*X3+7*F3**2*X2**2+13*F3**2*X3**2+13*F2**2*X2**2+
     & 13*F2**2*
     & Y2**2+13*F2**2*Y3**2+F1*F2*Y2*Y3+F1*F2*X2*X3+7*F1**2*
     & Y2**2+F1**2*Y3**2)/S/108
!
      A33(IELEM) = (-50*F2**2*X2*X3+22*F2*F3*X3**2-26*F3**2*
     & Y2*Y3+22*F1**2*X2*X3+10*F1*F2*Y3**2+10*F1*F2*X3**2+73*F2*F3*
     & Y2**2+22*F2*F3*Y3**2-
     &  50*F2**2*Y2*Y3+25*F1*F2*Y2**2+37*F1*F3*Y2**2+
     & 22*F1**2*Y2*Y3+22*F1*
     &  F3*Y3**2+37*F1*F3*X2**2+73*F2*F3*X2**2-
     & 26*F3**2*X2*X3+25*F1*
     &  F2*X2**2+22*F1*F3*X3**2+65*F3**2*Y2**2-58*F2*F3*Y2*Y3+
     & 14*F1**2*X3**2+
     &  17*F1**2*X2**2-58*F2*F3*X2*X3+26*F3**2*Y3**2+14*F2**2*
     & X3**2+14*F1*
     &  F3*Y2*Y3+14*F1*F3*X2*X3+65*F3**2*X2**2+26*F3**2*X3**2+53*
     & F2**2*X2**2+
     &  53*F2**2*Y2**2+14*F2**2*Y3**2-10*F1*F2*Y2*Y3-
     & 10*F1*F2*X2*X3+17*
     &  F1**2*Y2**2+14*F1**2*Y3**2)/S/648
!
      A34(IELEM) = -(-19*F2**2*X2*X3+11*F2*F3*X3**2-
     & 13*F3**2*Y2*Y3+5*F1**2
     &  *X2*X3+5*F1*F2*Y3**2+5*F1*F2*X3**2+17*F2*F3*Y2**2+
     & 11*F2*F3*Y3**2-
     &  19*F2**2*Y2*Y3+5*F1*F2*Y2**2+5*F1*F3*Y2**2+5*F1**2*
     & Y2*Y3+11*F1*
     &  F3*Y3**2+5*F1*F3*X2**2+17*F2*F3*X2**2-13*F3**2*X2*X3+
     & 5*F1*F2*X2**2+
     &  11*F1*F3*X3**2+13*F3**2*Y2**2-23*F2*F3*Y2*Y3+7*F1**2*X3**2+
     &  F1**2*X2**2-23*F2*F3*X2*X3+13*F3**2*Y3**2+7*F2**2*X3**2+
     & F1*F3*Y2*Y3+
     &  F1*F3*X2*X3+13*F3**2*X2**2+13*F3**2*X3**2+13*F2**2*X2**2+
     & 13*F2**2*Y2**2+
     &  7*F2**2*Y3**2-5*F1*F2*Y2*Y3-5*F1*F2*X2*X3+F1**2*Y2**2+
     & 7*F1**2*Y3**2)/S/108
!
      A44(IELEM) = (-13*F2**2*X2*X3+11*F2*F3*X3**2-
     &  13*F3**2*Y2*Y3-F1**2*X2*X3+
     &  5*F1*F2*Y3**2+5*F1*F2*X3**2+11*F2*F3*Y2**2+11*F2*F3*Y3**2-
     &  13*F2**2*Y2*Y3+11*F1*F2*Y2**2+5*F1*F3*Y2**2-F1**2*Y2*Y3+
     & 11*F1*F3*Y3**2+5*F1*F3*X2**2+11*F2*F3*X2**2-13*F3**2*
     & X2*X3+11*F1*F2*X2**2+
     &  11*F1*F3*X3**2+7*F3**2*Y2**2-17*F2*F3*Y2*Y3+
     & 7*F1**2*X3**2+7*F1**2*X2**2-17*F2*F3*X2*X3+
     & 13*F3**2*Y3**2+7*F2**2*X3**2-5*F1*F3*Y2*Y3-5*F1*F3*X2*X3+
     & 7*F3**2*X2**2+13*F3**2*X3**2+13*F2**2*X2**2+
     &  13*F2**2*Y2**2+7*F2**2*Y3**2-5*F1*F2*Y2*Y3-5*F1*F2*X2*X3+
     & 7*F1**2*Y2**2+7*F1**2*Y3**2)/S/36
!
!   TERMS OBTAINED BY SYMMETRY
!
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
      A41(IELEM) = A14(IELEM)
      A42(IELEM) = A24(IELEM)
      A43(IELEM) = A34(IELEM)
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO
!
!------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     00XX') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=(65*F1**2*Y2**2+65*F1**2*Y3**2+17*F3**2*Y2**2+53*F3**2*
     &Y3**2-88*F1*F2*Y2*Y3-88*F1*F3*Y2*Y3+53*F2**2*Y2**2+17*F2**2*Y3**2-
     &40*F2*F3*Y2*Y3+73*F1*F3*Y3**2-56*F2**2*Y2*Y3-56*F3**2*Y2*Y3+37*F1*
     &F3*Y2**2+37*F1*F2*Y3**2+73*F1*F2*Y2**2+25*F2*F3*Y2**2+25*F2*F3*Y3*
     &*2-104*F1**2*Y2*Y3)/S/648
!
      A12(IELEM)=(13*F1**2+17*F1*F2+5*F1*F3+F3**2+13*F2**2+5*F2*F3)*(Y2+
     &Y3)*(2*Y2-Y3)/S/648
!
      A13(IELEM)=-(13*F1**2+17*F1*F3+5*F1*F2+13*F3**2+F2**2+5*F2*F3)*(Y2
     &+Y3)*(Y2-2*Y3)/S/648
!
      A14(IELEM)=-(13*F1**2*Y2**2-13*F1**2*Y2*Y3+17*F1*F2*Y2**2-11*F1*F2
     &*Y2*Y3+5*F1*F3*Y2**2-11*F1*F3*Y2*Y3+F3**2*Y2**2-7*F3**2*Y2*Y3+13*F
     &2**2*Y2**2-7*F2**2*Y2*Y3+5*F2*F3*Y2**2-5*F2*F3*Y2*Y3+13*F1**2*Y3**
     &2+17*F1*F3*Y3**2+5*F1*F2*Y3**2+13*F3**2*Y3**2+F2**2*Y3**2+5*F2*F3*
     &Y3**2)/S/108
!
      A22(IELEM)=(14*F1**2*Y2**2+17*F1**2*Y3**2+14*F3**2*Y2**2+53*F3**2*
     &Y3**2+14*F1*F2*Y2*Y3-10*F1*F3*Y2*Y3+26*F2**2*Y2**2+65*F2**2*Y3**2-
     &58*F2*F3*Y2*Y3+25*F1*F3*Y3**2-26*F2**2*Y2*Y3-50*F3**2*Y2*Y3+10*F1*
     &F3*Y2**2+37*F1*F2*Y3**2+22*F1*F2*Y2**2+22*F2*F3*Y2**2+73*F2*F3*Y3*
     &*2+22*F1**2*Y2*Y3)/S/648
!
      A23(IELEM)=(13*F2**2+17*F2*F3+5*F1*F2+F1**2+13*F3**2+5*F1*F3)*(2*Y
     &2-Y3)*(Y2-2*Y3)/S/648
!
      A24(IELEM)=-(7*F1**2*Y2**2+F1**2*Y3**2+7*F3**2*Y2**2+13*F3**2*Y3**
     &2+F1*F2*Y2*Y3-5*F1*F3*Y2*Y3+13*F2**2*Y2**2+13*F2**2*Y3**2-23*F2*F3
     &*Y2*Y3+5*F1*F3*Y3**2-13*F2**2*Y2*Y3-19*F3**2*Y2*Y3+5*F1*F3*Y2**2+5
     &*F1*F2*Y3**2+11*F1*F2*Y2**2+11*F2*F3*Y2**2+17*F2*F3*Y3**2+5*F1**2*
     &Y2*Y3)/S/108
!
      A33(IELEM)=(17*F1**2*Y2**2+14*F1**2*Y3**2+65*F3**2*Y2**2+26*F3**2*
     &Y3**2-10*F1*F2*Y2*Y3+14*F1*F3*Y2*Y3+53*F2**2*Y2**2+14*F2**2*Y3**2-
     &58*F2*F3*Y2*Y3+22*F1*F3*Y3**2-50*F2**2*Y2*Y3-26*F3**2*Y2*Y3+37*F1*
     &F3*Y2**2+10*F1*F2*Y3**2+25*F1*F2*Y2**2+73*F2*F3*Y2**2+22*F2*F3*Y3*
     &*2+22*F1**2*Y2*Y3)/S/648
!
      A34(IELEM)=-(F1**2*Y2**2+7*F1**2*Y3**2+13*F3**2*Y2**2+13*F3**2*Y3*
     &*2-5*F1*F2*Y2*Y3+F1*F3*Y2*Y3+13*F2**2*Y2**2+7*F2**2*Y3**2-23*F2*F3
     &*Y2*Y3+11*F1*F3*Y3**2-19*F2**2*Y2*Y3-13*F3**2*Y2*Y3+5*F1*F3*Y2**2+
     &5*F1*F2*Y3**2+5*F1*F2*Y2**2+17*F2*F3*Y2**2+11*F2*F3*Y3**2+5*F1**2*
     &Y2*Y3)/S/108
!
      A44(IELEM)=(7*F1**2*Y2**2+7*F1**2*Y3**2+7*F3**2*Y2**2+13*F3**2*Y3*
     &*2-5*F1*F2*Y2*Y3-5*F1*F3*Y2*Y3+13*F2**2*Y2**2+7*F2**2*Y3**2-17*F2*
     &F3*Y2*Y3+11*F1*F3*Y3**2-13*F2**2*Y2*Y3-13*F3**2*Y2*Y3+5*F1*F3*Y2**
     &2+5*F1*F2*Y3**2+11*F1*F2*Y2**2+11*F2*F3*Y2**2+11*F2*F3*Y3**2-F1**2
     &*Y2*Y3)/S/36
!
!   TERMS OBTAINED BY SYMMETRY
!
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
      A41(IELEM) = A14(IELEM)
      A42(IELEM) = A24(IELEM)
      A43(IELEM) = A34(IELEM)
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO
!
!------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     00YY') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=(-56*F3**2*X2*X3+37*F1*F3*X2**2+73*F1*F3*X3**2-88*F1*F3
     &*X2*X3-88*F1*F2*X2*X3-40*F2*F3*X2*X3+73*F1*F2*X2**2-104*F1**2*X2*X
     &3-56*F2**2*X2*X3+37*F1*F2*X3**2+65*F1**2*X2**2+65*F1**2*X3**2+53*F
     &2**2*X2**2+17*F2**2*X3**2+17*F3**2*X2**2+53*F3**2*X3**2+25*F2*F3*X
     &2**2+25*F2*F3*X3**2)/S/648
!
      A12(IELEM)=-(13*F1**2+5*F1*F3+17*F1*F2+13*F2**2+F3**2+5*F2*F3)*(X2
     &+X3)*(-2*X2+X3)/S/648
!
      A13(IELEM)=(13*F1**2+17*F1*F3+5*F1*F2+13*F3**2+F2**2+5*F2*F3)*(X2+
     &X3)*(-X2+2*X3)/S/648
!
      A14(IELEM)=-(13*F1**2*X2**2-13*F1**2*X2*X3+5*F1*F3*X2**2-11*F1*F3*
     &X2*X3+17*F1*F2*X2**2-11*F1*F2*X2*X3+13*F2**2*X2**2-7*F2**2*X2*X3+F
     &3**2*X2**2-7*F3**2*X2*X3+5*F2*F3*X2**2-5*F2*F3*X2*X3+13*F1**2*X3**
     &2+17*F1*F3*X3**2+5*F1*F2*X3**2+13*F3**2*X3**2+F2**2*X3**2+5*F2*F3*
     &X3**2)/S/108
!
      A22(IELEM)=(-50*F3**2*X2*X3+10*F1*F3*X2**2+25*F1*F3*X3**2-10*F1*F3
     &*X2*X3+14*F1*F2*X2*X3-58*F2*F3*X2*X3+22*F1*F2*X2**2+22*F1**2*X2*X3
     &-26*F2**2*X2*X3+37*F1*F2*X3**2+14*F1**2*X2**2+17*F1**2*X3**2+26*F2
     &**2*X2**2+65*F2**2*X3**2+14*F3**2*X2**2+53*F3**2*X3**2+22*F2*F3*X2
     &**2+73*F2*F3*X3**2)/S/648
!
      A23(IELEM)=(13*F2**2+5*F1*F2+17*F2*F3+F1**2+13*F3**2+5*F1*F3)*(-2*
     &X2+X3)*(-X2+2*X3)/S/648
!
      A24(IELEM)=-(-19*F3**2*X2*X3+5*F1*F3*X2**2+5*F1*F3*X3**2-5*F1*F3*X
     &2*X3+F1*F2*X2*X3-23*F2*F3*X2*X3+11*F1*F2*X2**2+5*F1**2*X2*X3-13*F2
     &**2*X2*X3+5*F1*F2*X3**2+7*F1**2*X2**2+F1**2*X3**2+13*F2**2*X2**2+1
     &3*F2**2*X3**2+7*F3**2*X2**2+13*F3**2*X3**2+11*F2*F3*X2**2+17*F2*F3
     &*X3**2)/S/108
!
      A33(IELEM)=(-26*F3**2*X2*X3+37*F1*F3*X2**2+22*F1*F3*X3**2+14*F1*F3
     &*X2*X3-10*F1*F2*X2*X3-58*F2*F3*X2*X3+25*F1*F2*X2**2+22*F1**2*X2*X3
     &-50*F2**2*X2*X3+10*F1*F2*X3**2+17*F1**2*X2**2+14*F1**2*X3**2+53*F2
     &**2*X2**2+14*F2**2*X3**2+65*F3**2*X2**2+26*F3**2*X3**2+73*F2*F3*X2
     &**2+22*F2*F3*X3**2)/S/648
!
      A34(IELEM)=-(-13*F3**2*X2*X3+5*F1*F3*X2**2+11*F1*F3*X3**2+F1*F3*X2
     &*X3-5*F1*F2*X2*X3-23*F2*F3*X2*X3+5*F1*F2*X2**2+5*F1**2*X2*X3-19*F2
     &**2*X2*X3+5*F1*F2*X3**2+F1**2*X2**2+7*F1**2*X3**2+13*F2**2*X2**2+7
     &*F2**2*X3**2+13*F3**2*X2**2+13*F3**2*X3**2+17*F2*F3*X2**2+11*F2*F3
     &*X3**2)/S/108
!
      A44(IELEM)=(-13*F3**2*X2*X3+5*F1*F3*X2**2+11*F1*F3*X3**2-5*F1*F3*X
     &2*X3-5*F1*F2*X2*X3-17*F2*F3*X2*X3+11*F1*F2*X2**2-F1**2*X2*X3-13*F2
     &**2*X2*X3+5*F1*F2*X3**2+7*F1**2*X2**2+7*F1**2*X3**2+13*F2**2*X2**2
     &+7*F2**2*X3**2+7*F3**2*X2**2+13*F3**2*X3**2+11*F2*F3*X2**2+11*F2*F
     &3*X3**2)/S/36
!
!   TERMS OBTAINED BY SYMMETRY
!
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
      A41(IELEM) = A14(IELEM)
      A42(IELEM) = A24(IELEM)
      A43(IELEM) = A34(IELEM)
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO
!
!------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     00XY') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      S = SURFAC(IELEM)/XMUL
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=(44*F1*F3*Y2*X3+44*F1*F3*Y3*X2-73*F1*F3*Y3*X3-73*F1*F2*
     &Y2*X2-25*F2*F3*Y3*X3+44*F1*F2*Y3*X2-37*F1*F2*Y3*X3-25*F2*F3*Y2*X2+
     &20*F2*F3*Y2*X3+52*F1**2*Y2*X3+28*F3**2*Y2*X3-17*F2**2*Y3*X3-65*F1*
     &*2*Y2*X2+28*F2**2*Y2*X3-53*F2**2*Y2*X2+28*F3**2*Y3*X2+52*F1**2*Y3*
     &X2-65*F1**2*Y3*X3-17*F3**2*Y2*X2+28*F2**2*Y3*X2-53*F3**2*Y3*X3-37*
     &F1*F3*Y2*X2+44*F1*F2*Y2*X3+20*F2*F3*Y3*X2)/S/648
!
      A12(IELEM)=(13*F1**2+5*F1*F3+17*F1*F2+F3**2+13*F2**2+5*F2*F3)*(Y2+
     &Y3)*(-2*X2+X3)/S/648
!
      A13(IELEM)=-(13*F1**2+17*F1*F3+5*F1*F2+F2**2+13*F3**2+5*F2*F3)*(Y2
     &+Y3)*(-X2+2*X3)/S/648
!
      A14(IELEM)=-(-26*F1**2*Y2*X2+13*F1**2*Y2*X3-10*F1*F3*Y2*X2+5*F1*F3
     &*Y2*X3-34*F1*F2*Y2*X2+17*F1*F2*Y2*X3-2*F3**2*Y2*X2+F3**2*Y2*X3-26*
     &F2**2*Y2*X2+13*F2**2*Y2*X3-10*F2*F3*Y2*X2+5*F2*F3*Y2*X3+13*F1**2*Y
     &3*X2-26*F1**2*Y3*X3+17*F1*F3*Y3*X2-34*F1*F3*Y3*X3+5*F1*F2*Y3*X2-10
     &*F1*F2*Y3*X3+F2**2*Y3*X2-2*F2**2*Y3*X3+13*F3**2*Y3*X2-26*F3**2*Y3*
     &X3+5*F2*F3*Y3*X2-10*F2*F3*Y3*X3)/S/216
!
      A21(IELEM)=-(13*F1**2+5*F1*F3+17*F1*F2+F3**2+13*F2**2+5*F2*F3)*(2*
     &Y2-Y3)*(X2+X3)/S/648
!
      A22(IELEM)=-(-5*F1*F3*Y2*X3-5*F1*F3*Y3*X2+25*F1*F3*Y3*X3+22*F1*F2*
     &Y2*X2+73*F2*F3*Y3*X3+7*F1*F2*Y3*X2+37*F1*F2*Y3*X3+22*F2*F3*Y2*X2-2
     &9*F2*F3*Y2*X3+11*F1**2*Y2*X3-25*F3**2*Y2*X3+65*F2**2*Y3*X3+14*F1**
     &2*Y2*X2-13*F2**2*Y2*X3+26*F2**2*Y2*X2-25*F3**2*Y3*X2+11*F1**2*Y3*X
     &2+17*F1**2*Y3*X3+14*F3**2*Y2*X2-13*F2**2*Y3*X2+53*F3**2*Y3*X3+10*F
     &1*F3*Y2*X2+7*F1*F2*Y2*X3-29*F2*F3*Y3*X2)/S/648
!
      A23(IELEM)=(13*F2**2+5*F1*F2+17*F2*F3+F1**2+13*F3**2+5*F1*F3)*(2*Y
     &2-Y3)*(-X2+2*X3)/S/648
!
      A24(IELEM)=(-5*F1*F3*Y2*X3-5*F1*F3*Y3*X2+10*F1*F3*Y3*X3+22*F1*F2*Y
     &2*X2+34*F2*F3*Y3*X3-5*F1*F2*Y3*X2+10*F1*F2*Y3*X3+22*F2*F3*Y2*X2-29
     &*F2*F3*Y2*X3+11*F1**2*Y2*X3-25*F3**2*Y2*X3+26*F2**2*Y3*X3+14*F1**2
     &*Y2*X2-13*F2**2*Y2*X3+26*F2**2*Y2*X2-13*F3**2*Y3*X2-F1**2*Y3*X2+2*
     &F1**2*Y3*X3+14*F3**2*Y2*X2-13*F2**2*Y3*X2+26*F3**2*Y3*X3+10*F1*F3*
     &Y2*X2+7*F1*F2*Y2*X3-17*F2*F3*Y3*X2)/S/216
!
      A31(IELEM)=(13*F1**2+17*F1*F3+5*F1*F2+F2**2+13*F3**2+5*F2*F3)*(Y2-
     &2*Y3)*(X2+X3)/S/648
!
      A32(IELEM)=(13*F2**2+5*F1*F2+17*F2*F3+F1**2+13*F3**2+5*F1*F3)*(Y2-
     &2*Y3)*(-2*X2+X3)/S/648
!
      A33(IELEM)=-(7*F1*F3*Y2*X3+7*F1*F3*Y3*X2+22*F1*F3*Y3*X3+25*F1*F2*Y
     &2*X2+22*F2*F3*Y3*X3-5*F1*F2*Y3*X2+10*F1*F2*Y3*X3+73*F2*F3*Y2*X2-29
     &*F2*F3*Y2*X3+11*F1**2*Y2*X3-13*F3**2*Y2*X3+14*F2**2*Y3*X3+17*F1**2
     &*Y2*X2-25*F2**2*Y2*X3+53*F2**2*Y2*X2-13*F3**2*Y3*X2+11*F1**2*Y3*X2
     &+14*F1**2*Y3*X3+65*F3**2*Y2*X2-25*F2**2*Y3*X2+26*F3**2*Y3*X3+37*F1
     &*F3*Y2*X2-5*F1*F2*Y2*X3-29*F2*F3*Y3*X2)/S/648
!
      A34(IELEM)=-(5*F1*F3*Y2*X3-7*F1*F3*Y3*X2-22*F1*F3*Y3*X3-10*F1*F2*Y
     &2*X2-22*F2*F3*Y3*X3+5*F1*F2*Y3*X2-10*F1*F2*Y3*X3-34*F2*F3*Y2*X2+17
     &*F2*F3*Y2*X3+F1**2*Y2*X3+13*F3**2*Y2*X3-14*F2**2*Y3*X3-2*F1**2*Y2*
     &X2+13*F2**2*Y2*X3-26*F2**2*Y2*X2+13*F3**2*Y3*X2-11*F1**2*Y3*X2-14*
     &F1**2*Y3*X3-26*F3**2*Y2*X2+25*F2**2*Y3*X2-26*F3**2*Y3*X3-10*F1*F3*
     &Y2*X2+5*F1*F2*Y2*X3+29*F2*F3*Y3*X2)/S/216
!
      A41(IELEM)=-(-26*F1**2*Y2*X2+13*F1**2*Y3*X2-10*F1*F3*Y2*X2+5*F1*F3
     &*Y3*X2-34*F1*F2*Y2*X2+17*F1*F2*Y3*X2-2*F3**2*Y2*X2+F3**2*Y3*X2-26*
     &F2**2*Y2*X2+13*F2**2*Y3*X2-10*F2*F3*Y2*X2+5*F2*F3*Y3*X2+13*F1**2*Y
     &2*X3-26*F1**2*Y3*X3+17*F1*F3*Y2*X3-34*F1*F3*Y3*X3+5*F1*F2*Y2*X3-10
     &*F1*F2*Y3*X3+F2**2*Y2*X3-2*F2**2*Y3*X3+13*F3**2*Y2*X3-26*F3**2*Y3*
     &X3+5*F2*F3*Y2*X3-10*F2*F3*Y3*X3)/S/216
!
      A42(IELEM)=-(5*F1*F3*Y2*X3+5*F1*F3*Y3*X2-10*F1*F3*Y3*X3-22*F1*F2*Y
     &2*X2-34*F2*F3*Y3*X3-7*F1*F2*Y3*X2-10*F1*F2*Y3*X3-22*F2*F3*Y2*X2+17
     &*F2*F3*Y2*X3+F1**2*Y2*X3+13*F3**2*Y2*X3-26*F2**2*Y3*X3-14*F1**2*Y2
     &*X2+13*F2**2*Y2*X3-26*F2**2*Y2*X2+25*F3**2*Y3*X2-11*F1**2*Y3*X2-2*
     &F1**2*Y3*X3-14*F3**2*Y2*X2+13*F2**2*Y3*X2-26*F3**2*Y3*X3-10*F1*F3*
     &Y2*X2+5*F1*F2*Y2*X3+29*F2*F3*Y3*X2)/S/216
!
      A43(IELEM)=(7*F1*F3*Y2*X3-5*F1*F3*Y3*X2+22*F1*F3*Y3*X3+10*F1*F2*Y2
     &*X2+22*F2*F3*Y3*X3-5*F1*F2*Y3*X2+10*F1*F2*Y3*X3+34*F2*F3*Y2*X2-29*
     &F2*F3*Y2*X3+11*F1**2*Y2*X3-13*F3**2*Y2*X3+14*F2**2*Y3*X3+2*F1**2*Y
     &2*X2-25*F2**2*Y2*X3+26*F2**2*Y2*X2-13*F3**2*Y3*X2-F1**2*Y3*X2+14*F
     &1**2*Y3*X3+26*F3**2*Y2*X2-13*F2**2*Y3*X2+26*F3**2*Y3*X3+10*F1*F3*Y
     &2*X2-5*F1*F2*Y2*X3-17*F2*F3*Y3*X2)/S/216
!
      A44(IELEM)=(5*F1*F3*Y2*X3+5*F1*F3*Y3*X2-22*F1*F3*Y3*X3-22*F1*F2*Y2
     &*X2-22*F2*F3*Y3*X3+5*F1*F2*Y3*X2-10*F1*F2*Y3*X3-22*F2*F3*Y2*X2+17*
     &F2*F3*Y2*X3+F1**2*Y2*X3+13*F3**2*Y2*X3-14*F2**2*Y3*X3-14*F1**2*Y2*
     &X2+13*F2**2*Y2*X3-26*F2**2*Y2*X2+13*F3**2*Y3*X2+F1**2*Y3*X2-14*F1*
     &*2*Y3*X3-14*F3**2*Y2*X2+13*F2**2*Y3*X2-26*F3**2*Y3*X3-10*F1*F3*Y2*
     &X2+5*F1*F2*Y2*X3+17*F2*F3*Y3*X2)/S/72
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!   CASE NOT IMPLEMENTED
!
      ELSE
!
        WRITE(LU,1001) FORMUL
1001    FORMAT(1X,'MT99BB (BIEF) : MATRIX NOT IMPLEMENTED:',A16)
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
