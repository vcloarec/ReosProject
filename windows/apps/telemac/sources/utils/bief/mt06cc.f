!                   *****************
                    SUBROUTINE MT06CC
!                   *****************
!
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &        A22 , A23 , A24 , A25 , A26 ,
     &              A33 , A34 , A35 , A36 ,
     &                    A44 , A45 , A46 ,
     &                          A55 , A56 ,
     &                                A66 ,
     &  XMUL,SF,F,SURFAC,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE FOLLOWING MATRIX:
!code
!+     SUM(F*PSII*PSIJ)
!+
!+            WITH:  QUADRATIC P1
!+                   QUADRATIC P2
!+                   F P1 OR QUADRATIC
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
!| A66            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION F USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| FOURTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE5          |-->| FIFTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE6          |-->| SIXTH POINTS OF TRIANGLES (QUADRATIC)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT06CC => MT06CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
      INTEGER, INTENT(IN) :: IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A55(*),A56(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A66(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6,XSUR030,XSUR045,XSUR180
      DOUBLE PRECISION XSUR315,XSUR210,XSUR630,XSU1260
      DOUBLE PRECISION AUX315,AUX210,AUX630,AUX1260
      INTEGER IELMF,IELEM
!
!=======================================================================
!
!     EXTRACTS THE TYPE OF ELEMENT FOR F
!
      IELMF = SF%ELM
!
!  CASE WHERE F IS P0
!
      IF(IELMF.EQ.10) THEN
!
      XSUR030 = XMUL / 30.D0
      XSUR045 = XMUL / 45.D0
      XSUR180 = XMUL /180.D0
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        F1  =  F(IELEM) * SURFAC(IELEM)
!
!  DIAGONAL TERMS
!
        A11(IELEM) =   XSUR030 * F1
        A22(IELEM) =   A11(IELEM)
        A33(IELEM) =   A11(IELEM)
        A44(IELEM) =   8.D0 * XSUR045 * F1
        A55(IELEM) =   A44(IELEM)
        A66(IELEM) =   A44(IELEM)
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = - XSUR180 * F1
        A13(IELEM) =   A12(IELEM)
        A14(IELEM) =   0.D0
        A15(IELEM) = - XSUR045*F1
        A16(IELEM) =   0.D0
!
        A23(IELEM) =   A12(IELEM)
        A24(IELEM) =   0.D0
        A25(IELEM) =   0.D0
        A26(IELEM) =   A15(IELEM)
!
        A34(IELEM) =   A15(IELEM)
        A35(IELEM) =   0.D0
        A36(IELEM) =   0.D0
!
        A45(IELEM) =   4.D0*XSUR045*F1
        A46(IELEM) =   A45(IELEM)
!
        A56(IELEM) =   A45(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!  CASE WHERE F IS LINEAR
!
      ELSEIF(IELMF.EQ.11) THEN
!
      XSUR210 = XMUL /  210.D0
      XSUR315 = XMUL /  315.D0
      XSU1260 = XMUL / 1260.D0
!
      DO IELEM = 1 , NELEM
        AUX210 = SURFAC(IELEM) * XSUR210
        AUX315 = SURFAC(IELEM) * XSUR315
        AUX1260= SURFAC(IELEM) * XSU1260
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
!
!  DIAGONAL TERMS
!
        A11(IELEM) =      (5.D0*F1+     F2+     F3)*AUX210
        A22(IELEM) =      (     F1+5.D0*F2+     F3)*AUX210
        A33(IELEM) =      (     F1+     F2+5.D0*F3)*AUX210
        A44(IELEM) = 8.D0*(3.D0*F1+3.D0*F2+     F3)*AUX315
        A55(IELEM) = 8.D0*(     F1+3.D0*F2+3.D0*F3)*AUX315
        A66(IELEM) = 8.D0*(3.D0*F1+     F2+3.D0*F3)*AUX315
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = -(4.D0*F1+4.D0*F2-     F3)*AUX1260
        A13(IELEM) = -(4.D0*F1-     F2+4.D0*F3)*AUX1260
        A14(IELEM) =  (3.D0*F1-2.D0*F2-     F3)*AUX315
        A15(IELEM) = -(     F1+3.D0*F2+3.D0*F3)*AUX315
        A16(IELEM) =  (3.D0*F1-     F2-2.D0*F3)*AUX315
!
        A23(IELEM) =  (     F1-4.D0*F2-4.D0*F3)*AUX1260
        A24(IELEM) = -(2.D0*F1-3.D0*F2+     F3)*AUX315
        A25(IELEM) = -(     F1-3.D0*F2+2.D0*F3)*AUX315
        A26(IELEM) = -(3.D0*F1+     F2+3.D0*F3)*AUX315
!
        A34(IELEM) = -(3.D0*F1+3.D0*F2+     F3)*AUX315
        A35(IELEM) = -(     F1+2.D0*F2-3.D0*F3)*AUX315
        A36(IELEM) = -(2.D0*F1+     F2-3.D0*F3)*AUX315
!
        A45(IELEM) =  (2.D0*F1+3.D0*F2+2.D0*F3)*AUX315*4.D0
        A46(IELEM) =  (3.D0*F1+2.D0*F2+2.D0*F3)*AUX315*4.D0
!
        A56(IELEM) =  (2.D0*F1+3.D0*F3+2.D0*F2)*AUX315*4.D0
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.13) THEN
!
!-----------------------------------------------------------------------
!
!   QUADRATIC DISCRETISATION OF F:
!
      XSUR315 = XMUL /  315.D0
      XSUR630 = XMUL /  630.D0
      XSU1260 = XMUL / 1260.D0
!
      DO IELEM = 1 , NELEM
        AUX315 = SURFAC(IELEM) * XSUR315
        AUX630 = SURFAC(IELEM) * XSUR630
        AUX1260= SURFAC(IELEM) * XSU1260
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
        F4  =  F(IKLE4(IELEM))
        F5  =  F(IKLE5(IELEM))
        F6  =  F(IKLE6(IELEM))
!
!  DIAGONAL TERMS
!
        A11(IELEM) = (6.D0*(F4+F6)+9.D0*F1+2.D0*F5-F2-F3) * AUX630
        A22(IELEM) = (6.D0*(F4+F5)+2.D0*F6+9.D0*F2-F1-F3) * AUX630
        A33(IELEM) = (6.D0*(F6+F5)+9.D0*F3+2.D0*F4-F1-F2) * AUX630
        A44(IELEM) =  4.D0*(3.D0*(F6+F5)+9.D0*F4-F3) * AUX315
        A55(IELEM) =  4.D0*(3.D0*(F4+F6)+9.D0*F5-F1) * AUX315
        A66(IELEM) =  4.D0*(3.D0*(F4+F5)+9.D0*F6-F2) * AUX315
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = -(2.D0*(F1+F2)+4.D0*F4-F3) * AUX1260
        A13(IELEM) = -(2.D0*(F1+F3)+4.D0*F6-F2) * AUX1260
        A14(IELEM) =  (3.D0* F1    -2.D0*F5-F2) * AUX315
        A15(IELEM) = -(2.D0*(F4+F6)+4.D0*F5-F1) * AUX315
        A16(IELEM) =  (3.D0*F1     -2.D0*F5-F3) * AUX315
!
        A23(IELEM) =  (-2.D0*(F2+F3)-4.D0*F5+F1) * AUX1260
        A24(IELEM) =  (-2.D0*F6     +3.D0*F2-F1) * AUX315
        A25(IELEM) =  (-2.D0*F6     +3.D0*F2-F3) * AUX315
        A26(IELEM) =  (-2.D0*(F4+F5)-4.D0*F6+F2) * AUX315
!
        A34(IELEM) =  (-2.D0*(F6+F5)-4.D0*F4+F3) * AUX315
        A35(IELEM) =  (-2.D0*F4     +3.D0*F3-F2) * AUX315
        A36(IELEM) =  (-2.D0*F4     +3.D0*F3-F1) * AUX315
!
        A45(IELEM) =  2.D0*(6.D0*(F4+F5)+4.D0*F6-F1-F3) * AUX315
        A46(IELEM) =  2.D0*(6.D0*(F4+F6)+4.D0*F5-F2-F3) * AUX315
!
        A56(IELEM) =  2.D0*(6.D0*(F6+F5)+4.D0*F4-F2-F1) * AUX315
!
      ENDDO ! IELEM
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'MT06CC (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &         1X,'REAL NAME: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
