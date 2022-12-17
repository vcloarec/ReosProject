!                   *****************
                    SUBROUTINE VC01TT
!                   *****************
!
     &( XMUL,SF,F,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I) * F  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 TETRAHEDRON
!+
!+    F IS A VECTOR OF TYPE IELMF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH
!
!history  J-M HERVOUET (LNH)
!+        22/03/02
!+        V5P3
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF TETRAHEDRA
!| IKLE2          |-->| SECOND POINT OF TETRAHEDRA
!| IKLE3          |-->| THIRD POINT OF TETRAHEDRA
!| IKLE4          |-->| FOURTH POINT OF TETRAHEDRA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC01TT => VC01TT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSUR120,COEF,F1234,X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      DOUBLE PRECISION F1,F2,F3,F4
      INTEGER I1,I2,I3,I4,IELEM,IELMF
!
!***********************************************************************
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!
!   F IS LINEAR
!
      IF(IELMF.EQ.31.OR.IELMF.EQ.51) THEN
!
        XSUR120 = XMUL / 120.D0
!
        DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
!
          X2 = X(I2)-X(I1)
          X3 = X(I3)-X(I1)
          X4 = X(I4)-X(I1)
!
          Y2 = Y(I2)-Y(I1)
          Y3 = Y(I3)-Y(I1)
          Y4 = Y(I4)-Y(I1)
!
          Z2 = Z(I2)-Z(I1)
          Z3 = Z(I3)-Z(I1)
          Z4 = Z(I4)-Z(I1)
!
          COEF = XSUR120*
     &          (X2*Y3*Z4-X2*Y4*Z3-Y2*X3*Z4+Y2*X4*Z3+Z2*X3*Y4-Z2*X4*Y3)
!
          F1  = F(IKLE1(IELEM))
          F2  = F(IKLE2(IELEM))
          F3  = F(IKLE3(IELEM))
          F4  = F(IKLE4(IELEM))
!
          F1234 = F1+F2+F3+F4
          W1(IELEM) = COEF * (F1234+F1)
          W2(IELEM) = COEF * (F1234+F2)
          W3(IELEM) = COEF * (F1234+F3)
          W4(IELEM) = COEF * (F1234+F4)
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!   F IS CONSTANT BY ELEMENT
!
!   SAME METHOD BUT F HAS THE SAME VALUE
!   FOR THE 4 POINTS OF THE TETRAHEDRON
!
      ELSE IF(IELMF.EQ.30.OR.IELMF.EQ.50) THEN
!
        XSUR120 = XMUL / 120.D0
!
        DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
!
          X2 = X(I2)-X(I1)
          X3 = X(I3)-X(I1)
          X4 = X(I4)-X(I1)
!
          Y2 = Y(I2)-Y(I1)
          Y3 = Y(I3)-Y(I1)
          Y4 = Y(I4)-Y(I1)
!
          Z2 = Z(I2)-Z(I1)
          Z3 = Z(I3)-Z(I1)
          Z4 = Z(I4)-Z(I1)
!
          COEF = XSUR120*
     &          (X2*Y3*Z4-X2*Y4*Z3-Y2*X3*Z4+Y2*X4*Z3+Z2*X3*Y4-Z2*X4*Y3)
!
! COULD BE SIMPLIFIED BUT IS NICE TO KEEP
! THE SAME FORM AS ABOVE
!
          F1  = F(IELEM)
          F2  = F1
          F3  = F1
          F4  = F1
!
          F1234 = F1+F2+F3+F4
          W1(IELEM) = COEF * (F1234+F1)
          W2(IELEM) = COEF * (F1234+F2)
          W3(IELEM) = COEF * (F1234+F3)
          W4(IELEM) = COEF * (F1234+F4)
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
      ELSE
!
        WRITE(LU,102) IELMF,SF%NAME
102     FORMAT(1X,'VC01TT (BIEF):',/,
     &         1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &         1X,'REAL NAME OF F: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE VC01TT
