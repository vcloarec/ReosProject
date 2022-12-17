!                   *****************
                    SUBROUTINE VC08TT
!                   *****************
!
     &(XMUL,SF,SU,SV,SW,F,U,V,W,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /                  DF      DF
!+      V  =  XMUL   /       PSII  * ( U --  + V -- )   D(OMEGA)
!+       I          /OMEGA               DX      DY
!+
!+    PSI(I) IS A BASE OF TYPE P1 TETRAHEDRON
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
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Z              |-->| ELEVATIONS OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC08TT => VC08TT
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
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*),XMUL
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
!
!     STRUCTURES OF F, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ),   INTENT(IN) :: SF,SU,SV,SW
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*),W(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,Q1,Q2,Q3,Q4
      DOUBLE PRECISION F1MF2,F1MF3,F1MF4,HELP1,HELP2,HELP3
      DOUBLE PRECISION U1234,V1234,W1234,XSUR120
!
      INTEGER I1,I2,I3,I4,IELEM,IELMF,IELMU,IELMV,IELMW
!
!**********************************************************************
!
      XSUR120 = XMUL / 120.D0
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F AND VECTOR U ARE LINEAR
!
      IF( (IELMU.EQ.31.AND.IELMV.EQ.31.AND.IELMW.EQ.31.AND.IELMF.EQ.31)
     &    .OR.
     &    (IELMU.EQ.51.AND.IELMV.EQ.51.AND.IELMW.EQ.51.AND.IELMF.EQ.51)
     &  ) THEN
!
!        LOOP ON THE ELEMENTS
!
        DO IELEM = 1,NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
!
          X2  =   X(I2) - X(I1)
          X3  =   X(I3) - X(I1)
          X4  =   X(I4) - X(I1)
          Y2  =   Y(I2) - Y(I1)
          Y3  =   Y(I3) - Y(I1)
          Y4  =   Y(I4) - Y(I1)
          Z2  =   Z(I2) - Z(I1)
          Z3  =   Z(I3) - Z(I1)
          Z4  =   Z(I4) - Z(I1)
!
          U1  =  U(I1)
          U2  =  U(I2)
          U3  =  U(I3)
          U4  =  U(I4)
!
          V1  =  V(I1)
          V2  =  V(I2)
          V3  =  V(I3)
          V4  =  V(I4)
!
          Q1  =  W(I1)
          Q2  =  W(I2)
          Q3  =  W(I3)
          Q4  =  W(I4)
!
          U1234 = U1 + U2 + U3 + U4
          V1234 = V1 + V2 + V3 + V4
          W1234 = Q1 + Q2 + Q3 + Q4
!
          F1MF2  =  F(I1) - F(I2)
          F1MF3  =  F(I1) - F(I3)
          F1MF4  =  F(I1) - F(I4)
!
          HELP1 = (  (Y4*Z3-Y3*Z4)*F1MF2
     &              +(Y2*Z4-Y4*Z2)*F1MF3
     &              +(Y3*Z2-Y2*Z3)*F1MF4  ) * XSUR120
!
          HELP2 = (  (X3*Z4-X4*Z3)*F1MF2
     &              +(X4*Z2-X2*Z4)*F1MF3
     &              +(X2*Z3-X3*Z2)*F1MF4  ) * XSUR120
!
          HELP3 = (  (X4*Y3-X3*Y4)*F1MF2
     &              +(X2*Y4-X4*Y2)*F1MF3
     &              +(X3*Y2-X2*Y3)*F1MF4  ) * XSUR120
!
          W1(IELEM) = ( U1234 + U1 ) * HELP1
     &              + ( V1234 + V1 ) * HELP2
     &              + ( W1234 + Q1 ) * HELP3
          W2(IELEM) = ( U1234 + U2 ) * HELP1
     &              + ( V1234 + V2 ) * HELP2
     &              + ( W1234 + Q2 ) * HELP3
          W3(IELEM) = ( U1234 + U3 ) * HELP1
     &              + ( V1234 + V3 ) * HELP2
     &              + ( W1234 + Q3 ) * HELP3
          W4(IELEM) = ( U1234 + U4 ) * HELP1
     &              + ( V1234 + V4 ) * HELP2
     &              + ( W1234 + Q4 ) * HELP3
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMF,SF%NAME
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC08TT (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
