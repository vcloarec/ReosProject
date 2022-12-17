!                   *****************
                    SUBROUTINE MT03AA
!                   *****************
!
     &( A11 , A12 , A13 ,
     &  A21 , A22 , A23 ,
     &  A31 , A32 , A33 ,
     &  XMUL,SF,SG,SU,SV,F,G,U,V,
     &  XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                 /  -->   - -->          -->  --->
!+ A(I,J) = XMUL  /   KEL . GRAD(PSI1(I)) * U . GRAD(PSI2(J)) D(OMEGA)
!+               /OMEGA
!+         -->
!+         KEL CONSTANT VECTOR ON THE ELEMENT, WITH COMPONENTS F AND G
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        12/04/93
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
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| G              |-->| FUNCTION USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SG             |-->| STRUCTURE OF FUNCTIONS G
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SURFAC         |-->| AREA OF TRIANGLES
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| V              |-->| FUNCTION V USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT03AA => MT03AA
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),U(*),V(*)
!
!     STRUCTURES OF      F, G, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMF,IELMG,IELMU,IELMV,IELEM
!
      DOUBLE PRECISION SUR12,SUR36,X2,X3,Y2,Y3,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION V123,U123,DEN,KKX,KKY,AUX1,AUX2,AUX3,AUX4
!
!-----------------------------------------------------------------------
!
      SUR12 = XMUL/12.D0
      SUR36 = XMUL/36.D0
!
!-----------------------------------------------------------------------
!
      IELMF = SF%ELM
      IELMG = SG%ELM
      IELMU = SU%ELM
      IELMV = SV%ELM
!
      IF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.
     &   IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =  XEL(IELEM,2)
      X3  =  XEL(IELEM,3)
      Y2  =  YEL(IELEM,2)
      Y3  =  YEL(IELEM,3)
!
      U1  =  U(IKLE1(IELEM))
      U2  =  U(IKLE2(IELEM))
      U3  =  U(IKLE3(IELEM))
      V1  =  V(IKLE1(IELEM))
      V2  =  V(IKLE2(IELEM))
      V3  =  V(IKLE3(IELEM))
!
      U123 = U1 + U2 + U3
      V123 = V1 + V2 + V3
!
      DEN = SUR12 / SURFAC(IELEM)
      KKX = F(IELEM)*DEN
      KKY = G(IELEM)*DEN
!
      AUX1 = X2 * V123 - Y2 * U123
      AUX2 = X3 * V123 - Y3 * U123
      AUX3 = X2 * KKY  - Y2 * KKX
      AUX4 = X3 * KKY  - Y3 * KKX
!
      A11(IELEM)  = ( AUX1 - AUX2 ) * ( AUX3 - AUX4 )
      A22(IELEM)  =          AUX2   *          AUX4
      A12(IELEM)  =          AUX2   * ( AUX3 - AUX4 )
      A21(IELEM)  = ( AUX1 - AUX2 ) *          AUX4
!
!  USES HERE THE 'MAGIC SQUARE' PROPERTIES OF A DIFFUSION-LIKE MATRIX
!  (SUM OF EACH LINE AND EACH COLUMN IS 0)
!
      A13(IELEM) = - A11(IELEM) - A12(IELEM)
      A23(IELEM) = - A22(IELEM) - A21(IELEM)
      A31(IELEM) = - A11(IELEM) - A21(IELEM)
      A32(IELEM) = - A22(IELEM) - A12(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.10.AND.IELMG.EQ.10.AND.IELMU.EQ.12) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
!
      U1   =  U(IKLE1(IELEM))
      U2   =  U(IKLE2(IELEM))
      U3   =  U(IKLE3(IELEM))
      U4   =  U(IKLE4(IELEM))
      V1   =  V(IKLE1(IELEM))
      V2   =  V(IKLE2(IELEM))
      V3   =  V(IKLE3(IELEM))
      V4   =  V(IKLE4(IELEM))
!
      DEN = SUR36 / SURFAC(IELEM)
      KKX=F(IELEM)*DEN
      KKY=G(IELEM)*DEN
!
      A11(IELEM) = ((X2*KKY-X3*KKY+KKX*Y3-KKX*Y2)*(2*X2*V3+3*X2*V4+
     &          2*X2*V2+2*X2*V1-2*X3*V3-3*X3*V4-2*X3*V2-2*X3*V1+2*
     &          U3*Y3-2*U3*Y2+3*U4*Y3-3*U4*Y2+2*U2*Y3-2*U2*Y2+2*U1*
     &          Y3-2*U1*Y2))
      A22(IELEM) = ((X3*KKY-KKX*Y3)*(2*X3*V3+3*X3*V4+2*X3*V2+2*
     &          X3*V1-2*U3*Y3-3*U4*Y3-2*U2*Y3-2*U1*Y3))
      A12(IELEM) = ((X2*KKY-X3*KKY+KKX*Y3-KKX*Y2)*(2*X3*V3+3*X3*V4+
     &          2*X3*V2+2*X3*V1-2*U3*Y3-3*U4*Y3-2*U2*Y3-2*U1*Y3))
      A21(IELEM) = ((2*X2*V3+3*X2*V4+2*X2*V2+2*X2*V1-2*X3*V3-
     &          3*X3*V4-2*X3*V2-2*X3*V1+2*U3*Y3-2*U3*Y2+3*U4*Y3-3*
     &          U4*Y2+2*U2*Y3-2*U2*Y2+2*U1*Y3-2*U1*Y2)*(X3*KKY-KKX*Y3))
!
!  USES HERE THE 'MAGIC SQUARE' PROPERTIES OF A DIFFUSION-LIKE MATRIX
!  (SUM OF EACH LINE AND EACH COLUMN IS 0)
!
      A13(IELEM) = - A11(IELEM) - A12(IELEM)
      A23(IELEM) = - A22(IELEM) - A21(IELEM)
      A31(IELEM) = - A11(IELEM) - A21(IELEM)
      A32(IELEM) = - A22(IELEM) - A12(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!     OTHER TYPES OF FUNCTIONS F AND G
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
        WRITE(LU,111) IELMG,SG%NAME
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'MT03AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
111     FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
