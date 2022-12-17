!                   *****************
                    SUBROUTINE MT08TT
!                   *****************
!
     &( T,XM,XMUL,X,Y,SF,F,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+
!+               /
!+  A    = XMUL / P   F . GRAD(P ) * J(X,Y) DXDY
!+   I J       /S  J            I
!+
!+  BY ELEMENTARY CELL; THE ELEMENT IS THE P1 TRIANGLE
!+
!+  J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!note     ONLY THE Z COMPONENT IS TREATED HERE !!
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        21/03/02
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
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT08TT => MT08TT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN) :: NELEM,NELMAX
      INTEGER,INTENT(IN) :: IKLE(NELMAX,4)
!
      DOUBLE PRECISION,INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,12)
!
      DOUBLE PRECISION,INTENT(IN) :: XMUL
      DOUBLE PRECISION,INTENT(IN) :: F(*),X(*),Y(*)
!
!     STRUCTURE OF F
!
      TYPE(BIEF_OBJ),INTENT(IN) :: SF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,F1,F2,F3,F4,XSUR120
!
      INTEGER I1,I2,I3,I4,IELEM
!
!**********************************************************************
!
      XSUR120 = XMUL/120.D0
!
      IF(SF%ELM.NE.31.AND.SF%ELM.NE.51) THEN
        WRITE(LU,1001) SF%ELM
1001    FORMAT(1X,'MT08TT (BIEF): TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM=1,NELEM
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        I4 = IKLE(IELEM,4)
!
        X2 = X(I2)-X(I1)
        X3 = X(I3)-X(I1)
        X4 = X(I4)-X(I1)
!
        Y2 = Y(I2)-Y(I1)
        Y3 = Y(I3)-Y(I1)
        Y4 = Y(I4)-Y(I1)
!
        F1 = F(I1)
        F2 = F(I2)
        F3 = F(I3)
        F4 = F(I4)
!
        T(IELEM,1)=(
     & X2*Y4*F3-X3*Y4*F3+X4*Y3*F2-Y2*X4*F2+X2*Y4*F2+2*Y2*X3*F1
     &-2*X3*Y4*F1+Y2*X3*F2+Y2*X3*F3+2*X4*Y3*F1+2*X2*Y4*F1-2*Y2*X4*F1
     &-2*X2*Y3*F1-Y2*X4*F3-X2*Y3*F2-X2*Y3*F3+X4*Y3*F3-X3*Y4*F4
     &+X2*Y4*F4-Y2*X4*F4-X2*Y3*F4+Y2*X3*F4+X4*Y3*F4-X3*Y4*F2 )*XSUR120
!
        T(IELEM,2)=(
     & X3*Y4*F3-2*X4*Y3*F2+X3*Y4*F1-X4*Y3*F1-X4*Y3*F3+X3*Y4*F4
     &-X4*Y3*F4+2*X3*Y4*F2                        )*XSUR120
!
        T(IELEM,3)=(-X2*Y4+Y2*X4)*(F1+2*F3+F2+F4) *XSUR120
!
        T(IELEM,4)=(
     & -Y2*X3*F1-Y2*X3*F2-Y2*X3*F3+X2*Y3*F1+X2*Y3*F2+X2*Y3*F3+
     &2*X2*Y3*F4-2*Y2*X3*F4                       )*XSUR120
!
        XM(IELEM,01)=(
     & X2*Y4*F3-X3*Y4*F3+2*X4*Y3*F2-2*Y2*X4*F2+2*X2*Y4*F2+Y2*X3*F1
     &-X3*Y4*F1+2*Y2*X3*F2+Y2*X3*F3+X4*Y3*F1+X2*Y4*F1-Y2*X4*F1-X2*Y3
     &*F1-Y2*X4*F3-2*X2*Y3*F2-X2*Y3*F3+X4*Y3*F3
     &-X3*Y4*F4+X2*Y4*F4-Y2*X4*F4
     &-X2*Y3*F4+Y2*X3*F4+X4*Y3*F4-2*X3*Y4*F2    )*XSUR120
!
        XM(IELEM,02)=(
     &-X3*Y4+X4*Y3+X2*Y4-Y2*X4-X2*Y3+Y2*X3)*(F1+2*F3+F2+F4)*XSUR120
!
        XM(IELEM,03)=(
     & X2*Y4*F3-X3*Y4*F3+X4*Y3*F2-Y2*X4*F2+X2*Y4*F2+Y2*X3*F1-X3*Y4*F1
     &+Y2*X3*F2+Y2*X3*F3+X4*Y3*F1+X2*Y4*F1-Y2*X4*F1-X2*Y3*F1-Y2*X4*F3
     &-X2*Y3*F2-X2*Y3*F3+X4*Y3*F3-2*X3*Y4*F4+2*X2*Y4*F4-2*Y2*X4*F4-2
     &*X2*Y3*F4+2*Y2*X3*F4+2*X4*Y3*F4-X3*Y4*F2)*XSUR120
!
        XM(IELEM,04)= -(-X3*Y4+X4*Y3)*(F1+2*F3+F2+F4)*XSUR120
!
        XM(IELEM,05)=( X3*Y4*F3-X4*Y3*F2+X3*Y4*F1-X4*Y3*F1-X4*Y3*F3
     &                 +2*X3*Y4*F4-2*X4*Y3*F4+X3*Y4*F2)*XSUR120
!
        XM(IELEM,06)=( -X2*Y4*F3+Y2*X4*F2-X2*Y4*F2-X2*Y4*F1+Y2*X4*F1
     &                 +Y2*X4*F3-2*X2*Y4*F4+2*Y2*X4*F4)*XSUR120
!
        XM(IELEM,07)=( X3*Y4*F3-X4*Y3*F2+2*X3*Y4*F1-2*X4*Y3*F1
     &                -X4*Y3*F3+X3*Y4*F4-X4*Y3*F4+X3*Y4*F2)*XSUR120
!
        XM(IELEM,08)=( -X2*Y4*F3+Y2*X4*F2-X2*Y4*F2-2*X2*Y4*F1
     &                 +2*Y2*X4*F1+Y2*X4*F3-X2*Y4*F4+Y2*X4*F4)*XSUR120
!
        XM(IELEM,09)=( -X2*Y4*F3+2*Y2*X4*F2-2*X2*Y4*F2-X2*Y4*F1
     &                 +Y2*X4*F1+Y2*X4*F3-X2*Y4*F4+Y2*X4*F4 )*XSUR120
!
        XM(IELEM,10)=( -2*Y2*X3*F1-Y2*X3*F2-Y2*X3*F3+2*X2*Y3*F1
     &                 +X2*Y3*F2+X2*Y3*F3+X2*Y3*F4-Y2*X3*F4)*XSUR120
!
        XM(IELEM,11)=(-Y2*X3*F1-2*Y2*X3*F2-Y2*X3*F3+X2*Y3*F1
     &                +2*X2*Y3*F2+X2*Y3*F3+X2*Y3*F4-Y2*X3*F4)*XSUR120
!
        XM(IELEM,12)= -(-X2*Y3+Y2*X3)*(F1+2*F3+F2+F4)*XSUR120
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
