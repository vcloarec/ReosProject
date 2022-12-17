!                   *****************
                    SUBROUTINE MT04TT
!                   *****************
!
     &( T,XM,XMUL,SU,SV,SW,U,V,W,X,Y,Z,IKLE,NELEM,NELMAX,FORMUL)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES A MATRIX FOR THE SUPG METHOD.
!code
!+    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!+
!+                                 / ->             ->
!+                    A    = XMUL /  U . GRAD(P ) * U . GRAD(P ) * J(X,Y) DXDY
!+                     I J       /S            I              J
!+
!+    BY ELEMENTARY CELL; THE ELEMENT IS THE TETRAHEDRON WITH LINEAR INTERPOLATION
!+
!+    J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE VERTICAL COMPONENT IS HERE NEGLECTED,
!+            IN ACCORDANCE WITH THE PRINCIPLE NOTE,
!+            AND CONTRARY TO THE PRISMS
!code
!+     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!+     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!+     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!+     XM(IELEM, 4)  ---->  M(2,3) = M(3,2)
!+     XM(IELEM, 5)  ---->  M(2,4) = M(4,2)
!+     XM(IELEM, 6)  ---->  M(3,4) = M(4,3)
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        22/08/05
!+        V5P6
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
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| STRUCTURE OF FUNCTIONS U
!| SV             |-->| STRUCTURE OF FUNCTIONS V
!| SW             |-->| STRUCTURE OF FUNCTIONS W
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| U              |-->| FUNCTION USED IN THE FORMULA
!| V              |-->| FUNCTION USED IN THE FORMULA
!| W              |-->| FUNCTION USED IN THE FORMULA
!| X              |-->| ABSCISSAE OF POINTS
!| Y              |-->| ORDINATES OF POINTS
!| Z              |-->| ELEVATIONS OF POINTS
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT04TT => MT04TT
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*)
!
!     STRUCTURES OF U, V, W
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV,SW
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     SPECIFIC DECLARATIONS
!
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,W1,W2,W3,W4,XJAC
      DOUBLE PRECISION COEF,SUR120,AUX1,AUX2,AUX4,AUX5,AUX6,AUX7
      DOUBLE PRECISION A1,A2,A3
      INTEGER I1,I2,I3,I4,IELEM
!
!***********************************************************************
!
      SUR120=XMUL/120.D0
!
      IF(FORMUL(1:7).EQ.'MAUGUG2') THEN
!
      IF((SU%ELM.EQ.31.AND.SV%ELM.EQ.31.AND.SW%ELM.EQ.31).OR.
     &   (SU%ELM.EQ.51.AND.SV%ELM.EQ.51.AND.SW%ELM.EQ.51)     ) THEN
!
!-----------------------------------------------------------------------
!
!     LINEAR DISCRETISATION OF DIFFUSION COEFFICIENTS
!
!     LOOP ON THE TETRAHEDRONS
!
      DO IELEM=1,NELEM
!
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
!
!-----------------------------------------------------------------------
!
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
!
!-----------------------------------------------------------------------
!
      U1 = U(IKLE(IELEM,1))
      U2 = U(IKLE(IELEM,2))
      U3 = U(IKLE(IELEM,3))
      U4 = U(IKLE(IELEM,4))
      V1 = V(IKLE(IELEM,1))
      V2 = V(IKLE(IELEM,2))
      V3 = V(IKLE(IELEM,3))
      V4 = V(IKLE(IELEM,4))
      W1 = W(IKLE(IELEM,1))
      W2 = W(IKLE(IELEM,2))
      W3 = W(IKLE(IELEM,3))
      W4 = W(IKLE(IELEM,4))
!
      XJAC = X2*(Y3*Z4-Y4*Z3)+Y2*(X4*Z3-X3*Z4)+Z2*(X3*Y4-X4*Y3)
!
      COEF = SUR120/MAX(XJAC,1.D-8)
!
      A1 = -Y3*Z4+Y4*Z3+Y2*Z4-Z2*Y4-Y2*Z3+Z2*Y3
      A2 = -X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3
      A3 = -X3*Y4+X4*Y3+X2*Y4-Y2*X4-X2*Y3+Y2*X3
!
      AUX1 = U3*U2+U1*U3+U1*U2+U4**2+U1*U4+U4*U2+U3**2+U2**2+U1**2+U3*U4
!
      AUX2 = V1*U4+2*V1*U1+V1*U2+V3*U1+V3*U2+V1*U3+V2*U1+V2*U3
     &      +V4*U1+2*V2*U2+2*V3*U3+V4*U2+V2*U4+V4*U3+V3*U4+2*V4*U4
!
      AUX4 = V1**2+V1*V3+V3**2+V2**2+V3*V2+V1*V2+V4**2+V1*V4+V3*V4+V2*V4
      AUX5 = V3*W2+V3*W1+V1*W2+2*V1*W1+V2*W3+V2*W1+V1*W3+2*V3*W3+2*V2*W2
     &+V4*W1+V4*W3+V4*W2+2*V4*W4+V3*W4+V2*W4+V1*W4
      AUX6 = W2**2+W3**2+W2*W3+W1**2+W1*W3+W4**2+W1*W2+W1*W4+W4*W2+W4*W3
      AUX7 = 2*U2*W2+2*U3*W3+U3*W2+U3*W1+U1*W2+2*U1*W1+U2*W3+U2*W1+
     &U1*W3+U4*W2+U4*W1+2*U4*W4+U1*W4+U2*W4+U4*W3+U3*W4
!
      T(IELEM,1)= (A1**2*AUX1-A1*A2*AUX2+A1*A3*AUX7+A2**2*AUX4-A2*
     &A3*AUX5+A3**2*AUX6)*2*COEF
!
      XM(IELEM,1) = (2*(Y3*Z4-Y4*Z3)*A1*AUX1-(Y3*Z4-Y4*Z3)*A2*AUX2+
     &(Y3*Z4-Y4*Z3)*A3*AUX7-(X3*Z4-X4*Z3)*A1*AUX2+
     &2*(X3*Z4-X4*Z3)*A2*AUX4-(X3*Z4-X4*Z3)*A3*AUX5-
     &(-X3*Y4+X4*Y3)*A1*AUX7+(-X3*Y4+X4*Y3)*A2*AUX5-
     &2*(-X3*Y4+X4*Y3)*A3*AUX6)*COEF
!
      XM(IELEM,2) = (2*(-Y2*Z4+Z2*Y4)*A1*AUX1-(-Y2*Z4+Z2*Y4)*A2*AUX2+
     &(-Y2*Z4+Z2*Y4)*A3*AUX7-(-X2*Z4+Z2*X4)*A1*AUX2+
     &2*(-X2*Z4+Z2*X4)*A2*AUX4-(-X2*Z4+Z2*X4)*A3*AUX5-
     &(X2*Y4-Y2*X4)*A1*AUX7+(X2*Y4-Y2*X4)*A2*AUX5-
     &2*(X2*Y4-Y2*X4)*A3*AUX6)*COEF
!
      XM(IELEM,3) = (-2*(-Y2*Z3+Z2*Y3)*A1*AUX1+(-Y2*Z3+Z2*Y3)*A2*AUX2-
     &(-Y2*Z3+Z2*Y3)*A3*AUX7+(-X2*Z3+Z2*X3)*A1*AUX2-
     &2*(-X2*Z3+Z2*X3)*A2*AUX4+(-X2*Z3+Z2*X3)*A3*AUX5+
     &(X2*Y3-Y2*X3)*A1*AUX7-(X2*Y3-Y2*X3)*A2*AUX5+
     &2*(X2*Y3-Y2*X3)*A3*AUX6)*COEF
!
      T(IELEM,2) =((Y3*Z4-Y4*Z3)**2*AUX1-
     &(Y3*Z4-Y4*Z3)*(X3*Z4-X4*Z3)*AUX2
     &-(Y3*Z4-Y4*Z3)*(-X3*Y4+X4*Y3)*AUX7+
     &(X3*Z4-X4*Z3)**2*AUX4+
     &(X3*Z4-X4*Z3)*(-X3*Y4+X4*Y3)*AUX5+
     &(-X3*Y4+X4*Y3)**2*AUX6)*2*COEF
!
      XM(IELEM,4) =  (2*(-Y2*Z4+Z2*Y4)*(Y3*Z4-Y4*Z3)*AUX1-
     &(-Y2*Z4+Z2*Y4)*(X3*Z4-X4*Z3)*AUX2-
     &(-Y2*Z4+Z2*Y4)*(-X3*Y4+X4*Y3)*AUX7
     &-(Y3*Z4-Y4*Z3)*(-X2*Z4+Z2*X4)*AUX2+
     &2*(-X2*Z4+Z2*X4)*(X3*Z4-X4*Z3)*AUX4+
     &(-X2*Z4+Z2*X4)*(-X3*Y4+X4*Y3)*AUX5-
     &(Y3*Z4-Y4*Z3)*(X2*Y4-Y2*X4)*AUX7+
     &(X3*Z4-X4*Z3)*(X2*Y4-Y2*X4)*AUX5+
     &2*(X2*Y4-Y2*X4)*(-X3*Y4+X4*Y3)*AUX6)*COEF
!
      XM(IELEM,5) = (-2*(-Y2*Z3+Z2*Y3)*(Y3*Z4-Y4*Z3)*AUX1+
     &(-Y2*Z3+Z2*Y3)*(X3*Z4-X4*Z3)*AUX2+
     &(-Y2*Z3+Z2*Y3)*(-X3*Y4+X4*Y3)*AUX7+
     &(Y3*Z4-Y4*Z3)*(-X2*Z3+Z2*X3)*AUX2-
     &2*(-X2*Z3+Z2*X3)*(X3*Z4-X4*Z3)*AUX4-
     &(-X2*Z3+Z2*X3)*(-X3*Y4+X4*Y3)*AUX5+
     &(Y3*Z4-Y4*Z3)*(X2*Y3-Y2*X3)*AUX7-
     &(X3*Z4-X4*Z3)*(X2*Y3-Y2*X3)*AUX5-
     &2*(X2*Y3-Y2*X3)*(-X3*Y4+X4*Y3)*AUX6)*COEF
!
      T(IELEM,3) = ((-Y2*Z4+Z2*Y4)**2*AUX1-
     &(-Y2*Z4+Z2*Y4)*(-X2*Z4+Z2*X4)*AUX2-
     &(-Y2*Z4+Z2*Y4)*(X2*Y4-Y2*X4)*AUX7+
     &(-X2*Z4+Z2*X4)**2*AUX4+(-X2*Z4+Z2*X4)*(X2*Y4-Y2*X4)*AUX5+
     &(X2*Y4-Y2*X4)**2*AUX6)*2*COEF
!
      XM(IELEM,6) = (-2*(-Y2*Z3+Z2*Y3)*(-Y2*Z4+Z2*Y4)*AUX1+
     &(-Y2*Z3+Z2*Y3)*(-X2*Z4+Z2*X4)*AUX2+
     &(-Y2*Z3+Z2*Y3)*(X2*Y4-Y2*X4)*AUX7+
     &(-Y2*Z4+Z2*Y4)*(-X2*Z3+Z2*X3)*AUX2-
     &2*(-X2*Z3+Z2*X3)*(-X2*Z4+Z2*X4)*AUX4-
     &(-X2*Z3+Z2*X3)*(X2*Y4-Y2*X4)*AUX5+
     &(-Y2*Z4+Z2*Y4)*(X2*Y3-Y2*X3)*AUX7-
     &(-X2*Z4+Z2*X4)*(X2*Y3-Y2*X3)*AUX5-
     &2*(X2*Y3-Y2*X3)*(X2*Y4-Y2*X4)*AUX6)*COEF
!
      T(IELEM,4) = ((-Y2*Z3+Z2*Y3)**2*AUX1-
     &(-Y2*Z3+Z2*Y3)*(-X2*Z3+Z2*X3)*AUX2-
     &(-Y2*Z3+Z2*Y3)*(X2*Y3-Y2*X3)*AUX7
     &+(-X2*Z3+Z2*X3)**2*AUX4+(-X2*Z3+Z2*X3)*(X2*Y3-Y2*X3)*AUX5+
     &(X2*Y3-Y2*X3)**2*AUX6)*2*COEF
!
!
!-----------------------------------------------------------------------
!
      ENDDO ! IELEM
!
!---------------------------------------------------------------
!
      ELSE IF((SU%ELM.EQ.30.AND.SV%ELM.EQ.30.AND.SW%ELM.EQ.30).OR.
     &        (SU%ELM.EQ.50.AND.SV%ELM.EQ.50.AND.SW%ELM.EQ.50)    ) THEN
!
!   P0 DISCRETISATION OF DIFFUSION COEFFICIENTS
!
!   LOOP ON THE TETRAHEDRONS
!
      DO IELEM=1,NELEM
!
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
!
!-----------------------------------------------------------------------
!
      X2=X(I2)-X(I1)
      Y2=Y(I2)-Y(I1)
      Z2=Z(I2)-Z(I1)
      X3=X(I3)-X(I1)
      Y3=Y(I3)-Y(I1)
      Z3=Z(I3)-Z(I1)
      X4=X(I4)-X(I1)
      Y4=Y(I4)-Y(I1)
      Z4=Z(I4)-Z(I1)
!
!-----------------------------------------------------------------------
!
      U1 = U(IELEM)
      U2 = U1
      U3 = U1
      U4 = U1
      V1 = V(IELEM)
      V2 = V1
      V3 = V1
      V4 = V1
      W1 = W(IELEM)
      W2 = W1
      W3 = W1
      W4 = W1
!
      XJAC = X2*(Y3*Z4-Y4*Z3)+Y2*(X4*Z3-X3*Z4)+Z2*(X3*Y4-X4*Y3)
!
      COEF = SUR120/MAX(XJAC,1.D-8)
!
      A1 = -Y3*Z4+Y4*Z3+Y2*Z4-Z2*Y4-Y2*Z3+Z2*Y3
      A2 = -X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3
      A3 = -X3*Y4+X4*Y3+X2*Y4-Y2*X4-X2*Y3+Y2*X3
!
!     TODO:SIMPLIFICATIONS TO BE DONE HERE !!!!!!!!!!!!!!
!
      AUX1 = U3*U2+U1*U3+U1*U2+U4**2+U1*U4+U4*U2+U3**2+U2**2+U1**2+U3*U4
!
      AUX2 = V1*U4+2*V1*U1+V1*U2+V3*U1+V3*U2+V1*U3+V2*U1+V2*U3
     &      +V4*U1+2*V2*U2+2*V3*U3+V4*U2+V2*U4+V4*U3+V3*U4+2*V4*U4
!
      AUX4 = V1**2+V1*V3+V3**2+V2**2+V3*V2+V1*V2+V4**2+V1*V4+V3*V4+V2*V4
      AUX5 = V3*W2+V3*W1+V1*W2+2*V1*W1+V2*W3+V2*W1+V1*W3+2*V3*W3+2*V2*W2
     &+V4*W1+V4*W3+V4*W2+2*V4*W4+V3*W4+V2*W4+V1*W4
      AUX6 = W2**2+W3**2+W2*W3+W1**2+W1*W3+W4**2+W1*W2+W1*W4+W4*W2+W4*W3
      AUX7 = 2*U2*W2+2*U3*W3+U3*W2+U3*W1+U1*W2+2*U1*W1+U2*W3+U2*W1+
     &U1*W3+U4*W2+U4*W1+2*U4*W4+U1*W4+U2*W4+U4*W3+U3*W4
!
      T(IELEM,1)= (A1**2*AUX1-A1*A2*AUX2+A1*A3*AUX7+A2**2*AUX4-A2*
     &A3*AUX5+A3**2*AUX6)*2*COEF
!
      XM(IELEM,1) = (2*(Y3*Z4-Y4*Z3)*A1*AUX1-
     &(Y3*Z4-Y4*Z3)*A2*AUX2+
     &(Y3*Z4-Y4*Z3)*A3*AUX7-
     &(X3*Z4-X4*Z3)*A1*AUX2+
     &2*(X3*Z4-X4*Z3)*A2*AUX4-
     &(X3*Z4-X4*Z3)*A3*AUX5-
     &(-X3*Y4+X4*Y3)*A1*AUX7+
     &(-X3*Y4+X4*Y3)*A2*AUX5-
     &2*(-X3*Y4+X4*Y3)*A3*AUX6)*COEF
!
      XM(IELEM,2) = (2*(-Y2*Z4+Z2*Y4)*
     &A1*AUX1-(-Y2*Z4+Z2*Y4)*A2*AUX2+(-Y2*Z4+Z2*Y4)*A3*AUX7-
     &(-X2*Z4+Z2*X4)*A1*AUX2+2*(-X2*Z4+Z2*X4)*A2*AUX4-
     &(-X2*Z4+Z2*X4)*A3*AUX5-(X2*Y4-Y2*X4)*A1*AUX7+
     &(X2*Y4-Y2*X4)*A2*AUX5-2*(X2*Y4-Y2*X4)*A3*AUX6)*COEF
!
      XM(IELEM,3) = (-2*(-Y2*Z3+Z2*Y3)*A1*AUX1+(-Y2*Z3+Z2*Y3)*A2*AUX2-
     &(-Y2*Z3+Z2*Y3)*A3*AUX7+(-X2*Z3+Z2*X3)*A1*AUX2-
     &2*(-X2*Z3+Z2*X3)*A2*AUX4+(-X2*Z3+Z2*X3)*A3*AUX5+
     &(X2*Y3-Y2*X3)*A1*AUX7-(X2*Y3-Y2*X3)*A2*AUX5+
     &2*(X2*Y3-Y2*X3)*A3*AUX6)*COEF
!
      T(IELEM,2) =((Y3*Z4-Y4*Z3)**2*AUX1-
     &(Y3*Z4-Y4*Z3)*(X3*Z4-X4*Z3)*AUX2
     &-(Y3*Z4-Y4*Z3)*(-X3*Y4+X4*Y3)*AUX7+
     &(X3*Z4-X4*Z3)**2*AUX4+
     &(X3*Z4-X4*Z3)*(-X3*Y4+X4*Y3)*AUX5+
     &(-X3*Y4+X4*Y3)**2*AUX6)*2*COEF
!
      XM(IELEM,4) =  (2*(-Y2*Z4+Z2*Y4)*(Y3*Z4-Y4*Z3)*AUX1-
     &(-Y2*Z4+Z2*Y4)*(X3*Z4-X4*Z3)*AUX2-
     &(-Y2*Z4+Z2*Y4)*(-X3*Y4+X4*Y3)*AUX7
     &-(Y3*Z4-Y4*Z3)*(-X2*Z4+Z2*X4)*AUX2+
     &2*(-X2*Z4+Z2*X4)*(X3*Z4-X4*Z3)*AUX4+
     &(-X2*Z4+Z2*X4)*(-X3*Y4+X4*Y3)*AUX5-
     &(Y3*Z4-Y4*Z3)*(X2*Y4-Y2*X4)*AUX7+
     &(X3*Z4-X4*Z3)*(X2*Y4-Y2*X4)*AUX5+
     &2*(X2*Y4-Y2*X4)*(-X3*Y4+X4*Y3)*AUX6)*COEF
!
      XM(IELEM,5) = (-2*(-Y2*Z3+Z2*Y3)*(Y3*Z4-Y4*Z3)*AUX1+
     &(-Y2*Z3+Z2*Y3)*(X3*Z4-X4*Z3)*AUX2+
     &(-Y2*Z3+Z2*Y3)*(-X3*Y4+X4*Y3)*AUX7+
     &(Y3*Z4-Y4*Z3)*(-X2*Z3+Z2*X3)*AUX2-
     &2*(-X2*Z3+Z2*X3)*(X3*Z4-X4*Z3)*AUX4-
     &(-X2*Z3+Z2*X3)*(-X3*Y4+X4*Y3)*AUX5+
     &(Y3*Z4-Y4*Z3)*(X2*Y3-Y2*X3)*AUX7-
     &(X3*Z4-X4*Z3)*(X2*Y3-Y2*X3)*AUX5-
     &2*(X2*Y3-Y2*X3)*(-X3*Y4+X4*Y3)*AUX6)*COEF
!
      T(IELEM,3) = ((-Y2*Z4+Z2*Y4)**2*AUX1-
     &(-Y2*Z4+Z2*Y4)*(-X2*Z4+Z2*X4)*AUX2-
     &(-Y2*Z4+Z2*Y4)*(X2*Y4-Y2*X4)*AUX7+
     &(-X2*Z4+Z2*X4)**2*AUX4+(-X2*Z4+Z2*X4)*(X2*Y4-Y2*X4)*AUX5+
     &(X2*Y4-Y2*X4)**2*AUX6)*2*COEF
!
      XM(IELEM,6) = (-2*(-Y2*Z3+Z2*Y3)*(-Y2*Z4+Z2*Y4)*AUX1+
     &(-Y2*Z3+Z2*Y3)*(-X2*Z4+Z2*X4)*AUX2+
     &(-Y2*Z3+Z2*Y3)*(X2*Y4-Y2*X4)*AUX7+
     &(-Y2*Z4+Z2*Y4)*(-X2*Z3+Z2*X3)*AUX2-
     &2*(-X2*Z3+Z2*X3)*(-X2*Z4+Z2*X4)*AUX4-
     &(-X2*Z3+Z2*X3)*(X2*Y4-Y2*X4)*AUX5+
     &(-Y2*Z4+Z2*Y4)*(X2*Y3-Y2*X3)*AUX7-
     &(-X2*Z4+Z2*X4)*(X2*Y3-Y2*X3)*AUX5-
     &2*(X2*Y3-Y2*X3)*(X2*Y4-Y2*X4)*AUX6)*COEF
!
      T(IELEM,4) = ((-Y2*Z3+Z2*Y3)**2*AUX1-
     &(-Y2*Z3+Z2*Y3)*(-X2*Z3+Z2*X3)*AUX2-
     &(-Y2*Z3+Z2*Y3)*(X2*Y3-Y2*X3)*AUX7
     &+(-X2*Z3+Z2*X3)**2*AUX4+(-X2*Z3+Z2*X3)*(X2*Y3-Y2*X3)*AUX5+
     &(X2*Y3-Y2*X3)**2*AUX6)*2*COEF
!
!-----------------------------------------------------------------------
!
      ENDDO ! IELEM
!
!---------------------------------------------------------------
      ELSE
!
        WRITE(LU,1001) SU%ELM,SV%ELM,SW%ELM
1001    FORMAT(1X,'MT04TT (BIEF) : WRONG TYPE OF U,V OR W: ',
     &  I6,1X,I6,1X,I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     ELSEIF(FORMUL(1:7).EQ.'MAUGUG1') THEN
!
!
!
      ELSE
        WRITE(LU,4001) FORMUL
4001    FORMAT(1X,'MT04TT (BIEF) : UNEXPECTED FORMULA: ',A16)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
