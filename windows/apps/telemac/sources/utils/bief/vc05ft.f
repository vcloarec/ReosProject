!                   *****************
                    SUBROUTINE VC05FT
!                   *****************
!
     &(XMUL,SU,SV,U,V,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX,W1,W2,W3)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /         ->
!+    VEC(I) = XMUL  /    (U,V).N  PSI(I) D(GAMMA)
!+                  /GAMMA
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE ON A LATERAL VERTICAL
!+    MESH (PRISMS MESH SPLIT IN TETRAHEDRONS)
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W1,2,3 IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
!+        V6P0
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
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC05FT => VC05FT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NBOR(*),NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)   :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT):: W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
!
!-----------------------------------------------------------------------
!
!     STRUCTURES OF U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3
      DOUBLE PRECISION XSUR24
      INTEGER IELMU,IELMV,IELEM,N1,N2,N3,I1,I2,I3
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
      XSUR24 = XMUL/24.D0
!
!     U LINEAR BY TETRAHEDRONS
!
!-----------------------------------------------------------------------
!
      IF(IELMU.EQ.61.AND.IELMV.EQ.61) THEN
!
!-----------------------------------------------------------------------
!
!   LOOP ON THE BOUNDARY SIDES
!
        DO IELEM = 1,NELEM
!
!         LOCAL NUMBERING OF THE SIDE NODES
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
!
!         GLOBAL NUMBERING OF THE SIDE NODES
!
          N1 = NBOR(I1)
          N2 = NBOR(I2)
          N3 = NBOR(I3)
!
          U1 = U(I1)
          U2 = U(I2)
          U3 = U(I3)
          V1 = V(I1)
          V2 = V(I2)
          V3 = V(I3)
!
          X1 = X(N1)
          X2 = X(N2)-X1
          X3 = X(N3)-X1
          Y1 = Y(N1)
          Y2 = Y(N2)-Y1
          Y3 = Y(N3)-Y1
          Z1 = Z(N1)
          Z2 = Z(N2)-Z1
          Z3 = Z(N3)-Z1
!
!         COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
!
!         NOTE: VECTOR NORMAL TO THE TRIANGLE,
!               WHICH NORM IS THE SURFACE:
!
!                    0.5  (Y2*Z3-Y3*Z2)
!                    0.5  (X3*Z2-X2*Z3)
!                    0.5  (X2*Y3-X3*Y2)  : THIS TERM =0
!
!         INSPIRED FROM MASVEC ON TRIANGLES :
!
!
          W1(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(2*U1+  U2+  U3)
     &                          +(X3*Z2-X2*Z3)*(2*V1+  V2+  V3) )
          W2(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+2*U2+  U3)
     &                          +(X3*Z2-X2*Z3)*(  V1+2*V2+  V3) )
          W3(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+  U2+2*U3)
     &                          +(X3*Z2-X2*Z3)*(  V1+  V2+2*V3) )
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.51.AND.IELMV.EQ.51) THEN
!
!-----------------------------------------------------------------------
!
!   LOOP ON THE BOUNDARY SIDES
!
        DO IELEM = 1,NELEM
!
!         GLOBAL NUMBERING OF THE SIDE NODES
!
          N1 = NBOR(IKLE1(IELEM))
          N2 = NBOR(IKLE2(IELEM))
          N3 = NBOR(IKLE3(IELEM))
!
          U1 = U(N1)
          U2 = U(N2)
          U3 = U(N3)
          V1 = V(N1)
          V2 = V(N2)
          V3 = V(N3)
          X1 = X(N1)
          X2 = X(N2)-X1
          X3 = X(N3)-X1
          Y1 = Y(N1)
          Y2 = Y(N2)-Y1
          Y3 = Y(N3)-Y1
          Z1 = Z(N1)
          Z2 = Z(N2)-Z1
          Z3 = Z(N3)-Z1
!
!         COMPUTES THE AREA OF THE TRIANGLE (BY VECTOR PRODUCT)
!
!         NOTE: VECTOR NORMAL TO THE TRIANGLE,
!               WHICH NORM IS THE SURFACE:
!
!                    0.5  (Y2*Z3-Y3*Z2)
!                    0.5  (X3*Z2-X2*Z3)
!                    0.5  (X2*Y3-X3*Y2)  : THIS TERM =0
!
!         INSPIRED FROM MASVEC ON TRIANGLES :
!
!
          W1(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(2*U1+  U2+  U3)
     &                          +(X3*Z2-X2*Z3)*(2*V1+  V2+  V3) )
          W2(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+2*U2+  U3)
     &                          +(X3*Z2-X2*Z3)*(  V1+2*V2+  V3) )
          W3(IELEM) = XSUR24* (  (Y2*Z3-Y3*Z2)*(  U1+  U2+2*U3)
     &                          +(X3*Z2-X2*Z3)*(  V1+  V2+2*V3) )
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMU,SU%NAME
101     FORMAT(1X,'VC05FT (BIEF) :',/,
     &         1X,'DISCRETIZATION OF U NOT AVAILABLE:',1I6,
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
