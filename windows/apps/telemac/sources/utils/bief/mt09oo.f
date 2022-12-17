!                   *****************
                    SUBROUTINE MT09OO
!                   *****************
!
     &(A11,A12,A21,A22,XMUL,SF,F,G,SU,U,V,
     & IKLE1,IKLE2,NBOR,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                                  /           -->   -->         ->->
!+                    A    = XMUL  /   PSI1(I)* U . GRAD(PSI2(J)) U.N DX
!+                     I J        /L
!+
!+     BY ELEMENTARY CELL; THE ELEMENT IS THE P1 SEGMENT
!+
!+
!+     WARNING : F AND G MUST BE X AND Y VECTOR OF THE EL. SEGMENT
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  F  LEPEINTRE (LNH)
!+        04/09/92
!+
!+
!
!history  J. PARISI & C. PEYRARD (EDF R&D, LNHE)
!+        28/06/2013
!+        V5P9
!+    First version.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION F USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF SEGMENTS
!| IKLE2          |-->| SECOND POINTS OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!  EX_MT09OO => MT09OO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),NBOR(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL,U(*),V(*)
!
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A12(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A22(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION SUR24,U1N,U2N,U1,U2,V1,V2
      DOUBLE PRECISION TETA1,TETA2
!
!-----------------------------------------------------------------------
!
      SUR24  = XMUL/24.D0
!
!-----------------------------------------------------------------------
!
      IELMF = SU%ELM
!
!     F LINEAR BY SEGMENT, IN A BOUNDARY ARRAY
!     NOTE: IKLE IS HERE A BOUNDARY IKLE
!
      IF (IELMF.EQ.11) THEN
!
        DO IELEM = 1 , NELEM
!
        U1 = U(NBOR(IKLE1(IELEM)))
        U2 = U(NBOR(IKLE2(IELEM)))
!
        V1 = V(NBOR(IKLE1(IELEM)))
        V2 = V(NBOR(IKLE2(IELEM)))
!
        U1N=SQRT(U1**2+V1**2)
        U2N=SQRT(U2**2+V2**2)
!
        IF(U1N.GT.1.D-5.AND.U2N.GT.1.D-5) THEN
!
!
          TETA1= ASIN((U1*F(IELEM)+V1*G(IELEM))/U1N)
          TETA2= ASIN((U2*F(IELEM)+V2*G(IELEM))/U2N)
!
!
          A11(IELEM) = SUR24*(3.D0*SIN(2*TETA1)*(U1N**2)
     &                 +        SIN(2*TETA2)*(U2N**2)
     &                 + 2.D0*U1N*U2N*SIN(TETA1+TETA2))
          A12(IELEM) = -A11(IELEM)
          A21(IELEM) = SUR24*(SIN(2*TETA1)*(U1N**2)
     &                 +   3.D0*SIN(2*TETA2)*(U2N**2)
     &                 + 2.D0*U1N*U2N*SIN(TETA1+TETA2))
          A22(IELEM) = -A21(IELEM)
!
!    SI COURANT NUL
!
        ELSE
          A11(IELEM) = 0.D0
          A12(IELEM) = 0.D0
          A21(IELEM) = 0.D0
          A22(IELEM) = 0.D0
        ENDIF
!
      ENDDO
!
!     OTHER TYPES OF DISCRETISATION OF F
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'MT0900 (BIEF) :',/,
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
