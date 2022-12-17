!                   *****************
                    SUBROUTINE MT06OC
!                   *****************
!
     &(A11,A12,A13,A22,A23,A33,
     & XMUL,SF,F,LGSEG,IKLE1,IKLE2,IKLE3,NBOR,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                              /
!+                    A    =   /  F (P *P )*J(X,Y) DX
!+                     I J    /L      I  J
!+
!+     BY ELEMENTARY CELL; THE ELEMENT IS THE P2 SEGMENT
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  A FROEHLY (MATMECA)
!+        01/07/08
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
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION F USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF SEGMENTS
!| IKLE2          |-->| SECOND POINTS OF SEGMENTS
!| IKLE3          |-->| THIRD POINTS OF SEGMENTS (QUADRATIC)
!| LGSEG          |-->| LENGTH OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT06OC => MT06OC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NBOR(NELMAX,*)
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN)    :: LGSEG(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(NELMAX),A12(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A13(NELMAX),A22(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A23(NELMAX),A33(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION SUR30,SUR60,SUR420,DET1,F1,F2,F3
!
!-----------------------------------------------------------------------
!
      SUR30  = XMUL/30.D0
      SUR60  = XMUL/60.D0
      SUR420  = XMUL/420.D0
!
!-----------------------------------------------------------------------
!
      IELMF = SF%ELM
!
!     F CONSTANT BY SEGMENT, IN A BOUNDARY ARRAY
!
      IF(IELMF.EQ.0) THEN
!
      DO IELEM = 1 , NELEM
      F1 = F(IELEM)
      DET1 = LGSEG(IELEM) * SUR30
!
      A11(IELEM) = DET1 * (4.D0*F1)
      A12(IELEM) = DET1 * (-F1)
      A13(IELEM) = DET1 * (2.D0*F1)
      A22(IELEM) = A11(IELEM)
      A23(IELEM) = A13(IELEM)
      A33(IELEM) = DET1 * (16.D0*F1)
!
      ENDDO ! IELEM
!
!     F LINEAR BY SEGMENT, IN A BOUNDARY ARRAY
!     NOTE: IKLE IS HERE A BOUNDARY IKLE
!
      ELSEIF(IELMF.EQ.1) THEN
!
      DO IELEM = 1 , NELEM
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
!
      DET1 = LGSEG(IELEM) * SUR60
!
      A11(IELEM) = DET1 * (7.D0*F1+F2)
      A12(IELEM) = DET1 * (-F1-F2)
      A13(IELEM) = DET1 * (4.D0*F1)
      A22(IELEM) = DET1 * (F1+7.D0*F2)
      A23(IELEM) = DET1 * (4.D0*F2)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2)
!
      ENDDO ! IELEM
!
!     F LINEAR, IN AN ARRAY DEFINED ON THE DOMAIN
!
      ELSEIF(IELMF.EQ.11.OR.IELMF.EQ.21) THEN
!
      DO IELEM = 1 , NELEM
!
      F1 = F(NBOR(IELEM,1))
      F2 = F(NBOR(IELEM,2))
!
      DET1 = LGSEG(IELEM) * SUR60
!
      A11(IELEM) = DET1 * (7.D0*F1+F2)
      A12(IELEM) = DET1 * (-F1-F2)
      A13(IELEM) = DET1 * (4.D0*F1)
      A22(IELEM) = DET1 * (F1+7.D0*F2)
      A23(IELEM) = DET1 * (4.D0*F2)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2)
!
      ENDDO ! IELEM
!
!     F QUADRATIC BY SEGMENT, IN A BOUNDARY ARRAY
!     NOTE: IKLE IS HERE A BOUNDARY IKLE
!
      ELSEIF(IELMF.EQ.2) THEN
!
      DO IELEM = 1 , NELEM
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
      DET1 = LGSEG(IELEM) * SUR420
!
      A11(IELEM) = DET1 * (39.D0*F1-3.D0*F2+20.D0*F3)
      A12(IELEM) = DET1 * (-3.D0*F1-3.D0*F2-8.D0*F3)
      A13(IELEM) = DET1 * (20.D0*F1-8.D0*F2+16.D0*F3)
      A22(IELEM) = DET1 * (-3.D0*F1+39.D0*F2+20.D0*F3)
      A23(IELEM) = DET1 * (-8.D0*F1+20.D0*F2+16.D0*F3)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2+12.D0*F3)
!
      ENDDO ! IELEM
!
!     F QUADRATIC, IN AN ARRAY DEFINED ON THE DOMAIN
!
      ELSEIF(IELMF.EQ.13) THEN
!
      DO IELEM = 1 , NELEM
!
      F1 = F(NBOR(IELEM,1))
      F2 = F(NBOR(IELEM,2))
      F3 = F(NBOR(IELEM,3))
!
      DET1 = LGSEG(IELEM) * SUR420
!
      A11(IELEM) = DET1 * (39.D0*F1-3.D0*F2+20.D0*F3)
      A12(IELEM) = DET1 * (-3.D0*F1-3.D0*F2-8.D0*F3)
      A13(IELEM) = DET1 * (20.D0*F1-8.D0*F2+16.D0*F3)
      A22(IELEM) = DET1 * (-3.D0*F1+39.D0*F2+20.D0*F3)
      A23(IELEM) = DET1 * (-8.D0*F1+20.D0*F2+16.D0*F3)
      A33(IELEM) = DET1 * 16.D0 * (F1+F2+12.D0*F3)
!
      ENDDO ! IELEM
!
!     OTHER TYPES OF DISCRETISATION OF F
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'MT06OC (BIEF) :',/,
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
