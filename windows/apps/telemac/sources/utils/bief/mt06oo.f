!                   *****************
                    SUBROUTINE MT06OO
!                   *****************
!
     &(A11,A12,A22,XMUL,SF,F,LGSEG,IKLE1,IKLE2,NBOR,NELEM,NELMAX)
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
!+     BY ELEMENTARY CELL; THE ELEMENT IS THE P1 SEGMENT
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
!history  J-M HERVOUET (LNHE)
!+        20/03/08
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
!| LGSEG          |-->| LENGTH OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT06OO => MT06OO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),NBOR(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN)    :: LGSEG(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A12(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A22(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION SUR12,DET1,F1,F2,F12
!
!-----------------------------------------------------------------------
!
      SUR12  = XMUL/12.D0
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
!         TO BE OPTIMISED...
          F1 = F(IELEM)
          F2 = F(IELEM)
!
          F12 = F1 + F2
!
          DET1 = LGSEG(IELEM) * SUR12
!
          A11(IELEM) = DET1 * (F12+2*F1)
          A12(IELEM) = DET1 * F12
          A22(IELEM) = DET1 * (F12+2*F2)
!
        ENDDO
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
          F12 = F1 + F2
          DET1 = LGSEG(IELEM) * SUR12
          A11(IELEM) = DET1 * (F12+2*F1)
          A12(IELEM) = DET1 * F12
          A22(IELEM) = DET1 * (F12+2*F2)
!
        ENDDO
!
!     F LINEAR, IN AN ARRAY DEFINED ON THE DOMAIN
!
      ELSEIF(IELMF.EQ.11.OR.IELMF.EQ.21) THEN
!
        DO IELEM = 1 , NELEM
!
          F1 = F(NBOR(IKLE1(IELEM)))
          F2 = F(NBOR(IKLE2(IELEM)))
          F12 = F1 + F2
          DET1 = LGSEG(IELEM) * SUR12
          A11(IELEM) = DET1 * (F12+2*F1)
          A12(IELEM) = DET1 * F12
          A22(IELEM) = DET1 * (F12+2*F2)
!
        ENDDO
!
!     OTHER TYPES OF DISCRETISATION OF F
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'MT0600 (BIEF) :',/,
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
