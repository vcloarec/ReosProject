!                   *****************
                    SUBROUTINE VC01AA
!                   *****************
!
     &( XMUL,SF,F,SURFAC,
     &  IKLE1,IKLE2,IKLE3,NELEM,NELMAX,
     &  W1,W2,W3 )
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I) * F  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!+
!+    F IS A VECTOR OF DISCRETISATION P0, P1 OR DISCONTINUOUS P1
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        29/10/99
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC01AA => VC01AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION XSUR03,XSUR12,F1,F2,F3,F123,COEF
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!
!     F IS LINEAR
!
      IF(IELMF.EQ.11) THEN
!
      XSUR12 = XMUL / 12.D0
!
      DO IELEM = 1 , NELEM
!
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
        F123  = F1 + F2 + F3
!
        COEF = XSUR12 * SURFAC(IELEM)
!
        W1(IELEM) = COEF * ( F123 + F1 )
        W2(IELEM) = COEF * ( F123 + F2 )
        W3(IELEM) = COEF * ( F123 + F3 )
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     F IS CONSTANT BY ELEMENT
!
      ELSEIF(IELMF.EQ.10.AND.SF%DIM2.EQ.1) THEN
!
      XSUR03 = XMUL / 3.D0
!
      DO IELEM = 1 , NELEM
!
        W1(IELEM) = XSUR03 * SURFAC(IELEM) * F(IELEM)
        W2(IELEM) = W1(IELEM)
        W3(IELEM) = W1(IELEM)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     F IS DISCONTINUOUS P1
!
      ELSEIF(IELMF.EQ.15) THEN
!
      XSUR12 = XMUL / 12.D0
!
      DO IELEM = 1 , NELEM
!
        F1  = F(IELEM)
        F2  = F(IELEM+NELEM)
        F3  = F(IELEM+2*NELEM)
        F123  = F1 + F2 + F3
!
        COEF = XSUR12 * SURFAC(IELEM)
!
        W1(IELEM) = COEF * ( F123 + F1 )
        W2(IELEM) = COEF * ( F123 + F2 )
        W3(IELEM) = COEF * ( F123 + F3 )
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'VC01AA (BIEF) :',/,
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
