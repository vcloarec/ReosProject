!                   *****************
                    SUBROUTINE VC01OO
!                   *****************
!
     &(XMUL,SF,F,LGSEG,IKLE1,IKLE2,NBOR,NELEM,NELMAX,W1,W2)
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
!+    PSI(I) IS A BASE OF TYPE P1 SEGMENT
!+
!+    F IS A VECTOR OF TYPE IELMF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF SEGMENTS
!| IKLE2          |-->| SECOND POINT OF SEGMENTS
!| LGSEG          |-->| LENGTH OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC01OO => VC01OO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: NBOR(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: LGSEG(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION XSUR3,XSUR6,F1,F2,V1,V2
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!
!     F IS CONSTANT BY SEGMENTS
!
      IF(IELMF.EQ.0) THEN
!
      DO IELEM = 1,NELEM
        W1(IELEM) = 0.5D0*XMUL*F(IELEM)*LGSEG(IELEM)
        W2(IELEM) = W1(IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     F IS LINEAR BY SEGMENTS
!
      ELSEIF(IELMF.EQ.1) THEN
!
      XSUR3 = XMUL/3.D0
      XSUR6 = XMUL/6.D0
!
      DO IELEM = 1,NELEM
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        V1 = ( F1*XSUR3 + F2*XSUR6 )
        V2 = ( F2*XSUR3 + F1*XSUR6 )
        W1(IELEM) = V1 * LGSEG(IELEM)
        W2(IELEM) = V2 * LGSEG(IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     F IS LINEAR BY TRIANGLES OR QUADRILATERALS OR QUASI-BUBBLE
!
      ELSEIF(IELMF.EQ.11.OR.IELMF.EQ.12.OR.IELMF.EQ.21) THEN
!
      XSUR3 = XMUL/3.D0
      XSUR6 = XMUL/6.D0
!
      DO IELEM = 1,NELEM
        F1 = F(NBOR(IKLE1(IELEM)))
        F2 = F(NBOR(IKLE2(IELEM)))
        V1 = ( F1*XSUR3 + F2*XSUR6 )
        V2 = ( F2*XSUR3 + F1*XSUR6 )
        W1(IELEM) = V1 * LGSEG(IELEM)
        W2(IELEM) = V2 * LGSEG(IELEM)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'VC01OO (BIEF) :',/,
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
