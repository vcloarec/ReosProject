!                   *****************
                    SUBROUTINE VC00PP
!                   *****************
!
     &(XMUL,Z,SURFAC,IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     & W1,W2,W3,W4,W5,W6,FORMUL)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 PRISM
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        09/12/94
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
!| FORMUL         |-->| STRING WITH THE FORMULA DESCRIBING THE VECTOR
!| IKLE1          |-->| FIRST POINT OF PRISMS
!| IKLE2          |-->| SECOND POINT OF PRISMS
!| IKLE3          |-->| THIRD POINT OF PRISMS
!| IKLE4          |-->| FOURTH POINT OF PRISMS
!| IKLE5          |-->| FIFTH POINT OF PRISMS
!| IKLE6          |-->| SIXTH POINT OF PRISMS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| W5             |<--| RESULT IN NON ASSEMBLED FORM
!| W6             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Z              |-->| ELEVATIONS OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: Z(*)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W4(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W5(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
      DOUBLE PRECISION XSUR24,XSUR6,H1,H2,H3,SHT,COEF
!
!-----------------------------------------------------------------------
!
      XSUR24 = XMUL/24.D0
      XSUR6  = XMUL/6.D0
!
!-----------------------------------------------------------------------
!
      IF(FORMUL(1:7).EQ.'MASBAS ') THEN
!
!     STANDARD FORMULA
!
!     LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
        H1 = Z(IKLE4(IELEM)) - Z(IKLE1(IELEM))
        H2 = Z(IKLE5(IELEM)) - Z(IKLE2(IELEM))
        H3 = Z(IKLE6(IELEM)) - Z(IKLE3(IELEM))
        SHT = H1 + H2 + H3
!
        COEF = XSUR24 * SURFAC(IELEM)
!
        W1(IELEM) = COEF * (SHT+H1)
        W2(IELEM) = COEF * (SHT+H2)
        W3(IELEM) = COEF * (SHT+H3)
        W4(IELEM) = W1(IELEM)
        W5(IELEM) = W2(IELEM)
        W6(IELEM) = W3(IELEM)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(1:7).EQ.'MASBAS2') THEN
!
!     FORMULA WITH MASS-LUMPING
!
!     LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
        H1 = Z(IKLE4(IELEM)) - Z(IKLE1(IELEM))
        H2 = Z(IKLE5(IELEM)) - Z(IKLE2(IELEM))
        H3 = Z(IKLE6(IELEM)) - Z(IKLE3(IELEM))
!
        COEF = XSUR6 * SURFAC(IELEM)
!
        W1(IELEM) = COEF * H1
        W2(IELEM) = COEF * H2
        W3(IELEM) = COEF * H3
        W4(IELEM) = W1(IELEM)
        W5(IELEM) = W2(IELEM)
        W6(IELEM) = W3(IELEM)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,*) ' '
        WRITE(LU,*) 'UNKNOWN FORMULA IN VC00PP:',FORMUL
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
