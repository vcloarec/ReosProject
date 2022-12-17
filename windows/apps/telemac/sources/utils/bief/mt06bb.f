!                   *****************
                    SUBROUTINE MT06BB
!                   *****************
!
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SF,F,SURFAC,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE FOLLOWING MATRIX:
!code
!+     SUM(F*PSII*PSIJ)
!+
!+            WITH:  P1 QUASI-BUBBLE
!+                   P2 QUASI-BUBBLE
!+                   F P1 OR QUASI-BUBBLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
!+        10/01/95
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
!| A14            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| A44            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION F USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT06BB => MT06BB
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
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) ::                      A44(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      DOUBLE PRECISION F1,F2,F3,F4,XMS090,XMS180,XMS540
      DOUBLE PRECISION XMS018,XMS054,XMS006,XMS009,XMS036
      INTEGER IELMF,IELEM
!
!=======================================================================
!
!     EXTRACTS THE TYPE OF ELEMENT FOR F
!
      IELMF = SF%ELM
!
!  CASE WHERE F IS P0
!
      IF(IELMF.EQ.10) THEN
!
      XMS009 = XMUL /  9.D0
      XMS006 = XMUL /  6.D0
      XMS018 = XMUL / 18.D0
      XMS036 = XMUL / 36.D0
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        F1  =  F(IELEM) * SURFAC(IELEM)
!
!  DIAGONAL TERMS
!
        A11(IELEM) = F1*XMS009
        A22(IELEM) = F1*XMS009
        A33(IELEM) = F1*XMS009
        A44(IELEM) = F1*XMS006
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = F1*XMS036
        A13(IELEM) = F1*XMS036
        A14(IELEM) = F1*XMS018
        A23(IELEM) = F1*XMS036
        A24(IELEM) = F1*XMS018
        A34(IELEM) = F1*XMS018
!
      ENDDO ! IELEM
!
!
!-----------------------------------------------------------------------
!
!  CASE WHERE F IS LINEAR
!
      ELSEIF(IELMF.EQ.11) THEN
!
      XMS054 = XMUL /  54.D0
      XMS018 = XMUL /  18.D0
      XMS540 = XMUL / 540.D0
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
!
!  DIAGONAL TERMS
!
        A11(IELEM) = (SURFAC(IELEM)*(F3+F2+4*F1))*XMS054
        A22(IELEM) = (SURFAC(IELEM)*(F3+4*F2+F1))*XMS054
        A33(IELEM) = (SURFAC(IELEM)*(4*F3+F2+F1))*XMS054
        A44(IELEM) = (SURFAC(IELEM)*(F3+F2+F1))  *XMS018
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = (SURFAC(IELEM)*(   F3+ 7*F2+ 7*F1))*XMS540
        A13(IELEM) = (SURFAC(IELEM)*( 7*F3+   F2+ 7*F1))*XMS540
        A14(IELEM) = (SURFAC(IELEM)*( 7*F3+ 7*F2+16*F1))*XMS540
        A23(IELEM) = (SURFAC(IELEM)*( 7*F3+ 7*F2+   F1))*XMS540
        A24(IELEM) = (SURFAC(IELEM)*( 7*F3+16*F2+ 7*F1))*XMS540
        A34(IELEM) = (SURFAC(IELEM)*(16*F3+ 7*F2+ 7*F1))*XMS540
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.12) THEN
!
!-----------------------------------------------------------------------
!
!   QUASI-BUBBLE DISCRETISATION OF F:
!
!
      XMS090 = XMUL / 90.D0
      XMS180 = XMUL / 180.D0
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        F1  =  F(IKLE1(IELEM))
        F2  =  F(IKLE2(IELEM))
        F3  =  F(IKLE3(IELEM))
        F4  =  F(IKLE4(IELEM))
!
!  DIAGONAL TERMS
!
        A11(IELEM) = (SURFAC(IELEM)*(  F3+2*F4+  F2+6*F1))*XMS090
        A22(IELEM) = (SURFAC(IELEM)*(  F3+2*F4+6*F2+  F1))*XMS090
        A33(IELEM) = (SURFAC(IELEM)*(6*F3+2*F4+  F2+  F1))*XMS090
        A44(IELEM) = (SURFAC(IELEM)*(2*F3+9*F4+2*F2+2*F1))*XMS090
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = (SURFAC(IELEM)*(F4+2*F2+2*F1))*XMS180
        A13(IELEM) = (SURFAC(IELEM)*(2*F3+F4+2*F1))*XMS180
        A14(IELEM) = (SURFAC(IELEM)*(F3+4*F4+F2+4*F1))*XMS180
        A23(IELEM) = (SURFAC(IELEM)*(2*F3+F4+2*F2))*XMS180
        A24(IELEM) = (SURFAC(IELEM)*(F3+4*F4+4*F2+F1))*XMS180
        A34(IELEM) = (SURFAC(IELEM)*(4*F3+4*F4+F2+F1))*XMS180
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!   ANOTHER DISCRETISATION
!      ELSEIF(IELMF.EQ.XX) THEN
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'MT06BB (BIEF) :',/,
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
