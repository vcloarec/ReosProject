!                   *****************
                    SUBROUTINE MT07BB
!                   *****************
!
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SF,F,SURFAC,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE FOLLOWING MATRIX
!+                FOR QUASI-BUBBLE TRIANGLES:
!code
!+            T*M + (1-T) *DM
!+
!+            M  IS THE MASS MATRIX QB*QB
!+            DM IS MASS-LUMPED M
!+            T IS A VECTOR CONSTANT BY ELEMENT
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
!| F              |-->| FUNCTION USED IN THE FORMULA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT07BB => MT07BB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
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
      DOUBLE PRECISION T,XSUR06,XSUR09,XSUR36
      INTEGER IELEM
!
!=======================================================================
!
      XSUR06 = XMUL/6.D0
      XSUR09 = XMUL/9.D0
      XSUR36 = XMUL/36.D0
!
!-----------------------------------------------------------------------
!
      IF(SF%ELM.EQ.10) THEN
!
!-----------------------------------------------------------------------
!
!   P0 DISCRETISATION OF F:
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
        T = F(IELEM)
!
!  DIAGONAL TERMS
!
        A11(IELEM) = -(T-2)*SURFAC(IELEM)*XSUR09
        A22(IELEM) = A11(IELEM)
        A33(IELEM) = A11(IELEM)
        A44(IELEM) = -(T-2)*SURFAC(IELEM)*XSUR06
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = SURFAC(IELEM)*T*XSUR36
        A13(IELEM) = A12(IELEM)
        A14(IELEM) = 2*A12(IELEM)
        A23(IELEM) = A12(IELEM)
        A24(IELEM) = A14(IELEM)
        A34(IELEM) = A14(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!  ANOTHER DISCRETISATION OF F
!      ELSEIF(SF%ELM.EQ.XX) THEN
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) SF%ELM,SF%NAME
101     FORMAT(1X,'MT07BB (BIEF) :',/,
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
