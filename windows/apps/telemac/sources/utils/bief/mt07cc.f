!                   *****************
                    SUBROUTINE MT07CC
!                   *****************
!
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &        A22 , A23 , A24 , A25 , A26 ,
     &              A33 , A34 , A35 , A36 ,
     &                    A44 , A45 , A46 ,
     &                          A55 , A56 ,
     &                                A66 ,
     &  XMUL,SF,F,SURFAC,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE FOLLOWING MATRIX
!+                FOR P2 TRIANGLES:
!code
!+            T*M + (1-T) *DM
!+
!+            M  IS THE MASS MATRIX P2*P2
!+            DM IS MASS-LUMPED M
!+            T IS A VECTOR CONSTANT BY ELEMENT
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  A FROEHLY (MATMECA)
!+        01/08/08
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
!| ...            |<--| ELEMENTS OF MATRIX
!| A66            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT07CC => MT07CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A25(*),A26(*),A33(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A34(*),A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A55(*),A56(*),A66(*)
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
      DOUBLE PRECISION T,XSUR30,XSUR45,XSUR180
      INTEGER IELEM
!
!=======================================================================
!
      XSUR30 = XMUL/30.D0
      XSUR45 = XMUL/45.D0
      XSUR180 = XMUL/180.D0
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
!  FOR P2 ELEMENTS, DM(1) = DM(2) = DM(3) = 0
!               AND DM(4) = DM(5) = DM(6) = S/3
        A11(IELEM) = T*SURFAC(IELEM)*XSUR30
        A22(IELEM) = A11(IELEM)
        A33(IELEM) = A11(IELEM)
        A44(IELEM) = (15.D0-7.D0*T)*SURFAC(IELEM)*XSUR45
        A55(IELEM) = A44(IELEM)
        A66(IELEM) = A44(IELEM)
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM) = -SURFAC(IELEM)*T*XSUR180
        A13(IELEM) = A12(IELEM)
        A14(IELEM) = 0.D0
        A15(IELEM) = -SURFAC(IELEM)*T*XSUR45
        A16(IELEM) = 0.D0
        A23(IELEM) = A12(IELEM)
        A24(IELEM) = 0.D0
        A25(IELEM) = 0.D0
        A26(IELEM) = A15(IELEM)
        A34(IELEM) = A15(IELEM)
        A35(IELEM) = 0.D0
        A36(IELEM) = 0.D0
        A45(IELEM) = (15.D0-11.D0*T)*SURFAC(IELEM)*XSUR45
        A46(IELEM) = A45(IELEM)
        A56(IELEM) = A45(IELEM)
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
101     FORMAT(1X,'MT07CC (BIEF) :',/,
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
