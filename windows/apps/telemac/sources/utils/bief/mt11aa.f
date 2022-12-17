!                   *****************
                    SUBROUTINE MT11AA
!                   *****************
!
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   XMUL,SF,F,XEL,YEL,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD=1
!+
!+                  /           D
!+  A(I,J)=- XMUL  /  PSI2(J) * -- ( PSI1(I) F ) ) D(OMEGA)
!+                /OMEGA        DX
!+
!+
!+  BEWARE THE MINUS SIGN !!
!+
!+  PSI1: LINEAR
!+  PSI2: LINEAR
!+
!+  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION USED IN THE FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT11AA => MT11AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
!
      DOUBLE PRECISION SUR24,X2,X3,Y2,Y3,F1,F2,F3
!
!-----------------------------------------------------------------------
!
      SUR24 = XMUL/24.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!  SAME RESULT WHETHER F IS LINEAR OR QUASI-BUBBLE
!
      IF(IELMF.EQ.11.OR.IELMF.EQ.12) THEN
!
!================================
!  CASE OF DERIVATIVE WRT X =
!================================
!
      IF(ICOORD.EQ.1) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM)) * SUR24
      F2  =  F(IKLE2(IELEM)) * SUR24
      F3  =  F(IKLE3(IELEM)) * SUR24
!
!   DIAGONAL TERMS
!
      A11(IELEM) =     Y2  * (F3-F2-4*F1)  +     Y3  * ( F3-F2+4*F1)
      A22(IELEM) = (Y2+Y2) * (F3-F1)       +     Y3  * (-F3-4*F2+F1)
      A33(IELEM) =     Y2  * (4*F3+F2-F1)  + (Y3+Y3) * (-F2+F1)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)  =-(Y2+Y2) * (F2+F1)        +     Y3  * (F3+F2+F1+F1)
      A13(IELEM)  =      Y2 * (-F3-F2-F1-F1) + (Y3+Y3) * (F3+F1)
      A23(IELEM)  =      Y2 * (F3-F1)        - (Y3+Y3) * (F3+F2)
      A21(IELEM)  =      Y2 * (F3-F1)        +      Y3 * (-F3-F2-F2-F1)
      A31(IELEM)  =      Y2 * (F3+F3+F2+F1)  +      Y3 * (-F2+F1)
      A32(IELEM)  = (Y2+Y2) * (F3+F2)        +      Y3 * (-F2+F1)
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  CASE OF DERIVATIVE WRT Y =
!================================
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2  =  XEL(IELEM,2)
      X3  =  XEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM)) * SUR24
      F2  =  F(IKLE2(IELEM)) * SUR24
      F3  =  F(IKLE3(IELEM)) * SUR24
!
!   DIAGONAL TERMS
!
      A11(IELEM) =     X2  * (-F3+F2+4*F1) +   X3  * (-F3+F2-4*F1)
      A22(IELEM) = (X2+X2) * (-F3+F1)      +   X3  * (F3+4*F2-F1)
      A33(IELEM) =     X2  * (-4*F3-F2+F1) + (X3+X3) * (F2-F1)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)  = (X2+X2) * (F2+F1)        +     X3  * (-F3-F2-F1-F1)
      A13(IELEM)  =      X2 * (F3+F2+F1+F1)  - (X3+X3) * (F3+F1)
      A23(IELEM)  =      X2 * (-F3+F1)       + (X3+X3) * (F3+F2)
      A21(IELEM)  =      X2 * (-F3+F1)       +      X3 * (F3+F2+F2+F1)
      A31(IELEM)  =      X2 * (-F3-F3-F2-F1) +      X3 * (F2-F1)
      A32(IELEM)  =-(X2+X2) * (F3+F2)        +      X3 * (F2-F1)
!
      ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
201       FORMAT(1X,'MT11AA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
        ENDIF
!
!     ELSEIF(IELMF.EQ. ) THEN
!     OTHER TYPES OF FUNCTIONS F
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,101) IELMF
101     FORMAT(1X,'MT11AA (BIEF) :',/,
     &         1X,'DISCRETISATION OF F: ',1I6,' NOT AVAILABLE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
