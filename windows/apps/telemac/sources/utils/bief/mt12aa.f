!                   *****************
                    SUBROUTINE MT12AA
!                   *****************
!
     &(  A11 , A12 , A13 ,
     &   A21 , A22 , A23 ,
     &   A31 , A32 , A33 ,
     &   XMUL,SF,SU,SV,F,U,V,
     &   XEL,YEL,SURDET,
     &   IKLE1,IKLE2,IKLE3,
     &   NELEM,NELMAX,ICOORD)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+  EXAMPLE WITH ICOORD=1
!+
!+                  /                -->  --->
!+  A(I,J) = XMUL  /   PSIJ GRAD(F)   U .GRAD(PSII) D(OMEGA)
!+                /OMEGA
!+                                 -->
!+                      F VECTOR    U  VECTOR WITH COMPONENTS U,V,W
!+
!+
!+  BEWARE THE MINUS SIGN !!
!+
!+  PSI1: LINEAR
!+  PSI2: LINEAR
!+
!+  IT WOULD BE A DERIVATIVE WRT Y WITH ICOORD=2
!+  IT WOULD BE A DERIVATIVE WRT Z WITH ICOORD=3
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
!history  J-M HERVOUET (LNHE)
!+        15/06/2012
!+        V6P2
!+   SURFAC changed into SURDET and formulas changed accordingly
!+   This is just optimisation.
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
!| IKLE4          |-->| QUASI-BUBBLE POINT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SURDET         |-->| HERE = 1/(2*SURFAC)
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| V              |-->| FUNCTION V USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT12AA => MT12AA
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
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
!     STRUCTURES OF F, U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURDET(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION X2,X3,Y2,Y3,F1,F2,F3,DEN
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,U123,V123,XSUR06,XSUR24
!
!-----------------------------------------------------------------------
!
      XSUR06 = XMUL/06.D0
      XSUR24 = XMUL/24.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!  CASE WHERE F IS OF TYPE P1 AND U P0
!
      IF(IELMF.EQ.11.AND.IELMU.EQ.10.AND.IELMV.EQ.10) THEN
!
!================================
!  DERIVATIVE WRT X  =
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
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
!
      DEN = (F3*Y2 - F2*Y3) * XSUR06 * SURDET(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A23(IELEM) = (  X3 *V(IELEM) -  Y3    *U(IELEM) )*DEN
      A31(IELEM) =-(  X2 *V(IELEM) -     Y2 *U(IELEM) )*DEN
!
      A12(IELEM) = - A23(IELEM) - A31(IELEM)
      A13(IELEM) =   A12(IELEM)
      A21(IELEM) =   A23(IELEM)
      A32(IELEM) =   A31(IELEM)
!
! DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
!
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2  =  XEL(IELEM,2)
      X3  =  XEL(IELEM,3)
      Y2  =  YEL(IELEM,2)
      Y3  =  YEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
!
      DEN = (F3*X2 - F2*X3) * XSUR06 * SURDET(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A23(IELEM) = -( X3*V(IELEM) - Y3*U(IELEM) ) * DEN
      A31(IELEM) =  ( X2*V(IELEM) - Y2*U(IELEM) ) * DEN
!
      A12(IELEM) = - A23(IELEM) - A31(IELEM)
      A13(IELEM) =   A12(IELEM)
      A21(IELEM) =   A23(IELEM)
      A32(IELEM) =   A31(IELEM)
!
! DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
!
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
201       FORMAT(1X,'MT12AA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.11.AND.IELMU.EQ.11) THEN
!
!================================
!  DERIVATIVE WRT X  =
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
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
!
      U1  =  U(IKLE1(IELEM))
      U2  =  U(IKLE2(IELEM))
      U3  =  U(IKLE3(IELEM))
!
      V1  =  V(IKLE1(IELEM))
      V2  =  V(IKLE2(IELEM))
      V3  =  V(IKLE3(IELEM))
!
      U123 = U1 + U2 + U3
      V123 = V1 + V2 + V3
!
      DEN = (F3*Y2 - F2*Y3) * XSUR24 * SURDET(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM) = ( (X2-X3)*(V123+V2) + (Y3-Y2)*(U123+U2) )*DEN
!
      A13(IELEM) = ( (X2-X3)*(V123+V3) + (Y3-Y2)*(U123+U3) )*DEN
!
      A23(IELEM) = ( X3*(V123+V3) - Y3*(U123+U3) )*DEN
!
      A21(IELEM) = ( X3*(V123+V1) - Y3*(U123+U1) )*DEN
!
      A31(IELEM) =-( X2*(V123+V1) - Y2*(U123+U1) )*DEN
!
      A32(IELEM) =-( X2*(V123+V2) - Y2*(U123+U2) )*DEN
!
! DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
!
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2  =  XEL(IELEM,2)
      X3  =  XEL(IELEM,3)
      Y2  =  YEL(IELEM,2)
      Y3  =  YEL(IELEM,3)
!
      F1  =  F(IKLE1(IELEM))
      F2  =  F(IKLE2(IELEM)) - F1
      F3  =  F(IKLE3(IELEM)) - F1
!
      U1  =  U(IKLE1(IELEM))
      U2  =  U(IKLE2(IELEM))
      U3  =  U(IKLE3(IELEM))
!
      V1  =  V(IKLE1(IELEM))
      V2  =  V(IKLE2(IELEM))
      V3  =  V(IKLE3(IELEM))
!
      U123 = U1 + U2 + U3
      V123 = V1 + V2 + V3
!
      DEN = (F3*X2 - F2*X3) * XSUR24 * SURDET(IELEM)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM) =-( (X2-X3)*(V123+V2) + (Y3-Y2)*(U123+U2) )*DEN
!
      A13(IELEM) =-( (X2-X3)*(V123+V3) + (Y3-Y2)*(U123+U3) )*DEN
!
      A23(IELEM) =-( X3*(V123+V3) - Y3*(U123+U3) )*DEN
!
      A21(IELEM) =-( X3*(V123+V1) - Y3*(U123+U1) )*DEN
!
      A31(IELEM) = ( X2*(V123+V1) - Y2*(U123+U1) )*DEN
!
      A32(IELEM) = ( X2*(V123+V2) - Y2*(U123+U2) )*DEN
!
! DIAGONAL TERMS (THE SUM OF EACH COLUMN IS 0)
!
      A11(IELEM) = - A21(IELEM) - A31(IELEM)
      A22(IELEM) = - A12(IELEM) - A32(IELEM)
      A33(IELEM) = - A13(IELEM) - A23(IELEM)
!
      ENDDO ! IELEM
!
        ELSE
          WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
        ENDIF
!
!     OTHER TYPES OF F FUNCTIONS
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,101) IELMF,IELMU
101     FORMAT(1X,'MT12AA (BIEF) :',/,
     &        1X,'COMBINATION OF F AND U: ',1I6,2X,1I6,' NOT AVAILABLE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
