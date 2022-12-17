!                   *****************
                    SUBROUTINE MT02BB
!                   *****************
!
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SU,U,XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE DIFFUSION MATRIX FOR QUASI-BUBBLE TRIANGLES.
!+
!+            VISCOSITY CAN BE ISOTROPIC, OR NOT ISOTROPIC. IN THIS
!+                CASE U IS AN ARRAY WITH SECOND DIMENSION EQUAL TO 3.
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
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SURFAC         |-->| AREA OF TRIANGLES
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT02BB => MT02BB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) ::                      A44(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,U(*)
!     STRUCTURE OF U
      TYPE(BIEF_OBJ), INTENT(IN)      :: SU
!
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMNU,IELEM,ISO,IAD2,IAD3
!
      DOUBLE PRECISION X2,X3,Y2,Y3,AUX1,AUX2
      DOUBLE PRECISION NUX1,NUX2,NUX3,NUY1,NUY2,NUY3,NUZ1,NUZ2,NUZ3
!
!=======================================================================
!
!     EXTRACTS THE TYPE OF ELEMENT FOR VISCOSITY
!
      IELMNU = SU%ELM
      ISO = SU%DIM2
!
!     IF(IELMNU.EQ.10.AND.ISO.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!  P0 DISCRETISATION FOR VISCOSITY:
!
!-----------------------------------------------------------------------
!
      IF(IELMNU.EQ.11.AND.ISO.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!  P1 DISCRETISATION FOR ISOTROPIC VISCOSITY:
!
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        NUX1 = U(IKLE1(IELEM))
        NUX2 = U(IKLE2(IELEM))
        NUX3 = U(IKLE3(IELEM))
!
        AUX1 = XMUL/(108*SURFAC(IELEM))
        AUX2 = 3 * AUX1
!
!  EXTRADIAGONAL TERMS
!  SOME FACTORISATIONS REMAIN TO BE DONE
!
        A12(IELEM)=((2*X2**2+X2*X3-X3**2-Y3**2+Y3*Y2+2*Y2**2)*(
     &   NUX3+4*NUX2+4*NUX1))*AUX1
!
        A13(IELEM)=(-(X2**2-X2*X3-2*X3**2-2*Y3**2-Y3*Y2+Y2**2)*(
     &   4*NUX3+NUX2+4*NUX1))*AUX1
!
        A14(IELEM)=((4*NUX3+NUX2+4*NUX1)*X2*X3-2*(4*NUX3+NUX2+
     &   4*NUX1)*X3**2-2*(4*NUX3+NUX2+4*NUX1)*Y3**2+(4*NUX3+
     &   NUX2+4*NUX1)*Y3*Y2-2*(NUX3+4*NUX2+4*NUX1)*X2**2+(NUX3
     &   +4*NUX2+4*NUX1)*X2*X3+(NUX3+4*NUX2+4*NUX1)*Y3*Y2-2*(
     &   NUX3+4*NUX2+4*NUX1)*Y2**2)*AUX2
!
        A23(IELEM)=((2*X2**2-5*X2*X3+2*X3**2+2*Y3**2-5*Y3*Y2+
     &   2*Y2**2)*(4*NUX3+4*NUX2+NUX1))*AUX1
!
        A24(IELEM)=(-((4*NUX3+4*NUX2+NUX1)*X2**2-3*(4*NUX3+4*
     &   NUX2+NUX1)*X2*X3+2*(4*NUX3+4*NUX2+NUX1)*X3**2+2*(4*
     &   NUX3+4*NUX2+NUX1)*Y3**2-3*(4*NUX3+4*NUX2+NUX1)*Y3*Y2+
     &   (4*NUX3+4*NUX2+NUX1)*Y2**2+(NUX3+4*NUX2+4*NUX1)*X2**2
     &   +(NUX3+4*NUX2+4*NUX1)*X2*X3+(NUX3+4*NUX2+4*NUX1)*Y3*
     &    Y2+(NUX3+4*NUX2+4*NUX1)*Y2**2))*AUX2
!
        A34(IELEM)=(
     &     NUX1*(-5*Y3**2-Y3*Y2-2*Y2**2)+NUX2*(-5*Y3**
     &    2+11*Y3*Y2-8*Y2**2)+8*NUX3*(-Y3**2+Y3*Y2-Y2**2)+NUX1*(
     &    -2*X2**2-X2*X3-5*X3**2)+NUX2*(-8*X2**2+11*X2*X3-5*X3
     &    **2)+8*NUX3*(-X2**2+X2*X3-X3**2))*AUX2
!
!   THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE
!   MAGIC SQUARE:
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
        A22(IELEM) = - A12(IELEM) - A23(IELEM) - A24(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A34(IELEM)
        A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMNU.EQ.11.AND.ISO.EQ.3) THEN
!
!-----------------------------------------------------------------------
!
!  P1 DISCRETISATION FOR NONISOTROPIC VISCOSITY:
!
      IAD2 = SU%MAXDIM1
      IAD3 = 2*IAD2
      DO IELEM = 1 , NELEM
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        NUX1 = U(IKLE1(IELEM))
        NUX2 = U(IKLE2(IELEM))
        NUX3 = U(IKLE3(IELEM))
        NUY1 = U(IKLE1(IELEM)+IAD2)
        NUY2 = U(IKLE2(IELEM)+IAD2)
        NUY3 = U(IKLE3(IELEM)+IAD2)
        NUZ1 = U(IKLE1(IELEM)+IAD3)
        NUZ2 = U(IKLE2(IELEM)+IAD3)
        NUZ3 = U(IKLE3(IELEM)+IAD3)
!
        AUX1 = XMUL/(108*SURFAC(IELEM))
        AUX2 = 3 * AUX1
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)=(-4*NUX1*(Y3+Y2)*(Y3-2*Y2)-4*NUX2*(Y3+Y2)*(
     &   Y3-2*Y2)-NUX3*(Y3+Y2)*(Y3-2*Y2)+((NUY3+4*NUY2+4*NUY1)
     &   *X3-(Y3+4*Y2)*NUZ3-4*(Y3+4*Y2)*NUZ2-4*(Y3+4*Y2)*NUZ1
     &   )*X2+(NUZ3+4*NUZ2+4*NUZ1)*(2*Y3-Y2)*X3+2*(NUY3+4*
     &   NUY2+4*NUY1)*X2**2-(NUY3+4*NUY2+4*NUY1)*X3**2)*AUX1
!
        A13(IELEM)=(4*NUX1*(2*Y3-Y2)*(Y3+Y2)+NUX2*(2*Y3-Y2)*(Y3
     &   +Y2)+4*NUX3*(2*Y3-Y2)*(Y3+Y2)+((4*NUY3+NUY2+4*NUY1)*
     &   X3-4*(Y3-2*Y2)*NUZ3-(Y3-2*Y2)*NUZ2-4*(Y3-2*Y2)*NUZ1)
     &   *X2-(4*NUZ3+NUZ2+4*NUZ1)*(4*Y3+Y2)*X3-(4*NUY3+NUY2+4
     &   *NUY1)*X2**2+2*(4*NUY3+NUY2+4*NUY1)*X3**2)*AUX1
!
        A14(IELEM)=(4*NUX1*(-(2*Y3-Y2)*Y3+(Y3-2*Y2)*Y2)+NUX2*(-
     &   (2*Y3-Y2)*Y3+4*(Y3-2*Y2)*Y2)+NUX3*(-4*(2*Y3-Y2)*Y3+(
     &   Y3-2*Y2)*Y2)+((4*NUY3+NUY2+4*NUY1)*X3-4*NUZ3*Y3-NUZ2*
     &   Y3-4*NUZ1*Y3)*X2+((NUY3+4*NUY2+4*NUY1)*X3-(Y3-4*Y2)*
     &   NUZ3-4*(Y3-4*Y2)*NUZ2-4*(Y3-4*Y2)*NUZ1)*X2+(4*NUZ3+
     &   NUZ2+4*NUZ1)*(4*Y3-Y2)*X3-(NUZ3+4*NUZ2+4*NUZ1)*X3*Y2-
     &  2*(4*NUY3+NUY2+4*NUY1)*X3**2-2*(NUY3+4*NUY2+4*NUY1)*X2**2)*AUX2
!
        A23(IELEM)=(-((5*(4*NUY3+4*NUY2+NUY1)*X3-4*(5*Y3-4*
     &   Y2)*NUZ3-4*(5*Y3-4*Y2)*NUZ2-(5*Y3-4*Y2)*NUZ1)*X2+(4
     &   *NUZ3+4*NUZ2+NUZ1)*(4*Y3-5*Y2)*X3-2*(4*NUY3+4*NUY2+
     &   NUY1)*X2**2-2*(4*NUY3+4*NUY2+NUY1)*X3**2-4*(2*Y3-Y2)
     &   *(Y3-2*Y2)*NUX3-4*(2*Y3-Y2)*(Y3-2*Y2)*NUX2-(2*Y3-Y2)
     &   *(Y3-2*Y2)*NUX1))*AUX1
!
        A24(IELEM)=(-(5*X2**2*NUY3+8*X2**2*NUY2+5*X2**2*NUY1-
     &   11*X2*X3*NUY3-8*X2*X3*NUY2+X2*X3*NUY1+11*X2*NUZ3*Y3-10
     &   *X2*NUZ3*Y2+8*X2*NUZ2*Y3-16*X2*NUZ2*Y2-X2*NUZ1*Y3-10*
     &   X2*NUZ1*Y2+8*X3**2*NUY3+8*X3**2*NUY2+2*X3**2*NUY1-16*
     &   X3*NUZ3*Y3+11*X3*NUZ3*Y2-16*X3*NUZ2*Y3+8*X3*NUZ2*Y2-4
     &   *X3*NUZ1*Y3-X3*NUZ1*Y2+8*NUX3*Y3**2-11*NUX3*Y3*Y2+5*
     &   NUX3*Y2**2+8*NUX2*Y3**2-8*NUX2*Y3*Y2+8*NUX2*Y2**2+2*
     &   NUX1*Y3**2+NUX1*Y3*Y2+5*NUX1*Y2**2))*AUX2
!
        A34(IELEM)=(-(8*X2**2*NUY3+8*X2**2*NUY2+2*X2**2*NUY1-8
     &   *X2*X3*NUY3-11*X2*X3*NUY2+X2*X3*NUY1+8*X2*NUZ3*Y3-16*
     &   X2*NUZ3*Y2+11*X2*NUZ2*Y3-16*X2*NUZ2*Y2-X2*NUZ1*Y3-4*X2
     &   *NUZ1*Y2+8*X3**2*NUY3+5*X3**2*NUY2+5*X3**2*NUY1-16*X3
     &   *NUZ3*Y3+8*X3*NUZ3*Y2-10*X3*NUZ2*Y3+11*X3*NUZ2*Y2-10*
     &   X3*NUZ1*Y3-X3*NUZ1*Y2+8*NUX3*Y3**2-8*NUX3*Y3*Y2+8*NUX3
     &   *Y2**2+5*NUX2*Y3**2-11*NUX2*Y3*Y2+8*NUX2*Y2**2+5*NUX1
     &   *Y3**2+NUX1*Y3*Y2+2*NUX1*Y2**2))*AUX2
!
!   THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE
!   MAGIC SQUARE:
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
        A22(IELEM) = - A12(IELEM) - A23(IELEM) - A24(IELEM)
        A33(IELEM) = - A13(IELEM) - A23(IELEM) - A34(IELEM)
        A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,11) IELMNU,ISO
11      FORMAT(1X,
     &  'MT02BB (BIEF) : TYPE OF VISCOSITY NOT AVAILABLE : ',2I6)
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
