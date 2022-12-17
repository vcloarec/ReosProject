!                   *****************
                    SUBROUTINE MT02CC
!                   *****************
!
     &( A11 , A12 , A13 , A14 , A15, A16,
     &        A22 , A23 , A24 , A25, A26,
     &              A33 , A34 , A35, A36,
     &                    A44 , A45, A46,
     &                          A55, A56,
     &                               A66,
     &  XMUL,SU,U,XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE DIFFUSION MATRIX FOR P2 TRIANGLES.
!+
!+            VISCOSITY CAN BE ISOTROPIC, OR NOT ISOTROPIC. IN THIS
!+                CASE U IS AN ARRAY WITH SECOND DIMENSION EQUAL TO 3.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  ALGIANE FROEHLY (MATMECA)
!+        19/06/08
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
!| A14            |<--| ELEMENTS OF MATRIX
!| A15            |<--| ELEMENTS OF MATRIX
!| A16            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A25            |<--| ELEMENTS OF MATRIX
!| A26            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| A35            |<--| ELEMENTS OF MATRIX
!| A36            |<--| ELEMENTS OF MATRIX
!| A44            |<--| ELEMENTS OF MATRIX
!| A45            |<--| ELEMENTS OF MATRIX
!| A46            |<--| ELEMENTS OF MATRIX
!| FORMUL         |-->| FORMULA DESCRIBING THE MATRIX
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SURFAC         |-->| AREA OF TRIANGLES
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| V              |-->| FUNCTION V USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF!, EX_MT02CC => MT02CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A14(*),A15(*),A16(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A24(*),A25(*),A26(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A35(*),A36(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A44(*),A45(*),A46(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A55(*),A56(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A66(*)
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
      DOUBLE PRECISION X2,X3,Y2,Y3,AUX1,AUX2,AUX3,AUX4
      DOUBLE PRECISION NUX1,NUX2,NUX3
      DOUBLE PRECISION NUY1,NUY2,NUY3
      DOUBLE PRECISION NUZ1,NUZ2,NUZ3
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
        AUX1 = XMUL/(60.D0*SURFAC(IELEM))
        AUX2 = 3.D0 * AUX1
        AUX3 = 4.D0 * AUX1
        AUX4 = 8.D0 * AUX1
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)= -(-Y3**2+Y3*Y2-X3**2+X3*X2) *
     &               (2.D0*NUX1+2.D0*NUX2+NUX3) * AUX1
!
        A13(IELEM)=  (-Y3*Y2+Y2**2-X3*X2+X2**2) *
     &               (2.D0*NUX1+NUX2+2.D0*NUX3) * AUX1
!
        A14(IELEM)= ((-11.D0*NUX1-4.D0*NUX3-5.D0*NUX2) * (Y3**2+X3**2)
     &            +  (3.D0*NUX1-NUX3-2.D0*NUX2       ) * (Y2**2+X2**2)
     &            +  (8.D0*NUX1+7.D0*NUX2+5.D0*NUX3  ) * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A15(IELEM)=-((3.D0*NUX1-2.D0*NUX3-NUX2       ) * (Y3**2+X3**2)
     &            +  (3.D0*NUX1-NUX3-2.D0*NUX2       ) * (Y2**2+X2**2)
     &            +  (-6.D0*NUX1+3.D0*NUX2+3.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A16(IELEM)=-((-3.D0*NUX1+2.D0*NUX3+NUX2      ) * (Y3**2+X3**2)
     &            +  (11.D0*NUX1+5.D0*NUX3+4.D0*NUX2 ) * (Y2**2+X2**2)
     &            +  (-8.D0*NUX1-5.D0*NUX2-7.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A23(IELEM)=  (Y3*Y2+X3*X2) * (NUX1+2.D0*NUX3+2.D0*NUX2) * AUX1
!
        A24(IELEM)= ((-5.D0*NUX1-4.D0*NUX3-11.D0*NUX2) * (Y3**2+X3**2)
     &            +  (3.D0*NUX1+14.D0*NUX2+3.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A25(IELEM)=-((NUX1+2.D0*NUX3-3.D0*NUX2       ) * (Y3**2+X3**2)
     &            +  (3.D0*NUX1+3.D0*NUX3+14.D0*NUX2 ) * (Y3*Y2+X3*X2))
     &            *   AUX1
!
        A26(IELEM)= ((NUX1+2.D0*NUX3-3.D0*NUX2       ) * (Y3**2+X3**2)
     &            +  (NUX1-NUX3)                       * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A34(IELEM)= ((NUX1-3.D0*NUX3+2.D0*NUX2      ) * (Y2**2+X2**2)
     &            +  (NUX1-NUX2)                      * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A35(IELEM)=-((NUX1-3.D0*NUX3+2.D0*NUX2      ) * (Y2**2+X2**2)
     &            +  (3.D0*NUX1+3.D0*NUX2+14.D0*NUX3) * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A36(IELEM)=-((5.D0*NUX1+11.D0*NUX3+4.D0*NUX2) * (Y2**2+X2**2)
     &            +  (-3.D0*(NUX1+NUX2)-14.D0*NUX3  ) * (X3*X2+Y3*Y2))
     &            *  AUX1
!
        A45(IELEM)=-((-NUX1+NUX2                    ) * (Y3**2+X3**2)
     &            +  (-NUX1-6.D0*NUX2-3.D0*NUX3     ) * (X3*X2+Y3*Y2)
     &            +  (6.D0*NUX2+2.D0*NUX1+2.D0*NUX3 ) * (Y2**2+X2**2))
     &            *  AUX3
!
        A46(IELEM)=-((NUX1-NUX2                     ) * (Y3**2+X3**2)
     &            +  (NUX1-NUX3                     ) * (Y2**2+X2**2)
     &            +  (4.D0*NUX1+3.D0*NUX2+3.D0*NUX3 ) * (X3*X2+Y3*Y2))
     &            *  AUX3
!
        A56(IELEM)= ((-2.D0*NUX1-2.D0*NUX2-6.D0*NUX3) * (Y3**2+X3**2)
     &            +  (NUX1-NUX3                     ) * (Y2**2+X2**2)
     &            +  (NUX1+3.D0*NUX2+6.D0*NUX3      ) * (X3*X2+Y3*Y2))
     &            *  AUX3
!
!    DIAGONAL TERMS
!
        A11(IELEM)= ((Y3-Y2)**2+(X3-X2)**2)
     &            * (3.D0*NUX1+NUX2+NUX3) * AUX2
!
        A22(IELEM)=  (Y3**2+X3**2) * (NUX1+3.D0*NUX2+NUX3) * AUX2
!
        A33(IELEM)=  (Y2**2+X2**2) * (NUX1+NUX2+3.D0*NUX3) * AUX2
!
        A44(IELEM)=  ((2.D0*NUX1+NUX3+2.D0*NUX2) * (Y3**2+X3**2)
     &            +   (NUX1+NUX3+3.D0*NUX2     ) * (Y2**2+X2**2)
     &            +   (-4.D0*NUX2-NUX3         ) * (X3*X2+Y3*Y2))
     &            *   AUX4
!
        A55(IELEM)=  ((NUX1+NUX2+3.D0*NUX3     ) * (Y3**2+X3**2)
     &            +   (NUX1+NUX3+3.D0*NUX2     ) * (Y2**2+X2**2)
     &            +  (-NUX1-2.D0*NUX2-2.D0*NUX3) * (X3*X2+Y3*Y2))
     &            *   AUX4
!
        A66(IELEM) = ((NUX1+3.D0*NUX3+NUX2      ) * (Y3**2+X3**2)
     &             +  (2.D0*NUX1+2.D0*NUX3+NUX2 ) * (Y2**2+X2**2)
     &             +  (-NUX2-4.D0*NUX3          ) * (X3*X2+Y3*Y2))
     &             *  AUX4
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
        NUY1 = U(IKLE1(IELEM) + IAD2)
        NUY2 = U(IKLE2(IELEM) + IAD2)
        NUY3 = U(IKLE3(IELEM) + IAD2)
        NUZ1 = U(IKLE1(IELEM) + IAD3)
        NUZ2 = U(IKLE2(IELEM) + IAD3)
        NUZ3 = U(IKLE3(IELEM) + IAD3)
!
        AUX1 = XMUL/(60.D0 * SURFAC(IELEM))
        AUX2 = 4.D0 * AUX1
        AUX3 = 3.D0 * AUX1
!
!  EXTRADIAGONAL TERMS
!
      A12(IELEM) =
     &   ((NUY3+2.D0*NUY1+2.D0*NUY2) * (X3**2-X3*X2           ) +
     &    (NUZ3+2.D0*NUZ1+2.D0*NUZ2) * (Y2*X3+Y3*X2-2.D0*Y3*X3) +
     &    (NUX3+2.D0*NUX2+2.D0*NUX1) * (Y3**2-Y3*Y2           ) ) * AUX1
!
      A13(IELEM) =
     &   ((2.D0*NUY1+2.D0*NUY3+NUY2) * (X2**2-X3*X2           ) +
     &    (2.D0*NUZ1+2.D0*NUZ3+NUZ2) * (Y3*X2+Y2*X3-2.D0*Y2*X2) +
     &    (2.D0*NUX1+NUX2+2.D0*NUX3) * (Y2**2-Y3*Y2           ) ) * AUX1
!
      A14(IELEM) =
     &  -((- 3.D0*NUX1+ 2.D0*NUX2+      NUX3) * Y2**2         +
     &    (- 8.D0*NUX1- 7.D0*NUX2- 5.D0*NUX3) * Y3*Y2         +
     &    ( 11.D0*NUX1+ 5.D0*NUX2+ 4.D0*NUX3) * Y3**2         +
     &    (- 3.D0*NUY1+ 2.D0*NUY2+      NUY3) * X2**2         +
     &    (- 8.D0*NUY1- 7.D0*NUY2- 5.D0*NUY3) * X3*X2         +
     &    ( 11.D0*NUY1+ 5.D0*NUY2+ 4.D0*NUY3) * X3**2         +
     &    (  6.D0*NUZ1- 4.D0*NUZ2- 2.D0*NUZ3) * Y2*X2         +
     &    (  8.D0*NUZ1+ 7.D0*NUZ2+ 5.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (-22.D0*NUZ1-10.D0*NUZ2- 8.D0*NUZ3) * Y3*X3         ) * AUX1
!
      A15(IELEM) =
     &  -((  3.D0*NUX1- 2.D0*NUX2-      NUX3) * Y2**2         +
     &    (- 6.D0*NUX1+ 3.D0*NUX2+ 3.D0*NUX3) * Y3*Y2         +
     &    (  3.D0*NUX1-      NUX2- 2.D0*NUX3) * Y3**2         +
     &    (  3.D0*NUY1- 2.D0*NUY2-      NUY3) * X2**2         +
     &    (- 6.D0*NUY1+ 3.D0*NUY2+ 3.D0*NUY3) * X3*X2         +
     &    (  3.D0*NUY1-      NUY2- 2.D0*NUY3) * X3**2         +
     &    (- 6.D0*NUZ1+ 4.D0*NUZ2+ 2.D0*NUZ3) * Y2*X2         +
     &    (  6.D0*NUZ1- 3.D0*NUZ2- 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 6.D0*NUZ1+ 2.D0*NUZ2+ 4.D0*NUZ3) * Y3*X3         ) * AUX1
!
      A16(IELEM) =
     &   ((-11.D0*NUX1- 4.D0*NUX2- 5.D0*NUX3) * Y2**2         +
     &    (  8.D0*NUX1+ 5.D0*NUX2+ 7.D0*NUX3) * Y3*Y2         +
     &    (  3.D0*NUX1-      NUX2- 2.D0*NUX3) * Y3**2         +
     &    (-11.D0*NUY1- 4.D0*NUY2- 5.D0*NUY3) * X2**2         +
     &    (+ 8.D0*NUY1+ 5.D0*NUY2+ 7.D0*NUY3) * X3*X2         +
     &    (  3.D0*NUY1-      NUY2- 2.D0*NUY3) * X3**2         +
     &    ( 22.D0*NUZ1+ 8.D0*NUZ2+10.D0*NUZ3) * Y2*X2         +
     &    (- 8.D0*NUZ1- 5.D0*NUZ2- 7.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 6.D0*NUZ1+ 2.D0*NUZ2+ 4.D0*NUZ3) * Y3*X3         ) * AUX1
!
      A23(IELEM) =
     &   (( NUY1+2.D0*NUY2+2.D0*NUY3) * X3*X2                 +
     &    (-NUZ1-2.D0*NUZ2-2.D0*NUZ3) * (Y3*X2+Y2*X3)         +
     &    ( NUX1+2.D0*NUX2+2.D0*NUX3) * Y3*Y2                 ) * AUX1
!
      A24(IELEM) =
     &  -((- 3.D0*NUX1-14.D0*NUX2- 3.D0*NUX3) * Y3*Y2         +
     &    (  5.D0*NUX1+11.D0*NUX2+ 4.D0*NUX3) * Y3**2         +
     &    (- 3.D0*NUY1-14.D0*NUY2- 3.D0*NUY3) * X3*X2         +
     &    (  5.D0*NUY1+11.D0*NUY2+ 4.D0*NUY3) * X3**2         +
     &    (  3.D0*NUZ1+14.D0*NUZ2+ 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (-10.D0*NUZ1-22.D0*NUZ2- 8.D0*NUZ3) * Y3*X3         ) * AUX1
!
      A25(IELEM) =
     &  -((  3.D0*NUX1+14.D0*NUX2+ 3.D0*NUX3) * Y3*Y2         +
     &    (       NUX1- 3.D0*NUX2+ 2.D0*NUX3) * Y3**2         +
     &    (  3.D0*NUY1+14.D0*NUY2+ 3.D0*NUY3) * X3*X2         +
     &    (       NUY1- 3.D0*NUY2+ 2.D0*NUY3) * X3**2         +
     &    (- 3.D0*NUZ1-14.D0*NUZ2- 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 2.D0*NUZ1+ 6.D0*NUZ2- 4.D0*NUZ3) * Y3*X3         ) * AUX1
!
      A26(IELEM) =
     &   (( NUX1-NUX3) * Y3*Y2 + (-NUY3+NUY1) * X3*X2 +
     &    ( NUZ3-NUZ1) * (Y3*X2+X3*Y2)                +
     &    (2.D0*NUY3+NUY1-3.D0*NUY2) * X3**2          +
     &    (6.D0*NUZ2-4.D0*NUZ3-2.D0*NUZ1) * Y3*X3     +
     &    (2.D0*NUX3-3.D0*NUX2+     NUX1) * Y3**2     ) * AUX1
!
      A34(IELEM) =
     &   ((      NUY1-3.D0*NUY3+2.D0*NUY2)  * X2**2 +
     &    (-2.D0*NUZ1+6.D0*NUZ3-4.D0*NUZ2)  * Y2*X2 +
     &    (      NUX1+2.D0*NUX2-3.D0*NUX3)  * Y2**2 +
     &    (NUY1-NUY2) * X3*X2 + (NUX1-NUX2) * Y3*Y2 +
     &    (NUZ2-NUZ1) * (Y3*X2+Y2*X3)               )*AUX1
!
      A35(IELEM) =
     &  -((       NUX1+ 2.D0*NUX2- 3.D0*NUX3) * Y2**2         +
     &    (  3.D0*NUX1+ 3.D0*NUX2+14.D0*NUX3) * Y3*Y2         +
     &    (       NUY1+ 2.D0*NUY2- 3.D0*NUY3) * X2**2         +
     &    (  3.D0*NUY1+ 3.D0*NUY2+14.D0*NUY3) * X3*X2         +
     &    (- 3.D0*NUZ1- 3.D0*NUZ2-14.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (- 2.D0*NUZ1- 4.D0*NUZ2+ 6.D0*NUZ3) * Y2*X2         ) * AUX1
!
      A36(IELEM) =
     &   ((- 5.D0*NUX1- 4.D0*NUX2-11.D0*NUX3) * Y2**2         +
     &    (  3.D0*NUX1+3.D0*NUX2+ 14.D0*NUX3) * Y3*Y2         +
     &    (- 5.D0*NUY1- 4.D0*NUY2-11.D0*NUY3) * X2**2         +
     &    (  3.D0*NUY1+ 3.D0*NUY2+14.D0*NUY3) * X3*X2         +
     &    ( 10.D0*NUZ1+ 8.D0*NUZ2+22.D0*NUZ3) * Y2*X2         +
     &    (- 3.D0*NUZ1- 3.D0*NUZ2-14.D0*NUZ3) * (Y3*X2+Y2*X3) ) * AUX1
!
      A45(IELEM) =
     &   ((- 2.D0*NUY1- 6.D0*NUY2- 2.D0*NUY3) * X2**2         +
     &    (       NUY1+ 6.D0*NUY2+ 3.D0*NUY3) * X3*X2         +
     &    (  4.D0*NUZ1+12.D0*NUZ2+ 4.D0*NUZ3) * Y2*X2         +
     &    (-      NUZ1- 6.D0*NUZ2- 3.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (NUY1-NUY2)*X3**2 + (NUX1-NUX2)*Y3**2               +
     &    (- 2.D0*NUZ1+ 2.D0*NUZ2           ) * Y3*X3         +
     &    (- 2.D0*NUX1- 2.D0*NUX3- 6.D0*NUX2) * Y2**2         +
     &    (  6.D0*NUX2+ 3.D0*NUX3+      NUX1) * Y3*Y2         ) * AUX2
!
      A46(IELEM) =
     &  -((-NUY3+NUY1)*X2**2 + (NUX1-NUX2) * Y3**2            +
     &    ( NUX1-NUX3)*Y2**2 + (NUY1-NUY2) * X3**2            +
     &    (  4.D0*NUY1+ 3.D0*NUY3+ 3.D0*NUY2) * X3*X2         +
     &    (  2.D0*NUZ3- 2.D0*NUZ1           ) * Y2*X2         +
     &    ( -3.D0*NUZ3- 4.D0*NUZ1- 3.D0*NUZ2) * (Y3*X2+Y2*X3) +
     &    (- 2.D0*NUZ1+ 2.D0*NUZ2           ) * Y3*X3         +
     &    (  3.D0*NUX2+ 4.D0*NUX1+ 3.D0*NUX3) * Y3*Y2         ) * AUX2
!
      A56(IELEM) =
     &  -(( NUY3-NUY1)*X2**2 + (NUX3-NUX1) * Y2**2            +
     &    (- 3.D0*NUY2- 6.D0*NUY3-      NUY1) * X3*X2         +
     &    (  2.D0*NUZ1- 2.D0*NUZ3           ) * Y2*X2         +
     &    (  3.D0*NUZ2+      NUZ1+ 6.D0*NUZ3) * (Y3*X2+Y2*X3) +
     &    (  6.D0*NUY3+ 2.D0*NUY1+ 2.D0*NUY2) * X3**2         +
     &    (-12.D0*NUZ3- 4.D0*NUZ2- 4.D0*NUZ1) * Y3*X3         +
     &    (- 6.D0*NUX3- 3.D0*NUX2-      NUX1) * Y3*Y2         +
     &    (  2.D0*NUX2+ 2.D0*NUX1+ 6.D0*NUX3) * Y3**2         ) * AUX2
!
!   THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE
!   MAGIC SQUARE OR BY DIRECT COMPUTATION:
!
      A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
     &             - A15(IELEM) - A16(IELEM)
      A22(IELEM) = ((NUY1+NUY3+3.D0*NUY2) * X3**2            +
     &              (-6.D0*NUZ2-2.D0*NUZ1-2.D0*NUZ3) * Y3*X3 +
     &              (NUX3+NUX1+3.D0*NUX2) * Y3**2            ) * AUX3
      A33(IELEM) = ((NUY1+3.D0*NUY3+NUY2) * X2**2            +
     &              (-6.D0*NUZ3-2.D0*NUZ1-2.D0*NUZ2) * Y2*X2 +
     &              ( 3.D0*NUX3+NUX1+NUX2          ) * Y2**2 ) * AUX3
      A44(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
     &             - A45(IELEM) - A46(IELEM)
      A55(IELEM) = - A15(IELEM) - A25(IELEM) - A35(IELEM)
     &             - A45(IELEM) - A56(IELEM)
      A66(IELEM) = - A16(IELEM) - A26(IELEM) - A36(IELEM)
     &             - A46(IELEM) - A56(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,11) IELMNU,ISO
11    FORMAT(1X,'MT02CC (BIEF) :TYPE OF VISCOSITY NOT AVAILABLE: ',2I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
