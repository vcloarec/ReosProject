!                   *****************
                    SUBROUTINE MT05BB
!                   *****************
!
     &( A11 , A12 , A13 , A14 ,
     &  A21 , A22 , A23 , A24 ,
     &  A31 , A32 , A33 , A34 ,
     &  A41 , A42 , A43 , A44 ,
     &  XMUL,SU,SV,U,V,
     &  XEL,YEL,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,FORMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE FOLLOWING MATRIX FOR QUASI-BUBBLE TRIANGLES:
!code
!+    ->  --->
!+    U . GRAD
!
!history  J-M HERVOUET (LNH)    ; C   MOULIN (LNH)
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
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| A41            |<--| ELEMENTS OF MATRIX
!| A42            |<--| ELEMENTS OF MATRIX
!| A43            |<--| ELEMENTS OF MATRIX
!| A44            |<--| ELEMENTS OF MATRIX
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| QUASI-BUBBLE POINT
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
      USE BIEF, EX_MT05BB => MT05BB
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
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A41(*),A42(*),A43(*),A44(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!     STRUCTURES OF      U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
      CHARACTER(LEN=16) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSUR6,TIERS
      DOUBLE PRECISION K1T1,K2T1,K3T1,US2T1,VS2T1
      DOUBLE PRECISION L12T1,L13T1,L21T1,L23T1,L31T1,L32T1
      DOUBLE PRECISION K1T2,K2T2,K3T2,US2T2,VS2T2
      DOUBLE PRECISION L12T2,L13T2,L21T2,L23T2,L31T2,L32T2
      DOUBLE PRECISION K1T3,K2T3,K3T3,US2T3,VS2T3
      DOUBLE PRECISION L12T3,L13T3,L21T3,L23T3,L31T3,L32T3
!
!-----------------------------------------------------------------------
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,IELMU,IELMV
!
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION XSU216,XSUR72,XSUR24
      DOUBLE PRECISION X1T1,X2T1,X3T1,Y1T1,Y2T1,Y3T1
      DOUBLE PRECISION X1T2,X2T2,X3T2,Y1T2,Y2T2,Y3T2
      DOUBLE PRECISION X1T3,X2T3,X3T3,Y1T3,Y2T3,Y3T3
!
!=======================================================================
!
!     EXTRACTS THE TYPE OF ELEMENT FOR VELOCITY
!
      IELMU = SU%ELM
      IELMV = SV%ELM
!
!-----------------------------------------------------------------------
!
      TIERS  = 1.D0 /   6.D0
      XSUR6  = XMUL /   6.D0
      XSU216 = XMUL / 216.D0
      XSUR72 = XMUL /  72.D0
      XSUR24 = XMUL /  24.D0
!
!-----------------------------------------------------------------------
!
!     CASE WHERE U AND V ARE CONSTANT BY ELEMENT
!
      IF(IELMU.EQ.10.AND.IELMV.EQ.10) THEN
!
!-----------------------------------------------------------------------
!
!  P1 DISCRETISATION OF THE VELOCITY:
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
        U1 = U(IELEM)
        U2 = U(IELEM)
        U3 = U(IELEM)
        V1 = V(IELEM)
        V2 = V(IELEM)
        V3 = V(IELEM)
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)= (X2*(-V3-4*V2-7*V1)+X3*(-V3-4*V2-7*V1)+Y2*(
     &              U3+4*U2+7*U1)+Y3*(U3+4*U2+7*U1))*XSU216
!
        A13(IELEM)= (X2*(4*V3+V2+7*V1)+X3*(4*V3+V2+7*V1)+Y2*(-
     &              4*U3-U2-7*U1)+Y3*(-4*U3-U2-7*U1))*XSU216
!
        A14(IELEM)= (X2*(V3+4*V2+7*V1)+X3*(-4*V3-V2-7*V1)+Y2*(-
     &              U3-4*U2-7*U1)+Y3*(4*U3+U2+7*U1))*XSUR72
!
        A21(IELEM)= (2*X2*(-V3-7*V2-4*V1)+X3*(V3+7*V2+4*V1)+2
     &              *Y2*(U3+7*U2+4*U1)+Y3*(-U3-7*U2-4*U1))*XSU216
!
        A23(IELEM)= (2*X2*(4*V3+7*V2+V1)+X3*(-4*V3-7*V2-V1)+2
     &              *Y2*(-4*U3-7*U2-U1)+Y3*(4*U3+7*U2+U1))*XSU216
!
        A24(IELEM)= (3*X2*(-V3+V1)+X3*(4*V3+7*V2+V1)+3*Y2*(U3-
     &              U1)+Y3*(-4*U3-7*U2-U1))*XSUR72
!
        A31(IELEM)= (X2*(-7*V3-V2-4*V1)+2*X3*(7*V3+V2+4*V1)+Y2
     &              *(7*U3+U2+4*U1)+2*Y3*(-7*U3-U2-4*U1))*XSU216
!
        A32(IELEM)= (X2*(7*V3+4*V2+V1)+2*X3*(-7*V3-4*V2-V1)+Y2
     &              *(-7*U3-4*U2-U1)+2*Y3*(7*U3+4*U2+U1))*XSU216
!
        A34(IELEM)= (X2*(-7*V3-4*V2-V1)+3*X3*(V2-V1)+Y2*(7*U3+
     &              4*U2+U1)+3*Y3*(-U2+U1))*XSUR72
!
        A41(IELEM)= (X2*(-3*V3-4*V2-5*V1)+X3*(4*V3+3*V2+5*V1)
     &              +Y2*(3*U3+4*U2+5*U1)
     &              +Y3*(-4*U3-3*U2-5*U1))*XSUR72
!
        A42(IELEM)= (X2*(V3-V1)+X3*(-4*V3-5*V2-3*V1)+Y2*(-U3+U1)
     &              +Y3*(4*U3+5*U2+3*U1))*XSUR72
!
        A43(IELEM)= (X2*(5*V3+4*V2+3*V1)+X3*(-V2+V1)+Y2*(-5*U3-
     &              4*U2-3*U1)+Y3*(U2-U1))*XSUR72
!
!  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
        A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!     CASE WHERE U AND V ARE LINEAR
!
      ELSEIF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
!-----------------------------------------------------------------------
!
!  P1 DISCRETISATION OF THE VELOCITY:
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
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)= (X2*(-V3-4*V2-7*V1)+X3*(-V3-4*V2-7*V1)+Y2*(
     &              U3+4*U2+7*U1)+Y3*(U3+4*U2+7*U1))*XSU216
!
        A13(IELEM)= (X2*(4*V3+V2+7*V1)+X3*(4*V3+V2+7*V1)+Y2*(-
     &              4*U3-U2-7*U1)+Y3*(-4*U3-U2-7*U1))*XSU216
!
        A14(IELEM)= (X2*(V3+4*V2+7*V1)+X3*(-4*V3-V2-7*V1)+Y2*(-
     &              U3-4*U2-7*U1)+Y3*(4*U3+U2+7*U1))*XSUR72
!
        A21(IELEM)= (2*X2*(-V3-7*V2-4*V1)+X3*(V3+7*V2+4*V1)+2
     &              *Y2*(U3+7*U2+4*U1)+Y3*(-U3-7*U2-4*U1))*XSU216
!
        A23(IELEM)= (2*X2*(4*V3+7*V2+V1)+X3*(-4*V3-7*V2-V1)+2
     &              *Y2*(-4*U3-7*U2-U1)+Y3*(4*U3+7*U2+U1))*XSU216
!
        A24(IELEM)= (3*X2*(-V3+V1)+X3*(4*V3+7*V2+V1)+3*Y2*(U3-
     &              U1)+Y3*(-4*U3-7*U2-U1))*XSUR72
!
        A31(IELEM)= (X2*(-7*V3-V2-4*V1)+2*X3*(7*V3+V2+4*V1)+Y2
     &              *(7*U3+U2+4*U1)+2*Y3*(-7*U3-U2-4*U1))*XSU216
!
        A32(IELEM)= (X2*(7*V3+4*V2+V1)+2*X3*(-7*V3-4*V2-V1)+Y2
     &              *(-7*U3-4*U2-U1)+2*Y3*(7*U3+4*U2+U1))*XSU216
!
        A34(IELEM)= (X2*(-7*V3-4*V2-V1)+3*X3*(V2-V1)+Y2*(7*U3+
     &              4*U2+U1)+3*Y3*(-U2+U1))*XSUR72
!
        A41(IELEM)= (X2*(-3*V3-4*V2-5*V1)+X3*(4*V3+3*V2+5*V1)
     &              +Y2*(3*U3+4*U2+5*U1)
     &              +Y3*(-4*U3-3*U2-5*U1))*XSUR72
!
        A42(IELEM)= (X2*(V3-V1)+X3*(-4*V3-5*V2-3*V1)+Y2*(-U3+U1)
     &              +Y3*(4*U3+5*U2+3*U1))*XSUR72
!
        A43(IELEM)= (X2*(5*V3+4*V2+3*V1)+X3*(-V2+V1)+Y2*(-5*U3-
     &              4*U2-3*U1)+Y3*(U2-U1))*XSUR72
!
!  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
        A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.12.AND.IELMV.EQ.12) THEN
!
!-----------------------------------------------------------------------
!
!  QUASI-BUBBLE DISCRETISATION OF THE VELOCITY:
!
      IF(FORMUL(16:16).EQ.'N') THEN
!
!  N SCHEME
!
      DO IELEM = 1 , NELEM
!
!     COORDINATES OF THE SUB-TRIANGLE VERTICES
      X1T1 = XEL(IELEM,1)
      X2T1 = XEL(IELEM,2) - X1T1
      Y1T1 = YEL(IELEM,1)
      Y2T1 = YEL(IELEM,2) - Y1T1
!
      X1T2 = XEL(IELEM,2)
      X2T2 = XEL(IELEM,3) - X1T2
      Y1T2 = YEL(IELEM,2)
      Y2T2 = YEL(IELEM,3) - Y1T2
!
      X1T3 = XEL(IELEM,3)
      X2T3 = XEL(IELEM,1) - X1T3
      Y1T3 = YEL(IELEM,3)
      Y2T3 = YEL(IELEM,1) - Y1T3
!     POINT 3 IS ALWAYS THE CENTRE OF THE INITIAL TRIANGLE
      X4   = TIERS * (XEL(IELEM,1)+XEL(IELEM,2)+XEL(IELEM,3))
      Y4   = TIERS * (YEL(IELEM,1)+YEL(IELEM,2)+YEL(IELEM,3))
      X3T1 = X4 - X1T1
      Y3T1 = Y4 - Y1T1
      X3T2 = X4 - X1T2
      Y3T2 = Y4 - Y1T2
      X3T3 = X4 - X1T3
      Y3T3 = Y4 - Y1T3
!
      U1 = U(IKLE1(IELEM))
      U2 = U(IKLE2(IELEM))
      U3 = U(IKLE3(IELEM))
      U4 = U(IKLE4(IELEM))
      V1 = V(IKLE1(IELEM))
      V2 = V(IKLE2(IELEM))
      V3 = V(IKLE3(IELEM))
      V4 = V(IKLE4(IELEM))
!
      US2T1 = (U1+U2+U4)*XSUR6
      VS2T1 = (V1+V2+V4)*XSUR6
      US2T2 = (U2+U3+U4)*XSUR6
      VS2T2 = (V2+V3+V4)*XSUR6
      US2T3 = (U3+U1+U4)*XSUR6
      VS2T3 = (V3+V1+V4)*XSUR6
!
      K1T1 = US2T1 * (Y2T1-Y3T1) - VS2T1 * (X2T1-X3T1)
      K2T1 = US2T1 * (Y3T1     ) - VS2T1 * (X3T1     )
      K3T1 = US2T1 * (    -Y2T1) - VS2T1 * (    -X2T1)
!
      K1T2 = US2T2 * (Y2T2-Y3T2) - VS2T2 * (X2T2-X3T2)
      K2T2 = US2T2 * (Y3T2     ) - VS2T2 * (X3T2     )
      K3T2 = US2T2 * (    -Y2T2) - VS2T2 * (    -X2T2)
!
      K1T3 = US2T3 * (Y2T3-Y3T3) - VS2T3 * (X2T3-X3T3)
      K2T3 = US2T3 * (Y3T3     ) - VS2T3 * (X3T3     )
      K3T3 = US2T3 * (    -Y2T3) - VS2T3 * (    -X2T3)
!
      L12T1 = MAX( MIN(K1T1,-K2T1) , 0.D0 )
      L13T1 = MAX( MIN(K1T1,-K3T1) , 0.D0 )
      L21T1 = MAX( MIN(K2T1,-K1T1) , 0.D0 )
      L23T1 = MAX( MIN(K2T1,-K3T1) , 0.D0 )
      L31T1 = MAX( MIN(K3T1,-K1T1) , 0.D0 )
      L32T1 = MAX( MIN(K3T1,-K2T1) , 0.D0 )
!
      L12T2 = MAX( MIN(K1T2,-K2T2) , 0.D0 )
      L13T2 = MAX( MIN(K1T2,-K3T2) , 0.D0 )
      L21T2 = MAX( MIN(K2T2,-K1T2) , 0.D0 )
      L23T2 = MAX( MIN(K2T2,-K3T2) , 0.D0 )
      L31T2 = MAX( MIN(K3T2,-K1T2) , 0.D0 )
      L32T2 = MAX( MIN(K3T2,-K2T2) , 0.D0 )
!
      L12T3 = MAX( MIN(K1T3,-K2T3) , 0.D0 )
      L13T3 = MAX( MIN(K1T3,-K3T3) , 0.D0 )
      L21T3 = MAX( MIN(K2T3,-K1T3) , 0.D0 )
      L23T3 = MAX( MIN(K2T3,-K3T3) , 0.D0 )
      L31T3 = MAX( MIN(K3T3,-K1T3) , 0.D0 )
      L32T3 = MAX( MIN(K3T3,-K2T3) , 0.D0 )
!
!  EXTRADIAGONAL TERMS
!
      A12(IELEM) = - L12T1
      A13(IELEM) = - L21T3
      A14(IELEM) = - L13T1 - L23T3
      A21(IELEM) = - L21T1
      A23(IELEM) = - L12T2
      A24(IELEM) = - L13T2 - L23T1
      A31(IELEM) = - L12T3
      A32(IELEM) = - L21T2
      A34(IELEM) = - L13T3 - L23T2
      A41(IELEM) = - L31T1 - L32T3
      A42(IELEM) = - L31T2 - L32T1
      A43(IELEM) = - L31T3 - L32T2
!
      A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
      A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
      A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
      A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
!
      ENDDO ! IELEM
!
      ELSE
!
      DO IELEM = 1 , NELEM
!
!  TRADITIONAL METHOD
!
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
        U4 = U(IKLE4(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
        V4 = V(IKLE4(IELEM))
!
!  EXTRADIAGONAL TERMS
!
        A12(IELEM)= (X2*(-V4-V2-2*V1)+X3*(-V4-V2-2*V1)+Y2*(U4+U2+
     &              2*U1)+Y3*(U4+U2+2*U1))*XSUR72
!
        A13(IELEM)= (X2*(V3+V4+2*V1)+X3*(V3+V4+2*V1)+Y2*(-U3-U4-
     &              2*U1)+Y3*(-U3-U4-2*U1))*XSUR72
!
        A14(IELEM)= (X2*(V4+V2+2*V1)+X3*(-V3-V4-2*V1)+Y2*(-U4-U2-
     &              2*U1)+Y3*(U3+U4+2*U1))*XSUR24
!
        A21(IELEM)= (2*X2*(-V4-2*V2-V1)+X3*(V4+2*V2+V1)+2*Y2*(
     &              U4+2*U2+U1)+Y3*(-U4-2*U2-U1))*XSUR72
!
        A23(IELEM)= (2*X2*(V3+V4+2*V2)+X3*(-V3-V4-2*V2)+2*Y2*(-
     &              U3-U4-2*U2)+Y3*(U3+U4+2*U2))*XSUR72
!
        A24(IELEM)= (X2*(-V3+V1)+X3*(V3+V4+2*V2)+Y2*(U3-U1)+Y3*(-
     &              U3-U4-2*U2))*XSUR24
!
        A31(IELEM)= (X2*(-2*V3-V4-V1)+2*X3*(2*V3+V4+V1)+Y2*(2*
     &              U3+U4+U1)+2*Y3*(-2*U3-U4-U1))*XSUR72
!
        A32(IELEM)= (X2*(2*V3+V4+V2)+2*X3*(-2*V3-V4-V2)+Y2*(-2*
     &              U3-U4-U2)+2*Y3*(2*U3+U4+U2))*XSUR72
!
        A34(IELEM)= (X2*(-2*V3-V4-V2)+X3*(V2-V1)+Y2*(2*U3+U4+U2)+
     &              Y3*(-U2+U1))*XSUR24
!
        A41(IELEM)= (X2*(-V3-6*V4-2*V2-3*V1)+X3*(2*V3+6*V4+V2+
     &              3*V1)+Y2*(U3+6*U4+2*U2+3*U1)
     &              +Y3*(-2*U3-6*U4-U2-3*U1))*XSUR72
!
        A42(IELEM)= (X2*(V3-V1)+X3*(-2*V3-6*V4-3*V2-V1)+Y2*(-U3+
     &              U1)+Y3*(2*U3+6*U4+3*U2+U1))*XSUR72
!
        A43(IELEM)= (X2*(3*V3+6*V4+2*V2+V1)+X3*(-V2+V1)+Y2*(-3*
     &              U3-6*U4-2*U2-U1)+Y3*(U2-U1))*XSUR72
!
!  THE DIAGONAL TERMS ARE OBTAINED BY MEANS OF THE 'MAGIC SQUARE':
!
        A11(IELEM) = - A12(IELEM) - A13(IELEM) - A14(IELEM)
        A22(IELEM) = - A21(IELEM) - A23(IELEM) - A24(IELEM)
        A33(IELEM) = - A31(IELEM) - A32(IELEM) - A34(IELEM)
        A44(IELEM) = - A41(IELEM) - A42(IELEM) - A43(IELEM)
!
      ENDDO ! IELEM
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,11) IELMU,IELMV
11      FORMAT(1X,
     &  'MT05BB (BIEF) : TYPES OF VELOCITIES NOT AVAILABLE : ',2I6)
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
