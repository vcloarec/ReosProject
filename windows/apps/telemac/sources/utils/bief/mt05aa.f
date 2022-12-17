!                   *****************
                    SUBROUTINE MT05AA
!                   *****************
!
     &( A11 , A12 , A13 ,
     &  A21 , A22 , A23 ,
     &  A31 , A32 , A33 ,
     &  XMUL,SU,SV,U,V,
     &  XEL,YEL,IKLE,NELEM,NELMAX,FORMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                       /           ->  --->
!+       A(I,J) = XMUL  /  PSI1(I) * U . GRAD(PSI2(J)) D(OMEGA)
!+                     /OMEGA
!+
!+  PSI1: BASES OF TYPE P1 TRIANGLE
!+  PSI2: BASES OF TYPE P1 TRIANGLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        05/02/91
!+        V5P1
!+
!
!history  A FROEHLY (MATMECA)
!+        01/07/08
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
!| A21            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A31            |<--| ELEMENTS OF MATRIX
!| A32            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
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
      USE BIEF, EX_MT05AA => MT05AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,U(*),V(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: SU,SV
      CHARACTER(LEN=16) :: FORMUL
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMU,IELMV,IELEM
!
      DOUBLE PRECISION SUR24,SUR120,SUR216
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION U123,V123,COU1,COV1,COU2,COV2,COU3,COV3
      DOUBLE PRECISION QUATRU,QUATRV,SUR6,USUR2,VSUR2
      DOUBLE PRECISION K1,K2,K3,L12,L13,L21,L23,L31,L32
!
      INTRINSIC MAX,MIN
!
!-----------------------------------------------------------------------
!
      SUR6   = XMUL/  6.D0
      SUR24  = XMUL/ 24.D0
      SUR120 = XMUL/120.D0
      SUR216 = XMUL/216.D0
!
!-----------------------------------------------------------------------
!
      IELMU = SU%ELM
      IELMV = SV%ELM
!
!-----------------------------------------------------------------------
!
!  CASE WHERE U AND V ARE CONSTANT BY ELEMENT
!
      IF(IELMU.EQ.10.AND.IELMV.EQ.10) THEN
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2) * SUR24
      X3  =   XEL(IELEM,3) * SUR24
      Y2  =   YEL(IELEM,2) * SUR24
      Y3  =   YEL(IELEM,3) * SUR24
!
      QUATRU = 4 * U(IELEM)
      QUATRV = 4 * V(IELEM)
!
!   DIAGONAL TERMS
!
      A11(IELEM)    = (X3-X2) * QUATRV + (Y2-Y3) * QUATRU
      A22(IELEM)    = -X3     * QUATRV      +Y3  * QUATRU
      A33(IELEM)    =     X2  * QUATRV -  Y2     * QUATRU
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)  = -X3     * QUATRV +     Y3  * QUATRU
      A13(IELEM)  =     X2  * QUATRV - Y2      * QUATRU
      A23(IELEM)  =     X2  * QUATRV - Y2      * QUATRU
      A21(IELEM)  = (X3-X2) * QUATRV + (Y2-Y3) * QUATRU
      A31(IELEM)  = (X3-X2) * QUATRV + (Y2-Y3) * QUATRU
      A32(IELEM)  = -X3     * QUATRV +     Y3  * QUATRU
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!  CASE WHERE U AND V ARE LINEAR, QUASI-BUBBLE OR QUADRATIC AND N SCHEME
!
      ELSEIF(FORMUL(16:16).EQ.'N'   .AND.
     & ( (IELMU.EQ.11.AND.IELMV.EQ.11).OR.
     &   (IELMU.EQ.12.AND.IELMV.EQ.12).OR.
     &   (IELMU.EQ.13.AND.IELMV.EQ.13)      )  ) THEN
!
!     N SCHEME: U AND V ARE TREATED AS IF LINEAR
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        U1 = U(IKLE(IELEM,1))
        U2 = U(IKLE(IELEM,2))
        U3 = U(IKLE(IELEM,3))
        V1 = V(IKLE(IELEM,1))
        V2 = V(IKLE(IELEM,2))
        V3 = V(IKLE(IELEM,3))
!
        USUR2 = (U1+U2+U3)*SUR6
        VSUR2 = (V1+V2+V3)*SUR6
!
        K1 = USUR2 * (Y2-Y3) - VSUR2 * (X2-X3)
        K2 = USUR2 * (Y3   ) - VSUR2 * (X3   )
        K3 = USUR2 * (  -Y2) - VSUR2 * (  -X2)
!
        L12 = MAX(  MIN(K1,-K2) , 0.D0 )
        L13 = MAX(  MIN(K1,-K3) , 0.D0 )
        L21 = MAX(  MIN(K2,-K1) , 0.D0 )
        L23 = MAX(  MIN(K2,-K3) , 0.D0 )
        L31 = MAX(  MIN(K3,-K1) , 0.D0 )
        L32 = MAX(  MIN(K3,-K2) , 0.D0 )
!
!   DIAGONAL TERMS
!
        A11(IELEM)  = L12 + L13
        A22(IELEM)  = L21 + L23
        A33(IELEM)  = L31 + L32
!
!   EXTRADIAGONAL TERMS
!
        A12(IELEM)  = - L12
        A13(IELEM)  = - L13
        A23(IELEM)  = - L23
        A21(IELEM)  = - L21
        A31(IELEM)  = - L31
        A32(IELEM)  = - L32
!
      ENDDO ! IELEM
!
      ELSEIF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
!   TRADITIONAL SCHEME, U AND V LINEAR
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2) * SUR24
      X3  =   XEL(IELEM,3) * SUR24
      Y2  =   YEL(IELEM,2) * SUR24
      Y3  =   YEL(IELEM,3) * SUR24
!
      U1   =  U(IKLE(IELEM,1))
      U2   =  U(IKLE(IELEM,2))
      U3   =  U(IKLE(IELEM,3))
      V1   =  V(IKLE(IELEM,1))
      V2   =  V(IKLE(IELEM,2))
      V3   =  V(IKLE(IELEM,3))
!
      U123 =  U1 + U2 + U3
      V123 =  V1 + V2 + V3
!
!   DIAGONAL TERMS
!
      A11(IELEM)    = (X3-X2) * (V123+V1) + (Y2-Y3) * (U123+U1)
      A22(IELEM)    = -X3     * (V123+V2)      +Y3  * (U123+U2)
      A33(IELEM)    =     X2  * (V123+V3) -  Y2     * (U123+U3)
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)  = -X3     * (V123+V1) +     Y3  * (U123+U1)
      A13(IELEM)  =     X2  * (V123+V1) -  Y2     * (U123+U1)
      A23(IELEM)  =     X2  * (V123+V2) -  Y2     * (U123+U2)
      A21(IELEM)  = (X3-X2) * (V123+V2) + (Y2-Y3) * (U123+U2)
      A31(IELEM)  = (X3-X2) * (V123+V3) + (Y2-Y3) * (U123+U3)
      A32(IELEM)  = -X3     * (V123+V3) +     Y3  * (U123+U3)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.12.AND.IELMV.EQ.12) THEN
!
!   TRADITIONAL SCHEME, U AND V QUASI-BUBBLE
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
!
      U1   =  U(IKLE(IELEM,1))
      U2   =  U(IKLE(IELEM,2))
      U3   =  U(IKLE(IELEM,3))
      U4   =  U(IKLE(IELEM,4))
      V1   =  V(IKLE(IELEM,1))
      V2   =  V(IKLE(IELEM,2))
      V3   =  V(IKLE(IELEM,3))
      V4   =  V(IKLE(IELEM,4))
!
      COV1 =  5*V3+12*V4+5*V2+14*V1
      COU1 =  5*U3+12*U4+5*U2+14*U1
      COV2 =  5*V3+12*V4+14*V2+5*V1
      COU2 =  5*U3+12*U4+14*U2+5*U1
      COV3 =  14*V3+12*V4+5*V2+5*V1
      COU3 =  14*U3+12*U4+5*U2+5*U1
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM)  = ( -X3*COV1 + Y3*COU1 )*SUR216
      A13(IELEM)  = (  X2*COV1 - Y2*COU1 )*SUR216
      A21(IELEM)  = ( (X3-X2)*COV2 + (Y2-Y3)*COU2 )*SUR216
      A23(IELEM)  = (  X2*COV2 - Y2*COU2 )*SUR216
      A31(IELEM)  = ( (X3-X2)*COV3 + (Y2-Y3)*COU3 )*SUR216
      A32(IELEM)  = ( -X3*COV3 + Y3*COU3 )*SUR216
!
!   DIAGONAL TERMS (SUM OF EACH COLUMN = 0)
!
      A11(IELEM) = - A12(IELEM) - A13(IELEM)
      A22(IELEM) = - A21(IELEM) - A23(IELEM)
      A33(IELEM) = - A31(IELEM) - A32(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.13.AND.IELMV.EQ.13) THEN
!
!   TRADITIONAL SCHEME, U AND V P2
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      X2  =   XEL(IELEM,2)
      X3  =   XEL(IELEM,3)
      Y2  =   YEL(IELEM,2)
      Y3  =   YEL(IELEM,3)
!
      U1   =  U(IKLE(IELEM,1))
      U2   =  U(IKLE(IELEM,2))
      U3   =  U(IKLE(IELEM,3))
      U4   =  U(IKLE(IELEM,4))
      U5   =  U(IKLE(IELEM,5))
      U6   =  U(IKLE(IELEM,6))
      V1   =  V(IKLE(IELEM,1))
      V2   =  V(IKLE(IELEM,2))
      V3   =  V(IKLE(IELEM,3))
      V4   =  V(IKLE(IELEM,4))
      V5   =  V(IKLE(IELEM,5))
      V6   =  V(IKLE(IELEM,6))
!
!   EXTRADIAGONAL TERMS
!
      A12(IELEM) = ((V3+V2-2.D0*V1-4.D0*V5-8.D0*(V4+V6)) * X3
     &           -  (U2-4.D0*U5-8.D0*(U4+U6)-2.D0*U1+U3) * Y3) * SUR120
      A13(IELEM) = ((2.D0*V1-V3-V2+4.D0*V5+8.D0*(V4+V6)) * X2
     &           +  (U2-4.D0*U5-8.D0*(U4+U6)-2.D0*U1+U3) * Y2) * SUR120
      A21(IELEM) = ((V1-8.D0*(V4+V5)-4.D0*V6+V3-2.D0*V2) * (X2-X3)
     &           +  (2.D0*U2-U3-U1+8.D0*(U5+U4)+4.D0*U6) * (Y2-Y3))
     &           *   SUR120
      A23(IELEM) = ((8.D0*(V4+V5)-V1+4.D0*V6-V3+2.D0*V2) * X2
     &           -  (2.D0*U2-U3-U1+8.D0*(U5+U4)+4.D0*U6) * Y2) * SUR120
      A31(IELEM) = ((V1+V2-4.D0*V4-2.D0*V3-8.D0*(V6+V5)) * (X2-X3)
     &           +  (4.D0*U4-U2+8.D0*(U6+U5)+2.D0*U3-U1) * (Y2-Y3))
     &           *   SUR120
      A32(IELEM) = ((V1+V2-4.D0*V4-2.D0*V3-8.D0*(V6+V5)) * X3
     &           +  (4.D0*U4-U2+8.D0*(U6+U5)+2.D0*U3-U1) * Y3) * SUR120
!
!   DIAGONAL TERMS (SUM OF EACH COLUMN = 0)
!
      A11(IELEM) = - A12(IELEM) - A13(IELEM)
      A22(IELEM) = - A21(IELEM) - A23(IELEM)
      A33(IELEM) = - A31(IELEM) - A32(IELEM)
!
      ENDDO ! IELEM
!
!     OTHER TYPES OF U AND V DISCRETISATION
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(IELMU.EQ.IELMV) THEN
        WRITE(LU,101) IELMU
101     FORMAT(1X,'MT05AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF U AND V : ',1I6,' NOT AVAILABLE')
        ELSE
        WRITE(LU,201) IELMU,IELMV
201     FORMAT(1X,'MT05AA (BIEF) :',/,
     &         1X,'U AND V OF A DIFFERENT DISCRETISATION:',1I6,3X,1I6)
        ENDIF
!
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
