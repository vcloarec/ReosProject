!                   *****************
                    SUBROUTINE CFLP12
!                   *****************
!
     &(U,V,X,Y,IKLE,NELEM,NELMAX,W1)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COURANT NUMBER AT EACH POINT OF THE MESH
!+                AND FOR EACH TIMESTEP.
!+
!+            THE STABILITY CRITERION OF THE DISTRIBUTIVE SCHEME N
!+                IS HERE USED TO EVALUATE THE COURANT NUMBER.
!
!history  C MOULIN   (LNH)
!+
!+
!+
!
!history  JMH
!+        29/12/05
!+        V5P6
!+   MODIFICATIONS
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
!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
!| NELMAX         |-->| FIRST DIMENSION OF IKLE, MAXIMUM NUMBER OF ELEMENTS
!|                |   | IN THE MESH
!| U              |-->| VELOCITY ALONG X.
!| V              |-->| VELOCITY ALONG Y.
!| W1             |-->| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS GIVEN PER ELEMENT
!| Y              |-->| ORDINATES OF POINTS GIVEN PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)  :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(IN)  :: U(*),V(*)
      DOUBLE PRECISION, INTENT(IN)  :: X(NELMAX*3),Y(NELMAX*3)
      INTEGER         , INTENT(IN)  :: IKLE(NELMAX*4)
      DOUBLE PRECISION, INTENT(OUT) :: W1(NELMAX*4)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IT,IAD1,IAD2,IAD3,IG1,IG2,IG3
!
      DOUBLE PRECISION USUR2,VSUR2
      DOUBLE PRECISION SUR6,K1,K2,K3,L12,L13,L21,L23,L31,L32
      DOUBLE PRECISION X1,X2,X3,Y1,Y2,Y3,TIERS
!
      INTRINSIC MAX,MIN
!
!-----------------------------------------------------------------------
!
!     FOR A QUASI-BUBBLE TRIANGLE : NUMBERS OF THE VERTICES OF THE
!     SUB-TRIANGLES IN THE INITIAL TRIANGLE
!     IL(NUMBER OF THE SUB-TRIANGLE,LOCAL NUMBER IN THE SUB-TRIANGLE)
!
      INTEGER :: IL(3,3)
      PARAMETER ( IL = RESHAPE( (/
     &          1,2,3,2,3,1,4,4,4 /), SHAPE=(/ 3,3 /) ) )
!
!-----------------------------------------------------------------------
!
      TIERS= 1.D0 / 3.D0
      SUR6 = 1.D0 / 6.D0
!
!     INITIALISES W
!
      DO IELEM = 1 , 4*NELMAX
        W1(IELEM) = 0.D0
      ENDDO ! IELEM
!
!     USING THE PSI SCHEME,
!     LOOP ON THE 3 SUB-TRIANGLES AND PRE-ASSEMBLY
!
      DO IT=1,3
      DO IELEM = 1 , NELEM
!
!       ADDRESSES IN AN ARRAY (NELMAX,*)
        IAD1= IELEM + (IL(IT,1)-1)*NELMAX
        IAD2= IELEM + (IL(IT,2)-1)*NELMAX
        IAD3= IELEM + (IL(IT,3)-1)*NELMAX
!       GLOBAL NUMBERS IN THE INITIAL TRIANGLE
        IG1 = IKLE(IAD1)
        IG2 = IKLE(IAD2)
        IG3 = IKLE(IAD3)
!       COORDINATES OF THE SUB-TRIANGLE VERTICES
        X1 = X(IAD1)
        X2 = X(IAD2) - X1
        Y1 = Y(IAD1)
        Y2 = Y(IAD2) - Y1
!       POINT 3 IS ALWAYS AT THE CENTRE OF THE INITIAL TRIANGLE
        X3=TIERS*(X(IELEM)+X(IELEM+NELMAX)+X(IELEM+2*NELMAX))-X1
        Y3=TIERS*(Y(IELEM)+Y(IELEM+NELMAX)+Y(IELEM+2*NELMAX))-Y1
!
        USUR2 = (U(IG1)+U(IG2)+U(IG3))*SUR6
        VSUR2 = (V(IG1)+V(IG2)+V(IG3))*SUR6
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
        W1(IAD1) = W1(IAD1) + L12 + L13
        W1(IAD2) = W1(IAD2) + L21 + L23
        W1(IAD3) = W1(IAD3) + L31 + L32
!
      ENDDO ! IELEM
      ENDDO ! IT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
