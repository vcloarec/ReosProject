!                   *****************
                    SUBROUTINE CFLP11
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
!history  JMH
!+        17/08/94
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
      DOUBLE PRECISION, INTENT(IN)  :: X(NELMAX,*),Y(NELMAX,*)
      INTEGER         , INTENT(IN)  :: IKLE(NELMAX,*)
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
      DOUBLE PRECISION U1,U2,U3,V1,V2,V3,USUR2,VSUR2
      DOUBLE PRECISION SUR6,K1,K2,K3,L12,L13,L21,L23,L31,L32
      DOUBLE PRECISION X2,X3,Y2,Y3
!
      INTRINSIC MAX,MIN
!
!-----------------------------------------------------------------------
!
      SUR6 = 1.D0 / 6.D0
!
! LOOP ON THE ELEMENTS
!
        DO IELEM = 1, NELEM
!
          X2 = X(IELEM,2)
          X3 = X(IELEM,3)
          Y2 = Y(IELEM,2)
          Y3 = Y(IELEM,3)
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
          W1(IELEM,1) = L12 + L13
          W1(IELEM,2) = L21 + L23
          W1(IELEM,3) = L31 + L32
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
