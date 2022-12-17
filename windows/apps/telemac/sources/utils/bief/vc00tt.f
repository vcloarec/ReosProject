!                   *****************
                    SUBROUTINE VC00TT
!                   *****************
!
     &(XMUL,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,
     & NELEM,NELMAX,W1,W2,W3,W4,FORMUL,NPOIN2,NELEM2,IELM1)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 TETRAHEDRON
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)
!+        22/03/02
!+        V5P3
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
!| FORMUL         |-->| STRING WITH THE FORMULA DESCRIBING THE VECTOR
!| IELM1          |-->| TYPE OF ELEMENT
!| IKLE1          |-->| FIRST POINT OF TETRAHEDRA
!| IKLE2          |-->| SECOND POINT OF TETRAHEDRA
!| IKLE3          |-->| THIRD POINT OF TETRAHEDRA
!| IKLE4          |-->| FOURTH POINT OF TETRAHEDRA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN2         |-->| NUMBER OF POINTS IN THE UNDERLYING 2D MESH
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN2,NELEM2,IELM1
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSUR24,X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4,H1,H2,H3,H4
      DOUBLE PRECISION COEF
      INTEGER I1,I2,I3,I4,IELEM,IP,I12D,I22D,I32D,I42D,IELEM2D
      INTEGER IT1,IT2,IT3
!
!-----------------------------------------------------------------------
!
      XSUR24 = XMUL/24.D0
!
!-----------------------------------------------------------------------
!
      IF(FORMUL(1:7).EQ.'MASBAS ') THEN
!
!     STANDARD FORMULA
!
      DO IELEM = 1 , NELEM
!
        I1 = IKLE1(IELEM)
        I2 = IKLE2(IELEM)
        I3 = IKLE3(IELEM)
        I4 = IKLE4(IELEM)
!
        X2 = X(I2)-X(I1)
        X3 = X(I3)-X(I1)
        X4 = X(I4)-X(I1)
!
        Y2 = Y(I2)-Y(I1)
        Y3 = Y(I3)-Y(I1)
        Y4 = Y(I4)-Y(I1)
!
        Z2 = Z(I2)-Z(I1)
        Z3 = Z(I3)-Z(I1)
        Z4 = Z(I4)-Z(I1)
!
        W1(IELEM) =
     &       (X2*(Y3*Z4-Y4*Z3)+Y2*(X4*Z3-X3*Z4)+Z2*(X3*Y4-X4*Y3))*XSUR24
        W2(IELEM) = W1(IELEM)
        W3(IELEM) = W1(IELEM)
        W4(IELEM) = W1(IELEM)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(1:7).EQ.'MASBAS2'.AND.IELM1.EQ.51) THEN
!
!     FORMULA WITH MASS-LUMPING
!
!     LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
        I1 = IKLE1(IELEM)
        I2 = IKLE2(IELEM)
        I3 = IKLE3(IELEM)
        I4 = IKLE4(IELEM)
!
!       RETRIEVING THE LOWER PLANE NUMBER
!
        IP=(MIN(I1,I2,I3,I4)-1)/NPOIN2 +1
!
!       RETRIEVING THE 2D POINTS NUMBERS ON THE SAME VERTICAL
!
        I12D=MOD(I1-1,NPOIN2)+1
        I22D=MOD(I2-1,NPOIN2)+1
        I32D=MOD(I3-1,NPOIN2)+1
        I42D=MOD(I4-1,NPOIN2)+1
!
!       RETRIEVING THE ORIGINAL PRISM HEIGHTS ON THE VERTICAL
!       TWO OF THE FOUR HEIGHTS WILL BE EQUAL, THE TWO CORRESPONDING
!       POINTS OF THE TETRAHEDRON BEING ON THE SAME VERTICAL
!
        H1=Z(IP*NPOIN2+I12D)-Z((IP-1)*NPOIN2+I12D)
        H2=Z(IP*NPOIN2+I22D)-Z((IP-1)*NPOIN2+I22D)
        H3=Z(IP*NPOIN2+I32D)-Z((IP-1)*NPOIN2+I32D)
        H4=Z(IP*NPOIN2+I42D)-Z((IP-1)*NPOIN2+I42D)
!
        X2 = X(I2)-X(I1)
        X3 = X(I3)-X(I1)
        X4 = X(I4)-X(I1)
!
        Y2 = Y(I2)-Y(I1)
        Y3 = Y(I3)-Y(I1)
        Y4 = Y(I4)-Y(I1)
!
        Z2 = Z(I2)-Z(I1)
        Z3 = Z(I3)-Z(I1)
        Z4 = Z(I4)-Z(I1)
!
!       RETRIEVING THE CORRESPONDING TRIANGLE
        IELEM2D=MOD(IELEM-1,NELEM2)+1
!       IT1,IT2 AND IT3 FORM THE PROJECTION OF THE TETRAHEDRON ON THE
!       PLANE BELOW IT, IT IS A TRIANGLE
        IT1=IKLE1(IELEM2D)+(IP-1)*NPOIN2
        IT2=IKLE2(IELEM2D)+(IP-1)*NPOIN2
        IT3=IKLE3(IELEM2D)+(IP-1)*NPOIN2
        X2 = X(IT2)-X(IT1)
        X3 = X(IT3)-X(IT1)
        Y2 = Y(IT2)-Y(IT1)
        Y3 = Y(IT3)-Y(IT1)
!
!       HERE COEF SHOULD BE PROPORTIONNAL TO THE VOLUME OF THE ELEMENT
!       BUT THE 3 TETRAHEDRONS IN A PRISM DO NOT HAVE THE SAME VOLUME
!       THEN THE SUM OF 3D VOLUMES ON THE VERTICAL DO NOT SUM CORRECTLY
!       TO GIVE DEPTH * VOLU2D. THE FORMULA BELOW ENSURES THIS PROPERTY
!
        COEF=(X2*Y3-X3*Y2)*XSUR24
!
        W1(IELEM) = COEF*H1
        W2(IELEM) = COEF*H2
        W3(IELEM) = COEF*H3
        W4(IELEM) = COEF*H4
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,*) ' '
        WRITE(LU,*) 'UNKNOWN FORMULA IN VC00TT:',FORMUL
        WRITE(LU,*) 'WITH ELEMENT:',IELM1
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
