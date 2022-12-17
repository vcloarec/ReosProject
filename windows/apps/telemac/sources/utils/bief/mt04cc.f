!                   *****************
                    SUBROUTINE MT04CC
!                   *****************
!
     &( A11 , A12 , A13 , A14 , A15 , A16 ,
     &        A22 , A23 , A24 , A25 , A26 ,
     &              A33 , A34 , A35 , A36 ,
     &                    A44 , A45 , A46 ,
     &                          A55 , A56 ,
     &                                A66 ,
     &  XMUL,SU,SV,U,V,XEL,YEL,IKLE1,IKLE2,IKLE3,
     &  IKLE4,IKLE5,IKLE6,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE SUPG MATRIX:
!code
!+            ->--->        ->--->
!+           (U.GRAD(PI))* (U.GRAD(PJ))  WITH
!+
!+          PI OF P2 DISCRETISATION
!+          PJ OF P2 DISCRETISATION
!
!history  A FROEHLY (MATMECA)
!+        01/06/08
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
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| IKLE4          |-->| FOURTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE5          |-->| FIFTH POINTS OF TRIANGLES (QUADRATIC)
!| IKLE6          |-->| SIXTH POINTS OF TRIANGLES (QUADRATIC)
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
      USE BIEF!, EX_MT04CC => MT04CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
      INTEGER, INTENT(IN) :: IKLE5(NELMAX),IKLE6(NELMAX)
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
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!     STRUCTURES OF      U, V
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMU,IELMV,IELEM
!
      DOUBLE PRECISION X2,X3,Y2,Y3,ANS1,ANS2,ANS3,ANS4
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6
      DOUBLE PRECISION V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION UNSU2, AUX360, AUX180, AUX45, AUX720, AUX1260
      DOUBLE PRECISION AUX2520, AUX630
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
      IF(IELMU.EQ.11.AND.IELMV.EQ.11) THEN
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
        UNSU2 = XMUL/(X2*Y3-Y2*X3)
!
        AUX720 = UNSU2/360.D0
        AUX360 = UNSU2/180.D0
        AUX180 = UNSU2/90.D0
        AUX45 = 2.D0*UNSU2/45.D0
!
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
!
!  COMPUTES 15 OF THE 36 TERMS (SELECTED AMONG THE LEAST COMPLEX)
!
!
      ANS1 = 7.D0*U3**2*Y2**2-14.D0*U3*Y2*V3*X2-78.D0*U1*Y2*V1*X2+
     &       7.D0*V3**2*X2**2+39.D0*V1**2*X2**2-7.D0*U2*Y2*V3*X2-
     &       15.D0*U2*Y2*V1*X2+39.D0*U1**2*Y2**2-15.D0*V2*X2*U1*Y2-
     &       7.D0*V2*X2*U3*Y2-15.D0*U3*Y2*V1*X2-15.D0*U1*Y2*V3*X2+
     &       7.D0*V2**2*X2**2+7.D0*U2**2*Y2**2-14.D0*U2*Y2*V2*X2+
     &       7.D0*V3**2*X3**2-30.D0*U2*Y3*U1*Y2+14*U3*Y3*V3*X2+
     &       15.D0*U3*Y2*V1*X3-30.D0*V1*X3*V3*X2-14.D0*U3*Y3*V3*X3-
     &       78.D0*V1**2*X3*X2-15.D0*U3*Y3*V1*X3+15.D0*U1*Y2*V3*X3+
     &       15.D0*U3*Y3*V1*X2+7.D0*U2*Y3**2.D0*U3+14.D0*U3*Y2*V3*X3+
     &       78.D0*U1*Y2*V1*X3-14.D0*U3**2*Y3*Y2+15.D0*U1*Y3**2*U3+
     &       15.D0*U2*Y3**2.D0*U1-14.D0*V3**2*X3*X2+15.D0*V1*X3**2*V3-
     &       15.D0*U1*Y3*V3*X3-78.D0*U1**2*Y3*Y2+78.D0*U1*Y3*V1*X2+
     &       15.D0*U1*Y3*V3*X2-30.D0*U1*Y3*U3*Y2+15.D0*V2*X3**2*V1+
     &       7.D0*V2*X3**2*V3-14.D0*U2**2*Y3*Y2+7.D0*U3**2*Y3**2+
     &       39.D0*U1**2*Y3**2+14.D0*U2*Y2*V2*X3-78.D0*U1*Y3*V1*X3-
     &       14.D0*V2**2*X3*X2+15.D0*V1*X2**2*V3-14.D0*U2*Y3*V2*X3-
     &       15.D0*V2*X3*U1*Y3+15.D0*V2*X3*U1*Y2+7.D0*V2**2*X3**2+
     &       7.D0*U2**2*Y3**2+14.D0*U2*Y3*V2*X2+7.D0*V2*X2*U3*Y3
      A11(IELEM) = (7.D0*V2*X2**2*V3-14.D0*V2*X3*V3*X2+
     &              15.D0*V2*X2*U1*Y3+15.D0*U2*Y2*V1*X3+
     &              7.D0*V2*X3*U3*Y2-30.D0*V2*X3*V1*X2-
     &              7.D0*V2*X3*U3*Y3+39.D0*V1**2*X3**2+
     &              7.D0*U2*Y2**2*U3+15.D0*V2*X2**2*V1-
     &              14.D0*U2*Y3*U3*Y2-15.D0*U2*Y3*V1*X3-
     &              7.D0*U2*Y3*V3*X3+7.D0*U2*Y2*V3*X3+
     &              15.D0*U2*Y3*V1*X2+7.D0*U2*Y3*V3*X2+
     &              15.D0*U2*Y2**2*U1+15.D0*U1*Y2**2.D0*U3+
     &              ANS1) * AUX360
!
      A12(IELEM)= (-18.D0*U1**2*Y3**2-18.D0*V2**2*X3**2-
     &             18.D0*V1**2*X3**2+18.D0*U1**2*Y3*Y2+
     &             18.D0*V1**2*X3*X2-10.D0*V1*X3**2*V3+
     &             18.D0*V2**2*X3*X2+18.D0*U2**2*Y3*Y2+2.D0*V3**2*X3*X2+
     &             2.D0*U3**2*Y3*Y2-5.D0*V1*X2*U3*Y3-V2*X3*U1*Y2-
     &             5.D0*V2*X3*U3*Y2+2.D0*V2*X3*V1*X2+10.D0*V2*X3*V3*X2-
     &             V2*X2*U1*Y3-5.D0*V2*X2*U3*Y3-U2*Y2*V1*X3-
     &             5.D0*U2*Y2*V3*X3-5.D0*V1*X3*U3*Y2+10.D0*V1*X3*V3*X2+
     &             10.D0*U1*Y3*U3*Y2-18.D0*U1*Y3*V1*X2-5.D0*U1*Y3*V3*X2-
     &             18.D0*U1*Y2*V1*X3-5.D0*U1*Y2*V3*X3+2.D0*U2*Y3*U1*Y2+
     &             10.D0*U2*Y3*U3*Y2-U2*Y3*V1*X2-5.D0*U2*Y3*V3*X2-
     &             18.D0*U2*Y3*V2*X2-18.D0*U2*Y2*V2*X3-2.D0*U3*Y3*V3*X2-
     &             2.D0*U3*Y2*V3*X3-18.D0*U2**2*Y3**2+2.D0*V2*X3*U1*Y3+
     &             10.D0*V2*X3*U3*Y3+10.D0*V1*X3*U3*Y3+
     &             36.D0*U1*Y3*V1*X3+10.D0*U1*Y3*V3*X3-
     &             2.D0*U3**2*Y3**2-2.D0*V3**2*X3**2-10.D0*V2*X3**2*V3-
     &             10.D0*U1*Y3**2*U3-2.D0*V2*X3**2*V1-10.D0*U2*Y3**2*U3-
     &             2.D0*U2*Y3**2*U1+10.D0*U2*Y3*V3*X3+36.D0*U2*Y3*V2*X3+
     &             4.D0*U3*Y3*V3*X3+2.D0*U2*Y3*V1*X3)*(-AUX720)
!
      A13(IELEM) =(2.D0*U2**2*Y2**2+18.D0*U1**2*Y2**2+
     &            18.D0*V1**2*X2**2+2.D0*V2**2*X2**2+18.D0*V3**2*X2**2+
     &            10.D0*U2*Y2**2*U3-18.D0*U1**2*Y3*Y2-18.D0*V1**2*X3*X2+
     &            10.D0*U2*Y2**2*U1-2.D0*V2**2*X3*X2-2.D0*U2**2*Y3*Y2-
     &            18.D0*V3**2*X3*X2-18.D0*U3**2*Y3*Y2+V1*X2*U3*Y3-
     &            2.D0*V1*X2*U3*Y2+5.D0*V2*X3*U1*Y2+5.D0*V2*X3*U3*Y2-
     &            10.D0*V2*X3*V1*X2-10.D0*V2*X3*V3*X2+5.D0*V2*X2*U1*Y3-
     &            10.D0*V2*X2*U1*Y2+5.D0*V2*X2*U3*Y3-10.D0*V2*X2*U3*Y2+
     &            5.D0*U2*Y2*V1*X3-10.D0*U2*Y2*V1*X2+5.D0*U2*Y2*V3*X3-
     &            10.D0*U2*Y2*V3*X2+V1*X3*U3*Y2-2.D0*V1*X3*V3*X2-
     &            2.D0*U1*Y3*U3*Y2+18.D0*U1*Y3*V1*X2+U1*Y3*V3*X2+
     &            18.D0*U1*Y2*V1*X3-36.D0*U1*Y2*V1*X2+U1*Y2*V3*X3-
     &            2.D0*U1*Y2*V3*X2-10.D0*U2*Y3*U1*Y2-10.D0*U2*Y3*U3*Y2+
     &            5.D0*U2*Y3*V1*X2+5.D0*U2*Y3*V3*X2+2.D0*U2*Y3*V2*X2+
     &            2.D0*U2*Y2*V2*X3-4.D0*U2*Y2*V2*X2+18.D0*U3*Y3*V3*X2+
     &            18.D0*U3*Y2*V3*X3-36.D0*U3*Y2*V3*X2+18.D0*U3**2*Y2**2+
     &            10.D0*V2*X2**2*V3+10.D0*V2*X2**2*V1+2.D0*V1*X2**2*V3+
     &            2.D0*U1*Y2**2*U3) * AUX720
!
      ANS1 = 2.D0*U3**2*Y2**2-4.D0*U3*Y2*V3*X2+12.D0*U1*Y2*V1*X2+
     &       2.D0*V3**2*X2**2-6.D0*V1**2*X2**2-4.D0*U2*Y2*V3*X2+
     &       4.D0*U2*Y2*V1*X2-6.D0*U1**2*Y2**2+4.D0*V2*X2*U1*Y2-
     &       4.D0*V2*X2*U3*Y2+2.D0*U3*Y2*V1*X2+2.D0*U1*Y2*V3*X2+
     &       6.D0*V2**2*X2**2+6.D0*U2**2*Y2**2-12.D0*U2*Y2*V2*X2+
     &       4.D0*V3**2*X3**2-4.D0*U2*Y3*U1*Y2+6.D0*U3*Y3*V3*X2+
     &       4.D0*U3*Y2*V1*X3-8.D0*V1*X3*V3*X2-8.D0*U3*Y3*V3*X3-
     &       18.D0*V1**2*X3*X2-10.D0*U3*Y3*V1*X3+4.D0*U1*Y2*V3*X3+
     &       4.D0*U3*Y3*V1*X2+6.D0*U2*Y3**2*U3+6.D0*U3*Y2*V3*X3+
     &       18.D0*U1*Y2*V1*X3-6.D0*U3**2*Y3*Y2+10.D0*U1*Y3**2*U3+
     &       8.D0*U2*Y3**2*U1-6.D0*V3**2*X3*X2+10.D0*V1*X3**2*V3-
     &       10.D0*U1*Y3*V3*X3-18.D0*U1**2*Y3*Y2+18.D0*U1*Y3*V1*X2+
     &       4.D0*U1*Y3*V3*X2-8.D0*U1*Y3*U3*Y2+8.D0*V2*X3**2*V1+
     &       6.D0*V2*X3**2*V3-14.D0*U2**2*Y3*Y2+4.D0*U3**2*Y3**2+
     &       24.D0*U1**2*Y3**2+14.D0*U2*Y2*V2*X3-48.D0*U1*Y3*V1*X3-
     &       14.D0*V2**2*X3*X2-2*V1*X2**2*V3-16.D0*U2*Y3*V2*X3-
     &       8.D0*V2*X3*U1*Y3+2.D0*V2*X3*U1*Y2+8.D0*V2**2*X3**2+
     &       8.D0*U2**2*Y3**2+14.D0*U2*Y3*V2*X2+5.D0*V2*X2*U3*Y3+
     &       4.D0*V2*X2**2*V3-10.D0*V2*X3*V3*X2+2.D0*V2*X2*U1*Y3
      A14(IELEM) = (2.D0*U2*Y2*V1*X3+5.D0*V2*X3*U3*Y2-4.D0*V2*X3*V1*X2-
     &             6.D0*V2*X3*U3*Y3+24.D0*V1**2*X3**2+4.D0*U2*Y2**2*U3-
     &             4.D0*V2*X2**2*V1-10.D0*U2*Y3*U3*Y2-8.D0*U2*Y3*V1*X3-
     &             6.D0*U2*Y3*V3*X3+5.D0*U2*Y2*V3*X3+2.D0*U2*Y3*V1*X2+
     &             5.D0*U2*Y3*V3*X2-4.D0*U2*Y2**2*U1-2.D0*U1*Y2**2*U3+
     &             ANS1)*(- AUX180)
!
      ANS1 = 2.D0*U3**2*Y2**2-4.D0*U3*Y2*V3*X2+12.D0*U1*Y2*V1*X2+
     &       2.D0*V3**2*X2**2-6.D0*V1**2*X2**2-4.D0*U2*Y2*V3*X2+
     &       4.D0*U2*Y2*V1*X2-6.D0*U1**2*Y2**2+4.D0*V2*X2*U1*Y2-
     &       4.D0*V2*X2*U3*Y2+2.D0*U3*Y2*V1*X2+2.D0*U1*Y2*V3*X2+
     &       6.D0*V2**2*X2**2+6.D0*U2**2*Y2**2-12.D0*U2*Y2*V2*X2+
     &       6.D0*V3**2*X3**2+6.D0*U2*Y3*U1*Y2+8.D0*U3*Y3*V3*X2-
     &       3.D0*U3*Y2*V1*X3+6.D0*V1*X3*V3*X2-12.D0*U3*Y3*V3*X3+
     &       12.D0*V1**2*X3*X2+4.D0*U3*Y3*V1*X3-3.D0*U1*Y2*V3*X3-
     &       3.D0*U3*Y3*V1*X2+4.D0*U2*Y3**2*U3+8.D0*U3*Y2*V3*X3-
     &       12.D0*U1*Y2*V1*X3-8.D0*U3**2*Y3*Y2-4.D0*U1*Y3**2*U3-
     &       2.D0*U2*Y3**2.D0*U1-8.D0*V3**2*X3*X2-4.D0*V1*X3**2*V3+
     &       4.D0*U1*Y3*V3*X3+12.D0*U1**2*Y3*Y2-12.D0*U1*Y3*V1*X2-
     &       3.D0*U1*Y3*V3*X2+6.D0*U1*Y3*U3*Y2-2.D0*V2*X3**2*V1+
     &       4.D0*V2*X3**2*V3-8.D0*U2**2*Y3*Y2+6.D0*U3**2*Y3**2-
     &       6.D0*U1**2*Y3**2+8.D0*U2*Y2*V2*X3+12.D0*U1*Y3*V1*X3-
     &       8.D0*V2**2*X3*X2-2.D0*V1*X2**2*V3-4.D0*U2*Y3*V2*X3+
     &       2.D0*V2*X3*U1*Y3-3.D0*V2*X3*U1*Y2+2.D0*V2**2*X3**2
      A15(IELEM) = (2.D0*U2**2*Y3**2+8.D0*U2*Y3*V2*X2+4.D0*V2*X2*U3*Y3+
     &              4.D0*V2*X2**2*V3-8.D0*V2*X3*V3*X2-3.D0*V2*X2*U1*Y3-
     &              3.D0*U2*Y2*V1*X3+4.D0*V2*X3*U3*Y2+6.D0*V2*X3*V1*X2-
     &              4.D0*V2*X3*U3*Y3-6.D0*V1**2*X3**2+4.D0*U2*Y2**2*U3-
     &              4.D0*V2*X2**2*V1-8.D0*U2*Y3*U3*Y2+2.D0*U2*Y3*V1*X3-
     &              4.D0*U2*Y3*V3*X3+4.D0*U2*Y2*V3*X3-3.D0*U2*Y3*V1*X2+
     &              4.D0*U2*Y3*V3*X2-4.D0*U2*Y2**2*U1-2.D0*U1*Y2**2*U3+
     &              ANS1)* AUX180
!
      ANS1 = 8.D0*U3**2*Y2**2-16.D0*U3*Y2*V3*X2-48.D0*U1*Y2*V1*X2+
     &       8.D0*V3**2*X2**2+24.D0*V1**2*X2**2-6.D0*U2*Y2*V3*X2-
     &       10.D0*U2*Y2*V1*X2+24.D0*U1**2*Y2**2-10.D0*V2*X2*U1*Y2-
     &       6.D0*V2*X2*U3*Y2-8.D0*U3*Y2*V1*X2-8.D0*U1*Y2*V3*X2+
     &       4.D0*V2**2*X2**2+4.D0*U2**2*Y2**2-8.D0*U2*Y2*V2*X2+
     &       6.D0*V3**2*X3**2-8.D0*U2*Y3*U1*Y2+14.D0*U3*Y3*V3*X2+
     &       2.D0*U3*Y2*V1*X3-4.D0*V1*X3*V3*X2-12.D0*U3*Y3*V3*X3-
     &       18.D0*V1**2*X3*X2+4.D0*U3*Y3*V1*X3+2.D0*U1*Y2*V3*X3+
     &       2.D0*U3*Y3*V1*X2+4.D0*U2*Y3**2*U3+14.D0*U3*Y2*V3*X3+
     &       18.D0*U1*Y2*V1*X3-14.D0*U3**2*Y3*Y2-4.D0*U1*Y3**2*U3-
     &       2.D0*U2*Y3**2*U1-14.D0*V3**2*X3*X2-4.D0*V1*X3**2*V3+
     &       4.D0*U1*Y3*V3*X3-18.D0*U1**2*Y3*Y2+18.D0*U1*Y3*V1*X2+
     &       2.D0*U1*Y3*V3*X2-4.D0*U1*Y3*U3*Y2-2.D0*V2*X3**2*V1+
     &       4.D0*V2*X3**2*V3-6.D0*U2**2*Y3*Y2+6.D0*U3**2*Y3**2-
     &       6.D0*U1**2*Y3**2+6.D0*U2*Y2*V2*X3+12.D0*U1*Y3*V1*X3-
     &       6.D0*V2**2*X3*X2+8.D0*V1*X2**2*V3-4.D0*U2*Y3*V2*X3+
     &       2.D0*V2*X3*U1*Y3+4.D0*V2*X3*U1*Y2+2.D0*V2**2*X3**2+
     &       2.D0*U2**2*Y3**2+6.D0*U2*Y3*V2*X2+5.D0*V2*X2*U3*Y3+
     &       6.D0*V2*X2**2*V3-10.D0*V2*X3*V3*X2+4.D0*V2*X2*U1*Y3+
     &       4.D0*U2*Y2*V1*X3+5.D0*V2*X3*U3*Y2-8.D0*V2*X3*V1*X2
      A16(IELEM) = (-4.D0*V2*X3*U3*Y3-6.D0*V1**2*X3**2+6.D0*U2*Y2**2*U3+
     &             10.D0*V2*X2**2*V1-10.D0*U2*Y3*U3*Y2+2.D0*U2*Y3*V1*X3-
     &             4.D0*U2*Y3*V3*X3+5.D0*U2*Y2*V3*X3+4.D0*U2*Y3*V1*X2+
     &             5.D0*U2*Y3*V3*X2+10.D0*U2*Y2**2*U1+8.D0*U1*Y2**2*U3+
     &             ANS1)*(-AUX180)
!
      A22(IELEM)= ((7.D0*(V1*V3+V3**2+V1**2)+39.D0*V2**2+
     &             15.D0*(V2*V3+V2*V1))*X3**2+(-7.D0*U3*V1-
     &             14.D0*(U1*V1+U3*V3)-15.D0*(U2*V1+V2*U3+V2*U1+U2*V3)-
     &             78.D0*U2*V2-7.D0*U1*V3)*Y3*X3+(7.D0*(U1**2+U3**2+
     &             U1*U3)+39.D0*U2**2+15.D0*(U2*U3+U2*U1))*Y3**2)
     &             *AUX360
!
      A23(IELEM)=(((10.D0*(V1*V3+V2*V1)+18.D0*(V3**2+V2**2)+2.D0*(V2*V3+
     &            V1**2))*X3+(-U2*V3-V2*U3-5.D0*(U1*V3+U3*V1+V2*U1+
     &            U2*V1)-18.D0*(U2*V2+U3*V3)-2.D0*U1*V1)*Y3)*X2+(-U2*V3-
     &            V2*U3-5.D0*(U1*V3+U3*V1+U2*V1+V2*U1)-18.D0*(U2*V2+
     &            U3*V3)-2.D0*U1*V1)*Y2*X3+(18.D0*(U2**2+U3**2)+
     &            2.D0*(U2*U3+U1**2)+10.D0*(U2*U1+U1*U3))*Y3*Y2)*AUX720
!
      A24(IELEM) = (-8.D0*U1**2*Y3**2-24.D0*V2**2*X3**2-
     &             8.D0*V1**2*X3**2+2.D0*U1**2*Y3*Y2+2.D0*V1**2*X3*X2-
     &             6.D0*V1*X3**2*V3+30.D0*V2**2*X3*X2+
     &             30.D0*U2**2*Y3*Y2+2.D0*V3**2*X3*X2+2.D0*U3**2*Y3*Y2-
     &             V1*X2*U3*Y3-6.D0*V2*X3*U1*Y2-6.D0*V2*X3*U3*Y2+
     &             12.D0*V2*X3*V1*X2+12.D0*V2*X3*V3*X2-6.D0*V2*X2*U1*Y3-
     &             6.D0*V2*X2*U3*Y3-6.D0*U2*Y2*V1*X3-6.D0*U2*Y2*V3*X3-
     &             V1*X3*U3*Y2+2.D0*V1*X3*V3*X2+2.D0*U1*Y3*U3*Y2-
     &             2.D0*U1*Y3*V1*X2-U1*Y3*V3*X2-2.D0*U1*Y2*V1*X3-
     &             U1*Y2*V3*X3+12.D0*U2*Y3*U1*Y2+12.D0*U2*Y3*U3*Y2-
     &             6.D0*U2*Y3*V1*X2-6.D0*U2*Y3*V3*X2-30.D0*U2*Y3*V2*X2-
     &             30.D0*U2*Y2*V2*X3-2.D0*U3*Y3*V3*X2-2.D0*U3*Y2*V3*X3-
     &             24.D0*U2**2*Y3**2+8.D0*V2*X3*U1*Y3+10.D0*V2*X3*U3*Y3+
     &             6.D0*V1*X3*U3*Y3+16.D0*U1*Y3*V1*X3+6.D0*U1*Y3*V3*X3-
     &             4.D0*U3**2*Y3**2-4.D0*V3**2*X3**2-10.D0*V2*X3**2*V3-
     &             6.D0*U1*Y3**2*U3-8.D0*V2*X3**2*V1-10.D0*U2*Y3**2*U3-
     &             8.D0*U2*Y3**2*U1+10.D0*U2*Y3*V3*X3+48.D0*U2*Y3*V2*X3+
     &             8.D0*U3*Y3*V3*X3+8*U2*Y3*V1*X3)*AUX180
!
      A25(IELEM) = (6.D0*V3**2*X3**2+12.D0*U2*Y3*U1*Y2-2.D0*U3*Y3*V3*X2-
     &             U3*Y2*V1*X3+2.D0*V1*X3*V3*X2-12.D0*U3*Y3*V3*X3+
     &             2.D0*V1**2*X3*X2-4.D0*U3*Y3*V1*X3-U1*Y2*V3*X3-
     &             U3*Y3*V1*X2-4.D0*U2*Y3**2*U3-2.D0*U3*Y2*V3*X3-
     &             2.D0*U1*Y2*V1*X3+2.D0*U3**2*Y3*Y2+4.D0*U1*Y3**2*U3-
     &             2.D0*U2*Y3**2*U1+2.D0*V3**2*X3*X2+4.D0*V1*X3**2*V3-
     &             4.D0*U1*Y3*V3*X3+2.D0*U1**2*Y3*Y2-2.D0*U1*Y3*V1*X2-
     &             U1*Y3*V3*X2+2.D0*U1*Y3*U3*Y2-2.D0*V2*X3**2.D0*V1-
     &             4.D0*V2*X3**2*V3+30.D0*U2**2*Y3*Y2+6.D0*U3**2*Y3**2+
     &             2.D0*U1**2*Y3**2-30.D0*U2*Y2*V2*X3-4.D0*U1*Y3*V1*X3+
     &             30.D0*V2**2*X3*X2+12.D0*U2*Y3*V2*X3+2.D0*V2*X3*U1*Y3-
     &             6.D0*V2*X3*U1*Y2-6.D0*V2**2*X3**2-6.D0*U2**2*Y3**2-
     &             30.D0*U2*Y3*V2*X2-6.D0*V2*X2*U3*Y3+12.D0*V2*X3*V3*X2-
     &             6.D0*V2*X2*U1*Y3-6.D0*U2*Y2*V1*X3-6.D0*V2*X3*U3*Y2+
     &             12.D0*V2*X3*V1*X2+4.D0*V2*X3*U3*Y3+2.D0*V1**2*X3**2+
     &             12.D0*U2*Y3*U3*Y2+2.D0*U2*Y3*V1*X3+4.D0*U2*Y3*V3*X3-
     &             6.D0*U2*Y2*V3*X3-6.D0*U2*Y3*V1*X2-6.D0*U2*Y3*V3*X2)*
     &             (-AUX180)
!
      A26(IELEM) = (2.D0*U1**2*Y3**2-6.D0*V2**2*X3**2+2.D0*V1**2*X3**2+
     &             4.D0*U1**2*Y3*Y2+4.D0*V1**2*X3*X2+4.D0*V1*X3**2*V3-
     &             4.D0*V3**2*X3*X2-4.D0*U3**2*Y3*Y2+V2*X3*U1*Y2-
     &             V2*X3*U3*Y2-2.D0*V2*X3*V1*X2+2.D0*V2*X3*V3*X2+
     &             V2*X2*U1*Y3-V2*X2*U3*Y3+U2*Y2*V1*X3-U2*Y2*V3*X3-
     &             4.D0*U1*Y3*V1*X2-4.D0*U1*Y2*V1*X3-2.D0*U2*Y3*U1*Y2+
     &             2.D0*U2*Y3*U3*Y2+U2*Y3*V1*X2-U2*Y3*V3*X2+
     &             4.D0*U3*Y3*V3*X2+4.D0*U3*Y2*V3*X3-6.D0*U2**2*Y3**2+
     &             2.D0*V2*X3*U1*Y3+4.D0*V2*X3*U3*Y3-4.D0*V1*X3*U3*Y3-
     &             4.D0*U1*Y3*V1*X3-4.D0*U1*Y3*V3*X3+6.D0*U3**2*Y3**2+
     &             6.D0*V3**2*X3**2-4.D0*V2*X3**2*V3+4.D0*U1*Y3**2*U3-
     &             2.D0*V2*X3**2*V1-4.D0*U2*Y3**2*U3-2.D0*U2*Y3**2*U1+
     &             4.D0*U2*Y3*V3*X3+12.D0*U2*Y3*V2*X3-12.D0*U3*Y3*V3*X3+
     &             2.D0*U2*Y3*V1*X3)*AUX180
!
      A33(IELEM) = (15.D0*U2*Y2**2*U3+7.D0*U2*Y2**2*U1+7.D0*U1**2*Y2**2+
     &             7.D0*V1**2*X2**2+7.D0*U2**2*Y2**2+7.D0*V2**2*X2**2+
     &             39.D0*V3**2*X2**2-15.D0*V1*X2*U3*Y2-7.D0*V2*X2*U1*Y2-
     &             15.D0*V2*X2*U3*Y2-7.D0*U2*Y2*V1*X2-15.D0*U2*Y2*V3*X2-
     &             14.D0*U1*Y2*V1*X2-15.D0*U1*Y2*V3*X2-
     &             14.D0*U2*Y2*V2*X2+15.D0*V2*X2**2*V3+
     &             39.D0*U3**2*Y2**2-78.D0*U3*Y2*V3*X2+
     &             7.D0*V2*X2**2*V1+15.D0*V1*X2**2*V3+
     &             15.D0*U1*Y2**2*U3)*AUX360
!
      A34(IELEM) = (-(6.D0*(V3**2-V2**2)+4.D0*(V2*V3-V2*V1)-2.D0*(V1**2-
     &            V1*V3))*X2**2+(-(-2.D0*(V2*V3-V1*V3)+4.D0*(V2**2-
     &            V1**2))*X3-(-4.D0*(U2*V3-V2*U1+V2*U3-U1*V1-U2*V1)+
     &            12.D0*(U2*V2-U3*V3)-2.D0*(U1*V3+U3*V1))*Y2-
     &            (4.D0*(U1*V1-U2*V2)-U3*V1+V2*U3-U1*V3+U2*V3)*Y3)*X2-
     &            (4.D0*(U1*V1-U2*V2)-U3*V1+V2*U3-U1*V3+
     &            U2*V3)*Y2*X3-(-2.D0*(U1**2-U1*U3)-6.D0*(U2**2-U3**2)-
     &            4.D0*(U2*U1-U2*U3))*Y2**2-(4.D0*(U2**2-U1**2)+
     &            2.D0*(U1*U3-U2*U3))*Y3*Y2)*AUX180
!
      A35(IELEM) = -(6.D0*U2**2*Y2**2+2.D0*U1**2*Y2**2+2.D0*V1**2*X2**2+
     &             6.D0*V2**2*X2**2-6.D0*V3**2*X2**2-4.D0*U2*Y2**2*U3+
     &             2.D0*U1**2*Y3*Y2+2.D0*V1**2*X3*X2+4.D0*U2*Y2**2*U1+
     &             2.D0*V2**2*X3*X2+2.D0*U2**2*Y3*Y2+30.D0*V3**2*X3*X2+
     &             30.D0*U3**2*Y3*Y2-6.D0*V1*X2*U3*Y3+2.D0*V1*X2*U3*Y2-
     &             V2*X3*U1*Y2-6.D0*V2*X3*U3*Y2+2.D0*V2*X3*V1*X2+
     &             12.D0*V2*X3*V3*X2-V2*X2*U1*Y3-4.D0*V2*X2*U1*Y2-
     &             6.D0*V2*X2*U3*Y3+4.D0*V2*X2*U3*Y2-U2*Y2*V1*X3-
     &             4.D0*U2*Y2*V1*X2-6.D0*U2*Y2*V3*X3+4.D0*U2*Y2*V3*X2-
     &             6.D0*V1*X3*U3*Y2+12.D0*V1*X3*V3*X2+12.D0*U1*Y3*U3*Y2-
     &             2.D0*U1*Y3*V1*X2-6.D0*U1*Y3*V3*X2-2.D0*U1*Y2*V1*X3-
     &             4.D0*U1*Y2*V1*X2-6.D0*U1*Y2*V3*X3+2.D0*U1*Y2*V3*X2+
     &             2.D0*U2*Y3*U1*Y2+12.D0*U2*Y3*U3*Y2-U2*Y3*V1*X2-
     &             6.D0*U2*Y3*V3*X2-2.D0*U2*Y3*V2*X2-2.D0*U2*Y2*V2*X3-
     &             12.D0*U2*Y2*V2*X2-30.D0*U3*Y3*V3*X2-
     &             30.D0*U3*Y2*V3*X3+12.D0*U3*Y2*V3*X2-6.D0*U3**2*Y2**2-
     &             4.D0*V2*X2**2*V3+4.D0*V2*X2**2*V1-2.D0*V1*X2**2*V3-
     &             2.D0*U1*Y2**2*U3)*AUX180
!
      ANS1 = U3**2*Y2**2-2.D0*U3*Y2*V3*X2-2.D0*U1*Y2*V1*X2+V3**2*X2**2+
     &       V1**2*X2**2-3.D0*U2*Y2*V3*X2-3.D0*U2*Y2*V1*X2+U1**2*Y2**2-
     &       3.D0*V2*X2*U1*Y2-3.D0*V2*X2*U3*Y2-U3*Y2*V1*X2-U1*Y2*V3*X2+
     &       6.D0*V2**2*X2**2+6.D0*U2**2*Y2**2-12.D0*U2*Y2*V2*X2+
     &       V3**2*X3**2-2.D0*U2*Y3*U1*Y2+U3*Y3*V3*X2-2.D0*U3*Y3*V3*X3+
     &       V1**2*X3*X2-2.D0*U3*Y3*V1*X3+2.D0*U2*Y3**2*U3+U3*Y2*V3*X3-
     &       U1*Y2*V1*X3-U3**2*Y3*Y2+2.D0*U1*Y3**2*U3+2.D0*U2*Y3**2*U1-
     &       V3**2*X3*X2+2.D0*V1*X3**2*V3-2.D0*U1*Y3*V3*X3+U1**2*Y3*Y2-
     &       U1*Y3*V1*X2+2.D0*V2*X3**2*V1+2.D0*V2*X3**2*V3-
     &       9.D0*U2**2*Y3*Y2+U3**2*Y3**2+4.D0*U1**2*Y3**2+
     &       9.D0*U2*Y2*V2*X3-8.D0*U1*Y3*V1*X3-9.D0*V2**2*X3*X2+
     &       V1*X2**2*V3-8.D0*U2*Y3*V2*X3-2.D0*V2*X3*U1*Y3+
     &       V2*X3*U1*Y2+4.D0*V2**2*X3**2+4.D0*U2**2*Y3**2+
     &       9.D0*U2*Y3*V2*X2+2.D0*V2*X2*U3*Y3+3.D0*V2*X2**2*V3-
     &       4.D0*V2*X3*V3*X2+V2*X2*U1*Y3+U2*Y2*V1*X3+2.D0*V2*X3*U3*Y2-
     &       2.D0*V2*X3*V1*X2-2.D0*V2*X3*U3*Y3+4.D0*V1**2*X3**2+
     &       3.D0*U2*Y2**2.D0*U3+3.D0*V2*X2**2*V1-4.D0*U2*Y3*U3*Y2-
     &       2.D0*U2*Y3*V1*X3-2.D0*U2*Y3*V3*X3+2.D0*U2*Y2*V3*X3+
     &       U2*Y3*V1*X2+2.D0*U2*Y3*V3*X2+3.D0*U2*Y2**2*U1+U1*Y2**2*U3
      A44(IELEM) = ANS1*AUX45*2.D0
!
      ANS1 = 2.D0*U3**2*Y2**2-4.D0*U3*Y2*V3*X2-4.D0*U1*Y2*V1*X2+
     &       2.D0*V3**2*X2**2+2.D0*V1**2*X2**2-6.D0*U2*Y2*V3*X2-
     &       6.D0*U2*Y2*V1*X2+2.D0*U1**2*Y2**2-6.D0*V2*X2*U1*Y2-
     &       6.D0*V2*X2*U3*Y2-2.D0*U3*Y2*V1*X2-2.D0*U1*Y2*V3*X2+
     &       12.D0*V2**2*X2**2+12.D0*U2**2*Y2**2-24.D0*U2*Y2*V2*X2-
     &       4.D0*U2*Y3*U1*Y2+4.D0*U3*Y3*V3*X2+U3*Y2*V1*X3-
     &       2.D0*V1*X3*V3*X2+2.D0*U3*Y3*V1*X3+U1*Y2*V3*X3+U3*Y3*V1*X2+
     &       2.D0*U2*Y3**2*U3+4.D0*U3*Y2*V3*X3-4.D0*U3**2*Y3*Y2-
     &       2.D0*U1*Y3**2*U3-4.D0*V3**2*X3*X2-2.D0*V1*X3**2*V3+
     &       2.D0*U1*Y3*V3*X3+U1*Y3*V3*X2-2.D0*U1*Y3*U3*Y2+
     &       2.D0*V2*X3**2*V3-12.D0*U2**2*Y3*Y2-2.D0*U1**2*Y3**2+
     &       12.D0*U2*Y2*V2*X3+4.D0*U1*Y3*V1*X3-12.D0*V2**2*X3*X2+
     &       2.D0*V1*X2**2*V3-4.D0*U2*Y3*V2*X3+2.D0*V2*X3*U1*Y2+
     &       2.D0*V2**2*X3**2+2.D0*U2**2*Y3**2+12.D0*U2*Y3*V2*X2+
     &       4.D0*V2*X2*U3*Y3+6.D0*V2*X2**2*V3-8.D0*V2*X3*V3*X2+
     &       2.D0*V2*X2*U1*Y3+2.D0*U2*Y2*V1*X3+4.D0*V2*X3*U3*Y2-
     &       4.D0*V2*X3*V1*X2-2.D0*V2*X3*U3*Y3-2.D0*V1**2*X3**2+
     &       6.D0*U2*Y2**2*U3+6.D0*V2*X2**2*V1-8.D0*U2*Y3*U3*Y2-
     &       2.D0*U2*Y3*V3*X3+4.D0*U2*Y2*V3*X3+2.D0*U2*Y3*V1*X2+
     &       4.D0*U2*Y3*V3*X2+6.D0*U2*Y2**2*U1+2.D0*U1*Y2**2*U3
      A45(IELEM) = ANS1*(-AUX45)
!
      ANS1 = U3**2*Y2**2-2.D0*U3*Y2*V3*X2-2.D0*U1*Y2*V1*X2+V3**2*X2**2+
     &       V1**2*X2**2-3.D0*U2*Y2*V3*X2-3.D0*U2*Y2*V1*X2+U1**2*Y2**2-
     &       3.D0*V2*X2*U1*Y2-3.D0*V2*X2*U3*Y2-U3*Y2*V1*X2-U1*Y2*V3*X2+
     &       6.D0*V2**2*X2**2+6.D0*U2**2*Y2**2-12.D0*U2*Y2*V2*X2+
     &       6.D0*V3**2*X3**2-2.D0*U2*Y3*U1*Y2+3.D0*U3*Y3*V3*X2+
     &       U3*Y2*V1*X3-2.D0*V1*X3*V3*X2-12.D0*U3*Y3*V3*X3-V1**2*X3*X2-
     &       3.D0*U3*Y3*V1*X3+U1*Y2*V3*X3+U3*Y3*V1*X2+3.D0*U2*Y3**2*U3+
     &       3.D0*U3*Y2*V3*X3+U1*Y2*V1*X3-3.D0*U3**2*Y3*Y2+
     &       3.D0*U1*Y3**2*U3+U2*Y3**2*U1-3.D0*V3**2*X3*X2+
     &       3.D0*V1*X3**2*V3-3.D0*U1*Y3*V3*X3-U1**2*Y3*Y2+U1*Y3*V1*X2+
     &       U1*Y3*V3*X2-2.D0*U1*Y3*U3*Y2+V2*X3**2*V1+3.D0*V2*X3**2*V3-
     &       3.D0*U2**2*Y3*Y2+6.D0*U3**2*Y3**2+U1**2*Y3**2+
     &       3.D0*U2*Y2*V2*X3-2.D0*U1*Y3*V1*X3-3.D0*V2**2*X3*X2+
     &       V1*X2**2*V3-2.D0*U2*Y3*V2*X3-V2*X3*U1*Y3+V2*X3*U1*Y2+
     &       V2**2*X3**2+U2**2*Y3**2+3.D0*U2*Y3*V2*X2+2.D0*V2*X2*U3*Y3+
     &       3.D0*V2*X2**2*V3-4.D0*V2*X3*V3*X2+V2*X2*U1*Y3+U2*Y2*V1*X3+
     &       2.D0*V2*X3*U3*Y2-2.D0*V2*X3*V1*X2-3.D0*V2*X3*U3*Y3+
     &       V1**2*X3**2+3.D0*U2*Y2**2*U3+3.D0*V2*X2**2*V1-
     &       4.D0*U2*Y3*U3*Y2-U2*Y3*V1*X3-3.D0*U2*Y3*V3*X3+
     &       2.D0*U2*Y2*V3*X3+U2*Y3*V1*X2+2.D0*U2*Y3*V3*X2
      A55(IELEM) = (3.D0*U2*Y2**2*U1+U1*Y2**2*U3+ANS1)*2.D0*AUX45
!
!
! USES HERE THE 'MAGIC SQUARE' PROPERTIES
! (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
!
          A16(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
     &                 - A14(IELEM) - A15(IELEM)
!
          A26(IELEM) = - A12(IELEM) - A22(IELEM) - A23(IELEM)
     &                 - A24(IELEM) - A25(IELEM)
!
          A36(IELEM) = - A13(IELEM) - A23(IELEM) - A33(IELEM)
     &                 - A34(IELEM) - A35(IELEM)
!
          A46(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
     &                 - A44(IELEM) - A45(IELEM)
!
          A56(IELEM) = - A15(IELEM) - A25(IELEM) - A35(IELEM)
     &                 - A45(IELEM) - A55(IELEM)
!
          A66(IELEM) = - A16(IELEM) - A26(IELEM) - A36(IELEM)
     &                 - A46(IELEM) - A56(IELEM)
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMU.EQ.13.AND.IELMV.EQ.13) THEN
!
!-----------------------------------------------------------------------
!
!  P2 DISCRETISATION OF THE VELOCITY:
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
        U4 = U(IKLE4(IELEM))
        U5 = U(IKLE5(IELEM))
        U6 = U(IKLE6(IELEM))
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
        V4 = V(IKLE4(IELEM))
        V5 = V(IKLE5(IELEM))
        V6 = V(IKLE6(IELEM))
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
        UNSU2 = XMUL/(X2*Y3-Y2*X3)
        AUX630  = UNSU2/630.D0
        AUX1260 = UNSU2/1260.D0
        AUX2520 = UNSU2/2520.D0
!
!  COMPUTES 15 OF THE 36 TERMS (SELECTED AMONG THE LEAST COMPLEX)
!
      ANS1 = 112.D0*V4**2*X2**2-96.D0*U5*Y2*V5*X2+23.D0*V1*X2*U3*Y2+
     &       224.D0*U4*Y2*V4*X3+48.D0*U5*Y3*V4*X2+48.D0*V5*X2*U4*Y3+
     &       48.D0*U5**2*Y2**2-224.D0*U4*Y2*V4*X2-96.D0*U1*Y3*V6*X3+
     &       96.D0*U5*Y3*V5*X2+48.D0*V5*X3*U4*Y2-48.D0*U5*Y3*V4*X3+
     &       13.D0*V2**2*X3**2+48.D0*V5**2*X3**2+96.D0*U5*Y2*V5*X3-
     &       48.D0*V5*X3*U4*Y3+48.D0*U5*Y2*V4*X3-96.D0*V5*X3*V4*X2+
     &       224.D0*V4*X2*U4*Y3-224.D0*V4*X3*U4*Y3+13.D0*U2**2*Y3**2-
     &       32.D0*V2*X2*U4*Y3+112.D0*U4**2*Y3**2-192.D0*V1*X3*V6*X2-
     &       186.D0*V1*X3*U1*Y3+23.D0*U1*Y3*V3*X3-28.D0*U2*Y2**2*U6+
     &       13.D0*V2**2*X2**2-96.D0*U4*Y2*V1*X2-112.D0*U4*Y2*V6*X2+
     &       96.D0*U1*Y3*V6*X2-112.D0*V4*X3*U6*Y3+112.D0*V4**2*X3**2-
     &       112.D0*U4*Y3*V6*X3-96.D0*V4*X2*U1*Y2-112.D0*V4*X2*U6*Y2+
     &       112.D0*U6**2*Y3**2-32.D0*U2*Y2*V4*X3+32.D0*U2*Y2*V4*X2+
     &       28.D0*U2*Y3*V6*X3+13.D0*U2**2*Y2**2+13.D0*U3**2*Y3**2-
     &       96.D0*V5*X3*U5*Y3+48.D0*U5*Y3**2*U6-96.D0*U5*Y2*U4*Y3+
     &       93.D0*U1**2*Y3**2+93.D0*V1**2*X3**2+13.D0*V3**2*X3**2+
     &       112.D0*V6**2*X3**2+48.D0*U5*Y2**2*U6-224.D0*V6**2*X3*X2-
     &       26.D0*V3**2*X3*X2-26.D0*V2**2*X2*X3-224.D0*U6**2*Y2*Y3+
     &       48.D0*U5**2*Y3**2+48.D0*V5*X2**2*V4-32.D0*V2*X2**2*V4+
     &       4.D0*U5*Y2**2*U1+32.D0*V2*X2*U4*Y2+112.D0*U6**2*Y2**2
      ANS2 = -32.D0*V2*X3**2*V4-28.D0*V2*X2**2*V6-
     &       32.D0*U2*Y2**2*U4+V2*X2**2*V3-28.D0*V4*X2**2*V3-
     &       26.D0*V2*X2*U2*Y2-32.D0*U3*Y3**2*U6+93.D0*V1**2*X2**2-
     &       186.D0*U1**2*Y3*Y2+96.D0*U1*Y2**2*U6+112.D0*V6**2*X2**2+
     &       4.D0*U5*Y3**2*U1+V2*X3**2*V3-26.D0*U2**2*Y2*Y3+
     &       96.D0*V4*X2**2*V1+4.D0*V5*X2**2*V1+96.D0*V4*X3**2*V1+
     &       96.D0*V1*X2**2*V6-U2*Y2*V3*X2+48.D0*V5**2*X2**2-
     &       23.D0*V1*X3**2*V3-V2*X3*U3*Y3+13.D0*V3**2*X2**2-
     &       28.D0*U4*Y2**2*U3-U2*Y3*V3*X3-28.D0*U2*Y3**2*U6-
     &       23.D0*U2*Y3**2*U1-23.D0*U1*Y3**2*U3+112.D0*U4*Y3**2*U6-
     &       186.D0*V1*X2*U1*Y2-96.D0*V1*X3*U6*Y3-224.D0*U4**2*Y2*Y3-
     &       224.D0*V4**2*X3*X2-23.D0*U1*Y2**2*U3-32.D0*V6*X3**2*V3-
     &       48.D0*V5*X2*U4*Y2-48.D0*U5*Y2*V4*X2-26.D0*U3**2*Y3*Y2-
     &       186.D0*V1**2*X3*X2-23.D0*V2*X3**2*V1-V2*X2*U3*Y2+
     &       96.D0*U4*Y2**2*U1-23.D0*V1*X2**2*V3+48.D0*V5*X2**2*V6+
     &       U2*Y2**2*U3-28.D0*U4*Y3**2*U3-32.D0*U2*Y3**2*U4+
     &       4.D0*V5*X3**2*V1-96.D0*U5**2*Y2*Y3-96.D0*V5**2*X3*X2+
     &       48.D0*V5*X3**2*V4+48.D0*U5*Y3**2*U4+13.D0*U3**2*Y2**2+
     &       224.D0*V6*X2*U6*Y3-224.D0*V6*X2*U6*Y2+32.D0*V6*X2*U3*Y2-
     &       32.D0*V3*X2**2*V6-32.D0*U2*Y3*V4*X2+32.D0*U2*Y3*V4*X3
      ANS3 = 48.D0*V5*X3**2*V6-26.D0*V3*X2*U3*Y2+32.D0*V3*X2*U6*Y2-
     &       32.D0*V2*X3*U4*Y2+93.D0*U1**2*Y2**2+23.D0*U1*Y2*V3*X2+
     &       32.D0*V2*X3*U4*Y3+224.D0*V6*X3*U6*Y2-96.D0*U1*Y2*V6*X2+
     &       28.D0*V4*X2*U3*Y2+26.D0*V2*X2*U2*Y3+23.D0*V1*X3*U3*Y3+
     &       26.D0*U2*Y2*V2*X3-26.D0*U2*Y3*V2*X3+112.D0*V4*X3**2*V6+
     &       4.D0*U5*Y3*V1*X2-4.D0*U5*Y3*V1*X3+U2*Y3**2*U3-
     &       192.D0*U1*Y3*U6*Y2+112.D0*U4*Y2**2*U6+96.D0*V1*X3*U6*Y2+
     &       112.D0*U4**2*Y2**2-96.D0*U5*Y2*U6*Y3+46.D0*U1*Y3*U3*Y2+
     &       96.D0*V1*X3**2*V6+28.D0*U4*Y2*V3*X2+48.D0*V5*X3*U6*Y2-
     &       23.D0*U1*Y3*V3*X2-4.D0*V5*X3*U1*Y3-8.D0*V5*X3*V1*X2-
     &       48.D0*V5*X2*U6*Y2+96.D0*U1*Y3**2*U6-4.D0*V5*X2*U1*Y2-
     &       8.D0*U5*Y2*U1*Y3+48.D0*U5*Y2**2*U4+26.D0*V3*X3*U3*Y2+
     &       26.D0*V3*X2*U3*Y3-32.D0*V3*X2*U6*Y3+64.D0*U3*Y3*U6*Y2-
     &       96.D0*V5*X3*V6*X2-32.D0*U3*Y3*V6*X2+186.D0*V1*X2*U1*Y3+
     &       96.D0*V1*X2*U6*Y3+4.D0*V5*X3*U1*Y2+46.D0*V1*X3*V3*X2+
     &       96.D0*U4*Y3**2*U1+64.D0*V6*X3*V3*X2-26.D0*V3*X3*U3*Y3-
     &       224.D0*V6*X3*U6*Y3+4.D0*U5*Y2*V1*X3+112.D0*V4*X2**2*V6+
     &       32.D0*V3*X3*U6*Y3-32.D0*V3*X3*U6*Y2+48.D0*U5*Y2*V6*X3+
     &       46.D0*U2*Y3*U1*Y2+112.D0*U4*Y3*V6*X2+U2*Y3*V3*X2-
     &       28.D0*U4*Y3*V3*X2+96.D0*U4*Y3*V1*X2-96.D0*U4*Y3*V1*X3
      ANS4 = -23.D0*V1*X3*U3*Y2-23.D0*U1*Y2*V3*X3-23.D0*U2*Y3*V1*X2+
     &       96.D0*U1*Y2*V6*X3-32.D0*V6*X3*U3*Y2+32.D0*V6*X3*U3*Y3-
     &       28.D0*V2*X3*U6*Y2-192.D0*V4*X3*V1*X2-28.D0*V4*X2*U3*Y3+
     &       V2*X3*U3*Y2+96.D0*V4*X2*U1*Y3+56.D0*V2*X3*V6*X2-
     &       48.D0*U5*Y2*V6*X2+23.D0*U2*Y3*V1*X3+56.D0*U2*Y3*U6*Y2+
     &       112.D0*V4*X2*U6*Y3+28.D0*U4*Y3*V3*X3-2.D0*U2*Y3*U3*Y2-
     &       28.D0*U2*Y3*V6*X2-23.D0*V2*X3*U1*Y2-96.D0*V1*X2*U6*Y2+
     &       112.D0*V4*X3*U6*Y2-28.D0*V4*X3*U3*Y2-2.D0*V2*X3*V3*X2-
     &       224.D0*V4*X3*V6*X2+96.D0*V4*X3*U1*Y2+28.D0*V4*X3*U3*Y3+
     &       46.D0*V2*X3*V1*X2-28.D0*V2*X3**2*V6-4.D0*U5*Y2*V1*X2+
     &       23.D0*V2*X3*U1*Y3+28.D0*V2*X3*U6*Y3-28.D0*V2*X2*U6*Y3+
     &       112.D0*U4*Y2*V6*X3+V2*X2*U3*Y3-32.D0*U3*Y2**2*U6+
     &       48.D0*V5*X2*U6*Y3-23.D0*V1*X2*U3*Y3+56.D0*V4*X3*V3*X2-
     &       28.D0*V4*X3**2*V3-96.D0*V4*X3*U1*Y3-224.D0*U4*Y2*U6*Y3+
     &       23.D0*U2*Y2*V1*X2-23.D0*V2*X2*U1*Y3+56.D0*U4*Y2*U3*Y3+
     &       96.D0*U4*Y2*V1*X3-28.D0*U4*Y2*V3*X3+U2*Y2*V3*X3-
     &       48.D0*U5*Y3*V6*X3-48.D0*V5*X3*U6*Y3+4.D0*V5*X2*U1*Y3-
     &       28.D0*U2*Y2*V6*X3+28.D0*U2*Y2*V6*X2+23.D0*V2*X2*U1*Y2-
     &       192.D0*U4*Y2*U1*Y3+48.D0*U5*Y3*V6*X2+28.D0*V2*X2*U6*Y2-
     &       23.D0*U2*Y2*V1*X3-23.D0*V2*X2**2*V1+186.D0*V1*X3*U1*Y2
      A11(IELEM) = (64.D0*U2*Y3*U4*Y2+64.D0*V2*X3*V4*X2+
     &              ANS1+ANS2+ANS3+ANS4-23.D0*U2*Y2**2*U1)*AUX1260
!
      ANS1 =(8.D0*U2*U6+8.D0*U5*U1-32.D0*U5*U6+8.D0*U4*U3-32.D0*U4*U6+
     &      10.D0*U3*U1-32.D0*U4*U1+32.D0*U6*U3-54.D0*U2**2-
     &      64.D0*U5*U2+32.D0*U3*U5-96.D0*U5**2+18.D0*U2*U1+
     &      10.D0*U3**2+10.D0*U2*U3-64.D0*U6*U1-32.D0*U4*U5-
     &      32.D0*U2*U4-54.D0*U1**2+32.D0*U4**2-96.D0*U6**2)*Y3*Y2+
     &      (-32.D0*U4**2-32.D0*U3*U5+32.D0*U5*U6-8.D0*U2*U6+
     &      32.D0*U2*U4+54.D0*U2**2-18.D0*U2*U1+32.D0*U4*U6-
     &      8.D0*U5*U1+32.D0*U4*U1-32.D0*U6*U3-10.D0*U2*U3+64.D0*U5*U2-
     &      10.D0*U3**2+32.D0*U4*U5+96.D0*U5**2+96.D0*U6**2-8.D0*U4*U3-
     &      10.D0*U3*U1+64.D0*U6*U1+54.D0*U1**2)*Y3**2+((-96.D0*V5**2+
     &      10.D0*V2*V3+8.D0*V4*V3+8.D0*V2*V6-64.D0*V5*V2-54.D0*V1**2+
     &      32.D0*V3*V5+32.D0*V4**2+32.D0*V3*V6-96.D0*V6**2+
     &      10.D0*V3**2-32.D0*V5*V6-32.D0*V4*V6-32.D0*V4*V5-
     &      32.D0*V4*V1-64.D0*V1*V6+8.D0*V5*V1+18.D0*V2*V1-
     &      32.D0*V2*V4+10.D0*V1*V3-54.D0*V2**2)*X3+(96.D0*U5*V5+
     &      54.D0*V1*U1-5.D0*U2*V3+16.D0*U4*V1-16.D0*U6*V3-4.D0*V2*U6-
     &      4.D0*V4*U3-5.D0*V1*U3-5.D0*U1*V3+54.D0*U2*V2+16.D0*V4*U5-
     &      4.D0*U5*V1+16.D0*U4*V5-4.D0*U2*V6-4.D0*V5*U1-4.D0*U4*V3+
     &      96.D0*U6*V6+16.D0*V4*U6+16.D0*U4*V6+16.D0*U5*V6)*Y3)*X2
      ANS2 =(16.D0*V5*U6+32.D0*U5*V2-16.D0*U5*V3-5.D0*V2*U3+
     &      16.D0*U2*V4+32.D0*U1*V6-9.D0*V2*U1-16.D0*U3*V6-9.D0*U2*V1+
     &      16.D0*V2*U4+16.D0*V4*U1-10.D0*U3*V3+32.D0*U2*V5-16.D0*U3*V5-
     &      32.D0*U4*V4+32.D0*V1*U6)*Y3*X2+X3*Y2*(96.D0*U5*V5+
     &      54.D0*V1*U1-5.D0*U2*V3+16.D0*U4*V1-16.D0*U6*V3-4.D0*V2*U6-
     &      4.D0*V4*U3-5.D0*V1*U3-5.D0*U1*V3+54.D0*U2*V2+16.D0*V4*U5-
     &      4.D0*U5*V1+16.D0*U4*V5-4.D0*U2*V6-4.D0*V5*U1-4.D0*U4*V3+
     &      96.D0*U6*V6+16.D0*V4*U6+16.D0*U4*V6+16.D0*U5*V6+
     &      16.D0*V5*U6+32.D0*U5*V2-16.D0*U5*V3-5.D0*V2*U3+16.D0*U2*V4+
     &      32.D0*U1*V6-9.D0*V2*U1-16.D0*U3*V6-9.D0*U2*V1+16.D0*V2*U4+
     &      16.D0*V4*U1-10.D0*U3*V3+32.D0*U2*V5-16.D0*U3*V5-
     &      32.D0*U4*V4+32.D0*V1*U6)+(32.D0*V4*V5-18.D0*V2*V1+
     &      32.D0*V4*V1-10.D0*V1*V3+32.D0*V5*V6- 8.D0*V5*V1+
     &      32.D0*V2*V4-10.D0*V2*V3-8.D0*V4*V3+32.D0*V4*V6+
     &      54.D0*V2**2-8.D0*V2*V6+96.D0*V5**2+54.D0*V1**2-32.D0*V3*V6-
     &      32.D0*V4**2+96.D0*V6**2+64.D0*V1*V6-10.D0*V3**2+
     &      64.D0*V5*V2-32.D0*V3*V5)*X3**2+(-192.D0*U5*V5-
     &      108.D0*V1*U1+10.D0*U2*V3-32.D0*U4*V1+32.D0*U6*V3+
     &      8.D0*V2*U6+8.D0*V4*U3+10.D0*V1*U3+10.D0*U1*V3-
     &      108.D0*U2*V2-32.D0*V4*U5+8.D0*U5*V1-32.D0 *U4*V5)*Y3*X3
      A12(IELEM) = ((8.D0*U2*V6+8.D0*V5*U1+8.D0*U4*V3-192.D0*U6*V6-
     &             32.D0*V4*U6-32.D0*U4*V6-32.D0*U5*V6-32.D0*V5*U6-
     &             64.D0*U5*V2+32.D0*U5*V3+10.D0*V2*U3-32.D0*U2*V4-
     &             64.D0*U1*V6+18.D0*V2*U1+32.D0*U3*V6+18.D0*U2*V1-
     &             32.D0*V2*U4-32.D0*V4*U1+20.D0*U3*V3-64.D0*U2*V5+
     &             32.D0*U3*V5+64.D0*U4*V4-64.D0*V1*U6)*Y3*X3+
     &             ANS1+ANS2)*AUX2520
!
      ANS1 = -96.D0*V4**2*X2**2+192.D0*U5*Y2*V5*X2-18.D0*V1*X2*U3*Y2-
     &       96.D0*U4*Y2*V4*X3-16.D0*U5*Y3*V4*X2-16.D0*V5*X2*U4*Y3-
     &       96.D0*U5**2*Y2**2+192.D0*U4*Y2*V4*X2-96.D0*U5*Y3*V5*X2-
     &       16.D0*V5*X3*U4*Y2-96.D0*U5*Y2*V5*X3-16.D0*U5*Y2*V4*X3+
     &       32.D0*V5*X3*V4*X2-96.D0*V4*X2*U4*Y3+16.D0*V2*X2*U4*Y3+
     &       32.D0*V1*X3*V6*X2+8.D0*U2*Y2**2*U6+10.D0*V2**2*X2**2+
     &       16.D0*U2*Y2*V5*X3+16.D0*U2*Y3*V5*X2+64.D0*U4*Y2*V1*X2+
     &       32.D0*U4*Y2*V6*X2-16.D0*U1*Y3*V6*X2-32.D0*U2*Y3*U5*Y2+
     &       64.D0*V4*X2*U1*Y2+32.D0*V4*X2*U6*Y2+16.D0*U2*Y2*V4*X3-
     &       32.D0*U2*Y2*V4*X2+10.D0*U2**2*Y2**2+32.D0*U5*Y2*U4*Y3+
     &       32.D0*V5*X2**2*V2-32.D0*U5*Y2**2*U6-32.D0*V6**2*X3*X2+
     &       54.D0*V3**2*X3*X2-10.D0*V2**2*X2*X3-32.D0*U6**2*Y2*Y3-
     &       32.D0*V5*X2**2*V4+32.D0*V2*X2**2*V4+8.D0*U5*Y2**2*U1-
     &       32.D0*V2*X2*U4*Y2+32.D0*U6**2*Y2**2+8.D0*V2*X2**2*V6+
     &       32.D0*U2*Y2**2*U4+10.D0*V2*X2**2*V3+8.D0*V4*X2**2*V3-
     &       20.D0*V2*X2*U2*Y2-54.D0*V1**2*X2**2+54.D0*U1**2*Y3*Y2-
     &       32.D0*U1*Y2**2*U6+32.D0*V6**2*X2**2-10.D0*U2**2*Y2*Y3-
     &       64.D0*V4*X2**2*V1+8.D0*V5*X2**2*V1-32.D0*V5*X2*U2*Y2-
     &       32.D0*U5*Y2*V2*X2-64.D0*V3*X2**2*V5-32.D0*V1*X2**2*V6
      ANS2 = -10.D0*U2*Y2*V3*X2-96.D0*V5**2*X2**2-54.D0*V3**2*X2**2+
     &        8.D0*U4*Y2**2*U3+64.D0*V3*X2*U5*Y2-64.D0*U3*Y2**2*U5+
     &        108.D0*V1*X2*U1*Y2+96.D0*U4**2*Y2*Y3+96.D0*V4**2*X3*X2+
     &        18.D0*U1*Y2**2*U3+64.D0*U3*Y2*V5*X2+32.D0*V5*X2*U4*Y2+
     &        32.D0*U5*Y2*V4*X2+54.D0*U3**2*Y3*Y2+32.D0*U5*Y2**2*U2+
     &        54.D0*V1**2*X3*X2-10.D0*V2*X2*U3*Y2-64.D0*U4*Y2**2*U1+
     &        18.D0*V1*X2**2*V3-32.D0*V5*X2**2*V6+10.D0*U2*Y2**2*U3+
     &        96.D0*U5**2*Y2*Y3+96.D0*V5**2*X3*X2-54.D0*U3**2*Y2**2+
     &        32.D0*V6*X2*U6*Y3-64.D0*V6*X2*U6*Y2+32.D0*V6*X2*U3*Y2+
     &        16.D0*V2*X3*U5*Y2-32.D0*V3*X2**2*V6+16.D0*U2*Y3*V4*X2-
     &        32.D0*U2*Y3*U4*Y2-32.D0*V2*X3*V4*X2+108.D0*V3*X2*U3*Y2+
     &        32.D0*V3*X2*U6*Y2+16.D0*V2*X3*U4*Y2-32.D0*V2*X3*V5*X2-
     &        54.D0*U1**2*Y2**2-18.D0*U1*Y2*V3*X2+32.D0*V6*X3*U6*Y2+
     &        32.D0*U1*Y2*V6*X2-8.D0*V4*X2*U3*Y2+10.D0*V2*X2*U2*Y3+
     &        10.D0*U2*Y2*V2*X3+16.D0*V2*X2*U5*Y3-32.D0*V5*X3*U3*Y2+
     &        4.D0*U5*Y3*V1*X2+32.D0*U1*Y3*U6*Y2-32.D0*U4*Y2**2*U6-
     &        16.D0*V1*X3*U6*Y2-96.D0*U4**2*Y2**2+32.D0*U5*Y2*U6*Y3-
     &        18.D0*U1*Y3*U3*Y2+64.D0*U5*Y2*U3*Y3-8.D0*U4*Y2*V3*X2-
     &        16.D0*V5*X3*U6*Y2+9.D0*U1*Y3*V3*X2+64.D0*V5*X3*V3*X2-
     &        8.D0*V5*X3*V1*X2+32.D0*V5*X2*U6*Y2-8.D0*V5*X2*U1*Y2
      ANS3 = -8.D0*U5*Y2*U1*Y3-32.D0*U5*Y2**2*U4-54.D0*V3*X3*U3*Y2-
     &       54.D0*V3*X2*U3*Y3-16.D0*V3*X2*U6*Y3+32.D0*U3*Y3*U6*Y2+
     &       32.D0*V5*X3*V6*X2-16.D0*U3*Y3*V6*X2-54.D0*V1*X2*U1*Y3-
     &       16.D0*V1*X2*U6*Y3+4.D0*V5*X3*U1*Y2-18.D0*V1*X3*V3*X2+
     &       32.D0*V6*X3*V3*X2+4.D0*U5*Y2*V1*X3-32.D0*U5*Y2*V3*X3-
     &       32.D0*V4*X2**2*V6-16.D0*V3*X3*U6*Y2-16.D0*U5*Y2*V6*X3-
     &       10.D0*U2*Y3*U1*Y2-16.D0*U4*Y3*V6*X2+5.D0*U2*Y3*V3*X2+
     &       4.D0*U4*Y3*V3*X2-32.D0*U4*Y3*V1*X2+9.D0*V1*X3*U3*Y2+
     &       9.D0*U1*Y2*V3*X3+5.D0*U2*Y3*V1*X2-16.D0*U1*Y2*V6*X3-
     &       16.D0*V6*X3*U3*Y2+4.D0*V2*X3*U6*Y2+64.D0*V4*X3*V1*X2+
     &       4.D0*V4*X2*U3*Y3+5.D0*V2*X3*U3*Y2-32.D0*V4*X2*U1*Y3-
     &       8.D0*V2*X3*V6*X2+32.D0*U5*Y2*V6*X2-8.D0*U2*Y3*U6*Y2-
     &       16.D0*V4*X2*U6*Y3-10.D0*U2*Y3*U3*Y2+4.D0*U2*Y3*V6*X2+
     &       5.D0*V2*X3*U1*Y2+32.D0*V1*X2*U6*Y2-16.D0*V4*X3*U6*Y2+
     &       4.D0*V4*X3*U3*Y2-10.D0*V2*X3*V3*X2+32.D0*V4*X3*V6*X2-
     &       32.D0*V4*X3*U1*Y2-10.D0*V2*X3*V1*X2-8.D0*U5*Y2*V1*X2+
     &       4.D0*V2*X2*U6*Y3-16.D0*U4*Y2*V6*X3+5.D0*V2*X2*U3*Y3-
     &       32.D0*U3*Y2**2*U6-16.D0*V5*X2*U6*Y3+9.D0*V1*X2*U3*Y3-
     &       8.D0*V4*X3*V3*X2+32.D0*U4*Y2*U6*Y3-10.D0*U2*Y2*V1*X2
      A13(IELEM) = (5.D0*V2*X2*U1*Y3-8.D0*U4*Y2*U3*Y3-32.D0*U4*Y2*V1*X3+
     &             4.D0*U4*Y2*V3*X3+5.D0*U2*Y2*V3*X3-32.D0*V5*X2*U3*Y3+
     &             4.D0*V5*X2*U1*Y3+4.D0*U2*Y2*V6*X3-8.D0*U2*Y2*V6*X2-
     &             10.D0*V2*X2*U1*Y2+64.D0*U4*Y2*U1*Y3-
     &             16.D0*U5*Y3*V6*X2-8.D0*V2*X2*U6*Y2-
     &             32.D0*U5*Y3*V3*X2+5.D0*U2*Y2*V1*X3+
     &             10.D0*V2*X2**2*V1-54.D0*V1*X3*U1*Y2+
     &             10.D0*U2*Y2**2*U1+ANS1+ANS2+ANS3)*(-AUX2520)
!
      ANS1 = -48.D0*V4**2*X2**2-96.D0*U5*Y2*V5*X2-2.D0*V1*X2*U3*Y2+
     &       16.D0*U5*Y3*V4*X2+16.D0*V5*X2*U4*Y3+48.D0*U5**2*Y2**2+
     &       96.D0*U4*Y2*V4*X2-64.D0*U1*Y3*V6*X3+96.D0*U5*Y3*V5*X2+
     &       16.D0*V5*X3*U4*Y2-32.D0*U5*Y3*V4*X3+20.D0*V2**2*X3**2+
     &       48.D0*V5**2*X3**2+96.D0*U5*Y2*V5*X3-32.D0*V5*X3*U4*Y3+
     &       16.D0*U5*Y2*V4*X3-32.D0*V5*X3*V4*X2-96.D0*V4*X3*U4*Y3+
     &       20.D0*U2**2*Y3**2+8.D0*V2*X2*U4*Y3+48.D0*U4**2*Y3**2-
     &       56.D0*V1*X3*V6*X2-120.D0*V1*X3*U1*Y3+14.D0*U1*Y3*V3*X3+
     &       8.D0*U2*Y2**2*U6+18.D0*V2**2*X2**2+20.D0*U2*Y2*V5*X3-
     &       16.D0*U2*Y3*V5*X3+20.D0*U2*Y3*V5*X2+16.D0*U4*Y2*V1*X2+
     &       32.D0*U4*Y2*V6*X2+28.D0*U1*Y3*V6*X2-40.D0*U2*Y3*U5*Y2-
     &       64.D0*V4*X3*U6*Y3+48.D0*V4**2*X3**2-64.D0*U4*Y3*V6*X3+
     &       16.D0*V4*X2*U1*Y2+32.D0*V4*X2*U6*Y2+80.D0*U6**2*Y3**2+
     &       8.D0*U2*Y2*V4*X3-24.D0*U2*Y2*V4*X2+16.D0*U2*Y3*V6*X3+
     &       18.D0*U2**2*Y2**2+4.D0*U3**2*Y3**2-96.D0*V5*X3*U5*Y3+
     &       32.D0*U5*Y3**2*U6-32.D0*U5*Y2*U4*Y3+24.D0*V5*X2**2*V2+
     &       60.D0*U1**2*Y3**2+60.D0*V1**2*X3**2+4.D0*V3**2*X3**2+
     &       80.D0*V6**2*X3**2-64.D0*V6**2*X3*X2-6.D0*V3**2*X3*X2-
     &       8.D0*U5*Y3**2*U3-38.D0*V2**2*X2*X3-64.D0*U6**2*Y2*Y3+
     &       48.D0*U5**2*Y3**2+24.D0*V2*X2**2*V4-8.D0*U5*Y2**2*U1
      ANS2 = -24.D0*V2*X2*U4*Y2-16.D0*U6**2*Y2**2-8.D0*V2*X3**2*V4+
     &       8.D0*V2*X2**2*V6+24.D0*U2*Y2**2*U4-8.D0*V5*X3**2*V3-
     &       8.D0*V2*X2**2*V3+8.D0*V4*X2**2*V3-36.D0*V2*X2*U2*Y2-
     &       24.D0*U3*Y3**2*U6-6.D0*V1**2*X2**2-54.D0*U1**2*Y3*Y2-
     &       8.D0*U1*Y2**2*U6-16.D0*V6**2*X2**2-2.D0*V2*X3**2*V3-
     &       38.D0*U2**2*Y2*Y3-16.D0*V4*X2**2*V1-8.D0*V5*X2**2*V1-
     &       24.D0*V5*X2*U2*Y2-24.D0*U5*Y2*V2*X2+16.D0*V2*X3**2*V5+
     &       56.D0*V4*X3**2*V1-8.D0*V1*X2**2*V6+8.D0*U2*Y2*V3*X2+
     &       48.D0*V5**2*X2**2-14.D0*V1*X3**2*V3+2.D0*V2*X3*U3*Y3+
     &       2.D0*V3**2*X2**2+8.D0*U4*Y2**2*U3+2.D0*U2*Y3*V3*X3-
     &       16.D0*U2*Y3**2*U6-16.D0*U2*Y3**2*U1-14.D0*U1*Y3**2*U3+
     &       64.D0*U4*Y3**2*U6+12.D0*V1*X2*U1*Y2-64.D0*V1*X3*U6*Y3+
     &       2.D0*U1*Y2**2*U3-24.D0*V6*X3**2*V3-6.D0*U3**2*Y3*Y2+
     &       24.D0*U5*Y2**2*U2-54.D0*V1**2*X3*X2-16.D0*V2*X3**2*V1+
     &       8.D0*V2*X2*U3*Y2-16.D0*U4*Y2**2*U1+2.D0*V1*X2**2*V3-
     &       8.D0*U2*Y2**2*U3+16.D0*U2*Y3**2*U5-16.D0*U4*Y3**2*U3-
     &       8.D0*U2*Y3**2*U4-96.D0*U5**2*Y2*Y3-96.D0*V5**2*X3*X2+
     &       32.D0*V5*X3**2*V4+32.D0*U5*Y3**2*U4+2.D0*U3**2*Y2**2-
     &       16.D0*V2*X3*U5*Y3+64.D0*V6*X2*U6*Y3+32.D0*V6*X2*U6*Y2-
     &       8.D0*V6*X2*U3*Y2+20.D0*V2*X3*U5*Y2+8.D0*V3*X2**2*V6
      ANS3 = 8.D0*U2*Y3*V4*X2+8.D0*U2*Y3*V4*X3-16.D0*U2*Y3*U4*Y2-
     &       16.D0*V2*X3*V4*X2+32.D0*V5*X3**2*V6-4.D0*V3*X2*U3*Y2-
     &       8.D0*V3*X2*U6*Y2+8.D0*V2*X3*U4*Y2-40.D0*V2*X3*V5*X2-
     &       6.D0*U1**2*Y2**2-2.D0*U1*Y2*V3*X2+8.D0*V2*X3*U4*Y3+
     &       64.D0*V6*X3*U6*Y2+8.D0*U1*Y2*V6*X2-8.D0*V4*X2*U3*Y2+
     &       38.D0*V2*X2*U2*Y3+14.D0*V1*X3*U3*Y3+38.D0*U2*Y2*V2*X3-
     &       40.D0*U2*Y3*V2*X3+20.D0*V2*X2*U5*Y3-4.D0*V5*X3*U3*Y2+
     &       64.D0*V4*X3**2*V6-4.D0*U5*Y3*V1*X2-2.D0*U2*Y3**2*U3-
     &       56.D0*U1*Y3*U6*Y2-32.D0*U4*Y2**2*U6+8.D0*U5*Y3*V3*X3+
     &       28.D0*V1*X3*U6*Y2-48.D0*U4**2*Y2**2-32.D0*U5*Y2*U6*Y3+
     &       12.D0*U1*Y3*U3*Y2+8.D0*U5*Y2*U3*Y3+64.D0*V1*X3**2*V6-
     &       8.D0*U4*Y2*V3*X2+16.D0*V5*X3*U6*Y2-6.D0*U1*Y3*V3*X2+
     &       8.D0*V5*X3*V3*X2+8.D0*V5*X3*V1*X2+64.D0*U1*Y3**2*U6+
     &       8.D0*V5*X2*U1*Y2+8.D0*U5*Y2*U1*Y3+6.D0*V3*X3*U3*Y2+
     &       6.D0*V3*X2*U3*Y3-8.D0*V3*X2*U6*Y3+16.D0*U3*Y3*U6*Y2-
     &       32.D0*V5*X3*V6*X2-8.D0*U3*Y3*V6*X2+54.D0*V1*X2*U1*Y3+
     &       28.D0*V1*X2*U6*Y3-4.D0*V5*X3*U1*Y2+8.D0*V5*X3*U3*Y3+
     &       12.D0*V1*X3*V3*X2+56.D0*U4*Y3**2*U1+16.D0*V6*X3*V3*X2-
     &       8.D0*V3*X3*U3*Y3-160.D0*V6*X3*U6*Y3-4.D0*U5*Y2*V1*X3-
     &       4.D0*U5*Y2*V3*X3-32.D0*V4*X2**2*V6+24.D0*V3*X3*U6*Y3
      ANS4 = -8.D0*V3*X3*U6*Y2+16.D0*U5*Y2*V6*X3+16.D0*U2*Y3*U1*Y2+
     &       16.D0*U4*Y3*V6*X2-5.D0*U2*Y3*V3*X2-4.D0*U4*Y3*V3*X2+
     &       20.D0*U4*Y3*V1*X2-56.D0*U4*Y3*V1*X3-6.D0*V1*X3*U3*Y2-
     &       6.D0*U1*Y2*V3*X3-8.D0*U2*Y3*V1*X2+28.D0*U1*Y2*V6*X3-
     &       8.D0*V6*X3*U3*Y2+24.D0*V6*X3*U3*Y3-4.D0*V2*X3*U6*Y2-
     &       40.D0*V4*X3*V1*X2-4.D0*V4*X2*U3*Y3-5.D0*V2*X3*U3*Y2+
     &       20.D0*V4*X2*U1*Y3+8.D0*V2*X3*V6*X2+16.D0*U2*Y3*V1*X3+
     &       8.D0*U2*Y3*U6*Y2+16.D0*V4*X2*U6*Y3+16.D0*U4*Y3*V3*X3+
     &       10.D0*U2*Y3*U3*Y2-4.D0*U2*Y3*V6*X2-8.D0*V2*X3*U1*Y2+
     &       8.D0*V1*X2*U6*Y2+16.D0*V4*X3*U6*Y2-4.D0*V4*X3*U3*Y2+
     &       10.D0*V2*X3*V3*X2-32.D0*V4*X3*V6*X2+20.D0*V4*X3*U1*Y2+
     &       16.D0*V4*X3*U3*Y3+16.D0*V2*X3*V1*X2-16.D0*V2*X3**2*V6+
     &       8.D0*U5*Y2*V1*X2+16.D0*V2*X3*U1*Y3+16.D0*V2*X3*U6*Y3-
     &       4.D0*V2*X2*U6*Y3+16.D0*U4*Y2*V6*X3-5.D0*V2*X2*U3*Y3+
     &       8.D0*U3*Y2**2*U6+16.D0*V5*X2*U6*Y3-6.D0*V1*X2*U3*Y3+
     &       8.D0*V4*X3*V3*X2-16.D0*V4*X3**2*V3-56.D0*V4*X3*U1*Y3-
     &       32.D0*U4*Y2*U6*Y3-8.D0*V2*X2*U1*Y3+8.D0*U4*Y2*U3*Y3+
     &       20.D0*U4*Y2*V1*X3-4.D0*U4*Y2*V3*X3-5.D0*U2*Y2*V3*X3-
     &       4.D0*V5*X2*U3*Y3-32.D0*U5*Y3*V6*X3-32.D0*V5*X3*U6*Y3
      A14(IELEM) = (-4.D0*V5*X2*U1*Y3-4.D0*U2*Y2*V6*X3-8.D0*U2*Y2*V6*X2-
     &       40.D0*U4*Y2*U1*Y3+16.D0*U5*Y3*V6*X2-8.D0*V2*X2*U6*Y2-
     &       4.D0*U5*Y3*V3*X2-8.D0*U2*Y2*V1*X3+54.D0*V1*X3*U1*Y2+
     &       ANS1+ANS2+ANS3+ANS4) * (-AUX630)
!
      ANS1 = (-96.D0*U5*V5+12.D0*V1*U1+8.D0*U2*V3+12.D0*U4*V1-
     &       16.D0*U6*V3-8.D0*V2*U6-8.D0*V4*U3-V1*U3-U1*V3-20.D0*U2*V2+
     &       8.D0*U5*V1-8.D0*U2*V6+8.D0*V5*U1-8.D0*U4*V3+64.D0*U6*V6+
     &       32.D0*V4*U6+32.D0*U4*V6-12.D0*U5*V2-12.D0*U5*V3+
     &       8.D0*V2*U3-16.D0*U2*V4+12.D0*U1*V6-V2*U1-16.D0*U3*V6-
     &       U2*V1-16.D0*V2*U4+12.D0*V4*U1-20.D0*U3*V3-12.D0*U2*V5-
     &       12.D0*U3*V5+64.D0*U4*V4+12.D0*V1*U6)*Y3*X2+((24.D0*V3*V5+
     &       24.D0*V5*V2+20.D0*V3**2+32.D0*V3*V6-24.D0*V1*V6-
     &       16.D0*V2*V3+2.D0*V2*V1+16.D0*V4*V3-64.D0*V4**2+
     &       96.D0*V5**2-64.D0*V6**2+20.D0*V2**2-16.D0*V5*V1-
     &       64.D0*V4*V6+32.D0*V2*V4-12.D0*V1**2-24.D0*V4*V1+
     &       2.D0*V1*V3+16.D0*V2*V6)*X3+(96.D0*U5*V5-12.D0*V1*U1-
     &       8.D0*U2*V3-16.D0*U4*V1+8.D0*U6*V3+8.D0*V2*U6+8.D0*V4*U3+
     &       2.D0*V1*U3+2.D0*U1*V3+36.D0*U2*V2-8.D0*U5*V1+8.D0*U2*V6-
     &       8.D0*V5*U1+8.D0*U4*V3-32.D0*U6*V6-32.D0*V4*U6-32.D0*U4*V6+
     &       24.D0*U5*V2-8.D0*V2*U3+24.D0*U2*V4-8.D0*U1*V6+8.D0*U3*V6+
     &       24.D0*V2*U4-16.D0*V4*U1+4.D0*U3*V3+24.D0*U2*V5-
     &       96.D0*U4*V4-8.D0*V1*U6)*Y2)*X2+(-96.D0*U5*V5+12.D0*V1*U1+
     &       8.D0*U2*V3+12.D0*U4*V1-16.D0*U6*V3-8.D0*V2*U6)*Y2*X3
      ANS2 = (-8.D0*V4*U3-V1*U3-U1*V3-20.D0*U2*V2+8.D0*U5*V1-8.D0*U2*V6+
     &       8.D0*V5*U1-8.D0*U4*V3+64.D0*U6*V6+32.D0*V4*U6+32.D0*U4*V6-
     &       12.D0*U5*V2-12.D0*U5*V3+8.D0*V2*U3-16.D0*U2*V4+12.D0*U1*V6-
     &       V2*U1-16.D0*U3*V6-U2*V1-16.D0*V2*U4+12.D0*V4*U1-
     &       20.D0*U3*V3-12.D0*U2*V5-12.D0*U3*V5+64.D0*U4*V4+
     &       12.D0*V1*U6)*Y2*X3+(-8.D0*U6*U3-24.D0*U2*U4+48.D0*U4**2-
     &       2.D0*U3*U1+32.D0*U4*U6+8.D0*U6*U1-48.D0*U5**2+8.D0*U5*U1-
     &       18.D0*U2**2-2.D0*U3**2+6.D0*U1**2+16.D0*U6**2+16.D0*U4*U1-
     &       8.D0*U4*U3+8.D0*U2*U3-8.D0*U2*U6-24.D0*U5*U2)*Y2**2+
     &       (-64.D0*U4**2-24.D0*U6*U1+24.D0*U5*U2+24.D0*U3*U5-
     &       64.D0*U4*U6+16.D0*U2*U6-16.D0*U2*U3-16.D0*U5*U1-
     &       12.D0*U1**2+20.D0*U2**2+20.D0*U3**2+32.D0*U6*U3+
     &       96.D0*U5**2+2.D0*U2*U1+32.D0*U2*U4+2.D0*U3*U1-
     &       24.D0*U4*U1-64.D0*U6**2+16.D0*U4*U3)*Y3*Y2+(96.D0*U5*V5-
     &       12.D0*V1*U1-8.D0*U2*V3-8.D0*U4*V1+24.D0*U6*V3+
     &       8.D0*V2 *U6+8.D0*V4*U3+4.D0*U2*V2-8.D0*U5*V1+8.D0*U2*V6-
     &       8.D0*V5*U1+8.D0*U4*V3-96.D0*U6*V6-32.D0*V4*U6-32.D0*U4*V6+
     &       24.D0*U5*V3-8.D0*V2*U3+8.D0*U2*V4-16.D0*U1*V6+2.D0*V2*U1+
     &       24.D0*U3*V6+2.D0*U2*V1+8.D0*V2*U4-8.D0*V4*U1+36.D0*U3*V3+
     &       24.D0*U3*V5-32.D0*U4*V4-16.D0*V1*U6)*Y3*X3
      A15(IELEM) = ((16.D0*V1*V6-24.D0*V3*V6-18.D0*V3**2+48.D0*V6**2-
     &             8.D0*V4*V3+ 6.D0*V1**2+8.D0*V5*V1+8.D0*V2*V3+
     &             16.D0*V4**2-8.D0*V2*V4-48.D0*V5**2+32.D0*V4*V6+
     &             8.D0*V4*V1-2.D0*V2**2-2.D0*V2*V1-8.D0*V2*V6-
     &             24.D0*V3*V5)*X3**2+(-24.D0*V2*V4+6.D0*V1**2+
     &             32.D0*V4*V6+16.D0*V4*V1-8.D0*V4*V3+8.D0*V5*V1+
     &             48.D0*V4**2+8.D0*V2*V3+16.D0*V6**2-8.D0*V2*V6+
     &             8.D0*V1*V6-18.D0*V2**2-48.D0*V5**2-2.D0*V3**2-
     &             24.D0*V5*V2-2.D0*V1*V3-8.D0*V3*V6)*X2**2+
     &             (-48.D0*U5**2-24.D0*U3*U5-8.D0*U2*U4-2.D0*U2*U1+
     &             6.D0*U1**2-2.D0*U2**2-18.D0*U3**2+16.D0*U4**2+
     &             32.D0*U4*U6+8.D0*U2*U3+8.D0*U5*U1+48.D0*U6**2-
     &             24.D0*U6*U3-8.D0*U2*U6-8.D0*U4*U3+8.D0*U4*U1+
     &             16.D0*U6*U1)*Y3**2+ANS1+ANS2) * (-AUX630)
!
      ANS1 = -186.D0*U2*Y3*V2*X3-23.D0*U2*Y3**2*U1+48.D0*V4*X3**2*V6-
     &       96.D0*U4*Y3*V2*X3-23.D0*V2*X3**2*V3+112.D0*V4*X3**2*V5-
     &       32.D0*V4*X3**2*V1+4.D0*V2*X3**2*V6+96.D0*V4*X3**2*V2-
     &       28.D0*V5*X3**2*V1+48.D0*U4*Y3**2*U6+4.D0*U2*Y3**2*U6+
     &       96.D0*U2*Y3**2*U4+48.D0*U5*Y3**2*U6-32.D0*V3*X3**2*V5-
     &       23.D0*U2*Y3**2*U3-28.D0*U5*Y3**2*U1-28.D0*U4*Y3**2*U3-
     &       23.D0*V2*X3**2*V1+112.D0*U4**2*Y3**2+V3*X3**2*V1-
     &       32.D0*U4*Y3**2*U1+112.D0*U5**2*Y3**2+13.D0*U3**2*Y3**2+
     &       112.D0*V5**2*X3**2+13.D0*V3**2*X3**2+112.D0*U5*Y3**2*U4+
     &       48.D0*V6**2*X3**2-26.D0*V1*X3*U1*Y3+13.D0*U1**2*Y3**2-
     &       V3*X3*U1*Y3-26.D0*V3*X3*U3*Y3+U1*Y3**2*U3+
     &       48.D0*V5*X3**2*V6+112.D0*V4**2*X3**2-224.D0*V4*X3*U4*Y3-
     &       224.D0*V5*X3*U5*Y3-112.D0*V5*X3*U4*Y3-112.D0*V4*X3*U5*Y3-
     &       4.D0*V2*X3*U6*Y3+23.D0*U2*Y3*V3*X3+96.D0*V2*X3**2*V5+
     &       23.D0*V2*X3*U1*Y3+23.D0*V2*X3*U3*Y3+23.D0*U2*Y3*V1*X3-
     &       4.D0*U2*Y3*V6*X3+28.D0*U4*Y3*V3*X3+32.D0*U4*Y3*V1*X3-
     &       48.D0*U4*Y3*V6*X3+28.D0*U5*Y3*V1*X3-48.D0*V4*X3*U6*Y3+
     &       32.D0*V4*X3*U1*Y3+28.D0*V4*X3*U3*Y3-32.D0*U3*Y3**2*U5
      A22(IELEM) = (ANS1-48.D0*U5*Y3*V6*X3-48.D0*V5*X3*U6*Y3+
     &             28.D0*V5*X3*U1*Y3+96.D0*U2*Y3**2*U5+
     &             93.D0*U2**2*Y3**2-96.D0*U2*Y3*V5*X3-
     &             96.D0*V2*X3*U5*Y3+32.D0*V3*X3*U5*Y3+
     &             32.D0*U3*Y3*V5*X3+93.D0*V2**2*X3**2-
     &             96.D0*U2*Y3*V4*X3+48.D0*U6**2*Y3**2+
     &             13.D0*V1**2*X3**2-28.D0*V4*X3**2*V3-
     &             V1*X3*U3*Y3-96.D0*V6*X3*U6*Y3) * (AUX1260)
!
      ANS1 = (-32.D0*V5**2-18.D0*V2*V3-8.D0*V4*V3-8.D0*V2*V6+
     &       32.D0*V5*V2-10.D0*V1**2+32.D0*V3*V5+96.D0*V4**2+
     &       64.D0*V3*V6+96.D0*V6**2+54.D0*V3**2+32.D0*V5*V6+
     &       32.D0*V4*V6+32.D0*V4*V5-32.D0*V4*V1-32.D0*V1*V6-
     &       8.D0*V5*V1-10.D0*V2*V1+64.D0*V2*V4-10.D0*V1*V3+
     &       54.D0*V2**2)*X3+(32.D0*U5*V5+10.D0*V1*U1+9.D0*U2*V3+
     &       16.D0*U4*V1-32.D0*U6*V3+4.D0*V2*U6+4.D0*V4*U3+5.D0*V1*U3+
     &       5.D0*U1*V3-54.D0*U2*V2-16.D0*V4*U5+4.D0*U5*V1-16.D0*U4*V5+
     &       4.D0*U2*V6+4.D0*V5*U1+4.D0*U4*V3-96.D0*U6*V6-16.D0*V4*U6-
     &       16.D0*U4*V6-16.D0*U5*V6-16.D0*V5*U6-16.D0*U5*V2-
     &       16.D0*U5*V3+9.D0*V2*U3-32.D0*U2*V4+16.D0*U1*V6+5.D0*V2*U1-
     &       32.D0*U3*V6+5.D0*U2*V1-32.D0*V2*U4+16.D0*V4*U1-
     &       54.D0*U3*V3-16.D0*U2*V5-16.D0*U3*V5-96.D0*U4*V4+
     &       16.D0*V1*U6)*Y3
      A23(IELEM) = (ANS1*X2+(32.D0*U5*V5+10.D0*V1*U1+9.D0*U2*V3+
     &             16.D0*U4*V1-32.D0*U6*V3+ 4.D0*V2*U6+4.D0*V4*U3+
     &             5.D0*V1*U3+5.D0*U1*V3-54.D0*U2*V2-16.D0*V4*U5+
     &             4.D0*U5*V1-16.D0*U4*V5+4.D0*U2*V6+4.D0*V5*U1+
     &             4.D0*U4*V3-96.D0*U6*V6-16.D0*V4*U6-16.D0*U4*V6-
     &             16.D0*U5*V6-16.D0*V5*U6-16.D0*U5*V2-16.D0*U5*V3+
     &             9.D0*V2*U3-32.D0*U2*V4+16.D0*U1*V6+5.D0*V2*U1-
     &             32.D0*U3*V6+5.D0*U2*V1-32.D0*V2*U4+16.D0*V4*U1-
     &             54.D0*U3*V3-16.D0*U2*V5-16.D0*U3*V5-96.D0*U4*V4+
     &             16.D0*V1*U6)*Y2*X3+(96.D0*U4**2-10.D0*U1**2+
     &             96.D0*U6**2+54.D0*U2**2+54.D0*U3**2-32.D0*U5**2+
     &             64.D0*U6*U3+32.D0*U5*U6-8.D0*U5*U1-10.D0*U2*U1+
     &             64.D0*U2*U4-10.D0*U3*U1+32.D0*U4*U5-32.D0*U6*U1-
     &             8.D0*U2*U6-18.D0*U2*U3-8.D0*U4*U3+32.D0*U4*U6+
     &             32.D0*U3*U5-32.D0*U4*U1+32.D0*U5*U2)*Y3*Y2) *
     &             AUX2520
!
      ANS1 = ((8.D0*V2*V6+96.D0*V4**2+66.D0*V2**2+32.D0*V4*V6-
     &       16.D0*V2*V1-32.D0*V4*V1+32.D0*V5* V6-24.D0*V5*V1-
     &       8.D0*V3*V6+72.D0*V5*V2+ 2.D0*V1**2-32.D0*V3*V5+
     &       96.D0*V4*V5+96.D0*V5**2-16.D0*V2*V3+2.D0*V3**2-
     &       8.D0*V1*V6+72.D0*V2*V4+ 6.D0*V1*V3-24.D0*V4*V3)*X3+
     &       (-96.D0*U5*V5-2.D0* V1*U1+8.D0*U2*V3+16.D0*U4*V1+
     &       4.D0*U6*V3-4.D0*V2*U6+12.D0*V4*U3-3.D0*V1*U3-
     &       3.D0*U1*V3-66.D0*U2*V2-48.D0*V4*U5+12.D0*U5*V1-
     &       48.D0*U4*V5-4.D0*U2*V6+12.D0*V5*U1+ 12.D0*U4*V3-
     &       16.D0*V4*U6-16.D0*U4*V6-16.D0*U5*V6-16.D0*V5*U6-
     &       36.D0*U5*V2+16.D0*U5*V3+8.D0*V2*U3-36.D0*U2*V4+
     &       4.D0*U1*V6+8.D0*V2*U1+4.D0*U3*V6+8.D0*U2*V1-
     &       36.D0*V2*U4+16.D0*V4*U1-2.D0*U3*V3-36.D0*U2*V5+
     &       16.D0*U3*V5-96.D0*U4*V4+4.D0*V1*U6)*Y3)*X2+
     &       (-96.D0*U5*V5-2.D0*V1*U1+8.D0*U2*V3+16.D0*U4*V1+
     &       4.D0*U6*V3-4.D0*V2*U6+12.D0*V4*U3-3.D0*V1*U3-
     &       3.D0*U1*V3-66.D0*U2*V2-48.D0*V4*U5+12.D0*U5*V1-
     &       48.D0*U4*V5-4.D0*U2*V6+12.D0*V5*U1+12.D0*U4*V3-
     &       16.D0*V4*U6-16.D0*U4*V6-16.D0*U5*V6-16.D0*V5*U6-
     &       36.D0*U5*V2+16.D0*U5*V3+8.D0*V2*U3-36.D0*U2*V4)*Y2*X3
      ANS2 = (4.D0*U1*V6+8.D0*V2*U1+4.D0*U3*V6+8.D0*U2*V1-
     &        36.D0*V2*U4+16.D0*V4*U1-2.D0*U3*V3-36.D0*U2*V5+
     &        16.D0*U3*V5-96.D0*U4*V4+4.D0*V1*U6)*Y2*X3+
     &        (-8.D0*U6*U3+32.D0*U5*U6-32.D0*U3*U5-32.D0*U4*U1-
     &        24.D0*U4*U3-24.D0*U5*U1+96.D0*U4**2+66.D0*U2**2+
     &        2.D0*U3**2+8.D0*U2*U6+96.D0*U5**2-16.D0*U2*U1+
     &        72.D0*U2*U4-16.D0*U2*U3+32.D0*U4*U6+6.D0*U3*U1-
     &        8.D0*U6*U1+96.D0*U4*U5+2.D0*U1**2+72.D0*U5*U2)*Y3*Y2+
     &        (-64.D0*U4*U5+24.D0*U3*U5-32.D0*U5*U6-16.D0*U6*U1-
     &        56.D0*U2*U4-60.D0*U2**2+16.D0*U2*U1+16.D0*U5*U1+
     &        8.D0*U4*U1+8.D0*U6*U3-32.D0*U4*U6+16.D0*U4*U3-
     &        64.D0*U5*U2+14.D0*U2*U3+2.D0*U3*U1-20.D0*U1**2-
     &        4.D0*U3**2-80.D0*U5**2-48.D0*U6**2-48.D0*U4**2)*Y3**2
      A24(IELEM) = (ANS1+ANS2+(-56.D0*V2*V4+14.D0*V2*V3+16.D0*V5*V1+
     &             16.D0*V2*V1-64.D0*V4*V5-32.D0*V5*V6-16.D0*V1*V6-
     &             80.D0*V5**2+16.D0*V4*V3-60.D0*V2**2-4.D0*V3**2-
     &             48.D0*V6**2-20.D0*V1**2+2.D0*V1*V3-48.D0*V4**2-
     &             32.D0*V4*V6+8.D0*V4*V1+8.D0*V3*V6-64.D0*V5*V2+
     &             24.D0*V3*V5)*X3**2+(160.D0*U5*V5+40.D0*V1*U1-
     &             14.D0*U2*V3-8.D0*U4*V1-8.D0*U6*V3-16.D0*V4*U3-
     &             2.D0*V1*U3-2.D0*U1*V3+120.D0*U2*V2+64.D0*V4*U5-
     &             16.D0*U5*V1+64.D0*U4*V5-16.D0*V5*U1-16.D0*U4*V3+
     &             96.D0*U6*V6+32.D0*V4*U6+32.D0*U4*V6+32.D0*U5*V6+
     &             32.D0*V5*U6+64.D0*U5*V2-24.D0*U5*V3-14.D0*V2*U3+
     &             56.D0*U2*V4+16.D0*U1*V6-16.D0*V2*U1-8.D0*U3*V6-
     &             16.D0*U2*V1+ 56.D0*V2*U4-8.D0*V4*U1+8.D0*U3*V3+
     &             64.D0*U2*V5-24.D0*U3*V5+96.D0*U4*V4+
     &             16.D0*V1*U6)*Y3*X3)*AUX630
!
      ANS1 = ((-8.D0*V5*V2+16.D0*V3*V5+24.D0*V3*V6+16.D0*V3**2-
     &       2.D0*V2*V3+2.D0*V2*V1+8.D0*V2*V4-24.D0*V1*V6-32.D0*V5**2-
     &       16.D0*V1**2-16.D0*V4*V1+32.D0*V4**2)*X3+(12.D0*V1*U6+
     &       4.D0*U5*V2-32.D0*U4*V4-8.D0*U3*V5+4.D0*U2*V5-8.D0*U5*V3+
     &       V2*U3-4.D0*U2*V4+12.D0*U1*V6-V2*U1-12.D0*U3*V6-U2*V1-
     &       4.D0*V2*U4+8.D0*V4*U1-16.D0*U3*V3+32.D0*U5*V5+16.D0*V1*U1+
     &       U2*V3+8.D0*U4*V1-12.D0*U6*V3)*Y3)*X2+((12.D0*V1*U6+
     &       4.D0*U5*V2-32.D0*U4*V4-8.D0*U3*V5+4.D0*U2*V5-8.D0*U5*V3+
     &       V2*U3-4.D0*U2*V4+12.D0*U1*V6-V2*U1-12.D0*U3*V6-U2*V1-
     &       4.D0*V2*U4+8.D0*V4*U1-16.D0*U3*V3+32.D0*U5*V5+
     &       16.D0*V1*U1+U2*V3+8.D0*U4*V1-12.D0*U6*V3)*Y2+(-96.D0*U5*V5+
     &       4.D0*V1*U1+8.D0*U4*V1+24.D0*U6*V3-8.D0*V2*U6+8.D0*V4*U3-
     &       8.D0*V1*U3-8.D0*U1*V3-12.D0*U2*V2-32.D0*V4*U5+8.D0*U5*V1-
     &       32.D0*U4*V5-8.D0*U2*V6+8.D0*V5*U1+8.D0*U4*V3+96.D0*U6*V6-
     &       16.D0*U5*V2+24.D0*U5*V3-8.D0*U2*V4+2.D0*V2*U1+24.D0*U3*V6+
     &       2.D0*U2*V1-8.D0*V2*U4+8.D0*V4*U1+36.D0*U3*V3-16.D0*U2*V5+
     &       24.D0*U3*V5-32.D0*U4*V4)*Y3)*X3
      A26(IELEM)=(ANS1+(32.D0*V4*V5+8.D0*V2*V4-18.D0*V3**2+8.D0*V1*V3-
     &           8.D0*V5*V1+48.D0*V5**2-24.D0*V3*V6+6.D0*V2**2-
     &           8.D0*V4*V1-48.D0*V6**2-2.D0*V1**2-8.D0*V4*V3+
     &           16.D0*V4**2-2.D0*V2*V1+8.D0*V2*V6+16.D0*V5*V2-
     &           24.D0*V3*V5)*X3**2+(-16.D0*U4*U1-24.D0*U6*U1
     &           -8.D0*U5*U2+16.D0*U3*U5-16.D0*U1**2+8.D0*U2*U4+
     &           16.D0*U3**2-32.D0*U5**2+2.D0*U2*U1+32.D0*U4**2-
     &           2.D0*U2*U3+24.D0*U6*U3)*Y3*Y2+(-8.D0*U5*U1-24.D0*U3*U5-
     &           24.D0*U6*U3+6.D0*U2**2+8.D0*U2*U6+32.D0*U4*U5+
     &           16.D0*U5*U2-2.D0*U2*U1-8.D0*U4*U3+8.D0*U2*U4+
     &           8.D0*U3*U1+16.D0*U4**2+48.D0*U5**2-2.D0*U1**2-
     &           8.D0*U4*U1-18.D0*U3**2-48.D0*U6**2)*Y3**2)*(-AUX630)
!
      A33(IELEM) = ((-224.D0*U5*V5-26.D0*V1*U1+23.D0*U2*V3-96.D0*U6*V3+
     &             28.D0*V2*U6-4.D0*V4*U3+23.D0*V1*U3+23.D0*U1*V3-
     &             26.D0*U2*V2-48.D0*V4*U5+28.D0*U5*V1-48.D0*U4*V5+
     &             28.D0*U2*V6+28.D0*V5*U1-4.D0*U4*V3-224.D0*U6*V6-
     &             48.D0*V4*U6-48.D0*U4*V6-112.D0*U5*V6-112.D0*V5*U6+
     &             32.D0*U5*V2-96.D0*U5*V3+23.D0*V2*U3+32.D0*U1*V6-
     &             V2*U1-96.D0*U3*V6-U2*V1-186.D0*U3*V3+32.D0*U2*V5-
     &             96.D0*U3*V5-96.D0*U4*V4+32.D0*V1*U6)*Y2*X2+
     &             (4.D0*U4*U3+U2*U1+112.D0*U6**2+48.D0*U4**2+
     &             93.D0*U3**2-32.D0*U6*U1-23.D0*U3*U1+13.D0*U2**2-
     &             28.D0*U5*U1-28.D0*U2*U6+112.D0*U5**2-32.D0*U5*U2+
     &             13.D0*U1**2+48.D0*U4*U6+96.D0*U3*U5-23.D0*U2*U3+
     &             112.D0*U5*U6+48.D0*U4*U5+96.D0*U6*U3)*Y2**2+
     &             (-32.D0*V1*V6+48.D0*V4**2+96.D0*V3*V6+112.D0*V5*V6+
     &             48.D0*V4*V5+V2*V1+93.D0*V3**2-23.D0*V2*V3-
     &             23.D0*V1*V3-28.D0*V2*V6+112.D0*V5**2+4.D0*V4*V3+
     &             48.D0*V4*V6-28.D0*V5*V1+13.D0*V2**2+112.D0*V6**2+
     &             13.D0*V1**2+96.D0*V3*V5-32.D0*V5*V2)*X2**2)
     &             *AUX1260
!
      ANS1 = (-8.D0*V3*V6+32.D0*V5**2-24.D0*V2*V4-16.D0*V2**2+
     &       2.D0*V2*V3-32.D0*V6**2+16.D0*V1**2+16.D0*V1*V6+
     &       24.D0*V4*V1-16.D0*V5*V2-2.D0*V1*V3+8.D0*V3*V5)*X3*X2+
     &       (96.D0*U5*V5-4.D0*V1*U1+8.D0*U6*V3-8.D0*V2*U6+
     &       8.D0*V4*U3-2.D0*V1*U3-2.D0*U1*V3-36.D0*U2*V2-8.D0*U5*V1-
     &       8.D0*U2*V6-8.D0*V5*U1+8.D0*U4*V3+32.D0*U6*V6+32.D0*U5*V6+
     &       32.D0*V5*U6-24.D0*U5*V2+16.D0*U5*V3-24.D0*U2*V4-
     &       8.D0*U1*V6+8.D0*V2*U1+8.D0*U3*V6+8.D0*U2*V1-24.D0*V2*U4+
     &       12.D0*U3*V3-24.D0*U2*V5+16.D0*U3*V5-96.D0*U4*V4-
     &       8.D0*V1*U6)*Y2*X2+(16.D0*U2*V2+U1*V3-8.D0*V1*U6+
     &       12.D0*V2*U4+8.D0*U5*V2-4.D0*U3*V5+8.D0*U2*V5-4.D0*U5*V3-
     &       8.D0*U1*V6+12.D0*U2*V4-U2*V3-V2*U3-16.D0*V1*U1+4.D0*U6*V3+
     &       4.D0*U3*V6-12.D0*U4*V1+32.D0*U6*V6+V1*U3-12.D0*V4*U1-
     &       32.D0*U5*V5)*Y2*X3+(2.D0*U3*U1-8.D0*U4*U3-8.D0*U2*U1-
     &       32.D0*U5*U6+8.D0*U5*U1+24.D0*U2*U4+8.D0*U6*U1-8.D0*U6*U3+
     &       48.D0*U4**2-48.D0*U5**2+18.D0*U2**2+2.D0*U1**2-
     &       16.D0*U6**2-6.D0*U3**2+8.D0*U2*U6-16.D0*U3*U5+
     &       24.D0*U5*U2)*Y2**2
      A34(IELEM) = (ANS1+(8.D0*V1*V6-8.D0*V4*V3+18.D0*V2**2-16.D0*V6**2+
     &             2.D0*V1**2-6.D0*V3**2+2.D0*V1*V3+8.D0*V2*V6+
     &             8.D0*V5*V1-32.D0*V5*V6+24.D0*V2*V4-48.D0*V5**2-
     &             8.D0*V3*V6-8.D0*V2*V1+48.D0*V4**2-16.D0*V3*V5+
     &             24.D0*V5*V2)*X2**2+(16.D0*U2*V2+U1*V3-8.D0*V1*U6+
     &             12.D0*V2*U4+8.D0*U5*V2-4.D0*U3*V5+8.D0*U2*V5-
     &             4.D0*U5*V3-8.D0*U1*V6+12.D0*U2*V4-U2*V3-V2*U3-
     &             16.D0*V1*U1+4.D0*U6*V3+4.D0*U3*V6-12.D0*U4*V1+
     &             32.D0*U6*V6+V1*U3-12.D0*V4*U1-32.D0*U5*V5)*Y3*X2+
     &             (-8.D0*U6*U3-16.D0*U5*U2+24.D0*U4*U1-2.D0*U3*U1+
     &             16.D0*U6*U1+32.D0*U5**2+8.D0*U3*U5+16.D0*U1**2-
     &             32.D0*U6**2-24.D0*U2*U4+2.D0*U2*U3-
     &             16.D0*U2**2)*Y3*Y2)*AUX630
!
      ANS1 = ((96.D0*U5*V5-4.D0*V1*U1+8.D0*U6*V3-8.D0*V2*U6+8.D0*V4*U3-
     &        2.D0*V1*U3-2.D0*U1*V3-36.D0*U2*V2-8.D0*U5*V1-8.D0*U2*V6-
     &        8.D0*V5*U1+8.D0*U4*V3+32.D0*U6*V6+32.D0*U5*V6+
     &        32.D0*V5*U6-24.D0*U5*V2+16.D0*U5*V3-24.D0*U2*V4-
     &        8.D0*U1*V6+8.D0*V2*U1+8.D0*U3*V6+8.D0*U2*V1-24.D0*V2*U4+
     &        12.D0*U3*V3-24.D0*U2*V5+16.D0*U3*V5-96.D0*U4*V4-
     &        8.D0*V1*U6)*Y2+(-96.D0*U5*V5-2.D0*V1*U1+8.D0*U2*V3+
     &        4.D0*U4*V1-36.D0*U6*V3+12.D0*V2*U6-4.D0*V4*U3+8.D0*V1*U3+
     &        8.D0*U1*V3-2.D0*U2*V2-16.D0*V4*U5+12.D0*U5*V1-16.D0*U4*V5+
     &        12.D0*U2*V6+12.D0*V5*U1-4.D0*U4*V3-96.D0*U6*V6-
     &        16.D0*V4*U6-16.D0*U4*V6-48.D0*U5*V6-48.D0*V5*U6+
     &        16.D0*U5*V2-36.D0*U5*V3+8.D0*V2*U3+4.D0*U2*V4+16.D0*U1*V6-
     &        3.D0*V2*U1-36.D0*U3*V6-3.D0*U2*V1+4.D0*V2*U4+4.D0*V4*U1-
     &        66.D0*U3*V3+16.D0*U2*V5-36.D0*U3*V5+16.D0*V1*U6)*Y3)*X2+
     &        (-96.D0*U5*V5-2.D0*V1*U1+8.D0*U2*V3+4.D0*U4*V1-
     &        36.D0*U6*V3+12.D0*V2*U6-4.D0*V4*U3+8.D0*V1*U3+8.D0*U1*V3-
     &        2.D0*U2*V2-16.D0*V4*U5+12.D0*U5*V1-16.D0*U4*V5+
     &        12.D0*U2*V6+12.D0*V5*U1-4.D0*U4*V3-96.D0*U6*V6-
     &        16.D0*V4*U6-16.D0*U4*V6-48.D0*U5*V6-48.D0*V5*U6+
     &        16.D0*U5*V2-36.D0*U5*V3+8.D0*V2*U3+4.D0*U2*V4)*Y2*X3
      ANS2 = (16.D0*U1*V6-3.D0*V2*U1-36.D0*U3*V6-3.D0*U2*V1+
     &        4.D0*V2*U4+4.D0*V4*U1-66.D0*U3*V3+16.D0*U2*V5-
     &        36.D0*U3*V5+16.D0*V1*U6)*Y2*X3+(72.D0*V3*V6+
     &        2.D0*V1**2+66.D0*V3**2+72.D0*V3*V5-32.D0*V1*V6-
     &        16.D0*V2*V3-32.D0*V5*V2+6.D0*V2*V1+96.D0*V5**2+
     &        96.D0*V6**2+ 32.D0*V4*V5+2.D0*V2**2+
     &        32.D0*V4*V6-8.D0*V2*V4+96.D0*V5*V6-24.D0*V5*V1+
     &        8.D0*V4*V3-24.D0*V2*V6-16.D0*V1*V3-8.D0*V4*V1)*X3*X2+
     &        (8.D0*V1*V6-8.D0*V4*V3+18.D0*V2**2-16.D0*V6**2+
     &        2.D0*V1**2-6.D0*V3**2+2.D0*V1*V3+8.D0*V2*V6+
     &        8.D0*V5*V1-32.D0*V5*V6+24.D0*V2*V4-48.D0*V5**2-
     &        8.D0*V3*V6-8.D0*V2*V1+48.D0*V4**2-16.D0*V3*V5+
     &        24.D0*V5*V2)*X2**2+(2.D0*U3*U1-8.D0*U4*U3-8.D0*U2*U1-
     &        32.D0*U5*U6+8.D0*U5*U1+24.D0*U2*U4+8.D0*U6*U1-
     &        8.D0*U6*U3+48.D0*U4**2-48.D0*U5**2+18.D0*U2**2+
     &        2.D0*U1**2-16.D0*U6**2-6.D0*U3**2+8.D0*U2*U6-
     &        16.D0*U3*U5+24.D0*U5*U2)*Y2**2
      A35(IELEM) = ((72.D0*U6*U3+96.D0*U5*U6- 24.D0*U5*U1-
     &             16.D0*U3*U1-8.D0*U4*U1+8.D0*U4*U3+32.D0*U4*U6+
     &             72.D0*U3*U5-32.D0*U6*U1+32.D0*U4*U5-32.D0*U5*U2+
     &             66.D0*U3**2-24.D0*U2*U6-8.D0*U2*U4-16.D0*U2*U3+
     &             96.D0*U5**2+2.D0*U1**2+96.D0*U6**2+6.D0*U2*U1+
     &             2.D0*U2**2)*Y3*Y2+ANS1+ANS2)*(-AUX630)
!
      ANS1 = ((4.D0*V1*V6+4.D0*V6**2+8.D0*V3*V5+4.D0*V2*V1+6.D0*V2*V3-
     &        12.D0*V4**2-36.D0*V5**2-8.D0*V5*V6-21.D0*V2**2+
     &        8.D0*V5*V1-V3**2-24.D0*V4*V5+V1**2+12.D0*V4*V1+
     &        4.D0*V4*V3-4.D0*V2*V6-24.D0*V2*V4-2.D0*V1*V3-
     &        24.D0*V5*V2)*X3+(-48.D0*U5*V5-2.D0*V1*U1+3.D0*U2*V3+
     &        6.D0*U4*V1+2.D0*U6*V3+6.D0*V4*U3-V1*U3-U1*V3-
     &        24.D0*U2*V2-24.D0*V4*U5+6.D0*U5*V1-24.D0*U4*V5+6.D0*V5*U1+
     &        6.D0*U4*V3-8.D0*U6*V6-12.D0*V4*U6-12.D0*U4*V6-12.D0*U5*V6-
     &        12.D0*V5*U6-12.D0*U5*V2+6.D0*U5*V3+3.D0*V2*U3-12.D0*U2*V4+
     &        2.D0*U1*V6+3.D0*V2*U1+2.D0*U3*V6+3.D0*U2*V1-12.D0*V2*U4+
     &        6.D0*V4*U1-2.D0*U3*V3-12.D0*U2*V5+6.D0*U3*V5-48.D0*U4*V4+
     &        2.D0*V1*U6)*Y2)*X2+(12.D0*V4*V5+6.D0*V4*V1-4.D0*V4*V3+
     &        10.D0*V1*V6+10.D0*V2**2-2.D0*V2*V3+16.D0*V6**2+6.D0*V2*V4+
     &        16.D0*V5**2+12.D0*V4*V6-4.D0*V3*V6+12.D0*V4**2+
     &        10.D0*V1**2-4.D0*V2*V1-2.D0*V2*V6-2.D0*V1*V3+8.D0*V5*V6-
     &        2.D0*V5*V1+V3**2+10.D0*V5*V2-4.D0*V3*V5)*X3**2+
     &        (24.D0*V4**2+V1**2-6.D0*V5*V1+12.D0*V2**2+24.D0*V5**2-
     &        2.D0*V1*V6+12.D0*V4*V6-3*V2*V1-3.D0*V2*V3+4.D0*V6**2+
     &        V1*V3+24.D0*V4*V5-6.D0*V4*V1+12.D0*V5*V6+V3**2-6.D0*V4*V3-
     &        6.D0*V3*V5+12.D0*V5*V2+12.D0*V2*V4-2.D0*V3*V6)*X2**2
      ANS2 = (-32.D0*U5*V5-20.D0*V1*U1+2.D0*U2*V3-6.D0*U4*V1+4.D0*U6*V3+
     &       2.D0*V2*U6+4.D0*V4*U3+2.D0*V1*U3+2.D0*U1*V3-20.D0*U2*V2-
     &       12.D0*V4*U5+2.D0*U5*V1-12.D0*U4*V5+2.D0*U2*V6+2.D0*V5*U1+
     &       4.D0*U4*V3-32.D0*U6*V6-12.D0*V4*U6-12.D0*U4*V6-8.D0*U5*V6-
     &       8.D0*V5*U6-10.D0*U5*V2+4.D0*U5*V3+2.D0*V2*U3-6.D0*U2*V4-
     &       10.D0*U1*V6+4.D0*V2*U1+4.D0*U3*V6+4.D0*U2*V1-6.D0*V2*U4-
     &       6.D0*V4*U1-2.D0*U3*V3-10.D0*U2*V5+4.D0*U3*V5-24.D0*U4*V4-
     &       10.D0*V1*U6)*Y3*X3+(U1**2+8.D0*U5*U1-8.D0*U5*U6+4.D0*U4*U3+
     &       6.D0*U2*U3+4.D0*U6*U1-24.D0*U4*U5-4*U2*U6-24.D0*U2*U4-
     &       12.D0*U4**2+8.D0*U3*U5+4.D0*U6**2+12.D0*U4*U1-2.D0*U3*U1-
     &       21.D0*U2**2-U3**2-36.D0*U5**2+4.D0*U2*U1-
     &       24.D0*U5*U2)*Y3*Y2+(U3**2-4.D0*U3*U5+12.D0*U4**2-
     &       2.D0*U2*U3+16.D0*U6**2+10.D0*U2**2+16.D0*U5**2-4.D0*U2*U1+
     &       12.D0*U4*U6-2.D0*U5*U1+6.D0*U4*U1+10.D0*U5*U2-4.D0*U6*U3-
     &       4.D0*U4*U3+8.D0*U5*U6-2.D0*U3*U1-2.D0*U2*U6+6.D0*U2*U4+
     &       12.D0*U4*U5+10.D0*U6*U1+10.D0*U1**2)*Y3**2+
     &       (36.D0*U5*V5-V1*U1-3.D0*U2*V3-6.D0*U4*V1+2.D0*V2*U6)*Y2*X3
      A44(IELEM) = ((-2.D0*V4*U3+ V1*U3+U1*V3+21.D0*U2*V2+12.D0*V4*U5-
     &             4.D0*U5*V1+12.D0*U4*V5+2.D0*U2*V6-4.D0*V5*U1-
     &             2.D0*U4*V3-4.D0*U6*V6+4.D0*U5*V6+4.D0*V5*U6+
     &             12.D0*U5*V2-4.D0*U5*V3-3.D0*V2*U3+12.D0*U2*V4-
     &             2.D0*U1*V6-2.D0*V2*U1-2.D0*U2*V1+12.D0*V2*U4-
     &             6.D0*V4*U1+U3*V3+12.D0*U2*V5-4.D0*U3*V5+12.D0*U4*V4-
     &             2.D0*V1*U6)*Y2*X3+(36.D0*U5*V5- V1*U1-3.D0*U2*V3-
     &             6.D0*U4*V1+2.D0*V2*U6-2.D0*V4*U3+V1*U3+U1*V3+
     &             21.D0*U2*V2+12.D0*V4*U5-4.D0*U5*V1+12.D0*U4*V5+
     &             2.D0*U2*V6-4.D0*V5*U1-2.D0*U4*V3-4.D0*U6*V6+
     &             4.D0*U5*V6+4.D0*V5*U6+12.D0*U5*V2-4.D0*U5*V3-
     &             3.D0*V2*U3+12.D0*U2*V4-2.D0*U1*V6-2.D0*V2*U1-
     &             2.D0*U2*V1+12.D0*V2*U4-6.D0*V4*U1+U3*V3+12.D0*U2*V5-
     &             4.D0*U3*V5+12.D0*U4*V4-2.D0*V1*U6)*Y3*X2+(U3*U1-
     &             2.D0*U6*U1+12.D0*U4*U6-3.D0*U2*U1+ 24.D0*U4**2+
     &             24.D0*U5**2+12.D0*U2*U4-6.D0*U5*U1-2.D0*U6*U3+U1**2+
     &             4.D0*U6**2+12.D0*U2**2+U3**2+24.D0*U4*U5-6.D0*U4*U3-
     &             6.D0*U4*U1+12.D0*U5*U6-3.D0*U2*U3-6.D0*U3*U5+
     &             12.D0*U5*U2)*Y2**2+ANS1+ANS2)*8.D0*AUX630
!
      ANS1 = (-2.D0*V2*U3-4.D0*V1*U1+4.D0*U6*V3-4.D0*V4*U1+4.D0*V4*U3+
     &       4.D0*U4*V3-4.D0*V5*U1-8.D0*V4*U6+4.D0*U3*V5-8.D0*U4*V6-
     &       4.D0*U5*V1-4.D0*U4*V1+8.D0*V5*U6+4.D0*U5*V3+4.D0*U3*V6+
     &       2.D0*U2*V1-2.D0*U2*V3+2.D0*V2*U1-48.D0*U4*V4-4.D0*U1*V6+
     &       8.D0*U5*V6-4.D0*V1*U6+48.D0*U5*V5+4.D0*U3*V3)*Y2*X2+
     &       (-56.D0*U5*V5-20.D0*V1*U1+U2*V3-8.D0*U4*V1+4.D0*U6*V3+
     &       4.D0*V2*U6+4.D0*V4*U3+2.D0*V1*U3+2.D0*U1*V3-4.D0*U2*V2-
     &       16.D0*V4*U5+4.D0*U5*V1-16.D0*U4*V5+4.D0*U2*V6+4.D0*V5*U1+
     &       4.D0*U4*V3-24.D0*U6*V6-16.D0*V4*U6-16.D0*U4*V6-
     &       16.D0*U5*V6-16.D0*V5*U6+V2*U3+4.D0*U2*V4-8.D0*U1*V6+
     &       2.D0*V2*U1+4.D0*U3*V6+2.D0*U2*V1+4.D0*V2*U4-8.D0*V4*U1-
     &       4.D0*U3*V3-24.D0*U4*V4-8.D0*V1*U6)*Y3*X2+((-56.D0*U5*V5-
     &       20.D0*V1*U1+U2*V3-8.D0*U4*V1+4.D0*U6*V3+4.D0*V2*U6+4*V4*U3+
     &       2.D0*V1*U3+2.D0*U1*V3-4.D0*U2*V2-16.D0*V4*U5+4.D0*U5*V1-
     &       16.D0*U4*V5+4.D0*U2*V6+4.D0*V5*U1+4.D0*U4*V3-24.D0*U6*V6-
     &       16.D0*V4*U6-16.D0*U4*V6-16.D0*U5*V6-16.D0*V5*U6+V2*U3+
     &       4.D0*U2*V4-8.D0*U1*V6+2.D0*V2*U1+4.D0*U3*V6+2.D0*U2*V1+
     &       4.D0*V2*U4-8.D0*V4*U1-4.D0*U3*V3-24.D0*U4*V4-
     &       8.D0*V1*U6)*Y2)*X3
      ANS2 = (4.D0*U5*V2+4.D0*U2*V5+48.D0*U5*V5-8.D0*V4*U6+4.D0*U2*V4-
     &       8.D0*U4*V6+2.D0*U1*V3+8.D0*U4*V5-2.D0*U2*V3+8.D0*V4*U5-
     &       4.D0*V1*U6+4.D0*V2*U4+2.D0*V1*U3-4.D0*V4*U1-4.D0*V5*U1-
     &       4.D0*U5*V1-4.D0*U4*V1+4.D0*U2*V2-4.D0*U1*V6+4.D0*V2*U6-
     &       2.D0*V2*U3-4.D0*V1*U1-48.D0*U6*V6+4.D0*U2*V6)*Y3*X3+
     &       (-4.D0*U3*U1+32.D0*U4*U5-8.D0*U5*U1+16.D0*U6*U1-2.D0*U2*U3+
     &       32.D0*U5*U6+32.D0*U4*U6+24.D0*U4**2+24.D0*U6**2-8.D0*U4*U3+
     &       20.D0*U1**2-8.D0*U2*U4+4.D0*U3**2-8.D0*U6*U3+4.D0*U2**2-
     &       8.D0*U2*U6+56.D0*U5**2-4.D0*U2*U1+16.D0*U4*U1)*Y3*Y2+
     &       (24.D0*U6**2+2.D0*U1**2-4.D0*U2*U4-2.D0*U3*U1-2.D0*U2**2+
     &       4.D0*U5*U1+4.D0*U4*U1+8.D0*U4*U6-8.D0*U4*U5-4.D0*U2*U6+
     &       4.D0*U6*U1-4.D0*U5*U2-24.D0*U5**2+2.D0*U2*U3)*Y3**2+
     &       (-8.D0*V3*V6+16.D0*V1*V6+32.D0*V5*V6+4.D0*V3**2+4.D0*V2**2-
     &       4.D0*V2*V1+56.D0*V5**2-2.D0*V2*V3+20.D0*V1**2-8.D0*V2*V4+
     &       32.D0*V4*V6+24.D0*V6**2-8.D0*V5*V1+32.D0*V4*V5+16.D0*V4*V1-
     &       8.D0*V4*V3-8.D0*V2*V6-4.D0*V1*V3+24.D0*V4**2)*X3*X2
      A46(IELEM) = ((2.D0*V2*V3-4.D0*V2*V6-2.D0*V1*V3+4.D0*V1*V6+
     &       8.D0*V4*V6+4.D0*V4*V1-2.D0*V2**2-8.D0*V4*V5-24.D0*V5**2+
     &       24.D0*V6**2+2.D0*V1**2-4.D0*V2*V4+4.D0*V5*V1-
     &       4.D0*V5*V2)*X3**2+(-4.D0*V4*V3-8.D0*V5*V6-2.D0*V2*V1-
     &       2.D0*V3**2+4.D0*V1*V6-24.D0*V5**2+4.D0*V5*V1+2.D0*V2*V3+
     &       8.D0*V4*V6+24.D0*V4**2-4.D0*V3*V6+4.D0*V4*V1-4.D0*V3*V5+
     &       2.D0*V1**2)*X2**2+(-2.D0*U2*U1+4.D0*U6*U1+4.D0*U5*U1-
     &       4.D0*U4*U3-24.D0*U5**2+8.D0*U4*U6-8.D0*U5*U6+24.D0*U4**2-
     &       4.D0*U6*U3-2.D0*U3**2+4.D0*U4*U1+2.D0*U1**2+2.D0*U2*U3-
     &       4.D0*U3*U5)*Y2**2+ANS1+ANS2)*4.D0*(-AUX630)
!
      ANS1 = 24.D0*V4**2*X2**2-48.D0*U5*Y2*V5*X2-V1*X2*U3*Y2+
     &       12.D0*U4*Y2*V4*X3+12.D0*U5*Y3*V4*X2+12.D0*V5*X2*U4*Y3+
     &       24.D0*U5**2*Y2**2-48.D0*U4*Y2*V4*X2+6.D0*U1*Y3*V6*X3+
     &       36.D0*U5*Y3*V5*X2+12.D0*V5*X3*U4*Y2-12.D0*U5*Y3*V4*X3+
     &       V2**2*X3**2+24.D0*V5**2*X3**2+36.D0*U5*Y2*V5*X3-
     &       12.D0*V5*X3*U4*Y3+12.D0*U5*Y2*V4*X3-24.D0*V5*X3*V4*X2+
     &       12.D0*V4*X2*U4*Y3-8.D0*V4*X3*U4*Y3+U2**2*Y3**2+
     &       4.D0*U4**2*Y3**2+4.D0*V1*X3*V6*X2-2.D0*V1*X3*U1*Y3+
     &       3.D0*U1*Y3*V3*X3+12.D0*V2**2*X2**2+6.D0*U2*Y3*V5*X3+
     &       6.D0*U4*Y2*V1*X2-12.D0*U4*Y2*V6*X2-2.D0*U1*Y3*V6*X2-
     &       12.D0*V4*X3*U6*Y3+4.D0*V4**2*X3**2-12.D0*U4*Y3*V6*X3+
     &       6.D0*V4*X2*U1*Y2-12.D0*V4*X2*U6*Y2+24.D0*U6**2*Y3**2-
     &       12.D0*U2*Y2*V4*X2+6.D0*U2*Y3*V6*X3+12.D0*U2**2*Y2**2+
     &       12.D0*U3**2*Y3**2-48.D0*V5*X3*U5*Y3+24.D0*U5*Y3**2*U6-
     &       24.D0*U5*Y2*U4*Y3+12.D0*V5*X2**2*V2+U1**2*Y3**2+
     &       V1**2*X3**2+12.D0*V3**2*X3**2+24.D0*V6**2*X3**2+
     &       12.D0*U5*Y2**2*U6-12.D0*V6**2*X3*X2-3.D0*V3**2*X3*X2+
     &       12.D0*U5*Y3**2*U3-3.D0*V2**2*X2*X3-12.D0*U6**2*Y2*Y3+
     &       24.D0*U5**2*Y3**2+24.D0*V5*X2**2*V4+12.D0*V2*X2**2*V4-
     &       6.D0*U5*Y2**2*U1-12.D0*V2*X2*U4*Y2+4.D0*U6**2*Y2**2
      ANS2 = -2.D0*V2*X3**2*V4+12.D0*U2*Y2**2*U4+12.D0*V5*X3**2*V3-
     &        3.D0*V2*X2**2*V3-6.D0*V4*X2**2*V3-24.D0*V2*X2*U2*Y2+
     &        12.D0*U3*Y3**2*U6+V1**2*X2**2-U1**2*Y3*Y2-
     &        2.D0*U1*Y2**2*U6+4.D0*V6**2*X2**2-6.D0*U5*Y3**2*U1-
     &        3.D0*V2*X3**2*V3-3.D0*U2**2*Y2*Y3-6.D0*V4*X2**2*V1-
     &        6.D0*V5*X2**2*V1-12.D0*V5*X2*U2*Y2-12.D0*U5*Y2*V2*X2-
     &        6.D0*V2*X3**2*V5-2.D0*V4*X3**2*V1-6.D0*V3*X2**2*V5-
     &        2.D0*V1*X2**2*V6+3.D0*U2*Y2*V3*X2+24.D0*V5**2*X2**2-
     &        3.D0*V1*X3**2*V3+3.D0*V2*X3*U3*Y3+V3**2*X2**2-
     &        6.D0*U4*Y2**2*U3+6.D0*V3*X2*U5*Y2+3.D0*U2*Y3*V3*X3-
     &        6.D0*U2*Y3**2*U6-6.D0*U3*Y2**2*U5+U2*Y3**2*U1-
     &        3.D0*U1*Y3**2*U3+12.D0*U4*Y3**2*U6-2.D0*V1*X2*U1*Y2+
     &        6.D0*V1*X3*U6*Y3-12.D0*U4**2*Y2*Y3-12.D0*V4**2*X3*X2+
     &        U1*Y2**2*U3+12.D0*V6*X3**2*V3+6.D0*U3*Y2*V5*X2-
     &        24.D0*V5*X2*U4*Y2-24.D0*U5*Y2*V4*X2-3.D0*U3**2*Y3*Y2+
     &        12.D0*U5*Y2**2*U2-V1**2*X3*X2+V2*X3**2*V1+
     &        3.D0*V2*X2*U3*Y2-6*U4*Y2**2*U1+V1*X2**2*V3+
     &        12.D0*V5*X2**2*V6-3.D0*U2*Y2**2*U3
      ANS3 = -6.D0*U2*Y3**2*U5-2.D0*U2*Y3**2*U4-6.D0*V5*X3**2*V1-
     &       36.D0*U5**2*Y2*Y3-36.D0*V5**2*X3*X2+12.D0*V5*X3**2*V4+
     &       12.D0*U5*Y3**2*U4+U3**2*Y2**2+6.D0*V2*X3*U5*Y3+
     &       12.D0*V6*X2*U6*Y3-8.D0*V6*X2*U6*Y2+2.D0*V6*X2*U3*Y2-
     &       2.D0*V3*X2**2*V6+2*U2*Y3*V4*X3+24.D0*V5*X3**2*V6-
     &       2.D0*V3*X2*U3*Y2+2.D0*V3*X2*U6*Y2+U1**2*Y2**2-
     &       U1*Y2*V3*X2+2.D0*V2*X3*U4*Y3+12.D0*V6*X3*U6*Y2+
     &       2.D0*U1*Y2*V6*X2+6.D0*V4*X2*U3*Y2+3.D0*V2*X2*U2*Y3+
     &       3.D0*V1*X3*U3*Y3+3.D0*U2*Y2*V2*X3-2.D0*U2*Y3*V2*X3+
     &       12.D0*V4*X3**2*V6-4.D0*U5*Y3*V1*X2+6.D0*U5*Y3*V1*X3-
     &       3.D0*U2*Y3**2*U3+4.D0*U1*Y3*U6*Y2+12.D0*U4*Y2**2*U6-
     &       12.D0*U5*Y3*V3*X3-2.D0*V1*X3*U6*Y2+24.D0*U4**2*Y2**2-
     &       24.D0*U5*Y2*U6*Y3-6.D0*V1*X3**2*V6+6.D0*U4*Y2*V3*X2+
     &       12.D0*V5*X3*U6*Y2+6.D0*V5*X3*U1*Y3+8.D0*V5*X3*V1*X2-
     &       12.D0*V5*X2*U6*Y2-6.D0*U1*Y3**2*U6+6.D0*V5*X2*U1*Y2+
     &       8.D0*U5*Y2*U1*Y3+24.D0*U5*Y2**2*U4+3.D0*V3*X3*U3*Y2+
     &       3.D0*V3*X2*U3*Y3-24.D0*V5*X3*V6*X2+V1*X2*U1*Y3-
     &       2.D0*V1*X2*U6*Y3-4.D0*V5*X3*U1*Y2-12.D0*V5*X3*U3*Y3-
     &       2.D0*U4*Y3**2*U1-24*V3*X3*U3*Y3
      ANS4 = -48.D0*V6*X3*U6*Y3-4.D0*U5*Y2*V1*X3+12.D0*V4*X2**2*V6-
     &       12.D0*V3*X3*U6*Y3+12.D0*U5*Y2*V6*X3+8.D0*U4*Y3*V6*X2-
     &       U2*Y3*V3*X2-2.D0*U4*Y3*V3*X2-2.D0*U4*Y3*V1*X2+
     &       2.D0*U4*Y3*V1*X3-2.D0*U1*Y2*V6*X3-12.D0*V6*X3*U3*Y3-
     &       2.D0*V2*X3*U6*Y2+4.D0*V4*X3*V1*X2-2.D0*V4*X2*U3*Y3-
     &       V2*X3*U3*Y2-2.D0*V4*X2*U1*Y3+4.D0*V2*X3*V6*X2-
     &       12.D0*U5*Y2*V6*X2-U2*Y3*V1*X3+4.D0*U2*Y3*U6*Y2+
     &       8.D0*V4*X2*U6*Y3+2.D0*U2*Y3*U3*Y2-2.D0*U2*Y3*V6*X2+
     &       2.D0*V1*X2*U6*Y2+8.D0*V4*X3*U6*Y2-2.D0*V4*X3*U3*Y2+
     &       2.D0*V2*X3*V3*X2-16.D0*V4*X3*V6*X2-2.D0*V4*X3*U1*Y2-
     &       6.D0*V2*X3**2*V6+6.D0*U5*Y2*V1*X2-V2*X3*U1*Y3+
     &       6.D0*V2*X3*U6*Y3-2.D0*V2*X2*U6*Y3+8.D0*U4*Y2*V6*X3-
     &       V2*X2*U3*Y3-2.D0*U3*Y2**2*U6+12.D0*V5*X2*U6*Y3+
     &       4.D0*V4*X3*V3*X2+2.D0*V4*X3*U1*Y3-16.D0*U4*Y2*U6*Y3+
     &       3.D0*U2*Y2*V1*X2+4.D0*U4*Y2*U3*Y3-2.D0*U4*Y2*V1*X3-
     &       2.D0*U4*Y2*V3*X3-U2*Y2*V3*X3-24.D0*U5*Y3*V6*X3-
     &       24.D0*V5*X3*U6*Y3-4.D0*V5*X2*U1*Y3-2.D0*U2*Y2*V6*X3+
     &       3.D0*V2*X2*U1*Y2+4.D0*U4*Y2*U1*Y3+12.D0*U5*Y3*V6*X2-
     &       3.D0*V2*X2**2*V1+V1*X3*U1*Y2-3.D0*U2*Y2**2*U1
        A55(IELEM) = (ANS1+ANS2+ANS3+ANS4)*8.D0*AUX630
!
!
! USES HERE THE 'MAGIC SQUARE' PROPERTIES TO GET THE REMAINING TERMS
! (SUM OF EACH LINE = SUM OF EACH COLUMN = 0)
!
        A16(IELEM) = - A11(IELEM) - A12(IELEM) - A13(IELEM)
     &               - A14(IELEM) - A15(IELEM)
!
        A25(IELEM) = - A12(IELEM) - A22(IELEM) - A23(IELEM)
     &               - A24(IELEM) - A26(IELEM)
!
        A36(IELEM) = - A13(IELEM) - A23(IELEM) - A33(IELEM)
     &               - A34(IELEM) - A35(IELEM)
!
        A45(IELEM) = - A14(IELEM) - A24(IELEM) - A34(IELEM)
     &               - A44(IELEM) - A46(IELEM)
!
        A56(IELEM) = - A15(IELEM) - A25(IELEM) - A35(IELEM)
     &               - A45(IELEM) - A55(IELEM)
!
        A66(IELEM) = - A16(IELEM) - A26(IELEM) - A36(IELEM)
     &                - A46(IELEM) - A56(IELEM)
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
        IF(IELMU.EQ.IELMV) THEN
        WRITE(LU,101) IELMU
101     FORMAT(1X,'MT04CC (BIEF) :',/,
     &         1X,'DISCRETIZATION OF U AND V : ',1I6,' NOT AVAILABLE')
        ELSE
        WRITE(LU,201) IELMU,IELMV
201     FORMAT(1X,'MT04CC (BIEF) :',/,
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
