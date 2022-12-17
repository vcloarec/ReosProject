!                   *****************
                    SUBROUTINE MT06TT
!                   *****************
!
     &( T,XM,XMUL,SF,F,X,Y,Z,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FMATMA MATRIX FOR TETRAHEDRONS.
!+
!+            THE VECTOR F CAN BE P0 OR P1.
!code
!+     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!+     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!+     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!+     XM(IELEM, 4)  ---->  M(2,3) = M(3,2)
!+     XM(IELEM, 5)  ---->  M(2,4) = M(4,2)
!+     XM(IELEM, 6)  ---->  M(3,4) = M(4,3)
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        10/01/06
!+        V5P6
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
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| Z              |-->| ELEVATIONS OF POINTS
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT06TT => MT06TT
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     SPECIFIC DECLARATIONS
!
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,F1,F2,F3,F4
      INTEGER I1,I2,I3,I4
      INTEGER IELMF,IELEM
!
      DOUBLE PRECISION XSUR720,JACOB
!
!***********************************************************************
!
!     DISCRETISES VECTOR F
      IELMF=SF%ELM
      XSUR720=XMUL/720.D0
!
!-----------------------------------------------------------------------
!
! CASE WHERE F IS P1 LINEAR
!
      IF (IELMF .EQ. 31) THEN
!     LOOP ON THE TETRAHEDRONS
!
          DO IELEM=1,NELEM
!
              I1=IKLE(IELEM,1)
              I2=IKLE(IELEM,2)
              I3=IKLE(IELEM,3)
              I4=IKLE(IELEM,4)
!
              F1 = F(I1)
              F2 = F(I2)
              F3 = F(I3)
              F4 = F(I4)
!
!-----------------------------------------------------------------------
!
              X2=X(I2)-X(I1)
              Y2=Y(I2)-Y(I1)
              Z2=Z(I2)-Z(I1)
              X3=X(I3)-X(I1)
              Y3=Y(I3)-Y(I1)
              Z3=Z(I3)-Z(I1)
              X4=X(I4)-X(I1)
              Y4=Y(I4)-Y(I1)
              Z4=Z(I4)-Z(I1)
!
!     JACOBIAN : Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3)
!
!     VOLUME OF THE TETRAHEDRON:
!
!     (Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))/6
!
              JACOB = (   Z2*(X3*Y4-X4*Y3)
     &                  + Y2*(X4*Z3-X3*Z4)
     &                  + X2*(Y3*Z4-Y4*Z3) ) * XSUR720
!
              T(IELEM,1)  = JACOB*(2*F3+6*F1+2*F4+2*F2)
              T(IELEM,2)  = JACOB*(2*F3+2*F1+2*F4+6*F2)
              T(IELEM,3)  = JACOB*2*(F1+3*F3+F2+F4)
              T(IELEM,4)  = JACOB*(2*F1+2*F3+6*F4+2*F2)
!
              XM(IELEM,1) = JACOB*(F3+2*F1+F4+2*F2)
              XM(IELEM,2) = JACOB*(2*F3+F2+2*F1+F4)
              XM(IELEM,3) = JACOB*(F2+F3+2*F4+2*F1)
              XM(IELEM,4) = JACOB*(F1+2*F2+F4+2*F3)
              XM(IELEM,5) = JACOB*(2*F4+F3+F1+2*F2)
              XM(IELEM,6) = JACOB*(F1+2*F3+F2+2*F4)
!
!-----------------------------------------------------------------------
!
          ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
! CASE WHERE F IS CONSTANT BY P0 ELEMENT
!
      ELSEIF (IELMF .EQ. 30) THEN
!     LOOP ON THE TETRAHEDRONS
!
          DO IELEM=1,NELEM
!
              I1=IKLE(IELEM,1)
              I2=IKLE(IELEM,2)
              I3=IKLE(IELEM,3)
              I4=IKLE(IELEM,4)
!
!             SAME VALUE FOR THE 4 FI
!             NOTE THAT THE COMPUTATIONS FOR T AND XM
!             COULD BE SIMPLIFIED ...
!
              F1 = F(IELEM)
              F2 = F1
              F3 = F1
              F4 = F1
!
!-----------------------------------------------------------------------
!
              X2=X(I2)-X(I1)
              Y2=Y(I2)-Y(I1)
              Z2=Z(I2)-Z(I1)
              X3=X(I3)-X(I1)
              Y3=Y(I3)-Y(I1)
              Z3=Z(I3)-Z(I1)
              X4=X(I4)-X(I1)
              Y4=Y(I4)-Y(I1)
              Z4=Z(I4)-Z(I1)
!
!     JACOBIAN : Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3)
!
!     VOLUME OF THE TETRAHEDRON:
!
!     (Z2*(X3*Y4-X4*Y3)+Y2*(X4*Z3-X3*Z4)+X2*(Y3*Z4-Y4*Z3))/6
!
              JACOB = (   Z2*(X3*Y4-X4*Y3)
     &                  + Y2*(X4*Z3-X3*Z4)
     &                  + X2*(Y3*Z4-Y4*Z3) ) * XSUR720
!
              T(IELEM,1)  = JACOB*(2*F3+6*F1+2*F4+2*F2)
              T(IELEM,2)  = JACOB*(2*F3+2*F1+2*F4+6*F2)
              T(IELEM,3)  = JACOB*2*(F1+3*F3+F2+F4)
              T(IELEM,4)  = JACOB*(2*F1+2*F3+6*F4+2*F2)
!
              XM(IELEM,1) = JACOB*(F3+2*F1+F4+2*F2)
              XM(IELEM,2) = JACOB*(2*F3+F2+2*F1+F4)
              XM(IELEM,3) = JACOB*(F2+F3+2*F4+2*F1)
              XM(IELEM,4) = JACOB*(F1+2*F2+F4+2*F3)
              XM(IELEM,5) = JACOB*(2*F4+F3+F1+2*F2)
              XM(IELEM,6) = JACOB*(F1+2*F3+F2+2*F4)
!
!-----------------------------------------------------------------------
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
      ELSE
!
          WRITE(LU,101) IELMF,SF%NAME
101       FORMAT(1X,'MT06TT (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &        1X,'REAL NAME: ',A6)
          CALL PLANTE(1)
          STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE MT06TT
