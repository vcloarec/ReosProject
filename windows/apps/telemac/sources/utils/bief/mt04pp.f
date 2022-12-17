!                   *****************
                    SUBROUTINE MT04PP
!                   *****************
!
     &( T,XM,XMUL,SU,SV,SW,U,V,X,Y,Z,SURFAC,IKLE,NELEM,NELMAX,FORMUL)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MATRIX U GRAG (PSII) U GRAD (PSIJ).
!code
!+    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!+
!+    MAUGUG2 :
!+
!+                            / ->                 ->
!+               A    = XMUL /  U   . GRAD  (P ) * U . GRAD  (P ) * J(X,Y) DXDY
!+                I J       /S    2D      2D  I     2D     2D  J
!+
!+
!+    MAUGUG1 : SEE COMPONENTS F0 AND G0
!+
!+                            / ->                 ->
!+               A    = XMUL /  F   . GRAD  (P ) * U . GRAD  (P ) * J(X,Y) DXDY
!+                I J       /S    2D      2D  I     2D     2D  J
!+
!+
!+    BY ELEMENTARY CELL; THE ELEMENT IS THE P1 PRISM
!+
!+    J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!note     SIMPLIFICATIONS: AN AVERAGE HEIGHT OF THE PRISM IS TAKEN.
!+                          VELOCITIES ARE CONSIDERED CONSTANT ON THE ELEMENT.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        07/07/2005
!+
!+
!
!history
!+        16/09/2005
!+        V5P6
!+   VERTICAL UPWIND REMOVED
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/01/2013
!+        V6P3
!+   XEL and YEL sent instead of XPT and YPT for X and Y.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| STRUCTURE OF FUNCTIONS U
!| SV             |-->| STRUCTURE OF FUNCTIONS V
!| SW             |-->| STRUCTURE OF FUNCTIONS W
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| U              |-->| FUNCTION USED IN THE FORMULA
!| V              |-->| FUNCTION USED IN THE FORMULA
!| X              |-->| ABSCISSAE OF POINTS, PER ELEMENT
!| Y              |-->| ORDINATES OF POINTS, PER ELEMENT
!| Z              |-->| ELEVATIONS OF POINTS, STILL PER POINTS !!!
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT04PP => MT04PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,30)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!     STRUCTURES OF      U, V, W
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV,SW
!
      DOUBLE PRECISION, INTENT(IN) :: X(NELMAX,6),Y(NELMAX,6),Z(*)
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION U0,V0,F0,G0,HH,C,DX,SURNORMU
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM
!
      INTRINSIC SQRT
!
!**********************************************************************
!
      IF(SU%ELM.NE.41) THEN
        WRITE(LU,1001) SU%ELM
1001    FORMAT(1X,'MT04PP (BIEF) : TYPE OF U NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SV%ELM.NE.41) THEN
        WRITE(LU,2001) SV%ELM
2001    FORMAT(1X,'MT04PP (BIEF) : TYPE OF V NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!     HERE WSCONV WHICH IS IN FACT DEFINED PER LAYER
      IF(SW%ELM.NE.41) THEN
        WRITE(LU,3001) SW%ELM
3001    FORMAT(1X,'MT04PP (BIEF) : TYPE OF W NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(FORMUL(1:7).EQ.'MAUGUG2') THEN
!
!     LOOP ON THE 3D ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      I1 = IKLE(IELEM,1)
      I2 = IKLE(IELEM,2)
      I3 = IKLE(IELEM,3)
      I4 = IKLE(IELEM,4)
      I5 = IKLE(IELEM,5)
      I6 = IKLE(IELEM,6)
!
!     X2  =  X(I2) - X(I1)
!     X3  =  X(I3) - X(I1)
!     Y2  =  Y(I2) - Y(I1)
!     Y3  =  Y(I3) - Y(I1)
      X2  =  X(IELEM,2)
      X3  =  X(IELEM,3)
      Y2  =  Y(IELEM,2)
      Y3  =  Y(IELEM,3)
!
      U0 = (U(I1)+U(I2)+U(I3)+U(I4)+U(I5)+U(I6))/6.D0
      V0 = (V(I1)+V(I2)+V(I3)+V(I4)+V(I5)+V(I6))/6.D0
!     AVERAGE HEIGHT OF PRISM
      HH = (Z(I4)-Z(I1)+Z(I5)-Z(I2)+Z(I6)-Z(I3))/3.D0
      C  = XMUL*HH/24.D0/SURFAC(IELEM)
!
      T(IELEM,1)=2*C*(U0*Y3-U0*Y2-V0*X3+V0*X2)**2
      T(IELEM,2)=2*C*(-U0*Y3+V0*X3)**2
      T(IELEM,3)=2*C*(-U0*Y2+V0*X2)**2
      T(IELEM,4)=T(IELEM,1)
      T(IELEM,5)=T(IELEM,2)
      T(IELEM,6)=T(IELEM,3)
!
      XM(IELEM,01)= 2*(-U0*Y3+V0*X3)*(U0*Y3-U0*Y2-V0*X3+V0*X2)*C
      XM(IELEM,02)=-2*(-U0*Y2+V0*X2)*(U0*Y3-U0*Y2-V0*X3+V0*X2)*C
      XM(IELEM,03)=(U0*Y3-U0*Y2-V0*X3+V0*X2)*(U0*Y3-U0*Y2-V0*X3+V0*X2)*C
      XM(IELEM,04)=(-U0*Y3+V0*X3)*(U0*Y3-U0*Y2-V0*X3+V0*X2)*C
      XM(IELEM,05)=-(-U0*Y2+V0*X2)*(U0*Y3-U0*Y2-V0*X3+V0*X2)*C
      XM(IELEM,06)=-2*(-U0*Y2+V0*X2)*(-U0*Y3+V0*X3)*C
      XM(IELEM,07)=(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-U0*Y3+V0*X3)*C
      XM(IELEM,08)=(-U0*Y3+V0*X3)*(-U0*Y3+V0*X3)*C
      XM(IELEM,09)=-(-U0*Y2+V0*X2)*(-U0*Y3+V0*X3)*C
      XM(IELEM,10)=-(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-U0*Y2+V0*X2)*C
      XM(IELEM,11)=-(-U0*Y3+V0*X3)*(-U0*Y2+V0*X2)*C
      XM(IELEM,12)=(-U0*Y2+V0*X2)*(-U0*Y2+V0*X2)*C
      XM(IELEM,13)= 2*(-U0*Y3+V0*X3)*(U0*Y3-U0*Y2-V0*X3+V0*X2)*C
      XM(IELEM,14)=-2*(-U0*Y2+V0*X2)*(U0*Y3-U0*Y2-V0*X3+V0*X2)*C
      XM(IELEM,15)=-2*(-U0*Y2+V0*X2)*(-U0*Y3+V0*X3)*C
!
      ENDDO
!
      ELSEIF(FORMUL(1:7).EQ.'MAUGUG1') THEN
!
      DO IELEM = 1 , NELEM
!
      I1 = IKLE(IELEM,1)
      I2 = IKLE(IELEM,2)
      I3 = IKLE(IELEM,3)
      I4 = IKLE(IELEM,4)
      I5 = IKLE(IELEM,5)
      I6 = IKLE(IELEM,6)
!
!     X2  =  X(I2) - X(I1)
!     X3  =  X(I3) - X(I1)
!     Y2  =  Y(I2) - Y(I1)
!     Y3  =  Y(I3) - Y(I1)
      X2  =  X(IELEM,2)
      X3  =  X(IELEM,3)
      Y2  =  Y(IELEM,2)
      Y3  =  Y(IELEM,3)
!
!     VELOCITIES CONSIDERED CONSTANT
      U0 = (U(I1)+U(I2)+U(I3)+U(I4)+U(I5)+U(I6))/6.D0
      V0 = (V(I1)+V(I2)+V(I3)+V(I4)+V(I5)+V(I6))/6.D0
!
      SURNORMU=1.D0/MAX(SQRT(U0**2+V0**2),1.D-8)
      DX=SQRT(2.D0*SURFAC(IELEM))
!     HERE F0 AND G0 MUST BE
!     DX*U/(2*NORME(U)) AND DY*V/(2*NORME(U)) BUT CONSIDERS DY=DX
!     APPROXIMATED BY SQUARE ROOT OF (2*TRIANGLE AREA)
      F0 = 0.5D0*DX*U0*SURNORMU
      G0 = 0.5D0*DX*V0*SURNORMU
!     AVERAGE HEIGHT OF PRISM
      HH = (Z(I4)-Z(I1)+Z(I5)-Z(I2)+Z(I6)-Z(I3))/3.D0
      C  = XMUL*HH/24.D0/SURFAC(IELEM)
!
      T(IELEM,1)=2*(U0*Y3-U0*Y2-V0*X3+V0*X2)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      T(IELEM,2)=2*(-U0*Y3+V0*X3)*(-F0*Y3+G0*X3)*C
      T(IELEM,3)=2*(-U0*Y2+V0*X2)*(-F0*Y2+G0*X2)*C
      T(IELEM,4)=T(IELEM,1)
      T(IELEM,5)=T(IELEM,2)
      T(IELEM,6)=T(IELEM,3)
!
      XM(IELEM,01)=2*(-U0*Y3+V0*X3)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,02)=-2*(-U0*Y2+V0*X2)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,03)=(U0*Y3-U0*Y2-V0*X3+V0*X2)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,04)=(-U0*Y3+V0*X3)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,05)=-(-U0*Y2+V0*X2)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,06)=-2*(-U0*Y2+V0*X2)*(-F0*Y3+G0*X3)*C
      XM(IELEM,07)=(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y3+G0*X3)*C
      XM(IELEM,08)=(-U0*Y3+V0*X3)*(-F0*Y3+G0*X3)*C
      XM(IELEM,09)=-(-U0*Y2+V0*X2)*(-F0*Y3+G0*X3)*C
      XM(IELEM,10)=-(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y2+G0*X2)*C
      XM(IELEM,11)=-(-U0*Y3+V0*X3)*(-F0*Y2+G0*X2)*C
      XM(IELEM,12)=(-U0*Y2+V0*X2)*(-F0*Y2+G0*X2)*C
      XM(IELEM,13)=2*(-U0*Y3+V0*X3)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,14)=-2*(-U0*Y2+V0*X2)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,15)=-2*(-U0*Y2+V0*X2)*(-F0*Y3+G0*X3)*C
!
      XM(IELEM,16)= 2*(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y3+G0*X3)*C
      XM(IELEM,17)= -2*(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y2+G0*X2)*C
      XM(IELEM,21)= -2*(-U0*Y3+V0*X3)*(-F0*Y2+G0*X2)*C
      XM(IELEM,18)=(U0*Y3-U0*Y2-V0*X3+V0*X2)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,22)= (-U0*Y3+V0*X3)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,25)= -(-U0*Y2+V0*X2)*(F0*Y3-F0*Y2-G0*X3+G0*X2)*C
      XM(IELEM,19)= (U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y3+G0*X3)*C
      XM(IELEM,23)= (-U0*Y3+V0*X3)*(-F0*Y3+G0*X3)*C
      XM(IELEM,26)= -(-U0*Y2+V0*X2)*(-F0*Y3+G0*X3)*C
      XM(IELEM,28)= 2*(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y3+G0*X3)*C
      XM(IELEM,20)= -(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y2+G0*X2)*C
      XM(IELEM,24)= -(-U0*Y3+V0*X3)*(-F0*Y2+G0*X2)*C
      XM(IELEM,27)= (-U0*Y2+V0*X2)*(-F0*Y2+G0*X2)*C
      XM(IELEM,29)= -2*(U0*Y3-U0*Y2-V0*X3+V0*X2)*(-F0*Y2+G0*X2)*C
      XM(IELEM,30)= -2*(-U0*Y3+V0*X3)*(-F0*Y2+G0*X2)*C
!
      ENDDO
!
      ELSE
        WRITE(LU,4001) FORMUL
4001    FORMAT(1X,'MT04PP (BIEF) : UNEXPECTED FORMULA: ',A16)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
