!                   *****************
                    SUBROUTINE MT02TT
!                   *****************
!
     &(T,XM,XMUL,SF,SG,SH,F,G,H,X,Y,Z,IKLE,NELEM,NELMAX,NPOIN2)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE DIFFUSION MATRIX FOR TETRAHEDRONS.
!+
!+            THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P1
!+                DIAGONAL TENSOR.
!
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
!+        28/11/94
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
!history  J-M HERVOUET (LNHE)
!+        09/12/2011
!+        V6P2
!+   Element 51 now treated separately.
!
!history  U.H. Merkel
!+        18/07/2012
!+        V6P2
!+    Replaced EPSILON with CHOUIA due to nag compiler problems
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| G              |-->| FUNCTION USED IN THE FORMULA
!| H              |-->| FUNCTION USED IN THE FORMULA
!| IKLE           |-->| CONNECTIVITY TABLE.
!| INCHYD         |-->| IF YES, TREATS HYDROSTATIC INCONSISTENCIES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN UNDERLYING 2D MESH
!|                |   | (CASE OF ELEMENT 51, PRISMS CUT INTO TETRAHEDRA)
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SG             |-->| STRUCTURE OF FUNCTIONS G
!| SH             |-->| STRUCTURE OF FUNCTIONS H
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| X              |-->| ABSCISSAE OF POINTS
!| Y              |-->| ORDINATES OF POINTS
!| Z              |-->| ELEVATIONS OF POINTS
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT02TT => MT02TT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN2
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,4),XM(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*)
!
!     STRUCTURES OF F, G, H
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SH
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     SPECIFIC DECLARATIONS
!
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,VOL6
      INTEGER I1,I2,I3,I4,IELEM,ISO,IL1,IL2,IL3,IL4,ILOW
!
      DOUBLE PRECISION COEF,SUR24,DZ1,DZ2,DZ3,DZ4
      DOUBLE PRECISION FTOT,GTOT,HTOT,VTOT,WTOT,FGTOT,GHTOT,FHTOT
      DOUBLE PRECISION T1,T3,T5,T7,T9,T11,T13,T15,T17,T19,T21,T23
      DOUBLE PRECISION T35,T49,T28,T42,T51,T54
      DOUBLE PRECISION AUX,AUXX,AUXXX
      DOUBLE PRECISION AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AUX8,AUX9
!
      DOUBLE PRECISION, PARAMETER :: CHOUIA = 1.D-4
!
!***********************************************************************
!
      SUR24=1.D0/24.D0
      ISO = SF%DIM2
!
!     SEE VISCLM OF TELEMAC-3D
!     FOR THE TREATMENT OF P0 VERTICAL VISCOSITY ON THE VERTICAL
!
      IF(SH%DIMDISC.EQ.0) THEN
!       P1 VERTICAL VISCOSITY
!       NPOU0=SH%DIM1/NPLAN
!     ELSEIF(SH%DIMDISC.EQ.4111) THEN
!       P0 VERTICAL VISCOSITY ON THE VERTICAL (SEE II4,5,6)
!       NPOU0=0
      ELSE
        WRITE(LU,4001) SH%DIMDISC
4001    FORMAT(1X,'MT02TT (BIEF): DIMDISC OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     TRUE TETRAHEDRA
!
      IF(SF%ELM.EQ.31.AND.SG%ELM.EQ.31.AND.
     &   SH%ELM.EQ.31.AND.ISO.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!   LINEAR DISCRETISATION OF DIFFUSION COEFFICIENTS
!
!   LOOP ON THE TETRAHEDRONS
!
      DO IELEM=1,NELEM
!
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
!
!-----------------------------------------------------------------------
!
! VISCOSITY ALONG X Y AND Z
!
      HTOT=F(I1)+F(I2)+F(I3)+F(I4)
      VTOT=G(I1)+G(I2)+G(I3)+G(I4)
      WTOT=H(I1)+H(I2)+H(I3)+H(I4)
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
!-----------------------------------------------------------------------
!    COEF:  THANKS MAPLE...
!-----------------------------------------------------------------------
!
      T1  = X2*Y3
      T3  = X2*Y4
      T5  = X3*Y2
      T7  = X4*Y2
      T9  = X3*Z2
      T11 = X4*Z2
!     T13 = 4 TIMES THE TETRAHEDRON VOLUME ?
      T13 = T1*Z4-T3*Z3-T5*Z4+T7*Z3+T9*Y4-T11*Y3
!
      T15 = -Y2*Z3+Y3*Z2
      T17 =  X2*Z4-T11
      T19 = -Y3*Z4+Y4*Z3
      T21 =  X2*Z3-T9
      T23 = -Y2*Z4+Y4*Z2
      T35 =  X3*Z4-X4*Z3
      T49 =  X3*Y4-X4*Y3
!
!     BEWARE, PROBLEM WITH TIDAL FLATS HERE
!
      COEF=XMUL*SUR24/MAX(T13,1.D-10)
!     COEF=XMUL*SUR24/T13
!
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
!
      T(IELEM,1) =COEF*(HTOT*T28**2+VTOT*T42**2+WTOT*T54**2)
      T(IELEM,2) =COEF*(HTOT*T19**2+VTOT*T35**2+WTOT*T49**2)
      T(IELEM,3) =COEF*(HTOT*T23**2+VTOT*T17**2+WTOT*T51**2)
      XM(IELEM,1)=COEF*(HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      XM(IELEM,2)=COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      XM(IELEM,3)=-(XM(IELEM,2)+XM(IELEM,1)+T(IELEM,1))
      XM(IELEM,4)=COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
      XM(IELEM,5)= -(XM(IELEM,4)+T(IELEM,2)+XM(IELEM,1))
      XM(IELEM,6)= -(T(IELEM,3)+XM(IELEM,4)+XM(IELEM,2))
      T(IELEM,4) = -(XM(IELEM,3)+XM(IELEM,5)+XM(IELEM,6))
!
!-----------------------------------------------------------------------
!
      ENDDO
!
!     TETRAHEDRA FORMING PRISMS
!
      ELSEIF(SF%ELM.EQ.51.AND.SG%ELM.EQ.51.AND.SH%ELM.EQ.51.AND.
     &                                                    ISO.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!   LINEAR DISCRETISATION OF DIFFUSION COEFFICIENTS
!
!   LOOP ON THE TETRAHEDRONS
!
      DO IELEM=1,NELEM
!
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
!
!-----------------------------------------------------------------------
!
! VISCOSITY ALONG X, Y, AND Z
!
!     IL1: PLANE NUMBER OF POINT 1 MINUS 1 (0 FOR FIRST PLANE), ETC.
      IL1=(I1-1)/NPOIN2
      IL2=(I2-1)/NPOIN2
      IL3=(I3-1)/NPOIN2
      IL4=(I4-1)/NPOIN2
!     RANK OF LOWER PLANE MINUS 1
      ILOW=MIN(IL1,IL2,IL3,IL4)
!     NOW IL1=0 IF POINT AT THE BACK OF ORIGINAL PRISM
!         IL1=1 IF POINT AT THE TOP OF ORIGINAL PRISM ,ETC. FOR IL2...
      IL1=IL1-ILOW
      IL2=IL2-ILOW
      IL3=IL3-ILOW
      IL4=IL4-ILOW
!     DZ1, DZ2, DZ3, DZ4 WILL BE THE DELTA(Z) ABOVE OR BELOW EVERY POINT IN
!     THE ORIGINAL PRISM
      IF(IL1.EQ.0) THEN
        DZ1=Z(I1+NPOIN2)-Z(I1)
      ELSE
        DZ1=Z(I1)-Z(I1-NPOIN2)
      ENDIF
      IF(IL2.EQ.0) THEN
        DZ2=Z(I2+NPOIN2)-Z(I2)
      ELSE
        DZ2=Z(I2)-Z(I2-NPOIN2)
      ENDIF
      IF(IL3.EQ.0) THEN
        DZ3=Z(I3+NPOIN2)-Z(I3)
      ELSE
        DZ3=Z(I3)-Z(I3-NPOIN2)
      ENDIF
      IF(IL4.EQ.0) THEN
        DZ4=Z(I4+NPOIN2)-Z(I4)
      ELSE
        DZ4=Z(I4)-Z(I4-NPOIN2)
      ENDIF
!
!     SEE MT02PP FOR A SIMILAR LIMITATION
!     DIFFUSION CONNECTIONS CUT IF ONE POINT IS CRUSHED IN THE ELEMENT
!     THIS IS NECESSARY TO AVOID A MASS ERROR
!
      IF(DZ1.LT.CHOUIA.OR.DZ2.LT.CHOUIA.OR.
     &   DZ3.LT.CHOUIA.OR.DZ4.LT.CHOUIA ) THEN
        HTOT=0.D0
        VTOT=0.D0
        WTOT=0.D0
      ELSE
        HTOT=F(I1)+F(I2)+F(I3)+F(I4)
        VTOT=G(I1)+G(I2)+G(I3)+G(I4)
        WTOT=H(I1)+H(I2)+H(I3)+H(I4)
      ENDIF
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
!-----------------------------------------------------------------------
!
      VOL6 = X2*(Y3*Z4-Y4*Z3)+Y2*(X4*Z3-X3*Z4)+Z2*(X3*Y4-X4*Y3)
!
      T15 = -Y2*Z3+Y3*Z2
      T17 =  X2*Z4-X4*Z2
      T19 = -Y3*Z4+Y4*Z3
      T21 =  X2*Z3-X3*Z2
      T23 = -Y2*Z4+Y4*Z2
      T35 =  X3*Z4-X4*Z3
      T49 =  X3*Y4-X4*Y3
!
!     BEWARE, PROBLEM WITH TIDAL FLATS HERE
!
      COEF=XMUL*SUR24/MAX(VOL6,1.D-10)
!
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = X2*Y4-X4*Y2
      T54 = T49-X2*Y4+X4*Y2+X2*Y3-X3*Y2
!
      T(IELEM,1) =COEF*( HTOT*T28**2+VTOT*T42**2+WTOT*T54**2)
      T(IELEM,2) =COEF*( HTOT*T19**2+VTOT*T35**2+WTOT*T49**2)
      T(IELEM,3) =COEF*( HTOT*T23**2+VTOT*T17**2+WTOT*T51**2)
      XM(IELEM,1)=COEF*( HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      XM(IELEM,2)=COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      XM(IELEM,3)=-(XM(IELEM,2)+XM(IELEM,1)+T(IELEM,1))
      XM(IELEM,4)=COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
      XM(IELEM,5)= -(XM(IELEM,4)+T(IELEM,2)+XM(IELEM,1))
      XM(IELEM,6)= -(T(IELEM,3)+XM(IELEM,4)+XM(IELEM,2))
      T(IELEM,4) = -(XM(IELEM,3)+XM(IELEM,5)+XM(IELEM,6))
!
!-----------------------------------------------------------------------
!
      ENDDO
!
      ELSEIF(SF%ELM.EQ.30.AND.SG%ELM.EQ.30.AND.SH%ELM.EQ.30.AND.
     &       ISO.EQ.1) THEN
!
!
!-----------------------------------------------------------------------
!
!   P0 DISCRETISATION OF DIFFUSION COEFFICIENTS (CONSTANT ON AN ELEMENT)
!
!   LOOP ON THE PRISMS
!
      DO IELEM=1,NELEM
!
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
!
!     ONLY DIFFERENCE WITH LOOP 20
      HTOT=4*F(IELEM)
      VTOT=4*G(IELEM)
      WTOT=4*H(IELEM)
!     END OF ONLY DIFFERENCE WITH LOOP 20
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
!-----------------------------------------------------------------------
!    COEF:  THANKS MAPLE...
!-----------------------------------------------------------------------
!
      T1  = X2*Y3
      T3  = X2*Y4
      T5  = X3*Y2
      T7  = X4*Y2
      T9  = X3*Z2
      T11 = X4*Z2
!     T13 = 4 TIMES THE TETRAHEDRON VOLUME ?
      T13 = T1*Z4-T3*Z3-T5*Z4+T7*Z3+T9*Y4-T11*Y3
!
      T15 = -Y2*Z3+Y3*Z2
      T17 =  X2*Z4-T11
      T19 = -Y3*Z4+Y4*Z3
      T21 =  X2*Z3-T9
      T23 = -Y2*Z4+Y4*Z2
      T35 =  X3*Z4-X4*Z3
      T49 =  X3*Y4-X4*Y3
!
!     IF WIDTH MORE THAN 0.01 M
!
!     BEWARE, PROBLEM WITH TIDAL FLAT HERE
!
!      COEF=XMUL*SUR24/MAX(T13,0.01D0*SURF)
!      COEF=XMUL*SUR24/MAX(T13,1.D-3)
      COEF=XMUL*SUR24/MAX(T13,1.D-10)
!     COEF=XMUL*SUR24/T13
!
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
!
      T(IELEM,1) =COEF*(HTOT*T28**2+VTOT*T42**2+WTOT*T54**2)
      T(IELEM,2) =COEF*(HTOT*T19**2+VTOT*T35**2+WTOT*T49**2)
      T(IELEM,3) =COEF*(HTOT*T23**2+VTOT*T17**2+WTOT*T51**2)
      XM(IELEM,1)=COEF*(HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      XM(IELEM,2)=COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      XM(IELEM,3)=-(XM(IELEM,2)+XM(IELEM,1)+T(IELEM,1))
      XM(IELEM,4)=COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
      XM(IELEM,5)= -(XM(IELEM,4)+T(IELEM,2)+XM(IELEM,1))
      XM(IELEM,6)= -(T(IELEM,3)+XM(IELEM,4)+XM(IELEM,2))
      T(IELEM,4) = -(XM(IELEM,3)+XM(IELEM,5)+XM(IELEM,6))
!
!---------------------------------------------------------------
!
      ENDDO ! IELEM
!
      ELSEIF(SF%ELM.EQ.30.AND.ISO.EQ.6) THEN
!
!   P0 DISCRETISATION OF DIFFUSION COEFFICIENTS (CONSTANT ON AN ELEMENT)
!   NONISOTROPIC VISCOSITY ==> 6 COMPONENTS
!   THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P0 NON DIAGONAL
!   SYMMETRICAL TENSOR
!
!                / DXX DXY DXZ \   / F FG FH \
!            D = | DYX DYY DYZ | = | X  G GH |
!                \ DZX DZY DZZ /   \ X XX  H /
!-----------------------------------------------------------------------
!
!   LOOP ON THE TETRAHEDRONS
!
      DO IELEM=1,NELEM
!
      I1=IKLE(IELEM,1)
      I2=IKLE(IELEM,2)
      I3=IKLE(IELEM,3)
      I4=IKLE(IELEM,4)
!
      FTOT = 4.D0*F(IELEM          )
      GTOT = 4.D0*F(IELEM +   NELEM)
      HTOT = 4.D0*F(IELEM + 2*NELEM)
      GHTOT= 4.D0*F(IELEM + 3*NELEM)
      FHTOT= 4.D0*F(IELEM + 4*NELEM)
      FGTOT= 4.D0*F(IELEM + 5*NELEM)
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
!-----------------------------------------------------------------------
!    COEF:  THANKS MAPLE...
!-----------------------------------------------------------------------
!
      T13 = X2*Y3*Z4-X2*Y4*Z3-Y2*X3*Z4+Y2*X4*Z3+Z2*X3*Y4-Z2*X4*Y3
!
      AUX   = Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3
      AUXX  = X3*Z4-X4*Z3-X2*Z4+Z2*X4+X2*Z3-Z2*X3
      AUXXX = X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3
      AUX1 = FTOT*(Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3)
      AUX2 = FGTOT*(-X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3)
      AUX3 = FHTOT*(X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3)
      AUX4 = GTOT*(-X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3)
      AUX5 = GHTOT*(X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3)
      AUX6 = FGTOT*(Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3)
      AUX7 = HTOT*(X3*Y4-X4*Y3-X2*Y4+Y2*X4+X2*Y3-Y2*X3)
      AUX8 = FHTOT*(Y3*Z4-Y4*Z3-Y2*Z4+Z2*Y4+Y2*Z3-Z2*Y3)
      AUX9 = GHTOT*(-X3*Z4+X4*Z3+X2*Z4-Z2*X4-X2*Z3+Z2*X3)
!
      COEF=XMUL*SUR24/MAX(T13,1.D-14)
!
      T(IELEM,1) =COEF*(AUX*(AUX1+AUX2+AUX3)
     & -AUXX*(AUX4+AUX5+AUX6)+AUXXX*(AUX7+AUX8+AUX9))
      T(IELEM,2) =COEF*((Y3*Z4-Y4*Z3)*
     & (FTOT*(Y3*Z4-Y4*Z3)+FGTOT*(-X3*Z4+X4*Z3)+FHTOT*(X3*Y4-X4*Y3))
     & -(X3*Z4-X4*Z3)*(GTOT*(-X3*Z4+X4*Z3)+FGTOT*(Y3*Z4-Y4*Z3)
     & +GHTOT*(X3*Y4-X4*Y3))+(X3*Y4-X4*Y3)*(HTOT*(X3*Y4-X4*Y3)
     & +FHTOT*(Y3*Z4-Y4*Z3)+GHTOT*(-X3*Z4+X4*Z3)))
      T(IELEM,3) =COEF*((Y2*Z4-Z2*Y4)*(FTOT*(Y2*Z4-Z2*Y4)+
     & FGTOT*(-X2*Z4+Z2*X4)+FHTOT*(X2*Y4-Y2*X4))-
     & (X2*Z4-Z2*X4)*(GTOT*(-X2*Z4+Z2*X4)+FGTOT*(Y2*Z4-Z2*Y4)+
     & GHTOT*(X2*Y4-Y2*X4))+(X2*Y4-Y2*X4)*(HTOT*(X2*Y4-Y2*X4)+
     & FHTOT*(Y2*Z4-Z2*Y4)+GHTOT*(-X2*Z4+Z2*X4)))
      T(IELEM,4) =COEF*((Y2*Z3-Z2*Y3)*(FTOT*(Y2*Z3-Z2*Y3)+
     & FGTOT*(-X2*Z3+Z2*X3)+FHTOT*(X2*Y3-Y2*X3))-
     & (X2*Z3-Z2*X3)*(GTOT*(-X2*Z3+Z2*X3)+FGTOT*(Y2*Z3-Z2*Y3)+
     & GHTOT*(X2*Y3-Y2*X3))+(X2*Y3-Y2*X3)*(HTOT*(X2*Y3-Y2*X3)+
     & FHTOT*(Y2*Z3-Z2*Y3)+GHTOT*(-X2*Z3+Z2*X3)))
      XM(IELEM,1)=COEF*((X3*Z4-X4*Z3)*(AUX4+AUX5+AUX6)-
     & (Y3*Z4-Y4*Z3)*(AUX1+AUX2+AUX3)-(X3*Y4-X4*Y3)*(AUX7+AUX8+AUX9))
      XM(IELEM,2)=COEF*((Y2*Z4-Z2*Y4)*(AUX1+AUX2+AUX3)-
     & (X2*Z4-Z2*X4)*(AUX4+AUX5+AUX6)+(X2*Y4-Y2*X4)*(AUX7+AUX8+AUX9))
      XM(IELEM,3)=COEF*((X2*Z3-Z2*X3)*(AUX4+AUX5+AUX6)-
     & (Y2*Z3-Z2*Y3)*(AUX1+AUX2+AUX3)-(X2*Y3-Y2*X3)*(AUX7+AUX8+AUX9))
      XM(IELEM,4)=COEF*((X2*Z4-Z2*X4)*(GTOT*(-X3*Z4+X4*Z3)+
     & FGTOT*(Y3*Z4-Y4*Z3)+GHTOT*(X3*Y4-X4*Y3))-
     & (Y2*Z4-Z2*Y4)*(FTOT*(Y3*Z4-Y4*Z3)+FGTOT*(-X3*Z4+X4*Z3)+
     & FHTOT*(X3*Y4-X4*Y3))-(X2*Y4-Y2*X4)*(HTOT*(X3*Y4-X4*Y3)+
     & FHTOT*(Y3*Z4-Y4*Z3)+GHTOT*(-X3*Z4+X4*Z3)))
      XM(IELEM,5)=COEF*((Y2*Z3-Z2*Y3)*(FTOT*(Y3*Z4-Y4*Z3)+
     & FGTOT*(-X3*Z4+X4*Z3)+FHTOT*(X3*Y4-X4*Y3))-
     & (X2*Z3-Z2*X3)*(GTOT*(-X3*Z4+X4*Z3)+FGTOT*(Y3*Z4-Y4*Z3)
     & +GHTOT*(X3*Y4-X4*Y3))+(X2*Y3-Y2*X3)*(HTOT*(X3*Y4-X4*Y3)
     & +FHTOT*(Y3*Z4-Y4*Z3)+GHTOT*(-X3*Z4+X4*Z3)))
      XM(IELEM,6)=COEF*((X2*Z3-Z2*X3)*(GTOT*(-X2*Z4+Z2*X4)+
     & FGTOT*(Y2*Z4-Z2*Y4)+GHTOT*(X2*Y4-Y2*X4))-
     & (Y2*Z3-Z2*Y3)*(FTOT*(Y2*Z4-Z2*Y4)+FGTOT*(-X2*Z4+Z2*X4)+
     & FHTOT*(X2*Y4-Y2*X4))-(X2*Y3-Y2*X3)*(HTOT*(X2*Y4-Y2*X4)+
     & FHTOT*(Y2*Z4-Z2*Y4)+GHTOT*(-X2*Z4+Z2*X4)))
      ENDDO ! IELEM
!
      ELSE
!
        WRITE(LU,1001) SF%ELM,SG%ELM,SH%ELM
1001    FORMAT(1X,'MT02TT (BIEF) : WRONG TYPE OF F,G OR H: ',
     &  I6,1X,I6,1X,I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
