!                   *****************
                    SUBROUTINE MT02PT
!                   *****************
!
     &(T,XM,XMUL,SF,SG,SH,F,G,H,X,Y,Z,IKLE,NELEM,NELMAX,INCHYD)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE DIFFUSION MATRIX.
!+
!+            THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P1
!+                DIAGONAL TENSOR.
!+
!+            CASE WHERE THE PRISM IS DIVIDED INTO 3 TETRAHEDRONS.
!+                OPTIMISED COEFFICIENT COMPUTATION.
!code
!+     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!+     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!+     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!+     XM(IELEM, 4)  ---->  M(1,5) = M(5,1)
!+     XM(IELEM, 5)  ---->  M(1,6) = M(6,1)
!+     XM(IELEM, 6)  ---->  M(2,3) = M(3,2)
!+     XM(IELEM, 7)  ---->  M(2,4) = M(4,2)
!+     XM(IELEM, 8)  ---->  M(2,5) = M(5,2)
!+     XM(IELEM, 9)  ---->  M(2,6) = M(6,2)
!+     XM(IELEM,10)  ---->  M(3,4) = M(4,3)
!+     XM(IELEM,11)  ---->  M(3,5) = M(5,3)
!+     XM(IELEM,12)  ---->  M(3,6) = M(6,3)
!+     XM(IELEM,13)  ---->  M(4,5) = M(5,4)
!+     XM(IELEM,14)  ---->  M(4,6) = M(6,4)
!+     XM(IELEM,15)  ---->  M(5,6) = M(6,5)
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
      USE BIEF, EX_MT02PT => MT02PT
      USE DECLARATIONS_TELEMAC, ONLY : TETRA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,6)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: SF,SG,SH
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
      LOGICAL, INTENT(IN)             :: INCHYD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     SPECIFIC DECLARATIONS
!
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4
      DOUBLE PRECISION DIAG1,DIAG2,DIAG3,DIAG4
      DOUBLE PRECISION EXTR12,EXTR13,EXTR14,EXTR23,EXTR24,EXTR34
      INTEGER IT1,IT2,IT3,IT4,I,I1,I2,I3,I4,I5,I6,NUM1,NUM2,NUM3,NUM4
      INTEGER :: IGLOB(6),SENS(3),STO(6,6),IELEM
!
      DOUBLE PRECISION SUR24,HTOT,VTOT,WTOT,COEF
      DOUBLE PRECISION T1,T3,T5,T7,T9,T11,T13,T15,T17,T19,T21,T23
      DOUBLE PRECISION T35,T49,T28,T42,T51,T54,SURF
!
!     PRISM SPLITTING (SEE EXPLANATIONS IN DECLARATIONS_TELEMAC)
!
!     INTEGER TETRA(2,2,2,3,4)
!     DATA TETRA / 0,1,1,1,1,1,1,0,0,4,4,4,4,4,4,0,0,6,4,5,5,4,6,0,
!    &             0,2,2,2,2,2,2,0,0,6,6,6,6,6,6,0,0,3,1,2,2,1,3,0,
!    &             0,3,3,3,3,3,3,0,0,5,5,5,5,5,5,0,0,2,3,4,1,6,5,0,
!    &             0,4,5,4,6,6,5,0,0,2,3,3,1,2,1,0,0,4,5,3,6,2,1,0 /
!
!     STORAGE CONVENTION FOR THE PRISM EXTRADIAGONAL TERMS
!
      PARAMETER ( STO = RESHAPE((/
     &           00 , 01 , 02 , 03 , 04 , 05 ,
     &           01 , 00 , 06 , 07 , 08 , 09 ,
     &           02 , 06 , 00 , 10 , 11 , 12 ,
     &           03 , 07 , 10 , 00 , 13 , 14 ,
     &           04 , 08 , 11 , 13 , 00 , 15 ,
     &           05 , 09 , 12 , 14 , 15 , 00 /), (/6,6/)) )
!
!***********************************************************************
!
!     DIFFERENT WAYS TO SPLIT PRISMS (TO ENSURE MATCHING OF TETRAHEDRONS)
!
!     TETRA(2,2,2,3,4)
!
!     FIRST 3 DIMENSIONS : TYPE OF FACE
!                      1 : CUT RECTANGLE BETWEEN  LOW-LEFT AND HIGH-RIGHT
!                      2 : CUT RECTANGLE BETWEEN  HIGH-LEFT AND LOW-RIGHT
!
!     4TH DIMENSION : NUMBER OF TETRAHEDRON
!     5TH DIMENSION : 4 POINTS OF THE TETRAHEDRON (IN LOCAL PRISM NUMBERING)
!
!     1 1 2 CUTTING
!
!     TETRA(1,1,2,1,1)= 1
!     TETRA(1,1,2,1,2)= 2
!     TETRA(1,1,2,1,3)= 3
!     TETRA(1,1,2,1,4)= 6
!
!     TETRA(1,1,2,2,1)= 4
!     TETRA(1,1,2,2,2)= 6
!     TETRA(1,1,2,2,3)= 5
!     TETRA(1,1,2,2,4)= 1
!
!     TETRA(1,1,2,3,1)= 5
!     TETRA(1,1,2,3,2)= 2
!     TETRA(1,1,2,3,3)= 1
!     TETRA(1,1,2,3,4)= 6
!
!     2 1 1 CUTTING
!
!     TETRA(2,1,1,1,1)= 1
!     TETRA(2,1,1,1,2)= 2
!     TETRA(2,1,1,1,3)= 3
!     TETRA(2,1,1,1,4)= 4
!
!     TETRA(2,1,1,2,1)= 4
!     TETRA(2,1,1,2,2)= 6
!     TETRA(2,1,1,2,3)= 5
!     TETRA(2,1,1,2,4)= 2
!
!     TETRA(2,1,1,3,1)= 6
!     TETRA(2,1,1,3,2)= 3
!     TETRA(2,1,1,3,3)= 2
!     TETRA(2,1,1,3,4)= 4
!
!     1 2 1 CUTTING
!
!     TETRA(1,2,1,1,1)= 1
!     TETRA(1,2,1,1,2)= 2
!     TETRA(1,2,1,1,3)= 3
!     TETRA(1,2,1,1,4)= 5
!
!     TETRA(1,2,1,2,1)= 4
!     TETRA(1,2,1,2,2)= 6
!     TETRA(1,2,1,2,3)= 5
!     TETRA(1,2,1,2,4)= 3
!
!     TETRA(1,2,1,3,1)= 4
!     TETRA(1,2,1,3,2)= 1
!     TETRA(1,2,1,3,3)= 3
!     TETRA(1,2,1,3,4)= 5
!
!     2 2 1 CUTTING
!
!     TETRA(2,2,1,1,1)= 1
!     TETRA(2,2,1,1,2)= 2
!     TETRA(2,2,1,1,3)= 3
!     TETRA(2,2,1,1,4)= 4
!
!     TETRA(2,2,1,2,1)= 4
!     TETRA(2,2,1,2,2)= 6
!     TETRA(2,2,1,2,3)= 5
!     TETRA(2,2,1,2,4)= 3
!
!     TETRA(2,2,1,3,1)= 5
!     TETRA(2,2,1,3,2)= 2
!     TETRA(2,2,1,3,3)= 4
!     TETRA(2,2,1,3,4)= 3
!
!     1 2 2 CUTTING
!
!     TETRA(1,2,2,1,1)= 1
!     TETRA(1,2,2,1,2)= 2
!     TETRA(1,2,2,1,3)= 3
!     TETRA(1,2,2,1,4)= 5
!
!     TETRA(1,2,2,2,1)= 4
!     TETRA(1,2,2,2,2)= 6
!     TETRA(1,2,2,2,3)= 5
!     TETRA(1,2,2,2,4)= 1
!
!     TETRA(1,2,2,3,1)= 6
!     TETRA(1,2,2,3,2)= 3
!     TETRA(1,2,2,3,3)= 5
!     TETRA(1,2,2,3,4)= 1
!
!     2 1 2 CUTTING
!
!     TETRA(2,1,2,1,1)= 1
!     TETRA(2,1,2,1,2)= 2
!     TETRA(2,1,2,1,3)= 3
!     TETRA(2,1,2,1,4)= 6
!
!     TETRA(2,1,2,2,1)= 4
!     TETRA(2,1,2,2,2)= 6
!     TETRA(2,1,2,2,3)= 5
!     TETRA(2,1,2,2,4)= 2
!
!     TETRA(2,1,2,3,1)= 4
!     TETRA(2,1,2,3,2)= 1
!     TETRA(2,1,2,3,3)= 6
!     TETRA(2,1,2,3,4)= 2
!
!-----------------------------------------------------------------------
!
      SUR24=1.D0/24.D0
!
      IF(SF%ELM.EQ.41.AND.SG%ELM.EQ.41.AND.SH%ELM.EQ.41) THEN
!
!-----------------------------------------------------------------------
!
!   LINEAR DISCRETISATION OF DIFFUSION COEFFICIENTS
!
!   LOOP ON THE PRISMS
!
      DO IELEM=1,NELEM
!
      IGLOB(1)=IKLE(IELEM,1)
      IGLOB(2)=IKLE(IELEM,2)
      IGLOB(3)=IKLE(IELEM,3)
      IGLOB(4)=IKLE(IELEM,4)
      IGLOB(5)=IKLE(IELEM,5)
      IGLOB(6)=IKLE(IELEM,6)
!
      IF(IGLOB(1).GT.IGLOB(2)) THEN
        SENS(1)=1
      ELSE
        SENS(1)=2
      ENDIF
      IF(IGLOB(2).GT.IGLOB(3)) THEN
        SENS(2)=1
      ELSE
        SENS(2)=2
      ENDIF
      IF(IGLOB(3).GT.IGLOB(1)) THEN
        SENS(3)=1
      ELSE
        SENS(3)=2
      ENDIF
!
!     FOOTPRINT OF THE PRISM
!
      SURF=0.5D0*
     &    ((X(IGLOB(2))-X(IGLOB(1)))*(Y(IGLOB(3))-Y(IGLOB(1)))
     &    -(X(IGLOB(3))-X(IGLOB(1)))*(Y(IGLOB(2))-Y(IGLOB(1))))
!
! INITIALISES TO 0.D0
!
      T(IELEM,1)=0.D0
      T(IELEM,2)=0.D0
      T(IELEM,3)=0.D0
      T(IELEM,4)=0.D0
      T(IELEM,5)=0.D0
      T(IELEM,6)=0.D0
!
      XM(IELEM, 1)= 0.D0
      XM(IELEM, 2)= 0.D0
      XM(IELEM, 3)= 0.D0
      XM(IELEM, 4)= 0.D0
      XM(IELEM, 5)= 0.D0
!
      XM(IELEM, 6)= 0.D0
      XM(IELEM, 7)= 0.D0
      XM(IELEM, 8)= 0.D0
      XM(IELEM, 9)= 0.D0
!
      XM(IELEM,10)= 0.D0
      XM(IELEM,11)= 0.D0
      XM(IELEM,12)= 0.D0
!
      XM(IELEM,13)= 0.D0
      XM(IELEM,14)= 0.D0
      XM(IELEM,15)= 0.D0
!
!-----------------------------------------------------------------------
!     LOOP OVER  TI
!
      DO I=1,3
!
!     TETRAHEDRON POINTS NUMBERS IN THE PRISM NUMBERING
!
      NUM1=TETRA(SENS(1),SENS(2),SENS(3),I,1)
      NUM2=TETRA(SENS(1),SENS(2),SENS(3),I,2)
      NUM3=TETRA(SENS(1),SENS(2),SENS(3),I,3)
      NUM4=TETRA(SENS(1),SENS(2),SENS(3),I,4)
!
!     GLOBAL NUMBERS OF THE TETRAHEDRON POINTS
!
      IT1=IGLOB(NUM1)
      IT2=IGLOB(NUM2)
      IT3=IGLOB(NUM3)
      IT4=IGLOB(NUM4)
!
! VISCOSITY ALONG X Y AND Z
!
      HTOT=F(IT1)+F(IT2)+F(IT3)+F(IT4)
      VTOT=G(IT1)+G(IT2)+G(IT3)+G(IT4)
      WTOT=H(IT1)+H(IT2)+H(IT3)+H(IT4)
!
      X2=X(IT2)-X(IT1)
      Y2=Y(IT2)-Y(IT1)
      Z2=Z(IT2)-Z(IT1)
      X3=X(IT3)-X(IT1)
      Y3=Y(IT3)-Y(IT1)
      Z3=Z(IT3)-Z(IT1)
      X4=X(IT4)-X(IT1)
      Y4=Y(IT4)-Y(IT1)
      Z4=Z(IT4)-Z(IT1)
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
      COEF=XMUL*SUR24/MAX(T13,0.01D0*SURF)
!
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
!
      DIAG1  =COEF*( HTOT*T28**2 +VTOT*T42**2 +WTOT*T54**2)
      DIAG2  =COEF*( HTOT*T19**2 +VTOT*T35**2 +WTOT*T49**2)
      DIAG3  =COEF*( HTOT*T23**2 +VTOT*T17**2 +WTOT*T51**2)
      EXTR12 =COEF*( HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      EXTR13 =COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      EXTR23 =COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
!
!     DEDUCED FROM PROPERTIES OF THE DIFFUSION MATRIX
!
      EXTR14 = -(EXTR13+EXTR12+DIAG1)
      EXTR24 = -(EXTR23+DIAG2+EXTR12)
      EXTR34 = -(DIAG3+EXTR23+EXTR13)
      DIAG4  = -(EXTR14+EXTR24+EXTR34)
!
! ASSEMBLES ON PRISM
!
      T(IELEM,NUM1) = T(IELEM,NUM1)+ DIAG1
      T(IELEM,NUM2) = T(IELEM,NUM2)+ DIAG2
      T(IELEM,NUM3) = T(IELEM,NUM3)+ DIAG3
      T(IELEM,NUM4) = T(IELEM,NUM4)+ DIAG4
!
      XM(IELEM,STO(NUM1,NUM2))=XM(IELEM,STO(NUM1,NUM2))+EXTR12
      XM(IELEM,STO(NUM1,NUM3))=XM(IELEM,STO(NUM1,NUM3))+EXTR13
      XM(IELEM,STO(NUM1,NUM4))=XM(IELEM,STO(NUM1,NUM4))+EXTR14
      XM(IELEM,STO(NUM2,NUM3))=XM(IELEM,STO(NUM2,NUM3))+EXTR23
      XM(IELEM,STO(NUM2,NUM4))=XM(IELEM,STO(NUM2,NUM4))+EXTR24
      XM(IELEM,STO(NUM3,NUM4))=XM(IELEM,STO(NUM3,NUM4))+EXTR34
!
      ENDDO ! I
!
!---------------------------------------------------------------
!
      ENDDO ! IELEM
!
      ELSEIF(SF%ELM.EQ.40.AND.SG%ELM.EQ.40.AND.SH%ELM.EQ.40) THEN
!
!
!-----------------------------------------------------------------------
!
!   P0 DISCRETISATION OF DIFFUSION COEFFICIENTS (CONSTANT ON A PRISM)
!
!   LOOP ON THE PRISMS
!
      DO IELEM=1,NELEM
!
      IGLOB(1)=IKLE(IELEM,1)
      IGLOB(2)=IKLE(IELEM,2)
      IGLOB(3)=IKLE(IELEM,3)
      IGLOB(4)=IKLE(IELEM,4)
      IGLOB(5)=IKLE(IELEM,5)
      IGLOB(6)=IKLE(IELEM,6)
!
      IF(IGLOB(1).GT.IGLOB(2)) THEN
        SENS(1)=1
      ELSE
        SENS(1)=2
      ENDIF
      IF(IGLOB(2).GT.IGLOB(3)) THEN
        SENS(2)=1
      ELSE
        SENS(2)=2
      ENDIF
      IF(IGLOB(3).GT.IGLOB(1)) THEN
        SENS(3)=1
      ELSE
        SENS(3)=2
      ENDIF
!
!     FOOTPRINT OF THE PRISM
!
      SURF=0.5D0*
     &    ((X(IGLOB(2))-X(IGLOB(1)))*(Y(IGLOB(3))-Y(IGLOB(1)))
     &    -(X(IGLOB(3))-X(IGLOB(1)))*(Y(IGLOB(2))-Y(IGLOB(1))))
!
! INITIALISES TO 0.D0
!
      T(IELEM,1)=0.D0
      T(IELEM,2)=0.D0
      T(IELEM,3)=0.D0
      T(IELEM,4)=0.D0
      T(IELEM,5)=0.D0
      T(IELEM,6)=0.D0
!
      XM(IELEM, 1)= 0.D0
      XM(IELEM, 2)= 0.D0
      XM(IELEM, 3)= 0.D0
      XM(IELEM, 4)= 0.D0
      XM(IELEM, 5)= 0.D0
!
      XM(IELEM, 6)= 0.D0
      XM(IELEM, 7)= 0.D0
      XM(IELEM, 8)= 0.D0
      XM(IELEM, 9)= 0.D0
!
      XM(IELEM,10)= 0.D0
      XM(IELEM,11)= 0.D0
      XM(IELEM,12)= 0.D0
!
      XM(IELEM,13)= 0.D0
      XM(IELEM,14)= 0.D0
      XM(IELEM,15)= 0.D0
!
!-----------------------------------------------------------------------
!     LOOP OVER  TI
!
      DO I=1,3
!
!     TETRAHEDRON POINTS NUMBERS IN THE PRISM NUMBERING
!
      NUM1=TETRA(SENS(1),SENS(2),SENS(3),I,1)
      NUM2=TETRA(SENS(1),SENS(2),SENS(3),I,2)
      NUM3=TETRA(SENS(1),SENS(2),SENS(3),I,3)
      NUM4=TETRA(SENS(1),SENS(2),SENS(3),I,4)
!
!     GLOBAL NUMBERS OF THE TETRAHEDRON POINTS
!
      IT1=IGLOB(NUM1)
      IT2=IGLOB(NUM2)
      IT3=IGLOB(NUM3)
      IT4=IGLOB(NUM4)
!
! VISCOSITY ALONG X Y AND Z
!
      HTOT=4*F(IELEM)
      VTOT=4*G(IELEM)
      WTOT=4*H(IELEM)
!
      X2=X(IT2)-X(IT1)
      Y2=Y(IT2)-Y(IT1)
      Z2=Z(IT2)-Z(IT1)
      X3=X(IT3)-X(IT1)
      Y3=Y(IT3)-Y(IT1)
      Z3=Z(IT3)-Z(IT1)
      X4=X(IT4)-X(IT1)
      Y4=Y(IT4)-Y(IT1)
      Z4=Z(IT4)-Z(IT1)
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
      COEF=XMUL*SUR24/MAX(T13,0.01D0*SURF)
!
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
!
      DIAG1  =COEF*( HTOT*T28**2 +VTOT*T42**2 +WTOT*T54**2)
      DIAG2  =COEF*( HTOT*T19**2 +VTOT*T35**2 +WTOT*T49**2)
      DIAG3  =COEF*( HTOT*T23**2 +VTOT*T17**2 +WTOT*T51**2)
      EXTR12 =COEF*( HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      EXTR13 =COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      EXTR23 =COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
!
!     DEDUCED FROM PROPERTIES OF THE DIFFUSION MATRIX
!
      EXTR14 = -(EXTR13+EXTR12+DIAG1)
      EXTR24 = -(EXTR23+DIAG2+EXTR12)
      EXTR34 = -(DIAG3+EXTR23+EXTR13)
      DIAG4  = -(EXTR14+EXTR24+EXTR34)
!
! ASSEMBLES ON PRISM
!
      T(IELEM,NUM1) = T(IELEM,NUM1)+ DIAG1
      T(IELEM,NUM2) = T(IELEM,NUM2)+ DIAG2
      T(IELEM,NUM3) = T(IELEM,NUM3)+ DIAG3
      T(IELEM,NUM4) = T(IELEM,NUM4)+ DIAG4
!
      XM(IELEM,STO(NUM1,NUM2))=XM(IELEM,STO(NUM1,NUM2))+EXTR12
      XM(IELEM,STO(NUM1,NUM3))=XM(IELEM,STO(NUM1,NUM3))+EXTR13
      XM(IELEM,STO(NUM1,NUM4))=XM(IELEM,STO(NUM1,NUM4))+EXTR14
      XM(IELEM,STO(NUM2,NUM3))=XM(IELEM,STO(NUM2,NUM3))+EXTR23
      XM(IELEM,STO(NUM2,NUM4))=XM(IELEM,STO(NUM2,NUM4))+EXTR24
      XM(IELEM,STO(NUM3,NUM4))=XM(IELEM,STO(NUM3,NUM4))+EXTR34
!
      ENDDO ! I
!
!---------------------------------------------------------------
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
!
      ELSE
!
        WRITE(LU,1001) SF%ELM,SG%ELM,SH%ELM
1000    FORMAT(1X,'MT02PT (BIEF) : MAUVAIS TYPE DE F,G OU H : ',
     &  I6,1X,I6,1X,I6)
1001    FORMAT(1X,'MT02PT (BIEF) : WRONG TYPE OF F,G OR H: ',
     &  I6,1X,I6,1X,I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  TREATMENT OF HYDROSTATIC INCONSISTENCIES
!
      IF(INCHYD) THEN
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
!
        IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6))) THEN
!
          T(IELEM,1)  =0.D0
          T(IELEM,2)  =0.D0
          T(IELEM,3)  =0.D0
          T(IELEM,4)  =0.D0
          T(IELEM,5)  =0.D0
          T(IELEM,6)  =0.D0
          XM(IELEM, 1)=0.D0
          XM(IELEM, 2)=0.D0
          XM(IELEM, 3)=0.D0
          XM(IELEM, 4)=0.D0
          XM(IELEM, 5)=0.D0
          XM(IELEM, 6)=0.D0
          XM(IELEM, 7)=0.D0
          XM(IELEM, 8)=0.D0
          XM(IELEM, 9)=0.D0
          XM(IELEM,10)=0.D0
          XM(IELEM,11)=0.D0
          XM(IELEM,12)=0.D0
          XM(IELEM,13)=0.D0
          XM(IELEM,14)=0.D0
          XM(IELEM,15)=0.D0
!
        ENDIF
!
      ENDDO ! IELEM
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
