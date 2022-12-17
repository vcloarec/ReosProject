!                   *****************
                    SUBROUTINE VC18PP
!                   *****************
!
     &(XMUL,SF,SU,SV,F,U,V,X,Y,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,W1,W2,W3)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR:
!code
!+                        /   1                  DF      DF
!+      V      =  XMUL   /   ---  PSI(IH)  * ( U --  + V -- )   D(OMEGA)
!+       IH,P*          /P*  DZ*                 DX      DY
!+
!+
!+    PSI(I) IS A 2D P1 BASE ON THE BASE TRIANGLE OF THE PRISM!
!+
!+    F AND U ARE 3D LINEAR BY PRISM
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!warning  COMPUTATION IN THE TRANSFORMED MESH !
!
!history  A. DECOENE (INRIA-LNHE)
!+        15/11/04
!+        V5P5
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
!history  J-M HERVOUET (EDF R&D LNHE)
!+        07/01/2013
!+        V6P3
!+   X and Y are now given per element.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| IKLE1          |-->| FIRST POINT OF PRISMS
!| IKLE2          |-->| SECOND POINT OF PRISMS
!| IKLE3          |-->| THIRD POINT OF PRISMS
!| IKLE4          |-->| FOURTH POINT OF PRISMS
!| IKLE5          |-->| FIFTH POINT OF PRISMS
!| IKLE6          |-->| SIXTH POINT OF PRISMS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SURFAC         |-->| AREA OF TRIANGLES
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC18PP => VC18PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: X(NELMAX,6),Y(NELMAX,6),XMUL
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
!
!     STRUCTURES OF F, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION X2,X3,Y2,Y3,DEN
      DOUBLE PRECISION PX1,PX2,PX3,PY1,PY2,PY3
      DOUBLE PRECISION PXFI,PXFS,PYFI,PYFS
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6
      DOUBLE PRECISION SUS,SUI
      DOUBLE PRECISION SHUI1,SHUS1,SHUI2,SHUS2,SHUI3,SHUS3
      DOUBLE PRECISION HU1S3I,HU1I3S,HU1SI,HU2S3I,HU2I3S,HU2SI
      DOUBLE PRECISION HU3S3I,HU3I3S,HU3SI
      DOUBLE PRECISION SVS,SVI
      DOUBLE PRECISION SHVI1,SHVS1,SHVI2,SHVS2,SHVI3,SHVS3
      DOUBLE PRECISION HV1S3I,HV1I3S,HV1SI,HV2S3I,HV2I3S,HV2SI
      DOUBLE PRECISION HV3S3I,HV3I3S,HV3SI
!
      INTEGER I1,I2,I3,I4,I5,I6
!
!**********************************************************************
!
      DEN = XMUL/1440.D0
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F AND VECTOR U ARE LINEAR
!
      IF(IELMU.EQ.41.AND.IELMV.EQ.41.AND.IELMF.EQ.41) THEN
!
!        LOOP ON THE ELEMENTS
!
        DO IELEM = 1,NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
!         X2  =   X(I2) - X(I1)
!         X3  =   X(I3) - X(I1)
!         Y2  =   Y(I2) - Y(I1)
!         Y3  =   Y(I3) - Y(I1)
!
          X2 = X(IELEM,2)
          X3 = X(IELEM,3)
          Y2 = Y(IELEM,2)
          Y3 = Y(IELEM,3)
!
          U1  =  U(I1)
          U2  =  U(I2)
          U3  =  U(I3)
          U4  =  U(I4)
          U5  =  U(I5)
          U6  =  U(I6)
          V1  =  V(I1)
          V2  =  V(I2)
          V3  =  V(I3)
          V4  =  V(I4)
          V5  =  V(I5)
          V6  =  V(I6)
          F1  =  F(I1)
          F2  =  F(I2)
          F3  =  F(I3)
          F4  =  F(I4)
          F5  =  F(I5)
          F6  =  F(I6)
!
!    INTERMEDIATE COMPUTATIONS
!
          PX2=Y3*DEN
          PX3=-Y2*DEN
          PX1=-PX2-PX3
          PY2=-X3*DEN
          PY3=X2*DEN
          PY1=-PY3-PY2
!
          SUS    = U6+U5+U4
          SUI    = U3+U2+U1
          SHUS1  = 5 * (SUS+U4)
          SHUI1  = 5 * (SUI+U1)
          SHUS2  = 5 * (SUS+U5)
          SHUI2  = 5 * (SUI+U2)
          SHUS3  = 5 * (SUS+U6)
          SHUI3  = 5 * (SUI+U3)
          HU1S3I = SHUS1 + 3*SHUI1
          HU1I3S = SHUI1 + 3*SHUS1
          HU1SI  = SHUS1 +   SHUI1
          HU2S3I = SHUS2 + 3*SHUI2
          HU2I3S = SHUI2 + 3*SHUS2
          HU2SI  = SHUS2 +   SHUI2
          HU3S3I = SHUS3 + 3*SHUI3
          HU3I3S = SHUI3 + 3*SHUS3
          HU3SI  = SHUS3 +   SHUI3
!
          SVS    = V6+V5+V4
          SVI    = V3+V2+V1
          SHVS1  = 5 * (SVS+V4)
          SHVI1  = 5 * (SVI+V1)
          SHVS2  = 5 * (SVS+V5)
          SHVI2  = 5 * (SVI+V2)
          SHVS3  = 5 * (SVS+V6)
          SHVI3  = 5 * (SVI+V3)
          HV1S3I = SHVS1 + 3*SHVI1
          HV1I3S = SHVI1 + 3*SHVS1
          HV1SI  = SHVS1 +   SHVI1
          HV2S3I = SHVS2 + 3*SHVI2
          HV2I3S = SHVI2 + 3*SHVS2
          HV2SI  = SHVS2 +   SHVI2
          HV3S3I = SHVS3 + 3*SHVI3
          HV3I3S = SHVI3 + 3*SHVS3
          HV3SI  = SHVS3 +   SHVI3
!
          PXFI=PX1*F1+PX2*F2+PX3*F3
          PXFS=PX1*F4+PX2*F5+PX3*F6
          PYFI=PY1*F1+PY2*F2+PY3*F3
          PYFS=PY1*F4+PY2*F5+PY3*F6
!
          W1(IELEM)= PXFI * (HU1S3I+HU1SI)
     &             + PYFI * (HV1S3I+HV1SI)
     &             + PXFS * (HU1SI+HU1I3S)
     &             + PYFS * (HV1SI+HV1I3S)
!
          W2(IELEM)= PXFI * (HU2S3I+HU2SI)
     &             + PYFI * (HV2S3I+HV2SI)
     &             + PXFS * (HU2SI+HU2I3S)
     &             + PYFS * (HV2SI+HV2I3S)
!
          W3(IELEM)= PXFI * (HU3S3I+HU3SI)
     &             + PYFI * (HV3S3I+HV3SI)
     &             + PXFS * (HU3SI+HU3I3S)
     &             + PYFS * (HV3SI+HV3I3S)
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMF,SF%NAME
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC18PP (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
