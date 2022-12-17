!                   *****************
                    SUBROUTINE VC20AA
!                   *****************
!
     &( XMUL,SU,SV,U,V,XEL,YEL,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,
     &  W1,W2,W3)
!
!***********************************************************************
! BIEF   V8P0                                          21/08/2018
!***********************************************************************
!
!brief    COMPUTES STRAIN RATE TENSOR NORM FOR SPALART
!+               ALLMARAS TURBULENCE MODEL
!code
!+                   /
!+  VEC(I) = XMUL   /     PSI(I) *
!+                 /OMEGA
!+
!+              DU           DV       DU   DV
!+     *     (2(--)^2  +   2(--)^2 +((--)+(--))^2  D(OMEGA)
!+              DX           DY       DY   DX
!+
!+
!+    U AND V ARE VECTORS
!+
!+    PSI(I) IS A BASE OF TYPE IELM
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  A. BOURGOIN & R. ATA
!+        21/08/2016
!+        V7P2
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SURFAC         |-->| AREA OF TRIANGLES
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC20AA => VC20AA
      USE DECLARATIONS_SPECIAL
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURES OF U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMU,IELMV
      DOUBLE PRECISION FACT,XSUR12,U21,U31,V21,V31,X2,X3,Y2,Y3
!
!-----------------------------------------------------------------------
!
      XSUR12 = XMUL / 12.D0
!
!-----------------------------------------------------------------------
!
!     BEWARE: U IS QUASI-BUBBLE AND U IS P2; ARE TREATED AS IF LINEAR HERE
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
!
      IF(      (IELMU.EQ.11.AND.IELMV.EQ.11)
     &     .OR.(IELMU.EQ.12.AND.IELMV.EQ.12)
     &     .OR.(IELMU.EQ.13.AND.IELMV.EQ.13)) THEN
!
      DO IELEM = 1 , NELEM
!
        X2  = XEL(IELEM,2)
        X3  = XEL(IELEM,3)
        Y2  = YEL(IELEM,2)
        Y3  = YEL(IELEM,3)
!
        U21 = U(IKLE2(IELEM)) - U(IKLE1(IELEM))
        U31 = U(IKLE3(IELEM)) - U(IKLE1(IELEM))
        V21 = V(IKLE2(IELEM)) - V(IKLE1(IELEM))
        V31 = V(IKLE3(IELEM)) - V(IKLE1(IELEM))
!
        FACT = (2.D0*( X2*V31-X3*V21)**2+2.D0*(Y2*U31-Y3*U21)**2
     &       +(Y2*V31-Y3*V21+X2*U31-X3*U21)**2)*XSUR12/SURFAC(IELEM)
!
        W1(IELEM) = FACT
        W2(IELEM) = FACT
        W3(IELEM) = FACT
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,101) IELMU,SU%NAME
        WRITE(LU,201) IELMV,SV%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC20AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF V:',1I6,
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

