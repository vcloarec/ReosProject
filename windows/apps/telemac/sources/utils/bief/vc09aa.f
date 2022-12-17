!                   *****************
                    SUBROUTINE VC09AA
!                   *****************
!
     &(XMUL,SF,SG,SU,SV,F,G,U,V,
     & XEL,YEL,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W1,W2,W3 )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /                    DF        DF
!+      V  =  XMUL   /       PSII  * ( G U --  + G V -- )   D(OMEGA)
!+       I          /OMEGA                 DX        DY
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)
!+        09/12/94
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| G              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| STRING WITH FORMULA OF VECTOR
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SG             |-->| BIEF_OBJ STRUCTURE OF G
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
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
      USE BIEF, EX_VC09AA => VC09AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURES OF F, G, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMG,IELMV
!
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3,G1,G2,G3,U1,U2,U3,V1,V2,V3
      DOUBLE PRECISION XS120,T11,T12,T13,T22,T23,T33,FTX,FTY
      DOUBLE PRECISION U123,V123
!
!-----------------------------------------------------------------------
!
      XS120 = XMUL / 120.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMG=SG%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F AND G AND VECTOR U ARE LINEAR
!
      IF(      IELMF.EQ.11.AND.IELMG.EQ.11
     &    .AND.IELMU.EQ.11.AND.IELMV.EQ.11  ) THEN
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        F3 = F(IKLE3(IELEM))
!
        G1 = G(IKLE1(IELEM))
        G2 = G(IKLE2(IELEM))
        G3 = G(IKLE3(IELEM))
!
        U1 = U(IKLE1(IELEM))
        U2 = U(IKLE2(IELEM))
        U3 = U(IKLE3(IELEM))
!
        V1 = V(IKLE1(IELEM))
        V2 = V(IKLE2(IELEM))
        V3 = V(IKLE3(IELEM))
!
        FTX = F1 * (X3-X2) - F2 * X3 + F3*X2
        FTY = F1 * (Y2-Y3) + F2 * Y3 - F3*Y2
!
        U123 = U1 + U2 + U3
        V123 = V1 + V2 + V3
!
        T11 = FTX * ( V123 + V1 + V1 ) + FTY * ( U123 + U1 + U1 )
        T22 = FTX * ( V123 + V2 + V2 ) + FTY * ( U123 + U2 + U2 )
        T33 = FTX * ( V123 + V3 + V3 ) + FTY * ( U123 + U3 + U3 )
!
        T12 = FTX * ( V123 + V123 - V3 ) + FTY * ( U123 + U123 - U3 )
        T13 = FTX * ( V123 + V123 - V2 ) + FTY * ( U123 + U123 - U2 )
        T23 = FTX * ( V123 + V123 - V1 ) + FTY * ( U123 + U123 - U1 )
!
        W1(IELEM) = ( 2 * G1*T11 +     G2*T12 +     G3*T13 ) * XS120
        W2(IELEM) = (     G1*T12 + 2 * G2*T22 +     G3*T23 ) * XS120
        W3(IELEM) = (     G1*T13 +     G2*T23 + 2 * G3*T33 ) * XS120
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
        WRITE(LU,111) IELMG,SG%NAME
        WRITE(LU,201) IELMU,SU%NAME
        WRITE(LU,301)
101     FORMAT(1X,'VC09AA (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
111     FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &         1X,'REAL NAME: ',A6)
201     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
301     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
