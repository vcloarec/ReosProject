!                   *****************
                    SUBROUTINE VC08AA
!                   *****************
!
     &(XMUL,SF,SU,SV,F,U,V,XEL,YEL,IKLE,
     & NELEM,NELMAX,W1,W2,W3,FORMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /                  DF      DF
!+      V  =  XMUL   /       PSII  * ( U --  + V -- )   D(OMEGA)
!+       I          /OMEGA               DX      DY
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  A FROEHLY (MATMECA)
!+        01/07/08
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| STRING WITH FORMULA OF VECTOR
!| IKLE           |-->| CONNECTIVITY TABLE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
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
      USE BIEF, EX_VC08AA => VC08AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURES OF F, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,IELMU,IELMV
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION X2,Y2,X3,Y3,F1,F2,F3
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION SUR6,XSUR24,XSUR120,XSU216,F1MF3,F2MF1
      DOUBLE PRECISION K1,K2,K3,USUR2,VSUR2,PHIT
      DOUBLE PRECISION L12,L13,L21,L23,L31,L32,BETAN1,BETAN2,BETAN3
!
!-----------------------------------------------------------------------
!
      SUR6 = 1.D0 / 6.D0
      XSUR24 = XMUL/24.D0
      XSUR120= XMUL/120.D0
      XSU216 = XMUL/216.D0
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!
!     FUNCTION F AND VECTOR U ARE LINEAR
!
      IF(IELMF.EQ.11.AND.IELMU.EQ.11.AND.IELMV.EQ.11) THEN
!
        IF(FORMUL(14:16).EQ.'PSI') THEN
!
!     PSI SCHEME
!
          DO IELEM = 1 , NELEM
!
            X2 = XEL(IELEM,2)
            X3 = XEL(IELEM,3)
            Y2 = YEL(IELEM,2)
            Y3 = YEL(IELEM,3)
!
            F1 = F(IKLE(IELEM,1))
            F2 = F(IKLE(IELEM,2))
            F3 = F(IKLE(IELEM,3))
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
            BETAN1 = L12*(F1-F2) + L13*(F1-F3)
            BETAN2 = L21*(F2-F1) + L23*(F2-F3)
            BETAN3 = L31*(F3-F1) + L32*(F3-F2)
!
            PHIT = BETAN1 + BETAN2 + BETAN3
!
            IF(PHIT.GT.0.D0) THEN
              W1(IELEM) =   XMUL * MAX( MIN( BETAN1, PHIT),0.D0 )
              W2(IELEM) =   XMUL * MAX( MIN( BETAN2, PHIT),0.D0 )
              W3(IELEM) =   XMUL * MAX( MIN( BETAN3, PHIT),0.D0 )
            ELSE
              W1(IELEM) = - XMUL * MAX( MIN(-BETAN1,-PHIT),0.D0 )
              W2(IELEM) = - XMUL * MAX( MIN(-BETAN2,-PHIT),0.D0 )
              W3(IELEM) = - XMUL * MAX( MIN(-BETAN3,-PHIT),0.D0 )
            ENDIF
!
          ENDDO ! IELEM
!
        ELSE
!
!     NORMAL CENTERED SCHEME
!
          DO IELEM = 1 , NELEM
!
            X2 = XEL(IELEM,2)
            X3 = XEL(IELEM,3)
            Y2 = YEL(IELEM,2)
            Y3 = YEL(IELEM,3)
!
            F1MF3 = F(IKLE(IELEM,1)) - F(IKLE(IELEM,3))
            F2MF1 = F(IKLE(IELEM,2)) - F(IKLE(IELEM,1))
!
            U1 = U(IKLE(IELEM,1))
            U2 = U(IKLE(IELEM,2))
            U3 = U(IKLE(IELEM,3))
            V1 = V(IKLE(IELEM,1))
            V2 = V(IKLE(IELEM,2))
            V3 = V(IKLE(IELEM,3))
!
            W1(IELEM)=(  ( Y2*F1MF3 + Y3*F2MF1 ) * (U1+U1+U2+U3)
     &               - ( X2*F1MF3 + X3*F2MF1 ) * (V1+V1+V2+V3)  )*XSUR24
!
            W2(IELEM)=(  ( Y2*F1MF3 + Y3*F2MF1 ) * (U1+U2+U2+U3)
     &               - ( X2*F1MF3 + X3*F2MF1 ) * (V1+V2+V2+V3)  )*XSUR24
!
            W3(IELEM)=(  ( Y2*F1MF3 + Y3*F2MF1 ) * (U1+U2+U3+U3)
     &               - ( X2*F1MF3 + X3*F2MF1 ) * (V1+V2+V3+V3)  )*XSUR24
!
          ENDDO ! IELEM
!
        ENDIF
!
!-----------------------------------------------------------------------
!
!     FUNCTION F IS LINEAR AND VECTOR U QUASI-BUBBLE
!
      ELSEIF(IELMF.EQ.11.AND.IELMU.EQ.12.AND.IELMV.EQ.12) THEN
!
        IF(FORMUL(14:16).EQ.'PSI') THEN
!
!  PSI SCHEME (U4 DISCARDED HERE)
!             (AS IF U WAS P1   )
!
          DO IELEM = 1 , NELEM
!
            X2 = XEL(IELEM,2)
            X3 = XEL(IELEM,3)
            Y2 = YEL(IELEM,2)
            Y3 = YEL(IELEM,3)
!
            F1 = F(IKLE(IELEM,1))
            F2 = F(IKLE(IELEM,2))
            F3 = F(IKLE(IELEM,3))
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
            BETAN1 = L12*(F1-F2) + L13*(F1-F3)
            BETAN2 = L21*(F2-F1) + L23*(F2-F3)
            BETAN3 = L31*(F3-F1) + L32*(F3-F2)
!
            PHIT = BETAN1 + BETAN2 + BETAN3
!
            IF(PHIT.GT.0.D0) THEN
              W1(IELEM) =   XMUL * MAX( MIN( BETAN1, PHIT),0.D0 )
              W2(IELEM) =   XMUL * MAX( MIN( BETAN2, PHIT),0.D0 )
              W3(IELEM) =   XMUL * MAX( MIN( BETAN3, PHIT),0.D0 )
            ELSE
              W1(IELEM) = - XMUL * MAX( MIN(-BETAN1,-PHIT),0.D0 )
              W2(IELEM) = - XMUL * MAX( MIN(-BETAN2,-PHIT),0.D0 )
              W3(IELEM) = - XMUL * MAX( MIN(-BETAN3,-PHIT),0.D0 )
            ENDIF
!
          ENDDO ! IELEM
!
        ELSE
!
!  NORMAL CENTERED SCHEME
!
          DO IELEM = 1 , NELEM
!
            X2 = XEL(IELEM,2)
            X3 = XEL(IELEM,3)
            Y2 = YEL(IELEM,2)
            Y3 = YEL(IELEM,3)
!
            F1 = F(IKLE(IELEM,1))
            F2 = F(IKLE(IELEM,2)) - F1
            F3 = F(IKLE(IELEM,3)) - F1
!
            U1 = U(IKLE(IELEM,1))
            U2 = U(IKLE(IELEM,2))
            U3 = U(IKLE(IELEM,3))
            U4 = U(IKLE(IELEM,4))
            V1 = V(IKLE(IELEM,1))
            V2 = V(IKLE(IELEM,2))
            V3 = V(IKLE(IELEM,3))
            V4 = V(IKLE(IELEM,4))
!
            W1(IELEM)=(5*X2*F3*V3+12*X2*F3*V4+5*X2*F3*V2+14*X2*F3*V1-5
     &       *X3*F2*V3-12*X3*F2*V4-5*X3*F2*V2-14*X3*F2*V1-5*F3*U3*
     &       Y2-12*F3*U4*Y2-5*F3*U2*Y2-14*F3*U1*Y2+5*F2*U3*Y3+12*
     &       F2*U4*Y3+5*F2*U2*Y3+14*F2*U1*Y3)*XSU216
            W2(IELEM)=(5*X2*F3*V3+12*X2*F3*V4+14*X2*F3*V2+5*X2*F3*V1-5
     &       *X3*F2*V3-12*X3*F2*V4-14*X3*F2*V2-5*X3*F2*V1-5*F3*U3*
     &       Y2-12*F3*U4*Y2-14*F3*U2*Y2-5*F3*U1*Y2+5*F2*U3*Y3+12*
     &       F2*U4*Y3+14*F2*U2*Y3+5*F2*U1*Y3)*XSU216
            W3(IELEM)=(14*X2*F3*V3+12*X2*F3*V4+5*X2*F3*V2+5*X2*F3*V1-
     &       14*X3*F2*V3-12*X3*F2*V4-5*X3*F2*V2-5*X3*F2*V1-14*F3*
     &       U3*Y2-12*F3*U4*Y2-5*F3*U2*Y2-5*F3*U1*Y2+14*F2*U3*Y3+
     &       12*F2*U4*Y3+5*F2*U2*Y3+5*F2*U1*Y3)*XSU216
!
          ENDDO ! IELEM
!
        ENDIF
!
!-----------------------------------------------------------------------
!
!     FUNCTION F IS LINEAR AND VECTOR U P2
!
      ELSEIF(IELMF.EQ.11.AND.IELMU.EQ.13.AND.IELMV.EQ.13) THEN
!
        IF(FORMUL(14:16).EQ.'PSI') THEN
!
!  PSI SCHEME (U4,U5 AND U6 DISCARDED HERE)
!             (AS IF U WAS P1             )
!
          DO IELEM = 1 , NELEM
!
            X2 = XEL(IELEM,2)
            X3 = XEL(IELEM,3)
            Y2 = YEL(IELEM,2)
            Y3 = YEL(IELEM,3)
!
            F1 = F(IKLE(IELEM,1))
            F2 = F(IKLE(IELEM,2))
            F3 = F(IKLE(IELEM,3))
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
            BETAN1 = L12*(F1-F2) + L13*(F1-F3)
            BETAN2 = L21*(F2-F1) + L23*(F2-F3)
            BETAN3 = L31*(F3-F1) + L32*(F3-F2)
!
            PHIT = BETAN1 + BETAN2 + BETAN3
!
            IF(PHIT.GT.0.D0) THEN
              W1(IELEM) =   XMUL * MAX( MIN( BETAN1, PHIT),0.D0 )
              W2(IELEM) =   XMUL * MAX( MIN( BETAN2, PHIT),0.D0 )
              W3(IELEM) =   XMUL * MAX( MIN( BETAN3, PHIT),0.D0 )
            ELSE
              W1(IELEM) = - XMUL * MAX( MIN(-BETAN1,-PHIT),0.D0 )
              W2(IELEM) = - XMUL * MAX( MIN(-BETAN2,-PHIT),0.D0 )
              W3(IELEM) = - XMUL * MAX( MIN(-BETAN3,-PHIT),0.D0 )
            ENDIF
!
          ENDDO ! IELEM
!
        ELSE
!
!  NORMAL CENTERED SCHEME
!
          DO IELEM = 1 , NELEM
!
            X2 = XEL(IELEM,2)
            X3 = XEL(IELEM,3)
            Y2 = YEL(IELEM,2)
            Y3 = YEL(IELEM,3)
!
            F1 = F(IKLE(IELEM,1))
            F2 = F(IKLE(IELEM,2)) - F1
            F3 = F(IKLE(IELEM,3)) - F1
!
            U1 = U(IKLE(IELEM,1))
            U2 = U(IKLE(IELEM,2))
            U3 = U(IKLE(IELEM,3))
            U4 = U(IKLE(IELEM,4))
            U5 = U(IKLE(IELEM,5))
            U6 = U(IKLE(IELEM,6))
            V1 = V(IKLE(IELEM,1))
            V2 = V(IKLE(IELEM,2))
            V3 = V(IKLE(IELEM,3))
            V4 = V(IKLE(IELEM,4))
            V5 = V(IKLE(IELEM,5))
            V6 = V(IKLE(IELEM,6))
!
            W1(IELEM)=
     &          (2.D0*U1*Y3*F2-2.D0*U1*Y2*F3-V2*X2*F3-8.D0*V4*X3*F2+
     &           8.D0*U4*Y3*F2+V3*X3*F2-U2*Y3*F2-V3*X2*F3+4.D0*U5*Y3*F2-
     &           4.D0*U5*Y2*F3+U3*Y2*F3-4.D0*V5*X3*F2+V2*X3*F2-
     &           8.D0*U6*Y2*F3-8.D0*U4*Y2*F3+4.D0*V5*X2*F3+
     &           2.D0*V1*X2*F3-U3*Y3*F2+8.D0*V6*X2*F3-8.D0*V6*X3*F2+
     &           8.D0*V4*X2*F3-2.D0*V1*X3*F2+8.D0*U6*Y3*F2+
     &           U2*Y2*F3)*XSUR120
!
            W2(IELEM)=
     &         -(-8.D0*V5*X2*F3-4.D0*U6*Y3*F2-V1*X3*F2+4.D0*V6*X3*F2+
     &            8.D0*U4*Y2*F3-4.D0*V6*X2*F3+2.D0*U2*Y2*F3+
     &            8.D0*U5*Y2*F3+V3*X2*F3+8.D0*V5*X3*F2+2.D0*V2*X3*F2-
     &            2.D0*U2*Y3*F2-8.D0*U5*Y3*F2+V1*X2*F3-2.D0*V2*X2*F3-
     &            V3*X3*F2-8.D0*V4*X2*F3+U3*Y3*F2+U1*Y3*F2-U1*Y2*F3+
     &            4.D0*U6*Y2*F3-8.D0*U4*Y3*F2+8.D0*V4*X3*F2-U3*Y2*F3)
     &            *XSUR120
!
            W3(IELEM) =
     &           (-V5*X3*F2*8.D0+V2*X3*F2-U6*Y2*F3*8.D0-U4*Y2*F3*4.D0+
     &            V5*X2*F3*8.D0-V1*X2*F3+U3*Y3*F2*2.D0+V6*X2*F3*8.D0-
     &            V6*X3*F2*8.D0+V4*X2*F3*4.D0+V1*X3*F2+U6*Y3*F2*8.D0+
     &            U2*Y2*F3-U1*Y3*F2+U1*Y2*F3-V2*X2*F3-V4*X3*F2*4.D0+
     &            U4*Y3*F2*4.D0-V3*X3*F2*2.D0-U2*Y3*F2+V3*X2*F3*2.D0+
     &            U5*Y3*F2*8.D0-U5*Y2*F3*8.D0-U3*Y2*F3*2.D0)*XSUR120
!
          ENDDO ! IELEM
!
        ENDIF
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
101     FORMAT(1X,'VC08AA (BIEF) :',/,
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
