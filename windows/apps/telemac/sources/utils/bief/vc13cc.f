!                   *****************
                    SUBROUTINE VC13CC
!                   *****************
!
     &(XMUL,SF,F,XEL,YEL,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     & W1,W2,W3,W4,W5,W6,ICOORD)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /            --->
!+    VEC(I) = XMUL  /    PSI(I) * GRAD(F)  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF P2 TYPE
!
!note     IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!+                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!+                     P1, BUT GIVEN BY ELEMENTS.
!+                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,6).
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        12/05/2014
!+        V7P0
!+   Discontinuous elements better treated: new types 15, 16 and 17 for
!+   discontinuous linear, quasi-bubble, and quadratic, rather than
!+   using component DIMDISC=11, 12 or 13.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| IKLE4          |-->| FOURTH POINT OF TRIANGLES (QUADRATIC)
!| IKLE5          |-->| FIFTH POINT OF TRIANGLES (QUADRATIC)
!| IKLE6          |-->| SIXTH POINT OF TRIANGLES (QUADRATIC)
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| W5             |<--| RESULT IN NON ASSEMBLED FORM
!| W6             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC13CC => VC13CC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION XSUR6,XSUR30
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      XSUR6 = XMUL / 6.D0
      XSUR30= XMUL / 30.D0
!
!-----------------------------------------------------------------------
!     F OF TYPE P1
!-----------------------------------------------------------------------
!
      IF(IELMF.EQ.11) THEN
!
!================================
!  DERIVATIVE WRT X  =
!================================
!
        IF(ICOORD.EQ.1) THEN
!
        DO IELEM = 1 , NELEM
!
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (Y2*(F1-F3)+Y3*(F2-F1))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
!
        ENDDO ! IELEM
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
      ELSEIF(ICOORD.EQ.2) THEN
!
        DO IELEM = 1 , NELEM
!
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (X2*(F3-F1)+X3*(F1-F2))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
!
      ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
!     BEWARE: HERE F IS LINEAR BUT DISCONTINUOUS BETWEEN THE ELEMENTS
!
      ELSEIF(IELMF.EQ.15) THEN
!
!  X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO IELEM = 1 , NELEM
!
        F1  = F(IELEM)
        F2  = F(IELEM+NELMAX)
        F3  = F(IELEM+2*NELMAX)
!
        Y2  =  YEL(IELEM,2)
        Y3  =  YEL(IELEM,3)
!
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (Y2*(F1-F3)+Y3*(F2-F1))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
!
      ENDDO ! IELEM
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!  Y COORDINATE
!
      DO IELEM = 1 , NELEM
!
        F1  = F(IELEM)
        F2  = F(IELEM+NELMAX)
        F3  = F(IELEM+2*NELMAX)
!
        X2  =  XEL(IELEM,2)
        X3  =  XEL(IELEM,3)
!
        W1(IELEM) = 0.D0
        W2(IELEM) = 0.D0
        W3(IELEM) = 0.D0
        W4(IELEM) = (X2*(F3-F1)+X3*(F1-F2))*XSUR6
        W5(IELEM) = W4(IELEM)
        W6(IELEM) = W4(IELEM)
!
      ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!     F OF TYPE P2
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.13) THEN
!
!================================
!  DERIVATIVE WRT X  =
!================================
!
      IF(ICOORD.EQ.1) THEN
!
        DO IELEM = 1 , NELEM
!
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM))
        F3 = F(IKLE3(IELEM))
        F4 = F(IKLE4(IELEM))
        F5 = F(IKLE5(IELEM))
        F6 = F(IKLE6(IELEM))
!
        W1(IELEM) = ((2.D0*F1+F5-F4+F3-3.D0*F6)*Y2
     &            +  (F6-F5+3.D0*F4-F2-2.D0*F1)*Y3) * XSUR30
        W2(IELEM) = ((2.D0*F2-3.D0*F4+F1+F6-F5)*Y3
     &            +  (F3-F1-2.D0*F5+2.D0*F4   )*Y2) * XSUR30
        W3(IELEM) = ((3.D0*F6+F5-F4-F1-2.D0*F3)*Y2
     &            +  (F1-F2+2.D0*F5-2.D0*F6   )*Y3) * XSUR30
        W4(IELEM) = ((-4.D0*F6-8.D0*(F5-F4)+3.D0*F1+F3)*Y2
     &            +  (3.D0*(F2-F1)+4.D0*(F5-F6)       )*Y3) * XSUR30
        W5(IELEM) = ((4.D0*F6-8.D0*(F5-F4)-F1-3.D0*F3 )*Y2
     &            +  (3.D0*F2-4.D0*F4+F1+8.D0*(F5-F6) )*Y3) * XSUR30
        W6(IELEM) = ((4.D0*(F4-F5)+3.D0*(F1-F3)       )*Y2
     &            +  (4.D0*F4-3.D0*F1+8.D0*(F5-F6)-F2 )*Y3) * XSUR30
!
      ENDDO ! IELEM
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
      ELSEIF(ICOORD.EQ.2) THEN
!
        DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
!
        F1  = F(IKLE1(IELEM))
        F2  = F(IKLE2(IELEM))
        F3  = F(IKLE3(IELEM))
        F4  = F(IKLE4(IELEM))
        F5  = F(IKLE5(IELEM))
        F6  = F(IKLE6(IELEM))
!
        W1(IELEM) = ((F4-F5-2.D0*F1+3.D0*F6-F3 )*X2
     &            +  (-3.D0*F4-F6+2.D0*F1+F2+F5)*X3) * XSUR30
        W2(IELEM) = ((F5-F1-2.D0*F2+3.D0*F4-F6)*X3
     &            +  (-F3+F1-2.D0*(F4-F5)     )*X2 ) * XSUR30
        W3(IELEM) = ((-3.D0*F6+2.D0*F3+F1+F4-F5)*X2
     &            +  (2.D0*(F6-F5)-F1+F2       )*X3) * XSUR30
        W4(IELEM) = ((4.D0*F6-F3-3.D0*F1-8.D0*(F4-F5))*X2
     &            +  (4.D0*(F6-F5)+3.D0*(F1-F2)      )*X3) * XSUR30
        W5(IELEM) = ((3.D0*F3+F1-8.D0*(F4-F5)-4.D0*F6)*X2
     &            +  (8.D0*(F6-F5)+4.D0*F4-F1-3.D0*F2)*X3) * XSUR30
        W6(IELEM) = ((3.D0*(F3-F1)-4.D0*(F4-F5)      )*X2
     &            +  (8.D0*(F6-F5)+3.D0*F1+F2-4.D0*F4)*X3) * XSUR30
!
      ENDDO ! IELEM
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!     F IS P2 BUT DISCONTINUOUS BETWEEN THE ELEMENTS
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.17) THEN
!
!================================
!  DERIVATIVE WRT X  =
!================================
!
      IF(ICOORD.EQ.1) THEN
!
        DO IELEM = 1 , NELEM
!
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1  = F(IELEM         )
        F2  = F(IELEM+  NELMAX)
        F3  = F(IELEM+2*NELMAX)
        F4  = F(IELEM+3*NELMAX)
        F5  = F(IELEM+4*NELMAX)
        F6  = F(IELEM+5*NELMAX)
!
        W1(IELEM) = ((2.D0*F1+F5-F4+F3-3.D0*F6)*Y2
     &            +  (F6-F5+3.D0*F4-F2-2.D0*F1)*Y3) * XSUR30
        W2(IELEM) = ((2.D0*F2-3.D0*F4+F1+F6-F5)*Y3
     &            +  (F3-F1-2.D0*F5+2.D0*F4   )*Y2) * XSUR30
        W3(IELEM) = ((3.D0*F6+F5-F4-F1-2.D0*F3)*Y2
     &            +  (F1-F2+2.D0*F5-2.D0*F6   )*Y3) * XSUR30
        W4(IELEM) = ((-4.D0*F6-8.D0*(F5-F4)+3.D0*F1+F3)*Y2
     &            +  (3.D0*(F2-F1)+4.D0*(F5-F6)       )*Y3) * XSUR30
        W5(IELEM) = ((4.D0*F6-8.D0*(F5-F4)-F1-3.D0*F3 )*Y2
     &            +  (3.D0*F2-4.D0*F4+F1+8.D0*(F5-F6) )*Y3) * XSUR30
        W6(IELEM) = ((4.D0*(F4-F5)+3.D0*(F1-F3)       )*Y2
     &            +  (4.D0*F4-3.D0*F1+8.D0*(F5-F6)-F2 )*Y3) * XSUR30
!
      ENDDO
!
!================================
!  DERIVATIVE WRT Y  =
!================================
!
      ELSEIF(ICOORD.EQ.2) THEN
!
      DO IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
!
        F1  = F(IELEM         )
        F2  = F(IELEM+  NELMAX)
        F3  = F(IELEM+2*NELMAX)
        F4  = F(IELEM+3*NELMAX)
        F5  = F(IELEM+4*NELMAX)
        F6  = F(IELEM+5*NELMAX)
!
        W1(IELEM) = ((F4-F5-2.D0*F1+3.D0*F6-F3 )*X2
     &            +  (-3.D0*F4-F6+2.D0*F1+F2+F5)*X3) * XSUR30
        W2(IELEM) = ((F5-F1-2.D0*F2+3.D0*F4-F6)*X3
     &            +  (-F3+F1-2.D0*(F4-F5)     )*X2 ) * XSUR30
        W3(IELEM) = ((-3.D0*F6+2.D0*F3+F1+F4-F5)*X2
     &            +  (2.D0*(F6-F5)-F1+F2       )*X3) * XSUR30
        W4(IELEM) = ((4.D0*F6-F3-3.D0*F1-8.D0*(F4-F5))*X2
     &            +  (4.D0*(F6-F5)+3.D0*(F1-F2)      )*X3) * XSUR30
        W5(IELEM) = ((3.D0*F3+F1-8.D0*(F4-F5)-4.D0*F6)*X2
     &            +  (8.D0*(F6-F5)+4.D0*F4-F1-3.D0*F2)*X3) * XSUR30
        W6(IELEM) = ((3.D0*(F3-F1)-4.D0*(F4-F5)      )*X2
     &            +  (8.D0*(F6-F5)+3.D0*F1+F2-4.D0*F4)*X3) * XSUR30
!
      ENDDO
!
        ELSE
!
          WRITE(LU,201) ICOORD
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!      OTHERS
!      ELSEIF
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,101) IELMF,SF%NAME
101     FORMAT(1X,'VC13CC (BIEF) :',/,
     &         1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &         1X,'REAL NAME: ',A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
201   FORMAT(1X,'VC13CC (BIEF) : IMPOSSIBLE COMPONENT ',
     &       1I6,' CHECK ICOORD')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
