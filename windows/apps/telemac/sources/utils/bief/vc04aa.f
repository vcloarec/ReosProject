!                   *****************
                    SUBROUTINE VC04AA
!                   *****************
!
     &(XMUL,SU,SV,U,V,XEL,YEL,
     & IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W1,W2,W3,SPECAD)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /          DPSI     DPSI
!+      V  =  XMUL   /       ( U  --  + V  --  ) D(OMEGA)
!+       I          /OMEGA        DX       DY
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!
!note     SEE TESTS ON FORMUL TO UNDERSTAND HOW U AND V ARE DEALT WITH:
!+
!+     IF FORMUL(7:7).EQ.' ' : VELOCITY WITH COMPONENTS U AND V LINEAR.
!+
!+     IF FORMUL(7:7).EQ.'2' : VELOCITY EQUALS U*GRAD(V) U AND V LINEAR
!+                                 BUT V MAY BE PIECE-WISE LINEAR SO V%DIMDISC
!+                                 IS LOOKED AT IN THIS CASE.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)
!+        01/06/06
!+        V5P7
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
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SPECAD         |-->| IF YES, SPECIAL ADVECTION FIELD, SEE ABOVE
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
      USE BIEF, EX_VC04AA => VC04AA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)   :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT):: W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
!
!     STRUCTURES OF F, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: SU,SV
      DOUBLE PRECISION, INTENT(IN)  :: U(*),V(*)
!
      LOGICAL, INTENT(IN) :: SPECAD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMU,IELMV
      DOUBLE PRECISION XSUR6,X2,Y2,X3,Y3,U1,U2,U3,V1,V2,V3
      DOUBLE PRECISION GRADVX,GRADVY,DET
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
!
      XSUR6 = XMUL/6.D0
!
!-----------------------------------------------------------------------
!
      IF(.NOT.SPECAD) THEN
!
!     VELOCITY WITH LINEAR COMPONENTS U AND V
!
        IF(IELMU.NE.11.OR.IELMV.NE.11) THEN
          WRITE(LU,201) IELMU,SU%NAME
201       FORMAT(1X,'VC04AA: DISCRETIZATION OF U:',1I6,
     &           1X,'REAL NAME: ',A6)
301       FORMAT(1X,'CASE NOT IMPLEMENTED')
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO IELEM = 1 , NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          U1 = U(IKLE1(IELEM))
          U2 = U(IKLE2(IELEM))
          U3 = U(IKLE3(IELEM))
          V1 = V(IKLE1(IELEM))
          V2 = V(IKLE2(IELEM))
          V3 = V(IKLE3(IELEM))
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!       VELOCITY EQUALS U * GRAD(V)
!       WITH VARIOUS DISCRETISATIONS OF U AND V
!
        IF(IELMV.EQ.11) THEN
!
        IF(IELMU.EQ.11) THEN
!       V LINEAR, U LINEAR
        DO IELEM = 1 , NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IKLE1(IELEM))
          V2 = V(IKLE2(IELEM))
          V3 = V(IKLE3(IELEM))
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IKLE1(IELEM))*GRADVX
          U2 = U(IKLE2(IELEM))*GRADVX
          U3 = U(IKLE3(IELEM))*GRADVX
          V1 = U(IKLE1(IELEM))*GRADVY
          V2 = U(IKLE2(IELEM))*GRADVY
          V3 = U(IKLE3(IELEM))*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSEIF(IELMU.EQ.10) THEN
!       V LINEAR, U PIECE-WISE CONSTANT
        DO IELEM = 1 , NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IKLE1(IELEM))
          V2 = V(IKLE2(IELEM))
          V3 = V(IKLE3(IELEM))
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IELEM)*GRADVX
          U2 = U(IELEM)*GRADVX
          U3 = U(IELEM)*GRADVX
          V1 = U(IELEM)*GRADVY
          V2 = U(IELEM)*GRADVY
          V3 = U(IELEM)*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSE
          WRITE(LU,*) 'WRONG DISCRETISATION OF U IN VC04AA'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        ELSEIF(IELMV.EQ.15) THEN
!
        IF(IELMU.EQ.11) THEN
!       V PIECE-WISE LINEAR, U LINEAR
        DO IELEM=1,NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IELEM         )
          V2 = V(IELEM+  NELMAX)
          V3 = V(IELEM+2*NELMAX)
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IKLE1(IELEM))*GRADVX
          U2 = U(IKLE2(IELEM))*GRADVX
          U3 = U(IKLE3(IELEM))*GRADVX
          V1 = U(IKLE1(IELEM))*GRADVY
          V2 = U(IKLE2(IELEM))*GRADVY
          V3 = U(IKLE3(IELEM))*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSEIF(IELMU.EQ.10) THEN
!       V PIECE-WISE LINEAR, U LINEAR
        DO IELEM=1,NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IELEM         )
          V2 = V(IELEM+  NELMAX)
          V3 = V(IELEM+2*NELMAX)
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IELEM)*GRADVX
          U2 = U(IELEM)*GRADVX
          U3 = U(IELEM)*GRADVX
          V1 = U(IELEM)*GRADVY
          V2 = U(IELEM)*GRADVY
          V3 = U(IELEM)*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSE
          WRITE(LU,*) 'WRONG DISCRETISATION OF U IN VC04AA'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        ELSE
          WRITE(LU,501) SV%DIMDISC
          WRITE(LU,301)
501       FORMAT(1X,'VC04AA: V%DIMDISC=',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
