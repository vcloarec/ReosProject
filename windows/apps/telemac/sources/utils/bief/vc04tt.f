!                   *****************
                    SUBROUTINE VC04TT
!                   *****************
!
     &(XMUL,SU,SV,SW,U,V,W,F,H,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4,FORMUL,SPECAD,
     & NPOIN2,NELEM2)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                  /        D(PSII)       D(PSII)
!+      V  = XMUL  /     U * ------- + V * -------   D(OMEGA)
!+       I        /OMEGA       DX            DY
!+
!+                  /             D(PSII*)           D(PSII*)
!+         = XMUL  /    DZ * U * -------- + DZ * V * --------   D(OMEGA*)
!+                /OMEGA*           DX                 DY
!+
!+    PSII IS OF TYPE P1 TETRAHEDRON
!+
!+    REAL MESH HERE, CAN BE REGARDED AS A COMPUTATION
!+    IN A TRANSFORMED MESH, BUT WITH H IN THE INTEGRAL.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!warning  IF SPECAD=.TRUE., THE ADVECTING FIELD IS NOT ONLY
!+        U AND V BUT U+DM1*GRAD(ZCONV). GRAD(ZCONV) IS HERE H, DM1 IS F
!+
!
!history  J-M HERVOUET (LNH)
!+        22/03/02
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
!+        07/09/2011
!+        V6P2
!+   Adaptation to case SPECAD=.TRUE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FORMUL         |-->| STRING WITH FORMULA OF VECTOR
!| IKLE1          |-->| FIRST POINT OF TETRAHEDRA
!| IKLE2          |-->| SECOND POINT OF TETRAHEDRA
!| IKLE3          |-->| THIRD POINT OF TETRAHEDRA
!| IKLE4          |-->| FOURTH POINT OF TETRAHEDRA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELEM2         |-->| NUMBER OF 2D ELEMENTS (CASE SPECAD)
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SPECAD         |-->| IF YES, SPECIAL ADVECTION FIELD, SEE ABOVE
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Z              |-->| ELEVATIONS OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC04TT => VC04TT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NELEM2,NPOIN2
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
      LOGICAL, INTENT(IN) :: SPECAD
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!     STRUCTURES AND THERE REAL DATA
!
      TYPE(BIEF_OBJ),   INTENT(IN) :: SU,SV,SW
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*),F(*)
      DOUBLE PRECISION, INTENT(IN) :: H(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSUR24,XSUR120,X2,X3,X4,Y2,Y3,Y4,Z1,Z2,Z3,Z4
      DOUBLE PRECISION F123,F134,F142,F243
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4
      DOUBLE PRECISION UUUU,VVVV,WWWW,H1,H2,H3,H4
      INTEGER I1,I2,I3,I4,IELEM,IELEM2,IELMU,IELMV,IELMW
      INTEGER IP,I12D,I22D,I32D,I42D
!
!**********************************************************************
!
      XSUR24  = XMUL /  24.D0
      XSUR120 = XMUL / 120.D0
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
!
!-----------------------------------------------------------------------
!
!   HORIZONTAL TERMS
!
      IF(FORMUL(14:16).EQ.'HOR') THEN
!
!   LOOP ON THE ELEMENTS
!
        IF(IELMU.EQ.51.AND.IELMV.EQ.51) THEN
!
!-----------------------------------------------------------------------
!
!  U AND V DISCRETISED IN P1 PRISM:
!
          IF(.NOT.SPECAD) THEN
!
!     STANDARD CASE
!
            DO IELEM = 1 , NELEM
!
              I1 = IKLE1(IELEM)
              I2 = IKLE2(IELEM)
              I3 = IKLE3(IELEM)
              I4 = IKLE4(IELEM)
!
              X2 = X(I2) - X(I1)
              X3 = X(I3) - X(I1)
              X4 = X(I4) - X(I1)
!
              Y2 = Y(I2) - Y(I1)
              Y3 = Y(I3) - Y(I1)
              Y4 = Y(I4) - Y(I1)
!
!             REAL MESH
!
!             Z2 = Z(I2) - Z(I1)
!             Z3 = Z(I3) - Z(I1)
!             Z4 = Z(I4) - Z(I1)
!
              U1 = U(I1)
              U2 = U(I2)
              U3 = U(I3)
              U4 = U(I4)
              V1 = V(I1)
              V2 = V(I2)
              V3 = V(I3)
              V4 = V(I4)
!
!             RETRIEVING THE LOWER PLANE NUMBER
!
              IP=(MIN(I1,I2,I3,I4)-1)/NPOIN2 +1
!
!             TRANSFORMED MESH, 0 FOR LOWER POINTS, 1 FOR UPPER POINTS
!
              Z1=DBLE((I1-1)/NPOIN2+1-IP)
              Z2=DBLE((I2-1)/NPOIN2+1-IP)-Z1
              Z3=DBLE((I3-1)/NPOIN2+1-IP)-Z1
              Z4=DBLE((I4-1)/NPOIN2+1-IP)-Z1
!
!             RETRIEVING THE 2D POINTS NUMBERS ON THE SAME VERTICAL
!
              I12D=MOD(I1-1,NPOIN2)+1
              I22D=MOD(I2-1,NPOIN2)+1
              I32D=MOD(I3-1,NPOIN2)+1
              I42D=MOD(I4-1,NPOIN2)+1
!
!             RETRIEVING THE ORIGINAL PRISM HEIGHTS ON THE VERTICAL
!             TWO OF THE FOUR HEIGHTS WILL BE EQUAL, THE TWO CORRESPONDING
!             POINTS OF THE TETRAHEDRON BEING ON THE SAME VERTICAL
!
              H1=Z(IP*NPOIN2+I12D)-Z((IP-1)*NPOIN2+I12D)
              H2=Z(IP*NPOIN2+I22D)-Z((IP-1)*NPOIN2+I22D)
              H3=Z(IP*NPOIN2+I32D)-Z((IP-1)*NPOIN2+I32D)
              H4=Z(IP*NPOIN2+I42D)-Z((IP-1)*NPOIN2+I42D)
!
!             EACH CONTRIBUTION IS THE EXITING FLUX THROUGH ADJACENT FACES / 3
!             THE TOTAL FLUX IS ZERO, SO THE RESULT IS THE ENTERING FLUX
!             THROUGH THE OPPOSITE FACE /3
!             POINT 1 :   (FACE 123 + FACE 134 + FACE 142) /3 = - (FACE 243) /3
!             POINT 2 : - (FACE 134) /3
!             POINT 3 : - (FACE 142) /3
!             POINT 4 : - (FACE 123) /3
!
!             FIJK IS IN FACT 8 TIMES THE REAL FLUX THROUGH FACE IJK
!
!             IN THE REAL MESH (WITH COEFFICIENT XSUR24)
!
!             UUUU=U1+U2+U3+U4
!             VVVV=V1+V2+V3+V4
!
!             IN THE TRANSFORMED MESH (WITH COEFFICIENT XSUR120)
!
              UUUU=(H1*U1+H2*U2+H3*U3+H4*U4+(H1+H2+H3+H4)*(U1+U2+U3+U4))
              VVVV=(H1*V1+H2*V2+H3*V3+H4*V4+(H1+H2+H3+H4)*(V1+V2+V3+V4))
!
!             FLUXES WITH OUTWARD NORMAL VECTOR
!
              F123=(Z2*Y3-Z3*Y2)*UUUU+(X2*Z3-Z2*X3)*VVVV
              F134=(Z3*Y4-Z4*Y3)*UUUU+(X3*Z4-Z3*X4)*VVVV
              F142=(Z4*Y2-Z2*Y4)*UUUU+(X4*Z2-Z4*X2)*VVVV
              F243=-F123-F134-F142
!
!             REAL MESH
!
!             W1(IELEM) = -F243*XSUR24
!             W2(IELEM) = -F134*XSUR24
!             W3(IELEM) = -F142*XSUR24
!             W4(IELEM) = -F123*XSUR24
!
!             TRANSFORMED MESH
!
              W1(IELEM) = -F243*XSUR120
              W2(IELEM) = -F134*XSUR120
              W3(IELEM) = -F142*XSUR120
              W4(IELEM) = -F123*XSUR120
!
            ENDDO
!
          ELSE
!
!           CASE WITH SPECIFIC ADVECTING FIELD
!
            DO IELEM = 1 , NELEM
!
!             CORRESPONDING 2D ELEMENT ON THE VERTICAL
!             SEE NUMBERING OF ELEMENTS WHEN PRISMS ARE
!             CUT INTO TETRAHEDRONS
!
              IELEM2 = MOD(IELEM-1,NELEM2) + 1
!
              I1 = IKLE1(IELEM)
              I2 = IKLE2(IELEM)
              I3 = IKLE3(IELEM)
              I4 = IKLE4(IELEM)
!
              X2 = X(I2) - X(I1)
              X3 = X(I3) - X(I1)
              X4 = X(I4) - X(I1)
!
              Y2 = Y(I2) - Y(I1)
              Y3 = Y(I3) - Y(I1)
              Y4 = Y(I4) - Y(I1)
!
!             REAL MESH
!
!             Z2 = Z(I2) - Z(I1)
!             Z3 = Z(I3) - Z(I1)
!             Z4 = Z(I4) - Z(I1)
!
!
!             RETRIEVING THE LOWER PLANE NUMBER
!
              IP=(MIN(I1,I2,I3,I4)-1)/NPOIN2 +1
!
!             TRANSFORMED MESH, 0 FOR LOWER POINTS, 1 FOR UPPER POINTS
!
              Z1=DBLE((I1-1)/NPOIN2+1-IP)
              Z2=DBLE((I2-1)/NPOIN2+1-IP)-Z1
              Z3=DBLE((I3-1)/NPOIN2+1-IP)-Z1
              Z4=DBLE((I4-1)/NPOIN2+1-IP)-Z1
!
!             SPECIFIC ADVECTION FIELD
!
              U1 = U(I1)+F(I1)*H(IELEM2+(1-1)*NELEM2)
              U2 = U(I2)+F(I2)*H(IELEM2+(1-1)*NELEM2)
              U3 = U(I3)+F(I3)*H(IELEM2+(1-1)*NELEM2)
              U4 = U(I4)+F(I4)*H(IELEM2+(1-1)*NELEM2)
              V1 = V(I1)+F(I1)*H(IELEM2+(2-1)*NELEM2)
              V2 = V(I2)+F(I2)*H(IELEM2+(2-1)*NELEM2)
              V3 = V(I3)+F(I3)*H(IELEM2+(2-1)*NELEM2)
              V4 = V(I4)+F(I4)*H(IELEM2+(2-1)*NELEM2)
!
!             RETRIEVING THE 2D POINTS NUMBERS ON THE SAME VERTICAL
!
              I12D=MOD(I1-1,NPOIN2)+1
              I22D=MOD(I2-1,NPOIN2)+1
              I32D=MOD(I3-1,NPOIN2)+1
              I42D=MOD(I4-1,NPOIN2)+1
!
!             RETRIEVING THE ORIGINAL PRISM HEIGHTS ON THE VERTICAL
!             TWO OF THE FOUR HEIGHTS WILL BE EQUAL, THE TWO CORRESPONDING
!             POINTS OF THE TETRAHEDRON BEING ON THE SAME VERTICAL
!
              H1=Z(IP*NPOIN2+I12D)-Z((IP-1)*NPOIN2+I12D)
              H2=Z(IP*NPOIN2+I22D)-Z((IP-1)*NPOIN2+I22D)
              H3=Z(IP*NPOIN2+I32D)-Z((IP-1)*NPOIN2+I32D)
              H4=Z(IP*NPOIN2+I42D)-Z((IP-1)*NPOIN2+I42D)
!
!             EACH CONTRIBUTION IS THE EXITING FLUX THROUGH ADJACENT FACES / 3
!             THE TOTAL FLUX IS ZERO, SO THE RESULT IS THE ENTERING FLUX
!             THROUGH THE OPPOSITE FACE /3
!             POINT 1 :   (FACE 123 + FACE 134 + FACE 142) /3 = - (FACE 243) /3
!             POINT 2 : - (FACE 134) /3
!             POINT 3 : - (FACE 142) /3
!             POINT 4 : - (FACE 123) /3
!
!             FIJK IS IN FACT 8 TIMES THE REAL FLUX THROUGH FACE IJK
!
!             IN THE REAL MESH (WITH COEFFICIENT XSUR24)
!
!             UUUU=U1+U2+U3+U4
!             VVVV=V1+V2+V3+V4
!
!             IN THE TRANSFORMED MESH (WITH COEFFICIENT XSUR120)
!             SIMPLIFYING BY TAKING AVERAGE DEPTH * AVERAGE VELOCITY
!             WITH AVERAGE DEPTH = REAL VOLUME / TRANSFORMED VOLUME
!             DOES NOT SEEM TO WORK VERY WELL (NON LINEAR WAVES, BUMPRES)
!
              UUUU=(H1*U1+H2*U2+H3*U3+H4*U4+(H1+H2+H3+H4)
     &            *(U1+U2+U3+U4))
              VVVV=(H1*V1+H2*V2+H3*V3+H4*V4+(H1+H2+H3+H4)
     &            *(V1+V2+V3+V4))
!
!             FLUXES WITH OUTWARD NORMAL VECTOR
!
              F123=(Z2*Y3-Z3*Y2)*UUUU+(X2*Z3-Z2*X3)*VVVV
              F134=(Z3*Y4-Z4*Y3)*UUUU+(X3*Z4-Z3*X4)*VVVV
              F142=(Z4*Y2-Z2*Y4)*UUUU+(X4*Z2-Z4*X2)*VVVV
              F243=-F123-F134-F142
!
!             REAL MESH
!
!             W1(IELEM) = -F243*XSUR24
!             W2(IELEM) = -F134*XSUR24
!             W3(IELEM) = -F142*XSUR24
!             W4(IELEM) = -F123*XSUR24
!
!             TRANSFORMED MESH
!
              W1(IELEM) = -F243*XSUR120
              W2(IELEM) = -F134*XSUR120
              W3(IELEM) = -F142*XSUR120
              W4(IELEM) = -F123*XSUR120
!
            ENDDO
!
          ENDIF
!
!-----------------------------------------------------------------------
!
!     ELSEIF(IELMU.EQ.  ) THEN
!
!-----------------------------------------------------------------------
!
        ELSE
!
!-----------------------------------------------------------------------
!
          WRITE(LU,102) IELMU,SU%NAME
102       FORMAT(1X,'VC04TT (BIEF) :',/,
     &        1X,'DISCRETISATION OF U ET V : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF U : ',A6)
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(14:16).EQ.'TOT') THEN
!
        IF(IELMW.NE.31.AND.IELMW.NE.51) THEN
          WRITE(LU,302)
302       FORMAT(1X,'VC04TT (BIEF) :',/,
     &           1X,'UNEXPECTED CASE (IELMW.NE.31 AND .NE.51)')
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
!
          X2 = X(I2) - X(I1)
          X3 = X(I3) - X(I1)
          X4 = X(I4) - X(I1)
!
          Y2 = Y(I2) - Y(I1)
          Y3 = Y(I3) - Y(I1)
          Y4 = Y(I4) - Y(I1)
!
          Z2 = Z(I2) - Z(I1)
          Z3 = Z(I3) - Z(I1)
          Z4 = Z(I4) - Z(I1)
!
          UUUU=U(I1)+U(I2)+U(I3)+U(I4)
          VVVV=V(I1)+V(I2)+V(I3)+V(I4)
          WWWW=W(I1)+W(I2)+W(I3)+W(I4)
!
          F123=(Z2*Y3-Z3*Y2)*UUUU+(X2*Z3-Z2*X3)*VVVV+(X3*Y2-X2*Y3)*WWWW
          F134=(Z3*Y4-Z4*Y3)*UUUU+(X3*Z4-Z3*X4)*VVVV+(X4*Y3-X3*Y4)*WWWW
          F142=(Z4*Y2-Z2*Y4)*UUUU+(X4*Z2-Z4*X2)*VVVV+(X2*Y4-X4*Y2)*WWWW
          F243=-F123-F134-F142
!
          W1(IELEM) = -F243*XSUR24
          W2(IELEM) = -F134*XSUR24
          W3(IELEM) = -F142*XSUR24
          W4(IELEM) = -F123*XSUR24
!
!
!              ORIGINAL MAPLE FORMULAS FOR HORIZONTAL PART
!
!              W1(IELEM) = XSUR24*(
!          &U3*Z2*Y3+V4*X3*Z4-U4*Y3*Z4+U4*Z2*Y3+U4*Y4*Z3-U4*Z2*Y4-U4*
!          &Y2*Z3+U1*Y4*Z3+V1*Z2*X4-V4*X2*Z4-U3*Z2*Y4-U2*Y2*Z3+V4*X2*Z3+V4*Z2*
!          &X4-V4*X4*Z3+V2*Z2*X4-U2*Z2*Y4+U2*Y2*Z4+U2*Y4*Z3+U2*Z2*Y3-V1*Z2*X3+
!          &V1*X2*Z3-V1*X4*Z3+V1*X3*Z4+U3*Y2*Z4+U3*Y4*Z3-V3*X2*Z4-V3*X4*Z3+V3*
!          &X3*Z4-V3*Z2*X3+V3*X2*Z3+V3*Z2*X4+U1*Z2*Y3-U1*Y2*Z3+U1*Y2*Z4-V2*X2*
!          &Z4-V2*X4*Z3-V2*Z2*X3+V2*X2*Z3-U3*Y2*Z3+V2*X3*Z4-V4*Z2*X3-U1*Y3*Z4+
!          &U4*Y2*Z4-U3*Y3*Z4-V1*X2*Z4-U1*Z2*Y4-U2*Y3*Z4)
!
!              W2(IELEM) = XSUR24*(
!          &-U4*Y4*Z3+U4*Y3*Z4-V4*X3*Z4+V4*X4*Z3+U1*Y3*Z4-U1*Y4*Z3-V3
!          &*X3*Z4+V3*X4*Z3+U3*Y3*Z4-U3*Y4*Z3-V1*X3*Z4+V1*X4*Z3+U2*Y3*Z4-U2*Y4
!          &*Z3-V2*X3*Z4+V2*X4*Z3)
!
!              W3(IELEM) = XSUR24*(
!          &U4*Z2*Y4-U4*Y2*Z4-V4*Z2*X4+U3*Z2*Y4+V4*X2*Z4-V1*Z2*X4-U1*
!          &Y2*Z4-V3*Z2*X4+V3*X2*Z4-U3*Y2*Z4+V1*X2*Z4-U2*Y2*Z4+U2*Z2*Y4-V2*Z2*
!          &X4+V2*X2*Z4+U1*Z2*Y4)
!
!              W4(IELEM) = XSUR24*(
!          &U4*Y2*Z3-U4*Z2*Y3+V4*Z2*X3-V4*X2*Z3+U1*Y2*Z3-U1*Z2*Y3+U3*
!          &Y2*Z3-U3*Z2*Y3-V3*X2*Z3+V3*Z2*X3-V1*X2*Z3+V1*Z2*X3-U2*Z2*Y3+U2*Y2*
!          &Z3-V2*X2*Z3+V2*Z2*X3)
!
!              ORIGINAL MAPLE FORMULAS FOR VERTICAl PART
!
!              Q1 = W(I1)
!              Q2 = W(I2)
!              Q3 = W(I3)
!              Q4 = W(I4)
!
!              W1(IELEM) = (
!          &         X4*Y3*Q4-Y2*X4*Q4+X2*Y4*Q4-X2*Y3*Q4-X3*Y4*Q4+Y2*X3*Q4-X2*
!          &Y3*Q3-Y2*X4*Q3+X2*Y4*Q1-X3*Y4*Q3+X2*Y4*Q3+Y2*X3*Q3-X3*Y4*Q2+X4*Y3*
!          &Q2+X2*Y4*Q2-Y2*X4*Q2-X2*Y3*Q2+Y2*X3*Q2+Y2*X3*Q1-Y2*X4*Q1-X2*Y3*Q1+
!          &X4*Y3*Q3+X4*Y3*Q1-X3*Y4*Q1 )*XSUR24
!
!              W2(IELEM) = (
!          &         -X4*Y3*Q4+X3*Y4*Q4+X3*Y4*Q3+X3*Y4*Q2-X4*Y3*Q2-X4*Y3*Q3-X4
!          &*Y3*Q1+X3*Y4*Q1 )*XSUR24
!
!              W3(IELEM) = (
!          &         Y2*X4*Q4-X2*Y4*Q4+Y2*X4*Q3-X2*Y4*Q1-X2*Y4*Q3-X2*Y4*Q2+Y2*
!          &X4*Q2+Y2*X4*Q1 )*XSUR24
!
!              W4(IELEM) = (
!          &         X2*Y3*Q4-Y2*X3*Q4+X2*Y3*Q3-Y2*X3*Q3+X2*Y3*Q2-Y2*X3*Q2-Y2*
!          &X3*Q1+X2*Y3*Q1 )*XSUR24
!
        ENDDO
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,202) FORMUL
202     FORMAT(1X,'VC04TT (BIEF):',/,
     &         1X,'HOR OR VER LACKING AT THE END OF THE FORMULA : ',A16)
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
