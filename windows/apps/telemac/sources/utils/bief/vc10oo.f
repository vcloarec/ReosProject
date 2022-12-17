!                   *****************
                    SUBROUTINE VC10OO
!                   *****************
!
     &(XMUL,SF,SU,SV,F,U,V,XNOR,YNOR,LGSEG,
     & IKLE,NBOR,NELEM,NELMAX,W1,W2)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /               ->   ->
!+    VEC(I) = XMUL  /    PSI(I) * F  U  . N  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 SEGMENT
!+    F IS A STRUCTURE OF VECTOR
!+    ->
!+    U IS A VECTOR WITH COMPONENTS U AND V
!+    ->
!+    N IS THE OUTGOING NORMAL VECTOR TO THE ELEMENT
!
!warning  The Jacobian must be positive
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!warning  NELEM is sometimes over-dimensioned, some boundary elements in
!+        THE list 1 to NELEM in parallel being in another subdomain. In
!+        this case NBOR may give a dummy value, that will cause no
!+        crash, but the result W1 and W2 will be dummy also.
!
!history  J-M HERVOUET (LNH)
!+        15/12/1994
!+
!+
!
!history  ALGIANE FROEHLY (STAGIAIRE MATMECA)
!+        29/05/2008
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
!| IKLE           |-->| CONNECTIVITY TABLE
!| LGSEG          |-->| LENGTH OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| XNOR           |-->| FIRST COMPONENT OF NORMAL TO ELEMENT
!| YNOR           |-->| SECOND COMPONENT OF NORMAL TO ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC10OO => VC10OO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
      INTEGER, INTENT(IN) :: NBOR(*)
!
      DOUBLE PRECISION, INTENT(IN)    :: XNOR(NELMAX),YNOR(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: LGSEG(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURES OF F, U, V AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N1,N2,N3,NG1,NG2,NG3,IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION XSUR12,XSUR04,XSUR60,F1,F2,F3,U1,U2,U3
      DOUBLE PRECISION V1,V2,V3,VX1,VY1,VX2,VY2
!
!-----------------------------------------------------------------------
!
      XSUR12 = XMUL/12.D0
      XSUR04 = XMUL/ 4.D0
      XSUR60 = XMUL/60.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!         ->
!   F AND U ARE LINEAR FUNCTIONS ON TRIANGLES OR QUADRILATERALS
!
      IF( (IELMF.EQ.11.OR.IELMF.EQ.12.OR.IELMF.EQ.21) .AND.
     &    (IELMU.EQ.11.OR.IELMU.EQ.12.OR.IELMU.EQ.21) .AND.
     &    (IELMV.EQ.11.OR.IELMV.EQ.12.OR.IELMV.EQ.21)      ) THEN
!
      DO IELEM =1,NELEM
!
!     NUMBERING OF THE BOUNDARY NODES
!
!     GLOBAL NUMBERING
!
      NG1= NBOR(IKLE(IELEM,1))
      NG2= NBOR(IKLE(IELEM,2))
!
      F1 = F(NG1)
      F2 = F(NG2)
      U1 = U(NG1)
      U2 = U(NG2)
      V1 = V(NG1)
      V2 = V(NG2)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR04 * F1 * U1 + XSUR12 * ( F1 * U2 + F2 * ( U1 + U2 ) )
      VY1 = XSUR04 * F1 * V1 + XSUR12 * ( F1 * V2 + F2 * ( V1 + V2 ) )
      VX2 = XSUR04 * F2 * U2 + XSUR12 * ( F2 * U1 + F1 * ( U1 + U2 ) )
      VY2 = XSUR04 * F2 * V2 + XSUR12 * ( F2 * V1 + F1 * ( V1 + V2 ) )
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!   F LINEAR FUNCTION ON TRIANGLES OR QUADRILATERALS
!   ->
!   U LINEAR FUNCTIONS ON SEGMENTS
!
      ELSEIF( (IELMF.EQ.11.OR.IELMF.EQ.12.OR.IELMF.EQ.21) .AND.
     &        (IELMU.EQ.1                               ) .AND.
     &        (IELMV.EQ.1                               )      ) THEN
!
      DO IELEM =1,NELEM
!
!     NUMBERING OF THE BOUNDARY NODES
!
      N1 = IKLE(IELEM,1)
      N2 = IKLE(IELEM,2)
!
!     GLOBAL NUMBERING
!
      NG1= NBOR(N1)
      NG2= NBOR(N2)
!
      F1 = F(NG1)
      F2 = F(NG2)
      U1 = U(N1)
      U2 = U(N2)
      V1 = V(N1)
      V2 = V(N2)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR04 * F1 * U1 + XSUR12 * ( F1 * U2 + F2 * ( U1 + U2 ) )
      VY1 = XSUR04 * F1 * V1 + XSUR12 * ( F1 * V2 + F2 * ( V1 + V2 ) )
      VX2 = XSUR04 * F2 * U2 + XSUR12 * ( F2 * U1 + F1 * ( U1 + U2 ) )
      VY2 = XSUR04 * F2 * V2 + XSUR12 * ( F2 * V1 + F1 * ( V1 + V2 ) )
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!   F LINEAR FUNCTION ON SEGMENTS
!   ->
!   U LINEAR FUNCTIONS ON SEGMENTS
!
      ELSEIF(  IELMF.EQ.1 .AND.
     &         IELMU.EQ.1 .AND.
     &         IELMV.EQ.1        ) THEN
!
      DO IELEM =1,NELEM
!
!     NUMBERING OF THE BOUNDARY NODES
!
      N1 = IKLE(IELEM,1)
      N2 = IKLE(IELEM,2)
!
!     GLOBAL NUMBERING
!
      F1 = F(N1)
      F2 = F(N2)
      U1 = U(N1)
      U2 = U(N2)
      V1 = V(N1)
      V2 = V(N2)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR04 * F1 * U1 + XSUR12 * ( F1 * U2 + F2 * ( U1 + U2 ) )
      VY1 = XSUR04 * F1 * V1 + XSUR12 * ( F1 * V2 + F2 * ( V1 + V2 ) )
      VX2 = XSUR04 * F2 * U2 + XSUR12 * ( F2 * U1 + F1 * ( U1 + U2 ) )
      VY2 = XSUR04 * F2 * V2 + XSUR12 * ( F2 * V1 + F1 * ( V1 + V2 ) )
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO ! IELEM
!
!
!-----------------------------------------------------------------------
!
!   F LINEAR FUNCTION ON TRIANGLES
!       ->
!   AND U QUADRATIC FUNCTIONS ON TRIANGLES
!
      ELSEIF( (IELMF.EQ.11.OR.IELMF.EQ.12.OR.IELMF.EQ.21 ) .AND.
     &        (IELMU.EQ.13                               ) .AND.
     &        (IELMV.EQ.13                               )       ) THEN
!
      DO IELEM =1,NELEM
!
!     NUMBERING OF THE BOUNDARY NODES
!
!     GLOBAL NUMBERING
!
      NG1= NBOR(IKLE(IELEM,1))
      NG2= NBOR(IKLE(IELEM,2))
      NG3= NBOR(IKLE(IELEM,3))
!
      F1 = F(NG1)
      F2 = F(NG2)
      U1 = U(NG1)
      U2 = U(NG2)
      U3 = U(NG3)
      V1 = V(NG1)
      V2 = V(NG2)
      V3 = V(NG3)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR60 *(9.D0*F1*U1-F1*U2+12.D0*F1*U3+F2*(U1+U2)+8.D0*F2*U3)
      VY1 = XSUR60 *(9.D0*F1*V1-F1*V2+12.D0*F1*V3+F2*(V1+V2)+8.D0*F2*V3)
      VX2 = XSUR60 *(F1*(U1+U2)+8.D0*F1*U3-F2*U1+9.D0*F2*U2+12.D0*F2*U3)
      VY2 = XSUR60 *(F1*(V1+V2)+8.D0*F1*V3-F2*V1+9.D0*F2*V2+12.D0*F2*V3)
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!   F LINEAR FUNCTION ON TRIANGLES
!   ->
!   U QUADRATIC FUNCTIONS ON SEGMENTS
!
      ELSEIF( (IELMF.EQ.11.OR.IELMF.EQ.12.OR.IELMF.EQ.21) .AND.
     &        (IELMU.EQ.2                               ) .AND.
     &        (IELMV.EQ.2                               )       ) THEN
!
      DO IELEM =1,NELEM
!
!     NUMBERING OF THE BOUNDARY NODES
!
      N1 = IKLE(IELEM,1)
      N2 = IKLE(IELEM,2)
      N3 = IKLE(IELEM,3)
!
!     GLOBAL NUMBERING
!
      NG1= NBOR(N1)
      NG2= NBOR(N2)
      NG3= NBOR(N3)
!
      F1 = F(NG1)
      F2 = F(NG2)
      U1 = U(N1)
      U2 = U(N2)
      U3 = U(N3)
      V1 = V(N1)
      V2 = V(N2)
      V3 = V(N3)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR60 *(9.D0*F1*U1-F1*U2+12.D0*F1*U3+F2*(U1+U2)+8.D0*F2*U3)
      VY1 = XSUR60 *(9.D0*F1*V1-F1*V2+12.D0*F1*V3+F2*(V1+V2)+8.D0*F2*V3)
      VX2 = XSUR60 *(F1*(U1+U2)+8.D0*F1*U3-F2*U1+9.D0*F2*U2+12.D0*F2*U3)
      VY2 = XSUR60 *(F1*(V1+V2)+8.D0*F1*V3-F2*V1+9.D0*F2*V2+12.D0*F2*V3)
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!   F QUADRATIC FUNCTION ON SEGMENTS
!   ->
!   U QUADRATIC FUNCTIONS ON SEGMENTS
!
      ELSEIF(  IELMF.EQ.2 .AND.
     &         IELMU.EQ.2 .AND.
     &         IELMV.EQ.2        ) THEN
!
      DO IELEM =1,NELEM
!
!     NUMBERING OF THE BOUNDARY NODES
!
      N1 = IKLE(IELEM,1)
      N2 = IKLE(IELEM,2)
      N3 = IKLE(IELEM,3)
!
!     GLOBAL NUMBERING
!
      F1 = F(N1)
      F2 = F(N2)
      F3 = F(N3)
!
      U1 = U(N1)
      U2 = U(N2)
      U3 = U(N3)
!
      V1 = V(N1)
      V2 = V(N2)
      V3 = V(N3)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR60 *(9.D0*F1*U1-F1*U2+12.D0*F1*U3+F2*(U1+U2)+8.D0*F2*U3)
      VY1 = XSUR60 *(9.D0*F1*V1-F1*V2+12.D0*F1*V3+F2*(V1+V2)+8.D0*F2*V3)
      VX2 = XSUR60 *(F1*(U1+U2)+8.D0*F1*U3-F2*U1+9.D0*F2*U2+12.D0*F2*U3)
      VY2 = XSUR60 *(F1*(V1+V2)+8.D0*F1*V3-F2*V1+9.D0*F2*V2+12.D0*F2*V3)
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO ! IELEM
!
!
!-----------------------------------------------------------------------
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,110)
        WRITE(LU,111) IELMF,SF%NAME
        WRITE(LU,112) IELMU,SU%NAME
        WRITE(LU,113) IELMV,SV%NAME
        WRITE(LU,114)
110     FORMAT(1X,'VC10OO (BIEF):')
111     FORMAT(1X,'DISCRETIZATION OF F:',1I6,
     &         1X,'REAL NAME: ',A6)
112     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
113     FORMAT(1X,'DISCRETIZATION OF V:',1I6,
     &         1X,'REAL NAME: ',A6)
114     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
