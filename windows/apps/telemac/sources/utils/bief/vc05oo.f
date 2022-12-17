!                   *****************
                    SUBROUTINE VC05OO
!                   *****************
!
     &(XMUL,SU,SV,U,V,XNOR,YNOR,LGSEG,IKLE,NBOR,NELEM,NELMAX,W1,W2 )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /             ->   ->
!+    VEC(I) = XMUL  /    PSI(I) *  U  . N  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 SEGMENT
!+    ->
!+    U IS A VECTOR WITH COMPONENTS U AND V
!+    ->
!+    N IS THE OUTGOING NORMAL VECTOR TO THE ELEMENT
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)
!+        29/05/08
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
!| IKLE           |-->| CONNECTIVITY TABLE
!| LGSEG          |-->| LENGTH OF SEGMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
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
      USE BIEF, EX_VC05OO => VC05OO
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
!     STRUCTURES OF U, V AND REAL DATA
!
      TYPE(BIEF_OBJ)  , INTENT(IN) :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N1,N2,NG1,NG2,IELEM,IELMU,IELMV
      DOUBLE PRECISION XSUR06,U1,U2,V1,V2,VX1,VY1,VX2,VY2
!
!-----------------------------------------------------------------------
!
      XSUR06 = XMUL/6.D0
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
!
!-----------------------------------------------------------------------
!         ->
!   F AND U LINEAR FUNCTIONS ON TRIANGLES OR QUADRILATERALS
!
      IF( (IELMU.EQ.11.OR.IELMU.EQ.12.OR.IELMU.EQ.21) .AND.
     &    (IELMV.EQ.11.OR.IELMV.EQ.12.OR.IELMV.EQ.21)       ) THEN
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
      U1 = U(NG1)
      U2 = U(NG2)
      V1 = V(NG1)
      V2 = V(NG2)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR06 * ( U2 + U1 + U1 )
      VY1 = XSUR06 * ( V2 + V1 + V1 )
      VX2 = XSUR06 * ( U1 + U2 + U2 )
      VY2 = XSUR06 * ( V1 + V2 + V2 )
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO
!
!-----------------------------------------------------------------------
!   ->
!   U LINEAR FUNCTIONS ON SEGMENTS
!
      ELSEIF(IELMU.EQ.1.AND.IELMV.EQ.1) THEN
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
      U1 = U(N1)
      U2 = U(N2)
      V1 = V(N1)
      V2 = V(N2)
!
!   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
!
      VX1 = XSUR06 * ( U2 + U1 + U1 )
      VY1 = XSUR06 * ( V2 + V1 + V1 )
      VX2 = XSUR06 * ( U1 + U2 + U2 )
      VY2 = XSUR06 * ( V1 + V2 + V2 )
!
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
        WRITE(LU,110)
        WRITE(LU,112) IELMU,SU%NAME
        WRITE(LU,113) IELMV,SV%NAME
        WRITE(LU,114)
110     FORMAT(1X,'VC05OO (BIEF):')
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
