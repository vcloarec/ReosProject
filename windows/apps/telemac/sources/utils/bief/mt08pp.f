!                   *****************
                    SUBROUTINE MT08PP
!                   *****************
!
     &( T,XM,XMUL,SF,F,SURFAC,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+
!+                  /                     D
!+  A(I,J)=-XMUL   /  PSI2(J) *    F    * --( PSI1(I) ) D(OMEGA)
!+                /OMEGA                  DX
!+
!+  BEWARE THE MINUS SIGN !!
!+
!+  PSI1 AND PSI2: BASES OF TYPE P1 PRISM
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  NEED TO CHECK THE SIGN; SEE USE IN DIFF3D!!!!!!!!!!!!!!!!
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        28/11/94
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
!| F              |-->| FUNCTION USED IN THE FORMULA
!| FORMUL         |-->| FORMULA DESCRIBING THE RESULTING MATRIX
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| STRUCTURE OF FUNCTIONS F
!| SURFAC         |-->| AREA OF 2D ELEMENTS
!| T              |<->| WORK ARRAY FOR ELEMENT BY ELEMENT DIAGONAL
!| Z              |-->| ELEVATIONS OF POINTS
!| XM             |<->| OFF-DIAGONAL TERMS
!| XMUL           |-->| COEFFICIENT FOR MULTIPLICATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT08PP => MT08PP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
!
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,30)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      DOUBLE PRECISION PZ1,XSU360
      DOUBLE PRECISION Q1,Q2,Q3,Q4,Q5,Q6
      DOUBLE PRECISION W14,W41,W25,W52,W63,W36
!
      INTEGER I1,I2,I3,I4,I5,I6,IELEM
!
!**********************************************************************
!
      XSU360 = XMUL/360.D0
!
      IF(SF%ELM.NE.41) THEN
        WRITE(LU,1001) SF%ELM
1001    FORMAT(1X,'MT08PP (BIEF) : TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     LOOP ON THE ELEMENTS
!
      DO IELEM=1,NELEM
!
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        I4 = IKLE(IELEM,4)
        I5 = IKLE(IELEM,5)
        I6 = IKLE(IELEM,6)
!
        Q1  =  F(I1)
        Q2  =  F(I2)
        Q3  =  F(I3)
        Q4  =  F(I4)
        Q5  =  F(I5)
        Q6  =  F(I6)
!
!       INTERMEDIATE COMPUTATIONS
!
        PZ1=-XSU360*SURFAC(IELEM)
!
        W14 = Q1+2*Q4
        W41 = Q4+2*Q1
        W25 = Q2+2*Q5
        W52 = Q5+2*Q2
        W63 = Q6+2*Q3
        W36 = Q3+2*Q6
!
        T(IELEM,1)=PZ1*2*(3*W41+W52+W63)
        XM(IELEM,18)=-T(IELEM,1)
        XM(IELEM,16)=PZ1*(2*(W41+W52)+W63)
        XM(IELEM,19)=-XM(IELEM,16)
        XM(IELEM,1) = XM(IELEM,16)
        XM(IELEM,22)=-XM(IELEM,16)
        XM(IELEM,2)=PZ1*(2*(W41+W63)+W52)
        XM(IELEM,20)=-XM(IELEM,2)
        XM(IELEM,17)= XM(IELEM,2)
        XM(IELEM,25)=-XM(IELEM,2)
        T(IELEM,2)=PZ1*2*(W41+3*W52+W63)
        XM(IELEM,23)= -T(IELEM,2)
        XM(IELEM,21)=PZ1*(2*(W52+W63)+W41)
        XM(IELEM,24)=-XM(IELEM,21)
        XM(IELEM,6) = XM(IELEM,21)
        XM(IELEM,26)=-XM(IELEM,21)
        T(IELEM,3)=PZ1*2*(W41+W52+3*W63)
        XM(IELEM,27)=-T(IELEM,3)
        XM(IELEM,3)=PZ1*2*(3*W14+W25+W36)
        T(IELEM,4)=-XM(IELEM,3)
        XM(IELEM,7)=PZ1*(2*(W14+W25)+W36)
        XM(IELEM,28)=-XM(IELEM,7)
        XM(IELEM,4) = XM(IELEM,7)
        XM(IELEM,13)=-XM(IELEM,7)
        XM(IELEM,10)=PZ1*(2*(W14+W36)+W25)
        XM(IELEM,29)=-XM(IELEM,10)
        XM(IELEM,5) = XM(IELEM,10)
        XM(IELEM,14)=-XM(IELEM,10)
        XM(IELEM,8)=PZ1*2*(W14+3*W25+W36)
        T(IELEM,5)=-XM(IELEM,8)
        XM(IELEM,11)=PZ1*(2*(W25+W36)+W14)
        XM(IELEM,30)=-XM(IELEM,11)
        XM(IELEM,9) = XM(IELEM,11)
        XM(IELEM,15)=-XM(IELEM,11)
        XM(IELEM,12)=PZ1*2*(W14+W25+3*W36)
        T(IELEM,6)=-XM(IELEM,12)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
