!                   *****************
                    SUBROUTINE MV0606
!                   *****************
!
     &(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     & NPOIN,NELEM,NELMAX,W1,W2,W3,W4,W5,W6)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    MATRIX VECTOR OPERATIONS FOR P1 TRIANGLES.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND MATRIX M.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   THESE OPERATIONS ARE DIFFERENT DEPENDING ON THE DIAGONAL TYPE
!+   AND THE TYPE OF EXTRADIAGONAL TERMS.
!+
!+   IMPLEMENTED OPERATIONS:
!+
!+      OP = 'X=AY    '  : X = AY
!+      OP = 'X=-AY   '  : X = - AY
!+      OP = 'X=X+AY  '  : X = X + AY
!+      OP = 'X=X-AY  '  : X = X - AY
!+      OP = 'X=X+CAY '  : X = X + C AY
!+      OP = 'X=TAY   '  : X = TA Y (TRANSPOSE OF A)
!+      OP = 'X=-TAY  '  : X = - TA Y (- TRANSPOSE OF A)
!+      OP = 'X=X+TAY '  : X = X + TA Y
!+      OP = 'X=X-TAY '  : X = X - TA Y
!+      OP = 'X=X+CTAY'  : X = X + C TA Y
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
!+        V5P1
!+   First version
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
!+        22/03/2016
!+        V7P2
!+   Cleaning, missing STOP.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| IKLE1          |-->| FIRST POINTS OF ELEMENTS
!| IKLE2          |-->| SECOND POINTS OF ELEMENTS
!| IKLE3          |-->| THIRD POINTS OF ELEMENTS
!| IKLE4          |-->| FOURTH POINTS OF ELEMENTS
!| IKLE5          |-->| FIFTH POINTS OF ELEMENTS
!| IKLE6          |-->| SIXTH POINTS OF ELEMENTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF LINEAR POINTS
!| NPT2           |-->| NUMBER OF QUADRATIC POINTS
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| TYPDIA         |-->| TYPE OF DIAGONAL:
!|                |   | TYPDIA = 'Q' : ANY VALUE
!|                |   | TYPDIA = 'I' : IDENTITY
!|                |   | TYPDIA = '0' : ZERO
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!|                |   | TYPEXT = 'Q' : ANY VALUE
!|                |   | TYPEXT = 'S' : SYMMETRIC
!|                |   | TYPEXT = '0' : ZERO
!| W1             |<->| RESULT IN NON ASSEMBLED FORM
!| W2             |<->| RESULT IN NON ASSEMBLED FORM
!| W3             |<->| RESULT IN NON ASSEMBLED FORM
!| W4             |<->| RESULT IN NON ASSEMBLED FORM
!| W5             |<->| RESULT IN NON ASSEMBLED FORM
!| W6             |<->| RESULT IN NON ASSEMBLED FORM
!| XA             |<->| RESULT IN ASSEMBLED FORM
!| Y              |-->| VECTOR USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MV0606 => MV0606
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
      INTEGER, INTENT(IN) :: IKLE4(*),IKLE5(*),IKLE6(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W4(*),W5(*),W6(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: XA(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN) :: C
!
      CHARACTER(LEN=8), INTENT(IN) :: OP
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=AY    ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  + XA(IELEM,1)  * Y(I2)
     &                  + XA(IELEM,2)  * Y(I3)
     &                  + XA(IELEM,3)  * Y(I4)
     &                  + XA(IELEM,4)  * Y(I5)
     &                  + XA(IELEM,5)  * Y(I6)
!
          W2(IELEM) =
     &                  + XA(IELEM,1)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I3)
     &                  + XA(IELEM,7)  * Y(I4)
     &                  + XA(IELEM,8)  * Y(I5)
     &                  + XA(IELEM,9)  * Y(I6)
!
          W3(IELEM) =
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  + XA(IELEM,1)  * Y(I2)
     &                  + XA(IELEM,2)  * Y(I3)
     &                  + XA(IELEM,3)  * Y(I4)
     &                  + XA(IELEM,4)  * Y(I5)
     &                  + XA(IELEM,5)  * Y(I6)
!
          W2(IELEM) =
     &                  + XA(IELEM,16) * Y(I1)
     &                  + XA(IELEM,6)  * Y(I3)
     &                  + XA(IELEM,7)  * Y(I4)
     &                  + XA(IELEM,8)  * Y(I5)
     &                  + XA(IELEM,9)  * Y(I6)
!
          W3(IELEM) =
     &                  + XA(IELEM,17) * Y(I1)
     &                  + XA(IELEM,21) * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(IELEM,18) * Y(I1)
     &                  + XA(IELEM,22) * Y(I2)
     &                  + XA(IELEM,25) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(IELEM,19) * Y(I1)
     &                  + XA(IELEM,23) * Y(I2)
     &                  + XA(IELEM,26) * Y(I3)
     &                  + XA(IELEM,28) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(IELEM,20) * Y(I1)
     &                  + XA(IELEM,24) * Y(I2)
     &                  + XA(IELEM,27) * Y(I3)
     &                  + XA(IELEM,29) * Y(I4)
     &                  + XA(IELEM,30) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-AY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  - XA(IELEM,1) * Y(I2)
     &                  - XA(IELEM,2) * Y(I3)
     &                  - XA(IELEM,3) * Y(I4)
     &                  - XA(IELEM,4) * Y(I5)
     &                  - XA(IELEM,5) * Y(I6)
!
          W2(IELEM) =
     &                  - XA(IELEM,1) * Y(I1)
     &                  - XA(IELEM,6) * Y(I3)
     &                  - XA(IELEM,7) * Y(I4)
     &                  - XA(IELEM,8) * Y(I5)
     &                  - XA(IELEM,9) * Y(I6)
!
          W3(IELEM) =
     &                  - XA(IELEM,2)  * Y(I1)
     &                  - XA(IELEM,6)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I4)
     &                  - XA(IELEM,11) * Y(I5)
     &                  - XA(IELEM,12) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(IELEM,3)  * Y(I1)
     &                  - XA(IELEM,7)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I3)
     &                  - XA(IELEM,13) * Y(I5)
     &                  - XA(IELEM,14) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(IELEM,4)  * Y(I1)
     &                  - XA(IELEM,8)  * Y(I2)
     &                  - XA(IELEM,11) * Y(I3)
     &                  - XA(IELEM,13) * Y(I4)
     &                  - XA(IELEM,15) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(IELEM,5)  * Y(I1)
     &                  - XA(IELEM,9)  * Y(I2)
     &                  - XA(IELEM,12) * Y(I3)
     &                  - XA(IELEM,14) * Y(I4)
     &                  - XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  - XA(IELEM,1)  * Y(I2)
     &                  - XA(IELEM,2)  * Y(I3)
     &                  - XA(IELEM,3)  * Y(I4)
     &                  - XA(IELEM,4)  * Y(I5)
     &                  - XA(IELEM,5)  * Y(I6)
!
          W2(IELEM) =
     &                  - XA(IELEM,16) * Y(I1)
     &                  - XA(IELEM,6)  * Y(I3)
     &                  - XA(IELEM,7)  * Y(I4)
     &                  - XA(IELEM,8)  * Y(I5)
     &                  - XA(IELEM,9)  * Y(I6)
!
          W3(IELEM) =
     &                  - XA(IELEM,17) * Y(I1)
     &                  - XA(IELEM,21) * Y(I2)
     &                  - XA(IELEM,10) * Y(I4)
     &                  - XA(IELEM,11) * Y(I5)
     &                  - XA(IELEM,12) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(IELEM,18) * Y(I1)
     &                  - XA(IELEM,22) * Y(I2)
     &                  - XA(IELEM,25) * Y(I3)
     &                  - XA(IELEM,13) * Y(I5)
     &                  - XA(IELEM,14) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(IELEM,19) * Y(I1)
     &                  - XA(IELEM,23) * Y(I2)
     &                  - XA(IELEM,26) * Y(I3)
     &                  - XA(IELEM,28) * Y(I4)
     &                  - XA(IELEM,15) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(IELEM,20) * Y(I1)
     &                  - XA(IELEM,24) * Y(I2)
     &                  - XA(IELEM,27) * Y(I3)
     &                  - XA(IELEM,29) * Y(I4)
     &                  - XA(IELEM,30) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  + XA(IELEM,1) * Y(I2)
     &                  + XA(IELEM,2) * Y(I3)
     &                  + XA(IELEM,3) * Y(I4)
     &                  + XA(IELEM,4) * Y(I5)
     &                  + XA(IELEM,5) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(IELEM,1) * Y(I1)
     &                  + XA(IELEM,6) * Y(I3)
     &                  + XA(IELEM,7) * Y(I4)
     &                  + XA(IELEM,8) * Y(I5)
     &                  + XA(IELEM,9) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  + XA(IELEM,1)  * Y(I2)
     &                  + XA(IELEM,2)  * Y(I3)
     &                  + XA(IELEM,3)  * Y(I4)
     &                  + XA(IELEM,4)  * Y(I5)
     &                  + XA(IELEM,5)  * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(IELEM,16) * Y(I1)
     &                  + XA(IELEM,6)  * Y(I3)
     &                  + XA(IELEM,7)  * Y(I4)
     &                  + XA(IELEM,8)  * Y(I5)
     &                  + XA(IELEM,9)  * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(IELEM,17) * Y(I1)
     &                  + XA(IELEM,21) * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(IELEM,18) * Y(I1)
     &                  + XA(IELEM,22) * Y(I2)
     &                  + XA(IELEM,25) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(IELEM,19) * Y(I1)
     &                  + XA(IELEM,23) * Y(I2)
     &                  + XA(IELEM,26) * Y(I3)
     &                  + XA(IELEM,28) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(IELEM,20) * Y(I1)
     &                  + XA(IELEM,24) * Y(I2)
     &                  + XA(IELEM,27) * Y(I3)
     &                  + XA(IELEM,29) * Y(I4)
     &                  + XA(IELEM,30) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  - XA(IELEM,1) * Y(I2)
     &                  - XA(IELEM,2) * Y(I3)
     &                  - XA(IELEM,3) * Y(I4)
     &                  - XA(IELEM,4) * Y(I5)
     &                  - XA(IELEM,5) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(IELEM,1) * Y(I1)
     &                  - XA(IELEM,6) * Y(I3)
     &                  - XA(IELEM,7) * Y(I4)
     &                  - XA(IELEM,8) * Y(I5)
     &                  - XA(IELEM,9) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(IELEM,2)  * Y(I1)
     &                  - XA(IELEM,6)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I4)
     &                  - XA(IELEM,11) * Y(I5)
     &                  - XA(IELEM,12) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(IELEM,3)  * Y(I1)
     &                  - XA(IELEM,7)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I3)
     &                  - XA(IELEM,13) * Y(I5)
     &                  - XA(IELEM,14) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(IELEM,4)  * Y(I1)
     &                  - XA(IELEM,8)  * Y(I2)
     &                  - XA(IELEM,11) * Y(I3)
     &                  - XA(IELEM,13) * Y(I4)
     &                  - XA(IELEM,15) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(IELEM,5)  * Y(I1)
     &                  - XA(IELEM,9)  * Y(I2)
     &                  - XA(IELEM,12) * Y(I3)
     &                  - XA(IELEM,14) * Y(I4)
     &                  - XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  - XA(IELEM,1) * Y(I2)
     &                  - XA(IELEM,2) * Y(I3)
     &                  - XA(IELEM,3) * Y(I4)
     &                  - XA(IELEM,4) * Y(I5)
     &                  - XA(IELEM,5) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(IELEM,16) * Y(I1)
     &                  - XA(IELEM,6) * Y(I3)
     &                  - XA(IELEM,7) * Y(I4)
     &                  - XA(IELEM,8) * Y(I5)
     &                  - XA(IELEM,9) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(IELEM,17) * Y(I1)
     &                  - XA(IELEM,21) * Y(I2)
     &                  - XA(IELEM,10) * Y(I4)
     &                  - XA(IELEM,11) * Y(I5)
     &                  - XA(IELEM,12) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(IELEM,18) * Y(I1)
     &                  - XA(IELEM,22) * Y(I2)
     &                  - XA(IELEM,25) * Y(I3)
     &                  - XA(IELEM,13) * Y(I5)
     &                  - XA(IELEM,14) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(IELEM,19) * Y(I1)
     &                  - XA(IELEM,23) * Y(I2)
     &                  - XA(IELEM,26) * Y(I3)
     &                  - XA(IELEM,28) * Y(I4)
     &                  - XA(IELEM,15) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(IELEM,20) * Y(I1)
     &                  - XA(IELEM,24) * Y(I2)
     &                  - XA(IELEM,27) * Y(I3)
     &                  - XA(IELEM,29) * Y(I4)
     &                  - XA(IELEM,30) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM) + C * (
     &                  + XA(IELEM,1) * Y(I2)
     &                  + XA(IELEM,2) * Y(I3)
     &                  + XA(IELEM,3) * Y(I4)
     &                  + XA(IELEM,4) * Y(I5)
     &                  + XA(IELEM,5) * Y(I6)  )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(IELEM,1) * Y(I1)
     &                  + XA(IELEM,6) * Y(I3)
     &                  + XA(IELEM,7) * Y(I4)
     &                  + XA(IELEM,8) * Y(I5)
     &                  + XA(IELEM,9) * Y(I6)  )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12)*  Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5) )
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM) + C * (
     &                  + XA(IELEM,1) * Y(I2)
     &                  + XA(IELEM,2) * Y(I3)
     &                  + XA(IELEM,3) * Y(I4)
     &                  + XA(IELEM,4) * Y(I5)
     &                  + XA(IELEM,5) * Y(I6) )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(IELEM,16) * Y(I1)
     &                  + XA(IELEM,6) * Y(I3)
     &                  + XA(IELEM,7) * Y(I4)
     &                  + XA(IELEM,8) * Y(I5)
     &                  + XA(IELEM,9) * Y(I6) )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(IELEM,17) * Y(I1)
     &                  + XA(IELEM,21) * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12) * Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(IELEM,18) * Y(I1)
     &                  + XA(IELEM,22) * Y(I2)
     &                  + XA(IELEM,25) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(IELEM,19) * Y(I1)
     &                  + XA(IELEM,23) * Y(I2)
     &                  + XA(IELEM,26) * Y(I3)
     &                  + XA(IELEM,28) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(IELEM,20) * Y(I1)
     &                  + XA(IELEM,24) * Y(I2)
     &                  + XA(IELEM,27) * Y(I3)
     &                  + XA(IELEM,29) * Y(I4)
     &                  + XA(IELEM,30) * Y(I5) )
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  + XA(IELEM,1) * Y(I2)
     &                  + XA(IELEM,2) * Y(I3)
     &                  + XA(IELEM,3) * Y(I4)
     &                  + XA(IELEM,4) * Y(I5)
     &                  + XA(IELEM,5) * Y(I6)
!
          W2(IELEM) =
     &                  + XA(IELEM,1) * Y(I1)
     &                  + XA(IELEM,6) * Y(I3)
     &                  + XA(IELEM,7) * Y(I4)
     &                  + XA(IELEM,8) * Y(I5)
     &                  + XA(IELEM,9) * Y(I6)
!
          W3(IELEM) =
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  + XA(IELEM,16) * Y(I2)
     &                  + XA(IELEM,17) * Y(I3)
     &                  + XA(IELEM,18) * Y(I4)
     &                  + XA(IELEM,19) * Y(I5)
     &                  + XA(IELEM,20) * Y(I6)
!
          W2(IELEM) =
     &                  + XA(IELEM,1)  * Y(I1)
     &                  + XA(IELEM,21) * Y(I3)
     &                  + XA(IELEM,22) * Y(I4)
     &                  + XA(IELEM,23) * Y(I5)
     &                  + XA(IELEM,24) * Y(I6)
!
          W3(IELEM) =
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,25) * Y(I4)
     &                  + XA(IELEM,26) * Y(I5)
     &                  + XA(IELEM,27) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,28) * Y(I5)
     &                  + XA(IELEM,29) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,30) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-TAY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  - XA(IELEM,1) * Y(I2)
     &                  - XA(IELEM,2) * Y(I3)
     &                  - XA(IELEM,3) * Y(I4)
     &                  - XA(IELEM,4) * Y(I5)
     &                  - XA(IELEM,5) * Y(I6)
!
          W2(IELEM) =
     &                  - XA(IELEM,1) * Y(I1)
     &                  - XA(IELEM,6) * Y(I3)
     &                  - XA(IELEM,7) * Y(I4)
     &                  - XA(IELEM,8) * Y(I5)
     &                  - XA(IELEM,9) * Y(I6)
!
          W3(IELEM) =
     &                  - XA(IELEM,2)  * Y(I1)
     &                  - XA(IELEM,6)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I4)
     &                  - XA(IELEM,11) * Y(I5)
     &                  - XA(IELEM,12) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(IELEM,3)  * Y(I1)
     &                  - XA(IELEM,7)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I3)
     &                  - XA(IELEM,13) * Y(I5)
     &                  - XA(IELEM,14) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(IELEM,4)  * Y(I1)
     &                  - XA(IELEM,8)  * Y(I2)
     &                  - XA(IELEM,11) * Y(I3)
     &                  - XA(IELEM,13) * Y(I4)
     &                  - XA(IELEM,15) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(IELEM,5)  * Y(I1)
     &                  - XA(IELEM,9)  * Y(I2)
     &                  - XA(IELEM,12) * Y(I3)
     &                  - XA(IELEM,14) * Y(I4)
     &                  - XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) =
     &                  - XA(IELEM,16) * Y(I2)
     &                  - XA(IELEM,17) * Y(I3)
     &                  - XA(IELEM,18) * Y(I4)
     &                  - XA(IELEM,19) * Y(I5)
     &                  - XA(IELEM,20) * Y(I6)
!
          W2(IELEM) =
     &                  - XA(IELEM,1)  * Y(I1)
     &                  - XA(IELEM,21) * Y(I3)
     &                  - XA(IELEM,22) * Y(I4)
     &                  - XA(IELEM,23) * Y(I5)
     &                  - XA(IELEM,24) * Y(I6)
!
          W3(IELEM) =
     &                  - XA(IELEM,2)  * Y(I1)
     &                  - XA(IELEM,6)  * Y(I2)
     &                  - XA(IELEM,25) * Y(I4)
     &                  - XA(IELEM,26) * Y(I5)
     &                  - XA(IELEM,27) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(IELEM,3)  * Y(I1)
     &                  - XA(IELEM,7)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I3)
     &                  - XA(IELEM,28) * Y(I5)
     &                  - XA(IELEM,29) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(IELEM,4)  * Y(I1)
     &                  - XA(IELEM,8)  * Y(I2)
     &                  - XA(IELEM,11) * Y(I3)
     &                  - XA(IELEM,13) * Y(I4)
     &                  - XA(IELEM,30) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(IELEM,5)  * Y(I1)
     &                  - XA(IELEM,9)  * Y(I2)
     &                  - XA(IELEM,12) * Y(I3)
     &                  - XA(IELEM,14) * Y(I4)
     &                  - XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  + XA(IELEM,1) * Y(I2)
     &                  + XA(IELEM,2) * Y(I3)
     &                  + XA(IELEM,3) * Y(I4)
     &                  + XA(IELEM,4) * Y(I5)
     &                  + XA(IELEM,5) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(IELEM,1) * Y(I1)
     &                  + XA(IELEM,6) * Y(I3)
     &                  + XA(IELEM,7) * Y(I4)
     &                  + XA(IELEM,8) * Y(I5)
     &                  + XA(IELEM,9) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5)
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  + XA(IELEM,16) * Y(I2)
     &                  + XA(IELEM,17) * Y(I3)
     &                  + XA(IELEM,18) * Y(I4)
     &                  + XA(IELEM,19) * Y(I5)
     &                  + XA(IELEM,20) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(IELEM,1)  * Y(I1)
     &                  + XA(IELEM,21) * Y(I3)
     &                  + XA(IELEM,22) * Y(I4)
     &                  + XA(IELEM,23) * Y(I5)
     &                  + XA(IELEM,24) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,25) * Y(I4)
     &                  + XA(IELEM,26) * Y(I5)
     &                  + XA(IELEM,27) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,28) * Y(I5)
     &                  + XA(IELEM,29) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,30) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  - XA(IELEM,1) * Y(I2)
     &                  - XA(IELEM,2) * Y(I3)
     &                  - XA(IELEM,3) * Y(I4)
     &                  - XA(IELEM,4) * Y(I5)
     &                  - XA(IELEM,5) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(IELEM,1) * Y(I1)
     &                  - XA(IELEM,6) * Y(I3)
     &                  - XA(IELEM,7) * Y(I4)
     &                  - XA(IELEM,8) * Y(I5)
     &                  - XA(IELEM,9) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(IELEM,2)  * Y(I1)
     &                  - XA(IELEM,6)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I4)
     &                  - XA(IELEM,11) * Y(I5)
     &                  - XA(IELEM,12) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(IELEM,3)  * Y(I1)
     &                  - XA(IELEM,7)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I3)
     &                  - XA(IELEM,13) * Y(I5)
     &                  - XA(IELEM,14) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(IELEM,4)  * Y(I1)
     &                  - XA(IELEM,8)  * Y(I2)
     &                  - XA(IELEM,11) * Y(I3)
     &                  - XA(IELEM,13) * Y(I4)
     &                  - XA(IELEM,15) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(IELEM,5)  * Y(I1)
     &                  - XA(IELEM,9)  * Y(I2)
     &                  - XA(IELEM,12) * Y(I3)
     &                  - XA(IELEM,14) * Y(I4)
     &                  - XA(IELEM,15) * Y(I5)
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM)
     &                  - XA(IELEM,16) * Y(I2)
     &                  - XA(IELEM,17) * Y(I3)
     &                  - XA(IELEM,18) * Y(I4)
     &                  - XA(IELEM,19) * Y(I5)
     &                  - XA(IELEM,20) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(IELEM,1)  * Y(I1)
     &                  - XA(IELEM,21) * Y(I3)
     &                  - XA(IELEM,22) * Y(I4)
     &                  - XA(IELEM,23) * Y(I5)
     &                  - XA(IELEM,24) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(IELEM,2)  * Y(I1)
     &                  - XA(IELEM,6)  * Y(I2)
     &                  - XA(IELEM,25) * Y(I4)
     &                  - XA(IELEM,26) * Y(I5)
     &                  - XA(IELEM,27) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(IELEM,3)  * Y(I1)
     &                  - XA(IELEM,7)  * Y(I2)
     &                  - XA(IELEM,10) * Y(I3)
     &                  - XA(IELEM,28) * Y(I5)
     &                  - XA(IELEM,29) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(IELEM,4)  * Y(I1)
     &                  - XA(IELEM,8)  * Y(I2)
     &                  - XA(IELEM,11) * Y(I3)
     &                  - XA(IELEM,13) * Y(I4)
     &                  - XA(IELEM,30) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(IELEM,5)  * Y(I1)
     &                  - XA(IELEM,9)  * Y(I2)
     &                  - XA(IELEM,12) * Y(I3)
     &                  - XA(IELEM,14) * Y(I4)
     &                  - XA(IELEM,15) * Y(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM) + C * (
     &                  + XA(IELEM,1) * Y(I2)
     &                  + XA(IELEM,2) * Y(I3)
     &                  + XA(IELEM,3) * Y(I4)
     &                  + XA(IELEM,4) * Y(I5)
     &                  + XA(IELEM,5) * Y(I6)  )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(IELEM,1) * Y(I1)
     &                  + XA(IELEM,6) * Y(I3)
     &                  + XA(IELEM,7) * Y(I4)
     &                  + XA(IELEM,8) * Y(I5)
     &                  + XA(IELEM,9) * Y(I6)  )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I4)
     &                  + XA(IELEM,11) * Y(I5)
     &                  + XA(IELEM,12)*  Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,13) * Y(I5)
     &                  + XA(IELEM,14) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,15) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5) )
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
          I1 = IKLE1(IELEM)
          I2 = IKLE2(IELEM)
          I3 = IKLE3(IELEM)
          I4 = IKLE4(IELEM)
          I5 = IKLE5(IELEM)
          I6 = IKLE6(IELEM)
!
          W1(IELEM) = W1(IELEM) + C * (
     &                  + XA(IELEM,16) * Y(I2)
     &                  + XA(IELEM,17) * Y(I3)
     &                  + XA(IELEM,18) * Y(I4)
     &                  + XA(IELEM,19) * Y(I5)
     &                  + XA(IELEM,20) * Y(I6) )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(IELEM,1)  * Y(I1)
     &                  + XA(IELEM,21) * Y(I3)
     &                  + XA(IELEM,22) * Y(I4)
     &                  + XA(IELEM,23) * Y(I5)
     &                  + XA(IELEM,24) * Y(I6) )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(IELEM,2)  * Y(I1)
     &                  + XA(IELEM,6)  * Y(I2)
     &                  + XA(IELEM,25) * Y(I4)
     &                  + XA(IELEM,26) * Y(I5)
     &                  + XA(IELEM,27) * Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(IELEM,3)  * Y(I1)
     &                  + XA(IELEM,7)  * Y(I2)
     &                  + XA(IELEM,10) * Y(I3)
     &                  + XA(IELEM,28) * Y(I5)
     &                  + XA(IELEM,29) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(IELEM,4)  * Y(I1)
     &                  + XA(IELEM,8)  * Y(I2)
     &                  + XA(IELEM,11) * Y(I3)
     &                  + XA(IELEM,13) * Y(I4)
     &                  + XA(IELEM,30) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(IELEM,5)  * Y(I1)
     &                  + XA(IELEM,9)  * Y(I2)
     &                  + XA(IELEM,12) * Y(I3)
     &                  + XA(IELEM,14) * Y(I4)
     &                  + XA(IELEM,15) * Y(I5) )
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,3001) OP
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
!
1001  FORMAT(1X,'MV0606 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2001  FORMAT(1X,'MV0606 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3001  FORMAT(1X,'MV0606 (BIEF) : UNKNOWN OPERATION : ',A8)
!
      END
