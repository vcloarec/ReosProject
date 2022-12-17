!                   *******************
                    SUBROUTINE MV0606_2
!                   *******************
!
     &(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     & NPOIN,NELEM,W1,W2,W3,W4,W5,W6,DIM1XA)
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    MATRIX VECTOR OPERATIONS FOR P1 TRIANGLES.
!         LIKE MV0606 BUT STORAGE OF OFF-DIAGONAL TERMS INVERTED.
!
!warning This is a copy of MV0606, only the dimensions of XA are changed.
!
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        22/03/2016
!+        V7P2
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| DIM1XA         |-->| FIRST DIMENSION OF XA
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
      USE BIEF, EX_MV0606_2 => MV0606_2
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN,DIM1XA
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
      INTEGER, INTENT(IN) :: IKLE4(*),IKLE5(*),IKLE6(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W4(*),W5(*),W6(*)
      DOUBLE PRECISION, INTENT(IN)    :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: XA(DIM1XA,*)
      DOUBLE PRECISION, INTENT(IN)    :: C
!
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      CHARACTER(LEN=1), INTENT(IN)    :: TYPDIA,TYPEXT
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
     &                  + XA(1,IELEM)  * Y(I2)
     &                  + XA(2,IELEM)  * Y(I3)
     &                  + XA(3,IELEM)  * Y(I4)
     &                  + XA(4,IELEM)  * Y(I5)
     &                  + XA(5,IELEM)  * Y(I6)
!
          W2(IELEM) =
     &                  + XA(1,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I3)
     &                  + XA(7,IELEM)  * Y(I4)
     &                  + XA(8,IELEM)  * Y(I5)
     &                  + XA(9,IELEM)  * Y(I6)
!
          W3(IELEM) =
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5)
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
     &                  + XA(1,IELEM)  * Y(I2)
     &                  + XA(2,IELEM)  * Y(I3)
     &                  + XA(3,IELEM)  * Y(I4)
     &                  + XA(4,IELEM)  * Y(I5)
     &                  + XA(5,IELEM)  * Y(I6)
!
          W2(IELEM) =
     &                  + XA(16,IELEM) * Y(I1)
     &                  + XA(6,IELEM)  * Y(I3)
     &                  + XA(7,IELEM)  * Y(I4)
     &                  + XA(8,IELEM)  * Y(I5)
     &                  + XA(9,IELEM)  * Y(I6)
!
          W3(IELEM) =
     &                  + XA(17,IELEM) * Y(I1)
     &                  + XA(21,IELEM) * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(18,IELEM) * Y(I1)
     &                  + XA(22,IELEM) * Y(I2)
     &                  + XA(25,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(19,IELEM) * Y(I1)
     &                  + XA(23,IELEM) * Y(I2)
     &                  + XA(26,IELEM) * Y(I3)
     &                  + XA(28,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(20,IELEM) * Y(I1)
     &                  + XA(24,IELEM) * Y(I2)
     &                  + XA(27,IELEM) * Y(I3)
     &                  + XA(29,IELEM) * Y(I4)
     &                  + XA(30,IELEM) * Y(I5)
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
     &                  - XA(1,IELEM) * Y(I2)
     &                  - XA(2,IELEM) * Y(I3)
     &                  - XA(3,IELEM) * Y(I4)
     &                  - XA(4,IELEM) * Y(I5)
     &                  - XA(5,IELEM) * Y(I6)
!
          W2(IELEM) =
     &                  - XA(1,IELEM) * Y(I1)
     &                  - XA(6,IELEM) * Y(I3)
     &                  - XA(7,IELEM) * Y(I4)
     &                  - XA(8,IELEM) * Y(I5)
     &                  - XA(9,IELEM) * Y(I6)
!
          W3(IELEM) =
     &                  - XA(2,IELEM)  * Y(I1)
     &                  - XA(6,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I4)
     &                  - XA(11,IELEM) * Y(I5)
     &                  - XA(12,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(3,IELEM)  * Y(I1)
     &                  - XA(7,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I5)
     &                  - XA(14,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(4,IELEM)  * Y(I1)
     &                  - XA(8,IELEM)  * Y(I2)
     &                  - XA(11,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(5,IELEM)  * Y(I1)
     &                  - XA(9,IELEM)  * Y(I2)
     &                  - XA(12,IELEM) * Y(I3)
     &                  - XA(14,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I5)
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
     &                  - XA(1,IELEM)  * Y(I2)
     &                  - XA(2,IELEM)  * Y(I3)
     &                  - XA(3,IELEM)  * Y(I4)
     &                  - XA(4,IELEM)  * Y(I5)
     &                  - XA(5,IELEM)  * Y(I6)
!
          W2(IELEM) =
     &                  - XA(16,IELEM) * Y(I1)
     &                  - XA(6,IELEM)  * Y(I3)
     &                  - XA(7,IELEM)  * Y(I4)
     &                  - XA(8,IELEM)  * Y(I5)
     &                  - XA(9,IELEM)  * Y(I6)
!
          W3(IELEM) =
     &                  - XA(17,IELEM) * Y(I1)
     &                  - XA(21,IELEM) * Y(I2)
     &                  - XA(10,IELEM) * Y(I4)
     &                  - XA(11,IELEM) * Y(I5)
     &                  - XA(12,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(18,IELEM) * Y(I1)
     &                  - XA(22,IELEM) * Y(I2)
     &                  - XA(25,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I5)
     &                  - XA(14,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(19,IELEM) * Y(I1)
     &                  - XA(23,IELEM) * Y(I2)
     &                  - XA(26,IELEM) * Y(I3)
     &                  - XA(28,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(20,IELEM) * Y(I1)
     &                  - XA(24,IELEM) * Y(I2)
     &                  - XA(27,IELEM) * Y(I3)
     &                  - XA(29,IELEM) * Y(I4)
     &                  - XA(30,IELEM) * Y(I5)
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
     &                  + XA(1,IELEM) * Y(I2)
     &                  + XA(2,IELEM) * Y(I3)
     &                  + XA(3,IELEM) * Y(I4)
     &                  + XA(4,IELEM) * Y(I5)
     &                  + XA(5,IELEM) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(1,IELEM) * Y(I1)
     &                  + XA(6,IELEM) * Y(I3)
     &                  + XA(7,IELEM) * Y(I4)
     &                  + XA(8,IELEM) * Y(I5)
     &                  + XA(9,IELEM) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5)
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
     &                  + XA(1,IELEM)  * Y(I2)
     &                  + XA(2,IELEM)  * Y(I3)
     &                  + XA(3,IELEM)  * Y(I4)
     &                  + XA(4,IELEM)  * Y(I5)
     &                  + XA(5,IELEM)  * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(16,IELEM) * Y(I1)
     &                  + XA(6,IELEM)  * Y(I3)
     &                  + XA(7,IELEM)  * Y(I4)
     &                  + XA(8,IELEM)  * Y(I5)
     &                  + XA(9,IELEM)  * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(17,IELEM) * Y(I1)
     &                  + XA(21,IELEM) * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(18,IELEM) * Y(I1)
     &                  + XA(22,IELEM) * Y(I2)
     &                  + XA(25,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(19,IELEM) * Y(I1)
     &                  + XA(23,IELEM) * Y(I2)
     &                  + XA(26,IELEM) * Y(I3)
     &                  + XA(28,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(20,IELEM) * Y(I1)
     &                  + XA(24,IELEM) * Y(I2)
     &                  + XA(27,IELEM) * Y(I3)
     &                  + XA(29,IELEM) * Y(I4)
     &                  + XA(30,IELEM) * Y(I5)
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
     &                  - XA(1,IELEM) * Y(I2)
     &                  - XA(2,IELEM) * Y(I3)
     &                  - XA(3,IELEM) * Y(I4)
     &                  - XA(4,IELEM) * Y(I5)
     &                  - XA(5,IELEM) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(1,IELEM) * Y(I1)
     &                  - XA(6,IELEM) * Y(I3)
     &                  - XA(7,IELEM) * Y(I4)
     &                  - XA(8,IELEM) * Y(I5)
     &                  - XA(9,IELEM) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(2,IELEM)  * Y(I1)
     &                  - XA(6,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I4)
     &                  - XA(11,IELEM) * Y(I5)
     &                  - XA(12,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(3,IELEM)  * Y(I1)
     &                  - XA(7,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I5)
     &                  - XA(14,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(4,IELEM)  * Y(I1)
     &                  - XA(8,IELEM)  * Y(I2)
     &                  - XA(11,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(5,IELEM)  * Y(I1)
     &                  - XA(9,IELEM)  * Y(I2)
     &                  - XA(12,IELEM) * Y(I3)
     &                  - XA(14,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I5)
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
     &                  - XA(1,IELEM) * Y(I2)
     &                  - XA(2,IELEM) * Y(I3)
     &                  - XA(3,IELEM) * Y(I4)
     &                  - XA(4,IELEM) * Y(I5)
     &                  - XA(5,IELEM) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(16,IELEM) * Y(I1)
     &                  - XA(6,IELEM) * Y(I3)
     &                  - XA(7,IELEM) * Y(I4)
     &                  - XA(8,IELEM) * Y(I5)
     &                  - XA(9,IELEM) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(17,IELEM) * Y(I1)
     &                  - XA(21,IELEM) * Y(I2)
     &                  - XA(10,IELEM) * Y(I4)
     &                  - XA(11,IELEM) * Y(I5)
     &                  - XA(12,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(18,IELEM) * Y(I1)
     &                  - XA(22,IELEM) * Y(I2)
     &                  - XA(25,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I5)
     &                  - XA(14,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(19,IELEM) * Y(I1)
     &                  - XA(23,IELEM) * Y(I2)
     &                  - XA(26,IELEM) * Y(I3)
     &                  - XA(28,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(20,IELEM) * Y(I1)
     &                  - XA(24,IELEM) * Y(I2)
     &                  - XA(27,IELEM) * Y(I3)
     &                  - XA(29,IELEM) * Y(I4)
     &                  - XA(30,IELEM) * Y(I5)
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
     &                  + XA(1,IELEM) * Y(I2)
     &                  + XA(2,IELEM) * Y(I3)
     &                  + XA(3,IELEM) * Y(I4)
     &                  + XA(4,IELEM) * Y(I5)
     &                  + XA(5,IELEM) * Y(I6)  )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(1,IELEM) * Y(I1)
     &                  + XA(6,IELEM) * Y(I3)
     &                  + XA(7,IELEM) * Y(I4)
     &                  + XA(8,IELEM) * Y(I5)
     &                  + XA(9,IELEM) * Y(I6)  )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM)*  Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5) )
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
     &                  + XA(1,IELEM) * Y(I2)
     &                  + XA(2,IELEM) * Y(I3)
     &                  + XA(3,IELEM) * Y(I4)
     &                  + XA(4,IELEM) * Y(I5)
     &                  + XA(5,IELEM) * Y(I6) )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(16,IELEM) * Y(I1)
     &                  + XA(6,IELEM) * Y(I3)
     &                  + XA(7,IELEM) * Y(I4)
     &                  + XA(8,IELEM) * Y(I5)
     &                  + XA(9,IELEM) * Y(I6) )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(17,IELEM) * Y(I1)
     &                  + XA(21,IELEM) * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM) * Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(18,IELEM) * Y(I1)
     &                  + XA(22,IELEM) * Y(I2)
     &                  + XA(25,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(19,IELEM) * Y(I1)
     &                  + XA(23,IELEM) * Y(I2)
     &                  + XA(26,IELEM) * Y(I3)
     &                  + XA(28,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(20,IELEM) * Y(I1)
     &                  + XA(24,IELEM) * Y(I2)
     &                  + XA(27,IELEM) * Y(I3)
     &                  + XA(29,IELEM) * Y(I4)
     &                  + XA(30,IELEM) * Y(I5) )
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
     &                  + XA(1,IELEM) * Y(I2)
     &                  + XA(2,IELEM) * Y(I3)
     &                  + XA(3,IELEM) * Y(I4)
     &                  + XA(4,IELEM) * Y(I5)
     &                  + XA(5,IELEM) * Y(I6)
!
          W2(IELEM) =
     &                  + XA(1,IELEM) * Y(I1)
     &                  + XA(6,IELEM) * Y(I3)
     &                  + XA(7,IELEM) * Y(I4)
     &                  + XA(8,IELEM) * Y(I5)
     &                  + XA(9,IELEM) * Y(I6)
!
          W3(IELEM) =
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5)
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
     &                  + XA(16,IELEM) * Y(I2)
     &                  + XA(17,IELEM) * Y(I3)
     &                  + XA(18,IELEM) * Y(I4)
     &                  + XA(19,IELEM) * Y(I5)
     &                  + XA(20,IELEM) * Y(I6)
!
          W2(IELEM) =
     &                  + XA(1,IELEM)  * Y(I1)
     &                  + XA(21,IELEM) * Y(I3)
     &                  + XA(22,IELEM) * Y(I4)
     &                  + XA(23,IELEM) * Y(I5)
     &                  + XA(24,IELEM) * Y(I6)
!
          W3(IELEM) =
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(25,IELEM) * Y(I4)
     &                  + XA(26,IELEM) * Y(I5)
     &                  + XA(27,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(28,IELEM) * Y(I5)
     &                  + XA(29,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(30,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5)
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
     &                  - XA(1,IELEM) * Y(I2)
     &                  - XA(2,IELEM) * Y(I3)
     &                  - XA(3,IELEM) * Y(I4)
     &                  - XA(4,IELEM) * Y(I5)
     &                  - XA(5,IELEM) * Y(I6)
!
          W2(IELEM) =
     &                  - XA(1,IELEM) * Y(I1)
     &                  - XA(6,IELEM) * Y(I3)
     &                  - XA(7,IELEM) * Y(I4)
     &                  - XA(8,IELEM) * Y(I5)
     &                  - XA(9,IELEM) * Y(I6)
!
          W3(IELEM) =
     &                  - XA(2,IELEM)  * Y(I1)
     &                  - XA(6,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I4)
     &                  - XA(11,IELEM) * Y(I5)
     &                  - XA(12,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(3,IELEM)  * Y(I1)
     &                  - XA(7,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I5)
     &                  - XA(14,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(4,IELEM)  * Y(I1)
     &                  - XA(8,IELEM)  * Y(I2)
     &                  - XA(11,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(5,IELEM)  * Y(I1)
     &                  - XA(9,IELEM)  * Y(I2)
     &                  - XA(12,IELEM) * Y(I3)
     &                  - XA(14,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I5)
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
     &                  - XA(16,IELEM) * Y(I2)
     &                  - XA(17,IELEM) * Y(I3)
     &                  - XA(18,IELEM) * Y(I4)
     &                  - XA(19,IELEM) * Y(I5)
     &                  - XA(20,IELEM) * Y(I6)
!
          W2(IELEM) =
     &                  - XA(1,IELEM)  * Y(I1)
     &                  - XA(21,IELEM) * Y(I3)
     &                  - XA(22,IELEM) * Y(I4)
     &                  - XA(23,IELEM) * Y(I5)
     &                  - XA(24,IELEM) * Y(I6)
!
          W3(IELEM) =
     &                  - XA(2,IELEM)  * Y(I1)
     &                  - XA(6,IELEM)  * Y(I2)
     &                  - XA(25,IELEM) * Y(I4)
     &                  - XA(26,IELEM) * Y(I5)
     &                  - XA(27,IELEM) * Y(I6)
!
          W4(IELEM) =
     &                  - XA(3,IELEM)  * Y(I1)
     &                  - XA(7,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I3)
     &                  - XA(28,IELEM) * Y(I5)
     &                  - XA(29,IELEM) * Y(I6)
!
          W5(IELEM) =
     &                  - XA(4,IELEM)  * Y(I1)
     &                  - XA(8,IELEM)  * Y(I2)
     &                  - XA(11,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I4)
     &                  - XA(30,IELEM) * Y(I6)
!
          W6(IELEM) =
     &                  - XA(5,IELEM)  * Y(I1)
     &                  - XA(9,IELEM)  * Y(I2)
     &                  - XA(12,IELEM) * Y(I3)
     &                  - XA(14,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I5)
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
     &                  + XA(1,IELEM) * Y(I2)
     &                  + XA(2,IELEM) * Y(I3)
     &                  + XA(3,IELEM) * Y(I4)
     &                  + XA(4,IELEM) * Y(I5)
     &                  + XA(5,IELEM) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(1,IELEM) * Y(I1)
     &                  + XA(6,IELEM) * Y(I3)
     &                  + XA(7,IELEM) * Y(I4)
     &                  + XA(8,IELEM) * Y(I5)
     &                  + XA(9,IELEM) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5)
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
     &                  + XA(16,IELEM) * Y(I2)
     &                  + XA(17,IELEM) * Y(I3)
     &                  + XA(18,IELEM) * Y(I4)
     &                  + XA(19,IELEM) * Y(I5)
     &                  + XA(20,IELEM) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  + XA(1,IELEM)  * Y(I1)
     &                  + XA(21,IELEM) * Y(I3)
     &                  + XA(22,IELEM) * Y(I4)
     &                  + XA(23,IELEM) * Y(I5)
     &                  + XA(24,IELEM) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(25,IELEM) * Y(I4)
     &                  + XA(26,IELEM) * Y(I5)
     &                  + XA(27,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(28,IELEM) * Y(I5)
     &                  + XA(29,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(30,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5)
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
     &                  - XA(1,IELEM) * Y(I2)
     &                  - XA(2,IELEM) * Y(I3)
     &                  - XA(3,IELEM) * Y(I4)
     &                  - XA(4,IELEM) * Y(I5)
     &                  - XA(5,IELEM) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(1,IELEM) * Y(I1)
     &                  - XA(6,IELEM) * Y(I3)
     &                  - XA(7,IELEM) * Y(I4)
     &                  - XA(8,IELEM) * Y(I5)
     &                  - XA(9,IELEM) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(2,IELEM)  * Y(I1)
     &                  - XA(6,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I4)
     &                  - XA(11,IELEM) * Y(I5)
     &                  - XA(12,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(3,IELEM)  * Y(I1)
     &                  - XA(7,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I5)
     &                  - XA(14,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(4,IELEM)  * Y(I1)
     &                  - XA(8,IELEM)  * Y(I2)
     &                  - XA(11,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(5,IELEM)  * Y(I1)
     &                  - XA(9,IELEM)  * Y(I2)
     &                  - XA(12,IELEM) * Y(I3)
     &                  - XA(14,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I5)
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
     &                  - XA(16,IELEM) * Y(I2)
     &                  - XA(17,IELEM) * Y(I3)
     &                  - XA(18,IELEM) * Y(I4)
     &                  - XA(19,IELEM) * Y(I5)
     &                  - XA(20,IELEM) * Y(I6)
!
          W2(IELEM) = W2(IELEM)
     &                  - XA(1,IELEM)  * Y(I1)
     &                  - XA(21,IELEM) * Y(I3)
     &                  - XA(22,IELEM) * Y(I4)
     &                  - XA(23,IELEM) * Y(I5)
     &                  - XA(24,IELEM) * Y(I6)
!
          W3(IELEM) = W3(IELEM)
     &                  - XA(2,IELEM)  * Y(I1)
     &                  - XA(6,IELEM)  * Y(I2)
     &                  - XA(25,IELEM) * Y(I4)
     &                  - XA(26,IELEM) * Y(I5)
     &                  - XA(27,IELEM) * Y(I6)
!
          W4(IELEM) = W4(IELEM)
     &                  - XA(3,IELEM)  * Y(I1)
     &                  - XA(7,IELEM)  * Y(I2)
     &                  - XA(10,IELEM) * Y(I3)
     &                  - XA(28,IELEM) * Y(I5)
     &                  - XA(29,IELEM) * Y(I6)
!
          W5(IELEM) = W5(IELEM)
     &                  - XA(4,IELEM)  * Y(I1)
     &                  - XA(8,IELEM)  * Y(I2)
     &                  - XA(11,IELEM) * Y(I3)
     &                  - XA(13,IELEM) * Y(I4)
     &                  - XA(30,IELEM) * Y(I6)
!
          W6(IELEM) = W6(IELEM)
     &                  - XA(5,IELEM)  * Y(I1)
     &                  - XA(9,IELEM)  * Y(I2)
     &                  - XA(12,IELEM) * Y(I3)
     &                  - XA(14,IELEM) * Y(I4)
     &                  - XA(15,IELEM) * Y(I5)
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
     &                  + XA(1,IELEM) * Y(I2)
     &                  + XA(2,IELEM) * Y(I3)
     &                  + XA(3,IELEM) * Y(I4)
     &                  + XA(4,IELEM) * Y(I5)
     &                  + XA(5,IELEM) * Y(I6)  )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(1,IELEM) * Y(I1)
     &                  + XA(6,IELEM) * Y(I3)
     &                  + XA(7,IELEM) * Y(I4)
     &                  + XA(8,IELEM) * Y(I5)
     &                  + XA(9,IELEM) * Y(I6)  )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I4)
     &                  + XA(11,IELEM) * Y(I5)
     &                  + XA(12,IELEM)*  Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I5)
     &                  + XA(14,IELEM) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5) )
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
     &                  + XA(16,IELEM) * Y(I2)
     &                  + XA(17,IELEM) * Y(I3)
     &                  + XA(18,IELEM) * Y(I4)
     &                  + XA(19,IELEM) * Y(I5)
     &                  + XA(20,IELEM) * Y(I6) )
!
          W2(IELEM) = W2(IELEM) + C * (
     &                  + XA(1,IELEM)  * Y(I1)
     &                  + XA(21,IELEM) * Y(I3)
     &                  + XA(22,IELEM) * Y(I4)
     &                  + XA(23,IELEM) * Y(I5)
     &                  + XA(24,IELEM) * Y(I6) )
!
          W3(IELEM) = W3(IELEM) + C * (
     &                  + XA(2,IELEM)  * Y(I1)
     &                  + XA(6,IELEM)  * Y(I2)
     &                  + XA(25,IELEM) * Y(I4)
     &                  + XA(26,IELEM) * Y(I5)
     &                  + XA(27,IELEM) * Y(I6) )
!
          W4(IELEM) = W4(IELEM) + C * (
     &                  + XA(3,IELEM)  * Y(I1)
     &                  + XA(7,IELEM)  * Y(I2)
     &                  + XA(10,IELEM) * Y(I3)
     &                  + XA(28,IELEM) * Y(I5)
     &                  + XA(29,IELEM) * Y(I6) )
!
          W5(IELEM) = W5(IELEM) + C * (
     &                  + XA(4,IELEM)  * Y(I1)
     &                  + XA(8,IELEM)  * Y(I2)
     &                  + XA(11,IELEM) * Y(I3)
     &                  + XA(13,IELEM) * Y(I4)
     &                  + XA(30,IELEM) * Y(I6) )
!
          W6(IELEM) = W6(IELEM) + C * (
     &                  + XA(5,IELEM)  * Y(I1)
     &                  + XA(9,IELEM)  * Y(I2)
     &                  + XA(12,IELEM) * Y(I3)
     &                  + XA(14,IELEM) * Y(I4)
     &                  + XA(15,IELEM) * Y(I5) )
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
1001  FORMAT(1X,'MV0606_2 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE: ',A1)
2001  FORMAT(1X,'MV0606_2 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3001  FORMAT(1X,'MV0606_2 (BIEF) : UNKNOWN OPERATION : ',A8)
!
      END

