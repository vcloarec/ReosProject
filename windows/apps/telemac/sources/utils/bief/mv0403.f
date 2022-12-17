!                   *****************
                    SUBROUTINE MV0403
!                   *****************
!
     &(OP, X , DA,TYPDIA,
     & XA12,XA13,XA21,XA23,XA31,XA32,XA41,XA42,XA43,
     & TYPEXT, Y,C,
     & IKLE1,IKLE2,IKLE3,IKLE4,
     & NPOIN,NELEM,W1,W2,W3,W4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MATRIX VECTOR OPERATIONS FOR Q1 QUADRILATERALS.
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
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| IKLE1          |-->| FIRST POINTS OF ELEMENTS
!| IKLE2          |-->| SECOND POINTS OF ELEMENTS
!| IKLE3          |-->| THIRD POINTS OF ELEMENTS
!| IKLE4          |-->| FOURTH POINTS OF ELEMENTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF LINEAR POINTS
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
!| X              |<->| RESULT IN ASSEMBLED FORM
!| XA13           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA21           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA23           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA31           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA32           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA41           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA42           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA43           |-->| OFF-DIAGONAL TERM OF MATRIX
!| Y              |-->| VECTOR USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MV0403 => MV0403
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN
!
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*),IKLE4(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*),W4(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: XA12(*),XA13(*),XA21(*)
      DOUBLE PRECISION, INTENT(IN) :: XA23(*),XA31(*),XA32(*)
      DOUBLE PRECISION, INTENT(IN) :: XA41(*),XA42(*),XA43(*)
      DOUBLE PRECISION, INTENT(IN) :: C
!
      CHARACTER(LEN=8), INTENT(IN) :: OP
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=AY    ') THEN
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , Z  , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
     &                    + XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =     XA21(IELEM) * Y(IKLE1(IELEM))
     &                    + XA23(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) =     XA31(IELEM) * Y(IKLE1(IELEM))
     &                    + XA32(IELEM) * Y(IKLE2(IELEM))
            W4(IELEM) =     XA41(IELEM) * Y(IKLE1(IELEM))
     &                    + XA42(IELEM) * Y(IKLE2(IELEM))
     &                    + XA43(IELEM) * Y(IKLE3(IELEM))
            X(NPOIN+IELEM)=0.D0
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', X(NPOIN+1) , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-AY   ') THEN
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , Z  , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
     &                    - XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =   - XA21(IELEM) * Y(IKLE1(IELEM))
     &                    - XA23(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) =   - XA31(IELEM) * Y(IKLE1(IELEM))
     &                    - XA32(IELEM) * Y(IKLE2(IELEM))
            W4(IELEM) =   - XA41(IELEM) * Y(IKLE1(IELEM))
     &                    - XA42(IELEM) * Y(IKLE2(IELEM))
     &                    - XA43(IELEM) * Y(IKLE3(IELEM))
            X(NPOIN+IELEM)=0.D0
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', X(NPOIN+1) , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) + XA12(IELEM) * Y(IKLE2(IELEM))
     &                            + XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM) + XA21(IELEM) * Y(IKLE1(IELEM))
     &                            + XA23(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) = W3(IELEM) + XA31(IELEM) * Y(IKLE1(IELEM))
     &                            + XA32(IELEM) * Y(IKLE2(IELEM))
            W4(IELEM) = W4(IELEM) + XA41(IELEM) * Y(IKLE1(IELEM))
     &                            + XA42(IELEM) * Y(IKLE2(IELEM))
     &                            + XA43(IELEM) * Y(IKLE3(IELEM))
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
          CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) - XA12(IELEM) * Y(IKLE2(IELEM))
     &                            - XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM) - XA21(IELEM) * Y(IKLE1(IELEM))
     &                            - XA23(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) = W3(IELEM) - XA31(IELEM) * Y(IKLE1(IELEM))
     &                            - XA32(IELEM) * Y(IKLE2(IELEM))
            W4(IELEM) = W4(IELEM) - XA41(IELEM) * Y(IKLE1(IELEM))
     &                            - XA42(IELEM) * Y(IKLE2(IELEM))
     &                            - XA43(IELEM) * Y(IKLE3(IELEM))
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
          CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM)
     &              + C * (      XA12(IELEM) * Y(IKLE2(IELEM))
     &                         + XA13(IELEM) * Y(IKLE3(IELEM)) )
            W2(IELEM) = W2(IELEM)
     &              + C * (      XA21(IELEM) * Y(IKLE1(IELEM))
     &                         + XA23(IELEM) * Y(IKLE3(IELEM)) )
            W3(IELEM) = W3(IELEM)
     &              + C * (      XA31(IELEM) * Y(IKLE1(IELEM))
     &                         + XA32(IELEM) * Y(IKLE2(IELEM)) )
            W4(IELEM) = W4(IELEM)
     &              + C * (      XA41(IELEM) * Y(IKLE1(IELEM))
     &                         + XA42(IELEM) * Y(IKLE2(IELEM))
     &                         + XA43(IELEM) * Y(IKLE3(IELEM)) )
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
          CALL OV ('X=X+CYZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+CY   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
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
          CALL PLANTE(0)
          STOP
        ENDIF
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   + XA21(IELEM) * Y(IKLE2(IELEM))
     &                    + XA31(IELEM) * Y(IKLE3(IELEM))
     &                    + XA41(IELEM) * Y(IKLE4(IELEM))
            W2(IELEM) =   + XA12(IELEM) * Y(IKLE1(IELEM))
     &                    + XA32(IELEM) * Y(IKLE3(IELEM))
     &                    + XA42(IELEM) * Y(IKLE4(IELEM))
            W3(IELEM) =   + XA13(IELEM) * Y(IKLE1(IELEM))
     &                    + XA23(IELEM) * Y(IKLE2(IELEM))
     &                    + XA43(IELEM) * Y(IKLE4(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-TAY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   - XA21(IELEM) * Y(IKLE2(IELEM))
     &                    - XA31(IELEM) * Y(IKLE3(IELEM))
     &                    - XA41(IELEM) * Y(IKLE4(IELEM))
            W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
     &                    - XA32(IELEM) * Y(IKLE3(IELEM))
     &                    - XA42(IELEM) * Y(IKLE4(IELEM))
            W3(IELEM) =   - XA13(IELEM) * Y(IKLE1(IELEM))
     &                    - XA23(IELEM) * Y(IKLE2(IELEM))
     &                    - XA43(IELEM) * Y(IKLE4(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
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
          CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) + XA21(IELEM) * Y(IKLE2(IELEM))
     &                            + XA31(IELEM) * Y(IKLE3(IELEM))
     &                            + XA41(IELEM) * Y(IKLE4(IELEM))
            W2(IELEM) = W2(IELEM) + XA12(IELEM) * Y(IKLE1(IELEM))
     &                            + XA32(IELEM) * Y(IKLE3(IELEM))
     &                            + XA42(IELEM) * Y(IKLE4(IELEM))
            W3(IELEM) = W3(IELEM) + XA13(IELEM) * Y(IKLE1(IELEM))
     &                            + XA23(IELEM) * Y(IKLE2(IELEM))
     &                            + XA43(IELEM) * Y(IKLE4(IELEM))
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
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) - XA21(IELEM) * Y(IKLE2(IELEM))
     &                            - XA31(IELEM) * Y(IKLE3(IELEM))
     &                            - XA41(IELEM) * Y(IKLE4(IELEM))
            W2(IELEM) = W2(IELEM) - XA12(IELEM) * Y(IKLE1(IELEM))
     &                            - XA32(IELEM) * Y(IKLE3(IELEM))
     &                            - XA42(IELEM) * Y(IKLE4(IELEM))
            W3(IELEM) = W3(IELEM) - XA13(IELEM) * Y(IKLE1(IELEM))
     &                            - XA23(IELEM) * Y(IKLE2(IELEM))
     &                            - XA43(IELEM) * Y(IKLE4(IELEM))
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
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM)
     &                + C * (    + XA21(IELEM) * Y(IKLE2(IELEM))
     &                           + XA31(IELEM) * Y(IKLE3(IELEM))
     &                           + XA41(IELEM) * Y(IKLE4(IELEM)) )
            W2(IELEM) = W2(IELEM)
     &                + C * (    + XA12(IELEM) * Y(IKLE1(IELEM))
     &                           + XA32(IELEM) * Y(IKLE3(IELEM))
     &                           + XA42(IELEM) * Y(IKLE4(IELEM)) )
            W3(IELEM) = W3(IELEM)
     &                + C * (    + XA13(IELEM) * Y(IKLE1(IELEM))
     &                           + XA23(IELEM) * Y(IKLE2(IELEM))
     &                           + XA43(IELEM) * Y(IKLE4(IELEM)) )
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
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,3001) OP
        CALL PLANTE(0)
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
1001  FORMAT(1X,'MV0403 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2001  FORMAT(1X,'MV0403 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3001  FORMAT(1X,'MV0403 (BIEF) : UNKNOWN OPERATION : ',A8)
!
!-----------------------------------------------------------------------
!
      END
