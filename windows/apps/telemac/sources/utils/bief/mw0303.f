!                   *****************
                    SUBROUTINE MW0303
!                   *****************
!
     &(OP, X , DA,TYPDIA,XAS,TYPEXT, Y,C,
     & IKLEM1,DIMIKM,LIMVOI,MXPTVS,NPMAX,NPOIN,TRAV)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    FRONTAL MATRIX VECTOR PRODUCT FOR P1 TRIANGLES.
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
!+      OP = 'X=-AY   '  : X = -AY
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        11/03/2013
!+        V6P3
!+   Dimension of LIMVOI now set to 11.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| DIMIKM         |-->| FIRST DIMENSION OF IKLEM1
!| IKLEM1         |-->| DATA STRUCTURE FOR MATRIX-VECTOR PRODUCT
!|                |   | GIVES THE ADRESSES OF OFF-DIAGONAL TERMS
!|                |   | IN XAS AND NEIGHBOUR POINTS IN Y.
!|                |   | IKLEM1(*,*,1) : NON SYMMETRIC MATRIX
!|                |   | IKLEM1(*,*,2) : SYMMETRIC MATRIX
!|                |   | FIRST DIMENSION: NPMAX
!|                |   | 2ND DIM.: 1 : DIRECT PRODUCT, ADDRESS IN XAS
!|                |   |           2 : DIRECT PRODUCT, ADDRESS IN Y
!|                |   |           3 : TRANSPOSED PRODUCT, ADDRESS IN XAS
!|                |   |           4 : TRANSPOSED PRODUCT, ADDRESS IN Y
!| LIMVOI         |-->|
!| MXPTVS         |-->| MAXIMUM NUMBER OF NEIGHBOURS OF A POINT
!| NPMAX          |-->| MAXIMUM NUMBER OF POINTS.
!| NPOIN          |-->| NUMBER OF POINTS
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| TRAV           |-->| WORK ARRAY
!| TYPDIA         |-->| TYPE OF DIAGONAL:
!|                |   | TYPDIA = 'Q' : ANY VALUE
!|                |   | TYPDIA = 'I' : IDENTITY
!|                |   | TYPDIA = '0' : ZERO
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!|                |   | TYPEXT = 'Q' : ANY VALUE
!|                |   | TYPEXT = 'S' : SYMMETRIC
!|                |   | TYPEXT = '0' : ZERO
!| X              |<->| RESULT IN ASSEMBLED FORM
!| XAS            |-->| OFF-DIAGONAL TERMS OF MATRIX
!| Y              |-->| VECTOR USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MW0303 => MW0303
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: DIMIKM,MXPTVS,NPMAX,NPOIN
!                                                      11: SEE ALMESH
!                                                          AND OPTASS
      INTEGER, INTENT(IN) :: IKLEM1(DIMIKM,4,2),LIMVOI(11,2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*),TRAV(*)
      DOUBLE PRECISION, INTENT(IN)    :: DA(*),Y(*)
      DOUBLE PRECISION, INTENT(IN)    :: XAS(*),C
!
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      CHARACTER(LEN=1), INTENT(IN)    :: TYPDIA,TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
!   TREATMENT SPECIFIC TO THE TRANSPOSITION:
!
      I = 1
      IF(OP(3:3).EQ.'T'.OR.OP(4:4).EQ.'T'.OR.
     &   OP(5:5).EQ.'T'.OR.OP(6:6).EQ.'T') I = 3
!
!-----------------------------------------------------------------------
!
!   MATRIX VECTOR PRODUCT, SIMPLE FUNCTION OF THE SHAPE OF THE MATRIX:
!
      IF(TYPEXT(1:1).EQ.'S'.OR.TYPEXT(1:1).EQ.'Q') THEN
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
        CALL OPASS('X=WY    ',TRAV,XAS,IKLEM1(1,I,1),
     &             Y,IKLEM1(1,I+1,1),LIMVOI,MXPTVS,NPMAX)
        ELSEIF(TYPEXT(1:1).EQ.'S') THEN
        CALL OPASS('X=WY    ',TRAV,XAS,IKLEM1(1,I,2),
     &             Y,IKLEM1(1,I+1,2),LIMVOI,MXPTVS,NPMAX)
        ENDIF
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+YZ  ', TRAV , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', TRAV , Y , Z , C , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=YZ    ', TRAV , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=Y     ', TRAV , Y , Z , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', TRAV , Y , Z , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
!
        WRITE(LU,1001) TYPEXT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   IMPLEMENTED OPERATIONS:
!
      IF(OP(1:8).EQ.'X=AY    '.OR.OP(1:8).EQ.'X=TAY   ') THEN
        CALL OV ('X=Y     ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=-AY   '.OR.OP(1:8).EQ.'X=-TAY  ') THEN
        CALL OV ('X=-Y    ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X+AY  '.OR.OP(1:8).EQ.'X=X+TAY ') THEN
        CALL OV ('X=X+Y   ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X-AY  '.OR.OP(1:8).EQ.'X=X-TAY ') THEN
        CALL OV ('X=X-Y   ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X+CAY '.OR.OP(1:8).EQ.'X=X+CTAY') THEN
        CALL OV ('X=X+CY  ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
        CALL OV ('X=CY    ', X , TRAV , Z , C , NPOIN )
      ELSE
        WRITE(LU,3001) OP
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
1001  FORMAT(1X,'MW0303 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2001  FORMAT(1X,'MW0303 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3001  FORMAT(1X,'MW0303 (BIEF) : UNKNOWN OPERATION : ',A8)
!
!-----------------------------------------------------------------------
!
      END
