!                   *****************
                    SUBROUTINE MV0303
!                   *****************
!
     &(OP, X , DA,TYPDIA,XA12,XA13,XA21,XA23,XA31,XA32,
     & TYPEXT, Y,C,IKLE1,IKLE2,IKLE3,NPOIN,NELEM,W1,W2,W3
     & ,X_ERR,Y_ERR,DA_ERR)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
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
!+      OP = 'X=CAY   '  : X = CAY
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
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+        ADD MODASS=3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
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
!| X              |<->| RESULT IN ASSEMBLED FORM
!| XA13           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA21           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA23           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA31           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA32           |-->| OFF-DIAGONAL TERM OF MATRIX
!| Y              |-->| VECTOR USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MV0303=> MV0303
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: XA12(*),XA13(*),XA23(*)
      DOUBLE PRECISION, INTENT(IN) :: XA21(*),XA31(*),XA32(*)
      DOUBLE PRECISION, INTENT(IN) ::C
!
      CHARACTER(LEN=8), INTENT(IN) :: OP
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
      DOUBLE PRECISION, OPTIONAL, INTENT(INOUT) :: X_ERR(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: Y_ERR(*),DA_ERR(*)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
     &                    + XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =     XA23(IELEM) * Y(IKLE3(IELEM))
     &                    + XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) =     XA31(IELEM) * Y(IKLE1(IELEM))
     &                    + XA32(IELEM) * Y(IKLE2(IELEM))
          END DO
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV('X=C     ', X=W1, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W2, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W3, C=0.D0, DIM1=NELEM)
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=YZ    ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=YZ    ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=Y     ', X=X, Y=Y, DIM1=NPOIN)
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV('X=C     ', X=X, C=0.D0, DIM1=NPOIN)
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =  C * (   XA12(IELEM) * Y(IKLE2(IELEM))
     &                         + XA13(IELEM) * Y(IKLE3(IELEM))  )
            W2(IELEM) =  C * (   XA23(IELEM) * Y(IKLE3(IELEM))
     &                         + XA21(IELEM) * Y(IKLE1(IELEM))  )
            W3(IELEM) =  C * (   XA31(IELEM) * Y(IKLE1(IELEM))
     &                         + XA32(IELEM) * Y(IKLE2(IELEM))  )
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV('X=C     ', X=W1, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W2, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W3, C=0.D0, DIM1=NELEM)
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=CYZ   ', X=X, Y=Y, Z=DA, C=C, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=CYZ   ', X , Y , DA , C  , NPOIN,
     &         X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=CY    ', X=X, Y=Y, C=C, DIM1=NPOIN)
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV('X=C     ', X=X, C=0.D0, DIM1=NPOIN)
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
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
     &                    - XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =   - XA23(IELEM) * Y(IKLE3(IELEM))
     &                    - XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) =   - XA31(IELEM) * Y(IKLE1(IELEM))
     &                    - XA32(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV('X=C     ', X=W1, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W2, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W3, C=0.D0, DIM1=NELEM)
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=-YZ   ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=-YZ   ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=-Y    ', X=X, Y=Y, DIM1=NPOIN)
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV('X=C     ', X=X, C=0.D0, DIM1=NPOIN)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM)   + XA12(IELEM) * Y(IKLE2(IELEM))
     &                              + XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM)   + XA23(IELEM) * Y(IKLE3(IELEM))
     &                              + XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) = W3(IELEM)   + XA31(IELEM) * Y(IKLE1(IELEM))
     &                              + XA32(IELEM) * Y(IKLE2(IELEM))
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=X+YZ  ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=X+Y   ', X=X, Y=Y, DIM1=NPOIN)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM)   - XA12(IELEM) * Y(IKLE2(IELEM))
     &                              - XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM)   - XA23(IELEM) * Y(IKLE3(IELEM))
     &                              - XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) = W3(IELEM)   - XA31(IELEM) * Y(IKLE1(IELEM))
     &                              - XA32(IELEM) * Y(IKLE2(IELEM))
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=X-YZ  ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X-YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=X-Y   ', X=X, Y=Y, DIM1=NPOIN)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM)=W1(IELEM) + C * (XA12(IELEM) * Y(IKLE2(IELEM))
     &                                +XA13(IELEM) * Y(IKLE3(IELEM)))
            W2(IELEM)=W2(IELEM) + C * (XA23(IELEM) * Y(IKLE3(IELEM))
     &                                +XA21(IELEM) * Y(IKLE1(IELEM)))
            W3(IELEM)=W3(IELEM) + C * (XA31(IELEM) * Y(IKLE1(IELEM))
     &                                +XA32(IELEM) * Y(IKLE2(IELEM)))
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=X+CYZ  ', X=X, Y=Y, Z=DA, C=C, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+CYZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=X+CY   ', X=X, Y=Y, C=C, DIM1=NPOIN)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   + XA21(IELEM) * Y(IKLE2(IELEM))
     &                    + XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =   + XA12(IELEM) * Y(IKLE1(IELEM))
     &                    + XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) =   + XA13(IELEM) * Y(IKLE1(IELEM))
     &                    + XA23(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV('X=C     ', X=W1, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W2, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W3, C=0.D0, DIM1=NELEM)
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=YZ    ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=YZ    ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=Y     ', X=X, Y=Y, DIM1=NPOIN)
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV('X=C     ', X=X, C=0.D0, DIM1=NPOIN)
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-TAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   - XA21(IELEM) * Y(IKLE2(IELEM))
     &                    - XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
     &                    - XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) =   - XA13(IELEM) * Y(IKLE1(IELEM))
     &                    - XA23(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV('X=C     ', X=W1, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W2, C=0.D0, DIM1=NELEM)
          CALL OV('X=C     ', X=W3, C=0.D0, DIM1=NELEM)
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=-YZ   ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=-YZ   ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=-Y    ', X=X, Y=Y, DIM1=NPOIN)
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV('X=C     ', X=X, C=0.D0, DIM1=NPOIN)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) + XA21(IELEM) * Y(IKLE2(IELEM))
     &                            + XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM) + XA12(IELEM) * Y(IKLE1(IELEM))
     &                            + XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) = W3(IELEM) + XA13(IELEM) * Y(IKLE1(IELEM))
     &                            + XA23(IELEM) * Y(IKLE2(IELEM))
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=X+YZ  ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', X=X, Y=Y, DIM1=NPOIN)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) - XA21(IELEM) * Y(IKLE2(IELEM))
     &                            - XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM) - XA12(IELEM) * Y(IKLE1(IELEM))
     &                            - XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) = W3(IELEM) - XA13(IELEM) * Y(IKLE1(IELEM))
     &                            - XA23(IELEM) * Y(IKLE2(IELEM))
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=X-YZ  ', X=X, Y=Y, Z=DA, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X-YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=X-Y   ', X=X, Y=Y, DIM1=NPOIN)
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) + C*(XA21(IELEM) * Y(IKLE2(IELEM))
     &                                +XA31(IELEM) * Y(IKLE3(IELEM)))
            W2(IELEM) = W2(IELEM) + C*(XA12(IELEM) * Y(IKLE1(IELEM))
     &                                +XA32(IELEM) * Y(IKLE3(IELEM)))
            W3(IELEM) = W3(IELEM) + C*(XA13(IELEM) * Y(IKLE1(IELEM))
     &                                +XA23(IELEM) * Y(IKLE2(IELEM)))
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
          IF ( MODASS .EQ.1) THEN
            CALL OV('X=X+CYZ ', X=X, Y=Y, Z=DA, C=C, DIM1=NPOIN)
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+CYZ ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV('X=X+CY  ', X=X, Y=Y, Z=Z, C=C, DIM1=NPOIN)
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
1001  FORMAT(1X,'MV0303 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2001  FORMAT(1X,'MV0303 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3001  FORMAT(1X,'MV0303 (BIEF) : UNKNOWN OPERATION : ',A8)
!
      END
