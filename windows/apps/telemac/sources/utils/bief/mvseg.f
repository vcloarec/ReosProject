!                   ****************
                    SUBROUTINE MVSEG
!                   ****************
!
     &(OP, X , DA,TYPDIA,XA1,XA2,
     & TYPEXT, Y,C,NPOIN,NELEM,NSEG1,NSEG2,GLOSEG1,GLOSEG2,IELM1,IELM2)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MATRIX VECTOR PRODUCT FOR EDGE-BASED STORAGE.
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
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| GLOSEG1        |-->| FIRST POINT OF SEGMENTS
!| GLOSEG2        |-->| SECOND POINT OF SEGMENTS
!| IELM1          |-->| TYPE OF LINE ELEMENT.
!| IELM2          |-->| TYPE OF COLUMN ELEMENT.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF LINEAR POINTS
!| NSEG1          |-->| NUMBER OF SEGMENTS OF THE LINE ELEMENT
!| NSEG2          |-->| NUMBER OF SEGMENTS OF THE COLUMN ELEMENT
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| TYPDIA         |-->| TYPE OF DIAGONAL:
!|                |   | TYPDIA = 'Q' : ANY VALUE
!|                |   | TYPDIA = 'I' : IDENTITY
!|                |   | TYPDIA = '0' : ZERO
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!|                |   | TYPEXT = 'Q' : ANY VALUE
!|                |   | TYPEXT = 'S' : SYMMETRIC
!|                |   | TYPEXT = '0' : ZERO
!| X              |<->| RESULT IN ASSEMBLED FORM
!| XA1            |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA2            |-->| OFF-DIAGONAL TERM OF MATRIX
!| Y              |-->| VECTOR USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF,EX_MVSEG => MVSEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN) :: NSEG1,NSEG2,NELEM,IELM1,IELM2
!
      DOUBLE PRECISION, INTENT(IN)    :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: XA1(*),XA2(*)
      DOUBLE PRECISION, INTENT(IN)    :: C
!
      CHARACTER(LEN=8),INTENT(IN) :: OP
      CHARACTER(LEN=1),INTENT(IN) :: TYPDIA,TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG,I,MINSEG,MAXSEG
      DOUBLE PRECISION Z(1)
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
      MINSEG = MIN(NSEG1,NSEG2)
      MAXSEG = MAX(NSEG1,NSEG2)
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
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
!         SQUARE PART
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))+XA1(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
!
!         THE REST OF THE RECTANGULAR MATRIX
!
          IF(NSEG1.GT.NSEG2) THEN
!           PART OF X HAS NOT BEEN INITIALISED
!           BY THE CONTRIBUTION OF THE DIAGONAL
            IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
!             OPTIMISATION FOR QUASI-BUBBLE ELEMENT
              DO I = NPOIN+1,NPOIN+NELEM
                X(I)=0.D0
              ENDDO
            ELSE
              DO ISEG = MINSEG+1,MAXSEG
                X(GLOSEG2(ISEG))=0.D0
              ENDDO
            ENDIF
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=CYZ   ', X , Y , DA , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=CY    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))+C*XA1(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
!             OPTIMISATION FOR QUASI-BUBBLE ELEMENT
              DO I = NPOIN+1,NPOIN+NELEM
                X(I)=0.D0
              ENDDO
            ELSE
              DO ISEG = MINSEG+1,MAXSEG
                X(GLOSEG2(ISEG))=0.D0
              ENDDO
            ENDIF
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
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
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          WRITE(LU,2001) TYPDIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))-XA1(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
!             OPTIMISATION FOR QUASI-BUBBLE ELEMENT
              DO I = NPOIN+1,NPOIN+NELEM
                X(I)=0.D0
              ENDDO
            ELSE
              DO ISEG = MINSEG+1,MAXSEG
                X(GLOSEG2(ISEG))=0.D0
              ENDDO
            ENDIF
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))+XA1(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))-XA1(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))+C*XA1(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))+XA1(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
!             OPTIMISATION FOR QUASI-BUBBLE ELEMENT
              DO I = NPOIN+1,NPOIN+NELEM
                X(I)=0.D0
              ENDDO
            ELSE
              DO ISEG = MINSEG+1,MAXSEG
                X(GLOSEG2(ISEG))=0.D0
              ENDDO
            ENDIF
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-TAY   ') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))-XA1(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
!             OPTIMISATION FOR QUASI-BUBBLE ELEMENT
              DO I = NPOIN+1,NPOIN+NELEM
                X(I)=0.D0
              ENDDO
            ELSE
              DO ISEG = MINSEG+1,MAXSEG
                X(GLOSEG2(ISEG))=0.D0
              ENDDO
            ENDIF
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))+XA1(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))-XA1(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
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
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO ISEG = 1 , MINSEG
            X(GLOSEG1(ISEG))=
     &      X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
            X(GLOSEG2(ISEG))=
     &      X(GLOSEG2(ISEG))+C*XA1(ISEG)*Y(GLOSEG1(ISEG))
          ENDDO
          IF(NSEG1.GT.NSEG2) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG1(ISEG))=
     &        X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
            ENDDO
          ELSEIF(NSEG2.GT.NSEG1) THEN
            DO ISEG = MINSEG+1,MAXSEG
              X(GLOSEG2(ISEG))=
     &        X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
            ENDDO
          ENDIF
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          WRITE(LU,1001) TYPEXT
          CALL PLANTE(1)
          STOP
!
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
1001  FORMAT(1X,'MVSEG (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2001  FORMAT(1X,'MVSEG (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3001  FORMAT(1X,'MVSEG (BIEF) : UNKNOWN OPERATION : ',A8)
!
      END
