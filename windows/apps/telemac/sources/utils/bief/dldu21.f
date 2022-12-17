!                   *****************
                    SUBROUTINE DLDU21
!                   *****************
!
     &(DB,XB,TYPDIA,XA,TYPEXA,IKLE,NELEM,NELMAX,NPOIN,W,COPY,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    L D U FACTORISATION OF THE ELEMENTARY MATRICES
!+                IN MATRIX A
!+                FOR Q1 QUADRILATERALS.
!+
!+            REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY.
!code
!+            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:
!+
!+            LE * DE * UE
!+
!+            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!+
!+                                                   T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!+
!+            "DE" MATRICES ARE CONSIDERED LIKE DIAGONALS OF SIZE
!+            NPOIN X NPOIN, WHICH ARE FILLED WITH 1S FOR THE POINTS
!+            WHICH DO NOT BELONG TO THE CONSIDERED ELEMENT
!+
!+            THEN PERFORMS THE PRODUCT OF ALL THESE DIAGONALS
!+            YIELDING DIAGONAL DB
!
!warning  FOR NONSYMMETRICAL MATRICES: UE  LE
!+
!+  UE (THE BETAS) IS STORED IN XB (. , 1 TO  6)
!+
!+  LE (THE ALFAS) IS STORED IN XB (. , 7 TO 12)
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        24/04/97
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
!| COPY           |-->| IF .TRUE. A IS COPIED INTO B.
!|                |   | IF .FALSE. B IS CONSIDERED ALREADY INITIALISED
!| DB             |<--| DIAGONAL OF MATRIX B
!| IKLE           |-->| CONNECTIVITY TABLE
!| LV             |-->| VECTOR LENGTH OF THE COMPUTER
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| TYPDIA         |<--| TYPE OF DIAGONAL ( 'Q', 'I' , OR '0' )
!| TYPEXA         |<--| TYPE OF OFF-DIAGONAL TERMS ('Q','S',OR '0')
!| W              |-->| WORK ARRAY OF DIMENSION (NELMAX,3)
!| XA             |<--| OFF-DIAGONAL TERMS OF MATRIX A
!| XB             |<--| OFF-DIAGONAL TERMS OF MATRIX B
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DLDU21 => DLDU21
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NELEM,NELMAX,LV,NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: DB(NPOIN),XB(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)  :: XA(NELMAX,*)
      CHARACTER(LEN=1), INTENT(IN)  :: TYPDIA,TYPEXA
      INTEGER, INTENT(IN)           :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(OUT) :: W(NELMAX,4)
      LOGICAL, INTENT(IN)           :: COPY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
      DOUBLE PRECISION     A12,A13,A14
      DOUBLE PRECISION A21,A22,A23,A24
      DOUBLE PRECISION A31,A32,A33,A34
      DOUBLE PRECISION A41,A42,A43,A44
      DOUBLE PRECISION        BETA12,BETA13,BETA14
      DOUBLE PRECISION ALFA21,BETA22,BETA23,BETA24
      DOUBLE PRECISION ALFA31,ALFA32,BETA33,BETA34
      DOUBLE PRECISION ALFA41,ALFA42,ALFA43,BETA44
!
!-----------------------------------------------------------------------
!
! REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY
!
      IF(TYPDIA(1:1).NE.'I'.AND.NCSIZE.LE.1) THEN
        WRITE(LU,1001) TYPDIA(1:1)
1001    FORMAT(1X,'DLDU21 (BIEF) : DIAGONAL OF A NOT EQUAL TO I :',A1)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(TYPEXA(1:1).EQ.'S') THEN
!
        IF(COPY) CALL OV('X=Y     ', X=XB, Y=XA, DIM1=NELMAX*6)
!
        DO IELEM=1,NELEM
!
! MATRIX TO FACTORISE (SYMMETRICAL WITH 1S ON THE DIAGONAL)
!
! LINE 1
!         A11 = 1.D0
          A12 = XA(IELEM,1)
          A13 = XA(IELEM,2)
          A14 = XA(IELEM,3)
! LINE 2
          A22 = 1.D0
          A23 = XA(IELEM,4)
          A24 = XA(IELEM,5)
! LINE 3
          A33 = 1.D0
          A34 = XA(IELEM,6)
! LINE 4
          A44 = 1.D0
!
! CROUT L*U FACTORISATION
!
          ALFA21 = A12
          ALFA31 = A13
          ALFA41 = A14
!
          BETA12 =  A12
          BETA22 =  A22 - ALFA21*BETA12
          ALFA32 = (A23 - ALFA31*BETA12)/BETA22
          ALFA42 = (A24 - ALFA41*BETA12)/BETA22
!
          BETA13 =  A13
          BETA23 =  A23 - ALFA21*BETA13
          BETA33 =  A33 - ALFA31*BETA13 - ALFA32*BETA23
          ALFA43 = (A34 - ALFA41*BETA13 - ALFA42*BETA23)/BETA33
!
          BETA14 =  A14
          BETA24 =  A24 - ALFA21*BETA14
          BETA34 =  A34 - ALFA31*BETA14 - ALFA32*BETA24
          BETA44 =  A44 - ALFA41*BETA14 - ALFA42*BETA24 - ALFA43*BETA34
!
! STORES IN XB AND W2,...,W4
!
          XB(IELEM,1 ) = ALFA21
          XB(IELEM,2 ) = ALFA31
          XB(IELEM,3 ) = ALFA41
          XB(IELEM,4 ) = ALFA32
          XB(IELEM,5 ) = ALFA42
          XB(IELEM,6 ) = ALFA43
!
          W(IELEM,2)    = BETA22
          W(IELEM,3)    = BETA33
          W(IELEM,4)    = BETA44
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(TYPEXA(1:1).EQ.'Q') THEN
!
        IF(COPY) CALL OV('X=Y     ', X=XB, Y=XA, DIM1=NELMAX*12)
!
        DO IELEM=1,NELEM
!
! MATRIX TO FACTORISE (WITH 1S ON THE DIAGONAL)
!
!         A11 = 1.D0
          A22 = 1.D0
          A33 = 1.D0
          A44 = 1.D0
!
          A12 = XA(IELEM,1 )
          A13 = XA(IELEM,2 )
          A14 = XA(IELEM,3 )
          A23 = XA(IELEM,4 )
          A24 = XA(IELEM,5 )
          A34 = XA(IELEM,6 )
!
          A21 = XA(IELEM,7 )
          A31 = XA(IELEM,8 )
          A41 = XA(IELEM,9 )
          A32 = XA(IELEM,10)
          A42 = XA(IELEM,11)
          A43 = XA(IELEM,12)
!
! CROUT L*U FACTORISATION
!
          ALFA21 = A21
          ALFA31 = A31
          ALFA41 = A41
!
          BETA12 =  A12
          BETA22 =  A22 - ALFA21*BETA12
          ALFA32 = (A32 - ALFA31*BETA12)/BETA22
          ALFA42 = (A42 - ALFA41*BETA12)/BETA22
!
          BETA13 =  A13
          BETA23 =  A23 - ALFA21*BETA13
          BETA33 =  A33 - ALFA31*BETA13 - ALFA32*BETA23
          ALFA43 = (A43 - ALFA41*BETA13 - ALFA42*BETA23)/BETA33
!
          BETA14 =  A14
          BETA24 =  A24 - ALFA21*BETA14
          BETA34 =  A34 - ALFA31*BETA14 - ALFA32*BETA24
          BETA44 =  A44 - ALFA41*BETA14 - ALFA42*BETA24 - ALFA43*BETA34
!
! STORES IN XB AND W2,...,W4
! L D U FACTORISATION AT THE SAME TIME
!
          XB(IELEM,1 ) = BETA12
          XB(IELEM,2 ) = BETA13
          XB(IELEM,3 ) = BETA14
          XB(IELEM,4 ) = BETA23/BETA22
          XB(IELEM,5 ) = BETA24/BETA22
          XB(IELEM,6 ) = BETA34/BETA33
!
          XB(IELEM,07) = ALFA21
          XB(IELEM,08) = ALFA31
          XB(IELEM,09) = ALFA41
          XB(IELEM,10) = ALFA32
          XB(IELEM,11) = ALFA42
          XB(IELEM,12) = ALFA43
!
          W(IELEM,2)    = BETA22
          W(IELEM,3)    = BETA33
          W(IELEM,4)    = BETA44
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,2001) TYPEXA(1:1)
2001    FORMAT(1X,'DLDU21 (BIEF) : TYPE OF MATRIX NOT AVAILABLE :',A1)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  MULTIPLICATIVE ASSEMBLY OF THE DIAGONAL WITH INITIALISATION OF DB TO 1
!  SKIPS IKLE1 BECAUSE W1 = 1
!
      CALL ASMVEC(DB,IKLE(1,2),NPOIN,NELEM,NELMAX,3,W(1,2),.TRUE.,LV)
!
!  INVERTS DB
!
      CALL OV('X=1/Y   ', X=DB, Y=DB, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
