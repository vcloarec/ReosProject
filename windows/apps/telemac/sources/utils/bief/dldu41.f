!                   *****************
                    SUBROUTINE DLDU41
!                   *****************
!
     &(DB,XB,TYPDIA,XA,TYPEXA,
     & IKLE,NELEM,NELMAX,NPOIN,W,COPY,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    L D U FACTORISATION OF THE ELEMENTARY MATRICES
!+                IN MATRIX A
!+                FOR P1 PRISMS.
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
!warning  FOR THE NONSYMMETRICAL MATRICES: UE  LE
!+
!+  UE (THE BETAS) IS STORED IN XB (. , 1  TO 15)
!+
!+  LE (THE ALFAS) IS STORED IN XB (. , 16 TO 30)
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
      USE BIEF, EX_DLDU41 => DLDU41
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
      DOUBLE PRECISION, INTENT(OUT) :: W(NELMAX,6)
      LOGICAL, INTENT(IN)           :: COPY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
      DOUBLE PRECISION     A12,A13,A14,A15,A16
      DOUBLE PRECISION A21,A22,A23,A24,A25,A26
      DOUBLE PRECISION A31,A32,A33,A34,A35,A36
      DOUBLE PRECISION A41,A42,A43,A44,A45,A46
      DOUBLE PRECISION A51,A52,A53,A54,A55,A56
      DOUBLE PRECISION A61,A62,A63,A64,A65,A66
      DOUBLE PRECISION        BETA12,BETA13,BETA14,BETA15,BETA16
      DOUBLE PRECISION ALFA21,BETA22,BETA23,BETA24,BETA25,BETA26
      DOUBLE PRECISION ALFA31,ALFA32,BETA33,BETA34,BETA35,BETA36
      DOUBLE PRECISION ALFA41,ALFA42,ALFA43,BETA44,BETA45,BETA46
      DOUBLE PRECISION ALFA51,ALFA52,ALFA53,ALFA54,BETA55,BETA56
      DOUBLE PRECISION ALFA61,ALFA62,ALFA63,ALFA64,ALFA65,BETA66
!
!-----------------------------------------------------------------------
!
! REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY
!
      IF(TYPDIA(1:1).NE.'I'.AND.NCSIZE.LE.1) THEN
        WRITE(LU,1001) TYPDIA(1:1)
1001    FORMAT(1X,'DLDU41 (BIEF) : DIAGONAL OF A NOT EQUAL TO I :',A1)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(TYPEXA(1:1).EQ.'S') THEN
!
        IF(COPY) CALL OV('X=Y     ', X=XB, Y=XA, DIM1=NELMAX*15)
!
        DO IELEM=1,NELEM
!
! MATRIX TO FACTORISE (SYMMETRICAL WITH 1S ON THE DIAGONAL)
!
! LINE 1
!         A11 = 1.D0
          A12 = XA(IELEM,1 )
          A13 = XA(IELEM,2 )
          A14 = XA(IELEM,3 )
          A15 = XA(IELEM,4 )
          A16 = XA(IELEM,5 )
! LINE 2
          A22 = 1.D0
          A23 = XA(IELEM,6 )
          A24 = XA(IELEM,7 )
          A25 = XA(IELEM,8 )
          A26 = XA(IELEM,9 )
! LINE 3
          A33 = 1.D0
          A34 = XA(IELEM,10)
          A35 = XA(IELEM,11)
          A36 = XA(IELEM,12)
! LINE 4
          A44 = 1.D0
          A45 = XA(IELEM,13)
          A46 = XA(IELEM,14)
! LINE 5
          A55 = 1.D0
          A56 = XA(IELEM,15)
! LINE 6
          A66 = 1.D0
!
! CROUT L*U FACTORISATION
!
! COLUMN 1
          ALFA21 = A12
          ALFA31 = A13
          ALFA41 = A14
          ALFA51 = A15
          ALFA61 = A16
!
! COLUMN 2
          BETA12 =  A12
          BETA22 =  A22 - ALFA21*BETA12
          ALFA32 = (A23 - ALFA31*BETA12)/BETA22
          ALFA42 = (A24 - ALFA41*BETA12)/BETA22
          ALFA52 = (A25 - ALFA51*BETA12)/BETA22
          ALFA62 = (A26 - ALFA61*BETA12)/BETA22
!
! COLUMN 3
          BETA13 =  A13
          BETA23 =  A23 - ALFA21*BETA13
          BETA33 =  A33 - ALFA31*BETA13 - ALFA32*BETA23
          ALFA43 = (A34 - ALFA41*BETA13 - ALFA42*BETA23)/BETA33
          ALFA53 = (A35 - ALFA51*BETA13 - ALFA52*BETA23)/BETA33
          ALFA63 = (A36 - ALFA61*BETA13 - ALFA62*BETA23)/BETA33
!
! COLUMN 4
          BETA14 =  A14
          BETA24 =  A24 - ALFA21*BETA14
          BETA34 =  A34 - ALFA31*BETA14 - ALFA32*BETA24
          BETA44 =  A44 - ALFA41*BETA14 - ALFA42*BETA24 - ALFA43*BETA34
          ALFA54 = (A45 - ALFA51*BETA14 - ALFA52*BETA24 - ALFA53*BETA34
     &    )/BETA44
          ALFA64 = (A46 - ALFA61*BETA14 - ALFA62*BETA24 - ALFA63*BETA34
     &    )/BETA44
!
! COLUMN 5
          BETA15 =  A15
          BETA25 =  A25 - ALFA21*BETA15
          BETA35 =  A35 - ALFA31*BETA15 - ALFA32*BETA25
          BETA45 =  A45 - ALFA41*BETA15 - ALFA42*BETA25 - ALFA43*BETA35
          BETA55 =  A55 - ALFA51*BETA15 - ALFA52*BETA25 - ALFA53*BETA35
     &                  - ALFA54*BETA45
          ALFA65 = (A56 - ALFA61*BETA15 - ALFA62*BETA25 - ALFA63*BETA35
     &                  - ALFA64*BETA45
     &    )/BETA55
!
! COLUMN 6
          BETA16 =  A16
          BETA26 =  A26 - ALFA21*BETA16
          BETA36 =  A36 - ALFA31*BETA16 - ALFA32*BETA26
          BETA46 =  A46 - ALFA41*BETA16 - ALFA42*BETA26 - ALFA43*BETA36
          BETA56 =  A56 - ALFA51*BETA16 - ALFA52*BETA26 - ALFA53*BETA36
     &                  - ALFA54*BETA46
          BETA66 =  A66 - ALFA61*BETA16 - ALFA62*BETA26 - ALFA63*BETA36
     &                  - ALFA64*BETA46 - ALFA65*BETA56
!
! STORES IN XB AND W2,...,W6
!
          XB(IELEM,1 ) = ALFA21
          XB(IELEM,2 ) = ALFA31
          XB(IELEM,3 ) = ALFA41
          XB(IELEM,4 ) = ALFA51
          XB(IELEM,5 ) = ALFA61
!
          XB(IELEM,6 ) = ALFA32
          XB(IELEM,7 ) = ALFA42
          XB(IELEM,8 ) = ALFA52
          XB(IELEM,9 ) = ALFA62
!
          XB(IELEM,10) = ALFA43
          XB(IELEM,11) = ALFA53
          XB(IELEM,12) = ALFA63
!
          XB(IELEM,13) = ALFA54
          XB(IELEM,14) = ALFA64
!
          XB(IELEM,15) = ALFA65
!
          W(IELEM,2)    = BETA22
          W(IELEM,3)    = BETA33
          W(IELEM,4)    = BETA44
          W(IELEM,5)    = BETA55
          W(IELEM,6)    = BETA66
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(TYPEXA(1:1).EQ.'Q') THEN
!
        IF(COPY) CALL OV('X=Y     ', X=XB, Y=XA, DIM1=NELMAX*30)
!
        DO IELEM=1,NELEM
!
! MATRIX TO FACTORISE (WITH 1S ON THE DIAGONAL)
!
! LINE 1
!         A11 = 1.D0
          A12 = XA(IELEM,1 )
          A13 = XA(IELEM,2 )
          A14 = XA(IELEM,3 )
          A15 = XA(IELEM,4 )
          A16 = XA(IELEM,5 )
! LINE 2
          A21 = XA(IELEM,16)
          A22 = 1.D0
          A23 = XA(IELEM,6 )
          A24 = XA(IELEM,7 )
          A25 = XA(IELEM,8 )
          A26 = XA(IELEM,9 )
! LINE 3
          A31 = XA(IELEM,17)
          A32 = XA(IELEM,21)
          A33 = 1.D0
          A34 = XA(IELEM,10)
          A35 = XA(IELEM,11)
          A36 = XA(IELEM,12)
! LINE 4
          A41 = XA(IELEM,18)
          A42 = XA(IELEM,22)
          A43 = XA(IELEM,25)
          A44 = 1.D0
          A45 = XA(IELEM,13)
          A46 = XA(IELEM,14)
! LINE 5
          A51 = XA(IELEM,19)
          A52 = XA(IELEM,23)
          A53 = XA(IELEM,26)
          A54 = XA(IELEM,28)
          A55 = 1.D0
          A56 = XA(IELEM,15)
! LINE 6
          A61 = XA(IELEM,20)
          A62 = XA(IELEM,24)
          A63 = XA(IELEM,27)
          A64 = XA(IELEM,29)
          A65 = XA(IELEM,30)
          A66 = 1.D0
!
! CROUT L*U FACTORISATION
!
! COLUMN 1
          ALFA21 = A21
          ALFA31 = A31
          ALFA41 = A41
          ALFA51 = A51
          ALFA61 = A61
!
! COLUMN 2
          BETA12 =  A12
          BETA22 =  A22 - ALFA21*BETA12
          ALFA32 = (A32 - ALFA31*BETA12)/BETA22
          ALFA42 = (A42 - ALFA41*BETA12)/BETA22
          ALFA52 = (A52 - ALFA51*BETA12)/BETA22
          ALFA62 = (A62 - ALFA61*BETA12)/BETA22
!
! COLUMN 3
          BETA13 =  A13
          BETA23 =  A23 - ALFA21*BETA13
          BETA33 =  A33 - ALFA31*BETA13 - ALFA32*BETA23
          ALFA43 = (A43 - ALFA41*BETA13 - ALFA42*BETA23)/BETA33
          ALFA53 = (A53 - ALFA51*BETA13 - ALFA52*BETA23)/BETA33
          ALFA63 = (A63 - ALFA61*BETA13 - ALFA62*BETA23)/BETA33
!
! COLUMN 4
          BETA14 =  A14
          BETA24 =  A24 - ALFA21*BETA14
          BETA34 =  A34 - ALFA31*BETA14 - ALFA32*BETA24
          BETA44 =  A44 - ALFA41*BETA14 - ALFA42*BETA24 - ALFA43*BETA34
          ALFA54 = (A54 - ALFA51*BETA14 - ALFA52*BETA24 - ALFA53*BETA34
     &    )/BETA44
          ALFA64 = (A64 - ALFA61*BETA14 - ALFA62*BETA24 - ALFA63*BETA34
     &    )/BETA44
!
! COLUMN 5
          BETA15 =  A15
          BETA25 =  A25 - ALFA21*BETA15
          BETA35 =  A35 - ALFA31*BETA15 - ALFA32*BETA25
          BETA45 =  A45 - ALFA41*BETA15 - ALFA42*BETA25 - ALFA43*BETA35
          BETA55 =  A55 - ALFA51*BETA15 - ALFA52*BETA25 - ALFA53*BETA35
     &                  - ALFA54*BETA45
          ALFA65 = (A65 - ALFA61*BETA15 - ALFA62*BETA25 - ALFA63*BETA35
     &                  - ALFA64*BETA45
     &    )/BETA55
!
! COLUMN 6
          BETA16 =  A16
          BETA26 =  A26 - ALFA21*BETA16
          BETA36 =  A36 - ALFA31*BETA16 - ALFA32*BETA26
          BETA46 =  A46 - ALFA41*BETA16 - ALFA42*BETA26 - ALFA43*BETA36
          BETA56 =  A56 - ALFA51*BETA16 - ALFA52*BETA26 - ALFA53*BETA36
     &                  - ALFA54*BETA46
          BETA66 =  A66 - ALFA61*BETA16 - ALFA62*BETA26 - ALFA63*BETA36
     &                  - ALFA64*BETA46 - ALFA65*BETA56
!
! STORES IN XB AND W2,...,W6
! L D U FACTORISATION AT THE SAME TIME
!
          XB(IELEM,1 ) = BETA12
          XB(IELEM,2 ) = BETA13
          XB(IELEM,3 ) = BETA14
          XB(IELEM,4 ) = BETA15
          XB(IELEM,5 ) = BETA16
!
          XB(IELEM,6 ) = BETA23/BETA22
          XB(IELEM,7 ) = BETA24/BETA22
          XB(IELEM,8 ) = BETA25/BETA22
          XB(IELEM,9 ) = BETA26/BETA22
!
          XB(IELEM,10) = BETA34/BETA33
          XB(IELEM,11) = BETA35/BETA33
          XB(IELEM,12) = BETA36/BETA33
!
          XB(IELEM,13) = BETA45/BETA44
          XB(IELEM,14) = BETA46/BETA44
!
          XB(IELEM,15) = BETA56/BETA55
!
          XB(IELEM,16) = ALFA21
          XB(IELEM,17) = ALFA31
          XB(IELEM,18) = ALFA41
          XB(IELEM,19) = ALFA51
          XB(IELEM,20) = ALFA61
!
          XB(IELEM,21) = ALFA32
          XB(IELEM,22) = ALFA42
          XB(IELEM,23) = ALFA52
          XB(IELEM,24) = ALFA62
!
          XB(IELEM,25) = ALFA43
          XB(IELEM,26) = ALFA53
          XB(IELEM,27) = ALFA63
!
          XB(IELEM,28) = ALFA54
          XB(IELEM,29) = ALFA64
!
          XB(IELEM,30) = ALFA65
!
          W(IELEM,2)    = BETA22
          W(IELEM,3)    = BETA33
          W(IELEM,4)    = BETA44
          W(IELEM,5)    = BETA55
          W(IELEM,6)    = BETA66
!
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,2001) TYPEXA(1:1)
2001    FORMAT(1X,'DLDU41 (BIEF) : TYPE OF MATRIX NOT AVAILABLE :',A1)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  MULTIPLICATIVE ASSEMBLY OF THE DIAGONAL WITH INITIALISATION OF DB TO 1
!  SKIPS IKLE1 BECAUSE W1 = 1
!
      CALL ASMVEC(DB,IKLE(1,2),NPOIN,NELEM,NELMAX,5,W(1,2),.TRUE.,LV)
!
!  INVERTS DB
!
      CALL OV('X=1/Y   ', X=DB, Y=DB, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
