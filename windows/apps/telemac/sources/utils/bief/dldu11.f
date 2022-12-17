!                   *****************
                    SUBROUTINE DLDU11
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
!+                FOR P1 TRIANGLES.
!+
!+            REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY.
!code
!+            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:
!+
!+            LE X DE X UE
!+
!+            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!+
!+                                                T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!+
!+            "DE" MATRICES ARE CONSIDERED LIKE DIAGONALS OF SIZE
!+            NPOIN X NPOIN, WHICH ARE FILLED WITH 1S FOR THE POINTS
!+            WHICH DO NOT BELONG TO THE CONSIDERED ELEMENT
!+
!+            THEN PERFORMS THE PRODUCT OF ALL THESE DIAGONALS
!+            YIELDING DIAGONAL DB
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
      USE BIEF, EX_DLDU11 => DLDU11
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NELEM,NELMAX,LV,NPOIN
!     NO DEFAULT INITIALISATION FOR USER TYPE COMPONENTS ALLOWED
      DOUBLE PRECISION, INTENT(INOUT) :: DB(NPOIN),XB(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)  :: XA(NELMAX,*)
      CHARACTER(LEN=1), INTENT(IN)  :: TYPDIA,TYPEXA
      INTEGER, INTENT(IN)           :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(OUT) :: W(NELMAX,3)
      LOGICAL, INTENT(IN)           :: COPY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
!-----------------------------------------------------------------------
!
! REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY (EXCEPT IN PARALLEL MODE)
!
      IF(TYPDIA(1:1).NE.'I'.AND.NCSIZE.LE.1) THEN
        WRITE(LU,101) TYPDIA(1:1)
101     FORMAT(1X,'DLDU11 (BIEF) : DIAGONAL OF A NOT EQUAL TO I :',A1)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(TYPEXA(1:1).EQ.'S') THEN
!
        IF(COPY) CALL OV('X=Y     ', X=XB, Y=XA, DIM1=NELMAX*3)
!
        DO IELEM = 1 , NELEM
          W(IELEM,2) = 1.D0 - XB(IELEM,1)**2
          XB(IELEM,3) = (XB(IELEM,3)-XB(IELEM,1)*XB(IELEM,2))/W(IELEM,2)
          W(IELEM,3) = 1.D0 - XB(IELEM,2)**2 -XB(IELEM,3)**2
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSEIF(TYPEXA(1:1).EQ.'Q') THEN
!
        IF(COPY) CALL OV('X=Y     ', X=XB, Y=XA, DIM1=NELMAX*6)
!
        DO IELEM = 1 , NELEM
! L U FACTORISATION
          W(IELEM,2)=1.D0 - XB(IELEM,1)*XB(IELEM,4)
          XB(IELEM,6) = (XB(IELEM,6)-XB(IELEM,1)*XB(IELEM,5))/W(IELEM,2)
          XB(IELEM,3) =  XB(IELEM,3)-XB(IELEM,4)*XB(IELEM,2)
          W(IELEM,3)  = 1.D0-XB(IELEM,2)*XB(IELEM,5)
     &                - XB(IELEM,3)*XB(IELEM,6)
! L D U FACTORISATION
          XB(IELEM,3) = XB(IELEM,3) / W(IELEM,2)
        ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,201) TYPEXA(1:1)
201     FORMAT(1X,'DLDU11 (BIEF) : TYPE OF MATRIX NOT AVAILABLE :',A1)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  MULTIPLICATIVE ASSEMBLY OF THE DIAGONAL WITH INITIALISATION OF DB TO 1
!  SKIPS IKLE1 BECAUSE W1 = 1
!
      CALL ASMVEC(DB,IKLE(1,2),NPOIN,NELEM,NELMAX,2,W(1,2),.TRUE.,LV)
!
!  INVERTS DB
!
      CALL OV('X=1/Y   ', X=DB, Y=DB, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
