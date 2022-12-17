!                   ******************
                    SUBROUTINE DLDUSEG
!                   ******************
!
     &(DB,XB,TYPDIA,XA,TYPEXA,GLOSEG,NSEG,NPOIN,COPY)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    L D U FACTORISATION OF THE ELEMENTARY MATRICES BY SEGMENT
!+                FOR SEGMENTS.
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
!+
!+
!+
!+      (  1   X12 )   (  1   0 ) (  1       0     ) (  1   X12 )
!+      (          ) = (        ) (                ) (          )
!+      ( X21   1  )   ( X21  1 ) (  0   1-X12*X21 ) (  0     1 )
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        24/04/97
!+        V5P5
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
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| TYPDIA         |<--| TYPE OF DIAGONAL ( 'Q', 'I' , OR '0' )
!| TYPEXA         |<--| TYPE OF OFF-DIAGONAL TERMS ('Q','S',OR '0')
!| XA             |<--| OFF-DIAGONAL TERMS OF MATRIX A
!| XB             |<--| OFF-DIAGONAL TERMS OF MATRIX B
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DLDUSEG => DLDUSEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NSEG,NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: DB(NPOIN),XB(NSEG,*)
      DOUBLE PRECISION, INTENT(IN)  :: XA(NSEG,*)
      CHARACTER(LEN=1), INTENT(IN)  :: TYPDIA,TYPEXA
      INTEGER, INTENT(IN)           :: GLOSEG(NSEG,2)
      LOGICAL, INTENT(IN)           :: COPY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEG
!
!-----------------------------------------------------------------------
!
! REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY (EXCEPT IN PARALLEL MODE)
!
      IF(TYPDIA(1:1).NE.'I'.AND.NCSIZE.LE.1) THEN
        WRITE(LU,101) TYPDIA(1:1)
101     FORMAT(1X,'DLDUSEG (BIEF) : DIAGONAL OF A NOT IDENTITY :',A1)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(TYPEXA(1:1).EQ.'S') THEN
!
        IF(COPY) THEN
          CALL OV('X=Y     ', X=XB, Y=XA, DIM1=NSEG)
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(TYPEXA(1:1).EQ.'Q') THEN
!
        IF(COPY) THEN
          CALL OV('X=Y     ', X=XB , Y=XA, DIM1=2*NSEG)
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
        WRITE(LU,201) TYPEXA(1:1)
201     FORMAT(1X,'DLDUSEG (BIEF) : TYPE OF MATRIX NOT TREATED:',A1)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  MULTIPLICATIVE ASSEMBLY OF THE DIAGONAL WITH INITIALISATION
!  OF DB TO 1
!
      CALL OV('X=C     ' , X=DB, C=1.D0, DIM1=NPOIN)
!
      IF(TYPEXA(1:1).EQ.'S') THEN
!
      DO ISEG=1,NSEG
        DB(GLOSEG(ISEG,2))=DB(GLOSEG(ISEG,2))*(1.D0-XB(ISEG,1)**2)
      ENDDO
!
      ELSE
!
      DO ISEG=1,NSEG
        DB(GLOSEG(ISEG,2))=
     &  DB(GLOSEG(ISEG,2))*(1.D0-XB(ISEG,1)*XB(ISEG,2))
      ENDDO
!
      ENDIF
!
!  INVERTS DB (COULD DIVIDE BY 0)
!
      CALL OV('X=1/Y   ', X=DB, Y=DB, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
