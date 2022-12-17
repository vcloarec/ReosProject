!                   *****************
                    SUBROUTINE REMONT
!                   *****************
!
     &(X, XA,TYPEXA,B,IKLE,NELEM,NELMAX,NPOIN,IELM,DITR,COPY,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE SYSTEM U X = B (ELEMENT BY ELEMENT).
!code
!+            THE MATRIX U IS HERE THE RESULT OF A DECOMPOSITION
!+            DONE IN SUBROUTINE DECLDU
!+
!+            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM:
!+
!+            LE X DE X UE
!+
!+            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!+
!+                                                T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!
!code
!+-----------------------------------------------------------------------
!+  MEANING OF IELM :
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : TRIANGLE P1            3                       YES
!+  12 : TRIANGLE P2            6
!+  13 : TRIANGLE P1-ISO P1     6
!+  14 : TRIANGLE P2            7
!+  21 : QUADRILATERAL Q1       4                       YES
!+  22 : QUADRILATERAL Q2       8
!+  24 : QUADRILATERAL Q2       9
!+  31 : TETRAHEDRON P1         4
!+  32 : TETRAHEDRON P2        10
!+  41 : TELEMAC-3D PRISMS      6                       YES
!+  51 : PRISMS SPLIT IN TETRAHEDRONS  4                YES
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        05/02/91
!+        V5P3
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
!| B              |<--| RIGHT-HAND SIDE OF THE LINEAR SYSTEM TO BE SOLVED
!| COPY           |-->| IF .TRUE. B IS COPIED INTO X TO START WITH
!| DITR           |-->| CHARACTER, IF  'D' : DIRECT MATRIX A CONSIDERED
!|                |   |                'T' : TRANSPOSED MATRIX A CONSIDERED
!| IELM           |-->| TYPE OF ELEMENT (SEE ABOVE)
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| TYPEXA         |-->| TYPE OF OFF-DIAGONAL TERMS IN THE MATRIX
!| X              |<--| SOLUTION OF THE SYSTEM AX = B
!| XA             |<--| OFF-DIAGONAL TERMS OF THE MATRIX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_REMONT => REMONT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IELM,NPOIN,NELEM,NELMAX,LV
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: B(NPOIN),XA(NELMAX,*)
!
      CHARACTER(LEN=*), INTENT(IN) :: TYPEXA,DITR
!
      LOGICAL, INTENT(IN) :: COPY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
! 1) INITIALISES : X = SECOND MEMBER
!
      IF(COPY) CALL OV('X=Y     ', X=X, Y=B, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
! 2) INVERTS THE UPPER TRIANGULAR MATRICES (TRACEBACK)
!
!     2.1) TRANSPOSE CASE
!
      IF(TYPEXA(1:1).EQ.'S' .OR.
     &  (TYPEXA(1:1).EQ.'Q'.AND.DITR(1:1).EQ.'D')) THEN
!
      IF(IELM.EQ.11) THEN
!
        CALL REM11(X,XA(1,1),XA(1,2),XA(1,3),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
!
        CALL REM21(X,XA(1,1),XA(1,2),XA(1,3),XA(1,4),XA(1,5),XA(1,6),
     &               IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &               NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.41) THEN
!
        CALL REM41(X,XA(1,1),XA(1,2),XA(1,3),XA(1,4) ,XA(1,5) ,XA(1,6),
     &             XA(1,7),XA(1,8),XA(1,9),XA(1,10),XA(1,11),XA(1,12),
     &             XA(1,13),XA(1,14),XA(1,15),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &             IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX,NPOIN,LV)
!
!  VALUE FOR IELM NOT PERMITTED : ERROR
!
      ELSE
!
        WRITE(LU,101) IELM
101   FORMAT(1X,'REMONT (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     2.2) TRANSPOSE CASE
!
      ELSEIF(TYPEXA(1:1).EQ.'Q'.AND.DITR(1:1).EQ.'T') THEN
!
      IF(IELM.EQ.11) THEN
!
        CALL REM11(X,XA(1,4),XA(1,5),XA(1,6),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
!
        CALL REM21(X,XA(1,7),XA(1,8),XA(1,9),XA(1,10),XA(1,11),XA(1,12),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &             NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.41) THEN
!
        CALL REM41(X,
     &            XA(1,16),XA(1,17),XA(1,18),XA(1,19),XA(1,20),XA(1,21),
     &            XA(1,22),XA(1,23),XA(1,24),XA(1,25),XA(1,26),XA(1,27),
     &            XA(1,28),XA(1,29),XA(1,30),
     &            IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &            IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX,NPOIN,LV)
!
!  VALUE FOR IELM NOT PERMITTED : ERROR
!
      ELSE
!
        WRITE(LU,101) IELM
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     2.3) CASE NOT IMPLEMENTED
!
      ELSE
        WRITE(LU,201) TYPEXA(1:1)
201     FORMAT(1X,'REMONT (BIEF) : UNEXPECTED TYPE OF MATRIX :',A1)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
