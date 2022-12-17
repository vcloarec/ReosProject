!                   *****************
                    SUBROUTINE DESCEN
!                   *****************
!
     &(X, XA,TYPEXA,B,IKLE,NELEM,NELMAX,NPOIN,IELM,DITR,COPY,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE SYSTEM L X = B (ELEMENT BY ELEMENT).
!code
!+            MATRIX L IS HERE THE RESULT OF THE FACTORISATION
!+            PERFORMED IN SUBROUTINE DECLDU.
!+
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
!
!code
!+-----------------------------------------------------------------------
!+  MEANING OF IELM :
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : P1 TRIANGLE            3                       YES
!+  12 : P2 TRIANGLE            6
!+  13 : P1-ISO P1 TRIANGLE     6
!+  14 : P2 TRIANGLE            7
!+  21 : Q1 QUADRILATERAL       4                       YES
!+  22 : Q2 QUADRILATERAL       8
!+  24 : Q2 QUADRILATERAL       9
!+  31 : P1 TETRAHEDRON         4
!+  32 : P2 TETRAHEDRON        10
!+  41 : TELEMAC-3D PRISMS      6                       YES
!+  41 : PRISMS SPLIT IN TETRAHEDRONS     4             YES
!+
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        18/08/94
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
      USE BIEF, EX_DESCEN => DESCEN
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: IELM,NPOIN,NELEM,NELMAX,LV
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XA(NELMAX,*),B(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXA,DITR
      LOGICAL         , INTENT(IN)    :: COPY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! 1) INITIALISES : X = SECOND MEMBER
!
      IF(COPY) CALL OV('X=Y     ', X=X, Y=B, DIM1=NPOIN)
!
!-----------------------------------------------------------------------
!
! 2) INVERTS THE LOWER TRIANGULAR MATRICES (DESCENT)
!
!     2.1) TRANSPOSE CASE
!
      IF(TYPEXA(1:1).EQ.'S' .OR.
     &  (TYPEXA(1:1).EQ.'Q'.AND.DITR(1:1).EQ.'T')) THEN
!
      IF(IELM.EQ.11) THEN
!
        CALL DES11(X,XA(1,1),XA(1,2),XA(1,3),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
!
        CALL DES21(X,XA(1,1),XA(1,2),XA(1,3),XA(1,4),XA(1,5),XA(1,6),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &             NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.41) THEN
!
        CALL DES41(X,XA(1,1),XA(1,2),XA(1,3),XA(1,4),XA(1,5),XA(1,6),
     &             XA(1,7),XA(1,8),XA(1,9),XA(1,10),XA(1,11),XA(1,12),
     &             XA(1,13),XA(1,14),XA(1,15),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &             IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX,NPOIN,LV)
!
!  IELM NOT IMPLEMENTED: ERROR
!
      ELSE
!
        WRITE(LU,101) IELM
101   FORMAT(1X,'DESCEN (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!     2.2) DIRECT CASE
!
      ELSEIF(TYPEXA(1:1).EQ.'Q'.AND.DITR(1:1).EQ.'D') THEN
!
      IF(IELM.EQ.11) THEN
!
        CALL DES11(X,XA(1,4),XA(1,5),XA(1,6),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
!
        CALL DES21(X,XA(1,7),XA(1,8),XA(1,9),XA(1,10),XA(1,11),XA(1,12),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &             NELEM,NELMAX,NPOIN,LV)
!
      ELSEIF(IELM.EQ.41) THEN
!
        CALL DES41(X,
     &            XA(1,16),XA(1,17),XA(1,18),XA(1,19),XA(1,20),XA(1,21),
     &            XA(1,22),XA(1,23),XA(1,24),XA(1,25),XA(1,26),XA(1,27),
     &            XA(1,28),XA(1,29),XA(1,30),
     &            IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &            IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX,NPOIN,LV)
!
!  IELM NOT IMPLEMENTED: ERROR
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
        WRITE(LU,201) TYPEXA(1:1),DITR(1:1)
201     FORMAT(1X,'DESCEN (BIEF) : UNEXPECTED TYPE OF MATRIX :',A1,/,
     &         1X,'WITH DITR=',A1)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
