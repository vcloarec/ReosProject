!                   *****************
                    SUBROUTINE DECLDU
!                   *****************
!
     &(B,A,MESH,COPY,LV)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    L D U FACTORISATION OF THE ELEMENTARY MATRICES
!+                IN MATRIX A.
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
!+   ||||||   FINALLY: DB IS INVERTED BECAUSE THAT'S HOW IT WILL BE
!+                     USED IN DESREM
!
!code
!+-----------------------------------------------------------------------
!+  MEANING OF IELM :
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : P1 TRIANGLE            3                       YES
!+  21 : Q1 QUADRILATERAL       4                       YES
!+  41 : TELEMAC-3D PRISMS      6                       YES
!+
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (LNH)
!+        26/02/04
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
!| A              |<--| MATRIX A.
!| B              |<--| MATRICE B, THE RESULT.
!| COPY           |-->| IF .TRUE. A IS COPIED INTO B.
!|                |   | IF .FALSE. B IS CONSIDERED ALREADY INITIALISED
!| LV             |-->| VECTOR LENGTH OF THE COMPUTER
!| MESH           |-->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DECLDU => DECLDU
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: B
      TYPE(BIEF_OBJ) , INTENT(IN)    :: A
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL        , INTENT(IN)    :: COPY
      INTEGER        , INTENT(IN)    :: LV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NELMAX,IELM,NPOIN,NELEM
!
      CHARACTER(LEN=1) TYPDA,TYPEA
!
!-----------------------------------------------------------------------
!
      IELM   = A%ELMLIN
      NELEM  = MESH%NELEM
      NELMAX = MESH%NELMAX
!
      TYPDA = A%TYPDIA
      TYPEA = A%TYPEXT
!
      NPOIN = A%D%DIM1
!
!-----------------------------------------------------------------------
!
      IF(A%STO.EQ.1) THEN
!
      IF(IELM.EQ.11) THEN
!
        CALL DLDU11(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,MESH%W%R,COPY,LV)
!
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
!
        CALL DLDU21(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,MESH%W%R,COPY,LV)
!
      ELSEIF(IELM.EQ.41) THEN
!
        CALL DLDU41(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,MESH%W%R,COPY,LV)
!
!  IELM NOT IMPLEMENTED: ERROR
!
      ELSE
        WRITE(LU,101) IELM
101   FORMAT(1X,'DECLDU (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ELSEIF(A%STO.EQ.3) THEN
        CALL DLDUSEG(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &               MESH%GLOSEG%I,MESH%NSEG,NPOIN,COPY)
      ELSE
        WRITE(LU,*) 'UNKNOWN MATRIX STORAGE IN DECLDU'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  DESCRIPTION OF B
!
      B%TYPDIA='Q'
      B%TYPEXT=TYPEA
!
!-----------------------------------------------------------------------
!
      RETURN
      END
