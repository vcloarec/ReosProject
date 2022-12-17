!                   **********************************
                    DOUBLE PRECISION FUNCTION BIEF_SUM
!                   **********************************
!
     &( X )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SUMS UP THE COMPONENTS OF A VECTOR.
!+
!+            X CAN BE A VECTOR, OR
!+
!+            A BLOCK STRUCTURE OF VECTORS IN IDENTICAL NUMBER
!+                AND CHARACTERISTICS.
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION,
!+            IT IS PRESENTLY IGNORED
!
!history  J-M HERVOUET (LNHE)
!+        11/05/2009
!+        V6P0
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
!| X              |-->| BIEF_OBJ STRUCTURE OF WHICH X%R WILL BE SUMMED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_SUM => BIEF_SUM
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!     STRUCTURES: VECTORS OR BLOCKS
!
      TYPE(BIEF_OBJ), INTENT(IN) :: X
!
!-----------------------------------------------------------------------
!
!  CASE OF A VECTOR
!
      IF(X%TYPE.EQ.2) THEN
!
        BIEF_SUM = SOMME(X%R,X%DIM1*X%DIM2)
!
!-----------------------------------------------------------------------
!
!  CASE WHERE THE STRUCTURES ARE BLOCKS (TO BE CODED UP)
!
!     ELSEIF(X%TYPE.EQ.4) THEN
!
!-----------------------------------------------------------------------
!
!  ERROR
!
      ELSE
!
        WRITE(LU,60) X%NAME,X%TYPE
        WRITE(LU,63)
60      FORMAT(1X,'BIEF_SUM (BIEF): NAME OF X : ',A6,'  TYPE : ',1I6)
63      FORMAT(1X,'                 CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
