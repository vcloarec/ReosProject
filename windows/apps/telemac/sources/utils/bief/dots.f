!                   ******************************
                    DOUBLE PRECISION FUNCTION DOTS
!                   ******************************
!
     &( X , Y )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SCALAR PRODUCT OF TWO OBJECTS, WHICH CAN BE:
!+
!+            TWO VECTORS STRUCTURES, OR
!+
!+            TWO VECTOR BLOCKS STRUCTURES OF IDENTICAL NUMBER AND
!+                CHARACTERISTICS.
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS IGNORED
!+            FOR THE TIME BEING
!
!history  J-M HERVOUET (LNH)
!+        08/12/94
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
!| X              |-->| FIRST VECTOR OR BLOCK
!| Y              |-->| SECOND VECTOR OR BLOCK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DOTS => DOTS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN) :: X,Y
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IBL
!
!-----------------------------------------------------------------------
!
!     CASE WHERE THE STRUCTURES ARE BLOCKS
!
      IF(X%TYPE.EQ.4) THEN
!
        DOTS = 0.D0
        DO IBL = 1 , X%N
          DOTS=DOTS+DOT(X%ADR(IBL)%P%DIM1,X%ADR(IBL)%P%R,Y%ADR(IBL)%P%R)
        ENDDO
!
!-----------------------------------------------------------------------
!
!       CASE WHERE THE STRUCTURES ARE NOT BLOCKS
!       IT ASSUMES THAT Y HAS THE SAME TYPE AS X
!
      ELSEIF(X%TYPE.EQ.2) THEN
!
        DOTS = DOT(X%DIM1,X%R,Y%R)
!
!-----------------------------------------------------------------------
!
!     ERROR
!
      ELSE
!
        WRITE(LU,60) X%NAME,X%TYPE
        WRITE(LU,61) Y%NAME,Y%TYPE
        WRITE(LU,63)
60      FORMAT(1X,'DOTS (BIEF) : NAME OF X : ',A6,'  TYPE : ',1I6)
61      FORMAT(1X,'              NAME OF Y : ',A6,'  TYPE : ',1I6)
63      FORMAT(1X,'              NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
