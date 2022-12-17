!                   ********************
                    SUBROUTINE CHECK_DOT
!                   ********************
!
     &(X,T,TEXTE,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IN PARALLEL MODE, PRINTS THE EUCLIDIAN NORM OF A VECTOR,
!+                WHICH HAS NOT BEEN ASSEMBLED WITH PARCOM. E.G. A RIGHT
!+                HAND SIDE BEFORE CALLING SOLVE.
!
!history  J-M HERVOUET (LNHE)
!+        18/11/08
!+        V5P9
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
!| MESH           |-->| MESH STRUCTURE
!| T              |<->| A WORK BIEF_OBJ STRUCTURE
!| TEXTE          |-->| A TEXT TO BE PRINTED
!| X              |-->| THE BIEF_OBJ STRUCTURE WITH THE VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=*) :: TEXTE
      TYPE(BIEF_OBJ)   :: X,T
      TYPE(BIEF_MESH)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CALL OS('X=Y     ',X=T,Y=X)
      IF(NCSIZE.GT.1) CALL PARCOM(T,2,MESH)
      WRITE(LU,*) TEXTE,'=',P_DOTS(T,T,MESH)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
