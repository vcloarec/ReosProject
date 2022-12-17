!                   ***************
                    SUBROUTINE OSBD
!                   ***************
!
     & ( OP , X , Y , Z , C , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON VECTORS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   HERE X IS A VECTOR DEFINED ON THE BOUNDARY.
!+   Y AND Z ARE VECTORS DEFINED IN THE DOMAIN.
!+   X, Y AND Z MUST BE STRUCTURES.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   OP = 'X=Y     '     :  COPIES Y IN X
!+   OP = 'X=+Y    '     :  IDEM
!+   OP = 'X=X+Y   '     :  ADDS Y TO X
!+   OP = 'X=X-Y   '     :  SUBTRACTS Y FROM X
!+   OP = 'X=CY    '     :  MULTIPLIES Y BY C
!+   OP = 'X=X+CY  '     :  ADDS C.Y TO X
!+   OP = 'X=CXY   '     :  C.X.Y
!
!history  J-M HERVOUET (LNH)
!+        06/12/94
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
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| MESH           |-->| MESH STRUCTURE
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| X              |<--| RESULTING VECTOR
!| Y              |-->| VECTOR USED IN OPERATION OP
!| Z              |-->| VECTOR USED IN OPERATION OP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OSBD => OSBD
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      DOUBLE PRECISION, INTENT(IN) :: C
!
      CHARACTER(LEN=8), INTENT(IN) :: OP
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: Y,Z
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER NPTFR,IELMX,IELMY
!
!-----------------------------------------------------------------------
!
      IF(X%TYPE.NE.2.OR.Y%TYPE.NE.2) THEN
        WRITE(LU,101)
101     FORMAT(1X,'OSBD (BIEF) : X AND Y ARE NOT VECTORS')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IELMX = X%ELM
      IELMY = Y%ELM
!
      IF((DIMENS(IELMX).NE.1.OR.DIMENS(IELMY).NE.2).AND.
     &   (DIMENS(IELMX).NE.2.OR.DIMENS(IELMY).NE.3)) THEN
        WRITE(LU,103)
103     FORMAT(1X,'OSBD (BIEF) : X AND Y HAVE WRONG DIMENSIONS')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      NPTFR = X%DIM1
!
      IF(  IELMY.EQ.11.OR.
     &     IELMY.EQ.61.OR.
     &     IELMY.EQ.21.OR.
     &     IELMY.EQ.71.OR.
     &     IELMY.EQ.12.OR.
     &     IELMY.EQ.31.OR.
     &     IELMY.EQ.41.OR.
     &     IELMY.EQ.51      ) THEN
!       ARRAY NBOR
        CALL OVBD( OP , X%R , Y%R , Z%R , C , MESH%NBOR%I , NPTFR )
!
      ELSE
        WRITE(LU,105)
105     FORMAT(1X,'OSBD (BIEF) : DISCRETIZATIONS NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
