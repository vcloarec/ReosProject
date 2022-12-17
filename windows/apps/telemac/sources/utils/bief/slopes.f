!                   *****************
                    SUBROUTINE SLOPES
!                   *****************
!
     &(COEF,Z,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENT 1 / COS(ALFA)
!+                WHERE ALFA IS THE SLOPE OF A TRIANGULAR ELEMENT.
!+
!+            THIS COEFFICIENT IS USED IN THE BOTTOM FRICTION
!+                TERM.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        17/08/94
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
!| COEF           |<--| RESULT
!| MESH           |-->| MESH STRUCTURE
!| Z              |-->| BOTTOM ELEVATIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SLOPES => SLOPES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  STRUCTURES OF VECTOR
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: COEF
      TYPE(BIEF_OBJ), INTENT(IN)    :: Z
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER NELEM,NELMAX,IELM
!
!-----------------------------------------------------------------------
!
      IELM   = MESH%TYPELM
      NELEM  = MESH%NELEM
      NELMAX = MESH%NELMAX
!
!-----------------------------------------------------------------------
!
      COEF%ELM = IELM
      COEF%DIM1= BIEF_NBPTS(IELM,MESH)
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.10) THEN
        CALL SLOP10(COEF%R,MESH%XEL%R,MESH%YEL%R,
     &              Z%R,MESH%IKLE%I,NELEM,NELMAX)
      ELSE
        WRITE(LU,301) MESH%TYPELM
301     FORMAT(1X,'SLOPES (BIEF) : UNKNOWN ELEMENT:',1I6)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
