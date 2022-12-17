!                   *****************
                    SUBROUTINE CHGDIS
!                   *****************
!
     &(X,OLDELT,NEWELT,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   16/03/2011
!***********************************************************************
!
!brief    CHANGES THE DISCRETISATION OF A VECTOR.
!
!history  J-M HERVOUET (LNH)
!+        13/02/2008
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
!history  J-M HERVOUET (LNHE)
!+        16/03/2011
!+        V6P1
!+        SEQUENCE: X%DIM1 = NEWDIM1
!+                  X%ELM  = NEWELT
!+        MOVED AT THE END, CHGDIS MAY BE CALLED WITH X%ELM BEING OLDELT
!+        THEN IT MAKES OLDELT=NEWELT WHEREAS OLDELT WAS INTENT(IN) AND
!+        SHOULD NOT BE CHANGED BEFORE TESTS ON IT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| MESH STRUCTURE
!| NEWELT         |-->| NEW TYPE FOR X
!| OLDELT         |-->| OLD TYPE OF X
!| X              |<--| VECTOR TO BE MODIFIED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CHGDIS => CHGDIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      INTEGER, INTENT(IN)           :: NEWELT
      INTEGER, INTENT(INOUT)        :: OLDELT
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NEWDIM1
!
!-----------------------------------------------------------------------
!
      NEWDIM1 = BIEF_NBPTS(NEWELT,MESH)
!
      IF(NEWDIM1.GT.X%MAXDIM1) THEN
        WRITE(LU,201) X%NAME
201     FORMAT(1X,'CHGDIS (BIEF) : EXTENSION IMPOSSIBLE FOR ',A6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(OLDELT.EQ.11.AND.NEWELT.EQ.12) THEN
!
        CALL CG1112(X%R,NEWDIM1,X%DIM2,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX)
!
      ELSEIF(OLDELT.EQ.11.AND.NEWELT.EQ.13) THEN
!
        CALL CG1113(X%R,NEWDIM1,X%DIM2,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX)
!
      ELSEIF((OLDELT.EQ.12.OR.OLDELT.EQ.13).AND.NEWELT.EQ.11) THEN
!
!       DOES NOTHING (QUASI-BUBBLE OR QUADRATIC VALUES JUST LOST)
!
      ELSE
!
        WRITE(LU,11) OLDELT,NEWELT
11      FORMAT(1X,'CHGDIS: CASE NOT IMPLEMENTED:',I6,' ',I6)
        WRITE(LU,*) 'STRUCTURE X = ',X%NAME
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
      X%DIM1 = NEWDIM1
!     THIS MAY BE A HIDDEN MODIFICATION OF OLDELT, IF X%ELM IS SENT AS
!     OLDELT, HENCE THE INTENT(INOUT) FOR OLDELT.
      X%ELM  = NEWELT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
