!                   *****************
                    SUBROUTINE ADDBLO
!                   *****************
!
     &( BLOC , OBJ )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ADDS AN OBJECT TO A BLOCK STRUCTURE (WHICH IS A LIST)
!
!history  J-M HERVOUET (LNH)
!+        01/03/95
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
!| BLOC           |<->| FORTRAN NAME OF BLOCK
!| OBJ            |-->| BIEF_OBJ STRUCTURE TO BE ADDED IN THE BLOCK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ADDBLO => ADDBLO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)      :: BLOC
      TYPE(BIEF_OBJ), INTENT(IN), TARGET :: OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INCREASES THE NUMBER OF OBJECTS IN THE BLOCK
!
      BLOC%N = BLOC%N + 1
      IF(BLOC%N.GT.BLOC%MAXBLOCK) THEN
        WRITE(LU,*) 'ADDBLO : ',OBJ%NAME,' TOO SMALL'
        WRITE(LU,*) '         INCREASE MAXBLOCK IN ALLBLO'
        WRITE(LU,*) '         (CURRENTLY : ',BLOC%MAXBLOCK,')'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     ASSIGNS THE TARGET OBJ TO THE POINTER OF RANK BLOC%N
!
      BLOC%ADR(BLOC%N)%P => OBJ
!
!-----------------------------------------------------------------------
!
      RETURN
      END
