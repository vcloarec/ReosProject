!                   ***********************
                    LOGICAL FUNCTION CMPOBJ
!                   ***********************
!
     &( OBJ1 , OBJ2 )
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    COMPARES 2 OBJECTS.
!
!history  J-M HERVOUET (LNH)
!+        01/03/90
!+        V5P1
!+   First version
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/06/2015
!+        V7P1
!+   Now will never stop but just return TRUE or FALSE.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| OBJ1           |-->| BIEF_OBJ STRUCTURE TO BE COMPARED WITH THE OTHER
!| OBJ2           |-->| BIEF_OBJ STRUCTURE TO BE COMPARED WITH THE OTHER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CMPOBJ => CMPOBJ
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN) ::  OBJ1,OBJ2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM1,IELM2,TYP1,TYP2
!
!-----------------------------------------------------------------------
!
      CMPOBJ = .FALSE.
!
      TYP1 = OBJ1%TYPE
      TYP2 = OBJ2%TYPE
!
      IF(TYP1.EQ.TYP2) THEN
!
        IF(TYP1.EQ.2) THEN
!
!         VECTORS: CHECKS THE DISCRETISATION AND SIZE
!
          IELM1 = OBJ1%ELM
          IELM2 = OBJ2%ELM
          IF(IELM1.EQ.IELM2.AND.OBJ1%DIM1.EQ.OBJ2%DIM1) CMPOBJ = .TRUE.
!
        ELSEIF(TYP1.EQ.4) THEN
!
!         BLOCKS: CHECKS THE NUMBER OF OBJECTS
!
          IF(OBJ1%N.EQ.OBJ2%N) CMPOBJ=.TRUE.
!
        ELSE
!
!         ERROR OR MATRICES TO BE IMPLEMENTED...
!
          WRITE(LU,*) 'CMPOBJ (BIEF): OBJECT 1 : ',OBJ1%NAME,
     &                ' OF TYPE ',TYP1,' UNEXPECTED CASE'
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

