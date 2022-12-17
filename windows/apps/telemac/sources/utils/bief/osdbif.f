!                   *****************
                    SUBROUTINE OSDBIF
!                   *****************
!
     & ( OP , X , Y , INDIC , CRITER , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CONDITIONAL OPERATIONS ON VECTORS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   HERE X IS A VECTOR DEFINED IN THE DOMAIN.
!+   Y IS A VECTOR DEFINED ON THE BOUNDARY.
!+   X, Y AND Z MUST BE STRUCTURES.
!+
!+   INDIC IS AN ARRAY: NOT A STRUCTURE ||||||||
!+
!+   |||||||| : THE OPERATION IS ONLY PERFORMED IF THE CONDITION
!+              INDIC(K)=CRITER IS MET FOR A BOUNDARY NODE K.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   OP = 'X=Y     '     :  COPIES Y IN X
!+   OP = 'X=+Y    '     :  IDEM
!
!history  J-M HERVOUET (LNHE)
!+        22/08/05
!+        V5P6
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
!| CRITER         |-->| OPERATION DONE FOR I IF INDIC(I)=CRITER
!| INDIC          |-->| INTEGER ARRAY WHERE TO LOOK FOR CRITER
!| MESH           |-->| MESH STRUCTURE
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| X              |<--| RESULTING VECTOR
!| Y              |-->| VECTOR USED IN OPERATION OP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      TYPE(BIEF_OBJ) :: X,Y
      TYPE(BIEF_MESH) :: MESH
!
      INTEGER K,NPTFR,IELMX,IELMY
      INTEGER INDIC(*),CRITER
!
      CHARACTER(LEN=8) OP
!
!-----------------------------------------------------------------------
!
      IF(X%TYPE.NE.2.OR.Y%TYPE.NE.2) THEN
        WRITE(LU,101)
101     FORMAT(1X,'OSDBIF (BIEF) : X AND Y ARE NOT VECTORS')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IELMX = X%ELM
      IELMY = Y%ELM
!
! JP RENAUD 18/08/2005
! MODIFICATION FOR 3D MESHES: THE DOMAIN VECTOR DIMENSION IS 3
! AND THE BOUNDARY VECTOR DIMENSION IS 2. SO THE POSSIBLE
! COMBINATIONS ARE:
!     -2D: DIMESN(IELMX)==2 _AND_ DIMESN(IELMY)==1
!     -3D: DIMESN(IELMX)==3 _AND_ DIMESN(IELMY)==2
!
      IF( .NOT. (DIMENS(IELMX).EQ.3 .AND.DIMENS(IELMY).EQ.2 )
     &    .AND.
     &    .NOT. (DIMENS(IELMX).EQ.2 .AND. DIMENS(IELMY).EQ.1 ) ) THEN
!
        WRITE(LU,103)
103     FORMAT(1X,'OSDBIF (BIEF) : X AND Y WRONG DIMENSIONS')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      NPTFR = Y%DIM1
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=Y     '.OR.
     &   OP(1:8).EQ.'X=+Y    ') THEN
!
        DO K=1,NPTFR
          IF(INDIC(K).EQ.CRITER) X%R(MESH%NBOR%I(K)) = Y%R(K)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,1001) OP
1001    FORMAT(1X,'OSDBIF (BIEF) : UNKNOWN OPERATION: ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
