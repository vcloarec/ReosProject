!                   *****************
                    SUBROUTINE DIRICH
!                   *****************
!
     &(F, S, SM , FBOR,LIMDIR,WORK,MESH,KDIR,MSK,MASKPT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TAKES INTO ACCOUNT POINTS OF TYPE DIRICHLET IN A SYSTEM
!+                OF LINEAR EQUATIONS WITH SYMMETRICAL MATRIX.
!+
!+            IN THE EQUATIONS FOR POINTS NOT OF TYPE DIRICHLET :
!+                DIRICHLET VALUES ARE REMOVED.
!+
!+            IN THE EQUATIONS FOR POINTS OF TYPE DIRICHLET :
!+                DEFINES AN EQUATION FIXING THE IMPOSED VALUE.
!
!warning  FOR SYSTEMS OF MATRICES BLOCKS :
!+            THE EXTRA-DIAGONAL MATRICES MUST BE NONSYMMETRICAL
!+            BECAUSE TAKING INTO ACCOUNT THE DIRICHLET POINTS
!+            MAKES THEM NONSYMMETRICAL
!
!history  J-M HERVOUET (LNHE)
!+        11/07/2008
!+        V5P9
!+   LIMPRO DIMENSION MODIFIED FOR QUADRATIC ELEMENTS
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
!| F              |-->| VARIABLE THAT WILL BE GIVEN ITS DIRICHLET VALUE
!| FBOR           |-->| DIRICHLET BOUNDARY CONDITIONS
!| KDIR           |-->| CONVENTION FOR DIRICHLET BOUNDARY CONDITIONS
!| LIMDIR         |-->| TYPES OF BOUNDARY CONDITIONS
!|                |   | IF LIMDIR(K) = KDIR LE KTH BOUNDARY POINT
!|                |   | IS OF DIRICHLET TYPE.
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| S              |<->| MATRIX OF THE LINEAR SYSTEM
!| SM             |-->| RIGHT HAND SIDE
!| WORK           |-->| BLOCK OF WORK ARRAYS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIRICH => DIRICH
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!                                                   DIMLIM,6
      INTEGER        , INTENT(IN)    :: KDIR,LIMDIR(*)
      LOGICAL        , INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: WORK
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: F,SM,S
      TYPE(BIEF_OBJ) , INTENT(IN)    :: FBOR,MASKPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER DIMLIM
!
!----------------------------------------------------------------------
!
!  IF S IS A MATRIX
!
      IF(S%TYPE.EQ.3) THEN
!
      CALL DIRI01(F, S, SM , FBOR,LIMDIR,WORK%ADR(1)%P,WORK%ADR(2)%P,
     &            MESH,KDIR,MSK,MASKPT)
!
!  IF S IS A BLOCK OF 4 MATRICES
!
      ELSEIF(S%TYPE.EQ.4.AND.S%N.EQ.4) THEN
!
      DIMLIM=MAX(FBOR%ADR(1)%P%DIM1,
     &           FBOR%ADR(2)%P%DIM1)
!
      CALL DIRI04(F%ADR(1)%P,F%ADR(2)%P,
     &     S%ADR(1)%P,S%ADR(2)%P,S%ADR(3)%P,S%ADR(4)%P,
     &     SM%ADR(1)%P,SM%ADR(2)%P,
     &     WORK%ADR(1)%P,WORK%ADR(2)%P,WORK%ADR(3)%P,WORK%ADR(4)%P,
     &     FBOR%ADR(1)%P,FBOR%ADR(2)%P,
     &     LIMDIR(1:DIMLIM),LIMDIR(DIMLIM+1:2*DIMLIM),
     &     MESH,KDIR,MSK,MASKPT)
!
!  IF S IS A BLOCK OF 9 MATRICES
!
      ELSEIF(S%TYPE.EQ.4.AND.S%N.EQ.9) THEN
!
      DIMLIM=MAX(FBOR%ADR(1)%P%DIM1,
     &           FBOR%ADR(2)%P%DIM1,
     &           FBOR%ADR(3)%P%DIM1)
!
      CALL DIRI09(F%ADR(1)%P,F%ADR(2)%P,F%ADR(3)%P,
     &            S%ADR(1)%P,S%ADR(2)%P,S%ADR(3)%P,
     &            S%ADR(4)%P,S%ADR(5)%P,S%ADR(6)%P,
     &            S%ADR(7)%P,S%ADR(8)%P,S%ADR(9)%P,
     &            SM%ADR(1)%P,SM%ADR(2)%P,SM%ADR(3)%P,
     &            WORK%ADR(1)%P,WORK%ADR(2)%P,WORK%ADR(3)%P,
     &            WORK%ADR(4)%P,WORK%ADR(5)%P,WORK%ADR(6)%P,
     &            FBOR%ADR(1)%P,FBOR%ADR(2)%P,FBOR%ADR(3)%P,
     &            LIMDIR(         1:  DIMLIM),
     &            LIMDIR(  DIMLIM+1:2*DIMLIM),
     &            LIMDIR(2*DIMLIM+1:3*DIMLIM),
     &            MESH,KDIR,MSK,MASKPT)
!
!  ERROR
!
      ELSE
!
        WRITE(LU,1001) S%TYPE
1001    FORMAT(1X,'DIRICH (BIEF): WRONG TYPE FOR S:',1I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!----------------------------------------------------------------------
!
      RETURN
      END
