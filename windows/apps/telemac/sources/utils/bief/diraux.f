!                   *****************
                    SUBROUTINE DIRAUX
!                   *****************
!
     & ( X , Y , Z , W , F , INDIC , CRITER , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    HELPS PREPARE A LINEAR SYSTEM WITH DIRICHLET CONDITIONS.
!code
!+             X, Y AND Z MUST BE STRUCTURES.
!+
!+             HERE X IS A VECTOR DEFINED ON THE DOMAIN
!+                  Y IS A VECTOR DEFINED ON THE DOMAIN
!+                  Z IS A VECTOR DEFINED ON THE DOMAIN OR BOUNDARY
!+
!+             INDIC IS AN ARRAY, NOT A STRUCTURE ||||||||||
!+
!+  |||||||| : THE OPERATION IS ONLY PERFORMED IF INDIC(K)=CRITER
!+             FOR A GLOBAL OR BOUNDARY NUMBER K.
!+
!+  OPERATIONS :
!+
!+             W SET TO 0.D0 FOR POINTS WHERE INDIC(K) = CRITER
!+                   TO 1.D0 OTHERWISE
!+
!+             X = Y MULTIPLIED BY Z IF INDIC(K) = CRITER
!+
!+             F = Z IF INDIC(K) = CRITER
!+
!+  THESE OPERATIONS ARE USED TO TREAT THE POINTS OF TYPE DIRICHLET.
!+
!+             X IS THE SECOND MEMBER (WILL BE EQUAL TO THE DIAGONAL
!+             MULTIPLIED BY THE DIRICHLET VALUE Z)
!+
!+             F IS THE UNKNOWN (SET TO ITS DIRICHLET VALUE)
!+
!+             W IS A WORKING ARRAY USED TO CANCEL THE MATRICES
!+             TERMS TOUCHING DIRICHLET POINTS
!
!history  J-M HERVOUET (LNH)
!+        06/12/94
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
!| CRITER         |-->| INTEGER, CONVENTION FOR DIRICHLET
!| F              |-->| VARIABLE THAT WILL BE GIVEN ITS DIRICHLET VALUE
!|                |   | TAKEN IN Z
!| INDIC          |-->| BOUNDARY CONDITIONS AT VALUE CRITER OR NOT
!| MESH           |-->| MESH STRUCTURE
!| W              |<->| WORK ARRAY
!| X              |<--| Y MULTIPLIED BY Z IF INDIC(K) = CRITER
!| Y              |-->| VECTOR, A DATA
!| Z              |-->| DIRICHLET VALUES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIRAUX => DIRAUX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: X,W,F
      TYPE(BIEF_OBJ) , INTENT(IN)    :: Y,Z
      INTEGER        , INTENT(IN)    :: INDIC(*),CRITER
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NPOIN,IELMX,IELMZ,N
!
!-----------------------------------------------------------------------
!
      NPOIN = Z%DIM1
!
!-----------------------------------------------------------------------
!
!  W SET TO 1
!
      CALL OS( 'X=C     ' , X=W , C=1.D0 )
!
!-----------------------------------------------------------------------
!
      IELMX=X%ELM
      IELMZ=Z%ELM
!
      IF(IELMX.NE.IELMZ) THEN
!
        DO K=1,NPOIN
          IF(INDIC(K).EQ.CRITER) THEN
            N = MESH%NBOR%I(K)
            X%R(N) = Y%R(N) * Z%R(K)
            W%R(N) = 0.D0
            F%R(N) = Z%R(K)
          ENDIF
        ENDDO
!
      ELSE
!
        DO K=1,NPOIN
          IF(INDIC(K).EQ.CRITER) THEN
            X%R(K) = Y%R(K) * Z%R(K)
            W%R(K) = 0.D0
            F%R(K) = Z%R(K)
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
