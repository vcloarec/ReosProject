!                   ****************
                    SUBROUTINE GSEBE
!                   ****************
!
     &(B,A,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FACTORISES THE ELEMENTARY MATRICES IN MATRIX A
!+                USING THE GAUSS-SEIDEL EBE METHOD.
!+
!+           (A CAN ALSO BE A BLOCK OF MATRICES; IN THIS CASE ALL
!+                THE MATRICES IN THE BLOCK ARE TREATED).
!
!warning  REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY
!
!history  J-M HERVOUET (LNH)
!+        23/12/94
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
!| A              |<--| MATRIX A.
!| B              |<--| RESULTING MATRIX.
!| MESH           |-->| MESH STRUCTURE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GSEBE => GSEBE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN) :: A
      TYPE(BIEF_OBJ), INTENT(INOUT) :: B
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER SA,SB,I
!
!-----------------------------------------------------------------------
!
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        WRITE(LU,400) A%TYPE
400     FORMAT(1X,'GSEBE (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(B%TYPE.EQ.3) THEN
        SB = 0
      ELSEIF(B%TYPE.EQ.4) THEN
        SB = B%N
      ELSE
        WRITE(LU,401) B%TYPE
401     FORMAT(1X,'GSEBE (BIEF) :',1I6,' UNEXPECTED TYPE FOR B.')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(SA.EQ.0.AND.SB.EQ.0) THEN
!
!         B%D IS HERE A STRUCTURE OF VECTOR
!         USED AS DUMMY DIAGONAL
          CALL OM('M=N     ', M=B, N=A, MESH=MESH)
          B%TYPDIA='I'
!
      ELSEIF(SA.GT.0.AND.SB.GT.0) THEN
!
!       TAKES THE DIAGONALS OF BLOCK A
!
        DO I=1,SB
          CALL OM('M=N     ', M=B%ADR(I)%P, N=A%ADR(1+(SB+1)*(I-1))%P,
     &            MESH=MESH)
          B%ADR(I)%P%TYPDIA='I'
        ENDDO ! I
!
      ELSEIF(SA.NE.0.AND.SB.EQ.0) THEN
!
!       TAKES THE 1ST DIAGONAL OF BLOCK A
!
        CALL OM('M=N     ', M=B, N=A%ADR(1)%P, MESH=MESH)
        B%TYPDIA='I'
!
      ELSE
!
        WRITE(LU,402)
402     FORMAT(1X,'GSEBE (BIEF) : UNEXPECTED CASE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
