!                   *****************
                    SUBROUTINE PREBD9
!                   *****************
!
     &(X1,X2,X3,A11,A12,A13,A21,A22,A23,A31,A32,A33,
     & B1,B2,B3,D11,D12,D13,D21,D22,D23,D31,D32,D33,
     & MESH,PREXSM,DIADON)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BLOCK-DIAGONAL PRECONDITIONING OF A SYSTEM A X = B.
!
!history  J.M. HERVOUET (LNH)
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
!| A11            |<->| TERM (1,1) OF MATRIX
!| ...            |<->| ...
!| A33            |<->| TERM (3,3) OF MATRIX
!| B1             |<->| FIRST RIGHT-HAND SIDE
!| B2             |<->| SECOND RIGHT-HAND SIDE
!| B3             |<->| THIRD RIGHT-HAND SIDE
!| D11            |<--| DIAGONAL MATRIX
!| ...            |<--| ...
!| D33            |<--| DIAGONAL MATRIX
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X1,X2 AND B1,B2
!| X1             |<->| FIRST INITIAL GUESS
!| X2             |-->| SECOND INITIAL GUESS
!| X3             |-->| THIRD INITIAL GUESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PREBD9 => PREBD9
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
!
!-----------------------------------------------------------------------
!
!  VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: X3,B1
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,B2,B3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: D11,D12,D13,D21,D22,D23
      TYPE(BIEF_OBJ), INTENT(INOUT) :: D31,D32,D33
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A13,A21,A22
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A23,A31,A32,A33
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,NPOIN1,NPOIN2,NPOIN3
!
!-----------------------------------------------------------------------
!
      NPOIN1 = X1%DIM1
      NPOIN2 = X2%DIM1
      NPOIN3 = X3%DIM1
!
      IF(NPOIN2.NE.NPOIN1.AND.NPOIN3.NE.NPOIN1) THEN
        WRITE(LU,200)
200     FORMAT(1X,'PREBD9 (BIEF) : RECTANGULAR MATRICES',/,1X,
     &  'BLOCK-DIAGONAL PRECONDITIONING IMPOSSIBLE IN THIS CASE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  PREPARES THE DIAGONALS:
!
      IF(.NOT.DIADON) THEN
!
        CALL OS('X=Y     ', X=D11, Y=A11%D)
        CALL OS('X=Y     ', X=D12, Y=A12%D)
        CALL OS('X=Y     ', X=D13, Y=A13%D)
        CALL OS('X=Y     ', X=D21, Y=A21%D)
        CALL OS('X=Y     ', X=D22, Y=A22%D)
        CALL OS('X=Y     ', X=D23, Y=A23%D)
        CALL OS('X=Y     ', X=D31, Y=A31%D)
        CALL OS('X=Y     ', X=D32, Y=A32%D)
        CALL OS('X=Y     ', X=D33, Y=A33%D)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  L D U FACTORISATION OF THE DIAGONAL BLOCK:
!
!     ONLY D11 INVERTED IS NOW USED
      CALL OS('X=1/Y   ', X=D11, Y=D11)
!
      DO I = 1,NPOIN1
!
        D21%R(I) =  D21%R(I) * D11%R(I)
        D31%R(I) =  D31%R(I) * D11%R(I)
        D22%R(I) =  D22%R(I) - D21%R(I) * D12%R(I)
!
      ENDDO
!
!     ONLY D22 INVERTED IS NOW USED
      CALL OS('X=1/Y   ', X=D22, Y=D22)
!
      DO I = 1,NPOIN1
!
        D32%R(I) = (D32%R(I) - D31%R(I) * D12%R(I)) * D22%R(I)
        D23%R(I) =  D23%R(I) - D21%R(I) * D13%R(I)
        D33%R(I) =  D33%R(I)
     &             -D31%R(I)*D13%R(I)-D32%R(I)*D23%R(I)
        D12%R(I) =  D12%R(I) * D11%R(I)
        D13%R(I) =  D13%R(I) * D11%R(I)
        D23%R(I) =  D23%R(I) * D22%R(I)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
! CHANGE OF VARIABLES:
!
      IF(PREXSM) THEN
!
        CALL OS('X=X+YZ  ', X=X1, Y=X2, Z=D12)
        CALL OS('X=X+YZ  ', X=X1, Y=X3, Z=D13)
        CALL OS('X=X+YZ  ', X=X2, Y=X3, Z=D23)
!
      ENDIF
!
!  COMPUTES THE SQUARE ROOT
!  INVERTS D11,D22,D33
!  (THEY ARE ONLY USED IN THIS FORM FROM NOW ON)
!
!     INVERSION OF D11 ALREADY PERFORMED
!     INVERSION OF D22 ALREADY PERFORMED
      CALL OS('X=1/Y   ', X=D33, Y=D33)
      CALL OS('X=SQR(Y)', X=D11, Y=D11)
      CALL OS('X=SQR(Y)', X=D22, Y=D22)
      CALL OS('X=SQR(Y)', X=D33, Y=D33)
!
!=======================================================================
! MULTIPLIES A ON THE LEFT BY L INVERTED
!=======================================================================
!
!  A21 :
      CALL OM('M=M-DN  ', M=A21, N=A11, D=D21, MESH=MESH)
!  A22 :
      CALL OM('M=M-DN  ', M=A22, N=A12, D=D21, MESH=MESH)
!  A23 :
      CALL OM('M=M-DN  ', M=A23, N=A13, D=D21, MESH=MESH)
!  A31 :
      CALL OM('M=M-DN  ', M=A31, N=A11, D=D31, MESH=MESH)
      CALL OM('M=M-DN  ', M=A31, N=A21, D=D32, MESH=MESH)
!  A32 :
      CALL OM('M=M-DN  ', M=A32, N=A12, D=D31, MESH=MESH)
      CALL OM('M=M-DN  ', M=A32, N=A22, D=D32, MESH=MESH)
!  A33 :
      CALL OM('M=M-DN  ', M=A33, N=A13, D=D31, MESH=MESH)
      CALL OM('M=M-DN  ', M=A33, N=A23, D=D32, MESH=MESH)
!
!=======================================================================
! MULTIPLIES A ON THE RIGHT BY U INVERTED
!=======================================================================
!
!  A12 :
      CALL OM('M=M-ND  ', M=A12, N=A11, D=D12, MESH=MESH)
!  A22 :
      CALL OM('M=M-ND  ', M=A22, N=A21, D=D12, MESH=MESH)
!  A32 :
      CALL OM('M=M-ND  ', M=A32, N=A31, D=D12, MESH=MESH)
!  A13 :
      CALL OM('M=M-ND  ', M=A13, N=A11, D=D13, MESH=MESH)
      CALL OM('M=M-ND  ', M=A13, N=A12, D=D23, MESH=MESH)
!  A23 :
      CALL OM('M=M-ND  ', M=A23, N=A21, D=D13, MESH=MESH)
      CALL OM('M=M-ND  ', M=A23, N=A22, D=D23, MESH=MESH)
!  A33 :
      CALL OM('M=M-ND  ', M=A33, N=A31, D=D13, MESH=MESH)
      CALL OM('M=M-ND  ', M=A33, N=A32, D=D23, MESH=MESH)
!
!-----------------------------------------------------------------------
!
! NEW SECOND MEMBER
!
      IF(PREXSM) THEN
!
      DO I = 1,NPOIN1
        B2%R(I) = B2%R(I)-D21%R(I)*B1%R(I)
        B3%R(I) = B3%R(I)-D31%R(I)*B1%R(I)-D32%R(I)*B2%R(I)
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
