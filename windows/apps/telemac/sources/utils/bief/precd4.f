!                   *****************
                    SUBROUTINE PRECD4
!                   *****************
!
     &(X1,X2,A11,A12,A21,A22,
     & B1,B2,D1,D2,MESH,PRECON,PREXSM,DIADON)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!+               (SEE EXPLANATIONS IN PRECDT).
!+
!+            A IS A 4-MATRIX BLOCK HERE.
!
!history  J-M HERVOUET (LNHE)
!+        06/07/2009
!+        V5P0
!+   First version (of the header...)
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
!+        08/12/2015
!+        V7P1
!+   Rebuilding the diagonals with MESH%IFAC in parallel in case of
!+   diagonal preconditioning and DIADON=FALSE.
!+   Correction of a bug in parallel with preconditioning 5.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A11            |<->| TERM (1,1) OF MATRIX
!| A12            |<->| TERM (1,2) OF MATRIX
!| A21            |<->| TERM (2,1) OF MATRIX
!| A22            |<->| TERM (2,2) OF MATRIX
!| B1             |<->| FIRST RIGHT-HAND SIDE
!| B2             |<->| SECOND RIGHT-HAND SIDE
!| D1             |<--| DIAGONAL MATRIX
!| D2             |<--| DIAGONAL MATRIX
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PRECON         |-->| CHOICE OF PRECONDITIONING
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X1,X2 AND B1,B2
!| X1             |<->| FIRST INITIAL GUESS
!| X2             |-->| SECOND INITIAL GUESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PRECD4 => PRECD4
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: PRECON
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,B1,B2,D1,D2
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A21,A22
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  PREPARES THE DIAGONALS:
!
      IF(.NOT.DIADON) THEN
!
!       COPY
!
        CALL OS( 'X=Y     ' , X=D1 , Y=A11%D )
        CALL OS( 'X=Y     ' , X=D2 , Y=A22%D )
!
!       PARALLEL MODE: COMPLETE DIAGONAL BEFORE GOING FURTHER
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(D1,2,MESH)
          CALL PARCOM(D2,2,MESH)
        ENDIF
!
!       POSSIBLY ABSOLUTE VALUES
!
        IF(PRECON.EQ.5) THEN
          CALL OS( 'X=ABS(Y)' , X=D1 , Y=D1 )
          CALL OS( 'X=ABS(Y)' , X=D2 , Y=D2 )
        ENDIF
!
!       SQUARE ROOT
!
        CALL OS( 'X=SQR(Y)' , X=D1 , Y=D1 )
        CALL OS( 'X=SQR(Y)' , X=D2 , Y=D2 )
!
!-----------------------------------------------------------------------
!                                                    -1
!  CHANGE OF VARIABLES (D1,D2 AND D3 ACTUALLY HOLD D1 ,...)
!
        IF(PREXSM) THEN
          CALL OS( 'X=XY    ' , X=X1 , Y=D1 )
          CALL OS( 'X=XY    ' , X=X2 , Y=D2 )
        ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE INVERSE OF THE SQUARE ROOTS OF THE DIAGONALS
!  THIS GIVES BACK TRUE D1 AND D2 AND NOT D1 AND D2 INVERTED
!
        CALL OS('X=1/Y   ', X=D1, Y=D1, IOPT=2,
     &                      INFINI=1.D0, ZERO=1.D-10)
        CALL OS('X=1/Y   ', X=D2, Y=D2, IOPT=2,
     &                      INFINI=1.D0, ZERO=1.D-10)
!
      ELSE
!
!  CASE WHERE D IS GIVEN, CHANGE OF VARIABLES
!  CHANGE OF VARIABLE (D1,D2 REALLY HOLD D1,D2)
!
        IF(PREXSM) THEN
          CALL OS('X=Y/Z   ' , X=X1 , Y=X1 , Z=D1)
          CALL OS('X=Y/Z   ' , X=X2 , Y=X2 , Z=D2)
        ENDIF
!
      ENDIF
!
!=======================================================================
! PRECONDITIONING OF A11 :
!=======================================================================
!
      CALL OM('M=DMD   ', M=A11, D=D1, MESH=MESH)
!
!=======================================================================
! PRECONDITIONING OF A12 :
!=======================================================================
!
      CALL OM('M=DM    ', M=A12, D=D1, MESH=MESH)
      CALL OM('M=MD    ', M=A12, D=D2, MESH=MESH)
!
!=======================================================================
! PRECONDITIONING OF A21 :
!=======================================================================
!
      CALL OM('M=DM    ', M=A21, D=D2, MESH=MESH)
      CALL OM('M=MD    ', M=A21, D=D1, MESH=MESH)
!
!=======================================================================
! PRECONDITIONING OF A22 :
!=======================================================================
!
      CALL OM('M=DMD   ', M=A22, D=D2, MESH=MESH)
!
!=======================================================================
!
!     CASES WHERE THE DIAGONALS ARE KNOWN
!     (VALID ONLY WITH ONE SINGLE DOMAIN)
!
      IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
!
!       IF PRECON = 2 OR 3
        IF(2*(PRECON/2).EQ.PRECON.AND..NOT.DIADON) THEN
          A11%TYPDIA='I'
          A22%TYPDIA='I'
        ELSEIF(3*(PRECON/3).EQ.PRECON.AND..NOT.DIADON) THEN
          A11%TYPDIA='I'
          A22%TYPDIA='I'
          A12%TYPDIA='0'
          A21%TYPDIA='0'
        ENDIF
!
      ELSE
!
!       CASE OF DIAGONAL=IDENTITY, BUT ONLY AFTER ASSEMBLING
!
        IF((2*(PRECON/2).EQ.PRECON.OR.3*(PRECON/3).EQ.PRECON).AND.
     &                                                .NOT.DIADON) THEN
!         HERE THE DIAGONAL IS REDONE WITH IFAC, SO THAT A MATRIX-VECTOR
!         PRODUCT WILL LEAD TO A SUM OF THE SAME NUMBERS (BUT POSSIBLY
!         NOT IN THE SAME ORDER). OTHERWISE IT IS NOT SURE THAT THE
!         ASSEMBLED DIAGONAL WOULD GIVE EXACT 1.D0.
          DO I=1,A11%D%DIM1
            A11%D%R(I)=MESH%IFAC%I(I)
          ENDDO
          DO I=1,A22%D%DIM1
            A22%D%R(I)=MESH%IFAC%I(I)
          ENDDO
        ENDIF
!
      ENDIF
!
!=======================================================================
!
! PRECONDITIONING OF THE SECOND MEMBER
!
      IF(PREXSM) THEN
        CALL OS( 'X=XY    ' , X=B1 , Y=D1 )
        CALL OS( 'X=XY    ' , X=B2 , Y=D2 )
      ENDIF
!
!=======================================================================
!
      RETURN
      END

