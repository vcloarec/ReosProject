!                   *****************
                    SUBROUTINE PRECD1
!                   *****************
!
     &(X,A,B,D,MESH,PRECON,PREXSM,DIADON)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!+               (SEE EXPLANATIONS IN PRECDT).
!+
!+            A IS A SIMPLE MATRIX HERE.
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
!+   Rebuilding the diagonal with MESH%IFAC in parallel in case of
!+   diagonal preconditioning and DIADON=FALSE.
!+   Correction of a bug in parallel with preconditioning 5.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| BLOCK OF MATRICES
!| B              |-->| BLOCK OF RIGHT-HAND SIZES
!| D              |<--| BLOCK OF DIAGONALS
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PRECON         |-->| CHOICE OF PRECONDITIONING
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X AND B
!| X              |<->| BLOCK OF UNKNOWN VECTORS IN THE SYSTEM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PRECD1 => PRECD1
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B,D
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A
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
        CALL OS( 'X=Y     ' , X=D , Y=A%D )
!
!       PARALLEL MODE: COMPLETE DIAGONAL BEFORE GOING FURTHER
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(D,2,MESH)
        ENDIF
!
!       POSSIBLY TAKING THE ABSOLUTE VALUES
!
        IF(PRECON.EQ.5) THEN
          CALL OS( 'X=ABS(Y)' , X=D , Y=D )
        ENDIF
!
!       TAKING THE SQUARE ROOT
!
        CALL OS( 'X=SQR(Y)' , X=D , Y=D )
!
!-----------------------------------------------------------------------
!                                         -1
!  CHANGE OF VARIABLES (D ACTUALLY HOLDS D  )
!
        IF(PREXSM) CALL OS( 'X=XY    ' , X=X , Y=D )
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE INVERSE OF THE SQUARE ROOTS OF THE DIAGONALS
!  THIS GIVES BACK TRUE D AND NOT D INVERTED
!
        CALL OS('X=1/Y   ', X=D, Y=D, IOPT=2, INFINI=1.D0, ZERO=1.D-10)
!
      ELSE
!
!  CASE WHERE D IS GIVEN, CHANGE OF VARIABLES
!  CHANGE OF VARIABLE (D REALLY HOLDS D)
!
        IF(PREXSM) THEN
          CALL OS('X=Y/Z   ' , X=X , Y=X , Z=D)
        ENDIF
!
      ENDIF
!
!=======================================================================
! PRECONDITIONING OF A:
!=======================================================================
!
      CALL OM('M=DMD   ', M=A, D=D, MESH=MESH)
!     IF PRECON = 2 OR 3
      IF((2*(PRECON/2).EQ.PRECON.OR.3*(PRECON/3).EQ.PRECON).AND.
     &                                                 .NOT.DIADON) THEN
!       VALID ONLY WITH ONE SINGLE DOMAIN
        IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
          A%TYPDIA='I'
        ELSE
!         HERE THE DIAGONAL IS REDONE WITH IFAC, SO THAT A MATRIX-VECTOR
!         PRODUCT WILL LEAD TO A SUM OF THE SAME NUMBERS (BUT POSSIBLY
!         NOT IN THE SAME ORDER). OTHERWISE IT IS NOT SURE THAT THE
!         ASSEMBLED DIAGONAL WOULD GIVE EXACT 1.D0.
          DO I=1,A%D%DIM1
            A%D%R(I)=MESH%IFAC%I(I)
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
! PRECONDITIONING OF THE SECOND MEMBER
!
      IF(PREXSM) CALL OS( 'X=XY    ' , X=B , Y=D )
!
!=======================================================================
!
      RETURN
      END

