!                   *****************
                    SUBROUTINE DIRI04
!                   *****************
!
     &(X1,X2,A11,A12,A21,A22,SM1,SM2,T1,T2,T3,T4,
     & XBOR1,XBOR2,LIDIR1,LIDIR2,MESH,KDIR,MSK,MASKPT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TREATS THE DIRICHLET POINTS FOR THE FOLLOWING
!+                SYSTEM (BLOCK OF 4 MATRICES):
!code
!+         (     A11          A12              )  ( X1 )   ( SM1 )
!+         (                                   )  (    ) = (     )
!+         (     A21          A22              )  ( X2 )   ( SM2 )
!
!history  J-M HERVOUET (LNH)
!+        30/01/95
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
!| A12            |<->| MATRIX IN THE 2x2 LINEAR SYSTEM
!| A12            |<->| MATRIX IN THE 2x2 LINEAR SYSTEM
!| A21            |<->| MATRIX IN THE 2x2 LINEAR SYSTEM
!| A22            |<->| MATRIX IN THE 2x2 LINEAR SYSTEM
!| KDIR           |-->| CONVENTION FOR DIRICHLET BOUNDARY CONDITIONS
!| LIDIR1         |-->| TYPES OF BOUNDARY CONDITIONS FOR VARIABLE 1
!|                |   | IF LIMDIR(K) = KDIR THE KTH BOUNDARY POINT
!|                |   | IS OF DIRICHLET TYPE.
!| LIDIR2         |-->| TYPES OF BOUNDARY CONDITIONS FOR VARIABLE 2
!|                |   | IF LIMDIR(K) = KDIR THE KTH BOUNDARY POINT
!|                |   | IS OF DIRICHLET TYPE.
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| SM1            |-->| FIRST RIGHT-HAND SIDE OF THE SYSTEM.
!| SM2            |-->| SECOND RIGHT-HAND SIDE OF THE SYSTEM.
!| T1             |<->| WORK DOUBLE PRECISION ARRAY IN A BIEF_OBJ
!| T2             |<->| WORK DOUBLE PRECISION ARRAY IN A BIEF_OBJ
!| T3             |<->| WORK DOUBLE PRECISION ARRAY IN A BIEF_OBJ
!| T4             |<->| WORK DOUBLE PRECISION ARRAY IN A BIEF_OBJ
!| XBOR1          |-->| DIRICHLET BOUNDARY CONDITIONS OF VARIABLE 1
!| XBOR2          |-->| DIRICHLET BOUNDARY CONDITIONS OF VARIABLE 2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIRI04 => DIRI04
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,SM1,SM2,T1,T2,T3,T4
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A21,A22
      TYPE(BIEF_OBJ), INTENT(IN)    :: XBOR1,XBOR2,MASKPT
      INTEGER, INTENT(IN)           :: KDIR,LIDIR1(*),LIDIR2(*)
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      LOGICAL, INTENT(IN)           :: MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C
!
      CHARACTER(LEN=1) STODIA
!
!-----------------------------------------------------------------------
!
! 1) BUILDS ARRAYS T1,T2 CONTAINING:
!    THE X1 AND X2 IMPOSED VALUES IF THE POINT IS OF TYPE DIRICHLET
!    0 OTHERWISE
!
!    X1,X2  3 TAKE THEIR DIRICHLET VALUE
!
!=======================================================================
!
!   BOUNDARY CONDITION FOR X1 : "XBOR1" IMPOSED
!
      CALL CPSTVC(X1,T1)
      CALL OS('X=C     ', X=T1, C=0.D0)
      CALL OSDBIF ( 'X=Y     ',T1,XBOR1,LIDIR1,KDIR,MESH)
!
!-----------------------------------------------------------------------
!
!   BOUNDARY CONDITIONS FOR X2 : "XBOR2" IMPOSED
!
      CALL CPSTVC(X2,T2)
      CALL OS  ('X=C     ', X=T2, C=0.D0)
      CALL OSDBIF ( 'X=Y     ',T2,XBOR2,LIDIR2,KDIR,MESH)
!
!=======================================================================
!
!   2) COMPUTES THE PRODUCT OF THE MATRIX FOR THE SYSTEM TO SOLVE
!      AND T1,T2
!      THE RESULT IS DEDUCTED FROM THE SECOND MEMBERS
!
      CALL MATVEC('X=AY    ',T3,A11,T1,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T3,A12,T2,C,MESH,LEGO=.TRUE. )
      CALL MATVEC('X=AY    ',T4,A21,T1,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T4,A22,T2,C,MESH,LEGO=.TRUE. )
!
      CALL CPSTVC(X1,SM1)
      CALL CPSTVC(X2,SM2)
      CALL OS('X=X-Y   ', X=SM1, Y=T3)
      CALL OS('X=X-Y   ', X=SM2, Y=T4)
!
!=======================================================================
!
!  SECOND MEMBERS OF THE EQUATIONS FOR DIRICHLET POINTS
!  PREPARES THE LINEAR SYSTEM
!
      CALL DIRAUX(SM1,A11%D,XBOR1,T1,X1,LIDIR1,KDIR,MESH )
      CALL DIRAUX(SM2,A22%D,XBOR2,T2,X2,LIDIR2,KDIR,MESH )
!
      IF(MSK) THEN
        CALL OV('X=XY    ', X=SM1%R, Y=MASKPT%R, DIM1=SM1%DIM1)
        CALL OV('X=XY    ', X=X1%R, Y=MASKPT%R, DIM1=X1%DIM1)
        CALL OV('X=XY    ', X=T1%R, Y=MASKPT%R, DIM1=T1%DIM1)
        CALL OV('X=XY    ', X=SM2%R, Y=MASKPT%R, DIM1=SM2%DIM1)
        CALL OV('X=XY    ', X=X2%R, Y=MASKPT%R, DIM1=X2%DIM1)
        CALL OV('X=XY    ', X=T2%R, Y=MASKPT%R, DIM1=T2%DIM1)
      ENDIF
!
!=======================================================================
!
!   ERASES THE LINES AND COLUMNS FOR DIRICHLET POINTS
!
!   IT'S EQUIVALENT TO A DIAGONAL PRECONDITIONING WITH ARRAYS
!   T1,T2,T3
!
!   DOES NOT ALTER A11,A22,A33 DIAGONALS
!   BY GIVING THEM A DUMMY TYPE : '0'
!
!
!=======================================================================
! A11 PRECONDITIONING :
!=======================================================================
!
      STODIA = A11%TYPDIA
      A11%TYPDIA='0'
      CALL OM('M=DMD   ', M=A11, D=T1, MESH=MESH)
      A11%TYPDIA=STODIA
!
!=======================================================================
! A12 PRECONDITIONING :
!=======================================================================
!
      CALL OM('M=DM    ', M=A12, D=T1, MESH=MESH)
      CALL OM('M=MD    ', M=A12, D=T2, MESH=MESH)
!
!=======================================================================
! A21 PRECONDITIONING :
!=======================================================================
!
      CALL OM('M=DM    ', M=A21, D=T2, MESH=MESH)
      CALL OM('M=MD    ', M=A21, D=T1, MESH=MESH)
!
!=======================================================================
! A22 PRECONDITIONING :
!=======================================================================
!
      STODIA = A22%TYPDIA
      A22%TYPDIA='0'
      CALL OM('M=DMD   ', M=A22, D=T2, MESH=MESH)
      A22%TYPDIA=STODIA
!
!-----------------------------------------------------------------------
!
      RETURN
      END
