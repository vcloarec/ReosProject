!#if defined COMPAD_SYMBOLIC_LINSOLVE
!#  undef COMPAD_SYMBOLIC_LINSOLVE
!#endif COMPAD_SYMBOLIC_LINSOLVE
!
!#if defined COMPAD_DCO_T1S
!#  define COMPAD_SYMBOLIC_LINSOLVE
!#endif
!
!#if defined COMPAD_DCO_T1S_FD
!#  undef COMPAD_SYMBOLIC_LINSOLVE
!#endif
!
!#if defined COMPAD_SYMBOLIC_LINSOLVE
!#  include 'ad_solve_deep_copy_bief_obj.f'
!#  include 'ad_solve_T1X_set_bief_obj.f'
!#  include 'ad_solve_T1X_set_int_bief_obj.f'
!#  define AD_BIEF_OBJ_PRINT
!#  include 'ad_solve_print_bief_obj.f'
!#  undef AD_BIEF_OBJ_DIFF_PRINT
!#  define AD_BIEF_OBJ_DIFF_PRINT
!#    include 'ad_solve_diff_print_bief_obj.f'
!#    include 'ad_solve_diff_int_print_bief_obj.f'
!#  undef AD_BIEF_OBJ_DIFF_PRINT
!#endif COMPAD_SYMBOLIC_LINSOLVE


!                   *******************
                    SUBROUTINE AD_SOLVE
!                   *******************
!
     &(X, A,B,TB,CFG,INFOGR,MESH,AUX)
!
!***********************************************************************
! BIEF   V7P2                                               23/11/2016
!***********************************************************************
!
!brief    Wrapper for call to SOLVE to be used with Algorithmic
!+     Differentiation. See SOLVE for more details on arguments.
!+
!
!history  J.RIEHME (ADJOINTWARE) and S.E.BOURBAN (HRW)
!+        23/11/2016
!+        V7P2
!+        Implementation and validation
!
!history  S.E.BOURBAN (HRW) and J.RIEHME (ADJOINTWARE)
!+        20/03/2017
!+        V7P3
!+        Assembling recursive dependencies within this subroutine
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX OF THE SYSTEM (OR BLOCK OF MATRICES)
!| AUX            |-->| MATRIX FOR PRECONDITIONING.
!| B              |-->| RIGHT-HAND SIDE OF THE SYSTEM
!| CFG            |-->| STRUCTURE OF SOLVER CONFIGURATION
!|                |   | CFG%KRYLOV IS USED ONLY IF CFG%SLV = 7 (GMRES)
!| INFOGR         |-->| IF YES, PRINT A LOG.
!| MESH           |-->| MESH STRUCTURE.
!| TB             |-->| BLOCK OF VECTORS WITh AT LEAST
!|                |   | MAX(7,2+2*CFG%KRYLOV)*S VECTORS, S IS 1
!|                |   | IF A IS A MATRIX, 2 IF A BLOCK OF 4 MATRICES
!|                |   | AND 3 IF A BLOCK OF 9.
!| X              |<->| INITIAL VALUE, THEN SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SLVCFG), INTENT(INOUT) :: CFG
!
!     STRUCTURES OF VECTORS OR BLOCKS OF VECTORS
!
      TYPE(BIEF_OBJ), TARGET, INTENT(INOUT) :: X,B
      TYPE(BIEF_OBJ), INTENT(INOUT)         :: TB
!
!     STRUCTURES OF MATRIX OR BLOCK OF MATRICES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A,AUX
!
      LOGICAL, INTENT(IN) :: INFOGR
!
!     MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#if defined COMPAD_SYMBOLIC_LINSOLVE
!
!     LOCAL BIEF_OBJ
!
      TYPE(BIEF_OBJ)                :: A0, B0, XS
      TYPE(BIEF_OBJ)                :: A1, B1, X1A, X1B
!   addional for testing
      TYPE(BIEF_OBJ)                :: A00, B00
!
!     LOCAL INTEGERS
!
      INTEGER, PARAMETER            :: NIX = 0, VAL=0, DRV=1, ALL=-1
      INTEGER                       :: I
#endif /* COMPAD_SYMBOLIC_LINSOLVE */
      INTEGER, SAVE                 :: CNT = 0

!
!-----------------------------------------------------------------------
!
!     DENY REQUEST OF SYMBOLIC LINEAR SOLVER IF NOT AVAILABLE
!
#ifndef COMPAD_SYMBOLIC_LINSOLVE
      AD_SYMBLINSOLV = .FALSE.
#endif
      CNT = CNT + 1
!
!-----------------------------------------------------------------------
!
!     CHECK IF SYMBOLIC LINEAR SOLVER IS **NOT** REQUESTED
!
#if defined AD_BIEF_OBJ_PRINT
      CALL AD_PRINT_BIEF_OBJ( X, 1, "X (INPUT)", "" )
      CALL AD_PRINT_BIEF_OBJ( A, 1, "A (INPUT)", "" )
#endif

      IF ( .NOT. AD_SYMBLINSOLV ) THEN
!
!        WRITE(LU,*)' AD_SOLVE SYMBOLIC LINEAR SOLVER (', CNT,
!     &        ')  ::  not requested'
!
!       0.1 SOLVE ORIGINAL SYSTEM
!
        CALL SOLVE(X,A,B,TB,CFG,INFOGR,MESH,AUX)
!
        RETURN
!
      ENDIF
!
!
!-----------------------------------------------------------------------
!
#if defined COMPAD_SYMBOLIC_LINSOLVE && COMPAD
!
      WRITE(LU,*)' AD_SOLVE SYMBOLIC LINEAR SOLVER  (', CNT,
     &        ') ::  REQUESTED'
!
!     1. SYMBOLIC LINEAR SOLVER
!
!     1.1  TURN OF OTHER STABILISERS
      AD_LINSOLV_RESETDERIV               = .FALSE.
      AD_LINSOLV_DERIVATIVE_CONVERGENCE   = .FALSE.
!
!-----------------------------------------------------------------------
!
!     1.2 CREATE DEEP COPIES OF REQUIRED BIEF_OBJ
!
      CALL AD_DEEP_COPY_BIEF_OBJ( A00, A, CNT,1 )
      CALL AD_DEEP_COPY_BIEF_OBJ( B00, B, CNT,1 )

      CALL AD_DEEP_COPY_BIEF_OBJ( A0,  A, CNT,1 )
      CALL AD_DEEP_COPY_BIEF_OBJ( A1,  A, CNT,1 )
      CALL AD_DEEP_COPY_BIEF_OBJ( B0,  B, CNT,1 )
      CALL AD_DEEP_COPY_BIEF_OBJ( B1,  B, CNT,1 )
      CALL AD_DEEP_COPY_BIEF_OBJ( X1A, X, CNT,1 )
      CALL AD_DEEP_COPY_BIEF_OBJ( X1B, X, CNT,1 )
      CALL AD_DEEP_COPY_BIEF_OBJ( XS,  X, CNT,1 )

#if defined AD_BIEF_OBJ_PRINT
      CALL AD_PRINT_BIEF_OBJ( X,  CNT,1,'','')
#endif
#if defined AD_BIEF_OBJ_DIFF_PRINT
      CALL AD_DIFF_PRINT_BIEF_OBJ( X,ALL,X1A,ALL,CNT,1,'X <--> X1A','')
      CALL AD_DIFF_PRINT_BIEF_OBJ( X,ALL,X1B,ALL,CNT,1,'X <--> X1B','')
      CALL AD_DIFF_PRINT_BIEF_OBJ( X,ALL,XS ,ALL,CNT,1,'X <--> XS ','')
      CALL AD_DIFF_PRINT_BIEF_OBJ( B,ALL,B0 ,ALL,CNT,1,'B <--> B0 ','')
      CALL AD_DIFF_PRINT_BIEF_OBJ( B,ALL,B1 ,ALL,CNT,1,'B <--> B1 ','')
      CALL AD_DIFF_PRINT_BIEF_OBJ( A,ALL,A0 ,ALL,CNT,1,'A <--> A0 ','')
      CALL AD_DIFF_PRINT_BIEF_OBJ( A,ALL,A1 ,ALL,CNT,1,'A <--> A1 ','')
#endif
!
!-----------------------------------------------------------------------
!
!     SET TANGENTS OF COPIES TO ZERO
!
      CALL AD_T1X_SET_INT_BIEF_OBJ( A0,DRV, 0, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( B0,DRV, 0, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( XS,DRV, 0, 1 )

#if defined AD_BIEF_OBJ_DIFF_PRINT
      CALL AD_DIFF_INT_PRINT_BIEF_OBJ( A0,DRV, 0,CNT,1,'A0 <--> 0','')
      CALL AD_DIFF_INT_PRINT_BIEF_OBJ( B0,DRV, 0,CNT,1,'B0 <--> 0','')
      CALL AD_DIFF_INT_PRINT_BIEF_OBJ( XS,DRV, 0,CNT,1,'XS <--> 0','')
#endif
!
!-----------------------------------------------------------------------
!
!     COMPUTE PRIMAL SOLUTION
!
      CALL SOLVE(XS,A0,B0,TB,CFG,INFOGR,MESH,AUX)
!
#if defined AD_BIEF_OBJ_DIFF_PRINT
!     DIFFERS SOLUTION XS FROM STARTING POINT X ?
      CALL AD_DIFF_PRINT_BIEF_OBJ( X,ALL,XS ,ALL,CNT,1,'X <--> XS ','')
#endif
!
#if defined AD_BIEF_OBJ_DIFF_PRINT
!     TANGENTS OF SOLUTION STILL ZERO ???
      CALL AD_DIFF_INT_PRINT_BIEF_OBJ( XS,DRV, 0,CNT,1,'XS <--> 0','')
!      CALL AD_T1X_SET_INT_BIEF_OBJ( XS,DRV, 0, 1 )
#endif
!
#if defined AD_BIEF_OBJ_DIFF_PRINT
!     WAS A0 OR B0 (VALUE COMPONENT)  modified during solving the system ??
      CALL AD_DIFF_PRINT_BIEF_OBJ(A0,VAL,A00,VAL,CNT,1,'A0 <--> A00','')
      CALL AD_DIFF_PRINT_BIEF_OBJ(B0,VAL,B00,VAL,CNT,1,'B0 <--> B00','')
!     WAS A0 OR B0 (TANGENT COMPONENT)  modified during solving the system ??
      CALL AD_DIFF_PRINT_BIEF_OBJ(A0,DRV,A00,DRV,CNT,1,'A0 <--> A00','')
      CALL AD_DIFF_PRINT_BIEF_OBJ(B0,DRV,B00,DRV,CNT,1,'B0 <--> B00','')
#endif
!
!-----------------------------------------------------------------------
!
!     SOLVE LINEAR SYSTEM TO GET PARTIAL OF XS wrt. RHS B
!
      CALL AD_T1X_SET_INT_BIEF_OBJ( X1B,VAL, 0, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( X1B,DRV, 0, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( A1, DRV, 0, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( B1, DRV, 0, 1 )
      CALL AD_T1X_SET_BIEF_OBJ( B1, VAL, B, DRV, 1 )
!
#if defined AD_BIEF_OBJ_DIFF_PRINT
      CALL AD_DIFF_PRINT_BIEF_OBJ( B1, VAL, B , DRV,
     &                   CNT,1, 'VAL(B1) <--> T1S(B) ', '' )
#endif
!
      CALL SOLVE(X1B,A1,B1,TB,CFG,INFOGR,MESH,AUX)

!
!-----------------------------------------------------------------------
!
!     SOLVE LINEAR SYSTEM TO GET PARTIAL OF XS wrt. SYSTEM MATRIX A
!
      CALL AD_T1X_SET_INT_BIEF_OBJ( X1A,VAL, 0, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( X1A,DRV, 0, 1 )

      CALL AD_T1X_SET_BIEF_OBJ( A1,VAL, A,DRV, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( A1,DRV, 0, 1 )

      CALL AD_T1X_SET_INT_BIEF_OBJ( B1 ,VAL, 0, 1 )
      CALL AD_T1X_SET_INT_BIEF_OBJ( B1 ,DRV, 0, 1 )

      CALL MATRBL( 'X=X+CAY ', X=B1 , A=A1, Y=XS, C=-1.D0, MESH=MESH )
!      CALL MATRBL( 'X=X+CAY ', X=B1 , A=A1, Y=XS, C=1.D0, MESH=MESH )

!      CALL AD_T1X_SET_INT_BIEF_OBJ( TB,VAL, 0, 1 )
!      CALL AD_T1X_SET_INT_BIEF_OBJ( TB,DRV, 0, 1 )

      CALL SOLVE(X1A,A1,B1,TB,CFG,INFOGR,MESH,AUX)

!      CALL SOLVE(X1A,A0,B1,TB,CFG,INFOGR,MESH,AUX)

!
!-----------------------------------------------------------------------
!
!     ASSEMBLE FINAL A AND B (INOUT)
!
      CALL AD_T1X_SET_BIEF_OBJ( A,VAL, A00,VAL, 1 )
      CALL AD_T1X_SET_BIEF_OBJ( A,DRV, A00,DRV, 1 )
!
      CALL AD_T1X_SET_BIEF_OBJ( B,VAL, B00,VAL, 1 )
      CALL AD_T1X_SET_BIEF_OBJ( B,DRV, B00,DRV, 1 )
!
!-----------------------------------------------------------------------
!
!     ASSEMBLE SOLUTION AND ITS TANGENTS
!
      CALL AD_T1X_SET_BIEF_OBJ( X,VAL, XS,VAL, 1 )
!
!      CALL AD_T1X_SET_INT_BIEF_OBJ( X,  1, 0, 1 )

      CALL OS( 'X=X+Y   ' , X=X1B, Y=X1A )
      CALL AD_T1X_SET_BIEF_OBJ( X,DRV, X1B,VAL, 1 )
!
!-----------------------------------------------------------------------
!
      RETURN

      CONTAINS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        RECURSIVE SUBROUTINE AD_T1X_SET_BIEF_OBJ
     &  ( TGT, TWHAT, SRC, SWHAT, RECLVL )
!
!note   TGT is allocated properly to SRC
!
!       MODULES -----------------------
!
!        USE BIEF
!        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!       ARGUMENTS ---------------------
!
        TYPE(BIEF_OBJ), INTENT(INOUT)   :: TGT
        TYPE(BIEF_OBJ), INTENT(IN)      :: SRC
        INTEGER       , INTENT(IN)      :: TWHAT, SWHAT, RECLVL
!
!       LOCAL VARS --------------------
!
        INTEGER :: I
!
!       FOR VECTORS -------------------
!
        IF ( ASSOCIATED( TGT%R ) ) THEN
          IF ( SWHAT .EQ. 0 ) THEN
            CALL DCO_T1S_SET( TGT%R,DCO_T1S_GET_VALUE(SRC%R),TWHAT)
          ELSEIF ( SWHAT .EQ. 1 ) THEN
            CALL DCO_T1S_SET( TGT%R,DCO_T1S_GET_DERIVATIVE(SRC%R),TWHAT)
          ENDIF
        ENDIF
!
!       FOR MATRICES ------------------
!
        IF ( TGT%TYPE .EQ. 3 ) THEN
!         TYPE(BIEF_OBJ), POINTER :: D
          IF ( ASSOCIATED( TGT%D ) ) THEN
            CALL AD_T1X_SET_BIEF_OBJ
     &           ( TGT%D, TWHAT, SRC%D, SWHAT, RECLVL+1 )
          ENDIF
!         TYPE(BIEF_OBJ), POINTER :: X
          IF ( ASSOCIATED( TGT%X ) ) THEN
            CALL AD_T1X_SET_BIEF_OBJ
     &           ( TGT%X, TWHAT, SRC%X, SWHAT, RECLVL+1 )
          ENDIF
        ENDIF
!
!       FOR BLOCKS --------------------
!
!       TYPE(POINTER_TO_BIEF_OBJ), POINTER, DIMENSION(:) :: ADR
        IF ( ASSOCIATED( TGT%ADR ) ) THEN
          DO I = 1,TGT%N
            CALL AD_T1X_SET_BIEF_OBJ
     &           ( TGT%ADR(I)%P, TWHAT, SRC%ADR(I)%P, SWHAT, RECLVL+1 )
          ENDDO
        ENDIF

        END SUBROUTINE AD_T1X_SET_BIEF_OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        RECURSIVE SUBROUTINE AD_T1X_SET_INT_BIEF_OBJ
     &  ( TGT, WHAT, VAL, RECLVL )
!
!note   TGT is allocated properly
!
!       MODULES -----------------------
!
!        USE BIEF
!        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!       ARGUMENTS ---------------------
!
        TYPE(BIEF_OBJ), INTENT(INOUT)   :: TGT
        INTEGER       , INTENT(IN)      :: VAL, WHAT, RECLVL
!
!       LOCAL VARS --------------------
!
        INTEGER :: I
!
!       FOR VECTORS -------------------
!
        IF ( ASSOCIATED( TGT%R ) ) THEN
          CALL DCO_T1S_SET( TGT%R, VAL, WHAT)
        ENDIF
!
!       FOR MATRICES ------------------
!
        IF ( TGT%TYPE .EQ. 3 ) THEN
!         TYPE(BIEF_OBJ), POINTER :: D
          IF ( ASSOCIATED( TGT%D ) ) THEN
            CALL AD_T1X_SET_INT_BIEF_OBJ( TGT%D, WHAT,VAL,RECLVL+1 )
          ENDIF
!         TYPE(BIEF_OBJ), POINTER :: X
          IF ( ASSOCIATED( TGT%X ) ) THEN
            CALL AD_T1X_SET_INT_BIEF_OBJ( TGT%X, WHAT,VAL,RECLVL+1 )
          ENDIF
        ENDIF
!
!       FOR BLOCKS --------------------
!
!       TYPE(POINTER_TO_BIEF_OBJ), POINTER, DIMENSION(:) :: ADR
        IF ( ASSOCIATED( TGT%ADR ) ) THEN
          DO I = 1, TGT%N
            CALL AD_T1X_SET_INT_BIEF_OBJ
     &           ( TGT%ADR(I)%P, WHAT, VAL, RECLVL+1 )
          ENDDO
        ENDIF
!
        END SUBROUTINE AD_T1X_SET_INT_BIEF_OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        RECURSIVE SUBROUTINE AD_DEEP_COPY_BIEF_OBJ
     &  ( TGT, SRC, SCNT, RECLVL )
!
!       MODULES -----------------------
!
!        USE BIEF
!        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!       ARGUMENTS ---------------------
!
        TYPE(BIEF_OBJ), INTENT(INOUT) :: TGT
        TYPE(BIEF_OBJ), INTENT(IN)    :: SRC
        INTEGER       , INTENT(IN)    :: SCNT, RECLVL
!
!       LOCAL VARS --------------------
!
        INTEGER :: I
!
!       HEADER COMMON TO ALL OBJECTS
!
        TGT%KEY = SRC%KEY
        TGT%TYPE = SRC%TYPE
        TGT%FATHER = SRC%NAME
        TGT%NAME = 'c'//SRC%NAME
!        TGT%NAME(6) = 'c'
!
!       FOR VECTORS
!
        TGT%NAT = SRC%NAT
        TGT%ELM = SRC%ELM
        TGT%DIM1 = SRC%DIM1
        TGT%MAXDIM1 = SRC%MAXDIM1
        TGT%DIM2 = SRC%DIM2
        TGT%MAXDIM2 = SRC%MAXDIM2
        TGT%DIMDISC = SRC%DIMDISC
        TGT%STATUS = SRC%STATUS
        TGT%TYPR = SRC%TYPR
        TGT%TYPI = SRC%TYPI
!
!       DOUBLE PRECISION, POINTER,DIMENSION(:)::R
        NULLIFY( TGT%R )
        IF ( ASSOCIATED( SRC%R ) ) THEN
          ALLOCATE( TGT%R( SIZE(SRC%R) ) )
          TGT%R = SRC%R
!          TGT%R =  AD_DEEP_COPY_REAL_ARRAY( SRC%R, SRC%NAME//"%R", TGT )
        ENDIF
!
!       INTEGER, POINTER,DIMENSION(:)::I
        NULLIFY( TGT%I )
        IF ( ASSOCIATED( SRC%I ) ) THEN
          ALLOCATE( TGT%I( SIZE(SRC%I) ) )
          TGT%I = SRC%I
        ENDIF
!
!       FOR MATRICES
!
        TGT%STO = SRC%STO
        TGT%STOX = SRC%STOX
        TGT%ELMLIN = SRC%ELMLIN
        TGT%ELMCOL = SRC%ELMCOL
        TGT%TYPDIA = SRC%TYPDIA
        TGT%TYPEXT = SRC%TYPEXT
!
!       TYPE(BIEF_OBJ), POINTER :: D
        NULLIFY( TGT%D )
        IF ( SRC%TYPE .EQ. 3 ) THEN
          IF ( ASSOCIATED( SRC%D ) ) THEN
            ALLOCATE(TGT%D)
            CALL AD_DEEP_COPY_BIEF_OBJ( TGT%D, SRC%D, SCNT, RECLVL+1 )
          ENDIF
        ENDIF
!
!       TYPE(BIEF_OBJ), POINTER :: X
        NULLIFY( TGT%X )
        IF ( SRC%TYPE .EQ. 3 ) THEN
          IF ( ASSOCIATED( SRC%X ) ) THEN
            ALLOCATE(TGT%X)
            CALL AD_DEEP_COPY_BIEF_OBJ( TGT%X, SRC%X, SCNT, RECLVL+1  )
          ENDIF
        ENDIF
        TGT%PRO = SRC%PRO
!
!       FOR BLOCKS
!
        TGT%N = SRC%N
        TGT%MAXBLOCK = SRC%MAXBLOCK
!       TYPE(POINTER_TO_BIEF_OBJ), POINTER, DIMENSION(:) :: ADR
        IF ( ASSOCIATED( SRC%ADR ) ) THEN
          ALLOCATE( TGT%ADR( TGT%MAXBLOCK ) )
          DO I = 1, TGT%N
            ALLOCATE( TGT%ADR(I)%P )
            CALL AD_DEEP_COPY_BIEF_OBJ(TGT%ADR(I)%P,SRC%ADR(I)%P,
     &           SCNT, RECLVL+1  )
          ENDDO
          DO I = TGT%N+1, TGT%MAXBLOCK
            NULLIFY( TGT%ADR(I)%P )
          ENDDO
        ENDIF
!
        END SUBROUTINE  AD_DEEP_COPY_BIEF_OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        RECURSIVE SUBROUTINE AD_PRINT_BIEF_OBJ
     &  ( TGT, SCNT, RECLVL, MSG, FIL )
!
!       MODULES -----------------------
!
!        USE BIEF
!        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!       ARGUMENTS ---------------------
!
        TYPE(BIEF_OBJ), INTENT(IN)    :: TGT
        INTEGER       , INTENT(IN)    :: SCNT, RECLVL
        CHARACTER(LEN=*)              :: MSG, FIL
!
!       LOCAL VARS --------------------
!
        INTEGER                       :: I
        INTEGER, SAVE                 :: CNT = 0
!  /!\  HOW IS SPC ALLOCATED ?
        CHARACTER(LEN=2*RECLVL+2)     :: SPC
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TMPA
!
        SPC = ' '
        DO I=1,RECLVL-1
          SPC = SPC//'  '
        ENDDO
        IF( RECLVL .EQ. 1 ) THEN
          CNT = CNT + 1
          WRITE(LU,*)
          WRITE(LU,*)
     &      '========================  START  ',SCNT, '::',CNT,'  ==='
          WRITE(LU,*)'  AD_PRINT_BIEF_OBJ  :: ', MSG
          WRITE(LU,*)'======================================='
        ENDIF
!
!       HEADER COMMON TO ALL OBJECTS
!
        WRITE(LU,*) SPC,'(G)','  KEY ',TGT%KEY,' TYPE ',TGT%TYPE,
     &    ' FATHER::',TGT%FATHER,'::   NAME::',TGT%NAME,'::'
!
!       FOR VECTORS
!
        WRITE(LU,*) SPC,'(V)','  NAT', TGT%NAT, '   ELM',TGT%ELM,
     &     '  DIM1',TGT%DIM1, '  MAXDIM1', TGT%MAXDIM1,
     &     '  DIM2', TGT%DIM2,'  MAXDIM2', TGT%MAXDIM2,
     &     '  DIMDISC', TGT%DIMDISC, '  STATUS ', TGT%STATUS
        IF ( ASSOCIATED(TGT%R) .AND. TGT%TYPE .EQ. 2 ) THEN
#ifndef COMPAD_DCO_T1S
          DO I=1,SIZE(TGT%R)
            WRITE(LU,*) SPC,'  (V)',I,':', TGT%R(I)
          ENDDO
#else
          ALLOCATE( TMPA(SIZE(TGT%R)))
          CALL DCO_T1S_GET( TGT%R, TMPA, 1 )
          DO I=1,SIZE(TGT%R)
            WRITE(LU,*) SPC,'  (V)',I,':', TGT%R(I),'  T1S', TMPA(I)
          ENDDO
          DEALLOCATE(TMPA)
#endif
        ENDIF
!
!       FOR MATRICES
!
        WRITE(LU,*) SPC,'(M)','  STO', TGT%STO,'  STOX', TGT%STOX,
     &     '  ELMLIN', TGT%ELMLIN,'  ELMCOL', TGT%ELMCOL
        IF ( TGT%TYPE .EQ. 3 ) THEN
          IF ( ASSOCIATED(TGT%D) ) THEN
            CALL AD_PRINT_BIEF_OBJ( TGT%D, SCNT,RECLVL+1,"","" )
          ENDIF
          IF ( ASSOCIATED(TGT%X) ) THEN
            CALL AD_PRINT_BIEF_OBJ( TGT%X, SCNT,RECLVL+1,"","" )
          ENDIF
        ENDIF
!
!       FOR BLOCKS
!
        WRITE(LU,*) SPC,'(B)','  N', TGT%N,'  MAXBLOCK', TGT%MAXBLOCK
        IF ( TGT%TYPE .EQ. 4 ) THEN
          IF ( ASSOCIATED(TGT%ADR) ) THEN
            DO I = 1, TGT%N
              CALL AD_PRINT_BIEF_OBJ( TGT%ADR(I)%P,SCNT,RECLVL+1,"","" )
            ENDDO
          ENDIF
        ENDIF
!
        IF ( RECLVL.EQ.1 ) THEN
          WRITE(LU,*)
     &      '=======================   END   ',SCNT, '::',CNT,'  ==='
        ENDIF
!
        END SUBROUTINE AD_PRINT_BIEF_OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        RECURSIVE SUBROUTINE AD_DIFF_INT_PRINT_BIEF_OBJ
     &  ( TGTA,WHTA, PVAL, SCNT, RECLVL, MSG, FIL )
!
!       MODULES -----------------------
!
!        USE BIEF
!        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!       ARGUMENTS ---------------------
!
        TYPE(BIEF_OBJ), INTENT(IN)    :: TGTA
        INTEGER       , INTENT(IN)    :: WHTA, PVAL, SCNT, RECLVL
        CHARACTER(LEN=*)              :: MSG, FIL
!
!       LOCAL VARS --------------------
!
        INTEGER                       :: I
        INTEGER, SAVE                 :: CNT = 0
!  /!\  HOW IS SPC ALLOCATED ?
        CHARACTER(LEN=2*RECLVL+2)     :: SPC
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TMPA, TMPB
        LOGICAL                       :: DIFF
!
        SPC = ' '
        DO I=1,RECLVL-1
          SPC = SPC//'  '
        ENDDO
        IF( RECLVL .EQ. 1) THEN
          CNT = CNT + 1
          WRITE(LU,*)
          WRITE(LU,*)
     &      '========================  START  ',SCNT, '::', CNT,'  ==='
          WRITE(LU,*)'  AD_DIFF_INT_PRINT_BIEF_OBJ  :: ', MSG
          WRITE(LU,*)'    COMPARING :: ', TGTA%NAME,'  with value ',
     &           PVAL, '  what: ', WHTA
          WRITE(LU,*)'======================================='
        ENDIF
!
!       HEADER COMMON TO ALL OBJECTS
!
        WRITE(LU,*) SPC,'(G) TGTA','  KEY ',TGTA%KEY,
     &    ' TYPE ',TGTA%TYPE,' FATHER::',TGTA%FATHER,
     &    '::   NAME::',TGTA%NAME,'::'
!
!       FOR VECTORS
!
        WRITE(LU,*) SPC,'(V) TGTA ','  NAT', TGTA%NAT,
     &    '   ELM',TGTA%ELM,
     &    '  DIM1',TGTA%DIM1, '  MAXDIM1', TGTA%MAXDIM1,
     &    '  DIM2', TGTA%DIM2,'  MAXDIM2', TGTA%MAXDIM2,
     &    '  DIMDISC', TGTA%DIMDISC, '  STATUS ', TGTA%STATUS
        IF ( ASSOCIATED(TGTA%R) .AND. 2 == TGTA%TYPE) THEN
#ifndef  COMPAD_DCO_T1S
          DO I=1,SIZE(TGTA%R)
            IF ( TGTA%R(I) .NE. PVAL )
     &        WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &          ' BIEFDIFF VALUE:',TGTA%R(I), '  <--> ', PVAL
          ENDDO
#else
          ALLOCATE( TMPA(SIZE(TGTA%R)) )
          CALL DCO_T1S_GET( TGTA%R, TMPA, 1 )
          DO I=1,SIZE(TGTA%R)
            IF ( WHTA .EQ. 0 ) THEN
              IF ( TGTA%R(I) .NE. PVAL )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF VAL/PVAL :',TGTA%R(I), '  <--> ',  PVAL
            ELSEIF ( WHTA .EQ. 1 ) THEN
              IF ( TMPA(I) .NE. PVAL )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF T1S/PVAL :',TMPA(I), '  <--> ',  PVAL
            ELSE
              IF ( TGTA%R(I) .NE. PVAL )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF VAL/PVAL :',TGTA%R(I), '  <--> ',  PVAL
            ENDIF
          ENDDO
          DEALLOCATE(TMPA)
#endif
        ENDIF
!
!       FOR MATRICES
!
        WRITE(LU,*) SPC,'(M)  TGTA','  STO', TGTA%STO,
     &    '  STOX', TGTA%STOX,
     &    '  ELMLIN', TGTA%ELMLIN,'  ELMCOL', TGTA%ELMCOL
        IF ( TGTA%TYPE .EQ. 3 ) THEN
          IF ( ASSOCIATED(TGTA%D) ) THEN
            CALL AD_DIFF_INT_PRINT_BIEF_OBJ( TGTA%D,WHTA, PVAL,
     &           SCNT, RECLVL+1,MSG,FIL )
          ENDIF
          IF ( ASSOCIATED(TGTA%X) ) THEN
            CALL AD_DIFF_INT_PRINT_BIEF_OBJ( TGTA%X,WHTA, PVAL,
     &           SCNT, RECLVL+1,MSG,FIL )
          ENDIF
        ENDIF
!
!       FOR BLOCKS
!
        WRITE(LU,*) SPC,'(B)  TGTA','  N', TGTA%N,
     &    '  MAX BLOCK', TGTA%MAXBLOCK
        IF ( TGTA%TYPE .EQ. 4 ) THEN
          IF ( ASSOCIATED(TGTA%ADR) ) THEN
            DO I = 1, TGTA%N
              CALL AD_DIFF_INT_PRINT_BIEF_OBJ( TGTA%ADR(I)%P, PVAL,
     &             WHTA,SCNT, RECLVL+1,MSG,FIL )
            ENDDO
          ENDIF
        ENDIF
!
        IF ( RECLVL .EQ. 1 ) THEN
          WRITE(LU,*)'=======================   END   ',
     &      SCNT, '::', CNT,'  ==='
        ENDIF
!
        END SUBROUTINE AD_DIFF_INT_PRINT_BIEF_OBJ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
        RECURSIVE SUBROUTINE AD_DIFF_PRINT_BIEF_OBJ
     &  ( TGTA, WHTA, TGTB, WHTB, SCNT, RECLVL, MSG, FIL )
!
!       MODULES -----------------------
!
!        USE BIEF
!        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!       ARGUMENTS ---------------------
!
        TYPE(BIEF_OBJ), INTENT(IN)    :: TGTA, TGTB
        INTEGER       , INTENT(IN)    :: WHTA, WHTB, SCNT, RECLVL
        CHARACTER(LEN=*)              :: MSG, FIL
!
!       LOCAL VARS --------------------
!
        INTEGER                       :: I
        INTEGER, SAVE                 :: CNT = 0
!  /!\  HOW IS SPC ALLOCATED ?
        CHARACTER(LEN=2*RECLVL+2)     :: SPC
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TMPA, TMPB
        LOGICAL                       :: DIFF
!
        SPC = ' '
        DO I=1,RECLVL-1
          SPC = SPC//'  '
        ENDDO
        IF( RECLVL .EQ. 1 ) THEN
          CNT = CNT + 1
          WRITE(LU,*)
          WRITE(LU,*)
     &      '========================  START ',SCNT, '::', CNT,'  ==='
          WRITE(LU,*)'  AD_DIFF_PRINT_BIEF_OBJ  :: ', MSG
          WRITE(LU,*)'    COMPARING :: ', TGTA%NAME, ' with ',
     &         TGTB%NAME, '   what: ', WHTA, WHTB
          WRITE(LU,*)'======================================='
        ENDIF
!
!       HEADER COMMON TO ALL OBJECTS
!
        WRITE(LU,*) SPC,'(G) TGTA','  KEY ',TGTA%KEY,
     &     ' TYPE ',TGTA%TYPE,
     &     ' FATHER::',TGTA%FATHER,'::   NAME::',TGTA%NAME,'::'
        WRITE(LU,*) SPC,'(G) TGTB','  KEY ',TGTB%KEY,
     &     ' TYPE ',TGTB%TYPE,
     &     ' FATHER::',TGTB%FATHER,'::   NAME::',TGTB%NAME,'::'
        WRITE(LU,'(1x,A,A)',ADVANCE='NO') SPC,'(G)    CHECK :::  '
        IF ( TGTA%KEY .NE. TGTB%KEY   )
     &    WRITE(LU,'(A)',ADVANCE='NO') 'KEY'
        IF ( TGTA%TYPE .NE. TGTB%TYPE  )
     &    WRITE(LU,'(A)',ADVANCE='NO') 'TYPE'
        DIFF = TGTA%KEY .NE. TGTB%KEY
        DIFF = DIFF .AND.  ( TGTA%TYPE   .NE. TGTB%TYPE  )
        IF ( DIFF ) WRITE(LU,*) '  BIEFDIFF'
        WRITE(LU,*)
!
!       FOR VECTORS
!
        WRITE(LU,*) SPC,'(V) TGTA ','  NAT', TGTA%NAT,
     &     '  ELM',TGTA%ELM,
     &     '  DIM1',TGTA%DIM1, '  MAXDIM1', TGTA%MAXDIM1,
     &     '  DIM2', TGTA%DIM2,'  MAXDIM2', TGTA%MAXDIM2,
     &     '  DIMDISC', TGTA%DIMDISC, '  STATUS ', TGTA%STATUS
        WRITE(LU,*) SPC,'(V) TGTB ','  NAT', TGTB%NAT,
     &     '  ELM',TGTB%ELM,
     &     '  DIM1',TGTB%DIM1, '  MAXDIM1', TGTB%MAXDIM1,
     &     '  DIM2', TGTB%DIM2,'  MAXDIM2', TGTB%MAXDIM2,
     &     '  DIMDISC', TGTB%DIMDISC, '  STATUS ', TGTB%STATUS
        WRITE(LU,'(1x,A,A)',ADVANCE='NO') SPC,'(V)    CHECK :::  '
        IF ( TGTA%NAT .NE. TGTB%NAT )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' NAT  '
        IF ( TGTA%ELM .NE. TGTB%ELM )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' ELM  '
        IF ( TGTA%DIM1 .NE. TGTB%DIM1 )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' DIM1  '
        IF ( TGTA%MAXDIM1 .NE. TGTB%MAXDIM1 )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' MAXDIM1  '
        IF ( TGTA%DIM2 .NE. TGTB%DIM2 )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' DIM2  '
        IF ( TGTA%MAXDIM2 .NE. TGTB%MAXDIM2 )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' MAXDIM2  '
        IF ( TGTA%DIMDISC .NE. TGTB%DIMDISC )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' DIMDISC  '
        IF ( TGTA%STATUS .NE. TGTB%STATUS )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' STATUS  '
        IF ( TGTA%TYPR .NE. TGTB%TYPR )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' TYPR  '
        IF ( TGTA%TYPI .NE. TGTB%TYPI )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' TYPI  '
        DIFF = TGTA%ELM .NE. TGTB%ELM
        DIFF = DIFF .AND. ( TGTA%DIM1 .NE. TGTB%DIM1 )
        DIFF = DIFF .AND. ( TGTA%MAXDIM1 .NE. TGTB%MAXDIM1 )
        DIFF = DIFF .AND. ( TGTA%DIM2 .NE. TGTB%DIM2 )
        DIFF = DIFF .AND. ( TGTA%MAXDIM2 .NE. TGTB%MAXDIM2 )
        DIFF = DIFF .AND. ( TGTA%DIMDISC .NE. TGTB%DIMDISC  )
        DIFF = DIFF .AND. ( TGTA%STATUS .NE. TGTB%STATUS  )
        DIFF = DIFF .AND. ( TGTA%TYPR .NE. TGTB%TYPR  )
        DIFF = DIFF .AND. ( TGTA%TYPI .NE. TGTB%TYPI  )
        IF ( DIFF ) WRITE(LU,*) '  BIEFDIFF'
        WRITE(LU,*)

        IF ( ASSOCIATED(TGTA%R) .AND. 2 == TGTA%TYPE) THEN
#ifndef  COMPAD_DCO_T1S
          DO I=1,SIZE(TGTA%R)
            IF ( TGTA%R(I) .NE. TGTB%R(I) )
     &        WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &          ' BIEFDIFF VALUE:', TGTA%R(I), '  <--> ', TGTB%R(I)
          ENDDO
#else
          ALLOCATE( TMPA(SIZE(TGTA%R)), TMPB(SIZE(TGTB%R)) )
          CALL DCO_T1S_GET( TGTA%R, TMPA, 1 )
          CALL DCO_T1S_GET( TGTB%R, TMPB, 1 )
          DO I=1,SIZE(TGTA%R)
            IF ( WHTA .EQ. 0 .AND. WHTB .EQ. 0 ) THEN
              IF ( TGTA%R(I) .NE. TGTB%R(I) )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF VAL/VAL :',TGTA%R(I),'  <--> ',TGTB%R(I)
            ELSEIF ( WHTA .EQ. 0 .AND. WHTB .EQ. 1 ) THEN
              IF ( TGTA%R(I) .NE. TMPB(I) )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF VAL/T1S :',TGTA%R(I),'  <--> ',TMPB(I)
            ELSEIF ( WHTA .EQ. 1 .AND. WHTB .EQ. 0 ) THEN
              IF ( TMPA(I) .NE. TGTB%R(I) )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF T1S/VAL :',TMPA(I),'  <--> ',TGTB%R(I)
            ELSEIF ( WHTA .EQ. 1 .AND. WHTB .EQ. 1 ) THEN
              IF ( TMPA(I) .NE. TMPB(I) )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF T1S/T1S :',TMPA(I),'  <--> ',TGTB%R(I)
            ELSE
              IF ( TGTA%R(I) .NE. TGTB%R(I) )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF VAL/VAL :',TGTA%R(I),'  <--> ',TGTB%R(I)
              IF ( TMPA(I) .NE. TMPB(I) )
     &          WRITE(LU,*) SPC,'  (V)',I,'(',MSG,',',SCNT,')',
     &            ' BIEFDIFF T1S   :',TMPA(I),'  <--> ',TMPB(I)
            ENDIF
          ENDDO
          DEALLOCATE(TMPA,TMPB)
#endif
        ENDIF
!
!       FOR MATRICES
!
        WRITE(LU,*) SPC,'(M)  TGTA','  STO', TGTA%STO,
     &     '  STOX', TGTA%STOX,
     &     '  ELMLIN', TGTA%ELMLIN,'  ELMCOL', TGTA%ELMCOL
        WRITE(LU,*) SPC,'(M)  TGTB','  STO', TGTB%STO,
     &     '  STOX', TGTB%STOX,
     &     '  ELMLIN', TGTB%ELMLIN,'  ELMCOL', TGTB%ELMCOL
        WRITE(LU,'(1x,A,A)',ADVANCE='NO') SPC,'(M)     CHECK  :::  '
        IF ( TGTA%STO .NE. TGTB%STO )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' STO  '
        IF ( TGTA%STOX    .NE. TGTB%STOX   )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' STOX  '
        IF ( TGTA%ELMLIN    .NE. TGTB%ELMLIN   )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' ELMLIN  '
        IF ( TGTA%ELMCOL    .NE. TGTB%ELMCOL   )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' ELMCOL  '
        IF ( TGTA%TYPDIA    .NE. TGTB%TYPDIA   )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' TYPDIA  '
        IF ( TGTA%TYPEXT    .NE. TGTB%TYPEXT   )
     &    WRITE(LU,'(A)',ADVANCE='NO') ' TYPEXT  '
        WRITE(LU,*)
        DIFF = TGTA%STO .NE. TGTB%STO
        DIFF = DIFF .AND.  ( TGTA%STOX  .NE. TGTB%STOX )
        DIFF = DIFF .AND.  ( TGTA%ELMLIN.NE. TGTB%ELMLIN)
        DIFF = DIFF .AND.  ( TGTA%ELMCOL.NE. TGTB%ELMCOL)
        DIFF = DIFF .AND.  ( TGTA%DIM2  .NE. TGTB%DIM2 )
        DIFF = DIFF .AND.  ( TGTA%TYPDIA.NE. TGTB%TYPDIA)
        DIFF = DIFF .AND.  ( TGTA%TYPEXT.NE. TGTB%TYPEXT )
        IF ( DIFF ) WRITE(LU,*) '  BIEFDIFF'
        IF ( TGTA%TYPE .EQ. 3 ) THEN
          IF ( ASSOCIATED(TGTA%D) ) THEN
            WRITE(LU,*) SPC,'(M)  DIFFING NOW    TGTA%D  TGTB%D'
            CALL AD_DIFF_PRINT_BIEF_OBJ( TGTA%D,WHTA, TGTB%D,WHTB,
     &           SCNT, RECLVL+1,MSG,FIL )
          ENDIF
          IF ( ASSOCIATED(TGTA%X) ) THEN
            WRITE(LU,*) SPC,'(M)  DIFFING NOW   TGTA%X  TGTB%X'
            CALL AD_DIFF_PRINT_BIEF_OBJ( TGTA%X,WHTA, TGTB%X,WHTB,
     &           SCNT, RECLVL+1,MSG,FIL )
          ENDIF
        ENDIF
!
!       FOR BLOCKS
!
        WRITE(LU,*) SPC,'(B)  TGTA','  N', TGTA%N,
     &    '  MAX BLOCK', TGTA%MAXBLOCK
        WRITE(LU,*) SPC,'(B)  TGTB','  N', TGTB%N,
     &    '  MAXBLOCK', TGTB%MAXBLOCK
        WRITE(LU,'(1x,A,A)',ADVANCE='NO') SPC,'(B)    CHECK  :::  '
        IF ( TGTA%N .NE. TGTB%N ) WRITE(LU,'(A)',ADVANCE='NO') ' N  '
        IF ( TGTA%MAXBLOCK .NE. TGTB%MAXBLOCK )
     &     WRITE(LU,'(A)',ADVANCE='NO') ' MAXBLOCK  '
        DIFF = TGTA%N .NE. TGTB%N
        DIFF = DIFF .AND. ( TGTA%MAXBLOCK.NE.TGTB%MAXBLOCK )
        IF ( DIFF ) WRITE(LU,*) '  BIEFDIFF'
        WRITE(LU,*)
        IF ( TGTA%TYPE .EQ. 4 ) THEN
          IF ( ASSOCIATED(TGTA%ADR) ) THEN
            DO I = 1, TGTA%N
              WRITE(LU,*) SPC,'(M)  DIFFING NOW   BLOCKS ',
     &          'TGTA%ADR(',i,')%P    TGTB%ADR(',i,')%P'
              CALL  AD_DIFF_PRINT_BIEF_OBJ(TGTA%ADR(I)%P,WHTA,
     &          TGTB%ADR(I)%P, WHTB, SCNT, RECLVL+1,MSG,FIL )
            ENDDO
          ENDIF
        ENDIF
!
        IF ( 1 .EQ. RECLVL ) THEN
          WRITE(LU,*)
     &      '=======================   END    ',SCNT, '::', CNT,'  ==='
        ENDIF
!
        END SUBROUTINE AD_DIFF_PRINT_BIEF_OBJ
!
#endif /* COMPAD_SYMBOLIC_LINSOLVE .AND. COMPAD */
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      END
