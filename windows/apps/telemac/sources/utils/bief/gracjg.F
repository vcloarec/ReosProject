!                   *****************
                    SUBROUTINE GRACJG
!                   *****************
!
     &(X, A,B , MESH, D,AD,G,R, CFG,INFOGR,AUX)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    SOLVES THE LINEAR SYSTEM A X = B
!+                USING THE CONJUGATE GRADIENT METHOD.
!code
!+-----------------------------------------------------------------------
!+                        PRECONDITIONING
!+-----------------------------------------------------------------------
!+   CFG%PRECON VALUE  I                  MEANING
!+-----------------------------------------------------------------------
!+                     I
!+        0 OR 1       I  NO PRECONDITIONING
!+                     I
!+        2            I  DIAGONAL PRECONDITIONING USING THE MATRIX
!+                     I  DIAGONAL
!+        3            I  BLOCK-DIAGONAL PRECONDITIONING
!+                     I
!+        5            I  DIAGONAL PRECONDITIONING USING THE ABSOLUTE
!+                     I  VALUE OF THE MATRIX DIAGONAL
!+                     I
!+        7            I  CROUT EBE PRECONDITIONING
!+                     I
!+       11            I  GAUSS-SEIDEL EBE PRECONDITIONING
!+                     I
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (LNH)
!+        06/10/08
!+        V5P9
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   CALL PARMOY removed, and stop if Crout preconditionning is asked
!+   in parallel.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX OF THE SYSTEM
!| AD             |<->| WORK ARRAY: MATRICE A MULTIPLIED BY D.
!| AUX            |-->| MATRIX FOR PRECONDITIONING.
!| B              |-->| RIGHT-HAND SIDE OF THE SYSTEM
!| CFG            |-->| STRUCTURE OF SOLVER CONFIGURATION
!| D              |<->| WORK ARRAY: DIRECTION OF DESCENT.
!| G              |<->| DESCENT GRADIENT.
!| INFOGR         |-->| IF YES, PRINT A LOG.
!| MESH           |-->| MESH STRUCTURE.
!| R              |<->| RESIDUAL (MAY BE IN THE SAME MEMORY SPACE AS
!|                |   | GRADIENT DEPENDING ON CONDITIONING)
!| X              |<->| INITIAL VALUE, THEN SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC, ONLY : GRACJG_CNT
      USE BIEF, EX_GRACJG => GRACJG
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SLVCFG), INTENT(INOUT)    :: CFG
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: B
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: D,AD,G,R,X
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: AUX
      LOGICAL, INTENT(IN)            :: INFOGR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER M
!
      DOUBLE PRECISION XL,RMRM,RMDM,RMGM,TESTL
      DOUBLE PRECISION BETA,RO,DAD,RM1GM1,STO1,C
!
      LOGICAL RELAT,PREC,CROUT,GSEB,PRE3D,PREBE
!
      DOUBLE PRECISION, PARAMETER :: RMIN = 1.D-15
!
!-----------------------------------------------------------------------
!
      LOGICAL           :: FINISH
!
#if defined COMPAD_DCO_T1S || COMPAD_DCO_T1V
      INTEGER           :: DRV_ELMS
      DOUBLE PRECISION  :: DRV_ERR
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE  :: DRV0, DRV1
#endif
#if defined COMPAD_DCO_T1V
      INTEGER           :: T1V_NOD,I
#endif
!
!-----------------------------------------------------------------------
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!   INITIALISES
!
      GRACJG_CNT = GRACJG_CNT + 1
#if defined COMPAD_DCO_T1S || COMPAD_DCO_T1V
!      WRITE(LU,*) 'AD LINEAR SOLVER RESET DERIVATIVES :: ',
!     &     AD_LINSOLV_RESETDERIV
!      WRITE(LU,*) 'AD LINEAR SOLVER DERIVATIVE CONVERGENCE :: ',
!     &     AD_LINSOLV_DERIVATIVE_CONVERGENCE
!
! 1.  RESET INCOMING TANGENTS TO ZERO TO AVOID DEPENDENCY OF OVERALL
!     TANGENTS FROM INITIAL SOLUTIONS FOR FIRST CONJUGADE GRADIENT
!! COMPAD-DCO  BEGIN  JR2016
      IF (AD_LINSOLV_RESETDERIV ) THEN
#  if defined COMPAD_DCO_T1S
        CALL DCO_T1S_SET( X%ADR(1)%P%R, 0.D0, 1 )
#  endif
#  if defined COMPAD_DCO_T1V
        T1V_NOD = DCO_T1V_GET_NOD()
!        WRITE(LU,*) 'T1V  NOD  :: ',t1V_NOD
        DO I=1, T1V_NOD
          CALL DCO_T1V_SET( X%ADR(1)%P%R, 0.D0, 1, I )
        END DO
#  endif
      END IF
!! COMPAD-DCO  original code
!!      CALL DCO_T1S_SET( X%ADR(1)%P%R, 0.D0, 1 )
!! COMPAD-DCO  END    JR2016
!     BY SETTING X TO A FIXED STARTING VALUE WE DISABLE THE RESTART
!     FROM THE PREVISOU CG SOLUTION, AND SETTING TANGENTS TO ZERO TOO
!>    X%ADR(1)%P%R = -1.1D0
!
! 2.  PREPARE STORAGE OF GRADIENTS FOR TERMINATION TEST
      DRV_ELMS = X%ADR(1)%P%DIM1
      ALLOCATE( DRV0(DRV_ELMS), DRV1(DRV_ELMS))
!
! 3.  STORE TANGENT OF X0
#  if defined COMPAD_DCO_T1S
      CALL DCO_T1S_GET( X%ADR(1)%P%R, DRV0, 1 )
#  endif
#  if defined COMPAD_DCO_T1V
      CALL DCO_T1V_GET( X%ADR(1)%P%R, DRV0, 1, 1 )
#  endif
!
#endif
!
!
      STO1 = 0.D0
      CROUT =.FALSE.
      IF(07*(CFG%PRECON/07).EQ.CFG%PRECON) CROUT=.TRUE.
      GSEB=.FALSE.
      IF(11*(CFG%PRECON/11).EQ.CFG%PRECON) GSEB=.TRUE.
      PREBE=.FALSE.
      IF(13*(CFG%PRECON/13).EQ.CFG%PRECON) PREBE=.TRUE.
      PRE3D=.FALSE.
      IF(17*(CFG%PRECON/17).EQ.CFG%PRECON) PRE3D=.TRUE.
      PREC=.FALSE.
      IF(CROUT.OR.GSEB.OR.PREBE.OR.PRE3D) PREC=.TRUE.
!
!-----------------------------------------------------------------------
!   INITIALISES
!-----------------------------------------------------------------------
!
      M   = 0
!
!  NORMALISES THE SECOND MEMBER TO COMPUTE THE RELATIVE PRECISION:
!
      XL = P_DOTS(B,B,MESH)
!
!     OLIVIER BOITEAU'S (SINETICS) TEST : SECOND MEMBER TOO SMALL
!                                         ==> UNKNOWN = 0
!
      TESTL=SQRT(XL)
      IF(TESTL.LT.RMIN) THEN
        CALL OS('X=0     ',X=X)
        IF(INFOGR) THEN
          WRITE(LU,106) TESTL
        ENDIF
        GOTO 1000
      ENDIF
!
      IF(XL.LT.1.D0) THEN
        XL = 1.D0
        RELAT = .FALSE.
      ELSE
        RELAT = .TRUE.
      ENDIF
!
! COMPUTES THE INITIAL RESIDUAL AND POSSIBLY EXITS:
!
      CALL MATRBL( 'X=AY    ',R,A,X,  C,MESH)
!
      CALL OS('X=X-Y   ', X=R, Y=B)
      RMRM   = P_DOTS(R,R,MESH)
      RMGM   = RMRM
!
      IF (RMRM.LT.CFG%EPS**2*XL) GO TO 900
!
!-----------------------------------------------------------------------
! PRECONDITIONING :
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
!       COMPUTES C G0 = R0
        IF(CROUT.OR.GSEB.OR.PREBE) THEN
          IF(NCSIZE.GT.1) THEN
            WRITE(LU,*) 'NO CROUT PRECONDITIONNING IN PARALLEL'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL DOWNUP( G, AUX , R , 'D' , MESH )
        ELSEIF(PRE3D) THEN
          CALL CPSTVC(R%ADR(1)%P,G%ADR(1)%P)
          CALL TRID3D(AUX%X%R,G%ADR(1)%P%R,R%ADR(1)%P%R,
     &                MESH%NPOIN,BIEF_NBPTS(11,MESH))
        ENDIF
!       COMPUTES RMGM AND STORES
        RMGM = P_DOTS(R,G,MESH)
        STO1 = RMGM
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTES THE DIRECTION OF INITIAL DESCENT:
!-----------------------------------------------------------------------
!
      CALL OS('X=Y     ', X=D, Y=G)
!
!-----------------------------------------------------------------------
! COMPUTES THE INITIAL PRODUCT A D :
!-----------------------------------------------------------------------
!
      CALL MATRBL( 'X=AY    ',AD,A,D,C,MESH)
!
!-----------------------------------------------------------------------
! COMPUTES INITIAL RO :
!-----------------------------------------------------------------------
!
      DAD = P_DOTS(D,AD,MESH)
      RO = RMGM / DAD
!     USES RMGM BECAUSE HERE D0=G0
!
!-----------------------------------------------------------------------
!
! COMPUTES X1 = X0 - RO  * D
!
      CALL OS('X=X+CY  ',X=X,Y=D,C=-RO)
!
!     ALGORITHMIC DIFFERENTIATION
!     TAKE TANGENT OF X1 IF WE ARE IN DCO_T1S MODE AND STORE IT
#if defined COMPAD_DCO_T1S
      CALL DCO_T1S_GET( X%ADR(1)%P%R, DRV1, 1 )
#endif
#if defined COMPAD_DCO_T1V
      CALL DCO_T1V_GET( X%ADR(1)%P%R, DRV1, 1, 1 )
#endif
!
!-----------------------------------------------------------------------
!  ITERATIONS LOOP:
!-----------------------------------------------------------------------
!
2     M  = M  + 1
!
!-----------------------------------------------------------------------
! COMPUTES THE RESIDUAL : R(M) = R(M-1) - RO(M-1) A D(M-1)
!-----------------------------------------------------------------------
!
      CALL OS('X=X+CY  ',X=R,Y=AD,C=-RO)
!
!  SOME VALUES WILL CHANGE IN CASE OF PRECONDITIONING
!
      RM1GM1 = RMGM
      RMRM   = P_DOTS(R,R,MESH)
      RMDM   = RMRM
      RMGM   = RMRM
!
! CHECKS END:
!
!      IF(RMRM.LE.XL*CFG%EPS**2) GO TO 900
      FINISH = RMRM.LE.XL*CFG%EPS**2
#if defined COMPAD_DCO_T1S || COMPAD_DCO_T1V
!     CHECK DIFFERENCE BETWEEN TANGENTS OF LAST TWO STEPS
! 1.  SET TO ZERO TO AVOID ADDTIONAL TEST
      DRV_ERR = 0.D0
! 2a. ABSOLUTE CHANGE TO LAST STEP
      DRV_ERR  = SUM( DRV1 - DRV0 )**2
! 2b. RELATIVE CHANGE TO LAST STEP
!      DRV_ERR = SUM( DRV0*DRV0 )
!      IF ( DRV_ERR .GT. 0.0 ) THEN
!        DRV_ERR  = SUM( (DRV1 - DRV0 )**2 ) / DRV_ERR
!      ENDIF
! 3.  TEST CONVERGENCE
      IF ( AD_LINSOLV_DERIVATIVE_CONVERGENCE ) THEN
        FINISH = FINISH .AND. (DRV_ERR .LE. CFG%EPS**2 )
      ENDIF
!! COMPAD-DCO  original code
!!      FINISH = FINISH .AND. (DRV_ERR .LE. CFG%EPS**2 )
!#if defined COMPAD_DCO_T1S || COMPAD_DCO_T1V
!      WRITE(LU,*) '  CG  CONVTEST',CNT,M,FINISH,RMRM,' :: ',
!     &     SQRT(DRV_ERR),(DRV_ERR.LE.CFG%EPS**2), SQRT(SUM(DRV1**2)),
!     &     SQRT(SUM(DRV0**2))
!#endif
#endif
      IF ( FINISH ) GO TO 900
!
!-----------------------------------------------------------------------
! PRECONDITIONING : SOLVES C G = R
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
!
!       SOLVES C G = R
        IF(CROUT.OR.GSEB.OR.PREBE) THEN
          IF(NCSIZE.GT.1) THEN
            WRITE(LU,*) 'NO CROUT PRECONDITIONNING IN PARALLEL'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL DOWNUP( G, AUX , R , 'D' , MESH )
        ELSEIF(PRE3D) THEN
          CALL CPSTVC(R%ADR(1)%P,G%ADR(1)%P)
          CALL TRID3D(AUX%X%R,G%ADR(1)%P%R,R%ADR(1)%P%R,
     &                MESH%NPOIN,BIEF_NBPTS(11,MESH))
        ENDIF
!       COMPUTES RMGM AND RM1GM1
        RM1GM1 = STO1
        RMGM = P_DOTS(R,G,MESH)
        STO1=RMGM
!
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTES D BY RECURRENCE:
!-----------------------------------------------------------------------
!
      BETA = RMGM / RM1GM1
!     OPTIMISED EQUIVALENT OF THE 2 OS CALLS:
      CALL OS( 'X=Y+CZ  ' , X=D , Y=G , Z=D , C=BETA )
!
!-----------------------------------------------------------------------
! PRECONDITIONING :
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
!       COMPUTES RMDM
        RMDM=P_DOTS(R,D,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTES A D :
!-----------------------------------------------------------------------
!
      CALL MATRBL( 'X=AY    ',AD,A,D,C,MESH)
!
!-----------------------------------------------------------------------
! COMPUTES RO
!-----------------------------------------------------------------------
!
      DAD = P_DOTS(D,AD,MESH)
      RO = RMDM/DAD
!
! COMPUTES X(M) = X(M-1) - RO * D
!
      CALL OS('X=X+CY  ',X=X,Y=D,C=-RO)
!
!-----------------------------------------------------------------------
#if defined COMPAD_DCO_T1S || COMPAD_DCO_T1V
!     TAKE TANGENT OF X1 IF WE ARE IN DCO_T1S MODE
!     AND STORE TANGENT OF FORMER X
      DRV0 = DRV1
!     STORE TANGENT OF NEW X
#  if defined COMPAD_DCO_T1S
      CALL DCO_T1S_GET( X%ADR(1)%P%R, DRV1, 1 )
#  endif
#  if defined COMPAD_DCO_T1V
      CALL DCO_T1V_GET( X%ADR(1)%P%R, DRV1, 1, 1 )
#  endif
#endif
!-----------------------------------------------------------------------
!
      IF(M.LT.CFG%NITMAX) GO TO 2
!
!-----------------------------------------------------------------------
!
!     IF(INFOGR) THEN
        TESTL = SQRT( RMRM / XL )
        IF (RELAT) THEN
          WRITE(LU,104) M,TESTL
        ELSE
          WRITE(LU,204) M,TESTL
        ENDIF
!     ENDIF
      GO TO 1000
!
!-----------------------------------------------------------------------
!
900   CONTINUE
!
      IF(INFOGR) THEN
        TESTL = SQRT( RMRM / XL )
        IF (RELAT) THEN
          WRITE(LU,102) M,TESTL
        ELSE
          WRITE(LU,202) M,TESTL
        ENDIF
      ENDIF
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
#if defined COMPAD_DCO_T1S || COMPAD_DCO_T1V
!      WRITE(LU,*) '  CG_END CONVTEST',
!     &  CNT,M,FINISH,RMRM,' :: ',
!     &  SQRT(DRV_ERR),(DRV_ERR.LE.CFG%EPS**2), SQRT(SUM(DRV1**2))
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
!
!-----------------------------------------------------------------------
!
!   FORMATS
!
102   FORMAT(1X,'GRACJG (BIEF) : ',
     &                     1I8,' ITERATIONS, RELATIVE PRECISION:',G16.7)
202   FORMAT(1X,'GRACJG (BIEF) : ',
     &                     1I8,' ITERATIONS, ABSOLUTE PRECISION:',G16.7)
104   FORMAT(1X,'GRACJG (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' RELATIVE PRECISION:',G16.7)
106   FORMAT(1X,'GRACJG (BIEF) : ',
     &        ' SOLUTION X=0 BECAUSE L2-NORM OF B VERY SMALL:',G16.7)
204   FORMAT(1X,'GRACJG (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' ABSOLUTE PRECISION:',G16.7)
!
!-----------------------------------------------------------------------
!
      END

