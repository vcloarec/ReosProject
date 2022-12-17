!                   ****************
                    SUBROUTINE SOLVE
!                   ****************
!
     &(X, A,B,TB,CFG,INFOGR,MESH,AUX)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES A LINEAR SYSTEM OF THE FORM A X = B
!+
!+            USING ITERATIVE METHODS
!+                WITH POSSIBLE PRECONDITIONING.
!code
!+-----------------------------------------------------------------------
!+                        CHOICE OF THE METHOD
!+-----------------------------------------------------------------------
!+      VALUE          I       MEANING         I
!+-----------------------------------------------------------------------
!+                     I                       I
!+        1            I   CONJUGATE GRADIENT  I
!+                     I                       I
!+        2            I   CONJUGATE RESIDUAL  I
!+                     I                       I
!+        3            I   CONJUGATE GRADIENT  I
!+                     I  ON A NORMAL EQUATION I
!+                     I                       I
!+        4            I     MINIMUM ERROR     I
!+                     I                       I
!+        5            I        SQUARED        I
!+                     I   CONJUGATE GRADIENT  I
!+                     I                       I
!+        6            I        SQUARED        I
!+                     I   CONJUGATE GRADIENT  I
!+                     I        (CGSTAB)       I
!+                     I                       I
!+        7            I       GMRES           I
!+                     I                       I
!+        8            I     DIRECT : YSMP     I
!+                     I                       I
!+        9            I     DIRECT : MUMPS    I
!+                     I                       I
!+-----------------------------------------------------------------------
!
!warning  FOR SOME PRECONDITIONING ALGORITHMS, MATRIX A
!+            CAN BE MODIFIED
!code
!+                        PRECONDITIONING
!+
!+     NOTES     : SOME PRECONDITIONING ALGORITHMS CAN BE ADDED
!+                 (DIAGONAL 2, 3 OR 5 WITH THE OTHERS).
!+                 THAT'S WHY PRIME NUMBERS WERE USED TO CHARACTERISE
!+                 THE PRECONDITIONING ALGORITHMS.
!+                 SHOULD THE USER WANT TO CUMULATE THE EFFECTS, HE/SHE
!+                 SHOULD GIVE THE PRODUCT OF THE CORRESPONDING PRIMES
!+
!+-----------------------------------------------------------------------
!+        VALUE        I                  MEANING
!+-----------------------------------------------------------------------
!+        0 OR 1       I  NO PRECONDITIONING
!+                     I
!+        2            I  DIAGONAL PRECONDITIONING WITH THE MATRIX
!+                     I  DIAGONAL
!+                     I
!+        3            I  BLOCK DIAGONAL PRECONDITIONING
!+                     I
!+        5            I  DIAGONAL PRECONDITIONING WITH THE ABSOLUTE
!+                     I  VALUE OF THE MATRIX DIAGONAL
!+                     I
!+        7            I  CROUT'S PRECONDITIONING BY ELEMENT
!+                     I
!+       11            I  GAUSS-SEIDEL'S PRECONDITIONING BY ELEMENT
!+                     I
!+       13            I  PRECONDITIONING MATRIX SUPPLIED BY THE USER
!+                     I
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (LNHE)
!+        18/02/08
!+        V5P9
!+
!
!history  C. DENIS (SINETICS)
!+        19/03/10
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        07/12/2011
!+        V6P2
!+   Call to preverseg and preverebe modified.
!+
!
!history  J.PARISI (HRW)
!+        9/08/2012
!+        V6P2
!+   Call to SD_SOLVE_4 modified.
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+        COMPENSATED INTERFACE NODE ASSEMBLY (MODASS=3)
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/02/2015
!+        V7P1
!+   Arguments added to PRE4_MUMPS, to correct a bug when it calls
!+   SD_FABSG4.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
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
      USE BIEF, EX_SOLVE => SOLVE
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
      USE DECLARATIONS_TELEMAC, ONLY : TBB, BB, BX, FIRST_SOLVE
      USE DECLARATIONS_SPECIAL
!
      USE INTERFACE_PARALLEL, ONLY : P_MAX
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
!
      INTEGER PRESTO,IG,LV,S,NBL,I
      INTEGER IT1,IT2,IT3,IT4,IT5,IT6,IT7,IBL1,IBL2,K,IAD,ITB,ITBB
!
      LOGICAL DIADON,PREXSM
      INTEGER NPOIN_TOT
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
!     STRUCTURES OF BLOCKS OF WORKING ARRAYS
!
      TYPE(BIEF_OBJ), POINTER :: PB,PX
!
!-----------------------------------------------------------------------
!
!  ALLOCATES THE BLOCK OF BLOCKS TBB AND THE BLOCKS IN TBB
!
      IF(FIRST_SOLVE) THEN
        CALL ALLBLO(TBB,'TBB   ')
        CALL ALLBLO(BB ,'BB    ')
        CALL ALLBLO(BX ,'BX    ')
        FIRST_SOLVE=.FALSE.
      ENDIF
      NBL = 7
      IF(CFG%SLV.EQ.7) NBL = MAX(NBL,4+2*CFG%KRYLOV)
      IF(NBL.GT.TBB%N) THEN
        IF(TBB%N.NE.0) THEN
          DO I=1,TBB%N
            DEALLOCATE(TBB%ADR(I)%P%ADR)
            NULLIFY(TBB%ADR(I)%P%ADR)
            DEALLOCATE(TBB%ADR(I)%P)
            NULLIFY(TBB%ADR(I)%P)
          ENDDO
        ENDIF
        TBB%N=0
        CALL ALLBLO_IN_BLOCK(TBB,NBL,'BL    ')
      ENDIF
!
!-----------------------------------------------------------------------
!
      LV = MESH%LV
!
!-----------------------------------------------------------------------
!
!  VARIOUS TYPES OF SOLVED SYSTEMS S = 0 : NORMAL MATRIX
!                                  S = 1 : 1 MATRICE IN A BLOCK
!                                  S = 2 : 4 MATRICES IN A BLOCK
!                                  S = 3 : 9 MATRICES IN A BLOCK
!
      IF(A%TYPE.EQ.3) THEN
        S = 0
        BB%N = 0
        BX%N = 0
        CALL ADDBLO(BB,B)
        CALL ADDBLO(BX,X)
        PX => BX
        PB => BB
      ELSEIF(A%TYPE.EQ.4) THEN
        IF(A%N.EQ.1) THEN
          S = 1
        ELSEIF(A%N.EQ.4) THEN
          S = 2
        ELSEIF(A%N.EQ.9) THEN
          S = 3
        ENDIF
        PX => X
        PB => B
      ENDIF
!
!----------------
!  DIRECT SOLVERS
!  ---> YSMP
!----------------
!
      IF(CFG%SLV.EQ.8) THEN
!
        IF(NCSIZE.GT.1) THEN
          WRITE(LU,2019)
2019      FORMAT(1X,'USE THE PARALLEL DIRECT SOLVER MUMPS,',/,1X,
     &              'SOLVER = 9',///)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(S.EQ.0) THEN
          IF(A%TYPEXT.NE.'S'.AND.A%TYPEXT.NE.'Q') THEN
            WRITE(LU,*) 'SOLVE (BIEF): OFF-DIAGONAL TERMS'
            WRITE(LU,*) '              OF TYPE ',A%TYPEXT
            WRITE(LU,*) '              NOT IMPLEMENTED'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL SD_SOLVE_1(A%D%DIM1,MESH%NSEG,MESH%GLOSEG%I,
     &                    MESH%GLOSEG%DIM1,
     &                    A%D%R,A%X%R,X%R,B%R,INFOGR,A%TYPEXT)
        ELSEIF(S.EQ.1) THEN
          IF(A%ADR(1)%P%TYPEXT.NE.'S'.AND.A%ADR(1)%P%TYPEXT.NE.'Q') THEN
            WRITE(LU,*) 'SOLVE (BIEF): DIRECT SOLVER FOR SYMMETRIC'
            WRITE(LU,*) '              SYSTEMS ONLY'
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL SD_SOLVE_1(A%ADR(1)%P%D%DIM1,MESH%NSEG,MESH%GLOSEG%I,
     &                    MESH%GLOSEG%DIM1,
     &                    A%ADR(1)%P%D%R,A%ADR(1)%P%X%R,X%ADR(1)%P%R,
     &                    B%ADR(1)%P%R,INFOGR,A%ADR(1)%P%TYPEXT)
        ELSEIF(S.EQ.2) THEN
          CALL SD_SOLVE_4(MESH%NPOIN,MESH%NSEG,MESH%GLOSEG%I,
     &                    A%ADR(1)%P%D%R,A%ADR(2)%P%D%R,
     &                    A%ADR(3)%P%D%R,A%ADR(4)%P%D%R,
     &                    A%ADR(1)%P%X%R,A%ADR(2)%P%X%R,
     &                    A%ADR(3)%P%X%R,A%ADR(4)%P%X%R,
     &                    X%ADR(1)%P%R,X%ADR(2)%P%R,
     &                    B%ADR(1)%P%R,B%ADR(2)%P%R,INFOGR,
     &                    A%ADR(1)%P%TYPEXT,A%ADR(2)%P%TYPEXT,
     &                    A%ADR(3)%P%TYPEXT,A%ADR(4)%P%TYPEXT)
!       ELSEIF(S.EQ.3) THEN
        ELSE
          WRITE(LU,401) S
401       FORMAT(1X,'SOLVE (BIEF): S=',1I6,' CASE NOT IMPLEMENTED')
          CALL PLANTE(1)
          STOP
        ENDIF
        RETURN
!
      ELSEIF(CFG%SLV.EQ.9) THEN
!
!----------------
!  DIRECT SOLVERS
!  ---> MUMPS
!----------------
!
        IF(NCSIZE.LT.1) THEN
          WRITE(LU,3019)
3019      FORMAT(1X,'MUMPS ARE NOT AVAILABLE FOR SEQUENTIAL RUNS,',/,1X,
     &         'USE SEQUENITAL DIRECT SOLVER (SOLVER = 8) ',///)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       COMPUTING THE NUMBER OF POINTS IN THE MESH BEFORE PARTITIONING
!
        IF(NCSIZE.GT.1) THEN
          NPOIN_TOT=0
          DO I=1,MESH%NPOIN
            NPOIN_TOT=MAX(MESH%KNOLG%I(I),NPOIN_TOT)
          ENDDO
          NPOIN_TOT=P_MAX(NPOIN_TOT)
        ELSE
          NPOIN_TOT=MESH%NPOIN
        ENDIF
!
        IF(S.EQ.0) THEN
          WRITE(LU,402) S
402       FORMAT(1X,'SOLVE (BIEF): S=',1I6,1X,
     &              'CASE NOT YET MPLEMENTED FOR MUMPS')
          CALL PLANTE(1)
          STOP
        ELSEIF(S.EQ.1) THEN
          WRITE(LU,4011) S
 4011     FORMAT(1X,'SOLVE (BIEF): S=',1I6,' CASE NOT YET
     &           IMPLEMENTED FOR MUMPS')
          CALL PLANTE(1)
          STOP
        ELSEIF(S.EQ.2) THEN
          CALL PRE4_MUMPS(MESH%NPOIN,MESH%NSEG,MESH%GLOSEG%I,
     &                    A%ADR(1)%P%D%R,A%ADR(2)%P%D%R,
     &                    A%ADR(3)%P%D%R,A%ADR(4)%P%D%R,
     &                    A%ADR(1)%P%X%R,A%ADR(2)%P%X%R,
     &                    A%ADR(3)%P%X%R,A%ADR(4)%P%X%R,
     &                    X%ADR(1)%P%R,X%ADR(2)%P%R,
     &                    B%ADR(1)%P%R,B%ADR(2)%P%R,
     &                    A%ADR(1)%P%TYPEXT,A%ADR(2)%P%TYPEXT,
     &                    A%ADR(3)%P%TYPEXT,A%ADR(4)%P%TYPEXT,
     &                    MESH%KNOLG%I,NPOIN_TOT)
        ELSE
          WRITE(LU,401) S
          CALL PLANTE(1)
          STOP
        ENDIF
        RETURN
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      PRESTO = CFG%PRECON
      IF(CFG%PRECON.EQ.0) CFG%PRECON = 1
!
!-----------------------------------------------------------------------
!
!  MANAGES WORKING ARRAYS : ITB --> NEXT AVAILABLE VECTOR
!
!  ITB  --> NEXT AVAILABLE VECTOR IN TB
!  ITBB --> NEXT AVAIALBLE BLOCK  IN TBB
!
      ITB  = 1
      ITBB = 1
!
!  ALLOCATES TWO WORKING BLOCKS WITH, EITHER A VECTOR,
!  OR A BLOCK OF VECTORS (CASE WHERE S IS 0).
!  THESE TWO BLOCKS ARE COMMON TO ALL THE METHODS.
!
!     FOR THE PRECONDITIONING MATRICES
      IF(3*(CFG%PRECON/3).EQ.CFG%PRECON) THEN
!       BLOCK DIAGONAL PRECONDITIONING : S**2 DIAGONALS
        CALL SOLAUX(IT1, TB,TBB,ITB,ITBB,S**2)
      ELSE
!       OTHER : S DIAGONALS
        CALL SOLAUX(IT1, TB,TBB,ITB,ITBB,S)
      ENDIF
!
      CALL SOLAUX(IT2, TB,TBB,ITB,ITBB,S)
!
      IF(CFG%SLV.EQ.7) THEN
!       SPECIAL GMRES : ARRAYS DEPENDING ON THE SIZE OF KRYLOV
!                       TBB(IBL1) : BLOCK OF CFG%KRYLOV VECTORS
!                       OR BLOCK OF CFG%KRYLOV BLOCKS OF S VECTORS
!                       TBB(IBL2) : IDEM
!
        IBL1=ITBB
        ITBB = ITBB + 1
        IBL2=ITBB
        ITBB = ITBB + 1
        DO I=1,TBB%ADR(IBL1)%P%N
          NULLIFY(TBB%ADR(IBL1)%P%ADR(I)%P)
        ENDDO
        TBB%ADR(IBL1)%P%N=0
        DO I=1,TBB%ADR(IBL2)%P%N
          NULLIFY(TBB%ADR(IBL2)%P%ADR(I)%P)
        ENDDO
        TBB%ADR(IBL2)%P%N=0
        DO K=1,CFG%KRYLOV
          CALL SOLAUX(IAD, TB,TBB,ITB,ITBB,S)
          CALL ADDBLO(TBB%ADR(IBL1)%P,TBB%ADR(IAD)%P)
          CALL SOLAUX(IAD, TB,TBB,ITB,ITBB,S)
          CALL ADDBLO(TBB%ADR(IBL2)%P,TBB%ADR(IAD)%P)
        ENDDO ! K
!       AVOIDS A WARNING FROM THE INTEL COMPILER
        IT3=-1
        IT4=-1
        IT5=-1
        IT6=-1
        IT7=-1
      ELSE
!       OTHER METHODS (COULD SOMETIMES NOT ALLOCATE IT6 OR IT7)
        CALL SOLAUX(IT3, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT4, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT5, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT6, TB,TBB,ITB,ITBB,S)
        CALL SOLAUX(IT7, TB,TBB,ITB,ITBB,S)
!       AVOIDS A WARNING FROM THE CRAY COMPILER
        IBL1=1
        IBL2=1
!
      ENDIF
!
!     CROUT'S PRECONDITIONING : REQUIRES A PRELIMINARY PRECONDITIONING
!     THAT SETS DIAGONALS TO 1.
!     THE GRADIENT WILL BE DISTINGUISHED FROM THE RESIDUE (POINTER IG)
!
      IF(  7*(CFG%PRECON/ 7).EQ.CFG%PRECON.OR.
     &    11*(CFG%PRECON/11).EQ.CFG%PRECON.OR.
     &    13*(CFG%PRECON/13).EQ.CFG%PRECON.OR.
     &    17*(CFG%PRECON/17).EQ.CFG%PRECON     ) THEN
        IG=IT6
        IF(2*(CFG%PRECON/2).NE.CFG%PRECON.AND.
     &     3*(CFG%PRECON/3).NE.CFG%PRECON) THEN
!         SELECTS DIAGONAL
          CFG%PRECON=2*CFG%PRECON
        ENDIF
      ELSE
!       NOTE IT5 =-1 IF CFG%SLV.EQ.7 BUT IN THIS CASE IG IS USELESS
        IG=IT5
      ENDIF
!
!  END OF: MANAGES THE WORKING ARRAYS
!
!-----------------------------------------------------------------------
!                               -1/2      -1/2  1/2        -1/2
!  DIAGONAL PRECONDITIONINGS : D     A  D      D     X  = D      B
!
      DIADON = .FALSE.
      PREXSM = .TRUE.
!
      IF(3*(CFG%PRECON/3).EQ.CFG%PRECON.AND.(S.EQ.2.OR.S.EQ.3)) THEN
!       BLOCK-DIAGONAL PRECONDITIONING (4 OR 9 MATRICES)
        CALL PREBDT(X,A,B,TBB%ADR(IT1)%P,MESH,PREXSM,DIADON,S)
!       DOES NOT MOFIFY D11, D22, D33 WHEN PRECDT IS CALLED
        DIADON = .TRUE.
      ENDIF
!
      IF(2*(CFG%PRECON/2).EQ.CFG%PRECON.OR.
     &   3*(CFG%PRECON/3).EQ.CFG%PRECON.OR.
     &   5*(CFG%PRECON/5).EQ.CFG%PRECON) THEN
        CALL PRECDT(X,A,B,TBB%ADR(IT1)%P,MESH,
     &              CFG%PRECON,PREXSM,DIADON,S)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  BUILDS THE PRECONDITIONING MATRICES:
!
      IF(7*(CFG%PRECON/7).EQ.CFG%PRECON) THEN
        CALL DCPLDU(AUX,A,MESH,.TRUE.,LV)
      ELSEIF(11*(CFG%PRECON/11).EQ.CFG%PRECON) THEN
        CALL GSEBE(AUX,A,MESH)
      ELSEIF(13*(CFG%PRECON/13).EQ.CFG%PRECON) THEN
!       DOES NOTHING, AUX IS SUPPLIED BY THE SUBROUTINE CALLING
      ELSEIF(17*(CFG%PRECON/17).EQ.CFG%PRECON) THEN
        IF(CFG%SLV.NE.1.AND.CFG%SLV.NE.2) THEN
          WRITE(LU,*) 'PRECONDITIONING 17'
          WRITE(LU,*) 'NOT IMPLEMENTED FOR SOLVER ',CFG%SLV
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(AUX%TYPE.NE.3) THEN
          WRITE(LU,*) 'PRECONDITIONING 17'
          WRITE(LU,*) 'NOT IMPLEMENTED FOR BLOCKS OF MATRICES'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(AUX%STO.EQ.1) THEN
          CALL PREVEREBE(AUX%X%R,A%X%R,A%TYPDIA,A%TYPEXT,
     &                   MESH%IKLE%I,MESH%NPOIN,MESH%NELEM,
     &                   MESH%NELMAX,MESH,MESH%TYPELM)
        ELSE
          CALL PREVERSEG(AUX%X%R,A%D%R,A%X%R,A%TYPDIA,A%TYPEXT,
     &                   MESH%NPOIN,MESH,MESH%NSEG,MESH%TYPELM)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  PARALLEL MODE: SECOND MEMBER
!
      IF(NCSIZE.GT.1) THEN
        IF (MODASS .EQ. 1) THEN
          CALL PARCOM(B,2,MESH)
        ELSEIF (MODASS .EQ. 3) THEN
          CALL PARCOM_COMP(B,B%E,2,MESH)
        ENDIF
      ENDIF
      IF (MODASS .EQ.3)THEN
        B%R=B%R+B%E
        B%E=0.D0
      ENDIF
!
!-----------------------------------------------------------------------
!
!  SOLVES THE LINEAR SYSTEM:
!
      IF(CFG%SLV.EQ.1) THEN
!
!       CONJUGATE GRADIENT
!
        CALL GRACJG(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,
     &              TBB%ADR(IT5)%P,TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
!
      ELSEIF(CFG%SLV.EQ.2) THEN
!
!       CONJUGATE RESIDUAL
!
        CALL RESCJG(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,
     &              TBB%ADR(IT4)%P,TBB%ADR(IT5)%P,
     &              TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
!
      ELSEIF(CFG%SLV.EQ.3) THEN
!
!       NORMAL EQUATION
!
        CALL EQUNOR(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,
     &              TBB%ADR(IT4)%P,TBB%ADR(IT5)%P,
     &              TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
!
      ELSEIF(CFG%SLV.EQ.4) THEN
!
!       MINIMUM ERROR
!
        CALL ERRMIN(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,TBB%ADR(IT5)%P,
     &              TBB%ADR(IG)%P,
     &              CFG,INFOGR,AUX)
!
      ELSEIF(CFG%SLV.EQ.5) THEN
!
!       SQUARED CONJUGATE GRADIENT
!
        CALL CGSQUA(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,TBB%ADR(IT4)%P,
     &              TBB%ADR(IT5)%P,TBB%ADR(IT6)%P,TBB%ADR(IT7)%P,
     &              CFG,INFOGR)
!
      ELSEIF(CFG%SLV.EQ.6) THEN
!
!       CGSTAB
!
        CALL CGSTAB(PX, A,PB, MESH,
     &              TBB%ADR(IT2)%P,TBB%ADR(IT3)%P,TBB%ADR(IT4)%P,
     &              TBB%ADR(IT5)%P,TBB%ADR(IT6)%P,TBB%ADR(IT7)%P,
     &              CFG,INFOGR,AUX)
!
      ELSEIF(CFG%SLV.EQ.7) THEN
!
!       GENERALISED MINIMUM RESIDUAL
!
        CALL GMRES(PX, A,PB,MESH,
     &             TBB%ADR(IT2)%P,TBB%ADR(IBL1)%P,TBB%ADR(IBL2)%P,
     &             CFG,INFOGR,AUX)
!
!
      ELSE
!
        WRITE(LU,400) CFG%SLV
400     FORMAT(1X,'SOLVE (BIEF) :',1I6,' METHOD NOT AVAILABLE :')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INVERSES THE CHANGE IN VARIABLE IF PRECONDITIONING
!                                                DIAGONAL
!                                                DIAGONAL-BLOCK
!
      IF(2*(CFG%PRECON/2).EQ.CFG%PRECON.OR.
     &   3*(CFG%PRECON/3).EQ.CFG%PRECON.OR.
     &   5*(CFG%PRECON/5).EQ.CFG%PRECON    ) THEN
        CALL OS('X=XY    ', X=PX, Y=TBB%ADR(IT1)%P)
      ENDIF
!
      IF(3*(CFG%PRECON/3).EQ.CFG%PRECON.AND.(S.EQ.2.OR.S.EQ.3)) THEN
        CALL UM1X(X,TBB%ADR(IT1)%P,S)
      ENDIF
!
!-----------------------------------------------------------------------
!
      CFG%PRECON = PRESTO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
