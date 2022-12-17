!                   ****************
                    SUBROUTINE GMRES
!                   ****************
!
     & (X,A,B,MESH,R0,V,AV,CFG,INFOGR,AUX)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    SOLVES A LINEAR SYSTEM A X = B
!+                USING THE GMRES (GENERALISED MINIMUM RESIDUAL) METHOD.
!code
!+  ALGORITHM:
!+
!+        |
!+        |   INITIALISATION
!+        |   ---------------
!+        |
!+        |   R0 = B - A X0
!+        |
!+        |   V1 = R0 / IIR0II
!+        |
!+        |   K = DIMENSION OF THE KRYLOV SPACE
!+        |
!+        |
!+        |
!+        |
!+        |   ITERATIONS
!+        |   ----------
!+        |
!+        | 1) BUILDS AN ORTHONORMAL BASE (VI) OF DIMENSION K+1
!+        |
!+        |   I = 1,...,K
!+        |
!+        |       VI+1 = A * VI
!+        |
!+        |       J = 1,...,I
!+        |
!+        |       HI+1,J = ( VI+1 , VJ )   HESSENBERG MATRIX (K+1,K)
!+        |
!+        |       VI+1 <--- VI+1  -  HI+1,J * VJ
!+        |
!+        |       VI+1 = VI+1 / IIVI+1II
!+        |
!+        |
!+        | 2) MULTIPLIES H BY Q  =  RK * RK-1 * ... * R1
!+        |
!+        |  GIVENS' ROTATION MATRIX RI :
!+        |                                    -                     -
!+        |                                   I ID(J-1)               I
!+        |                                   I                       I
!+        |                                   I        CJ  SJ         I
!+        |                              RI = I       -SJ  CJ         I
!+        |                                   I                       I
!+        |                                   I              ID(K-J)  I
!+        |                                    -                     -
!+        |
!+        |                         2    2
!+        |       WITH      CJ + SJ  =  1
!+        |
!+        |                 CJ AND SJ SUCH THAT H BECOMES TRIANGULAR
!+        |
!+        |                 ID(J-1) :    IDENTITY MATRIX J-1*J-1
!+        |
!+        |
!+        | 3) SOLVES THE SYSTEM H Y = Q E
!+        |                         -      -
!+        |       E VECTOR (NR0,0,0,0,0,0,.....)
!+        |
!+        |       NR0 NORM OF THE RESIDUAL
!+        |
!+        |
!+        |
!+        | 4) COMPUTES X(M+1) = X(M)  +  V * Y
!+        |
!+        |  V : MATRIX (3*NPOIN,K) WHICH COLUMNS ARE THE VJ
!+        |
!+        | 5) CHECKS THE RESIDUAL...
!+        |
!
!warning  CROUT AND GSEBE PRECONDITIONING ARE TREATED HERE LIKE A
!+            LU PRECONDITIONING. FOR CROUT IT MEANS THAT THE DIAGONAL
!+            OF THE PRECONDITIONING MATRIX IS NOT TAKEN INTO ACCOUNT
!code
!+-----------------------------------------------------------------------
!+                        PRECONDITIONING
!+-----------------------------------------------------------------------
!+    PRECON VALUE     I                  MEANING
!+-----------------------------------------------------------------------
!+        0 OR 1       I  NO PRECONDITIONING
!+                     I
!+        2            I  DIAGONAL PRECONDITIONING USING THE MATRIX
!+                     I  DIAGONAL
!+                     I
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
!history  J-M HERVOUET & C. MOULIN (LNH)
!+        26/08/1993
!+        FIRST IMPLEMENTATION
!+   First version.
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
!+        12/06/2014
!+        V7P0
!+   The case A=diagonal caused a crash, because in that case the Krylov
!+   space cannot be a basis. This case is now treated since the
!+   solution is easily found : X=A%D/B
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/07/2015
!+        V7P1
!+   Now when the algorithm fails the dimension of the Krylov space
!+   is reduced. This is more general than the previous solution
!+   that assumed the matrix diagonal. Various cases with diagonal
!+   matrix also tested.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX OF THE SYSTEM
!| AUX            |-->| MATRIX FOR PRECONDITIONING.
!| AV             |<->| WORK ARRAY
!| B              |-->| RIGHT-HAND SIDE OF THE SYSTEM
!| CFG            |-->| STRUCTURE OF SOLVER CONFIGURATION
!| INFOGR         |-->| IF YES, PRINT A LOG.
!| MESH           |-->| MESH STRUCTURE.
!| R              |<->| RESIDUAL
!| V              |<->| WORK ARRAY
!| X              |<->| INITIAL VALUE, THEN SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GMRES => GMRES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SLVCFG), INTENT(INOUT)    :: CFG
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: B
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: X,V,AV,R0
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: AUX
      LOGICAL, INTENT(IN)            :: INFOGR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     HARDCODED MAXIMUM CFG%KRYLOV=20
!
      DOUBLE PRECISION H(21,20),C(20),S(20),E(21),BID
!
      DOUBLE PRECISION R,ZZ,NB,PREC,NR0
!
      INTEGER I,J,K,L,M
!
      LOGICAL RELAT,CROUT,GSEB,PRECO
!
      INTRINSIC SQRT,ABS
!
!-----------------------------------------------------------------------
!
      CROUT=.FALSE.
      IF(MOD(CFG%PRECON,7).EQ.0) CROUT=.TRUE.
      GSEB=.FALSE.
      IF(MOD(CFG%PRECON,11).EQ.0) GSEB=.TRUE.
      PRECO=.FALSE.
      IF(CROUT.OR.GSEB.OR.MOD(CFG%PRECON,13).EQ.0) PRECO=.TRUE.
!
      IF(PRECO) THEN
!                  -1
!       COMPUTES  L   B
        CALL GODOWN(B, AUX,B,'D',MESH,.FALSE.)
      ENDIF
!
! INITIALISES
!
      NB = P_DOTS(B,B,MESH)
      NB = SQRT(NB)
!
      RELAT = .TRUE.
      IF(NB.LT.1.D0) THEN
        NB = 1.D0
        RELAT = .FALSE.
      ENDIF
!
      M = 0
!
! INITIALISES THE RESIDUAL R : A X0 - B
!
      CALL MATRBL( 'X=AY    ',R0,A,X,BID,MESH)
!
      IF(PRECO) THEN
        CALL GODOWN(R0, AUX,R0,'D',MESH,.FALSE.)
        CALL PUOG(X,AUX,X, 'D',MESH,.FALSE.)
      ENDIF
!
      CALL OS('X=X-Y   ', X=R0, Y=B)
!
! CHECKS THAT THE ACCURACY IS NOT ALREADY REACHED
!
      NR0=P_DOTS(R0,R0,MESH)
      NR0=SQRT(NR0)
      PREC = NR0/NB
!
      IF(PREC.LE.CFG%EPS) GO TO 3000
!
!-----------------------------------------------------------------------
!                   ITERATIONS LOOP
!-----------------------------------------------------------------------
!
20    CONTINUE
!
      M = M+1
!
!     K CAN BE REDUCED DURING THE ITERATION IF THE ALGORITHM FAILS
!
      K = CFG%KRYLOV
!
! COMPUTES THE VECTOR V1 = - R0 / NORM(R0)
! (- SIGN BECAUSE R = AX - B INSTEAD OF B - AX)
!
      CALL OS('X=CY    ', X=V%ADR(1)%P, Y=R0, C=-1.D0/NR0)
!
!-----------------------------------------------------------------------
!         COMPUTES THE ORTHONORMAL BASE AND MATRIX H
!-----------------------------------------------------------------------
!
!     K-1 1ST COLUMNS
!
      DO J=1,K-1
!
        IF(PRECO) THEN
          CALL GOUP(B , AUX , V%ADR(J)%P ,'D',MESH,.TRUE.)
          CALL MATRBL( 'X=AY    ',AV%ADR(J)%P,A,B,BID,MESH)
          CALL GODOWN(AV%ADR(J)%P,AUX,AV%ADR(J)%P,'D',MESH,.FALSE.)
        ELSE
          CALL MATRBL( 'X=AY    ',AV%ADR(J)%P,A,V%ADR(J)%P,BID,MESH)
        ENDIF
!
!       NEXT VECTOR IN THE BASIS : V%ADR(J+1)%P
!
        CALL OS('X=Y     ',X=V%ADR(J+1)%P,Y=AV%ADR(J)%P)
!
!       ORTHOGONALISATION PROCESS
!
        DO I = 1,J
          H(I,J) = P_DOTS( V%ADR(J+1)%P , V%ADR(I)%P , MESH )
          CALL OS('X=X+CY  ',X=V%ADR(J+1)%P,Y=V%ADR(I)%P,C=-H(I,J))
        ENDDO
!
        H(J+1,J)=SQRT(P_DOTS(V%ADR(J+1)%P,V%ADR(J+1)%P,MESH))
!
        IF(H(J+1,J).LT.1.D-20) THEN
          IF(INFOGR) THEN
            WRITE(LU,*) 'GMRES: NORM OF VECTOR ',J+1
            WRITE(LU,*) 'IN THE BASIS EQUALS ',H(J+1,J)
            WRITE(LU,*) 'SIZE OF KRYLOV SPACE REDUCED TO ',J
          ENDIF
!         REDUCTION OF KRYLOV SPACE
          K=J
          GO TO 2000
        ENDIF
        CALL OS('X=CX    ', X=V%ADR(J+1)%P, C=1.D0/H(J+1,J))
!
      ENDDO ! J
!
! K-TH COLUMN (VECTOR V(K+1) IS NOT COMPLETELY BUILT)
!
2000  CONTINUE
!
      IF(PRECO) THEN
        CALL GOUP(B , AUX , V%ADR(K)%P , 'D' ,MESH,.TRUE.)
        CALL MATRBL( 'X=AY    ',AV%ADR(K)%P,A,B,BID,MESH)
        CALL GODOWN(AV%ADR(K)%P,AUX,AV%ADR(K)%P,'D',MESH,.FALSE.)
      ELSE
        CALL MATRBL( 'X=AY    ',AV%ADR(K)%P,A,V%ADR(K)%P,BID,MESH)
      ENDIF
!
      H(K+1,K) = P_DOTS( AV%ADR(K)%P , AV%ADR(K)%P , MESH )
!
      DO I = 1,K
        H(I,K) = P_DOTS( AV%ADR(K)%P , V%ADR(I)%P , MESH )
        H(K+1,K) = H(K+1,K) - H(I,K)**2
      ENDDO
!
!     IN THEORY H(K+1,K) IS POSITIVE
!     TO MACHINE ACCURACY
      H(K+1,K) = SQRT( MAX(H(K+1,K),0.D0) )
!
!-----------------------------------------------------------------------
! BUILDS GIVENS' ROTATIONS AND APPLIES TO H AND E
!-----------------------------------------------------------------------
!
!     OTHER COMPONENTS FOR E ARE 0
      E(1) = NR0
!
!  ROTATIONS IN RANGE [1 - K]
!
      DO I = 1 , K
!
!       MODIFIES COLUMN I OF H BY THE PREVIOUS ROTATIONS
        IF(I.GE.2) THEN
          DO J = 1,I-1
            ZZ       =  C(J) * H(J,I) + S(J) * H(J+1,I)
            H(J+1,I) = -S(J) * H(J,I) + C(J) * H(J+1,I)
            H(J,I)   = ZZ
          ENDDO ! J
        ENDIF
!       MODIFIES COLUMN I OF H BY ROTATION I
        R = SQRT( H(I,I)**2 + H(I+1,I)**2 )
        IF(ABS(R).LT.1.D-6) THEN
          IF(INFOGR) THEN
            WRITE(LU,92) R
          ENDIF
          GO TO 3000
        ENDIF
        C(I) =  H(I,I)   / R
        S(I) =  H(I+1,I) / R
        H(I,I) = R
!       H(I+1,I) = 0.D0    (WILL NOT BE USED AGAIN)
!       MODIFIES VECTOR E
        E(I+1) = -S(I) * E(I)
        E(I  ) =  C(I) * E(I)
!
      ENDDO ! I
!
!-----------------------------------------------------------------------
! SOLVES SYSTEM H*Y = E     (H UPPER TRIANGULAR OF DIMENSION K)
!                            Y SAME AS E
!
! H(I,I) <> 0 HAS ALREADY BEEN CHECKED ON R
!-----------------------------------------------------------------------
!
      E(K) = E(K) / H(K,K)
      DO J = K-1,1,-1
        DO L = J+1,K
          E(J) = E(J) - H(J,L) * E(L)
        ENDDO
        E(J) = E(J) / H(J,J)
      ENDDO
!
!-----------------------------------------------------------------------
! BUILDS THE SOLUTION FOR STEP M : X(M+1) = X(M) + VK * Y(K)
!-----------------------------------------------------------------------
!
      DO L = 1,K
!
!  COMPUTES THE NEW SOLUTION X
!
        CALL OS('X=X+CY  ', X=X , Y=V%ADR(L)%P , C=E(L))
!
!  COMPUTES THE RESIDUAL : RM+1 = RM + A * VK * ZK
!
        CALL OS('X=X+CY  ', X=R0, Y=AV%ADR(L)%P, C= E(L))
!
      ENDDO
!
! CHECKS THAT THE ACCURACY IS NOT ALREADY REACHED
! THE RESIDUAL NORM IS GIVEN BY ABS(E(K+1))
!
!     NR0  =NORM OF R0
      NR0  = ABS(E(K+1))
      PREC = NR0/NB
!
      IF (PREC.GE.CFG%EPS.AND.M.LT.CFG%NITMAX)  GOTO 20
!
3000  CONTINUE
!
!-----------------------------------------------------------------------
!
      IF(PRECO) THEN
        CALL GOUP( X,AUX,X,'D',MESH,.FALSE.)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IF(INFOGR) THEN
        IF(M.LT.CFG%NITMAX) THEN
          IF(INFOGR) THEN
          IF(RELAT) THEN
            WRITE(LU,102) M,PREC
          ELSE
            WRITE(LU,202) M,PREC
          ENDIF
          ENDIF
        ELSE
          IF(RELAT) THEN
            WRITE(LU,104) M,PREC
          ELSE
            WRITE(LU,204) M,PREC
          ENDIF
        ENDIF
!     ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
!
 92   FORMAT(1X,'GMRES (BIEF) : ALGORITHM FAILED  R=',G16.7,/,
     &       1X,'               IF THE MATRIX IS DIAGONAL, CHOOSE',/,
     &       1X,'               THE CONJUGATE GRADIENT SOLVER.')
102   FORMAT(1X,'GMRES (BIEF) : ',
     &                     1I8,' ITERATIONS, RELATIVE PRECISION:',G16.7)
202   FORMAT(1X,'GMRES (BIEF) : ',
     &                     1I8,' ITERATIONS, ABSOLUTE PRECISION:',G16.7)
104   FORMAT(1X,'GMRES (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' RELATIVE PRECISION:',G16.7)
204   FORMAT(1X,'GMRES (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' ABSOLUTE PRECISION:',G16.7)
!
!-----------------------------------------------------------------------
!
      END

