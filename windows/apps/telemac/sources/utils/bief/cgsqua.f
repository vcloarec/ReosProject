!                   *****************
                    SUBROUTINE CGSQUA
!                   *****************
!
     &(X,A,B,MESH, G,G0,P,K,H,AHPK,CFG,INFOGR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE LINEAR SYSTEM A X = B
!+                USING THE SQUARED CONJUGATE GRADIENT METHOD.
!code
!+     ALGORITHM:
!+
!+        |
!+        |   INITIALISATION
!+        |   ---------------
!+        |
!+        |    0            N
!+        |   X  VECTOR IN R , APPROXIMATION OF THE SOLUTION
!+        |
!+        |      0      0
!+        |     G  = A X  - B
!+        |
!+        |     K0 = P0 = G0
!+        |
!+        |
!+        |   ITERATIONS
!+        |   ----------
!+        |
!+        |              M     0
!+        |       M   ( K  ,  G  )
!+        |     RO =  ------------
!+        |               M    0
!+        |           (A P ,  G  )
!+        |
!+        |      M     M    M     M
!+        |     H   = K - RO * A P
!+        |
!+        |      M+1   M    M       M    M
!+        |     G   = G - RO * A ( H  + K  )
!+        |
!+        |      M+1   M      M     M    M
!+        |     X   = X   - RO * ( H  + K  )
!+        |
!+        |                 0     M+1
!+        |              ( G  , G   )
!+        |     BETA =   ------------
!+        |                 0   M
!+        |              ( G , G    )
!+        |
!+        |      M+1   M+1        M    M     M       M
!+        |     P  =  G   + 2*BETA  * H + BETA**2 * P
!+        |
!+        |      M+1   M+1        M    M
!+        |     K  =  G   +   BETA  * H
!+        |
!
!code
!+-----------------------------------------------------------------------
!+                        PRECONDITIONING
!+                        (ALSO SEE SOLV01)
!+-----------------------------------------------------------------------
!+    PRECON VALUE     I                  MEANING
!+-----------------------------------------------------------------------
!+        0            I  NO PRECONDITIONING
!+        2            I  DIAGONAL PRECONDITIONING WITH THE MATRIX
!+                     I  DIAGONAL
!+        3            I  DIAGONAL PRECONDITIONING WITH THE CONDENSED
!+                     I  MATRIX
!+        5            I  OTHER
!+-----------------------------------------------------------------------
!code
!+  MEANING OF IELM :
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : P1 TRIANGLE            3                       YES
!+  12 : P2 TRIANGLE            6                       YES
!+  13 : P1-ISO P1 TRIANGLE     6                       YES
!+  14 : P2 TRIANGLE            7
!+  21 : Q1 QUADRILATERAL       4                       YES
!+  22 : Q2 QUADRILATERAL       8
!+  24 : Q2 QUADRILATERAL       9
!+  31 : P1 TETRAHEDRON         4                       YES
!+  32 : P2 TETRAHEDRON        10
!+  41 : MITHRIDATE PRISMS      6                       YES
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        24/04/97
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
!| A              |-->| MATRIX OF THE SYSTEM
!| AHPK           |<->| WORK STRUCTURE
!| B              |-->| RIGHT-HAND SIDE OF SYSTEM
!| CFG            |-->| CFG(1): STORAGE OF MATRIX
!|                |   | CFG(2): MATRIX VECTOR PRODUCT
!| G              |<->| GRADIENT.
!| G0             |<->| INITIAL GRADIENT
!| H              |<->| WORK STRUCTURE
!| INFOGR         |-->| IF YES, INFORMATION PRINTED
!| K              |<->| WORK STRUCTURE
!| MESH           |-->| MESH STRUCTURE
!| P              |<->| WORK STRUCTURE
!| X              |<--| INITIAL VALUE, THEN SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CGSQUA => CGSQUA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: X,G,G0,P,K,H,AHPK
      TYPE(BIEF_OBJ)  , INTENT(IN   ) :: B
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: A
      TYPE(SLVCFG)    , INTENT(INOUT) :: CFG
      LOGICAL         , INTENT(IN)    :: INFOGR
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XL,RO,TESTL,RL,GMP1G0,BETA,GMG0,C
!
      INTEGER M
!
      LOGICAL RELAT
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
! COMPUTES THE NORM OF THE SECOND MEMBER
!
      XL = P_DOTS(B,B,MESH)
!
      IF(XL.LT.1.D0) THEN
        XL = 1.D0
        RELAT = .FALSE.
      ELSE
        RELAT = .TRUE.
      ENDIF
!
      M = 0
!
! INITIALISES G  : A X0 - B
!
      CALL MATRBL( 'X=AY    ',G,A,X,C,  MESH)
!
      CALL OS('X=X-Y   ', X=G, Y=B)
!
! CHECKS THAT THE ACCURACY HAS NOT ALREADY BEEN REACHED
!
      RL   = P_DOTS(G,G,MESH)
!
      IF(RL.LT.CFG%EPS**2*XL) THEN
        TESTL = SQRT(RL/XL)
        IF (INFOGR) THEN
          IF(RELAT) THEN
            WRITE(LU,101) M,TESTL
          ELSE
            WRITE(LU,201) M,TESTL
          ENDIF
        ENDIF
        GOTO 1000
      ENDIF
!
! INITIALISES G0 , P , AND K
!
      CALL OS('X=Y     ', X=G0, Y=G)
      CALL OS('X=Y     ', X=P , Y=G)
      CALL OS('X=Y     ', X=K , Y=G)
!
      M = 1
20    CONTINUE
!
! COMPUTES AP (IN H, RECOMPUTED LATER)
!
      CALL MATRBL( 'X=AY    ',H,A,P,C,  MESH)
!
! COMPUTES RO
!
      RO = P_DOTS(K,G0,MESH) / P_DOTS(H,G0,MESH)
!
! COMPUTES H+K (IN H, AP ALREADY BEING IN H)
!
      CALL OS('X=CX    ', X=H, C=-RO)
      CALL OS('X=X+CY  ', X=H, Y=K, C=2.D0)
!
!            M+1   M       M       M
! COMPUTES  X   = X  -   RO * (H+K)     (H+K IN H)
!
      CALL OS('X=X+CY  ', X=X, Y=H, C=-RO)
!
! COMPUTES A(H+K)  (HERE H+K IN H, A(H+K) IN AHPK)
!
      CALL MATRBL( 'X=AY    ',AHPK,A,H,C,  MESH)
!
!             M     0
! COMPUTES ( G   , G  )
!
      GMG0 = P_DOTS(G,G0,MESH)
!
! COMPUTES GM
!
      CALL OS('X=X+CY  ', X=G, Y=AHPK, C=-RO)
!
      RL   = P_DOTS(G,G,MESH)
      IF (RL.GT.CFG%EPS**2*XL) THEN
        IF (M.GE.CFG%NITMAX) THEN
            TESTL=SQRT(RL/XL)
            IF(RELAT) THEN
              WRITE(LU,103) M,TESTL
            ELSE
              WRITE(LU,203) M,TESTL
            ENDIF
          GOTO 1000
        ELSE
          M = M + 1
        ENDIF
      ELSE
        IF(INFOGR) THEN
          TESTL=SQRT(RL/XL)
          IF(RELAT) THEN
            WRITE(LU,101) M,TESTL
          ELSE
            WRITE(LU,201) M,TESTL
          ENDIF
        ENDIF
        GOTO 1000
      ENDIF
!
!             M+1   0
! COMPUTES ( G   , G  )
!
      GMP1G0 = P_DOTS(G,G0,MESH)
!
! COMPUTES BETA
!
      BETA = GMP1G0 / GMG0
!
!           M
! COMPUTES H   (H+K IN H)
!
      CALL OS('X=X-Y   ', X=H, Y=K)
!
!           M+1
! COMPUTES P
!
      CALL OS('X=CX    ', X=P, C=BETA**2 )
      CALL OS('X=X+Y   ', X=P, Y=G)
      CALL OS('X=X+CY  ', X=P, Y=H, C=2*BETA)
!
!           M+1
! COMPUTES K
!
      CALL OS('X=Y     ', X=K, Y=G)
      CALL OS('X=X+CY  ', X=K, Y=H, C=BETA)
!
      GOTO 20
!
1000  RETURN
!
!-----------------------------------------------------------------------
!
!   FORMATS
!
101   FORMAT(1X,'CGSQUA (BIEF) : ',1I8,' ITERATIONS',
     &          ' RELATIVE PRECISION: ',G16.7)
201   FORMAT(1X,'CGSQUA (BIEF) : ',1I8,' ITERATIONS',
     &          ' ABSOLUTE PRECISION: ',G16.7)
103   FORMAT(1X,'CGSQUA (BIEF) : EXCEEDING MAXIMUM ITERATIONS ',1I8,
     &          ' RELATIVE PRECISION: ',G16.7)
203   FORMAT(1X,'CGSQUA (BIEF) : EXCEEDING MAXIMUM ITERATIONS ',1I8,
     &          ' ABSOLUTE PRECISION:',G16.7)
!
!-----------------------------------------------------------------------
!
      END
