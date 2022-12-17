!                   *****************
                    SUBROUTINE CGSTAB
!                   *****************
!
     &(X, A,B , MESH, P,Q,R,S,T,V, CFG,INFOGR,AUX)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    SOLVES THE LINEAR SYSTEM A X = B
!+        USING THE SQUARED CONJUGATE GRADIENT METHOD STABILLISED.
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
!+        5            I  DIAGONAL PRECONDITIONING BUT ACCEPTS 0 OR
!+                     I  NEGATIVE VALUES ON THE DIAGONAL
!+        7            I  CROUT
!+-----------------------------------------------------------------------
!
!history  R RATKE (HANNOVER); A MALCHEREK (HANNOVER); J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   CALL PARMOY removed, and stop if Crout preconditionning is asked
!+   in parallel.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX OF THE SYSTEM
!| AUX            |-->| PRECONDITIONING MATRIX
!| B              |-->| RIGHT-HAND SIDE OF SYSTEM
!| CFG            |-->| CFG(1): STORAGE OF MATRIX
!|                |   | CFG(2): MATRIX VECTOR PRODUCT
!| INFOGR         |-->| IF YES, INFORMATION PRINTED
!| MESH           |-->| MESH STRUCTURE
!| P              |<->| WORK STRUCTURE
!| Q              |<->| WORK STRUCTURE
!| R              |<->| WORK STRUCTURE
!| S              |<->| WORK STRUCTURE
!| T              |<->| WORK STRUCTURE
!| V              |<->| WORK STRUCTURE
!| X              |<--| VALEUR INITIALE, PUIS SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CGSTAB => CGSTAB
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: X,P,Q,R,S,T,V
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: AUX,A,B
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(SLVCFG)    , INTENT(INOUT) :: CFG
      LOGICAL         , INTENT(IN)    :: INFOGR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ALFA,ALFA1,BETA,BETA1,OMEG,OMEG1,OMEG2
      DOUBLE PRECISION XL,TESTL,RMRM,C
!
      INTEGER M
!
      LOGICAL RELAT,CROUT
!
      DOUBLE PRECISION, PARAMETER :: RMIN = 1.D-15
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!   INITIALISES
      CROUT=.FALSE.
      IF(7*(CFG%PRECON/7).EQ.CFG%PRECON) CROUT=.TRUE.
!
!-----------------------------------------------------------------------
!   INITIALISES
!-----------------------------------------------------------------------
!
      M   = 0
!
!  NORM OF THE SECOND MEMBER TO COMPUTE THE RELATIVE ACCURACY:
!
      XL = P_DOTS(B,B,MESH)
      IF (XL.LT.1.D0) THEN
        XL = 1.D0
        RELAT = .FALSE.
      ELSE
        RELAT = .TRUE.
      ENDIF
!
! IF THE SECOND MEMBER IS 0, X=0 AND IT'S THE END
!
      IF(SQRT(XL).LT.RMIN) THEN
        RMRM = 0.D0
        CALL OS( 'X=0     ' , X=X )
        GOTO 900
      ENDIF
!
! COMPUTES THE INITIAL RESIDUAL AND EXITS IF RESIDUAL IS SMALL:
!
      CALL MATRBL( 'X=AY    ',V,A,X,C,  MESH)
!
      CALL OS( 'X=Y-Z   ' , X=R , Y=B , Z=V )
      RMRM   = P_DOTS(R,R,MESH)
!
      IF (RMRM.LT.CFG%EPS**2*XL) GO TO 900
!
!-----------------------------------------------------------------------
! PRECONDITIONING :
!-----------------------------------------------------------------------
!
      IF(CROUT) THEN
!       COMPUTES C R  = B
        IF(NCSIZE.GT.1) THEN
          WRITE(LU,*) 'NO CROUT PRECONDITIONNING IN PARALLEL'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL DOWNUP(R, AUX , B , 'D' , MESH)
      ELSE
        CALL OS('X=Y     ', X=R, Y=B)
      ENDIF
!
!-----------------------------------------------------------------------
! RESUMES INITIALISATIONS
!-----------------------------------------------------------------------
!
      IF(CROUT) THEN
        IF(NCSIZE.GT.1) THEN
          WRITE(LU,*) 'NO CROUT PRECONDITIONNING IN PARALLEL'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL DOWNUP(V, AUX , V , 'D' , MESH)
      ENDIF
!
      CALL OS('X=X-Y   ', X=R, Y=V)
      CALL OS('X=Y     ', X=P, Y=R)
      CALL OS('X=0     ', X=V )
      CALL OS('X=0     ', X=Q )
!
      ALFA  = 1.D0
      BETA  = 1.D0
      OMEG1 = 1.D0
!
!-----------------------------------------------------------------------
!  ITERATIONS:
!-----------------------------------------------------------------------
!
2     CONTINUE
      M  = M  + 1
!
      BETA1 = P_DOTS(R,P,MESH)
      OMEG2 = OMEG1*BETA1/BETA
      OMEG  = OMEG2/ALFA
      BETA  = BETA1
!
      CALL OS('X=Y+CZ  ', X=Q, Y=R, Z=Q, C= OMEG )
      CALL OS('X=X+CY  ', X=Q, Y=V, Z=V, C=-OMEG2)
!
      CALL MATRBL( 'X=AY    ',V,A,Q,C,  MESH)
!
      IF(CROUT) THEN
        IF(NCSIZE.GT.1) THEN
          WRITE(LU,*) 'NO CROUT PRECONDITIONNING IN PARALLEL'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL DOWNUP(V, AUX , V , 'D' , MESH)
      ENDIF
!
      OMEG1 = P_DOTS(P,V,MESH)
      OMEG1 = BETA1/OMEG1
!
      CALL OS('X=Y+CZ  ', X=S, Y=R, Z=V, C=-OMEG1)
!
      CALL MATRBL( 'X=AY    ',T,A,S,C,  MESH)
!
      IF(CROUT) THEN
        IF(NCSIZE.GT.1) THEN
          WRITE(LU,*) 'NO CROUT PRECONDITIONNING IN PARALLEL'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL DOWNUP(T, AUX , T , 'D' , MESH)
      ENDIF
!
      ALFA  = P_DOTS(T,S,MESH)
      ALFA1 = P_DOTS(T,T,MESH)
      ALFA  = ALFA/ALFA1
!
      CALL OS('X=X+CY  ', X=X, Y=Q, C=OMEG1)
      CALL OS('X=X+CY  ', X=X, Y=S, C=ALFA )
!
      CALL OS('X=Y+CZ  ', X=R, Y=S, Z=T, C=-ALFA )
!
      RMRM   = P_DOTS(R,R,MESH)
!
! TESTS CONVERGENCE :
!
      IF (RMRM.LE.XL*CFG%EPS**2) GO TO 900
!
      IF(M.LT.CFG%NITMAX) GO TO 2
!
!-----------------------------------------------------------------------
!
!     IF(INFOGR) THEN
        TESTL = SQRT( RMRM / XL )
        IF(RELAT) THEN
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
        IF(RELAT) THEN
          WRITE(LU,103) M,TESTL
        ELSE
          WRITE(LU,203) M,TESTL
        ENDIF
      ENDIF
!
1000  RETURN
!
!-----------------------------------------------------------------------
!
!   FORMATS
!
103   FORMAT(1X,'CGSTAB (BIEF) : ',1I8,' ITERATIONS',
     &          ' RELATIVE PRECISION: ',G16.7)
203   FORMAT(1X,'CGSTAB (BIEF) : ',1I8,' ITERATIONS',
     &          ' ABSOLUTE PRECISION: ',G16.7)
104   FORMAT(1X,'CGSTAB (BIEF) : EXCEEDING MAXIMUM ITERATIONS ',1I8,
     &          ' RELATIVE PRECISION: ',G16.7)
204   FORMAT(1X,'CGSTAB (BIEF) : EXCEEDING MAXIMUM ITERATIONS',1I8,
     &          ' ABSOLUTE PRECISION:',G16.7)
!
!-----------------------------------------------------------------------
!
      END

