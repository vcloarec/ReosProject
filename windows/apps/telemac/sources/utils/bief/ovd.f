!                   **************
                    SUBROUTINE OVD
!                   **************
!
     & ( OP , X , Y , Z , C , NPOIN , IOPT , D , EPS )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON VECTORS INCLUDING DIVISIONS
!+                DIVISION BY 0 CAN BE TESTED.
!+
!+            IN THE EVENT OF A DIVIDE CHECK, CAN EITHER STOP THE
!+                PROGRAM OR SET THE RESULT OF THE OPERATION TO
!+                A VALUE: D.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   OP = 'X=1/Y   '     :  COPIES INVERSE OF Y IN X
!+   OP = 'X=C/Y   '     :  DIVIDES C BY Y
!+   OP = 'X=Y/Z   '     :  DIVIDES Y BY Z
!+   OP = 'X=CY/Z  '     :  DIVIDES C.Y BY Z
!+   OP = 'X=CXY/Z '     :  DIVIDES C.X.Y BY Z
!+   OP = 'X=X+CY/Z'     :  ADDS C.Y/Z TO X
!
!warning  DIVIDE OPERATIONS INTERNALLY TAKE CARE OF DIVISIONS BY 0.
!+            SUCCESSFUL EXIT OF OVD IS THEREFORE NOT A PROOF THAT Y
!+            OR Z NEVER ARE 0
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        26/11/93
!+        V5P2
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
!| C              |-->| A GIVEN CONSTANT
!| D              |-->| A DIAGONAL MATRIX
!| EPS            |-->| THRESHOLD TO AVOID DIVISIONS BY ZERO
!| IOPT           |-->| OPTION FOR DIVISIONS BY ZERO
!|                |   | 1: NO TEST DONE (WILL CRASH IF DIVISION BY 0.).
!|                |   | 2: INFINITE TERMS REPLACED BY CONSTANT INFINI.
!|                |   | 3: STOP IF DIVISION BY ZERO.
!|                |   | 4: DIVISIONS BY 0. REPLACED BY DIVISIONS/ZERO
!|                |   |    ZERO BEING AN OPTIONAL ARGUMENT
!| NPOIN          |-->| SIZE OF VECTORS
!| OP             |-->| STRING INDICATING THE OPERATION TO BE DONE
!| X              |<--| RESULTING VECTOR
!| Y              |-->| TO BE USED IN THE OPERATION
!| Z              |-->| TO BE USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN,IOPT
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN),Z(NPOIN),C,D,EPS
      CHARACTER(LEN=8), INTENT(IN)    :: OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=1/Y   ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO I=1,NPOIN
            X(I) = 1.D0/Y(I)
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSE
            X(I) = D
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSE
            WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.4) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSEIF (Y(I).GE.0.D0) THEN
            X(I) =  1.D0/EPS
          ELSE
            X(I) = -1.D0/EPS
          ENDIF
!
        ENDDO ! I
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=C/Y   ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO I=1,NPOIN
            X(I) = C/Y(I)
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = C/Y(I)
          ELSE
            X(I) = D
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = C/Y(I)
          ELSE
            WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.4) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = C/Y(I)
          ELSEIF (Y(I).GE.0.D0) THEN
            X(I) =  1.D0/EPS
          ELSE
            X(I) = -1.D0/EPS
          ENDIF
!
        ENDDO ! I
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=Y/Z   ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO I=1,NPOIN
            X(I) = Y(I) / Z(I)
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSE
            WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO ! I
        ELSEIF(IOPT.EQ.4) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSEIF (ABS(Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  Y(I) / EPS
          ELSE
            X(I) = -Y(I) / EPS
          ENDIF
!
        ENDDO ! I
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CY/Z  ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO I=1,NPOIN
            X(I) = C*Y(I) / Z(I)
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSE
            WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.4) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSEIF (ABS(C*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  C*Y(I) / EPS
          ELSE
            X(I) = -C*Y(I) / EPS
          ENDIF
!
        ENDDO ! I
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CXY/Z ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO I=1,NPOIN
            X(I) = C*X(I)*Y(I) / Z(I)
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSE
            WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.4) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSEIF (ABS(C*X(I)*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  C*Y(I) / EPS
          ELSE
            X(I) = -C*Y(I) / EPS
          ENDIF
!
        ENDDO ! I
!
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CY/Z') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO I=1,NPOIN
            X(I) = X(I) + C * Y(I) / Z(I)
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C * Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C * Y(I) / Z(I)
          ELSE
            WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ENDDO ! I
!
        ELSEIF(IOPT.EQ.4) THEN
!
        DO I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C*Y(I) / Z(I)
          ELSEIF (ABS(C*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) = X(I) + C*Y(I) / EPS
          ELSE
            X(I) = X(I) - C*Y(I) / EPS
          ENDIF
!
        ENDDO ! I
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,4000) OP
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
2000  FORMAT(1X,'OVD (BIEF) : DIVIDE BY ZERO AT POINT ',1I6,
     &          ' FOR OPERATION ',A8,/,1X,
     &          'THE CRITERION IS ',G16.7)
4000  FORMAT(1X,'OVD (BIEF) : UNKNOWN OPERATION: ',A8)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
