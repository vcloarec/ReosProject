!                   ******************
                    SUBROUTINE OV_COMP
!                   ******************
!
     & ( OP , X , Y , Z , C , NPOIN,
     &   X_ERR , Y_ERR , Z_ERR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON VECTORS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   OP = 'X=C     '     :  SETS X TO C
!+   OP = 'X=Y     '     :  COPIES Y IN X
!+   OP = 'X=+Y    '     :  IDEM
!+   OP = 'X=-Y    '     :  COPIES -Y IN X
!+   OP = 'X=1/Y   '     :  COPIES INVERSE OF Y IN X
!+   OP = 'X=Y+Z   '     :  ADDS Y AND Z
!+   OP = 'X=Y-Z   '     :  SUBTRACTS Z TO Y
!+   OP = 'X=YZ    '     :  Y.Z
!+   OP = 'X=-YZ   '     :  -Y.Z
!+   OP = 'X=XY    '     :  X.Y
!+   OP = 'X=X+YZ  '     :  ADDS Y.Z TO X
!+   OP = 'X=X-YZ  '     :  SUBTRACTS Y.Z FROM X
!+   OP = 'X=CXY   '     :  C.X.Y
!+   OP = 'X=CYZ   '     :  C.Y.Z
!+   OP = 'X=CXYZ  '     :  C.X.Y.Z
!+   OP = 'X=X+CYZ '     :  ADDS C.Y.Z TO X
!+   OP = 'X=Y/Z   '     :  DIVIDES Y BY Z
!+   OP = 'X=CY/Z  '     :  DIVIDES C.Y BY Z
!+   OP = 'X=CXY/Z '     :  DIVIDES C.X.Y BY Z
!+   OP = 'X=X+CY/Z'     :  ADDS C.Y/Z TO X
!+   OP = 'X=X+Y   '     :  ADDS Y TO X
!+   OP = 'X=X-Y   '     :  SUBTRACTS Y FROM X
!+   OP = 'X=CX    '     :  MULTIPLIES X BY C
!+   OP = 'X=CY    '     :  MULTIPLIES Y BY C
!+   OP = 'X=Y+CZ  '     :  ADDS C.Z TO Y
!+   OP = 'X=X+CY  '     :  ADDS C.Y TO X
!+   OP = 'X=SQR(Y)'     :  SQUARE ROOT OF Y
!+   OP = 'X=ABS(Y)'     :  ABSOLUTE VALUE OF Y
!+   OP = 'X=N(Y,Z)'     :  NORM OF THE VECTOR WITH COMPONENTS Y AND Z
!+   OP = 'X=Y+C   '     :  ADDS C TO Y
!+   OP = 'X=X+C   '     :  ADDS C TO X
!+   OP = 'X=Y**C  '     :  Y TO THE POWER C
!+   OP = 'X=COS(Y)'     :  COSINE OF Y
!+   OP = 'X=SIN(Y)'     :  SINE OF Y
!+   OP = 'X=ATN(Y)'     :  ARC TANGENT OF Y
!+   OP = 'X=A(Y,Z)'     :  ATAN2(Y,Z)
!+   OP = 'X=+(Y,C)'     :  MAXIMUM OF Y AND C
!+   OP = 'X=-(Y,C)'     :  MINIMUM OF Y AND C
!+   OP = 'X=+(Y,Z)'     :  MAXIMUM OF Y AND Z
!+   OP = 'X=-(Y,Z)'     :  MINIMUM OF Y AND Z
!+   OP = 'X=YIFZ<C'     :  COPIES Y IN X IF Z < C , FOR EACH POINT
!+   OP = 'X=C(Y-Z)'     :  MULTIPLIES C BY (Y-Z)
!
!warning  DIVIDE OPERATIONS INTERNALLY TAKE CARE OF DIVISIONS BY 0.
!+            SUCCESSFUL EXIT OF OV IS THEREFORE NOT A PROOF THAT Y OR Z
!+            NEVER ARE 0. FOR SUCH OPERATIONS, PREFERABLY USE OVD
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        17/03/2010
!+        V6P0
!+   ORIGINAL IDEA IN ULYSSE. THANK YOU D. LAURENCE
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
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN),Z(*),C
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION,OPTIONAL,INTENT(IN):: Y_ERR(NPOIN),Z_ERR(NPOIN)
      DOUBLE PRECISION,OPTIONAL, INTENT(INOUT) :: X_ERR(NPOIN)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION ERROR,ERROR1,TEMP(NPOIN),TEST
      DOUBLE PRECISION TMP
!
      INTRINSIC SQRT,ABS,COS,SIN,ATAN,MAX,MIN
!
!-----------------------------------------------------------------------
!
      TEMP=0.D0
      ERROR=0.D0
      ERROR1=0.D0
      SELECT CASE(OP(3:8))
!
!-----------------------------------------------------------------------
!
        CASE('C     ')
!
        DO I=1,NPOIN
          X(I) = C
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('0     ')
!
        DO I=1,NPOIN
          X(I) = 0.D0
          IF (PRESENT (X_ERR)) THEN
            X_ERR(I)=0.D0
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('Y     ')
!
        DO I=1,NPOIN
          X(I) = Y(I)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR))THEN
            X_ERR(I) = Y_ERR(I)
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('+Y    ')
!
        DO I=1,NPOIN
          X(I) = Y(I)
          IF (PRESENT (X_ERR).AND. PRESENT (Y_ERR)) THEN
            X_ERR(I)=Y_ERR(I)
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('-Y    ')
!
        DO I=1,NPOIN
          X(I) = - Y(I)
          IF (PRESENT (X_ERR).AND. PRESENT (Y_ERR)) THEN
            X_ERR(I)=-Y_ERR(I)
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('1/Y   ')
!
        DO I=1,NPOIN
          X(I) = 1.D0/Y(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('Y+Z   ')
!
        DO I=1,NPOIN
          CALL TWOSUM(Y(I),Z(I),X(I),ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR)
     &      .AND. PRESENT (Z_ERR)) THEN
            X_ERR(I)=(Y_ERR(I)+Z_ERR(I))+ERROR
          ENDIF
          !X(I) = Y(I) + Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('Y-Z   ')
!
        DO I=1,NPOIN
          CALL TWOSUM(Y(I),-Z(I),X(I),ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR)
     &      .AND. PRESENT (Z_ERR)) THEN
            X_ERR(I)=(Y_ERR(I)-Z_ERR(I))+ERROR
          ENDIF
          !X(I) = Y(I) - Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('YZ    ')
!
        DO I=1,NPOIN
          CALL TWOPROD(Y(I),Z(I),X(I),ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR)
     &      .AND. PRESENT (Z_ERR)) THEN
            X_ERR(I)=(Y(I) * Z_ERR(I))+(Y_ERR(I) * Z(I))
     &               +(Y_ERR(I) * Z_ERR(I))
            X_ERR(I)=X_ERR(I)+ ERROR
          ENDIF
          !X(I) = Y(I) * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('-YZ   ')
!
        DO I=1,NPOIN
          CALL TWOPROD(-Y(I),Z(I),X(I),ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR)
     &      .AND. PRESENT (Z_ERR)) THEN
            X_ERR(I)=(-Y(I) * Z_ERR(I))+(-Y_ERR(I) * Z(I))
            X_ERR(I)=X_ERR(I)+ ERROR
          ENDIF
          !X(I) = -Y(I) * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('XY    ')
!
        DO I=1,NPOIN
          TEST=0.D0
          CALL TWOPROD(X(I),Y(I),TEST,ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR)) THEN
            X_ERR(I)=(X(I) * Y_ERR(I))+(X_ERR(I) * Y(I))
     &               +(X_ERR(I) * Y_ERR(I))
            X_ERR(I)=X_ERR(I)+ ERROR
          ENDIF
          X(I) = TEST
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X+YZ  ')
!
        DO I=1,NPOIN
          CALL TWOPROD(Y(I),Z(I),TEMP(I),ERROR1)
          TMP = X(I)
          CALL TWOSUM(TMP,TEMP(I),X(I),ERROR)
          IF (PRESENT (X_ERR).AND. PRESENT (Y_ERR)
     &       .AND. PRESENT (Z_ERR))THEN
            X_ERR(I)=X_ERR(I)+(Y(I) * Z_ERR(I))
     &               +(Y_ERR(I) * Z(I))
            X_ERR(I)=X_ERR(I)+ ERROR1
            X_ERR(I)=X_ERR(I)+ERROR
          ENDIF
          !X(I) = X(I) + Y(I) * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X-YZ  ')
!
        DO I=1,NPOIN
          CALL TWOPROD(Y(I),Z(I),TEMP(I),ERROR1)
          TMP = X(I)
          CALL TWOSUM(TMP,-TEMP(I),X(I),ERROR)
          IF (PRESENT (X_ERR).AND. PRESENT (Y_ERR)
     &       .AND. PRESENT (Z_ERR))THEN
            TEMP(I)=(Y(I) * Z_ERR(I))
     &              +(Y_ERR(I) * Z(I))+(Y_ERR(I) * Z_ERR(I))
            TEMP(I)=TEMP(I)+ERROR1
            X_ERR(I)=(X_ERR(I)-TEMP(I))+ERROR
          ENDIF
          !X(I) = X(I) - Y(I) * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('CXY   ')
!
        DO I=1,NPOIN
          X(I) = C * X(I) * Y(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('CYZ   ')
!
        DO I=1,NPOIN
          CALL TWOPROD(C*Y(I),Z(I),X(I),ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR)
     &      .AND. PRESENT (Z_ERR)) THEN
            X_ERR(I)= (C*Y(I) * Z_ERR(I))
     &               +(C*Y_ERR(I) * Z(I))+(C*Y_ERR(I) * Z_ERR(I))
            X_ERR(I)=  X_ERR(I)+ERROR
          ENDIF
          !X(I) = C* Y(I) * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('CXYZ  ')
!
        DO I=1,NPOIN
          X(I) = C * X(I) * Y(I) * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X+CYZ ')
!
        DO I=1,NPOIN
          X(I) = X(I) + C * Y(I) * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('Y/Z   ')
!
        DO I=1,NPOIN
          X(I) = Y(I) / Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('CY/Z  ')
!
        DO I=1,NPOIN
          X(I) = C*Y(I) / Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('CXY/Z ')
!
        DO I=1,NPOIN
          X(I) = C*X(I)*Y(I) / Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X+CY/Z')
!
        DO I=1,NPOIN
          X(I) = X(I) + C * Y(I) / Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X+Y   ')
!
        DO I=1,NPOIN
          TMP = X(I)
          CALL TWOSUM(TMP,Y(I),X(I),ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR))THEN
            X_ERR(I)=(X_ERR(I)+Y_ERR(I))+ERROR
          ENDIF
          !X(I) = X(I) + Y(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X-Y   ')
        DO I=1,NPOIN
          TMP = X(I)
          CALL TWOSUM(TMP,-Y(I),X(I),ERROR)
          IF (PRESENT (X_ERR))THEN
            X_ERR(I)=(X_ERR(I)-Y_ERR(I))+ERROR
          ENDIF
          !X(I) = X(I) - Y(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('CX    ')
!
        DO I=1,NPOIN
          X(I) = C * X(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('CY    ')
!
        DO I=1,NPOIN
          CALL TWOPROD(C,Y(I),X(I),ERROR)
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR)) THEN
            X_ERR(I)=(C * Y_ERR(I))
            X_ERR(I)=X_ERR(I)+ERROR
          ENDIF
          !X(I) = C * Y(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('Y+CZ  ')
!
        DO I=1,NPOIN
          X(I) = Y(I) + C * Z(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X+CY  ')
!
        DO I=1,NPOIN
          CALL TWOPROD(C,Y(I),TEMP(I),ERROR1)
          TMP = X(I)
          CALL TWOSUM(TMP,TEMP(I),X(I),ERROR)
          IF (PRESENT (X_ERR).AND. PRESENT (Y_ERR))THEN
            X_ERR(I)=X_ERR(I)+(C* Y_ERR(I))
            ERROR=ERROR + ERROR1
            X_ERR(I)=X_ERR(I)+ERROR
          ENDIF
          !X(I) = X(I) + C * Y(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('SQR(Y)')
!
        DO I=1,NPOIN
          X(I) = SQRT(Y(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('ABS(Y)')
!
        DO I=1,NPOIN
          X(I) = ABS(Y(I))
          IF (PRESENT (X_ERR) .AND. PRESENT (Y_ERR))THEN
            X_ERR(I) = ABS(Y_ERR(I))
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('N(Y,Z)')
!
        DO I=1,NPOIN
          X(I) = SQRT( Y(I)**2 + Z(I)**2 )
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('Y+C   ')
!
        DO I=1,NPOIN
          X(I) = Y(I) + C
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('X+C   ')
!
        DO I=1,NPOIN
          X(I) = X(I) + C
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('Y**C  ')
!
        DO I=1,NPOIN
          IF(Y(I).GE.0.D0) THEN
            X(I) = Y(I)**C
          ELSE
            WRITE(LU,101)
101         FORMAT(1X,'OV (BIEF): Y**C FORBIDDEN IF Y < 0')
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('COS(Y)')
!
        DO I=1,NPOIN
          X(I) = COS(Y(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('SIN(Y)')
!
        DO I=1,NPOIN
          X(I) = SIN(Y(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('ATN(Y)')
!
        DO I=1,NPOIN
          X(I) = ATAN(Y(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('A(Y,Z)')
!
        DO I=1,NPOIN
          X(I) = ATAN2(Y(I),Z(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('+(Y,C)')
!
        DO I=1,NPOIN
          X(I) = MAX(Y(I),C)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('-(Y,C)')
!
        DO I=1,NPOIN
          X(I) = MIN(Y(I),C)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('+(Y,Z)')
!
        DO I=1,NPOIN
          X(I) = MAX(Y(I),Z(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('-(Y,Z)')
!
        DO I=1,NPOIN
          X(I) = MIN(Y(I),Z(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('YIFZ<C')
!
        DO I=1,NPOIN
          IF ( Z(I).LT.C ) X(I) = Y(I)
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE('C(Y-Z)')
!
        DO I=1,NPOIN
          X(I) = C*(Y(I)-Z(I))
        ENDDO
!
!-----------------------------------------------------------------------
!
        CASE DEFAULT
!
          WRITE(LU,1001) OP
1001      FORMAT(1X,'OV (BIEF) : UNKNOWN OPERATION: ',A8)
          CALL PLANTE(1)
          STOP
!
!-----------------------------------------------------------------------
!
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
