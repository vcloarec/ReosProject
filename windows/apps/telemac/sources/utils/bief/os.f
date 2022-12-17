!                   *************
                    SUBROUTINE OS
!                   *************
!
     & ( OP , X , Y , Z , C , IOPT , INFINI , ZERO )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON STRUCTURES.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   ON ARRAYS OR VECTORS :
!+
!+   OP = 'X=C     '     :  SETS X TO C
!+   OP = 'X=0     '     :  SETS X TO 0
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
!warning  OPERATIONS 1/Y AND Y/Z INTERNALLY TAKE CARE OF DIVISIONS
!+            BY 0. SUCCESSFUL EXIT OF OS IS THEREFORE NOT A PROOF THAT
!+            Y OR Z ARE NOT 0
!
!history  J-M HERVOUET (LNHE)
!+        18/08/05
!+        V5P6
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
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+        ADD MODASS=3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT
!| INFINI         |-->| PRESCRIBED VALUE IN CASE OF DIVISION BY 0.
!| IOPT           |-->| OPTION FOR DIVISIONS BY ZERO
!|                |   | 1: NO TEST DONE (WILL CRASH IF DIVISION BY 0.).
!|                |   | 2: INFINITE TERMS REPLACED BY CONSTANT INFINI.
!|                |   | 3: STOP IF DIVISION BY ZERO.
!|                |   | 4: DIVISIONS BY 0. REPLACED BY DIVISIONS/ZERO
!|                |   |    ZERO BEING AN OPTIONAL ARGUMENT
!| OP             |-->| STRING INDICATING THE OPERATION TO BE DONE
!| X              |<--| RESULT (A BIEF_OBJ STRUCTURE)
!| Y              |-->| TO BE USED IN THE OPERATION
!| Z              |-->| TO BE USED IN THE OPERATION
!| ZERO           |-->| A THRESHOLD MINIMUM VALUE FOR DIVISIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OS => OS
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     OPTIONAL ARGUMENTS
!
      INTEGER,          INTENT(IN), OPTIONAL :: IOPT
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: INFINI
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: ZERO
!
!     ARGUMENTS
!
      TYPE(BIEF_OBJ),   INTENT(INOUT), OPTIONAL, TARGET :: X
      TYPE(BIEF_OBJ),   INTENT(IN)   , OPTIONAL, TARGET :: Y,Z
      DOUBLE PRECISION, INTENT(IN)   , OPTIONAL :: C
      CHARACTER(LEN=8), INTENT(IN)              :: OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER IBL,TYPX,IDIM,N,NMAX
      LOGICAL YAY,YAZ,YAC
      TYPE(BIEF_OBJ), POINTER :: YY,ZZ
      DOUBLE PRECISION CC
!
!-----------------------------------------------------------------------
!
      TYPX = X%TYPE
!
      YAY=.FALSE.
      YAZ=.FALSE.
      YAC=.FALSE.
      IF(OP(3:3).EQ.'Y'.OR.OP(4:4).EQ.'Y'.OR.OP(5:5).EQ.'Y'.OR.
     &   OP(6:6).EQ.'Y'.OR.OP(7:7).EQ.'Y'.OR.OP(8:8).EQ.'Y') YAY=.TRUE.
      IF(OP(3:3).EQ.'Z'.OR.OP(4:4).EQ.'Z'.OR.OP(5:5).EQ.'Z'.OR.
     &   OP(6:6).EQ.'Z'.OR.OP(7:7).EQ.'Z'.OR.OP(8:8).EQ.'Z') YAZ=.TRUE.
!
!     CHECKS THAT CONSTANT C IS IN THE REQUIRED OPERATION
!     I.E. IF THERE IS C IN OP, EXCEPT WHEN IT IS X=COS(Y)
!
      IF((OP(3:3).EQ.'C'.AND.OP(4:4).NE.'O').OR.
     &    OP(4:4).EQ.'C'.OR.OP(5:5).EQ.'C'.OR.
     &    OP(6:6).EQ.'C'.OR.OP(7:7).EQ.'C'.OR.OP(8:8).EQ.'C') YAC=.TRUE.
!
      IF(PRESENT(C)) THEN
        CC=C
      ELSE
        IF(YAC) THEN
          WRITE(LU,2) OP
2         FORMAT(1X,'OS (BIEF) : C MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(YAY) THEN
        IF(PRESENT(Y)) THEN
          YY=>Y
        ELSE
          WRITE(LU,11) OP
11        FORMAT(1X,'OS (BIEF) : Y MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        YY=>X
      ENDIF
!
!     OPERATION WITH Y AND Z (IF THERE IS Z THERE SHOULD BE Y)
!
      IF(YAZ) THEN
!
        IF(PRESENT(Z)) THEN
!
        ZZ=>Z
!
!       COMPARES TYPES OF Y AND Z
!
        IF(.NOT.CMPOBJ(Y,Z)) THEN
          WRITE(LU,41) Y%NAME,Y%ELM,Z%NAME,Z%ELM
41        FORMAT(1X,'OS (BIEF) : DIFFERENT TYPES FOR ',A6,' (',1I2,
     &              ') AND ',A6,' (',1I2,')')
          CALL PLANTE(1)
          STOP
        ENDIF
!
        ELSE
!
          WRITE(LU,21) OP
21        FORMAT(1X,'OS (BIEF) : Z MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ELSE
        ZZ=>X
      ENDIF
!
!-----------------------------------------------------------------------
!     VECTORS
!-----------------------------------------------------------------------
!
      IF(TYPX.EQ.2) THEN
!
!     OPERATION WITH Y : Y IS CHECKED
!
        IF(YAY) THEN
!         DIFFERENT TYPES: X THEN TAKES ITS STRUCTURE FROM Y
          IF(.NOT.CMPOBJ(X,Y)) CALL CPSTVC(Y,X)
        ENDIF
!
!       CHECKS MEMORY
!
        IF(X%DIM1.GT.X%MAXDIM1) THEN
          WRITE(LU,101) X%NAME
101       FORMAT(1X,'OS (BIEF) : BEYOND ALLOWED MEMORY IN: ',A6)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(.NOT.PRESENT(IOPT)) THEN
!
        IF(X%DIM2.GT.1) THEN
!
          DO IDIM = 1 , X%DIM2
            CALL OV_2(OP,X%R,IDIM,YY%R,IDIM,
     &                            ZZ%R,IDIM,CC,X%MAXDIM1,X%DIM1)
          END DO
!
        ELSE
!
          IF ( MODASS .EQ.1 .OR. MODASS .EQ. 2) THEN
            CALL OV(OP,X%R,YY%R,ZZ%R,CC,X%DIM1)
          ELSEIF (MODASS .EQ. 3 .OR. MODASS .EQ. 4) THEN
            CALL OV_COMP(OP,X%R,YY%R,ZZ%R,CC,X%DIM1,
     &        X%E, YY%E , ZZ%E)
          ENDIF
!
        ENDIF
!
        ELSE
!
        IF(X%DIM2.GT.1) THEN
!
          DO IDIM = 1 , X%DIM2
            CALL OVD_2(OP,X%R,IDIM,YY%R,IDIM,ZZ%R,IDIM,CC,
     &                 X%MAXDIM1,X%DIM1,IOPT,INFINI,ZERO)
          END DO
!
        ELSE
!
          CALL OVD(OP,X%R,YY%R,ZZ%R,CC,X%DIM1,IOPT,INFINI,ZERO)
!
        ENDIF
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(TYPX.EQ.4) THEN
!
!-----------------------------------------------------------------------
!     BLOCKS
!-----------------------------------------------------------------------
!
        DO IBL = 1 , X%N
          IF(YAY) THEN
            IF(.NOT.CMPOBJ(X%ADR(IBL)%P,Y%ADR(IBL)%P)) THEN
              CALL CPSTVC(Y%ADR(IBL)%P,X%ADR(IBL)%P)
            ENDIF
          ENDIF
!
!         CHECKS MEMORY
!
          N = X%ADR(IBL)%P%DIM1
          NMAX = X%ADR(IBL)%P%MAXDIM1
          IF(N.GT.NMAX) THEN
            WRITE(LU,101) X%ADR(IBL)%P%NAME
            WRITE(LU,201) X%NAME
201         FORMAT(1X,'            THIS VECTOR IS IN BLOCK: ',A6)
            CALL PLANTE(1)
            STOP
          ENDIF
!
          IF(.NOT.PRESENT(IOPT)) THEN
!
          IF(X%ADR(IBL)%P%DIM2.GT.1) THEN
!
          DO IDIM = 1 , X%ADR(IBL)%P%DIM2
            CALL OV_2(OP,X%ADR(IBL)%P%R,IDIM,
     &                  YY%ADR(IBL)%P%R,IDIM,
     &                  ZZ%ADR(IBL)%P%R,IDIM, CC , NMAX , N )
          END DO
!
          ELSE
!
            CALL OV(OP,X%ADR(IBL)%P%R,
     &                YY%ADR(IBL)%P%R,
     &                ZZ%ADR(IBL)%P%R, CC , N )
!
          ENDIF
!
          ELSE
!
          IF(X%ADR(IBL)%P%DIM2.GT.1) THEN
!
          DO IDIM = 1 , X%ADR(IBL)%P%DIM2
            CALL OVD_2(OP,X%ADR(IBL)%P%R,IDIM,
     &                   YY%ADR(IBL)%P%R,IDIM,
     &                   ZZ%ADR(IBL)%P%R,IDIM, CC , NMAX , N ,
     &                   IOPT,INFINI,ZERO)
          END DO
!
          ELSE
!
            CALL OVD(OP,X%ADR(IBL)%P%R,
     &                 YY%ADR(IBL)%P%R,
     &                 ZZ%ADR(IBL)%P%R, CC , N ,IOPT,INFINI,ZERO)
!
          ENDIF
!
          ENDIF
!
!
        ENDDO ! IBL
!
!-----------------------------------------------------------------------
!
!     ERROR OR OBJECT NOT TREATED
!
      ELSE
!
        WRITE(LU,1001) X%TYPE,X%NAME
1001    FORMAT(1X,'OS (BIEF): OBJECT TYPE NOT IMPLEMENTED: ',1I3,/,
     &         1X,'NAME: ',1A6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
