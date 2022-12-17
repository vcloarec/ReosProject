!                   *****************
                    SUBROUTINE EQUNOR
!                   *****************
!
     &(X, A,B , MESH, D,AD,AG,G,R, CFG,INFOGR,AUX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE LINEAR SYSTEM A X = B
!+                USING METHODS OF THE TYPE CONJUGATE GRADIENT.
!code
!+-----------------------------------------------------------------------
!+                        PRECONDITIONING
!+-----------------------------------------------------------------------
!+    PRECON VALUE     I                  MEANING
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
!+        24/04/97
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
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX OF THE SYSTEM
!| AD             |<->| WORK ARRAY: MATRICE A MULTIPLIED BY D.
!| AG             |<->| WORK ARRAY: A MULTIPLIED BY DESCENT GRADIENT
!| AUX            |-->| MATRIX FOR PRECONDITIONING.
!| B              |-->| RIGHT-HAND SIDE OF THE SYSTEM
!| CFG            |-->| STRUCTURE OF SOLVER CONFIGURATION
!| D              |<->| WORK ARRAY: DIRECTION OF DESCENT.
!| G              |<->| DESCENT GRADIENT.
!| INFOGR         |-->| IF YES, PRINT A LOG.
!| MESH           |-->| MESH STRUCTURE.
!| R              |<->| RESIDUAL (MAY BE IN THE SAME MEMORY SPACE AS
!|                |   | GRADIENT DEPENDING ON CONDITIONING)
!| X              |<--| INITIAL VALUE, THEN SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_EQUNOR => EQUNOR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SLVCFG), INTENT(INOUT)    :: CFG
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: B
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: D,AD,G,AG,R,X
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: AUX
      LOGICAL, INTENT(IN)            :: INFOGR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER M
!
      DOUBLE PRECISION XL,RMRM,TESTL
      DOUBLE PRECISION BETA,ADAD,RO
      DOUBLE PRECISION TGMTGM,TG1TG1,C
!
      LOGICAL RELAT,PREC,CROUT,GSEB
!
!-----------------------------------------------------------------------
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!   INITIALISES
!
      CROUT =.FALSE.
      IF(7*(CFG%PRECON/7).EQ.CFG%PRECON) CROUT=.TRUE.
      GSEB=.FALSE.
      IF(11*(CFG%PRECON/11).EQ.CFG%PRECON) GSEB=.TRUE.
      PREC=.FALSE.
      IF(CROUT.OR.GSEB.OR.13*(CFG%PRECON/13).EQ.CFG%PRECON) PREC=.TRUE.
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
!
      IF (RMRM.LT.CFG%EPS**2*XL) GO TO 900
!
!-----------------------------------------------------------------------
! PRECONDITIONING :
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
!
!       COMPUTES C G0 = R
        CALL DOWNUP(G, AUX , R , 'D' , MESH)
!
!  C IS HERE CONSIDERED SYMMETRICAL,
!  SHOULD OTHERWISE SOLVE TC GPRIM = G
!
!        T -1
!         C   G   IS PUT IN B
!
        CALL DOWNUP(B , AUX , G , 'T' , MESH)
!
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTES THE DIRECTION OF INITIAL DESCENT:
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
        CALL MATRBL( 'X=TAY   ',D,A,B,  C,MESH)
      ELSE
        CALL MATRBL( 'X=TAY   ',D,A,G,  C,MESH)
      ENDIF
!
      TGMTGM = P_DOTS(D,D,MESH)
!
!-----------------------------------------------------------------------
! COMPUTES THE INITIAL PRODUCT A D:
!-----------------------------------------------------------------------
!
      CALL MATRBL('X=AY    ',AD,A,D,C,MESH)
!
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
!
!         COMPUTES  C DPRIM = AD  (DPRIM PUT IN AG)
          CALL DOWNUP(AG, AUX , AD , 'D' , MESH)
!
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTES INITIAL RO :
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
        ADAD = P_DOTS(AG,AG,MESH)
      ELSE
        ADAD = P_DOTS(AD,AD,MESH)
      ENDIF
      RO = TGMTGM/ADAD
!
!-----------------------------------------------------------------------
!
! COMPUTES X1 = X0 - RO  * D
!
      CALL OS('X=X+CY  ', X=X, Y=D, C=-RO)
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
      CALL OS('X=X+CY  ', X=R, Y=AD, C=-RO)
!
!  SOME VALUES WILL CHANGE IN CASE OF PRECONDITIONING
!
      RMRM   = P_DOTS(R,R,MESH)
!
! CHECKS END :
!
      IF (RMRM.LE.XL*CFG%EPS**2) GO TO 900
!
!-----------------------------------------------------------------------
! PRECONDITIONING : SOLVES C G = R
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
!
!         UPDATES G BY RECURRENCE (IN AG: DPRIM)
          CALL OS('X=X+CY  ', X=G, Y=AG, C=-RO)
!
          CALL DOWNUP(B , AUX , G , 'T' , MESH)
!
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTES D BY RECURRENCE:
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
!                                          T  T -1          T -1
!                               AD IS HERE  A  C  G    B IS  C   G
        CALL MATRBL( 'X=TAY   ',AD,A,B,  C,MESH)
      ELSE
!                               AD IS HERE TAG
        CALL MATRBL( 'X=TAY   ',AD,A,G,  C,MESH)
      ENDIF
!
      TG1TG1 = TGMTGM
      TGMTGM = P_DOTS(AD,AD,MESH)
      BETA = TGMTGM / TG1TG1
!
      CALL OS('X=CX    ', X=D, C=BETA)
!
!                               AD IS HERE TAG
      CALL OS('X=X+Y   ', X=D, Y=AD)
!
!-----------------------------------------------------------------------
! COMPUTES A D :
!-----------------------------------------------------------------------
!
      CALL MATRBL( 'X=AY    ',AD,A,D,  C,MESH)
!
      IF(PREC) THEN
!
!           COMPUTES  C DPRIM = AD  (DPRIM PUT IN AG)
            CALL DOWNUP(AG , AUX , AD , 'D' , MESH)
!
      ENDIF
!
!-----------------------------------------------------------------------
! COMPUTES RO
!-----------------------------------------------------------------------
!
      IF(PREC) THEN
        ADAD = P_DOTS(AG,AG,MESH)
      ELSE
        ADAD = P_DOTS(AD,AD,MESH)
      ENDIF
      RO = TGMTGM/ADAD
!
! COMPUTES X(M) = X(M-1) - RO * D
!
      CALL OS('X=X+CY  ', X=X, Y=D, C=-RO)
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
1000  RETURN
!
!-----------------------------------------------------------------------
!
!   FORMATS
!
102   FORMAT(1X,'EQUNOR (BIEF) : ',
     &                     1I8,' ITERATIONS, RELATIVE PRECISION:',G16.7)
202   FORMAT(1X,'EQUNOR (BIEF) : ',
     &                     1I8,' ITERATIONS, ABSOLUTE PRECISION:',G16.7)
104   FORMAT(1X,'EQUNOR (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' RELATIVE PRECISION:',G16.7)
204   FORMAT(1X,'EQUNOR (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' ABSOLUTE PRECISON:',G16.7)
!
!-----------------------------------------------------------------------
!
      END
