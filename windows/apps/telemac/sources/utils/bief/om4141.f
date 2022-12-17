!                   *****************
                    SUBROUTINE OM4141
!                   *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & IKLE,NELEM,NELMAX,NDIAG,STOX)
!
!***********************************************************************
! BIEF   V7P3
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES WITH P1 PRISMS
!+                OR ANY OTHER ELEMENT WITH THE SAME NUMBER OF POINTS.
!code
!+   D: DIAGONAL MATRIX
!+   C: CONSTANT
!+
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON MATRICES M AND N, D AND C.
!+
!+   THE RESULT IS MATRIX M.
!+
!+      OP = 'M=N     '  : COPIES N IN M
!+      OP = 'M=CN    '  : MULTIPLIES N BY C
!+      OP = 'M=CM    '  : MULTIPLIES M BY C
!+      OP = 'M=M+CN  '  : ADDS CN TO M
!+      OP = 'M=M+N   '  : ADDS N TO M
!+      OP = 'M=MD    '  : M X D
!+      OP = 'M=DM    '  : D X M
!+      OP = 'M=DMD   '  : D X M X D
!+      OP = 'M=0     '  : SETS M TO 0
!+      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!+                         (OLD MATSNS)
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!+                         (OLD MASKEX)
!+                         THE MASK IS TAKEN FROM D
!+      OP = 'M=M+D   '  : ADDS D TO M
!
!code
!+  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!+
!+     XA(IELEM, 1)  ---->  A(1,2)
!+     XA(IELEM, 2)  ---->  A(1,3)
!+     XA(IELEM, 3)  ---->  A(1,4)
!+     XA(IELEM, 4)  ---->  A(1,5)
!+     XA(IELEM, 5)  ---->  A(1,6)
!+     XA(IELEM, 6)  ---->  A(2,3)
!+     XA(IELEM, 7)  ---->  A(2,4)
!+     XA(IELEM, 8)  ---->  A(2,5)
!+     XA(IELEM, 9)  ---->  A(2,6)
!+     XA(IELEM,10)  ---->  A(3,4)
!+     XA(IELEM,11)  ---->  A(3,5)
!+     XA(IELEM,12)  ---->  A(3,6)
!+     XA(IELEM,13)  ---->  A(4,5)
!+     XA(IELEM,14)  ---->  A(4,6)
!+     XA(IELEM,15)  ---->  A(5,6)
!+     XA(IELEM,16)  ---->  A(2,1)
!+     XA(IELEM,17)  ---->  A(3,1)
!+     XA(IELEM,18)  ---->  A(4,1)
!+     XA(IELEM,19)  ---->  A(5,1)
!+     XA(IELEM,20)  ---->  A(6,1)
!+     XA(IELEM,21)  ---->  A(3,2)
!+     XA(IELEM,22)  ---->  A(4,2)
!+     XA(IELEM,23)  ---->  A(5,2)
!+     XA(IELEM,24)  ---->  A(6,2)
!+     XA(IELEM,25)  ---->  A(4,3)
!+     XA(IELEM,26)  ---->  A(5,3)
!+     XA(IELEM,27)  ---->  A(6,3)
!+     XA(IELEM,28)  ---->  A(5,4)
!+     XA(IELEM,29)  ---->  A(6,4)
!+     XA(IELEM,30)  ---->  A(6,5)
!
!history  J-M HERVOUET (LNHE) ; F  LEPEINTRE (LNH)
!+        09/07/2008
!+        V5P9
!+   First version dates back to a long time ago...
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
!+        11/09/2017
!+        V7P3
!+   Adding the argument STOX
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| D              |-->| A DIAGONAL MATRIX
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| STOX           |-->| STORAGE MODE OF OFF-DIAGONAL TERMS
!|                |   | 1: XM(NELMAX,*) 2: XM(*,NELMAX)
!| TYPDIM         |<->| TYPE OF DIAGONAL OF M:
!|                |   | TYPDIM = 'Q' : ANY VALUE
!|                |   | TYPDIM = 'I' : IDENTITY
!|                |   | TYPDIM = '0' : ZERO
!| TYPDIN         |<->| TYPE OF DIAGONAL OF N:
!|                |   | TYPDIN = 'Q' : ANY VALUE
!|                |   | TYPDIN = 'I' : IDENTITY
!|                |   | TYPDIN = '0' : ZERO
!| TYPEXM         |-->| TYPE OF OFF-DIAGONAL TERMS OF M:
!|                |   | TYPEXM = 'Q' : ANY VALUE
!|                |   | TYPEXM = 'S' : SYMMETRIC
!|                |   | TYPEXM = '0' : ZERO
!| TYPEXN         |-->| TYPE OF OFF-DIAGONAL TERMS OF N:
!|                |   | TYPEXN = 'Q' : ANY VALUE
!|                |   | TYPEXN = 'S' : SYMMETRIC
!|                |   | TYPEXN = '0' : ZERO
!| XM             |-->| OFF-DIAGONAL TERMS OF M
!| XN             |-->| OFF-DIAGONAL TERMS OF N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OM4141 => OM4141
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NDIAG,STOX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*),XN(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IELEM,I1,I2,I3,I4,I5,I6
!
!-----------------------------------------------------------------------
!
!     CHECKING THE OFF-DIAGONAL STORAGE...
!
      IF(STOX.EQ.2.AND.OP(3:8).NE.'N     ') THEN
        WRITE(LU,*) 'OM4141 (BIEF): OFF-DIAGONAL STORAGE 2'
        WRITE(LU,*) '               NOT IMPLEMENTED'
        WRITE(LU,*) '               WITH OPERATION ',OP
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(OP(3:8).EQ.'N     ') THEN
!
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OV('X=Y     ', X=DM, Y=DN, DIM1=NDIAG)
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
!         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
          WRITE(LU,6) TYPDIN(1:1)
6         FORMAT(1X,'OM4141 (BIEF) : TYPDIN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          IF(STOX.EQ.2) THEN
!           SLICING THE COPY WOULD MAKE TOO MANY LOOPS, THE WHOLE COPIED
!           EVEN IF NELMAX MAY BE LARGER THAN NELEM
            CALL OV('X=Y     ', X=XM, Y=XN, DIM1=15*NELMAX)
          ELSE
            DO I=1,15
              CALL OV('X=Y     ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
            ENDDO
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(STOX.EQ.2) THEN
!           SLICING THE COPY WOULD MAKE TOO MANY LOOPS, THE WHOLE COPIED
!           EVEN IF NELMAX MAY BE LARGER THAN NELEM
            CALL OV('X=Y     ' , X=XM, Y=XN, DIM1=30*NELMAX)
          ELSE
            DO I=1,30
              CALL OV( 'X=Y     ' , X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
            ENDDO
          ENDIF
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,11) TYPEXN(1:1)
11        FORMAT(1X,'OM4141 (BIEF) : TYPEXN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
!
        CALL OV('X=Y     ', X=DM, Y=DN, DIM1=NDIAG)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV('X=Y     ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,15
            CALL OV('X=Y     ', X=XM(1,I), Y=XN(1,I+15), DIM1=NELEM)
            CALL OV('X=Y     ', X=XM(1,I+15), Y=XN(1,I), DIM1=NELEM)
          ENDDO
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'CN    ') THEN
!
        CALL OV('X=CY    ', X=DM, Y=DN, C=C, DIM1=NDIAG)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV('X=CY    ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,30
            CALL OV('X=CY    ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'CM    ') THEN
!
        CALL OV('X=CX    ', X=DM, C=C, DIM1=NDIAG)
!
        IF(TYPEXM(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV('X=CX    ', X=XM(1,I),C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,30
            CALL OV('X=CX    ', X=XM(1,I),C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,11) TYPEXM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+CN  ') THEN
!
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV('X=X+C   ', X=DM, C=C, DIM1=NDIAG)
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV('X=X+CY  ', X=DM, Y=DN, C=C, DIM1=NDIAG)
        ENDIF
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV('X=X+CY  ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
          IF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,15
            CALL OV('X=X+CY  ', X=XM(1,I+15), Y=XN(1,I),
     &              C=C, DIM1=NELEM)
          ENDDO ! I
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
98          FORMAT(1X,'OM4141 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,30
            CALL OV('X=X+CY  ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+N   '.OR.
     &      (OP(3:8).EQ.'M+TN  ').AND.TYPEXN(1:1).NE.'Q') THEN
!
        CALL OV('X=X+Y   ', X=DM, Y=DN, DIM1=NDIAG)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV('X=X+Y   ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
          IF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,15
            CALL OV('X=X+Y   ', X=XM(1,I+15), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,30
            CALL OV('X=X+Y   ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'MD    ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV('X=XY    ', X=DM, Y=D, DIM1=NDIAG)
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV('X=Y     ', X=DM, Y=D, DIM1=NDIAG)
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          WRITE(LU,13) TYPDIM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
!
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,2))
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,3))
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,4))
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,5))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,6))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,3))
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,4))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,5))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,6))
          XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,4))
          XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,5))
          XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,6))
          XM(IELEM,13) = XM(IELEM,13) * D(IKLE(IELEM,5))
          XM(IELEM,14) = XM(IELEM,14) * D(IKLE(IELEM,6))
          XM(IELEM,15) = XM(IELEM,15) * D(IKLE(IELEM,6))
!
          XM(IELEM,16) = XM(IELEM,16) * D(IKLE(IELEM,1))
          XM(IELEM,17) = XM(IELEM,17) * D(IKLE(IELEM,1))
          XM(IELEM,18) = XM(IELEM,18) * D(IKLE(IELEM,1))
          XM(IELEM,19) = XM(IELEM,19) * D(IKLE(IELEM,1))
          XM(IELEM,20) = XM(IELEM,20) * D(IKLE(IELEM,1))
          XM(IELEM,21) = XM(IELEM,21) * D(IKLE(IELEM,2))
          XM(IELEM,22) = XM(IELEM,22) * D(IKLE(IELEM,2))
          XM(IELEM,23) = XM(IELEM,23) * D(IKLE(IELEM,2))
          XM(IELEM,24) = XM(IELEM,24) * D(IKLE(IELEM,2))
          XM(IELEM,25) = XM(IELEM,25) * D(IKLE(IELEM,3))
          XM(IELEM,26) = XM(IELEM,26) * D(IKLE(IELEM,3))
          XM(IELEM,27) = XM(IELEM,27) * D(IKLE(IELEM,3))
          XM(IELEM,28) = XM(IELEM,28) * D(IKLE(IELEM,4))
          XM(IELEM,29) = XM(IELEM,29) * D(IKLE(IELEM,4))
          XM(IELEM,30) = XM(IELEM,30) * D(IKLE(IELEM,5))
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          WRITE(LU,151)
151       FORMAT(1X,
     &    'OM4141 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,200)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'DM    ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV('X=XY    ', X=DM, Y=D, DIM1= NDIAG)
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV('X=Y     ', X=DM, Y=D, DIM1=NDIAG)
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          WRITE(LU,13) TYPDIM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
!
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,1))
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,1))
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,1))
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,1))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,1))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,2))
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,2))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,2))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,2))
          XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,3))
          XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,3))
          XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,3))
          XM(IELEM,13) = XM(IELEM,13) * D(IKLE(IELEM,4))
          XM(IELEM,14) = XM(IELEM,14) * D(IKLE(IELEM,4))
          XM(IELEM,15) = XM(IELEM,15) * D(IKLE(IELEM,5))
!
          XM(IELEM,16) = XM(IELEM,16) * D(IKLE(IELEM,2))
          XM(IELEM,17) = XM(IELEM,17) * D(IKLE(IELEM,3))
          XM(IELEM,18) = XM(IELEM,18) * D(IKLE(IELEM,4))
          XM(IELEM,19) = XM(IELEM,19) * D(IKLE(IELEM,5))
          XM(IELEM,20) = XM(IELEM,20) * D(IKLE(IELEM,6))
          XM(IELEM,21) = XM(IELEM,21) * D(IKLE(IELEM,3))
          XM(IELEM,22) = XM(IELEM,22) * D(IKLE(IELEM,4))
          XM(IELEM,23) = XM(IELEM,23) * D(IKLE(IELEM,5))
          XM(IELEM,24) = XM(IELEM,24) * D(IKLE(IELEM,6))
          XM(IELEM,25) = XM(IELEM,25) * D(IKLE(IELEM,4))
          XM(IELEM,26) = XM(IELEM,26) * D(IKLE(IELEM,5))
          XM(IELEM,27) = XM(IELEM,27) * D(IKLE(IELEM,6))
          XM(IELEM,28) = XM(IELEM,28) * D(IKLE(IELEM,5))
          XM(IELEM,29) = XM(IELEM,29) * D(IKLE(IELEM,6))
          XM(IELEM,30) = XM(IELEM,30) * D(IKLE(IELEM,6))
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          WRITE(LU,161)
161       FORMAT(1X,
     &    'OM4141 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,200)
200       FORMAT(1X,'OM4141 (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'DMD   ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV('X=XY    ', X=DM, Y=D, DIM1=NDIAG)
          CALL OV('X=XY    ', X=DM, Y=D, DIM1=NDIAG)
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV('X=YZ    ', X=DM, Y=D, Z=D, DIM1=NDIAG)
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          WRITE(LU,13) TYPDIM(1:1)
13        FORMAT(1X,'OM4141 (BIEF) : TYPDIM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
!
            I1 = IKLE(IELEM,1)
            I2 = IKLE(IELEM,2)
            I3 = IKLE(IELEM,3)
            I4 = IKLE(IELEM,4)
            I5 = IKLE(IELEM,5)
            I6 = IKLE(IELEM,6)
!
            XM(IELEM, 1) =  XM(IELEM, 1) * D(I2) * D(I1)
            XM(IELEM, 2) =  XM(IELEM, 2) * D(I3) * D(I1)
            XM(IELEM, 3) =  XM(IELEM, 3) * D(I4) * D(I1)
            XM(IELEM, 4) =  XM(IELEM, 4) * D(I5) * D(I1)
            XM(IELEM, 5) =  XM(IELEM, 5) * D(I6) * D(I1)
            XM(IELEM, 6) =  XM(IELEM, 6) * D(I3) * D(I2)
            XM(IELEM, 7) =  XM(IELEM, 7) * D(I4) * D(I2)
            XM(IELEM, 8) =  XM(IELEM, 8) * D(I5) * D(I2)
            XM(IELEM, 9) =  XM(IELEM, 9) * D(I6) * D(I2)
            XM(IELEM,10) =  XM(IELEM,10) * D(I4) * D(I3)
            XM(IELEM,11) =  XM(IELEM,11) * D(I5) * D(I3)
            XM(IELEM,12) =  XM(IELEM,12) * D(I6) * D(I3)
            XM(IELEM,13) =  XM(IELEM,13) * D(I5) * D(I4)
            XM(IELEM,14) =  XM(IELEM,14) * D(I6) * D(I4)
            XM(IELEM,15) =  XM(IELEM,15) * D(I6) * D(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
!
          DO IELEM = 1 , NELEM
!
            I1 = IKLE(IELEM,1)
            I2 = IKLE(IELEM,2)
            I3 = IKLE(IELEM,3)
            I4 = IKLE(IELEM,4)
            I5 = IKLE(IELEM,5)
            I6 = IKLE(IELEM,6)
!
            XM(IELEM, 1) =  XM(IELEM, 1) * D(I2) * D(I1)
            XM(IELEM, 2) =  XM(IELEM, 2) * D(I3) * D(I1)
            XM(IELEM, 3) =  XM(IELEM, 3) * D(I4) * D(I1)
            XM(IELEM, 4) =  XM(IELEM, 4) * D(I5) * D(I1)
            XM(IELEM, 5) =  XM(IELEM, 5) * D(I6) * D(I1)
            XM(IELEM, 6) =  XM(IELEM, 6) * D(I3) * D(I2)
            XM(IELEM, 7) =  XM(IELEM, 7) * D(I4) * D(I2)
            XM(IELEM, 8) =  XM(IELEM, 8) * D(I5) * D(I2)
            XM(IELEM, 9) =  XM(IELEM, 9) * D(I6) * D(I2)
            XM(IELEM,10) =  XM(IELEM,10) * D(I4) * D(I3)
            XM(IELEM,11) =  XM(IELEM,11) * D(I5) * D(I3)
            XM(IELEM,12) =  XM(IELEM,12) * D(I6) * D(I3)
            XM(IELEM,13) =  XM(IELEM,13) * D(I5) * D(I4)
            XM(IELEM,14) =  XM(IELEM,14) * D(I6) * D(I4)
            XM(IELEM,15) =  XM(IELEM,15) * D(I6) * D(I5)
            XM(IELEM,16) =  XM(IELEM,16) * D(I2) * D(I1)
            XM(IELEM,17) =  XM(IELEM,17) * D(I3) * D(I1)
            XM(IELEM,18) =  XM(IELEM,18) * D(I4) * D(I1)
            XM(IELEM,19) =  XM(IELEM,19) * D(I5) * D(I1)
            XM(IELEM,20) =  XM(IELEM,20) * D(I6) * D(I1)
            XM(IELEM,21) =  XM(IELEM,21) * D(I3) * D(I2)
            XM(IELEM,22) =  XM(IELEM,22) * D(I4) * D(I2)
            XM(IELEM,23) =  XM(IELEM,23) * D(I5) * D(I2)
            XM(IELEM,24) =  XM(IELEM,24) * D(I6) * D(I2)
            XM(IELEM,25) =  XM(IELEM,25) * D(I4) * D(I3)
            XM(IELEM,26) =  XM(IELEM,26) * D(I5) * D(I3)
            XM(IELEM,27) =  XM(IELEM,27) * D(I6) * D(I3)
            XM(IELEM,28) =  XM(IELEM,28) * D(I5) * D(I4)
            XM(IELEM,29) =  XM(IELEM,29) * D(I6) * D(I4)
            XM(IELEM,30) =  XM(IELEM,30) * D(I6) * D(I5)
!
          ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,21) TYPEXM(1:1)
21        FORMAT(1X,'OM4141 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'0     ') THEN
!
        CALL OV('X=C     ', X=DM, C=0.D0, DIM1=NDIAG)
!
        IF(TYPEXM(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV('X=C     ', X=XM(1,I), C=0.D0, DIM1=NELEM)
          ENDDO
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,30
            CALL OV('X=C     ', X=XM(1,I), C=0.D0, DIM1=NELEM)
          ENDDO
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,711) TYPEXM(1:1)
711       FORMAT(1X,'OM4141 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!       TYPDIM IS NOT CHANGED
!        TYPDIM(1:1)='0'
!       TYPEXM IS NOT CHANGED
!        TYPEXM(1:1)='0'
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'X(M)  ') THEN
!
        IF(TYPEXM(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV('X=Y     ', X=XM(1,I+15), Y=XM(1,I), DIM1=NELEM)
          ENDDO
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,811) TYPEXM(1:1)
811       FORMAT(1X,'OM4141 (BIEF) : MATRIX ALREADY NON SYMMETRICAL: ',
     &           A1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPEXM(1:1)='Q'
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'MSK(M)') THEN
!
      IF(TYPEXM(1:1).EQ.'S') THEN
        J = 15
      ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
        J = 30
      ELSEIF(TYPEXM(1:1).EQ.'0') THEN
        J = 0
      ELSE
        WRITE(LU,200) TYPEXM
        J = 0
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(J.GT.0) THEN
        DO I = 1,J
          CALL OV('X=XY    ', X=XM(1,I), Y=D, DIM1=NELEM)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+D   ') THEN
!
        CALL OV('X=X+Y   ', X=DM, Y=D, DIM1=NDIAG)
!       HERE THERE IS A DOUBT ABOUT TYPDIM
        TYPDIM(1:1)='Q'
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,41) OP
41      FORMAT(1X,'OM4141 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
