!                   *****************
                    SUBROUTINE OM2121
!                   *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & IKLE,NELEM,NELMAX,NDIAG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES WITH Q1 QUADRILATERAL
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
!+      OP = 'M=TN    '  : COPIES TRANSPOSE OF N IN M
!+      OP = 'M=CN    '  : MULTIPLIES N BY C
!+      OP = 'M=CM    '  : MULTIPLIES M BY C
!+      OP = 'M=M+CN  '  : ADDS CN TO M
!+      OP = 'M=M+CTN '  : ADDS C TRANSPOSE(N) TO M
!+      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!+      OP = 'M=M+N   '  : ADDS N TO M
!+      OP = 'M=MD    '  : M X D
!+      OP = 'M=DM    '  : D X M
!+      OP = 'M=DMD   '  : D X M X D
!+      OP = 'M=M+D   '  : ADDS D TO M
!+      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!+                         (OLD MATSNS)
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!+                         (OLD MASKEX)
!+                         THE MASK IS TAKEN FROM D
!
!code
!+  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2)
!+     XM(IELEM, 2)  ---->  M(1,3)
!+     XM(IELEM, 3)  ---->  M(1,4)
!+     XM(IELEM, 4)  ---->  M(2,3)
!+     XM(IELEM, 5)  ---->  M(2,4)
!+     XM(IELEM, 6)  ---->  M(3,4)
!+     XM(IELEM, 7)  ---->  M(2,1)
!+     XM(IELEM, 8)  ---->  M(3,1)
!+     XM(IELEM, 9)  ---->  M(4,1)
!+     XM(IELEM,10)  ---->  M(3,2)
!+     XM(IELEM,11)  ---->  M(4,2)
!+     XM(IELEM,12)  ---->  M(4,3)
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        21/09/93
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
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| D              |-->| A DIAGONAL MATRIX
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
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
      USE BIEF, EX_OM2121 => OM2121
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NDIAG
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*),XN(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IELEM
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
6         FORMAT(1X,'OM2121 (BIEF) : TYPDIN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV('X=Y     ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,12
            CALL OV('X=Y     ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,40) TYPEXN(1:1)
40        FORMAT(1X,'OM2121 (BIEF) : TYPEXN UNKNOWN :',A1)
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
          DO I=1,6
            CALL OV('X=Y     ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,6
            CALL OV('X=Y     ', X=XM(1,I), Y=XN(1,I+6), DIM1=NELEM)
            CALL OV('X=Y     ', X=XM(1,I+6), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,40) TYPEXN(1:1)
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
          DO I=1,6
            CALL OV('X=CY    ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,12
            CALL OV('X=CY    ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,40) TYPEXN(1:1)
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
          DO I=1,6
            CALL OV('X=CX    ', X=XM(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,12
            CALL OV('X=CX    ', X=XM(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,40) TYPEXM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+CN  ' .OR.
     &      (OP(3:8).EQ.'M+CTN '.AND.TYPEXN(1:1).NE.'Q') ) THEN
!
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV('X=X+C   ', X=DM, C=C, DIM1=NDIAG)
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV('X=X+CY  ', X=DM, Y=DN, C=C, DIM1=NDIAG)
        ENDIF
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV('X=X+CY  ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
          IF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,6
            CALL OV('X=X+CY  ', X=XM(1,I+6), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
98         FORMAT(1X,'OM2121 (BIEF) : TYPEXM = ',A1,' DOES NOT GO ',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,12
            CALL OV('X=X+CY  ', X=XM(1,I), Y=XN(1,I), C=C, DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+CTN ') THEN
!
!  THE CASES WHERE N IS SYMMETRICAL ARE TREATED WITH M=M+CN
!
        CALL OV('X=X+CY  ', X=DM, Y=DN, C=C, DIM1=NDIAG)
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,6
            CALL OV('X=X+CY  ', X=XM(1,I), Y=XN(1,I+6), C=C, DIM1=NELEM)
            CALL OV('X=X+CY  ', X=XM(1,I+6), Y=XN(1,I  ),
     &              C=C, DIM1=NELEM )
          ENDDO ! I
        ELSE
          WRITE(LU,40) TYPEXN(1:1)
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
          DO I=1,6
            CALL OV('X=X+Y   ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
          IF(TYPEXM(1:1).EQ.'Q') THEN
            DO I=1,6
              CALL OV('X=X+Y   ', X=XM(1,I+6), Y=XN(1,I), DIM1=NELEM)
            ENDDO ! I
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,12
            CALL OV('X=X+Y   ', X=XM(1,I), Y=XN(1,I), DIM1=NELEM)
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+TN  ') THEN
!
!  THE CASES WHERE N IS SYMMETRICAL ARE TREATED WITH M=M+N
!
        CALL OV('X=X+Y   ', X=DM, Y=DN, DIM1=NDIAG)
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,6
            CALL OV('X=X+Y   ', X=XM(1,I), Y=XN(1,I+6), DIM1=NELEM)
            CALL OV('X=X+Y   ', X=XM(1,I+6), Y=XN(1,I  ), DIM1=NELEM)
          ENDDO ! I
        ELSE
          WRITE(LU,40) TYPEXN(1:1)
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
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
!
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,2))
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,3))
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,4))
!
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,3))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,4))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,4))
!
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,1))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,1))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,1))
!
          XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,2))
          XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,2))
          XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,3))
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          WRITE(LU,180)
180       FORMAT(1X,
     &    'OM2121 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
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
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,2))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,3))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,4))
!
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,1))
          XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,3))
          XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,4))
!
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,1))
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,2))
          XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,4))
!
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,1))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,2))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,3))
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          WRITE(LU,230)
230       FORMAT(1X,
     &    'OM2121 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,200)
200       FORMAT(1X,'OM2121 (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
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
13        FORMAT(1X,'OM2121 (BIEF) : TYPDIM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'S') THEN
!
        DO IELEM = 1 , NELEM
          XM(IELEM, 1)=XM(IELEM, 1) * D(IKLE(IELEM,2))*D(IKLE(IELEM,1))
          XM(IELEM, 2)=XM(IELEM, 2) * D(IKLE(IELEM,3))*D(IKLE(IELEM,1))
          XM(IELEM, 3)=XM(IELEM, 3) * D(IKLE(IELEM,4))*D(IKLE(IELEM,1))
          XM(IELEM, 4)=XM(IELEM, 4) * D(IKLE(IELEM,3))*D(IKLE(IELEM,2))
          XM(IELEM, 5)=XM(IELEM, 5) * D(IKLE(IELEM,4))*D(IKLE(IELEM,2))
          XM(IELEM, 6)=XM(IELEM, 6) * D(IKLE(IELEM,4))*D(IKLE(IELEM,3))
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
          XM(IELEM, 1)=XM(IELEM, 1) * D(IKLE(IELEM,2))*D(IKLE(IELEM,1))
          XM(IELEM, 2)=XM(IELEM, 2) * D(IKLE(IELEM,3))*D(IKLE(IELEM,1))
          XM(IELEM, 3)=XM(IELEM, 3) * D(IKLE(IELEM,4))*D(IKLE(IELEM,1))
          XM(IELEM, 4)=XM(IELEM, 4) * D(IKLE(IELEM,3))*D(IKLE(IELEM,2))
          XM(IELEM, 5)=XM(IELEM, 5) * D(IKLE(IELEM,4))*D(IKLE(IELEM,2))
          XM(IELEM, 6)=XM(IELEM, 6) * D(IKLE(IELEM,4))*D(IKLE(IELEM,3))
          XM(IELEM, 7)=XM(IELEM, 7) * D(IKLE(IELEM,2))*D(IKLE(IELEM,1))
          XM(IELEM, 8)=XM(IELEM, 8) * D(IKLE(IELEM,3))*D(IKLE(IELEM,1))
          XM(IELEM, 9)=XM(IELEM, 9) * D(IKLE(IELEM,4))*D(IKLE(IELEM,1))
          XM(IELEM,10)=XM(IELEM,10) * D(IKLE(IELEM,3))*D(IKLE(IELEM,2))
          XM(IELEM,11)=XM(IELEM,11) * D(IKLE(IELEM,4))*D(IKLE(IELEM,2))
          XM(IELEM,12)=XM(IELEM,12) * D(IKLE(IELEM,4))*D(IKLE(IELEM,3))
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,241) TYPEXM(1:1)
241       FORMAT(1X,'OM2121 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+D   ') THEN
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV('X=X+Y   ', X=DM, Y=D, DIM1=NDIAG)
        ELSE
          WRITE(LU,13) TYPDIM(1:1)
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
          DO I=1,6
            CALL OV('X=C     ', X=XM(1,I), C=0.D0, DIM1=NELEM)
          ENDDO
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,12
            CALL OV('X=C     ', X=XM(1,I), C=0.D0, DIM1=NELEM)
          ENDDO
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,711) TYPEXM(1:1)
711       FORMAT(1X,'OM2121 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)='0'
!       TYPEXM(1:1) IS NOT CHANGED
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'X(M)  ') THEN
!
        IF(TYPEXM(1:1).EQ.'S') THEN
          CALL OV('X=Y     ', X=XM(1, 7), Y=XM(1,1), DIM1=NELEM)
          CALL OV('X=Y     ', X=XM(1, 8), Y=XM(1,2), DIM1=NELEM)
          CALL OV('X=Y     ', X=XM(1, 9), Y=XM(1,3), DIM1=NELEM)
          CALL OV('X=Y     ', X=XM(1,10), Y=XM(1,4), DIM1=NELEM)
          CALL OV('X=Y     ', X=XM(1,11), Y=XM(1,5), DIM1=NELEM)
          CALL OV('X=Y     ', X=XM(1,12), Y=XM(1,6), DIM1=NELEM)
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          WRITE(LU,811) TYPEXM(1:1)
811       FORMAT(1X,'OM2121 (BIEF) : MATRIX ALREADY NON SYMMETRICAL: ',
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
        J = 6
      ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
        J = 12
      ELSEIF(TYPEXM(1:1).EQ.'0') THEN
        J = 0
      ELSE
        WRITE(LU,711) TYPEXM
        J = 0
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(J.GT.0) THEN
        DO I = 1,J
          CALL OV('X=XY    ', X=XM(1,I), Y=D, DIM1=NELEM)
        ENDDO ! I
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,42) OP
42      FORMAT(1X,'OM2121 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
