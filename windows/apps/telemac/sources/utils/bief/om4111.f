!                   *****************
                    SUBROUTINE OM4111
!                   *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,
     & SIZDN,SZMDN,SIZXN,NETAGE, NELMAX3D)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES.
!code
!+   M: P1 TRIANGLE
!+   N: BOUNDARY MATRIX
!+   D: DIAGONAL MATRIX
!+   C: CONSTANT
!+
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON MATRICES M AND N, D AND C.
!+
!+   THE RESULT IS MATRIX M.
!+
!+      OP = 'M=M+N   '  : ADDS N TO M
!+      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!
!code
!+  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!+
!+      XM(     ,1)  ---->  M(1,2)
!+      XM(     ,2)  ---->  M(1,3)
!+      XM(     ,3)  ---->  M(2,3)
!+      XM(     ,4)  ---->  M(2,1)
!+      XM(     ,5)  ---->  M(3,1)
!+      XM(     ,6)  ---->  M(3,2)
!
!history  J-M HERVOUET (LNHE)
!+        06/12/94
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
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| NELMAX3D       |-->| MAXIMUM NUMBER OF 3D ELEMENTS
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| SIZDN          |-->| SIZE OF DIAGONAL DN
!| SIZXN          |-->| SIZE OF OFF-DIAGONAL TERMS XN
!| SZMDN          |-->| MAXIMUM SIZE OF DIAGONAL DN
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
      USE BIEF, EX_OM4111 => OM4111
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NETAGE,SIZDN,SZMDN,SIZXN
      INTEGER, INTENT(IN)             :: NELMAX3D
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),XN(NELMAX3D/NETAGE,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(SZMDN,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NELMAX3D/NETAGE,NETAGE,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'M=M+NF  ') THEN
!
        IF(TYPDIM.EQ.'Q'.AND.TYPDIN.EQ.'Q') THEN
          CALL OV('X=X+Y   ', X=DM, Y=DN, DIM1=SIZDN)
        ELSE
          WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
199       FORMAT(1X,'OM4111 (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
            XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
            XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
            XM(K,1,16) = XM(K,1,16) + XN(K,4)
            XM(K,1,17) = XM(K,1,17) + XN(K,5)
            XM(K,1,21) = XM(K,1,21) + XN(K,6)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
            XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
            XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
            XM(K,1,16) = XM(K,1,16) + XN(K,1)
            XM(K,1,17) = XM(K,1,17) + XN(K,2)
            XM(K,1,21) = XM(K,1,21) + XN(K,3)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
            XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
            XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
          ENDDO ! K
!
        ELSE
          WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99        FORMAT(1X,'OM4111 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+TNF ') THEN
!
        CALL OV('X=X+Y   ', X=DM, Y=DN, DIM1=SIZDN)
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1, 1) = XM(K,1, 1) + XN(K,4)
            XM(K,1, 2) = XM(K,1, 2) + XN(K,5)
            XM(K,1, 6) = XM(K,1, 6) + XN(K,6)
            XM(K,1,16) = XM(K,1,16) + XN(K,1)
            XM(K,1,17) = XM(K,1,17) + XN(K,2)
            XM(K,1,21) = XM(K,1,21) + XN(K,3)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
            XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
            XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
            XM(K,1,16) = XM(K,1,16) + XN(K,1)
            XM(K,1,17) = XM(K,1,17) + XN(K,2)
            XM(K,1,21) = XM(K,1,21) + XN(K,3)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
            XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
            XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
          ENDDO ! K
!
        ELSE
          WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+NS  ') THEN
!
        CALL OV('X=X+Y   ', X=DM(1,NETAGE+1), Y=DN, DIM1=SIZDN)
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
            XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
            XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
            XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,4)
            XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,5)
            XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,6)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
            XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
            XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
            XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,1)
            XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,2)
            XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,3)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
            XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
            XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
          ENDDO ! K
!
        ELSE
          WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+TNS ') THEN
!
        CALL OV('X=X+Y   ', X=DM(1,NETAGE+1), Y=DN, DIM1=SIZDN)
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,4)
            XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,5)
            XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,6)
            XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,1)
            XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,2)
            XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,3)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
            XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
            XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
            XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,1)
            XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,2)
            XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,3)
          ENDDO ! K
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
            XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
            XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
          ENDDO ! K
!
        ELSE
          WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,71) OP
71      FORMAT(1X,'OM4111 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
