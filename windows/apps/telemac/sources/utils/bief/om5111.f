!                   *****************
                    SUBROUTINE OM5111
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
!+   M: TETRAHEDRON
!+   N: TRIANGLE MATRIX (BOTTOM OR SURFACE)
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
!+        28/08/02
!+        V5P3
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
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NETAGE,SIZDN,SZMDN,SIZXN
      INTEGER, INTENT(IN)             :: NELMAX3D
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),XN(NELMAX3D/(3*NETAGE),*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(SZMDN,*)
      DOUBLE PRECISION,INTENT(INOUT)::XM(NELMAX3D/(3*NETAGE),3,NETAGE,*)
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
199       FORMAT(1X,'OM5111 (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
!         XM(K,1,1,  )  K : TRIANGLE NUMBER
!                       1 : T1 TETRAHEDRON, ONE SIDE OF WHICH IS AT THE BOTTOM
!                       1 : 1ST LAYER, THAT AT THE BOTTOM
!
          DO K = 1 , SIZXN
            XM(K,1,1,01) = XM(K,1,1,01) + XN(K,1)
            XM(K,1,1,02) = XM(K,1,1,02) + XN(K,2)
            XM(K,1,1,04) = XM(K,1,1,04) + XN(K,3)
            XM(K,1,1,07) = XM(K,1,1,07) + XN(K,4)
            XM(K,1,1,08) = XM(K,1,1,08) + XN(K,5)
            XM(K,1,1,10) = XM(K,1,1,10) + XN(K,6)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1,1,01) = XM(K,1,1,01) + XN(K,1)
            XM(K,1,1,02) = XM(K,1,1,02) + XN(K,2)
            XM(K,1,1,04) = XM(K,1,1,04) + XN(K,3)
            XM(K,1,1,07) = XM(K,1,1,07) + XN(K,1)
            XM(K,1,1,08) = XM(K,1,1,08) + XN(K,2)
            XM(K,1,1,10) = XM(K,1,1,10) + XN(K,3)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1,1,01) = XM(K,1,1,01) + XN(K,1)
            XM(K,1,1,02) = XM(K,1,1,02) + XN(K,2)
            XM(K,1,1,04) = XM(K,1,1,04) + XN(K,3)
          ENDDO
!
        ELSE
          WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99        FORMAT(1X,'OM5111 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
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
            XM(K,1,1,01) = XM(K,1,1,01) + XN(K,4)
            XM(K,1,1,02) = XM(K,1,1,02) + XN(K,5)
            XM(K,1,1,04) = XM(K,1,1,04) + XN(K,6)
            XM(K,1,1,07) = XM(K,1,1,07) + XN(K,1)
            XM(K,1,1,08) = XM(K,1,1,08) + XN(K,2)
            XM(K,1,1,10) = XM(K,1,1,10) + XN(K,3)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1,1,01) = XM(K,1,1,01) + XN(K,1)
            XM(K,1,1,02) = XM(K,1,1,02) + XN(K,2)
            XM(K,1,1,04) = XM(K,1,1,04) + XN(K,3)
            XM(K,1,1,07) = XM(K,1,1,07) + XN(K,1)
            XM(K,1,1,08) = XM(K,1,1,08) + XN(K,2)
            XM(K,1,1,10) = XM(K,1,1,10) + XN(K,3)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,1,1,01) = XM(K,1,1,01) + XN(K,1)
            XM(K,1,1,02) = XM(K,1,1,02) + XN(K,2)
            XM(K,1,1,04) = XM(K,1,1,04) + XN(K,3)
          ENDDO
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
!         XM(K,1,1,  )  K      : TRIANGLE NUMBER
!                       2      : T2 TETRAHEDRON, ONE SIDE OF WHICH IS AT THE SURFACE
!                       NETAGE : LAST LAYER, THAT AT THE SURFACE
!
          DO K = 1 , SIZXN
            XM(K,2,NETAGE,02) = XM(K,2,NETAGE,02) + XN(K,1)
            XM(K,2,NETAGE,01) = XM(K,2,NETAGE,01) + XN(K,2)
            XM(K,2,NETAGE,10) = XM(K,2,NETAGE,10) + XN(K,3)
            XM(K,2,NETAGE,08) = XM(K,2,NETAGE,08) + XN(K,4)
            XM(K,2,NETAGE,07) = XM(K,2,NETAGE,07) + XN(K,5)
            XM(K,2,NETAGE,04) = XM(K,2,NETAGE,04) + XN(K,6)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,2,NETAGE,02) = XM(K,2,NETAGE,02) + XN(K,1)
            XM(K,2,NETAGE,01) = XM(K,2,NETAGE,01) + XN(K,2)
            XM(K,2,NETAGE,10) = XM(K,2,NETAGE,10) + XN(K,3)
            XM(K,2,NETAGE,08) = XM(K,2,NETAGE,08) + XN(K,1)
            XM(K,2,NETAGE,07) = XM(K,2,NETAGE,07) + XN(K,2)
            XM(K,2,NETAGE,04) = XM(K,2,NETAGE,04) + XN(K,3)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,2,NETAGE,02) = XM(K,2,NETAGE,02) + XN(K,1)
            XM(K,2,NETAGE,01) = XM(K,2,NETAGE,01) + XN(K,2)
            XM(K,2,NETAGE,04) = XM(K,2,NETAGE,04) + XN(K,3)
          ENDDO
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
            XM(K,2,NETAGE,02) = XM(K,2,NETAGE,02) + XN(K,4)
            XM(K,2,NETAGE,01) = XM(K,2,NETAGE,01) + XN(K,5)
            XM(K,2,NETAGE,10) = XM(K,2,NETAGE,10) + XN(K,6)
            XM(K,2,NETAGE,08) = XM(K,2,NETAGE,08) + XN(K,1)
            XM(K,2,NETAGE,07) = XM(K,2,NETAGE,07) + XN(K,2)
            XM(K,2,NETAGE,04) = XM(K,2,NETAGE,04) + XN(K,3)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,2,NETAGE,02) = XM(K,2,NETAGE,02) + XN(K,1)
            XM(K,2,NETAGE,01) = XM(K,2,NETAGE,01) + XN(K,2)
            XM(K,2,NETAGE,10) = XM(K,2,NETAGE,10) + XN(K,3)
            XM(K,2,NETAGE,08) = XM(K,2,NETAGE,08) + XN(K,1)
            XM(K,2,NETAGE,07) = XM(K,2,NETAGE,07) + XN(K,2)
            XM(K,2,NETAGE,04) = XM(K,2,NETAGE,04) + XN(K,3)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , SIZXN
            XM(K,2,NETAGE,02) = XM(K,2,NETAGE,02) + XN(K,1)
            XM(K,2,NETAGE,01) = XM(K,2,NETAGE,01) + XN(K,2)
            XM(K,2,NETAGE,04) = XM(K,2,NETAGE,04) + XN(K,3)
          ENDDO
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
71      FORMAT(1X,'OM5111 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
