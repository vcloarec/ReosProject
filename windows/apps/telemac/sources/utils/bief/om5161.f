!                   *****************
                    SUBROUTINE OM5161
!                   *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   C,
     & NULONE,NELBOR,NBOR,NELMAX,SIZDN,SIZXN,SZMXN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES.
!code
!+   M: T1 TETRAHEDRON
!+   N: BOUNDARY MATRIX, T1 TRIANGLE
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
!+        23/06/2008
!+        V5P9
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
!history  S.E.BOURBAN (HRW)
!+        21/03/2017
!+        V7P3
!+   Replacement of the DATA declarations by the PARAMETER associates
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| SIZDN          |-->| SIZE OF DIAGONAL DN
!| SIZXN          |-->| SIZE OF OFF-DIAGONAL TERMS XN
!| SZMXN          |-->| MAXIMUM SIZE OF OFF-DIAGONAL TERMS XN
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OM5161 => OM5161
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELMAX,SIZDN,SIZXN,SZMXN
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      INTEGER, INTENT(IN)             :: NULONE(SZMXN,3)
      INTEGER, INTENT(IN)             :: NELBOR(*),NBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),XN(SZMXN,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IEL,NUL1,NUL2,NUL3
!
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
!     BEWARE: IN FORTRAN THE FIRST INDEX VARIES MOST QUICKLY
!     123456789 SHOULD NOT BE USED
!
      INTEGER :: CONVNSY(4,4)
      PARAMETER ( CONVNSY = RESHAPE( (/
     &      123456789,     7    ,     8    ,    9     ,
     &          1    , 123456789,    10    ,   11     ,
     &          2    ,     4    , 123456789,   12     ,
     &          3    ,     5    ,     6    , 123456789 /),
     &      SHAPE=(/ 4,4 /) ) )
      INTEGER :: CONVSYM(4,4)
      PARAMETER ( CONVSYM = RESHAPE( (/
     &      123456789,     1    ,     2    ,    3     ,
     &          1    , 123456789,     4    ,    5     ,
     &          2    ,     4    , 123456789,    6     ,
     &          3    ,     5    ,     6    , 123456789 /),
     &      SHAPE=(/ 4,4 /) ) )
!
!***********************************************************************
!
      IF(OP(1:8).EQ.'M=M+N   ') THEN
!
        IF(TYPDIM.EQ.'Q'.AND.TYPDIN.EQ.'Q') THEN
          CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , SIZDN )
        ELSE
          WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
199       FORMAT(1X,'OM5161 (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          IF(NCSIZE.GT.1) THEN
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            IF(IEL.GT.0) THEN
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 3)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 4)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 5)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 6)
            ENDIF
          ENDDO
          ELSE
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 3)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 4)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 5)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 6)
          ENDDO
          ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          IF(NCSIZE.GT.1) THEN
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            IF(IEL.GT.0) THEN
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 3)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 3)
            ENDIF
          ENDDO
          ELSE
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 3)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 3)
          ENDDO
          ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          IF(NCSIZE.GT.1) THEN
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            IF(IEL.GT.0) THEN
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVSYM(NUL1,NUL2) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVSYM(NUL1,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVSYM(NUL2,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL2,NUL3) ) + XN(K, 3)
            ENDIF
          ENDDO
          ELSE
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVSYM(NUL1,NUL2) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVSYM(NUL1,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVSYM(NUL2,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL2,NUL3) ) + XN(K, 3)
          ENDDO
          ENDIF
!
        ELSE
          WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99        FORMAT(1X,'OM5161 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+TN  ') THEN
!
        CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , SIZXN )
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          IF(NCSIZE.GT.1) THEN
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            IF(IEL.GT.0) THEN
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 4)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 5)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 6)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 3)
            ENDIF
          ENDDO
          ELSE
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 4)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 5)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 6)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 3)
          ENDDO
          ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          IF(NCSIZE.GT.1) THEN
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            IF(IEL.GT.0) THEN
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 3)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 3)
            ENDIF
          ENDDO
          ELSE
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVNSY(NUL1,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL1,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL2,NUL3) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL3) ) + XN(K, 3)
            XM( IEL , CONVNSY(NUL2,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL2,NUL1) ) + XN(K, 1)
            XM( IEL , CONVNSY(NUL3,NUL1) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL1) ) + XN(K, 2)
            XM( IEL , CONVNSY(NUL3,NUL2) ) =
     &      XM( IEL , CONVNSY(NUL3,NUL2) ) + XN(K, 3)
          ENDDO
          ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          IF(NCSIZE.GT.1) THEN
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            IF(IEL.GT.0) THEN
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVSYM(NUL1,NUL2) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVSYM(NUL1,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVSYM(NUL2,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL2,NUL3) ) + XN(K, 3)
            ENDIF
          ENDDO
          ELSE
          DO K = 1 , SIZXN
            IEL = NELBOR(K)
            NUL1=NULONE(K,1)
            NUL2=NULONE(K,2)
            NUL3=NULONE(K,3)
            XM( IEL , CONVSYM(NUL1,NUL2) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL2) ) + XN(K, 1)
            XM( IEL , CONVSYM(NUL1,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL1,NUL3) ) + XN(K, 2)
            XM( IEL , CONVSYM(NUL2,NUL3) ) =
     &      XM( IEL , CONVSYM(NUL2,NUL3) ) + XN(K, 3)
          ENDDO
          ENDIF
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
71      FORMAT(1X,'OM5161 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
