!                   *****************
                    SUBROUTINE OM1201
!                   *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   C,
     & NULONE,NELBOR,NBOR,NELMAX,NDIAG,NPTFR,NELEBX,NELEB)
!
!***********************************************************************
! BIEF   V7P0                                         21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES.
!code
!+   M: QUASI-BUBBLE TRIANGLE
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
!
!code
!+  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!+
!+      XM(     , 1)  ---->  M(1,2)
!+      XM(     , 2)  ---->  M(1,3)
!+      XM(     , 3)  ---->  M(1,4)
!+      XM(     , 4)  ---->  M(2,3)
!+      XM(     , 5)  ---->  M(2,4)
!+      XM(     , 6)  ---->  M(3,4)
!+      XM(     , 7)  ---->  M(2,1)
!+      XM(     , 8)  ---->  M(3,1)
!+      XM(     , 9)  ---->  M(4,1)
!+      XM(     ,10)  ---->  M(3,2)
!+      XM(     ,11)  ---->  M(4,2)
!+      XM(     ,12)  ---->  M(4,3)
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
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
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEB          |-->| NUMBER OF BOUNDARY ELEMENTS
!| NELEBX         |-->| MAXIMUM NUMBER OF BOUNDARY ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
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
      USE BIEF, EX_OM1201 => OM1201
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELMAX,NDIAG,NPTFR,NELEBX,NELEB
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      INTEGER, INTENT(IN)             :: NULONE(NELEBX),NELBOR(NELEBX)
      INTEGER, INTENT(IN)             :: NBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),XN(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IEL
!
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!     BEWARE: ONLY WORKS FOR QUASI-BUBBLE
      INTEGER :: CORNSY(4,2)
      PARAMETER ( CORNSY = RESHAPE( (/
     &                        1,4,8,0,  7,10,2,0/), SHAPE=(/ 4,2 /) ) )
      INTEGER :: CORSYM(4)
      PARAMETER ( CORSYM = (/ 1,4,2,0      /) )
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'M=M+N   ') THEN
!
        IF(TYPDIM.EQ.'Q'.AND.TYPDIM.EQ.'Q'.AND.NDIAG.GE.NPTFR) THEN
          CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , NPTFR )
        ELSE
          WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
199       FORMAT(1X,'OM1201 (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          DO K = 1 , NELEB
            IEL = NELBOR(K)
            XM( IEL , CORNSY(NULONE(K),1) ) =
     &      XM( IEL , CORNSY(NULONE(K),1) ) + XN(K)
            XM( IEL , CORNSY(NULONE(K),2) ) =
     &      XM( IEL , CORNSY(NULONE(K),2) ) + XN(K+NELEBX)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , NELEB
            IEL = NELBOR(K)
            XM( IEL , CORNSY(NULONE(K),1) ) =
     &      XM( IEL , CORNSY(NULONE(K),1) ) + XN(K)
            XM( IEL , CORNSY(NULONE(K),2) ) =
     &      XM( IEL , CORNSY(NULONE(K),2) ) + XN(K)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , NELEB
            IEL = NELBOR(K)
            XM( IEL , CORSYM(NULONE(K)) ) =
     &      XM( IEL , CORSYM(NULONE(K)) ) + XN(K)
          ENDDO
!
        ELSE
          WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99        FORMAT(1X,'OM1201 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+TN  ') THEN
!
        CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , NPTFR )
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!         CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
!
          DO K = 1 , NELEB
            IEL = NELBOR(K)
            XM( IEL , CORNSY(NULONE(K),1) ) =
     &      XM( IEL , CORNSY(NULONE(K),1) ) + XN(K+NELEBX)
            XM( IEL , CORNSY(NULONE(K),2) ) =
     &      XM( IEL , CORNSY(NULONE(K),2) ) + XN(K)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
!
          DO K = 1 , NELEB
            IEL = NELBOR(K)
            XM( IEL , CORNSY(NULONE(K),1) ) =
     &      XM( IEL , CORNSY(NULONE(K),1) ) + XN(K)
            XM( IEL , CORNSY(NULONE(K),2) ) =
     &      XM( IEL , CORNSY(NULONE(K),2) ) + XN(K)
          ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!         CASE WHERE BOTH MATRICES ARE SYMMETRICAL
!
          DO K = 1 , NELEB
            IEL = NELBOR(K)
            XM( IEL , CORSYM(NULONE(K)) ) =
     &      XM( IEL , CORSYM(NULONE(K)) ) + XN(K)
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
        WRITE(LU,141) OP
141     FORMAT(1X,'OM1201 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
