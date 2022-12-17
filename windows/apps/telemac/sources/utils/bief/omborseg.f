!                   *******************
                    SUBROUTINE OMBORSEG
!                   *******************
!
     &(OP,DM,XM,TYPEXM,DN,XN,TYPEXN,C,
     & NDIAG,MSEG1,MSEG2,NSEG1,NSEG2,NBOR)
!
!***********************************************************************
! BIEF   V6P3                                   01/01/2013
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES WITH AN EDGE-BASED STORAGE
!+         WHERE N IS A BOUNDARY MATRIX
!
!code
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
!history  F. DECUNG (LNHE)
!+        2012
!+        V6P3
!+   Adapted from omseg.f
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| MSEG1          |-->| NUMBER OF SEGMENTS OF LINE ELEMENT OF M
!| MSEG2          |-->| NUMBER OF SEGMENTS OF COLUMN ELEMENT OF M
!| NSEG1          |-->| NUMBER OF SEGMENTS OF LINE ELEMENT OF N
!| NSEG2          |-->| NUMBER OF SEGMENTS OF COLUMN ELEMENT OF N
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
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
      USE BIEF, EX_OMBORSEG => OMBORSEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NDIAG,MSEG1,MSEG2,NSEG1,NSEG2
      INTEGER, INTENT(IN) :: NBOR(*)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*)
!     XM AND XN MAY ONLY BE OF SIZE NSEG1 IF THE MATRIX IS SYMMETRICAL
!     SIZE GIVEN HERE ONLY TO CHECK BOUNDS
      DOUBLE PRECISION, INTENT(INOUT) :: XM(MSEG1+MSEG2)
      DOUBLE PRECISION, INTENT(IN)    :: XN(NSEG1+NSEG2)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPEXM,TYPEXN
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*)
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC MIN
!
      DOUBLE PRECISION Z(1)
!
      INTEGER DIMX,DIMY
!
!-----------------------------------------------------------------------
!     BASICALLY, FOR SQUARED MATRICES :
!     XM(1:NSEG) <=> XN(1:NSEG)
!     XM(DIMX+1:DIMX+NSEG) <=> XN(NSEG+1:2*NSEG)
!
      DIMX=MIN(MSEG1,MSEG2)
      DIMY=MAX(NSEG1,NSEG2)
!
      IF(OP(3:8).EQ.'M+N   ') THEN
!
        CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR, NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          CALL OV('X=X+Y   ', X=XM, Y=XN, DIM1=NSEG2)
          IF(TYPEXM(1:1).EQ.'Q') THEN
          CALL OV('X=X+Y   ' , X=XM(DIMX+1:DIMX+NSEG2) , Y=XN,
     &            DIM1=NSEG2)
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
98         FORMAT(1X,'OMBORSEG (BIEF) : TYPEXM = ',A1,
     &      ' DOES NOT GO',/,1X,'FOR THE OPERATION : ',A8,
     &      ' WITH TYPEXN = ',A1)
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL OV('X=X+Y   ' , X=XM, Y=XN, DIM1=NSEG2)
          CALL OV('X=X+Y   ' , X=XM(DIMX+1:DIMX+NSEG2),
     &            Y=XN(DIMY+1:DIMY+NSEG2), DIM1=NSEG2)
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          WRITE(LU,11) TYPEXN(1:1)
11        FORMAT(1X,'OMBORSEG (BIEF) : TYPEXN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        WRITE(LU,41) OP
41      FORMAT(1X,'OMBORSEG (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
