!                   *****************
                    SUBROUTINE ROTNE0
!                   *****************
!
     &(MESH,M1,A11,A12,A21,A22,SMU,SMV,UN,VN,H0,MSK,MASKEL,S,DT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE MATRICES SOLVING HELMHOLTZ EQUATIONS
!+               (STEPS 1 AND 3 OF BOUSSINESQ ALGORITHM).
!
!history  J-M HERVOUET (LNH)     ; C MOULIN (LNH)
!+        17/08/1994
!+        V5P2
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
!| A11            |<--| MATRIX WHICH MULTIPLIES U IN THE EQUATION FOR U
!| A12            |<--| MATRIX WHICH MULTIPLIES V IN THE EQUATION FOR U
!| A21            |<--| MATRIX WHICH MULTIPLIES U IN THE EQUATION FOR V
!| A22            |<--| MATRIX WHICH MULTIPLIES V IN THE EQUATION FOR V
!| H0             |-->| REFERENCE DEPTH
!| M1             |<->| WORK MATRIX
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| S              |-->| VOID STRUCTURE
!| SMU            |<--| SECOND MEMBER IN THE EQUATION FOR U
!| SMV            |<--| SECOND MEMBER IN THE EQUATION FOR V
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: MSK
      DOUBLE PRECISION, INTENT(IN)   :: DT
      TYPE(BIEF_OBJ), INTENT(IN)     :: MASKEL,H0,S,UN,VN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: SMU,SMV
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: A11,A12,A21,A22,M1
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMU
!
      DOUBLE PRECISION SL11,C,SURDT
!
      CHARACTER(LEN=16) FORMUL
!
!-----------------------------------------------------------------------
!
      IELMU=UN%ELM
!
!------------------------------------------------------------------
!
      SURDT = 1.D0 / DT
!
!     MATRIX FOR U IN THE EQUATION FOR U (INITIALLY STORED IN M1)
!
      FORMUL='FFBT        0XX0'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        XX00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     FORMUL='FFBT        0X0X'
      FORMUL='FFBT        0XX0'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        00XX'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     ADDITION TO A11 MATRIX
!
      IF(A11%TYPEXT.EQ.'S') CALL OM('M=X(M)  ', M=A11, MESH=MESH)
      CALL OM('M=M+N   ', M=A11, N=M1, MESH=MESH)
!
!     SECOND MEMBER SMU
!
      CALL MATVEC( 'X=X+AY  ',SMU,M1,UN,C,MESH)
!
!------------------------------------------------------------------
!
!     MATRIX FOR V IN THE EQUATION FOR U (INITIALLY STORED IN M1)
!
      FORMUL='FFBT        0Y0X'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        XY00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        00XY'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        0X0Y'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      CALL OM('M=N     ', M=A12, N=M1, MESH=MESH)
!
!     SECOND MEMBER SMU
!
      CALL MATVEC( 'X=X+AY  ',SMU,M1,VN,C,MESH)
!
!------------------------------------------------------------------
!
!     MATRIX FOR V IN THE EQUATION FOR V (INITIALLY STORED IN M1)
!
      FORMUL='FFBT        0YY0'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        YY00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        0YY0'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        00YY'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     ADDITION TO A22 MATRIX
!
      IF(A22%TYPEXT.EQ.'S') CALL OM('M=X(M)  ', M=A22, MESH=MESH)
      CALL OM('M=M+N   ', M=A22, N=M1, MESH=MESH)
!
!     SECOND MEMBER SMV
!
      CALL MATVEC( 'X=X+AY  ',SMV,M1,VN,C,MESH)
!
!------------------------------------------------------------------
!
!     MATRIX FOR U IN THE EQUATION FOR V (INITIALLY STORED IN M1)
!
      FORMUL='FFBT        0X0Y'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        00XY'
      SL11 = SURDT / 3.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        XY00'
      SL11 = SURDT / 2.D0
      CALL MATRIX(M1,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        0Y0X'
      SL11 = SURDT / 6.D0
      CALL MATRIX(M1,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      CALL OM('M=N     ', M=A21, N=M1, MESH=MESH)
!
!     SECOND MEMBER SMV
!
      CALL MATVEC( 'X=X+AY  ',SMV,M1,UN,C,MESH)
!
!------------------------------------------------------------------
!
      RETURN
      END
