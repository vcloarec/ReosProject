!                   *****************
                    SUBROUTINE METGRA
!                   *****************
!
     &(RO,GRADJ,GRADJN,JCOUT1,DESC,NPARAM,OPTID,RSTART,R02,R03)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ONE STEP OF GRADIENT METHOD.
!
!history  E. BARROS
!+        02/08/1993
!+
!+
!
!history  A. LEOPARDI (UNINA)
!+        04/10/2000
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
!| DESC           |<--| VECTOR USED TO CHANGE THE SET OF STRICKLERS'
!| GRADJ          |-->| GRADIENT OF COST FUNCTION (ITERATION K)
!| GRADJN         |-->| GRADIENT OF COST FUNCTION (ITERATION K-1)
!| JCOUT1         |-->| COST FUNCTION
!| NPARAM         |-->| TOTAL NUMBER OF PARAMETERS TO ESTIMATE
!| OPTID          |-->| METHOD 1=GRADIENT, 2=GRADIENT CONJUGUE, 3=LAGRANGE)
!| R02            |<--| COEFFICIENT IN THE GRADIENT METHOD
!| R03            |<--| COEFFICIENT IN THE GRADIENT METHOD
!| RO             |<->| COEFFICIENT OF THE GRADIENT
!| RSTART         |-->| IF YES, STARTING FROM SCRATCH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER , INTENT(IN)             :: NPARAM,OPTID
      DOUBLE PRECISION , INTENT(IN)    :: JCOUT1
      LOGICAL , INTENT(IN)             :: RSTART
      TYPE(BIEF_OBJ) , INTENT(IN)      :: GRADJ,GRADJN
      TYPE(BIEF_OBJ) , INTENT(INOUT)   :: DESC
      DOUBLE PRECISION , INTENT(INOUT) :: R02,R03,RO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION R1,DENOM,GRAD_JN
!
!     COMPUTES THE TRUE GRADIENT (WHICH TAKES THE TRUE NUMBER OF
!                                           PARAMETERS INTO ACCOUNT)
      DENOM=0.D0
      GRAD_JN=0.D0
      DO I = 1,NPARAM
        DENOM  = DENOM + GRADJ%R(I)**2
        GRAD_JN=GRAD_JN+GRADJN%R(I)**2
      ENDDO
!
      IF(DENOM.LT.1.D-12) THEN
        WRITE(LU,*) 'METGRA: GRADIENT TOO SMALL, STOP'
        WRITE(LU,*) 'DENOM = ',DENOM
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!     RO = - JCOUT / GRADJ*GRADJ
!-----------------------------------------------------------------------
!
      IF(OPTID.EQ.1.OR.OPTID.EQ.3.OR.RSTART) THEN
!
        R02 = - JCOUT1 / DENOM
        RO = R02
        R03=0.5D0*R02
!
!       COMPUTES THE DIRECTION OF INITIAL DESCENT
!
        CALL OV('X=Y     ', X=DESC%R, Y=GRADJ%R, DIM1=NPARAM)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OPTID.EQ.2) THEN
!
        R02 = - JCOUT1 / DENOM
!
        R1 = GRAD_JN/DENOM
!
!       COMPUTES THE DIRECTION OF DESCENT
!
        CALL OV('X=Y+CZ  ', X=DESC%R, Y=GRADJ%R, Z=DESC%R, C=R1,
     &          DIM1=NPARAM)
!
        DENOM=0.D0
        DO I=1,NPARAM
          DENOM=DENOM+GRADJ%R(I)*DESC%R(I)
        ENDDO
        R03 = - JCOUT1/DENOM
        RO =R03
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
