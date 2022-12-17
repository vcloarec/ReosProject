!                   *****************
                    SUBROUTINE NEWSTR
!                   *****************
!
     &(SETSTR,SETSTR2,DESC,RO,RSTART,NPARAM)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE NEW SET OF FRICTION COEFFICIENTS.
!
!history  E. BARROS
!+        22/03/1994
!+        V2P2
!+
!
!history  A. LEOPARDI (UNINA)
!+        02/10/2000
!+        V5P1
!+
!
!history  J-M HERVOUET TEL:
!+        22/10/2001
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
!| DESC           |-->| DIRECTION OF DESCENT
!| NPARAM         |-->| NUMBER OF PARAMETERS TO ESTIMATE
!| RO             |-->| SETSTR=SETSTR2+RO*DESC
!| RSTART         |-->| LOGICAL, RESTART COMPUTATION BECAUSE OUT OF LIMITS
!| SETSTR         |-->| SET OF FRICTION COEFFICIENTS IN A BIEF_OBJ STRUCTURE
!| SETSTR2        |-->| OLD SET
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION , INTENT(IN)    :: RO
      TYPE (BIEF_OBJ)  , INTENT(IN)    :: DESC
      TYPE (BIEF_OBJ)  , INTENT(IN)    :: SETSTR2
      TYPE (BIEF_OBJ)  , INTENT(INOUT) :: SETSTR
      LOGICAL          , INTENT(INOUT) :: RSTART
      INTEGER          , INTENT(IN)    :: NPARAM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!---------------------------------------------------------------------
!
      CALL OV('X=Y+CZ  ', X=SETSTR%R, Y=SETSTR2%R, Z=DESC%R, C=RO,
     &        DIM1=NPARAM)
!
!     TESTS LIMITS
!     LIMITS (1,100)
!
      RSTART=.FALSE.
      DO I=1,NPARAM
        IF (SETSTR%R(I).LT.1.D0) THEN
          SETSTR%R(I)=1.D0
          RSTART=.TRUE.
        ENDIF
        IF (SETSTR%R(I).GT.100.D0) THEN
          SETSTR%R(I)=100.D0
          RSTART=.TRUE.
        ENDIF
      ENDDO
!
!---------------------------------------------------------------------
!
      RETURN
      END
