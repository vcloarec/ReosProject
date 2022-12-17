!                   ********************
                    SUBROUTINE BIEF_INIT
!                   ********************
!
     &(CHAINE,NCAR,PINIT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  J-M HERVOUET (LNH)
!+
!+        V6P0
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
!| CHAINE         |<->| NAME OF CURRENT DIRECTORY
!| CODE           |-->| NAME OF CALLING PROGRAMME
!| NCAR           |<->| LENGTH OF CHAIN
!| PINIT          |-->| LOGICAL, IF YES, INITIALIZE PARALLELISM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_INIT => BIEF_INIT
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN)               :: PINIT
      CHARACTER(LEN=MAXLENTMPDIR), INTENT(INOUT) :: CHAINE
      INTEGER, INTENT(INOUT)            :: NCAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     TO RUN IN PARALLEL MODE
!
!     P_INIT (PARALLEL) RETURNS THE WORKING DIRECTORY AND ITS LENGTH
!     P_INIT (PARAVOID) RETURNS NCAR = 0 AND NCSIZE = 0
!
      IF(PINIT) CALL P_INIT(CHAINE,NCAR,IPID,NCSIZE)
!
!-----------------------------------------------------------------------
!
!     LANGUAGE AND LOGICAL UNIT FOR OUTPUTS
!
      CALL READ_CONFIG(CHAINE,NCAR)

#if defined BOGUS
      ! Bogus call so that they are added when running rescan
      CALL INPOLY
#endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END
