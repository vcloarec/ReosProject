!                   **********************
                    SUBROUTINE READ_CONFIG
!                   **********************
!
     &(CHAINE,NCAR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS LANGUAGE AND LOGICAL UNIT OF OUTPUTS IN A FILE WRITTEN
!         BY PERL OR PYTHON LAUNCHING SCRIPTS
!
!history  J-M HERVOUET (LNH)
!+
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
!| CHAINE         |-->| FULL ADDRESS OF TEMPORARY DIRECTORY
!| NCAR           |-->| LENGTH OF STRING CHAINE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=PATH_LEN), INTENT(IN)    :: CHAINE
      INTEGER      , INTENT(IN)    :: NCAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL YACONFIG
      INTEGER NC
      CHARACTER(LEN=257) CONFIG
      INTEGER ID
!
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
        CONFIG(1:NCAR+6)=CHAINE(1:NCAR) // 'CONFIG'
        NC=NCAR+6
      ELSE
        CONFIG(1:6)='CONFIG'
        NC=6
      ENDIF
!
      YACONFIG=.FALSE.
      INQUIRE(FILE=CONFIG(1:NC),EXIST=YACONFIG)
      IF(YACONFIG) THEN
!
        CALL GET_FREE_ID(ID)
        OPEN(ID,FILE=CONFIG(1:NC), FORM='FORMATTED')
        READ(ID,*) LNG
!
!       DO NOT OVERLOAD LU IN PARALLEL MODE (WINNT)
!       (KEEP THE REDIRECTION ON CHANNEL 95 MADE BY P_INIT)
!       IF(LU.NE.95) READ(ID,*) LU
        CLOSE(ID)
!
      ELSE
!
        WRITE(LU,*) 'READ_CONFIG: FILE CONFIG NOT FOUND: ',
     &              CONFIG(1:NC)
        WRITE(LU,*) 'DEFAULTS VALUES OF LU AND LNG: ',LU,' AND ',LNG
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
