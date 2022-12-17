!                   ***************************
                    SUBROUTINE BIEF_CLOSE_FILES
!                   ***************************
!
     &(FILES,NFILES,PEXIT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  J-M HERVOUET (LNHE)
!+        01/04/2009
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILES          |-->| ARRAY OF BIEF_FILE STRUCTURES
!| NFILES         |-->| TOTAL NUMBER OF FILES
!| PEXIT          |-->| LOGICAL, IF YES, P_EXIT WILL BE CALLED
!|                |   | TO STOP PARALLELISM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_CLOSE_FILES => BIEF_CLOSE_FILES
!
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)     :: NFILES
      LOGICAL, INTENT(IN)               :: PEXIT
      TYPE(BIEF_FILE)   , INTENT(INOUT) :: FILES(NFILES)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IERR
!
!-----------------------------------------------------------------------
!
      DO I=1,NFILES
!
        IF(FILES(I)%NAME(1:1).NE.' ') THEN
!
!         CLOSES THE FILE
!
          IF(FILES(I)%TYPE.EQ.'CONLIM') CYCLE
          IF((FILES(I)%FMT(1:7).EQ.'SERAFIN')
     &       .OR.(FILES(I)%FMT.EQ.'MED     ')) THEN
            IF(FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
              IF(IPID.EQ.0.OR.FILES(I)%ACTION(1:4).EQ.'READ') THEN
                CALL CLOSE_BND(FILES(I)%FMT, FILES(I)%LU, IERR)
                CALL CHECK_CALL(IERR,'BIEF_CLOSE_FILE:CLOSE_BND')
                CALL CLOSE_MESH(FILES(I)%FMT, FILES(I)%LU, IERR)
                CALL CHECK_CALL(IERR,'BIEF_CLOSE_FILE:CLOSE_MESH')
              ENDIF
            ELSE
              CALL CLOSE_BND(FILES(I)%FMT, FILES(I)%LU, IERR)
              CALL CHECK_CALL(IERR,'BIEF_CLOSE_FILE:CLOSE_BND')
              CALL CLOSE_MESH(FILES(I)%FMT, FILES(I)%LU, IERR)
              CALL CHECK_CALL(IERR,'BIEF_CLOSE_FILE:CLOSE_MESH')
            ENDIF
          ELSE
            CLOSE(FILES(I)%LU)
          ENDIF
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     PARALLEL MODE: STOPS IF PEXIT
!
      IF(PEXIT) CALL P_EXIT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
