!                       ************************
                        SUBROUTINE FRICTION_USER
!                       ************************
!
!
!***********************************************************************
! TELEMAC2D   V8P2
!***********************************************************************
!
!brief    DEFINES FRICTION ZONES (BY NODE).
!+        The file format for giving a zone number to zones is here:
!+        node-number   zone-number
!+        for all the nodes, example:
!+        1  123
!+        2  123
!+        3  47
!+        4  47
!+        etc.
!+
!
!history  F. HUVELIN
!+        15/04/2004
!+        V5P4
!+   First version.
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/01/2014
!+        V7P0
!+   Provisionnal version, with ZONES FILE, but this file has yet to be
!+   treated in partel. KNOGL replaced by GLOBAL_TO_LOCAL_POINT.
!
!history  Y AUDOUIN (EDF LAB, LNHE)
!+        19/03/2014
!+        V7P0
!+   Zones file now written in local numbering. Plus checking of point
!+   numbers added by JMH and 2 trailing temporary arrays removed.
!
!history R KOPMANN (BAW)
!+        12/11/2019
!+        V8P2
!+   Reading FRIC_ID from geometry file if there.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,K,IVAL2,NFILE,RECORD,IERR
      CHARACTER(LEN=PATH_LEN) :: NOMFILE
      DOUBLE PRECISION BID
      LOGICAL NOBFR
!
!-----------------------------------------------------------------------
!
! READING FRIC_ID FROM GEOMETRY FILE IF FRICTION DATA=YES
      RECORD = 0
      BID = 0.D0
      NOBFR = .FALSE.
!
      IF(FRICTB) THEN
        CALL FIND_VARIABLE(T2D_FILES(T2DGEO)%FMT,T2D_FILES(T2DGEO)%LU,
     &                     'FRIC_ID         ',T1%R, MESH%NPOIN,IERR,
     &                     RECORD=RECORD,TIME_RECORD=BID)
        IF(IERR.EQ.0) THEN
          WRITE(LU,6)
 6        FORMAT(1X,'FRICTION_USER: FRICTION ID  READ IN THE ',/,
     &            1X,'GEOMETRY FILE')
          NOBFR = .TRUE.
!
! COPIYNG FRICTION ID IN KFROPT
          DO I = 1,NPOIN
            KFROPT%I(I) = NINT(T1%R(I))
          END DO
        ELSE
          WRITE(LU,7)
 7        FORMAT(1X,'FRICTION_USER : FRICTION ID  NOT FOUND IN THE',/,
     &           1X,'GEOMETRY FILE')
        ENDIF
      ENDIF
!
      NFILE   = T2D_FILES(T2DZFI)%LU
      NOMFILE = T2D_FILES(T2DZFI)%NAME
!
!     READING FILE WITH FRICTION IDS (WHICH IS NOW WRITTEN IN LOCAL
!     NUMBERING)
!
      IF(.NOT.NOBFR) THEN
        DO K=1,NPOIN
          READ(NFILE,*,END=997,ERR=998) I, IVAL2
          IF(K.EQ.I) THEN
            KFROPT%I(I) = IVAL2
          ELSE
          WRITE(LU,*) 'ERROR IN THE ZONES FILE: ',NOMFILE
            WRITE(LU,*) 'POINT ',K,' EXPECTED, POINT ',I,' FOUND'
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF !NOBFR
      GOTO 997
!
!-----------------------------------------------------------------------
!             ERROR WHEN READING
!-----------------------------------------------------------------------
!
998   CONTINUE
      WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
      WRITE(LU,*) 'READ ERROR'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
997   CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
