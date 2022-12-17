!                   *****************
                    SUBROUTINE FONSTR
!                   *****************
!
     &(H,ZF,Z,CHESTR,NGEO,FFORMAT,NFON,NOMFON,MESH,FFON,LISTIN,
     & N_NAMES_PRIV,NAMES_PRIVE,PRIVE)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    LOOKS FOR 'BOTTOM' IN THE GEOMETRY FILE.
!+
!+            LOOKS FOR 'BOTTOM FRICTION' (COEFFICIENTS).
!
!note     THE NAMES OF THE VARIABLES HAVE BEEN DIRECTLY
!+         WRITTEN OUT AND ARE NOT READ FROM 'TEXTE'.
!+         THIS MAKES IT POSSIBLE TO HAVE A GEOMETRY FILE
!+         COMPILED IN ANOTHER LANGUAGE.
!
!history  J-M HERVOUET (LNH)
!+        17/08/94
!+        V5P6
!+   First version
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
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the format FFORMAT
!
!history Y AUDOUIN (LNHE)
!+        25/05/2015
!+        V7P0
!+        Modification to comply with the hermes module
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/07/2015
!+        V7P1
!+   Now able to read user variables and put them in PRIVE arrays.
!
!history  R ATA (EDF LAB, LNHE)
!+        24/05/2016
!+        V7P2
!+   BID initialised for cases of selafin files without records.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHESTR         |<--| FRICTION COEFFICIENT (DEPENDING ON FRICTION LAW)
!| FFON           |-->| FRICTION COEFFICIENT IF CONSTANT
!| H              |<--| WATER DEPTH
!| LISTIN         |-->| IF YES, WILL GIVE A REPORT
!| MESH           |-->| MESH STRUCTURE
!| N_NAMES_PRIV   |-->| NUMBER OF PRIVATE ARRAYS WITH GIVEN NAMES
!| NAMES_PRIV     |-->| NAMES OF PRIVATE ARRAYS GIVEN BY USER
!| NFON           |-->| LOGICAL UNIT OF BOTTOM FILE
!| NGEO           |-->| LOGICAL UNIT OF GEOMETRY FILE
!| NOMFON         |-->| NAME OF BOTTOM FILE
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS
!| Z              |<--| FREE SURFACE ELEVATION
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FONSTR => FONSTR
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: H,ZF,Z,CHESTR,PRIVE
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      DOUBLE PRECISION, INTENT(IN)  :: FFON
      LOGICAL, INTENT(IN)           :: LISTIN
      INTEGER, INTENT(IN)           :: NGEO,NFON,N_NAMES_PRIV
      CHARACTER(LEN=72), INTENT(IN) :: NOMFON
      CHARACTER(LEN=32), INTENT(IN) :: NAMES_PRIVE(4)
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IERR,RECORD,I
!
      DOUBLE PRECISION BID
!
      LOGICAL CALFON,CALFRO,LUZF,LUH,LUZ
!
!-----------------------------------------------------------------------
!
!    INITIALISES
!
      LUH    =.FALSE.
      LUZ    =.FALSE.
      LUZF   =.FALSE.
      CALFRO =.TRUE.
      BID    =0.D0
      RECORD =0
!
!-----------------------------------------------------------------------
!
!     LOOKS FOR THE FRICTION COEFFICIENT IN THE FILE
!
      IF(LNG.EQ.LNG_FR)
     &         CALL FIND_VARIABLE(FFORMAT, NGEO,'FROTTEMENT      ',
     &                            CHESTR%R, MESH%NPOIN,
     &                            IERR,RECORD=RECORD,TIME_RECORD=BID)
      IF(LNG.EQ.LNG_EN)
     &         CALL FIND_VARIABLE(FFORMAT, NGEO,'BOTTOM FRICTION ',
     &                            CHESTR%R, MESH%NPOIN,
     &                            IERR,RECORD=RECORD,TIME_RECORD=BID)
!     CASE OF A GEOMETRY FILE IN ANOTHER LANGUAGE
      IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_FR) THEN
        CALL FIND_VARIABLE(FFORMAT, NGEO,'BOTTOM FRICTION ',
     &                  CHESTR%R, MESH%NPOIN,
     &                 IERR,RECORD=RECORD,TIME_RECORD=BID)
      ENDIF
      IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_EN) THEN
        CALL FIND_VARIABLE(FFORMAT, NGEO, 'FROTTEMENT      ',
     &                 CHESTR%R,MESH%NPOIN,
     &                 IERR,RECORD=RECORD,TIME_RECORD=BID)
      ENDIF
      IF(IERR.EQ.0) THEN
        CALFRO = .FALSE.
        WRITE(LU,6)
6       FORMAT(1X,'FONSTR : FRICTION COEFFICIENTS READ IN THE',/,
     &         1X,'         GEOMETRY FILE')
      ENDIF
!
!     LOOKS FOR THE BOTTOM ELEVATION IN THE FILE
!
      IF(LNG.EQ.LNG_FR)
     &         CALL FIND_VARIABLE(FFORMAT, NGEO,'FOND            ',
     &                            ZF%R, MESH%NPOIN,
     &                            IERR,RECORD=RECORD,TIME_RECORD=BID)
      IF(LNG.EQ.LNG_EN) CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                            'BOTTOM          ',ZF%R, MESH%NPOIN,
     &                            IERR,RECORD=RECORD,TIME_RECORD=BID)
      IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_FR) THEN
        CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                 'BOTTOM          ', ZF%R,MESH%NPOIN,
     &                 IERR,RECORD=RECORD,TIME_RECORD=BID)
      ENDIF
      IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_EN) THEN
        CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                 'FOND            ', ZF%R,MESH%NPOIN,
     &                 IERR,RECORD=RECORD,TIME_RECORD=BID)
      ENDIF
!     MESHES FROM BALMAT ?
      IF(IERR.EQ.HERMES_VAR_UNKNOWN_ERR)
     &                   CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                   'ALTIMETRIE      ', ZF%R,MESH%NPOIN,
     &                   IERR,RECORD=RECORD,TIME_RECORD=BID)
!     TOMAWAC IN FRENCH ?
      IF(IERR.EQ.HERMES_VAR_UNKNOWN_ERR)
     &                   CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                   'COTE_DU_FOND    ', ZF%R,MESH%NPOIN,
     &                   IERR,RECORD=RECORD,TIME_RECORD=BID)
!     TOMAWAC IN ENGLISH ?
      IF(IERR.EQ.HERMES_VAR_UNKNOWN_ERR)
     &                   CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                   'BOTTOM_LEVEL    ', ZF%R,MESH%NPOIN,
     &                   IERR,RECORD=RECORD,TIME_RECORD=BID)
      LUZF = IERR.EQ.0
!
      IF(.NOT.LUZF) THEN
!       LOOKS FOR WATER DEPTH AND FREE SURFACE ELEVATION
        IF(LNG.EQ.LNG_FR) CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                              'HAUTEUR D''EAU   ', H%R,MESH%NPOIN,
     &                              IERR,RECORD=RECORD,TIME_RECORD=BID)
        IF(LNG.EQ.LNG_EN) CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                              'WATER DEPTH     ', H%R,MESH%NPOIN,
     &                              IERR,RECORD=RECORD,TIME_RECORD=BID)
        IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_FR) THEN
          CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                   'WATER DEPTH     ', H%R,MESH%NPOIN,
     &                   IERR,RECORD=RECORD,TIME_RECORD=BID)

        ENDIF
        IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_EN) THEN
          CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                   'HAUTEUR D''EAU   ', H%R,MESH%NPOIN,
     &                   IERR,RECORD=RECORD,TIME_RECORD=BID)
        ENDIF
        LUH = IERR.EQ.0
        IF(LNG.EQ.LNG_FR) CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                              'SURFACE LIBRE   ', Z%R,MESH%NPOIN,
     &                              IERR, RECORD=RECORD,TIME_RECORD=BID)
        IF(LNG.EQ.LNG_EN) CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                              'FREE SURFACE    ',Z%R, MESH%NPOIN,
     &                              IERR, RECORD=RECORD,TIME_RECORD=BID)
        IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_FR) THEN
          CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                   'FREE SURFACE    ', Z%R,MESH%NPOIN,
     &                   IERR, RECORD=RECORD,TIME_RECORD=BID)
        ENDIF
        IF((IERR.EQ.HERMES_VAR_UNKNOWN_ERR).AND.LNG.EQ.LNG_EN) THEN
          CALL FIND_VARIABLE(FFORMAT, NGEO,
     &                   'SURFACE LIBRE   ', Z%R,MESH%NPOIN,
     &                   IERR, RECORD=RECORD,TIME_RECORD=BID)
        ENDIF
        LUZ = IERR.EQ.0
      ENDIF
!
!     INITIALISES THE BOTTOM ELEVATION
!
      IF(LUZF) THEN
!
        CALFON = .FALSE.
!
      ELSE
!
        IF (LUZ.AND.LUH) THEN
!
          CALL OS( 'X=Y-Z   ',X=ZF,Y=Z,Z=H)
          WRITE(LU,25)
25        FORMAT(1X,'FONSTR (BIEF): ATTENTION, THE BOTTOM RESULTS',/,
     &              '               FROM DEPTH AND SURFACE ELEVATION',
     &            /,'               FOUND IN THE GEOMETRY FILE')
          CALFON = .FALSE.
!
        ELSE
!
          CALFON = .TRUE.
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! BUILDS THE BOTTOM IF IT WAS NOT IN THE GEOMETRY FILE
!
      IF(NOMFON(1:1).NE.' ') THEN
!       A BOTTOM FILE WAS GIVEN, (RE)COMPUTES THE BOTTOM ELEVATION
        IF(LISTIN) THEN
          WRITE(LU,2224) NOMFON
          IF(.NOT.CALFON) THEN
            WRITE(LU,2226)
          ENDIF
        ENDIF
2224    FORMAT(/,1X,'FONSTR (BIEF): BATHYMETRY GIVEN IN FILE : ',A72)
2226    FORMAT(  1X,'               BATHYMETRY FOUND IN THE',/,
     &           1X,'               GEOMETRY FILE IS IGNORED',/)
!
        CALL FOND(ZF%R,MESH%X%R,MESH%Y%R,MESH%NPOIN,NFON,
     &            MESH%NBOR%I,MESH%KP1BOR%I,MESH%NPTFR)
!
      ELSEIF(CALFON) THEN
        IF(LISTIN) THEN
          WRITE(LU,2228)
        ENDIF
2228    FORMAT(/,1X,'FONSTR (BIEF): NO BATHYMETRY IN THE GEOMETRY FILE',
     &         /,1X,'               AND NO BATHYMETRY FILE. THE BOTTOM',
     &         /,1X,'               LEVEL IS FIXED TO ZERO BUT STILL',
     &         /,1X,'               CAN BE MODIFIED IN CORFON.',
     &         /,1X)
        CALL OS( 'X=0     ',X=ZF)
      ENDIF
!
!-----------------------------------------------------------------------
!
! LOOKING FOR THE USER VARIABLES
!
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          CALL FIND_VARIABLE(FFORMAT,NGEO,NAMES_PRIVE(I)(1:16),
     &                   PRIVE%ADR(I)%P%R,MESH%NPOIN,
     &                   IERR,RECORD=RECORD,TIME_RECORD=BID)
          IF(IERR.EQ.0) THEN
            WRITE(LU,*) 'VARIABLE ',NAMES_PRIVE(I)(1:32),
     &                                   ' FOUND IN THE GEOMETRY FILE'
          ELSE
            WRITE(LU,*) 'VARIABLE ',NAMES_PRIVE(I)(1:32),
     &                               ' NOT FOUND IN THE GEOMETRY FILE'
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE BOTTOM FRICTION COEFFICIENT
!
      IF(CALFRO) THEN
        CALL OS('X=C     ', X=CHESTR, C=FFON)
      ENDIF
      CALL STRCHE
!
!-----------------------------------------------------------------------
!
      RETURN
      END

